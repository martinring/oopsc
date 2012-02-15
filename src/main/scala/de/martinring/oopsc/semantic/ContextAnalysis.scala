package de.martinring.oopsc.semantic

import de.martinring.oopsc._
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.syntactic.Class._
import de.martinring.oopsc.semantic.Transform._
import de.martinring.oopsc.output._
import de.martinring.util.Failable.pimpOption


/**
 * Object for the contextual analysis. Utilizes the [[de.martinring.oopsc.Transform]] monad.
 * @author Martin Ring
 */
object ContextAnalysis {  
  /**
   * Analyses a program and checks if it satisfies the following criteria:
   * 
   *  - All classes must have unique names (including the implicitly existent
   *    classes `Integer`, `Boolean` and `Object`)
   *  - All classes must themselves be successfully analysed.
   *  - There must be a class named `Main` defined which must have a method
   *    `main` that must not take parameters or have a return type other than
   *    void)
   */
  def analyse(p: Program): Transform[Program] = for {
    _       <- bind(predefined) >> bind(p.classes)    
    classes <- sequence((p.classes ++ predefinedClasses) map (c => (signature(c.name))))    
    classes <- sequence(classes map (c => enter(c.name.relative)(analyse(c))))
    main    <- getType(Root / "Main") ! ("missing main class" at p)
    entry   <- getMethod(Root / "Main" / "main") ! ("missing main method" at main)
    _       <- throwIf(!entry.parameters.isEmpty)("main method must not take parameters" at entry.parameters.head) >>
               throwIf(entry.typed != voidType.name)("main method must be of void type" at entry.typed)
  } yield Program(classes) at p

  /**
   * Binds the signature of a class and it's methods. This checks if there is no 
   * cyclic inheritance involving this class and the signatures of the overridden
   * methods match. 
   * Also imports the virtual method table of the base class and creates aliases 
   * for methods and attributes of the base class. Determines the size of the 
   * class.
   * Fails if variable or method names are used more than once.
   */
  def signature(c: Name, actual: Class = null): Transform[Class] = for {
    n <- resolve(c) 
    c <- getType(n)
    r <- c.name match {
      case r: AbsoluteName => just(c)
      case i: RelativeName => enter(n)( for {
        _        <- require(c != actual && (actual == null || c.baseType.map(_!=c.name).getOrElse(true)))(
                            "Cyclic inheritance" at actual.baseType.get) >>
                    require(c.baseType.map(_ != c.name).getOrElse(true))(
                          (c.name + " extends itself") at c.baseType.get)          
        name     <- resolveClassName(c.name)
        base     <- c.baseType match {
          case Some(n) => for {
            baseName <- resolve(n) 
            base     <- getType(baseName)
            base     <- signature(baseName, if (actual == null) c else actual)
            _        <- getVMT(base.name) >>= setVMT(name)
            _        <- sequence(base.methods map createAlias) >>
                        sequence(base.attributes map createAlias)
          } yield Some(base)
          case None    => just(None) 
        }                               
        _        <- bind(c.attributes) >> bind(c.methods)
        val baseSize = base.map(_.size).getOrElse(Class.headerSize)
        val size     = baseSize + c.attributes.size
        attrs    <- sequence(c.attributes.zip(baseSize until size) map analyse)
        methods  <- sequence(c.methods map (m => enter(m.name.relative)(signature(m))))
        result   <- update(Class(name, attrs, methods, base.map(_.name), size) at c)
      } yield result)
    }
  } yield r

  private def insert(c: List[Method], m: Method, index: Int = 0): (Int, List[Method]) = c match {
    case Nil  => (index, List(m))
    case h::t => if (h.name.relative == m.name.relative) (index, m::t)
                 else {
                   val (i, t2) = insert(t, m, index + 1)
                   (i, h::t2)
                 }
  }
  
  /**
   * Binds the signature of a method. And checks if the method is allready existent
   * in the vmt of the current class. If that is the case the method must satisfy
   * the following:
   * 
   *  - it must not narrow the visibility of the overridden method
   *  - it must not have a different return type than the overriden method   
   *  - it must not have different parameter length or types than the overridden
   *    method
   *    
   * All methods must declare only uniquely named variables and parameters. They
   * can though declare variables or parameters that shadow outer names.
   */
  def signature(m: Method): Transform[Method] = for {
    name   <- resolve(m.name)
    _      <- bind(m.parameters) >> bind(m.variables)
    params <- sequence(m.parameters.zip(-1-m.parameters.size until -1) map analyse)    
    typed  <- resolveClassName(m.typed)
    self   <- currentClass
    vmt    <- currentClass >>= getVMT
    index  <- vmt indexWhere (_.relative == m.name.relative) match {
      case -1 => for {
        _ <- setVMT(self)(vmt :+ name)
      } yield vmt.size
      case x  => for {
        o <- getMethod(vmt(x))
        _ <- require(o.visibility <= m.visibility)("overriding method may not narrow visibility" at m) >>
             require(o.typed == typed)("method must have the same return type ("+o.typed+") as the overridden method" at m) >>
             require(o.parameters.size == params.size)("method must have the same number of parameters ("+o.parameters.size+") as the overridden method" at m) >>
             sequence(o.parameters.zip(params) map { case (a,b) => require(a.typed == b.typed)("overriding method has incompatible parameter type (required "+a.typed+")" at b) }) >>
             setVMT(self)(vmt.updated(x,name))
      } yield x
    }
    result <- update(Method(name, params, m.variables, m.body, typed, Some(index), m.visibility) at m)
  } yield result
  
  /**
   * Analyses a class. It is presumed, that `signature` has been executed before.
   * Calls analysis on all methods and updates the bindings.
   * @param c the class to analyse
   */
  def analyse(c: Class): Transform[Class] = for {    
    methods <- sequence(c.methods map (m => enter(m.name.relative)(analyse(m))))
    result  <- update(c.copy(methods = methods) at c)
  } yield result

  /**
   * Analyses a variable. And inserts the provided offset. Fails if the type of
   * the variable is unknown.
   * @param v tuple of variable and offset to analyse
   */
  def analyse(v: (Variable, Int)): Transform[Variable] = v match { case (v,i) => for {
    name   <- resolve(v.name)
    typed  <- resolveClassName(v.typed)
    result <- update(v.copy(name = name, offset = Some(i), typed = typed) at v)
  } yield result }

  /**
   * Analyses a method. Presumes that, `signaure` has been executed before.
   * Introduces local variables `SELF` and `BASE` of the type of the current class
   * and its base class (`Object` if not declared).
   * @param m the method to analyse
   */
  def analyse(m: Method): Transform[Method] = for {
    self       <- currentClass
    base       <- getType(self) map (_.baseType.get)
    path       <- path
    _          <- bind(Variable(m.name.asInstanceOf[AbsoluteName] / "SELF", self, Some(-2-m.parameters.size), false)) >>
                  bind(Variable(m.name.asInstanceOf[AbsoluteName] / "BASE", base, Some(-2-m.parameters.size), false))
    variables  <- sequence(m.variables.zip(1 to m.variables.size) map analyse)
    body       <- sequence(m.body map analyse)
    val (b,t)   = body.span(!_.returns) // Partition body in parts before and after return
    _          <- throwIf(m.typed != voidType.name && !body.exists(_.returns))(("missing return in method " + m.name) at m) >>
                  throwIf(t.size > 1)(Warn(t.head.pos, "statements after return will never be reached"))
    result     <- update(m.copy(variables = variables, body = b :+ (t.headOption getOrElse Return(None))) at m)
  } yield result

  /**
   * Analyses a statement:
   * 
   *  - `READ` and `WRITE` statements must have an operand of type `Integer`
   *  - the operand of a `READ` statement must be an l-value
   *  - `WHILE` and `IF` statements must have a condition of type `Boolean`.
   *  - `CALL` statements must contain a call of a method which returns nothing.
   *  - the left and right expressions of an `ASSIGN` must match in types and the
   *    left expression must be an l-value.
   *  - the expression of a `RETURN` statement must match the return type of the
   *    current method. Methods with return type `VOID` must not return anything
   *    and thus only contain plain `RETURN` statements or none.
   *  - all contained expressions are analysed by combinating `analyse`
   */
  def analyse(st: Statement): Transform[Statement] = st match {
    case r: Read => for {
      operand <- analyse(r.operand) >>= requireType(intClass)
      _       <- throwIf(!operand.isLValue) ("l-value expected" at operand)
    } yield Read(operand) at r

    case w: Write => for {
      operand <- analyse(w.operand) >>= unBox >>= requireType(intType)
    } yield Write(operand) at w

    case w: While => for {
      condition <- analyse(w.condition) >>= unBox >>= requireType(boolType)
      body      <- sequence(w.body map analyse)
    } yield While(condition, body) at w

    case i: If => for {
      condition <- analyse(i.condition) >>= unBox >>= requireType(boolType)
      body      <- sequence(i.body map analyse)
      elseBody  <- sequence(i.elseBody map analyse)
    } yield If(condition, body, elseBody) at i

    case c: Call => for {
      call <- analyse(c.call) >>= requireType(voidType)
    } yield Call(call) at c

    case a: Assign => for {
      left  <- analyse(a.left)
      _     <- throwIf(!left.isLValue || isBase(a.left) || isSelf(a.left)) ("l-value expected" at left)      
      right <- analyse(a.right) >>= box >>= requireType(left.typed)
    } yield Assign(left, right) at a

    case r@Return(Some(expr)) => for {
      m     <- currentMethod.map(_.getOrElse(sys.error("not in a method"))) >>= getMethod
      expr  <- analyse(expr) >>= box >>= requireType(m.typed)
    } yield Return(Some(expr)) at r

    case r@Return(None) => for {
      m     <- currentMethod.map(_.getOrElse(sys.error("not in a method"))) >>= getMethod
      expr  <- throwIf(m.typed != voidType.name)(("expected " + m.typed) at r)
    } yield r
  }

  /**
   * Analyses an expression:
   * 
   *  - The operand of an unary expression must be of type `Boolean` if the 
   *    operator is `NOT` or `Integer` if the operator is `-`. Their return type
   *    matches the type of the operand.
   *  - The operands of a binary expression must be of type `Integer` if the 
   *    operand is one of `+`, `-`, `*`, `/` or `MOD`. In that case the type of
   *    the expression is `Integer`. 
   *  - The operands of binary expressions with operators `<`, `<=`, `>` or `>=`
   *    must be of type `Integer`. The resulting expression has type `Boolean`.
   *  - `AND`, `OR`, `AND THEN` and `OR ELSE` are operators for `Boolean` 
   *    expressions. Their resulting type is also `Boolean`
   *  - Binary expressions with operators `=` and `#` must have matching operand
   *    types. The return type is `Boolean`
   *  - Literals are untouched by this
   *  - `NEW` expressions must refer to an existing class name
   *  - The type of the left hand side of an `ACCESS` statements is inferred and
   *    then the right hand side is passed to the operation `analyseMember`
   *  - Variables and method calls are also passed to `analyseMember`. If they 
   *    refer to an attribute or a method an `ACCESS` statement on `SELF` is
   *    introduced.
   *  - All contained expressions are recursively passed to this function.
   *  
   *  @param e the Expression to analyse
   */
  def analyse(e: Expression): Transform[Expression] = e match {
    case u: Unary => u.operator match {
      case "-" => for {
        operand  <- analyse(u.operand) >>= unBox >>= requireType(intType)
      } yield new Unary(u.operator, operand, intType.name) at u

      case "NOT" => for {
        operand  <- analyse(u.operand) >>= unBox >>= requireType(boolType)
      } yield new Unary(u.operator, operand, boolType.name) at u      
    }

    case b: Binary => b.operator match {
      case "+" | "-" | "*" | "/" | "MOD" => for {
        left  <- analyse(b.left) >>= unBox >>= requireType(intType)
        right <- analyse(b.right) >>= unBox >>= requireType(intType)
      } yield new Binary(b.operator, left, right, intType.name) at b

      case "<" | "<=" | ">" | ">=" => for {
        left  <- analyse(b.left) >>= unBox >>= requireType(intType)
        right <- analyse(b.right) >>= unBox >>= requireType(intType)
      } yield new Binary(b.operator, left, right, boolType.name) at b

      case "AND" | "OR" | "THEN" | "ELSE" => for {
        left  <- analyse(b.left) >>= unBox >>= requireType(boolType)
        right <- analyse(b.right) >>= unBox >>= requireType(boolType)
      } yield new Binary(b.operator, left, right, boolType.name) at b

      case "=" | "#" => for {
        left  <- analyse(b.left) >>= unBox
        right <- analyse(b.right) >>= unBox >>= requireType(left.typed)
      } yield new Binary(b.operator, left, right, boolType.name) at b
    }

    case l: Literal => just(l)

    case n: New => for {
      t <- resolveClassName(n.typed)
    } yield n.copy(typed = t) at n

    case a: Access => for {      
      left  <- analyse(a.left) >>= box      
      ps    <- sequence(a.right.parameters map (analyse(_) >>= box))
      c     <- currentClass
      right <- enter(left.typed)(analyseMember(c, a.right.copy(parameters = ps) at a.right, isBase(a.left)))
    } yield Access(left, right) at a

    case voc: VarOrCall => for {
      ps   <- sequence(voc.parameters map (analyse(_) >>= box))
      c    <- currentClass
      voc  <- analyseMember(c, voc.copy(parameters = ps) at voc)
      d    <- resolve(voc.name) >>= get
      c    <- currentClass
      r    <- d match {
        case m: Method   => for {
           l <- analyse(VarOrCall(new RelativeName("SELF"), Nil, c) at voc) >>= box
        } yield Access(l, voc) at voc
        case a: Variable if a.isAttribute => for {
           l <- analyse(VarOrCall(new RelativeName("SELF"), Nil, c) at voc) >>= box
        } yield Access(l, voc) at voc
        case _ => just(voc)
      }
    } yield r
  }

  /**
   * Analyses the call of a member of a class (an attribute or a method):
   * 
   *  - The member must be visible from the callers context.
   *  - The call must not provide arguments if it is an attribute or a variable
   *  - The call must provide the right number and types of arguments on method
   *    calls
   */
  def analyseMember(caller: AbsoluteName, voc: VarOrCall, ofBase: Boolean = false): Transform[VarOrCall] = for {
    caller <- getType(caller)
    callee <- currentClass >>= getType 
    isam   <- caller isA callee
    name   <- resolve(voc.name)
    r      <- get(name)
    _      <- require(r.visibility != Visibility.Private   || caller == callee)(
                (name + " is only accessible from within " + callee.name) at name) >>
              require(r.visibility != Visibility.Protected || isam)(
                (name + " is only accessible from within " + callee.name + " or deriving classes") at name) >>
              require(r.isInstanceOf[Variable] || r.isInstanceOf[Method])(
                "variable or method expected" at voc)
    r      <- r match {
      case a: Variable if a.isAttribute => for {
            _ <- throwIf(voc.parameters.size != 0)("attributes can't take arguments" at voc)
        } yield voc.copy(name = name, typed = a.typed, isLValue = true) at voc
      case m: Method    => for {
            _ <- throwIf(voc.parameters.size != m.parameters.size)(
                Error(voc.pos,"wrong number of arguments for method " + voc.name +
                              " (should be " + m.parameters.size + ")"))
            _ <- sequence(voc.parameters.zip(m.parameters).map { case (x,y) => requireType(y.typed)(x) })
        } yield voc.copy(name = name, typed = m.typed, static = ofBase) at voc
      case v: Variable  => for {
            _ <- throwIf(voc.parameters.size != 0)("variables can't take arguments" at voc)
        } yield voc.copy(name = name, typed = v.typed, isLValue = true) at voc       
    } } yield r    

  /**
   * Boxes an expression if necessary. (As declared in [[de.martinring.oopsc.syntactic.Class.box]])
   */
  def box(expr: Expression): Transform[Expression] = for {
    t2   <- getType(expr.typed)
  } yield Class.box.get(t2).map(t => Box(expr, t.name) at expr)
               .getOrElse(if (expr.isLValue) DeRef(expr, expr.typed) at expr else expr)

  /**
   * Unboxes an expression if necessary. (As declared in [[de.martinring.oopsc.syntactic.Class.unBox]])
   */
  def unBox(expr: Expression): Transform[Expression] = for {
    t2   <- getType(expr.typed)
    r    <- if (expr.isLValue) unBox(DeRef(expr, expr.typed) at expr)
            else just(Class.unBox.get(t2).map(t => UnBox(expr, t.name) at expr).getOrElse(expr) at expr)
  } yield r

  /**
   * Checks if the expression has the specified type.
   */
  def requireType(t1: Class)(expr: Expression): Transform[Expression] = for {
    t2 <- getType(expr.typed)
    _  <- t2 isA t1 >>= (x => require(x)("type mismatch\n    expected: %s\n    found: %s".format(t1.name, expr.typed) at expr))
  } yield expr
  
  def requireType(t: Name)(expr: Expression): Transform[Expression] = for {
    t1    <- getType(t)
    expr  <- requireType(t1)(expr)
  } yield expr

  def isBase(e: Expression): Boolean = e match {
    case voc: VarOrCall if voc.name.relative == "BASE" => true
    case _ => false
  }
  
  def isSelf(e: Expression): Boolean = e match {
    case voc: VarOrCall if voc.name.relative == "SELF" => true
    case _ => false
  }
  
  implicit def extension_isA(a: Class): { def isA(b: Class): Transform[Boolean] } = new {
    /**
     * Checks if class `b` is a subclass of class `a` or they are the same.
     */
    def isA(b: Class): Transform[Boolean] =
      if (a.name == b.name) just(true)
      else if (a.name == Class.nullType.name) b isA Class.objectClass
      else if (a.name != Class.objectClass.name && a.baseType.isDefined) getType(a.baseType.get) >>= (a => a isA b)
      else just(false)
  }
}