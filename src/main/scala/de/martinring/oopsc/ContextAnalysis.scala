/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.martinring.oopsc

import de.martinring.oopsc.ast._
import de.martinring.oopsc.ast.Class._
import de.martinring.oopsc.Transform._
import de.martinring.util.Failable.pimpOption
import de.martinring.util._


/*
 * Object for the contextual analysis. Utilizes the @see Transform monad.
 */
object ContextAnalysis {
  def analyse(p: Program): Transform[Program] = for {
    _       <- bind(predefined) >> bind(p.classes)
    classes <- sequence((p.classes ++ predefinedClasses) map (c => enter(c.name.relative)(signature(c))))
    classes <- sequence(classes map (c => enter(c.name.relative)(analyse(c))))
    main    <- getType(Root / "Main") ! ("Missing main class" at p)
    entry   <- getMethod(Root / "Main" / "main") ! ("Missing main method" at main)
    _       <- throwIf(!entry.parameters.isEmpty)("Main method must not take parameters" at entry.parameters.head) >>
               throwIf(entry.typed != voidType.name)("Main method must not return anything" at entry)
  } yield Program(classes) at p

  def signature(c: Class, actual: Class = null): Transform[Class] = c.name match {
    case r: AbsoluteName => just(c)
    case i: RelativeName => for {
      _        <- require(c != actual)("Cyclic inheritance" at actual)
      baseName <- resolveClassName(c.baseType getOrElse sys.error(c.name + " has no base type"))
      base     <- getType(baseName)
      base     <- signature(base, if (actual == null) c else actual)
      name     <- resolveClassName(c.name)
      _        <- bind(c.attributes) >> bind(c.methods) >>
                  getVMT(baseName) >>= setVMT(name)
      val size  = base.size + c.attributes.size
      attrs    <- sequence(c.attributes.zip(base.size until size) map analyse)
      methods  <- sequence(c.methods map (m => enter(m.name.relative)(signature(m))))
      result   <- update(Class(name, attrs, methods, Some(base.name), size) at c)
    } yield result
  }

  def insert(c: List[Method], m: Method, index: Int = 0): (Int, List[Method]) = c match {
    case Nil  => (index, List(m))
    case h::t => if (h.name.relative == m.name.relative) (index, m::t)
                 else {
                   val (i, t2) = insert(t, m, index + 1)
                   (i, h::t2)
                 }
  }

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
        _ <- require(o.typed == typed)("Method must have the same return type ("+o.typed+") as the overridden method" at m) >>
             require(o.parameters.size == params.size)("Method must have the same number of parameters ("+o.parameters.size+") as the overridden method" at m) >>
             sequence(o.parameters.zip(params) map { case (a,b) => require(a.typed == b.typed)("Overriding method has incompatible parameter type (required "+a.typed+")" at b) }) >>
             setVMT(self)(vmt.updated(x,name))
      } yield x
    }
    result <- update(m.copy(name = name, parameters = params, typed = typed, index = Some(index)) at m)
  } yield result

  def analyse(c: Class): Transform[Class] = for {
    methods <- sequence(c.methods map (m => enter(m.name.relative)(analyse(m))))
    result  <- update(c.copy(methods = methods) at c)
  } yield result

  def analyse(v: (Variable, Int)): Transform[Variable] = v match { case (v,i) => for {
    name   <- resolve(v.name)
    typed  <- resolveClassName(v.typed)
    result <- update(v.copy(name = name, offset = Some(i), typed = typed) at v)
  } yield result }

  def analyse(m: Method): Transform[Method] = for {
    self       <- currentClass
    base       <- getType(self) map (_.baseType getOrElse sys.error("no base"))
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
      t     <- getType(left.typed)
      right <- analyse(a.right) >>= box >>= requireType(t)
    } yield Assign(left, right) at a

    case r@Return(Some(expr)) => for {
      m     <- currentMethod.map(_.getOrElse(sys.error("not in a method"))) >>= getMethod
      expr  <- analyse(expr) >>= box >>= requireType(m.typed)
    } yield Return(Some(expr)) at r

    case r@Return(None) => for {
      m     <- currentMethod.map(_.getOrElse(sys.error("not in a method"))) >>= getMethod
      expr  <- throwIf(m.typed != voidType.name)(Error(r.pos, "expected " + m.typed))
    } yield r
  }

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

      case "AND" | "OR" => for {
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
      right <- enter(left.typed)(analyseMember(a.right.copy(parameters = ps) at a.right, isBase(a.left)))
    } yield Access(left, right) at a

    case voc: VarOrCall => for {
      ps   <- sequence(voc.parameters map (analyse(_) >>= box))
      voc  <- analyseMember(voc.copy(parameters = ps) at voc)
      d    <- resolve(voc.name) >>= get
      c    <- currentClass
      r    <- d match {
        case m: Method   => for {
           l <- analyse(VarOrCall(new RelativeName("SELF"), Nil, c) at voc) >>= box
        } yield Access(l, voc) at voc
        case a: Variable if a.isAttribute => for {
           l <- analyse(VarOrCall(new RelativeName("SELF"), Nil, c) at voc) >>= box
        } yield Access(l, voc) at voc
        case _ => transform(c => Success((c,voc)))
      }
    } yield r
  }

  def analyseMember(voc: VarOrCall, ofBase: Boolean = false): Transform[VarOrCall] = for {
    name  <- resolve(voc.name)
    r     <- get(name) >>= ( _ match {
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
        } yield voc.copy(name = name, typed = v.typed, isLValue = true) at voc } )
  } yield r

  def box(expr: Expression): Transform[Expression] = for {
    t2   <- getType(expr.typed)
  } yield Class.box.get(t2).map(t => Box(expr, t.name) at expr)
               .getOrElse(if (expr.isLValue) DeRef(expr, expr.typed) at expr else expr)

  def unBox(expr: Expression): Transform[Expression] = for {
    t2   <- getType(expr.typed)
    r    <- if (expr.isLValue) unBox(DeRef(expr, expr.typed) at expr)
            else just(Class.unBox.get(t2).map(t => UnBox(expr, t.name) at expr).getOrElse(expr) at expr)
  } yield r

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
    def isA(b: Class): Transform[Boolean] =
      if (a.name == b.name) just(true)
      else if (a.name == Class.nullType.name) b isA Class.objectClass
      else if (a.name != Class.objectClass.name && a.baseType.isDefined) getType(a.baseType.get) >>= (a => a isA b)
      else just(false)
  }
}