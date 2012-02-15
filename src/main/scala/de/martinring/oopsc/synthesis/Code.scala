package de.martinring.oopsc.synthesis

import de.martinring.oopsc.App
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.synthesis.Assembler._
import de.martinring.oopsc.semantic._
import de.martinring.oopsc.semantic.Transform._
import de.martinring.util.Success

/**
 * Code generation
 **/
object  Code {
  var labelCounter = 0
  val stack = "_stack"
  val heap = "_heap"
  val end = "_end"
  val error = "_error"
  val divisionByZero = "_divisionByZero"
  val nullPointer = "_nullPointer"
  val outOfMemory = "_outOfMemory"    
  
  /**
   * Generate Code for an [[de.martinring.oopsc.syntactic.Element]].
   */
  def generate(element: Element): Transform[Instr] = element match {
    case Program(classes) => for {
      classes   <- sequence(classes map (x => enter(x.name)(generate(x))))
      main      <- generate(Access(New(AbsoluteName(List("Main"))), VarOrCall(AbsoluteName(List("Main", "main")))))
    } yield Instructions("Program")(
      R1 << 1       || "R1 is allways 1",
      R2 << stack   || "R2 points to stack",
      R4 << heap    || "R4 points to next free position on Heap",
      main,
      R0 << end     || "exit program",
      Label(error),
      R5 << ~R7,
      R6 << (R5 === O),
      Instruction("JPC", R6, end),
      Instruction("SYS", 1, 5),
      R7 <<+ R1,      
      R0 << error,
      Label(divisionByZero),
      string("Runtime error: Division by 0"),
      Label(nullPointer),
      string("Runtime error: Null pointer"),
      Label(outOfMemory),
      string("Runtime error: Out of memory"),
      Instructions("CLASSES")(classes :_*),            
      Label(stack)  || "Start of stack",
      Instruction("DAT", App.arguments.stackSize, 0),
      Label(heap)   || "Start of heap",
      Instruction("DAT", App.arguments.heapSize, 0),      
      Label(end)    || "End of program")    

    case c: Class => for {
      methods <- sequence(c.methods map (x => enter(x.name)(generate(x))))
      vmt     <- getVMT(c.name.asInstanceOf[AbsoluteName])
    } yield Instructions("CLASS " + c.name)(
      Label(c.name.label),
      Instructions("VMT OF CLASS " + c.name)(vmt map (m => Instruction("DAT", 1, m.label)) :_*),
      Instructions("METHODS OF CLASS " + c.name)(methods :_*))

    case m: Method => for {
      c     <- currentClass
      l     <- currentMethod map (_.get.path.mkString("_"))
      body  <- sequence(m.body map generate)
    } yield Instructions("METHOD " + m.name)(
      Label(l),
      R2  <<+ R1,
      ~R2 <<  R3   || "Save old stack frame",
      R3  <<  R2   || "Current position on stack is new frame",
      ( if (!m.variables.isEmpty) Instructions("Create space for local variables")(
        R5 << m.variables.size,
        R2 <<+ R5)
        else No ),
      Instructions("BODY")(body :_*) )

    case Read(operand) => for {
        newInt  <- generate(New(Class.intClass.name))
        operand <- generate(operand)
    } yield Instructions("READ")(
      operand,
      newInt,
      R5 << ~R2 ,
      R6 << Class.headerSize ,
      R5 <<+ R6 ,
      Instruction("SYS", 0, 6),
      ~R5 << R6   || "Write character to new Integer",
      R5 << ~R2    || "Get new Int from stack",
      R2 <<- R1 ,
      R6 << ~R2  || "Take target from stack",
      R2 <<- R1 ,
      ~R6 << R5   || "Assign"
    )

    case Write(operand) => for {
      operand <- generate(operand)
    } yield Instructions("WRITE")(
      operand,
      R5 << ~R2,
      R2 <<- R1,
      Instruction("SYS", 1, 5)
    )

    case w@While(condition, body) => for {
      condition <- generate(condition)
      body <- sequence(body map generate)
      whileLabel <- label(w) map (_ + "_while")
      endLabel <- label(w) map (_ + "_endWhile")
    } yield Instructions("WHILE")(
      Instructions("CONDITION")(
        Label(whileLabel),
        condition,
        R5 << ~R2  || "Get condition from stack",
        R2 <<- R1,
        R5 << (R5 === O) || "if 0 then",
        Instruction("JPC", R5, endLabel) || "leave loop"),
      Instructions("BODY")(body :_*),
      R0 << whileLabel,
      Label(endLabel))

    case f@Forever(body) => for {      
      body <- sequence(body map generate)
      forever <- label(f)      
    } yield Instructions("FOREVER")(
      Label(forever),
      Instructions("BODY")(body :_*),
      R0 << forever)
      
    case i@If(condition, body, elseBody) => for {
      condition <- generate(condition)
      body <- sequence(body map generate)
      elseBody <- sequence(elseBody map generate)
      elseLabel <- label(i) map (_ + "_else")
      endLabel <- label(i) map (_ + "_endIf")
    } yield Instructions("IF")(
      Instructions("CONDITION")(
        condition,
        R5 << ~R2 || "Get condition from stack",
        R2 <<- R1,
        R5 << (R5 === O) || "if 0 then",
        Instruction("JPC", R5, elseLabel) || "jump to END IF"),
      Instructions("THEN")(
        Instructions("")(body :_*),
        R0 << endLabel),
      Instructions("ELSE")(
        Label(elseLabel),
        Instructions("ELSE BODY")(elseBody :_*)),
      Label(endLabel))


    case Call(call) => for {
      call <- generate(call)
    } yield Instructions("CALL")(call)

    case Assign(left, right) => for {
      left <- generate(left)
      right <- generate(right)
    } yield Instructions("ASSIGNMENT")(
      left,
      right,
      R5 << ~R2  || "Take right value from stack",
      R2 <<- R1,
      R6 << ~R2  || "Take ref to left val from stack",
      R2 <<- R1,
      ~R6 << R5  || "Assign")

    case r@Return(Some(expr)) => for {
      m    <- currentMethod map (_.getOrElse(sys.error("not in a method"))) >>= getMethod
      expr <- generate(expr)
    } yield Instructions("RETURN")(
      expr,
      R6 << ~R2  || "Take ref to return value from stack",
      R5 << (m.variables.size + 3 + m.parameters.size),
      R2 <<- R5  || "Correct stack",
      ~R2 << R6  || "Write ref to return value to stack",
      R3 <<- R1,
      R5 << ~R3  || "Get return address",
      R3 <<+ R1,
      R3 << ~R3  || "Get old stack frame",
      R0 << R5   || "return"
    )

    case r@Return(None) => for {
      m  <- currentMethod map (_.getOrElse(sys.error("not in a method"))) >>= getMethod
    } yield Instructions("RETURN")(
      R5 << (m.variables.size + 3 + m.parameters.size),
      R2 <<- R5  || "Correct stack",
      R3 <<- R1,
      R5 << ~R3  || "Get return address",
      R3 <<+ R1,
      R3 << ~R3  || "Get old stack frame",
      R0 << R5   || "return"
    )

    case Unary(operator, operand, typed) => for {
      operand <- generate(operand)
    } yield Instructions("UNARY " + operator)(
      operand,
      R5 << ~R2,
      operator match {
        case "-" => Instructions("-")(
          R6 << 0,
          R6 <<- R5,
          ~R2 << R6
        )
        case "NOT" => Instructions("NOT")(
          R5 << (R5 === O),
          ~R2 << R5 )})           
      
    /** shortcut operators */
    case b@Binary(operator @ ("THEN" | "ELSE"), left, right, typed) => for {
      left  <- generate(left)
      right <- generate(right)
      skip  <- label(b)
    } yield Instructions("BINARY " + operator)(
      left,
      R5 << ~R2,
      operator match {
        case "THEN" => Instructions("SHORTCUT AND")(
          R5 << (R5 === O),
          Instruction("JPC",R5,skip),
          R2 <<- R1
        )          
        case "ELSE" => Instructions("SHORTCUT THEN")(          
          Instruction("JPC",R5,skip),
          R2 <<- R1
        )
      },
      right,
      Label(skip)
    )
    
    case b@Binary(operator, left, right, typed) => for {
      left <- generate(left)
      right <- generate(right)      
      skip <- label(b)
    } yield Instructions("BINARY " + operator)(
      left,
      right,
      R6 << ~R2,
      R2 <<- R1,
      R5 << ~R2,
      operator match {
        case "+"   => R5 <<+ R6
        case "-"   => R5 <<- R6
        case "*"   => R5 <<* R6
        case "/"   => Instructions("")(                        
            Instruction("JPC", R6, skip),            
            R7 << divisionByZero,
            R0 << error,
            Label(skip),
            R5 <</ R6
          ) 
        case "MOD" => R5 <<% R6
        case "AND" => R5 <<& R6
        case "OR"  => R5 <<| R6
        case ">"   => Instructions("")(
          R5 <<- R6,
          R5 << (R5 > O)
        )
        case ">="  => Instructions("")(
          R5 <<- R6,
          R5 << (R5 < O),
          R5 <<^ R1
        )
        case "<"   => Instructions("")(
          R5 <<- R6,
          R5 << (R5 < O)
        )
        case "<="  => Instructions("")(
          R5 <<- R6,
          R5 << (R5 > O),
          R5 <<^ R1
        )
        case "="   => Instructions("")(
          R5 <<- R6,
          R5 << (R5 === O)
        )
        case "#"   => Instructions("")(
          R5 <<- R6,
          R5 << (R5 === O),
          R5 <<^ R1
        )
      },
      ~R2 << R5
    )

    case Literal(value, typed) => transform( c => Success{
      (c, Instructions(value + ": " + typed)(
        R5 << value,
        R2 <<+ R1,
        ~R2 << R5
      ))})

    case n@New(typed) => for {
      clazz <- getType(typed)
    } yield Instructions("NEW " + typed)(        
        R5 << end,
        R6 << clazz.size,
        R6 <<+ R4,
        R5 <<- R6,
        R5 << (R5 < O),
        R7 << outOfMemory,
        Instruction("JPC",R5,error),
        R2 <<+ R1,
        R5 << clazz.name.label || "Get reference to VMT",        
        ~R4 << R5     || "Put VMT Address on Heap",
        ~R2 << R4     || "Put reference to new object on stack",        
        R5 << clazz.size || "Class size",
        R4 <<+ R5     || "Inc heap"
      )

    case a@Access(left, right) => for {
      l     <- generate(left)
      ps    <- sequence(right.parameters map generate)
      r     <- enter(left.typed)(generate(right))
    } yield Instructions("ACCESS")(l,Instructions("Parameters")(ps :_*),r)

    case voc@VarOrCall(name, params, typed, lvalue, static) => for {
      returnLabel <- label(voc)
      t           <- currentClass
      decl        <- get(name)     
    } yield decl match {
        case a@Variable(name, typed, offset, true, _) =>
          Instructions("REF TO ATTRIBUTE " + name)(
            R5 << ~R2,
            R6 << offset.get,
            R5 <<+ R6,
            ~R2 << R5)
        case Method(name, params, variables, body, typed, index, _) => if (static) {
          Instructions("STATIC CALL OF METHOD " + name)(
            R5 << returnLabel,
            R2 <<+ R1,
            ~R2 << R5,
            R0 << name.asInstanceOf[AbsoluteName].path.mkString("_"),
            Label(returnLabel) )
        } else {
          Instructions("DYNAMIC CALL OF METHOD " + name)(
            R6 << params.length,
            R2 <<- R6,
            R5 << ~R2 || "Get VMT",                        
            R2 <<+ R6,            
            R2 <<+ R1,
            R6 << returnLabel,
            ~R2 << R6,            
            R5 << ~R5,
            R6 << index.get,
            R5 <<+ R6,            
            R5 << ~R5,
            R0 << R5,
            Label(returnLabel) )
        }
        case v@Variable(name, typed, offset, false, _) =>
          Instructions("REF TO VARIABLE " + name)(
            R5 << offset.get,
            R5 <<+ R3,
            R2 <<+ R1,
            ~R2 << R5) 
         }

    case Box(expr, typed) => for {      
      newType <- generate(New(typed))
      expr    <- generate(expr)
    } yield Instructions("BOX")(
      newType,
      expr,
      R5 << ~R2      || "Take value from stack",
      R2 <<- R1,
      R6 << ~R2      || "Get reference to new object (stays on the stack)",
      R7 << Class.headerSize,
      R6 <<+ R7      || "Determine location in new object",
      ~R6 << R5      || "Write value to object")

    case UnBox(expr, typed) => for {
      expr <- generate(expr)
    } yield Instructions("UNBOX")(
      expr,
      R5 << ~R2      || "Get reference to object from stack",
      R6 << Class.headerSize,
      R5 <<+ R6      || "Determine address of value",
      R5 << ~R5      || "Read value",
      ~R2 << R5      || "Write to stack")

    case d@DeRef(expr, typed) => for {
      expr <- generate(expr)
      skip <- label(d).map(_ + "_skipNP")
    } yield Instructions("DEREF")(
      expr,
      R5 << ~R2,
      R5 << ~R5,
      Instruction("JPC",R5,skip),
      R7 << nullPointer,
      R0 << error,
      Label(skip),
      ~R2 << R5)
  }

  def label(element: Element): Transform[String] = {
    labelCounter += 1
    for {
      p <- path    
    } yield p.mkString("_") + "_" + element.pos.line + "_" + element.pos.column + "_" + labelCounter
  }
  
  def string(s: String): Instructions = {
    val is = ("\n"+s+"\n").toList.map { case x: Char => Instruction("DAT",1,x.toInt) } ++
             List(Instruction("DAT",1,0))
    Instructions(s)(is :_* )
  }
}
