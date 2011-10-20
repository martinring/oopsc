package de.martinring.oopsc.assembler

import de.martinring.oopsc.ast._
import de.martinring.oopsc.Assembler._
import de.martinring.oopsc._
import de.martinring.oopsc.Transform._

/*
 * Code generation
 */
object  Code {  
  val stack = "_stack"
  val heap = "_heap"
  val end = "_end"

  def generate(element: Element): Transform[Instr] = element match {
    case Program(main) => for {
      mainClass <- generate(main)
      main      <- generate(Access(New("Main"), "main"))      
    } yield Instructions("Program")(
      R1 << 1       || "R1 is allways 1",
      R2 << stack   || "R2 points to stack",
      R4 << heap    || "R4 points to next free position on Heap",
      main,
      R0 << end     || "exit program",
      mainClass,
      Label(stack)  || "Start of stack",
      Instruction("DAT", App.stackSize, 0),
      Label(heap)   || "Start of heap",
      Instruction("DAT", App.heapSize, 0),
      Label(end)    || "End of program")

    case c: Class => for {
      _       <- enter(c)
      methods <- sequence(c.methods map generate)
      _       <- leave
    } yield Instructions("CLASS " + c.name)(methods :_*)

    case m: Method => for {
      _     <- enter(m)
      c     <- currentType
      val l = c + "_" + m.name
      body  <- sequence(m.body map generate)     
      _     <- leave
    } yield Instructions("METHOD " + m.name)(      
      label(l),
      R2 <<+ R1,
      ~R2 << R3 || "Save old stack frame",
      R3 << R2  || "Current position on stack is new frame",
      ( if (!m.variables.isEmpty) Instructions("Create space for local variables")(
        R5 << m.variables.size,
        R2 <<+ R5)
        else No ),
      Instructions("BODY")(body :_*),
      R5 << m.variables.size + 3,
      R2 <<- R5  || "Correct stack",
      R3 <<- R1 ,
      R5 << ~R3 || "Get return address",
      R3 <<+ R1 ,
      R3 << ~R3 || "Get old stack frame",
      R0 << R5  || "return"
    )
    
    case Read(operand) => for {
        newInt  <- generate(New("Integer"))
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
      
    case While(condition, body) => for {
      condition <- generate(condition)
      body <- sequence(body map generate)
      whileLabel <- nextLabel
      endLabel <- nextLabel
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
      
    case If(condition, body) => for {
      condition <- generate(condition)
      body <- sequence(body map generate)
      endLabel <- nextLabel
    } yield Instructions("IF")(
      Instructions("CONDITION")(
        condition,
        R5 << ~R2 || "Get condition from stack",
        R2 <<- R1,
        R5 << (R5 === O) || "if 0 then",
        Instruction("JPC", R5, endLabel) || "jump to END IF"),
      Instructions("THEN")(body :_*),
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

    case Unary(operator, operand, typed) => for {      
      operand <- generate(operand)     
    } yield Instructions("UNARY " + operator)(
      operand,      
      R5 << ~R2,
      operator match {
        case "-" => Instructions("")(
          R6 << 0,
          R6 <<- R5,
          ~R2 << R6
        )
      })
    
    case Binary(operator, left, right, typed) => for {
      left <- generate(left)
      right <- generate(right)
    } yield Instructions("BINARY " + operator)(
      left,
      right,
      R5 << ~R2,
      R2 <<- R1,
      R6 << ~R2,
      operator match {
        case "+"   => R6 <<+ R5
        case "-"   => R6 <<- R5
        case "*"   => R6 <<* R5
        case "/"   => R6 <</ R5
        case "MOD" => R6 <<% R5
        case ">"   => Instructions("")(
          R6 <<- R5,
          R6 << (R6 > O)
        )
        case ">="  => Instructions("")(
          R6 <<- R5,
          R6 << (R6 < O),        
          Instruction("XOR", R6, R1)
        )
        case "<"   => Instructions("")(
          R6 <<- R5,
          R6 << (R6 < O)
        )
        case "<="  => Instructions("")(
          R6 <<- R5,
          R6 << (R6 > O),
          Instruction("XOR", R6, R1)
        )
        case "="   => Instructions("")(
          R6 <<- R5,
          R6 << (R6 === O) 
        )
        case "#"   => Instructions("")(
          R6 <<- R5,
          R6 << (R6 === O),
          Instruction("XOR", R6, R1)
        )
      },
      ~R2 << R6
    ) 
          
    case Literal(value, typed) => success{
      Instructions(value + ": " + typed.name)(
        R5 << value,
        R2 <<+ R1,
        ~R2 << R5
      )}
    
    case New(typed) => for {
      clazz <- resolveClass(typed)
    } yield Instructions("NEW " + typed.name)(
        R2 <<+ R1,
        ~R2 << R4   || "Put reference to new object on stack",
        R5 << clazz.size.get,
        R4 <<+ R5     || "Inc heap"
      )
    
    case Access(left, right, typed, lvalue) => for {
      l     <- generate(left)
      t     <- resolveClass(left.typed)      
      r     <- generateMember(t, right)
    } yield Instructions("ACCESS")(l,r)
      
    case Name(name, typed, lvalue) => for {      
      decl   <- resolve(name)
    } yield decl match {
        case Variable(name, typed, offset) => Instructions("REF TO VARIABLE " + name)(
            R5 << offset,
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
      R6 <<+ R7        || "Determine location in new object",
      ~R6 << R5      || "Write value to object")

    case UnBox(expr, typed) => for {
      expr <- generate(expr)
    } yield Instructions("UNBOX")(
      expr,
      R5 << ~R2  || "Get reference to object from stack",
      R6 << Class.headerSize,
      R5 <<+ R6    || "Determine address of value",
      R5 << ~R5  || "Read value",
      ~R2 << R5    || "Write to stack")

    case DeRef(expr, typed) => for {
      expr <- generate(expr)
    } yield Instructions("DEREF")(
      expr,
      R5 << ~R2,
      R5 << ~R5,
      ~R2 << R5)           
  }
  
  def generateMember(t: Class, expr: Expression) = for {
    returnLabel <- nextLabel
  } yield expr match {
    case Name(name, typed, lvalue) => 
      val decl = t.members.find(_.name == name).get
      decl match {
        case Attribute(name, typed, offset) => 
          Instructions("REF TO ATTRIBUTE " + name)(
            R5 << ~R2,
            R6 << offset,
            R5 <<+ R6,
            ~R2 << R5)
        case Method(name, variables, body) => 
          Instructions("CALL OF METHOD " + name)(
            R5 << returnLabel,
            R2 <<+ R1,
            ~R2 << R5,            
            R0 << t.name + "_" + name,
            Label(returnLabel))
        }
    }  
}