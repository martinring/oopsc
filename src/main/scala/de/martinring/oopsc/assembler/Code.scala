package de.martinring.oopsc.assembler

import de.martinring.oopsc.ast._
import de.martinring.oopsc.Assembler._
import de.martinring.oopsc._
import de.martinring.oopsc.Transform._


object  Code {
  val lf = "\n"
  def lines(ls: String*) = ls.mkString(lf)
  def indent(ls: String) = ls.split(lf).map("  " + _).mkString(lf)  
  def section(title: String)(content: String*): String = {
    ";;; " + title + " ;;;\n" + 
    indent(lines(content :_*))
  }
  
  implicit def pimpString(s: String) = new {
    def |> (comment: String) = s + " " + comment
  }

  val stack = "_stack"
  val heap = "_heap"
  val end = "_end"

  def generate(element: Element): Transform[String] = element match {
    case Program(main) => for {
      main <- generate(main)
    } yield lines(
      "; Start-Code: initialize registers",
      ( R(1) := 1     ) |> "R1 is allways 1",
      ( R(2) := stack ) |> "R2 points to stack",
      ( R(4) := heap  ) |> "R4 points to next free position on Heap",
      ( indent(main)  ),
      ( R(0) := end   ) |> "exit program")

    case c: Class => for {
      _       <- enter(c)
      methods <- merge(c.methods map generate)
      _       <- leave
    } yield section("CLASS " + c.name)(methods :_*)      

    case m: Method => for {
      _     <- enter(m)
      val label = "blub" //TODO
      body  <- merge(m.body map generate)     
      _     <- leave
    } yield lines(
      section("METHOD " + m.name)(      
        ( label ),
        ( R(2) += R(1) ),
        ( Address(R(2)) = R(3) ) |> "Save old stack frame",
        ( R(3) := R(2) ) |> "Current position on stack is new frame",
        ( if (!m.variables.isEmpty) lines (
            ( R(5) := m.variables.size ),
            ( R(2) += R(5) ) |> "Create space for local variables"
          ) else "; Method has no variables"),
        lines(body :_*)             
      ),
      ( R(5) := m.variables.size + 3 ),
      ( R(2) -= R(5) ) |> "Correct stack",
      ( R(3) -= R(1) ),
      ( R(5) := Address(R(3))) |> "Get return address",
      ( R(3) += R(1) ),
      ( R(3) := Address(R(3))) |> "Get old stack frame",
      ( R(0) := R(5) ) |> "return"
    )
    
    case Read(operand) => for {
        newInt  <- generate(New("Integer"))
        operand <- generate(operand)
    } yield lines(
      operand,
      newInt,
      section("READ")(
        ( R(5) := Address(R(2)) ),
        ( R(6) := Class.headerSize ),
        ( R(5) += R(6) ),
        ( "SYS 0, 6" ),
        ( Address(R(5)) = R(6) )  |> "Write character to new Integer",
        ( R(5) := Address(R(2)) )   |> "Get new Int from stack",
        ( R(2) -= R(1) ),
        ( R(6) := Address(R(2)) ) |> "Take target from stack",
        ( R(2) -= R(1) ),
        ( Address(R(6)) = R(5) )  |> "Assign"
      ))
      
    case Write(operand) => for {
      operand <- generate(operand)
    } yield lines(
      operand,
      section("WRITE")(
        R(5) := Address(R(2)),
        R(2) -= R(1),
        "SYS 1, 5"
      )
    )
      
    case While(condition, body) => for {
      condition <- generate(condition)
      body <- merge(body map generate)
      val whileLabel = "TODO: LABEL" // TODO!
      val endLabel = "TODO: LABEL" // TODO!      
    } yield lines(
      section("WHILE")(
        whileLabel,
        condition,
        ( R(5) := Address(R(2)) ) |> "Get condition from stack",
        ( R(2) -= R(1) ),
        ( "ISZ R5, R5;" ) |> "if 0 then",
        ( "JPC R5, " + endLabel +";" ) |> "leave loop"      
      ),
      section("DO")(body :_*),
      "MRI R0, " + whileLabel,
      endLabel)
      
    case If(condition, body) => for {
      condition <- generate(condition)
      body <- merge(body map generate)
      val endLabel = "TODO: LABEL" // TODO! 
    } yield lines(
      section("IF")(
        condition,
        ( R(5) := Address(R(2)) ) |> "Get condition from stack",
        ( R(2) -= R(1) ),
        ( "ISZ R5, R5;" ) |> "if 0 then",
        ( "JPC R5, " + endLabel +";" ) |> "jump to END IF"),
      section("THEN")(body :_*),
      endLabel        
      )
      
    case Call(call) => for {
      call <- generate(call)
    } yield section("CALL")(call)
      
    case Assign(left, right) => for {
      left <- generate(left)
      right <- generate(right)
    } yield section("ASSIGNMENT")(
      ( R(5) := Address(R(2)) ) |> "Take right value from stack",
      ( R(2) -= R(1) ),
      ( R(6) := Address(R(2)) ) |> "Take ref to left val from stack",
      ( R(2) -= R(1) ),
      ( Address(R(6)) = R(5) )  |> "Assign"
    )    

    case Unary(operator, operand, typed) => for {      
      operand <- generate(operand)     
    } yield lines(
      operand,
      section("UNARY " + operator)(
        R(5) := Address(R(2)),
        operator match {
          case "-" => lines(
            R(6) := 0,
            R(6) -= R(5),
            Address(R(2)) = R(6)
          )
        }
      )
    )
    
    case Binary(operator, left, right, typed) => for {
      left <- generate(left)
      right <- generate(right)
    } yield section("BINARY " + operator)(
      R(5) := Address(R(2)),
      R(2) -= R(1),
      R(6) := Address(R(2)),
      operator match {
        case "+"   => R(6) += R(5)
        case "-"   => R(6) -= R(5)
        case "*"   => R(6) *= R(5)
        case "/"   => R(6) /= R(5)
        case "MOD" => R(6) %= R(5)
        case ">"   => lines(
          R(6) -= R(5),
          "ISP R6, R6"
        )
        case ">="  => lines(
          R(6) -= R(5),
          "ISN R6, R6",
          "XOR R6, R1"
        )
        case "<"   => lines(
          R(6) -= R(5),
          "ISN R6, R6"
        )
        case "<="  => lines(
          R(6) -= R(5),
          "ISP R6, R6",
          "XOR R6, R1"
        )
        case "="   => lines(
          R(6) -= R(5),
          "ISZ R6, R6"
        )
        case "#"   => lines(
          R(6) -= R(5),
          "ISZ R6, R6",
          "XOR R6, R1"
        )
      },
      Address(R(2)) = R(6)
    ) 
          
    case Literal(value, typed) => success{
      section(value + ": " + typed.name)(
        R(5) := value,
        R(2) += R(1),
        Address(R(2)) = R(5)
      )
    }
    
    case New(typed) => for {
      clazz <- resolveClass(typed)
    } yield section("NEW " + typed.name)(
        R(2) += R(1),
        (Address(R(2)) = R(4)) |> "Put reference to new object on stack",
        R(5) := 0 // TODO: Object SIze
      )
    
    case Access(left, right, typed) => for {
      left  <- generate(left)
      t     <- resolveClass(left.typed)
      _     <- enterType(t)
      right <- generate(right)
      _     <- leave
    } yield lines(
      left,
      right
    )
    
    case Name(name, typed) => for {
      decl   <- resolve(name)
    } yield decl match {
      case Variable(name, typed, offset) => section("REF TO VARIABLE " + name)(
          R(5) := offset,
          R(5) += R(3),
          R(2) += R(1),
          Address(R(2)) = R(5)
        )
      case Attribute(name, typed, offset) => section("REF TO ATTRIBUTE " + name)(
          R(5) := Address(R(2)),
          R(6) := offset,
          R(5) += R(6),
          Address(R(2)) = R(5)
        )
      case Method(name, variables, body) => 
        val returnLabel = "returnLabel" //TODO
        section("CALL OF METHOD " + name)(
          R(5) := returnLabel,
          R(2) += R(1),
          Address(R(2)) = R(5),
          R(0) := "label", //TODO
          returnLabel + ":"
        )
    }
      
    case Box(expr, typed) => for {
      newType <- generate(New(typed))
      expr <- generate(expr)
    } yield section("BOX")(
      newType,
      expr,
      ( R(5) := Address(R(2)) ) |> "Take value from stack",
      ( R(2) -= R(1) ),
      ( R(6) := Address(R(2)) ) |> "Get reference to new object (stays on the stack)",
      ( R(7) := Class.headerSize ),
      ( R(6) += R(7) ) |> "Determine location in new object",
      ( Address(R(6)) = R(5) ) |> "Write value to object"
    )

    case UnBox(expr, typed) => for {
      expr <- generate(expr)
    } yield section("UNBOX")(
      expr,
      ( R(5) := Address(R(2)) ) |> "Get reference to object from stack",
      ( R(6) := Class.headerSize ),
      ( R(5) += R(6) ) |> "Determine address of value",
      ( R(5) := Address(R(5)) ) |> "Read value",
      ( Address(R(2)) = R(5) ) |> "Write to stack"
    )

    case DeRef(expr, typed) => for {
      expr <- generate(expr)
    } yield section("DEREF")(
      expr,
      R(5) := Address(R(2)),
      R(5) := Address(R(5)),
      Address(R(2)) = R(5)
    )   
  }
}