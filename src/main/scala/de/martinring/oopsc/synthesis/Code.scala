package de.martinring.oopsc.synthesis

import de.martinring.oopsc.App
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.semantic._
import scala.collection.mutable.Map

/**
 * Code generation
 **/
class CodeGenerator extends Assembler {  
  private var stack: Stack = null
  private var stackFrame: R = null
  private var objectStack: Stack = null
    
  private val heap = "_heap"
  private val heapPointer = "_heapPointer"
  private val divisionByZero = "_divisionByZero"
  private val nullPointer = "_nullPointer"
  private val outOfMemory = "_outOfMemory"  
  
  private val positions = Map[String,String]()
  
  private val error: String = "_error"
  
  private def error(label: String) {
    R7 := label
    goto(error)
  }  
  
  def apply(p: Program)(implicit context: Context): List[String] = {
    init()
    generate(p)
    instructions.toList
  }    
  
  /**
   * Generate Code for an [[de.martinring.oopsc.syntactic.Element]].
   */
  private def generate(element: Element)(implicit context: Context): Unit = element match {
    case Program(classes) => {              
      val runtimeError = "_runtimeError"      
      stack = Stack("stack", App.arguments.stackSize)
      stackFrame = 'stackFrame
      objectStack = Stack("objectStack", App.arguments.stackSize)
      generate( Access(New(Root / "Main"), VarOrCall(Root / "Main" / "main", static = true)) )
      goto(end) // "exit program"
      local {
        force(R7)        
        label(error)        
        print(runtimeError)
        print(R7)
        goto(end)
//        
//        label(error)
//        'current := ~R7
//        gotoIf('current === O)(if (App.arguments.debugMode) "_position" else end)
//        write('current)
//        R7 += 1
//        goto(error)
//        if (App.arguments.debugMode) {                      
//          label("_position")
//          write(' '.toInt)
//          write('i'.toInt)
//          write('n'.toInt)
//          write(' '.toInt)
//          R7 := ~"_debugPosition"
//          label("_writePosition")          
//          'current := ~R7
//          gotoIf('current === O)(end)
//          write('current)
//          R7 += 1
//          goto("_writePosition")
//        }
      }
      classes foreach generate
      stack.allocate()
      objectStack.allocate()
      label(heapPointer)
      Instruction("DAT", 1, heapPointer)
      label(heap)   // "Start of heap"
      Instruction("DAT", App.arguments.heapSize, 0)
      label(runtimeError)
      string("\n Runtime error: ")
      label(divisionByZero)
      string("Division by 0")
      label(nullPointer)
      string("Null pointer")
      label(outOfMemory)
      string("Out of memory")
      if (App.arguments.debugMode) {        
        positions("_pos_init") = "initialization"
        label("_debugPosition")
        Instruction("DAT", 1, "_pos_init")
        for((l,text) <- positions) {
          label(l)
          string(text)
        }
      }
      label(end)    // "End of program"
      write('\n'.toInt)
    }

    case c: Class => {      
      label(c.name.label) // Virtual Method Table
      val vmt = context.vmts.getOrElse(c.name.asInstanceOf[AbsoluteName],List())
      vmt.foreach(m => Instruction("DAT",1,m.label))
      c.methods foreach generate      
    }

    case m: Method => {      
      label(m.name.label) // "Method"
      debugInfo("method " + m.name + " called")
      if (App.arguments.debugMode) local {
        positions(m.name.label + "_debug") = m.name.toString
        'debugPosition := "_debugPosition"
        ~'debugPosition := m.name.label + "_debug"
      }
      stack.push(stackFrame)
      stackFrame := objectStack.position
      stackFrame += 1
      for (_ <- m.variables) objectStack.push(0) // initialize variables with null
      m.body foreach generate
    }

    case Read(operand) => local {
      debugInfo("read")
      generate(operand)
      generate(New(Class.intClass.name))
      'newInt := objectStack.pop
      'target := Class.headerSize
      'target += 'newInt
      ~'target := read // "Write character to new Integer"
      ~stack.pop := 'newInt // "Assign"
    }

    case Write(operand) => local {
      debugInfo("write")
      generate(operand)
      write(stack.pop)
    }

    case w@While(condition, body) =>
      debugInfo("while")
      val whileLabel = w.label + "_while"
      val endLabel = w.label + "_endWhile"
      label(whileLabel)
      generate(condition)
      gotoIf(!stack.pop)(endLabel)
      body foreach generate
      goto(whileLabel)
      label(endLabel)

    case f@Forever(body) =>
      debugInfo("forever")
      val forever = f.label + "_forever"
      label(forever)
      body foreach generate
      goto(forever)    
      
    case i@If(condition, body, elseBody) =>
      debugInfo("if")
      val elseLabel = i.label + "_else"
      val endLabel = i.label + "_endIf"
      generate(condition)                   
      gotoIf(!stack.pop)(elseLabel)      
      body foreach generate
      goto(endLabel)
      label(elseLabel)
      elseBody foreach generate
      label(endLabel)    
    
    case Call(call) => generate(call)

    case Assign(left, right) =>
      debugInfo("assign")
      generate(right)
      generate(left)
      local {
        'left := ~stack.pop
        ~'left := objectStack.pop
      }

    case r@Return(Some(expr)) =>
      debugInfo("return " + expr)
      generate(expr)
      local {
        'result := objectStack.top
        stackFrame := stack.pop
        objectStack.position := stackFrame
        objectStack.push('result)
        'returnAddress := stack.pop
        goto('returnAddress) // "return"
      }
    
    case r@Return(None) => local {
      debugInfo("return")
      stackFrame := stack.pop
      objectStack.position := stackFrame
      'returnAddress := stack.pop
      goto('returnAddress) // "return"
    }

    case Unary(operator, operand, typed) => 
      debugInfo("unary " + operator)
      generate(operand)      
      operator match {
        case "-" => local {
          'negation := 0
          'negation -= stack.top
          stack.update('negation)
        }
        case "NOT" =>
          stack.update(!stack.top)
      }    
      
    /** shortcut operators */
    case b@Binary(operator @ ("THEN" | "ELSE"), left, right, typed) => 
      debugInfo("binary " + operator)
      val skip = b.label + "_skip"
      generate(left)      
      operator match {
        case "THEN" =>            
          gotoIf(!stack.top)(skip)
          stack.position -= 1
        case "ELSE" =>
          gotoIf(stack.top)(skip)
          stack.position -= 1          
      }
      generate(right)
      label(skip)    
    
    case b@Binary(operator, left, right, typed) =>
      debugInfo("binary " + operator)
      generate(left)
      generate(right)
      local {
        'right := stack.pop
        'left := stack.top
        operator match {
          case "+"   => 'left += 'right
          case "-"   => 'left -= 'right
          case "*"   => 'left *= 'right
          case "/"   => 
            val skip = b.label + "_skip"
            gotoIf('right)(skip) // right is not 0 we skip the error
            error(divisionByZero)
            label(skip)
            'left /= 'right
          case "MOD" => 'left %= 'right
          case "AND" => 'left &= 'right
          case "OR"  => 'left |= 'right
          case ">"   => 
            'left -= 'right
            'left := ('left > O)        
          case ">="  => 
            'left -= 'right
            'left := ('left < O)
            'left ^= 1 
          case "<"   => 
            'left -= 'right
            'left := ('left < O)        
          case "<="  => 
            'left -= 'right
            'left := ('left > O)
            'left ^= 1
          case "="   => 
            'left -= 'right
            'left := ('left === O)
          case "#"   => 
            'left -= 'right
            'left := ('left === O)
            'left ^= 1
        }
        stack.update('left)
      }

    case Literal(value, typed) => 
      debugInfo("literal " + value + ": " + typed)
      stack.push(value)

    case n@New(typed) => local {
      debugInfo("New " + typed.relative)
      val clazz = context.get(typed.asInstanceOf[AbsoluteName]).asInstanceOf[Class]
      val skip = n.label + "_skipOOM"
      local { // Check if there is enough memory                
        'current := ~heapPointer
        'current += (clazz.size)
        'space := end
        'space -= 'current
        gotoIf('space > O)(skip) // if there is space left, we skip the error 
        error(outOfMemory)
        label(skip)
      }
      'heapPointer := heapPointer      
      'heap := ~'heapPointer // "Get heap pointer"
      ~'heap := clazz.name.label // "Put VMT Address on Heap"
      objectStack.push('heap) // "Put reference to new object on stack"
      if (clazz.attributes.size > 0) local {
        'zero := 0
        for (i <- 0 until clazz.attributes.size) {          
          'heap += 1
          ~'heap := 'zero
        }
      }
      'heap += 1 // "inc heap"
      ~'heapPointer := 'heap // "Update heap pointer"
    }

    case a@Access(left, right) =>
      debugInfo("access " + right.name)
      generate(left)      
      right.parameters foreach generate
      generate(right)
    
    case voc@VarOrCall(name, params, typed, lvalue, static) => local {
      val decl = context.get(name.asInstanceOf[AbsoluteName])      
      decl match {
        case a@Variable(name, typed, offset, true, _) =>
          debugInfo("get attribute "+name)
          'address := objectStack.pop
          'address += offset.get
          stack.push('address)
        case Method(name, params, variables, body, typed, index, _) =>
          val returnLabel = voc.label + "_return"
          if (true) { // TOTO: reenable dynamic calls
            debugInfo("static call of " + name)
            stack.push(returnLabel)
            goto(name.label)
            label(returnLabel)
          } 
          else {            
            debugInfo("dynamic call of " + name)
            objectStack.position -= params.size
            'vmt := ~objectStack.top // "Get VMT"
            objectStack.position += params.size
            'vmt += index.get // Determine address of method pointer
            stack.push(returnLabel)
            'method := ~'vmt // Get address of method
            goto('method) // Goto method
            label(returnLabel)
          }
        case v@Variable(name, typed, offset, false, _) =>
          debugInfo("get variable "+name)
          'address := stackFrame
          'address += offset.get
          stack.push('address)
      }
    }

    case Box(expr, typed) => local {
      debugInfo("box " + expr.toString)
      generate(New(typed))
      generate(expr)
      'value := stack.pop // Take value from stack
      'new := objectStack.top // Get reference to new object (stays on the stack)
      'new += Class.headerSize // Determine location in new object
      ~'new := 'value // Write value to object
    }
    
    case UnBox(expr, typed) => local {
      debugInfo("unbox " + expr.toString)
      generate(expr)
      'object := objectStack.pop // Get reference to object from stack
      'object += Class.headerSize // Determine address of value
      'object := ~'object // Read value
      stack.push('object) // Write to stack
    }

    case d@DeRef(expr, typed) => local {
      debugInfo("deref")
      generate(expr)
      val skip = d.label + "_skipNP"
      'ref := ~stack.pop
      gotoIf('ref)(skip)
      error(nullPointer)
      label(skip)      
      objectStack.push('ref)
    }
  }
    
  private implicit def elementLabel(element: Element): { def label: String } = new {
    def label: String = {
      labelCounter = labelCounter + 1
      element.getClass.getSimpleName + "_" + element.pos.line + "_" + element.pos.column + "_" + labelCounter
    }
  }
}
