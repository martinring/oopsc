package de.martinring.oopsc.synthesis

import de.martinring.oopsc.App
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.semantic._

/**
 * Code generation
 **/
object Code extends Assembler {
  val stack         = Stack("stack", App.arguments.stackSize)
  val stackFrame: R = 'stackFrame
  val objectStack   = Stack("objectStack", App.arguments.stackSize)  
    
  val heap = "_heap"
  val heapPointer = "_heapPointer"
  val divisionByZero = "_divisionByZero"
  val nullPointer = "_nullPointer"
  val outOfMemory = "_outOfMemory"  
  
  /**
   * Generate Code for an [[de.martinring.oopsc.syntactic.Element]].
   */
  def generate(element: Element)(implicit context: Context): Unit = element match {
    case Program(classes) => {                  
      generate( Access(New(Root / "Main"), VarOrCall(Root / "Main" / "main")) )
      goto(end) // "exit program"
      local {
        Label(error)  
        force(R7)
        'current := ~R7
        gotoIf('current === O)(end)
        write('current)
        R7 += one
        goto(error)
      }
      Label(divisionByZero)
      string("Runtime error: Division by 0")
      Label(nullPointer)
      string("Runtime error: Null pointer")
      Label(outOfMemory)
      string("Runtime error: Out of memory")
      Label(stackOverflow)
      string("Runtime error: Stack overflow")
      classes foreach generate
      stack.allocate()
      objectStack.allocate()
      Label(heapPointer)
      Instruction("DAT", 1, heapPointer)
      Label(heap)   // "Start of heap"
      Instruction("DAT", App.arguments.heapSize, 0)
      Label(end)    // "End of program"
    }

    case c: Class => {
      Label(c.name.label) // "Virtual Method Table"
      val vmt = context.vmts.getOrElse(c.name.asInstanceOf[AbsoluteName],List())
      vmt.foreach(m => Instruction("DAT",1,m.label))
      c.methods foreach generate      
    }

    case m: Method => {
      Label(m.name.label) // "Method"
      stack.push(stackFrame)
      stackFrame := stack.position
      for (_ <- m.variables) stack.push(0) // initialize variables with null
      m.body foreach generate
    }

    case Read(operand) => local {
      generate(operand)
      generate(New(Class.intClass.name))      
      'newInt := stack.pop
      'target := Class.headerSize
      'target += 'newInt      
      ~'target := read // "Write character to new Integer"      
      ~stack.pop := 'newInt // "Assign"      
    }

    case Write(operand) => local {
      generate(operand)      
      write(stack.pop)            
    }

    case w@While(condition, body) =>
      val whileLabel = label(w) + "_while"
      val endLabel = label(w) + "_endWhile"
      Label(whileLabel)
      generate(condition)
      gotoIf(stack.pop === O)(endLabel)      
      body foreach generate
      goto(whileLabel)
      Label(endLabel)    

    case f@Forever(body) =>
      val forever = label(f)
      Label(forever)
      body foreach generate
      goto(forever)    
      
    case i@If(condition, body, elseBody) =>
      val elseLabel = label(i) + "_else"
      val endLabel = label(i) + "_endIf"
      generate(condition)                   
      gotoIf(stack.pop === O)(elseLabel)      
      body foreach generate
      goto(endLabel)
      Label(elseLabel)
      elseBody foreach generate
      Label(endLabel)    
    
    case Call(call) => generate(call)

    case Assign(left, right) =>
      generate(right)
      generate(left)
      local {
        'left := stack.pop
        ~'left := stack.pop      
      }

    case r@Return(Some(expr), offset) => 
      generate(expr) 
      local {      
        'result := stack.top        
        stack.position -= offset
        stack.update('result)
        stackFrame -= 1
        'returnAddress := ~stackFrame
        stackFrame += 1
        stackFrame := ~stackFrame
        goto('returnAddress)
      }
    
    case r@Return(None, offset) => local {      
      stack.position -= offset
      stackFrame -= 1
      'returnAddress := ~stackFrame  // "Get return address"
      stackFrame += 1
      stackFrame := ~stackFrame  // "Get old stack frame"
      goto('returnAddress) // "return"
    }

    case Unary(operator, operand, typed) => 
      generate(operand)      
      operator match {
        case "-" => local {
          'negation := 0
          'negation -= stack.top
          stack.update('negation)
        }
        case "NOT" =>
          stack.update(stack.top === O)
      }    
      
    /** shortcut operators */
    case b@Binary(operator @ ("THEN" | "ELSE"), left, right, typed) => 
      val skip = label(b) + "_skip"
      generate(left)      
      operator match {
        case "THEN" =>            
          gotoIf(stack.top === O)(skip)
          stack.position -= one
        case "ELSE" =>
          gotoIf(stack.top)(skip)
          stack.position -= one
      }
      generate(right)
      Label(skip)    
    
    case b@Binary(operator, left, right, typed) => 
      val skip = label(b) + "_skip"
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
            gotoIf('right)(skip) // right is not 0 we skip the error
            error(divisionByZero)
            Label(skip)
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
      stack.push(value)    

    case n@New(typed) => local {
      val clazz = context.get(typed.asInstanceOf[AbsoluteName]).asInstanceOf[Class]
      val skip = label(n) + "_skipOOM"
      local { // Check if there is enough memory                
        'current := ~heapPointer
        'current += (clazz.size)
        'space := end
        'space -= 'current
        gotoIf('space > O)(skip) // if there is space left, we skip the error 
        error(outOfMemory)
        Label(skip)
      }
      'heapPointer := heapPointer
      'heap := ~'heapPointer // "Get heap pointer"
      ~'heap := clazz.name.label // "Put VMT Address on Heap"      
      stack.push('heap) // "Put reference to new object on stack"
      if (clazz.size > 1) local {
        'zero := 0
        for (i <- 1 to clazz.size) {
          'heap += 1
          ~'heap := 'zero
        }
      }
      'heap += one // "inc heap"
      ~'heapPointer := 'heap // "Update heap pointer"
    }

    case a@Access(left, right) => local {
      generate(left)
      right.parameters foreach generate
      generate(right)
    }
    
    case voc@VarOrCall(name, params, typed, lvalue, static) => local {
      val decl = context.get(name.asInstanceOf[AbsoluteName])
      val returnLabel = label(voc) + "_return"
      decl match {
        case a@Variable(name, typed, offset, true, _) => 
          'address := stack.top
          'address += offset.get
          stack.update('address)
        case Method(name, params, variables, body, typed, index, _) =>
          if (static) {
            stack.push(returnLabel)
            goto(name.label)
            Label(returnLabel)
          } 
          else {                     
            stack.position -= params.length
            'vmt := ~stack.top // "Get VMT"
            stack.position += params.length
            stack.push(returnLabel)            
            'vmt += index.get // Determine address of method pointer         
            'method := ~'vmt // Get address of method
            goto('method) // Goto method
            Label(returnLabel)
          }
        case v@Variable(name, typed, offset, false, _) =>
          'address := stackFrame
          'address += offset.get
          stack.push('address)
      }
    }

    case Box(expr, typed) => local {            
      generate(New(typed))
      generate(expr)
      'value := stack.pop // "Take value from stack"
      'new := stack.top // "Get reference to new object (stays on the stack)"      
      'new += Class.headerSize // "Determine location in new object"
      ~'new := 'value // "Write value to object")
    }
    
    case UnBox(expr, typed) => local {
      generate(expr)
      'object := stack.top // "Get reference to object from stack"      
      'object += Class.headerSize // "Determine address of value",
      'object := ~'object // "Read value"
      stack.update('object) // "Write to stack")
    }

    case d@DeRef(expr, typed) => local {      
      generate(expr)
      val skip = label(d) + "_skipNP"
      'ref := ~stack.top
      gotoIf('ref)(skip)
      error(nullPointer)
      Label(skip)
      stack.update('ref)
    }
  }

  def label(element: Element): String = {
    labelCounter = labelCounter + 1
    element.getClass.getSimpleName + "_" + element.pos.line + "_" + element.pos.column + "_" + labelCounter
  }
}
