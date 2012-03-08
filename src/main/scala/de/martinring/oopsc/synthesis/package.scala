package de.martinring.oopsc
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.synthesis.oopsvm._
import de.martinring.oopsc.semantic.Context

package object synthesis {
  import Code._
  
  private val stack: Stack = Stack("stack", App.arguments.stackSize)
  private val objectStack: Stack = Stack("objectStack", App.arguments.stackSize)  
  private val stackFrame = variable("stackFrame")  
  
  def generate(e: Element)(implicit context: Context): Code[Unit] = e match {
    case Program(classes) =>     
      stack.initialize >>
      objectStack.initialize >>
      generate( Access(New(Root / "Main"), VarOrCall(Root / "Main" / "main")) ) >>
      exit >>
      sequence(classes map generate) >>
      runtimeErrors.resources >>
      garbageCollector.resources >>
      stack.allocate >>
      objectStack.allocate >>
      heap.allocate      

    case c: Class =>
      val vmt = context.getVMT(c.name)
      Label(c.name.label) >>
      sequence(vmt map (m => DAT(1,Label(m.label)) : Code[Unit])) >>
      sequence(c.methods map generate)
    
    case m: Method =>      
      Label(m.name.label) >>
      stack.push(stackFrame)("stack frame") >>
      (stackFrame := objectStack.position + 1) >>
      (if(m.variables.size > 0) 
        variable(z => (z:=0) >> sequence(m.variables map (v => objectStack.push(z)("null for " + v.name))))
      else just(())) >>
      sequence(m.body map generate)
      
    case Read(operand) =>
      generate(New(Class.intClass.name)) >>
      generate(operand) >>
      variable( newInt =>
      (newInt := objectStack.pop("new int")) >>
      (~(newInt + Class.headerSize + 1) := read) >>
      (~stack.pop("var to assign result of read") := newInt))
      
    case Write(operand) => generate(operand) >> write(stack.pop("value to write"))
      
    case w@While(condition, body) =>
      val whileLabel = Label(w.label + "_while")
      val endLabel = Label(w.label + "_endWhile")
      whileLabel >> generate(condition) >> when(stack.pop("while condition") === 0).goto(endLabel) >>
      sequence(body map generate) >> goto(whileLabel) >> endLabel   

    case f@Forever(body) =>      
      val forever = Label(f.label + "_forever")
      forever >> sequence(body map generate) >> goto(forever)
      
    case i@If(condition, body, elseBody) =>      
      val elseLabel = Label(i.label + "_else")
      val endLabel = Label(i.label + "_endIf")
      generate(condition) >> when(stack.pop("if condition") === 0).goto(elseLabel) >>
      sequence(body map generate) >> goto(endLabel) >> elseLabel >>
      sequence(elseBody map generate) >> endLabel
    
    case Call(call) => generate(call)
      
    case Assign(left, right) => debugInfo("what???")
      generate(right) >> generate(left) >> 
      (~stack.pop("left hand side of assignment") := objectStack.pop("right hand side of assignment"))
        

    case r@Return(Some(expr), offset) => 
      generate(expr) >> variable ( result =>
      (result := objectStack.top("return value")) >>
      debugInfo("correct objectStack by -" + offset) >>
      (objectStack.position -= offset) >>
      (objectStack.top("return value") := result) >>      
      (stackFrame := stack.pop("old stack frame")) >>
      goto(stack.pop("return address")))

    case r@Return(None, offset) =>
      debugInfo("correct objectStack by -" +offset) >>
      (objectStack.position -= offset) >>      
      (stackFrame := stack.pop("old stack frame")) >>
      goto(stack.pop("return address"))

    case Unary(operator, operand, typed) => 
      generate(operand) >> (operator match {
        case "-" => (stack.top("operand") := -stack.top("negated operand"))
        case "NOT" => (stack.top("operand") := !stack.top("not operand"))
      })
      
    /** shortcut operators */
    case b@Binary(operator @ ("THEN" | "ELSE"), left, right, typed) => 
      val skip = Label(b.label + "_skip")
      generate(left) >> (operator match {
        case "THEN" => (when (stack.top("left operand of AND THEN") === 0) goto (skip))
        case "ELSE" => (when (stack.top("left operand of OR ELSE")) goto (skip))
      }) >> (stack.position -= 1) >> generate(right) >> skip
    
    case b@Binary(operator @ ("==" | "!="), left, right, typed) =>
      generate(left) >> generate(right) >> variable(right =>
      (right := objectStack.pop("right operand of " + operator)) >>
      (operator match {
        case "==" => stack.push(objectStack.pop("right operand of " + operator) === right)("result of " + operator)
        case "!=" => stack.push(!(objectStack.pop("right operand of " + operator) === right))("result of " + operator)
      }))
      
    case b@Binary(operator, left, right, typed) =>
      generate(left) >> generate(right) >>
      variable( left => variable ( right => 
      (right := stack.pop("right operand of " + operator)) >>
      (left := stack.top("left operand of " + operator)) >>
      (operator match {
        case "+"   => left += right
        case "-"   => left -= right
        case "*"   => left *= right
        case "/"   => 
          runtimeErrors.require(right)(runtimeErrors.nullPointer) >>
          (left /= right)
        case "MOD" => left %= right
        case "AND" => left &= right
        case "OR"  => left |= right
        case ">"   => left := left > right            
        case ">="  => left := left >= right            
        case "<"   => left := left < right            
        case "<="  => left := left <= right
        case "="   => left := left === right            
        case "#"   => left := !(left === right)
      })) >> (stack.top("result of " + operator) := left))
      
    case Literal.NULL => objectStack.push(0)("null pointer")
      
    case Literal(value, typed) => stack.push(value)("literal " + value)
    
    case n@New(typed) =>
      val clazz = context.get(typed.asInstanceOf[AbsoluteName]).asInstanceOf[Class]
      runtimeErrors.require(heap.space >= clazz.size)(runtimeErrors.outOfMemory) >>
      heap.position.modify( pos =>
      (~pos := Label(clazz.name.label)) >>
      objectStack.push(pos)("position of new " + typed + " on heap") >>
      variable( init => (init := 0) >> sequence(0.to(clazz.size).map(_ => (pos += 1) >> (~pos := init)).toList))) >>
      (heap.space -= clazz.size)

    case a@Access(left, right) =>
      generate(left) >> sequence(right.parameters map generate) >> generate(right)      
    
    case voc@VarOrCall(name, params, typed, lvalue, static) =>      
      context.get(name.asInstanceOf[AbsoluteName]) match {
        case Variable(_, _, offset, true, _) =>
          stack.push(objectStack.pop("object with " + name.relative) + offset.get)("address of " + name)
        case Variable(_, _, offset, false, _) =>
          stack.push(stackFrame + offset.get)("address of " + name)
        case Method(name, params, variables, body, typed, index, _) =>
          val returnLabel = Label(voc.label + "_return")
          if (static) 
            stack.push(returnLabel)("return address") >> 
            goto(Label(name.label)) >> returnLabel
          else            
            stack.push(returnLabel)("return address") >> 
            goto(~(~(~(objectStack.position - (params.length))) + index.get)) >> 
            returnLabel
      }
      
    case Box(expr, typed) =>
      generate(expr) >> generate(New(typed)) >>
      (~(objectStack.top("boxed " + typed) + Class.headerSize + 1) := stack.pop(typed + " to box"))
    
    case UnBox(expr, typed) =>
      generate(expr) >> (stack.push(~(objectStack.pop(typed + " to unbox") + (Class.headerSize + 1)))("unboxed " + typed))

    case d@DeRef(expr, typed) =>
      generate(expr) >> variable( r => (r := ~stack.pop("pointer to dereference")) >> 
      runtimeErrors.require(r)(runtimeErrors.nullPointer) >>
      objectStack.push(r)("dereferenced " + typed))
  }      

  case class Stack(name: String, size: Int) {
    val start: Label = Label("_" + name)

    val position: Code[R] = variable("_" + name)

    val initialize: Code[Unit] = position >>= (r => r := start)
    val allocate: Code[Unit] = start >> DAT(size, 0)

    def push(r: Value)(what: String): Code[Unit] =  (position += 1) >> (~position := r) >> debugInfo("+ " + name + " <- " + what)
    def pop(what: String): Value = result ( r => debugInfo("- " + name + " -> " + what) >> (r := ~position) >> (position -= 1) )

    def top(what: String): LValue = new LValue {
      def assignTo(r: R) = debugInfo(". " + name + " -> " + what) >> (r := ~position)
      def :=(v: Value) = debugInfo(". " + name + " <- " + what) >> (~position := v)
    }
  }

  object heap {
    val size = App.arguments.heapSize
      
    private val start1: Label = Label("_heap_1")
    private val start2: Label = Label("_heap_2")
    
    val current = MemoryVar("currentHeap")
    val position = MemoryVar("heap_pointer")
    val space = MemoryVar("heapSpace")
    
    val switch =             
      variable( p => variable( c =>             
      (c := current) >>
      (p := start1 * c) >>
      (c ^= 1) >>
      (p += start2 * c) >>
      (current := c)) >>
      (position := p)) >>
      (space := size)
    
    val allocate: Code[Unit] = 
      current.allocate(0) >>
      position.allocate(start1) >>
      space.allocate(size) >>
      start1 >> DAT(size,0) >>
      start2 >> DAT(size,0) 
  }
    
  
  object garbageCollector {
    private val code = Label("_garbageCollector")
    val returnAddress = MemoryVar("gcReturnAddress")
    
    def runIf(cond: Value) = {
      counter += 1
      val ret = Label("gc_return_" + counter)
      (returnAddress := ret) >> when(cond).goto(code) >> ret
    }
    
    val resources = 
      returnAddress.allocate(0) >>
      code >> heap.switch >> goto(returnAddress)
  }
  
  object runtimeErrors {
    private val label = Label("_error")
    private val print = Label("_print")
    
    private def string(l: String, s: String) = 
      Label(l) >> sequence(for (c <- "\n"+s+"\n\0" toList) yield DAT(1,c.toInt) : Code[Unit])
    
    def require(v: Value)(error: Code[Unit]) = {
      counter += 1
      val skip = Label("skip_" + counter)
      when(v).goto(skip) >> error >> skip
    }
    
    def resources = 
      string("_outOfMemory"," *** runtime error: out of memory ***") >>
      string("_nullPointer"," *** runtime error: null pointer ***") >>
      string("_divBy0"," *** runtime error: division by zero ***") >>
      label >> (R6 := ~R7) >> (when (R6) goto print) >>
      exit >> print >> write(R6) >> (R7 += 1) >>
      goto(label)
    
    def outOfMemory = (R7 := Label("_outOfMemory")) >> goto(label)
    def nullPointer = (R7 := Label("_nullPointer")) >> goto(label)
    def divByZero = (R7 := Label("_divBy0")) >> goto(label)
  }    
  
  private var counter: Int = 0
  
  private implicit def elementLabel(element: Element): { def label: String } = new {
    def label: String = element.getClass.getSimpleName + "_" + ((element.pos.line,element.pos.column) match {
      case (0,0) => counter += 1; counter
      case (l,c) => l + "_" + c
    })
  }
}
