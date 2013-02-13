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
      stack.push(stackFrame) >>
      (stackFrame := objectStack.position + 1) >>
      (if(m.variables.size > 0) 
        variable(z => (z:=0) >> sequence(m.variables map (v => objectStack.push(z))))
      else just(())) >>
      sequence(m.body map generate)
      
    case Read(operand) =>
      generate(New(Class.intClass.name)) >>      
      generate(operand) >>
      variable( newInt =>      
      (newInt := objectStack.pop) >>
      (~(newInt + Class.headerSize + 1) := read) >>
      (~stack.pop := newInt))
      
    case Write(operand) => generate(operand) >> write(stack.pop)
      
    case w@While(condition, body) =>
      val whileLabel = Label(w.label + "_while")
      val endLabel = Label(w.label + "_endWhile")
      whileLabel >> generate(condition) >> when(stack.pop === 0).goto(endLabel) >>
      sequence(body map generate) >> goto(whileLabel) >> endLabel   

    case f@Forever(body) =>      
      val forever = Label(f.label + "_forever")
      forever >> sequence(body map generate) >> goto(forever)
      
    case i@If(condition, body, elseBody) =>      
      val elseLabel = Label(i.label + "_else")
      val endLabel = Label(i.label + "_endIf")
      generate(condition) >> when(stack.pop === 0).goto(elseLabel) >>
      sequence(body map generate) >> goto(endLabel) >> elseLabel >>
      sequence(elseBody map generate) >> endLabel
    
    case Call(call) => generate(call)
      
    case Assign(left, Literal.NULL) =>
      generate(left) >> 
      (~stack.pop := 0)
    
    
    case Assign(left, right) =>
      generate(right) >> generate(left) >> 
      (~stack.pop := objectStack.pop)
        

    case r@Return(Some(expr), offset) => 
      generate(expr) >> variable ( result =>
      (result := objectStack.top) >>      
      (objectStack.position -= offset) >>
      (objectStack.top := result) >>      
      (stackFrame := stack.pop) >>
      goto(stack.pop))

    case r@Return(None, offset) =>      
      (objectStack.position -= offset) >>      
      (stackFrame := stack.pop) >>
      goto(stack.pop)

    case Unary(operator, operand, typed) => 
      generate(operand) >> (operator match {
        case "-" => (stack.top := -stack.top)
        case "NOT" => (stack.top := !stack.top)
      })
      
    /** shortcut operators */
    case b@Binary(operator @ ("THEN" | "ELSE"), left, right, typed) => 
      val skip = Label(b.label + "_skip")
      generate(left) >> (operator match {
        case "THEN" => (when (stack.top === 0) goto (skip))
        case "ELSE" => (when (stack.top) goto (skip))
      }) >> (stack.position -= 1) >> generate(right) >> skip

    case b@Binary(operator @ ("==" | "!="), left, right, typed) =>
      generate(left) >> generate(right) >> variable(right =>
      (right := objectStack.pop) >>
      (operator match {
        case "==" => stack.push(objectStack.pop === right)
        case "!=" => stack.push(!(objectStack.pop === right))
      }))
      
    case b@Binary(operator, left, right, typed) =>
      generate(left) >> generate(right) >>
      variable( left => variable ( right => 
      (right := stack.pop) >>
      (left := stack.top) >>
      (operator match {
        case "+"   => left += right
        case "-"   => left -= right
        case "*"   => left *= right
        case "/"   => 
          runtimeErrors.require(right)(runtimeErrors.divByZero) >>
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
      })) >> (stack.top := left))
      
    case Literal.NULL => objectStack.push(0)
      
    case Literal(value, typed) => stack.push(value)
    
    case n@New(typed) =>
      val clazz = context.get(typed.asInstanceOf[AbsoluteName]).asInstanceOf[Class]
      garbageCollector.runIf(heap.space < clazz.size) >>
      // if the garbage collector ran but could not free up enough memory
      // we need to throw a runtime error. This also occurs if the garbage
      // collector is invoked while it is allready running. (see garbageCollector.running)
      runtimeErrors.require(heap.space >= clazz.size)(runtimeErrors.outOfMemory) >>
      heap.position.modify(pos =>      
      (~pos := Label(clazz.name.label)) >>
      objectStack.push(pos) >>
      variable( init => (init := 0) >> sequence(1.to(clazz.size).map(_ => (pos += 1) >> (~pos := init)).toList)) >> (pos += 1)) >>
      (heap.space -= (clazz.size + 2))

    case a@Access(left, right) =>
      generate(left) >> 
      runtimeErrors.require(objectStack.top)(runtimeErrors.nullPointer) >> 
      sequence(right.parameters map generate) >> 
      generate(right)      
    
    case voc@VarOrCall(name, params, typed, lvalue, static) =>
      context.get(name.asInstanceOf[AbsoluteName]) match {
        case Variable(_, _, offset, true, _) =>
          stack.push(objectStack.pop + offset.get)
        case Variable(_, _, offset, false, _) =>
          stack.push(stackFrame + offset.get)
        case Method(name, params, variables, body, typed, index, _) =>
          val returnLabel = Label(voc.label + "_return")
          if (static) 
            stack.push(returnLabel) >> 
            goto(Label(name.label)) >> 
            returnLabel
          else            
            stack.push(returnLabel) >> 
            goto(~(~(~(objectStack.position - (params.length))) + index.get)) >> 
            returnLabel
      }
      
    case Box(expr, typed) =>
      generate(expr) >> generate(New(typed)) >>
      (~(objectStack.top + Class.headerSize + 1) := stack.pop)
    
    case UnBox(expr, typed) =>
      generate(expr) >> (stack.push(~(objectStack.pop + (Class.headerSize + 1))))

    case DeRef(expr, typed) =>
      generate(expr) >> variable( r => (r := ~stack.pop) >>       
      objectStack.push(r))
  }      

  case class Stack(name: String, size: Int) {
    val start: Label = Label("_" + name)    
    val position: Code[R] = variable("_" + name)
    val end: Label = Label("_" + name + "_end")

    val initialize: Code[Unit] = position >>= (r => r := start)
    val allocate: Code[Unit] = start >> DAT(size, 0) >> end

    def push(r: Value): Code[Unit] = 
      (position += 1) >>
      //unfortunately this code runs out of registers...
      //runtimeErrors.require(position <= end)(runtimeErrors.stackOverflow) >> 
      (~position := r)
      
    def pop: Value = 
      result ( r => 
        (r := ~position) >> 
        (position -= 1) )

    def top: LValue = new LValue {
      def assignTo(r: R) = (r := ~position)
      def :=(v: Value) = (~position := v)
    }
  }  
  
  object heap {
    val size = App.arguments.heapSize
      
    private val start1: Label = Label("_heap_1")
    private val start2: Label = Label("_heap_2")
    
    val current = MemoryVar("currentHeap")
    val position = MemoryVar("heapPointer")
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
    
    val allocate = 
      current.allocate(1) >>
      position.allocate(start2) >>
      space.allocate(size) >>
      start1 >> DAT(size,0) >>
      start2 >> DAT(size,0) 
  }
    
  
  object garbageCollector {
    private val code = Label("_garbageCollector")
    val returnAddress = MemoryVar("gcReturnAddress")
    val running = MemoryVar("gcRunning")
    val before  = MemoryVar("gcBefore")    
    
    def runIf(cond: Value) = {
      counter += 1
      val ret = Label("gc_return_" + counter)
      when(running).goto(ret) >> 
      (returnAddress := ret) >> 
      when(cond).goto(code) >> 
      ret
    }
    
    val gcexit = Label("_gcexit")
    val copynext  = Label("_gc_coopynext")
    val clonenext = Label("_gc_clonenext")
    
    def resources(implicit context: Context) =
      returnAddress.allocate(0) >>
      running.allocate(0) >>
      before.allocate(0) >>      
      code >>            
      (running := 1) >>
      heap.switch >>      
      (before := objectStack.position) >>      
      copynext >>        
      stack.push(objectStack.pop) >>      
      when(objectStack.position === objectStack.start).goto(clonenext) >>
      goto(copynext) >>
      clonenext >>      
      when(objectStack.position === before).goto(gcexit) >>
      objectStack.push(stack.pop) >>
      when(objectStack.top === 0).goto(clonenext) >>
      generate(VarOrCall(Root / "Object" / "_clone", static = false)) >>
      goto(clonenext) >>
      gcexit >>       
      (running := 0) >>      
      goto(returnAddress)
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
      string("_outOfMemory",   " *** runtime error: out of memory ***") >>
      string("_nullPointer",   " *** runtime error: null pointer ***") >>
      string("_divBy0",        " *** runtime error: division by zero ***") >>
      string("_stackOverflow", " *** runtime error: stack overflow ***") >>
      label >> (R6 := ~R7) >> (when (R6) goto print) >>
      exit >> print >> write(R6) >> (R7 += 1) >>
      goto(label)
    
    def outOfMemory   = (R7 := Label("_outOfMemory"))   >> goto(label)
    def nullPointer   = (R7 := Label("_nullPointer"))   >> goto(label)
    def divByZero     = (R7 := Label("_divBy0"))        >> goto(label)
    def stackOverflow = (R7 := Label("_stackOverflow")) >> goto(label)    
  }    
  
  private var counter: Int = 0
  
  private implicit def elementLabel(element: Element): { def label: String } = new {
    def label: String = element.getClass.getSimpleName + "_" + ((element.pos.line,element.pos.column) match {
      case (0,0) => counter += 1; counter
      case (l,c) => l + "_" + c
    })
  }
}
