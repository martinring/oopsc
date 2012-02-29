package de.martinring.oopsc
import de.martinring.oopsc.syntactic._
import de.martinring.oopsc.synthesis.oopsvm._
import de.martinring.oopsc.semantic.Context

package object synthesis {
  import Code._
  
  private val stack: Stack = Stack("_stack", App.arguments.stackSize)
  private val objectStack: Stack = Stack("_objectStack", App.arguments.stackSize)
  private val heap: Heap = Heap("_heap", App.arguments.heapSize)
  private val stackFrame = variable("_stackFrame")
  private val test = variable("_test")  
  private val end = Label("_end")
  
  def generate(e: Element)(implicit context: Context): Code[Unit] = e match {
    case Program(classes) => lines (        
      stack.initialize,      
      objectStack.initialize,
      generate( Access(New(Root / "Main"), VarOrCall(Root / "Main" / "main", static = true)) ),       
      goto(end),
      sequence(classes map generate),
      stack.allocate,
      objectStack.allocate,
      heap.allocate,
      end)

    case c: Class => lines (
      Label(c.name.label),
      sequence(context.getVMT(c.name) map (m => instruction(DAT(1, Label(m.label))))),
      sequence(c.methods map generate))
    
    case m: Method => lines (
      Label(m.name.label),
      stack.push(stackFrame),
      stackFrame += 1,
      sequence(m.variables map (_ => stack.push(0))) >> sequence(m.body map generate) )
      
    case Read(operand) =>
      generate(operand) >> generate(New(Class.intClass.name)) >>
      stack.pop.use( newInt => lines (
        newInt := stack.pop,
        ~(newInt + Class.headerSize) := read,
        ~stack.pop := newInt ) 
      )
      
    case Write(operand) => generate(operand) >> write(stack.pop)
      
    case w@While(condition, body) =>
      val whileLabel = Label(w.label + "_while")
      val endLabel = Label(w.label + "_endWhile")      
      whileLabel >> generate(condition) >> when(!stack.pop).goto(endLabel) >>
      sequence(body map generate) >> goto(whileLabel) >> endLabel      

    case f@Forever(body) =>      
      val forever = Label(f.label + "_forever")
      forever >> sequence(body map generate) >> goto(forever)
      
    case i@If(condition, body, elseBody) =>      
      val elseLabel = Label(i.label + "_else")
      val endLabel = Label(i.label + "_endIf")
      generate(condition) >> when(!stack.pop).goto(elseLabel) >>
      sequence(body map generate) >> goto(endLabel) >> elseLabel >>
      sequence(elseBody map generate) >> endLabel
    
    case Call(call) => generate(call)

    case Assign(left, right) => lines(
      generate(right), generate(left),
      ~stack.pop := stack.pop)                

    case r@Return(Some(expr)) => 
      generate(expr) >> variable ( result => lines ( 
      result := stack.top, 
      stackFrame := stack.pop,
      stack.position := stackFrame,
      stack.push(result))) >>
      goto(stack.pop)

    case r@Return(None) => 
      (stackFrame := stack.pop) >> (stack.position := stackFrame) >> 
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
        case "THEN" => (when (!stack.top) goto (skip))
        case "ELSE" => (when (stack.top) goto (skip))
      }) >> (stack.position -= 1) >> generate(right) >> skip
    
    case b@Binary(operator, left, right, typed) =>
      generate(left) >> generate(right) >>
      variable ( left => variable ( right => lines (
      right := stack.pop, left := stack.top,
      operator match {
        case "+"   => left += right
        case "-"   => left -= right
        case "*"   => left *= right
        case "/"   => left /= right
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

    case Literal(value, typed) => stack.push(value)
    
    case n@New(typed) =>
      val clazz = context.get(typed.asInstanceOf[AbsoluteName]).asInstanceOf[Class]      
      heap.position.modify( heap =>
        (~heap := Label(clazz.name.label)) >> stack.push(heap) >>
        (heap += (clazz.attributes.size+1)) )

    case a@Access(left, right) =>
      generate(left) >> sequence(right.parameters map generate) >> generate(right)      
    
    case voc@VarOrCall(name, params, typed, lvalue, static) =>      
      context.get(name.asInstanceOf[AbsoluteName]) match {
        case Variable(_, _, offset, true, _) =>
          stack.push(stack.pop + offset.get)
        case Variable(_, _, offset, false, _) =>          
          stack.push(stackFrame + offset.get)
        case Method(name, params, variables, body, typed, index, _) =>
          val returnLabel = Label(voc.label + "_return")
          if (static) stack.push(returnLabel) >> goto(Label(name.label)) >> returnLabel 
          // TODO: Reenable dynamic calls...
          else stack.push(returnLabel) >> goto(Label(name.label)) >> returnLabel }

    case Box(expr, typed) =>
      generate(New(typed)) >> generate(expr) >> variable( value => lines(
        value := stack.pop,
        ~(stack.top + Class.headerSize) := value)
      )
    
    case UnBox(expr, typed) =>
      generate(expr) >> (stack.top := ~(stack.top + Class.headerSize))

    case d@DeRef(expr, typed) => 
      generate(expr) >> (stack.top := (~stack.top))
  }      
  
  private implicit def elementLabel(element: Element): { def label: String } = new {
    def label: String = element.getClass.getSimpleName + "_" + element.pos.line + "_" + element.pos.column
  }
}
