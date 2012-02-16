; R1: one
; R2: stack
; R3: stackFrame
; R4: objectStack
MRI R1, 1
MRI R2, stack_start
MRI R4, objectStack_start
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Main
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: size
MRI R5, 0
SUB R2, R5
; R6: vmt
MRM  R6, (R2)
MRM  R6, (R6)
ADD R2, R5
; R5: value
MRI R5, VarOrCall_0_0_1_return
ADD R2, R1
MMR (R2), R5
; R5: index
MRI R5, 1
ADD R6, R5
; R5: method
MRM  R5, (R6)
MRR R0, R5
VarOrCall_0_0_1_return: 
MRI R0, _end
_error: 
; R5: current
MRM  R5, (R7)
; R6: atEnd
ISZ R6, R5
JPC R6, _end
SYS 1, 5
ADD R7, R1
MRI R0, _error
_divisionByZero: 
DAT 1, 10
DAT 1, 82
DAT 1, 117
DAT 1, 110
DAT 1, 116
DAT 1, 105
DAT 1, 109
DAT 1, 101
DAT 1, 32
DAT 1, 101
DAT 1, 114
DAT 1, 114
DAT 1, 111
DAT 1, 114
DAT 1, 58
DAT 1, 32
DAT 1, 68
DAT 1, 105
DAT 1, 118
DAT 1, 105
DAT 1, 115
DAT 1, 105
DAT 1, 111
DAT 1, 110
DAT 1, 32
DAT 1, 98
DAT 1, 121
DAT 1, 32
DAT 1, 48
DAT 1, 10
DAT 1, 0
_nullPointer: 
DAT 1, 10
DAT 1, 82
DAT 1, 117
DAT 1, 110
DAT 1, 116
DAT 1, 105
DAT 1, 109
DAT 1, 101
DAT 1, 32
DAT 1, 101
DAT 1, 114
DAT 1, 114
DAT 1, 111
DAT 1, 114
DAT 1, 58
DAT 1, 32
DAT 1, 78
DAT 1, 117
DAT 1, 108
DAT 1, 108
DAT 1, 32
DAT 1, 112
DAT 1, 111
DAT 1, 105
DAT 1, 110
DAT 1, 116
DAT 1, 101
DAT 1, 114
DAT 1, 10
DAT 1, 0
_outOfMemory: 
DAT 1, 10
DAT 1, 82
DAT 1, 117
DAT 1, 110
DAT 1, 116
DAT 1, 105
DAT 1, 109
DAT 1, 101
DAT 1, 32
DAT 1, 101
DAT 1, 114
DAT 1, 114
DAT 1, 111
DAT 1, 114
DAT 1, 58
DAT 1, 32
DAT 1, 79
DAT 1, 117
DAT 1, 116
DAT 1, 32
DAT 1, 111
DAT 1, 102
DAT 1, 32
DAT 1, 109
DAT 1, 101
DAT 1, 109
DAT 1, 111
DAT 1, 114
DAT 1, 121
DAT 1, 10
DAT 1, 0
Main: 
DAT 1, Main_printNumber
DAT 1, Main_main
Main_printNumber: 
ADD R2, R1
MMR (R2), R3
MRR R3, R2
; R5: size
MRI R5, 2
ADD R2, R5
; R5: offset
MRI R5, -2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_13_19_3_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_13_19_3_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
; R6: offset
MRI R6, 1
ADD R5, R6
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_13_23_5_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_13_23_5_skipNP: 
MMR (R2), R5
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: right
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_14_12_11_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_14_12_11_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: lvalue
MRI R5, 0
ADD R2, R1
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
SUB R6, R5
ISN R6, R6
MMR (R2), R6
; R5: cond
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, If_0_0_7_else
; R5: lvalue
MRI R5, 45
ADD R2, R1
MMR (R2), R5
; R5: wvalue
MRM  R5, (R2)
SUB R2, R1
SYS 1, 5
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_16_24_13_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_16_24_13_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: operand
MRM  R5, (R2)
; R6: negation
MRI R6, 0
SUB R6, R5
MMR (R2), R6
; R5: bvalue
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
; R7: size
MRI R7, 1
ADD R6, R7
MMR (R6), R5
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: right
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
MRI R0, If_0_0_8_endIf
If_0_0_7_else: 
If_0_0_8_endIf: 
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: lvalue
MRI R5, 10
ADD R2, R1
MMR (R2), R5
; R5: bvalue
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
; R7: size
MRI R7, 1
ADD R6, R7
MMR (R6), R5
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: right
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
While_0_0_16_while: 
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_19_15_20_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_19_15_20_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_19_22_22_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_19_22_22_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
SUB R6, R5
ISP R6, R6
XOR R6, R1
MMR (R2), R6
; R5: cond
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, While_0_0_17_endWhile
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_20_20_25_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_20_20_25_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: lvalue
MRI R5, 10
ADD R2, R1
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
MUL R6, R5
MMR (R2), R6
; R5: bvalue
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
; R7: size
MRI R7, 1
ADD R6, R7
MMR (R6), R5
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: right
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
MRI R0, While_0_0_16_while
While_0_0_17_endWhile: 
While_0_0_27_while: 
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_22_15_31_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_22_15_31_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: lvalue
MRI R5, 10
ADD R2, R1
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
SUB R6, R5
ISN R6, R6
XOR R6, R1
MMR (R2), R6
; R5: cond
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, While_0_0_28_endWhile
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_23_20_34_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_23_20_34_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: lvalue
MRI R5, 10
ADD R2, R1
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
JPC R5, Binary_0_0_32
MRI R7, _divisionByZero
MRI R0, _error
Binary_0_0_32: 
DIV R6, R5
MMR (R2), R6
; R5: bvalue
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
; R7: size
MRI R7, 1
ADD R6, R7
MMR (R6), R5
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: right
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_24_19_39_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_24_19_39_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_24_28_41_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_24_28_41_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
JPC R5, Binary_0_0_37
MRI R7, _divisionByZero
MRI R0, _error
Binary_0_0_37: 
DIV R6, R5
MMR (R2), R6
; R5: lvalue
MRI R5, 48
ADD R2, R1
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
ADD R6, R5
MMR (R2), R6
; R5: wvalue
MRM  R5, (R2)
SUB R2, R1
SYS 1, 5
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_25_23_44_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_25_23_44_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: offset
MRI R5, 2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_25_34_46_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_25_34_46_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
MOD R6, R5
MMR (R2), R6
; R5: bvalue
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
; R7: size
MRI R7, 1
ADD R6, R7
MMR (R6), R5
; R5: offset
MRI R5, 1
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: right
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
MRI R0, While_0_0_27_while
While_0_0_28_endWhile: 
; R5: offset
MRI R5, 0
SUB R2, R5
SUB R3, R1
; R6: returnAddress
MRM  R6, (R3)
ADD R3, R1
MRM  R3, (R3)
MRR R0, R6
Main_main: 
ADD R2, R1
MMR (R2), R3
MRR R3, R2
; R5: offset
MRI R5, -2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_31_14_49_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_31_14_49_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
; R6: offset
MRI R6, 1
ADD R5, R6
MMR (R2), R5
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: newInt
MRM  R5, (R2)
SUB R2, R1
; R6: targetPos
MRI R6, 1
ADD R6, R5
; R7: rvalue
SYS 0, 7
MMR (R6), R7
; R6: target
MRM  R6, (R2)
SUB R2, R1
MMR (R6), R5
While_0_0_51_while: 
; R5: offset
MRI R5, -2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_32_15_55_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_32_15_55_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
; R6: offset
MRI R6, 1
ADD R5, R6
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_32_15_57_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_32_15_57_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
; R6: size
MRI R6, 1
ADD R5, R6
MRM  R5, (R5)
MMR (R2), R5
; R5: lvalue
MRI R5, 1
ADD R2, R1
MMR (R2), R5
; R5: operand
MRM  R5, (R2)
; R6: negation
MRI R6, 0
SUB R6, R5
MMR (R2), R6
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
SUB R6, R5
ISZ R6, R6
XOR R6, R1
MMR (R2), R6
; R5: cond
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, While_0_0_52_endWhile
; R5: offset
MRI R5, -2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_33_13_59_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_33_13_59_skipNP: 
MMR (R2), R5
; R5: size
MRI R5, 0
SUB R2, R5
; R6: vmt
MRM  R6, (R2)
MRM  R6, (R6)
ADD R2, R5
; R5: value
MRI R5, VarOrCall_33_13_60_return
ADD R2, R1
MMR (R2), R5
; R5: index
MRI R5, 0
ADD R6, R5
; R5: method
MRM  R5, (R6)
MRR R0, R5
VarOrCall_33_13_60_return: 
; R5: lvalue
MRI R5, 32
ADD R2, R1
MMR (R2), R5
; R5: wvalue
MRM  R5, (R2)
SUB R2, R1
SYS 1, 5
; R5: offset
MRI R5, -2
ADD R5, R3
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R5, (R2)
MRM  R5, (R5)
JPC R5, DeRef_35_18_62_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_35_18_62_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
; R6: offset
MRI R6, 1
ADD R5, R6
MMR (R2), R5
; R5: max
MRI R5, _end
; R6: heap
MRI R6, _heapPointer
MRM  R6, (R6)
; R7: current
MRI R7, 2
ADD R7, R6
SUB R5, R7
ISN R5, R5
MRI R7, _outOfMemory
JPC R5, _error
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Integer
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
; R5: newInt
MRM  R5, (R2)
SUB R2, R1
; R6: targetPos
MRI R6, 1
ADD R6, R5
; R7: rvalue
SYS 0, 7
MMR (R6), R7
; R6: target
MRM  R6, (R2)
SUB R2, R1
MMR (R6), R5
MRI R0, While_0_0_51_while
While_0_0_52_endWhile: 
; R5: offset
MRI R5, 0
SUB R2, R5
SUB R3, R1
; R6: returnAddress
MRM  R6, (R3)
ADD R3, R1
MRM  R3, (R3)
MRR R0, R6
Boolean: 
Integer: 
Object: 
stack_start: 
DAT 50, 0
stack_end: 
objectStack_start: 
DAT 50, 0
objectStack_end: 
_heapPointer: 
DAT 1, _heapPointer
_heap: 
DAT 100, 0
_end: 