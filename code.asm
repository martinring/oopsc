; R1: one
; R2: stack
; R3: stackFrame
; R4: objectStack
MRI R1, 1
MRI R2, stack_start
MRI R4, objectStack_start
; R5: heapPointer
MRI R5, _heapPointer
; R6: heap
MRM  R6, (R5)
; R7: value
MRI R7, Main
MMR (R6), R7
ADD R2, R1
MMR (R2), R6
ADD R6, R1
; R7: zero
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
MRI R5, 0
ADD R6, R5
; R5: method
MRM  R5, (R6)
MRR R0, R5
VarOrCall_0_0_1_return: 
MRI R0, _end
_error: 
; R5: current
MRM  R5, (R7)
ISZ R5, R5
JPC R5, _end
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
DAT 1, Main_main
Main_main: 
ADD R2, R1
MMR (R2), R3
MRR R3, R2
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
