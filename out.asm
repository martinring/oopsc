; R1: one
MRI R1, 1
; R2: stack
MRI R2, stack_start
; R3: stackFrame
; R4: objectStack
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
; R7: zero
MRI R7, 0
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R6), R7
ADD R6, R1
MMR (R5), R6
MRI R5, 0
SUB R2, R5
; R5: vmt
MRM  R6, (R2)
MRM  R5, (R6)
MRI R6, 0
ADD R2, R6
MRI R6, VarOrCall_0_0_2_return
ADD R2, R1
MMR (R2), R6
ADD R5, R1
; R6: method
MRM  R6, (R5)
MRR R0, R6
VarOrCall_0_0_2_return: 
MRI R0, _end
_error: 
; R5: current
MRM  R5, (R7)
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
_stackOverflow: 
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
DAT 1, 83
DAT 1, 116
DAT 1, 97
DAT 1, 99
DAT 1, 107
DAT 1, 32
DAT 1, 111
DAT 1, 118
DAT 1, 101
DAT 1, 114
DAT 1, 102
DAT 1, 108
DAT 1, 111
DAT 1, 119
DAT 1, 10
DAT 1, 0
Main: 
DAT 1, Main_printNumber
DAT 1, Main_main
Main_printNumber: 
ADD R2, R1
MMR (R2), R3
MRR R3, R2
MRI R5, 0
ADD R2, R1
MMR (R2), R5
MRI R5, 0
ADD R2, R1
MMR (R2), R5
; R5: address
MRR R5, R3
MRI R6, -2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_13_19_4_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_13_19_4_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
ADD R5, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_13_23_6_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_13_23_6_skipNP: 
MMR (R2), R5
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: value
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_14_12_12_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_14_12_12_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
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
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, If_0_0_8_else
MRI R5, 45
ADD R2, R1
MMR (R2), R5
MRM  R5, (R2)
SUB R2, R1
SYS 1, 5
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
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_16_24_15_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_16_24_15_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
; R5: negation
MRI R5, 0
MRM  R6, (R2)
SUB R5, R6
MMR (R2), R5
; R5: value
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
ADD R6, R1
MMR (R6), R5
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: value
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
MRI R0, If_0_0_9_endIf
If_0_0_8_else: 
If_0_0_9_endIf: 
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
MRI R5, 10
ADD R2, R1
MMR (R2), R5
; R5: value
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
ADD R6, R1
MMR (R6), R5
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: value
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
While_0_0_19_while: 
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_19_15_23_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_19_15_23_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_19_22_25_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_19_22_25_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
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
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, While_0_0_20_endWhile
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
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_20_20_29_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_20_20_29_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
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
; R5: value
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
ADD R6, R1
MMR (R6), R5
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: value
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
MRI R0, While_0_0_19_while
While_0_0_20_endWhile: 
While_0_0_31_while: 
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_22_15_35_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_22_15_35_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
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
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, While_0_0_32_endWhile
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
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_23_20_39_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_23_20_39_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
MRI R5, 10
ADD R2, R1
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
JPC R5, Binary_0_0_37_skip
MRI R7, _divisionByZero
MRI R0, _error
Binary_0_0_37_skip: 
DIV R6, R5
MMR (R2), R6
; R5: value
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
ADD R6, R1
MMR (R6), R5
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: value
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_24_19_44_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_24_19_44_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_24_28_46_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_24_28_46_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
JPC R5, Binary_0_0_42_skip
MRI R7, _divisionByZero
MRI R0, _error
Binary_0_0_42_skip: 
DIV R6, R5
MMR (R2), R6
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
MRM  R5, (R2)
SUB R2, R1
SYS 1, 5
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
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_25_23_50_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_25_23_50_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
; R5: address
MRR R5, R3
MRI R6, 2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_25_34_52_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_25_34_52_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
MOD R6, R5
MMR (R2), R6
; R5: value
MRM  R5, (R2)
SUB R2, R1
; R6: new
MRM  R6, (R2)
ADD R6, R1
MMR (R6), R5
; R5: address
MRR R5, R3
ADD R5, R1
ADD R2, R1
MMR (R2), R5
; R5: left
MRM  R5, (R2)
SUB R2, R1
; R6: value
MRM  R6, (R2)
SUB R2, R1
MMR (R5), R6
MRI R0, While_0_0_31_while
While_0_0_32_endWhile: 
MRI R5, 0
SUB R2, R5
SUB R3, R1
; R5: returnAddress
MRM  R5, (R3)
ADD R3, R1
MRM  R3, (R3)
MRR R0, R5
Main_main: 
ADD R2, R1
MMR (R2), R3
MRR R3, R2
; R5: address
MRR R5, R3
MRI R6, -2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_31_14_55_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_31_14_55_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
ADD R5, R1
MMR (R2), R5
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
; R6: target
MRI R6, 1
ADD R6, R5
; R7: value
SYS 0, 7
MMR (R6), R7
MRM  R7, (R2)
SUB R2, R1
MMR (R7), R5
While_0_0_58_while: 
; R5: address
MRR R5, R3
MRI R6, -2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_32_15_62_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_32_15_62_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
ADD R5, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_32_15_64_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_32_15_64_skipNP: 
MMR (R2), R5
; R5: object
MRM  R5, (R2)
ADD R5, R1
MRM  R5, (R5)
MMR (R2), R5
ADD R2, R1
MMR (R2), R1
; R5: negation
MRI R5, 0
MRM  R6, (R2)
SUB R5, R6
MMR (R2), R5
; R5: right
MRM  R5, (R2)
SUB R2, R1
; R6: left
MRM  R6, (R2)
SUB R6, R5
ISZ R6, R6
XOR R6, R1
MMR (R2), R6
MRM  R5, (R2)
SUB R2, R1
ISZ R5, R5
JPC R5, While_0_0_59_endWhile
; R5: address
MRR R5, R3
MRI R6, -2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_33_13_66_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_33_13_66_skipNP: 
MMR (R2), R5
MRI R5, 0
SUB R2, R5
; R5: vmt
MRM  R6, (R2)
MRM  R5, (R6)
MRI R6, 0
ADD R2, R6
MRI R6, VarOrCall_33_13_67_return
ADD R2, R1
MMR (R2), R6
MRI R6, 0
ADD R5, R6
; R6: method
MRM  R6, (R5)
MRR R0, R6
VarOrCall_33_13_67_return: 
MRI R5, 32
ADD R2, R1
MMR (R2), R5
MRM  R5, (R2)
SUB R2, R1
SYS 1, 5
; R5: address
MRR R5, R3
MRI R6, -2
ADD R5, R6
ADD R2, R1
MMR (R2), R5
; R5: ref
MRM  R6, (R2)
MRM  R5, (R6)
JPC R5, DeRef_35_18_69_skipNP
MRI R7, _nullPointer
MRI R0, _error
DeRef_35_18_69_skipNP: 
MMR (R2), R5
; R5: address
MRM  R5, (R2)
ADD R5, R1
MMR (R2), R5
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
; R6: target
MRI R6, 1
ADD R6, R5
; R7: value
SYS 0, 7
MMR (R6), R7
MRM  R7, (R2)
SUB R2, R1
MMR (R7), R5
MRI R0, While_0_0_58_while
While_0_0_59_endWhile: 
MRI R5, 0
SUB R2, R5
SUB R3, R1
; R5: returnAddress
MRM  R5, (R3)
ADD R3, R1
MRM  R3, (R3)
MRR R0, R5
Boolean: 
Integer: 
Object: 
stack_start: 
DAT 200, 0
stack_end: 
objectStack_start: 
DAT 200, 0
objectStack_end: 
_heapPointer: 
DAT 1, _heapPointer
_heap: 
DAT 500, 0
_end: 
