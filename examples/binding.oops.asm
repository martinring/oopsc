; Program
  MRI R1, 1; R1 is allways 1
  MRI R2, _stack; R2 points to stack
  MRI R4, _heap; R4 points to next free position on Heap
  ; ACCESS
    ; NEW Main
      MRI R5, _end
      MRI R6, 1
      ADD R6, R4
      SUB R5, R6
      ISN R5, R5
      MRI R7, _outOfMemory
      JPC R5, _error
      ADD R2, R1
      MRI R5, Main; Get reference to VMT
      MMR (R4), R5; Put VMT Address on Heap
      MMR (R2), R4; Put reference to new object on stack
      MRI R5, 1; Class size
      ADD R4, R5; Inc heap
    ; END NEW Main
    ; Parameters
      
    ; END Parameters
    ; DYNAMIC CALL OF METHOD Main.main
      MRI R6, 0
      SUB R2, R6
      MRM  R5, (R2); Get VMT
      ADD R2, R6
      ADD R2, R1
      MRI R6, Main_0_0_42
      MMR (R2), R6
      MRM  R5, (R5)
      MRI R6, 0
      ADD R5, R6
      MRM  R5, (R5)
      MRR R0, R5
      Main_0_0_42: 
    ; END DYNAMIC CALL OF METHOD Main.main
  ; END ACCESS
  MRI R0, _end; exit program
  _error: 
  MRM  R5, (R7)
  ISZ R6, R5
  JPC R6, _end
  SYS 1, 5
  ADD R7, R1
  MRI R0, _error
  _divisionByZero: 
  ; Runtime error: Division by 0
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
  ; END Runtime error: Division by 0
  _nullPointer: 
  ; Runtime error: Null pointer
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
  ; END Runtime error: Null pointer
  _outOfMemory: 
  ; Runtime error: Out of memory
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
  ; END Runtime error: Out of memory
  ; CLASSES
    ; CLASS A
      A: 
      ; VMT OF CLASS A
        DAT 1, A_print
      ; END VMT OF CLASS A
      ; METHODS OF CLASS A
        ; METHOD A.print
          A_print: 
          ADD R2, R1
          MMR (R2), R3; Save old stack frame
          MRR R3, R2; Current position on stack is new frame
          
          ; BODY
            ; WRITE
              ; UNBOX
                ; DEREF
                  ; ACCESS
                    ; DEREF
                      ; REF TO VARIABLE A.print.SELF
                        MRI R5, -2
                        ADD R5, R3
                        ADD R2, R1
                        MMR (R2), R5
                      ; END REF TO VARIABLE A.print.SELF
                      MRM  R5, (R2)
                      MRM  R5, (R5)
                      JPC R5, A_print_8_15_2_skipNP
                      MRI R7, _nullPointer
                      MRI R0, _error
                      A_print_8_15_2_skipNP: 
                      MMR (R2), R5
                    ; END DEREF
                    ; Parameters
                      
                    ; END Parameters
                    ; REF TO ATTRIBUTE A.a
                      MRM  R5, (R2)
                      MRI R6, 1
                      ADD R5, R6
                      MMR (R2), R5
                    ; END REF TO ATTRIBUTE A.a
                  ; END ACCESS
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, A_print_8_15_4_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  A_print_8_15_4_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                MRM  R5, (R2); Get reference to object from stack
                MRI R6, 1
                ADD R5, R6; Determine address of value
                MRM  R5, (R5); Read value
                MMR (R2), R5; Write to stack
              ; END UNBOX
              MRM  R5, (R2)
              SUB R2, R1
              SYS 1, 5
            ; END WRITE
            ; RETURN
              MRI R5, 3
              SUB R2, R5; Correct stack
              SUB R3, R1
              MRM  R5, (R3); Get return address
              ADD R3, R1
              MRM  R3, (R3); Get old stack frame
              MRR R0, R5; return
            ; END RETURN
          ; END BODY
        ; END METHOD A.print
      ; END METHODS OF CLASS A
    ; END CLASS A
    ; CLASS B
      B: 
      ; VMT OF CLASS B
        DAT 1, B_print
        DAT 1, B_printBase
      ; END VMT OF CLASS B
      ; METHODS OF CLASS B
        ; METHOD B.print
          B_print: 
          ADD R2, R1
          MMR (R2), R3; Save old stack frame
          MRR R3, R2; Current position on stack is new frame
          
          ; BODY
            ; WRITE
              ; UNBOX
                ; DEREF
                  ; ACCESS
                    ; DEREF
                      ; REF TO VARIABLE B.print.SELF
                        MRI R5, -2
                        ADD R5, R3
                        ADD R2, R1
                        MMR (R2), R5
                      ; END REF TO VARIABLE B.print.SELF
                      MRM  R5, (R2)
                      MRM  R5, (R5)
                      JPC R5, B_print_17_15_6_skipNP
                      MRI R7, _nullPointer
                      MRI R0, _error
                      B_print_17_15_6_skipNP: 
                      MMR (R2), R5
                    ; END DEREF
                    ; Parameters
                      
                    ; END Parameters
                    ; REF TO ATTRIBUTE B.a
                      MRM  R5, (R2)
                      MRI R6, 2
                      ADD R5, R6
                      MMR (R2), R5
                    ; END REF TO ATTRIBUTE B.a
                  ; END ACCESS
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, B_print_17_15_8_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  B_print_17_15_8_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                MRM  R5, (R2); Get reference to object from stack
                MRI R6, 1
                ADD R5, R6; Determine address of value
                MRM  R5, (R5); Read value
                MMR (R2), R5; Write to stack
              ; END UNBOX
              MRM  R5, (R2)
              SUB R2, R1
              SYS 1, 5
            ; END WRITE
            ; RETURN
              MRI R5, 3
              SUB R2, R5; Correct stack
              SUB R3, R1
              MRM  R5, (R3); Get return address
              ADD R3, R1
              MRM  R3, (R3); Get old stack frame
              MRR R0, R5; return
            ; END RETURN
          ; END BODY
        ; END METHOD B.print
        ; METHOD B.printBase
          B_printBase: 
          ADD R2, R1
          MMR (R2), R3; Save old stack frame
          MRR R3, R2; Current position on stack is new frame
          
          ; BODY
            ; CALL
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE B.printBase.BASE
                    MRI R5, -2
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE B.printBase.BASE
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, B_printBase_22_9_10_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  B_printBase_22_9_10_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; STATIC CALL OF METHOD A.print
                  MRI R5, A_22_14_11
                  ADD R2, R1
                  MMR (R2), R5
                  MRI R0, A_print
                  A_22_14_11: 
                ; END STATIC CALL OF METHOD A.print
              ; END ACCESS
            ; END CALL
            ; RETURN
              MRI R5, 3
              SUB R2, R5; Correct stack
              SUB R3, R1
              MRM  R5, (R3); Get return address
              ADD R3, R1
              MRM  R3, (R3); Get old stack frame
              MRR R0, R5; return
            ; END RETURN
          ; END BODY
        ; END METHOD B.printBase
      ; END METHODS OF CLASS B
    ; END CLASS B
    ; CLASS Main
      Main: 
      ; VMT OF CLASS Main
        DAT 1, Main_main
      ; END VMT OF CLASS Main
      ; METHODS OF CLASS Main
        ; METHOD Main.main
          Main_main: 
          ADD R2, R1
          MMR (R2), R3; Save old stack frame
          MRR R3, R2; Current position on stack is new frame
          ; Create space for local variables
            MRI R5, 3
            ADD R2, R5
          ; END Create space for local variables
          ; BODY
            ; ASSIGNMENT
              ; REF TO VARIABLE Main.main.a
                MRI R5, 1
                ADD R5, R3
                ADD R2, R1
                MMR (R2), R5
              ; END REF TO VARIABLE Main.main.a
              ; NEW A
                MRI R5, _end
                MRI R6, 2
                ADD R6, R4
                SUB R5, R6
                ISN R5, R5
                MRI R7, _outOfMemory
                JPC R5, _error
                ADD R2, R1
                MRI R5, A; Get reference to VMT
                MMR (R4), R5; Put VMT Address on Heap
                MMR (R2), R4; Put reference to new object on stack
                MRI R5, 2; Class size
                ADD R4, R5; Inc heap
              ; END NEW A
              MRM  R5, (R2); Take right value from stack
              SUB R2, R1
              MRM  R6, (R2); Take ref to left val from stack
              SUB R2, R1
              MMR (R6), R5; Assign
            ; END ASSIGNMENT
            ; ASSIGNMENT
              ; REF TO VARIABLE Main.main.b
                MRI R5, 2
                ADD R5, R3
                ADD R2, R1
                MMR (R2), R5
              ; END REF TO VARIABLE Main.main.b
              ; NEW B
                MRI R5, _end
                MRI R6, 3
                ADD R6, R4
                SUB R5, R6
                ISN R5, R5
                MRI R7, _outOfMemory
                JPC R5, _error
                ADD R2, R1
                MRI R5, B; Get reference to VMT
                MMR (R4), R5; Put VMT Address on Heap
                MMR (R2), R4; Put reference to new object on stack
                MRI R5, 3; Class size
                ADD R4, R5; Inc heap
              ; END NEW B
              MRM  R5, (R2); Take right value from stack
              SUB R2, R1
              MRM  R6, (R2); Take ref to left val from stack
              SUB R2, R1
              MMR (R6), R5; Assign
            ; END ASSIGNMENT
            ; ASSIGNMENT
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.a
                    MRI R5, 1
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.a
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_34_9_22_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_34_9_22_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; REF TO ATTRIBUTE A.a
                  MRM  R5, (R2)
                  MRI R6, 1
                  ADD R5, R6
                  MMR (R2), R5
                ; END REF TO ATTRIBUTE A.a
              ; END ACCESS
              ; BOX
                ; NEW Integer
                  MRI R5, _end
                  MRI R6, 2
                  ADD R6, R4
                  SUB R5, R6
                  ISN R5, R5
                  MRI R7, _outOfMemory
                  JPC R5, _error
                  ADD R2, R1
                  MRI R5, Integer; Get reference to VMT
                  MMR (R4), R5; Put VMT Address on Heap
                  MMR (R2), R4; Put reference to new object on stack
                  MRI R5, 2; Class size
                  ADD R4, R5; Inc heap
                ; END NEW Integer
                ; 65: Integer
                  MRI R5, 65
                  ADD R2, R1
                  MMR (R2), R5
                ; END 65: Integer
                MRM  R5, (R2); Take value from stack
                SUB R2, R1
                MRM  R6, (R2); Get reference to new object (stays on the stack)
                MRI R7, 1
                ADD R6, R7; Determine location in new object
                MMR (R6), R5; Write value to object
              ; END BOX
              MRM  R5, (R2); Take right value from stack
              SUB R2, R1
              MRM  R6, (R2); Take ref to left val from stack
              SUB R2, R1
              MMR (R6), R5; Assign
            ; END ASSIGNMENT
            ; ASSIGNMENT
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.b
                    MRI R5, 2
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.b
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_35_9_24_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_35_9_24_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; REF TO ATTRIBUTE B.a
                  MRM  R5, (R2)
                  MRI R6, 2
                  ADD R5, R6
                  MMR (R2), R5
                ; END REF TO ATTRIBUTE B.a
              ; END ACCESS
              ; BOX
                ; NEW Integer
                  MRI R5, _end
                  MRI R6, 2
                  ADD R6, R4
                  SUB R5, R6
                  ISN R5, R5
                  MRI R7, _outOfMemory
                  JPC R5, _error
                  ADD R2, R1
                  MRI R5, Integer; Get reference to VMT
                  MMR (R4), R5; Put VMT Address on Heap
                  MMR (R2), R4; Put reference to new object on stack
                  MRI R5, 2; Class size
                  ADD R4, R5; Inc heap
                ; END NEW Integer
                ; 66: Integer
                  MRI R5, 66
                  ADD R2, R1
                  MMR (R2), R5
                ; END 66: Integer
                MRM  R5, (R2); Take value from stack
                SUB R2, R1
                MRM  R6, (R2); Get reference to new object (stays on the stack)
                MRI R7, 1
                ADD R6, R7; Determine location in new object
                MMR (R6), R5; Write value to object
              ; END BOX
              MRM  R5, (R2); Take right value from stack
              SUB R2, R1
              MRM  R6, (R2); Take ref to left val from stack
              SUB R2, R1
              MMR (R6), R5; Assign
            ; END ASSIGNMENT
            ; ASSIGNMENT
              ; REF TO VARIABLE Main.main.c
                MRI R5, 3
                ADD R5, R3
                ADD R2, R1
                MMR (R2), R5
              ; END REF TO VARIABLE Main.main.c
              ; DEREF
                ; REF TO VARIABLE Main.main.b
                  MRI R5, 2
                  ADD R5, R3
                  ADD R2, R1
                  MMR (R2), R5
                ; END REF TO VARIABLE Main.main.b
                MRM  R5, (R2)
                MRM  R5, (R5)
                JPC R5, Main_main_36_14_27_skipNP
                MRI R7, _nullPointer
                MRI R0, _error
                Main_main_36_14_27_skipNP: 
                MMR (R2), R5
              ; END DEREF
              MRM  R5, (R2); Take right value from stack
              SUB R2, R1
              MRM  R6, (R2); Take ref to left val from stack
              SUB R2, R1
              MMR (R6), R5; Assign
            ; END ASSIGNMENT
            ; ASSIGNMENT
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.c
                    MRI R5, 3
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.c
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_37_9_28_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_37_9_28_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; REF TO ATTRIBUTE A.a
                  MRM  R5, (R2)
                  MRI R6, 1
                  ADD R5, R6
                  MMR (R2), R5
                ; END REF TO ATTRIBUTE A.a
              ; END ACCESS
              ; DEREF
                ; ACCESS
                  ; DEREF
                    ; REF TO VARIABLE Main.main.a
                      MRI R5, 1
                      ADD R5, R3
                      ADD R2, R1
                      MMR (R2), R5
                    ; END REF TO VARIABLE Main.main.a
                    MRM  R5, (R2)
                    MRM  R5, (R5)
                    JPC R5, Main_main_37_16_31_skipNP
                    MRI R7, _nullPointer
                    MRI R0, _error
                    Main_main_37_16_31_skipNP: 
                    MMR (R2), R5
                  ; END DEREF
                  ; Parameters
                    
                  ; END Parameters
                  ; REF TO ATTRIBUTE A.a
                    MRM  R5, (R2)
                    MRI R6, 1
                    ADD R5, R6
                    MMR (R2), R5
                  ; END REF TO ATTRIBUTE A.a
                ; END ACCESS
                MRM  R5, (R2)
                MRM  R5, (R5)
                JPC R5, Main_main_37_17_33_skipNP
                MRI R7, _nullPointer
                MRI R0, _error
                Main_main_37_17_33_skipNP: 
                MMR (R2), R5
              ; END DEREF
              MRM  R5, (R2); Take right value from stack
              SUB R2, R1
              MRM  R6, (R2); Take ref to left val from stack
              SUB R2, R1
              MMR (R6), R5; Assign
            ; END ASSIGNMENT
            ; CALL
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.a
                    MRI R5, 1
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.a
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_38_9_34_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_38_9_34_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; DYNAMIC CALL OF METHOD A.print
                  MRI R6, 0
                  SUB R2, R6
                  MRM  R5, (R2); Get VMT
                  ADD R2, R6
                  ADD R2, R1
                  MRI R6, A_38_11_35
                  MMR (R2), R6
                  MRM  R5, (R5)
                  MRI R6, 0
                  ADD R5, R6
                  MRM  R5, (R5)
                  MRR R0, R5
                  A_38_11_35: 
                ; END DYNAMIC CALL OF METHOD A.print
              ; END ACCESS
            ; END CALL
            ; CALL
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.b
                    MRI R5, 2
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.b
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_39_9_36_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_39_9_36_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; DYNAMIC CALL OF METHOD B.print
                  MRI R6, 0
                  SUB R2, R6
                  MRM  R5, (R2); Get VMT
                  ADD R2, R6
                  ADD R2, R1
                  MRI R6, B_39_11_37
                  MMR (R2), R6
                  MRM  R5, (R5)
                  MRI R6, 0
                  ADD R5, R6
                  MRM  R5, (R5)
                  MRR R0, R5
                  B_39_11_37: 
                ; END DYNAMIC CALL OF METHOD B.print
              ; END ACCESS
            ; END CALL
            ; CALL
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.c
                    MRI R5, 3
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.c
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_40_9_38_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_40_9_38_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; DYNAMIC CALL OF METHOD A.print
                  MRI R6, 0
                  SUB R2, R6
                  MRM  R5, (R2); Get VMT
                  ADD R2, R6
                  ADD R2, R1
                  MRI R6, A_40_11_39
                  MMR (R2), R6
                  MRM  R5, (R5)
                  MRI R6, 0
                  ADD R5, R6
                  MRM  R5, (R5)
                  MRR R0, R5
                  A_40_11_39: 
                ; END DYNAMIC CALL OF METHOD A.print
              ; END ACCESS
            ; END CALL
            ; CALL
              ; ACCESS
                ; DEREF
                  ; REF TO VARIABLE Main.main.b
                    MRI R5, 2
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.main.b
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_main_41_9_40_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_main_41_9_40_skipNP: 
                  MMR (R2), R5
                ; END DEREF
                ; Parameters
                  
                ; END Parameters
                ; DYNAMIC CALL OF METHOD B.printBase
                  MRI R6, 0
                  SUB R2, R6
                  MRM  R5, (R2); Get VMT
                  ADD R2, R6
                  ADD R2, R1
                  MRI R6, B_41_11_41
                  MMR (R2), R6
                  MRM  R5, (R5)
                  MRI R6, 1
                  ADD R5, R6
                  MRM  R5, (R5)
                  MRR R0, R5
                  B_41_11_41: 
                ; END DYNAMIC CALL OF METHOD B.printBase
              ; END ACCESS
            ; END CALL
            ; RETURN
              MRI R5, 6
              SUB R2, R5; Correct stack
              SUB R3, R1
              MRM  R5, (R3); Get return address
              ADD R3, R1
              MRM  R3, (R3); Get old stack frame
              MRR R0, R5; return
            ; END RETURN
          ; END BODY
        ; END METHOD Main.main
      ; END METHODS OF CLASS Main
    ; END CLASS Main
    ; CLASS Boolean
      Boolean: 
      ; VMT OF CLASS Boolean
        
      ; END VMT OF CLASS Boolean
      ; METHODS OF CLASS Boolean
        
      ; END METHODS OF CLASS Boolean
    ; END CLASS Boolean
    ; CLASS Integer
      Integer: 
      ; VMT OF CLASS Integer
        
      ; END VMT OF CLASS Integer
      ; METHODS OF CLASS Integer
        
      ; END METHODS OF CLASS Integer
    ; END CLASS Integer
    ; CLASS Object
      Object: 
      ; VMT OF CLASS Object
        
      ; END VMT OF CLASS Object
      ; METHODS OF CLASS Object
        
      ; END METHODS OF CLASS Object
    ; END CLASS Object
  ; END CLASSES
  _stack: ; Start of stack
  DAT 50, 0
  _heap: ; Start of heap
  DAT 100, 0
  _end: ; End of program
; END Program
