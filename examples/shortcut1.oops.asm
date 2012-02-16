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
      MRI R6, Main_0_0_54
      MMR (R2), R6
      MRM  R5, (R5)
      MRI R6, 0
      ADD R5, R6
      MRM  R5, (R5)
      MRR R0, R5
      Main_0_0_54: 
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
    ; CLASS Main
      Main: 
      ; VMT OF CLASS Main
        DAT 1, Main_main
        DAT 1, Main_a
      ; END VMT OF CLASS Main
      ; METHODS OF CLASS Main
        ; METHOD Main.main
          Main_main: 
          ADD R2, R1
          MMR (R2), R3; Save old stack frame
          MRR R3, R2; Current position on stack is new frame
          
          ; BODY
            ; IF
              ; CONDITION
                ; BINARY THEN
                  ; UNBOX
                    ; ACCESS
                      ; DEREF
                        ; REF TO VARIABLE Main.main.SELF
                          MRI R5, -2
                          ADD R5, R3
                          ADD R2, R1
                          MMR (R2), R5
                        ; END REF TO VARIABLE Main.main.SELF
                        MRM  R5, (R2)
                        MRM  R5, (R5)
                        JPC R5, Main_main_4_8_5_skipNP
                        MRI R7, _nullPointer
                        MRI R0, _error
                        Main_main_4_8_5_skipNP: 
                        MMR (R2), R5
                      ; END DEREF
                      ; Parameters
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
                      ; END Parameters
                      ; DYNAMIC CALL OF METHOD Main.a
                        MRI R6, 1
                        SUB R2, R6
                        MRM  R5, (R2); Get VMT
                        ADD R2, R6
                        ADD R2, R1
                        MRI R6, Main_4_8_6
                        MMR (R2), R6
                        MRM  R5, (R5)
                        MRI R6, 1
                        ADD R5, R6
                        MRM  R5, (R5)
                        MRR R0, R5
                        Main_4_8_6: 
                      ; END DYNAMIC CALL OF METHOD Main.a
                    ; END ACCESS
                    MRM  R5, (R2); Get reference to object from stack
                    MRI R6, 1
                    ADD R5, R6; Determine address of value
                    MRM  R5, (R5); Read value
                    MMR (R2), R5; Write to stack
                  ; END UNBOX
                  MRM  R5, (R2)
                  ; SHORTCUT AND
                    ISZ R5, R5
                    JPC R5, Main_main_0_0_11
                    SUB R2, R1
                  ; END SHORTCUT AND
                  ; BINARY THEN
                    ; 0: Boolean
                      MRI R5, 0
                      ADD R2, R1
                      MMR (R2), R5
                    ; END 0: Boolean
                    MRM  R5, (R2)
                    ; SHORTCUT AND
                      ISZ R5, R5
                      JPC R5, Main_main_0_0_10
                      SUB R2, R1
                    ; END SHORTCUT AND
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_4_38_8_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_4_38_8_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 88: Integer
                              MRI R5, 88
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 88: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_4_38_9
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_4_38_9: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    Main_main_0_0_10: 
                  ; END BINARY THEN
                  Main_main_0_0_11: 
                ; END BINARY THEN
                MRM  R5, (R2); Get condition from stack
                SUB R2, R1
                ISZ R5, R5; if 0 then
                JPC R5, Main_main_0_0_12_else; jump to END IF
              ; END CONDITION
              ; THEN
                ; 
                  ; WRITE
                    ; 88: Integer
                      MRI R5, 88
                      ADD R2, R1
                      MMR (R2), R5
                    ; END 88: Integer
                    MRM  R5, (R2)
                    SUB R2, R1
                    SYS 1, 5
                  ; END WRITE
                ; END 
                MRI R0, Main_main_0_0_13_endIf
              ; END THEN
              ; ELSE
                Main_main_0_0_12_else: 
                ; ELSE BODY
                  
                ; END ELSE BODY
              ; END ELSE
              Main_main_0_0_13_endIf: 
            ; END IF
            ; IF
              ; CONDITION
                ; BINARY ELSE
                  ; UNBOX
                    ; ACCESS
                      ; DEREF
                        ; REF TO VARIABLE Main.main.SELF
                          MRI R5, -2
                          ADD R5, R3
                          ADD R2, R1
                          MMR (R2), R5
                        ; END REF TO VARIABLE Main.main.SELF
                        MRM  R5, (R2)
                        MRM  R5, (R5)
                        JPC R5, Main_main_7_8_14_skipNP
                        MRI R7, _nullPointer
                        MRI R0, _error
                        Main_main_7_8_14_skipNP: 
                        MMR (R2), R5
                      ; END DEREF
                      ; Parameters
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
                      ; END Parameters
                      ; DYNAMIC CALL OF METHOD Main.a
                        MRI R6, 1
                        SUB R2, R6
                        MRM  R5, (R2); Get VMT
                        ADD R2, R6
                        ADD R2, R1
                        MRI R6, Main_7_8_15
                        MMR (R2), R6
                        MRM  R5, (R5)
                        MRI R6, 1
                        ADD R5, R6
                        MRM  R5, (R5)
                        MRR R0, R5
                        Main_7_8_15: 
                      ; END DYNAMIC CALL OF METHOD Main.a
                    ; END ACCESS
                    MRM  R5, (R2); Get reference to object from stack
                    MRI R6, 1
                    ADD R5, R6; Determine address of value
                    MRM  R5, (R5); Read value
                    MMR (R2), R5; Write to stack
                  ; END UNBOX
                  MRM  R5, (R2)
                  ; SHORTCUT THEN
                    JPC R5, Main_main_0_0_19
                    SUB R2, R1
                  ; END SHORTCUT THEN
                  ; UNBOX
                    ; ACCESS
                      ; DEREF
                        ; REF TO VARIABLE Main.main.SELF
                          MRI R5, -2
                          ADD R5, R3
                          ADD R2, R1
                          MMR (R2), R5
                        ; END REF TO VARIABLE Main.main.SELF
                        MRM  R5, (R2)
                        MRM  R5, (R5)
                        JPC R5, Main_main_7_22_17_skipNP
                        MRI R7, _nullPointer
                        MRI R0, _error
                        Main_main_7_22_17_skipNP: 
                        MMR (R2), R5
                      ; END DEREF
                      ; Parameters
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
                          ; 88: Integer
                            MRI R5, 88
                            ADD R2, R1
                            MMR (R2), R5
                          ; END 88: Integer
                          MRM  R5, (R2); Take value from stack
                          SUB R2, R1
                          MRM  R6, (R2); Get reference to new object (stays on the stack)
                          MRI R7, 1
                          ADD R6, R7; Determine location in new object
                          MMR (R6), R5; Write value to object
                        ; END BOX
                      ; END Parameters
                      ; DYNAMIC CALL OF METHOD Main.a
                        MRI R6, 1
                        SUB R2, R6
                        MRM  R5, (R2); Get VMT
                        ADD R2, R6
                        ADD R2, R1
                        MRI R6, Main_7_22_18
                        MMR (R2), R6
                        MRM  R5, (R5)
                        MRI R6, 1
                        ADD R5, R6
                        MRM  R5, (R5)
                        MRR R0, R5
                        Main_7_22_18: 
                      ; END DYNAMIC CALL OF METHOD Main.a
                    ; END ACCESS
                    MRM  R5, (R2); Get reference to object from stack
                    MRI R6, 1
                    ADD R5, R6; Determine address of value
                    MRM  R5, (R5); Read value
                    MMR (R2), R5; Write to stack
                  ; END UNBOX
                  Main_main_0_0_19: 
                ; END BINARY ELSE
                MRM  R5, (R2); Get condition from stack
                SUB R2, R1
                ISZ R5, R5; if 0 then
                JPC R5, Main_main_0_0_20_else; jump to END IF
              ; END CONDITION
              ; THEN
                ; 
                  ; WRITE
                    ; 67: Integer
                      MRI R5, 67
                      ADD R2, R1
                      MMR (R2), R5
                    ; END 67: Integer
                    MRM  R5, (R2)
                    SUB R2, R1
                    SYS 1, 5
                  ; END WRITE
                ; END 
                MRI R0, Main_main_0_0_21_endIf
              ; END THEN
              ; ELSE
                Main_main_0_0_20_else: 
                ; ELSE BODY
                  
                ; END ELSE BODY
              ; END ELSE
              Main_main_0_0_21_endIf: 
            ; END IF
            ; IF
              ; CONDITION
                ; BINARY ELSE
                  ; UNARY NOT
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_10_12_22_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_10_12_22_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 68: Integer
                              MRI R5, 68
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 68: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_10_12_23
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_10_12_23: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    MRM  R5, (R2)
                    ; NOT
                      ISZ R5, R5
                      MMR (R2), R5
                    ; END NOT
                  ; END UNARY NOT
                  MRM  R5, (R2)
                  ; SHORTCUT THEN
                    JPC R5, Main_main_0_0_27
                    SUB R2, R1
                  ; END SHORTCUT THEN
                  ; UNARY NOT
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_10_30_25_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_10_30_25_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 69: Integer
                              MRI R5, 69
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 69: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_10_30_26
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_10_30_26: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    MRM  R5, (R2)
                    ; NOT
                      ISZ R5, R5
                      MMR (R2), R5
                    ; END NOT
                  ; END UNARY NOT
                  Main_main_0_0_27: 
                ; END BINARY ELSE
                MRM  R5, (R2); Get condition from stack
                SUB R2, R1
                ISZ R5, R5; if 0 then
                JPC R5, Main_main_0_0_28_else; jump to END IF
              ; END CONDITION
              ; THEN
                ; 
                  ; WRITE
                    ; 88: Integer
                      MRI R5, 88
                      ADD R2, R1
                      MMR (R2), R5
                    ; END 88: Integer
                    MRM  R5, (R2)
                    SUB R2, R1
                    SYS 1, 5
                  ; END WRITE
                ; END 
                MRI R0, Main_main_0_0_29_endIf
              ; END THEN
              ; ELSE
                Main_main_0_0_28_else: 
                ; ELSE BODY
                  
                ; END ELSE BODY
              ; END ELSE
              Main_main_0_0_29_endIf: 
            ; END IF
            ; IF
              ; CONDITION
                ; BINARY THEN
                  ; UNBOX
                    ; ACCESS
                      ; DEREF
                        ; REF TO VARIABLE Main.main.SELF
                          MRI R5, -2
                          ADD R5, R3
                          ADD R2, R1
                          MMR (R2), R5
                        ; END REF TO VARIABLE Main.main.SELF
                        MRM  R5, (R2)
                        MRM  R5, (R5)
                        JPC R5, Main_main_13_8_30_skipNP
                        MRI R7, _nullPointer
                        MRI R0, _error
                        Main_main_13_8_30_skipNP: 
                        MMR (R2), R5
                      ; END DEREF
                      ; Parameters
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
                          ; 70: Integer
                            MRI R5, 70
                            ADD R2, R1
                            MMR (R2), R5
                          ; END 70: Integer
                          MRM  R5, (R2); Take value from stack
                          SUB R2, R1
                          MRM  R6, (R2); Get reference to new object (stays on the stack)
                          MRI R7, 1
                          ADD R6, R7; Determine location in new object
                          MMR (R6), R5; Write value to object
                        ; END BOX
                      ; END Parameters
                      ; DYNAMIC CALL OF METHOD Main.a
                        MRI R6, 1
                        SUB R2, R6
                        MRM  R5, (R2); Get VMT
                        ADD R2, R6
                        ADD R2, R1
                        MRI R6, Main_13_8_31
                        MMR (R2), R6
                        MRM  R5, (R5)
                        MRI R6, 1
                        ADD R5, R6
                        MRM  R5, (R5)
                        MRR R0, R5
                        Main_13_8_31: 
                      ; END DYNAMIC CALL OF METHOD Main.a
                    ; END ACCESS
                    MRM  R5, (R2); Get reference to object from stack
                    MRI R6, 1
                    ADD R5, R6; Determine address of value
                    MRM  R5, (R5); Read value
                    MMR (R2), R5; Write to stack
                  ; END UNBOX
                  MRM  R5, (R2)
                  ; SHORTCUT AND
                    ISZ R5, R5
                    JPC R5, Main_main_0_0_39
                    SUB R2, R1
                  ; END SHORTCUT AND
                  ; BINARY AND
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_13_23_33_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_13_23_33_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 71: Integer
                              MRI R5, 71
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 71: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_13_23_34
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_13_23_34: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_13_33_36_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_13_33_36_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 72: Integer
                              MRI R5, 72
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 72: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_13_33_37
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_13_33_37: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    MRM  R6, (R2)
                    SUB R2, R1
                    MRM  R5, (R2)
                    AND R5, R6
                    MMR (R2), R5
                  ; END BINARY AND
                  Main_main_0_0_39: 
                ; END BINARY THEN
                MRM  R5, (R2); Get condition from stack
                SUB R2, R1
                ISZ R5, R5; if 0 then
                JPC R5, Main_main_0_0_40_else; jump to END IF
              ; END CONDITION
              ; THEN
                ; 
                  ; WRITE
                    ; 73: Integer
                      MRI R5, 73
                      ADD R2, R1
                      MMR (R2), R5
                    ; END 73: Integer
                    MRM  R5, (R2)
                    SUB R2, R1
                    SYS 1, 5
                  ; END WRITE
                ; END 
                MRI R0, Main_main_0_0_41_endIf
              ; END THEN
              ; ELSE
                Main_main_0_0_40_else: 
                ; ELSE BODY
                  
                ; END ELSE BODY
              ; END ELSE
              Main_main_0_0_41_endIf: 
            ; END IF
            ; IF
              ; CONDITION
                ; BINARY THEN
                  ; 0: Boolean
                    MRI R5, 0
                    ADD R2, R1
                    MMR (R2), R5
                  ; END 0: Boolean
                  MRM  R5, (R2)
                  ; SHORTCUT AND
                    ISZ R5, R5
                    JPC R5, Main_main_0_0_49
                    SUB R2, R1
                  ; END SHORTCUT AND
                  ; BINARY OR
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_16_24_43_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_16_24_43_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 88: Integer
                              MRI R5, 88
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 88: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_16_24_44
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_16_24_44: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    ; UNBOX
                      ; ACCESS
                        ; DEREF
                          ; REF TO VARIABLE Main.main.SELF
                            MRI R5, -2
                            ADD R5, R3
                            ADD R2, R1
                            MMR (R2), R5
                          ; END REF TO VARIABLE Main.main.SELF
                          MRM  R5, (R2)
                          MRM  R5, (R5)
                          JPC R5, Main_main_16_33_46_skipNP
                          MRI R7, _nullPointer
                          MRI R0, _error
                          Main_main_16_33_46_skipNP: 
                          MMR (R2), R5
                        ; END DEREF
                        ; Parameters
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
                            ; 89: Integer
                              MRI R5, 89
                              ADD R2, R1
                              MMR (R2), R5
                            ; END 89: Integer
                            MRM  R5, (R2); Take value from stack
                            SUB R2, R1
                            MRM  R6, (R2); Get reference to new object (stays on the stack)
                            MRI R7, 1
                            ADD R6, R7; Determine location in new object
                            MMR (R6), R5; Write value to object
                          ; END BOX
                        ; END Parameters
                        ; DYNAMIC CALL OF METHOD Main.a
                          MRI R6, 1
                          SUB R2, R6
                          MRM  R5, (R2); Get VMT
                          ADD R2, R6
                          ADD R2, R1
                          MRI R6, Main_16_33_47
                          MMR (R2), R6
                          MRM  R5, (R5)
                          MRI R6, 1
                          ADD R5, R6
                          MRM  R5, (R5)
                          MRR R0, R5
                          Main_16_33_47: 
                        ; END DYNAMIC CALL OF METHOD Main.a
                      ; END ACCESS
                      MRM  R5, (R2); Get reference to object from stack
                      MRI R6, 1
                      ADD R5, R6; Determine address of value
                      MRM  R5, (R5); Read value
                      MMR (R2), R5; Write to stack
                    ; END UNBOX
                    MRM  R6, (R2)
                    SUB R2, R1
                    MRM  R5, (R2)
                    OR R5, R6
                    MMR (R2), R5
                  ; END BINARY OR
                  Main_main_0_0_49: 
                ; END BINARY THEN
                MRM  R5, (R2); Get condition from stack
                SUB R2, R1
                ISZ R5, R5; if 0 then
                JPC R5, Main_main_0_0_50_else; jump to END IF
              ; END CONDITION
              ; THEN
                ; 
                  ; WRITE
                    ; 88: Integer
                      MRI R5, 88
                      ADD R2, R1
                      MMR (R2), R5
                    ; END 88: Integer
                    MRM  R5, (R2)
                    SUB R2, R1
                    SYS 1, 5
                  ; END WRITE
                ; END 
                MRI R0, Main_main_0_0_51_endIf
              ; END THEN
              ; ELSE
                Main_main_0_0_50_else: 
                ; ELSE BODY
                  
                ; END ELSE BODY
              ; END ELSE
              Main_main_0_0_51_endIf: 
            ; END IF
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
        ; END METHOD Main.main
        ; METHOD Main.a
          Main_a: 
          ADD R2, R1
          MMR (R2), R3; Save old stack frame
          MRR R3, R2; Current position on stack is new frame
          
          ; BODY
            ; WRITE
              ; UNBOX
                ; DEREF
                  ; REF TO VARIABLE Main.a.a
                    MRI R5, -2
                    ADD R5, R3
                    ADD R2, R1
                    MMR (R2), R5
                  ; END REF TO VARIABLE Main.a.a
                  MRM  R5, (R2)
                  MRM  R5, (R5)
                  JPC R5, Main_a_23_11_53_skipNP
                  MRI R7, _nullPointer
                  MRI R0, _error
                  Main_a_23_11_53_skipNP: 
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
              ; BOX
                ; NEW Boolean
                  MRI R5, _end
                  MRI R6, 2
                  ADD R6, R4
                  SUB R5, R6
                  ISN R5, R5
                  MRI R7, _outOfMemory
                  JPC R5, _error
                  ADD R2, R1
                  MRI R5, Boolean; Get reference to VMT
                  MMR (R4), R5; Put VMT Address on Heap
                  MMR (R2), R4; Put reference to new object on stack
                  MRI R5, 2; Class size
                  ADD R4, R5; Inc heap
                ; END NEW Boolean
                ; 1: Boolean
                  MRI R5, 1
                  ADD R2, R1
                  MMR (R2), R5
                ; END 1: Boolean
                MRM  R5, (R2); Take value from stack
                SUB R2, R1
                MRM  R6, (R2); Get reference to new object (stays on the stack)
                MRI R7, 1
                ADD R6, R7; Determine location in new object
                MMR (R6), R5; Write value to object
              ; END BOX
              MRM  R6, (R2); Take ref to return value from stack
              MRI R5, 4
              SUB R2, R5; Correct stack
              MMR (R2), R6; Write ref to return value to stack
              SUB R3, R1
              MRM  R5, (R3); Get return address
              ADD R3, R1
              MRM  R3, (R3); Get old stack frame
              MRR R0, R5; return
            ; END RETURN
          ; END BODY
        ; END METHOD Main.a
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
