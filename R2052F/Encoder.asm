TITLE Encoder
 _______________________________________________________________________________________
;;
 After prefix(es), encoding opcode, modR/M byte.
 
 SIB, displacement and immediate temporary stored before in parameters analyzes
 will be written after returning from here.

 This might be the more surprising part of RosAsm: The Assembler is *not* Table(s)
 driven (zero Table). More Code / Less Data. But also, much much faster, because
 all this no end Cases selection works in fact a bit like a tree search. Some Cases
 have their specific treatements. The first Checking apply upon the number of Chars.
 After this, the chars Cases parsing works like a Tree.

 This organisation advantages, once the killing typing work is done, are: Speed,
 flexibility in holding any weird coding case, ease of maintainance, even with
 few knowledge.
;;
 _______________________________________________________________________________________


[Absolute | test #1 0_8000_0000 | jz M0> | neg #1 | M0: ]


Encode:
    mov eax D$imm32, D$ABSimm32 eax ;| Absolute eax | mov D$ABSimm32 eax
; sIm
    and eax 0_FFFF_FF80
    If eax = 0_FFFF_FF80
        and D$ABSimm32 0_FF | mov B$SignedImm8 &TRUE
    Else_If D$imm32 < 080
        mov B$SignedImm8 &TRUE
    Else
        mov B$SignedImm8 &FALSE
    End_If

    mov esi D$LineStart,  edi D$CodeListPtr

    call Store8cars

    ; First, check for special encodings (send down here)

    On ah = 'J',  jmp Letter_J

    On B$esi+3 = '_', jmp J_Branching

    ifnot op1 'M',  L1>
      ifnot op2 'O', L1>
        ifnot op3 'V', L1>
          jmp MOVinstructions

L1: ifnot op1 'S', L1>
      ifnot op2 'E', L1>
        ifnot op3 'T',  L1>
          jmp SETinstructions

L1: ifnot op1 'C', L1>>
      ifnot op2 'M', L1>>
        ifnot op3 'O', L3>
          ifnot op4 'V', L1>
            jmp CMOVinstructions

L3:     cmp op6 Separators | jb L1>   ; CMPXCHG / CMPXCHG8B

        On op4 = 'X', jmp L1>

        Ifnot op3 'P', L1>
          ifnot op4 'P', L4>
            ifnot op5 'S', L5>        ; CMPPS... with given Condition (ex: CMP_SS_LT)
              jmp XMMcomparePS
L5:         ifnot op5 'D', L1>        ; CMPPD...
              jmp XMMcomparePD
L4:       ifnot op4 'S', L4>
            ifnot op5 'S', L5>        ; CMPSS...
              jmp XMMcompareSS
L5:         ifnot op5 'D', L1>        ; CMPSD...
              jmp XMMcompareSD        ; CMPSD with trailing imm is hold by (REP) CMPSD

L4:       ifnot op4 'E', L4>
            jmp TrySSE2
L4:       ifnot op4 'L', L4>
            jmp TrySSE2
L4:       ifnot op4 'N', L4>
            jmp TrySSE2
L4:       ifnot op4 'O', L4>
            jmp TrySSE2
L4:       ifnot op4 'U', L1>
            jmp TrySSE2


L1: ifnot op1 'F', L1>
      jmp Math

L1:    On op3 > Separators,  jmp TreeLetters

L1: ; Two Letters mnemonics:

      ifnot op1 'B', L1>
        ifnot op2 'T', L2>                       ; BT
          mov op1 001111
          ifnot B$Operands ImmToReg, L3>
            mov op2 00_1011_1010,  op3 00_1110_0000 | jmp op_op_reg1_imm8
L3:       ifnot B$Operands ImmToMem, L3>
            mov op2 00_1011_1010,  op3 00_0010_0000 | jmp op_op_modRm_imm8
L3:       ifnot B$Operands RegToReg, L3>
            mov op2 00_1010_0011,  op3 00_1100_0000 | jmp op_op_reg2reg1
L3:       ifnot B$Operands RegToMem, L3>
            mov op2 00_1010_0011 | jmp op_op_modReg2Rm
L3:       BadOperand
L2:     BadMnemonic
L1:   ifnot op1 'I', L1>
        ifnot op2 'N', L2>                        ; IN
          ifnot B$FirstGender reg, L4>
            ifnot B$FirstReg 0, L4>           ; reg = 0  for eax, ax, al
          ifnot B$SecondGender imm, L3>
            mov op1 00_1110_0100 | jmp w_imm8
L3:       ifnot B$SecondGender reg, L4>
            ifnot B$SecondReg RegDX, L4>
              ifnot B$SecondOperandwBit WordSize, L4>
                If B$FirstOperandwBit = ByteSize
                    LastOpCode 0EC
                Else                 ; Direct output because of "in al/eax DX".
                    LastOpcode 0ED
                End_If
L4:       BadOperand
L2:     BadMnemonic
L1:   ifnot op1 'O', L1>>
        ifnot op2 'R', L2>>                                 ; OR
          ifnot B$Operands RegToReg, L3>
            mov op1 00_1010,  op2 00_1100_0000 | jmp w_reg1reg2
L3:       ifnot B$Operands MemToReg, L3>
            mov op1 00_1010 | jmp w_modReg1Rm
L3:       ifnot B$Operands RegToMem, L3>
            mov op1 00_1000 | jmp w_modReg2Rm
L3:       ifnot B$Operands ImmToReg, L3>
            ifnot B$FirstReg RegEAX, L4>
              If B$FirstOperandwBit > ByteSize
                cmp B$LabelInside &TRUE | je L5>
                cmp B$SignedImm8 &TRUE | je L4>
                ;cmp D$ABSimm32 080 | jb L4>
              End_If
L5:           mov op1 00_1100 | jmp w_imm
L4:         mov op1 00_1000_0000,  op2 00_1100_1000 | jmp sw_reg1_imm
L3:       ifnot B$Operands ImmToMem, L3>
            mov op1 00_1000_0000,  op2 00_1000 | jmp sw_modRm_imm
L3:       BadOperand
L2:     ;BadMnemonic
L1:   BadMnemonic

TreeLetters:  On op4 > Separators,  jmp FourLetters

        ifnot op1 'A', L1>>
          ifnot op2 'A', L2>>
            ifnot op3 'A', L3>
              mov op1 0011_0111 | jmp OP                             ; AAA
L3:         ifnot op3 'D', L3>
                mov op1 00_1101_0101
                If B$ParametersNumber = 1
                    jmp Op_imm8
                Else
                    mov op2 00_1010 | jmp OP_OP    ; AAD
                End_If
L3:         ifnot op3 'M', L3>
                mov op1 00_1101_0100
                If B$ParametersNumber = 1
                    jmp Op_imm8
                Else
                    mov op2 00_1010 | jmp OP_OP    ; AAM
                End_If
L3:         ifnot op3 'S', L3>
               mov op1 0011_1111 | jmp OP                            ; AAS
L3:         BadMnemonic
L2:       ifnot op2 'D', L2>>
            ifnot op3 'C', L3>>  ; >                                  ; ADC
              ifnot B$Operands RegToReg, L4>
                mov op1 0001_0010,  op2 00_1100_0000 | jmp w_reg1reg2
L4:           ifnot B$Operands MemToReg, L4>
                mov op1 0001_0010 | jmp w_modReg1Rm
L4:           ifnot B$Operands RegToMem, L4>
                mov op1 0001_0000 | jmp w_modReg2Rm
L4:           ifnot B$Operands ImmToReg, L4>
                ifnot B$FirstReg RegEAX, L5>
                  If B$FirstOperandwBit > ByteSize
                    cmp B$LabelInside &TRUE | je L6>
                    cmp B$SignedImm8 &TRUE | je L5>
                    ;cmp D$ABSimm32 080 | jb L5>
                  End_If
L6:               mov op1 0001_0100 | jmp w_imm
L5:             mov op1 00_1000_0000,  op2 00_1101_0000 | jmp sw_reg1_imm
L4:           ifnot B$Operands ImmToMem, L4>
                mov op1 00_1000_0000,  op2 0001_0000 | jmp sw_modRm_imm
L4:           BadOperand

L3:         ifnot op3 'D', L3>>                               ; ADD  ; sub
              ifnot B$Operands RegToReg, L4>
                mov op1 0010,  op2 00_1100_0000 | jmp w_reg1reg2
L4:           ifnot B$Operands MemToReg, L4>
                mov op1 0010 | jmp w_modReg1Rm
L4:           ifnot B$Operands RegToMem, L4>
                mov op1 0 | jmp w_modReg2Rm
L4:           ifnot B$Operands ImmToReg, L4>
                ifnot B$FirstReg RegEAX, L5>
                  If B$FirstOperandwBit > ByteSize
                    cmp B$LabelInside &TRUE | je L6>
                    cmp B$SignedImm8 &TRUE | je L5>
                    ;cmp D$ABSimm32 080 | jb L5>
                  End_If
L6:               mov op1 00_0100 | jmp w_imm

L5:             mov op1 00_1000_0000,  op2 00_1100_0000 | jmp sw_reg1_imm

L4:           ifnot B$Operands ImmToMem, L4>
                mov op1 00_1000_0000,  op2 0 | jmp sw_modRm_imm
L4:           BadOperand
L3:         BadMnemonic
L2:       ifnot op2 'N', L2>>
            ifnot op3 'D', L3>>                        ; AND
              ifnot B$Operands RegToReg, L4>
                mov op1 0010_0010,  op2 00_1100_0000 | jmp w_reg1reg2
L4:           ifnot B$Operands MemToReg, L4>
                mov op1 0010_0010 | jmp w_modReg1Rm
L4:           ifnot B$Operands RegToMem, L4>
                mov op1 0010_0000 | jmp w_modReg2Rm
L4:           ifnot B$Operands ImmToReg, L4>
                ifnot B$FirstReg RegEAX, L5>
                  If B$FirstOperandwBit > ByteSize
                    cmp B$LabelInside &TRUE | je L6>
                    cmp B$SignedImm8 &TRUE | je L5>
                    ;cmp D$ABSimm32 080 | jb L5>
                  End_If
L6:               mov op1 0010_0100 | jmp w_imm
                ; immToreg missing in intel doc > verify:
L5:             mov op1 00_1000_0000,  op2 00_1110_0000 | jmp sw_Reg1_imm
L4:           ifnot B$Operands ImmToMem, L4>
                mov op1 00_1000_0000,  op2 0010_0000 | jmp sw_modRm_imm
L4:           BadOperand
L3:         BadMnemonic
L2:       BadMnemonic

L1:     ifnot op1 'B', L1>>
          ifnot op2 'S', L2>>
            ifnot op3 'F', L3>                         ; BSF
              ifnot B$Operands RegToReg, L4>
                mov op1 00_1111,  op2 00_1011_1100,  op3 00_1100_0000
                jmp op_op_reg1reg2                         ; intel doc: reverse order (???)
L4:           ifnot B$Operands MemToReg, L4>
                mov op1 00_1111,  op2 00_1011_1100 | jmp op_op_modreg1Rm
L4:           BadOperand
L3:         ifnot op3 'R', L3>                                     ; BSR
              ifnot B$Operands RegToreg, L4>
                mov op1 00_1111,  op2 00_1011_1101,  op3 00_1100_0000
                jmp op_op_reg1reg2                         ; intel doc: reverse order (???)
L4:           ifnot B$Operands MemToreg, L4>
                mov op1 00_1111,  op2 00_1011_1101 | jmp op_op_modReg1Rm
L4:           BadOperand
L3:         BadMnemonic
L2:       ifnot op2 'T', L2>>
            ifnot op3 'C', L3>                          ; BTC
              mov op1 00_1111,  op2 00_1011_1010
              ifnot B$Operands ImmToReg, L4>
                mov op3 00_1111_1000 | jmp op_op_reg1_imm8
L4:           ifnot B$Operands ImmToMem, L4>
                mov op3 0011_1000 | jmp op_op_modRm_imm8
L4:           mov op2 00_1011_1011
              ifnot B$Operands RegToreg, L4>
                mov op3 00_1100_0000 | jmp op_op_reg2reg1
L4:           ifnot B$Operands RegToMem, L4>
                jmp op_op_modReg2Rm
L4:           BadOperand
L3:         ifnot op3 'R', L3>                          ; BTR
              mov op1 00_1111
              ifnot B$Operands ImmToreg, L4>
                mov op2 00_1011_1010,  op3 00_1111_0000 | jmp op_op_reg1_imm8
L4:           ifnot B$Operands ImmToMem, L4>
                mov op2 00_1011_1010,  op3 0011_0000 | jmp op_op_modRm_imm8
L4:           ifnot B$Operands RegToreg, L4>
                mov op2 00_1011_0011,  op3 00_1100_0000 | jmp op_op_reg2reg1
L4:           ifnot B$Operands RegToMem, L4>
                mov op2 00_1011_0011 | jmp op_op_modReg2Rm
L4:           BadOperand
L3:         ifnot op3 'S', L3>                          ; BTS
              mov op1 00_1111
              ifnot B$Operands ImmToreg, L4>
                mov op2 00_1011_1010,  op3 00_1110_1000 | jmp op_op_reg1_imm8
L4:           ifnot B$Operands ImmToMem, L4>
                mov op2 00_1011_1010,  op3 0010_1000 | jmp op_op_modRm_imm8
L4:           ifnot B$Operands RegToreg, L4>
                mov op2 00_1010_1011,  op3 00_1100_0000 | jmp op_op_reg2reg1
L4:           ifnot B$Operands RegToMem, L4>
                mov op2 00_1010_1011 | jmp op_op_modReg2Rm
L4:           BadOperand
L3:         ;BadMnemonic
L2:       BadMnemonic

L1:     ifnot op1 'C', L1>>
          ifnot op2 'B', L2>
            ifnot op3 'W', L3>                 ; CBW
              mov op1 066,  op2 00_1001_1000 | jmp op_op
L3:         BadMnemonic
L2:       ifnot op2 'D', L2>
            ifnot op3 'Q', L3>                 ; CDQ
              mov op1 00_1001_1001 | jmp op
L3:         BadMnemonic
L2:       ifnot op2 'L', L2>
            ifnot op3 'C', L3>                 ; CLC
              mov op1 00_1111_1000 | jmp op
L3:         ifnot op3 'D',  L3>                 ; CLD
              mov op1 00_1111_1100 | jmp op
L3:         ifnot op3 'I', L3>                 ; CLI
              mov op1 00_1111_1010 | jmp op
L3:         BadMnemonic
L2:       ifnot op2 'M', L2>>
            ifnot op3 'C', L3>                 ; CMC
              mov op1 00_1111_0101 | jmp op
L3:         ifnot op3 'P',  L3>>                 ; CMP
              ifnot B$Operands RegToReg, L4>
                mov op1 0011_1010,  op2 00_1100_0000 | jmp w_reg1reg2
L4:           ifnot B$Operands MemToReg, L4>
                mov op1 0011_1010 | jmp w_modReg1Rm
L4:           ifnot B$Operands RegToMem, L4>
                mov op1 0011_1000 | jmp w_modReg2Rm
L4:           ifnot B$Operands ImmToReg, L4>

                ifnot B$FirstReg RegEAX, L5>
                  If B$FirstOperandwBit = ByteSize
                     mov op1 03C
                  Else
                      cmp B$LabelInside &TRUE | je L6>
                      cmp B$SignedImm8 &TRUE | je L5>
                      ;cmp D$ABSimm32 080 | jb L5>
L6:                   mov op1 03D
                  End_If
                  jmp w_imm

L5:             mov op1 00_1000_0000,  op2 00_1111_1000 | jmp sw_reg1_imm
L4:           ifnot B$Operands ImmToMem, L4>
                mov op1 00_1000_0000,  op2 0011_1000 | jmp sw_modRm_imm
L4:           BadOperand
L3:         BadMnemonic
L2:       ifnot op2 'W', L2>
            ifnot op3 'D', L3>                ; CWD
              mov op1 066,  op2 00_1001_1001 | jmp op_op
L3:         ;BadMnemonic
L2:       BadMnemonic
L1:     ifnot op1 'D', L1>>
          ifnot op2 'A', L2>
            ifnot op3 'A', L3>               ; DAA
              mov op1 0010_0111 | jmp op
L3:         ifnot op3 'S', L3>
              mov op1 0010_1111 | jmp op
L3:         BadMnemonic
L2:     ifnot op2 'E', L2>
          ifnot op3 'C', L3>                  ; DEC
            ifnot B$FirstGender reg, L4>
              cmp B$wBit ByteSize | jne L5>
              mov op1 00_1111_1110,  op2 00_1100_1000 | jmp w_reg1    ; alternate shorter >
L5:           mov op1 0_0100_1000 | jmp reg1_in_op                     ; only full size
L4:         ifnot B$FirstGender mem, L4>
              mov op1 00_1111_1110,  op2 00_1000 | jmp w_modRm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'I', L2>
          ifnot op3 'V', L3>                  ; DIV
            mov op1 00_1111_0110
            ifnot B$FirstGender reg, L4>
              mov op2 00_1111_0000 | jmp w_reg1
L4:         ifnot B$FirstGender mem, L4>
              mov op2 0011_0000 | jmp w_modRm
L4:         BadOperand
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'H', L1>
        ifnot op2 'L', L2>
          ifnot op3 'T', L3>                  ; HLT
            mov op1 00_1111_0100 | jmp op
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'I', L1>
        ifnot op2 'N', L2>
          ifnot op3 'C', L3>                 ; INC
            ifnot B$FirstGender Reg, L4>
              cmp B$wBit ByteSize | jne L5>
              mov op1 00_1111_1110 | mov op2 00_1100_0000 | jmp w_reg1 ; alternate shorter >
L5:           mov op1 0_0100_0000 | jmp reg1_in_op                      ; only full size
L4:         ifnot B$FirstGender mem, L4>
              mov op1 00_1111_1110 | mov op2 0 | jmp w_modRm
L4:         BadOperand
;L3:       ifnot op3 'S', L3>                  ; INS
;            mov op1 00_0110_1100 | jmp OPw
L3:       ifnot op3 'T', L3>                  ; INT
            ifnot D$imm32 3, L4>
              mov B$immInside &False
              mov op1 00_1100_1100 | jmp op_P1          ; INT 3
L4:         mov op1 00_1100_1101 | jmp op_imm8     ; INT n
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'L', L1>>
        ifnot op2 'A', L2>
          ifnot op3 'R', L3>                  ; LAR
            mov op1 00_1111,  op2 0010
            ifnot B$Operands RegToReg, L4>
              mov op3 00_1100_0000 | jmp op_op_reg1reg2
L4:         ifnot B$Operands MemToreg, L4>
              jmp op_op_modReg1Rm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'D', L2>
          ifnot op3 'S', L3>                     ; LDS
            ifnot B$Operands MemToReg, L4>
              mov op1 00_1100_0101 | jmp op_modReg1Rm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'E', L2>
          ifnot op3 'A', L3>                       ; LEA
            ifnot B$Operands MemToReg, L4>
            On B$FirstOperandwBit = ByteSize, error D$LeaSizePtr
              mov op1 00_1000_1101 | jmp op_modReg1Rm
L4:         Error D$LeaTypesPtr
L3:       ifnot op3 'S', L3>                       ; LES
            ifnot B$Operands MemToReg, L4>
              mov op1 00_1100_0100 | jmp op_modReg1Rm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'F', L2>
          ifnot op3 'S', L3>                       ; LFS
            ifnot B$Operands MemToReg, L4>
              mov op1 00_1111
               mov op2 00_1011_0100 | jmp op_op_modReg1Rm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'G', L2>
          ifnot op3 'S', L3>                       ; LGS
            ifnot B$Operands MemToReg, L4>
              mov op1 00_1111
               mov op2 00_1011_0101 | jmp op_op_modReg1Rm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'S', L2>
          ifnot op3 'L', L3>
            mov op1 00_1111,  op2 0011
            ifnot B$Operands RegToReg, L4>             ; LSL
              mov op3 00_1100_0000 | jmp op_op_reg1reg2
L4:         ifnot B$Operands MemToReg, L4>
              jmp op_op_modreg1Rm
L4:         BadOperand
L3:       ifnot op3 'S', L3>                          ; LSS
            ifnot B$Operands MemToReg, L4>
            mov op1 00_1111, op2 00_1011_0010 | jmp op_op_modReg1Rm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'T', L2>
          ifnot op3 'R', L3>                        ; LTR
            mov op1 00_1111,  op2 0
            ifnot B$FirstGender reg, L4>
              mov op3 00_1101_1000 | jmp op_op_reg16
L4:         ifnot B$FirstGender mem, L4>
              mov op3 0001_1000 | jmp op_op_modRm16
L4:         BadOperand
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'M', L1>
        ifnot op2 'U', L2>
          ifnot op3 'L', L3>                     ; MUL
            mov op1 00_1111_0110
            ifnot B$FirstGender reg, L4>
              mov op2 00_1110_0000 | jmp w_reg1
L4:         ifnot B$FirstGender mem, L4>
              mov op2 0010_0000 | jmp w_modRm
L4:         BadOperand
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'N', L1>>
        ifnot op2 'E', L2>
          ifnot op3 'G', L3>                   ; NEG
            mov op1 00_1111_0110
            ifnot B$FirstGender reg, L4>
              mov op2 00_1101_1000 | jmp w_reg1
L4:         ifnot B$FirstGender mem, L4>
              mov op2 0001_1000 | jmp w_modRm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'O', L2>
          ifnot op3 'P', L3>                   ; NOP
            mov op1 00_1001_0000 | jmp op
L3:       ifnot op3 'T', L3>                   ; NOT
            mov op1 00_1111_0110
            ifnot B$FirstGender reg, L4>
              mov op2 00_1101_0000 | jmp w_reg1
L4:         ifnot B$FirstGender mem, L4>
              mov op2 0001_0000 | jmp w_modRm
L4:         BadOperand
L3:       ;BadMnemonic
L2:     BadMnemonic

L1:   ifnot op1 'O', L1>
        ifnot op2 'U', L2>
          ifnot op3 'T', L3>                   ; OUT
            ifnot B$Operands RegToImm, L4>
              mov op1 0E6
              If B$SecondOperandwBit > ByteSize
                inc op1
                On B$SecondOperandwBit = WordSize, ToOpcode 066
              End_If
              jmp w_imm8
L4:         ifnot B$Operands RegToReg, L4>
              Ifnot B$secondReg RegEax, L5>
              Ifnot B$FirstReg RegDx, L5>
              mov op1 0EE
              If B$SecondOperandwBit > ByteSize
                inc op1
                On B$SecondOperandwBit = WordSize, ToOpcode 066
              End_If
              jmp w_P2
L5:
L4:         BadOperand
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'P', L1>>
        ifnot op2 'O', L2>
          ifnot op3 'P', L3>                     ; POP
            ifnot B$FirstGender reg, L4>
              On B$wBit = ByteSize, BadOperand
              ifnot B$FirstRegGender sReg, L5>             ; max. G.P.regs = 00111
                cmp B$FirstReg 0010_0000 | jae L6>         ; regFS is 00100000; GS, 00101000
                  mov op1 00_0111 | jmp sReg2                         ; CS / DS / ES / SS
L6:             mov op1 00_1111, op2 00_1000_0001 | jmp op_sreg3      ; FS / GS
L5:          ; mov op1 00_1000_1111,  op2 00_1100_0000 | jmp op_reg1   ; G.P.
              mov op1 0_0101_1000 | jmp reg1_in_op                      ; register alternate
L4:         ifnot B$FirstGender mem, L4>
              On B$wBit = ByteSize, BadOperand
              mov op1 00_1000_1111,  op2 0 | jmp op_modRm
L4:         BadOperand
L3:       ifnot op3 'R', L3>                       ; POR - Bitwise Or N N N Y
              mov op1 00_1110_1011 | jmp OQRegMemToReg
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'R', L1>>
        ifnot op2 'C', L2>>
          ifnot op3 'L', L3>>                      ; RCL
              ifnot B$Operands RegToReg, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1101_0000 | jmp w_reg1_P2cl
L4:           ifnot B$Operands RegToMem, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 0001_0000 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot D$imm32 1, L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000, op2 00_1101_0000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem, L6>
                  mov op1 00_1101_0000, op2 0001_0000 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000, op2 00_1101_0000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 0001_0000 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand

L3:       ifnot op3 'R', L3>>                            ; RCR
              ifnot B$Operands RegToReg, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1101_1000 | jmp w_reg1_P2cl
L4:           ifnot B$Operands RegToMem, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 0001_1000 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot D$imm32 1, L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000,  op2 00_1101_1000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem, L6>
                  mov op1 00_1101_0000,  op2 0001_1000 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000,  op2 00_1101_1000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 0001_1000 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'E', L2>
          ifnot op3 'T', L3>                    ; RET same segment only
            ifnot B$ParametersNumber 0, L5>
L6:           mov op1 00_1100_0011 | jmp op
L5:         ifnot B$FirstGender imm, L4>
              If D$imm32 = 0
                  mov B$DisInside &False
                  mov B$ParametersNumber 0
                  mov B$immInside &FALSE | jmp L6<
              End_If
              mov op1 00_1100_0010 | jmp op_imm16
L4:         BadOperand
L3:       BadMnemonic

L2:     ifnot op2 'O', L2>>
          ifnot op3 'L', L3>>                    ; ROL
              ifnot B$Operands RegToReg, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1100_0000 | jmp w_reg1_P2cl
L4:           ifnot B$Operands RegToMem, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 0 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot B$imm32 1,  L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000,  op2 00_1100_0000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem, L6>
                  mov op1 00_1101_0000,  op2 0 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000,  op2 00_1100_0000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 0 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand


L3:       ifnot op3 'R', L3>>                   ; ROR
              ifnot B$Operands RegToReg, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1100_1000 | jmp w_reg1_P2cl
L4:           ifnot B$Operands RegToMem, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1000 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot D$imm32 1, L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000,  op2 00_1100_1000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem, L6>
                  mov op1 00_1101_0000,  op2 00_1000 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000, op2 00_1100_1000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 00_1000 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'S', L2>
          ifnot op3 'M', L3>                         ; RSM
            mov op1 00_1111,  op2 00_1010_1010 | jmp op_op
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'S', L1>>
        ifnot op2 'A', L2>>
          ifnot op3 'L', L3>
            mov op2 'H'
               jmp SHL2              ; SAL same instruction as SHL
L3:       ifnot op3 'R', L3>>                ; SAR
              ifnot B$Operands RegToReg L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1111_1000 | jmp w_reg1_P2cl
L4:           ifnot B$Operands RegToMem, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010, op2 0011_1000 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot D$imm32 1, L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000,  op2 00_1111_1000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem, L6>
                  mov op1 00_1101_0000,  op2 0011_1000 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000,  op2 00_1111_1000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 0011_1000 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'B', L2>>
          ifnot op3 'B', L3>>             ; SBB
            ifnot B$Operands RegToReg, L4>
              mov op1 0001_1010,  op2 00_1100_0000 | jmp w_Reg1Reg2
L4:         ifnot B$Operands MemToReg, L4>
              mov op1 0001_1010 | jmp w_modReg1Rm
L4:         ifnot B$Operands RegToMem, L4>
              mov op1 0001_1000 | jmp w_modReg2Rm
L4:         ifnot B$Operands ImmToReg, L4>
              ifnot B$FirstReg RegEAX, L5>
                If B$FirstOperandwBit > ByteSize
                    cmp D$ABSimm32 080 | jb L5>
                End_If
                mov op1 0001_1100 | jmp w_imm
L5:           mov op1 00_1000_0000,  op2 00_1101_1000 | jmp sw_reg1_imm
L4:         ifnot B$Operands ImmToMem, L4>
              mov op1 00_1000_0000,  op2 0001_1000 | jmp sw_modRm_imm
L4:         BadOperand
L3:       BadMnemonic

L2:     ifnot op2 'H', L2>>
          ifnot op3 'L', L3>>                             ; SHL/SAL
SHL2:         ifnot B$Operands RegToReg, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 00_1110_0000 | jmp w_reg1_P2cl
L4:           ifnot B$Operands RegToMem, L4>
                ifnot B$SecondReg RegCL, L5>
                  mov op1 00_1101_0010,  op2 0010_0000 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot D$imm32 1, L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000,  op2 00_1110_0000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem, L6>
                  mov op1 00_1101_0000,  op2 0010_0000 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000,  op2 00_1110_0000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 0010_0000 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand
L3:       ifnot op3 'R', L3>>                                 ; SHR
            ifnot B$Operands RegToReg, L4>
              ifnot B$SecondReg RegCL, L5>
                mov op1 00_1101_0010,  op2 00_1110_1000 | jmp w_reg1_P2cl
L4:         ifnot B$Operands RegToMem, L4>
              ifnot B$SecondReg RegCL, L5>
                mov op1 00_1101_0010,  op2 0010_1000 | jmp w_modRm_P2cl
L5:           BadOperand
L4:         ifnot B$SecondGender imm, L4>
              ifnot B$imm32 1, L5>
                ifnot B$Operands ImmToReg, L6>
                  mov op1 00_1101_0000,  op2 00_1110_1000 | jmp w_reg1_P2
L6:             ifnot B$Operands ImmToMem,  L6>
                  mov op1 00_1101_0000,  op2 0010_1000 | jmp w_modRm_P2
L6:             BadOperand
L5:           ifnot B$Operands ImmToReg, L5>
                mov op1 00_1100_0000,  op2 00_1110_1000 | jmp w_reg1_imm8
L5:           ifnot B$Operands ImmToMem, L5>
                mov op1 00_1100_0000,  op2 0010_1000 | jmp w_modRm_imm8
L5:           ;BadOperand
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'T', L2>
          ifnot op3 'C', L3>              ; STC
            mov op1 00_1111_1001 | jmp op
L3:       ifnot op3 'D', L3>              ; STD
            mov op1 00_1111_1101 | jmp op
L3:       ifnot op3 'I', L3>              ; STI
            mov op1 00_1111_1011 | jmp op
L3:       ifnot op3 'R', L3>              ; STR ("ax" or "W$edi")
              dec edi ; Because there is no need of 066 override here
              On B$FirstOperandwbit <> WordSize, BadOperandSize
              mov op1 0F, Op2 0
            ifnot B$FirstGender reg, L4>
              mov op3 00_1100_1000 | jmp op_op_reg1
L4:         ifnot B$FirstGender mem, L4>
              mov op3 00_1000 | jmp op_op_modRm
L4:         BadOperand
L3:       BadMnemonic
L2:     ifnot op2 'U', L2>>
          ifnot op3 'B', L3>>                         ; SUB
            ifnot B$Operands RegToReg, L4>
              mov op1 0010_1010,  op2 00_1100_0000 | jmp w_reg1reg2
L4:         ifnot B$Operands MemToReg, L4>
              mov op1 0010_1010 | jmp w_modReg1Rm
L4:         ifnot B$Operands RegToMem, L4>
              mov op1 0010_1000 | jmp w_modReg2Rm
L4:         ifnot B$Operands ImmToReg, L4>
              ifnot B$FirstReg RegEAX, L5>
                If B$FirstOperandwBit > ByteSize
                    cmp D$ABSimm32 080 | jb L5>
                End_If
                mov op1 0010_1100 | jmp w_imm
L5:           mov op1 00_1000_0000,  op2 00_1110_1000 | jmp sw_reg1_imm
L4:         ifnot B$Operands ImmtoMem, L4>
              mov op1 00_1000_0000,  op2 0010_1000 | jmp sw_modRm_imm
L4:         BadOperand
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'U', L1>
        ifnot op2 'D', L2>
          ifnot op3 '2', L3>           ; UD2 ;;; may add UD0 / UD1 (???....)
            mov op1 0F,  op2 0B | jmp op_op
               ;UD2 – Undefined instruction 0000 FFFF : 0000 1011
               ; used for sofware tests: generates opcode exception
L3:       ;BadMnemonic
L2:     BadMnemonic
L1:   ifnot op1 'X', L1>>
        ifnot op2 'O', L2>>
          ifnot op3 'R', L3>>             ; XOR
            ifnot B$Operands RegToreg, L4>
              mov op1 0011_0010,  op2 00_1100_0000 | jmp w_reg1reg2
L4:         ifnot B$Operands MemToReg, L4>
              mov op1 0011_0010 | jmp w_modReg1Rm
L4:         ifnot B$Operands RegToMem, L4>
              mov op1 0011_0000 | jmp w_modReg2Rm
L4:         ifnot B$Operands ImmToReg, L4>
              ifnot B$FirstReg RegEAX, L5>
                If B$FirstOperandwBit > ByteSize
                    cmp D$ABSimm32 080 | jb L5>
                End_If
                mov op1 0011_0100 | jmp w_imm
L5:           mov op1 00_1000_0000
                    mov op2 00_1111_0000 | jmp sw_reg1_imm
L4:         ifnot B$Operands ImmToMem, L4>
              mov op1 00_1000_0000,  op2 0011_0000 | jmp sw_modRm_imm
L4:         BadOperand
L3:       ;BadMnemonic
L2:     ;BadMnemonic
L1:   BadMnemonic

 ______________________________________________________________________________________

FourLetters:    On op5 > Separators,  jmp FiveLetters

        ifnot op1 'A', L1>
          ifnot op2 'R', L2>
            ifnot op3 'P', L3>
              ifnot op4 'L', L4>          ; ARPL  ; BadOperandSize
                On B$FirstOperandwbit <> WordSize, BadOperandSize
               ; dec edi ; Kill 066 or not ??? Should work the same in both cases...
                mov op1 00_0110_0011
                ifnot B$Operands RegToreg, L5>
                  mov op2 00_1100_0000 | jmp op_reg2reg1
L5:             ifnot B$Operands regToMem, L5>
                  jmp op_modReg2Rm
L5:             BadOperand
L4:           ;BadMnemonic
L3:         ;BadMnemonic
L2:       BadMnemonic

L1:     ifnot op1 'C', L1>>
          ifnot op2 'A', L2>
            ifnot op3 'L', L3>
              ifnot op4 'L', L4>           ; CALL
                ifnot B$FirstGender dis, L5>
                  On B$LocalSize <> 0, error D$NoLocalCallPtr
                  mov D$Relative RelativeFlag
                  mov op1 00_1110_1000
                  jmp op_dis
L5:             ifnot B$FirstGender Reg, L5>
                  mov op1 00_1111_1111,  op2 00_1101_0000 | jmp op_reg1
L5:             ifnot B$FirstGender mem, L5>
                  mov op1 00_1111_1111,  op2 0001_0000 | jmp op_modRm
                                         ; 015 > 10101
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic

     ;CALL   Call Procedure (in other segment)
     ;direct                1001 1010 : unsigned full offset, selector
     ;indirect              1111 1111 : mod 011 r/m


L2:       ifnot op2 'L', L2>
            ifnot op3 'T', L3>
              ifnot op4 'S', L4>                    ; CLTS
                mov op1 00_1111,  op2 00_0110 | jmp op_op
L4:           ;BadMnemonic
L3:         BadMnemonic

L2:       ifnot op2 'M', L2>
            ifnot op3 'P', L3>
              ifnot op4 'S', L4>                     ; CMPS
                ; CMPS – Compare String B[Operands]   1010 011w
                error D$NotYetMnemoPtr
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'W', L2>
            ifnot op3 'D', L3>
              ifnot op4 'E', L4>                     ; CWDE
                mov op1 00_1001_1000 | jmp op
L4:           ;BadMnemonic
L3:         ;BadMnemonic
L2:       BadMnemonic

L1:     ifnot op1 'E', L1>
          ifnot op2 'M', L2>
            ifnot op3 'M', L3>
              ifnot op4 'S', L4>                    ; EMMS
                cmp B$ParametersNumber 0 | jne L5>
                  mov op1 00_1111, op2 00_01110111 | jmp op_op
L5:             BadOperand
L4:           ;BadMnemonic
L3:         ;BadMnemonic
L2:       BadMnemonic

L1:     ifnot op1 'I', L1>>
          ifnot op2 'D', L2>
            ifnot op3 'I', L3>
              ifnot op4 'V', L4>               ; IDIV
                mov op1 00_1111_0110
                ifnot B$FirstGender reg, L5>
                  mov op2 00_1111_1000 | jmp w_reg1
L5:             ifnot B$FirstGender mem, L5>
                  mov op2 0011_1000 | jmp w_modRm
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic

L2:       ifnot op2 'M', L2>>
            ifnot op3 'U', L3>>
              ifnot op4 'L', L4>>                  ; IMUL
                cmp B$ParametersNumber 2 | je L6>
                cmp B$ParametersNumber 3 | je L7>

                  ifnot B$FirstGender reg, L5>
                    mov op1 00_1111_0110,  op2 00_0111_01000 | jmp w_reg1
L5:               ifnot B$FirstGender mem, L5>
                    mov op1 00_1111_0110,  op2 0010_1000 | jmp w_modRm
L5:               BadOperand

L6:             cmp B$wBit ByteSize | je L5>
                ifnot B$Operands RegToReg, L6>
                    mov op1 00_1111,  op2 00_1010_1111,  op3 00_1100_0000 | jmp op_op_reg1reg2
; 3 Parameters:
L7:             On B$ThirdGender <> imm, jmp L5>
                cmp B$wBit ByteSize | je L5>
                IfNot B$Operands RegToReg, L6> ; imul eax ebx 0-1
L8:               mov op1 00_0110_1001,  op2 00_1100_0000 | jmp s_reg1reg2_imm
L6:             ifnot B$Operands MemToReg, L6>

                  cmp B$immInside &TRUE | je L7>
                  mov op1 00_1111,  op2 00_1010_1111 | jmp op_op_modReg1Rm
                  ; 069 (ou 06B)
L7:               mov op1 00_0110_1001 | jmp s_modReg1Rm_imm
L6:             ifnot B$Operands ImmToReg, L6>
                  mov B$ParametersNumber 3
                  mov dh, B$FirstReg, B$SecondReg dh | jmp L8<  ; add: "> imul eax 213"
L6:             ;BadOperand
L5:           BadOperand
L4:         ;BadMnemonic
L3:       BadMnemonic

L2:     ifnot op2 'N', L2>
          ifnot op3 'T', L3>
            ifnot op4 '3', L4>
                LastOpcode 0CC          ; INT3

L4:         ifnot op4 '4', L4> ; INT4
                LastOpcode 0CC ; INT3 substitution (user really wrote 'int3' _by hand_)

L4:         ifnot op4 'O', L4>            ; INTO
              mov op1 00_1100_1110 | jmp op

L4:         BadMnemonic

L3:       ifnot op3 'S', L3>
            ifnot op4 'B', L4>                  ; INSB
              LastOpcode 00_0110_1100
L4:         ifnot op4 'W', L4>                  ; INSW
              ToOpcode 066
              LastOpcode 0011_01101
L4:         ifnot op4 'D', L4>                  ; INSD
              LastOpcode 00_0110_1101

L4:         BadMnemonic

L3:       ifnot op3 'V', L3>
            ifnot op4 'D', L4>            ; INVD
              mov op1 00_1111,  op2 00_1000 | jmp op_op
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'R', L2>
          ifnot op3 'E', L3>
            ifnot op4 'T', L4>            ; IRET
              mov op1 00_1100_1111 | jmp op
L4:         ;BadMnemonic
L3:       ;BadMnemonic
L2:     BadMnemonic

L1:     ifnot op1 'L', L1>>
          ifnot op2 'A', L2>
            ifnot op3 'H', L3>
              ifnot op4 'F', L4>                ; LAHF
                mov op1 00_1001_1111 | jmp op
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'G', L2>
            ifnot op3 'D', L3>
              ifnot op4 'T', L4>                 ; LGDT
                ifnot B$FirstGender mem, L5>
                  mov op1 00_1111,  op2 1,  op3 0001_0000 | jmp op_op_modRm
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'I', L2>
            ifnot op3 'D', L3>
              ifnot op4 'T', L4>                       ; LIDT
                ifnot B$FirstGender mem, L5>
                mov op1 00_1111,  op2 1,  op3 0001_1000 | jmp op_op_modRm
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'L', L2>
            ifnot op3 'D', L3>
              ifnot op4 'T', L4>                      ; LLDT
                ifnot B$FirstGender mem, L4>
                mov op1 00_1111,  op2 0,  op3 0001_0000 | jmp op_op_modRm16
L4:             ifnot B$FirstGender reg, L5>
                mov op1 00_1111,  op2 0,  op3 001101_0000 | jmp op_op_reg16
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'D', L2>
            ifnot op3 'T', L3>
              ifnot op4 'R', L4>                      ; LDTR
                ifnot B$FirstGender reg, L5>
                  mov op1 00_1111,  op2 0,  op3 00_1101_0000 | jmp op_op_reg16
L5:              ifnot B$FirstGender mem, L5>
                  mov op1 00_1111,  op2 0,  op3 0001_0000 | jmp op_op_modRm16
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'M', L2>
            ifnot op3 'S', L3>
              ifnot op4 'W',  L4>
                ifnot B$FirstGender Reg, L5>               ; LMSW
                  mov op1 00_1111,  op2 1,  op3 00_1111_0000 | jmp op_op_reg1
L5:             ifnot B$FirstGender mem, L5>
                  mov op1 00_1111,  op2 1,  op3 0011_0000 | jmp op_op_modRm
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'O', L4>
            ifnot op3 'D', L3>
              ifnot op4 'S', L4>
                ; LODS   Load String Operand  1010 110w    ??????????????????
                error D$NotYetMnemoPtr
L4:           BadMnemonic
L3:         ifnot op3 'O', L3>
              ifnot op4 'P', L4>                     ; LOOP
                ;On B$LocalSize <> UpShort, error D$NoPlainLabelForLoopPtr
                cmp B$LocalSize UpShort | je L7> | cmp B$LocalSize DownShort | je L7> ; jE! allowed down loop!
                error D$NoPlainLabelForLoopPtr
          L7:   mov op1 00_1110_0010 | jmp op_dis8
L4:           ;BadMnemonic
L3:         ;BadMnemonic
L2:       BadMnemonic

L1:     ifnot op1 'N', L1>                           ; NOPE
          ifnot op2 'O', L4>
            ifnot op3 'P', L4>
              ifnot op4 'E', L4>                     ; Empty Macro trick.
                mov B$SIBinside &FALSE, B$DisInside &FALSE, B$DummyDis &FALSE,
                    B$immInside &FALSE, B$PossibleFirstImmLabel &FALSE,
                    B$PossibleImmLabel &FALSE, B$mm3Dsuffix 0
                ret

L1:     ifnot op1 'O', L1>
          ifnot op2 'U', L2>
            ifnot op3 'T', L3>
              ifnot op4 'S', L4>                     ; OUTS
                error D$NotYetMnemoPtr
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'R', L2>
            ifnot op3 'P', L3>
              ifnot op4 'D', L4>                     ; ORPD
                 ToOpcode 001100110 | jmp L5>
L4:           ifnot op4 'S', L4>                     ; ORPS
L5:              mov op1 001010110 | jmp XMMmemXMM
L4:           ;BadMnemonic
L3:         ;BadMnemonic
L2:       BadMnemonic
L1:     ifnot op1 'P', L1>>
          ifnot op2 'A', L2>
            ifnot op3 'D', L3>
              ifnot op4 'D', L4>                   ; PADD > PADDB
                mov cl 'B' | mov op1 00_11111100
                    jmp gg2
L4:           BadMnemonic
L3:        ifnot op3 'N', L3>
              ifnot op4 'D', L4>                   ; PAND
                mov op1 00_11011011
                If B$FirstRegGender = XmmReg
                    ToOpcode 066 | jmp XmmMemXmm
                Else_If B$FirstRegGender = mmReg
                    jmp mmTwo
                End_If
                BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic

L2:       ifnot op2 'S', L2>>
            ifnot op3 'L', L3>
              ifnot op4 'L', L4>                            ; PSLL
                mov cl 'W' | mov op1 00_11110000, op2 0011_110_000 | jmp gg3
L4:           BadMnemonic
L3:         ifnot op3 'R', L3>
              ifnot op4 'A', L4>                            ; PSRA
                mov cl 'W' | mov op1 00_11100000, op2 0011_100_000 | jmp gg3
L4:           ifnot op4 'L', L4>                            ; PSRL
                mov cl, 'W' | mov op1 00_11010000, op2 0011_010_000 | jmp gg3
L4:           BadMnemonic
L3:         ifnot op3 'U', L3>
              ifnot op4 'B', L4>                            ; PSUB
                mov cl 'B' | mov op1 00_11111000 | jmp gg2
L4:           BadMnemonic
L3:         BadMnemonic

L2:       ifnot op2 'O', L2>
            ifnot op3 'P', L3>
              ifnot op4 'A', L4>                       ; POPA
                mov op1 00_0110_0001 | jmp op
L4:           ifnot op4 'F', L4>                       ; POPF
                mov op1 00_1001_1101 | jmp op
L4:           ;BadMnemonic
L3:         BadMnemonic

L2:       ifnot op2 'U', L2>>
            ifnot op3 'S', L3>>
              ifnot op4 'H', L4>>                         ; PUSH
                ifnot B$FirstGender reg, L5>
                  On B$wBit = ByteSize, BadOperand
                  ifnot B$FirstRegGender sReg, L6>       ; max. G.P.regs = 00111
                    cmp B$FirstReg 0010_0000 | jae L7>   ; regFS is 00100000; GS, 00101000
                      mov op1 00_0110 | jmp sReg2                        ; CS / DS / ES / SS
L7:                 mov op1 00_1111,  op2 00_1000_0000 | jmp op_sreg3    ; FS / GS
L6:              ;mov op1 00_1111_1111,  op2 00_1111_0000 | jmp op_reg1  ; G.P.
                  mov op1 0_0101_0000 | jmp reg1_in_op                    ; register alternate
L5:             ifnot B$FirstGender mem, L5>
                  On B$wBit = ByteSize, BadOperand
                    mov op1 00_1111_1111,  op2 0011_0000 | jmp op_modRm
L5:             ifnot B$FirstGender imm, L5>
                  mov op1 00_0110_1000 | jmp s_imm
L5:             ifnot B$FirstGender dis, L5>
                   mov op1 00_0110_1000 | jmp op_P1
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'X', L2>
            ifnot op3 'O', L3>
              ifnot op4 'R', L4>                       ; PXOR
                mov op1 00_11101111 | jmp OQregMemToReg
L4:           BadMnemonic
L3:         BadMnemonic
L2:       BadMnemonic

L1: ifnot op1 'R', L1>
      ifnot op2 'E', L2>
        ifnot op3 'T', L2>
          ifnot op4 'F', L2>                    ; RETF
            ifnot B$ParametersNumber 0, L5>
L6:           mov op1 00_1100_1011 | jmp op
L5:         ifnot B$FirstGender imm, L4>
              If D$imm32 = 0
                  mov B$DisInside &False
                  mov B$ParametersNumber 0
                  mov B$immInside &FALSE | jmp L6<
              End_If
              mov op1 00_1100_1010 | jmp op_imm16
L4:         BadOperand
L2:      BadMnemonic

L1:     ifnot op1 'S', L1>>
          ifnot op2 'A', L2>
            ifnot op3 'H', L3>
              ifnot op4 'F', L4>               ; SAHF
                mov op1 00_1001_1110 | jmp op
L4:           BadMnemonic
L3:         ifnot op3 'L', L3>
              ifnot op4 'C', L4>               ; SALC
                mov op1 0D6 | jmp op
L4:           BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'C', L2>
            ifnot op3 'A', L3>
              ifnot op4 'S', L4>               ; SCAS
                error D$NotYetMnemoPtr
                ; SCAS – Scan String 1101 111w
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'G', L2>
            ifnot op3 'D', L3>
              ifnot op4 'T', L4>                ; SGDT
                ifnot B$FirstGender mem, L5>
                  mov op1 00_1111,  op2 1,  op3 0 | jmp op_op_modRm
L5:             BadOperand
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:       ifnot op2 'H', L2>>
            mov op1 00_1111
            cmp op3 'L' | je L7>
            cmp op3 'R' | je L6>
L9:           BadMnemonic
L7:           mov op2 00_1010_0100 | jmp L5>
L6:           mov op2 00_1010_1100
L5:           ifnot op4 'D', L9<                 ; SHLD / SHRD
                ifnot B$Operands RegToReg, L3>
                  mov op3 00_1100_0000
                  ifnot B$ThirdGender imm, L4>
                    jmp op_op_reg2reg1_imm8
L4:               ifnot B$ThirdGender Reg, L9<
                    ifnot B$ThirdReg RegCl, L9<
                      or op2 1 | jmp op_op_reg2reg1_cl
L3:             ifnot B$Operands RegToMem, L9<
                  ifnot B$ThirdGender imm, L4>
                    jmp op_op_ModReg2Rm_imm8
L4:               ifnot B$ThirdGender Reg, L9<
                    ifnot B$ThirdReg RegCl, L9<
                    or op2 1 | jmp op_op_modReg2Rm_cl

L2:      ifnot op2 'I', L2>
           ifnot op3 'D', L3>
             ifnot op4 'T', L4>                      ; SIDT
               ifnot B$FirstGender mem, L5>
                 mov op1 00_1111,  op2 1, op3 00_1_000 | jmp op_op_modRm
L5:            BadOperand
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'L', L2>
           ifnot op3 'D', L3>
             ifnot op4 'T', L4>                      ; SLDT
               mov op1 00_1111,  op2 0
               ifnot B$FirstGender reg, L5>
                 mov op3 00_1100_0000 | jmp op_op_reg1
L5:            ifnot B$FirstGender mem, L5>
                 mov op3 0 | jmp op_op_modRm
L5:            ;BadOperand
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'M', L2>
           ifnot op3 'S', L3>
             ifnot op4 'W', L4>                 ; SMSW
               mov op1 00_1111,  op2 1
               ifnot B$FirstGender Reg, L5>
                 mov op3 00_1110_0000 | jmp op_op_reg1
L5:            ifnot B$FirstGender mem, L5>
                 mov op3 0010_0000 | jmp op_op_modRm
L5:            BadOperand
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'T', L2>
           ifnot op3 'O', L3>
             ifnot op4 'S', L4>
               error D$NotYetMnemoPtr
               ; STOS – Store String Data 1010 101w
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'T', L1>
         ifnot op2 'E', L2>
           ifnot op3 'S', L3>
             ifnot op4 'T', L4>                      ; TEST
               ifnot B$Operands RegToReg, L5>
                 mov op1 00_1000_0100,  op2 00_1100_0000 | jmp w_reg2reg1
L5:            ifnot B$Operands memToReg, L5>
                 mov op1 00_1000_0100 | jmp w_modReg1Rm
L5:            ifnot B$Operands RegToMem, L5>
                 mov op1 00_1000_0100 | jmp w_modReg2Rm
L5:            ifnot B$Operands immToReg, L5>
                 ifnot B$FirstReg RegEax, L6>
                   mov op1 00_1010_1000 | jmp w_imm
L6:                mov op1 00_1111_0110,  op2 00_1100_0000 | jmp w_reg1_imm
L5:            ifnot B$Operands immToMem, L5>
                   mov op1 00_1111_0110,  op2 0 | jmp w_modRm_imm
L5:            BadOperand
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'V', L1>
         ifnot op2 'E', L2>
           ifnot op3 'R', L3>
             ifnot op4 'R', L4>                    ; VERR
               mov op1 00_1111,  op2 0
               ifnot B$FirstGender reg, L5>
                 mov op3 00_1110_0000 | jmp op_op_reg16
L5:            ifnot B$FirstGender mem, L5>
                 mov op3 0010_0000 | jmp op_op_modRm16
L5:            BadOperand
L4:          ifnot op4 'W', L4>                     ; VERW
               mov op1 00_1111,  al 0
               ifnot B$FirstGender reg, L5>
                 mov op3 00_1110_1000 | jmp op_op_reg16
L5:            ifnot B$FirstGender mem, L5>
                 mov op3 0010_1000 | jmp op_op_modRm16
L5:            BadOperand
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'W', L1>
         ifnot op2 'A', L2>
           ifnot op3 'I', L3>
             ifnot op4 'T', L4>                      ; WAIT
               mov op1 00_1001_1011 | jmp op
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'X', L1>>
         ifnot op2 'A', L2>
           ifnot op3 'D', L3>
             ifnot op4 'D', L4>                        ; XADD
               mov op1 00_1111,  op2 00_1100_0000
               ifnot B$Operands RegToReg, L5>
                 mov op3 00_1100_0000 | jmp op_w_reg2reg1
L5:            ifnot B$Operands RegToMem, L5>
                 jmp op_w_modReg2Rm
L5:            BadOperand
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'C', L2>>
           ifnot op3 'H', L3>>
             ifnot op4 'G', L4>>                   ; XCHG
               ifnot B$Operands RegToReg, L5>
                 ifnot B$FirstReg RegEax, L6>
                   mov op2 B$FirstReg,  op1 B$SecondReg
                   mov B$FirstReg op1,  B$SecondReg op2  ; exchange the regs order
L6:              ifnot B$SecondReg RegEax, L6>
                   cmp B$SecondOperandwBit ByteSize | jne L7>
                      mov op1 086, op2 00_11_000_000 | jmp op_reg2reg1
L7:                mov op1 00_1001_0000 | jmp XOPreg1
L6:                mov op1 00_1000_0110,  op2 00_1100_0000 | jmp w_reg1reg2
L5:            ifnot B$Operands MemToReg, L5>
                   mov op1 00_1000_0110 | jmp w_modReg1Rm
L5:            ifnot B$Operands RegToMem, L5>
                   mov op1 00_1000_0110 | jmp w_modReg2Rm
L5:            BadOperand
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'L', L2>
           ifnot op3 'A', L3>
             ifnot op4 'T', L4>                   ; XLAT
               mov op1 00_1101_0111 | jmp op
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ;BadMnemonic
L1:    BadMnemonic

 _______________________________________________________________________________________


FiveLetters:     On op6 > Separators,  jmp SixLetters



; Align: One imm parameter. Here, i just fill with NOPs up to whished adress.
; I disagree with alignment use. Do what you want here, according your own
; whishes:

       Ifnot op1 'A', L1>>
         ifnot op2 'L', L2>>
           ifnot op3 'I', L3>>
             ifnot op4 'G', L3>>
               ifnot op5 'N', L3>>                    ; ALIGN
                 cmp B$ParametersNumber 1 | jne L6>
                 cmp B$FirstGender imm | jne L6>
                    cmp D$imm32 4 | jl L6>
                    cmp D$imm32 0100 | ja L6>
                    mov ecx D$imm32 | bsr eax ecx | bsf ebx ecx
                    On eax <> ebx, error D$BadAlignPtr
                    dec ecx | test edi ecx | jz L9>
                    mov eax edi | and eax ecx | sub ecx eax | inc ecx
                    cmp ecx 5 | jb L7>
                    mov al 0EB | stosb | mov al cl | sub al 2 | stosb
                    sub ecx 2
L7:                 mov al 090 | rep stosb
                    call SetAlignFlag
L9:                 mov D$imm32 0, B$immInside &FALSE
                 ret
L6:              BadOperand
L3:        BadMnemonic
L2:      ifnot op2 'D', L2>
           ifnot op3 'D', L3>
             ifnot op4 'P', L4>
               ifnot op5 'D', L5>                    ; ADDPD
                  ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                    ; ADDPS
L6:               mov op1 001011000 | jmp XMMmemXMM
L5:            BadMnemonic
L4:          ifnot op4 'S', L4>
               ifnot op5 'D', L5>                    ; ADDSD
                  ToOpcode 0011110010 | jmp L6>
L5:            ifnot op5 'S', L5>                    ; ADDSS
                  ToOpcode 0011110011                ; additional frefix for ADDSS
L6:               mov op1 001011000 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'N', L2>
           ifnot op3 'D', L3>
             ifnot op4 'P', L4>
               ifnot op5 'D', L5>                    ; ANDPD
                  ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                    ; ANDPS
L6:               mov op1 001010100 | jmp XMMmemXMM
L5:            BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'B', L1>
         ifnot op2 'O', L2>
           ifnot op3 'U', L3>
             ifnot op4 'N', L4>
               ifnot op5 'D', L5>                     ; BOUND
                 ifnot B$Operands memToreg, L6>
                   mov op1 00_0110_0010 | jmp op_modReg1Rm
L6:              BadOperand
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'S', L2>
           ifnot op3 'W', L3>
             ifnot op4 'A', L4>
               ifnot op5 'P', L5>                        ; BSWAP
                 ifnot B$FirstGender reg, L6>
                   mov op1 00_1111,  op2 00_1100_1000 | jmp op_reg1
L6:              BadOperand
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic

L1:     ifnot op1 'C', L1>>
          ifnot op2 'A', L1>>
            ifnot op3 'L', L3>>
              ifnot op4 'L', L3>>
               ifnot op5 'F', L3>>
L5:             ifnot B$FirstGender mem, L4>
                ifnot B$ParametersNumber 2, L5>             ; CALLF jE! added
                ifnot B$SecondGender imm, L4>
                cmp B$EregInside 0 | jne L4>
                ToOpcode  09A
                cmp B$FirstParaMem 'W' | je L2>
                cmp B$FirstParaMem 'D' | jne L4>
                cmp B$PossibleImmLabel 0 | je L8>; | int 3
                mov D$CodeListPtr edi | pushad | call SecondParameterLabel | popad
            L8:
                mov eax D$imm32 | stosd | jmp L6>
            L2: mov eax D$imm32 | stosw | test eax 0FFFF0000 | jne L7>
            L6: mov eax D$dis32 | stosw | test eax 0FFFF0000 | jne L7>
                call ClearParameters
                ret
L5:           mov op1 00_1111_1111,  op2 0001_1000 | jmp op_modRm
L4:          BadOperand
L3:         BadMnemonic
L7:       error D$OverWordPtr

L1:    ifnot op1 'C', L1>>
         ifnot op2 'M', L2>>
           ifnot op3 'P', L3>
             ifnot op4 'P', L4>
               ifnot op5 'D', L5>                 ; CMPPD SSE2
                  ToOpcode 066 | mov op1 0C2 | jmp XmmMemXmmImm7
L5:            ifnot op5 'S', L5>                 ; CMPPS SSE2
                 mov op1 0011000010 | jmp XmmMemXmmImm7
L5:            BadMnemonic
L4:          ifnot op4 'S', L4>
               ifnot op5 'B', L5>                 ; CMPSB / W / D
                 mov op1 00_1010_0110 | jmp op
L5:            ifnot op5 'W', L5>
                 ToOpcode 066
                 mov op1 00_1010_0111 | jmp op
L5:            ifnot op5 'D', L5>
                 If B$FirstRegGender = XmmReg     ; CMPSD SSE2
                    ; Alternate
                    ToOpcode 0011110010 | mov op1 0011000010 | jmp XmmMemXmmImm7
                 End_If
                 mov op1 00_1010_0111 | jmp op

L5:            ifnot op5 'S', L5>                  ; CMPSS SSE2
                 ToOpcode 0F3 | mov op1 0011000010 | jmp XmmMemXmmImm7

L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'P', L2>
           ifnot op3 'U', L3>
             ifnot op4 'I', L4>
               ifnot op5 'D', L5>                    ; CPUID
                 mov op1 00_1111,  op2 00_1010_0010 | jmp op_op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'D', L1>>
         ifnot op2 'I', L2>>
           ifnot op3 'V', L3>>
             ifnot op4 'P', L4>>
               ifnot op5 'D', L5>>                        ; DIVPD
                 ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>>                        ; DIVPS
L6:              mov op1 001011110 | jmp XMMmemXMM
L5:            BadMnemonic
L4:          ifnot op4 'S', L4>>
               ifnot op5 'D', L5>>                        ;DIVSD
                   ToOpcode 0011110010 | jmp L6>
L5:            ifnot op5 'S', L5>>                        ;DIVSS
                   ToOpcode 0011110011
L6:                mov op1 001011110 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'E', L1>>
         ifnot op2 'N', L2>>
           ifnot op3 'T', L3>>
             ifnot op4 'E', L4>>
               ifnot op5 'R', L5>>                        ; ENTER

                ToOpcode 0C8

                mov esi D$LineStart
L6:             lodsb
                    If al be EOI
                        mov eax NotEnough | jmp EnterError
                    End_If
                    ifnot al Space,  L6<

                    call TranslateAny

                    If eax > 0FFFC
                        mov eax EnterStack | jmp EnterError
                    End_If
                    test eax 0011 | jz S0>
                        mov eax EnterStack | jmp EnterError

S0:                 ToOpcode al, ah        ; 16-bit >>> displacement

                    .If B$esi-1 = Space
                        call TranslateAny   ; test both numbers at once
                        If eax > 31
                            mov eax EnterLevel | jmp EnterError    ; 8-bit level (L)
                        End_If
                        ToOpcode al
                    .Else
                        ToOpcode  0
                    .End_If

                    mov esi D$LineStart
                    While B$esi > EOI
                        mov B$esi 0 | inc esi
                    End_While

                    ret

EnterError: mov esi D$LineStart
            While B$esi > EOI
                mov B$esi 0 | inc esi
            End_While
            jmp OutOnError

L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'I', L1>
         ifnot op2 'R', L2>
           ifnot op3 'E', L3>
             ifnot op4 'T', L4>
               ifnot op5 'D', L5>               ; IRETD
                 mov op1 00_1100_1111 | jmp op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'L', L1>>
         ifnot op2 'D', L2>
           ifnot op3 'D', L3>
             ifnot op4 'Q', L4>
               ifnot op5 'U', L5>       ; LDDQU
                IfNot B$Operands MemToReg, L6>
                ToOpcode 0F2 | mov Op1 0F0 | jmp MMXmemXMM
L6: BadOperand
L2:      ifnot op2 'E', L2>
           ifnot op3 'A', L3>
             ifnot op4 'V', L4>
               ifnot op5 'E', L5>
                 mov op1 00_1100_1001 | jmp op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'O', L2>
           ifnot op3 'D', L3>
             ifnot op4 'S', L4>
               ifnot op5 'B', L5>              ; LODSB  /  W /  D
                 mov op1 00_0101_01100 | jmp op
L5:            ifnot op5 'W', L5>
                 ToOpcode 066
                 mov op1 00_1010_1101 | jmp op
L5:            ifnot op5 'D', L5>
                 mov op1 00_1010_1101 | jmp op
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'O', L3>
             ifnot op4 'P', L4>
               ifnot op5 'Z', L6>                 ; LOOPZ  /  E
L5:              On B$LocalSize <> UpShort, error D$NoPlainLabelForLoopPtr
                 mov op1 00_1110_0001 | jmp op_dis8
L6:            cmp op5, 'E' | je L5<
               BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'M', L1>>
         ifnot op2 'A', L2>
           ifnot op3 'X', L3>
             ifnot op4 'S', L4>
               ifnot op5 'S', L5>                 ; MAXSS
                 ToOpcode 0F3 | jmp L6>
L5:            ifnot op5 'D', L5>                 ; MAXSD
                 ToOpcode 0F2
L6:              mov op1 05F | jmp XMMmemXMM
L5:            BadMnemonic
L4:          ifnot op4 'P', L4>
               ifnot op5 'D', L5>                 ; MAXPD
                   ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                 ; MAXPS
L6:                mov op1 001011111 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ifnot op2 'I', L2>
           ifnot op3 'N', L3>
             ifnot op4 'S', L4>
               ifnot op5 'D', L5>                  ; MINSD
                   ToOpcode 0011110010 | jmp L6>
L5:            ifnot op5 'S', L5>                  ; MINSS
                 ToOpcode 0011110011 | jmp L6>
L5:            BadMnemonic
L4:          ifnot op4 'P', L4>
               ifnot op5 'D', L5>                 ; MINPD
                   ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                 ; MINPS
L6:                mov op1 001011101 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'U', L2>
           ifnot op3 'L', L3>
             ifnot op4 'S', L4>
               ifnot op5 'D', L5>                 ; MULSD
                 ToOpcode 0011110010 | jmp L6>
L5:            ifnot op5 'S', L5>                 ; MULSS
                 ToOpcode 0011110011 | jmp L6>
L5:            BadMnemonic
L4:          ifnot op4 'P', L4>
               ifnot op5 'D', L5>                 ; MULPD
                   ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                 ; MULPS
L6:                mov op1 001011001 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'W', L2>
           ifnot op3 'A', L3>
             ifnot op4 'I', L4>
               ifnot op5 'T', L5>                 ; MWAIT
                 ToOpcode 0F, 01, 0C9 | ret
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'O', L1>>
         ifnot op2 'U', L2>
           ifnot op3 'T', L3>
             ifnot op4 'S', L4>
               ifnot op5 'B', L5>                ; OUTSB  /  W  /  D
                 mov op1 00_0110_1110 | jmp op
L5:            ifnot op5 'W', L5>
                 ToOpcode 066
                 mov op1 00_0110_1111 | jmp op
L5:            ifnot op5 'D', L5>
                 mov op1 00_0110_1111 | jmp op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'P', L1>>
         ifnot op2 'A', L2>>
           ifnot op3 'D', L3>>
             ifnot op4 'D', L4>>
               cmp op5 'B' | je L6>
               cmp op5 'W' | je L6>
               cmp op5 'D' | jne L5>                         ; PADDB/W/D
L6:              mov cl op5 | mov op1 00_11111100
                     jmp gg2
L5:            ifnot op5 'Q', L5>                            ; PADDQ
                     mov op1 0011010100 | jmp OQRegMemToReg
L5:            ifnot op5 'S', L5>                            ; PADDS
                 mov cl 'B' | mov op1 00_11101100
                    jmp gg2
L5:          ;  BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'N', L3>
             ifnot op4 'D', L4>                      ; PANDB
               ifnot op5 'Q', L5>
                 mov op1 00_11011011 | jmp mmTwo
L5:            ifnot op5 'N', L5>                    ; PANDN
                 mov op1 00_11011111 | jmp OQRegMemToReg
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'U', L3>
             ifnot op4 'S', L4>                      ; PAUSE
               ifnot op5 'E', L5>
                   ToOpcode 0011110011 | LastOpcode 0010010000
L3:        ifnot op3 'V', L3>
             ifnot op4 'G', L4>
               ifnot op5  'B', L5>                   ; PAVGB
                 mov op1 0E0 | jmp OQregMemToReg
L5:            ifnot op5 'W', L5>>                   ; PAVGW
                 mov op1 0E3 | jmp OQregMemToReg
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'F', L2>>
           Ifnot op3 '2', L3>
             ifnot op4 'I', L4>
               ifnot op5 'D', L5>               ; PF2ID
                   mov op1 01D | jmp mm3D
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        Ifnot op3 'A', L3>
             ifnot op4 'C', L4>
               ifnot op5 'C', L5>               ; PFACC
                   mov op1 0AE | jmp mm3D
L5:            BadMnemonic
L4:          ifnot op4 'D', L4>
               ifnot op5 'D', L5>               ; PFADD
                   mov op1 09E | jmp mm3D
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        Ifnot op3 'M', L3>
             ifnot op4 'A', L4>
               ifnot op5 'X', L5>               ; PFMAX
                   mov op1 0A4 | jmp mm3D
L5:            BadMnemonic
L4:          ifnot op4 'I', L4>
               ifnot op5 'N', L5>               ; PFMIN
                   mov op1 094 | jmp mm3D
L5:            BadMnemonic
L4:          ifnot op4 'U', L4>
               ifnot op5 'L', L5>               ; PFMUL
                   mov op1 0B4 | jmp mm3D
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        Ifnot op3 'R', L3>
             ifnot op4 'C', L4>
               ifnot op5 'P', L5>               ; PFRCP
                   mov op1 096 | jmp mm3D
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        Ifnot op3 'S', L3>
             ifnot op4 'U', L4>
               ifnot op5 'B', L5>               ; PFSUB
                   mov op1 09A | jmp mm3D
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'I', L2>
           Ifnot op3 '2', L3>
             ifnot op4 'F', L4>
               ifnot op5 'D', L5>               ; PI2FD
                   mov op1 0D | jmp mm3D
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'M', L2>
           Ifnot op3 'A', L3>
             ifnot op4 'D', L4>
               ifnot op5 'D', L5>               ; PMADD
                 mov op1 00_11110101 | jmp OQRegMemToReg
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'U', L3>
             ifnot op4 'L', L4>
               ifnot op5 'H', L5>                  ; PMULH
                 mov op1 00_11100101 | jmp OQRegMemToReg
L5:            ifnot op5 'L', L5>                  ; PMULL
                 mov op1 00_11010101 | jmp OQRegMemToReg
L5:            ifnot op5 'W', L5>                  ; PMULW
                 mov op1 0D5 | jmp OQRegMemToReg
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'O', L2>>
           ifnot op3 'P', L3>
             ifnot op4 'A', L4>
               ifnot op5 'D', L5>                 ; POPAD
                 mov op1 00_0110_0001 | jmp op
L5:             ifnot op5 'W', L5>                 ; POPAW
                 ToOpcode 066 | mov op1 00_0110_0001 | jmp op
L5:            BadMnemonic
L4:          ifnot op4 'F', L4>
               ifnot op5 'D',L5>                 ; POPFD
                 mov op1 00_1001_1101 | jmp op
L5:            ifnot op5 'W',L5>                 ; POPFW
                 ToOpcode 066 | mov op1 00_1001_1101 | jmp op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:       ifnot op2 'S', L2>>
            ifnot op3 'L', L3>
              ifnot op4 'L', L4>                  ; PSLLW/D/Q
                cmp op5 'W' | je L6>
                cmp op5 'D' | je L6>
                cmp op5 'Q' | jne L5>
L6:               mov cl op5 |  mov op1 00_11110000, op2 0011_110_000 | jmp gg3
L5:             ;BadMnemonic
L4:           BadMnemonic
L3:         ifnot op3 'R', L3>
              ifnot op4 'A', L4>
                cmp op5 'W' | je L6>
                cmp op5 'D' | jne L5>                             ; PSRAW/D
L6:               mov cl op5 |  mov op1 00_11100000, op2 0011_100_000 | jmp gg3
L5:             BadMnemonic
L4:           ifnot op4 'L', L4>
                cmp op5 'W' | je L6>
                cmp op5 'D' | je L6>
                cmp op5 'Q' | jne L5>                          ; PSRLW/D/Q
L6:               mov cl op5 | mov op1 00_11010000, op2 0011_010_000 | jmp gg3
L5:             BadMnemonic
L4:           BadMnemonic
L3:         ifnot op3 'U', L3>
              ifnot op4 'B', L4>
                cmp op5 'B' | je L6>
                cmp op5 'W' | je L6>
                cmp op5 'D' | jne L5>                           ; PSUBB/W/D
L6:               mov cl op5 | mov op1 00_11111000 | jmp gg2
L5:             cmp op5 'Q' | jne L5>                           ; PSUBQ
                  mov op1 0011111011 | jmp OQregMemToReg
L5:            ifnot op5 'S', L5>                               ; PSUBS
                 mov cl 'B' | mov op1 00_11101000 | jmp gg2
L5:             ;BadMnemonic
L4:           ;BadMnemonic
L3:         BadMnemonic
L2:      ifnot op2 'U', L2>
           ifnot op3 'S', L3>
             ifnot op4 'H', L4>
               ifnot op5 'A', L5>                 ; PUSHA
                 mov op1 00_0110_0000 | jmp op
L5:            ifnot op5 'F', L5>                 ; PUSHF
                 mov op1 00_0100_11100 | jmp op
L5:            ifnot op5 'W', L5>                 ; PUSHW
                  ifnot B$FirstGender imm, L6>
                    mov op1 00_0110_1000 | jmp s_imm16
L6:               BadOperand
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'R', L1>>
         ifnot op2 'C', L2>
           ifnot op3 'P', L3>
             ifnot op4 'S', L4>
               ToOpcode 0011110011 | jmp L5>           ; RCPSS
L4:          ifnot op4 'P', L4>
L5:            ifnot op5 'S', L5>                      ; RCPPS
                 mov op1 001010011 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'D', L2>
           ifnot op3 'M', L3>
             ifnot op4 'S', L4>
               ifnot op5 'R', L5>                      ; RDMSR
                 mov op1 00_1111,  op2 0011_0010 | jmp op_op
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'P', L3>
             ifnot op4 'M', L4>
               ifnot op5 'C', L5>                      ; RDPMC
                 mov op1 00_1111,  op2 0011_0011 | jmp op_op
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'T', L3>
             ifnot op4 'S', L4>
               ifnot op5 'C', L5>                      ; RDTSC
                 mov op1 00_1111,  op2 0011_0001 | jmp op_op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemnonic
L2:      BadMnemonic
L1:    ifnot op1 'S', L1>>
         ifnot op2 'C', L2>
           ifnot op3 'A', L3>
             ifnot op4 'S', L4>
               ifnot op5 'B', L5>                 ; SCASB / W / D
                 mov op1 00_1010_1110    ; 011011110B mistake in intel doc
                    jmp op
L5:            ifnot op5 'W', L5>
                 ToOpcode 066
                 mov op1 00_1010_1111     ; 011011111B
                   jmp op
L5:            ifnot op5 'D', L5>
                 mov op1 00_1010_1111     ; 011011111B
                     jmp op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'T', L2>
           ifnot op3 'O', L3>
             ifnot op4 'S', L4>
               ifnot op5 'B', L5>                     ; STOSB / W / D
                 mov op1 00_1010_1010 | jmp op
L5:            ifnot op5 'W', L5>
                 ToOpcode 066
                 mov op1 00_1010_1011 | jmp op
L5:            ifnot op5 'D', L5>
                 mov op1 00_1010_1011 | jmp op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'U', L2>
           ifnot op3 'B', L3>
             ifnot op4 'S', L4>
               ifnot op5 'D', L5>                     ; SUBSD
                   ToOpcode 0011110010 | jmp L6>
L5:            ifnot op5 'S', L5>                     ; SUBSS
                   ToOpcode 0011110011 | jmp L6>
L5:            BadMnemonic
L4:          ifnot op4 'P', L4>
               ifnot op5 'D', L5>                     ; SUBPD
                   ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                     ; SUBPS
L6:                mov op1 001011100 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'W', L1>
         ifnot op2 'R', L2>
           ifnot op3 'M', L3>
             ifnot op4 'S', L4>
               ifnot op5 'R', L5>                             ; WRMSR
                 mov op1 00_1111,  op2 0011_0000 | jmp op_op
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'X', L1>
         ifnot op2 'L', L2>
           ifnot op3 'A', L3>
             ifnot op4 'T', L4>
               ifnot op5 'B', L5>                            ; XLATB
                 mov op1 00_1101_0111 | jmp op              ; XLATB 11   >>>  XLAT  ???
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'O', L2>
           ifnot op3 'R', L3>
             ifnot op4 'P', L4>
               ifnot op5 'D', L5>                            ; XORPD
                   ToOpcode 001100110 | jmp L6>
L5:            ifnot op5 'S', L5>                            ; XORPS
L6:                mov op1 001010111 | jmp XMMmemXMM
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ;BadMnemonic
L1:    BadMnemonic

 _______________________________________________________________________________________

SixLetters:     On op7 > Separators,  jmp SevenLetters

       ifnot op1 'A', L1>
         ifnot op2 'N', L2>
           ifnot op3 'D', L3>
             ifnot op4 'N', L4>
               ifnot op5 'P', L5>
                 ifnot op6 'D', L6>                      ; ANDNPD
                   ToOpcode 001100110 | jmp L7>
L6:              ifnot op6 'S', L6>                      ; ANDNPS
L7:                mov op1 001010101 | jmp XMMmemXMM
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic

L1:    ifnot op1 'C', L1>
         ifnot op2 'O', L2>
           ifnot op3 'M', L3>
             ifnot op4 'I', L4>
               ifnot op5 'S', L5>
                 ifnot op6 'S', L6>                      ; COMISS
                   mov op1 00101111 | jmp XMMmemXMM
L6:              ifnot op6 'D', L6>                      ; COMISD
                   ToOpcode 001100110 | mov op1 02F | jmp XMMmemXMM  ; 66 0F 2F xx
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'H', L1>
         ifnot op2 'A', L2>
           ifnot op3 'D', L3>
             ifnot op4 'D', L4>
               ifnot op5 'P', L5>
                 ifnot op6 'D', L6>         ; HADDPD 66,0F,7C,/r HADDPD xmm1, xmm2/m128
                   ToOpcode 066 | mov Op1 07C | jmp XmmMemXmm
L6:              ifnot op6 'S', L6>         ; HADDPS F2,0F,7C,/r HADDPS xmm1, xmm2/m128
                   ToOpcode 0F2 | mov Op1 07C | jmp XmmMemXmm
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'S', L2>
           ifnot op3 'U', L3>
             ifnot op4 'B', L4>
               ifnot op5 'P', L5>
                 ifnot op6 'D', L6>         ; HSUBPD 66,0F,7D,/r HSUBPD xmm1, xmm2/m128
                   ToOpcode 066 | mov Op1 07D | jmp XmmMemXmm
L6:              ifnot op6 'S', L6>         ; HSUBPS F2,0F,7D,/r HSUBPS xmm1, xmm2/m128
                   ToOpcode 0F2 | mov Op1 07D | jmp XmmMemXmm
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'I', L1>
         ifnot op2 'N', L2>
           ifnot op3 'V', L3>
             ifnot op4 'L', L4>
               ifnot op5 'P', L5>
                 ifnot op6 'G', L6>     ; INVLPG
                   ifnot B$FirstGender mem, L7>
                     mov op1 00_1111,  op2 1,  op3 00_111_000 | jmp op_op_modRm
L7:                BadOperand
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'L', L1>
         ifnot op2 'F', L2>
           ifnot op3 'E', L3>
             ifnot op4 'N', L4>
               ifnot op5 'C', L5>
                 ifnot op6 'E', L6>                 ; LFENCE
                     ToOpcode 00001111, 0010101110 | LastOpcode 0011101000
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'O', L2>
           ifnot op3 'O', L3>
             ifnot op4 'P', L4>
               ifnot op5 'N', L5>
                 ifnot op6 'Z', L6>                 ; LOOPNZ /  E
L7:                On B$LocalSize <> UpShort, error D$NoPlainLabelForLoopPtr
                   mov op1 00_1110_0000 | jmp op_dis8
L6:              cmp op6 'E' | je L7<
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'M', L1>
         ifnot op2 'F', L2>
           ifnot op3 'E', L3>
             ifnot op4 'N', L4>
               ifnot op5 'C', L5>
                 Ifnot op6 'E', L6>                         ; MFENCE
                     ToOpcode 00001111, 0010101110 | LastOpcode 0011110000
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'P', L1>>
         ifnot op2 'A', L2>>
           ifnot op3 'D', L3>>
             ifnot op4 'D', L4>>
               ifnot op5 'S', L5>
                 cmp op6 'B' | je L7>
                 cmp op6 'W' | jne L6>                         ; PADDSB/W
L7:                mov cl op6 | mov op1 00_11101100  ; = 0EC
                      jmp gg2
L6:              BadMnemonic
L5:            ifnot op5 'U', L5>
                 ifnot op6 'S', L6>                            ; PADDUS
                   mov cl, 'B' | mov op1 00_11011100
                       jmp gg2
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          BadMnemonic
L2:        ifnot op2 'C', L2>
             ifnot op3 'M', L3>
               ifnot op4 'P', L4>
                 ifnot op5 'E', L5>
                   ifnot op6 'Q', L6>                          ; PCMPEQ
                     mov cl 'B' | mov op1 00_01110100
                         jmp gg2
L6:                BadMnemonic
L5:              ifnot op5 'G', L5>
                   ifnot op6 'T', L6>                          ; PCMPGT
                     mov cl 'B' | mov op1 00_01100100
                         jmp gg2
L6:                ;BadMnemonic
L5:              ;BadMnemonic
L4:            ;BadMnemonic
L3:          BadMnemonic
L2:      ifnot op2 'E', L2>
           ifnot op3 'X', L3>
             ifnot op4 'T', L4>
               ifnot op5 'R', L5>
                 ifnot op6 'W', L6>                             ; PEXTRW
                     mov op1 0011000101 | jmp OQregRegImm8
L6:                ;BadMnemonic
L5:              ;BadMnemonic
L4:            ;BadMnemonic
L3:          BadMnemonic
L2:      ifnot op2 'I', L2>
           ifnot op3 'N', L3>
             ifnot op4 'S', L4>
               ifnot op5 'R', L5>
                 ifnot op6 'W', L6>                              ; PINSRW
                     mov op1 0011000100 | jmp OQregmemImm8
L6:                ;BadMnemonic
L5:              ;BadMnemonic
L4:            ;BadMnemonic
L3:          BadMnemonic
L2:      ifnot op2 'F', L2>
           ifnot op3 'S', L3>
             ifnot op4 'U', L4>
               ifnot op5 'B', L5>
                 ifnot op6 'R', L6>            ; PFSUBR
                     mov op1 0AA | jmp mm3D
L6:                ;BadMnemonic
L5:              ;BadMnemonic
L4:            ;BadMnemonic
L3:          BadMnemonic
L2:      ifnot op2 'M', L2>>
           ifnot op3 'A', L3>
             ifnot op4 'X', L4>
               ifnot op5 'S', L5>
                 ifnot op6 'W', L6>                         ; PMAXSW
                    mov op1 0011101110 | jmp OQregMemToReg
L6:                BadMnemonic
L5:            ifnot op5 'U', L5>
                 ifnot op6 'B', L6>                         ; PMAXUB
                    mov op1 0011011110 | jmp OQregMemToReg
L5:             ;BadMnemonic
L4:            ;BadMnemonic
L3:        ifnot op3 'I', L3>
             ifnot op4 'N', L4>
               ifnot op5 'S', L5>
                 ifnot op6 'W', L6>                         ; PMINSW
                    mov op1 0011101010 | jmp OQregMemToReg
L6:                BadMnemonic
L5:            ifnot op5 'U', L5>
                 ifnot op6 'B', L6>                         ; PMINUB
                    mov op1 0011011010 | jmp OQregMemToReg
L5:              ;BadMnemonic
L4:            ;BadMnemonic
L3:        ifnot op3 'U', L3>
             ifnot op4 'L', L4>
               ifnot op5 'H', L5>
                 ifnot op6 'W', L6>                         ; PMULHW
                    mov op1 0E5 | jmp OQregMemToReg
L6:                ;BadMnemonic

L5:            ifnot op5 'L', L5>
                 ifnot op6 'W', L6>                     ; PMULLW
                   mov op1 0D5 | jmp OQRegMemToReg

L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'S', L2>>
           ifnot op3 'A', L3>
             ifnot op4 'D', L4>
               ifnot op5 'B', L5>
                 ifnot op6 'W', L6>                         ; PSADBW
                     mov op1 0011110110 | jmp OQregMemToReg
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'H', L3>
             ifnot op4 'U', L4>
               ifnot op5 'F', L5>
                 ifnot op6 'D', L6>                         ; PSHUFD
                    ToOpcode 001100110 | mov op1 001110000 | jmp XmmMemXmmImm8
L6:              ifnot op6 'W', L6>                         ; PSHUFW
                     mov op1 001110000 | jmp mmTwoImm8
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'L', L3>
             ifnot op4 'L', L4>>
               ifnot op5 'D', L5>>
                 ifnot op6 'Q', L6>>                         ; PSLLDQ
                     If B$ImmInside = &FALSE
                        error D$MissingOperandPtr
                     Else_If B$FirstRegGender <> XmmReg
                        BadOperand
                     End_If
                     Imm8Size
                     ToOpcode 001100110, 00001111, 001110011
                     mov op1 0011111000 | or op1 B$FirstReg | LastOpcode op1
L3:        ifnot op3 'R', L3>
             ifnot op4 'L', L4>
               ifnot op5 'D', L5>
                 ifnot op6 'Q', L6>                         ; PSRLDQ
                     ToOpcode 001100110, 00001111, 001110011
                     On B$Operands <> ImmToReg, jmp L7>
                     On B$FirstRegGender <> XmmReg, jmp L7>
                     Imm8Size
                         mov op1 0011_011_000 | or op1 B$FirstReg | LastOpcode op1
L7:                  BadOperand
L3:        ifnot op3 'U', L3>
             ifnot op4 'B', L4>
               ifnot op5 'S', L5>
                 cmp op6 'B' | je L7>
                 cmp op6 'W' | jne L6>                         ; PSUBSB/W
L7:                mov cl op6 | mov op1 00_11101000 | jmp gg2
L6:              ;BadMnemonic
L5:            ifnot op5 'U', L5>
                 ifnot op6 'S', L6>                            ; PSUBUS
                   mov cl 'B' | mov op1 00_11011000 | jmp gg2
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'U', L2>
           ifnot op3 'S', L3>
             ifnot op4 'H', L4>
               ifnot op5 'A', L5>
                 ifnot op6 'D', L6>                             ; PUSHAD
                   mov op1 00_0110_0000 | jmp op
L6:             ifnot op6 'W', L6>                             ; PUSHAW
                   ToOpcode 066 | mov op1 00_0110_0000 | jmp op
L6:              BadMnemonic
L5:            ifnot op5 'F', L5>                               ; PUSHFD
                 ifnot op6 'D', L6>
                   mov op1 00_1001_1100 | jmp op
L6:              ifnot op6 'W', L6>                             ; PUSHFW
                   ToOpcode 066 | mov op1 00_1001_1100 | jmp op
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'S', L1>>
         ifnot op2 'F', L2>
           ifnot op3 'E', L3>
             ifnot op4 'N', L4>
               ifnot op5 'C', L5>
                 ifnot op6 'E', L6>                           ; SFENCE
                    ToOpcode 00001111, 0010101110, 0011111000 | ret
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'H', L2>
           ifnot op3 'U', L3>
             ifnot op4 'F', L4>
               ifnot op5 'P', L5>
                 ifnot op6 'D', L6>                           ; SHUFPD
                 ;   01100110:00001111:11000110:11 xmmreg1 xmmreg2:imm8 (0/255)
                    ToOpcode 001100110 | mov op1 0011000110 | jmp XMMmemXMMimm3
L6:              ifnot op6 'S', L6>                           ; SHUFPS
                   mov op1 0011000110 | jmp XMMmemXMMimmFF
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'Q', L2>
           ifnot op3 'R', L3>
             ifnot op4 'T', L4>
               ifnot op5 'S', L5>
                 ifnot op6 'D', L6>                       ; SQRTSD
                     ToOpcode 0011110010 | jmp L7>
L6:              IfNot op6 'S', L6>                       ; SQRTSS
                     ToOpcode 0F3 | jmp L7>
L6:              BadMnemonic
L5:            ifnot op5 'P', L5>
                 ifnot op6 'D', L6>                       ; SQRTPD
                     ToOpcode 001100110 | jmp L7>
L6:              ifnot op6 'S', L6>                       ; SQRTPS
L7:                  mov op1 001010001 | jmp XMMmemXMM
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'W', L1>
         ifnot op2 'B', L2>
           ifnot op3 'I', L3>
             ifnot op4 'N', L4>
               ifnot op5 'V', L5>
                 ifnot op6 'D', L6>                           ; WBINVD
                   mov op1 00_1111,  op2 00_1001 | jmp op_op
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ;BadMnemonic
L1:    BadMnemonic
 ________________________________________________________________________________________

SevenLetters:        On op8 > Separators,  jmp heightLetters
       ifnot op1 'C', L1>>
         ifnot op2 'L', L2>
           ifnot op3 'F', L3>
             ifnot op4 'L', L4>
               ifnot op5 'U', L5>
                 ifnot op6 'S', L6>
                   ifnot op7 'H', L7>                      ; CLFLUSH
                       Parms 1
                       On B$FirstGender <> mem, BadOperand
                       On B$FirstOperandwbit <> ByteSize, BadOperandSize
                       ToOpcode 00001111 | mov op1 0010101110, op2 00_111_000 | jmp op_modRm

; CLFLUSH: intel doc says one time "00001111:10101110:mod r/m"
; and another time: 0F AE /7 mem8

L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic

L2:      ifnot op2 'M', L2>
           ifnot op3 'P', L3>
             ifnot op4 'X', L4>
               ifnot op5 'C', L5>
                 ifnot op6 'H', L6>
                   ifnot op7 'G', L7>                      ; CMPXCHG
                     mov op1 00_1111,  op2 00_1011_0000
                 ;    cmp B$SecondReg RegEax | jne L0         ; ???????????????
                 ;    cmp B$SecondOperandWbit DoubleSize
                  ;          jne CMPXCHG8  ; ???????????????????
                       ifnot B$Operands RegToReg, L0>
                         mov op3 00_1100_0000 | jmp op_w_reg2reg1
L0:                    ifnot B$Operands RegToMem, L9>
                         jmp op_w_modReg2Rm
L9:                    ifnot B$Operands MemToReg, L8>    ; ???????
                         jmp op_w_modReg1Rm
L8:                  BadOperand
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'L', L1>
         ifnot op2 'D', L2>
           ifnot op3 'M', L3>
             ifnot op4 'X', L4>
               ifnot op5 'C', L5>
                 ifnot op6 'S', L6>
                   ifnot op7 'R', L7>                               ; LDMXCSR
                     ifnot B$FirstGender mem, L8>
                       ifnot B$FirstOperandwbit DoubleSize, L9>
                         ToOpcode 0F | mov op1 0AE, op2 00_010_000 | jmp op_ModRm
L9:                    error D$OperandSizePtr
L8:                  BadOperand
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:       BadMnemonic
L1:    ifnot op1 'M', L1>
         ifnot op2 'O', L2>
           ifnot op3 'N', L3>
             ifnot op4 'I', L4>
               ifnot op5 'T', L5>
                 ifnot op6 'O', L6>
                   ifnot op7 'R', L7>                   ; MONITOR
                     ToOpcode 0F, 01, 0C8 | ret
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:       BadMnemonic
L1:    ifnot op1 'P', L1>>
         ifnot op2 'A', L2>
           ifnot op3 'D', L3>
             ifnot op4 'D', L4>
               ifnot op5 'U', L5>
                 ifnot op6 'S', L6>
                   cmp op7 'B' | je L8>
                   cmp op7 'W' | jne L7>                ; PADDUSB/W
L8:                  mov cl op7 | mov op1 00_11011100
                        jmp gg2
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          BadMnemonic
L3:        ifnot op3 'V', L3>
             ifnot op4 'G', L4>
               ifnot op5 'U', L5>
                 ifnot op6 'S', L6>
                   ifnot op7 'B', L7>                   ; PAVGUSB
                       mov op1 0BF | jmp mm3D
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:        ifnot op2 'C', L2>>
             ifnot op3 'M', L3>>
               ifnot op4 'P', L4>>
                 ifnot op5 'E', L5>
                   ifnot op6 'Q', L6>
                     cmp op7 'B' | je L8>
                     cmp op7 'W' | je L8>
                     cmp op7 'D' | jne L7>                     ; PCMPEQB/W/D
L8:                    mov cl op7 | mov op1 00_01110100
                         jmp gg2
L7:                  ;BadMnemonic
L6:                ;BadMnemonic
L5:                ifnot op5 'G', L5>
                     ifnot op6 'T', L6>
                       cmp op7 'B' | je L8>
                       cmp op7 'W' | je L8>
                       cmp op7 'D' | jne L7>                    ; PCMPGTB/W/D
L8:                      mov cl op7 | mov op1 00_01100100
                             jmp gg2
L7:                    ;BadMnemonic
L6:                  ;BadMnemonic
L5:               ;BadMnemonic
L4:             ;BadMnemonic
L3:          BadMnemonic
L2:        ifnot op2 'F', L2>
             ifnot op3 'C', L3>
               ifnot op4 'M', L4>
                 ifnot op5 'P', L5>
                   ifnot op6 'E', L6>
                     ifnot op7 'Q', L7>    ; PFCMPEQ
                         mov op1 0B0 | jmp mm3D
L7:                  BadMnemonic
L6:                ifnot op6 'G', L6>
                     ifnot op7 'E', L7>    ; PFCMPGE
                         mov op1 090 | jmp mm3D
L7:                  ifnot op7 'T', L7>    ; PFCMPGT
                         mov op1 0A0 | jmp mm3D
L5:               ;BadMnemonic
L4:             BadMnemonic
L3:          ifnot op3 'R', L3>
               ifnot op4 'S', L4>
                 ifnot op5 'Q', L5>
                   ifnot op6 'R', L6>
                     ifnot op7 'T', L7>    ; PFRSQRT
                         mov op1 097 | jmp mm3D
L7:
L6:
L5:
L4:
L3:          BadMnemonic
L2:        ifnot op2 'M', L2>>
             ifnot op3 'A', L3>
               ifnot op4 'D', L4>
                 ifnot op5 'D', L5>
                   ifnot op6 'W', L6>
                     ifnot op7 'D', L7>                      ; PMADDWD
                        mov op1 0F5
                        If B$FirstRegGender = XmmReg
                            ToOpcode 066 | jmp XmmMemXmm
                        Else_If B$FirstRegGender = mmReg
                            jmp mmTwo
                        End_If
L7: BadMnemonic
L6:
L5:
L4:
L3:          ifnot op3 'U', L3>
               ifnot op4 'L', L4>
                 ifnot op5 'H', L5>
                   ifnot op6 'R', L6>
                     ifnot op7 'W', L7>                      ; PMULHRW
                         mov op1 0B7 | jmp mm3D
L7: BadMnemonic
L6:                ifnot op6 'U', L6>
                     ifnot op7 'W', L7>                      ; PMULHUW
                         mov op1 0011100100 | jmp OQregMemToReg
L6:     ;BadMnemonic
L7: BadMnemonic
L5:              ifnot op5 'U', L5>
                   ifnot op6 'D', L6>
                     ifnot op7 'Q', L7>                      ; PMULUDQ
                         mov op1 0F4 | jmp OQRegMemToReg
L7:
L6:
L5:
L4:
L3:          BadMnemonic
L2:        ifnot op2 'S', L2>
             ifnot op3 'H', L3>
               ifnot op4 'U', L4>
                 ifnot op5 'F', L5>
                      cmp op7 'W' | jne L7>                      ; PSHUFLW
                   If op6 = 'H'                      ; PSHUFHW
                       ToOpcode 0011110011 | jmp P0>
                   End_If
                   ifnot op6 'L', L6>
                       ToOpcode 0011110010
P0:                    mov op1 001110000 | jmp XmmMemXmmImm3
L3:          ifnot op3 'U', L3>
               ifnot op4 'B', L4>
                 ifnot op5 'U', L5>
                   ifnot op6 'S', L6>
                     cmp op7 'B' | je L8>
                     cmp op7 'W' | jne L7>                      ; PSUBUSB/W
L8:                    mov cl op7 | mov op1 00_11011000 | jmp gg2
L7:                  ;BadMnemonic
L6:                ;BadMnemonic
L5:              ;BadMnemonic
L4:            ;BadMnemonic
L3:          BadMnemonic
L2:         ifnot op2 'U', L2>
              ifnot op3 'N', L3>
                ifnot op4 'P', L4>
                  ifnot op5 'C', L5>
                    ifnot op6 'K', L6>
                      ifnot op7 'H', L7>                        ; PUNPCKH/L
                        mov cl, 'B' | mov op1 00_01101000 | jmp gg2
L7:                   ifnot op7 'L', L7>
                        mov cl, 'B' | mov op1 00_01100000 | jmp gg2
L7:                   ;BadMnemonic
L6:                 ;BadMnemonic
L5:               ;BadMnemonic
L4:             ;BadMnemonic
L3:           ;BadMnemonic
L2:       BadMnemonic
L1:    ifnot op1 'R', L1>
         ifnot op2 'S', L2>
           ifnot op3 'Q', L3>
             ifnot op4 'R', L4>
               ifnot op5 'T', L5>
                 ifnot op6 'S', L6>
                   ToOpcode 0011110011 | jmp L7>     ; RSQRTSS
L6:              ifnot op6 'P', L6>
L7:                ifnot op7 'S', L7>                ; RSQRTPS
                      mov op1 001010010 | jmp XMMmemXMM
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'S', L1>>
         ifnot op2 'Y', L2>
           ifnot op3 'S', L3>
             ifnot op4 'E', L4>
               ifnot op5 'X', L5>
                 ifnot op6 'I', L6>
                   ifnot op7 'T', L7>                ;  SYSEXIT (no use in win Apps)
                       parms 0
                       mov al 0F | stosb | mov al 035 | stosb | ret
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:      ifnot op2 'T', L2>
           ifnot op3 'M', L3>
             ifnot op4 'X', L4>
               ifnot op5 'C', L5>
                 ifnot op6 'S', L6>
                   ifnot op7 'R', L7>                ;  STMXCSR
                       parms 1
                       On B$FirstGender <> mem, BadOperand  ; xmmmemxmm
                       On B$FirstOperandWbit <> DoubleSize, BadOperandSize
                       ToOpcode 0F, 0AE
                       mov al 00_011_000 | or al B$ModBits | or al B$RmBits | LastOpcode al
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ;BadMnemonic
L1:    ifnot op1 'U', L1>
         ifnot op2 'C', L2>
           ifnot op3 'O', L3>
             ifnot op4 'M', L4>
               ifnot op5 'I', L5>
                 ifnot op6 'S', L6>
                   ifnot op7 'D', L7>                ;  UCOMISD
                       ToOpcode 001100110 | jmp L8>
L7:                ifnot op7 'S', L7>                ;  UCOMISS
L8:                   mov op1 00101110 | jmp XMMmemXMM
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ;BadMnemonic
L1:    BadMnemonic


 _________________________________________________________________________________________


heightLetters:        On B$esi+8 > Separators,  jmp NineLetters

L1:  ifnot op1 'A', L1>>
       ifnot op2 'D', L2>>
         ifnot op3 'D', L3>>
           ifnot op4 'S', L4>>
             ifnot op5 'U', L5>
               ifnot op6 'B', L6>
                 ifnot op7 'P', L7>
                   ifnot op8 'D', L8>   ; ADDSUBPD 66,0F,D0,/r ADDSUBPS xmm1, xmm2/m128
                     ToOpcode 066 | mov Op1 0D0 | jmp XmmMemXmm
L8:                ifnot op8 'S', L8>   ; ADDSUBPS F2,0F,D0,/r ADDSUBPD xmm1, xmm2/m128
                     ToOpcode 0F2 | mov Op1 0D0 | jmp XmmMemXmm
L8:                ;BadMnemonic
L8:              ;BadMnemonic
L7:            ;BadMnemonic
L6:          ;BadMnemonic
L5:        ;BadMnemonic
L4:      ;BadMnemonic
L3:    ;BadMnemonic
L2:   BadMnemonic
L1:  ifnot op1 'C', L1>>
       ifnot op2 'V', L2>>
         ifnot op3 'T', L3>>
           ifnot op4 'D', L4>>
             ifnot op5 'Q', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'P', L7>
                   ifnot op8 'D', L8>                 ; CVTDQ2PD
                     ToOpcode 0011110011 | mov op1 0011100110 | jmp XMMmemXMM
L8:                ifnot op8 'S', L8>                 ; CVTDQ2PS
                     mov op1 001011011 | jmp XMMmemXMM
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            ;BadMnemonic
L5:          BadMnemonic
L4:        ifnot op4 'P', L4>>
             ifnot op5 'D', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'D', L7>
                   ifnot op8 'Q', L8>                 ; CVTPD2DQ
                     ToOpcode 0011110010 | mov op1 0011100110 | jmp XMMmemXMM
L8:                BadMnemonic
L7:              ifnot op7 'P', L7>
                   ifnot op8 'I', L8>                 ; CVTPD2PI
                     ToOpcode 001100110 | mov op1 00101101 | jmp MMXmemXMM
L8:                ifnot op8 'S', L8>                 ; CVTPD2PS
                     ToOpcode 001100110 | mov op1 001011010 | jmp MMXmemXMM
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            BadMnemonic
L5:          ifnot op5 'I', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'P', L7>
                   ifnot op8 'D', L8>                 ; CVTPI2PD
                       ToOpcode 001100110 | jmp L9>
L8:                ifnot op8 'S', L8>                 ;  CVTPI2PS
L9:                    mov op1  00101010 | jmp XMMmemXMM
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            BadMnemonic
L5:          ifnot op5 'S', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'D', L7>
                   ifnot op8 'Q', L8>                 ;  CVTPS2DQ
                     ToOpcode 066 | mov op1 05B | jmp XMMmemXMM
L8:                BadMnemonic
L7:              ifnot op7 'P', L7>
                   ifnot op8 'D', L8>                 ;  CVTPS2PD
                      mov op1 001011010 | jmp XMMmemXMM
L8:                ifnot op8 'I', L8>                 ;  CVTPS2PI
                       mov op1  00101101 | jmp MMXmemXMM
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            ;BadMnemonic
L5:          BadMnemonic
L4:        ifnot op4 'S', L4>>
             ifnot op5 'D', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'S', L7>
                   ifnot op8 'S', L8>                 ;  CVTSD2SS
                       ToOpcode 0011110010 | mov op1 001011010 | jmp XMMmemXMM
L8:                ifnot op8 'I', L8>                 ;  CVTSD2SI
                       mov op1 02D | jmp LowXMMtoDwordWithF2
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            BadMnemonic
L5:          ifnot op5 'I', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'S', L7>
                   ifnot op8 'D', L8>                 ;  CVTSI2SD
                       mov op1 02A | jmp DwordToLowXMMWithF2
L8:                ifnot op8 'S', L8>                 ;  CVTSI2SS
L9:                    mov op1 00101010 | jmp DwordToLowXMM
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            BadMnemonic
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            BadMnemonic
L5:          ifnot op5 'S', L5>
               ifnot op6 '2', L6>
                 ifnot op7 'S', L7>
                   ifnot op8 'D', L8>                 ;  CVTSS2SD
                       ToOpcode 0011110011 | mov op1 001011010 | jmp XMMmemXMM
L8:                ifnot op8 'I', L8>                 ;  CVTSS2SI
L9:                    mov op1 00101101 | jmp LowXMMtoDword
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            ;BadMnemonic
L5:          ;BadMnemonic
L4:       ;BadMnemonic
L3:     ;BadMnemonic
L2:   BadMnemonic
L1:  ifnot op1 'M', L1>
       ifnot op2 'A', L2>
         ifnot op3 'S', L3>
           ifnot op4 'K', L4>
             ifnot op5 'M', L5>
               ifnot op6 'O', L6>
                 ifnot op7 'V', L7>
                   ifnot op8 'Q', L8>                 ; MASKMOVQ
                       mov op1 0011110111 | jmp mmOne
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            ;BadMnemonic
L5:          ;BadMnemonic
L4:       ;BadMnemonic
L3:     ;BadMnemonic
L2:   BadMnemonic
L1:  ifnot op1 'P', L1>>
       ifnot op2 'A', L2>>
         ifnot op3 'C', L3>>
           ifnot op4 'K', L4>>
             ifnot op5 'S', L5>>
               ifnot op6 'S', L6>>
                 ifnot op7 'D', L7>
                   ifnot op8 'W', L8>                 ; PACKSSDW
                       .If B$FirstRegGender = XmmReg
                           ToOpcode 001100110 | mov B$FirstRegGender mmReg
                           If B$Operands = RegToReg
                               On B$SecondRegGender = XmmReg, mov B$SecondRegGender mmReg
                           End_If
                       .End_If
                       mov op1 00_01101011 | jmp mmTwo
L8:                BadMnemonic
L7:              ifnot op7 'W', L7>
                   ifnot op8 'B', L8>                 ; PACKSSWB
                       .If B$FirstRegGender = XmmReg
                           ToOpcode 001100110 | mov B$FirstRegGender mmReg
                           If B$Operands = RegToReg
                               On B$SecondRegGender = XmmReg, mov B$SecondRegGender mmReg
                           End_If
                       .End_If
                     mov op1 00_01100011 | jmp mmTwo
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            BadMnemonic
L5:          ifnot op5 'U', L5>
               ifnot op6 'S', L6>
                 ifnot op7 'W', L7>
                   ifnot op8 'B', L8>                 ; PACKUSWB
                       .If B$FirstRegGender = XmmReg
                           ToOpcode 001100110 | mov B$FirstRegGender mmReg
                           If B$Operands = RegToReg
                               On B$SecondRegGender = XmmReg, mov B$SecondRegGender mmReg
                           End_If
                       .End_If
                       mov op1 00_01100111 | jmp mmTwo
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            ;BadMnemonic
L5:          ;BadMnemonic
L4:        ;BadMnemonic
L3:      BadMnemonic
L2:      ifnot op2 'M', L2>
           ifnot op3 'O', L3>
             ifnot op4 'V', L4>
               ifnot op5 'M', L4>
                 ifnot op6 'S', L4>
                   ifnot op7 'K', L4>
                     ifnot op8 'B', L4>                   ; PMOVMSKB
                         mov op1 0D7 | jmp OQreg32Reg
L4: BadMnemonic
L3:        ifnot op3 'U', L3>
             ifnot op4 'L', L4>>
               ifnot op5 'L', L4>>
                 ifnot op6 'U', L4>>
                   ifnot op7 'D', L4>>
                     ifnot op8 'Q', L4>>                   ; PMULLUDQ
                         mov op1 0011110100 | jmp OQregMemToReg
L3: BadMnemonic
L2:      ifnot op2 'R', L2>
           ifnot op3 'E', L3>
             ifnot op4 'F', L4>
               ifnot op5 'E', L5>
                 ifnot op6 'T', L6>
                   ifnot op7 'C', L7>
                     ifnot op8 'H', L8>                   ; PREFETCH (AMD).
                         cmp B$ParametersNumber 1 | jne L9>
                         cmp B$FirstGender mem | jne L9>
                         cmp B$FirstOperandwBit ByteSize | jne L9>
                       ;  cmp B$SIBinside &TRUE | je L9>
                       ;  cmp B$EregInside &TRUE | je L9>
                             mov al 0F | stosb | mov al 0D | stosb
                             mov al B$ModBits | or al B$RmBits | stosb | ret
L9: error D$PrefetchMemPtr
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:            ;BadMnemonic
L5:          ;BadMnemonic
L4:        ;BadMnemonic
L3: BadMnemonic
L2:    ifnot op2 'U', L2>
         ifnot op3 'N', L3>
           ifnot op4 'P', L4>
             ifnot op5 'C', L5>
               ifnot op6 'K', L6>
                 ifnot op7 'H', L7>
                   cmp op8 'B' | je L9>
                   cmp op8 'W' | je L9>
                   cmp op8 'D' | jne L8>                         ; PUNPCKHB/W/D
PUNPCKH_one:
L9:                  mov cl op8
                     mov op1 00_01101000 | jmp gg2
L8:                BadMnemonic
L7:              ifnot op7 'L', L7>
                   cmp op8 'B' | je L9>
                   cmp op8 'W' | je L9>
                   cmp op8 'D' | jne L8>                         ; PUNPCKLB/W/
PUNPCKL_one:
L9:                  mov cl op8
                     mov op1 00_01100000 | jmp gg2
L8:                ;BadMnemonic
L7:              ;BadMnemonic
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:    ifnot op2 'F', L2>
         ifnot op3 'R', L3>
           ifnot op4 'C', L4>
             ifnot op5 'P', L5>
               ifnot op6 'I', L6>
                 ifnot op7 'T', L7>
                   ifnot op8 '1', L8>           ; PFRCPIT1
                       mov op1 0A6 | jmp mm3D
L8:                ifnot op8 '2', L8>           ; PFRCPIT2
                       mov op1 0B6 | jmp mm3D
L8:                  ;BadMnemonic
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:           BadMnemonic
L4:        ifnot op4 'S', L4>
             ifnot op5 'Q', L5>
               ifnot op6 'I', L6>
                 ifnot op7 'T', L7>
                   ifnot op8 '1', L8>           ; PFRSQIT1
                       mov op1 0A7 | jmp mm3D
L8:                  ;BadMnemonic
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'U', L1>
         ifnot op2 'N', L2>
           ifnot op3 'P', L3>
             ifnot op4 'C', L4>
               ifnot op5 'K', L5>
                 ifnot op6 'H', L6>
                   ifnot op7 'P', L7>
                     ifnot op8 'D', L8>                             ; UNPCKHPD
                         ToOpcode 001100110 | jmp L9>
L8:                  ifnot op8 'S', L8>                             ; UNPCKHPS
L9:                      mov op1 0010101 | jmp XMMmemXMM
L8:                  ;BadMnemonic
L7:                BadMnemonic
L6:              ifnot op6 'L', L6>
                   ifnot op7 'P', L7>
                     ifnot op8 'D', L8>                             ; UNPCKLPD
                         ToOpcode 001100110 | jmp L9>
L8:                  ifnot op8 'S', L8>                             ; UNPCKLPS
L9:                      mov op1 0010100 | jmp XMMmemXMM
L8:                  ;BadMnemonic
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      BadMnemonic
L1:    ifnot op1 'S', L1>
         ifnot op2 'Y', L2>
           ifnot op3 'S', L3>
             ifnot op4 'E', L4>
               ifnot op5 'N', L5>
                 ifnot op6 'T', L6>
                   IFNOT op7 'E', L7>
                     ifnot op8 'R', L8>                             ; SYSENTER
                       parms 0
                       mov al 0F | stosb | mov al 034 | stosb | ret
L8:                  ;BadMnemonic
L7:                ;BadMnemonic
L6:              ;BadMnemonic
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:      ;BadMnemonic
L1:    BadMnemonic

____________________________________________________________________________________________


NineLetters:   cmp B$esi+9 Separators | ja TenLetters

       ifnot op1 'C', L1>>
         ifnot op2 'M', L2>
           ifnot op3 'P', L3>>
             ifnot op4 'X', L3>>
               ifnot op5 'C', L3>>
                 ifnot op6 'H', L3>>
                   ifnot op7 'G', L3>>
                     ifnot op8 '8', L3>>
                       ifnot B$esi+8 'B', L3>>             ; CMPXCHG8B
                         On B$FirstOperandwbit <> QuadSize, error D$OperandSizePtr
                         mov op1 0F, op2 0C7, op3 00_001_000
                         jmp Op_Op_ModRm

L0:                        BadOperand
; ...............
L2:      ifnot op2 'V', L2>
           ifnot op3 'T', L3>
             ifnot op4 'T', L3>
               ifnot op5 'P', L5>
                 ifnot op6 'D', L6>
                   ifnot op7 '2', L3>
                     ifnot op8 'D', L8>
                       ifnot B$esi+8 'Q', L3>             ; CVTTPD2DQ
                           ToOpcode 001100110 | mov op1 0011100110 | jmp XMMmemXMM
L3:                    ifnot B$esi+8 'I', L3>             ; CVTTPD2PI
                          ToOpcode 066 | mov op1 02C | jmp MMXmemXMM
L6:              ifnot op6 'S', L3>
                   ifnot op7 '2', L3>
                     ifnot op8 'D', L8>
                       ifnot B$esi+8 'Q', L3>             ; CVTTPS2DQ
                           ToOpcode 0011110011 | mov op1 001011011 | jmp XMMmemXMM
L8:                  ifnot op8 'P', L3>
                       ifnot B$esi+8 'I', L3>             ; CVTTPS2PI
C1:                      mov op1 00101100 | jmp MMXmemXMM
L2:
L3: BadMnemonic

L5:            ifnot op5 'P', L5>
                 ifnot op6 'S', L3>
                   ifnot op7 '2', L3>
                     ifnot op8 'P', L3>
                       ifnot B$esi+8 'I', L3>             ; CVTTPS2SI
C1:                       mov op1 00101100 | jmp LowXMMtoDword
L5:            ifnot op5 'S', L3>
                 ifnot op6 'D', L5>
                   ifnot op7 '2', L3>
                     ifnot op8 'S', L3>
                       ifnot B$esi+8 'I', L3>             ; CVTTSD2SI
                           mov op1 02C | jmp LowXMMtoDwordWithF2
L5:              ifnot op6 'S', L3>
                   ifnot op7 '2', L3>
                     ifnot op8 'S', L3>
                       ifnot B$esi+8 'I', L3>             ; CVTTSS2SI r32, xmm/m32
                           mov op1 02C | jmp LowXMMtoDword

L3: BadMnemonic

L1:    ifnot op1 'P', L3>>
         ifnot op2 'R', L2>>
           ifnot op3 'E', L3>>
             ifnot op4 'F', L3>>
               ifnot op5 'E', L3>>
                 ifnot op6 'T', L3>>
                   ifnot op7 'C', L3>>
                     ifnot op8 'H', L3>>                   ; PREFETCH0/1/2 (Intel).
                       If B$esi+8 = 'W'
                        ; AMD PREFETCHW:
                          mov al 0F | stosb | mov al 0D | stosb
                          mov al 00_001_000 | or al B$ModBits | or al B$RmBits | stosb | ret
                       End_If
                       mov bl B$esi+8                     ; 'W/0/1/2'
                       inc bl | sub bl '0'                ;    1/2/3
                       cmp bl 3 | ja L3>>                 ; Same as PREFETCHT0/1/2 (alternate).
                       cmp bl 0 | je L3>>
                       cmp B$ParametersNumber 1 | jne L1>
                         cmp B$FirstGender mem | jne L1>
                         cmp B$FirstOperandwBit ByteSize | jne L1>
                       ;  cmp B$SIBinside &TRUE | je L1>
                       ;  cmp B$EregInside &TRUE | je L1>
                             mov al 0F | stosb
                             mov al 018                ; INTEL (0/1/2)
                             stosb
                             mov al bl | shl al 3
                             or al B$ModBits | or al B$RmBits | stosb | ret
L1: error D$PrefetchMemPtr

; PUNPCKH.. / PUNPCKL.. are send to PUNPCKH / L .

L2:     ifnot op2 'U', L3>>
           ifnot op3 'N', L3>>
             ifnot op4 'P', L3>>
               ifnot op5 'C', L3>>
                 ifnot op6 'K', L3>>
                   ifnot op7 'L', L7>
                     If W$esi+7 = 'BW'            ; PUNPCKLBW
                         jmp PUNPCKL_one
                     Else_If W$esi+7 = 'WD'       ; PUNPCKLWD
                         jmp PUNPCKL_one
                     Else_If W$esi+7 = 'DQ'       ; PUNPCKLDQ
                         jmp PUNPCKL_one
                     End_If
                     BadMnemonic
L7:               ifnot op7 'H', L3>
                     If W$esi+7 = 'BW'            ; PUNPCKLBW
                         jmp PUNPCKH_one
                     Else_If W$esi+7 = 'WD'       ; PUNPCKLWD
                         jmp PUNPCKH_one
                     Else_If W$esi+7 = 'DQ'       ; PUNPCKLDQ
                         jmp PUNPCKH_one
                     End_If
L3: BadMnemonic

TenLetters: cmp B$esi+10 Separators | ja ElevenLetters
       ifnot op1 'M', L1>>
         ifnot op2 'A', L3>>
           ifnot op3 'S', L3>>
             ifnot op4 'K', L3>>
               ifnot op5 'M', L3>>
                 ifnot op6 'O', L3>>
                   ifnot op7 'V', L3>>
                     ifnot op8 'D', L3>>
                       ifnot B$esi+8 'Q', L3>>
                         ifnot B$esi+9 'U', L3>>   ; MASKMOVDQU
                             ToOpcode 001100110 | mov op1 0011110111 | jmp XmmXmm
L3: BadMnemonic

L1:    ifnot op1 'P', L3>>
         ifnot op2 'R', L2>>
           ifnot op3 'E', L3>>
             ifnot op4 'F', L3>>
               ifnot op5 'E', L3>>
                 ifnot op6 'T', L3>>
                   ifnot op7 'C', L3>>
                     ifnot op8 'H', L3>>                   ; PREFETCHT0/1/2 (Intel).
                       ifnot B$esi+8 'T', L3>>            ; Same as PREFETCH0/1/2
                       mov bl B$esi+9                     ; 'W/0/1/2'
                       inc bl | sub bl '0'                ;    1/2/3
                       cmp bl 3 | ja L3>>
                       cmp bl 0 | je L3>>
                       cmp B$ParametersNumber 1 | jne L1>
                         cmp B$FirstGender mem | jne L1>
                         cmp B$FirstOperandwBit ByteSize | jne L1>
                       ;  cmp B$SIBinside &TRUE | je L1>
                       ;  cmp B$EregInside &TRUE | je L1>
                             mov al 0F | stosb
                             mov al 018                ; INTEL (0/1/2)
                             stosb
                             mov al bl | shl al 3
                             or al B$ModBits | or al B$RmBits | stosb | ret
L1: error D$PrefetchMemPtr

L2:      ifnot op2 'U', L3>>
           ifnot op3 'N', L3>>
             ifnot op4 'P', L3>>
               ifnot op5 'C', L3>>
                 ifnot op6 'K', L3>>
                   ifnot op7 'H', L7>>
                     mov op1 0001101101
L8:                  ifnot op8 'Q', L3>>
                       ifnot B$esi+8 'D', L3>>
                         ifnot B$esi+9 'Q', L3>>  ; PUNPCKHQDQ
                             ToOpcode 066 | jmp XmmMemXmm
L7: mov op1 001101100 | jmp L8<                    ; PUNPCKLQDQ

L3: BadMnemonic

ElevenLetters: cmp B$esi+11 Separators | ja TwelveLetters

L1:    ifnot op1 'P', L3>>
         ifnot op2 'R', L3>>
           ifnot op3 'E', L3>>
             ifnot op4 'F', L3>>
               ifnot op5 'E', L3>>
                 ifnot op6 'T', L3>>
                   ifnot op7 'C', L3>>
                     ifnot op8 'H', L3>>                   ; PREFETCHNTA
                       ifnot B$esi+8 'N', L3>>
                         ifnot B$esi+9 'T', L3>>
                           ifnot B$esi+10 'A', L3>>
                             cmp B$ParametersNumber 1 | jne L1>
                               cmp B$FirstGender mem | jne L1>
                               cmp B$FirstOperandwBit ByteSize | jne L1>
                             ;  cmp B$SIBinside &TRUE | je L1>
                             ;  cmp B$EregInside &TRUE | je L1>
                                   mov al 0F | stosb | mov al 018 | stosb
                                   mov al 0
                                   or al B$ModBits | or al B$RmBits | stosb | ret
L1: error D$PrefetchMemPtr
L3: BadMnemonic


TwelveLetters: BadMnemonic
 _________________________________________________________________________________________

; 'J'  >>>  all 'J'  first letter here
;
;  Special treatment for all 'tttn' opcodes and all '.MOV...' instructions
 _________________________________________________________________________________________


J_Branching:
    If op1 = 'U'
        ToOpcode 02E
    Else_If op1 = 'L'
        ToOpcode 03E
    End_If
    add esi 4 | call Store8cars

Letter_J:        On op2 = 'M',  jmp JMPmnemo

         ifnot op2 'C', L2>
           ifnot op3 'X', L1>
             ifnot op4 'Z', L1>                     ; JCXZ
               cmp op5 Separators | ja L1>
                 ToOpcode 067 | jmp X0>

L2:      ifnot op2 'E', L1>
           ifnot op3 'C', L1>
             ifnot op4 'X', L1>
               ifnot op5 'Z', L1>                   ; JECXZ
                 cmp op6 Separators | ja L1>
X0:                cmp B$LocalSize DownShort | je R1>
                   cmp B$LocalSize UpShort | jne R0>
R1:                  mov op1 00_1110_0011 | jmp op_dis8
R0:                error D$NoPlainLabelForJECXPtr


L1:      push esi
           inc esi
           call SearchFortttnBits
         pop esi
      ;   call Store8Cars

; JCC:
    On B$FirstGender <> Dis, BadOperand

    cmp B$LocalSize DownShort | je L1>
    cmp B$LocalSize UpShort | jne L2>
L1:   mov op1 00_0111_0000 | or op1 B$tttnBits | jmp op_dis8
L2: cmp B$LocalSize DownLong | je L3>
    cmp B$LocalSize Uplong | jne L4>
L3:   mov D$Relative RelativeFlag
      On B$ShortenJumpsWanted = &TRUE, call SetShortenJmpFlag
      mov op1 00_1111,  op2 00_1000_0000 | or op2 B$tttnBits | jmp op_op_dis
L4: cmp B$LabelInside &TRUE | je L3<

    BadOperand


JMPmnemo:
            ifnot op3 'P', L9>>
              cmp op4 Separators | ja L1>>
              mov D$Relative RelativeFlag
              cmp B$LocalSize DownShort | Je L4>
              cmp B$LocalSize UpShort | jne L5>

              ;On B$ShortenJumpsWanted = &TRUE, call SetShortenJmpFlag

L4:           mov op1 00_1110_1011 | jmp op_dis8     ; Short
L5:           cmp B$FirstGender dis | jne L6>        ; Long
                cmp B$LocalSize DownLong | je K0>
                cmp B$LocalSize UpLong | jne K1>
K0:                  On B$ShortenJumpsWanted = &TRUE, call SetJMPShortenJmpFlag
K1:                  mov op1 00_1110_1001 | jmp op_dis
L6:           cmp B$FirstGender Reg | jne L7>
                mov op1 00_1111_1111,  op2 00_11_100_000 | jmp op_reg1
L7:           ifnot B$FirstGender mem, L8>
                mov D$Relative 0
                mov op1 00_1111_1111,  op2 00_100_000 | jmp op_modRm
L8:           BadOperand
L9:    BadMnemonic

L1:         ifnot op4 'F', L3>>
             cmp op5 Separators | ja L3>>
L5:             ifnot B$FirstGender mem, L4>
                ifnot B$ParametersNumber 2, L5>             ; JMPF jE! added
                ifnot B$SecondGender imm, L4>
                cmp B$EregInside 0 | jne L4>
                ToOpcode  0EA
                cmp B$FirstParaMem 'W' | je L2>
                cmp B$FirstParaMem 'D' | jne L4>
                cmp B$PossibleImmLabel 0 | je L8>; | int 3
                mov D$CodeListPtr edi | pushad | call SecondParameterLabel | popad
            L8:
                mov eax D$imm32 | stosd | jmp L6>
            L2: mov eax D$imm32 | stosw | test eax 0FFFF0000 | jne L7>
            L6: mov eax D$dis32 | stosw | test eax 0FFFF0000 | jne L7>
                call ClearParameters
                ret
L5:           mov op1 00_1111_1111,  op2 0010_1000 | jmp op_modRm
L4:          BadOperand
L3:         BadMnemonic
L7:       error D$OverWordPtr
 ________________________________________________________________________________________

; MOVs
 ________________________________________________________________________________________

; (segments registers are over 00111)

MOVinstructions:                                        ; killing exceptions !!!
                 On op4 <> Space,  jmp MOVsMnemo

      ifnot B$Operands RegToreg, L1>>
        ifnot B$SecondRegGender sReg, L2>               ; general purpose? yes > L2
          On B$FirstRegGender <> Reg,  error D$GPregisterPtr
          GPreg1
          mov op1 00_1000_1100,  op2 00_1100_0000
          or op2 B$SecondReg | or op2 B$FirstReg | jmp op_op_P2     ; reg2 is an sreg
L2:     ifnot B$FirstRegGender sReg, L2>               ; general purpose? yes > L2
          GPreg2
          ifnot B$FirstReg RegSS, L3>
            mov op1 00_1000_1110,  op2 00_1100_0000           ; reg1 is an RegSS
            or op2 B$FirstReg | or op2 B$SecondReg | jmp op_op_P2
L3:       mov op1 00_1000_1110,  op2 00_1100_0000             ; reg1 is an sreg
          or op2 B$FirstReg | or op2 B$SecondReg | jmp op_op_P2

L2:   ifnot B$SecondRegGender dReg, L2>               ; Debug Register2?
        On B$FirstRegGender <> Reg,  error D$GPregisterPtr
        GPreg1
        ToOpcode 0F, 0010_0001  ; 0000 1111 : 0010 0001 : 11 eee reg
        mov al B$SecondReg | shl al 3 | or al B$FirstReg | or al 0011_000_000
        LastOpcode al

L2:   ifnot B$FirstRegGender dReg, L2>               ; Debug Register1?
        GPreg2
        ToOpcode 0F, 0010_0011  ; 0000 1111 : 0010 0011 : 11 eee reg
        mov al B$FirstReg | shl al 3 | or al B$SecondReg | or al 0011_000_000
        LastOpcode al

L2:   ifnot B$SecondRegGender cReg, L2>               ; Control Register2?
        On B$FirstRegGender <> Reg,  error D$GPregisterPtr
        GPreg1
        ToOpcode 0F, 0010_0000  ; 0000 1111 : 0010 0000 : 11 eee reg
        mov al B$SecondReg | shl al 3 | or al B$FirstReg | or al 0011_000_000
        LastOpcode al

L2:   ifnot B$FirstRegGender cReg, L2>               ; Control Register1?
        GPreg2
        ToOpcode 0F, 0010_0010  ; 0000 1111 : 0010 0010 : 11 eee reg
        mov al B$FirstReg | shl al 3 | or al B$SecondReg | or al 0011_000_000
        LastOpcode al

L2:     mov op1 00_1000_1010,  op2 00_1100_0000 | jmp w_reg1reg2 ; 1/2 general P.

L1:   ifnot B$Operands MemToReg, L1>
        ifnot B$FirstRegGender sReg, L2> | mov B$wBitDisagree &FALSE ; jE!
          ifnot B$FirstReg RegSS, L3>
            mov op1 00_1000_1110
              mov op2 B$FirstReg | jmp op_modRm_P2
L3:       mov op1 00_1000_1110,  op2 B$FirstReg | jmp op_modRm_P2
L2:     GPreg1
        cmp B$FirstReg RegEAX | jne L3>
          cmp B$ModBits 0 | jne L3>
            cmp B$RmBits 00101 | jne L3>
              mov op1 00_1010_0000 | jmp w_dis  ;!!!!!!!!! simple label only !!!!!!!!

L3:     mov op1 00_1000_1010 | jmp w_modReg1Rm

L1:   ifnot B$Operands RegToMem, L1>
        ifnot B$SecondRegGender sReg, L2> | mov B$wBitDisagree &FALSE ; jE!
          mov op1 00_1000_1100,  op2 B$SecondReg | jmp op_modRm_P2
L2:    GPreg2
       cmp B$SecondReg RegEAX | jne L3>
          cmp B$ModBits 0 | jne L3>
            cmp B$RmBits 00101 | jne L3>
               mov op1 00_1010_0010 | jmp w_dis  ;!!!!!!!!! simple label only !!!!!!!!

L3:     mov op1 00_1000_1000 | jmp w_modreg2Rm

L1:  ifnot B$Operands ImmToreg, L1>
       ;mov op1 00_1100_0110,  op2 00_1100_0000 | jmp w_reg1_imm ; alternate shorter >
        mov op1 B$wBit | shl op1 3 | or op1 00_1011_0000 | jmp reg_in_op_imm
L1:  ifnot B$Operands ImmToMem, L1>
        On B$FirstOperandwbit >= QuadSize, jmp L1>

        mov op1 00_1100_0110,  op2 0 | jmp w_modRm_imm

L1:  BadOperand


;MOV – Move to/from Control Registers
;CR0 from register               0000 1111 : 0010 0010 : 11 000 reg
;CR2 from register               0000 1111 : 0010 0010 : 11 010 reg
;CR3 from register               0000 1111 : 0010 0010 : 11 011 reg
;CR4 from register               0000 1111 : 0010 0010 : 11 100 reg
;register from CR0-CR4           0000 1111 : 0010 0000 : 11 eee reg

;MOV – Move to/from Debug Registers
;DR0-DR3 from register           0000 1111 : 0010 0011 : 11 eee reg
;DR4-DR5 from register           0000 1111 : 0010 0011 : 11 eee reg
;DR6-DR7 from register           0000 1111 : 0010 0011 : 11 eee reg
;register from DR6-DR7           0000 1111 : 0010 0001 : 11 eee reg
;register from DR4-DR5           0000 1111 : 0010 0001 : 11 eee reg
;register from DR0-DR3           0000 1111 : 0010 0001 : 11 eee reg


MOVsMnemo:
    On op5 > Separators, jmp L0>>
      ifnot op4 'D', L4>                                ; MOVD
        If B$FirstGender = mem
            On B$FirstOperandwBit <> DoubleSize, BadOperandSize
        Else_If B$SecondGender = mem
            On B$SecondOperandwBit <> DoubleSize, BadOperandSize
        End_If
          mov op1 00_01101110
          cmp B$FirstRegGender XmmReg | je XmmFour
          cmp B$SecondRegGender XmmReg | je XmmFour
          jmp mmFour
L4:   ifnot op4 'Q', L4>                                ; MOVQ ; THE HELL!!!!!!!!!!!!!!
         If B$FirstRegGender = XmmReg
             ToOpcode 0F3 | mov op1 07E | jmp MovqXMMmemXMMmem ;XMMtoFromMem ; MovqXMMmemXMMmem
         Else_If B$SecondRegGender = XmmReg
             ToOpcode 066 | mov op1 0C6 | jmp MovqXMMmemXMMmem; XMMtoFromMem ; MovqXMMmemXMMmem
           ; This 0C6 will be 0D6 by the xor in the 'MovqXMMmemXMMmem' Routine.
           ; This encodage is not fully sure. May be an error in Intel Doc. Not
           ; implemented in NASM...
         Else_If B$Operands = RegToMem
            mov op1 07F | jmp MMregToMem ;mmTwo ;mmFour
         Else
            mov op1 06F | jmp mmTwo ;mmFour
         End_If

L4:   On op4 = 'S',  error D$NotYetMnemoPtr       ; MOVS  (???)

BadMnemonic

L0: On op6 > Separators,  jmp MovSixLetters


L4:   ifnot op4 'S', L4>>
        ifnot op5 'B', L5>                  ; MOVSB
          mov op1 00_1010_0100 | jmp op
L5:     ifnot op5 'W', L5>                  ; MOVSW
          ToOpcode 066
          mov op1 00_1010_0101 | jmp op
L5:     ifnot op5 'D', L5>                  ; MOVSD
          If B$FirstRegGender = XmmReg
            ; down there
          Else_If B$SecondRegGender = XmmReg
            ; down there
          Else
              mov op1 00_0101_00101 | jmp op
          End_If

          ToOpcode 0011110010 | mov op1 00010000 | jmp XMMmemXMMmem

L5:     ifnot op5 'S', L5>                      ; MOVSS (SSE SIMD)
          ToOpcode 0011110011
          mov op1 010 | jmp XMMmem32XMMmem32
L5:     ifnot op5 'X', L5>
          mov op1 00_1111,  op2 00_1011_1110    ; MOVSX
          ifnot B$Operands RegToreg, L6>
            mov op3 00_1100_0000 | jmp op_w_reg1reg2X
L6:       ifnot B$Operands MemToreg, L6>
            jmp op_w_modReg1RmX
L6:       ;BadOperand
L5:     BadMnemonic
L4:     ifnot op4 'Z', L4>                      ; MOVZX
          ifnot op5 'X', L4>
            mov op1 00_1111
            mov op2 00_1011_0110
            ifnot B$Operands RegToReg, L7>
              mov op3 00_1100_0000 | jmp op_w_reg1reg2X
L7:         ifnot B$Operands MemToReg, L7>
              jmp op_w_modReg1RmX
L7:         BadOperand
L6:       ;BadMnemonic
L5:     ;BadMnemonic
L4:   BadMnemonic


MovSixLetters:  On op7 > Separators, jmp MovSevenLetter

      ifnot op4 'A', L4>
        ifnot op5 'P', L5>
          ifnot op6 'D', L6>      ; MOVAPD
              ToOpcode 001100110 | mov op1 00101000 | jmp XMMmemXMMmem
L6:       ifnot op6 'S', L6>      ; MOVAPS
            mov op1 00101000 | jmp XMMmemXMMmem
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'D', L4>
        ifnot op5 'Q', L5>
          ifnot op6 'A', L6>      ; MOVDQA
              ToOpcode 001100110 | mov op1 06F | jmp MovqXMMmemXMMmem
L6:       ifnot op6 'U', L6>      ; MOVDQU
              ToOpcode 0011110011 | mov op1 001101111 | jmp MovqXMMmemXMMmem
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'H', L4>
        ifnot op5 'P', L5>
          ifnot op6 'D', L6>      ; MOVHPD
              ToOpcode 001100110 | mov op1 00010110 | jmp XMMtoFromMem
L6:       ifnot op6 'S', L6>      ; MOVHPS
            mov op1 0010110 | jmp XMMTOFromMem
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'L', L4>
        ifnot op5 'P', L5>
          ifnot op6 'D', L6>      ; MOVLPD
              ToOpcode 001100110 | jmp L7>
L6:       ifnot op6 'S', L6>      ; MOVLPS
L7:           mov op1 0010010 | jmp XMMTOFromMem
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:         ifnot op4 'N', L4>
               ifnot op5 'T', L5>
                 Ifnot op6 'I', L6>                         ; MOVNTI
                     ToOpcode 0F
                     mov op1 0011000011 | jmp op_modReg2Rm
L6:              Ifnot op6 'Q', L6>                         ; MOVNTQ
                     mov op1 0011100111 | jmp MMregToMem
L6:              ;BadMnemonic
L5:            BadMnemonic
L4:   ifnot op4 'U', L4>
        ifnot op5 'P', L5>
          ifnot op6 'D', L6>      ; MOVUPD
            ToOpcode 001100110 | jmp L7>
L6:       ifnot op6 'S', L6>      ; MOVUPS
L7:         mov op1 0010000 | jmp XMMmemXMMmem
L6:       ;BadMnemonic
L5:     ;BadMnemonic
L4:   BadMnemonic


MovSevenLetter: On op8 > Separators, jmp MovHeightLetters
      ifnot op4 'D', L4>
        ifnot op5 'Q', L5>
          ifnot op6 '2', L6>
            ifnot op7 'Q', L7>                   ; MOVDQ2Q
                ToOpcode 0011110010 | mov op1 0011010110 | jmp mmXmm
L7:         ;BadMnemonic
L6:       BadMnemonic
L5:     ifnot op5 'D', L5>
          ifnot op6 'U', L6>
            ifnot op7 'P', L7>                   ; MOVDDUP F2,0F,12,/r MOVDDUP xmm1, xmm2/m64
              ToOPcode 0F2 | mov Op1 012 | jmp XmmMemXmm
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'H', L4>
        ifnot op5 'L', L5>
          ifnot op6 'P', L6>
            ifnot op7 'S', L7>                   ; MOVHLPS
              mov op1 0010010 | jmp XMMXMM
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'L', L4>
        ifnot op5 'H', L5>
          ifnot op6 'P', L6>
            ifnot op7 'S', L7>                   ; MOVLHPS
              mov op1 0010110 | jmp XMMXMM
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'N', L4>
        ifnot op5 'T', L5>
          ifnot op6 'D', L6>
            ifnot op7 'Q', L7>                               ; MOVNTDQ
                ToOpcode 001100110 | mov op1 0011100111 | jmp XMMtoMem
L7:         BadMnemonic
L6:       ifnot op6 'P', L6>
            ifnot op7 'D', L7>                               ; MOVNTPD
                ToOpcode 001100110 | jmp L8>
L7:         ifnot op7 'S', L7>                               ; MOVNTPS
L8:             mov op1 00101011 | jmp XMMtoMem
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     BadMnemonic
L4:   ifnot op4 'Q', L4>
        ifnot op5 '2', L5>
          ifnot op6 'D', L6>
            ifnot op7 'Q', L7>                   ; MOVQ2DQ
                ToOpcode 0011110011 | mov op1 0011010110 | jmp Xmmmm ;XmmXmm mmXmm
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     ;BadMnemonic
L4:   BadMnemonic



MovHeightLetters: On B$esi+8 > Separators, BadMnemonic
      ifnot op4 'M', L4>
        ifnot op5 'S', L5>
          ifnot op6 'K', L6>
            ifnot op7 'P', L7>
              ifnot op8 'D', L8>                     ; MOVMSKPD
                ; 01100110:00001111:01010000:11 r32 xmmreg
                ToOpcode 001100110 | jmp L9>
L8:           ifnot op8 'S', L8>                     ; MOVMSKPS
                ; 00001111:01010000:11 r32 xmmreg    ; (unique)
L9:             ToOpcode 001111, 001010000
                cmp B$Operands RegToReg | jne L9>
                cmp B$FirstRegGender Reg | jne L9>>
                cmp B$SecondRegGender XMMreg | jne L9>>
                On B$FirstOperandwBit <> DoubleSize, BadOperandSize
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                LastOpcode al
L9:             BadOperand
L8:           ;BadMnemonic
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     ;BadMnemonic
L4:   ifnot op4 'S', L4>
        ifnot op5 'H', L5>
          ifnot op6 'D', L6>
            ifnot op7 'U', L7>
              ifnot op8 'P', L8>    ; MOVSHDUP: F3,0F,16,/r MOVSHDUP xmm1, xmm2/m128
                ToOpcode 0F3 | mov Op1 016 | jmp XmmMemXmm
L8:           ;BadMnemonic
L7:         ;BadMnemonic
L6:       BadMnemonic
L5:     ifnot op5 'L', L5>
          ifnot op6 'D', L6>
            ifnot op7 'U', L7>
              ifnot op8 'P', L8>    ; MOVSLDUP: F3,0F,12,/r MOVSLDUP xmm1, xmm2/m128
                ToOpcode 0F3 | mov Op1 012 | jmp XmmMemXmm
L8:           ;BadMnemonic
L7:         ;BadMnemonic
L6:       ;BadMnemonic
L5:     ;BadMnemonic
L4:   BadMnemonic



SETinstructions:

    push esi
      add esi 3 | call SearchFortttnBits
    pop esi

    mov op1 00_1111,  op2 00_1001_0000 | or op2 B$tttnBits
    On B$FirstOperandWbit <> 0,  error D$NeedByteSizePtr
    ifnot B$FirstGender Reg, L1>
      mov op3 00_1100_0000 | jmp op_op_reg1
L1: ifnot B$FirstGender mem, L2>
     mov op3 0 | jmp op_op_modRm
L2: BadOperand



CMOVinstructions:                      ; CMOVcc

    push esi
      add esi 4 | call SearchFortttnBits
    pop esi

    mov op1 00_1111,  op2 00_0100_0000 | or op2 B$tttnBits

    ifnot B$Operands RegToReg, L1>
      mov op3 00_1100_0000 | jmp op_op_reg1reg2
L1: ifnot B$Operands MemToreg, L2>
      jmp op_op_modReg1Rm                           ; Intel doc says: ModMemRm (???!!!)
L2: BadOperand

____________________________________________________________________________________________
; XMM comparisons:
;
; the notation i choose is, for example, CMP_PS_AE.
; Setting the conditions a end simplify analyzes.
;
; Encodage is ended by a 8 bit imm (from 0 to 7) specifying the comparison mode.

XMMcomparePS: ; CMPPS... encounted.
    mov B$imm32 0FF, B$immInside &TRUE, B$TrueSize ByteSize

    ...If B$esi+7 < Separators             ; B$esi+7 = op8 > 7 Chars mnemonic
        ..If op6 = 'E'
            If op7 = 'A'
                mov B$imm32 0                   ; =  EA
            End_If
        ..Else_If op6 = 'L'
            If op7 = 'T'
                mov B$imm32 1                   ; <  LT
            Else_If op7 = 'E'
                mov B$imm32 2                   ; <= LE
            End_If
        ..End_If

    ...Else_If B$esi+8 < Separators        ; > 8 Chars
        ..If op6 = 'N'
            .If op7 = 'E'
                If op8 = 'Q'
                    mov B$imm32 4               ; <> NEQ
                End_If
            .Else_If op7 = 'L'
                If op8 = 'T'
                    mov B$imm32 5               ; NLT  (not <)
                Else_If op8 > 'E'
                    mov B$imm32 6               ; NLE  (not <=)
                End_If
            .End_If
        ..Else_If op6 = 'O'
            .If op7 = 'R'
                If op8 = 'D'
                    mov B$imm32 7               ; Ordonated
                End_If
            .end_If
        ..End_If

    ...Else_If B$esi+10 < Separators
        .If D$esi+5 = 'UNOR'
            If B$esi+9 = 'D'                    ; CMPPSUNORD
                mov B$imm32 3                   ; UnOrdonated.... oughhhh!!!!!!!!!
            End_If
        .End_If

    ...End_If

    On B$imm32 = 0FF, BadMnemonic

    mov op1 0011000010 | jmp XMMmemXMM

_________________________________________________________________

; Turn, for example,
; 'CMP_UNORD_SD'
;             ^edi  ('SD' in eax)
;          ^esi
; 'CMP_SD_UNORD':

TrySSE2:
    push esi, edi
        lea ebx D$esi+2
        While B$esi > Separators | inc esi | End_While | dec esi
        mov ax W$esi-1
        mov edi esi | sub esi 2
        std
            while esi <> ebx | movsb | End_While
        cld
        dec edi | stosw
    pop edi esi

    call Store8cars

    ifnot op4 'P', L4>
        ifnot op5 'S', L5>      ; CMPPS... with given Condition (ex: CMP_SS_LT)
            jmp XMMcomparePS
L5:     ifnot op5 'D', L1>      ; CMPPD...
            jmp XMMcomparePD
L4: ifnot op4 'S', L1>
        ifnot op5 'S', L5>      ; CMPSS...
            jmp XMMcompareSS
L5:     ifnot op5 'D', L1>      ; CMPSD...
            jmp XMMcompareSD

L1: BadMnemonic
ret


XMMcompareSS: ; CMPSS... encounted.
    ToOpcode 0011110011 | jmp XMMcomparePS ; (Prefix + same encodage).

XMMcompareSD: ; CMPSS... encounted.
    ToOpcode 0011110010 | jmp XMMcomparePS ; (Prefix + same encodage).

XMMcomparePD: ; CMPSS... encounted.
    ToOpcode 0001100110 | jmp XMMcomparePS ; (Prefix + same encodage).

 _________________________________________________________________________________________

; all math mnemonics begin by 'F'.

; as FPU instructions operands are either memory or ST regs, no control is done here
; for other operands than 'mem' (if not mem >>> reg supposed). All needed controls are
; done in destination 'reg's routines. See down there 'op_STreg:' for exemple.


Math:

    On op4 > Separators, jmp M4>>
      ifnot op2 'L', L2>
        ifnot op3 'D', L3>                      ; FLD – Load Real
          ifnot B$FirstGender mem, L4>
            ifnot B$FirstOperandWbit DoubleSize, L5>
              mov op1 00_11011_001, op2 0 | jmp op_ModRm
L5:         ifnot B$FirstOperandWbit QuadSize, L5>
              mov op1 00_11011_101, op2 0 | jmp op_ModRm
L5:         ifnot B$FirstOperandWbit TenSize, L5>
              mov op1 00_11011_011, op2 00101_000 | jmp op_ModRm
L5:         BadOperandSize
L4:       mov op1 00_1101_1001, op2 00_11000_000 | jmp op_STregP1
L3:     BadMnemonic
L2:   ifnot op2 'S', L2>
        ifnot op3 'T', L3>                        ; FST – Store Real
          ifnot B$FirstGender mem, L4>
            ifnot B$FirstOperandWbit DoubleSize, L5>
              mov op1 00_11011_001, op2 00010_000 | jmp op_ModRm
L5:         ifnot B$FirstOperandWbit QuadSize, L5>
              mov op1 00_11011_101, op2 00010_000 | jmp op_ModRm
L5:         BadOperandSize
L4:       mov op1 00_11011_101, op2 00_11010_000 | jmp op_STregP1
L3:     ;BadMnemonic
L2:   BadMnemonic
 ____________________________________

M4: On op5 > Separators, jmp M5>>
      ifnot op2 'A', L2>
        ifnot op3 'B', L3>
          ifnot op4 'S', L4>                    ; FABS – Absolute Value
            mov op1 00_1101_1001, op2 00_1110_0001 | jmp op_op
L4:       BadMnemonic
L3:     ifnot op3 'D', L3>
          ifnot op4 'D', L4>                    ; FADD – Add
            ifnot B$FirstGender mem, L5>
              ifnot B$FirstOperandWbit DoubleSize, L6>
                mov op1 00_11011_000, op2 0 | jmp op_ModRm
L6:           ifnot B$FirstOperandWbit QuadSize, L6>
                mov op1 00_11011_100, op2 0 | jmp op_ModRm
L6:           BadOperandSize
L5:         mov op1 0011011_000, op2 00_11_000_000 | jmp d_STreg
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'B', L2>
          ifnot op3 'L', L3>
            ifnot op4 'D', L4>                     ; FBLD – Load Binary Coded Decimal
              ifnot B$FirstGender mem, L5>
                  ifnot B$FirstOperandwbit TenSize, L6>
                  mov op1 0DF, op2 00_100_000 | jmp op_modRm
L6:             BadOperandSize
L5:           BadOperand
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'C', L2>>
          ifnot op3 'H', L3>
            ifnot op4 'S', L4>                     ; FCHS – Change Sign
              mov op1 00_11011_001, op2 00_1110_0000 | jmp op_op
L4:         BadMnemonic
L3:       ifnot op3 'O', L3>
            ifnot op4 'M', L4>                     ; FCOM – Compare Real
L5:           ifnot B$FirstGender mem, L5>
                ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1 00_11011_000, op2 00_010_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit QuadSize, L6>
                  mov op1 00_11011_100, op2 00_010_000 | jmp op_ModRM
L6:             BadOperandSize
L5:           mov op1 00_11011_000, op2 00_11_010_000 | jmp op_STreg
L4:         ifnot op4 'S', L4>                     ; FCOS – Cosine of ST(0)
              mov op1 00_11011_001, op2 00_1111_1111 | jmp op_op
L4:         BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'D', L2>
          ifnot op3 'I', L3>
            ifnot op4 'V', L4>
              ifnot B$FirstGender mem, L5>                 ; FDIV – Divide
                ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1 00_11011_000, op2 00_110_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit QuadSize, L6>
                  mov op1 00_11011_100, op2 00_110_000 | jmp op_ModRm
L6:             BadOperandSize
L5:           mov op1 00_11011_000, op2 00_1111_0_000 | jmp d_RSTreg
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'I', L2>>
          ifnot op3 'L', L3>
            ifnot op4 'D', L4>                ; FILD – Load Integer
              ifnot B$FirstGender mem, L5>
                ifnot B$FirstOperandWbit WordSize, L6>
                  mov op1 00_11011_111, op2 0 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1 00_11011_011, op2 0 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit QuadSize, L6>
                  mov op1 00_11011_111, op2 00_101_000 | jmp op_ModRm
L6:             BadOperandSize
L5:           BadOperand
L4:         BadMnemonic
L3:       ifnot op3 'S', L3>
            ifnot op4 'T', L4>
              ifnot B$FirstGender mem, L5>               ; FIST – Store Integer
                ifnot B$FirstOperandWbit WordSize, L6>
                  mov op1 00_11011_111, op2 00_010_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1  00_11011_011, op2 00_010_000 | jmp op_ModRm
L6:             BadOperandSize
L5:           BadOperand
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'L', L2>
          ifnot op3 'D', L3>
            ifnot op4 '1', L4>                             ; FLD1 – Load +1.0 into ST(0)
              mov op1 00_11011_001, op2 00_1110_1000 | jmp op_op
L4:         ifnot op4 'Z', L4>                             ; FLDZ – Load +0.0 into ST(0)
              mov op1 00_11011_001, op2 00_1110_1110 | jmp op_op
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'M', L2>
          ifnot op3 'U', L3>
            ifnot op4 'L', L4>
              ifnot B$FirstGender mem, L5>              ; FMUL – Multiply
                ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1 00_11011_000, op2 00_001_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit QuadSize, L6>
                  mov op1 00_11011_100, op2 00_001_000 | jmp op_ModRm
L6:             BadOperandSize
L5:           mov op1 00_11011_000, op2 00_1100_1_000 | jmp d_STreg
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'N', L2>
          ifnot op3 'O', L3>
            ifnot op4 'P', L4>                ; FNOP – No Operation
              mov op1 00_11011_001, op2 00_1101_0000 | jmp op_op
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'S', L2>>
          ifnot op3 'I', L3>
            ifnot op4 'N', L4>                 ; FSIN – Sine
              mov op1 00_11011_001, op2 00_1111_1110 | jmp op_op
L4:         BadMnemonic
L3:       ifnot op3 'T', L3>>
            ifnot op4 'P', L4>                 ; FSTP – Store Real and Pop
              ifnot B$FirstGender mem, L5>
                ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1 00_11011_001, op2 00_011_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit QuadSize, L6>
                  mov op1 00_11011_101, op2 00_011_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit TenSize, L6>
                  mov op1 00_11011_011, op2 00_111_000 | jmp op_ModRm
L6:             BadOperandSize
L5:           mov op1 00_11°11_101, op2 00_11__011_000 | jmp op_STregP1
L4:         BadMnemonic
L3:       ifnot op3 'U', L3>
            ifnot op4 'B', L4>                          ; FSUB – Subtract
              ifnot B$FirstGender mem, L5>
                ifnot B$FirstOperandWbit DoubleSize, L6>
                  mov op1 00_11011_000, op2 00_100_000 | jmp op_ModRm
L6:             ifnot B$FirstOperandWbit QuadSize, L6>
                  mov op1 00_11011_100, op2 00_100_000 | jmp op_ModRm
L6:             BadOperandSize
L5:           mov op1 00_11011_000, op2 00_1110_0_000 | jmp d_RSTreg ; d_STreg
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'T', L2>
          ifnot op3 'S', L3>
            ifnot op4 'T', L4>                         ; FTST – Test
              mov op1 00_11011_001, op2 00_1110_0100 | jmp op_op
L4:         ;BadMnemonic
L3:       BadMnemonic
L2:     ifnot op2 'X', L2>
          ifnot op3 'A', L3>
            ifnot op4 'M', L4>                          ; FXAM – Examine
              mov op1 00_11011_001, op2 00_1110_0101 | jmp op_op
L4:         BadMnemonic
L3:       ifnot op3 'C', L3>
            ifnot op4 'H', L4>                         ; FXCH – Exchange ST(0) and ST(i)
               mov op1 00_11011_001, op2 00_1100_1_000 | jmp op_STreg
L4:          ;BadMnemonic
L3:        ;BadMnemonic
L2:     BadMnemonic

 ____________________________________

M5: On op6 > Separators, jmp M6>>

      ifnot op2 '2', L2>
        ifnot op3 'X', L3>
          ifnot op4 'M', L4>
            ifnot op5 '1', L5>                     ;F2XM1 – Compute 2 ST(0) – 1
              mov op1 00_11011_001, op2 00_11110000 | jmp op_op
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'A', L2>
        ifnot op3 'D', L3>
          ifnot op4 'D', L4>
            ifnot op5 'P', L5>                    ; FADDP – Add and Pop ST(0) ¬ ST(0) + ST(i)
              On B$ParametersNumber <> 2, NoFpAssume
              On B$SecondReg <> 0, error D$FADDPreg0Ptr
              mov op1 00_11011_110, op2 00_11_000_000 | jmp op_STreg
L5:         ;BadMnemonic
L4:       ;BadMemonic
L3:     BadMnemonic
L2:   ifnot op2 'B', L2>
        ifnot op3 'S', L3>
          ifnot op4 'T', L4>
            ifnot op5 'P', L5>             ; FBSTP – Store Binary Coded Decimal and Pop
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandwbit TenSize, L7>
                mov op1 0DF, op2 00_110_000 | jmp op_modRm
L7:             BadOperandSize
L6:           BadOperand
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'C', L2>
        ifnot op3 'L', L3>
          ifnot op4 'E', L4>
            ifnot op5 'X', L5>              ; FCLEX – Clear Exceptions
              ToOpcode 09B, 0DB | LastOpCode 0E2
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'O', L3>
          ifnot op4 'M', L4>
            ifnot op5 'P', L5>              ; FCOMP – Compare Real and Pop
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_000, op2 00_011_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit QuadSize, L7>
                  mov op1 00_11011_100, op2 00_011_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           mov op1 00_11011_000, op2 00_11_011_000 | jmp op_STreg
L5:         ifnot op5 'I', L5>                 ; FCOMI – Compare Real and Set EFLAGS
              mov op1 00_11011_011, op2 00_11_110_000 | jmp op_STreg
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'D', L2>
        ifnot op3 'I', L3>
          ifnot op4 'V', L4>
            ifnot op5 'P', L5>                    ; FDIVP – Divide and Pop
              On B$ParametersNumber <> 2, NoFpAssume
              mov op1 00_11011_110, op2 00_1111_1_000 | jmp opSTreg
L5:         ifnot op5 'R', L5>                    ; FDIVR – Reverse Divide
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_000, op2 00_111_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit QuadSize, L7>
                  mov op1 00_11011_100, op2 00_111_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           mov op1 00_11011_000, op2 00_1111_1_000 | jmp d_RSTreg
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:      ifnot op2 'E', L2>
           ifnot op3 'M', L3>
             ifnot op4 'M', L4>
               ifnot op5 'S', L5>               ; FEMMS (not an FPU -3DNaow!-)
                   mov al 0F | stosb | mov al 0E | stosb | ret
L5:            ;BadMnemonic
L4:          ;BadMnemonic
L3:        BadMnemonic
L2:   ifnot op2 'F', L2>
        ifnot op3 'R', L3>
          ifnot op4 'E', L4>
            ifnot op5 'E', L5>                       ; FFREE – Free ST(i) Register
              mov op1 00_11011_101, op2 00_1100_0_000 | jmp op_STregP1
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'I', L2>>
        ifnot op3 'A', L3>
          ifnot op4 'D', L4>
            ifnot op5 'D', L5>                      ; FIADD – Add Integer
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  mov op1 00_11011_110, op2 0 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_010, op2 0 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'C', L3>
          ifnot op4 'O', L4>
            ifnot op5 'M', L5>               ; FICOM – Compare Integer
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  mov op1 00_11011_110, op2 00_010_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_010, op2 00_010_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'D', L3>
          ifnot op4 'I', L4>
            ifnot op5 'V', L5>                               ; FIDIV
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  mov op1 00_11011_110, op2 00_110_000 | jmp op_modRm
L7:             ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_010, op2 00_110_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'M', L3>
          ifnot op4 'U', L4>
            ifnot op5 'L', L5>                              ; FIMUL
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  mov op1 00_11011_110, op2 00_001_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_010, op2 00_001_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           Badoperand
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'N', L3>
          ifnot op4 'I', L4>
            ifnot op5 'T', L5>                      ; FINIT – Initialize Floating-Point Unit
              toOpcode 09B, 0DB | LastOpcode 0E3
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'S', L3>>
          ifnot op4 'T', L4>
            ifnot op5 'P', L5>                         ; FISTP – Store Integer and Pop
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  mov op1 00_11011_111, op2 00_011_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_011, op2 00_011_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit QuadSize, L7>
                  mov op1 00_11011_111, op2 00_111_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         BadMnemonic
L4:       ifnot op4 'U', L4>
            ifnot op5 'B', L5>                         ; FISUB
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  dec edi       ; No need 066 here.
                  mov op1 00_11011_110, op2 00_100_000 | jmp op_ModRm
L7:             ifnot B$FirstOperandWbit DoubleSize, L7>
                  mov op1 00_11011_010, op2 00_100_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'L', L2>
        ifnot op3 'D', L3>
          ifnot op4 'C', L4>
            ifnot op5 'W', L5>                      ; FLDCW – Load Control Word
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  dec edi                             ; scratch 066 prefix (no use)
                  mov op1 00_11011_001, op2 00_101_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         BadMnemonic
L4:       ifnot op4 'P', L4>
            ifnot op5 'I', L5>                         ; FLDPI – Load p into ST(0)
              mov op1 00_11011_001, op2 00_11101011 | jmp op_op
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'M', L2>
        ifnot op3 'U', L3>
          ifnot op4 'L', L4>
            ifnot op5 'P', L5>                     ; FMULP – Multiply
              On B$ParametersNumber <> 2, NoFpAssume
              mov op1 00_11011_110, op2 00_1100_1_000 | jmp op_STreg
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'P', L2>
        ifnot op3 'R', L3>
          ifnot op4 'E', L4>
            ifnot op5 'M', L5>                       ; FPREM – Partial Remainder
              mov op1 00_11011_001, op2 00_1111_1000 | jmp op_op
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'T', L3>
          ifnot op4 'A', L4>
            ifnot op5 'N', L5>                       ; FPTAN – Partial Tangent
              mov op1 00_11011_001, op2 00_1111_0010 | jmp op_op
L5:         BadMnemonic
L4:       BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'S', L2>>
        ifnot op3 'A', L3>
          ifnot op4 'V', L4>
            ifnot op5 'E', L5>                  ; FSAVE – Store FPU State 108 Bytes
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit xSize, L7>
                  ToOpcode 09B
                  mov op1 00_11011_101, op2 00_110_000 | jmp op_ModRm
L7:             error D$XmarkerPtr
L6:           BadOperand
L5:         BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'Q', L3>
          ifnot op4 'R', L4>
            ifnot op5 'T', L5>                  ; FSQRT – Square Root
              mov op1 00_11011_001, op2 00_1111_1010 | jmp op_op
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:  ifnot op3 'T', L3>>
          ifnot op4 'C', L4>
            ifnot op5 'W', L5>                  ; FSTCW – Store Control Word
              ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  dec edi                               ; scratch 066 prefix (no use here)
                  ToOpcode 09B
                  mov op1 00_11011_001, op2 00_111_000 | jmp op_ModRm
L7:             BadOperandSize
L6:           BadOperand
L5:         BadMnemonic
L4:     ifnot op4 'S', L4>
           ifnot op5 'W', L5>                  ; FSTSW – Store Status Word into AX or mem
             dec edi                               ; scratch 066 prefix (no use here)
             ToOpcode 09B
             ifnot B$FirstGender mem, L6>
                ifnot B$FirstOperandWbit WordSize, L7>
                  mov op1 0DD, op2 00_111_000 | jmp op_ModRm
L7:             BadOperandSize
L6:          ifnot B$FirstGender reg, L6>
               ifnot B$FirstOperandWbit WordSize, L7>
                 ifnot B$FirstReg regAX, L6>
                   mov op1 0DF, op2 00_11100000 | jmp op_op_P1
L7:            BadOperandSize
L6:          BadOperand
L5:        BadMnemonic
L4:      BadMnemonic
L3:    ifnot op3 'U', L3>
         ifnot op4 'B', L4>
           ifnot op5 'P', L5>                   ; FSUBP – Subtract and Pop
             On B$ParametersNumber <> 2, NoFpAssume
             On B$SecondReg <> RegSt0, error D$FSUBPreg0Ptr  ; FADDPreg0
             mov op1 00_11011_110, op2 00_1110_1_000 | jmp d_STreg  ;;;op_STreg
L5:        ifnot op5 'R', L5>                   ; FSUBR – Reverse Subtract
             ifnot B$FirstGender mem, L6>
               ifnot B$FirstOperandWbit DoubleSize, L7>
                 mov op1 00_11011_000, op2 00_101_000 | jmp op_ModRm
L7:            ifnot B$FirstOperandWbit QuadSize, L7>
                 mov op1 00_11011_100, op2 00_101_000 | jmp op_ModRm
L7:            BadOperandSize
L6:          mov op1 00_11011_000, op2 00_1110_1_000 | jmp d_RSTreg
L5:        ;BadMnemonic
L4:      ;BadMnemonic
L3:    BadMnemonic
L2:  ifnot op2 'U', L2>
       ifnot op3 'C', L3>
         ifnot op4 'O', L4>
           ifnot op5 'M', L5>              ; FUCOM – Unordered Compare Real
             mov op1 00_11011_101, op2 00_1110_0_000 | jmp op_STreg
L5:        ;BadMnemonic
L4:      ;BadMnemonic
L3:    BadMnemonic
L2:  ifnot op2 'Y', L2>
       ifnot op3 'L', L3>
         ifnot op4 '2', L4>
           ifnot op5 'X', L5>              ; FYL2X – ST(1) ´ log 2 (ST(0))
             mov op1 00_11011_001, op2 00_1111_0001 | jmp op_op
L5:        ;BadMnemonic
L4:      ;BadMnemonic
L3:    BadMnemonic
L2:  ifnot op2 'W', L2>
       ifnot op3 'A', L3>
         ifnot op4 'I', L4>
           ifnot op5 'T', L5>                   ; FWAIT – Wait until FPU Ready
             mov op1 00_1001_1011 | jmp op
L5:        ;BadMnemonic
L4:      ;BadMnemonic
L3:    ;BadMnemonic
L2:  BadMnemonic

 ____________________________________

M6: On op7 > Separators, jmp M7>>

      ifnot op2 'C', L2>
        ifnot op3 'O', L3>
          ifnot op4 'M', L4>
            ifnot op5 'P', L5>
              ifnot op6 'P', L6>          ; FCOMPP – Compare Real and Pop Twice
                mov op1 00_11011_110, op2 00_11_011_001 | jmp op_op
L6:           BadMnemonic
L5:         ifnot op5 'I', L5>
              ifnot op6 'P', L6>          ; FCOMIP – Compare Real, Set EFLAGS, and Pop
                mov op1 00_11011_111, op2 00_11_110_000 | jmp op_STreg
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       BadMnemonic
 ________________________________

L3:     On op3 = 'M', jmp FCMOVcc        ; >>>>>>>>>> down there
 ________________________________

L2:   ifnot op2 'D', L2>
        ifnot op3 'I', L3>
          ifnot op4 'V', L4>
            ifnot op5 'R', L5>
              ifnot op6 'P', L6>              ; FDIVRP – Reverse Divide and Pop
                On B$ParametersNumber <> 2, NoFpAssume
                mov op1 00_11011_110, op2 00_1111_0_000 | jmp op_STreg
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'F', L2>
        ifnot op3 'R', L3>
          ifnot op4 'E', L4>
            ifnot op5 'E', L5>                       ; FFREEP   Free ST(i) Register and pop
              ifnot op6 'P', L6>
              mov op1 0DF, op2 0C0 | jmp op_STregP1
L6:
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'I', L2>>
        ifnot op3 'C', L3>
          ifnot op4 'O', L4>
            ifnot op5 'M', L5>
              ifnot op6 'P', L6>           ; FICOMP – Compare Integer and Pop
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit WordSize, L8>
                    mov op1 00_11011_110, op2 00_011_000 | jmp op_ModRm
L8:               ifnot B$FirstOperandWbit DoubleSize, L8>
                    mov op1 00_11011_010, op2 00_011_000 | jmp op_ModRm
L8:               BadOperandSize
L7:             BadOperand
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'D', L3>
          ifnot op4 'I', L4>
            ifnot op5 'V', L5>
              ifnot op6, 'R', L6>                           ; FIDIVR
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit WordSize, L8>
                    mov op1 00_11011_110, op2 00_111_000 | jmp op_ModRm
L8:               ifnot B$FirstOperandWbit DoubleSize, L8>
                    mov op1 00_11011_010, op2 00_111_000 | jmp op_ModRm
L8:               BadOperandSize
L7:             BadOperand
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       BadMnemonic
L3:     ifnot op3 'S', L3>>
          ifnot op4 'T', L4>
            ifnot op5 'T', L5>>
              ifnot op6 'P', L6>>       ; FISTTP
                ifnot B$FirstGender mem, L7>
                If B$FirstOperandWbit = WordSize
                    mov op1 0DF
                Else_If B$FirstOperandWbit = doubleSize
                    mov op1 0DB
                Else_If B$FirstOperandWbit = QuadSize
                    mov op1 0DD
                Else
                    BadOperand
                End_If
                mov op2 00_001_000 | jmp op_ModRm

L4:       ifnot op4 'U', L4>
            ifnot op5 'B', L5>
              ifnot op6 'R', L6>                       ; FISUBR
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit WordSize, L8>
                    dec edi         ; No need of 066 here.
                    mov op1 00_11011_110, op2 00_101_000 | jmp op_ModRm
L8:               ifnot B$FirstOperandWbit DoubleSize, L8>
                    mov op1 00_11011_010, op2 00_101_000 | jmp op_ModRm
L8:               BadOperandSize
L7:             BadOperand
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:      BadMnemonic
L2:    ifnot op2 'L', L2>>
         ifnot op3 'D', L3>>
           ifnot op4 'E', L4>
             ifnot op5 'N', L5>
               ifnot op6 'V', L6>                ; FLDENV – Load FPU Environment
                 ifnot B$FirstGender mem, L7>
                   ifnot B$FirstOperandWbit xSize, L8>
                     mov op1 00_11011_001, op2 00_100_000 | jmp op_ModRm  ;  m28 bytes
L8:                error D$XmarkerPtr
L7:              BadOperand
L6:           ;BadMnemonic
L5:         BadMnemonic
L4:       ifnot op4 'L', L4>
            ifnot op5 '2', L5>
              ifnot op6 'E', L6>           ; FLDL2E – Load log 2 (e) into ST(0)
                mov op1 00_11011_001, op2 00_1110_1010 | jmp op_op
L6:           ifnot op6 'T', L6>           ; FLDL2T – Load log 2 (10) into ST(0)
                mov op1 00_11011_001, op2 00_1110_1001 | jmp op_op
L6:           BadMnemonic
L5:         ifnot op5 'G', L5>
              ifnot op6 '2', L6>            ; FLDLG2 – Load log 10 (2) into ST(0)
                mov op1 00_11011_001, op2 00_1110_1100 | jmp op_op
L6:           BadMnemonic
L5:         ifnot op5 'N', L5>
              ifnot op6 '2', L6>           ; FLDLN2 – Load log = (2) into ST(0)
                mov op1 00_11011_001, op2 00_1110_1101 | jmp op_op
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'N', L2>>
        ifnot op3 'C', L3>
           ifnot op4 'L', L4>
             ifnot op5 'E', L5>
               ifnot op6 'X', L6>                      ; FNCLEX
                  ToOpcode 0DB | LastOpcode 0E2
L6:         ;BadMnemonic
L5:       ;BadMnemonic
L4:     BadMnemonic
L3:      ifnot op3 'I', L3>
           ifnot op4 'N', L4>
             ifnot op5 'I', L5>
               ifnot op6 'T', L6>                      ; FNINIT*
                  ToOpcode 0DB | LastOpcode 0E3
L6:         ;BadMnemonic
L5:       ;BadMnemonic
L4:     BadMnemonic
L3:     ifnot op3 'S', L3>>
          ifnot op4 'A', L4>
            ifnot op5 'V', L5>                  ; FNSAVE – Store FPU Environ. 108 Bytes
              ifnot op6 'E', L6>
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit xSize, L8>
                    mov op1 0D9, op2 00_111_000 | jmp op_ModRm
L8:               error D$XmarkerPtr
L7:           BadOperand
L6:         ;BadMnemonic
L5:       BadMnemonic
L4:       ifnot op4 'T', L4>>
            ifnot op5 'C', L5>                  ; FNSTCW  ; 'FSTCW'
              ifnot op6 'W', L6>
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit wordSize, L8>
                    dec edi                             ; scratch 066 prefix (no use here)
                    mov op1 00_11011_001, op2 00_111_000 | jmp op_ModRm
                    ;mov op1 00_11011_101, op2 00_110_000 | jmp op_ModRm
L8:               BadOperandSize
L7:           BadOperand
L6:         BadMnemonic
L5:         ifnot op5 'S', L5>                  ; FNSTSW
              ifnot op6 'W', L6>
                dec edi ; No need of 066 here.
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit wordSize, L8>
                    mov op1 0DD, op2 00_111_000 | jmp op_ModRm
L7:             ifnot B$FirstGender reg, L7>
                  ifnot B$FirstOperandWbit wordSize, L8>
                    ifnot B$FirstReg regAX, L7>
                      mov op1 0DF, op2 0E0 | jmp op_op_P1
L8:               BadOperandSize
L7:           BadOperand
L6:         ;BadMnemonic
L5:       ;BadMnemonic
L4:     ;BadMnemonic
L3:   BadMnemonic
L2:   ifnot op2 'P', L2>
        ifnot op3 'A', L3>
          ifnot op4 'T', L4>
            ifnot op5 'A', L5>
              ifnot op6 'N', L6>            ; FPATAN – Partial Arctangent
                mov op1 00_11011_001, op2 00_1111_0011 | jmp op_op
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       BadMnemonic
L3:     ifnot op3 'R', L3>
          ifnot op4 'E', L4>
            ifnot op5 'M', L5>
              ifnot op6 '1', L6>            ; FPREM1 – Partial Remainder (IEEE)
                mov op1 00_11011_001, op2 00_1111_0101 | jmp op_op
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'R', L2>
        ifnot op3 'S', L3>
          ifnot op4 'T', L4>
            ifnot op5 'O', L5>
              ifnot op6 'R', L6>            ; FRSTOR – Restore FPU State
                 ifnot B$FirstGender mem, L7>
                   ifnot B$FirstOperandWbit xSize, L8>
                     mov op1 00_11011_101, op2 00_100_000 | jmp op_ModRm ; 180 bytes
L8:                error D$XmarkerPtr
L7:              BadOperand
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'S', L2>>
        ifnot op3 'C', L3>
          ifnot op4 'A', L4>
            ifnot op5 'L', L5>
              ifnot op6 'E', L6>           ; FSCALE – Scale
                mov op1 0D9, op2 0FD | jmp op_op
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       BadMnemonic
L3:     ifnot op3 'T', L3>
          ifnot op4 'E', L4>
            ifnot op5 'N', L5>
              ifnot op6 'V', L6>            ; FSTENV – Store FPU Environment
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandWbit xSize, L8>
                    mov op1 00_11011_001, op2 00_110_000 | jmp op_ModRm  ; 28 bytes
L8:               error D$XmarkerPtr
L7:             BadOperand
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       BadMnemonic
L3:     ifnot op3 'U', L3>
          ifnot op4 'B', L4>
            ifnot op5 'R', L5>
              ifnot op6 'P', L6>            ; FSUBRP – Reverse Subtract and Pop
                On B$ParametersNumber <> 2, NoFpAssume
                mov op1 00_11011_110, op2 0E0 ;00_1110_0_00 |
                jmp op_STreg
L6:           ;BadMnemonic
L5:         ;Badmnemonic
L4:       ;BadMnemonic
L3:     BadMnemonic
L2:   ifnot op2 'U', L2>
        ifnot op3 'C', L3>
          ifnot op4 'O', L4>
            ifnot op5 'M', L5>
              ifnot op6 'P', L6>       ; FUCOMP – Unordered Compare Real and Pop
                mov op1 00_11011_101, op2 00_1110_1_000 | jmp op_STreg
L6:           ifnot op6 'I', L6>       ; FUCOMI – Unorderd Compare Real and Set EFLAGS
                mov op1 00_11011_011, op2 00_11_101_000 | jmp op_STreg
L6:          ;BadMnemonic
L5:        ;Badmnemonic
L4:      ;BadMnemonic
L3:   BadMnemonic
L2:   ifnot op2 'X', L2>
        ifnot op3 'S', L3>
          ifnot op4 'A', L4>
            ifnot op5 'V', L5>
              ifnot op6 'E', L6>                      ; FXSAVE
                ifnot B$FirstGender mem, L7>
                  ifnot B$FirstOperandwbit XSize, L7>
                      mov al 0F | stosb | mov op1 0AE, op2 0 | jmp op_ModRm
L7:               error D$XmarkerPtr
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       ;BadMnemonic
L3:     ;BadMnemonic
L2:   BadMnemonic


 ____________________________________

M7: On op8 > Separators, jmp M8>>

      On op2 = 'C', jmp FCMOVcc       ;  >>>>>>>>>>>>>  Down there

      ifnot op2 'D', L2>
        ifnot op3 'E', L7>
          ifnot op4 'C', L7>
            ifnot op5 'S', L7>
              ifnot op6 'T', L7>
                ifnot op7 'P', L7>         ; FDECSTP – Decrement Stack-Top Pointer
                  mov op1 00_11011_001, op2 00_1111_0110 | jmp op_op
L7:     BadMnemonic
L2:   ifnot op2 'I', L2>
        ifnot op3 'N', L7>
          ifnot op4 'C', L7>
            ifnot op5 'S', L7>
              ifnot op6 'T', L7>
                ifnot op7 'P', L7>         ; FINCSTP – Increment Stack Pointer
                  mov op1 00_11011_001, op2 00_1111_0111 | jmp op_op
L7:     BadMnemonic
L2:   ifnot op2 'N', L2>
        ifnot op3 'S', L7>
          ifnot op4 'T', L7>
            ifnot op5 'E', L7>
              ifnot op6 'N', L7>
                ifnot op7 'V', L7>         ; FNSTENV
                  ifnot B$FirstOperandwbit XSize, L8>
                    mov op1 0D9, op2 00_110_000 | jmp op_ModRm
L8:               error D$XmarkerPtr
L7:     BadMnemonic
L2:   ifnot op2 'R', L2>
        ifnot op3 'N', L7>
          ifnot op4 'D', L7>
            ifnot op5 'I', L7>
              ifnot op6 'N', L7>
                ifnot op7 'T', L7>         ; FRNDINT – Round to Integer
                  mov op1 00_11011_001, op2 00_1111_1100 | jmp op_op
L7:     BadMnemonic
L2:   ifnot op2 'S', L2>
        ifnot op3 'I', L7>
          ifnot op4 'N', L7>
            ifnot op5 'C', L7>
              ifnot op6 'O', L7>
                ifnot op7 'S', L7>         ; FSINCOS – Sine and Cosine
                  mov op1 00_11011_001, op2 00_1111_1011 | jmp op_op
L7:     BadMnemonic
L2:   ifnot op2 'U', L2>
        ifnot op3 'C', L3>
          ifnot op4 'O', L4>
            ifnot op5 'M', L5>
              ifnot op6 'P', L6>
                ifnot op7 'P', L7>      ; FUCOMPP – Unordered Compare Real and Pop Twice
                  mov op1 00_11011_010, op2 00_1110_1001 | jmp op_op
L7:             BadMnemonic
L6:           ifnot op6 'I', L6>
                ifnot op7 'P', L7>  ; FUCOMIP – Unorderd Compare Real, Set EFLAGS, and Pop
                  mov op1 00_11011_111, op2 00_11_101_000 | jmp op_STreg
L7:             ;BadMnemnoic
L6:           ;BadMnemonic
L5:         ;BadMnemonic
L4:       ;BadMnemnonic
L3:     BadMnemonic
L2:   ifnot op2 'X', L2>
        ifnot op3 'R', L3>
          ifnot op4 'S', L7>
            ifnot op5 'T', L7>
              ifnot op6 'O', L7>
                ifnot op7 'R', L7>       ; FXRSTOR
                  ifnot B$FirstGender mem, L8>
                     ifnot B$FirstOperandwbit XSize, L8>
                         ToOpcode 0F | mov op1 0AE, op2 00_001_000 | jmp op_ModRm
L8:                  error D$XmarkerPtr
L7:     BadMnemonic
L3: ifnot op3 'T', L7>
          ifnot op4 'R', L7>
            ifnot op5 'A', L7>
              ifnot op6 'C', L7>
                ifnot op7 'T', L7>       ; FXTRACT – Extract Exponent and Significand
                  mov op1 00_11011_001, op2 00_1111_0100 | jmp op_op
L7:     BadMnemonic
L2:   ifnot op2 'Y', L2>
        ifnot op3 'L', L7>
          ifnot op4 '2', L7>
            ifnot op5 'X', L7>
              ifnot op6 'P', L7>
                ifnot op7 '1', L7>     ; FYL2XP1 – ST(1) ´ log 2 (ST(0) + 1.0)
                  mov op1 00_11011_001, op2 00_1111_1001 | jmp op_op
L7:     ;BadMnemonic
L2:   ;BadMnemonic

M8: On D$esi = 'FCMO', jmp FCMOVcc
    BadMnemonic

 ______________________________________________________________________________________

FCMOVcc:                       ; Conditional Move on EFLAG
      ifnot op3 'M', L8>>
        ifnot op4 'O', L8>>
          ifnot op5 'V', L8>>
            On op7 > Separators, jmp L2>
              ifnot op6 'E', L1>
                mov op1 00_11011_010, op2 00_11_001_000 | jmp op_STreg  ; e
L1:           ifnot op6 'B', L1>
                mov op1 00_11011_010, op2 00_11_000_000 | jmp op_STreg  ; b
L1:           ifnot op6 'U', L1>
                mov op1 00_11011_010, op2 00_11_011_000 | jmp op_STreg  ; u
L1:           ifnot op6 'A', L7>>
E0:             mov op1 00_11011_011, op2 00_11_010_000 | jmp op_STreg  ; a nbe
L2:         On op8 > Separators, jmp L3>
              ifnot op6 'N', L4>
                ifnot op7 'E', L5>
Z0:               mov op1 00_11011_011, op2 00_11_001_000 | jmp op_STreg ; <> nz
L5:             ifnot op7 'A', L5>
B0:               mov op1 00_11011_010, op2 00_11_010_000 | jmp op_STreg ; na be
L5:             ifnot op7 'B', L5>
A0:               mov op1 00_11011_011, op2 00_11_000_000 | jmp op_STreg ; nb ae
L5:             ifnot op7 'U', L5>
                  mov op1 00_11011_011, op2 00_11_011_000 | jmp op_STreg ; nu
L5:             ifnot op7 'Z', L7>
                  jmp Z0<
L4:           ifnot op7 'E', L7>
                ifnot op6 'B', L5>
                  jmp B0<
L5:             ifnot op6 'A', L7>
                  jmp A0<
L3:         On B$esi+9 > Separators, jmp L7>
              ifnot op6 'N', L7>
                ifnot op7 'B', L7>
                  ifnot op8 'E', L7>
                    jmp E0<<
L7:    error D$BadFPUcondPtr
L8:  BadMnemonic

 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 End codage control and write routines. Jump from 'Encode' > RET > return to 'EncodeLines'

 'imm' and 'dis' are written after by 'EncodeLine'

 all the '......_P2' routines are for bit instruction that are two parameters when
 written in mnemonics, but only one when written in opCode. exemple:
                             shl eax, 1
 the immediate 1 doesn't appear in code flow but is given by the code instruction itself.
;;
 ___________________________________________________________________________________________

; macros and subs used down there:

[ToOpcode | mov B$edi #1 | inc edi | #+1]

[LastOpcode | mov B$edi #1 | inc edi | ret]       ; this RET is the true Encode job return

Params:
  On B$wBitDisagree = &TRUE,  error D$MixTypePtr
ParamsAny:
  On B$ParametersNumber < cl,  Error D$NotEnoughPtr
  On B$ParametersNumber > cl,  Error D$TooMuchPtr
ret


[Parms | mov cl, #1 | call Params]

[ParmsAny | mov cl, #1 | call ParamsAny]

Xparams:                                         ; used by for MOVSX / MOVZX
    call ParamsAny

    cmp B$FirstOperandWbit WordSize | jne L8>
      ToOpcode 066
      On B$SecondOperandWbit = WordSize,  error D$MixTypePtr

L8: On B$FirstOperandWbit = ByteSize,  error D$MixTypePtr
    On B$SecondOperandWbit = DoubleSize,  error D$MixTypePtr
    mov dl B$SecondOperandWbit | and dl 1 | mov B$wBit dl
    ret

[Xparms | mov cl, #1 | call Xparams]

im8Size:
    On B$ImmInside = &False, BadOperand
    On D$imm32 > 0FF,  error D$OverBytePtr
    mov B$TrueSize ByteSize     ; for 'StoreImm' in case of bigger sized first parameter
  ret

[imm8Size | call im8Size]

im16Size:
    On D$imm32 >= 010000,  error D$OverWordPtr
    mov B$TrueSize WordSize
  ret

[imm16Size | call im16Size]

ds8Size:
    On D$dis32 >= 0100,  error D$OverBytePtr
    cmp B$LocalSize DownLong | je L1>
    cmp B$LocalSize UpLong | jne L2>
L1:   error D$LongDisPtr
L2: mov B$LongRelative &FALSE,  D$Relative RelativeFlag
    ret

[dis8Size | call ds8Size]

GPrg1:                                    ; test a reg to be a general purpose
  On B$FirstReg > 00_0111,  error D$GPregisterPtr
  On B$FirstRegGender > reg,  error D$GPregisterPtr
ret

GPrg2:
  On B$SecondReg > 00_0111,  error D$GPregisterPtr
  On B$SecondRegGender > reg,  error D$GPregisterPtr
ret

GPrg1_2:
  call GPrg1
  call GPrg2
ret

[GPreg1 | call GPrg1]     [GPreg2 | call GPrg2]     [GPreg1_2 | call GPrg1_2]

[LastSbitEdi: 0]

; The OpCodes concerned by Sign Extensions are: adc, add, and, cmp, imul, or, push, sbb,
; sub, xor.

sIm:
    cmp B$immInside &TRUE | jne L9>
    push eax
        mov eax D$imm32
        If eax <= 07F
L1:         mov B$sBit 0010
        Else
            and eax 0_FFFF_FF80
            On eax = 0_FFFF_FF80, mov B$sBit 0010
        End_If

        On B$sBit = 0010, call CkeckPossibleImmLabel
    pop eax
    If B$sBit = 0010
        mov B$TrueSize ByteSize | and D$imm32 0FF | mov B$immSign &FALSE
      ; &FALSE, because we will not check the dWord High Bit in StoreImm
    End_If
L9: ret


; This is a simplified version of 'First / SecondParameterLabel'. In case of something
; remaining in the line, we abort the sBit encodage.

CkeckPossibleImmLabel:
    push esi
        mov esi D$LineStart
L0:     lodsb | cmp al Space | jne L0<
L0:     lodsb
            cmp al EOI | je L9>>
            cmp al meEOI | je L9>>   ; and not "cmp al EOI | JBE L9>>" because zeros inside...
            cmp al 'A' | jb L0<
            cmp al, 'Z' | ja L0<
            cmp al 'E' | jne L1>
            push eax
                call IsItAnEreg              ; usefull only in case of mem adressing
            pop eax
            IfEregNotFound L1>
            add esi 2 | jmp L0<
L1:     mov B$sBit 0
L9: pop esi
ret


[signImm | call sIm]


STrg12: On B$SecondRegGender <> STreg, error D$STwishedPtr
STrg1: On B$FirstRegGender <> STreg, error D$STwishedPtr  | ret

[STregs1_2 | Call STrg12]

[StReg1 | call STrg1]
 ________________________________________________________________________________________
 ________________________________________________________________________________________
;;
 FPU job:

 each time it is possible, syntax is free. Good and equivalent, for exemple:

                FDIV = FDIV ST1 = FDIV ST0 ST1
                       FDIV ST2 = FDIV ST0 ST2
;;

op_STregP1:
    On B$ParametersNumber > 1, error D$TooMuchPtr
    ifnot B$ParametersNumber 0,  L1>
      ToOpcode op1 | LastOpcode op2
op_STreg:
    ifnot B$ParametersNumber 0,  L1>
      ToOpcode op1 | inc op2 | LastOpcode op2
L1: ifnot B$ParametersNumber 1,  L2>
      STreg1 | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2
L2: ifnot B$ParametersNumber 2,  L3>
 ;    ;;; On B$FirstReg <> RegST0, error ST0wished
 ;     On B$SecondReg <> RegST0, error ST0wished
 ;   ;;STregs1_2 | ToOpcode op1 | or op2 B$SecondReg | LastOpcode op2
 ;     STregs1_2 | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

 ; Last choice > more flexibility:

        STregs1_2 | ToOpcode op1
        If B$FirstReg = RegST0
            or op2 B$SecondReg
        Else_If B$SecondReg = RegST0
            or op2 B$FirstReg
        Else
            error D$ST0wishedPtr
        End_If

        LastOpcode op2

L3: Error D$TooMuchPtr

d_STreg:
    ifnot B$ParametersNumber 0,  L1>
      ToOpcode op1 | inc op2 | LastOpcode op2
L1: ifnot B$ParametersNumber 1,  L2>
      STreg1 | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2
L2: ifnot B$ParametersNumber 2, L8>
      STregs1_2 | cmp B$FirstReg RegST0 | jne L3>
        or op2 B$SecondReg | ToOpcode op1 | LastOpcode op2
L3:   On B$SecondReg <> RegST0, error D$ST0wishedPtr
        or op1 00_100 | ToOpcode op1                        ; 00_100 = direction bit
        or op2 B$FirstReg | LastOpcode op2
L8: Error D$TooMuchPtr

; for FDIV / FDIVR // FSUB / FSUBR, R bit (second byte: 0000x000) XORed for reversed forms:
; R bit set by caller (jumper) to 0 for FDIV / FSUB,  to 1 for FDIVR / FSUBR

d_RSTreg:
    ifnot B$ParametersNumber 0,  L1>
      ToOpcode op1 | inc op2 | LastOpcode op2
L1: ifnot B$ParametersNumber 1,  L2>
      STreg1 | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2
L2: ifnot B$ParametersNumber 2,  L8<
    STregs1_2 | cmp B$FirstReg RegST0 | jne L1>
L0:   or op2 B$SecondReg | ToOpcode op1 | LastOpcode op2
L1: On B$SecondReg <> RegST0, error D$ST0wishedPtr
      or op1 00_0100 | ToOpcode op1                          ; 00_0100 = direction bit
      xor op2 00_1000 | or op2 B$FirstReg | LastOpcode op2   ; 00_1000 = Reverse bit

 ________________________________________________________________________________________
 ________________________________________________________________________________________
;;
 MMX job:

 As MMX coding is much more regular than it is for interger, less work is done in the
 'text routing' level and more work is done here. op1 is in fact the second opcode
 (00_1111 is always store at first here). All cases are covered by 4 routines, mm4,
 mm2, gg2, gg3.

 'gg_value' routine inputs granularity (Byte, Word, Dword or Qword) as a letter
 (B, W, D or Q) set in CL reg ( validity test done by mnemonics parser). Instead of
 4 different cases holding, we add 2 to ascii value and keep last two bits. Trick:

 cl = 'B' = 00_01000010 + 0010 = 00_01000100 >>> and 0011 >>> 00_00  >>>  gg_Byte
 cl = 'W' = 00_01010111 + 0010 = 00_01011001 >>> and 0011 >>> 00_01  >>>  gg_Word
 cl = 'D' = 00_01000100 + 0010 = 00_01000110 >>> and 0011 >>> 00_10  >>>  gg_dWord
 cl = 'Q' = 00_01010011 + 0010 = 00_01010011 >>> and 0011 >>> 00_11  >>>  gg_qWord
;;

[gg_value | add cl 2 | and cl 00_11 | or op1 cl]

;;
 Exemple for mm4:      MOVD - Move doubleword N N Y N

   reg to mmreg    00001111:01101110: 11 mmxreg1 reg2   ; movd MM0 eax
   reg from mmxreg 00001111:01111110: 11 mmxreg2 reg1   ; movd eax MM0
   mem to mmxreg   00001111:01101110: mod mmxreg1 r/m   ; movd MM0 D$val
   mem from mmxreg 00001111:01111110: mod mmxreg2 r/m   ; movd D$val MM0

 2 mnemonics only for mm4. In both cases op1 (true second opcode) is the same for the
 4 cases, but with 00_10000 add in cases 2 and 4.
;;

mmFour: ParmsAny 2 | ToOpcode 00_1111
     cmp B$Operands RegToReg | jne L1>
      cmp B$FirstRegGender mmReg | jne L2>
        ToOpcode op1                                                 ; case 1
        GPreg2 | mov op1 B$FirstReg | shl op1 3 | or op1 B$SecondReg
        or op1 00_11000000 | LastOpcode op1
L2:   cmp B$SecondRegGender mmReg | jne L2>
        or op1 00_10000 | toOpcode op1                               ; case 2
        GPreg1 | mov op1 B$SecondReg | shl op1 3 | or op1 B$FirstReg
        or op1 00_11000000 | LastOpcode op1
L2:   BadOperand
L1: cmp B$Operands MemToReg | jne L1>
      ToOpcode op1 | mov op1 B$ModBits | or op1 B$RmBits             ; case 3
      On B$FirstRegGender <> mmReg, error D$OperandsTypesPtr
      mov op2 B$FirstReg | shl op2 3 | or op1 op2 | LastOpcode op1
L1: cmp B$Operands RegToMem | jne L1>
      or op1 00_10000 | ToOpcode op1                                 ; case 4
      mov op1 B$ModBits | or op1 B$RmBits
      On B$SecondRegGender <> mmReg, error D$OperandsTypesPtr
      mov op2 B$SecondReg | shl op2 3 | or op1 op2 | LastOpcode op1
L1: BadOperand



; Exemple for gg2:       PADD - Add with wrap-around Y Y Y N
;   mmxreg2 to mmxreg1 0000 1111: 111111gg: 11 mmxreg1 mmxreg2    ; padd MM0 MM1
;   memory to mmxreg   0000 1111: 111111gg: mod mmxreg r/m        ; padd MM0 B$Val

gg2: gg_Value    ; >>> mm2:

    .If B$FirstRegGender = XmmReg
        ToOpcode 066 | mov B$FirstRegGender mmReg
        If B$SecondRegGender = XmmReg
            mov B$SecondRegGender mmReg
        End_If
    .End_If

; Exemple for mm2:       PAND - Bitwise And N N N Y
;   mmxreg2 to mmxreg1 0000 1111:11011011: 11 mmxreg1 mmxreg2   ; pand MM0 MM1
;   memory to mmxreg   0000 1111:11011011: mod mmxreg r/m       ; pand MM0 Q$Val

mmTwo:

ParmsAny 2 | ToOpcode 00_1111, op1
L0: cmp B$Operands RegToReg | jne L1>
      cmp B$FirstRegGender  mmReg | jne L8>
      cmp B$SecondRegGender mmReg | jne L8>
        mov op1 B$FirstReg | shl op1 3 | or op1 B$SecondReg
        or op1 00_11000000 | LastOpcode op1
L1: mov op1 B$ModBits | or op1 B$RmBits
    cmp B$Operands MemToReg | jne L8>
      cmp B$FirstRegGender mmReg | jne L8>
      mov op2 B$FirstReg | shl op2 3 | or op1 op2 | LastOpcode op1
L8: BadOperand

mmTwoImm8:
    ParmsAny 3 | ToOpcode 00_1111, op1
    On B$immInside = &FALSE, error D$EndingImmPtr
    On D$imm32 > 0FF, BadOperandSize
        mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
        jmp L0<<

mmOne:
    ParmsAny 2
    cmp B$Operands RegToReg | jne L9>
        cmp B$FirstRegGender MMreg | jne L9>
            cmp B$SecondRegGender MMreg | jne L9>
                ToOpcode 0F, op1
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                LastOpcode al

L9: BadOperand


; Used only with movntq and one of movq:

MMregToMem:
    ParmsAny 2
    cmp B$Operands RegToMem | jne L9>
        cmp B$SecondRegGender mmReg | jne L9>
            cmp B$FirstOperandWbit Xsize | je L7>
            cmp B$FirstOperandWbit QuadSize | jne L8>
L7:             ToOpcode 0F, op1
                mov al B$SecondReg | shl al 3 | or al B$ModBits | or al B$RmBits
                LastOpcode al
L8: BadOperandSize
L9: BadOperand

;;
 Exemple for gg3:       PSLL  - Packed shift left logical N Y Y Y
   mmxreg1 by mmxreg2  00001111:111100gg: 11 mmxreg1 mmxreg2        ; psll MM0 MM1
   mmxreg by memory    00001111:111100gg: mod mmxreg r/m            ; psll MM0 D$val
   mmxreg by immediate 00001111:011100gg: 11 110 mmxreg: imm8 data  ; psll MM1 28
                                             ^^^ /n is here (op2)
 3 mnemonic for gg3. For all, case 1 and 2 have the same op1 code.
 For all, case 3 is 011100gg. (070 + gg)
;;

gg3:
    .If B$FirstRegGender = XmmReg
        ToOpcode 066 | mov B$FirstRegGender mmReg
        If B$SecondRegGender = XmmReg
            mov B$SecondRegGender mmReg
        End_If
    .End_If

    push ecx
        ParmsAny 2 | ToOpcode 00_1111
    pop ecx
    cmp B$Operands RegToReg | jne L1>
      cmp B$FirstRegGender  mmReg | jne L8>>
      cmp B$SecondRegGender mmReg | jne L8>>
        gg_Value | ToOpcode op1                                 ; case 1
        mov op1 B$FirstReg | shl op1 3 | or op1 B$SecondReg
        or op1 00_11000000 | LastOpcode op1
L1: cmp B$Operands MemToReg | jne L1>
      cmp B$FirstRegGender mmReg | jne L8>
        gg_Value | ToOpcode op1                                 ; case 2
        mov op1 B$ModBits | or op1 B$RmBits
        mov op2 B$FirstReg | shl op2 3 | or op1 op2 | LastOpcode op1
L1: cmp B$Operands immToReg | jne L8>
      cmp B$FirstRegGender mmReg | jne L8>
      ; xor op1 00_010_000_000 (???!!!...)
       mov op1 070 | gg_Value | ToOpcode op1         ; case 3
        imm8Size | or op2 B$FirstReg | LastOpcode op2
L8: BadOperand
____________________________________________________________________________________________
____________________________________________________________________________________________

; XMM job:

; example: ADDPS XMM1 XMM2
;          ADDPS XMM5 X$MyPackedData   ; (X$ or O$) (Sib allowed)

XMMXMM:
    ParmsAny 2
    ToOpcode 0F, op1
    cmp B$Operands RegToReg | jne L8>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al
L8: BadOperand


XmmMemXmmImm7:
ParmsAny 3
    On B$immInside = &FALSE, error D$EndingImmPtr
    On D$imm32 > 00111, BadOperandSize
        mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
        jmp L0>>


XmmMemXmmImm8:
ParmsAny 3
    On B$immInside = &FALSE, error D$EndingImmPtr
    On D$imm32 > 0FF, BadOperandSize
        mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
        jmp L0>

XmmMemXmmImm3:
ParmsAny 3
    On B$immInside = &FALSE, error D$EndingImmPtr
    On D$imm32 > 0011, BadOperandSize
        mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
        jmp L0>

XmmMemXmmImmFF:
ParmsAny 3
    On B$immInside = &FALSE, error D$EndingImmPtr
        mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
        jmp L0>


;Xmmgg2:  gg_Value >>> now gg2 extended

XMMmemXMM:
    ParmsAny 2
L0: ToOpcode 0F, op1
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L8>
        cmp B$SecondOperandwbit xSize | je L1>
        cmp B$SecondOperandwbit OctoSize | jne L7>
L1:     cmp B$FirstRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


MMXmemXMM:
    ParmsAny 2
L0: ToOpcode 0F, op1
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  mmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L8>
        cmp B$SecondOperandwbit xSize | je L1>
        cmp B$SecondOperandwbit OctoSize | jne L7>
L1:     cmp B$FirstRegGender mmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


mmXmm:
    ParmsAny 2
L0: ToOpcode 0F, op1
    cmp B$Operands RegToReg | jne L8>
        cmp B$FirstRegGender  mmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


Xmmmm:
    ParmsAny 2
L0: ToOpcode 0F, op1
    cmp B$Operands RegToReg | jne L8>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender mmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


; Example CVTSI2SS xmm1 eax
;         CVTSI2SS xmm D$memory

DwordToLowXMM:
    ParmsAny 2
    ToOpcode 00_11110011, 001111, op1
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender Reg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L8>
        cmp B$SecondOperandwbit doubleSize | jne L7>
        cmp B$FirstRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


DwordToLowXMMWithF2:
    ParmsAny 2
    ToOpcode 00_11110010, 001111, op1
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender Reg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L8>
        cmp B$SecondOperandwbit doubleSize | jne L7>
        cmp B$FirstRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


; Example: CVTTPS2SI ebx xmm3
;          CVTTPS2SI edx O$memory

LowXMMtoDword:
    ParmsAny 2
    ToOpcode 00_11110011, 001111, op1
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  Reg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L8>
        cmp B$SecondOperandwbit OctoSize | je L1>
        cmp B$SecondOperandwbit XSize | jne L7>
L1:     cmp B$FirstRegGender Reg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


LowXMMtoDwordWithF2:
    ParmsAny 2
    ToOpcode 00_11110010, 001111, op1
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  Reg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L8>
        cmp B$SecondOperandwbit OctoSize | je L1>
        cmp B$SecondOperandwbit XSize | jne L7>
L1:     cmp B$FirstRegGender Reg | jne L8>>
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


XMMmemXMMmem:
    ParmsAny 2
    ToOpcode 0F
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L2>
        cmp B$SecondOperandwbit xSize | je L1>
        cmp B$SecondOperandwbit OctoSize | jne L7>
L1:     cmp B$FirstRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L2: cmp B$Operands RegToMem | jne L8>
        cmp B$FirstOperandwbit xSize | je L1>
        cmp B$FirstOperandwbit OctoSize | jne L7>
L1:     cmp B$SecondRegGender XmmReg | jne L8>>
            or op1 1 | ToOpcode op1
            mov al B$SecondReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


; Special for MOVSS, that deals with one F$ mem:

XMMmem32XMMmem32:
    ParmsAny 2
    ToOpcode 0F
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L2>
        cmp B$SecondOperandwbit doubleSize | je L1>
        cmp B$SecondOperandwbit xSize | je L1>
        cmp B$SecondOperandwbit OctoSize | jne L7>
L1:     cmp B$FirstRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L2: cmp B$Operands RegToMem | jne L8>
        cmp B$FirstOperandwbit doubleSize | je L1>
        cmp B$FirstOperandwbit xSize | je L1>
        cmp B$FirstOperandwbit OctoSize | jne L7>
L1:     cmp B$SecondRegGender XmmReg | jne L8>>
            or op1 1 | ToOpcode op1
            mov al B$SecondReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


; Same as uper but with "xor op1 0010000" (instead of "or 1").
; For MOVQ/MOVDQA/MOVDQU only.

MovqXMMmemXMMmem:
    ParmsAny 2
    ToOpcode 0F
    cmp B$Operands RegToReg | jne L2>
        cmp B$FirstRegGender  XmmReg | jne L8>>
        cmp B$SecondRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al 00_11_000_000 | or al B$SecondReg
            LastOpcode al

L2: cmp B$Operands MemToReg | jne L2>
        cmp B$SecondOperandwbit xSize | je L1>
        cmp B$SecondOperandwbit QuadSize | jne L7>
L1:     cmp B$FirstRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L2: cmp B$Operands RegToMem | jne L8>
        cmp B$FirstOperandwbit xSize | je L1>
        cmp B$FirstOperandwbit QuadSize | jne L7>
L1:     cmp B$SecondRegGender XmmReg | jne L8>>
            xor op1 0010000 | ToOpcode op1
            mov al B$SecondReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


XMMtoFromMem:
    ParmsAny 2
    ToOpcode 0F

    cmp B$Operands MemToReg | jne L2>
        cmp B$SecondOperandwbit xSize | je L1>
        cmp B$SecondOperandwbit OctoSize | jne L7>
L1:     cmp B$FirstRegGender XmmReg | jne L8>>
            ToOpcode op1
            mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L2: cmp B$Operands RegToMem | jne L8>
        cmp B$FirstOperandwbit xSize | je L1>
        cmp B$FirstOperandwbit OctoSize | jne L7>
L1:     cmp B$SecondRegGender XmmReg | jne L8>
            or op1 1 | ToOpcode op1
            mov al B$SecondReg | shl al 3 | or al B$ModBits | or al B$RmBits
            LastOpcode al

L7: BadOperandSize
L8: BadOperand


XMMtoMem:
    ParmsAny 2
    ToOpcode 0F | jmp L2<
;;
 Exemple for Xmm4:      MOVD - Move doubleword N N Y N

   reg to Xmmreg    0000 1111:01101110: 11 Xmmreg1 reg2   ; movd XMM0 eax
   reg from Xmmxreg 0000 1111:01111110: 11 Xmmreg2 reg1   ; movd eax XMM0
   mem to Xmmxreg   0000 1111:01101110: mod Xmmreg1 r/m   ; movd XMM0 D$val
   mem from Xmmxreg 0000 1111:01111110: mod Xmmreg2 r/m   ; movd D$val XMM0

 2 mnemonics only for mm4. In both cases op1 (true second opcode) is the same for the
 4 cases, but with 00_10000 add in cases 2 and 4.

 This is exactely the same encoding as for mmFour, but with an added 066 prefix. So:
;;

XmmFour:
    ToOpcode 066
    cmp B$FirstRegGender XmmReg | jne L1>
        mov B$FirstRegGender mmReg | jmp mmFour
L1: cmp B$SecondRegGender XmmReg | jne L1>
        mov B$SecondRegGender mmReg | jmp mmFour
L1: BadOperand


____________________________________
; These Routines are for 066 prefixed for SSE SIMD Integer Instructions.

; Example: PAVGB mm1,  mm2/m64    (0F E0 /r)
;          PAVGB xmm1, xmm2/m128  (66 0F E0, /r)

OQregMemToReg:
    ParmsAny 2
    ..If B$Operands = RegToReg
        .If B$FirstRegGender = MMreg
            If B$SecondRegGender = MMreg
                ToOpcode 0F, op1
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                LastOpcode al
            End_If
            BadOperand

        .Else_If B$FirstRegGender = XMMreg
            If B$SecondRegGender = XMMreg
                ToOpcode 066, 0F, op1
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                LastOpcode al
            End_If
            BadOperand

        .End_If
        BadOperand

    ..Else_If B$Operands = MemToReg
        On B$SecondOperandwbit = xSize, mov B$SecondOperandwbit Octosize
        .If B$FirstRegGender = MMreg
            If B$SecondOperandwbit = QuadSize
                ToOpcode 0F, op1
                mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
                LastOpcode al
            End_If
            BadOperandSize
        .Else_If B$FirstRegGender = XMMreg
            If B$SecondOperandwbit = OctoSize
                ToOpcode 066, 0F, op1
                mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
                LastOpcode al
            End_If
            BadOperandSize

        .End_If
        BadOperand
    ..End_If
    BadOperand

;;
 Example: PEXTRW r32,  mm, imm8  (0F C5 /r ib)
          PEXTRW r32, xmm, imm8  (66 0F C5 /r ib) (Actually used only for PEXTRW)

 Note: Encodage of Registers fields are given in reverse of usual order in Intel Doc.
 Don't know if it is an error or not. I set here the record as given > to be controled.
 (same for PINSRW).
 
 'Werewolf' says it should be reversed. Therefore the replacement.
;;

OQregRegImm8:
    ParmsAny 3
    On B$immInside = &FALSE, error D$EndingImmPtr
    cmp B$Operands RegToReg | jne L9>
        cmp B$FirstRegGender reg | jne L9>
            mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
            cmp B$SecondRegGender XMMreg | jne L1>
                cmp D$Imm32, 00111 | ja L8>
                ToOpcode 066 | jmp L2>
L1:         cmp B$SecondRegGender MMreg | jne L9>
                cmp D$Imm32, 0011 | ja L8>
L2:                 ToOpcode 0F, op1
                    ;mov al B$SecondReg | shl al 3 | or al 0011_000_000 | or al B$FirstReg ;!!!!
                    mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                    LastOpcode al

L8: BadOperandSize
L9: BadOperand


; Example: PINSRW  mm, r32/mem16, imm8 ; <<<< this one in reserse order too !!!!!!!!!!!!!!!!
;          PINSRW xmm, r32/mem16, imm8
;
; in all case mm (xmm) apear as regBits.

OQregMemImm8:
    ParmsAny 3
    On B$immInside = &FALSE, error D$EndingImmPtr
    mov B$TrueSize ByteSize                 ; to ajust imm size to 8 bits storage
    ..If B$Operands = RegToReg
        .If B$FirstRegGender = MMreg
            If B$SecondRegGender = reg
                cmp D$Imm32, 0011 | ja L8>>
                ToOpcode 0F, op1
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                LastOpcode al
            End_If
            BadOperand

        .Else_If B$FirstRegGender = XMMreg
            If B$SecondRegGender = reg
                cmp D$Imm32, 00111 | ja L8>>
                ToOpcode 066, 0F, op1
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg
                LastOpcode al
            End_If
            BadOperand

        .End_If
        BadOperand


    ..Else_If B$Operands = MemToReg
        cmp B$SecondOperandwbit Wordsize | jne L8>>
        .If B$FirstRegGender = MMreg
                cmp D$Imm32, 0011 | ja L8>>
                ToOpcode 0F, op1
                mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
                LastOpcode al
        .Else_If B$FirstRegGender = XMMreg
                cmp D$Imm32, 00111 | ja L8>>
                ToOpcode 066, 0F, op1
                mov al B$FirstReg | shl al 3 | or al B$ModBits | or al B$RmBits
                LastOpcode al
        .End_If
        BadOperand
    ..End_If

    BadOperand
L8: BadOperandSize


; Example: MOVMSKB reg32  mmreg
;          MOVMSKB reg32 Xmmreg   ; here also dest at last

OQReg32Reg:
    ParmsAny 2
    cmp B$Operands RegToReg | jne L9>
        cmp B$FirstRegGender reg | jne L9>
            cmp B$SecondRegGender XMMreg | jne L1>
                ToOpcode 066 | jmp L2>
L1:         cmp B$SecondRegGender MMreg | jne L9>
L2:             ToOpcode 0F, op1
                mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$secondReg
                LastOpcode al

L9: BadOperand

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 3D Now: out of FEMMS and PREFETCH/W, all 3D Now instructions aply on

 mmReg1, mmReg2/mm64

 and all start with 0F 0F, and end with the true opcode (suffix) written at last by
 EncodeLines
;;

[mm3Dsuffix: 0]

mm3D:
    ParmsAny 2 | ToOpcode 0F, 0F | mov B$mm3Dsuffix op1
    cmp B$FirstRegGender mmReg | jne L8>>
    cmp B$Operands RegToReg | jne L1>
        cmp B$SecondRegGender mmReg | jne L8>>
            mov op1 B$FirstReg | shl op1 3 | or op1 B$SecondReg
            or op1 00_11_000_000 | LastOpcode op1
L1: cmp B$Operands MemToReg | jne L8>
        On B$SecondOperandwBit <> QuadSize, error D$Mem3DPtr
        mov op1 B$FirstReg | shl op1 3 | or op1 B$ModBits | or op1 B$RmBits | LastOpcode op1
L8: BadOperand

 ________________________________________________________________________________________


op: Parms 0 | LastOpcode op1

op_P1: Parms 1 | LastOpcode op1

op_imm8: Parms 1 | imm8Size | LastOpcode op1

op_imm16: Parms 1 | imm16Size | LastOpcode op1

reg_in_op_imm: Parms 2 | GPreg1 | or op1 B$FirstReg | LastOpcode op1 ; for mov reg imm (alternate)

reg1_in_op: GPreg1 | or op1 B$FirstReg                       ; for dec / inc
op_dis: Parms 1 | lastOpcode op1

op_dis8: Parms 1 | dis8Size | LastOpcode op1

op_reg1: Parms 1 | GPreg1 | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

  ; This is only for push/pop sreg when reg is FS or GS. We overwrite the no use 066h
  ; previously written because of size:
op_sreg3: Parms 1 | dec edi | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

op_modRm: Parms 1 | ToOpcode op1 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

op_modRm_P2: Parms 2 | ToOpcode op1 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

op_reg2reg1:
  Parms 2 | GPreg1_2 | ToOpcode op1
  mov op1 B$SecondReg | shl op1 3 | or op1 B$FirstReg | or op2 op1 | LastOpcode op2

op_w_modReg1Rm:
  Parms 2 | GPreg1 | ToOpcode op1
  or op2 B$wBit | ToOpcode op2
  mov op3 B$FirstReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_w_modReg1RmX:
  XParms 2 | GPreg1 | ToOpcode op1
  or op2 B$wBit | ToOpcode op2
  mov op3 B$FirstReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_w_modreg2Rm:
  Parms 2 | GPreg2 | ToOpcode op1
  or op2 B$wBit | ToOpcode op2
  mov op3 B$SecondReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_w_reg2reg1:
  Parms 2 | GPreg1_2 | ToOpcode op1
  or op2 B$wBit | ToOpcode op2
  mov op1 B$SecondReg | shl op1 3 | or op3 op1 | or op3 B$FirstReg | LastOpcode op3

op_w_reg1reg2X:                ; special for B$wBit]s disagree in MOVSX / MOVZX
  XParms 2 | GPreg1_2 | ToOpcode op1
  or op2 B$wBit | ToOpcode op2
;  mov op1 B$SecondReg | shl op1 3 | or op3 op1 | or op3 B$FirstReg | LastOpcode op3
  mov op1 B$FirstReg | shl op1 3 | or op3 op1 | or op3 B$SecondReg | LastOpcode op3

op_modReg1Rm:
  Parms 2 | GPreg1 | ToOpcode op1
  mov op2 B$FirstReg | shl op2 3 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

op_modReg2Rm:
  Parms 2 | GPreg2 | ToOpcode op1
  mov op2 B$SecondReg | shl op2 3 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2
 ______________________________________________________________________________________

op_op: Parms 0 | ToOpcode op1 | LastOpcode op2

op_op_P1: Parms 1 | ToOpcode op1 | LastOpcode op2

op_op_P2: ParmsAny 2 | ToOpcode op1 | LastOpcode op2

op_op_dis: Parms 1 | ToOpcode op1 | LastOpcode op2

op_op_reg1:
  Parms 1 | GPreg1 | ToOpcode op1, op2
  or op3 B$FirstReg | LastOpcode op3

op_op_reg16:
  Parms 1 | GPreg1 | On B$FirstOperandWbit <> WordSize,  error D$VERRwordPtr
  ToOpcode op1, op2 | or op3 B$FirstReg | LastOpcode op3

op_op_reg1reg2:
  Parms 2 | GPreg1_2 | ToOpcode op1, op2
  mov op1 B$FirstReg | shl op1 3 | or op3 op1 | or op3 B$SecondReg | LastOpcode op3

op_op_reg1_imm8:
  Parms 2 | GPreg1 | imm8Size | ToOpcode op1, op2 | or op3 B$FirstReg | LastOpcode op3

op_op_reg2reg1:
  Parms 2 | GPreg1_2 | ToOpcode op1, op2
  mov op1 B$SecondReg | shl op1 3 | or op3 op1 | or op3 B$FirstReg | LastOpcode op3

op_op_reg2reg1_cl:
  Parms 3 | GPreg1_2
  On B$ThirdOperandWbit <> ByteSize,  error D$NeedByteSizePtr
  ToOpcode op1, op2
  mov op1 B$SecondReg | shl op1 3 | or op3 op1 | or op3 B$FirstReg | LastOpcode op3

op_op_reg2reg1_imm8:
  Parms 3 | imm8Size | GPreg1_2 | ToOpcode op1, op2
  mov op1 B$SecondReg | shl op1 3 | or op3 op1 | or op3 B$FirstReg | LastOpcode op3

op_op_modRm:
  Parms 1 | ToOpcode op1, op2 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_op_modRm16:
  Parms 1
  On B$FirstOperandWbit <> WordSize,  error D$VERRwordPtr
  ToOpcode op1 | ToOpcode op2 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3


op_op_modRm_imm8:
  Parms 2 | imm8Size | ToOpcode op1, op2
  or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_op_modReg2Rm:
  Parms 2 | GPreg2 | ToOpcode op1, op2
  mov op3 B$SecondReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_op_modReg2Rm_cl:
  Parms 3 | GPreg2 | ToOpcode op1, op2
  On B$ThirdOperandWbit <> ByteSize,  error D$NeedByteSizePtr
  mov op3 B$SecondReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_op_modReg2Rm_imm8:
  Parms 3 | GPreg2 | imm8Size | ToOpcode op1, op2
  mov op3 B$SecondReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

op_op_modReg1Rm:
  Parms 2 | GPreg1 | ToOpcode op1, op2
  mov op3 B$FirstReg | shl op3 3 | or op3 B$ModBits | or op3 B$RmBits | LastOpcode op3

 ________________________________________________________________________________________

OPw: Parms 0 | or op1 B$wBit | LastOpcode op1

w_P2: ParmsAny 2 | or op1 B$wBit | LastOpcode op1

w_imm8:
  Parms 2 | imm8Size | On B$SecondReg <> RegEax,  error D$OnlyAccPtr
  or op1 B$wBit | LastOpcode op1

w_imm: Parms 2 | or op1 B$wBit | lastOpcode op1

; w_dis is used only by MOV mem to accum.   or  accum. to mem.:

w_dis: Parms 2 | or op1 B$wBit | LastOpcode op1

w_reg1:
  Parms 1 | GPreg1 | or op1 B$wBit | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

w_reg1_P2:
  Parms 2 | imm8Size | GPreg1
  mov B$immInside &FALSE         ; exemple: SHL eax, 1  >>> no more imm
  or op1 B$wBit | ToOpcode op1
  or op2 B$FirstReg | LastOpcode op2

w_reg1_P2cl:
    ParmsAny 2 | GPreg1
    cmp B$SecondReg RegCL | jne L1>
    cmp B$SecondOperandWbit ByteSize | je L2>
L1:   error D$MixTypePtr
L2: or op1 B$FirstOperandWbit | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

w_reg1reg2:
  Parms 2 | GPreg1_2 | or op1 B$wBit | ToOpcode op1
  mov op1 B$FirstReg | shl op1 3 | or op2 op1 | or op2 B$SecondReg | LastOpcode op2

w_reg2reg1:
  Parms 2 | GPreg1_2 | or op1 B$wBit | ToOpcode op1
  mov op1 B$SecondReg | shl op1 3 | or op2 op1 | or op2 B$FirstReg | LastOpcode op2

w_reg1_imm8:
  Parms 2 | GPreg1 | imm8Size
  or op1 B$wBit | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

w_reg1_imm:
  Parms 2 | GPreg1 | or op1 B$wBit | ToOpcode op1 | or op2 B$FirstReg | LastOpcode op2

w_modRm:
  Parms 1 | or op1 B$wBit | ToOpcode op1
  or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

w_modRm_P2:
  Parms 2 | mov B$immInside &FALSE         ; exemple: SHL eax, 1  >>> no more imm
  or op1 B$wBit | ToOpcode op1
  or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

w_modRm_P2cl:
    ParmsAny 2
    On B$FirstOperandWbit = WordSize, ToOpcode 066
    cmp B$SecondReg RegCL | jne L1>
    cmp B$SecondOperandWbit ByteSize | je L2>
L1:   error D$MixTypePtr
L2: or op1 B$FirstOperandWbit | ToOpcode op1
    or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

w_modReg1Rm:
  Parms 2 | GPreg1 | or op1 B$wBit | ToOpcode op1
  mov op2 B$FirstReg | shl op2 3 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

w_modReg2Rm:
  Parms 2 | GPreg2 | or op1 B$wBit | ToOpcode op1
  mov op2 B$SecondReg | shl op2 3 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

w_modRm_imm8:
  Parms 2 | imm8Size | or op1 B$wBit | ToOpcode op1
  or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

w_modRm_imm:
  Parms 2 | or op1 B$wBit | ToOpcode op1
  or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

 _______________________________________________________________________________________


sw_reg1_imm:
  Parms 2 | GPreg1 | signImm
 ; On B$wBitDisagr&TRUEee = , or op1 B$sBit
  On B$FirstOperandWbit <> ByteSize,  or op1 B$sBit
  or op1 B$wBit | ToOpcode op1
  or op2 B$FirstReg | LastOpcode op2

sw_modRm_imm:
  Parms 2 | signImm
  ;On B$wBitDisagree = &TRUE,  or op1 B$sBit
  On B$FirstOperandWbit <> ByteSize,  or op1 B$sBit
  or op1 B$wBit | ToOpcode op1
  or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

 ________________________________________________________________________________________

s_reg1reg2_imm:      ; imul only: pb: can be 8 or 16 or 32 bits imm >>> sbts (?)
  Parms 3 | signImm | GPreg1_2
 ; On B$wBitDisagree = &TRUE,  or op1 B$sBit
 ;  On B$FirstOperandWbit <> ByteSize,  or op1 B$sBit     ; supposed of no use...
  or op1 B$sBit | ToOpcode op1
  mov op1 B$FirstReg | shl op1 3 | or op2 op1 | or op2 B$SecondReg | LastOpcode op2

s_modReg1Rm_imm:     ; idem
  Parms 3 | signImm | GPreg1
  ; On B$wBitDisagree = &TRUE, ...   ; supposed of no use...
  or op1 B$sBit | ToOpcode op1
  mov op2 B$FirstReg | shl op2 3 | or op2 B$ModBits | or op2 B$RmBits | LastOpcode op2

 ; used by 'push imm' only:
s_imm: Parms 1 | mov B$TrueSize DoubleSize | signImm | or op1 B$sBit | LastOpcode op1

s_imm16: Parms 1 | ToOpcode 066 | mov B$TrueSize wordSize | signImm | or op1 B$sBit
         LastOpcode op1

 _________________________________________________________________________________________

sReg2:    ; for POP only. Nothing special here because segment regs are greater code
          ; than 0111B
          ;
          ; a 066h prefix have been written because of size. This is no use for this:
  dec edi
  Parms 1 | or op1 B$FirstReg | LastOpcode op1

 _________________________________________________________________________________________

 ; Only for XCHG with EAX as second register:

XOPreg1: Parms 2 | or op1 B$FirstReg | LastOpcode op1













