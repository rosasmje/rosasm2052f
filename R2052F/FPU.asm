TITLE FPU

;;
  'UsedByTheAssembler' (very old traditional Routines for Ascii to Float on Stack)
  
  'UsedBytheEqualParser' 
  
  'UsedByTheDebugger' (and by the Disassembler for 'FloatToUString')
  
  'UsedForStudy' (Under developements)
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

UsedByTheAssembler:

[Int10: 10   CurrentConvertedDigit: 0   NumberOfDecimalPlaces: 0   FaultCharAndPos: 0]

atof:
    finit | mov D$FaultCharAndPos 0
    push edi, esi
      call convert                  ; convert mantissa (returns with 'E' if exponent present)

      neg edx                                ; save -1 * decimal places
      mov D$NumberOfDecimalPlaces edx

      IF al <> 'E'
        mov B$FaultCharAndPos al             ; faulty char present in mantissa
        pop ecx | mov edi esi | sub edi ecx  ; fault char pos
        fldz                                 ; assume zero exponent
      Else_If al = 'E'
        call convert                         ; convert exponent
        mov B$FaultCharAndPos al      ; Save faulty character present in exponent
        pop ecx | mov edi esi | sub edi ecx  ; fault char pos
      End_If

      fiadd D$NumberOfDecimalPlaces          ; adjust exponent for dec. places in mantissa
      call falog                             ; raise 10 to power
      fmul ST0 ST1                           ; exponent * mantissa

      ; Provide information about faulty character and its position

      mov eax edi                            ; Get possible fault position
      shl eax 8                              ; shift to high word
      or eax D$FaultCharAndPos               ; OR faults togeter
    pop edi
ret                                      ; return ST(0) = result

_________________________________________________________________
;;
 CONVERT:      Called by ATOF to convert ASCII number with possible sign and/or decimal point
 Call with:    ESI = address of string
 Returns:      ST(0) = result
               AL    = first unconvertable character
               EDX    = number of digits after decimal point
               ESI = address+1 of character in AL
 Uses:         AH, CX
;;
_________________________________________________________________        ;

convert:                                ; convert numeric field
    fldz | mov ecx 0, edx 0-1           ; initialize result, sign, decimal count

   lodsb
        cmp al AddSign | je L2>         ; if + sign proceed
        cmp al SubSign | jne L3>        ; is it - sign? no > test if numeric
          dec ecx                       ; yes, set sign flag

L2: lodsb                               ; get next character

L3: cmp al '0' | jb L4>                 ; is character valid?
      cmp al,'9' | ja L4>
        and eax 0f                      ; isolate lower four bits
        mov D$CurrentConvertedDigit eax ; and save digit value
        fimul D$int10                   ; previous value * 10
        fiadd D$CurrentConvertedDigit   ; accumulate new digit

        or edx edx | js L2<             ; past decimal point? no > convert next digit
          inc edx | jmp L2<             ; yes > count digits / convert next digit

L4: cmp al '.' | jne L5>                ; no point > proceed
      inc edx | jmp L2<                 ; indicate decimal found > convert more digits
L5:    jcxz L6>                         ; jump if result pos.
        fchs                            ; make result negative

L6: or edx edx | jns L7>                ; decimal point found? yes > jump
      mov edx 0                         ; no, return zero places

L7: ret                                 ; return ST(0) = result


_____________________________________________________________________________________
;;
-------------------------------------------------------;
 Calculate Common Antilog on 80x87   ;
       ;
 Call  :    st(0)     = logarithm (base 10)  ;
       ;
 Return:    st(0)     = antilog to base 10  ;
       ;
 Coprocessor should be initialised before call  ;
-------------------------------------------------------;
;;
; FWAIT doesn't seam to be of any use (works the same without on my computer).

[FalogOldcw: W$ 0   FalogNewcw: 0]

falog:
        fldl2t                          ; st= log2(10), st(1)=argument
        fmulp   st1 st0                 ; st= log2(10) * argument
        fld     st0                     ; take copy
     ;   fwait
     ;   fstcw   W$Falogoldcw            ; store old control word
     ;   fwait                           ; wait 'till it arrives
     ;   mov     ax W$Falogoldcw         ; Load control word
     ;   and     ax 0f3ff                ; Field to "round down"
     ;   or      ax 0400                 ; Set precision to 53 bit mantissa
     ;   mov     W$Falognewcw ax         ; Got new control word
     ;   fldcw   W$Falognewcw            ; Force rounding mode
        frndint                         ; Round from real to integer
     ;   fldcw   W$Falogoldcw            ; Restore old rounding mode
        fld     st0                     ; take copy
        fxch    st2                     ; Get original product
        fsubrp  st1 st0                 ; Get fractional part
        fld1
        fchs
        fxch    st1                     ; Scale fractional part
        fscale    ;
        fstp    st1                     ; Discard coprocessor junk
        f2xm1                           ; Raise 2 to power-1
        fld1                            ; Push 1 onto stack
        faddp   st1 st0                 ; Correct for the -1
        fmul    st0 st0                 ; Square result
        fscale                          ; Scale by integer part
        fstp    st1                     ; Return with result in st(0)
      ret

____________________________________________________________________________________________
____________________________________________________________________________________________

UsedBytheEqualParser:

Proc FloatToAscii:
;;
  This procedure was written by Raymond Filiatreault, December 2002
  (Modified Betov December 2002). 
  (Slightly modified by Scarmatil to fit upon RosAsm's signs, December 2003) 
  
  This FloatToAscii function converts an 80-bit REAL number (Src) to its
  decimal representation as a zero terminated alphanumeric string which
  is returned at the specified memory destination unless an invalid
  operation is reported by the FPU. The format of the string can be 
  specified as regular, or scientific notation. The number of decimal
  places returned must also be specified but the total number of digits
  must not exceed 18.
  
  The source can be an 80-bit REAL number from the FPU itself or from
  memory. If the source is taken from the FPU, its value will be preserved
  there if no error is reported.
  
  The source is not checked for validity. This is the programmer's
  responsibility.
  
  This procedure is based on using an FPU instruction to convert the
  REAL number into a specific packed decimal format. After unpacking,
  the decimal point is positioned as required.
  
  Only EAX is used to return error or success. All other registers are
  preserved.
____________________________________________________________________________________________

  Calling:     > call FloatToAscii Source, Destination, Decimal, FLAG
  
  Source: Either a Pointer to a Data [T$Source: ...], or &NULL if you 
          "fld F$ / R$ / T$ Source"  before calling.
          
  Destination: Pointer to a Data Buffer for the Ascii (Space-terminated) String 
               (Max Size = 25 Bytes).
  
  Decimal: The Number of wanted decimals (Max = 15).
  
  FLAG (for notation choice): Either SCIENTIFIC or REGULAR.
____________________________________________________________________________________________
;;

; Flags:

[REGULAR 0    SCIENTIFIC 1]

    Arguments @Source, @Destination, @Decimal, @Flag
    Local @temporary, @eSize, @oldcw, @truncw, @stword
    Structure @BCD 12, @bcdstr 0

        fclex                   ;clear exception flags on FPU

      ; Get the specified number of decimals for result (MAX = 15):
        On D@Decimal > 0F, mov D@Decimal 0F

      ; The FPU will be initialized only if the source parameter is not taken
      ; from the FPU itself (D@ Source <> &NULL):
        .If D@Source = &NULL
            fld st0             ;copy it to preserve the original value
        .Else
            mov eax D@Source
            If eax > 0400_000
                finit | fld T$eax
              ; Check first if value on FPU is valid or equal to zero:
                ftst                    ;test value on FPU
                fstsw W@stword          ;get result
                test W@stword 04000     ;check it for zero or NAN
                jz L0>                  ;continue if valid non-zero
                test W@stword 0100      ;now check it for NAN
                jnz L1>                 ;Src is NAN or infinity - cannot convert
                  ; Here: Value to be converted = 0
                    mov eax D@Destination | mov W$eax '0' ; Write '0', 0 szstring
                    mov eax &TRUE | finit | ExitP
            Else
L1:             finit | mov eax &FALSE | ExitP
            End_If
        .End_If

      ; Get the size of the number:
L0:     fld st0                 ;copy it
        fabs                    ;insures a positive value
        fld1 | fldl2t
        fdivp ST1 ST0           ;->1/[log2(10)]
        fxch | fyl2x            ;->[log2(Src)]/[log2(10)] = log10(Src)

        fstcw W@oldcw           ;get current control word
        mov ax W@oldcw
        or ax 0C00              ;code it for truncating
        mov W@truncw ax
        fldcw W@truncw          ;change rounding code of FPU to truncate

        fist D@eSize            ;store characteristic of logarithm
        fldcw W@oldcw           ;load back the former control word

        ftst                    ;test logarithm for its sign
        fstsw W@stword          ;get result
        test W@stword 0100      ;check if negative
        jz L0>
            dec D@eSize

L0:     On D@eSize > 15, mov D@Flag SCIENTIFIC

      ; Multiply the number by a power of 10 to generate a 16-digit integer:
L0:     fstp st0                ;get rid of the logarithm
        mov eax 15
        sub eax D@eSize         ;exponent required to get a 16-digit integer
        jz L0>                  ;no need if already a 16-digit integer
            mov D@temporary eax
            fild D@temporary
            fldl2t | fmulp ST1 ST0       ;->log2(10)*exponent
            fld st0 | frndint | fxch
            fsub st0 st1        ;keeps only the fractional part on the FPU
            f2xm1               ;->2^(fractional part)-1
            fld1
            faddp ST1 ST0       ;add 1 back
            fscale              ;re-adjust the exponent part of the REAL number
            fxch
            fstp st0
            fmulp ST1 ST0       ;->16-digit integer

L0:     fbstp T@bcdstr          ;transfer it as a 16-digit packed decimal
        fstsw W@stword          ;retrieve exception flags from FPU
        test W@stword 1         ;test for invalid operation
        jnz L1<<                ;clean-up and return error

      ; Unpack bcd, the 10 bytes returned by the FPU being in the little-endian style:
        push ecx, esi, edi
            lea esi D@bcdstr+9
            mov edi D@Destination
            mov al B$esi        ;sign byte
            dec esi | dec esi
            If al = 080
                mov al minusSign      ;insert sign if negative number
            Else
                mov al Space      ;insert space if positive number
            End_If
            stosb

            ...If D@Flag = REGULAR
              ; Verify number of decimals required vs maximum allowed:
                mov eax 15 | sub eax D@eSize
                cmp eax D@Decimal | jae L0>
                    mov D@Decimal eax

              ; ;check for integer digits:
L0:             mov ecx D@eSize
                or ecx ecx           ;is it negative
                jns L3>
                  ; Insert required leading 0 before decimal digits:
                    mov ax '0o' | stosw
                    neg ecx
                    cmp ecx D@Decimal | jbe L0>
                        jmp L8>>

L0:                 dec ecx | jz L0>
                        stosb | jmp L0<
L0:
                    mov ecx D@Decimal | inc ecx
                    add ecx D@eSize | jg L4>
                        jmp L8>>

              ; Do integer digits:
L3:             inc ecx
L0:             movzx eax B$esi | dec esi | ror ax 4 | ror ah 4
                add ax '00' | stosw | sub ecx 2 | jg L0<
                jz L0>
                    dec   edi

L0:             cmp D@Decimal 0 | jz L8>>
                    mov al pointSign | stosb
                    If ecx <> 0
                        mov al ah | stosb
                        mov ecx D@Decimal | dec ecx | jz L8>>
                    Else
                        mov ecx D@Decimal
                    End_If

              ; Do decimal digits:
L4:             movzx eax B$esi
                dec esi
                ror ax 4 | ror ah 4 | add ax 03030 | stosw
                sub ecx 2 | jg L4<
                jz L1>
                    dec   edi
L1:             jmp L8>>

          ; scientific notation
            ...Else
                 mov ecx D@Decimal | inc ecx
                movzx eax B$esi | dec esi
                ror ax 4 | ror ah 4 | add ax '00' | stosb
                mov al pointSign | stosb
                mov al ah | stosb
                sub ecx 2 | jz L7>
                jns L0>
                    dec edi | jmp L7>
L0:             movzx eax B$esi
                dec esi
                ror ax 4 | ror ah 4
                add ax '00' | stosw | sub ecx 2 | jg L0<
                jz L7>
                    dec edi

L7:             mov al 'E' | stosb
                mov al plusSign, ecx D@eSize | or ecx ecx | jns L0>
                    mov al minusSign | neg ecx
L0:             stosb
              ; Note: the absolute value of the size could not exceed 4931
                mov eax ecx
                mov cl 100
                div cl          ;->thousands & hundreds in AL, tens & units in AH
                push eax
                    and eax 0FF ;keep only the thousands & hundreds
                    mov cl 10
                    div cl      ;->thousands in AL, hundreds in AH
                    add ax '00' ;convert to characters
                    stosw       ;insert them
                pop eax
                shr eax 8       ;get the tens & units in AL
                div cl          ;tens in AL, units in AH
                add ax '00'     ;convert to characters
                stosw           ;insert them
            ...End_If

L8:         mov B$edi Space         ;string terminating character
        pop edi, esi, ecx

        finit | mov eax D@eSize
EndP
____________________________________________________________________________________________
;;
  This procedure was written by Raymond Filiatreault, December 2002
  Modified Betov, December 2002
  (Slightly modified by Scarmatil to fit upon RosAsm's signs, December 2003)
  
  This AsciitoFloat function converts a decimal number from a zero terminated
  alphanumeric string format (Src) to an 80-bit REAL number and returns
  the result as an 80-bit REAL number at the specified destination (either
  the FPU top or a memory location), unless an invalid operation is
  reported by the FPU.
 
  The source can be a string in regular numeric format or in scientific
  notation. The number of digits (including leading 0's) must not exceed
  18. If in scientific format, the exponent must be within +/-4931
 
  The source is checked for validity. The procedure returns an error if
  a character other than those acceptable is detected prior to the
  terminating zero or the above limits are exceeded.
 
  This procedure is based on converting the digits into a specific packed
  decimal format which can be used by the FPU and then adjusted for an
  exponent of 10.
 
  Only EAX is used to return error or success. All other registers are
  preserved.
____________________________________________________________________________________________

  Calling:     > call AsciitoFloat Source, Destination
  
  Source: Pointer to a Floating Point String (either regular or scientific 
          notation).
  
  Destination: Either a Pointer to a [T$FPValue: ...] or &NULL.

  In case of &NULL Destination, the result is left on the FPU Stack, and
  you have to you have to pop the result by yourself. Usefull in cases when
  you want Real8 or Real4, or when you want to go on computing with the result.
____________________________________________________________________________________________
;;
Proc AsciitoFloat:
    Arguments @lpSrc, @lpDest
    Local @stword, @ten
    Structure @BCD 12, @bcdstr 0
    Uses ebx, ecx, edx, esi, edi

        mov eax 0, ebx 0, edx 0, ecx 19, D@ten 10
        mov edi D@BCD | mov D$edi 0, D$edi+4 0, D$edi+8 0 | add edi 8

        mov esi D@lpSrc
        mov al B$esi
        If al = Space       ; string empty?
            jmp E7>>
        Else_If al = minusSign
            mov B$edi+1 080
            inc esi
        End_If

        ; Strip pointless 0
        While B$esi = '0'
            If B$esi+1 = pointSign | inc esi | jmp L2> | End_If
            On B$esi+1 < '0', jmp L2>
            On B$esi+1 > '9', jmp L2>
            inc esi
        End_While

      ; Convert the digits to packed decimal:
L2:     lodsb | On al = 'e', mov al 'E'

      ; bh used to set the decimal point flag (one point only):
        ...If al = pointSign
            If bh = 0
                or bh 1 | jmp L2<
            End_If
        ...Else_If al = 'E'
            On cl < 19, jmp L6>>        ;error if no digit before E
        ...Else_If al = Space
            If cl < 19                  ;error if no digit before terminating Space
                xor al al | rol al 4 | ror ax 4 | mov B$edi al | jmp L5>>
            End_If
        ...Else
            ..If al >= '0'
                .If al <= '9'           ;error if bad Char.
                    dec ecx
                    If ecx > 0          ;error if more than 18 digits in number
                        sub al '0' | On bh = 0, inc bl
                        test ah 040 | jz L1>
                            rol al 4 | ror ax 4 | mov B$edi al | dec edi | xor eax eax  | jmp L2<<
L1:                     mov ah al | or ah 040 | jmp L2<<
                    End_If
                .End_If
            ..End_If
        ...End_If

        jmp E7>>                        ; Error case if falling here.

      ; Output:
L5:     fbld T@bcdstr
        mov eax 18 | sub al bl | sub edx eax | call XexpY edx
        fmulp ST1 ST0
        fstsw W@stword                      ;retrieve exception flags from FPU
        wait | test W@stword 1 | jnz E7>>   ;test for invalid operation
        mov eax D@lpDest
        If D@lpDest <> &NULL
            mov eax D@lpDest |  fstp T$eax      ;store result at specified address
        End_If
        jmp E8>>

      ; Scientific notation (exponent in edx):
L6:     movzx eax B$esi | inc esi
        cmp al plusSign | je L0>
            cmp al minusSign | jne L6>
            stc | rcr eax 1         ;keep sign of exponent in most significant bit of EAX
L0:     lodsb                               ;get next digit after sign

L6:     push eax |
            and eax 0FF | jnz L0>           ;continue if 1st byte of exponent is not terminating 0
L6:             pop eax | jmp E7>>          ;no exponent
L0:         sub al '0' | jc L6<             ;unacceptable character
            cmp al 9 | ja L6<               ;unacceptable character
            push eax
                mov eax edx | mul D@ten | mov edx eax
            pop eax
            add edx eax | cmp edx 4931 | ja L6<     ;exponent too large
            lodsb
            cmp al Space | jne L0<
        pop eax                             ;retrieve exponent sign flag
        rcl eax 1 | jnc L0>                 ;is most significant bit set?
            neg edx
L0:     jmp L5<<

E7:     mov eax &FALSE | finit | jmp E9>
E8:     mov eax &TRUE
E9: EndP


;put 10 to the proper exponent (value in EDX) on the FPU

Proc XexpY:
    Argument @Tempdw

        fild D@tempdw           ;load the exponent
        fldl2t                  ;load log2(10)
        fmulp ST1 ST0           ;->log2(10)*exponent

;at this point, only the log base 2 of the 10^exponent is on the FPU
;the FPU can compute the antilog only with the mantissa
;the characteristic of the logarithm must thus be removed

        fld ST0                 ;copy the logarithm
        frndint                 ;keep only the characteristic
        fsub ST1 ST0            ;keeps only the mantissa
        fxch                    ;get the mantissa on top

        f2xm1                   ;->2^(mantissa)-1
        fld1
        faddp ST1 ST0           ;add 1 back

;the number must now be readjusted for the characteristic of the logarithm

        fscale                  ;scale it with the characteristic

;the characteristic is still on the FPU and must be removed

        fxch                    ;bring it back on top
        fstp ST0                ;clean-up the register
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Routines for FPU study.
;;

UsedForStudy:
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Strings Tables used to translate from ST0 to String:

  The effective range of integer Values is for 'Fp.Bit1' (Value 1) considered
  as an implyed 'FPBit0'. The Max Value of the Mantissa (all Bits set on) is:
  Max qWord:      18446744073709551615  (0FFFF_FFFF__FFFF_FFFF) (???....).
;;
 [FPBit64: B$ 0  '18446744073709551616', 0  ; Max + 1 (not used).
  FPBit63: B$ 1   '9223372036854775808', 0  ; <<<<<<<<<<<<<<<<<<< Last high Bit.
  FPBit62: B$ 1   '4611686018427387904', 0
  FPBit61: B$ 1   '2305843009213693952', 0
  FPBit60: B$ 1   '1152921504606846976', 0
  FPBit59: B$ 2    '576460752303423488', 0
  FPBit58: B$ 2    '288230376151711744', 0
  FPBit57: B$ 2    '144115188075855872', 0
  FPBit56: B$ 3     '72057594037927936', 0
  FPBit55: B$ 3     '36028797018963968', 0
  FPBit54: B$ 3     '18014398509481984', 0
  FPBit53: B$ 4      '9007199254740992', 0
  FPBit52: B$ 4      '4503599627370496', 0
  FPBit51: B$ 4      '2251799813685248', 0
  FPBit50: B$ 4      '1125899906842624', 0
  FPBit49: B$ 5       '562949953421312', 0

  FPBit48: B$ 5       '281474976710656', 0
  FPBit47: B$ 5       '140737488355328', 0
  FPBit46: B$ 6        '70368744177664', 0
  FPBit45: B$ 6        '35184372088832', 0
  FPBit44: B$ 6        '17592186044416', 0
  FPBit43: B$ 7         '8796093022208', 0
  FPBit42: B$ 7         '4398046511104', 0
  FPBit41: B$ 7         '2199023255552', 0
  FPBit40: B$ 7         '1099511627776', 0
  FPBit39: B$ 8          '549755813888', 0
  FPBit38: B$ 8          '274877906944', 0
  FPBit37: B$ 8          '137438953472', 0
  FPBit36: B$ 9           '68719476736', 0
  FPBit35: B$ 9           '34359738368', 0
  FPBit34: B$ 9           '17179869184', 0
  FPBit33: B$ 10           '8589934592', 0

  FPBit32: B$ 10           '4294967296', 0
  FPBit31: B$ 10           '2147483648', 0
  FPBit30: B$ 10           '1073741824', 0
  FPBit29: B$ 11            '536870912', 0
  FPBit28: B$ 11            '268435456', 0
  FPBit27: B$ 11            '134217728', 0
  FPBit26: B$ 12             '67108864', 0
  FPBit25: B$ 12             '33554432', 0
  FPBit24: B$ 12             '16777216', 0
  FPBit23: B$ 13              '8388608', 0
  FPBit22: B$ 13              '4194304', 0
  FPBit21: B$ 13              '2097152', 0
  FPBit20: B$ 13              '1048576', 0
  FPBit19: B$ 14               '524288', 0
  FPBit18: B$ 14               '262144', 0
  FPBit17: B$ 14               '131072', 0

  FPBit16: B$ 15                '65536', 0
  FPBit15: B$ 15                '32768', 0
  FPBit14: B$ 15                '16384', 0
  FPBit13: B$ 16                 '8192', 0
  FPBit12: B$ 16                 '4096', 0
  FPBit11: B$ 16                 '2048', 0
  FPBit10: B$ 16                 '1024', 0
  FPBit9:  B$ 17                  '512', 0
  FPBit8:  B$ 17                  '256', 0
  FPBit7:  B$ 17                  '128', 0
  FPBit6:  B$ 18                   '64', 0
  FPBit5:  B$ 18                   '32', 0
  FPBit4:  B$ 18                   '16', 0  ; ...
  FPBit3:  B$ 19                    '8', 0  ; ...
  FPBit2:  B$ 19                    '4', 0  ; Exponent = 2
  FPBit1:  B$ 19                    '2', 0] ; Exponent = 1
;;
  The Strings computations begins at the Label depending on the Exponent, and goes along
  downward, for each Bit set on in the Mantissa.
;;
 [Fp.Bit1:  B$ 19 '1', 0                    ; Exponent = 0
; Position 20 is the Point, in the String.
  Fp.Bit2:  B$ 21 '5', 0                    ; Exponent = -1
  Fp.Bit3:  B$ 21 '25', 0                   ; Exponent = -2
  Fp.Bit4:  B$ 21 '125', 0                  ; ...
  Fp.Bit5:  B$ 22  '625', 0                 ; ...
  Fp.Bit6:  B$ 22  '3125', 0
  Fp.Bit7:  B$ 22  '15625', 0
  Fp.Bit8:  B$ 23   '78125', 0
  Fp.Bit9:  B$ 23   '390625', 0
  Fp.Bit10: B$ 23   '1953125', 0
  Fp.Bit11: B$ 24    '9765625', 0
  Fp.Bit12: B$ 24    '48828125', 0
  Fp.Bit13: B$ 24    '244140625', 0
  Fp.Bit14: B$ 24    '1220703125', 0
  Fp.Bit15: B$ 25     '6103515625', 0
  Fp.Bit16: B$ 25     '30517578125', 0

  Fp.Bit17: B$ 25     '152587890625', 0
  Fp.Bit18: B$ 26      '762939453125', 0
  Fp.Bit19: B$ 26      '3814697265625', 0
  Fp.Bit20: B$ 26      '19073486328125', 0
  Fp.Bit21: B$ 27       '95367431640625', 0
  Fp.Bit22: B$ 27       '476837158203125', 0
  Fp.Bit23: B$ 27       '2384185791015625', 0
  Fp.Bit24: B$ 27       '11920928955078125', 0
  Fp.Bit25: B$ 28        '59604644775390625', 0
  Fp.Bit26: B$ 28        '298023223876953125', 0
  Fp.Bit27: B$ 28        '1490116119384765625', 0
  Fp.Bit28: B$ 29         '7450580596923828125', 0
  Fp.Bit29: B$ 29         '37252902984619140625', 0
  Fp.Bit30: B$ 29         '186264514923095703125', 0
  Fp.Bit31: B$ 30          '931322574615478515625', 0
  Fp.Bit32: B$ 30          '4656612873077392578125', 0

  Fp.Bit33: B$ 30          '23283064365386962890625', 0
  Fp.Bit34: B$ 30          '116415321826934814453125', 0
  Fp.Bit35: B$ 31           '582076609134674072265625', 0
  Fp.Bit36: B$ 31           '2910383045673370361328125', 0
  Fp.Bit37: B$ 31           '14551915228366851806640625', 0
  Fp.Bit38: B$ 32            '72759576141834259033203125', 0
  Fp.Bit39: B$ 32            '363797880709171295166015625', 0
  Fp.Bit40: B$ 32            '1818989403545856475830078125', 0
  Fp.Bit41: B$ 33             '9094947017729282379150390625', 0
  Fp.Bit42: B$ 33             '45474735088646411895751953125', 0
  Fp.Bit43: B$ 33             '227373675443232059478759765625', 0
  Fp.Bit44: B$ 33             '1136868377216160297393798828125', 0
  Fp.Bit45: B$ 34              '5684341886080801486968994140625', 0
  Fp.Bit46: B$ 34              '28421709430404007434844970703125', 0
  Fp.Bit47: B$ 34              '142108547152020037174224853515625', 0
  Fp.Bit48: B$ 35               '710542735760100185871124267578125', 0

  Fp.Bit49: B$ 35               '3552713678800500929355621337890625', 0
  Fp.Bit50: B$ 35               '17763568394002504646778106689453125', 0
  Fp.Bit51: B$ 36                '88817841970012523233890533447265625', 0
  Fp.Bit52: B$ 36                '444089209850062616169452667236328125', 0
  Fp.Bit53: B$ 36                '2220446049250313080847263336181640625', 0
  Fp.Bit54: B$ 36                '11102230246251565404236316680908203125', 0
  Fp.Bit55: B$ 37                 '55511151231257827021181583404541015625', 0
  Fp.Bit56: B$ 37                 '277555756156289135105907917022705078125', 0
  Fp.Bit57: B$ 37                 '1387778780781445675529539585113525390625', 0
  Fp.Bit58: B$ 38                  '6938893903907228377647697925567626953125', 0
  Fp.Bit59: B$ 38                  '34694469519536141888238489627838134765625', 0
  Fp.Bit60: B$ 38                  '173472347597680709441192448139190673828125', 0
  Fp.Bit61: B$ 39                   '867361737988403547205962240695953369140625', 0
  Fp.Bit62: B$ 39                   '4336808689942017736029811203479766845703125', 0
  Fp.Bit63: B$ 39                   '21684043449710088680149056017398834228515625', 0
  Fp.Bit64: B$ 39                   '108420217248550443400745280086994171142578125', 0]
  ;  (???!!!...)  .000000000000000000108420217248550443400745280086994171142578125
  ; Does this mean something, internally ??? I suppose only the very first Char(s)...
  ; If this length is no use, it would be interresting to cut off (speed...).

[FPBits:
 FpBit64, FpBit63, FpBit62, FpBit61
 FpBit60, FpBit59, FpBit58, FpBit57, FpBit56, FpBit55, FpBit54, FpBit53, FpBit52, FpBit51
 FpBit50, FpBit49, FpBit48, FpBit47, FpBit46, FpBit45, FpBit44, FpBit43, FpBit42, FpBit41
 FpBit40, FpBit39, FpBit38, FpBit37, FpBit36, FpBit35, FpBit34, FpBit33, FpBit32, FpBit31
 FpBit30, FpBit29, FpBit28, FpBit27, FpBit26, FpBit25, FpBit24, FpBit23, FpBit22, FpBit21
 FpBit20, FpBit19, FpBit18, FpBit17, FpBit16, FpBit15, FpBit14, FpBit13, FpBit12, FpBit11
 FpBit10,  FpBit9,  FpBit8,  FpBit7,  FpBit6,  FpBit5,  FpBit4,  FpBit3,  FpBit2,  FpBit1

 Fp.Bits:
 Fp.Bit1,  Fp.Bit2,  Fp.Bit3,  Fp.Bit4,  Fp.Bit5,  Fp.Bit6,  Fp.Bit7,  Fp.Bit8,  Fp.Bit9,  Fp.Bit10
 Fp.Bit11, Fp.Bit12, Fp.Bit13, Fp.Bit14, Fp.Bit15, Fp.Bit16, Fp.Bit17, Fp.Bit18, Fp.Bit19, Fp.Bit20
 Fp.Bit21, Fp.Bit22, Fp.Bit23, Fp.Bit24, Fp.Bit25, Fp.Bit26, Fp.Bit27, Fp.Bit28, Fp.Bit29, Fp.Bit30
 Fp.Bit31, Fp.Bit32, Fp.Bit33, Fp.Bit34, Fp.Bit35, Fp.Bit36, Fp.Bit37, Fp.Bit38, Fp.Bit39, Fp.Bit40
 Fp.Bit41, Fp.Bit42, Fp.Bit43, Fp.Bit44, Fp.Bit45, Fp.Bit46, Fp.Bit47, Fp.Bit48, Fp.Bit49, Fp.Bit50
 Fp.Bit51, Fp.Bit52, Fp.Bit53, Fp.Bit54, Fp.Bit55, Fp.Bit56, Fp.Bit57, Fp.Bit58, Fp.Bit59, Fp.Bit60
 Fp.Bit61, Fp.Bit62, Fp.Bit63, LastFPBits: Fp.Bit64]

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  'ST0ToAscii' translates an FP value, provided in ST0, to an Ascii String, in Decimal
  or Scientific notation (depending on the Exponent size).
  
  A Pointer to the output Buffer is expected, when calling.
  
  Also expected, a Value giving the wanted number of Decimals. This number can be from
  1 to 64. 
  
  The Function does not process any rounding (in case of "0001" or "999" endings) and
  chooses by itself if the Scientific Notation is whishable or not.
  
  'ST0ToAscii' does not make any additional use of the FPU Registers for computations
  and relies entirely, for the String building, on Data Check Tables for the translation,
  which is made by direct addition upon the String (a temporary version of the String is 
  build before formating the final output to the caller's Buffer).
  
  So forth, it is not at all designed for speed competition (10 times slower than similar
  Functions, even worse on Numbers with very high and very low Exponents). As opposed to
  the reverse Function ('AsciiToST0', which, being to be used in Encoders, requires the
  higher possible speed), 'ST0ToAscii', being designed for Outputs, does not require a
  great speed, but requires to preserve the FPU Contents (at any time cost).
  
  Note that, the OS may corrupt the Contents of FPU, when runing the GDI Functions. So, 
  for viewing intermediate results, along FP computations (for viewing sub-results), you
  must not output the partial Results on the screen during your Computation, but, instead,
  you must save all of your temporary results in as many Buffers, and output them all after
  your Computations are over.
  
  Usage:
  
> [MyFp: R$ 123.0885]
> [MyFPString: B$ ? #80]
>
> fld R$MyFp
> call ST0ToAscii MyFpString, 4
;;

Proc ST0ToAscii:
    Argument @Destination, @Decimals
    Local @Exponent, @AbortPosition
    Uses esi, edi, ecx, ebx, edx
    [@String: B$ ? #90] [@Real10: T$ ?]

        mov eax @String | add eax 32 | add eax D@Decimals | mov D@AbortPosition eax

      ; Zeroed output String (20 zeros // 1 Point // XXX++ zeros):
        mov edi @String, eax '0000', ecx 23 | rep stosd | mov B@String+20 '.'

      ; Get a copy of ST0:
        fstp T@Real10 | fld T@Real10

      ; If Zero, Write and exit:
        mov eax D@Real10 | or eax D@Real10+4
        If eax = 0
@Zero:      mov edi D@Destination, D$edi '0', D$edi+1 0 | ExitP
        End_If

      ; If negative, write the Sign and adjust the output Pointer to next Pos:
        mov esi @Real10, edi D@Destination
        movzx ebx W$esi+8 | Test ebx 08000 | jz L0>
            mov B$edi '-' | and ebx (not 08000) | inc D@Destination

      ; Biased signed Exponent in ebx:
L0:     On ebx = 0, jmp @Zero

        mov D@Exponent 0

      ; It seems that the Binary exponent = 07FFF means something like "undefined" (...)
      ; and seems to appear when dividing. OK. But:
      ; It also seems that Binary exponent = 0 may be given for a zeor value (...).

        ;If ebx < 03FFF ; Ex: 0.000123456...
        If ebx < 03FEE ; Ex: 0.000123456...
            ;While ebx < 03FFF ; Guga Note. This is incorrect. (Check the charmap on file C:\masm32\CBLOCKC.DLL)
                               ; It must be smaller then 03FF8. Also, to allow 0,0001 values we must use a test file
                               ; containing 100000 Zero Bytes and one Byte (1) to result in computations of 1/100000.
                               ; This will result us on a value of 03FF5. This allow us to compute 0,00001
                               ; In case we have a percentage of 1e-003 (0.001)
                               ; 03FF1 = 1 e-4 ==> 1/10000 = 0.0001. (A file with 1.000.000)
                               ; To we divide one more time for 10. we need to subtract the value below by 4.
                               ; So, to we allow, 0.00001 = 1e-5, the next value will be 03FF1-3 = 03FEE
                               ; This is the same as a file with 10000000 bytes. But the percentage in ST0, result in
                               ; 10000000 /100 = 100000, that is 1e-5. This is good if we analyse files around 10 Mb.
            While ebx < 03FEE
                fmul R$TenTable+8
                fstp T@Real10 | fld T@Real10 | dec D@Exponent
                movzx ebx W$@Real10+8 | and ebx (not 08000)
                On ebx = 0, jmp @Zero
            End_While

        Else                                            ; Ex: 123.456...
            ;While ebx > (03FFF+29)
            While ebx > (03FEE+29) ;4016
                fdiv R$TenTable+8
                fstp T@Real10 | fld T@Real10
                movzx ebx W$@Real10+8 | and ebx (not 08000) | inc D@Exponent
                On ebx = 07FFF, jmp @Zero
            End_While

        End_If

L1:     ;sub ebx 03FFF | mov eax ebx | shl eax 2
        sub ebx 03FFF | mov eax ebx | shl eax 2

      ; 'ebx' will hold the pointer to the Table of Pointers to Decimal Strings.
      ; For example, if eax = 0 (Exponent 0), ebx points to 'Fp.Bit1' Pointer,
      ; in 'FPBits' Table (4 'sub' because dWords Table):
        mov ebx Fp.Bits | sub ebx eax | sub ebx 4

      ; Parse now the Mantissa (64 Bits). Each Bit is a power of 2 figured in the
      ; [FPBit64 // Fp.Bit64] Strings Table:

L1:     mov ecx 32, edx @Real10, edx D$edx+4            ; (High dWord).
L0:     shr edx 1 | jnc L5>
            lea esi D$ebx+ecx*4
            cmp esi FPBits | jb L5>
            cmp esi LastFPBits | ja L6>>
                mov esi D$esi, edi @String, eax 0
                lodsb | add edi eax | cmp edi D@AbortPosition | ja L6>>
                .While B$esi <> 0
                    On B$edi = '.', inc edi
                    cmp edi D@AbortPosition | ja L5>
                    lodsb | sub al '0' | add B$edi al | mov eax 0
                  ; Left reporting, if needed:
                    While B$edi+eax > '9'
                        sub B$edi+eax 10
                        dec eax | On B$edi+eax = '.', dec eax
                        inc B$edi+eax
                    End_While
                    inc edi
                .End_While
L5:     loop L0<

        mov ecx 32, edx @Real10, edx D$edx              ; (Low dWord).
        add ebx 4+(31*4)
L0:     shr edx 1 | jnc L5>
            lea esi D$ebx+ecx*4
            cmp esi FPBits | jb L5>
            cmp esi LastFPBits | ja L6>
                mov esi D$esi, edi @String, eax 0
                lodsb | add edi eax | cmp edi D@AbortPosition | ja L6>
                .While B$esi <> 0
                    On B$edi = '.', inc edi
                    cmp edi D@AbortPosition | ja L5>
                    lodsb | sub al '0' | add B$edi al | mov eax 0
                  ; Left reporting, if needed:
                    While B$edi+eax > '9'
                        sub B$edi+eax 10
                        dec eax | On B$edi+eax = '.', dec eax
                        inc B$edi+eax
                    End_While
                    inc edi
                .End_While
L5:     loop L0<

L6:   ; Now, format the String. First, cut off zeroed tail:
        mov esi @String | add esi 89
        While B$esi = '0' | dec esi | End_While | mov B$esi+1 0

      ; Search for first significant Char:
        mov esi @String | While B$esi = '0' | inc esi | End_While
        On B$esi = '.' dec esi

      ; Copy to caller Destination:
        mov edi D@Destination | While B$esi <> '.' | movsb | End_While
        mov B$edi 0
      ; Copy the Point and the wanted Decimals, if not End_Of_String:
        ...If B$esi+1 <> 0
            movsb                                       ; Point.
            mov ecx D@Decimals
L1:         lodsb | cmp al 0 | je L2>
                .If D$esi = '9999'
                    If D$esi+4 = '9999'
                        inc al | mov B$esi 0
                    End_If
                .End_If
                stosb | loop L1<                        ; Decimals.

L2:         While B$edi-1 = '0' | dec edi | End_While | mov B$edi 0

        ...Else
            While B$edi-1 = '0' | dec edi | inc D@Exponent | End_While

        ...End_If

        On B$edi-1 = '.', dec edi

        While B$edi-1 = '0' | dec edi | inc D@Exponent | End_While

      ; Write Exponent if Needed:
        mov eax D@Exponent

        If eax <> 0
            test eax 08000_0000 | jnz L1>
                mov D$edi ' e+ ' | jmp L2>
L1:             mov D$edi ' e- ' | neg eax
L2:         add edi 3 | mov ecx 10

            mov dl 0FF | push edx                       ; Push stack end mark
            mov ecx 10
L0:         mov edx 0
            div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2:         pop eax                                     ; Retrieve Backward
            cmp al 0FF | je L9>                         ; Over?
            add al '0' | stosb | jmp L2<                ; Write
        End_If

L9:     mov B$edi 0
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
* 'AsciiToST0' reads the Ascii String expression of a Real Number, and stores it on the
  FPU Stack Top.

* Scientific or Decimal notation.

* 20 or 30% faster than fast Ascii-to-Float Routines. Twice faster than previous RosAsm one.
  
* Uses only one (ST0) FPU Register.

* No 'finit'.
  
* Usage:

>   [SourceString: B$ '12345678.12345', 0]
>
>   call AsciiToST0 SourceString 
>
>   If eax = &NO_ERROR
>       fstp F$MyReal4 // R$MyReal // ...
>   Else_If eax = TOOBIG_INTEGER
>     ; Error Message...
>   Else_If eax = TOOBIG_EXPONENT
>     ; Error Message...
>   End_If

* Eax, on return may hold &NO_ERROR, TOOBIG_INTEGER, or TOOBIG_EXPONENT
;;

[TOOBIG_INTEGER 1    TOOBIG_EXPONENT 2]

[CharAfterFpNumber: ?]
[Tempo1: T$ ?]
[Tempo2: T$ ?]

[TenTable2:  TenDiv0 TenDiv1 TenDiv2 TenDiv3 TenDiv4 TenDiv5
             TenDiv6 TenDiv7 TenDiv8 TenDiv9 TenDiv10 TenDiv11
             TenDiv12 TenDiv13 TenDiv14]

[TenDiv0: F$ 1e+0] [TenDiv1: F$ 1e+1] [TenDiv2: F$ 1e+2] [TenDiv3: F$ 1e+3] [TenDiv4: F$ 1e+4]
[TenDiv5: F$ 1e+5] [TenDiv6: F$ 1e+6] [TenDiv7: F$ 1e+7] [TenDiv8: F$ 1e+8] [TenDiv9: F$ 1e+9]
[TenDiv10: F$ 1e+10] [TenDiv11: F$ 1e+11] [TenDiv12: F$ 1e+12] [TenDiv13: F$ 1e+13] [TenDiv14: F$ 1e+14]

[TenDiv100: F$ 1e+100]
[TenDiv1000: F$ 1e+1000]

[Teste: Q$ 0]

[TenDivisor: F$ 1e+10]

Proc AsciiToST0:
    Argument @String
    Local @Sign, @Digits
    Uses esi, edi, ebx, ecx, edx
    [@Tempo: T$ ?]

        mov esi D@String, D@Sign &FALSE
        While B$esi <= ' ' | inc esi | End_While

        If B$esi = '+'
            inc esi
        Else_If B$esi = '-'
            inc esi | mov D@Sign &TRUE
        End_If

        While B$esi <= ' ' | inc esi | End_While
        While B$esi = '0' | inc esi | End_While

        mov ebx 0, ecx 0, edx 0, eax 0

        .Do
            lodsb
                If al = ' '
                    movzx eax B$esi | inc esi
                Else_If al = '%'
                    movzx eax B$esi | inc esi
                End_If
                cmp al '9' | ja L1>
                cmp al '0' | jb L1>

            lea ebx D$ebx+ebx*4 | lea ebx D$eax+ebx*2-030   ; ebx = ebx*10 + (al)-'0'
            inc ecx             ; ebx is the last bytes. ecx = the amount of integer bytes
       .Loop_Until ecx >= 10


        .Do
            lodsb
                If al = ' '
                    movzx eax B$esi | inc esi
                Else_If al = '%'
                    movzx eax B$esi | inc esi
                End_If
                cmp al '9' | ja L1>
                cmp al '0' | jb L1>

                lea edx D$edx+edx*4 | lea edx D$eax+edx*2-030   ; edx = edx*10 + (al)-'0'
                inc ecx
        .Loop_Until ecx >= 18
                mov eax TOOBIG_INTEGER | ExitP

L1:   ; The Integer part is now in ebx:edx. Write it in ST0:
      ; The Higher part of the integer is stored in ebx (Ex.: 123456789102. the 1st 10 chars 1234567891 in ebx)
      ; The lower Part of the integer is stored in  edx. (Ex.: 102 from the above example. The last chars after the 10th)

        mov D@Digits ecx
        mov eax ebx | or eax edx
        .If eax <> 0
            mov D$Tempo1 ebx | fild D$Tempo1
            If ecx > 10
                sub ecx 10
                mov ecx D$TenTable2+ecx*4 | fmul F$ecx
                mov D$Tempo1 edx | fiadd D$Tempo1
            End_If
        .Else
            fldz
        .End_If
      _____________________________________________________________________________
      ; Now, parse the Decimals, if any, the same way ( also limited to 18 Digits):
      ; Uses the reverse of TenTable. TenTable2 is in reversed dword order, because we are
      ; analysing fracional part multiples of Ten (0.1, 0.01 etc)

        ...If B$esi-1 = '.'
          ; Pop and save the Integer Part, in order to use only 1 Register:

            fstp D$Tempo1
            mov ebx 0, ecx 0, edx 0, eax 0

        .Do
            lodsb
                If al = ' '
                    movzx eax B$esi | inc esi
                Else_If al = '%'
                    movzx eax B$esi | inc esi
                End_If
                cmp al '9' | ja L1>
                cmp al '0' | jb L1>

            lea ebx D$ebx+ebx*4 | lea ebx D$eax+ebx*2-030   ; ebx = ebx*10 + (al)-'0'
            inc ecx             ; ebx is the last bytes. ecx = the amount of integer bytes
       .Loop_Until ecx >= 10


       .Do
            lodsb
                If al = ' '
                    movzx eax B$esi | inc esi
                Else_If al = '%'
                    movzx eax B$esi | inc esi
                End_If
                cmp al '9' | ja L1>
                cmp al '0' | jb L1>

                lea edx D$edx+edx*4 | lea edx D$eax+edx*2-030   ; edx = edx*10 + (al)-'0'
                inc ecx
        .Loop_Until ecx >= 18
            ; Too much: No Error case for Decimals > cut off.

L1:         mov eax ebx | or eax edx
          ; Decimal in ebx:edx, Write it in ST0:
            .If eax <> 0
                mov D$Tempo2 ebx | fild F$Tempo2
                If ecx > 10
                    sub ecx 10
                    mov ecx D$TenTable2+ecx*4 |  fld F$ecx | fld F$TenDiv10
                    fdiv ST2 ST0
                    fld F$Tempo1
                    fadd ST0 ST3
                Else
                    mov ecx D$TenTable2+ecx*4 |  fld F$ecx
                    fdiv ST1 ST0
                    fld F$Tempo1
                    fadd ST0 ST2
                End_If
                ; Guga Note. I have also to change the debug part because for more then 8 digits it is adding an e+008 stuff
            .Else
              ; If Decimal Part = 0, load the previously saved Integer Part:
                fld F$Tempo1

            .End_If
        ...End_If
        ffree ST1
        ffree ST2
        ffree ST3

      ; The Complete User Number is now in ST0.
      _____________________________________________________________
      ; Scientific Notation. (The exponent must be within +4932/-4932).
        While B$esi-1 = ' ' | inc esi | End_While

        mov al B$esi-1 | or al 32

        ...If al = 'e'
            mov edx &FALSE
            If B$esi = '-'
                mov edx &TRUE | inc esi
            Else_If B$esi = '+'
                inc esi
            End_If

          ; Compute the Exponent Value:
            mov ebx 0, eax 0, ecx 0

        .Do
            lodsb
                If al = ' '
                    movzx eax B$esi | inc esi
                Else_If al = '%'
                    movzx eax B$esi | inc esi
                End_If
                cmp al '9' | ja L1>
                cmp al '0' | jb L1>

            lea ebx D$ebx+ebx*4 | lea ebx D$eax+ebx*2-030   ; ebx = ebx*10 + (al)-'0'
            inc ecx             ; ebx is the last bytes. ecx = the amount of integer bytes
       .Loop_Until ecx >= 10



L1:         ; Ebx > 4932 Exit. ebx > 0 do computations, ebx = 0 do nothing.
            ..If ebx > 4932
                mov eax TOOBIG_EXPONENT | ExitP
            ;End_If
            ..Else_If ebx > 0

          ; Negative Exponent, if edx = &TRUE // Exponent in ebx // User Number in ST0.
          ; No additional use of FPU Registers:
            ;.While ebx > 0
                .If ebx > 14;8
                    mov ecx 0
                    mov D$Tempo1 1 | fild D$Tempo1; | fld F$TenDiv10
                    Do
                        fmul F$TenDiv1;TenDiv10
                        inc ecx
                    Loop_Until ecx = ebx ; Error on the debug. If i have a string like 1.25e1001 it will display only
                                         ; 1.25e000
                    ;mov D$Tempo1 ebx | fild D$Tempo1 | fld F$TenDiv10
                    ;TenDiv10
                    fmul ST0 ST1
                .Else
                    mov eax ebx
                    If edx = &TRUE
                        mov ecx D$TenTable2+eax*4 |  fld F$ecx
                        fdivr ST0 ST1
                    Else
                        mov ecx D$TenTable2+eax*4 | fmul F$ecx
                    End_If
                .End_If

                ffree ST1
                ffree ST2

            ..End_If


        ...End_If

        On B@Sign = &TRUE, fchs

        dec esi | mov D$CharAfterFpNumber esi

        mov eax &NO_ERROR
EndP

____________________________________________________________________________________________

; 18 values, not counting '1'.

[ST0TenTable: R$ 1, 10, 100, 1000, 10_000, 100_000, 1000_000, 10_000_000
              100_000_000, 1000_000_000, 10_000_000_000

              100_000_000_000, 1000_000_000_000, 10_000_000_000_000
              100_000_000_000_000, 1000_000_000_000_000
              10_000_000_000_000_000, 100_000_000_000_000_000
              1000_000_000_000_000_000]
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Last Kenny 'Ascii2Float':
 
 [testfloat: B$'.09e+1' 0]
[maxexp: D$100]
Function Ascii2Float:
local @numneg, @decplace, @expneg, @num, @exp
; IN:  esi = pointer to ascii string (terminated by <= space)
; OUT: st0 = floating point value

    mov D@decplace 0
    mov D@exp 0
    mov D@num 0
L1: cmp B$esi '-' | jne L1>
        mov B@numneg 1
        inc esi
L1: xor ecx ecx
    xor edx edx
L0: lodsb | inc edx
    cmp al '.' | je L1>
    cmp al 'e' | je L2>
    cmp al '9' | ja L8>
    cmp al '0' | jb L8>
    cmp al ' ' | jbe L9>
        sub al '0'                  ; convert Decimal to binary:
        lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
        lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10       
    jmp L0<
L1: xor edx edx | jmp L0<
L2: mov D@num ecx
    lodsb | cmp al '-' | sete B@expneg
    mov D@decplace edx
    xor ecx ecx
    jmp L0<
L8: ; Error case
L9: cmp B@decplace 0
    cmove ebx edx
    cmovne ebx D@decplace
    dec ebx
   
    xor edx edx
    cmp D@num 0
    cmove eax ecx ; eax = number
    cmovne eax D@num
    cmovne edx ecx
    mov D@num eax
    mov D@exp edx
   
    fld F$f0.1
    fld F$f10.0
   
    fild D@num
    cmp D@decplace 0 | je L2>
   
L1: fmul st2 | dec ebx | jnz L1<
   
L2: cmp edx 0 | je L9>
    cmp edx D$maxexp | cmove edx D$maxexp
    cmp B@expneg 1 | je L2>
L3:     fmul st1 | dec edx | jnz L3<
        jmp L9>
L2:     fmul st2 | dec edx | jnz L2<

L9: On B@numneg = 1, fchs
    fxch st2 | fcompp
   
    int 3
EndF
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

UsedByTheDebugger:

; From FPU to Ascii
;

;;
; Procedures Originaly written by Tim Roberts.

;[TempoAsciiFpu: B$ 0 #32]; [BCDtempo: T$ ?]

[ten: R$ 10.0    ten7: 1.0e6
 ten_1: T$ 1.0e1,    1.0e2,    1.0e3,    1.0e4,    1.0e5,    1.0e6,    1.0e7,   1.0e8
           1.0e9,    1.0e10,   1.0e11,   1.0e12,   1.0e13,   1.0e14,   1.0e15
 ten_16:   1.0e16,   1.0e32,   1.0e48,   1.0e64,   1.0e80,   1.0e96,   1.0e112, 1.0e128
           1.0e144,  1.0e160,  1.0e176,  1.0e192,  1.0e208,  1.0e224,  1.0e240
 ten_256:  1.0e256,  1.0e512,  1.0e768,  1.0e1024, 1.0e1280, 1.0e1536, 1.0e1792
           1.0e2048, 1.0e2304, 1.0e2560, 1.0e2816, 1.0e3072, 1.0e3328, 1.0e3584, 1.0e3840
           1.0e4096, 1.0e4352, 1.0e4608, 1.0e4864]

[TenMax: T$ 1e+4932]

; changed in 09/02/2019

; Compute the power 10 of a number
; Exponent . The power (x) you want to scale. Ex: 10^x

Proc PowerOf10:
    Arguments @Exponent
    Structure @TempStorage 10, @pRealLimitDis 0
    Uses ecx, edx, edi

    mov eax D@Exponent
    lea edi D@pRealLimitDis
    xor edx edx
    mov ecx eax

    Test_If eax 0_8000_0000
        neg eax
    Test_End

    fld1

    mov dl al | and edx 0f
    If edx > 0
        lea edx D$edx+edx*4 | fld T$ten_1+edx*2-10 | fmulp st1 st0
    End_If

    mov dl al | shr dl 4 | and edx 0F
    If edx > 0
        lea edx D$edx+edx*4 | fld T$ten_16+edx*2-10 | fmulp st1 st0
    End_If

    mov dl ah | and edx 01F
    .If edx > 0
        lea edx D$edx+edx*4 | fld T$ten_256+edx*2-10 | fmulp st1 st0
        ; We need to check for the limits of 1e+4932. Since this is a power of ten, we can't have here more then 1e4932
        ; So we must respect the limit range of 1.189731495080226e+4932  to 1.189731495357232e+4932 (from DisassemblerFloatToUString)
        ; to avoid we have an infinit value to be computed. This happens when we are analysing values like
        ; T$ 1e-4917 or T$ 1e-4916 for example.
        ; The hexadecimal value on such cases is: 07FFF 080000000 00000000

        fstp T$edi

        If_and D$edi+4 = 080000000, W$edi+8 = 07FFF
            fld T$TenMax
        Else
            fld T$edi
        End_If

    .End_If

    Test_If ecx 0_8000_0000
        fdivp st1 st0
    Test_Else
        fmulp st1 st0
    Test_End

EndP
;;

; Computes the power 10 of a number.
; The inputed power (x) must be in ST0. Ex: 10^x
; The returned value is also in ST0
; Created in 10/02/2019
Proc ST0PowerOf10:

    fldl2t
    fmulp ST1 ST0
    fld ST0
    frndint
    fxch ST1
    fsub ST0 ST1
    f2xm1
    fld1
    faddp ST1 ST0
    fscale
    fstp ST1

EndP

; changed in 09/02/2019
Proc FloatToBCD:
    Arguments @Input, @Output
    Uses esi, edi, ecx, eax

    mov esi D@Input
    add esi 9
    mov edi D@Output
    xor eax eax

    ;  The 1st bytes of the BCD Will always be 0. So we will need to bypass them to properly
    ; achieve a better result in case we are dealing with negative exponent values or other non
    ; integer values that may result in more then One zero byte at the start. Example:
    ; Sometimes when we are analysing the number 1, it can have the following starting bytes:
    ; 00999999999999999 . So, the 1st Byte is ignored to the form in edi this: 09999999999.
    ; On this example it is the number 0.999999999999999e-6 that is in fact 1e-6
    If B$esi = 0
        mov ecx 9
        dec esi
    Else
        mov ecx 10
    End_If

    Do
        mov al B$esi | dec esi | rol ax 12 | rol ah 4
        and ax 0F0F
        add ax '00'
        mov W$edi ax
        add edi 2
        dec ecx
    Loop_Until ecx = 0
    mov B$edi 0

EndP


;;
    FloatToUString - Updated in 10/02/2019
    
    This function converts a FPU Number to decimal string (positive or negative) to be displayed on the debugger

    Parameters:
        
        Float80Pointer - A pointer to a variable containing a TenByte (80 bit) value to be converted to decimal string.
        
        DestinationPointer - A buffer to hold the converted string. The size of the buffer must be at least 32 bytes.
                             A maximum of 19 chars (including the dot) will be converted.
                             If the number cannot be converted, the buffer will contain a string representing the proper
                             category of the FPU error, such as: QNAN, SNAN, Infinite, Indefinite.
        
        TruncateBytes - The total amount of bytes to truncate. You can truncate a maximum of 3 numbers
                        on the end of a string. The truncation is to prevent rounding errors of some
                        compilers when tries to convert Floating Point Units.
                        For Terabytes we can discard the last 3 Bytes that are related to a diference in the error mode.
                        But, if you want to maintain accuracy, leave this parameter as 0.
        
        AddSubNormalMsg - A flag to enable append a warning message at the end of the number stored on the buffer at the DestinationPointer,
                          labeling it as a "(Bad)" number (positive or negative) meaning that the number is way too below the limit for
                          the FPU TenByte and is decreasing precision.
                           
                          To append the warning message, set this flag to &TRUE. Otherwise, set it to &FALSE.
                                                      
                          The 80-bit floating point format has a range (including subnormals) from approximately 3.65e-4951 to 1.18e4932.
                          Normal numbers from within the range 3.36210314311209208e-4932 to 1.18e4932) keeps their accuracy.
                          Numbers below that limit are called "SubNormal" (or denormalized) on a range from 3.65e-4951 to 3.362103...e-4932
                           
                          All subnormal numbers decreases their precision as they are going away from the limit of a normal number.
                          It have an approximated amount of 2^63 subnormal numbers that are way too close to zero and decreasing precision.
                           
                          The limit of a normal number is: 3.36210314311209208e-4932 (equivalent to declare it as: "FPULimit: D$ 0, 080000000, W$ 01")
                          
                          

    Return Values:

        The function will return one of the following equates:

        Equate                              Value   Description
        
        SpecialFPU_PosValid                 0       The FPU contains a valid positive number.
        SpecialFPU_NegValid                 1       The FPU contains a valid negative number.
        SpecialFPU_PosSubNormal             2       The FPU produced a positive Subnormal (denormalized) number.
                                                    Although it´s range is outside the range 3.6...e-4932, the number lost it´ precision, but it is still valid
                                                    Ex: 0000 00000000 00000000
                                                        0000 00000000 FFFFFFFF
                                                        0000 00000000 00008000
                                                        0000 00000001 00000000
                                                        0000 FFFFFFFF FFFFFFFF
        SpecialFPU_NegSubNormal             3       The FPU produced a negative Subnormal (denormalized) number.
                                                    Although it´s range is outside the range -3.6...e-4932, the number lost it´ precision, but it is still valid
                                                    Ex: 8000 00000000 00000000 (0) (Negative zero must be considered only as zero)
                                                        8000 00000000 FFFFFFFF (-0.0000000156560127730E-4933)
                                                        8000 01000000 00000000 (-0.2626643080556322880E-4933)
                                                        8000 FFFFFFFF 00000001 (-6.7242062846585856000E-4932)
        SpecialFPU_Zero                     4       The FPU contains a valid zero number
        SpecialFPU_QNAN                     5       QNAN - Quite NAN (Not a number)
        SpecialFPU_SNAN                     6       SNAN - Signaling NAN (Not a number)
        SpecialFPU_NegInf                   7       Negative Infinite
        SpecialFPU_PosInf                   8       Positive Infinite
        SpecialFPU_Indefinite               9       Indefinite

        These 4 equates below are not the official ones from IEEE. They were created to represente the cases when the Integer bit of the TenByte was not
        present by some error on compilers. A tenbyte always should have this bit settled (value = 1). When it is not settled the FPU simply will 
        refuses to process. To handle this lack of category of error we created the 4 ones below.
        The integer bit is the 63th bit of the tenbyte (or 31 of the 2nd dword)
        
        SpecialFPU_SpecialIndefQNan         10      Special INDEFINITE QNAN
        SpecialFPU_SpecialIndefSNan         11      Special INDEFINITE SNAN
        SpecialFPU_SpecialIndefNegInfinite  12      Special INDEFINITE Negative Infinite
        SpecialFPU_SpecialIndefPosInfinite  13      Special INDEFINITE Positive Infinite


    Remark:
            Used on ComputeImmediateExpression , toFloat , toDouble , toExtended , toFloats , MouseHintDrawWindow

    See also: DisassemblerFloatToUString WriteFP10 ComputeImmediateExpression RealTenFPUNumberCategory
    
;;

[Float_Ten: R$ 10]

Proc FloatToUString:
    Arguments @Float80Pointer, @DestinationPointer, @TruncateBytes, @AddSubNormalMsg
    Local @ExponentSize, @ControlWord, @ExponentHandle, @FPURoundConst, @FPUStatusHandle, @tempdw, @IsNegative, @extra10x, @FPUMode
    Structure @TmpStringBuff 42, @pTempoAsciiFpuDis 0, @pBCDtempoDis 32
    Uses esi, edi, edx, ecx, ebx

    ; @FPUStatusHandle = 0 Default
    ; @FPUStatusHandle = 1 Increased. Positive exponent value
    ; @FPUStatusHandle = 2 Decreased. Negative exponent value

    mov edi D@TmpStringBuff, ecx 42, al 0 | REP STOSB ;call 'RosMem.ZeroMemory'
    mov D@ExponentSize 0, D@FPUStatusHandle 0, D@IsNegative &FALSE, D@extra10x 0 D@FPUMode SpecialFPU_PosValid
    mov edi D@DestinationPointer, eax D@Float80Pointer
    mov ebx eax ; D@Float80Pointer

    .If_and D$eax = 0, D$eax+4 = 0
        If_Or W$eax+8 = 0, W$eax+8 = 08000
            mov W$eax+8 0
            mov B$edi '0', B$edi+1 0
            mov eax D@FPUMode
            ExitP
        End_If
    .End_If

    Test_If B$ebx+9 0_80
        call RealTenFPUNumberCategory ebx
        mov D@FPUMode eax
        If eax >= SpecialFPU_QNAN ; do we have any special FPU being used ? Yes, display the proper message and exit
            mov ebx eax
            call WriteFPUErrorMsg eax, edi
            mov eax ebx
            ExitP
        End_If
        mov D@IsNegative &TRUE | mov B$edi '-' | inc edi
        xor B$ebx+9 0_80
    Test_Else
        call RealTenFPUNumberCategory eax
        mov D@FPUMode eax
        If eax >= SpecialFPU_QNAN ; do we have any special FPU being used ? Yes, display the proper message and exit
            mov ebx eax
            call WriteFPUErrorMsg eax, edi
            mov eax ebx
            ExitP
        End_If
    Test_End

    finit | fclex | fstcw W@ControlWord
    fld T$ebx
    ; extract the exponent. 1e4933
    call GetExponentFromST0 &FPU_EXCEPTION_INVALIDOPERATION__&FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_ZERODIV__&FPU_EXCEPTION_OVERFLOW__&FPU_EXCEPTION_UNDERFLOW__&FPU_EXCEPTION_PRECISION__&FPU_PRECISION_64BITS
    mov D@ExponentSize eax
    ffree ST0
    .If D@ExponentSize < FPU_ROUND

        fld T$ebx
        fld st0 | frndint | fcomp st1 | fstsw ax

        Test_If ax &FPU_EXCEPTION_STACKFAULT
            lea ecx D@pBCDtempoDis
            fbstp T$ecx     ; ->TBYTE containing the packed digits
            fwait
            lea eax D@pTempoAsciiFpuDis
            lea ecx D@pBCDtempoDis
            call FloatToBCD ecx, eax
            mov eax FPU_MAXDIGITS+1 | mov ecx D@ExponentSize | sub eax ecx | inc ecx
            lea esi D@pTempoAsciiFpuDis | add esi eax

            If B$esi = '0'
                inc esi | dec ecx
            End_If

            mov eax 0
            rep movsb | jmp L9>>
        Test_End

        ffree ST0

    .End_If

L1:      ; Necessary for FPU 80 Bits. If it is 0, the correct is only 0 and not 0.e+XXXXX.
        If D@ExponentSize = 080000000
            mov D@ExponentSize 0
        Else_If D@ExponentSize = 0
            mov D@ExponentSize 0
        End_If

        ; multiply the number by the power of 10 to generate required integer and store it as BCD

        ; We need to extract here all the exponents of a given number and multiply the result by the power of FPU_MAXDIGITS+1 (1e17)
        ; So, if our number is 4.256879e9, the result must be 4.256879e17. If we have 3e-2 the result is 3e17.
        ; If we have 0.1 the result is 1e17 and so on.
        ; If we have as a result a power of 1e16. It means that we need to decrease the iExp by 1, because the original
        ; exponential value is wrong.
        ; This result will be stored in ST0

        ..If D@ExponentSize <s 0
            mov eax D@ExponentSize | neg eax | add eax FPU_MAXDIGITS ; always add MaxDigits-1
            mov edx D@ExponentSize | lea edx D$eax+edx
            .If eax > 4932
                mov edx eax | sub edx 4932 | sub edx FPU_MAXDIGITS
                mov D@extra10x edx | add D@extra10x FPU_MAXDIGITS
                mov eax 4932
                If D@extra10x >= FPU_MAXDIGITS
                    inc D@ExponentSize
                End_If
            .Else_If edx >= FPU_MAXDIGITS
                inc D@ExponentSize
            .End_If
        ..Else_If D@ExponentSize > 0
            mov eax FPU_MAXDIGITS+1 | sub eax D@ExponentSize
            ;If D@ExponentSize > FPU_MAXDIGITS+1
             ;   mov eax eax
            ;End_if
        ..Else ; Exponent size = 0
            mov eax FPU_MAXDIGITS+1
        ..End_If
        mov D@tempdw eax

        fild D@tempdw
        call ST0PowerOf10
        fld T$ebx | fmulp ST0 ST1

        If D@extra10x > 0
            ; Calculate the exponencial value of the extrabytes
            fild F@extra10x
            call ST0PowerOf10
            fmulp ST0 ST1 ; and multiply it to we get XXe17 or xxe16
        End_If

        ; now we must get the power of FPU_MAXDIGITS+1. In this case, we will get the value 1e17.
        mov D@FPURoundConst FPU_MAXDIGITS+1

        ; Calculate the exponencial value accordying to the value in @FPURoundConst
        fild F@FPURoundConst
        call ST0PowerOf10
        fxch ; exchange the values ST0 = 4.294967e16 ST1 = 1e17

        ; let's see if ST0 is smaller then ST1
;        Fpu_If Q$ten < T@RealDivision
            fcom
            fstsw ax    ; retrieve exception flags from FPU
            fwait
                mov D@FPUStatusHandle eax ; save exception flags
            sahf | jnb L0> | fmul R$Float_Ten | dec D@ExponentSize | L0:
;        Fpu_End_If

        lea ecx D@pBCDtempoDis
        fbstp T$ecx             ; ->TBYTE containing the packed digits
        fwait

        lea ecx D@pBCDtempoDis
        lea eax D@pTempoAsciiFpuDis
        call FloatToBCD ecx, eax
        ffree ST0

        ; Adjust the Exponent when some Exceptions occurs and try to Fix whenever is possible the rounding numbers
        lea eax D@pTempoAsciiFpuDis
        call FPURoundFix eax, D@FPUStatusHandle D@ExponentSize, D@TruncateBytes
        mov D@ExponentSize eax

        lea esi D@pTempoAsciiFpuDis | mov ecx D@ExponentSize
        inc ecx

        ..If_And ecx <= FPU_ROUND, ecx > 0
            mov eax 0

            While B$esi <= ' ' | inc esi | End_While
            While B$esi = '0'
                inc esi
                ; It may happens that on rare cases where we had an ecx = 0-1, we have only '0' on esi.
                ; So while we are cleaning it, if all is '0', we set edi to one single '0', to avoid we have a
                ; Empty String.
                If B$esi = 0
                    mov B$edi '0' | inc edi
                    jmp L9>>
                End_If
            End_While

            Do
                On B$esi = 0, Jmp L1>
                movsb
                dec ecx
            Loop_Until ecx = 0
            L1:

            If B$esi <> 0
                mov B$edi '.' | inc edi
                mov ecx FPU_MAXDIGITS+2

            Do
                On B$esi = 0, Jmp L1>
                movsb
                dec ecx
            Loop_Until ecx = 0
            L1:

                While B$edi-1 = '0' | dec edi | End_While
                On B$edi-1 = '.', dec edi

            End_If

        ..Else
            While B$esi <= ' ' | inc esi | End_While
            While B$esi = '0' | inc esi | End_While

            .If B$esi <> 0
                movsb | mov B$edi '.' | inc edi
                mov ecx FPU_MAXDIGITS+2

                Do
                    On B$esi = 0, Jmp L1>
                    movsb
                    dec ecx
                Loop_Until ecx = 0
                L1:

                ; Clean last Zeros at the end of the Number String.
                While B$edi-1 = '0' | dec edi | End_While

                If B$edi-1 = '.'
                    dec edi
                End_If

                mov B$edi 'e' | mov eax D@ExponentSize
                mov B$edi+1 '+'

                Test_If eax 0_8000_0000
                    neg eax | mov B$edi+1 '-'
                Test_End

                inc edi | inc edi

                push edi
                push ecx
                push esi
                push eax
                    mov D@ExponentHandle eax
                    lea esi D@ExponentHandle
                    mov ecx 4
                    call toUDword
                    mov esi edi, edi DecimalBuffer
                    Do | movsb | LoopUntil B$esi-1 = 0
                pop eax
                pop esi
                pop ecx
                pop edi
                call SimpleStringCopy DecimalBuffer, edi
                add edi eax

            .Else
                mov B$edi '0' | inc edi
            .End_If

        ..End_If

        ; For developers only:
        ; Uncomment these function if you want to analyse the Exceptions modes of the FPU Data.

            ;call TestingFPUExceptions D@FPUStatusHandle ; Control Word exceptions
            ;call TestingFPUStatusRegister D@FPUStatusHandle ; Status Registers envolved on the operation

L9:     mov B$edi 0 | fldcw W@ControlWord | fwait

    If D@IsNegative = &TRUE
        mov eax D@Float80Pointer | xor B$eax+9 0_80
    End_If

    .If D@AddSubNormalMsg = &TRUE
        If_Or D@FPUMode = SpecialFPU_PosSubNormal, D@FPUMode = SpecialFPU_NegSubNormal
            call SimpleStringCopy {B$ " (Bad)", 0}, edi
        End_If
    .End_If
    mov eax D@FPUMode

EndP

;;
    This function will return the exponent of a power of 10 on a TenByte data
;;
Proc GetExponentFromST0:
    Arguments @CWFlag
    Local @OldControlWord, @truncw, @ExponentSize

    ; log10(x)
    fldlg2  ; log10(2)
    fld ST1 ; copy Src
    fabs    ; insures a positive value
    fyl2x   ;->[log2(Src)]*[log10(2)] = log10(Src)

    fstcw W@OldControlWord
    wait
    movzx eax W@OldControlWord
    or eax D@CWFlag ; code it for The necessary Flags. &FPU_ROUNDINGMODE_TRUNCATE, &FPU_EXCEPTION_INVALIDOPERATION, &FPU_EXCEPTION_DENORMALIZED, &FPU_EXCEPTION_ZERODIV, &FPU_EXCEPTION_OVERFLOW, &FPU_EXCEPTION_UNDERFLOW, &FPU_EXCEPTION_PRECISION, &FPU_PRECISION_64BITS etc
    mov W@truncw ax
    fldcw W@truncw   ; insure rounding code of FPU to the ones on our flag
    fist D@ExponentSize     ; store characteristic of logarithm
    fldcw W@OldControlWord  ; load back the former control word

    ftst                    ; test logarithm for its sign
    fstsw ax                ; get result
    wait
    sahf                    ; transfer to CPU flags
    sbb D@ExponentSize 0    ; decrement ExponentSize if log is negative
    fstp ST0                ; get rid of the logarithm

    mov eax D@ExponentSize

EndP

_______________________________________________________________________________________________

;;
FPU Control Registers

The FPU Control Word. Accordying to The FPU Control Register in B_U_Asm, we need to perfectly set the Control
word register in order to we gain precision of the math operation envolved.
To do that i (guga) built a serie of new equates related to the Bit settings of the Control Registers.
All of the equates originally uses a shl operation, but to make our life easier i built the equates based
on the bits they are settled. Like:

Exception Masks - Uses Bits 0 to 5

FPU_EXCEPTION_INVALIDOPERATION 01 ; Invalid Operation. Value: 01. Bit0 used. (Value: 1 shl 0 = 0)
FPU_EXCEPTION_DENORMALIZED 02 ; Denormalized. Value: 01. Bit1 used. (Value: 1 shl 1 = 02)
FPU_EXCEPTION_ZERODIV 04 ; Zero divide. Value: 01. Bit2 used. (Value: 1 shl 2 = 04)
FPU_EXCEPTION_OVERFLOW 08 ; Overflow. Value: 01. Bit3 used. (Value: 1 shl 3 = 08)
FPU_EXCEPTION_UNDERFLOW 010 ; Underflow. Value: 01. Bit4 used. (Value: 1 shl 4 = 010)
FPU_EXCEPTION_PRECISION 020 ; Precision. Value: 01. Bit5 used. (Value: 1 shl 05 = 020)

; Reserved - Bits 6 to 7
FPU_RESERVEDBIT6 040 ; Reserved Bit6. Value: 01. Bit6 reserved. (Value: 1 shl 6) = 040
FPU_RESERVEDBIT7 080 ; Reserved Bit7. Value: 01. Bit7 reserved. (Value: 1 shl 7) = 080

; Precision Control - Bits 8 to 9
FPU_PRECISION_24BITS 0 ; 24 Bits. Value: 0. Bit8 used. (Value: 0 shl 08 = 0)
FPU_PRECISION_RESERVED 0100 ; Reserved. Value: 01. Bit8 used. (Value: 1 shl 08 = 0100)
FPU_PRECISION_53BITS 0200 ; 53 Bits. Value: 02 (0000_0010). Bit9 used. (Value: 2 shl 08 = 0200)
FPU_PRECISION_64BITS 0300 ; 64 Bits. Value: 03 (0000_0011). Bit9 used. (Value: 3 shl 08 = 0300)

; Rounding Mode - Bits 10 to 11
FPU_ROUNDINGMODE_NEAREST_EVEN 0 ; Nearest or even. Value: 0. Bit10 used. (Value: 0 shl 10 = 0)
FPU_ROUNDINGMODE_DOWN 0400 ; Down. Value: 01. Bit10 used. (Value: 1 shl 10 = 0400)
FPU_ROUNDINGMODE_UP 0800 ; Up. Value: 02 (0000_0010). Bit10 used. (Value: 2 shl 10 = 0800)
FPU_ROUNDINGMODE_TRUNCATE 0C00 ; Truncate. Value: 03 (0000_0011). Bit10 used. (Value: 3 shl 10 = 0C00)

; Reserved - Bits 12 to 15
FPU_RESERVEDBIT12 01000 ; Reserved Bit12. Value: 01. Bit12 reserved. (Value: 1 shl 12 = 01000)
FPU_RESERVEDBIT13 02000 ; Reserved Bit13. Value: 01. Bit13 reserved. (Value: 1 shl 13 = 02000)
FPU_RESERVEDBIT14 04000 ; Reserved Bit14. Value: 01. Bit14 reserved. (Value: 1 shl 14 = 04000)
FPU_RESERVEDBIT15 08000 ; Reserved Bit15. Value: 01. Bit15 reserved. (Value: 1 shl 15 = 08000)

The control word commonly uses a combination of the above equates. Like in this functin where we had originally:

mov W@MyControlWord 027F | fldcw W@MyControlWord

That is:

mov W@MyControlWord &FPU_EXCEPTION_INVALIDOPERATION__&FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_ZERODIV__&FPU_EXCEPTION_OVERFLOW__&FPU_EXCEPTION_UNDERFLOW__&FPU_EXCEPTION_PRECISION__&FPU_RESERVEDBIT6__&FPU_PRECISION_53BITS
fldcw W@MyControlWord

So, the MyControlWord uses the following equates:
    &FPU_EXCEPTION_INVALIDOPERATION
    &FPU_EXCEPTION_DENORMALIZED
    &FPU_EXCEPTION_ZERODIV
    &FPU_EXCEPTION_OVERFLOW
    &FPU_EXCEPTION_UNDERFLOW
    &FPU_EXCEPTION_PRECISION
    &FPU_RESERVEDBIT6 ; This makes absolutelly no difference at all. It is not used by the processor. (PIII)
    &FPU_PRECISION_53BITS
;;

;[SpecialFPU: ?]

[FPU_ROUND 7]
[FPU_MAXDIGITS 16]

;;
    DisassemblerFloatToUString
    
    This function converts a FPU Number to decimal string. A total of 17 digits can be converted

    Parameters:
        Float80Pointer - A pointer to a variable containing a TenByte (80 bit) value

        DestinationPointer - A buffer to hold the converted string. The size of the buffer
                             must be 32 bytes.

        TruncateBytes - The total amount of bytes to truncate. You can truncate a maximum of 3 numbers
                        on the end of a string. The truncation is to prevent rounding errors of some
                        compilers when tries to convert Floating Point Units.
                        For Terabytes we can discard the last 3 Bytes that are related to a diference in the error mode.
                        But, if you want to maintain accuracy, leave this parameter as 0.

    Return Values:
        The function will return one of the following equates:

        Equate                          Value   Description
        
        DisSpecialFPU_ValidNormal         0       The FPU contains a valid positive or negative number.
        DisSpecialFPU_ValidSubNormal      1       The FPU contains a positive or negative Subnormal (denormalized) number. Although it´s range is outside the range 3.6...e-4932, the number lost it´s precision, but it is still valid
        DisSpecialFPU_Invalid             2       The FPU contains a Invalid number: QNAN, SNAN, Infinite, Indefinite etc. But even on failure cases, the function will try to fix it
            

    See also: FloatToUString WriteFP10 ComputeImmediateExpression

;;

Proc DisassemblerFloatToUString:
    Arguments @Float80Pointer, @DestinationPointer, @TruncateBytes
    Local @ExponentSize, @ControlWord, @ExponentHandle, @FPURoundConst, @FPUStatusHandle, @IsNegative, @tempdw, @extra10x, @FPUMode
    Structure @TmpStringBuff 42, @pTempoAsciiFpuDis 0, @pBCDtempoDis 32
    Uses esi, edi, edx, ecx, ebx

    ; @FPUStatusHandle = 0 Default
    ; @FPUStatusHandle = 1 Increased. Positive exponent value
    ; @FPUStatusHandle = 2 Decreased. Negative exponent value

    mov edi D@TmpStringBuff, ecx 42, al 0 | REP STOSB ;call 'RosMem.ZeroMemory' D@TmpStringBuff, 42
    mov D@ExponentSize 0, D@FPUStatusHandle 0, D@IsNegative &FALSE, D@extra10x 0, D@FPUMode SpecialFPU_PosValid
    mov edi D@DestinationPointer, eax D@Float80Pointer
    mov ebx eax


    .If_and D$eax = 0, D$eax+4 = 0
        If_Or W$eax+8 = 0, W$eax+8 = 08000
            mov W$eax+8 0
            mov B$edi '0', B$edi+1 0
            mov eax D@FPUMode
            ExitP
        End_If
    .End_If

    Test_If B$eax+9 0_80
        xor B$eax+9 0_80 | mov D@IsNegative &TRUE | mov B$edi '-' | inc edi
    Test_End

    ; This function will fix eventual FPU errors for the disasembler. It will check for NAN, Indefinite, Infinite
    ; SNAN, QNAN, etc and will fix to the proper value.
    call FPU10DisasmFix eax
    mov D@FPUMode eax

    finit | fclex | fstcw W@ControlWord
    fld T$ebx
    ; extract the exponent. 1e4933
    call GetExponentFromST0 &FPU_EXCEPTION_INVALIDOPERATION__&FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_ZERODIV__&FPU_EXCEPTION_OVERFLOW__&FPU_EXCEPTION_UNDERFLOW__&FPU_EXCEPTION_PRECISION__&FPU_PRECISION_64BITS
    mov D@ExponentSize eax
    ffree ST0

        .If D@ExponentSize < FPU_ROUND
            fld T$ebx
            fld st0 | frndint | fcomp st1 | fstsw ax
            Test_If ax &FPU_EXCEPTION_STACKFAULT

                lea ecx D@pBCDtempoDis
                fbstp T$ecx     ; ->TBYTE containing the packed digits
                fwait

                lea eax D@pTempoAsciiFpuDis
                lea ecx D@pBCDtempoDis
                call FloatToBCD ecx, eax
                mov eax FPU_MAXDIGITS+1 | mov ecx D@ExponentSize | sub eax ecx | inc ecx
                lea esi D@pTempoAsciiFpuDis | add esi eax

                If B$esi = '0'
                    inc esi | dec ecx
                End_If

                mov eax 0
                rep movsb | jmp L9>>
            Test_End
            ffree ST0
        .End_If

L1:      ; Necessary for FPU 80 Bits. If it is 0, the correct is only 0 and not 0.e+XXXXX.
        If D@ExponentSize = 080000000
            mov D@ExponentSize 0
        Else_If D@ExponentSize = 0
            mov D@ExponentSize 0
        End_If

        ; We need to extract here all the exponents of a given number and multiply the result by the power of FPU_MAXDIGITS+1 (1e17)
        ; So, if our number is 4.256879e9, the result must be 4.256879e17. If we have 3e-2 the result is 3e17.
        ; If we have 0.1 the result is 1e17 and so on.
        ; If we have as a result a power of 1e16. It means that we need to decrease the iExp by 1, because the original
        ; exponential value is wrong.
        ; This result will be stored in ST0


        ..If D@ExponentSize <s 0
            mov eax D@ExponentSize | neg eax | add eax FPU_MAXDIGITS ; always add MaxDigits-1
            mov edx D@ExponentSize | lea edx D$eax+edx
            .If eax > 4932
                mov edx eax | sub edx 4932 | sub edx FPU_MAXDIGITS

                mov D@extra10x edx | add D@extra10x FPU_MAXDIGITS
                mov eax 4932
                If D@extra10x >= FPU_MAXDIGITS
                    inc D@ExponentSize
                End_If
            .Else_If edx >= FPU_MAXDIGITS
                inc D@ExponentSize
            .End_If
        ..Else_If D@ExponentSize > 0
            mov eax FPU_MAXDIGITS+1 | sub eax D@ExponentSize
            ;If D@ExponentSize > FPU_MAXDIGITS+1
             ;   mov eax eax
            ;End_if
        ..Else ; Exponent size = 0
            mov eax FPU_MAXDIGITS+1
        ..End_If
        mov D@tempdw eax

        fild D@tempdw
        call ST0PowerOf10
        fld T$ebx | fmulp ST0 ST1

        If D@extra10x > 0
            ; Calculate the exponencial value of the extrabytes
            fild F@extra10x
            call ST0PowerOf10
            fmulp ST0 ST1 ; and multiply it to we get XXe17 or xxe16
        End_If

        ; now we must get the power of FPU_MAXDIGITS+1. In this case, we will get the value 1e17.
        mov D@FPURoundConst FPU_MAXDIGITS+1

        ; Calculate the exponencial value accordying to the value in @iExp
        fild F@FPURoundConst
        call ST0PowerOf10
        fxch ; exchange the values ST0 = 4.294967e16 ST1 = 1e17

        ; let's see if ST0 is smaller then ST1
;        Fpu_If Q$ten < T@RealDivision
            fcom
            fstsw ax    ; retrieve exception flags from FPU
            fwait
                mov D@FPUStatusHandle eax ; save exception flags
            sahf | jnb L0> | fmul R$Float_Ten | dec D@ExponentSize | L0:
;        Fpu_End_If

        lea ecx D@pBCDtempoDis
        fbstp T$ecx     ; ->TBYTE containing the packed digits
        fwait

        lea ecx D@pBCDtempoDis
        lea eax D@pTempoAsciiFpuDis
        call FloatToBCD ecx, eax
        ffree ST0

        ; Adjust the Exponent when some Exceptions occurs and try to Fix whenever is possible the rounding numbers
        lea eax D@pTempoAsciiFpuDis
        call FPURoundFix eax, D@FPUStatusHandle, D@ExponentSize, D@TruncateBytes
        mov D@ExponentSize eax

        lea esi D@pTempoAsciiFpuDis | mov ecx D@ExponentSize
        inc ecx

        ..If_And ecx <= FPU_ROUND, ecx > 0
            mov eax 0

            While B$esi <= ' ' | inc esi | End_While
            While B$esi = '0'
                inc esi
                ; It may happens that on rare cases where we had an ecx = 0-1, we have only '0' on esi.
                ; So while we are cleaning it, if all is '0', we set edi to one single '0', to avoid we have a
                ; Empty String.
                If B$esi = 0
                    mov B$edi '0' | inc edi
                    jmp L9>>
                End_If
            End_While

            Do
                On B$esi = 0, Jmp L1>
                movsb
                dec ecx
            Loop_Until ecx = 0
            L1:

            If B$esi <> 0
                mov B$edi '.' | inc edi
                mov ecx FPU_MAXDIGITS+2

            Do
                On B$esi = 0, Jmp L1>
                movsb
                dec ecx
            Loop_Until ecx = 0
            L1:

                While B$edi-1 = '0' | dec edi | End_While
                On B$edi-1 = '.', dec edi

            End_If

        ..Else
            While B$esi <= ' ' | inc esi | End_While
            While B$esi = '0' | inc esi | End_While

            .If B$esi <> 0
                movsb | mov B$edi '.' | inc edi
                mov ecx FPU_MAXDIGITS+2

                Do
                    On B$esi = 0, Jmp L1>
                    movsb
                    dec ecx
                Loop_Until ecx = 0
                L1:

                ; Clean last Zeros at the end of the Number String.
                While B$edi-1 = '0' | dec edi | End_While

                If B$edi-1 = '.'
                    dec edi
                End_If


                mov B$edi 'e' | mov eax D@ExponentSize
                mov B$edi+1 '+'

                Test_If eax 0_8000_0000
                    neg eax | mov B$edi+1 '-'
                Test_End

                inc edi | inc edi

                push edi
                push ecx
                push esi
                push eax
                    mov D@ExponentHandle eax
                    lea esi D@ExponentHandle
                    mov ecx 4
                    call toUDword
                    mov esi edi, edi DecimalBuffer
                    Do | movsb | LoopUntil B$esi-1 = 0
                pop eax
                pop esi
                pop ecx
                pop edi
                ;push esi | ZCopy DecimalBuffer | pop esi
                call SimpleStringCopy DecimalBuffer, edi
                add edi eax
            .Else
                mov B$edi '0' | inc edi
            .End_If

        ..End_If

        ; For developers only:
        ; Uncomment these function if you want to analyse the Exceptions modes of the FPU Data.

            ;call TestingFPUExceptions D@FPUStatusHandle ; Control Word exceptions
            ;call TestingFPUStatusRegister D@FPUStatusHandle ; Status Registers envolved on the operation

L9:     mov B$edi 0 | fldcw W@ControlWord | fwait

    If D@IsNegative = &TRUE
        mov eax D@Float80Pointer | xor B$eax+9 0_80
    End_If

    mov eax D@FPUMode

EndP


_________________________________________________________________________

; Updated 09/02/2019

Proc FPURoundFix:
    Arguments @pTmpFPUString, @Flag, @iExp, @TruncateBytes
    Local @CountChar, @Truncating
    Uses esi, edi, ecx

    mov D@CountChar 0
    mov esi D@pTmpFPUString
    mov edi esi

    While B$esi = '0' | inc esi | End_While
    On B$esi = 0, ExitP

    mov ecx 32

    Do
        On B$esi = 0, jmp L1>
        inc D@CountChar
        inc esi
        dec ecx
    Loop_Until ecx <= 0

   L1:

    mov eax D@TruncateBytes
    If eax > 3
        mov eax 3
    End_if
    inc eax
    mov D@Truncating eax

    mov esi edi
    mov ecx D@CountChar

    ; 1st Locate all exceptions, except &FPU_EXCEPTION_PRECISION
    ;.Test_If_And D@Flag &FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_OVERFLOW
    ;.Test_If_And D@Flag &FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_OVERFLOW__&FPU_EXCEPTION_UNDERFLOW
    .Test_If_And D@Flag &FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_UNDERFLOW
        dec D@iExp
        jmp L0>
    .Test_Else_If_And D@Flag &FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_OVERFLOW
        jmp L0>
    .Test_Else_If_And D@Flag &FPU_EXCEPTION_OVERFLOW

   L0:
        ; All of such cases we need to increase iexp
        inc D@iExp
        ; 1st Locate all nearest "10" with positive or negative exponentials. Like:
        ; 9.9999999999999981e-4917 that have a value of &FPU_EXCEPTION_OVERFLOW
        ; All of such cases we don't need to do anything because we already increased iExp.

        sub ecx D@Truncating ; for Terabyte we discard the last 3 Bytes that are related to a diference in the error mode
        ;sub ecx 4 ; for Terabyte we discard the last 3 Bytes that are related to a diference in the error mode
        .If B$esi = '9'
            inc esi ; go to the next char
            Do
                If B$esi <> '9'
                    jmp L1>
                End_If
                inc esi
                dec ecx
            Loop_Until ecx <= 0

            mov D$edi '1000', D$edi+4 '0000', D$edi+8 '0000', D$edi+12 '0000', W$edi+16 '00', B$edi+18 0
            L1:

            ; 2nd Locate all nearest "1" with positive or negative exponentials. Like:
            ; 1.0000000000000001e-859 that have a value of &FPU_EXCEPTION_PRECISION
            ; 5.0000000000000011e-4909 that have a value of &FPU_EXCEPTION_PRECISION
            ; 1.0000000000000001e-640 that have a value of &FPU_EXCEPTION_PRECISION
            ; All of such cases we don't need to do anything because we already increased iExp.

        .Else_If_And B$esi >= '1', B$esi <= '9'

            inc esi ; go to the next char
            Do
                If B$esi <> '0'
                    jmp L1>
                End_If
                inc esi
                dec ecx
            Loop_Until ecx <= 0

            mov W$edi+16 '00', B$edi+18 0
            L1:

        .End_If

    .Test_Else ; Only the ones with &FPU_EXCEPTION_PRECISION alone or with  &FPU_PRECISION_64BITS, for example.

            ; 1st Locate all nearest "10" with positive or negative exponentials. Like:
            ; 9.9999999999999999e-1 that have a value of &FPU_EXCEPTION_PRECISION  &FPU_PRECISION_64BITS
            ; 9.9999999999999999e-5 that have a value of &FPU_EXCEPTION_PRECISION  &FPU_PRECISION_64BITS
            ; 9.99999999999999888e-1121 that have a value of &FPU_EXCEPTION_PRECISION  &FPU_PRECISION_64BITS
            ; All of such cases we need to increase iexp

        sub ecx D@Truncating ; for Terabyte we discard the last 3 Bytes that are related to a diference in the error mode
        ;sub ecx 4 ; for Terabyte
        .If B$esi = '9'
            inc esi ; go to the next char
            Do
                If B$esi <> '9'
                    jmp L1>
                End_If
                inc esi
                dec ecx
            Loop_Until ecx <= 0

            mov D$edi '1000', D$edi+4 '0000', D$edi+8 '0000', D$edi+12 '0000', W$edi+16 '00', B$edi+18 0
            inc D@iExp
            L1:

            ; 2nd Locate all nearest "1" with positive or negative exponentials. Like:
            ; 1.0000000000000001e-859 that have a value of &FPU_EXCEPTION_PRECISION
            ; 5.0000000000000011e-4909 that have a value of &FPU_EXCEPTION_PRECISION
            ; 1.0000000000000001e-640 that have a value of &FPU_EXCEPTION_PRECISION
            ; All of such cases we don't need to do anything.

        .Else_If_And B$esi >= '1', B$esi <= '9'

            inc esi ; go to the next char
            Do
                If B$esi <> '0'
                    jmp L1>
                End_If
                inc esi
                dec ecx
            Loop_Until ecx <= 0

            mov W$edi+16 '00', B$edi+18 0
            L1:

        .End_If

    .Test_End

    mov eax D@iExp

EndP

_________________________________________________________________________

; see RealTenFPUNumberCategory
; Fixed in 10/02/2019

[DisSpecialFPU_ValidNormal 0] ; The FPU contains a valid positive or negative result
[DisSpecialFPU_ValidSubNormal 1] ; The FPU produced a positive or negative Subnormal (denormalized) number. So, although it´ range is outside the range 3.6...e-4932, the number lost it´ precision, but it is still valid
[DisSpecialFPU_Invalid 2] ; The FPU number is invalid. QNAN, SNAN, Infinite, Indefinite

Proc FPU10DisasmFix:
    Arguments @Float80Pointer
    Local @FPUErrorMode
    Uses ebx

    mov D@FPUErrorMode DisSpecialFPU_ValidNormal
    mov ebx D@Float80Pointer

    ; Based on RealTenFPUNumberCategory function
    ; Note: The denormalized values do not need to be fixed on this function

    ..If_And W$ebx+8 = 0, D$ebx+4 >= 0 ; This is denormalized, but it is possible. (Valid Positive SubNormal)

        mov D@FPUErrorMode DisSpecialFPU_ValidSubNormal

    ..Else_If_And W$ebx+8 > 0, W$ebx+8 < 07FFF; This is ok only if the fraction Dword is bigger or equal to 080000000
        .If D$ebx+4 < 080000000
        ; On this error, we need to check the next good value. Ex.: We have the number:
        ; 07ED 13900F00 00000000
        ; This will generate a NAN category number. To fix we need to check the immediate next good value, that is:
        ; 07ED 80000000 00000000 (1.0361967820008025600E-4321)
        ; Of course, that we could also look for the previous imemdiatelly good value that is:
        ; 07EC FFFFFFFF FFFFFFFF (1.0361967820008025600E-4321). So the values are the same and we can assume that
        ; our NAN is, in fact, 1.0361967820008025600E-4321 (07ED 80000000 00000000). For the disassembler point
        ; of view this is logical, because the values that preceed and forward the "GAP" (The NAN) are exactly the same
        ; Like on the following diagram:
        ;    Previous Good Value               NAN - Not calculated             Next Good Value
        ; 07EC FFFFFFFF FFFFFFFF        -->   07ED 13900F00 00000000   -->  07ED 80000000 00000000
        ; (1.0361967820008025600E-4321)                                    (1.0361967820008025600E-4321)
        ; Between the last and next good value we have a GAP of 80000000 00000001 unused bytes. Like:
        ; 07ED 80000000 00000000 - 07EC FFFFFFFF FFFFFFFF = 80000000 00000001
        ; This GAP is used for Debugging and error mode only. For example, defining the NANs, or checking for zero divisions etc
        ; but for the disassembler point of view, this is useless, because we are analysing a defined value with
        ; certain Bytes that certainly was badly generated due to an error on the linker/compiler that failed to check for
        ; the NANs or error mode values or by any other unknown error. For example, if the user was trying to build things like:
        ; T$ 1.0361967820008025600E-4321, instead the compiler output this bytes: 07ED 80000000 00000000, it outputed
        ; bad ones, like 07ED 7FFFFFFF 00000000, or 07ED 13900F00 00000000 like in our example.

            mov D$ebx+4 080000000, D$ebx 0
            mov D@FPUErrorMode DisSpecialFPU_Invalid
        .End_If

    ..Else_If W$ebx+8 = 07FFF
        ; This is infinite
        ; Last good one: 7FFE FFFFFFFF FFFFFFFF (1.1897314953572319232E+4932)
        ; Next Good one: 8000 00000000 00000000 (0)
        ; Accordying to our example : AllFPUVAlues3.exe i settled them as 1e+4933 1e+4934 etc etc.
        ; The generated value was: 7FFF 80000000 00000000
        ; So, the limit is 1.1897314953572319232E+4932. We also should fix this on the assembler.
        mov W$ebx+8 07FFE, D$ebx+4 0FFFFFFFF, D$ebx 0FFFFFFFF
        mov D@FPUErrorMode DisSpecialFPU_Invalid

        ; Below is similar to W$ebx+8 = 0, bit it will generate a negative 0 that does not exists
    ..Else_If_And W$ebx+8 = 08000, D$ebx+4 = 0, D$ebx = 0
            ; 8000 00000000 00000000 (-0)
        mov W$ebx+8 0
        mov D@FPUErrorMode DisSpecialFPU_Invalid

    ..Else_If_And W$ebx+8 = 08000, D$ebx+4 >= 0 ; This is denormalized, but possible. Valid Negative Subnormal

        mov D@FPUErrorMode DisSpecialFPU_ValidSubNormal

    ..Else_If_And W$ebx+8 > 08000, W$ebx+8 < 0FFFF
        .If D$ebx+4 < 080000000
        ; 87ED 13900F00 00000000
        ; Previous good value: 87EC FFFFFFFF FFFFFFFF (-1.0361967820008025600E-4321)
        ; Next Good value: 87ED 80000000 00000000 (-1.0361967820008025600E-4321)
            mov D$ebx+4 080000000, D$ebx 0
            mov D@FPUErrorMode DisSpecialFPU_Invalid
        .End_If

    ..Else_If W$ebx+8 = 0FFFF
        ; This is indefinite
        ; Last good one: FFFE FFFFFFFF FFFFFFFF (-1.1897314953572319232E+4932)
        ; Next Good one: 0000 00000000 00000000 (0)
        ; The same logical conclusion as in 07FFF
        mov W$ebx+8 0FFFE, D$ebx+4 0FFFFFFFF, D$ebx 0FFFFFFFF
        mov D@FPUErrorMode DisSpecialFPU_Invalid

    ..End_If

    mov eax D@FPUErrorMode

EndP
_________________________________________________________________________


Proc FPU8DisasmFix:
    Arguments @Float80Pointer
    Uses ebx

    mov ebx D@Float80Pointer
    ; Based on RealEightFPUNumberCategory
    ; Note: The denormalized values do not need to be fixed on this function

    ..If_And D$ebx+4 = 080000000, D$ebx = 0
        ; This is a signed Zero. There is no such a thing. Fix it to zero only
        mov D$ebx 0

    ; Check for NAN. Bits 52 to 62 are settled to 1 (Including the sign bit)
    ; Bit 63 is also settled to 1.
    ..Else_If_And B$ebx+6 >= 0F0, B$ebx+7 >= 07F ; NAN

        .If B$ebx+7 = 07F; NAN
            ; (1st fraction bit = 1) All exponents bits = 1, Sign bit = 0
            ; This can be +infinite, QNAN, SNAN
            ; Last good one: 7FEF FFFF FFFFFFFF (1.797693134862316E+308)
            ; Next Good one: 8000 0000 00000000 (0)
            ; Accordying to our example : AllFpuValuesr8.exe i settled them as 1e+308 1e+309 etc etc.
            ; The generated value was: 7FF00000 00000000
            ; So, the limit is 1.797693134862316E+308. We also should fix this on the assembler.
            mov D$ebx 0FFFFFFFF, D$ebx+4 07FEFFFFF
        .Else_If B$ebx+7 = 0FF; NAN
            ; (1st fraction bit = 1) All exponents bits = 1, Sign bit = 1
            ; This can be +infinite, QNAN, SNAN
            ; Last good one: FFEF FFFF FFFFFFFF (-1.797693134862316E+308)
            ; Next Good one: 0000 0000 00000000 (0)
            ; Accordying to our example : AllFpuValuesR8Neg.exe i settled them as 1e+308 1e+309 etc etc.
            ; The generated value was: FFF00000 00000000
            ; So, the limit is -1.797693134862316E+308. We also should fix this on the assembler.
            mov D$ebx 0FFFFFFFF, D$ebx+4 0FFEFFFFF

        .End_If
    ..End_If

EndP

_________________________________________________________________________


Proc FPU4DisasmFix:
    Arguments @Float80Pointer
    Uses ebx

    mov ebx D@Float80Pointer

    ; Based on RealFourFPUNumberCategory
    ; Note: The denormalized values do not need to be fixed on this function

    .If D$ebx = 080000000
    ; This is a signed Zero. There is no such a thing. Fix it to zero only
        mov D$ebx 0

    ; Check for NAN. Bits 31 to 24 are settled to 1 (Including the sign bit)
    ; Bit 23 is also settled to 1.
    .Else_If_And B$ebx+3 >= 07F, B$ebx+2 >= 080 ; NAN

        If B$ebx+3 = 07F ; NAN
            ; This can be +infinite, QNAN, SNAN
            ; Last good one: 7F7F FFFF (3.4028234663852886e+38)
            ; Next Good one: 8000 0000 (0)
            ; Accordying to our example : AllFpuValuesF8.exe i settled them as 1e+38 1e+39 etc etc.
            ; The generated value was: 07F800000
            ; So, the limit is 3.4028234663852886e+38. We also should fix this on the assembler.
            mov D$ebx 07F7FFFFF

        Else_If B$ebx+3 = 0FF ; NAN
            ; This can be -infinite, indefinite, Special QNAN/SNAN Indefinite Categories
            ; Last good one: FF7F FFFF (-3.4028234663852886e+38)
            ; Next Good one: 0000 0000 (0)
            ; Accordying to our example : AllFpuValuesF8.exe i settled them as -1e+38 -1e+39 etc etc.
            ; The generated value was: 0FF800000
            ; So, the limit is -3.4028234663852886e+38. We also should fix this on the assembler.
            mov D$ebx 0FF7FFFFF

        End_If
    .End_If

EndP
_________________________________________________________________________

Proc RealFourFPUNumberCategory:
    Arguments @Float80Pointer
    Uses ebx

    mov ebx D@Float80Pointer

    ; Bits 31 to 24 are settled to 0 (Including the sign bit)
    ; Bit 23 is also settled to 0.
    ..If_And B$ebx+3 = 0, B$ebx+2 <= 07F
    ; This is denormalized, but it is possible
    ..Else_If_And B$ebx+3 = 080, B$ebx+2 <= 07F
    ; This is signed denormalized, but it is possible
    ..Else_If D$ebx = 080000000
    ; This is a signed Zero. There is no such a thing. Fix it to zero only
        ;mov D$ebx 0

    ; Check for NAN. Bits 31 to 24 are settled to 1 (Including the sign bit)
    ; Bit 23 is also settled to 1.
    ..Else_If_And B$ebx+3 >= 07F, B$ebx+2 >= 080 ; NAN
        ; All Fraction Bits = 0, Sign Bit = 0, Exponent Bits = 1
        .If D$ebx = 07F800000;W$ebx = 0, B$ebx+2 = 080, B$ebx+3 = 07F
            ;+INFINITE
        ; All Fraction Bits = 0, Sign Bit = 1, Exponent Bits = 1
        .Else_If D$ebx = 0FF800000;W$ebx = 0, B$ebx+2 = 080, B$ebx+3 = 0FF
            ;-INFINITE
        ; All Fraction bits = 0 (1st fraction bit = 1) All exponents bits = 1, Sign bit = 1
        .Else_If D$ebx = 0FFC00000
            ; INDEFINITE

        .Else_If B$ebx+3 = 07F;, B$ebx+2 >= 0C0 ; NAN
            ; (1st fraction bit = 1) All exponents bits = 1, Sign bit = 0
            If B$ebx+2 >= 0C0
                ; QNAN
            Else ; (1st fraction bit = 0) All exponents bits = 1, Sign bit = 0
                ; SNAN
            End_If

        .Else_If B$ebx+3 = 0FF;, B$ebx+2 >= 0C0 ; NAN
            ; (1st fraction bit = 1) All exponents bits = 1, Sign bit = 1
            If B$ebx+2 >= 0C0
                ; Special INDEFINITE QNAN
            Else ; (1st fraction bit = 0) All exponents bits = 1, Sign bit = 1
                ; Special INDEFINITE SNAN
            End_If

        .End_If
    ..End_If

EndP
_________________________________________________________________________

Proc RealEightFPUNumberCategory:
    Arguments @Float80Pointer
    Uses ebx

    mov ebx D@Float80Pointer

    ; Bits 52 to 62 are settled to 0 (Including the sign bit)
    ; Bit 63 is also settled to 0.
    ..If_And B$ebx+6 <= 0F, B$ebx+7 = 0
    ; This is denormalized, but it is possible

    ; Bits 52 to 62 are settled to 0
    ; Bit 63 is also settled to 1. (Sign bit = 1)
    ..Else_If_And B$ebx+6 <= 0F, B$ebx+7 = 080;B$ebx+3 = 080, B$ebx+2 <= 07F
    ; This is signed denormalized, but it is possible

    ..Else_If_And D$ebx+4 = 080000000, D$ebx = 0
    ; This is a signed Zero. There is no such a thing. Fix it to zero only
        ;mov D$ebx 0

    ; Check for NAN. Bits 52 to 62 are settled to 1 (Including the sign bit)
    ; Bit 63 is also settled to 1.
    ..Else_If_And B$ebx+6 >= 0F0, B$ebx+7 >= 07F ; NAN
        ; All Fraction Bits = 0, Sign Bit = 0, Exponent Bits = 1
        .If_And D$ebx = 0, D$ebx+4 = 07FF00000
            ;+INFINITE
        ; All Fraction Bits = 0, Sign Bit = 1, Exponent Bits = 1
        .Else_If_And D$ebx = 0, D$ebx+4 = 0FFF00000
            ;-INFINITE
        ; All Fraction bits = 0 (1st fraction bit = 1) All exponents bits = 1, Sign bit = 1
        .Else_If_And D$ebx = 0, D$ebx+4 = 0FFF80000;D$ebx = 0FFC00000
            ; INDEFINITE

        .Else_If B$ebx+7 = 07F; NAN
            ; (1st fraction bit = 1) All exponents bits = 1, Sign bit = 0
            If B$ebx+6 >= 0F8
                ; QNAN
            Else ; (1st fraction bit = 0) All exponents bits = 1, Sign bit = 0
                ; SNAN
            End_If

        .Else_If B$ebx+7 = 0FF; NAN
            ; (1st fraction bit = 1) All exponents bits = 1, Sign bit = 1
            If B$ebx+6 >= 0F8
                ; Special INDEFINITE QNAN
            Else ; (1st fraction bit = 0) All exponents bits = 1, Sign bit = 1
                ; Special INDEFINITE SNAN
            End_If

        .End_If
    ..End_If

EndP
_________________________________________________________________________

;;
    Updated in 16/02/2019 (Gustavo Trigueiros - aka: Guga)

    RealTenFPUNumberCategory
        This function identifies the Errors existant in a Real10 FPU data.

    Parameters:
        Float80Pointer - A pointer to a variable containing a TenByte (80 bit) value

    Returned Values:
    
        The function will return one of the following equates:

        Equate                              Value   Description
        
        SpecialFPU_PosValid                 0       The FPU contains a valid positive number.
        SpecialFPU_NegValid                 1       The FPU contains a valid negative number.
        SpecialFPU_PosSubNormal             2       The FPU produced a positive Subnormal (denormalized) number.
                                                    Although it´s range is outside the range 3.6...e-4932, the number lost it´ precision, but it is still valid
                                                    A denormal(Subnormal) value the integer bit is not set
                                                    Ex: 0000 00000000 00000000
                                                        0000 00000000 FFFFFFFF
                                                        0000 00000000 00008000
                                                        0000 00000001 00000000
                                                        0000 FFFFFFFF FFFFFFFF
        SpecialFPU_NegSubNormal             3       The FPU produced a negative Subnormal (denormalized) number.
                                                    Although it´s range is outside the range -3.6...e-4932, the number lost it´ precision, but it is still valid
                                                    A denormal(Subnormal) value the integer bit is not set
                                                    Ex: 8000 00000000 00000000 (0) (Negative zero must be considered only as zero)
                                                        8000 00000000 FFFFFFFF (-0.0000000156560127730E-4933)
                                                        8000 01000000 00000000 (-0.2626643080556322880E-4933)
                                                        8000 FFFFFFFF 00000001 (-6.7242062846585856000E-4932)
        SpecialFPU_Zero                     4       The FPU contains a valid zero number
        SpecialFPU_QNAN                     5       QNAN - Quite NAN (Not a number)
        SpecialFPU_SNAN                     6       SNAN - Signaling NAN (Not a number)
        SpecialFPU_NegInf                   7       Negative Infinite
        SpecialFPU_PosInf                   8       Positive Infinite
        SpecialFPU_Indefinite               9       Indefinite

        These 4 equates below are not the official ones from IEEE. They were created to represente the cases when the Integer bit of the TenByte was not
        present by some error on compilers. A tenbyte always should have this bit settled (value = 1). When it is not settled the FPU simply will 
        refuses to process. To handle this lack of category of error we created the 4 ones below.
        The integer bit is the 63th bit of the tenbyte (or 31 of the 2nd dword)
        
        SpecialFPU_SpecialIndefQNan         10      Special INDEFINITE QNAN (Same as QNAN, but happened on an TenByte without the integer bit set)
        SpecialFPU_SpecialIndefSNan         11      Special INDEFINITE SNAN (Same as SNAN, but happened on an TenByte without the integer bit set)
        SpecialFPU_SpecialIndefNegInfinite  12      Special INDEFINITE Negative Infinite (Same as Negative Infinite, but happened on an TenByte without the integer bit set)
        SpecialFPU_SpecialIndefPosInfinite  13      Special INDEFINITE Positive Infinite (Same as Positive Infinite, but happened on an TenByte without the integer bit set)

    Remarks: For better analyse what a TenByte looks like, it is better you think on it as a Structure formed by 10 bytes containing 2 dword (the fraction bits) followed
             by a Word containing the exponent plus the Sign bit. Like this:

            [TenByteFormat:
                TenByteFormat.Significand1: D$ 0
                TenByteFormat.Significand2: D$ 0
                TenByteFormat.Exponent: W$ 0] ; including the sign on the last bit

            And the pseudo equates that may represent the "Tenbyte structure"
            
            TenByteFormat.Significand1Dis 0
            TenByteFormat.Significand2Dis 4
            TenByteFormat.ExponentDis 8

            Size_Of_TenByteFormat 10

            According to this site https://www.doc.ic.ac.uk/~eedwards/compsys/float/nan.html , the ranges (In little endian) for better categorize NANs and other errors on a Real4 and Real8 are:

            For single-precision values (Real4):

                Positive infinity is represented by the bit pattern 7F800000
                Negative infinity is represented by the bit pattern FF800000

                A signalling NaN (SNAN) is represented by any bit pattern
                between 7F800001 and 7FBFFFFF or between FF800001 and FFBFFFFF

                A quiet NaN (QNAN) is represented by any bit pattern 
                between 7FC00000 and 7FFFFFFF or between FFC00000 and FFFFFFFF

            For double-precision values (Real4):

                Positive infinity is represented by the bit pattern 7FF0000000000000
                Negative infinity is represented by the bit pattern FFF0000000000000

                A signalling NaN is represented by any bit pattern 

                between 7FF0000000000001 and 7FF7FFFFFFFFFFFF or 
                between FFF0000000000001 and FFF7FFFFFFFFFFFF

                A quiet NaN is represented by any bit pattern 

                between 7FF8000000000000 and 7FFFFFFFFFFFFFFF or 
                between FFF8000000000000 and FFFFFFFFFFFFFFFF


            Thus, for Extended-precision values (Real10):

                Positive INFINITY is represented by the bit pattern 7FFF_80000000_00000000
                Negative INFINITY is represented by the bit pattern FFFF_80000000_00000000

                INDEFINITE is represented by the bit pattern FFFF_C0000000_00000000

                A Quiet NaN would be represented by any bit pattern 

                between 7FFF_C0000000_00000001 and 7FFF_FFFFFFFF_FFFFFFFF or 
                between FFFF_C0000000_00000001 and FFFF_FFFFFFFF_FFFFFFFF

                A Signaling NaN would be represented by any bit pattern 

                between 7FFF_80000000_00000001 and 7FFF_BFFFFFFF_FFFFFFFF or 
                between FFFF_80000000_00000001 and FFFF_BFFFFFFF_FFFFFFFF

            
            The explicit 1 in bit 63 (bit 31 of the second dword = 080000000) always remains set for the REAL10 format, including all the NANs.
            Otherwise, the FPU seems to reject it as an unknown format.


        Additional Info:

        D$ 0, 080000000, W$ 01 3.36210314311209208e-4932

        ; Allowed Ranges:
        ; W$ebx+8 = 0, D$ebx+4 = any value . Ex.: 0000 30000000 00000000 (2.9355023956557458e-4942)
        ;       0000 00000000 00000000 (0) to 0000 FFFFFFF FFFFFFFF (4.2026289288901166080E-4933)
        ;       Weird: 0000 80000000 00000000 (3.3621031431120931840E-4932)
        ; W$ebx+8 > 0 < 07FFF, D$ebx+4 >= 080000000. Ex.: 0001 80000000 00000000 | 7FFE 80000000 00000000
        ; Ranges:   0001 80000000 00000000 (3.3621031431120931840E-4932)
        ;           7FFE FFFFFFFF 00000000 (1.1897314950802259968E+4932)
        ;           7FFE FFFFFFFF FFFFFFFF (1.1897314953572319232E+4932)
        ; W$ebx+8 = 07FFF, error in any case  (+INF = 7FFF 80000000 00000000)
        ;                                     (+NAN = all other values up to 7FFF 80000000 00000000)
        ; W$ebx+8 = 08000, D$ebx+4 = any value . Ex.: 0000 30000000 00000000
        ;       8000 00000000 00000000 (-0) to 8000 FFFFFFFF FFFFFFFF (-6.7242062862241863680E-4932)
        ;       Weird: 8000 80000000 00000000 (-3.3621031431120931840E-4932)
        ; W$ebx+8 > 08000 < FFFF, D$ebx+4 >= 080000000. Ex.: 8FF1 F00F0000 00000000
        ;           8001 80000000 00000000 (-3.3621031431120931840E-4932)
        ;           FFFE FFFFFFFF FFFFFFFF (-1.1897314953572319232E+4932)
        ; W$ebx+8 = 0FFFF, error in any case. (-INF = FFFF 80000000 00000000)
        ;                                     (-NAN = all other values up or equal to FFFF 80000000 00000000)
        ;                                     (INDEFINITE = all other values below to FFFF 80000000 00000000)
        ; FFFF 80000000 00400000 QNAN

    See also:
        FloatToUString

    References:
    
        http://www.ray.masmcode.com/tutorial/fpuchap2.htm
        https://www.doc.ic.ac.uk/~eedwards/compsys/float/nan.html
        Intel® 64 and IA-32 Architectures Software Developer’s Manual pg 89-92

    Many Thanks to Raymond Filiatreault to help clarify all of this.

;;


[SpecialFPU_PosValid 0] ; The FPU contains a valid positive result
[SpecialFPU_NegValid 1] ; The FPU contains a valid negative result
[SpecialFPU_PosSubNormal 2] ; The FPU produced a positive Subnormal (denormalized) number. So, although it´ range is outside the range 3.6...e-4932, the number lost it´ precision, but it is still valid
[SpecialFPU_NegSubNormal 3] ; The FPU produced a negative Subnormal (denormalized) number. So, although it´ range is outside the range -3.6...e-4932, the number lost it´ precision, but it is still valid
[SpecialFPU_Zero 4] ; The FPU contains a valid zero number
[SpecialFPU_QNAN 5] ; QNAN
[SpecialFPU_SNAN 6] ; SNAN
[SpecialFPU_NegInf 7] ; Negative Infinite
[SpecialFPU_PosInf 8] ; Positive Infinite
[SpecialFPU_Indefinite 9] ; Indefinite
[SpecialFPU_SpecialIndefQNan 10] ; Special INDEFINITE QNAN
[SpecialFPU_SpecialIndefSNan 11] ; Special INDEFINITE SNAN
[SpecialFPU_SpecialIndefNegInfinite 12] ; Special INDEFINITE Negative Infinite
[SpecialFPU_SpecialIndefPosInfinite 13] ; Special INDEFINITE Positive Infinite

Proc RealTenFPUNumberCategory:
    Arguments @Float80Pointer
    Local @FPUErrorMode
    Uses ebx


    mov ebx D@Float80Pointer
    mov D@FPUErrorMode SpecialFPU_PosValid

    ; 1st located all zero numbers (no bits settled on the whole TenByte or, only the Sign bit (15th of the last word = 79th bit of the Tenbyte ) was settled.
    ; Ex: D$ 0, 0, W$ 0 ; (0)
    ; Ex: D$ 0, 0, W$ 08000 ; negative zero is only zero
    .If_and D$ebx = 0, D$ebx+4 = 0
        ; ebx+8 = 08000 means a negative zero. But, there´s no such a thing as a negative zero, thus, it should be considered only 0
        If_Or W$ebx+8 = 0, W$ebx+8 = 08000
            mov eax SpecialFPU_Zero ; eax wil exist as Zero
            ExitP
        End_If
    .End_If



    ; Possible NANs contains always the 1st 2 bits of the 2nd word settled and the last 2 bit settled (pg 91/100 on Intel Manual)
    ; The biased exponent is on this form for NAN 11..11

     ; 2nd located all denormalized, but possible positive numbers.
     ; Ex: D$ 0FFFFFFFF, 0, W$ 0         ; (1.56560127731845315e-4941)
     ; Ex: D$ 0, 01, W$ 0                ; (1.56560127768297311e-4941)
     ; Ex: D$ 0FFFFFFFF, 0FFFFFFFF, W$ 0 ; (6.72420628622418417e-4932)
                                         ; (6.7242062862241870120e-4932) (Olly)

    ...If_And W$ebx+8 = 0, D$ebx+4 >= 0 ; On denormal (Subnormal) values, the integer bit is not set

        mov D@FPUErrorMode SpecialFPU_PosSubNormal

    ...Else_If_And W$ebx+8 = 08000, D$ebx+4 >= 0 ; On denormal (Subnormal) values, the integer bit is not set

    ; 3rd located all denormalized, but possible negative numbers. Bit 15th of the last word (Bit79 of the tenbyte) is settled
    ; Ex: D$ 0FFFFFFFF, 0, W$ 08000 ; (-1.56560127731845315e-4941)
    ; Ex: D$ 0, 01000000, W$ 08000  ; (-2.626643080565632194e-4934)  in olly dbg = (-0.2626643080556323050e-4933)
    ; Ex: D$ 01, 0FFFFFFFF, W$ 08000 ; (-6.72420628465858289e-4932)  in olly dbg = (-6.7242062846585857350e-4932)

        mov D@FPUErrorMode SpecialFPU_NegSubNormal

    ...Else_If W$ebx+8 = 07FFF ; Locate all positive infinite, QNAN, Special indefinite 00__0111_1111__1111_1111
    ; 00__0110_0000__0000_0011 06003 ; error happen here
    ; 00__0111_1111__1111_1111 07FFF ; error happen here

    ; 00__0111_1111__1111_1110 07FFE ; ok normal
    ; 00__0110_0000__0000_0010 06002 ; ok normal
    ; 00__0000_0000__0000_0001 01 ; ok normal

        ; locate all: Positive Infinite (Bit 15 of the last word is not set), the second dword are zero and the 1st dword contains only the integer bit 00__1000_0000__0000_0000
        ; Ex: D$ 0, 0, W$ 07FFF  ; 07FFF 00000000 00000000 when bit15 is not set it is positive 00__0111_1111__1111_1111
        ;.If_And D$ebx+4 = 0, D$ebx = 0 ; Error when 31th bit is not set to 1 (compiler error ?), but still related to Positive infinite

            ;mov D@FPUErrorMode SpecialFPU_PosInf

        .If_And D$ebx+4 = 080000000, D$ebx = 0; 2nd dword = 00__1000_0000__0000_0000__0000_0000__0000_0000 1st dword = 0

            mov D@FPUErrorMode SpecialFPU_PosInf

        .Else_If_And D$ebx+4 >= 0C0000000, D$ebx >= 01

            mov D@FPUErrorMode SpecialFPU_QNAN

        .Else_If_And D$ebx+4 >= 080000000, D$ebx >= 01

            mov D@FPUErrorMode SpecialFPU_SNAN

        .Else_If_And D$ebx+4 >= 040000000, D$ebx >= 01
            ; 00__0100_0000__0000_0000__0000_0000__0000_0000 If the compiler made an error and didn´t inserted the integer bit (bit 63 of the tenbyte or 31 of the 2nd dword)
            mov D@FPUErrorMode SpecialFPU_SpecialIndefQNan

        .Else_If D$ebx >= 01
            ; 2nd Dword = 0, and at least 1 bit settled on the 1st dword. If the compiler made an error and didn´ inserted the integer bit (bit 63 of the tenbyte or 31 of the 2nd dword)

            mov D@FPUErrorMode SpecialFPU_SpecialIndefSNan

        .Else
            ; all remaining cases, result only in D$ebx = 0. The lack of the 63th bit on this case could represente also a Indefinite Positive, but we are here labeling it as Indefinite infinite to be more
            ; logical with the Indefinite category
            mov D@FPUErrorMode SpecialFPU_SpecialIndefPosInfinite

        .End_If

    ...Else_If W$ebx+8 = 0FFFF ; Locate all negative infinite, QNAN, indefinite

        .If_And D$ebx+4 = 080000000, D$ebx = 0; 2nd dword = 00__1000_0000__0000_0000__0000_0000__0000_0000 1st dword = 0

            mov D@FPUErrorMode SpecialFPU_NegInf

        .Else_If_And D$ebx+4 >= 0C0000000, D$ebx >= 01

            mov D@FPUErrorMode SpecialFPU_QNAN

        .Else_If_And D$ebx+4 >= 080000000, D$ebx >= 01

            mov D@FPUErrorMode SpecialFPU_SNAN

        .Else_If_And D$ebx+4 = 0C0000000, D$ebx = 0

            mov D@FPUErrorMode SpecialFPU_Indefinite

        .Else_If_And D$ebx+4 >= 040000000, D$ebx >= 01
            ; 00__0100_0000__0000_0000__0000_0000__0000_0000 If the compiler made an error and didn´t inserted the integer bit (bit 63 of the tenbyte or 31 of the 2nd dword)
            mov D@FPUErrorMode SpecialFPU_SpecialIndefQNan

        .Else_If D$ebx >= 01
            ; 2nd Dword = 0, and at least 1 bit settled on the 1st dword. If the compiler made an error and didn´ inserted the integer bit (bit 63 of the tenbyte or 31 of the 2nd dword)

            mov D@FPUErrorMode SpecialFPU_SpecialIndefSNan

        .Else
            ; all remaining cases, result only in D$ebx = 0.

            mov D@FPUErrorMode SpecialFPU_SpecialIndefNegInfinite

        .End_If

    ...Else

        ; Now we must analyse the cases where the integer Bit (Bit 63 of tenbyte or 31th of the 2nd dword) is not settled by some error
        ; and the FPU will simply refuse to process
        ; Ex: [NumTest18: D$ 0, 013900F00, W$ 07ED]
        ..If D$ebx+4 < 080000000 ; Integer bit is never settled

            .If_And D$ebx+4 >= 040000000, D$ebx >= 01
                ; 00__0100_0000__0000_0000__0000_0000__0000_0000 If the compiler made an error and didn´t inserted the integer bit (bit 63 of the tenbyte or 31 of the 2nd dword)
                mov D@FPUErrorMode SpecialFPU_SpecialIndefQNan

            .Else_If D$ebx >= 01
                ; 2nd Dword = 0, and at least 1 bit settled on the 1st dword. If the compiler made an error and didn´ inserted the integer bit (bit 63 of the tenbyte or 31 of the 2nd dword)

                mov D@FPUErrorMode SpecialFPU_SpecialIndefSNan

            .Else
                ; all remaining cases, result only in D$ebx = 0 and we must only check if it is positive or negative
                ; Sign bit is settled
                Test_If B$ebx+9 0_80
                    mov D@FPUErrorMode SpecialFPU_SpecialIndefNegInfinite
                Test_Else
                    ; Sign bit is never settled
                    mov D@FPUErrorMode SpecialFPU_SpecialIndefPosInfinite
                Test_End
            .End_If
        ..End_If

    ...End_If

    .If D@FPUErrorMode = SpecialFPU_PosValid
        Test_If B$ebx+9 0_80
            mov D@FPUErrorMode SpecialFPU_NegValid
        Test_End
    .End_If

    mov eax D@FPUErrorMode

EndP

;;

Proc RealTenFPUNumberCategory_Old:
    Arguments @Float80Pointer
    Local @FPUErrorMode
    Uses edi, ebx


    mov ebx D@Float80Pointer
    mov D@FPUErrorMode SpecialFPU_PosValid

    ...If_And W$ebx+8 = 0, D$ebx+4 = 0 ; This is denormalized, but it is possible.
        ; 0000 00000000 00000000
        ; 0000 00000000 FFFFFFFF
        ;mov eax eax
        mov D@FPUErrorMode SpecialFPU_PosSubNormal
    ...Else_If_And W$ebx+8 = 0, D$ebx+4 > 0 ; This is Ok.
        ; 0000 00000001 00000000
        ; 0000 FFFFFFFF FFFFFFFF
        ;mov eax eax
        mov D@FPUErrorMode SpecialFPU_PosSubNormal
    ...Else_If_And W$ebx+8 > 0, W$ebx+8 < 07FFF; This is ok only if the fraction Dword is bigger or equal to 080000000
        .If D$ebx+4 < 080000000
            .Test_If D$ebx+4 040000000
                ; QNAN 40000000
                mov D@FPUErrorMode SpecialFPU_QNAN
            .Test_Else
                If_And D$ebx+4 > 0, D$ebx > 0
                    ; SNAN only if at least 1 bit is set
                    mov D@FPUErrorMode SpecialFPU_SNAN
                Else ; All fraction Bits are 0
                    ; Bit 15 is never reached. The bit is 0 from W$ebx+8
                    ; -INFINITE ; Bit15 = 0
                    mov D@FPUErrorMode SpecialFPU_NegInf
                End_If
            .Test_End
        .End_If
    ...Else_If W$ebx+8 = 07FFF; This is ok only if the fraction Dword is bigger or equal to 080000000
        .Test_If D$ebx+4 040000000
            ; QNAN 40000000
            mov D@FPUErrorMode SpecialFPU_QNAN
        .Test_Else
            If_And D$ebx+4 > 0, D$ebx > 0
                ; SNAN only if at least 1 bit is set
                mov D@FPUErrorMode SpecialFPU_SNAN
            Else ; All fraction Bits are 0
                ; Bit 15 is never reached. The bit is 0 from W$ebx+8
                ; -INFINITE ; Bit15 = 0
;               Test_If W$ebx+8 = 0FFFF ; we need to see if Bit 15 is set
 ;                  ; -INFINITE ; Bit15 = 0
  ;             Test_Else
   ;                ; +INFINITE ; Bit15 = 1
    ;           Test_End
                ;mov D$edi '-INF', B$edi+4 0
                mov D@FPUErrorMode SpecialFPU_NegInf
            End_If
        .Test_End
        ; Below is similar to W$ebx+8 = 0
    ...Else_If_And W$ebx+8 = 08000, D$ebx+4 = 0 ; This is denormalized, but possible.
        ; 8000 00000000 00000000 (0)
        ; 8000 00000000 FFFFFFFF (-0.0000000156560127730E-4933)
        mov D@FPUErrorMode SpecialFPU_NegSubNormal
    ...Else_If_And W$ebx+8 = 08000, D$ebx+4 > 0 ; This is Ok.
        ; 8000 01000000 00000000 (-0.2626643080556322880E-4933)
        ; 8000 FFFFFFFF 00000001 (-6.7242062846585856000E-4932)
        mov D@FPUErrorMode SpecialFPU_NegSubNormal
    ...Else_If_And W$ebx+8 > 08000, W$ebx+8 < 0FFFF; This is ok only if the fraction Dword is bigger or equal to 080000000
        .If D$ebx+4 < 080000000
            .Test_If D$ebx+4 040000000
                ; QNAN 40000000
                mov D@FPUErrorMode SpecialFPU_QNAN
            .Test_Else
                If_And D$ebx+4 > 0, D$ebx > 0
                    ; SNAN only if at least 1 bit is set
                    ;mov D$edi 'SNaN', B$edi+4 0
                    mov D@FPUErrorMode SpecialFPU_SNAN
                Else ; All fraction Bits are 0
                    ; Bit 15 is always reached. The bit is 1 from W$ebx+8
                    ; +INFINITE ; Bit15 = 1
                    ;mov D$edi '+INF', B$edi+4 0
                    mov D@FPUErrorMode SpecialFPU_PosInf
                End_If
            .Test_End
        .End_If

    ...Else_If W$ebx+8 = 0FFFF; This is to we identify indefined or other NAN values

        .If_And D$ebx+4 >= 040000000, D$ebx = 0
            ; INDEFINITE
            mov D@FPUErrorMode SpecialFPU_Indefinite
        .Else
            .Test_If D$ebx+4 040000000
                ; Special INDEFINITE QNAN 40000000
                mov D@FPUErrorMode SpecialFPU_SpecialIndefQNan
            .Test_Else
                If_And D$ebx+4 > 0, D$ebx > 0
                    ; Special INDEFINITE SNAN only if at least 1 bit is set
                    mov D@FPUErrorMode SpecialFPU_SpecialIndefSNan
                Else ; All fraction Bits are 0
                    ; Bit 15 is always reached. The bit is 1 from W$ebx+8
                    ; Special INDEFINITE +INFINITE ; Bit15 = 1
                    mov D@FPUErrorMode SpecialFPU_SpecialIndefInfinite
                End_If
            .Test_End
        .End_If
    ...End_If

    .If D@FPUErrorMode = SpecialFPU_PosValid
        Test_If B$ebx+9 0_80
            mov D@FPUErrorMode SpecialFPU_NegValid
        Test_End
    .End_If

    mov eax D@FPUErrorMode

EndP
;;

Proc WriteFPUErrorMsg:
    Arguments @FPUcategoryID, @pOutputBuff

    .If D@FPUcategoryID = SpecialFPU_PosValid
        call SimpleStringCopy {B$ "Valid Normal Positive", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_NegValid
        call SimpleStringCopy {B$ "Valid Normal Negative", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_PosSubNormal
        call SimpleStringCopy {B$ "Valid Subnormal Positive", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_NegSubNormal
        call SimpleStringCopy {B$ "Valid Subnormal Negative", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_Zero
        call SimpleStringCopy {B$ "Valid Zero Number", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_QNAN
        call SimpleStringCopy {B$ "QNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SNAN
        call SimpleStringCopy {B$ "SNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_NegInf
        call SimpleStringCopy {B$ "-INFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_PosInf
        call SimpleStringCopy {B$ "+INFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_Indefinite
        call SimpleStringCopy {B$ "INDEFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefQNan
        call SimpleStringCopy {B$ "Special INDEFINITE QNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefSNan
        call SimpleStringCopy {B$ "Special INDEFINITE SNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefNegInfinite
        call SimpleStringCopy {B$ "Special INDEFINITE -INFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefPosInfinite
        call SimpleStringCopy {B$ "Special INDEFINITE +INFINITE", 0}, D@pOutputBuff
    .Else
        call SimpleStringCopy {B$ "Unknown FPU error", 0}, D@pOutputBuff
    .End_If

EndP
;;
Proc WriteFPUErrorMsg_Old:
    Arguments @FPUcategoryID, @pOutputBuff

    .If D@FPUcategoryID = SpecialFPU_PosValid
        call SimpleStringCopy {B$ "Valid Normal Positive", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_NegValid
        call SimpleStringCopy {B$ "Valid Normal Negative", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_PosSubNormal
        call SimpleStringCopy {B$ "Valid Subnormal Positive", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_NegSubNormal
        call SimpleStringCopy {B$ "Valid Subnormal Negative", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_QNAN
        call SimpleStringCopy {B$ "QNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SNAN
        call SimpleStringCopy {B$ "SNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_NegInf
        call SimpleStringCopy {B$ "-INFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_PosInf
        call SimpleStringCopy {B$ "+INFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_Indefinite
        call SimpleStringCopy {B$ "INDEFINITE", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefQNan
        call SimpleStringCopy {B$ "Special INDEFINITE QNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefSNan
        call SimpleStringCopy {B$ "Special INDEFINITE SNAN", 0}, D@pOutputBuff
    .Else_If D@FPUcategoryID = SpecialFPU_SpecialIndefInfinite
        call SimpleStringCopy {B$ "Special INDEFINITE +INFINITE", 0}, D@pOutputBuff
    .Else
        call SimpleStringCopy {B$ "Unknown FPU error", 0}, D@pOutputBuff
    .End_If

EndP
;;

; simple routine to copy a null termninated string to a buffer 12/02/2019
; Return Values. Eax contains the total amount of bytes copied
Proc SimpleStringCopy:
    Arguments @Input, @Output
    Uses edi, esi

    mov esi D@Input
    mov edi D@Output
    xor eax eax
    While B$esi <> 0
        movsb
        inc eax
    End_While
    mov B$edi 0

EndP

_________________________________________________________________

;;

When the equate is set to FPU_STATUSREG_C0 it means that we have an rounding error.
The Value found in ST0 is smaller then the one in ST1 (in the functionality we are using)
;;
Proc TestingFPUStatusRegister:
    Arguments @StatusReg
    Uses esi, eax

    mov eax D@StatusReg

    Test_If eax &FPU_EXCEPTION_INVALIDOPERATION
        ZCopy {" &FPU_EXCEPTION_INVALIDOPERATION ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_DENORMALIZED
        ZCopy {" &FPU_EXCEPTION_DENORMALIZED ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_ZERODIV
        ZCopy {" &FPU_EXCEPTION_ZERODIV ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_OVERFLOW
        ZCopy {" &FPU_EXCEPTION_OVERFLOW ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_UNDERFLOW
        ZCopy {" &FPU_EXCEPTION_UNDERFLOW ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_PRECISION
        ZCopy {" &FPU_EXCEPTION_PRECISION ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_STACKFAULT
        ZCopy {" &FPU_EXCEPTION_STACKFAULT ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_XFLAG
        ZCopy {" &FPU_EXCEPTION_XFLAG ", 0}
    Test_End

    Test_If eax &FPU_STATUSREG_C0
        ZCopy {" &FPU_STATUSREG_C0 ", 0}
    Test_End

    Test_If eax &FPU_STATUSREG_C1
        ZCopy {" &FPU_STATUSREG_C1 ", 0}
    Test_End

    Test_If eax &FPU_STATUSREG_C2
        ZCopy {" &FPU_STATUSREG_C2 ", 0}
    Test_End

    ; Are the ST(x) registers not empty ? We will show only the used Stack registers, ST0, ST1, ST2...ST7
    Test_If_Not eax &FPU_STATUSREG_ST0
        ZCopy {" &FPU_STATUSREG_ST0 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST1
        ZCopy {" &FPU_STATUSREG_ST1 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST2
        ZCopy {" &FPU_STATUSREG_ST2 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST3
        ZCopy {" &FPU_STATUSREG_ST3 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST4
        ZCopy {" &FPU_STATUSREG_ST4 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST5
        ZCopy {" &FPU_STATUSREG_ST5 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST6
        ZCopy {" &FPU_STATUSREG_ST6 ", 0}
    Test_End

    Test_If_Not eax &FPU_STATUSREG_ST7
        ZCopy {" &FPU_STATUSREG_ST7 ", 0}
    Test_End

    Test_If eax &FPU_STATUSREG_C3
        ZCopy {" &FPU_STATUSREG_C3 ", 0}
    Test_End

    Test_If eax &FPU_STATUSREG_RESERVED
        ZCopy {" &FPU_STATUSREG_RESERVED ", 0}
    Test_End

EndP

_________________________________________________________________

;;
    This Function displays the FPU control registers used on the arithmetic operation used to store the
    value of the Control Word. It will output at edi the String related to the equate used on the operation.
    So, at edi you must have enough Buffer Size to the String be stored.
    
    Parameter:
    
    ControlWord - The inputed Control Word Value to be analysed.
    
    Note: This function is to be used to analyse if a given FPU value stored in eax have some relationship
    with the rounding mode, or eventual errors in the result of a certain value.
    Although the proper function to be used for such analysis is TestingFPUStatusRegister , we can use this function
    to display if we have variances on the precision or in the rounding mode, and compare the results with
    the ones provided in the TestingFPUStatusRegister function.
    
    Check in B_U_Asm at "The FPU Control Register" for further usage of the envolved equates.
    
    Usage example:
        
        xor eax eax
        fld R$SomeValue    
        FSTSW AX
        call TestingFPUExceptions eax
;;

Proc TestingFPUExceptions:
    Arguments @ControlWord
    Uses esi, eax

    mov eax D@ControlWord

    Test_If eax &FPU_EXCEPTION_INVALIDOPERATION
        ZCopy {" FPU_EXCEPTION_INVALIDOPERATION ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_DENORMALIZED
        ZCopy {" &FPU_EXCEPTION_DENORMALIZED ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_ZERODIV
        ZCopy {" &FPU_EXCEPTION_ZERODIV ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_OVERFLOW
        ZCopy {" &FPU_EXCEPTION_OVERFLOW ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_UNDERFLOW
        ZCopy {" &FPU_EXCEPTION_UNDERFLOW ", 0}
    Test_End

    Test_If eax &FPU_EXCEPTION_PRECISION
        ZCopy {" &FPU_EXCEPTION_PRECISION ", 0}
    Test_End

;;
    Test_If eax &FPU_RESERVEDBIT6
        ZCopy {" &FPU_RESERVEDBIT6 ", 0}
    Test_End
    
    Test_If eax &FPU_RESERVEDBIT7
        ZCopy {" &FPU_RESERVEDBIT7 ", 0}
    Test_End
;;

    Test_If eax &FPU_PRECISION_24BITS
        ZCopy {" &FPU_PRECISION_24BITS ", 0}
    Test_End

;;
    Test_If eax &FPU_PRECISION_RESERVED
        ZCopy {" &FPU_PRECISION_RESERVED ", 0}
    Test_End
;;

    Test_If eax &FPU_PRECISION_53BITS
        ZCopy {" &FPU_PRECISION_53BITS ", 0}
    Test_End

    Test_If eax &FPU_PRECISION_64BITS
        ZCopy {" &FPU_PRECISION_64BITS ", 0}
    Test_End

    Test_If eax &FPU_ROUNDINGMODE_NEAREST_EVEN
        ZCopy {" &FPU_ROUNDINGMODE_NEAREST_EVEN ", 0}
    Test_End

    Test_If eax &FPU_ROUNDINGMODE_DOWN
        ZCopy {" &FPU_ROUNDINGMODE_DOWN ", 0}
    Test_End

    Test_If eax &FPU_ROUNDINGMODE_UP
        ZCopy {" &FPU_ROUNDINGMODE_UP ", 0}
    Test_End

    Test_If eax &FPU_ROUNDINGMODE_TRUNCATE
        ZCopy {" &FPU_ROUNDINGMODE_TRUNCATE ", 0}
    Test_End

;;
    Test_If eax &FPU_RESERVEDBIT12
        ZCopy {" &FPU_RESERVEDBIT12 ", 0}
    Test_End
    
    Test_If eax &FPU_RESERVEDBIT13
        ZCopy {" &FPU_RESERVEDBIT13 ", 0}
    Test_End

    Test_If eax &FPU_RESERVEDBIT14
        ZCopy {" &FPU_RESERVEDBIT14 ", 0}
    Test_End

    Test_If eax &FPU_RESERVEDBIT15
        ZCopy {" &FPU_RESERVEDBIT15 ", 0}
    Test_End
;;

EndP



; Guga FPUIF macros


[Fpu_If | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R0>>]
[Fpu_Else_If | jmp R5>> | R0: | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R0>>]
[Fpu_Else | jmp R5>> | R0:]
[Fpu_End_If | R0: | R5:]

[.Fpu_If | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R1>>]
[.Fpu_Else_If | jmp R6>> | R1: | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R1>>]
[.Fpu_Else | jmp R6>> | R1:]
[.Fpu_End_If | R1: | R6:]

[..Fpu_If | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R2>>]
[..Fpu_Else_If | jmp R7>> | R2: | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R2>>]
[..Fpu_Else | jmp R7>> | R2:]
[..Fpu_End_If | R2: | R7:]

[...Fpu_If | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R3>>]
[...Fpu_Else_If | jmp R8>> | R3: | fld #1 | fld #3 | fcompp | fstsw ax | fwait | sahf | jn#2 R3>>]
[...Fpu_Else | jmp R8>> | R3:]
[...Fpu_End_If | R3: | R8:]

[Fpu_If_And    | Fpu_If #1 #2 #3    | #+3]
[.Fpu_If_And   | .Fpu_If #1 #2 #3   | #+3]
[..Fpu_If_And  | ..Fpu_If #1 #2 #3  | #+3]
[...Fpu_If_And | ...Fpu_If #1 #2 #3 | #+3]

[Fpu_Else_If_And    | Fpu_Else    | Fpu_If_And    #F>L]
[.Fpu_Else_If_And   | .Fpu_Else   | .Fpu_If_And   #F>L]
[..Fpu_Else_If_And  | ..Fpu_Else  | ..Fpu_If_And  #F>L]
[...Fpu_Else_If_And | ...Fpu_Else | ...Fpu_If_And #F>L]

































