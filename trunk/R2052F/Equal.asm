TITLE Equal
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Author name: Scarmatil (Julien OTELE MANDA)

 eMail: < scarmatil@gmail.com>
 
 Last Update: 27-07-2005      
____________________________________________________________________________________________

 ProcessEqual  EqualParser  WriteOperationCode  IntegratedFunctionManagement
 EvalOneEquation   GetStringType  ProcessarrayDestination  ComputeOperationsIn_EPPSourceEquation
 
 
 EPP_All_Data    
 
____________________________________________________________________________________________
HOW TO USE THE EQUAL PREPARSER: 

This pre-parser actually groups three differents parsers :


=> Evaluation parser:  (Expression1 OP Expression2) 

- Expression1 and Expression2 are the two expression you want to compare, 
  SourceExpression can be a single register (Common or FPU), memory data or an equation combining the 
  operators below with any memory data, register or constant.

- OP is the comparison operator, it can be one of the following:  =  <  >

- The whole expression (including the parentheses are replaced by a boolean DWord value containing 
  the result of the evaluation of this expression.


=> String maker:  str(Destination) = Source

- Destination can be a 32 bits register, a Data Label (i.e a constant) or a Memory DWord. 

- Source can be a string, a 32 bits register, a Data Label or a Memory DWord. 
 
- /!\ The destination is a pointer, you have to make sure there is enough space to write the string /!\

- /!\ Each source value is a null-terminated string, make sure that the trailing null character is not missing. /!\ 

example :
[CRLF: W$ 0D0A 0] [strbuf:B$ ? #128] [strbuf2:B$ ? #128]
 > ecx = strbuf2
 > str(ecx) = 'Scarmatil'
 > str(strbuf) = 'hop !' CRLF ecx ' et ' ecx ' sont sur un bateau. ' ecx " tombe a l'eau? " CRLF "Qui reste-t-il ?"
 


=> Equation Parser:   Destination = SourceExpression 

- Destination can be a register (Common or FPU) or memory Data (including array, see below for the syntax).

- SourceExpression can be a single register (Common or FPU), memory data or an equation combining the 
  operators below with any memory data, register or constant.
  It can also be a call, providing that the return value is in EAX.
    
- You can use parentheses with no limit of nesting (except your time while compiling)

- To display the code output by the preparser inside RosAsm, simply double-click on the Destination 
  operand and choose 'unfold' in the menu.

* Operator list : 
    => ^       ; power operator, the first operand has to be strictly positive 
    => * / + - : common operator
    => // **   ; signed operators 

* Available functions : 
  - cos(), sin(), abs(), tan(), sqrt(), ln(), log10(), log2(), exp() and atan().  

* Conversions :
 The parser can perform any conversion as long as they are not impossible (store a DWord in a byte !?):
  > ST0 = (D$a+D$b)/6+(D$c*7)       ; integer result stored in a FPU register
  > Q$MyQValue = B$MyByteVal  
  > ...
    
* Signed operations:
  To force signed integer division and multiplication you can use '//' and '**' operators .
  Notice that if one of the operand is a floating point value (6.2,T$MyVal,...) the operation
  will be signed even with '*' and '/' operators.

* Addressing support
    - X$(expression)  : this will be interpreded as : X$expression (like in basic RosAsm syntax)
    - Without parentheses if the writing is the same that in basic RosAsm syntax when it is possible
      and the operation otherwise.
ex:
(1) - edx = D$(eax+3)     ; mov edx D$eax+3
(2) - edx = (D$eax)+3     ; mov edx D$eax | add edx 3 
(3) - edx = D$eax+3       ; mov edx D$eax+3
(3) - edx = D$eax+D$ebx   ; mov edx D$eax | add edx D$ebx  
(4) - edx = D$(eax+D$ebx) ; add eax D$ebx | mov edx D$eax 

* Array support:    
    - X$Array(index)  : this will be interpreded as : X$Array+index*(X_size)

ex:- [MyTable: D$ 'ZERO' 'ONE ' 'TWO ' 'THRE' 'FOUR' 'FIVE']
     eax = D$MyTable(4)  ; eax = 'FOUR'
                 
                        
* Supported data size (both in source and destination): B$, W$, D$, Q$, F$, R$ and T$    


/!\  No FINIT is written by the parser, write it yourself at the beginning of       /!\
     the code if you use expressions containing reals .
    
examples :

 > eax = 1
 > edx = &TRUE+(2*2)
 > D$Value1 = D$Value2
 > W$Value1+2 = W$Value2+2
 > D$Handle = call 'DLLNAME.DllFunction', Para1, ...    
 > ST2 = (tan(45)/sqrt(3.5+9)-1/T$MyReal)               
 > eax = (D$a+D$b)/6+(D$c*7)
 > D$Val3 = (3 * D$a + 12) / (24 * D$b) - D$c * D$c * 18 / (13 + D$z) 
 > ebx = sin(45) 
 > T$FloatNumber = sqrt(abs(7+-3*ebx)+R$FpVal)
 > eax = F$MyArrayName(eax)
 > D$DestArray(ecx) = W$SourceArray(eax)   

        ____________________________________________________________________________________________
 
  Basically this parser works as follow :
  It first copies the equation into EPP_SourceEquation and the destination operand into 
  EPP_DestOperand 
  Then the equation is formated to ease the parsing.

  The parsing begins and the first closing parenthesis is searched. Each time the parser meets an 
  opening parenthesis, it keeps its position.
  Once the first closing parenthesis is found, the operations in these parentheses are processed
  in this order: ^ then * and / and finally + and - (to keep prority).  
  
  Then each operation type is processed and the corresponding code is written thanks to 
  the macro EPP_Code                                                          
;;
____________________________________________________________________________________________
EqualParser:
    mov esi D$CodeSourceA, edi D$CodeSourceB | mov ecx esi | add ecx D$StripLen
    mov B$ErrorLevel 0, D$bracketCounter 0      ; error0
   ; mov D$StatementsCounter 1, D$InstructionsCounter 0
    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

    .While esi < ecx
        ..If B$esi = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While
        ..Else_If_And B$esi+1 = '=', B$esi = Space, B$esi+2 = Space
            call ParseOneEqualLine
        ..Else_If B$esi = openSign
            call EvaluateEquation
        ..End_If

        If B$esi = EOI
            add D$StatementsPtr 4
        Else_If B$esi = OpenBracket
            add D$StatementsPtr 4
        Else_If B$esi = OpenVirtual
            add D$StatementsPtr 4
        End_If

        movsb
    .End_While

; post-parsing optimization :

    jmp L1>
    While ebx <> CODE_NOT_FOUND
        call DeleteFinalCodeLine ebx edi | sub edi eax

L1:     EPP_FindCodePos 'end_equal||' D$CodeSourceB edi
        mov ebx eax
    End_While
    mov B$edi-1 EOI, B$edi 0

L2:
    sub edi D$CodeSourceB | mov D$StripLen edi
    Exchange D$CodeSourceA D$CodeSourceB

ret
____________________________________________________________________________________________
EvaluateEquation:
    ; Look for Equation between parentheses which can be evualuated
    ; There three possibles comparison operators (=,<,>)
    ; ex: (eax*7=F$a/2)  ;  (D$Array(ecx)<exp(ecx))
    mov eax INITIAL_STATE
    lea edx D$esi+1

    While B$edx > EOI
        If_And B$edx = closeSign, eax = FOUND_EQUATION_TEST
            jmp L1>
        End_If
        On B$edx = openSign, inc eax
        On B$edx = closeSign, dec eax

        .If_Or B$edx = '=', B$edx = '<', B$edx = '>'
            If eax = INITIAL_STATE
                mov ebx edx
                mov eax FOUND_EQUATION_TEST
            Else
                mov eax 0 ; unpaired parenthesis
            End_If
        .End_If
        inc edx
    End_While
L1:
    If eax = FOUND_EQUATION_TEST
        call EvalOneEquation esi edx ebx edi
    End_If
ret
____________________________________________________________________________________________
Proc EvalOneEquation:
    ; This routine evaluates each member of the Equation between D@openSign and D@closeSign,
    ; then compare them using de D@CompPos operator (=,<,>) and replace the whole expression
    ; by a boolean value stored in a DWord (&TRUE if the comparison is true &FALSE otherwise)
    Arguments @openSign @closeSign @CompPos @ProducedCode
    Uses ebx, ecx

    ; debug Data
    move D$LastEqualLine   D@ProducedCode

    ; init EPP_Code
    move D$EPP_WrittenCode    D@ProducedCode
    move D$EPP_CodeBegining   D@ProducedCode

    EPP_Code 'PUSH ESI|MOV ESI ESP|'

    push D$EPP_WrittenCode
        mov D$EPP_WrittenCode EPP_DestOperand
        EPP_Code 'T$&8 '
    pop D$EPP_WrittenCode
    EPP_Code 'SUB ESP 0A|{!8 ESI-0A}|'

    mov esi D@openSign | inc esi
    mov edi EPP_SourceEquation
    mov ebx D@CompPos
    While esi < ebx | movsb | End_While | mov B$edi EOI

    push D$EPP_CodeBegining
        move D$EPP_CodeBegining D$EPP_WrittenCode
        call ComputeOperationsIn_EPPSourceEquation &FALSE
        call StoreResultInFirstOperand
        call OptimizeEqualPreparserCode EPP_CODE_BEGIN EPP_CODE_END
        call UpdateLocalDataShift
    pop D$EPP_CodeBegining

    push D$EPP_WrittenCode
        mov D$EPP_WrittenCode EPP_DestOperand
        EPP_Code 'T$&9 '
    pop D$EPP_WrittenCode
    EPP_Code 'SUB ESP 0A|{!9 ESI-014}|'

    mov esi D@CompPos | inc esi
    mov edi EPP_SourceEquation
    mov ebx D@closeSign
    While esi < ebx | movsb | End_While | mov B$edi EOI

    push D$EPP_CodeBegining
        move D$EPP_CodeBegining D$EPP_WrittenCode
        call ComputeOperationsIn_EPPSourceEquation &FALSE
        call StoreResultInFirstOperand
        call OptimizeEqualPreparserCode EPP_CODE_BEGIN EPP_CODE_END
        call UpdateLocalDataShift
    pop D$EPP_CodeBegining

    EPP_Code 'FLD T$!9|FLD T$!8|'

    mov ebx D@CompPos
    If B$ebx = '='
        EPP_Code 'FCOMP ST1|FSTSW AX|AND AX 04400|CMP AX 04000|JNE &1|MOV EAX 1|JMP &3|!1:|MOV EAX 0|!3:|'
    Else_If B$ebx = '<'
        EPP_Code 'FCOMP ST1|FSTSW AX|AND AX 0500|CMP AX 0100|JNE &1|MOV EAX 1|JMP &3|!1:|MOV EAX 0|!3:|'
    Else_If B$ebx = '>'
        EPP_Code 'FCOMP ST1|FSTSW AX|AND AX 04500|TEST AX AX|JNE &1|MOV EAX 1|JMP &3|!1:|MOV EAX 0|!3:|'
    End_If
    EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'

    ; Replaces the two expressions and the comparison operator by the DWord result
    mov esi D@openSign | While B$esi > EOI | dec esi | End_While | inc esi
    mov ebx D@openSign
    mov edi D$EPP_WrittenCode
        While esi < ebx | movsb | End_While
    mov D$EPP_WrittenCode edi
    EPP_Code 'D$?'
    mov esi D@closeSign | inc esi |While B$esi > EOI | inc esi | End_While | mov ebx esi
    mov esi D@closeSign | inc esi
    mov edi D$EPP_WrittenCode
        While esi <= ebx | movsb | End_While
    mov D$EPP_WrittenCode edi
    mov B$edi meEOI
    EPP_Code 'MOV ESP ESI|POP ESI||'

    ; remove what was written before D$EPP_CodeBegining (which was initially before the expression)
    mov edi D$EPP_CodeBegining
    mov eax 0
    If B$edi-1 <> EOI
        mov B$edi-1 EOI
        sub edi 2 | While B$edi > EOI | dec edi | End_While | inc edi
        call DeleteFinalCodeLine edi D$EPP_WrittenCode
    End_If

    ; Updates edi
    mov edi D$EPP_WrittenCode | sub edi eax

    call TranslateExpressions
EndP
____________________________________________________________________________________________
ParseOneEqualLine:
    mov ebx esi, edx edi   ; D$esi = ' = '  ; eax = 1  (keep ebx at ' = ')

  ; Go to start of Instruction, and abort if, for example "If eax = 1" is encounted
  ; because of such user Defined Equates. In such Cases, we will have either one more
  ; Space backward, or, maybe, '[', if encounted inside a Declaration:
    While B$esi-1 > EOI
        On B$esi-1 = ColonSign, jmp L2>
        dec esi | dec edi
        .If B$esi <= OpenVirtual
            If B$esi = Space
                jmp L1>
            Else_If B$esi = OpenBracket
                jmp L1>
            Else_If B$esi = OpenVirtual
L1:             mov esi ebx, edi edx | ret
            End_If
        .End_If
    End_While

L2: call ProcessEqual ebx edi

    ;Sets esi (in CodeSourceA) and edi (in CodeSourceB) on the end of Line (EOI)
    mov esi ebx
    While B$esi <> EOI | inc esi | End_While

    mov edi edx
    While B$edi <> EOI | inc edi | End_While
    On B$edi-1 = meEOI, mov B$edi-1 EOI     ;The code mustn't end with 0102 (meEOI,EOI) (or RosAsm hangs when unfolding)
ret
____________________________________________________________________________________________
Proc ProcessEqual:
    ; D@EqualPos is the position of ' = '
    Arguments @EqualPos @ProducedCode
    pushad

    ; debug Data
    move D$LastEqualLine   D@ProducedCode
    On D$FirstEqualLine = &TRUE, move D$FirstEqualLine D@ProducedCode

    ; init EPP_Code
    move D$EPP_WrittenCode D@ProducedCode
    move D$EPP_CodeBegining   D@ProducedCode

    ; copies the Destination operand in EPP_DestOperand
    mov esi D@EqualPos
    While B$esi <> EOI | dec esi | End_While | inc esi
    mov edi EPP_DestOperand
    While B$esi > Space | movsb | End_While | mov B$edi Space

    ; copies the line into EPP_SourceEquation
    mov esi D@EqualPos | add esi 3
    mov edi EPP_SourceEquation
    While B$esi > EOI | movsb | End_While | mov B$edi EOI

    call IsStringMaker | If eax = &TRUE | popad | ExitP | End_If

    call IsItaCall | If eax = &TRUE | popad | ExitP | End_If

    call ReserveGlobalFunctionAdress

    call ComputeOperationsIn_EPPSourceEquation &FALSE

    call ProcessArrayDestination

    call StoreResultInFirstOperand

    EPP_Code 'end_equal||'

    call OptimizeEqualPreparserCode EPP_CODE_BEGIN EPP_CODE_END

    call UpdateLocalDataShift

    call TranslateExpressions

    ;call ShowSource 0 7

    popad
EndP
____________________________________________________________________________________________
IsStringMaker:

    If_Or W$EPP_DestOperand <> 'ST', B$EPP_DestOperand+2 <> 'R', B$EPP_DestOperand+3 <> openSign
        mov eax &FALSE | ret
    End_If

    EPP_Code 'PUSH ESI|PUSH EDI|'

    lea esi D$EPP_DestOperand+4 | mov D$EPP_Operand1 esi
    If_And B$esi+1 = memMarker, B$esi <> 'D'
        error EqualPreparser_InvalidStringDest_Type
    Else_If_And B$esi >= '0', B$esi <= '9'
        error EqualPreparser_InvalidStringDest_Numeric
    End_If

    While B$esi > openSign | inc esi | End_While | mov B$esi Space

    EPP_Code 'MOV EDI #1|'
    mov ecx EPP_SourceEquation
    .While B$ecx > EOI
        If B$ecx = TextSign
            mov D$EPP_TextPointer ecx
            EPP_Code '{&1:B$ #text 0}|MOV ESI !1|&2:|CMP B$ESI 0|JZ &3|MOVSB|JMP !2|!3:|'
            inc ecx | While B$ecx > TextSign | inc ecx | End_While
            On B$ecx = TextSign, inc ecx

        Else
            mov D$EPP_Operand2 ecx
            EPP_Code 'MOV ESI #2|&2:|CMP B$ESI 0|JZ &3|MOVSB|JMP !2|!3:|'
            While B$ecx <> Space | inc ecx | End_While
        End_If
        While B$ecx = Space | inc ecx | End_While
    .End_While

    EPP_Code 'MOV B$EDI 0|POP EDI|POP ESI||'
    mov eax &TRUE

ret
____________________________________________________________________________________________
ReserveGlobalFunctionAdress:
    ; Initialize labels to be used later for global functions declaration and call. (not used)

    call CreateNoMeanlabel
    mov edi EPP_atofAddress | mov B$edi+8 0
    call CreateNoMeanlabel
    mov edi EPP_ftoaAddress | mov B$edi+8 0
ret
____________________________________________________________________________________________
Proc ComputeOperationsIn_EPPSourceEquation:
    ; Writes code from the most nested parenthesis to the least one.
    ; The parser search for the first couple of parentheses with no nested parenthesis inside and then
    ; computes the operations in the operator's priority order.
    ; When an operation immediately computable is found while searching for theses couples of parentheses, this
    ; operation is computed first. (See TestIfThisOperationIsComputable)
    ;
    ; This allows keep the very last results on the top of the FPU Stack, so that no move is needed.
    ; For instance with this equation:
    ; > T$res = (T$a-T$b)*T$e+(T$b+T$c)
    ;
    ; - if the operations between parentheses were computed first this would lead to
    ;       > T$res = ST1*T$e+ST0    ;with ST0=T$b+T$c ; ST1=T$a-T$b
    ;       > T$res = ST2*ST0+ST1    ;with ST0=T$e     ; ST1=T$b+T$c ; ST2=T$a-T$b
    ;   The first operation to compute is ST2*ST0, then ST2 must be copied on TOS with 'FLD ST2'
    ;
    ; - On the other hand, if we compute an operation as soon as is it possible, we have:
    ;       > T$res = ST0*T$e+(T$b+T$c)     ; with ST0=T$a-T$b
    ;       > T$res = ST0+(T$b+T$c)         ; with ST0=(T$a-T$b)*T$e
    ;   Here the '+' operation is not immediately computable, so the parsing continues to
    ;       > T$res = ST1+ST0               ; with ST1=(T$a-T$b)*T$e  ; ST0=T$b+T$c
    ;       > T$res = ST0                   ; with ST0=(T$a-T$b)*T$e+(T$b+T$c)
    ;   All the operations have been made on TOS :)
    Arguments @SecondPass
    Uses esi, edi

    ; add rounding parentheses
    mov edi EPP_SourceEquation
    call ShiftLineRight 1 | mov B$edi openSign
    While B$edi > EOI | inc edi | End_While | mov B$edi closeSign | mov B$edi+1 EOI

    mov eax &FALSE

    ; Checks the parentheses number.
    mov eax 0
    mov esi EPP_SourceEquation
    While B$esi > EOI
        On B$esi = openSign, inc eax
        On B$esi = closeSign, dec eax
        inc esi
    End_While
    On eax <> 0, error Parenthesis

    call FormatEqualLine
    On B$UseCommonAddressingSyntax = &TRUE, call SetCommonAddressingSyntax
    call EqualFirstScan D@SecondPass

    mov D$FPUStackNumber 0
    mov B$ParsingInsideArray &FALSE
    mov esi EPP_SourceEquation
    .While B$esi > EOI
        .If B$esi = openSign
            call CheckIfArray
            mov ebx esi | inc esi

        .Else_If B$esi = closeSign
            call WriteCodeForTheseParentheses ebx
            On eax = &FALSE, error EqualPreparser_UnexpectedError
            call IntegratedFunctionManagement ebx eax
            mov esi EPP_SourceEquation
            mov B$ParsingInsideArray &FALSE

        .Else_If B$esi = Space
            inc esi

        .Else_If_Or B$esi < OperatorSigns, B$esi = SignedMulSign, B$esi = SignedDivSign
            call TestIfThisOperationIsComputable esi
            If eax <> &FALSE
                call GetOperandTypeAndPos esi
                call WriteOperationCode eax ecx
                mov esi EPP_SourceEquation
                mov B$ParsingInsideArray &FALSE

            Else
                inc esi
            End_If
        .Else
            inc esi
        .End_If
    .End_While

EndP
____________________________________________________________________________________________
SetCommonAddressingSyntax:
; Look for memory data followed by an expression which can be an address
; [i.e. : Memory+register(+,*)constant+(register,constant)+constant)  ]
; and surround the expression with parentheses.

    mov esi EPP_SourceEquation
    .While B$esi > EOI
L1:     If_Or B$esi <= LowSigns, B$esi = SignedMulSign, B$esi = SignedDivSign
            inc esi
            jmp L1<
        End_If

        call GetStringType esi
        While B$esi > Space | inc esi | End_While | inc esi
        ...Test_If ax MemoryOperand
            ...If B$esi = addSign
                ..If_And B$esi+2 > LowSigns, B$esi+2 <> SignedMulSign, B$esi+2 <> SignedDivSign
                    add esi 2 | call GetStringType esi
                    ..Test_If ax constantOperand
                        ReplaceByAddressingOperator plusSign   ; Memory+constant

                    ..Test_Else_If ax registerOperand
                        ReplaceByAddressingOperator plusSign   ; Memory+register

                        While B$esi > Space | inc esi | End_While | inc esi
                        .If_Or B$esi = mulSign, B$esi = SignedMulSign
                            add esi 2 | call GetStringType esi
                            .Test_If ax ConstantOperand
                                ReplaceByAddressingOperator addressSign   ; Memory+register*constant

                                While B$esi > Space | inc esi | End_While | inc esi
                                If B$esi = addSign
                                    add esi 2 | call GetStringType esi
                                    Test_If ax ConstantOperand
                                        ReplaceByAddressingOperator plusSign   ; Memory+register*constant+constant
                                    Test_End
                                Else_If B$esi = subSign
                                    add esi 2 | call GetStringType esi
                                    Test_If ax ConstantOperand
                                        ReplaceByAddressingOperator minusSign   ; Memory+register*constant-constant
                                    Test_End
                                End_If
                            .Test_End

                        .Else_If B$esi = addSign
                            add esi 2 | call GetStringType esi
                            Test_If ax ConstantOperand
                                ReplaceByAddressingOperator plusSign    ; Memory+register+constant
                            Test_End
                        .Else_If B$esi = subSign
                            add esi 2 | call GetStringType esi
                            Test_If ax ConstantOperand
                                ReplaceByAddressingOperator minusSign    ; Memory+register-constant
                            Test_End
                        .End_If
                    ..Test_End
                ..End_If
            ...End_If
        ...Test_End
    .End_While

ret
____________________________________________________________________________________________
Proc GetStringType:
    ; D@StringPos points on the string whose type is to be found. (it can be terminated by EOI,meEOI,Space or &NULL)
    Arguments @StringPos
    Uses esi, ecx

    mov esi D@StringPos
    call IsARegister D@StringPos
    ..If eax = &FALSE
        .If B$esi+1 = memMarker
            or ax MemoryOperand
            If B$esi = 'T'
                or ax (FloatingPointOperand or TByteValue)
            Else_If B$esi = 'R'
                or ax (FloatingPointOperand or RealValue)
            Else_If B$esi = 'F'
                or ax (FloatingPointOperand or FloatValue)
            Else_If B$esi = 'Q'
                or ax QWordValue
            Else_If B$esi = 'D'
                or ax DWordValue
            Else_If B$esi = 'W'
                or ax WordValue
            Else_If B$esi = 'B'
                or ax ByteValue
            End_If
        .Else_If_And B$esi >= '0', B$esi <= '9'
            mov ecx esi
            While B$ecx > Space
                If B$ecx = pointSign
                    or ax FloatingPointOperand
                    jmp L1>
                Else
                    inc ecx
                End_If
            End_While
L1:         or ax (ConstantOperand or NumericOperand)
        .Else_If_And B$esi = minusSign, B$esi+1 >= '0', B$esi+1 <= '9'
            or ax (ConstantOperand or NumericOperand)
        .Else
            While B$esi > Space | inc esi | End_While | inc esi
            If B$esi = openSign
                or ax FunctionOperand
            Else
                or ax ConstantOperand
            End_If
        .End_If
    ..End_If

EndP
____________________________________________________________________________________________
Proc ProcessArrayDestination:
    ; If the destination operand is an array (ex: F$Array(ecx+3) ) The current state is saved and
    ; the address of the array is computed and stored in EDI (resp. ECX). Then the state of the current operation is
    ; restored with X$EDI (resp. X$ECX) as new destination operand (with X the size of array's elements)
    Local @AddressRegister

    mov D$DestinationArrayRegister &FALSE
    mov esi EPP_DestOperand
    .While B$esi > Space
        .If B$esi = openSign
            or B$UsedRegisters (EPP_EDI or EPP_ECX)
            call BackupFirstResults

            ; Copy address in EPP_SourceEquation
            mov edi EPP_SourceEquation | mov B$edi openSign | inc edi
            While B$esi > Space | movsb | End_While | mov B$edi closeSign | inc edi
            If B$EPP_DestOperand+2 <> openSign
                mov B$edi mulSign | inc edi | call GetAddressingCoef EPP_DestOperand | mov B$edi al | inc edi
                mov B$edi addSign | inc edi
                lea esi D$EPP_DestOperand+2 | While B$esi <> openSign | movsb | End_While
            End_If
            mov B$edi EOI

            ; defines addressing register
            If_Or W$EPP_SourceEquation_Backup+1 = 'CX', W$EPP_SourceEquation_Backup+1 = 'CH',
                                                        W$EPP_SourceEquation_Backup+1 = 'CL'
                jmp L1>
            Else_If_And W$EPP_SourceEquation_Backup+2 = 'CX', B$EPP_SourceEquation_Backup+1 = 'E'
                jmp L1>
            Else_If_And W$EPP_SourceEquation_Backup+4 = 'CX', B$EPP_SourceEquation_Backup+3 = 'E',
                                                              B$EPP_SourceEquation_Backup+2 = memMarker
L1:             mov D@AddressRegister 'EDI'
                mov D$DestinationArrayRegister EPP_EDI
            Else
                mov D@AddressRegister 'ECX'
                mov D$DestinationArrayRegister EPP_ECX
            End_If
            move D$EPP_DestOperand D@AddressRegister | mov B$EPP_DestOperand+3 Space

            mov al B$UsedRegisters_Backup | mov B$UsedRegisters al
            call WriteRestoringCodeForUsedRegisters

            call ComputeOperationsIn_EPPSourceEquation &TRUE

            ; Stores result in EDI/ECX
            mov D$EPP_LastStorageStatement_InMemory StrNull
            mov D$EPP_LastStorageStatement          StrNull
            mov D$EPP_Operand1 EPP_SourceEquation | inc D$EPP_Operand1
            mov D$EPP_Operand2 EPP_DestOperand
            mov edi EPP_SourceEquation | call StoreIn32BitsRegister
            call ImmediateCodeParser D$EPP_LastStorageStatement_InMemory
            call ImmediateCodeParser D$EPP_LastStorageStatement

            call RestoreFirstResults
            move D$EPP_DestOperand+2 D@AddressRegister | mov B$EPP_DestOperand+5 Space

            ExitP
        .End_If

        inc esi
    .End_While

EndP
____________________________________________________________________________________________
WriteRestoringCodeForUsedRegisters:
    ; This writes the code which restores from the stack the registers which have been used
    ; during the computation of the equation.

    mov cl minusSign
    mov bx '04'

    Test_If B$UsedRegisters EPP_EDI
        EPP_Code 'MOV EDI D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_ECX
        EPP_Code 'MOV ECX D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_EAX
        On bh > '9', add bh 7
        EPP_Code 'MOV EAX D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_EBX
        On bh = 010, mov bl '1', bh '0'
        EPP_Code 'MOV EBX D$ESI' cl bx '|'
        add bh 4
    Test_End

    Test_If B$UsedRegisters EPP_EDX
        On bh = 010, mov bl '1', bh '0'
        On bh > '9', add bh 7
        EPP_Code 'MOV EDX D$ESI' cl bx '|'
    Test_End


ret
____________________________________________________________________________________________
Proc BackupFirstResults:
    Uses esi, edi

    mov esi EPP_DestOperand
    mov edi EPP_DestOperand_Backup
    While B$esi > Space | movsb | End_While

    mov esi EPP_SourceEquation
    mov edi EPP_SourceEquation_Backup
    While B$esi > EOI | movsb | End_While | mov B$edi EOI

    mov al B$UsedRegisters | mov B$UsedRegisters_Backup al
EndP
____________________________________________________________________________________________
Proc RestoreFirstResults:
    Uses esi, edi

    mov esi EPP_DestOperand_Backup
    mov edi EPP_DestOperand
    While B$esi > Space | movsb | End_While

    mov esi EPP_SourceEquation_Backup
    mov edi EPP_SourceEquation
    While B$esi > EOI | movsb | End_While | mov B$edi EOI

    mov al B$UsedRegisters_Backup | mov B$UsedRegisters al
EndP
____________________________________________________________________________________________
Proc CheckIfArray:
    ; Sets B$ParsingInsideArray to &TRUE, if the last parenthesis is after an array.
    Uses esi

    dec esi
    If_And esi <> EPP_SourceEquation, B$esi-1 > openSign
        dec esi | While B$esi > Space | dec esi | End_While
        On B$esi+2 = memMarker, mov B$ParsingInsideArray &TRUE
    End_If

EndP
____________________________________________________________________________________________
Proc TestIfThisOperationIsComputable:
    ; return &FALSE if the operation at D@OperatorPos has NOT the priority to the next one.
    ; otherwise the operator is returned in eax and its sign in ecx.
    ; ex: eax = 8+3*eax+(7-D$a)/2
    ; - if D@OperatorPos points on '*', the function returns '*' (i.e. mulSign) and &FALSE in ecx (unsigned operation)
    ; - if D@OperatorPos points on the first '+', the function returns &FALSE
    ;
    Arguments @OperatorPos
    Uses ebx, esi

    mov esi D@OperatorPos
    call GetSignPriority D$esi | mov bl al

    add esi 2 | While B$esi <> Space | inc esi | End_While | inc esi
    call GetSignPriority D$esi

    mov esi D@OperatorPos
    .If bl >= al
        If B$esi = SignedMulSign
            mov ecx &TRUE
            mov eax mulSign
        Else_If B$esi = SignedDivSign
            mov ecx &TRUE
            mov eax divSign
        Else
            mov ecx &FALSE
            movzx eax B$esi
        End_If
    .Else
        mov eax &FALSE
    .End_If

EndP
____________________________________________________________________________________________
Proc GetSignPriority:
    Argument @Sign

    If B@Sign = closeSign
        mov al 0
    Else_If_Or B@Sign = addSign, B@Sign = subSign
        mov al 1
    Else_If_Or B@Sign = mulSign, B@Sign = divSign, B@Sign = SignedMulSign, B@Sign = SignedDivSign
        mov al 2
    Else_If_Or B@Sign = expSign
        mov al 3
    Else
        mov al 255
    End_If
EndP
____________________________________________________________________________________________
IsItaCall:
; if the equation is a CALL, the result (EAX) is stored in memory and then written in the Destination operand.
    mov esi EPP_SourceEquation
    .If D$esi = 'CALL'
        EPP_Code 'PUSH ESI|MOV ESI ESP|'

        mov edi D$EPP_WrittenCode
        While B$esi > EOI | movsb | End_While | mov B$edi meEOI | inc edi
        mov D$EPP_WrittenCode edi

        EPP_Code 'SUB ESP 04|{?? ESI-04}|MOV D$? EAX|'
        mov B$EPP_SourceEquation Space | mov B$EPP_SourceEquation+1 'D' | mov B$EPP_SourceEquation+2 memMarker
        lea edi D$EPP_SourceEquation+3 | call WriteNoMeanLabel | mov B$edi Space | mov B$edi+1 EOI

        call ProcessArrayDestination
        call StoreResultInFirstOperand

        mov B$UsedRegisters EPP_ESI
        EPP_FindCodePos 'CALL' EPP_CODE_BEGIN EPP_CODE_END
        call OptimizeEqualPreparserCode eax EPP_CODE_END

        call TranslateExpressions
        mov eax &TRUE
    .Else
        mov eax &FALSE
    .End_If
ret
____________________________________________________________________________________________
Proc ImmediateCodeParser:
    ; Writes the given code at edi.
    ; special chars:
    ;  '||'    => EOI
    ;  '??'    => Writes a new meaningless label
    ;  '?'     => Writes the current meaningless label
    ;  '#n'    => Writes the operand n (1 or 2)
    ;  '#atof' => Writes the address of the AsciiToFloat conversion function (not used)
    ;  '#ftoa' => Writes the address of the FloatToAscii conversion function (not used)
    ;  '#text' => Writes the text pointed by D$EPP_TextPointer (D$EPP_TextPointer points on the opening TextSign)
    ;  '&n'    => Writes a new meaningless label and store it the n-th internal variable
    ;             (0 to 7, See EPP_InternalMeaninglessLabels)
    ;             &7 is used for FPU state adresss (See EqualFirstScan)
    ;  '!n'    => Writes the n-th meaningless label
    ;
    ;
    ;  - the character erased by the security-EOI at the end of the string is
    ;    returned in AL (in case it must be rewritten)
    Arguments @Content
    Uses ecx, esi, edi, ebx

    mov esi D@Content
    mov edi D$EPP_WrittenCode
    .While B$esi <> 0
        ...If B$esi = ' '
            mov B$edi Space | inc edi, esi
        ...Else_If W$esi = '||'
            mov B$edi EOI | inc edi | add esi 2
        ...Else_If B$esi = '+'
            mov B$edi plusSign | inc esi,edi
        ...Else_If B$esi = '-'
            mov B$edi minusSign | inc esi,edi
        ...Else_If B$esi = '|'
            mov B$edi meEOI | inc edi, esi
        ...Else_If B$esi = ':'
            mov B$edi colonSign | inc edi, esi
        ...Else_If B$esi = '$'
            mov B$edi memMarker | inc edi, esi
        ...Else_If B$esi = '&'
            call CreateNoMeanLabel
            push edi
                movzx eax B$esi+1 | sub eax '0' | mov ebx 9 | mul ebx
                lea edi D$EPP_InternalMeaninglessLabels+eax
                mov B$edi+8 0
                call WriteNoMeanLabel
            pop edi
            call WriteNoMeanLabel
            add esi 2
        ...Else_If B$esi = '!'
            push esi
                movzx eax B$esi+1 | sub eax '0' | mov ebx 9 | mul ebx
                lea esi D$EPP_InternalMeaninglessLabels+eax
                While B$esi <> 0 | movsb | End_While
            pop esi
            add esi 2
        ...Else_If B$esi = '?'
            If B$esi+1 = '?' | call CreateNoMeanLabel  | inc esi | End_If
            call WriteNoMeanLabel
            inc esi
        ...Else_If B$esi = '#'
            ..If B$esi+1 <> '#'
                .If D$esi+1 <> 'text'
                    If B$esi+1 = '1'
                        mov ecx D$EPP_Operand1 | add esi 2
                    Else_If B$esi+1 = '2'
                        mov ecx D$EPP_Operand2 | add esi 2
                    Else_If D$esi+1 = 'atof'
                        mov ecx D$EPP_atofAddress  | add esi 5
                    Else_If D$esi+1 = 'ftoa'
                        mov ecx D$EPP_ftoaAddress  | add esi 5
                    End_If
                    While B$ecx > Space | mov al B$ecx | mov B$edi al | inc ecx, edi | End_While
                .Else
                    mov ecx D$EPP_TextPointer | mov B$edi TextSign | inc ecx, edi
                    While B$ecx > TextSign | mov al B$ecx | mov B$edi al | inc ecx, edi | End_While
                    mov B$edi TextSign | inc ecx, edi
                    add esi 5
                .End_If
            ..Else
                mov B$edi numSign | inc edi
                add esi 2
            ..End_If
        ...Else
            movsb
        ...End_If
    .End_While

    mov al B$edi
    mov B$edi EOI
    mov D$EPP_WrittenCode edi

EndP
____________________________________________________________________________________________
Proc ReplaceOperand:
    ; Replaces the given operand by
    ;   - the current meaningless label if D@Replacement = 0
    ;   - the content of D@Replacement if D@Replacement <> 0  (a string of 1,2,3 or 4 characters)
    ; When D@Datatype = 'X', the new operand is prefixed by 'X$' (0 for no prefix)
    ; When D@operand = 0, both operands and their operator will be deleted.
    ; If the position of the second operand has changed and if both operands points into EPP_SourceEquation
    ; then D$EPP_Operand2 is updated.
    ;
    ; ex:  - ReplaceOperand 0 'B' 'ECX' ; => replaces both operands by 'B$ECX'
    ;      - ReplaceOperand 1 'W' 0     ; => replaces the first operand by 'W$AAAAAAAA'
    ;      - ReplaceOperand 2 0 'EDI'   ; => replaces the second operand by 'EDI'
    ;
    Arguments  @operand @DataType @Replacement
    Local @UpdateOperand2
    Uses edi, ecx, esi

    .If D@operand = 2
        mov edi D$EPP_Operand2
        mov D@UpdateOperand2 &FALSE
    .Else
        mov edi D$EPP_Operand1
        mov ecx D$EPP_Operand2
        If_Or edi < EPP_SourceEquation, edi > EPP_SourceEquation_End,
              ecx < EPP_SourceEquation, ecx > EPP_SourceEquation_End
            mov D@UpdateOperand2 &FALSE
        Else
            mov D@UpdateOperand2 &TRUE
        End_If
    .End_If

    mov ecx 0
    While B$edi > Space | inc edi, ecx | End_While
    If D@operand = 0
        add edi 3 | add ecx 3
        While B$edi > Space | inc edi, ecx | End_While
    End_If

    call ShiftLineLeft ecx
    sub edi ecx
    On D@UpdateOperand2 = &TRUE, sub D$EPP_Operand2 ecx

    ..If D@Replacement = 0
        If D@DataType <> 0
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 10
            call ShiftLineRight 10
            move D$edi D@DataType | mov B$edi+1 memMarker | add edi 2
            call WriteNoMeanLabel
        Else
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 8
            call ShiftLineRight 8
            call WriteNoMeanLabel
        End_If
    ..Else
        mov ecx 0 | lea esi D@Replacement
        While B$esi <> 0
            On ecx = 4, jmp L1>
            inc ecx, esi
        End_While
L1:     .If D@DataType <> 0
            add ecx 2
            call ShiftLineRight ecx
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 ecx
            mov al B@DataType | mov B$edi al | mov B$edi+1 memMarker | add edi 2
        .Else
            call ShiftLineRight ecx
            On D@UpdateOperand2 = &TRUE, add D$EPP_Operand2 ecx
        .End_If

        lea esi D@Replacement
        mov ecx 0
        While B$esi <> 0
            On ecx = 4, jmp L1>
            movsb | inc ecx
        End_While
L1:
   ..End_If
EndP
____________________________________________________________________________________________
Proc WriteCodeForTheseParentheses:
    ; ^ then is first searched between the parentheses then * and / and finally + and - (to keep priority)
    ; if nothing is found, we check if there is no space between the parentheses (i.e there is either
    ; an equate or an immediate number (integer or not), and when no space is found the
    ; parentheses are removed, otherwise : error)
    Argument @OpeningParenthesis
    Uses ebx

    mov eax 0
    mov ecx D@OpeningParenthesis
    .While B$ecx <> closeSign
        If B$ecx = expSign
            call GetOperandTypeAndPos ecx
            call WriteOperationCode expsign &FALSE
            On eax = &TRUE, call RemoveParentheses
            mov ecx D@OpeningParenthesis
        End_If
        On eax = &TRUE, ExitP
        inc ecx
    .End_While

    mov eax 0
    mov ecx D@OpeningParenthesis
    .While B$ecx <> closeSign
        If B$ecx = mulSign
            mov eax &FALSE
            jmp L1>
        Else_If B$ecx = SignedMulSign
            mov eax &TRUE
L1:         call GetOperandTypeAndPos ecx
            call WriteOperationCode mulSign eax
            On eax = &TRUE, call RemoveParentheses
            mov ecx D@OpeningParenthesis
        Else_If B$ecx = divSign
            mov eax &FALSE
            jmp L1>
        Else_If B$ecx = SignedDivSign
            mov eax &TRUE
L1:         call GetOperandTypeAndPos ecx
            call WriteOperationCode divSign eax
            On eax = &TRUE, call RemoveParentheses
            mov ecx D@OpeningParenthesis
        End_If
        On eax = &TRUE, ExitP
        inc ecx
    .End_While


    mov eax 0
    mov ecx D@OpeningParenthesis
    .While B$ecx <> closeSign
        If B$ecx = addSign
            call GetOperandTypeAndPos ecx
            call WriteOperationCode addsign &FALSE
            On eax = &TRUE, call RemoveParentheses
            mov ecx D@OpeningParenthesis
        Else_If B$ecx = subSign
            call GetOperandTypeAndPos ecx
            call WriteOperationCode subSign &FALSE
            On eax = &TRUE, call RemoveParentheses
            mov ecx D@OpeningParenthesis
        End_If
        On eax = &TRUE, ExitP
        inc ecx
    .End_While


    mov ecx D@OpeningParenthesis | add ecx 2
    mov D$EPP_Operand1 ecx
    mov eax 0

    While B$ecx <> closeSign
        If B$ecx = Space
            On B$ecx+1 <> closeSign, jmp L9>        ; space found => error
        End_If

        On B$ecx = pointSign, mov eax &TRUE       ; Is this a floating point number

        inc ecx
    End_While

    .If eax = &TRUE
        dec D@OpeningParenthesis
        If D@OpeningParenthesis = EPP_SourceEquation  ; saves the FP number in memory only if there is no more operation.
            EPP_Code '{??:R$ #1}|'
            call ReplaceOperand 1 'R' 0
        End_If
    .End_If

    call RemoveParentheses
    If eax = &TRUE
        mov eax EPP_NO_OPERATION
        ExitP
    End_If

L9:
    mov eax &FALSE

EndP
____________________________________________________________________________________________
ShiftLineRight:
    ; shift right the line (until EOI) pointed by edi of X bytes.
    push esi edi ecx
        mov ecx 1
        While B$edi <> EOI | inc edi ecx | End_While
        mov esi edi
        push ecx edi
            add edi D$esp+24
            std
                rep movsb
            cld
        pop edi ecx
        If ecx <= D$esp+16   ; erase the last EOI(s) if it was not previously overwritten by the shift.
            mov esi D$esp+16 | sub esi ecx
            mov ecx 0
            While ecx <= esi | mov B$edi+ecx Space | inc ecx | End_While
        End_If
    pop ecx edi esi
ret 4
____________________________________________________________________________________________
Proc ShiftLineLeft:
    ; shift left the line (until EOI) pointed by edi of D@shift bytes.
    Argument @shift
    Local @EndOfLine
    Uses esi edi ecx

    mov ecx 1
    mov esi edi
    While B$edi <> EOI | inc edi ecx | End_While | mov D@EndOfLine edi
    mov edi esi | sub edi D@shift
    rep movsb

    mov esi D@EndOfLine | sub esi D@shift | inc esi
    While esi <= D@EndOfLine | mov B$esi 0 | inc esi | End_While
EndP
____________________________________________________________________________________________
Proc GetOperandTypeAndPos:
    ;The type of each operand is identified and stored in as flags. (See EPP_Types)
    ;BX contains the type of the first operand and DX contains the type of the second one.
    ;The position of the first character of each operand is stored in D$EPP_Operand1 and D$EPP_Operand1
    Argument @OperatorPos
    Uses eax

    mov ebx 0
    mov edx 0

    mov eax D@OperatorPos | sub eax 2
    While B$eax > Space | dec eax | End_While | inc eax | mov D$EPP_Operand1 eax
    call GetStringType eax
    mov bx ax

    mov eax D@OperatorPos | add eax 2 | mov D$EPP_Operand2 eax
    call GetStringType eax
    mov dx ax

EndP
____________________________________________________________________________________________
Proc WriteOperationCode:
  ; D@OperationType can be divSign,mulSign,addSign or subSign
  ; BX contains the type of the first operand and DX contains the type of the second one. (See GetOperandTypeAndPos)
  ; return &TRUE if code has been written.
  Arguments @OperationType @Signed
  Uses D$DestinationType

    If B$ParsingInsideArray = &TRUE
        mov D$DestinationType (MemoryOperand or DWordValue)
    End_If

    Test_If_And bx NumericOperand, dx NumericOperand
        call ComputeImmediateExpression D@OperationType
        mov eax &FALSE

    Test_Else_If_Not_And bx FloatingPointOperand, dx FloatingPointOperand, D$DestinationType FloatingPointOperand
        call ReplaceIndirectAddressing
        call WriteIntegerOperationCode D@OperationType D@Signed
        mov eax &TRUE

    Test_Else ; at least one operand non-integer
        call ReplaceIndirectAddressing
        call WriteFloatingPointOperationCode D@OperationType
        call StoreFPUResult 0
        mov eax &TRUE

    Test_End

EndP
____________________________________________________________________________________________
Proc StoreFPUResult:
; The result of the FP operation is either left on the FPU Stack or stored in memory.
; In the first case, the operand is replaced by 'st0' (in lowercase so as to be locatable
; afterwards (see incFPUStackNumber)) otherwise it is replaced by a meaningless label.
    Argument @operand

    If D$FPUStackNumber < 6
        call ReplaceOperand D@operand 0 'st0'
    Else
        EPP_Code 'SUB ESP 0A|{?? ESI-000}|FSTP T$?|'

        call ReplaceOperand D@operand 'T' 0
        call decFPUStackNumber
    End_If

EndP
____________________________________________________________________________________________
; As the temporary FP result are stored in the FPU Stack (ST0-ST6), each time a new result is stored the
; last ones are pushed on the stack. Theses two functions update the temporary FP result on the stack
; by incrementing or decrementing the register number.
; The registers to be updated are identified by the 'st' prefix (instead of 'ST').
Proc incFPUStackNumber:
    Uses ecx

    mov ecx EPP_SOURCE_BEGIN
    jmp L1>
    While eax <> CODE_NOT_FOUND
        inc B$eax+2 | lea ecx D$eax+1
L1:     EPP_FindCodePos 'st' ecx EPP_SOURCE_END
    End_While
    inc D$FPUStackNumber

EndP
                            __________________________________________________
Proc decFPUStackNumber:
    Uses ecx

    mov ecx EPP_SOURCE_BEGIN
    jmp L1>
    While eax <> CODE_NOT_FOUND
        dec B$eax+2 | lea ecx D$eax+1
L1:     EPP_FindCodePos 'st' ecx EPP_SOURCE_END
    End_While
    dec D$FPUStackNumber

EndP
____________________________________________________________________________________________
Proc ReplaceIndirectAddressing:
    ; Replaces in EPP_SourceEquation, the operand of this type: 'F$D$AAAAAAA' (which are written when an array is met)
    ; by 'F$ECX' , then the code to move 'MOV ECX D$AAAAAAAA' is written

    call ExtractIndirectAddress 1 EPP_ECX
    If eax = &TRUE
        call ExtractIndirectAddress 2 EPP_EDI
    Else
        call ExtractIndirectAddress 2 EPP_ECX
    End_If

EndP

____________________________________________________________________________________________
Proc ExtractIndirectAddress:
    ; Extracts the address (which is stored in a memory DWord) of the given operand (1 or 2),
    ; move it into the given register and then replace the operand.
    ; D@register can be (EPP_EAX,EPP_EBX,EPP_ECX,EPP_EDX,EPP_ESI,EPP_EDI)
    ;
    ; return &FALSE if no extraction was done on the given operand, &TRUE otherwise.
    Argument @Operand @register
    Uses esi, edi, edx

    If D@Operand = 1
        mov esi D$EPP_Operand1
    Else
        mov esi D$EPP_Operand2
    End_If

    .If_And B$esi+1 = memMarker, B$esi+2 = 'D', B$esi+3 = memMarker
        EPP_Code 'MOV ' | call WriteRegister D@Register | mov edx eax
        push D$EPP_Operand1
            mov D$EPP_Operand1 esi | add D$EPP_Operand1 2
            EPP_Code ' #1|'
        pop D$EPP_Operand1

        call ReplaceOperand D@Operand D$esi edx
        mov eax &TRUE
    .Else
        mov eax &FALSE
    .End_If

EndP
____________________________________________________________________________________________
Proc WriteRegister:
    Argument @register

    If D@register = EPP_EAX
        EPP_CODE 'EAX'
        mov eax 'EAX'
    Else_If D@register = EPP_EBX
        EPP_CODE 'EBX'
        mov eax 'EBX'
    Else_If D@register = EPP_ECX
        EPP_CODE 'ECX'
        mov eax 'ECX'
    Else_If D@register = EPP_EDX
        EPP_CODE 'EDX'
        mov eax 'EDX'
    Else_If D@register = EPP_ESI
        EPP_CODE 'ESI'
        mov eax 'ESI'
    Else_If D@register = EPP_EDI
        EPP_CODE 'EDI'
        mov eax 'EDI'
    End_If
EndP
____________________________________________________________________________________________
Proc WriteAddressingCode:
    ; Writes the code which is before an array. Basically this translate F$Array(6) to F$Array+6*4
    Argument @DataType


    .If B@DataType = 'T'
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV EBX 10|MUL EBX|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'R'
        EPP_Code 'MOV EAX #1|SHL EAX 3|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'F'
        EPP_Code 'MOV EAX #1|SHL EAX 2|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'B'
        EPP_Code 'MOV EAX #1|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'D'
        EPP_Code 'MOV EAX #1|SHL EAX 2|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'W'
        EPP_Code 'MOV EAX #1|SHL EAX 1|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .Else_If B@DataType = 'Q'
        EPP_Code 'MOV EAX #1|SHL EAX 3|SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
    .End_If

    add D$EPP_Operand2 2 | EPP_Code 'ADD D$? #2|' | sub D$EPP_Operand2 2
    call ReplaceOperand 1 'D' 0
EndP
____________________________________________________________________________________________
Proc RemoveParentheses:
    ; Return &TRUE if the two parentheses surrounding EPP_Operand1 have been removed from EPP_SourceEquation,
    ; &FALSE otherwise.
    Uses edi, ecx

    mov eax &FALSE
    mov edi D$EPP_Operand1

    .If B$edi-2 = openSign
        mov ecx 0
        While B$edi+ecx > Space | inc ecx | End_While | dec ecx
        If B$edi+ecx+2 = closeSign
            call ShiftLineLeft 2
            lea edi D$edi+ecx+(3-2)
            call ShiftLineLeft 2
            mov eax &TRUE    ; the parentheses have been removed
        End_If
    .End_If

EndP
____________________________________________________________________________________________
TranslateExpressions:
    ; This part converts characters used by the preparser to RosAsm characters.
    ; It is called once all modifications of the code have been performed.
    mov ecx D$EPP_CodeBegining
    .While B$ecx <> EOI
        If B$ecx = plusSign
            mov B$ecx addSign
        Else_If B$ecx = minusSign
            mov B$ecx subSign
        Else_If B$ecx = pointSign
            mov B$ecx '.'
        Else_If B$ecx =  addressSign
            mov B$ecx mulSign
        Else_If W$ecx = 'st'
            mov W$ecx 'ST'
        Else_If B$ecx = TextSign
            inc ecx | While B$ecx <> TextSign | inc ecx | End_While
        End_If
        inc ecx
    .End_While

ret
____________________________________________________________________________________________
Proc IntegerDivisionCode_DWORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> DWORD Destination
    or B$UsedRegisters EPP_EDX
    .Test_If_And bx ConstantOperand, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        EPP_Code 'MOV EDX 0|MOV EAX #1|' D@Symb_I 'DIV #2|'
    .Test_Else_If_And bx Word, dx DWordValue
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'DIV #2|'
    .Test_Else_If_And bx Word, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx DWord, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx DWordValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And  bx ByteValue, dx DWord
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'DIV #2|'

    .Test_Else_If_And bx ByteValue, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'DIV EBX|'
    .Test_Else_If_And bx DWordValue, dx DWordValue
        EPP_Code 'MOV EDX 0|MOV EAX #1|' D@Symb_I 'DIV #2|'
    .Test_End
    mov eax DWordValue
EndP
____________________________________________________________________________________________
Proc IntegerMultiplicationCode_DWORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> DWORD Destination
    or B$UsedRegisters EPP_EDX
    .Test_If_And bx ConstantOperand, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        EPP_Code 'MOV EAX #1|' D@Symb_I 'MUL #2|'
    .Test_Else_If_And bx Word, dx DWordValue
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'MUL #2|'
    .Test_Else_If_And bx Word, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx DWord, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx DWordValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And  bx ByteValue, dx DWord
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|' D@Symb_I 'MUL #2|'

    .Test_Else_If_And bx ByteValue, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X EAX #1|MOV' D@Symb_S 'X EBX #2|' D@Symb_I 'MUL EBX|'
    .Test_Else_If_And bx DWordValue, dx DWordValue
        EPP_Code 'MOV EAX #1|' D@Symb_I 'MUL #2|'
    .Test_End
    mov eax DWordValue
EndP
____________________________________________________________________________________________
Proc IntegerDivisionCode_WORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> WORD Destination
    .Test_If_And bx ConstantOperand, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        mov eax ByteValue
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV EDX 0|MOV AX #1|' D@Symb_I 'DIV #2|'
        mov eax WordValue
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        mov eax ByteValue
    .Test_Else_If_And bx DWord, dx ByteValue
        ; error

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters (EPP_EBX or EPP_EDX)
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X AX #1|MOV BX #2|' D@Symb_I 'DIV BX|'
        mov eax WordValue
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters (EPP_EBX or EPP_EDX)
        EPP_Code 'MOV EDX 0|MOV AX #1|MOV BX #2|' D@Symb_I 'DIV BX|'
        mov eax WordValue
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        ; error
    .Test_Else_If_And  bx DWordValue, dx Word
        or B$UsedRegisters (EPP_EBX or EPP_EDX)
        EPP_Code 'LEA EAX #1|MOV DX W$EAX+2|MOV AX W$EAX|' D@Symb_I 'DIV #2|'
        mov eax WordValue
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV EDX 0|MOV' D@Symb_S 'X AX #1|' D@Symb_I 'DIV #2|'
        mov eax WordValue
    .Test_Else_If_And  bx ByteValue, dx DWord
        ; error

    .Test_Else_If_And bx ByteValue, dx ByteValue
        EPP_Code 'MOV' D@Symb_S 'X AX #1|' D@Symb_I 'DIV #2|'
        mov eax ByteValue
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV EDX 0|MOV AX #1|' D@Symb_I 'DIV #2|'
        mov eax WordValue
    .Test_Else_If_And bx DWordValue, dx DWordValue
        ; error
    .Test_End

EndP
____________________________________________________________________________________________
Proc IntegerMultiplicationCode_WORD_Destination:
    Arguments @Symb_S @Symb_I

    ; >> WORD Destination
    .Test_If_And bx ConstantOperand, dx ByteValue
        EPP_Code 'MOV AL #1|' D@Symb_I 'MUL #2|'
        mov eax WordValue
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV AX #1|' D@Symb_I 'MUL #2|'
        mov eax WordValue
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx ByteValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV' D@Symb_S 'X AX #2|' D@Symb_I 'MUL #1|'
        mov eax WordValue
    .Test_Else_If_And bx DWord, dx ByteValue
        ; error

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        EPP_Code 'MOV AL #2|' D@Symb_I 'MUL #1|'
        mov eax WordValue
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV AX #2|' D@Symb_I 'MUL #1|'
        mov eax WordValue
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        ; error
    .Test_Else_If_And  bx DWordValue, dx Word
        ; error
    .Test_Else_If_And  bx ByteValue, dx Word
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV' D@Symb_S 'X AX #1|' D@Symb_I 'MUL #2|'
        mov eax WordValue
    .Test_Else_If_And  bx ByteValue, dx DWord
        ; error

    .Test_Else_If_And bx ByteValue, dx ByteValue
        EPP_Code 'MOV AL #1|' D@Symb_I 'MUL #2|'
        mov eax WordValue
    .Test_Else_If_And bx WordValue, dx WordValue
        or B$UsedRegisters EPP_EDX
        EPP_Code 'MOV AX #1|' D@Symb_I 'MUL #2|'
        mov eax WordValue
    .Test_Else_If_And bx DWordValue, dx DWordValue
        ; error
    .Test_End

EndP
____________________________________________________________________________________________
Proc IntegerDivisionCode_BYTE_Destination:
    Arguments @Symb_S @Symb_I

    ; >> BYTE Destination
    .Test_If_And bx ConstantOperand, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        mov eax ByteValue
    .Test_Else_If_And bx ConstantOperand, dx WordValue
        ; error
    .Test_Else_If_And bx ConstantOperand, dx DWordValue
                                                                                                                                                                                                                                                                                                                                                                                                                                        ; error
    .Test_Else_If_And bx Word, dx DWordValue
        ; error
    .Test_Else_If_And bx Word, dx ByteValue
        EPP_Code 'MOV AX #1|' D@Symb_I 'DIV #2|'
        mov eax ByteValue
    .Test_Else_If_And bx DWord, dx ByteValue
        ; error

    .Test_Else_If_And bx ByteValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X AX #1|MOV BL #2|' D@Symb_I 'DIV BL|'
        mov eax ByteValue
    .Test_Else_If_And bx WordValue, dx ConstantOperand
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV AX #1|MOV BL #2|' D@Symb_I 'DIV BL|'
        mov eax ByteValue
    .Test_Else_If_And  bx DWordValue, dx ConstantOperand
        ; error
    .Test_Else_If_And  bx DWordValue, dx Word
        ; error
    .Test_Else_If_And  bx ByteValue, dx Word
        ; error
    .Test_Else_If_And  bx ByteValue, dx DWord
        ; error

    .Test_Else_If_And bx ByteValue, dx ByteValue
        or B$UsedRegisters EPP_EBX
        EPP_Code 'MOV' D@Symb_S 'X AX #1||' D@Symb_I 'DIV #2|'
        mov eax ByteValue
    .Test_Else_If_And bx WordValue, dx WordValue
        ; error '
    .Test_Else_If_And bx DWordValue, dx DWordValue
        ; error
    .Test_End

EndP
____________________________________________________________________________________________
Proc WriteIntegerOperationCode:
    ; BX contains the type of the first operand and DX contains the type of the second one.
    ; D@Symb_I and D@Symb_S contain the letters which changes in mnemonics from a signed to
    ; an unsigned operation. This way, no further test of the sign is needed.
    Arguments @OperationType @Signed
    Local @Symb_I @Symb_S @OperationString
    Uses ecx, esi

    If D@Signed = &TRUE
        mov D@Symb_I 'I'
        mov D@Symb_S 'S'
    Else
        mov D@Symb_I ''
        mov D@Symb_S 'Z'
    End_If

    mov eax 0_FF00_0000 ; error check

    .If D@OperationType = divSign
        or B$UsedRegisters EPP_EAX
        Test_If D$DestinationType DWordValue
            call IntegerDivisionCode_DWORD_Destination D@Symb_S D@Symb_I

        Test_Else_If D$DestinationType WordValue
            call IntegerDivisionCode_WORD_Destination D@Symb_S D@Symb_I

        Test_Else D$DestinationType ByteValue
            call IntegerDivisionCode_BYTE_Destination D@Symb_S D@Symb_I

        Test_End

    ; >> MULTIPLICATION
    .Else_If D@OperationType = mulSign
        or B$UsedRegisters EPP_EAX
        Test_If D$DestinationType DWordValue
            call IntegerMultiplicationCode_DWORD_Destination D@Symb_S D@Symb_I

        Test_Else_If D$DestinationType WordValue
            call IntegerMultiplicationCode_WORD_Destination D@Symb_S D@Symb_I

        Test_Else D$DestinationType ByteValue
            ; error

        Test_End

    ; >> POWER
    .Else_If D@OperationType = expSign
        or B$UsedRegisters (EPP_EAX or EPP_EBX or EPP_ECX)
        mov cx 0 | Test_If_Not_And bx DWordValue, bx ConstantOperand | mov cl B@Symb_S | mov ch 'X' | Test_End
        Test_If dx DWordValue
            mov esi 'ECX'
        Test_Else_If dx WordValue
            mov esi 'CX'
        Test_Else_If dx WordValue
            mov esi 'CL'
        Test_Else ; constant
            mov esi 'ECX'
        Test_End

        EPP_Code 'MOV ECX 0|MOV EAX 1|MOV' cx ' EBX #1|&1:|CMP ' esi ' #2|JE &2|'
        EPP_Code  D@Symb_I 'MUL EBX|INC ECX|JMP !1|!2:|'

        mov eax DWordValue

    ; >> ADDITION AND SUBSTRACTION
    .Else_If_Or D@OperationType = addSign, D@OperationType = subSign
        or B$UsedRegisters EPP_EAX
        If D@OperationType = addSign
            mov D@OperationString 'ADD'
        Else
            mov D@OperationString 'SUB'
        End_If
        mov cx 0
        .Test_If D$DestinationType DWordValue
            Test_If_Not_And bx DWordValue, bx ConstantOperand | mov cl B@Symb_S | mov ch 'X' | Test_End
            EPP_Code 'MOV' cx ' EAX #1|' D@OperationString ' EAX #2|'
            mov eax DWordValue

        .Test_Else_If D$DestinationType WordValue
            Test_If_Not_And  bx WordValue, bx ConstantOperand | mov cl B@Symb_S | mov ch 'X' | Test_End
            EPP_Code 'MOV' cx ' AX #1|' D@OperationString ' AX #2|'
            mov eax WordValue

        .Test_Else_If D$DestinationType ByteValue
            EPP_Code 'MOV AL #1|' D@OperationString ' AL #2|'
            mov eax ByteValue

        .Test_End

    .EndIf

    ; stores result in memory
    .If eax = DWordValue
        EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? EAX|'
        call ReplaceOperand 0 'D' 0
    .Else_If eax = WordValue
        EPP_Code 'SUB ESP 02|{?? ESI-000}|MOV W$? AX|'
        call ReplaceOperand 0 'W' 0
    .Else_If eax = ByteValue
        EPP_Code 'SUB ESP 01|{?? ESI-000}|MOV B$? AL|'
        call ReplaceOperand 0 'B' 0
    .Else
        error EqualPreparser_SourceInvalid
    .End_If

EndP
____________________________________________________________________________________________
Proc WriteFloatingPointOperationCode:
    ; Writing of data loading according to its type.
    ; BX contains the type of the first operand and DX contains the type of the second one.
    Arguments @OperationType

    ; copy integers registers into memory and change the operand type to MemoryOperand
    ..Test_If bx RegisterOperand
        .Test_If_Not bx FloatingPointOperand
            and bx ((not RegisterOperand) and 0_FFFF)
            or bx MemoryOperand
            Test_If_Not bx DWordValue
                call ExtendOperandToDWord 1 MemoryOperand
            Test_Else
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|'
                call ReplaceOperand 1 'D' 0
            Test_End
        .Test_End
    ..Test_End

    ..Test_If dx RegisterOperand
        .Test_If_Not dx FloatingPointOperand
            and dx ((not RegisterOperand) and 0_FFFF)
            or dx MemoryOperand
            Test_If_Not dx DWordValue
                call ExtendOperandToDWord 1 MemoryOperand
            Test_Else
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #2|'
                call ReplaceOperand 2 'D' 0
            Test_End
        .Test_End
    ..Test_End


    ...Test_If_Not_And bx FloatingPointOperand, dx FloatingPointOperand
        ; only when the result is FP
        ..Test_If_Not_And bx ConstantOperand, dx ConstantOperand
            EPP_Code 'FILD #1|' | call incFPUStackNumber
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand
        ..Test_Else_If_And bx ConstantOperand, dx ConstantOperand
            EPP_Code '{??:R$ #1}|FLD R$?|{??:R$ #2}|FLD R$?|' | call incFPUStackNumber
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
        ..Test_Else_If bx ConstantOperand
            EPP_Code '{??:R$ #1}|FLD R$?|' | call incFPUStackNumber
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand
        ..Test_Else
            EPP_Code '{??:R$ #2}|FLD R$?|' | call incFPUStackNumber
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_IntegerOperand_ST0
        ..Test_End

    ...Test_Else_If bx ConstantOperand
        ..Test_If dx FloatingPointOperand
            ; #1 constant / #2 Floating Point
            ; ex: ( 51 - R$float1 )
            EPP_Code '{??:R$ #1}|'

            .Test_If dx TByteValue
                Test_If_Not dx RegisterOperand | EPP_Code 'FLD #2|' | Test_End
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ConstantOperand_ST0
            .Test_Else
                EPP_Code 'FLD R$?|' | call incFPUStackNumber
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_FloatingPointOperand
            .Test_End
        ..Test_Else
            ; #1 constant / #2 integer
            ; ex: ( 7.4 + D$val1 )
            EPP_Code '{??:R$ #1}|FLD R$?|' | call incFPUStackNumber
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand
        ..Test_End

    ...Test_Else_If dx ConstantOperand
        ..Test_If bx FloatingPointOperand
            ; #1 Floating Point / #2 constant
            ; ex: ( T$float2 / 8 )
            EPP_Code '{??:R$ #2}|'

            .Test_If bx TByteValue
                Test_If_Not bx RegisterOperand | EPP_Code 'FLD #1|' | Test_End
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_ConstantOperand
            .Test_Else
                EPP_Code 'FLD R$?|' | call incFPUStackNumber
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_FloatingPointOperand_ST0
            .Test_End
        ..Test_Else
            ; #1 integer / #2 constant
            ; ex: ( D$val1 + 7.4 )
            EPP_Code '{??:R$ #2}|FLD R$?|' | call incFPUStackNumber
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_IntegerOperand_ST0
        ..Test_End

    ...Test_Else ; Both operand non-constant :
        ..Test_If_Not bx FloatingPointOperand
            ; #1 integer / #2 Floating Point
            ; ex: ( D$val2 * F$float2 )
            Test_If_Not dx RegisterOperand | EPP_Code 'FLD #2|' | call incFPUStackNumber | Test_End
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_IntegerOperand_ST0

        ..Test_Else_If_Not dx FloatingPointOperand
            ; #1 Floating Point / #2 integer
            ; ex: ( R$float1 - W$val3 )
            Test_If_Not bx RegisterOperand | EPP_Code 'FLD #1|' | call incFPUStackNumber | Test_End
            call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_IntegerOperand

        ..Test_Else
            ; #1 Floating Point / #2 Floating Point
            ; ex: ( R$float2 + F$float5 )

            .Test_If_And bx TByteValue, dx TByteValue
                Test_If_And bx RegisterOperand, dx RegisterOperand
                    call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
                Test_Else_If dx RegisterOperand
                    EPP_Code 'FLD #1|' | call incFPUStackNumber
                    call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_ST1
                Test_Else_If bx RegisterOperand
                    EPP_Code 'FLD #2|' | call incFPUStackNumber
                    call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
                Test_Else
                    EPP_Code 'FLD #1|FLD #2|' | call incFPUStackNumber
                    call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST1_ST0
                Test_End

            .Test_Else_If bx TByteValue
                Test_If_Not bx RegisterOperand | EPP_Code 'FLD #1|' | call incFPUStackNumber | Test_End
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_FloatingPointOperand

            .Test_Else_If dx TByteValue
                Test_If_Not dx RegisterOperand | EPP_Code 'FLD #2|' | call incFPUStackNumber | Test_End
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_FloatingPointOperand_ST0

            .Test_Else
                EPP_Code 'FLD #1|' | call incFPUStackNumber
                call WriteFloatingOperationInstruction D@OperationType EqualPreparser_ST0_FloatingPointOperand

            .Test_End

        ..Test_End
    ...Test_End

EndP
____________________________________________________________________________________________
Proc StoreResultInFirstOperand:
  ; This function store the result of the operations in the destination operand.
    Uses edx, esi, edi

    mov edi EPP_SourceEquation
    mov ebx EPP_DestOperand
    mov D$EPP_Operand1 EPP_SourceEquation | inc D$EPP_Operand1
    mov D$EPP_Operand2 EPP_DestOperand

    If D$DestinationArrayRegister = EPP_ECX
        mov edx EPP_EDI
    Else_If D$DestinationArrayRegister = EPP_EDI
        mov edx EPP_ECX
    Else
        mov edx EPP_EDI
    End_If
    call ExtractIndirectAddress 1 edx
    If eax = &TRUE
        or B$UsedRegisters dl
    End_If

    If_And W$ebx = 'SI', B$ebx+2 < Separators
        mov D$ebx 'W$ES', B$ebx+1 memMarker, B$ebx+4 'I', B$ebx+5 plusSign, B$ebx+6 '4', B$ebx+7 Space
    Else_If_And W$ebx = 'ES', B$ebx+2 = 'I', B$ebx+3 < Separators
        mov D$ebx 'D$ES', B$ebx+1 memMarker, B$ebx+4 'I', B$ebx+5 plusSign, B$ebx+6 '4', B$ebx+7 Space
    End_If

    mov D$EPP_LastStorageStatement_InMemory StrNull
    mov D$EPP_LastStorageStatement          StrNull

    .If B$ebx+1 = memMarker
        call StoreInMemory
    .Else_If B$ebx+2 = Space
        ; 2 characters destination
        If B$ebx+1 = 'H'
            jmp L1>
        Else_If B$ebx+1 = 'L'
L1:         call StoreIn8BitsRegister
        Else
            call StoreIn16BitsRegister
        End_If
        call GetRegister ebx | not al | and B$UsedRegisters al
    .Else_If B$ebx+3 = Space
        ; 3 characters destination
        If B$ebx = 'E'
            call StoreIn32BitsRegister
            call GetRegister ebx | not al | and B$UsedRegisters al
        Else_If W$ebx = 'ST'
            call StoreInFPURegister
        End_If
    .Else
        error EqualPreparser_DestInvalid
    .End_If

    EPP_Code 'POP EDX|POP EBX|POP EAX|POP ECX|POP EDI|'
    call ImmediateCodeParser D$EPP_LastStorageStatement_InMemory
    EPP_Code  'MOV ESP ESI|POP ESI|'
    call ImmediateCodeParser D$EPP_LastStorageStatement
EndP
____________________________________________________________________________________________
Proc GetRegister:
    Argument @RegisterPos
    Uses esi

    mov esi D@RegisterPos
    On B$esi = 'E', inc esi

    If B$esi = 'A'
        mov eax EPP_EAX
    Else_If B$esi = 'B'
        mov eax EPP_EBX
    Else_If B$esi = 'C'
        mov eax EPP_ECX
    Else_If W$esi = 'DI'
        mov eax EPP_EDI
    Else_If W$esi = 'SI'
        mov eax EPP_ESI
    Else_If B$esi = 'D'
        mov eax EPP_EDX
    End_If

EndP
____________________________________________________________________________________________
Proc ExtendOperandToDWord:
    Argument @Operand @type
    or B$UsedRegisters EPP_EAX

    .If D@Operand = 1
        If D@type = MemoryOperand
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #1|MOV D$? EAX|'
            call ReplaceOperand 1 'D' 0
        Else
            EPP_Code 'MOVZX EAX #1|' | call ReplaceOperand 1 0 'EAX'
        End_If
    .Else
        If D@type = MemoryOperand
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #2|MOV D$? EAX|'
            call ReplaceOperand 2 'D' 0
        Else
            EPP_Code 'MOVZX EAX #2|' | call ReplaceOperand 2 0 'EAX'
        End_If
    .End_If

EndP
____________________________________________________________________________________________
Proc WriteFloatingOperationInstruction:
    Arguments @OperationType @OperandsType

    .Test_If D@OperandsType EqualPreparser_ST1_ST0
        .If D@OperationType = addSign
            EPP_Code 'FADDP ST1 ST0|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBP ST1 ST0|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMULP ST1 ST0|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVP ST1 ST0|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_ST1
        .If D@OperationType = addSign
            EPP_Code 'FADDP ST1 ST0|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBRP ST1 ST0|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMULP ST1 ST0|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVRP ST1 ST0|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_IntegerOperand_ST0
        .If D@OperationType = addSign
            EPP_Code 'FIADD #1|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FISUBR #1|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FIMUL #1|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FIDIVR #1|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FILD #1|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_IntegerOperand
        .If D@OperationType = addSign
            EPP_Code 'FIADD #2|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FISUB #2|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FIMUL #2|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FIDIV #2|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FILD #2|FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_FloatingPointOperand_ST0
        .If D@OperationType = addSign
            EPP_Code 'FADD #1|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBR #1|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL #1|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVR #1|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD #1|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_FloatingPointOperand
        .If D@OperationType = addSign
            EPP_Code 'FADD #2|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUB #2|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL #2|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIV #2|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD #2|FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ConstantOperand_ST0
        .If D@OperationType = addSign
            EPP_Code 'FADD R$?|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUBR R$?|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL R$?|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIVR R$?|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD R$?|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else_If D@OperandsType EqualPreparser_ST0_ConstantOperand
        .If D@OperationType = addSign
            EPP_Code 'FADD R$?|'
        .Else_If D@OperationType = subSign
            EPP_Code 'FSUB R$?|'
        .Else_If D@OperationType = mulSign
            EPP_Code 'FMUL R$?|'
        .Else_If D@OperationType = divSign
            EPP_Code 'FDIV R$?|'
        .Else_If D@OperationType = expSign
            EPP_Code 'FLD R$?|FXCH|'
            EPP_Code 'FYL2X|FLD ST0|FRNDINT|FSUB ST1 ST0|'
            EPP_Code 'FLD1|FSCALE|FSTP ST1|FXCH|F2XM1|'
            EPP_Code 'FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        .End_If

    .Test_Else
        error EqualPreParser_UnexpectedError
    .Test_End

EndP
____________________________________________________________________________________________
Proc IntegratedFunctionManagement:
    ; This handle Arrays and functions, respectively manged by WriteArrayCode and WriteFunctionCode
    ; At this point, if we have: _FUNCTION_PARAMETER_
    ; D@OpeningParenthesis points on PARAMETER.
    ; #1 is set on PARAMETER and #2 is set on FUNCTION
    Argument @OpeningParenthesis
    Local @FunctionNamePos @ParameterNamePos @FunctionNameLength
    Uses ebx, ecx, edi

    mov ebx D@OpeningParenthesis
    mov D$EPP_Operand1 ebx
    mov D@ParameterNamePos ebx

    dec ebx | On ebx = EPP_SourceEquation, ExitP   ; Exit if no function name in front of the parenthesis
    On B$ebx-1 <= openSign, ExitP                  ;

    mov D@FunctionNameLength 0
    lea ecx D$ebx-1 | While B$ecx > Space | dec ecx | inc D@FunctionNameLength | End_While
    inc ecx | mov D@FunctionNamePos ecx | inc D@FunctionNameLength

    mov D$EPP_Operand2 ecx
    If B$ecx+1 = memMarker ; Arrays
        call WriteArrayCode D@FunctionNamePos D@ParameterNamePos D@FunctionNameLength

    Else ; ex: _COS_F$VAL1_
        call WriteFunctionCode D@FunctionNamePos D@FunctionNameLength

    End_If

EndP
____________________________________________________________________________________________
Proc WriteArrayCode:
    Arguments @ArrayNamePos @ParameterNamePos @FunctionNameLength
    ; we have: _X$ARRAY_PARAMETER_
    ; #1 points on PARAMETER and #2 points on X$ARRAY

    mov ecx D@ArrayNamePos
    mov ebx D@ParameterNamePos

    call GetStringType D$EPP_Operand1
    ..Test_If ax MemoryOperand ; _F$TBL_D$AAAAAAAA_ ;  _F$_D$MyVal
        If_Or B$ebx = 'B', B$ebx = 'W'
            call ExtendOperandToDWord 1 RegisterOperand | jmp L1>
        Else_If B$ebx = 'D'
L1:         On B$ecx+2 <> Space, call WriteAddressingCode D$ecx
            call EraseArrayName D@FunctionNameLength

        Else ; index is not an integer
             error EqualPreparser_InvalidIndex
        End_If

    ..Test_Else_If ax RegisterOperand  ; _F$ARRAY_ECX_
        If B$ecx+2 <> Space ; if not an expression
            On B$ecx = 'T', jmp L1<<
            mov B$ebx-1 plusSign
            Test_If ax DWordValue
                lea edi D$ebx+3 | call shiftLineRight 2         ; _F$ARRAY_ECX_  => _F$ARRAY+ECX*4_
                mov B$ebx+3 addressSign
                call GetAddressingCoef ecx | mov B$ebx+4 al
            Test_Else
                lea edi D$ebx+2 | call shiftLineRight 2         ; _W$ARRAY_CL_   => _W$ARRAY+CL*2_
                mov B$ebx+2 addressSign
                call GetAddressingCoef ecx | mov B$ebx+3 al
            Test_End
        Else ; F$_ECX  ; D$_EDI
            mov edi ebx | call shiftLineLeft 1
        End_If

    ..Test_Else_If ax NumericOperand  ; _F$ARRAY_2_
        .If B$ecx+2 <> Space ; if not an expression
            On B$ecx = 'T', jmp L1<<
            If B$ebx-2 <> plusSign                     ; first step  : _F$ARRAY_13_ => _F$ARRAY+_(_13_*_4_)_
                mov B$ebx-1 plusSign
                mov edi ebx | call shiftLineRight 3
                mov B$ebx Space | mov B$ebx+1 openSign | mov B$ebx+2 Space

                add ebx 3 | While B$ebx > Space | inc ebx | End_While | inc ebx

                mov edi ebx | call shiftLineRight 6

                mov B$ebx mulSign | mov B$ebx+1 Space | call GetAddressingCoef ecx | mov B$ebx+2 al
                mov B$ebx+3 Space | mov B$ebx+4 closeSign | mov B$ebx+5 Space

            Else                                       ; second step : _F$ARRAY+_52_ => _F$ARRAY+52_
                mov edi ebx | call shiftLineLeft 1
            End_If
        .Else ; F$_1  ; D$_2254
            error EqualPreparser_InvalidAddress
        .End_If

    ..Test_Else
        error EqualPreparser_InvalidIndex

    ..Test_End

EndP
____________________________________________________________________________________________
Proc EraseArrayName:
    Argument @FunctionNameLength

    mov edi D$EPP_Operand1
    mov ecx D@FunctionNameLength | sub ecx 2
    call ShiftLineLeft ecx

EndP
____________________________________________________________________________________________
Proc LoadOperandOnTOS:
    ;This writes the code which load the D@Operand (1 or 2) on the Top of the FPU Stack.
    Arguments @Operand
    Uses ebx, ecx

    If D@Operand = 1
        mov ebx D$EPP_Operand1
        mov cx '#1'
    Else
        mov ebx D$EPP_Operand2
        mov cx '#2'
    End_If

    .If B$ebx+1 = memMarker
        If_Or B$ebx = 'T', B$ebx = 'F', B$ebx = 'R'
            EPP_Code 'FLD ' cx '|' | call incFPUStackNumber
        Else_If B$ebx = 'B'
            call ExtendOperandToDWord 1 MemoryOperand
            EPP_Code 'FILD ' cx '|' | call incFPUStackNumber
        Else  ;  others integer
            EPP_Code 'FILD ' cx '|' | call incFPUStackNumber
        End_If
    .Else_If W$ebx = 'st'
        ; result is already on TOS

    .Else ; constant
        call IsARegister ebx
        Test_If ax (ByteValue or WordValue)
            call ExtendOperandToDWord 1 MemoryOperand
            EPP_Code 'FILD ' cx '|'
        Test_Else_If ax DWordValue
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? ' cx '|FILD D$?|'
        Test_Else
            EPP_Code '{??:R$ ' cx '}|FLD R$?|' | call incFPUStackNumber
        Test_End

    .End_If

EndP
____________________________________________________________________________________________
Proc IsARegister:
    ; Returns &FALSE if D@pos does NOT points on a register (it can be terminated by EOI,meEOI,Space or &NULL)
    ; Otherwise returns '(RegisterOperand OR SizeOp)' with SizeOp, the size of the register.
    ; (which can be ByteValue,WordValue,DWordValue,FloatingPointOperand+TByteValue)
    Argument @pos
    Uses esi

    mov eax &FALSE
    mov esi D@pos
    ..If B$esi+2 <= Space
        .If_Or B$esi+1 = 'H', B$esi+1 = 'L'
            If_And B$esi >= 'A', B$esi <= 'D'
                mov eax (RegisterOperand or ByteValue)
            End_If

        .Else_If_Or W$esi = 'AX', W$esi = 'BX', W$esi = 'CX', W$esi = 'DX',
                    W$esi = 'BP', W$esi = 'DI', W$esi = 'SI'
            mov eax (RegisterOperand or WordValue)
        .End_If

    ..Else_If B$esi+3 <= Space
        .If B$esi = 'E'
            If_Or W$esi+1 = 'AX', W$esi+1 = 'BX', W$esi+1 = 'CX', W$esi+1 = 'DX',
                  W$esi+1 = 'BP', W$esi+1 = 'DI', W$esi+1 = 'SI'
                mov eax (RegisterOperand or DWordValue)
            End_If
        .Else_If_Or W$esi = 'ST', W$esi = 'st'
            ; 'st' is for temporary results stored on the FPU Stack (See incFPUStackNumber)
            If_And B$esi+2 >= '0', B$esi+2 <= '7'
                mov eax (RegisterOperand or FloatingPointOperand or TByteValue)
            End_If
        .End_If
    ..End_If

EndP
____________________________________________________________________________________________
Proc GetAddressingCoef:
    ; returns in al the corresponding size of D@SizeMarkerPos
    Argument @SizeMarkerPos
    Uses ecx

    mov ecx D@SizeMarkerPos
    .If B$ecx = 'B'
        mov al '1'
    .Else_If B$ecx = 'R'
        mov al '8'
    .Else_If B$ecx = 'F'
        mov al '4'
    .Else_If B$ecx = 'D'
        mov al '4'
    .Else_If B$ecx = 'W'
        mov al '2'
    .Else_If B$ecx = 'Q'
        mov al '8'
    .End_If
EndP
____________________________________________________________________________________________
Proc WriteFunctionCode:
    ;Set the result of the operations on the FPU Stack and compute the function
    ; we have: _FUNCTION_PARAMETER_
    ; #1 points on PARAMETER and #2 points on FUNCTION
    Arguments @FunctionNamePos @FunctionNameLength

    .If_str D@FunctionNamePos = 'COS'
        call LoadOperandOnTOS 1
        EPP_Code 'FCOS|'
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'SIN'
        call LoadOperandOnTOS 1
        EPP_Code 'FSIN|'
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'TAN'
        call LoadOperandOnTOS 1
        EPP_Code 'FPTAN|FINCSTP|FFREE ST7|'
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'ABS'
        call LoadOperandOnTOS 1
        EPP_Code 'FABS|'
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'SQRT'
        call LoadOperandOnTOS 1
        EPP_Code 'FSQRT|'
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'LOG10'
        EPP_Code 'FLDLG2|' | call incFPUStackNumber
        call LoadOperandOnTOS 1
        EPP_Code 'FYL2X|' | call decFPUStackNumber
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'LOG2'
        EPP_Code 'FLD1|' | call incFPUStackNumber
        call LoadOperandOnTOS 1
        EPP_Code 'FYL2X|' | call decFPUStackNumber
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'LN'
        EPP_Code 'FLDLN2|' | call incFPUStackNumber
        call LoadOperandOnTOS 1
        EPP_Code 'FYL2X|' | call decFPUStackNumber
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'ATAN'
        call LoadOperandOnTOS 1
        EPP_Code 'FLD1|'
        EPP_Code 'FPATAN|'
        call StoreFPUResult 1

    .Else_If_str D@FunctionNamePos = 'EXP'
        call LoadOperandOnTOS 1
        EPP_Code 'FLDL2E|FMULP ST1 ST0|FLD ST0|FRNDINT|FSUB ST1 ST0|FLD1|'
        EPP_Code 'FSCALE|FSTP ST1|FXCH|F2XM1|FLD1|FADDP ST1 ST0|FMULP ST1 ST0|'
        call StoreFPUResult 1

    .Else
        error EqualPreparser_InvalidFunction
    .End_If


    mov edi D$EPP_Operand1 | call ShiftLineLeft D@FunctionNameLength ; erase function name

EndP
____________________________________________________________________________________________
Proc UpdateLocalDataShift:
    Local @LocalDataShift @RegisterLetter

    mov D@LocalDataShift 0
    mov ecx D$EPP_CodeBegining
    mov ebx &FALSE


    EPP_FindCodePos 'MOV ESI ESP'  EPP_CODE_BEGIN EPP_CODE_END
    jmp L1>
    .While eax <> CODE_NOT_FOUND
        add D@LocalDataShift 4 | inc eax

L1:     EPP_FindCodePos 'PUSH' eax EPP_CODE_BEGIN EPP_CODE_END
    .End_While

    jmp L1>>
    .While eax <> CODE_NOT_FOUND
        mov ebx &TRUE
        mov esi D$EPP_SearchedStringAddress
        If B$esi+2 < Separators
            mov al B$esi+1 | sub al '0' | On al > 9 , sub al 7
        Else
            mov al B$esi+1 | sub al '0' | On al > 9 , sub al 7 | shl al 4
            mov ah B$esi+2 | sub ah '0' | On ah > 9 , sub ah 7
            add al ah
        End_If
        movzx edx al | add D@LocalDataShift edx

        EPP_FindCodePos 'ESI-' ecx EPP_CODE_END | add eax 5 | mov ecx eax
        ; We always have 'SUB ESP &0|{&1 ESI-0xx}|'
        If D@LocalDataShift < 010
            mov al B@LocalDataShift | add al '0' | On al > '9', add al 7
            mov B$ecx al
            lea edi D$ecx+2 | call ShiftLineLeft 1
        Else_If D@LocalDataShift > 0FF
            error EqualPreparser_LocalStackOverflow
        Else
            mov al B@LocalDataShift
            mov ah al | and ah 0F | shr al 4
            add ah '0' | On ah > '9', add ah 7 | mov B$ecx+1 ah
            add al '0' | On al > '9', add al 7 | mov B$ecx   al
        End_If


L1:     EPP_FindCodePos 'SUB ESP &0' ecx EPP_CODE_END | mov ecx eax
    .End_While

    .If ebx = &FALSE
        EPP_ReplaceCode 'PUSH ESI|MOV ESI ESP'  0           EPP_CODE_BEGIN EPP_CODE_END
        EPP_ReplaceCode 'MOV ESP ESI|POP ESI'   0           EPP_CODE_BEGIN EPP_CODE_END
        EPP_ReplaceCode 'D$ESI+4'               'ESI'       EPP_CODE_BEGIN EPP_CODE_END
        EPP_ReplaceCode 'MOV &1 D$ESI-??'       0           EPP_CODE_BEGIN EPP_CODE_END

    .End_If

EndP
____________________________________________________________________________________________
Proc ReplaceCodeMacroProc:
    Arguments @CodeToReplace @CodeReplacement @Begin @End
    Uses ebx

    jmp L1>
    While eax <> CODE_NOT_FOUND
        call ReplaceCodeProc eax D@CodeReplacement ebx

L1:     call FindCodePosProc D@CodeToReplace D@Begin D@End
    End_While

EndP
____________________________________________________________________________________________
Proc ReplaceCodeProc:
    ; This procedure replaces the code at D@CodeToReplacePos by D@CodeReplacement.
    ; D@CodeLength is the length of the code to be replaced.
    ; In D@CodeReplacement two specials characters can be used :
    ;   - '!n' which is replaced by the a single characters and '&n' by an instruction/operand
    ;       (with n a digit between 1 and 9)
    ;       See FindCodePosProc for more information about theses internal variables.
    Arguments @CodeToReplacePos @CodeReplacement @CodeLength
    USes esi edi ecx

    mov edi EPP_ReplacementBuffer
    mov esi D@CodeReplacement

    mov ecx 0
    lodsb
    ..While al <> 0
        inc ecx
        .If al = '!'
            movzx edx B$esi | sub edx '0' | inc esi
            mov al B$EPP_SearchedSingleCharacter+edx

        .Else_If al = '&'
            movzx edx B$esi | sub edx '0' | inc esi | shl edx 2
            push esi
                mov esi D$EPP_SearchedStringAddress+edx
                .While B$esi > LowSigns
                    movsb  | inc ecx
                    While B$esi = memMarker | movsb | End_While
                    On B$esi = '}', jmp L2>
                .End_While
                dec ecx
L2:         pop esi
            dec edi | mov al B$edi
        .Else_If al = ' '
            mov al Space
        .Else_If al = '$'
            mov al memMarker
        .Else_If al = '|'
            If B$esi = '|'
                mov al EOI | inc esi
            Else
                mov al meEOI
            End_If
        .End_If
        stosb

        lodsb
    ..End_While
    mov B$edi 0

    On ecx = 0, inc D@CodeLength
    mov edi D@CodeToReplacePos | add edi D@CodeLength
    call ShiftLineLeft D@CodeLength
    mov eax D@CodeLength | sub D$EPP_WrittenCode eax
    mov edi D@CodeToReplacePos
    call ShiftLineRight ecx
    add D$EPP_WrittenCode ecx

    mov esi EPP_ReplacementBuffer
    While B$esi <> 0 | movsb | End_While

EndP
____________________________________________________________________________________________
Proc FindCodePosProc:
    ; Returns: - in eax the position of D@CodeToFind between D@Begin and D@End
    ;          - in ebx the length of the code found.
    ; and CODE_NOT_FOUND otherwise.
    ; In D@CodeToFind the following specials characters are used :
    ;   - '?'  means 'any character'
    ;   - '!n' represents a single character which must be the same each timle it appears.
    ;          (n is a number between 0 and 9)
    ;   - '&n' represents a mnemonic or an operand '}'/Space/meEOI/EOI-terminated which must be the same
    ;          each time it appears. (n is a number between 0 and 9)
    ;   '&n' and '!n' internal variables are re-initialized each time this procedure is called, so that
    ;   they can be used by a call to ReplaceCodeProc right after this procedure.
    ;
    ; - D@Begin can be:
    ;   * EPP_CODE_BEGIN   : to search up from the beginning of the produced code(in one Equal line).
    ;   * EPP_SOURCE_BEGIN : to search up from the beginning of the produced code(in one Equal line).
    ;
    ; - D@End can be:
    ;   * EPP_CODE_END   : to search up to the end of the produced code(in one Equal line).
    ;   * EPP_SOURCE_END : to search up to the end of the produced code(in one Equal line).
    ;
    ; This function must be called BEFORE TranslateExpressions
    Arguments  @CodeToFind @Begin @End
    Local @FirstCharPos @result
    Uses ecx, esi, edi, edx

    On D@Begin = EPP_CODE_BEGIN, move D@Begin D$EPP_CodeBegining
    On D@End   = EPP_CODE_END  , move D@End   D$EPP_WrittenCode
    On D@Begin = EPP_SOURCE_BEGIN, move D@Begin EPP_SourceEquation
    If D@End   = EPP_SOURCE_END
        mov ecx EPP_SourceEquation
        While B$ecx <> EOI | inc ecx | End_While
        mov D@End ecx
    End_If

    mov ecx D@End | sub ecx D@Begin
    Test_If ecx 0_8000_0000  ; (D$EPP_WrittenCode - D@From) < 0   ==>   ExitP
        mov eax CODE_NOT_FOUND
        ExitP
    Test_End

    mov D@FirstCharPos &FALSE
    mov D@result CODE_NOT_FOUND
    mov esi D@CodeToFind
    mov edi D@Begin
    mov ebx 0
    call InitSearchVariables

    ..While ecx >s 0
        lodsb | inc ebx
        ..If al = ' '
            mov al Space
        ..Else_If al = '?'
            mov al B$edi
        ..Else_If al = '$'
            mov al memMarker
        ..Else_If al = ':'
            mov al colonSign
        ..Else_If al = '-'
            mov al minusSign
        ..Else_If al = '+'
            mov al plusSign
        ..Else_If al = '*'
            mov al addressSign
        ..Else_If al = '|'
            If B$esi = '|'
                mov al EOI | inc esi
            Else
                mov al meEOI
            End_If
        ..Else_If al = '!'
            movzx edx B$esi | sub edx '0' | inc esi
            If B$EPP_SearchedSingleCharacter+edx = 0
                mov al B$edi
                mov B$EPP_SearchedSingleCharacter+edx al
            Else
                mov al B$EPP_SearchedSingleCharacter+edx
            End_If
        ..Else_If al = '&'
            movzx edx B$esi | sub edx '0' | inc esi | shl edx 2
            .If D$EPP_SearchedStringAddress+edx = 0
                mov D$EPP_SearchedStringAddress+edx edi
                .While B$edi > LowSigns
                    inc edi | inc ebx
                    While B$edi = memMarker | inc edi | inc ebx | End_While
                    On B$edi = '}', jmp L2>
                .End_While
L2:             dec edi ebx | mov al B$edi
            .Else
                call CompareCode edi D$EPP_SearchedStringAddress+edx
                If eax = &TRUE
                    .While B$edi > LowSigns
                        inc edi | inc ebx
                        While B$edi = memMarker | inc edi | inc ebx | End_While
                        On B$edi = '}', jmp L2>
                    .End_While
L2:                 dec edi ebx | mov al B$edi
                Else
                    mov D@FirstCharPos &TRUE
                    mov al 0
                End_If
            .End_If

        ..Else_If al = 0
            move D@result D@FirstCharPos
            jmp L5>
        ..End_If

        .If D@FirstCharPos <> &FALSE
            If B$edi <> al
                mov D@FirstCharPos &FALSE
                mov esi D@CodeToFind
                call InitSearchVariables
            Else
                inc edi
            End_If
        .Else
            repne scasb
            mov D@FirstCharPos edi | dec D@FirstCharPos
            mov ebx 0
        .End_If

    ..End_While
L5: mov eax D@result

EndP
____________________________________________________________________________________________
Proc CompareCode:
    ; returns &TRUE if D@Code1 and D@Code2 points on identicals mnemonics or operands ('}'/Space/meEOI/EOI-terminated)
    Arguments @Code1 @Code2
    Uses esi edi

    mov esi D@Code1
    mov edi D@Code2

    lodsb
    .While al = B$edi
        inc edi
        .If_And B$esi < LowSigns, B$edi < LowSigns, B$esi <> memMarker, B$edi <> memMarker
            mov eax &TRUE  | ExitP
        .Else_If_And  B$esi = 125 , B$edi = 125 ; 125 = '}'
            mov eax &TRUE | ExitP
        .Else_If_And  B$esi < LowSigns, B$edi = 125, B$esi <> memMarker
            mov eax &TRUE | ExitP
        .Else_If_And  B$edi < LowSigns, B$esi = 125, B$edi <> memMarker
            mov eax &TRUE | ExitP
        .Else_If_Or  B$esi = 125 , B$edi = 125
            mov eax &FALSE | ExitP
        .Else_If_And B$esi < LowSigns, B$esi <> memMarker
            mov eax &FALSE | ExitP
        .Else_If_And  B$edi < LowSigns, B$edi <> memMarker
            mov eax &FALSE | ExitP
        .End_If
        lodsb
    .End_While
    mov eax &FALSE

EndP
____________________________________________________________________________________________
InitSearchVariables:
    push ecx
        mov ecx 0
        While ecx < SEARCH_VARIABLE_NUMBER
            mov B$EPP_SearchedSingleCharacter+ecx 0
            mov D$EPP_SearchedStringAddress+ecx*4   0
            inc ecx
        End_While
    pop ecx
ret
____________________________________________________________________________________________
Proc DeleteCodeLine:
    ; Delete a code line while parsing. (the end of the source is D$EPP_WrittenCode)
    Argument @LinePos
    Uses ecx, edi

    mov edi D$EPP_WrittenCode | inc edi
    call DeleteFinalCodeLine D@LinePos edi | sub edi eax
    mov D$EPP_WrittenCode edi

EndP
____________________________________________________________________________________________
Proc DeleteFinalCodeLine:
    ; LinePos point on the beginning of a code line (after meEOI or EOI)
    ; returns the number of characters deleted.
    Arguments @LinePos @EndOfSource
    Uses ecx, esi, edi

    mov edi D@LinePos
    mov esi D@LinePos | While B$esi > EOI | inc esi | End_While
    mov eax esi | sub eax D@LinePos | inc eax
    mov ecx D@EndOfSource
    If_And B$esi = EOI, B$edi-1 = meEOI
        dec edi | dec ecx
    Else
        inc esi
    End_If
    sub ecx esi
    rep movsb

    ; reset to 0 the end of the source (needed for the macros unfolding)
    mov ecx D@EndOfSource | sub ecx eax
    While ecx < D@EndOfSource | mov B$ecx 0 | inc ecx | End_While

EndP
____________________________________________________________________________________________
Proc OptimizeEqualPreparserCode:
    Arguments @From @To

    Test_If_Not B$UsedRegisters EPP_EAX
        EPP_ReplaceCode 'PUSH EAX' 0 D@From  D@To
        EPP_ReplaceCode 'POP EAX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_EBX
        EPP_ReplaceCode 'PUSH EBX' 0 D@From  D@To
        EPP_ReplaceCode 'POP EBX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_ECX
        EPP_ReplaceCode 'PUSH ECX' 0 D@From  D@To
        EPP_ReplaceCode 'POP ECX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_EDX
        EPP_ReplaceCode 'PUSH EDX' 0 D@From  D@To
        EPP_ReplaceCode 'POP EDX'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_ESI
        EPP_ReplaceCode 'PUSH ESI' 0 D@From  D@To
        EPP_ReplaceCode 'POP ESI'  0 D@From  D@To
    Test_End

    Test_If_Not B$UsedRegisters EPP_EDI
        EPP_ReplaceCode 'PUSH EDI' 0 D@From  D@To
        EPP_ReplaceCode 'POP EDI'  0 D@From  D@To
    Test_End


    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI?&2}|MOV ?$&1 &3|MOV &4 ?$&1',
                    'MOV &4 &3',
                    D@From D@To


    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI?&2}|MOV ?$&1 &3|PUSH ?$&1',
                    'PUSH &3',
                    D@From D@To


    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI-&2}|MOV D$&1 &3|POP &4|MOV &5 D$&1',
                    'MOV &5 &3|POP &4',
                    D@From D@To

    EPP_ReplaceCode 'MOV &1 &1',
                    0,
                    D@From D@To

    EPP_ReplaceCode 'SUB ESP &0|{&1 ESI?&2}|FSTP ?$&1|FLD ?$&1',
                    0,
                    D@From D@To


EndP
____________________________________________________________________________________________
Proc EqualFirstScan:
    ; Replaces each register by a meaningless label and writes the code that copies its value in the memory.
    ; D@SecondPass is true when we are computing the address of an array.
    Argument @SecondPass

    mov B$FPUInSourceExpression &FALSE
    mov B$UsedRegisters  EPP_ESI

    ; identify destination type
    call GetStringType EPP_DestOperand
    mov D$DestinationType eax

    If D@SecondPass = &FALSE
        EPP_Code 'PUSH ESI|MOV ESI ESP|PUSH EDI|PUSH ECX|PUSH EAX|PUSH EBX|PUSH EDX|'
    Else
        ;...
    End_If

    Test_If_And D$DestinationType RegisterOperand, D$DestinationType FloatingPointOperand
        EPP_Code 'SUB ESP 06C|{&7 ESI-000}|FSAVE X$!7|FINIT|'
    Test_End

    mov esi EPP_SourceEquation
    .While B$esi > EOI
        mov D$EPP_Operand1 esi
        call GetStringType esi
        ...Test_If eax RegisterOperand
            ..Test_If_Not D$NoOperation &TRUE
                .Test_If eax ByteValue
                    EPP_Code 'SUB ESP 01|{?? ESI-000}|MOV B$? #1|'
                    call ReplaceOperand 1 'B' 0
                .Test_Else_If eax WordValue
                    If W$esi = 'SI'
                        EPP_Code '{?? ESI}|'
                        call ReplaceOperand 1 'W' 0
                    Else
                        EPP_Code 'SUB ESP 02|{?? ESI-000}|MOV W$? #1|'
                        call ReplaceOperand 1 'W' 0
                    End_If
                .Test_Else_If eax DWordValue
                    If_And W$esi = 'ES', B$esi+2 = 'I'
                        EPP_Code '{?? ESI}|'
                        call ReplaceOperand 1 'D' 0
                    Else
                        EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|'
                        call ReplaceOperand 1 'D' 0
                    End_If
                .Test_End
            ..Test_Else
                Test_If eax FloatingPointOperand
                    EPP_Code 'SUB ESP 0A|{?? ESI-000}|FLD #1|FSTP T$?|'
                    call ReplaceOperand 1 'T' 0
                    mov B$FPUInSourceExpression &TRUE
                Test_End
            ..Test_End
        ...Test_End
        While B$esi > Space | inc esi | End_While | inc esi ; jump to next space
    .End_While


EndP
____________________________________________________________________________________________
FormatEqualLine:
; - Surrounds each parenthesis and operator by two spaces and performs a slight error processing.
; - Minus signs in front of numeric operands are replaced by minusSign
; - When '.' is a floating point (i.e not in a user-label) then it is replaced by pointSign
; - if an operator is doubled (**,//), it is replaced by its signed equivalent (SignedMulSign,SignedDivSign)

    mov B$NoOperation &TRUE
    mov esi EPP_SourceEquation
    .While B$esi > EOI
        ..If B$esi = openSign
            .If B$esi+1 = subSign   ; '(-9', '(-1'   OK
                If_And B$esi+2 >= '0', B$esi+2 <= '9'
                    mov B$esi+1 minusSign
                    call SourroundCurrentSign
                    inc esi
                Else
                    error EqualPreparser_OperatorsInvalid
                End_If
            .Else_If_And B$esi+1 < openSign, B$esi+1 <> Space
                error EqualPreparser_OperatorsInvalid
            .Else
                call SourroundCurrentSign
            .End_If

        ..Else_If B$esi =  closeSign   ; ')+',')/', etc..   OK
            If B$esi+1 > closeSign
                error EqualPreparser_OperatorsInvalid
            Else
                call SourroundCurrentSign
            End_If

        ..Else_If_And B$esi < OperatorSigns, B$esi > Separators
            mov B$NoOperation &FALSE
            mov al B$esi
            .If B$esi+1 = subSign          ; ...  +-7 ...  OK
                If_And B$esi+2 >= '0', B$esi+2 <= '9'
                    mov B$esi+1 minusSign
                    call SourroundCurrentSign
                    inc esi
                Else
                    error EqualPreparser_OperatorsInvalid
                End_If
            .Else_If B$esi+1 = al
                lea edi D$esi+1  | call ShiftLineLeft 1       ; delete the doubled operator
                On al = mulSign, mov B$esi SignedMulSign
                On al = divSign, mov B$esi SignedDivSign
                call SourroundCurrentSign

            .Else_If B$esi+1 < openSign
                error EqualPreparser_OperatorsInvalid
            .Else
                call SourroundCurrentSign
            .End_If

        ..Else_If B$esi = '.'
            If_And B$esi+1 >= '0', B$esi+1 <= '9', B$esi-1 >= '0', B$esi-1 <= '9'
                mov B$esi pointSign
            End_If
            inc esi

        ..Else
            inc esi
        ..End_If
    .End_While

ret
                                                ____________________________
SourroundCurrentSign:
    mov edi esi
    mov al B$esi
    .If B$esi-1 = Space
        If B$esi+1 <> Space     ; adds one space after the operator
            call ShiftLineRight 1
            mov B$esi+1 Space
        End_If
        add esi 2
    .Else_If B$esi+1 = Space    ; adds one space before the operator
        call ShiftLineRight 1
        mov B$esi   Space
        add esi 3
    .Else                       ; adds two spaces
        call ShiftLineRight 2
        mov B$esi     Space
        mov B$esi+1   al
        mov B$esi+2   Space
        add esi 3
    .End_If

ret
____________________________________________________________________________________________
____________________________________________________________________________________________
Proc ComputeImmediateExpression:
; This function computes immediates operations between constants.
; D@OperationType may be one of these (addSign,subSign,mulSign,divSign,expSign)
    Argument @OperationType
    Local @ValueExponent @temp
    Uses ecx, esi, edi
    finit
    call AsciiToFloat D$EPP_Operand1 0    | On eax = &FALSE, error EqualPreparser_InvalidFPValue
    call AsciiToFloat D$EPP_Operand2 0    | On eax = &FALSE, error EqualPreparser_InvalidFPValue

    On D@OperationType = addSign, fadd  ST0 ST1
    On D@OperationType = subSign, fsubr ST0 ST1
    On D@OperationType = mulSign, fmul  ST0 ST1
    On D@OperationType = divSign, fdivr ST0 ST1
    If D@OperationType = expSign
        fxch | fyl2x | fld ST0 | frndint | fsub ST1 ST0
        fld1 | fscale | fstp ST1 | fxch | f2xm1
        fld1 | faddp ST1 ST0 | fmulp ST1 ST0
    End_If

    fstp T$EPP_ImmediateResult
    call FloatToUString EPP_ImmediateResult EPP_ImmediateResultString

    .If_str EPP_ImmediateResultString = 'INF'
        jmp L1>>
    .Else_If_str EPP_ImmediateResultString = 'SNaN'
        jmp L1>>
    .Else_If_str EPP_ImmediateResultString = 'QNaN'
        jmp L1>
    .Else_If_str EPP_ImmediateResultString = '-SNaN'
        jmp L1>
    .Else_If_str EPP_ImmediateResultString = '-QNaN'
        jmp L1>
    .Else_If_str EPP_ImmediateResultString = '-INF'
L1:     error EqualPreparser_InvalidImmVal
    .Else
        mov esi EPP_ImmediateResultString
        mov ecx 0
        While B$esi <> 0
            If B$esi = '.'
                mov B$esi pointSign
            Else_If B$esi = 'e'
                mov B$esi 'E'
            Else_If B$esi = '+'
                mov B$esi plusSign
            Else_If B$esi = '-'
                mov B$esi minusSign
            End_If
            inc esi, ecx
        End_While
        call ReplaceOperand 0 0 ' '
        mov edi D$EPP_Operand1 | inc edi | dec ecx
        call ShiftLineRight ecx
        mov esi EPP_ImmediateResultString
        mov edi D$EPP_Operand1
        While B$esi <> 0 | movsb | End_While
    .End_If

EndP
____________________________________________________________________________________________
StoreInMemory:

    ...If_Or B$ebx = 'T', B$ebx = 'R', B$ebx = 'F'
    ;
    ; -> FIRST OPERAND IS FLOATING POINT (T$,R$ or F$):
    ;
        ..If B$edi+2 = memMarker
        ; second operand is memory data
            .If B$edi+1 = 'B'
                call ExtendOperandToDWord 1 MemoryOperand
                jmp L1>
            .Else_If_Or B$edi+1 = 'D', B$edi+1 = 'Q', B$edi+1 = 'W'
L1:             EPP_Code 'FILD #1|FSTP #2|'

            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FSTP #2|'
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FSTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | call IsARegister eax
            Test_If ax (ByteValue or WordValue)
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #1|MOV D$? EAX|FILD D$?|FSTP #2|'
            Test_Else_If ax DWordValue
                EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|FILD D$?|FSTP #2|'
            Test_Else
                EPP_Code '{??:R$ #1}|FLD R$?|FSTP #2|'
            Test_End
        ..End_If
    ;
    ; -> FIRST OPERAND IS INTEGER :
    ;
    ...Else_If B$ebx = 'B'
    ; BYTE
        ..If B$edi+2 = memMarker
            .If B$edi+1 = 'B'
                EPP_Code 'MOV AL #1|MOV #2 AL|' | or B$UsedRegisters EPP_EAX
            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            error EqualPreparser_SourceInvalid

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | call IsARegister eax
            If ax <> &FALSE
                Test_If_Not ax ByteValue | error EqualPreparser_SourceInvalid | Test_End
            End_If
            EPP_Code 'MOV #2 #1|'
        ..End_If

    ...Else_If B$ebx = 'W'
    ; WORD
        ..If B$edi+2 = memMarker
            .If B$edi+1 = 'B'
                EPP_Code 'MOVZX AX #1|MOV #2 AX|' | or B$UsedRegisters EPP_EAX
            .Else_If B$edi+1 = 'W'
                EPP_Code 'MOV AX #1|MOV #2 AX|' | or B$UsedRegisters EPP_EAX
            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FISTP #2|'
            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FISTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | call IsARegister eax
            If ax <> &FALSE
                Test_If_Not ax (ByteValue or WordValue) | error EqualPreparser_SourceInvalid | Test_End
            End_If
            Test_If ax ByteValue
                EPP_Code 'MOVZX #2 #1|'
            Test_Else
                EPP_Code 'MOV #2 #1|'
            Test_End
        ..End_If

    ...Else_If B$ebx = 'Q'
    ; QWORD
        ..If B$edi+2 = memMarker
            .If B$edi+1 = 'Q'
                mov B$edi+1 'D'  ; the QWORD is moved in two steps, DWord by DWord
                mov B$ebx   'D'
                EPP_Code 'PUSH #1|POP #2|PUSH #1+4|POP #2+4|'

            .Else_If_Or B$edi+1 = 'B', B$edi+1 = 'W'
                call ExtendOperandToDWord 1 RegisterOperand
                jmp L1>
            .Else_If B$edi+1 = 'D'
L1:             mov B$ebx 'D'
                EPP_Code 'PUSH #1|POP #2|MOV #2+4 0|'

            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FISTP #2|'

            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FISTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | call IsARegister eax
            Test_If ax ByteValue
                mov B$ebx 'B'
            Test_Else_If ax WordValue
                mov B$ebx 'W'
            Test_Else
                mov B$ebx 'D'
            Test_End
            EPP_Code 'MOV #2 #1|MOV #2+4 0|'
        ..End_If

    ...Else_If B$ebx = 'D'
    ; DWORD
        ..If B$edi+2 = memMarker
        ; second operand is memory data
            .If_Or B$edi+1 = 'B', B$edi+1 = 'W'
                call ExtendOperandToDWord 1 RegisterOperand
                jmp L1>
            .Else_If B$edi+1 = 'D'
L1:             EPP_Code 'PUSH #1|POP #2|'
            .Else_If_Or B$edi+1 = 'T', B$edi+1 = 'R', B$edi+1 = 'F'
                EPP_Code 'FLD #1|FISTP #2|'
            .Else
                error EqualPreparser_SourceInvalid
            .End_If

        ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
            EPP_Code 'FISTP #2|'

        ..Else
        ; second operand is a single register, an integer number or an Equate
            lea eax D$edi+1 | call IsARegister eax
            Test_If ax (ByteValue or WordValue)
                EPP_Code 'MOVZX #2 #1|'
            Test_Else
                EPP_Code 'MOV #2 #1|'
            Test_End
        ..End_If

    ...Else
    ; Invalid data identifier
        error EqualPreparser_DestInvalid
    ...End_If

ret
____________________________________________________________________________________________
Proc StoreIn32BitsRegister:

    ..If B$edi+2 = memMarker
        ; second operand is memory data
        .If_Or B$edi+1 = 'B', B$edi+1 = 'W'
            mov D$EPP_LastStorageStatement_InMemory {'MOVZX #2 #1|',0}
        .Else_If B$edi+1 = 'D'
            mov D$EPP_LastStorageStatement_InMemory {'MOV #2 #1|',0}
        .Else_If_Or B$edi+1 = 'F', B$edi+1 = 'R', B$edi+1 = 'T'
            EPP_Code 'SUB ESP 04|{?? ESI-000}|FLD #1|FISTP D$?|'
            mov D$EPP_LastStorageStatement_InMemory {'MOV #2 D$?|',0}
        .Else
            error EqualPreparser_SourceInvalid
        .End_If

    ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
        EPP_Code 'SUB ESP 04|{?? ESI-000}|FISTP D$?|'
        mov D$EPP_LastStorageStatement_InMemory {'MOV #2 D$?|',0}

    ..Else
    ; second operand is an integer number or a register or an equate
        lea eax D$edi+1 | call IsARegister eax
        Test_If ax (ByteValue or WordValue)
            mov D$EPP_LastStorageStatement {'MOVZX #2 #1|',0}
        Test_Else
            mov D$EPP_LastStorageStatement {'MOV #2 #1|',0}
        Test_End
    ..End_If

EndP
____________________________________________________________________________________________
StoreInFPURegister:

    ..If B$edi+2 = memMarker
    ; second operand is memory data
        EPP_Code 'FRSTOR X$!7|'
        .If_Or B$edi+1 = 'B'
            call ExtendOperandToDWord 1 MemoryOperand
            jmp L1>
        .Else_If_Or B$edi+1 = 'W', B$edi+1 = 'D', B$edi+1 = 'Q'
L1:         If W$EPP_DestOperand+1 = 'T7'
                EPP_Code 'FFREE ST7|FILD #1|FINCSTP|'
            Else
                inc B$EPP_DestOperand+2
                EPP_Code 'FILD #1|FSTP #2|'
            End_If
        .Else_If_Or B$edi+1 = 'R', B$edi+1 = 'F', B$edi+1 = 'T'
            If W$EPP_DestOperand+1 = 'T7'
                EPP_Code 'FFREE ST7|FLD #1|FINCSTP|'
            Else
                inc B$EPP_DestOperand+2
                EPP_Code 'FLD #1|FSTP #2|'
            End_If

        .Else
            error EqualPreparser_SourceInvalid
        .End_If

    ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
        EPP_Code 'SUB ESP 0A|{?? ESI-000}|FSTP T$?|'
        call ReplaceOperand 1 'T' 0
        mov D$EPP_Operand2 EPP_DestOperand
        EPP_Code 'FRSTOR X$!7|'
        If W$EPP_DestOperand+1 = 'T7'
            EPP_Code 'FFREE ST7|FLD #1|FINCSTP|'
        Else
            inc B$EPP_DestOperand+2
            EPP_Code 'FLD #1|FSTP #2|'
        End_If

    ..Else
        ; second operand is a register, an integer number or an equate
        EPP_Code 'FRSTOR X$!7|'
        On W$EPP_DestOperand+1 = 'T7', EPP_Code 'FFREE ST7|'
        lea eax D$edi+1 | call IsARegister eax
        Test_If ax (ByteValue or WordValue)
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOVZX EAX #1|MOV D$? EAX|FILD D$?|'
        Test_Else_If ax DWordValue
            EPP_Code 'SUB ESP 04|{?? ESI-000}|MOV D$? #1|FILD D$?|'
        Test_Else
            EPP_Code '{??:R$ #1}|FLD R$?|'
        Test_End

        If W$EPP_DestOperand+1 = 'T7'
            EPP_Code 'FINCSTP|'
        Else
            inc B$EPP_DestOperand+2
            EPP_Code 'FSTP #2|'
        End_If
    ..End_If

ret
____________________________________________________________________________________________
StoreIn16BitsRegister:

    ..If B$edi+2 = memMarker
    ; second operand is memory data
        .If B$edi+1 = 'B'
            mov D$EPP_LastStorageStatement_InMemory {'MOVZX #2 #1|',0}
        .Else_If B$edi+1 = 'W'
            mov D$EPP_LastStorageStatement_InMemory {'MOV #2 #1|',0}
        .Else_If_Or B$edi+1 = 'F', B$edi+1 = 'R', B$edi+1 = 'T'
            EPP_Code 'SUB ESP 02|{?? ESI-000}|FLD #1|FISTP W$?|'
            mov D$EPP_LastStorageStatement_InMemory {'MOV #2 W$?|',0}
        .Else
            error EqualPreparser_SourceInvalid
        .End_If

    ..Else_If W$edi+1 = 'st'
        ; result on the FPU Stack
        EPP_Code 'SUB ESP 02|{?? ESI-000}|FISTP W$?|'
        mov D$EPP_LastStorageStatement_InMemory {'MOV #2 W$?|',0}

    ..Else
    ; second operand is an integer number, a register or an equate
        lea eax D$edi+1 | call IsARegister eax
        Test_If ax ByteValue
            mov D$EPP_LastStorageStatement {'MOVZX #2 #1|',0}
        Test_Else_If_Not ax DWord
            mov D$EPP_LastStorageStatement {'MOV #2 #1|',0}
        Test_Else
            error EqualPreparser_SourceInvalid
        Test_End
    ..End_If

ret
____________________________________________________________________________________________
StoreIn8BitsRegister:

    .If B$edi+2 = memMarker
    ; second operand is memory data
        If B$edi+1 = 'B'
            mov D$EPP_LastStorageStatement_InMemory {'MOV #2 #1|',0}
        Else
            error EqualPreparser_SourceInvalid
        End_If
    .Else
    ; second operand is an integer number, a register or an equate
        On W$edi+1 = 'st', error EqualPreparser_SourceInvalid
        lea eax D$edi+1 | call IsARegister eax
        If ax <> &FALSE
            Test_If_Not ax ByteValue | error EqualPreparser_SourceInvalid | Test_End
        End_If
        mov D$EPP_LastStorageStatement {'MOV #2 #1|',0}
    .End_If

ret
____________________________________________________________________________________________








____________________________________________________________________________________________
; EPP Macros:
[EPP_Code
#If #1=str
    EPP_CodeDirect #1
#Else_If #1=mem
    EPP_CodePtr #1
#Else_If #1=reg
    #If #1=D
        EPP_CodeReg #1 D
    #Else_If #1=W
        EPP_CodeReg #1 W
    #Else_If #1=B
        EPP_CodeReg #1 B
    #End_If
#End_If
#+1]

[EPP_CodeReg | {&0: 0} | mov #2$&0 #1 | call ImmediateCodeParser &0]
[EPP_CodePtr | lea eax #1 | call ImmediateCodeParser eax]
[EPP_CodeDirect | {&0:B$ #1,0} | call ImmediateCodeParser &0]

; returns in fourth parameter (if it exists) the length of string
[EPP_FindCodePos | {&0:B$ #1,0} | push ebx | call FindCodePosProc &0 #2 #3 | pop ebx ]

[EPP_ReplaceCode | {&0:B$ #2,0} | call ReplaceCodeMacroProc {#1,0} &0 #3 #4 ]

[ReplaceByAddressingOperator
mov B$esi-2 #1
lea edi D$esi   | call ShiftLineLeft 1
lea edi D$esi-2 | call ShiftLineLeft 1
sub esi 2]


[Test_If    |#=2| test #1 #2 | jz T0> ]
[.Test_If   |#=2| test #1 #2 | jz T1>>]
[..Test_If  |#=2| test #1 #2 | jz T2>>]
[...Test_If |#=2| test #1 #2 | jz T3>>]

[Test_If_Not     |#=2| test #1 #2 | jnz T0> ]
[.Test_If_Not    |#=2| test #1 #2 | jnz T1>>]
[..Test_If_Not   |#=2| test #1 #2 | jnz T2>>]
[...Test_If_Not  |#=2| test #1 #2 | jnz T3>>]


[Test_Else    | jmp T5>  | T0: ]
[.Test_Else   | jmp T6>> | T1: ]
[..Test_Else  | jmp T7>> | T2: ]
[...Test_Else | jmp T8>> | T3: ]

[Test_End    | T0: | T5: ]
[.Test_End   | T1: | T6: ]
[..Test_End  | T2: | T7: ]
[...Test_End | T3: | T8: ]



[Test_If_And    | Test_If    #1 #2 | #+2]
[.Test_If_And   | .Test_If   #1 #2 | #+2]
[..Test_If_And  | ..Test_If  #1 #2 | #+2]
[...Test_If_And | ...Test_If #1 #2 | #+2]

[Test_If_Not_And    | Test_If_Not    #1 #2 | #+2]
[.Test_If_Not_And   | .Test_If_Not   #1 #2 | #+2]
[..Test_If_Not_And  | ..Test_If_Not  #1 #2 | #+2]
[...Test_If_Not_And | ...Test_If_Not #1 #2 | #+2]


[Test_Else_If    | Test_Else    | Test_If    #1 #2]
[.Test_Else_If   | .Test_Else   | .Test_If   #1 #2]
[..Test_Else_If  | ..Test_Else  | ..Test_If  #1 #2]
[...Test_Else_If | ...Test_Else | ...Test_If #1 #2]

[Test_Else_If_Not    | Test_Else    | Test_If_Not    #1 #2]
[.Test_Else_If_Not   | .Test_Else   | .Test_If_Not   #1 #2]
[..Test_Else_If_Not  | ..Test_Else  | ..Test_If_Not  #1 #2]
[...Test_Else_If_Not | ...Test_Else | ...Test_If_Not #1 #2]


[Test_Else_If_And    | Test_Else    | Test_If_And    #F>L ]
[.Test_Else_If_And   | .Test_Else   | .Test_If_And   #F>L ]
[..Test_Else_If_And  | ..Test_Else  | ..Test_If_And  #F>L ]
[...Test_Else_If_And | ...Test_Else | ...Test_If_And #F>L ]

[Test_Else_If_Not_And    | Test_Else    | Test_If_Not_And    #F>L ]
[.Test_Else_If_Not_And   | .Test_Else   | .Test_If_Not_And   #F>L ]
[..Test_Else_If_Not_And  | ..Test_Else  | ..Test_If_Not_And  #F>L ]
[...Test_Else_If_Not_And | ...Test_Else | ...Test_If_Not_And #F>L ]



[If_Or    | cmp #1 #3 | j#2 O0>  | #+3 | jmp I1>  | O0: ]
[.If_Or   | cmp #1 #3 | j#2 O1>> | #+3 | jmp J1>> | O1: ]
[..If_Or  | cmp #1 #3 | j#2 O2>> | #+3 | jmp K1>> | O2: ]
[...If_Or | cmp #1 #3 | j#2 O3>> | #+3 | jmp Z1>> | O3: ]

[Else_If_Or    | Else    | If_Or    #F>L]
[.Else_If_Or   | .Else   | .If_Or   #F>L]
[..Else_If_Or  | ..Else  | ..If_Or  #F>L]
[...Else_If_Or | ...Else | ...If_Or #F>L]


[If_And    | If #1 #2 #3    | #+3]
[.If_And   | .If #1 #2 #3   | #+3]
[..If_And  | ..If #1 #2 #3  | #+3]
[...If_And | ...If #1 #2 #3 | #+3]

[Else_If_And    | Else    | If_And    #F>L]
[.Else_If_And   | .Else   | .If_And   #F>L]
[..Else_If_And  | ..Else  | ..If_And  #F>L]
[...Else_If_And | ...Else | ...If_And #F>L]




; * max string length = 10
; * OK if #3 is included in #1, jump to next 'If' otherwise
;
; ex:
;   If_str eax = 'glouglou'
;       ;...
;   Else
;       ;...
;   End_If

[.If_str | {&0: #3,0} | mov esi &0 | mov edi #1 | mov ecx 10 | repe cmpsb
         On ecx = 0, jmp J1>  | On B$esi-1 <> 0, jmp J1> ]

[.Else_If_str| .Else | .If_str #1 #2 #3]


____________________________________________________________________________________________
EPP_All_Data:
[EPP_SourceEquation: B$ ? #256   EPP_SourceEquation_End: B$ ?]  ; last char is EOI
[EPP_DestOperand: B$? #64]     ; last char is Space

[EPP_SourceEquation_Backup: B$ ? #256 ]
[EPP_DestOperand_Backup: B$? #64]
[EPP_atofAddress: B$ ? #9] ; null-terminated
[EPP_ftoaAddress: B$ ? #9] ; null-terminated
[EPP_ImmediateResultString: B$ ? #32]

[EPP_InternalMeaninglessLabels: B$ ? #(9*10)]   ; 8 and 9 are reserved for Evaluation parser


[EPP_SearchedSingleCharacter: B$ ? #SEARCH_VARIABLE_NUMBER]
[EPP_SearchedStringAddress:   D$ ? #SEARCH_VARIABLE_NUMBER]
[EPP_ReplacementBuffer: B$ ? #128 ]

[EqualPreparser_DestInvalid: B$             'Destination operand is invalid.' 0
 EqualPreparser_SourceInvalid:              'Source expression is invalid.' 0
 EqualPreparser_UnexpectedError:            'Unexpected preparsing error.' 0
 EqualPreparser_InvalidIndex:               'Invalid index type.' 0
 EqualPreparser_InvalidAddress:             'Invalid address.' 0
 EqualPreparser_OperatorsInvalid:           'Two operators are following each other.' 0
 EqualPreparser_ParameterInvalid:           'Invalid integrated function parameter.' 0
 EqualPreparser_InvalidFPValue:             'Invalid floating point value.' 0
 EqualPreparser_OptimisationError:          'Error occurred while optimizing. Please report this error :)' 0
 EqualPreparser_InvalidFunction:            'No such function implemented.' 0
 EqualPreparser_InvalidStringDest_Type:     'A string pointer MUST be a DWord .' 0
 EqualPreparser_InvalidStringDest_Numeric:  'You cannot write at a numeric address.' 0
 EqualPreparser_InvalidStringinSource:      'Invalid string in source operand.'  0
 EqualPreparser_InvalidImmVal:              'Invalid immediate expression.'  0
 EqualPreparser_LocalStackOverflow:         'Local stack overflow !'  0

FPUInSourceExpression: B$ 0   UsedRegisters_Backup: 0   UsedRegisters: 0   NoOperation: 0  ParsingInsideArray: 0
DestinationArrayRegister: 0    UseCommonAddressingSyntax: &TRUE   StrNull: 0]

[EPP_ImmediateResult: T$ ?]

[EPP_CodeBegining: ?   EPP_Operand1: ?     EPP_Operand2: ?  EPP_WrittenCode:  ?  FPUStackNumber: ?
 DestinationType: ?  EPP_TextPointer: ?   EPP_LastStorageStatement: ?   EPP_LastStorageStatement_InMemory: ?]


 [INITIAL_STATE 07FFFFFFF
  FOUND_EQUATION_TEST 0FFFFFF
  SEARCH_VARIABLE_NUMBER  9

  EPP_EAX 00______1
  EPP_EBX 00_____10
  EPP_ECX 00____100
  EPP_EDX 00___1000
  EPP_ESI 00__10000
  EPP_EDI 00_100000

  EPP_NO_OPERATION        2
  CODE_NOT_FOUND          0_FFFF_FFFF
  EPP_CODE_BEGIN              0
  EPP_CODE_END                0
  EPP_SOURCE_BEGIN            1
  EPP_SOURCE_END              1

  SignedMulSign  '*'
  SignedDivSign  '/'

  pointSign      'o'
  concatSign     '&'

  ; +,- and * used for addressing in RosAsm syntax
  minusSign      'm'
  plusSign       'p'
  addressSign    'x'

  ; All data types used in this TITLE are below
  EPP_Types 0

        FunctionOperand      00__0000_1000__0000_0000
        NumericOperand       00__0100_0000__0000_0000
        RegisterOperand      00__0010_0000__0000_0000
        ConstantOperand      00__0001_0000__0000_0000

        MemoryOperand        00__0000_0001__0000_0000
                QWordValue   00_____________0000_1000
                DWordValue   00_____________0000_0100
                WordValue    00_____________0000_0010
                ByteValue    00_____________0000_0001

        FloatingPointOperand 00_____________1000_0000
                  TByteValue 00_____________0001_0000
                  RealValue  00_____________0000_1000
                  FloatValue 00_____________0000_0100





  EqualPreparser_IntegerOperand_ST0            00________1
  EqualPreparser_ST0_IntegerOperand            00_______10
  EqualPreparser_FloatingPointOperand_ST0      00______100
  EqualPreparser_ST0_FloatingPointOperand      00_____1000
  EqualPreparser_ST1_ST0                       00____10000
  EqualPreparser_ST0_ST1                       00___100000
  EqualPreparser_ST0_ConstantOperand           00__1000000
  EqualPreparser_ConstantOperand_ST0           00_10000000]
____________________________________________________________________________________________




____________________________________________________________________________________________
____________________________________________________________________________________________
;DEBUG

[TempSourceBuffer: ? #1024]

[ShowSourceTitle: 'SOURCE ' ]
[ShowSourceTitle_ID: 0 0 0 ]                    ; [<ShowSourceTitle_ID: 0 0 0 ]  disables the breakpoints !!!!!
[FirstEqualLine: &TRUE   LastEqualLine: 0]

; call ShowSource edi 1    ; show from edi and diplay 1 in the title
; call ShowSource 0 7      ; show the whole written code and diplay 7 in the title
; call ShowSource ebx 0    ; show the line where ebx points on
Proc ShowSource:
    Arguments @ShowFrom @Title

    pushad

        mov ebx, D@Title | mov edi ShowSourceTitle_ID | inc edi
        std
            mov ecx, 2
L1:         mov al bl | and al 0F | On al >= 0A, add al 7
            add al, '0' | stosb | shr ebx, 4 | loop L1
        cld

        If D@ShowFrom = 0
            mov esi D$LastEqualLine
            ;While B$esi <> EOI | inc esi | End_While | inc esi  ; shows instruction after the current one
            ;While B$esi <> EOI | dec esi | End_While | dec esi  ; shows instruction before the current one
            ;While B$esi <> EOI | dec esi | End_While | inc esi
            mov edx &TRUE ; only one line is displayed
        Else_If D@Title = 0
            ;While B$esi <> EOI | inc esi | End_While | inc esi  ; shows instruction after the current one
            ;While B$esi <> EOI | dec esi | End_While | dec esi  ; shows instruction before the current one
            mov esi D@ShowFrom
            While B$esi <> EOI | dec esi | End_While | inc esi
            mov edx &FALSE
        Else
            ; shows from pointer
            mov esi D@ShowFrom
            mov edx &TRUE ; only one line is displayed
        End_If

        ;mov ebx D$LastEqualLine | While B$ebx <> EOI | inc ebx | End_While
        ; ebx points on the end of the source currently written

        mov ecx 0
        mov edi TempSourceBuffer

        ; Each charater is replaced by its source equivalent so as to being diplayed.
        ; If a character is neither alphanumeric nor a RosAsm operator, it is replaced by '~' so as to be easily locatable.

L0:     lodsb
        .If al = TextSign
            mov al '"'
        .Else_If al = pointSign
             mov al '.'
        .Else_If al = minusSign
            mov al '-'
        .Else_If al = plusSign
            mov al '+'
        .Else_If al = SignedDivSign
            mov al '/'
        .Else_If al = SignedMulSign
            mov al '*'
        .Else_If al = numSign
            mov al '#'
        .Else_If al = CommaSign
            mov al ','
        .Else_If al = OpenVirtual
            mov al '{'
        .Else_If al = CloseVirtual
            mov al '}'
        .Else_If al = Openbracket
            mov al '['
        .Else_If al = Closebracket
            mov al ']'
        .Else_If al = memMarker
            mov al '$'
        .Else_If al = colonSign
            mov al ':'
        .Else_If al = openSign
            mov al '('
        .Else_If al = closeSign
            mov al ')'
        .Else_If al = addSign
            mov al '+'
        .Else_If al = subSign
            mov al '-'
        .Else_If al = mulSign
            mov al '*'
        .Else_If al = divSign
            mov al '/'
        .Else_If al = expSign
            mov al '^'
        .Else_If al = '{'
            mov al '{'
        .Else_If al = '}'
            mov al '}'
        .Else_If al = '.'
            mov al '.'
        .Else_If al = Space
            mov al '_'
        .Else_If al = 0
            mov al '@'
        .Else_If al = EOI
            mov al '|'
            On edx = &TRUE, jmp L9>
            On esi >= ebx, jmp L9>
            mov ax 0A0D | stosw
            mov al 13 | stosb
            mov al 10
        .Else_If al = meEOI
            mov al 13 | stosb
            mov al 10
        .Else_If al < 48
            mov al '~'
        .Else_If al > 90
            mov al '~'
        .End_If

        stosb
        inc ecx

        jmp L0<<

L9:     mov B$edi 0

        ;call 'USER32.DialogBoxParamA' D$hInstance 23000  &NULL EqualDebugProcProc  &NULL
        call 'USER32.MessageBoxA' D$hwnd, TempSourceBuffer, ShowSourceTitle, &MB_ICONINFORMATION
    popad


EndP
____________________________________________________________________________________________
Proc EqualDebugProcProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        mov eax &TRUE
        call 'User32.SetWindowTextA' D@Adressee ShowSourceTitle
        call 'User32.SetDlgItemTextA' D@Adressee 101 TempSourceBuffer

    Else_If D@Message = &WM_CLOSE
        call 'User32.EndDialog' D@Adressee 0


    Else
        popad | mov eax &FALSE | jmp L9>

    End_If

    popad | mov eax &TRUE

L9: EndP

