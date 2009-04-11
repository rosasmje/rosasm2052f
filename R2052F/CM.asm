TITLE CM

;;
  'NewConditionalMacroSubstitution'


  Conditional Macros
  
  'MacroWithIf', is called from the normal 'ReplaceOneMacro' Routine, before calling
  for the 'ReplaceFromMacroData' Routine, that does all of the Macros Replacements
  jobs. If a "#If something" is found out, 'MacroWithIf' runs that Conditional Macros
  Parser
  
  The Conditional macro parser job is a substitution one: It reads the Body Macro
  and modifed it, depending on the Conditionals, on the Parameters states, and so on.
  
  It calls forst to 'ExpandMacroOnParametersLoop', that, as its name says, expands
  the Macro Body accordingly, for example, to the "#+1" key, so that the Conditions
  of each Parameter could be considered.
  
  The, it call for 'ConditionalMacroSubstitution' that does all of the Conditional
  stuff, and finaly to the 'CheckUserDefinedError' stuff, if a user defined error
  must be executed.
  
  For this implementation, a new family of Internal Variables was created. Before,
  we had the "&1,... &99" internal Variables ('MacrosVariablesTable') that are 128
  Bytes Tables, where to store any String. The new family is from "&&0" to "&&99",
  and are a set of 100 dWords Internal Counters ('MacroCounters').
  
  Each Macro Counter may hold any dWord and may be tested as such by the Conditional
  Macros Engine. But, for _writing_, the stored _BYTE_ is considered. This feature
  enable with Chars manipulations, in the Macros buildings, and are particulary usefull
  for defining Local Labels, for example, from I0 to I9, when storing '0' (Ascii 48),
  into an Internal Counter, that can be incremented / decremented, in complex macros
  Sets, for HLL Constructs.
  
  Plus, these Internal Counters, that are now set up by the normal macros, at
  'ReplaceFromMacroData' >>> 'StoreMacroVariableByNumber', can also hold a special
  record, for example, by "&&2=Pos", to the actual Source Pointer, that is used
  by the Errors Manager when pointing out to an error, inside the Source. This
  feature can be used by the Conditional Macros Parser, in case of "User Defined
  Macro Error", so that the Source Pointing Location could be defined at another
  location that the one of the actual error Detection.
  
  This is usefull for unpaired HLL Construct: Instead of pointing to the Statement
  where the error has been detected (say, an "If" / "While"...), it can point to the
  _previous_ "If" / "While", that was the real unpaired Construct.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


[CmStart: ?    UserMacroEnd: ?]

;;
  Here, we just parse the original macro Body. If some #If construct is found,
  we call for the creation of the Macro Body substitute, depending on the
  matching, between the Conditions and the Macro Statement parameters.
;;

[UserDefinedError: ?   UserMacroLength: ?]

InitMacroWithIf:
  ; '#IF' found. Take a work Copy into Trash1:
    mov D$Trash1 0, D$Trash2 0
    mov D$Trash1ptr, Trash1, D$Trash2Ptr Trash2
    mov esi D$CmStart, edi D$Trash1ptr, ecx D$UserMacroLength
    rep movsb | mov D$edi 0

    mov B$UserDefinedError &FALSE, B$ErrorWhileUnfoldingCM &FALSE
ret

MacroWithIf:
    push esi, edi, ecx

        mov D$CmStart esi | mov D$UserMacroEnd esi | add D$UserMacroEnd ecx
        mov D$UserMacroLength ecx
      ; Cooked '#' ?
L0:     cmp B$esi numSign | jne L1>>
            cmp W$esi+1 'IF' | jne L1>
                cmp B$esi+3 Space | ja L1>
                    call InitMacroWithIf

                    call ExpandMacroOnParametersLoop

                    call ConditionalMacroSubstitution

                    On B$UserDefinedError = &TRUE, call CheckUserDefinedError

                  ; Do the final substitution:
                  ; esi will point to a Trash Table and ecx will be the new length.
                    mov esi D$Trash1Ptr | pop eax, edi, eax | ret

L1:     inc esi | loop L0<
    pop ecx, edi, esi
ret
____________________________________________________________________________________________

;;
  If a Macro ends, for example, with some '#+2' loop, we unfold the Macro Loop to
  reflect the Parameters in the Macro definition, by doing a substitution of the
  Declaration.
  
  Why, is because, if the '#If' are inside a loop, the Parameters checking must
  be made fiting with each case of the Conditional job.
;;

[EvocationParametersNumber: ?    GoExpandLoop: ?    InsideCondition: ?   LoopIndice: ?]

ExpandMacroOnParametersLoop:
    mov B$GoExpandLoop &FALSE, B$InsideCondition &FALSE, D$LoopIndice 0

    call CmParametersCount

    If ecx = 0
        mov D$EvocationParametersNumber 0 | ret
    Else_If ecx > 100
        error TooMuchCMParam
    Else
        mov D$EvocationParametersNumber ecx
        call AnalyzeOfAllParameters
    End_If

    mov esi D$Trash1ptr

  ; Is there any '#+x' Macro Loop?
    .While B$esi <> 0
        ...If B$esi = NumSign
            ..If B$esi+1 = AddSign
                On B$GoExpandLoop = &TRUE, call CMerror D$NestedMacroLoopPtr, esi, D$Trash1ptr
                On B$InsideCondition = &TRUE, call CMerror D$ConditionalLoopPtr, esi, D$Trash1ptr

                mov B$GoExpandLoop &TRUE

                add esi 2 | call GetIndice1_99 | mov ecx ebx
                mov eax D$EvocationParametersNumber
              ; ecx = #+x Num // eax = Number of Parameters in the Macro Evocation
                .If eax > ecx
                  ; Does the number match with the Evocation Number of Parameters?
                    mov edx 0 | div ecx
                    On edx <> 0, call CMerror D$MacParaPtr, esi, D$Trash1ptr

                .Else_If eax < ecx
                    call CMerror D$MacParaPtr, esi, D$Trash1ptr

                .Else
                ; If eax = ecx + End of MacroBody: No neeed to expand, but strip the '#+X'
                    If B$esi = 0
                        While B$esi <> NumSign | dec esi | End_While
                        mov B$esi 0 | ret
                    End_If

                .End_If

            ..Else_If W$esi+1 = 'IF'
                mov B$InsideCondition &TRUE

            ..Else_If D$esi+1 = 'ENDI'
                mov B$InsideCondition &FALSE

            ..End_If
        ...End_If

L5:     inc esi
    .End_While

    mov D$LoopIndice ecx

    ...If B$GoExpandLoop = &TRUE
      ; ecx = Loop Step to be added to the Parameters:
        mov edi D$Trash2ptr

L0:     mov esi D$Trash1ptr

        .While B$esi <> 0
            lodsb
            .If al = NumSign
                If B$esi = AddSign
                  ; #+x found: Add x to the ecx to be add to the expressed Parameters, and loop:
                    inc esi | call GetIndice1_99
                    add ecx ebx | cmp ecx D$EvocationParametersNumber | jbe L0<
                      ; esi >>> Next Char after '#+x'. Remove the remaining '|', if any:
                        On B$esi < Separators, inc esi
                        On B$edi-1 < Separators, dec edi

                Else_If B$esi < '1'
                    stosb

                Else_If B$esi <= '9'
                  ; Write '#':
                    stosb
                  ; Make the (Parameter '#x') Text Number, a Number, in ebx:
                    call GetIndice1_99 | dec esi | mov eax ebx
                    add eax ecx | sub eax D$LoopIndice
                    call WriteEaxDecimal

                Else
                   stosb
                End_If

            .Else
                stosb

            .End_If
        .End_While

        mov D$edi 0

        Exchange D$Trash1ptr D$Trash2ptr
    ...End_If
ret
____________________________________________________________________________________________

WriteEaxDecimal:
    push ecx, edx
        mov dl 0FF | push edx
        mov ecx 10
L0:     mov edx 0
        div ecx | push edx | cmp eax 0 | ja L0<
L2:     pop eax
        cmp al 0FF | je L9>
        add al '0' | stosb | jmp L2<
L9: pop edx, ecx
ret
__________________________________________________________________________________________

[Trash1ptr: ?    Trash2Ptr: ?]

;;
  To parse all of the possible #If Constructs, with possible nested levels, and so on...
  we use _TWO_ Tables. One is the Source, the other the Destination. When one Pass
  is finished, we 'Exchange' the Tables Pointer, and loop it all, until no more '#If'
  could be found out.
;;

ConditionalMacroSubstitution:
    call FlagConditionals
    call ParseTheConditionals
    call Remove0FFs
ret


Remove0FFs:
    mov esi D$Trash1ptr, edi esi

    While B$esi <> 0
        ..If B$esi = 0FF
L0:         inc esi | cmp B$esi 0FF | je L0<
                .If B$esi = 0
                  ; End
                    If edi = D$Trash1ptr
                        mov D$edi 'NOPE', B$edi+4 0, ecx 4 | ret
                    Else
                        On B$edi-1 =< EOI, dec edi
                    End_If
                .Else_If B$edi-1 <= EOI
                  ; Don't output two EOI/meEOI:
                    cmp B$esi EOI | jbe L0<
                .Else_If B$esi > EOI
                  ; Ensure, at least, one meEOI:
                    mov B$edi meEOI | inc edi
                .Else
                    movsb
                .End_If
        ..Else
            movsb
        ..End_If
    End_While

    While B$edi-1 =< EOI | dec edi | End_While

    mov B$edi 0

    mov ecx edi | sub ecx D$Trash1ptr
ret
____________________________________________________________________________________________

[LastIfPos: ?]

Proc ParseOneIf:
    Argument @Base, @Indice
    Uses ecx

      ; esi points  at"#IF ":
        mov esi D@Base | add esi 4

L0:     call GetIfCondition

        mov cl B@Indice

        .If B$CmCondition = &TRUE
          ; Arase the #If Statement:

            mov ebx D@Base
L1:         mov B$ebx 0FF | inc ebx | cmp ebx esi | jb L1<
            dec esi
          ; Keep the wanted Statements:
L2:         inc esi | cmp B$esi IfNumSign | jne L2<
                      If B$esi+1 = IF_FLAG
                          mov eax D@Indice | inc eax
                          call ParseOneIf esi, eax
                      End_If
                      cmp B$esi+2 cl | jne L2<
          ; Arase everything down to the matching #ENDIF:
L3:         mov B$esi 0FF | inc esi
            cmp B$esi ENDIF_FLAG | jne L3<
            cmp B$esi+1 cl | jne L3<
          ; Arase the #ENDIF Statement:
            mov W$esi 0FFFF | add esi 2
L4:         mov B$esi 0FF | inc esi | cmp B$esi EOI | ja L4<


        .Else
          ; Arase the #If Statement:

            mov ebx D@Base
L5:         mov B$ebx 0FF | inc ebx | cmp ebx esi | jb L5<

          ; Arase the whole #IF Block down to the matching #ENDIF // #ELSEIF // #ELSE:
L6:         mov B$esi 0FF | inc esi
            cmp B$esi IfNumSign | jne L6<
            cmp B$esi+2 cl | jne L6<
            If B$esi+1 = ELSEIF_FLAG  ; "#ELSEIF "
                mov D@Base esi | add esi 8 | jmp L0<<
            Else_If B$esi+1 = ELSE_FLAG ; "#ELSE"
                mov ebx esi | add esi 5 | jmp L1<<
            End_If
          ; Arase the #ENDIF Statement:
            mov W$esi 0FFFF | add esi 2
L7:         mov B$esi 0FF | inc esi | cmp B$esi EOI | ja L7<
            mov B$esi 0FF

        .End_If
EndP
____________________________________________________________________________________________

ParseTheConditionals:
    mov esi D$Trash1ptr

    While B$esi <> 0
        ..If B$esi = IfNumSign
            If B$esi+1 = IF_FLAG
                call ParseOneIf esi, 1
            End_If
        ..End_If

        inc esi
    End_While
    mov esi D$Trash1ptr
ret
____________________________________________________________________________________________
;;
  Flaging:
  
  The 'NumSign' Char of #If and friends are replaced by a dedicated 'IfNumSign',
  that makes further parsing easier. The first Char after # is replace by 1, 2,
  3, 4, respectively, for 'IF', 'ELSEIF', 'ELSE' and 'ENDIF, and the third Char
  is replaced by the nesting indice (1,... 0FF).
  
  Example: Level 4 "#IF" >>> Bytes: IfNumSign(01B), IF_FLAG(1), 4
;;

[IF_FLAG 1, ELSEIF_FLAG 2, ELSE_FLAG 3, ENDIF_FLAG 4]

FlagConditionals:
    mov esi D$Trash1ptr, ecx 0

    .While B$esi <> 0
        ...If B$esi = NumSign
            ..If W$esi+1 = 'IF'
                mov B$esi IfNumSign
                inc cl | mov B$esi+1 IF_FLAG, B$esi+2 cl

            ..Else_If D$esi+1 = 'ELSE'
                mov B$esi IfNumSign
                mov B$esi+2 cl
                If B$esi+5 < Separators
                    mov B$esi+1 ELSE_FLAG
                Else
                    mov B$esi+1 ELSEIF_FLAG
                End_If

            ..Else_If D$esi+1 = 'ENDI'
                mov B$esi IfNumSign
                mov B$esi+1 ENDIF_FLAG, B$esi+2 cl | dec cl
                On ecx = 0FF, error D$UnpairedMacroIfPtr

           ..Else_If D$esi+1 = 'ERRO'
                mov B$UserDefinedError &TRUE

            ..End_If

        ...End_If

        inc esi
    .End_While

    On ecx <> 0, error D$UnpairedMacroIfPtr
ret
____________________________________________________________________________________________
;;
  Some '#Error' has been found out inside the initial macro Declaration.
  If it is still in the Conditional substitution, we run the Usr Error Message
;;

[ConditionalErrorString: '#Error Syntax is: #Error "My error message"', 0
 ConditionalErrorPosString: '#Error Syntax is: #Error &&24 "My error message"', 0
 BadStatementCounter: 'Bad Statement Counter in the Macro internal Counter', 0]

[ErrorWhileUnfoldingCM: ?]

CheckUserDefinedError:
    mov esi D$Trash1Ptr

    .While B$esi > 0
        ...If B$esi = numSign
            ..If D$esi+1 = 'ERRO'
                .If D$esi+5 = 'RPOS'
                    While W$esi <> '&&'
                        inc esi
                        On B$esi = TextSign, error ConditionalErrorPosString
                        On B$esi = 0, error ConditionalErrorPosString
                    End_While

                    add esi 2
                    mov ebx 0, eax 0
L0:                 lodsb
                    cmp al '9' | ja L9>
                    cmp al '0' | jb L9>
                    cmp al LowSigns | jb L9>
                        sub al '0'
                        lea ebx D$ebx+ebx*4
                        lea ebx D$eax+ebx*2
                    jmp L0<

                  ; Set in 'SaveStatementCounter':
L9:                ; lea ebx D$MacroCounters+ebx*4

                    If ebx = 0
                        jmp L7>
                    Else_If ebx > 100
                        jmp L7>
                    Else
                        mov ebx D$MacroCounters+ebx*4
                        On ebx < D$StatementsTable, jmp L7>
                        mov eax D$ebx
                    End_If

                    If eax < D$CodeSource
                        jmp L7>
                    Else_If eax > D$SourceEnd
L7:                     error BadStatementCounter
                    Else
                        mov D$StatementsPtr ebx
                    End_If

                .End_If

              ; Scan for first TextSign:
                While B$esi <> TextSign
                    inc esi | On B$esi = 0, error ConditionalErrorString
                End_While
                inc esi | mov edi TrashString

              ; Scan for second TextSign:
                While B$esi <> TextSign
                    movsb | On B$esi = 0, error ConditionalErrorString
                End_While
                mov B$edi 0

                mov B$ErrorWhileUnfoldingCM &TRUE | error TrashString
            ..End_If

        ...End_If

        inc esi
    .End_While
ret
____________________________________________________________________________________________

[CmCondition: ?]

GetIfCondition:
; esi point to the Condition, right after the '#If ". Example: '#1<>', '#N=', '&55=',...

    lodsb
    ...If al = NumSign
        lodsb
        ..If al = 'N'
          ; Found: '#If #N'
            call GetNCondition

        ..Else_If al = 'L'
            ; Found: '#If #L'
                mov ebx D$EvocationParametersNumber | call GetParamCondition

        ..Else_If al = 'F'
                mov ebx 1 | call GetParamCondition

        ..Else_If al >= '1'
            If al =< '9'
              ; Found: '#If #1' ... '#If #99'
                dec esi
                call GetIndice1_99 | call GetParamCondition

            Else
                call CMerror D$BadConditionalmacroPtr, esi, D$Trash1ptr

            End_If

        ..End_If

    ...Else_If al = '&'
        If B$esi = '&'
            inc esi
            call GetIndice0_99 | dec esi
            call GetInternalCounterCondition

        Else
            call GetIndice1_99 | dec esi
            call GetInternalVariableCondition
        End_If

    ...Else
        call CMerror D$BadConditionalmacroPtr, esi, D$Trash1ptr

    ...End_If
ret


GetNCondition:
;;
  '#If #n' found. 'n' means 'Number of Parameters. This Number will be computed by the Macros Parser,
  again, but, as we need to know it now, we compute it 'privately', here, for this specific purpose.
  No matter if stupid: Simpler.
;;
    mov ecx D$EvocationParametersNumber
  ; Parameters Number in ecx. Take the condition Char:
    ;lodsb
    mov ax W$esi | inc esi

    ..If ax = '<='
        inc esi
        call GetIndice0_99
        If ecx <= ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If ax = '=<'
        inc esi
        call GetIndice0_99
        If ecx =< ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If ax = '>='
        inc esi
        call GetIndice0_99
        If ecx >= ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If ax = '=>'
        inc esi
        call GetIndice0_99
        If ecx => ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If ax = '<>'
        inc esi
        call GetIndice0_99

        If ecx <> ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If al = '='
      ; compute the expected Decimal into Binary:
        call GetIndice0_99

        If ecx = ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If al = '<'
        call GetIndice0_99

        If ecx < ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else_If al = '>'
        call GetIndice0_99

        If ecx > ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    ..Else
        call CMerror D$BadConditionalmacroPtr, esi, D$Trash1ptr

    ..End_If
ret
____________________________________________________________________________________________

[Over100CmIndice:
'A Condition is testing a Parameter indice bigger than the parameters Number', 0]

GetParamCondition:
  ; Parameter number in ebx ('AnalyzeOfAllParameters')
    dec ebx
    On ebx > D$EvocationParametersNumber, error Over100CmIndice
    move D$CmParamType D$AllParametersTypesAnsSizes+ebx*8
    move D$CmParamSize D$AllParametersTypesAnsSizes+ebx*8+4
    ;call AnalyzeOfCmParameter

    call AnalyzeOfIfExpectation
  ; >>> ebx = D$CmParamType or D$CmParamSize // eax = 'MEM', 'REG,... , 'D', 'W',...

    .If B$esi-1 = '='
        If eax = ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    .Else_If W$esi-1 = '<>'
        If eax <> ebx
            mov B$CmCondition &TRUE
        Else
            mov B$CmCondition &FALSE
        End_If

    .End_If

    While B$esi > LowSigns | inc esi | End_While | inc esi
ret
____________________________________________________________________________________________

GetInternalCounterCondition:
  ; Internal Counter Indice in ebx
    On ebx > MACRO_VARIABLES, error D$MacroVariableIndicePtr
    mov ecx D$MacroCounters+ebx*4

    mov B$CmCondition &FALSE

  ; Take the Condition case:
    .If B$esi = '='
        inc esi | mov edx '='
        If B$esi = '<'
            inc esi | mov edx '<='
        Else_If B$esi = '>'
            inc esi | mov edx '>='
        End_If
    .Else_If W$esi = '<>'
        add esi 2 | mov edx '<>'
    .Else_If B$esi = '<'
        inc esi | mov edx '<'
        If B$esi = '='
            inc esi | mov edx '<='
        End_If
    .Else_If B$esi = '>'
        inc esi | mov edx '>'
        If B$esi = '='
            inc esi | mov edx '>='
        End_If
    .Else
        error UnexpectedCondition
    .End_If

    On B$esi = Space, inc esi

  ; Read the Number >>> ebx will be, for example, the "39" in "#If &&1>39":
    push ecx, edx
        If B$esi = TextSign
            inc esi | call GetAttributionChar | inc esi
        Else
          ; The 'GetAttributionNumber' of the old Macro Engine makes stupid use of ecx:
            mov ecx 0FF
            call GetAttributionNumber
        End_If
    pop edx, ecx

  ; Compare. ecx = Internal Counter // ebx = Wished Condition:
    If edx = '='
        On ecx = ebx, mov B$CmCondition &TRUE
    Else_If edx = '<>'
        On ecx <> ebx, mov B$CmCondition &TRUE
    Else_If edx = '<'
        On ecx < ebx, mov B$CmCondition &TRUE
    Else_If edx = '>'
        On ecx > ebx, mov B$CmCondition &TRUE
    Else_If edx = '<='
        On ecx <= ebx, mov B$CmCondition &TRUE
    Else_If edx = '>='

        On ecx >= ebx, mov B$CmCondition &TRUE
    End_If
ret
____________________________________________________________________________________________

[UnexpectedCondition: 'Unexpected Condition in Macro Declaration', 0]

GetInternalVariableCondition:
  ; Internal Variable Indice in ebx
    On ebx > MACRO_VARIABLES, error D$MacroVariableIndicePtr

  ; WriteMacroVariable // MacrosVariablesTable >>> 128 Bytes for each Variable
    shl ebx 7 | add ebx MacrosVariablesTable

    mov B$CmCondition &FALSE

    .If B$esi = '='
        inc esi
        If B$esi = '0'
            On B$ebx = 0, mov B$CmCondition &TRUE
        Else
            error UnexpectedCondition
        End_If

    .Else_If W$esi = '<>'
        add esi 2
        If B$esi = '0'
            inc esi
            ; jE! correction!
            On B$ebx <> 0, mov B$CmCondition &TRUE
        Else
            error UnexpectedCondition
        End_If

    .Else
        error UnexpectedCondition

    .End_If
ret
____________________________________________________________________________________________

[CmParamType: ?    CmParamSize: ?]

L7: ;showme D$Trash1ptr
    error BadConditionalmacro

AnalyzeOfCmParameter:
  ; Param Number in ebx
    push esi
    mov esi D$InstructionAptr, ecx 0

L0: inc esi | cmp B$esi EOI | jbe L7<
              cmp B$esi Space | jne L0<
              inc ecx | cmp ecx ebx | jb L0<

    inc esi ; >>> Firt Param Char.

    mov D$CmParamType 0, D$CmParamSize 0
;;
  esi point to the 'ebx' Parameter? What is it?
  register, memory, immediate, WinEquate, Symbol, string
  Byte, Word, dWord, qWord, xWord, FPU4, FPU8, FPU10
;;

    ...If B$esi = TextSign
        mov D$CmParamType 'STR'

    ...Else_If B$esi+1 = memMarker
        mov D$CmParamType 'MEM', al B$esi | and al 00_01111111 | mov B$CmParamSize al

    ...Else
      ; Rip the 'High Bit Done Flag' if any:
        push D$esi, esi
            and B$esi 00_01111111
            call IsItAreg


        ..If ah <> 0
          ; The Parameter is a reg
            mov al B$OneOperandwBit

            .If al = ByteSize
                    mov al 'B'

            .Else_If al = wordSize
                    mov al 'W'

            .Else_If al = doubleSize
                If ah = STreg
                    mov al 'F'
                Else
                    mov al 'D'
                End_If

            .Else_If al = QuadSize
                If al = STreg
                    mov al 'R'
                Else
                    mov al 'Q'
                End_If

            .Else_If al = TenSize
                mov al 'T'

            .Else  ;_If al = Xsize
                mov al 'X'

            .End_If

            mov D$CmParamType 'REG', B$CmParamSize al

        ..Else_If B$esi < '0'
          ; Might be a Win32 Equate or an error
            mov D$CmParamType 'SYM', B$CmParamSize 'D'

        ..Else_If B$esi <= '9'
            mov D$CmParamType 'IMM', B$CmParamSize 'D'

        ..Else
          ; Anything else is condidered a Symbol (Equate, Label, Win32 Equate)
            mov D$CmParamType 'SYM', B$CmParamSize 'D'

        ..End_If

        pop esi, D$esi

    ...End_If

L9: pop esi
ret
____________________________________________________________________________________________

[AllParametersTypesAnsSizes: ? ? #100]

[TooMuchCMParam: 'More than 100 Parameters with this macro Evocation', 0]

AnalyzeOfAllParameters:
    mov edi AllParametersTypesAnsSizes, ebx 1
  ; ecx is previously set by 'CmParametersCount'
L0: push edi, ebx, ecx
        call AnalyzeOfCmParameter
    pop ecx, ebx, edi
    mov eax D$CmParamType | stosd
    mov eax D$CmParamSize | stosd

    inc ebx | loop L0<
ret
____________________________________________________________________________________________

;;
  esi point after '#If #1=' // '#If #L<' // ...
  
  this should be, for example: '#If #1=D' or '#If #1=str'
  
  Here, we do nothing but load, in eax, the #If= Condition. If the Condition is a Type, we
  load the CmParamType, given by the previous call to 'AnalyzeOfCmParameter', in ebx, for
  comparison? If the Consition is a Size, we set ebx to CmParamSize, as well.
;;

AnalyzeOfIfExpectation:
    mov eax D$esi | and eax 0FFFFFF

    .If eax = 'STR'
        mov ebx D$CmParamType
    .Else_If eax = 'MEM'
        mov ebx D$CmParamType
    .Else_If eax = 'REG'
        mov ebx D$CmParamType
    .Else_If eax = 'IMM'
        mov ebx D$CmParamType
    .Else_If eax = 'SYM'
        mov ebx D$CmParamType
    .Else
        On B$esi+1 > EOI, call CMerror, D$BadConditionalmacroPtr, esi, D$Trash1ptr
      ; If ' D', and so on:
        and eax 0FF | movzx ebx B$CmParamSize

        If al = 'D'
        Else_If al = 'W'
        Else_If al = 'B'
        Else_If al = 'Q'
        Else_If al = 'F'
        Else_If al = 'R'
        Else_If al = 'T'
        Else_If al = 'X'
        Else
            call CMerror, D$BadConditionalmacroPtr, esi, D$Trash1ptr
        End_If

    .End_If
ret
____________________________________________________________________________________________

GetIndice1_99:
    On B$esi >= '1', jmp L0>
        call CMerror BadCMIndice, esi, D$Trash1ptr

GetIndice0_99:
    On B$esi < '0', error BadCMIndice
L0: On B$esi > '9', error BadCMIndice

    mov ebx 0, eax 0
L0: lodsb
    cmp al '9' | ja L9>
    cmp al '0' | jb L9>
    cmp al LowSigns | jb L9>
        sub al '0'
        lea ebx D$ebx+ebx*4
        lea ebx D$eax+ebx*2
    jmp L0<

L9: ; Number in ebx
    On ebx > 99, error BadCMIndice
ret
____________________________________________________________________________________________

;;
  How many Parameters in the User Statement (the Macro Evocation), for the '#n=x'
  Condition.
;;

CmParametersCount:
    push esi
        mov esi D$InstructionAptr, ecx 0

L0:     inc esi | mov al B$esi
            cmp al EOI | je L9>
            cmp al meEOI | je L9>
            cmp al Space | jne L0<
                inc ecx | jmp L0<
L9: pop esi
ret
____________________________________________________________________________________________

Proc CMerror:
    Arguments @error, @Source, @Base

        If D@Source <> &NULL
            mov esi D@Source, ebx esi | On esi = D@Base, jmp L2>
            While B$esi > EOI
                dec esi | On esi = D@Base, jmp L2>
            End_While

L2:         While B$ebx > EOI | inc ebx | End_While
            mov B$ebx 0

            error D@Error, esi

        Else
            error D@Error

        End_If
EndP








































































































































