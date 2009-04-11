TITLE WriteCheck

;;
  Write-Time syntax error checker. First Try&See implementation:
  
  Checks for Mnemonics and Macros evocations in Code.
;;


[WriteCheckPointer: ?   WriteChar: ?    WriteCheckerThreadID: ?    WeAreChecking: ?]

[WriteCheckerWanted: ?    WriteCheckerRuning: ?]

Proc WriteChecker:
    Argument @Pointer, @Char

        On B$WriteCheckerRuning =  &TRUE, ExitP

      ; BackSpace:
        On eax = 8, ExitP

        mov B$WriteCheckerRuning &TRUE

        pushad

            mov eax D@Pointer, D$WriteChar 0

            If B$eax-1 <= ' '
                mov B$WriteCheckerRuning &FALSE

            Else_If B$eax-1 <> ':'
                mov D$WriteCheckPointer eax | move D$WriteChar D@Char

                call 'KERNEL32.CreateThread' &NULL, 0, WriteCheckerThread, 0,
                                             &THREAD_PRIORITY_IDLE, ;THREAD_PRIORITY_NORMAL,
                                             WriteCheckerThreadID
            Else
                mov B$WriteCheckerRuning &FALSE

            End_If

        popad
EndP
____________________________________________________________________________________________


Proc WriteCheckerThread: ; 'CharMessage'
    Local @Exit @ContinueStatus @hThread

    pushad

      ; The Pointer is given when user hits CRLF, Space or Comma:
        mov esi D$WriteCheckPointer | dec esi
        mov D$BlockEndTextPtr esi

      ; We go to start of the word:
L0:     dec esi | cmp B$esi ',' | je L1>
                  cmp B$esi '|' | je L1>
                  cmp B$esi ':' | je L1>
                  cmp B$esi ' ' | ja L0<

      ; Keep track of component case, in eax (Either Instruction or Parameter):
L1:     mov ebx esi | While B$ebx = ' ' | dec ebx | End_While
        If B$ebx = '|'
            mov eax 1
        Else_If B$ebx < ' '
          ; "1" >>> First Component (Mnemonic or Macro)
            mov eax 1
        Else
          ; "2" >>> Parameter
            mov eax 2
        End_If

        inc esi | mov D$BlockStartTextPtr esi

      ; Must be a Statement > Color = Statememts Color (1) ?
        mov ebx esi | sub ebx D$CodeSource | add ebx D$ColorsMap

      ; If Code Color Statement:
        ...If B$ebx = 1
            ..If D$WriteChar = ':'
                call CheckUniqueSymbol

          ; If Instruction:
            ..Else_If eax = 1
                mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr
                mov eax &FALSE
                If ecx < 15
                    call CheckMnemonic
                End_If

                If eax = &FALSE
                    call CheckForMacro
                End_IF

                .If eax = &FALSE
                    mov B$BlockInside &TRUE | call AskForRedrawNow | Beep
                .End_If

            ..End_If

      ; If Data Color Statement:
        ...Else_If B$ebx = 2
            ..If D$WriteChar = ':'
                call CheckUniqueSymbol

            ..Else

            ..End_If

        ...End_If

    popad

    mov B$WriteCheckerRuning &FALSE

    call 'Kernel32.ExitThread' 0
EndP
____________________________________________________________________________________________

[TrashCode: ? #8]
CheckMnemonic:
    mov B$WeAreChecking &TRUE

    mov edi MnemonicCopy, esi D$BlockStartTextPtr, D$LineStart esi
    mov ecx D$BlockEndTextPtr | sub ecx esi | inc ecx
L0: lodsb | and eax (not 32) | stosb | loop L0<
    mov B$edi 0

    mov edi TrashCode | call Encode

    mov B$WeAreChecking &FALSE


    .If B$CompileErrorHappend = &TRUE
        If eax = NotAnOpcode
            mov eax &FALSE
        Else
            mov eax &TRUE
        End_If

    .Else
        mov eax &TRUE

    .End_If
ret
____________________________________________________________________________________________

; User entered a Colon Char:

CheckUniqueSymbol:
  ; Compute the lenght of the actualy edited Label into ebx:
    mov esi D$CurrentWritingPos, ebx 0 | dec esi

  ; Do not check again Exported Labels:
    cmp B$esi-1 ':' | je L9>>

L0: dec esi | inc ebx
    cmp B$esi ' ' | jbe L1>
    cmp B$esi '[' | jne L0<

  ; Do not consider Local Labels:
    cmp ebx 3 | jb L9>>

L1: std
        mov esi D$CodeSource, edx D$SourceEnd

        While esi < edx
            inc esi
            If B$esi = ':'
                push esi
                    mov edi D$CurrentWritingPos, ecx ebx
                    dec edi | cmp esi edi | je L1>

                    repe cmpsb | jne L1>
                    cmp B$esi ' ' | jbe L0>
                    cmp B$esi '[' | jne L1>

L0:                     cld
                        pop esi
                        inc edi | mov D$BlockStartTextPtr edi
                        mov eax D$CurrentWritingPos | dec eax | mov D$BlockEndTextPtr eax

                        Beep | ret

L1:             pop esi
            End_If

        End_While
L8: cld

L9: ret
____________________________________________________________________________________________

CheckForMacro:
    call IsItaMacro

    .If eax = 0
        If D$TitleTable +4 > 0
            push D$CodeSource, D$SourceEnd, D$SourceLen
                move D$CodeSource D$RealCodeSource
                move D$SourceEnd D$RealSourceEnd
                move D$SourceLen D$RealSourceLen
                call IsItaMacro
            pop D$SourceLen, D$SourceEnd, D$CodeSource
        End_If
    .End_If
ret


























