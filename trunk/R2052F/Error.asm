TITLE Error
 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 'error' deal. 'OutOnError'

 how it works:

 Because of the structure of RosAsm (multiple passes with source transformations), pointing
 a bad statement, in true source file is a real head break. I thaught of two solutions:
 line numbers, that would allowed jumping from one state to a previous one, and error
 level flag that would allowed jumping to specific error check routines level-dependant.
 i choose the second solution. Heavy, but it works,... as far as i can know. Sometimes
 dirty; always difficult to understand. Sorry; after 3 writings from scratch, i think it
 can't be really simple anyway.

 In this version, 2 main routines (+ others...) are used, depending on the fact that the 
 error occurred inside EOIs (|...|) or inside brackets ([...]). Some ajustements are made:
 
- when a statement is the result of macro expending (doesn't exist in first source text),
 it is inside 'meEOIs' (Macro expension End Of Instruction) so that they are not counted
 in 'StatementCounter'.
 
- when error occur in data storing work any 'non data' bracket must be added to
 'bracketCounter'
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[OldStackPointer: ?]

[bracketCounter: ?  StatementsCounter: ?  ErrorLevel: ?  InsideBracket: ?
 InsideComment: ?   InsideMLC: ?]

[Error | pushad | CookedError #2>L | popad | mov eax, #1 | jmp OutOnError]

[CookedError | mov esi #1 | call ViewCookedError]
 ________________________________________________________________________________________

[LinesCounter: ?    DontCountNext: ?    EndingComma: ?
 StatementsTable: ?    StatementsTable2: ?    StatementsPtr: ?    StatementsPtr2: ?]

TestStatementsTable:
    mov ecx 25, esi D$StatementsTable

L0: lodsd

    If eax = 0
        hexprint 0 | jmp L9>
    Else_If eax < D$CodeSource
        hexprint 0
    Else_If eax > D$SourceEnd
        Hexprint 0-1
    Else
        mov ebx eax | add ebx 3
        mov dl B$ebx, B$OldChar dl, B$ebx 0, D$OldCharPos ebx

        pushad
            mov esi eax
            call SetDebuggeeText
            call AskForRedrawNow
        popad
        hexprint eax ; for viewing the source.
    End_If
  ; Uses '25',for partial testing of big Files:
  ; loop L0<
    jmp L0<

L9: ret
_____________________________________________________________________________________

; final error prompting:

[ErrorMessagePtr: ?]

ShowError:
; First, we make the MessageBox a little wider if the title is longer than the Message.
; Win set the Box width on the Message (not on the Title).
    pushad
        mov ecx 10, edx 0
        While B$eax > 0
            inc eax | inc ecx        ; > lenght of Title
        End_While
        mov edi Trash2
        While B$esi <> 0FF
            movsb | inc edx          ; lenght of Message Text
        End_While

        If edx < ecx
            sub ecx edx
            shl ecx 1
            mov al ' ' | rep stosb   ; ajusts text widht around the title width
        End_If
;;
  If CookedErrorMessage Buffer is not empty, this is because Errror Macro has been
  evocated with two Parameters 1) Error Title 2) Pointer to the internal Member of
  the Instruction that produced the error. If two Parameters, we add this Member
  under the Statement viewed in the Dialog Box. 'CookedErrorMessage' has been "Un-Cooked"
  by 'ViewCookedError'.
;;
        If D$CookedErrorMessage = 0
            mov al 0 | stosb
        Else
            mov W$edi CRLF, W$edi+2 CRLF | add edi 2 ;4
            mov D$edi '>>> ' | add edi 4
            mov esi CookedErrorMessage

            While B$esi <> 0
                movsb | On esi = EndOfCookedErrorMessage, jmp L1>
            End_While
L1:         mov B$edi 0

        End_If

        mov esi D$OldCharPos, al B$OldChar, B$esi al
        dec esi | mov D$BlockEndTextPtr esi
    popad
    pushad
      ;call ErrorMessageBox Trash2, eax
      call ReleaseAsmTables
    popad

    call SetDebuggeeText

    call ErrorMessageBox Trash2, D$ErrorMessagePtr
ret


SetDebuggeeText:
    mov D$BlockStartTextPtr esi, D$UpperLine esi, D$CurrentWritingPos esi

    call TryToMove

    mov ebx 0, esi D$UpperLine               ; Where is the block at screen?
L0: lodsb | On al = LF, inc ebx
    cmp esi D$BlockStartTextPtr | jb L0<

    mov ecx D$LineNumber | shr ecx 1         ; if higher than half screen,
    shr ecx 1
    .If ebx < ecx                             ; scroll to half screen.
        sub ecx ebx                           ; if down, we are at end of source...

      ; Also, don't move, for error pointing, upper than possible:
        If D$ActualTitle <> 0
            mov edx D$ActualTitle
        Else
            mov edx D$CodeSource
        End_If

L2:     cmp D$UpperLine edx | jbe L3>
        call UpOneLine | loop L2<
   .End_If

L3: mov B$blockInside &TRUE

    mov esi D$OldCharPos, al B$OldChar, B$esi al

    call SearchForEndOfErrorBlock
    call SetCaret D$BlockEndTextPtr

  ; Clear possible previous ShiftPos, in case user first hit [Shift] right after
  ; the error isq pointed out:
    move D$ShiftBlockCol D$CaretRow, D$ShiftBlockLine D$CaretLine

    call AskForRedrawNow
ret


[LastErrorBlockPipe: ?]

SearchForEndOfErrorBlock:
    mov esi D$BlockStartTextPtr, ecx 0, D$LastErrorBlockPipe 0
    On B$esi = '[', jmp SearchForEndOfErrorBracketBlock

L0: lodsb | On esi = D$SourceEnd, jmp L2>>

    If al = "'"
        While B$esi <> "'" | inc esi | On esi = D$SourceEnd, jmp L2>>
        End_While | inc esi
        mov ecx 0
    Else_If al = '"'
        While B$esi <> '"' | inc esi | On esi = D$SourceEnd, jmp L2>
        End_While | inc esi
        mov ecx 0
    Else_If al = ';'
        On ecx = 0, jmp L2>
        While B$esi >= ' ' | inc esi | On esi = D$SourceEnd, jmp L2>
        End_While | add esi 2
    Else_If al = ','
        mov ecx 1
    Else_If al = '|'
        On D$LastErrorBlockPipe = 0, mov D$LastErrorBlockPipe, esi
    Else_If al < ' '
        On ecx = 0, jmp L2>
    Else_If al = ' '

    Else
        mov ecx 0
    End_If
    jmp L0<<

L2: sub esi 2 | mov D$BlockEndTextPtr esi
    If D$LastErrorBlockPipe <> 0
        mov eax D$LastErrorBlockPipe | sub eax 2 | mov D$BlockEndTextPtr eax
    End_If

    move D$CurrentWritingPos D$BlockEndTextPtr
ret


SearchForEndOfErrorBracketBlock:
L0: lodsb | On esi = D$SourceEnd, jmp L2>

    If al = "'"
        While B$esi <> "'" | inc esi | End_While | inc esi
    Else_If al = '"'
        While B$esi <> '"' | inc esi | End_While | inc esi
    Else_If al = ';'
        While B$esi >= ' ' | inc esi | End_While | add esi 2
    Else_If al = ']'
        jmp L2>
    End_If
    jmp L0<

L2: mov D$BlockEndTextPtr esi
ret

 _________________________________________________________________________________________

[CompileErrorHappend: B$ ?    FirstPass: ?]

OutOnError:
    mov D$ErrorMessagePtr eax

    mov B$CompileErrorHappend &TRUE, D$NextSearchPos 0

    On B$WeAreChecking = &TRUE, ret
    On B$WeAreUnfolding = &TRUE, jmp UnfoldingError
   ; Error in the second part of the Encode-DecodeBox 'B$Errorlevel 7'
    On B$Errorlevel = 7,  jmp EncodeError
    On B$WeAreInTheCodeBox = &TRUE, jmp EncodeBoxError

    cld
L0: mov esp D$OldStackPointer               ; restor stack (...comming from anywhere)

L1: pushad

       call CloseProgressBar
    popad

    On B$Errorlevel = 0,  jmp Error0        ; open text error in 'SourceCleaner'
    On B$Errorlevel = 1,  jmp Error1        ; error inside square bracket
    On B$Errorlevel = 2,  jmp Error2        ; error in statements
    On B$Errorlevel = 3,  jmp Error3        ; error inside     ;;;;;;;;;;;;StoreDatas
    On B$Errorlevel = 4,  jmp Error4        ; error in DLL name ; mov B$Errorlevel 1
    On B$Errorlevel = 5,  jmp Error5        ; error in api name  ; mov B$Errorlevel 5
    On B$Errorlevel = 6,  jmp Error6        ; error in the form of api call
  ; Error7 is reserved for the Encode-Decode Box.
    On B$Errorlevel = 8,  jmp Error8        ; error in Win Equate Name
    On B$Errorlevel = 11,  jmp Error11      ; error in ClASSes
   ; On B$Errorlevel = 12,  jmp Error12      ; Short Displacement error

  ; Error9 (or none) falls there:
    call AskForRedrawNow

    If D$CookedErrorMessage <> 0
        call ErrorMessageBox CookedErrorMessage, D$ErrorMessagePtr
    Else
        call ErrorMessageBox 0, D$ErrorMessagePtr
    End_If

    call ReleaseAsmTables

    ret ; for 'no search' errors (esi known) or 'Main:' missing

____________________________________________________________________________________________

[DashLine: '___________________', 0]

;;
  To YeoH: '.zh' should be the extension of your 'RosAsmStrings' File.
  
  I hope you will succeed to make this "...If D$StringsLanguage = '.zh'" work... ;)
  As, off course, as you may guess, i cannot see anything on my Computer...
;;

Proc ErrorMessageBox:
    Arguments @Text1, @Text2

        ...If D$StringsLanguage = '.zh'
          ; Unicode:
            mov esi D@Text1, edi Trash1, ecx 0
            If esi <> 0
                While W$esi > 0
                    movsw | inc ecx | cmp ecx 400 | ja L1>
                End_While
            End_If
L1:         mov W$edi 0

            .If D@Text2 <> 0
                mov esi D@Text2, edi Trash2, ecx 0
                If esi <> 0
                    While W$esi > 0
                        movsw | inc ecx | cmp ecx 400 | ja L1>
                    End_While
                End_If
L1:             mov W$edi 0
            .Else
                mov W$Trash2 0
            .End_If

          ; Tag Dialog 10

            call 'USER32.DialogBoxIndirectParamW' D$hinstance, ErrorUnicodeDialog,
                                                  &NULL, ErrorMessageProcW, &NULL

        ...Else
          ; Ascii:
            If D@Text1 <> 0
                mov esi D@Text1, edi Trash1, ecx 0 | mov D$edi CRLF | add edi 2
                While B$esi > 0
                    movsb | inc ecx | cmp ecx 400 | ja L1>
                End_While
            End_If
L1:         mov B$edi 0

            .If D@Text2 <> 0
                mov esi D@Text2, edi Trash2, ecx 0 | mov D$edi CRLF | add edi 2
                If esi <> 0
                    While B$esi > 0
                        movsb | inc ecx | cmp ecx 400 | ja L1>
                    End_While
                End_If
L1:             mov B$edi 0
            .Else
                mov B$Trash2 0
            .End_If

            mov al B$Trash1+2 | or al B$Trash2
          ; Tag Dialog 10
            On al <> 0,
            call 'USER32.DialogBoxParamA' D$hinstance, 10, D$hWnd, ErrorMessageProcA, &NULL

        ...End_If
EndP


[SongFontHandle: ?]

[ErrorUnicodeDialog: D$ 090CC08C2 0        ; Style
 U$ 03 0 0 0DC 065             ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 '' 0                          ; Title
 12 'SimSun' 0]                ; Font  MS Song // SimSun  //   'MS Song', 0] ;

[Control0000: D$ 050800004 0      ; Style
 U$ 0 01 0DC 028               ; Dim
 0A                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[Control0001: D$ 050800004 0      ; Style
 U$ 0 02A 0DC 028              ; Dim
 014                           ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[Control0002: D$ 050000000 0      ; Style
 U$ 050 054 037 010            ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data


[uError: U$ 'Error', 0]

; Tag Dialog 10

Proc ErrorMessageProcW:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongW' D@Adressee, &GCL_HICON, D$wc_hIcon

        call 'User32.SendDlgItemMessageW' D@Adressee, 10, &WM_SETFONT,
                                              D$Font1Handle, &FALSE

        On D$NationalFontHandle <> 0,
            call 'User32.SendDlgItemMessageW' D@Adressee, 20, &WM_SETFONT,
                                              D$NationalFontHandle, &FALSE

        call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &EM_SETMARGINS,
                                          &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10
        call 'USER32.SendDlgItemMessageW' D@Adressee, 20, &EM_SETMARGINS,
                                          &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10

        call 'USER32.SendMessageW' D@Adressee, &WM_SETTEXT, &NULL, uError
                                   ;D$ErrorMessageTitlePtr

        call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_SETTEXT, 0, Trash1

        On B$trash2 <> 0,
        call 'USER32.SendDlgItemMessageW' D@Adressee, 20, &WM_SETTEXT, 0, Trash2

        call 'User32.GetDlgItem' D@Adressee, 1
        call 'USER32.SetFocus' eax

        jmp L8>>
;;
    ...Else_If D@Message = &WM_SETFONT
        If D$NationalFontHandle <> 0
            popad | mov eax D$NationalFontHandle | ExitP
        End_If
;;
    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        If D@wParam = &IDCANCEL
            call 'USER32.EndDialog' D@Adressee, &NULL

        Else_If D@wParam = &IDOK
            call 'USER32.EndDialog' D@Adressee, &NULL

        End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        call 'USER32.SendMessageW' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP

    ...Else
L8:     popad | mov eax &FALSE | ExitP

    ...End_If

    popad | mov eax &TRUE
EndP


Proc ErrorMessageProcA:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

        call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &EM_SETMARGINS,
                                        &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10

        call 'USER32.SendMessageA' D@Adressee, &WM_SETTEXT, &NULL,
                                   D$ErrorMessageTitlePtr

        call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &EM_SETMARGINS,
                                        &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 10


        call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_SETTEXT, 0, Trash1

        On D$NationalFontHandle <> 0,
            call 'User32.SendDlgItemMessageA' D@Adressee, 20, &WM_SETFONT,
                                              D$NationalFontHandle, &FALSE

        call 'User32.SendDlgItemMessageW' D@Adressee, 10, &WM_SETFONT,
                                              D$Font1Handle, &FALSE

        On B$trash2 <> 0,
            call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &WM_SETTEXT, 0, Trash2

        call 'User32.GetDlgItem' D@Adressee, 1
        call 'USER32.SetFocus' eax

        jmp L8>>

    ...Else_If D@Message = &WM_COMMAND

        If W@wParam = &IDCANCEL
            call 'USER32.EndDialog' D@Adressee, &NULL
        Else_If W@wParam = &IDOK
            call 'USER32.EndDialog' D@Adressee, &NULL
        End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP

    ...Else
L8:     popad | mov eax &FALSE | ExitP

    ...End_If

    popad | mov eax &TRUE
EndP


____________________________________________________________________________________________

[CookedErrorMessage: ? #20] [EndOfCookedErrorMessage: ? ?]

;;
  This shows the internal Source of error. Can't work actually as i don't know how to
  to make sure that esi points to something inside the Assembler Tables Memory, and i
  don't find any function that would say if the pointer is valid or not. Nothing like
  'ReadProcessmemory' in the Debugger. There is 'KERNEL32.SetErrorMode', but i don't
  know if it works or not for x86, and what Message is suppose to be sent back.
  
  Would probably require SEH...
;;

ViewCookedError:
    pushad
        While B$esi > EOI | dec esi | End_While | inc esi

        mov edi CookedErrorMessage

L0:     lodsb
        .If al = TextSign
            mov al '"'
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
        .Else_If al = Space
            mov al ' '
        .Else_If al = EOI
            mov al '|' | jmp L9>
        .Else_If al = meEOI
            mov al '|'
        .End_If

        stosb

        On edi < EndOfCookedErrorMessage, jmp L0<<

L9:     mov B$edi 0

    popad
ret

 _________________________________________________________________________________________

; open text error in 'SourceCleaner'

[OldChar: ?    OldCharPos: ?]

SetEndOfErrorText:

    mov esi D$StatementsPtr, esi D$esi

    .If esi < D$CodeSource
        mov esi D$CodeSource

    .Else_If esi > D$SourceEnd
        mov esi D$SourceEnd | std
L0:     lodsb
        If al = LF
            inc esi ; stop
        Else_If al = '|'
            inc esi ; stop
        Else_If al = '['
                    ; stop
        Else_If esi > D$CodeSource
                    ; stop
        Else
            jmp L0<
        End_If
        cld
    .End_If

DirectSourcePointing:
    mov ecx 200
    push esi, eax
L0:     lodsb
        If al <= CR
            dec esi ; stop
        Else_If al = '|'
            dec esi ; stop
        Else_If al = ']'
                    ; stop
        Else_If esi > D$SourceEnd
                    ; stop
        Else
            loop L0<
        End_If
        mov al B$esi, B$OldChar al, B$esi 0FF, D$OldCharPos esi
        dec esi | mov D$BlockEndTextPtr esi  ;, D$CurrentWritingPos esi ; ?Case of Bad Pos?
    pop eax esi

    call VerifyNotOneChar
ret


; Because Blocks of one char was not outputed, in older version of the Editor.
; Should be of no more use now:

VerifyNotOneChar:
    push esi, eax
        mov eax D$BlockEndTextPtr
        cmp eax D$BlockStartTextPtr | ja L9>
            inc D$BlockEndTextPtr
            mov eax D$BlockEndTextPtr, al B$eax
                On al = CR, inc D$BlockEndTextPtr
                mov esi D$OldCharPos, al B$OldChar, B$esi al
                mov esi D$BlockEndTextPtr | inc esi
                mov al B$esi, B$OldChar al, B$esi 0FF, D$OldCharPos esi
L9: pop eax, esi
ret


Error0:

    call SetEndOfErrorText

    jmp ShowError
 _________________________________________________________________________________________

; error inside square bracket: we search [bracketCounter] square bracket number

Error1:
    push eax
      mov esi, D$CodeSource | mov ecx, 0
L1:   lodsb | cmp al '"' | jne L3>
L2:   lodsb | cmp al '"' | jne L2<         ; strip "text"
        jmp L1<
L3:   cmp al "'" | jne L5>
L4:   lodsb | cmp al "'" | jne L4<         ; strip 'text'
        jmp L1<


L5: cmp al ';' | jne L7>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            Do
                inc esi | cmp esi D$SourceEnd | jae L9>
            Loop_Until D$esi = MLC
            add esi 3 | cmp esi D$SourceEnd | jae L9>
        Else
L6:         lodsb | cmp al LF | jne L6<
        End_If
        jmp L1<

;L5:   cmp al ';' | jne L7>
;L6:   lodsb | cmp al LF | jne L6<          ; strip comments
;        jmp L1<

L7:   cmp al '[' | jne L1<
        inc ecx | cmp ecx D$bracketCounter | jb L1<

      push esi
L8:     lodsb | cmp al ']' | jne L8<
          mov al B$esi, B$esi 0FF, B$OldChar al, D$OldCharPos esi
      pop esi
L9: pop eax

    dec esi | jmp ShowError
 _________________________________________________________________________________________

; error in statements. We search D$StatementsCounter '|' number
; Nothing but a modified version of text cleaner first part.

Error2:
    mov esi D$StatementsCounter
 ; showme eax
    push esi, eax
L0:     lodsb
        If al = CR
            dec esi ; stop
        Else_If al = '|'
            dec esi ; stop
        Else_If al = ']'
                    ; stop
        Else_If esi > D$SourceEnd
                    ; stop
        Else
            jmp L0<
        End_If

    mov al B$esi, B$OldChar al, D$OldCharPos esi, B$esi 0FF

    pop eax esi

    jmp ShowError

 ________________________________________________________________________________________
;;
 error inside ReplaceEquate in brackets statements or...
 error inside StoreData: value in [bracketCounter] did not count equates and macros
 so that we have to do a new count in ecx before jumping to error1 bracket research.
;;

Error3:
;;
 now: ecx = lenght, esi > start of bad Name in Data.
 Used now only by 'SearchRegularLabel' when filling Data symbols evocations. At this
 time, we do not have any more the source Data pointers available in "StatementsTable".
;;
    pushad

    While B$esi-1 > LowSigns | dec esi | End_While

    push esi
        mov ecx 1
        While B$esi > LowSigns | inc esi | inc ecx | End_While
    pop esi

    dec ecx | mov D$LenOfSearchedString ecx

    mov edi SearchString | rep movsb | mov al 0 | stosb

    push D$DownSearch, D$CaseSearch, D$WholeWordSearch, D$CurrentWritingPos
        mov B$DownSearch &TRUE, B$CaseSearch &FALSE, B$WholeWordSearch &TRUE
        move D$CurrentWritingPos D$CodeSource

        mov D$Trash2 0

        push D$NextSearchPos
            mov D$NextSearchPos 0
L0:         call StringSearch | On B$BlockInside = &FALSE, jmp L7>>

          ; just in case the searched word is too inside a comment:
            mov esi D$BlockStartTextPtr | dec esi
            .While B$esi > LF
                If B$esi = '"'
                    dec esi
                    While B$esi <> '"'
                        dec esi
                    End_While
                Else_If B$esi = "'"
                    dec esi
                    While B$esi <> "'"
                        dec esi
                    End_While
                Else_If B$esi = ';'
                    jmp L0<
                End_If
                dec esi
            .End_While

            mov esi D$BlockStartTextPtr, edi Trash2, ecx D$BlockEndTextPtr
            sub ecx D$BlockStartTextPtr | inc ecx | rep movsb | mov al 0 | stosb

L7: pop D$NextSearchPos
    pop D$CurrentWritingPos, D$WholeWordSearch, D$CaseSearch, D$DownSearch
    popad
    pushad
        call AskForRedrawNow

        If D$Trash2 <> 0
            call ErrorMessageBox Trash2, D$ErrorMessagePtr
        Else
            call ErrorMessageBox esi, D$ErrorMessagePtr
        End_If
        call ReleaseAsmTables
    popad
ret

_________________________________________________________________________________________

; Error in DLL name:

Error4:
    mov B$CompileErrorHappend &TRUE

    push esi

;L9: pushad
;        ;call 'USER32.MessageBoxA' D$hwnd, esi, eax, &MB_SYSTEMMODAL
;        call AskForRedrawNow
;
;        call ErrorMessageBox esi, D$ErrorMessagePtr
;    popad

    mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
    move D$CurrentWritingPos D$CodeSource
    mov ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec edi | mov al '.' | stosb | mov D$LenOfSearchedString ecx

    call  StringSearch

    call AskForRedrawNow

    pop esi

    call ErrorMessageBox esi, D$ErrorMessagePtr

    call ReleaseAsmTables
ret


; error in function name:

Error5:
    mov B$CompileErrorHappend &TRUE

    ;push esi

    cld

;L9: ;call 'USER32.MessageBoxA' D$hwnd, esi, eax, &MB_SYSTEMMODAL
;    call ErrorMessageBox esi, D$ErrorMessagePtr

    mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE, B$CaseSearch &FALSE
    move D$CurrentWritingPos D$CodeSource

    mov esi D$StartOfFunctionName, ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec ecx
    mov D$LenOfSearchedString ecx

    call  StringSearch

    call AskForRedrawNow

    ;pop esi

    call ErrorMessageBox D$StartOfFunctionName, D$ErrorMessagePtr

    call ReleaseAsmTables
ret


; for bad api call formulation:

Error6: ; mov B$ErrorLevel 6
    push esi
    mov B$CompileErrorHappend &TRUE

L9: ;pushad
      ;call 'USER32.MessageBoxA' D$hwnd, esi, eax, &MB_SYSTEMMODAL
    ;  call ErrorMessageBox esi, D$ErrorMessagePtr
    ;popad

    mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
    move D$CurrentWritingPos D$CodeSource
    mov ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec ecx | mov D$LenOfSearchedString ecx

    call StringSearch

    call AskForRedrawNow

    pop esi

    call ErrorMessageBox esi, D$ErrorMessagePtr

    call ReleaseAsmTables
ret


; For Bad Win32 Equate Name:

Error8:
    mov B$CompileErrorHappend &TRUE

    push esi

;;;    mov B$esi 0
;;;    While B$esi <> '&' | dec esi | End_While

   ; pushad
   ;   ;call 'USER32.MessageBoxA' D$hwnd, esi, eax, &MB_SYSTEMMODAL
   ;   call ErrorMessageBox esi, D$ErrorMessagePtr
   ; popad

    push D$DownSearch, D$WholeWordSearch
        mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
        move D$CurrentWritingPos D$CodeSource
        mov ecx 0

L0:     inc ecx | lodsb | stosb | cmp al 0 | jne L0<
        dec ecx | mov D$LenOfSearchedString ecx


L1:     call StringSearch | cmp B$StringFound &FALSE | je L2>
        mov esi D$BlockEndTextPtr | mov al B$esi+1

      ; Next Char Must be some separator, or some '_':
        If al = '_'
            ; OK
        Else
            call WordEdge | On B$Edge = &FALSE, jmp L1<
        End_If

L2:     call ReleaseAsmTables
    pop D$WholeWordSearch, D$DownSearch

    pop esi

    call AskForRedrawNow

    call ErrorMessageBox esi, D$ErrorMessagePtr
ret


Error9:
    call DirectSourcePointing | jmp ShowError
ret


Error11:
    mov B$CompileErrorHappend &TRUE

    push esi

L9: ;pushad
    ;  ;call 'USER32.MessageBoxA' D$hwnd, esi, eax, &MB_SYSTEMMODAL
    ;  call ErrorMessageBox esi, D$ErrorMessagePtr
    ;popad

    mov edi SearchString, B$DownSearch &TRUE, B$WholeWordSearch &FALSE
    move D$CurrentWritingPos D$CodeSource
    mov esi OneCLASSname, ecx 0

L0: inc ecx | lodsb | stosb | cmp al 0 | jne L0<
    dec ecx | mov D$LenOfSearchedString ecx

L1: call StringSearch
    mov esi D$BlockStartTextPtr | dec esi
    While B$esi = ' ' | dec esi | End_While
    mov eax D$esi-4 | and eax (not 020202020)

  ;  On eax <> 'CLAS', jmp L1<
  ;  On B$esi-5 <> '5', jmp L1<

    sub esi 5 | mov D$BlockStartTextPtr esi

    While B$esi <> ']' | inc esi | End_While
    mov D$BlockEndTextPtr esi

    call ReleaseAsmTables
    call AskforRedraw

    pop esi

    call ErrorMessageBox esi, D$ErrorMessagePtr
ret

;;
Error12:
    push eax
      ; Write the Number out out of range Bytes at 'TooLongOf'
        mov edi D$TooLongOfPtr, D$edi '    ' | mov eax ebx | call WriteEax

      ; Kill the Line Break between 'ShortDis' and 'TooLongOf'
        mov esi D$TooLongOfPtr | dec esi
        While B$esi >= ' ' | mov B$esi ' ' | dec esi | End_While
        While B$esi < ' ' | mov B$esi ' ' | dec esi | End_While
    pop eax
    
    jmp Error2
ret
;;


[CookedError2 | mov esi #1 | call ViewCookedError2]

[SyntaxErrorInMacro1 | pushad | inc #2 | CookedError2 #2>L | popad
pushad
mov eax, #1 | move D$ErrorMessagePtr #1 | call ErrorMessageBox CookedErrorMessage, D$ErrorMessagePtr
popad]


ViewCookedError2:
    pushad
    mov eax esi


    sub esi 20 ; 20 is the size of CookedErrorMessage message
    mov edi CookedErrorMessage

    Do
        movsb
    Loop_Until esi = eax

     mov B$edi 0

    popad
ret








