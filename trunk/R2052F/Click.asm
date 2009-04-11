TITLE Click
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
 Search feature by right-clicking on text: We search for main Labels, macros or
 equates declarations, data declarations. This is to say that, if we are inside
 square brackets, any fitting first word, or odd word, or ':' ended word is good;
 if outside, only ':' ended words.
;;

[InsideText: ?  RCstart: ?  RCend: ?  DataDeclaration: ?    MacroDeclaration: ?]

[OldEditData: ? #21] [PreviousSearch: ? #32]
[OneWordChars: B$ '0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.' 0
 OneWordCharsLen: D$ len   Edge: B$ 0   OddWord: 0   BackAnyWhere: &TRUE]

; WordEdge looks if a char is or not inside OneWordChars and tells if word edge or not.
; we preserve the flags because direction flag may be set on or off by caller:

WordEdge:
    pushfd | cld
        push ecx, esi, edi
            mov ecx D$OneWordCharsLen, edi OneWordChars, B$Edge &TRUE
            repne scasb | jne L9>
                mov B$Edge &FALSE
L9:     pop edi, esi, ecx
    popfd
ret


[ShowEquateTitle: 'Win Equate Value', 0]

[ShowEquateHexa: ? #40]

ShowEquate:
     mov ebx, eax

     mov edi ShowEquateHexa, esi TrashString
     While B$esi <> 0
        movsb
     End_While
     mov eax '   =' | stosd | mov eax '    ', ecx 3 | rep stosd

     push edi, ebx
         std
            mov ecx, 9
L1:         If ecx = 5
                mov al '_' | stosb
            End_If
            If ecx = 1
                mov al '_' | stosb
            End_If
            mov al bl | and al 0F | add al, '0' |  On al > '9', add al 7
            stosb | shr ebx, 4 | loop L1<
         cld
     pop ebx, edi
     inc edi

     mov D$edi '_h  ', D$edi+4 '  [' | add edi 7

     mov eax ebx | call WriteEaxDecimal

     mov D$edi ']   ', B$edi+4 0

     call 'USER32.MessageBoxA' D$hwnd, ShowEquateHexa, ShowEquateTitle, &MB_SYSTEMMODAL
ret

;;
 I suppress OdWord testing for Equates pointing because it is much difficult work for
 NOP: Usually Equates are defined BEFORE they are used. A wrong pointing could only
 append if user define some Equate AFTER reusing it in Equates definitions (???!!!).
 Good for him!
;;

[PossibleWinApi: B$ ?   PossibleOpCode: ?   LocalSymbol: ?] [NumberDashLines: ?]

RightClick: ; 'InternSearch'
    mov B$ShiftBlockInside &FALSE

    push D$BlockInside
        call LeftButtonSimulation | call LeftButtonUp
        mov eax D$CurrentWritingPos
    pop D$BlockInside

    ..If B$BlockInside = &TRUE
       .If eax >= D$BlockStartTextPtr
            If eax <= D$BlockEndTextPtr
               jmp RightClickOnBlock
            End_If
       .End_If
    ..End_If

    call LeftButtonSimulation | call LeftButtonUp | call AskForRedraw

  ; Save all 'EditData's
    mov esi EditData, edi OldEditData, ecx 21 | rep movsd

    mov esi D$CurrentWritingPos

  ; Special Selections cases when Clicking exactely on '"' or '[':
    If B$esi = '"'
        call SelectDoubleQuotedText | ret
    Else_If B$esi = '['
        call SelectDataBlock | ret
    End_If

  ; Is it a Right-Click on 'blank' area for simply routing back?
  ; Or on a DashLine >>> do nothing.
    mov al B$esi
    If B$esi = '_'
        On B$esi+1 = '_', ret
        On B$esi-1 = '_', ret
    End_If

    call WordEdge

    .If B$Edge <> &FALSE
        If B$esi <= ' '
            jmp BlankRightClick
        Else_If B$esi = '&'
          ; '&' not considered in the WordEdge Routine, but valid here for Win32 Equates
        Else
          ; Abort on alien Char that could be found, for example, in Strings:
            ret
        End_If
    .End_If

    mov B$PossibleWinApi &FALSE

; Reused by 'SearchFromTreeListBox' and the Debugger 'DataView_ShowDeclaration':
InternalRightClick: ; 'InternSearch'
    mov B$InsideMLC &FALSE, B$InsideComment &FALSE

  ; Go to start of Clicked word:
    std
L0:     lodsb | call WordEdge | cmp B$Edge &TRUE | jne L0<
    cld
    inc esi

  ; Special case for Numbers:
    ..If B$esi+1 >= '0'
        .If B$esi+1 <= '9'
            call RightClickedNumber | ret
        .End_If
    ..End_If

  ; Special case of Tag Comment:
    mov eax D$esi+1 | or eax 020202020
    If eax = 'tag '
        call IsItTag | On eax = &TRUE, jmp TagParser
    End_If

  ; To differenciate for example a Click on 'mov' (if 'mov' is a Macro) from '[mov | ...':
    push esi
        While B$esi = ' ' | dec esi | End_While
        mov B$PossibleOpCode &FALSE
        On B$esi = '[', mov B$PossibleOpCode &TRUE
    pop esi

    inc esi

  ; Special case for OS Equates, plus, take care of Strings Delimiters for Api calls:
    If B$esi-1 = '&'
        push esi
            call NewGetEquates | mov edx esi
        pop esi
        On B$EquateFound = &TRUE, call ShowEquate
        ret
    Else_If B$esi-1 = "'"
        mov B$PossibleWinApi &TRUE
    Else_If B$esi-1 = "'"
        mov B$PossibleWinApi &TRUE
    End_If

  ; First Char, Low Case into ah:
    mov ah B$esi | or ah 32

  ; edx > second char (> edi)
    inc esi | mov ebx 0, edx esi

  ; Search for end of Clicked word:
L0: lodsb | inc ebx | call WordEdge | cmp B$Edge &TRUE | jne L0<
    sub ebx 1 | jc L9>>             ; ebx = length-1
        cmp ebx 0 | je L9>>
    cmp ebx 1 | ja L0>              ; Abort if local label
    cmp B$esi-2 '9' | ja L0>
        ret

  ; Now, edi (edx) > start+1 of right clicked word; ebx = lenght-1. Search maching Symbol:
L0: mov esi D$CodeSource, ecx D$SourceLen, B$InsideBracket &FALSE, B$InsideComment &FALSE
    and D$MacroNamePointer 0 ;jE!
;;
  Clean up, in actual Style to be continued from here, when i will have time.
  Old Routines yet here at 'OldRightClick', 'OldInternalRightClick'.
;;
    jmp L0>

T0: lodsb | jmp L1>                         ; simplified loop for strip texts and comments
T1: loop T0<
    On B$InsideMLC = &TRUE, jmp C0>         ; Because unpaired MLC are allowed
      ret

L0: lodsb | cmp al ah | je L3>>             ; test XORed AH each pass in order to handle
            xor ah 32 | cmp al ah | je L3>> ; case without modifying AL (following tests)
        jmp L1>
L2: loop L0<

C0:     .If B$PossibleWinApi = &TRUE
            call WinApiFirstPass ; SearchWinApi
        .Else
            mov B$MnemonicHelpFound &FALSE
            call SearchMneMonic             ; Nothing found > it it a Mnemonic?
            If B$MnemonicHelpFound = &FALSE
                On ebx < 3, call SearchForReg
            End_If
        .End_If
        ret

L1: cmp B$InsideMLC &TRUE | jne L1>
      cmp D$esi-2 MLC | jne T1<
        mov B$InsideMLC &FALSE | jmp L2<
L1: cmp B$InsideComment &TRUE | jne L1>
      cmp al LF  | jne T1<<
        mov B$InsideComment &FALSE | jmp L2<
L1: cmp B$InsideText &FALSE | je L1>
      cmp al B$InsideText | jne T1<<
        mov B$InsideText &FALSE | jmp L2<<
L1: cmp al "'" | jne L1>
      mov B$InsideText al | jmp T1<<
L1: cmp al '"' | jne L1>
      mov B$InsideText al | jmp T1<<
L1: cmp al '[' | jne L1>
        call ScaningBracket

S0:   cmp B$esi ' ' | jne L2<<
        inc esi | sub ecx 1 | jnc S0<        ; strip double spaces
          ret

L1: cmp al ']' | jne L1>
      mov B$InsideBracket &FALSE, B$DataDeclaration &FALSE, B$MacroDeclaration &FALSE
      jmp L2<<

L1: cmp al ';' | jne L1>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            mov B$InsideMLC &TRUE | jmp T1<<
        Else
            mov B$InsideComment &TRUE | jmp T1<<
        End_If

L1: cmp al '|' | jne L1>
      mov B$InsideBracket &FALSE | jmp L2<<
L1: cmp al ':' | jne L2<<
      mov B$DataDeclaration &TRUE

            jmp L2<<                         ; (avoids pointing equates datas).

L3: mov al B$esi-2 | call WordEdge | cmp B$Edge &FALSE | je L2<<     ; left edge?

        mov D$NumberDashLines 0

        pushad | mov ecx ebx, edi edx

C0:       lodsb | mov ah B$edi | inc edi

          ; case insensitive comparison:
            If al >= 'A'
                On al <= 'Z', or al 020
            End_If

            If ah >= 'A'
                On ah <= 'Z', or ah 020
            End_If

          ; Edi is pointing to the clicked Word. Esi ---> Parsed Source:

            .If B$edi-1 <> '_'
                While B$esi-1 = '_'
                    lodsb
                    If al >= 'A'
                        On al <= 'Z', or al 020
                    End_If
                    inc D$NumberDashLines | dec ecx | jz X1>
                End_While
            .End_If

            .If B$esi-1 <> '_'
                While B$edi-1 = '_'
                    mov ah B$edi | inc edi
                    If ah >= 'A'
                        On ah <= 'Z', or ah 020
                    End_If
                    dec D$NumberDashLines | dec ecx | jz X1>
                End_While
            .End_If

            cmp ah al | jne C1>
            loop C0<

X1:         mov al B$esi | call WordEdge
            If B$Edge = &FALSE
                popad | jmp L2<<
            End_If

            popad | jmp C2>

C1:     popad | jne L2<<

   ; mov al B$esi+ebx | call WordEdge | cmp B$Edge &FALSE | je L2<<    ; right edge?

C2: If B$MacroDeclaration = &TRUE
      ; Was it first word?
        cmp esi D$MacroNamePointer | jne L2<<
    End_If

    push ebx
        add ebx D$NumberDashLines
        cmp B$esi+ebx ':'
    pop ebx
    je L4>                                 ; Label?

; as we have tested for '|' (> InsideBracket = FALSE), "test B$OddWord 1" applies either
; uppon first word of macro def. or odd word of equate def. But data body could still
; be pointed as odd equate dec. So we finally test 'B$DataDeclaration'.

      cmp B$InsideBracket &TRUE | jne L2<<                     ; equ. / macro
          cmp B$DataDeclaration &TRUE | je L2<<  ; avoid pointing data body instead of Equate
              If B$MacroDeclaration = &FALSE
                call PairedEquate | On B$ValidEquateOrMacro = &FALSE, jmp L2<<
              End_If

L4: dec esi                                                    ; found
    mov D$BlockStartTextPtr esi, D$RCstart esi                 ; RCstart/End used by
    add esi ebx | mov D$BlockEndTextPtr esi, D$RCend esi       ; 'BackClick'
    mov B$BlockInside &TRUE
    inc esi | mov D$CurrentWritingPos esi

    std | mov ecx 0
L5:     lodsb | inc ecx
        cmp al LF | jne L5<                    ; search for start of line
        cld | ;dec ecx

    call StorePosInBackTable

    add esi 2 | mov D$UpperLine esi                            ; and set all needed
    call UpOneLine | call UpOneLine | call UpOneLine           ; variables for Pos, in

  ; Would be a good thing... but doesn't work. The Block seems cleared by 'TryToMove'
  ; when it can't move any more upward... complicated... later...

  ;  call UpOneLine | call UpOneLine | call UpOneLine
  ;  call UpOneLine | call UpOneLine | call UpOneLine

    mov D$CaretLine 3, D$CaretRow ecx, D$PhysicalCaretRow ecx  ; case user wish editing
    call TryToMove
    mov D$RightScroll 0 | call AskForRedraw

    If B$PossibleOpCode = &TRUE
        mov esi D$BlockStartTextPtr, ah B$esi, edx esi, ebx D$BlockEndTextPtr
        or ah 32 | inc edx | sub ebx esi
        call SearchMneMonic
    End_If

L9: ret


SearchForReg:
    pushad
        dec edx | mov esi edx, edi MnemonicCopy, ecx ebx | inc ecx
L0:     lodsb | On al > 'Z', and eax (not 020) | stosb | loop L0<
        mov B$edi 0
        mov esi MnemonicCopy
        Call IsItaReg
        On eax <> 0, call Help B_U_AsmName, {'Registers', 0}, ContextHlpMessage
    popad
ret
____________________________________________________________________________________________

[ValidEquateOrMacro: ?]
;;
[LowSigns            31
    TextSign            30
;;

;;
  'PairedEquate' job is to make sure, for the Right-Click feature, that, the pointed
  word is really an Equate Declaration, and not an Evocation.
;;

PairedEquate:
  ; esi points to the Second Char of the pointed word.
    pushad
    push D$CodeSourceA, D$CodeSourceB
        While B$esi >= '0' | inc esi | End_While
        mov ecx 2

L0:     While B$esi <> '[' | dec esi | inc ecx | End_While

      ; Case of '[' in Text or Comments:
      cmp D$MacroNamePointer 0 | je L1> ; jE!
        If esi > D$MacroNamePointer
            dec esi | inc ecx | jmp L0<
        End_If
L1:   ;and D$MacroNamePointer 0 ; ?? jE!
        mov edi Trash1, D$StripLen ecx | rep movsb | mov D$edi CRLF2

        move D$CodeSourceA Trash1, D$CodeSourceB Trash2

      ; CoolParsers
        call KillMultiLineComments
        call NewKillVirtualCRLF
        call KillMeaninglessCommas

      ; HotParsers
        call StripUnderscore
        call TranslateAsciiToMyAscii
        call StripUnneededSpaces
        call ConvertCommasToSpace

        mov esi D$CodeSourceA, edx D$StripLen | add edx esi

        While esi < edx
            If B$esi < Separators
                mov B$esi Space
            End_If
            inc esi
        End_While

        call StripUnneededSpaces

        mov esi D$CodeSourceA, edx D$StripLen | add edx esi | dec edx
        mov D$ValidEquateOrMacro &FALSE

      ; Don't know why, but it seems that when stripping un-needed Spaces,
      ; the last one might be lost. Probably when followed by EOI:
        On B$edx <> Space, mov B$edx Space

        While esi < edx
            inc esi | On B$esi = Space, xor D$ValidEquateOrMacro &TRUE
            On B$esi = Space, inc eax
        End_While
    pop D$CodeSourceB, D$CodeSourceA
    popad
ret



BlankRightClick:    On B$BackAnyWhere = &TRUE, call BackClick | ret


[MacroNamePointer: ?]

ScaningBracket:
    mov B$InsideBracket &TRUE, B$DataDeclaration &FALSE, B$MacroDeclaration &FALSE

  ; Verify that this is not an Alternate Syntax Instruction:
    push eax
        mov al B$esi-2 | or al 020
        If al = 'd'
L0:         pop eax
            While B$esi > LF | inc esi | End_While
            ret
        Else_If al = 'r'
            jmp L0<
        End_If
    pop eax

    push esi

      ; Go to first word and keep pointer as required for final test (+1):
        While B$esi = ' ' | inc esi | End_While
        mov D$MacroNamePointer esi | inc D$MacroNamePointer

      ; Skip first word:
        While B$esi > ' '
            inc esi
            If B$esi = '|'
                mov B$MacroDeclaration &TRUE | jmp L9>
            End_If
        End_While

      ; What last Char in first word:
        If B$esi-1 = ':'
            mov B$DataDeclaration &TRUE | jmp L9>
        Else_If B$esi-1 = '|'
            mov B$MacroDeclaration &TRUE | jmp L9>
        End_If

      ; What is next non space Char:
        While B$esi = ' ' | inc esi | End_While | lodsb

        If al = '|'
            mov B$MacroDeclaration &TRUE
        Else_If al = CR
            mov B$MacroDeclaration &TRUE
        Else_If al = ';'
            mov B$MacroDeclaration &TRUE
        End_If
L9: pop esi
ret

____________________________________________________________________________________________

SelectDoubleQuotedText:
    mov D$BlockStartTextPtr esi | inc D$BlockStartTextPtr

    mov B$TextGoingOn &FALSE | lodsb | call IsItFirstText

L1: lodsb | On esi >= D$SourceEnd, ret
            call IsItFirstText | je L1<

    sub esi 2
    mov D$BlockEndTextPtr esi, B$BlockInside &TRUE
    call AskForRedraw
ret


SelectDataBlock:
    inc esi | mov D$BlockStartTextPtr esi, B$TextGoingOn &FALSE
    .While B$esi <> ']'

        .If B$esi = ';'
            If D$esi-2 = MLC   ; (LF ; ; CR)
                Do
                    inc esi | On esi >= D$SourceEnd, ret
                Loop_Until D$esi-2 = MLC
            Else
                While B$esi <> LF
                    inc esi
                End_While
            End_If
        .End_If

L1:     lodsb | On esi >= D$SourceEnd, ret
                call IsItFirsttext | je L1<
    .End_While

L9: dec esi
    mov D$BlockEndTextPtr esi, B$BlockInside &TRUE
    call AskForRedraw
ret

____________________________________________________________________________

; User selected a Block of text and then RightClick uppon it:

[FloatHandle: 0
 Float_Copy_String: B$ 'Copy', 0
 Float_Delete_String: 'Delete', 0
 Float_Replace_String: 'Replace', 0

 Float_SearchFromTop_String: 'Search from Top', 0
 Float_SearchUp_String: 'Search Up', 0
 Float_SearchDown_String: 'Search Down', 0

 Float_Unfold_String: 'Unfold', 0         Float_BookMark_String: 'BookMark', 0
 Float_UnBookMark_String: 'UnBookMark', 0 Float_Number_String: 'Number forms', 0
 Float_SelReplace_String: 'Replace in Selection', 0
 Float_BadDisLabel_String: 'Bad Disassembly', 0
 Float_Turn_Code_String: 'This should have been Code', 0]


[Float_Copy 5500    Float_SearchFromTop 5501    Float_SearchUp 5502    Float_SearchDown 5503
 Float_Unfold 5504  Float_BookMark 5505         Float_UnBookMark 5506  Float_Number 5507
 Float_ReArange 5508 Float_SelReplace 5509         Float_Delete 5510      Float_Replace 5511
 Float_BadDisLabel 5512]

RightClickOnBlock:
    call 'USER32.CreatePopupMenu' | mov D$FloatHandle eax
    call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, Float_Copy, Float_Copy_String
    call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, Float_Delete, Float_Delete_String
    call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, Float_Replace, Float_Replace_String

    call 'USER32.AppendMenuA' D$FloatHandle &MF_SEPARATOR &NULL &NUll
    call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING Float_SearchFromTop,
                             Float_SearchFromTop_String
    call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING Float_SearchUp Float_SearchUp_String
    call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING Float_SearchDown Float_SearchDown_String

    call IsItaNumber
    If eax > 0
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_Number, Float_Number_String
    End_If

    call IsItanEqual | On eax = &TRUE, jmp L0>

    call IsItaMacro
    If eax > 0
L0:     call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_Unfold, Float_Unfold_String
    End_If

    call IsItaLabel
    If eax = 1
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_BookMark,
                                  Float_BookMark_String
    Else_If eax = 2
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
        call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_UnBookMark,
                                  Float_UnBookMark_String
    End_If

    .If D$IsDebugging = &FALSE
        mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr
        If ecx > 50
            call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
            call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_SelReplace,
                                    Float_SelReplace_String
        End_If
    .End_If


    .If B$ThisSourceIsDisassembled = &TRUE
        call IsItDisassembledLabel
        If eax = &TRUE
            call 'USER32.AppendMenuA' D$FloatHandle, &MF_SEPARATOR, &NULL, &NUll
            call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, Float_BadDisLabel,
                                      Float_BadDisLabel_String
        end_If
    .End_If


    call 'USER32.GetWindowRect' D$hwnd RECT
    mov eax D$RECTleft | add eax 20 | add D$MousePosX eax
    mov eax D$RECTtop | add D$MousePosY eax

    call 'KERNEL32.GetCurrentThreadId'
    call 'USER32.SetWindowsHookExA' &WH_KEYBOARD, FloatMenuProc, &NULL, eax
    mov D$hHook eax

    mov eax D$MousePosX | add eax D$BpMarginWidth

    call 'USER32.TrackPopupMenu' D$FloatHandle,
                                 0, eax, D$MousePosY, 0,
                                 D$EditWindowHandle, &NULL

    call 'USER32.UnhookWindowsHookEx' D$hHook
ret


IsItDisassembledLabel:
    mov esi D$BlockEndTextPtr | On B$esi+1 <> ':', jmp L7>

    mov esi D$BlockStartTextPtr
    If D$esi = 'Code'
        mov D$DisLabelTypeWas CODEFLAG
        call GetDisLabelHexaValue
        xor B$GetHexaFromTextError &TRUE | mov eax D$GetHexaFromTextError

    Else_If D$esi = 'Data'
        mov D$DisLabelTypeWas DATAFLAG
        call GetDisLabelHexaValue
        xor B$GetHexaFromTextError &TRUE | mov eax D$GetHexaFromTextError

    Else
L7:     mov eax 0

    End_If
ret


GetDisLabelHexaValue:
;;
  The "On B$esi = '_', jmp L1>" are for cases of Label that are appended with a "_Symbol"
  taken from the 'StringsMap'.
;;
    mov esi D$BlockStartTextPtr, edi CopyOfLabelHexa | add esi 4
    While B$esi <> ':'
        movsb | On B$esi = '_', jmp L1>
    End_While
L1: mov B$edi 0

    call GetHexaFromText CopyOfLabelHexa

    If B$GetHexaFromTextError = &TRUE
        mov D$DisAddressWas 0
    Else
        mov D$DisAddressWas eax
    End_If

    mov D$CopyOfNextLabelHexa 0
    mov eax D$CopyOfLabelHexa
    .While esi < D$SourceEnd
        inc esi
        .If D$esi = eax
            mov ebx esi
            While B$esi <> ':'
                inc esi | On B$esi <= ' ', jmp L2>
            End_While

            mov esi ebx, edi CopyOfNextLabelHexa
            While B$esi <> ':'
                movsb | On B$esi = '_', jmp L1>
            End_While
L1:         mov B$edi 0
            push D$GetHexaFromTextError
                call GetHexaFromText CopyOfNextLabelHexa
                If B$GetHexaFromTextError = &TRUE
                    mov D$NextDisAddressWas 0, D$CopyOfNextLabelHexa 0
                Else
                    mov D$NextDisAddressWas eax
                End_If
            pop D$GetHexaFromTextError
        .End_If
L2: .End_While
ret


Proc FloatMenuProc:
    Arguments @nCode, @wParam, @lParam

        ..If D@nCode = &HC_ACTION ; HC_NOREMOVE
            On D@wParam = &VK_ESCAPE
L1:             mov B$BlockInside &FALSE
            End_If
        ..End_If

L9:     mov eax &FALSE ; Forwarding
EndP


CopyFromFloatMenu:
    call 'USER32.DestroyMenu' D$FloatHandle
    call ControlC
ret


SetFloatSearch:
    mov esi D$BlockStartTextPtr, edi SearchString, ecx D$BlockEndTextPtr
    sub ecx esi | inc ecx
    mov D$LenOfSearchedString ecx
    rep movsb
ret

SearchUpFromFloatMenu:
    mov B$DownSearch &FALSE
    call SetFloatSearch | call SetCaret D$BlockStartTextPtr | call StringSearch
ret

SearchFromTopFromFloatMenu:
    mov B$DownSearch &TRUE
    call SetFloatSearch
    call FullUp | mov D$CaretRow 1, D$CaretLine 0 | move D$CurrentWritingPos D$CodeSource
    call StringSearch
ret

SearchDownFromFloatMenu:
    mov B$DownSearch &TRUE
    call SetFloatSearch | call StringSearch
ret


RightClickedNumber:
    inc esi
    push D$BlockStartTextPtr, D$BlockEndTextPtr
        mov D$BlockStartTextPtr esi
        push esi
L0:         lodsb | call WordEdge | cmp B$Edge &TRUE | jne L0<
            sub esi 2 | mov D$BlockEndTextPtr esi
        pop esi
        call IsItaNumber
    pop D$BlockEndTextPtr, D$BlockStartTextPtr
    If eax <> 0
        call ViewClickedNumber
    End_If
ret


[ClickedNumberValue: ?    HexaInBlock: ?    BinaryInBlock: ?]

[NumberCopy: ? #25]

IsItaNumber:
    mov eax 0, esi D$BlockStartTextPtr, bl B$esi
    mov B$HexaInBlock &FALSE, B$BinaryInBlock &TRUE

    cmp bl '0' | jb L9>>
        cmp bl '9' | ja L9>>

L0: inc esi
    cmp B$esi '_' | je L1>
    cmp B$esi '0' | jb L2>  ; 010_0000_0000
    cmp B$esi 'F' | ja L2>
    cmp B$esi 'A' | jae L1>
    cmp B$esi '9' | ja L2>
L1: jmp L0<

L2: mov ecx esi | sub ecx D$BlockStartTextPtr
    If ecx > 50
        mov eax 0 | jmp L9>>
    End_If
    mov esi D$BlockStartTextPtr, edi NumberCopy

    While esi <= D$BlockEndTextPtr
L3:     lodsb
        If al >= 'a'
            On al <= 'f', sub al 32
        End_If

        If al = '_'
            jmp L3<
        Else_If al > 'F'
            mov eax 0 | jmp L9>>
        Else_If al < '0'
            mov eax 0 | jmp L9>>
        End_If

        On al > '9',  mov B$HexaInBlock &TRUE
        On al > '1', mov B$BinaryInBlock &FALSE

        On al <> '_', stosb
    End_While

    mov D$OldStackPointer esp
    mov B$edi 0
    mov esi NumberCopy
    .If W$esi = '00'
        If B$BinaryInBlock = &TRUE
            call ClickBinary
        Else
            mov eax 0
        End_If
    .Else_If B$esi = '0'
        call ClickHexa
    .Else
        If B$HexaInBlock = &FALSE
            call ClickDecimal
        Else
            mov eax 0
        End_If
    .End_If

  ; eax = Number if any (or 0):
L9: mov D$ClickedNumberValue eax
ret
____________________________________________________________________________________________

; Numbers translations Routines without error report (no Menu Option on failure, instead)

ClickBinary:
    lodsw                                               ; clear first '00'
NackedClickBinary:
    mov ebx 0, edx 0, ecx 0
L0: lodsb | cmp al Closebracket | jbe L9>
    sub al '0' | shld edx ebx 1 | shl ebx 1 | or bl al
    cmp edx ecx | jb L8>
        mov ecx edx
            cmp al 2 | jb L0<
L8:             mov ecx D$BinTypePtr | jmp BadNumberFormat
L9: mov eax ebx
ret


ClickHexa:
    lodsb                                               ; clear first '0'
NackedClickHexa:
    mov ebx 0,  edx 0, ecx 0
L0: lodsb | cmp al LowSigns | jbe L9>
        sub al '0' | cmp al 9 | jbe L2>
            sub al 7
L2: shld edx ebx 4 | shl ebx 4 | or bl al
    cmp edx ecx | jb L8>
        mov ecx edx
            cmp al 0F | jbe L0<
L8: mov ecx HexType | jmp BadClickFormat
L9: mov eax ebx
ret


ClickDecimal:
    mov eax 0, ecx 0

L2: mov cl B$esi | inc esi                        ; (eax used for result > no lodsb)
    cmp cl LowSigns | jbe  L9>

      mov edx 10 | mul edx | jo L3>               ; loaded part * 10
                                                  ; Overflow >>> Qword
        sub  ecx '0' | jc L7>
        cmp  ecx 9   | ja L7>

          add  eax ecx | jnc  L2<
            jmp  L4>                              ; carry >>> Qword

                                                  ; if greater than 0FFFF_FFFF:
L3: sub ecx '0' | jc L7>
    cmp ecx 9   | ja L7>

      add eax ecx

L4:   adc edx 0
      mov cl B$esi | inc  esi
      cmp cl LowSigns | jbe L9>

        mov ebx eax, eax edx, edx 10 | mul edx    ; high part * 10
          jo L6>                                  ; Qword overflow
            xchg eax ebx | mov edx 10 | mul edx   ; low part * 10
            add  edx ebx
            jnc   L3<                             ; carry >>> overflow
L6:           mov eax 0 | ret

L7: mov ecx D$DezimalTypePtr | jmp BadNumberFormat
L9: ret                                           ; >>> number in EDX:EAX


BadClickFormat:
    dec esi
L0: lodsb | On al = 'X', lodsb

    ..If al = 'H'
        cmp B$esi LowSigns | ja L7>
    ..Else_If al = 'D'
        cmp B$esi LowSigns | ja L7>
    ..Else_If al = 'B'
        cmp B$esi LowSigns | ja L7>
    ..Else
      ; Try to read a Type Marker at the end, and re-run if possible:
L7:     While B$esi > LowSigns | inc esi | End_While | dec esi | lodsb
        .If al = 'H'
            If ecx = HexType
                mov eax 0 | ret
            End_If
        .Else_If al = 'D'
            If ecx = DezimalType
                mov eax 0 | ret
            End_If
        .Else_If al = 'B'
            If ecx = BinType
                mov eax 0 | ret
            End_If
        .Else
            mov eax 0 | ret
        .End_If
    ..End_If

    dec esi
;;
 esi now points to the last Char of the Number. We overwrite it: We kill the Types Markers
 and we fill at the other end (start), with zeros:
;;
    push edi
        mov edi esi | dec esi | On B$esi = 'X', dec esi
        std
            While B$esi > LowSigns | movsb | End_While
        cld
        While B$edi > LowSigns | mov B$edi '0' | dec edi | End_While
    pop edi

    inc esi | While B$esi = '0' | inc esi | End_While

    If al = 'H'
        jmp NackedClickHexa
    Else_If al = 'D'
        jmp ClickDecimal
    Else  ; al = 'B'
        jmp NackedClickBinary
    End_If
____________________________________________________________________________________________



[ClickedNumberText: ClickedHexa: "

                                    

                                                               

                                                        



", 0

ClickedNumberTitle: 'Bases forms', 0]


ViewClickedNumber:
    mov eax D$ClickedNumberValue | lea edi D$ClickedHexa+8

  ; Write Hexa form:
    call WriteEax
    mov al ' '
    While B$edi <> CR
        stosb
    End_While
    add edi 8

  ; Write Decimal form:
    mov eax D$ClickedNumberValue
    mov dl 0FF | push edx                       ; Push stack end mark
    mov ecx 10
L0: mov edx 0
    div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L9:
    mov al ' '
    While B$edi <> CR
        stosb
    End_While
    add edi 8

  ; Write Binary form:
    mov D$edi '00_ ' | add edi 3
    mov ebx D$ClickedNumberValue, ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4

    mov al '_' | stosb | stosb

L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<
    mov al '_' | stosb | mov ecx 4
L0: shl ebx 1 | mov al '0' | adc al 0 | stosb | loop L0<

    call 'USER32.MessageBoxA' D$hwnd, ClickedNumberText, ClickedNumberTitle, &MB_SYSTEMMODAL
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Little feature for "Replace all" in Selected Block: Whole words / Case insensitive

; Tag Dialog 1050

BlockReplaceAll:
    call 'USER32.DialogBoxParamA' D$hinstance, 1050, D$hwnd, BlockReplaceAllProc, &NULL
ret


[BlockFrCase: &TRUE    BlockWwSearch: &TRUE]

; Tag Dialog 1050

Proc BlockReplaceAllProc:
    Arguments @Adressee, @Message, @wParam, @lParam
    Local @StartOfBlock, @EndOfBlock

    pushad

    ...If D@Message = &WM_INITDIALOG
       ; call 'USER32.SendDlgItemMessageA' D@Adressee, 50, &BM_SETCHECK, D$BlockFrCase, 0
       ; call 'USER32.SendDlgItemMessageA' D@Adressee, 51, &BM_SETCHECK, D$BlockWwSearch, 0
        call 'USER32.GetDlgItem' D@Adressee, 10
        call 'USER32.SetFocus' eax
        popad | mov eax &TRUE | ExitP

    ...Else_If D@Message = &WM_COMMAND

        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If D@wParam = &IDCANCEL
            call 'USER32.EndDialog' D@Adressee, 0

        ..Else_If D@wParam = &IDOK
            call 'USER32.SendDlgItemMessageA' D@Adressee, 50, &BM_GETCHECK, 0, 0
            mov D$BlockFrCase eax

            call 'USER32.SendDlgItemMessageA' D@Adressee, 51, &BM_GETCHECK, 0, 0
            mov D$BlockWwSearch eax

            call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_GETTEXT, 80,
                                              SearchString
            call 'USER32.SendDlgItemMessageA' D@Adressee, 11, &WM_GETTEXT, 80,
                                              ReplaceWithString
            mov esi SearchString, ecx 0
            While B$esi > 0 | inc esi | inc ecx | End_While
            mov esi ReplaceWithString, D$LenOfReplaceString 0
            While B$esi > 0 | inc esi | inc D$LenOfReplaceString | End_While

            .If ecx > 0
                push D$UpperLine, D$CaretRow, D$CaretLine
                push D$DownSearch, D$CaseSearch, D$WholeWordSearch
                    mov B$BlockInside &FALSE

                    mov D$LenOfSearchedString ecx
                  ; No 'String not found" Message at the end:
                    mov B$OnReplaceAll &TRUE
                    move D$NextSearchPos D$BlockStartTextPtr
                    move D$CaseSearch D$BlockFrCase, D$WholeWordSearch D$BlockWwSearch
                    mov B$DownSearch &TRUE
                    move D@StartOfBlock D$BlockStartTextPtr, D@EndOfBlock D$BlockEndTextPtr

L0:                 call StringSearch

                    If B$BlockInside = &TRUE
                        mov eax D@EndOfBlock | cmp D$BlockStartTextPtr eax | ja L1>
                        call StringReplace | jmp L0<
                    End_If

L1:                 mov B$OnReplaceAll &FALSE, B$Disassembling &FALSE, B$BlockInside &TRUE
                    move D$BlockStartTextPtr D@StartOfBlock, D$BlockEndTextPtr D@EndOfBlock
                pop D$WholeWordSearch, D$CaseSearch, D$DownSearch
                pop D$CaretLine, D$CaretRow, D$UpperLine
                call AskForRedraw
            .End_If

            call 'USER32.EndDialog' D@Adressee, 0
        ..End_If

    ...Else
L8:     popad | mov eax &FALSE | ExitP

    ...End_If

    popad | mov eax &TRUE
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________


; If user DoubleClick or RightClick on Block, we check if the pointed thing is a Macro
; evocation (to append an 'Unfold' Item in the Floating Menu). Called from
; 'RightClickOnBlock'.

IsItaMacro:
    mov esi D$BlockStartTextPtr

  ; Verify we are not pointing a Declaration:
    push esi
        While B$esi-1 = ' '
            dec esi
        End_While
        mov al B$esi-1
    pop esi
    If al = '['
        mov eax 0 | ret
    End_If

    mov edi D$CodeSource, al '[', ecx D$SourceLen
    mov edx D$BlockEndTextPtr | sub edx D$BlockStartTextPtr | inc edx

  ; Search for '[' in user Source:
L0: repne scasb | jne L8>
        push eax, ecx, esi, edi
            While B$edi = ' '
                inc edi                     ; Strip possible leading Space(s).
            End_While
            mov ecx edx
L1:         mov al B$esi, bl B$edi | inc esi | inc edi | dec ecx
            or al 020 | or bl 020 | cmp al bl | jne L2>          ; No case compare with Block.
            cmp ecx 0 | ja L1<
               jmp L9>
L2:     pop edi, esi, ecx, eax

L3: cmp ecx 0 | ja L0<

L8: mov eax 0 | ret                         ; No mach found.

  ; Mach found, but is it a Macro?
L9: While B$edi = ' '
        inc edi                             ; strip trailing Space(s)
    End_While
    cmp B$edi CR | je L9>                   ; Macro.
    cmp B$edi '|' | je L9>                  ; Macro too.
    cmp B$edi ';' | je L9>                  ; Macro too with comment after symbol.
        pop edi, esi, ecx, eax | jmp L3<         ; Equate, Data or Macro with the same begining

L9: pop edi, esi, ecx, eax

    move D$InstructionToUnfold D$BlockStartTextPtr
    dec edi | mov eax edi, D$UnfoldedMacro eax
ret


; Checks if we are pointing on an Equal PreParser line.


[ASCII_DOLLAR 024, ASCII_PARAGRAPH 0A7]

IsItAnEqual:
    mov esi D$BlockStartTextPtr, eax 0, B$UnfoldEqual &FALSE
    If B$esi-1 = ASCII_DOLLAR
        sub esi 2
    Else_If B$esi-1 = ASCII_PARAGRAPH
        sub esi 2
    End_If

    mov D$InstructionToUnfold esi

  ; Go to the start of the Statement:
L0: dec esi
    If B$esi = '|'
        jmp L1>
    Else_If B$esi = LF
        jmp L1>
    Else_If B$esi = ' '
        je L0<
    Else
        ret
    End_If

  ; OK, the Selection is the first Member of a Statement. Go to next Component:
L1: mov esi D$BlockEndTextPtr
L0: inc esi
    If B$esi = ' '
        jmp L1>
    Else_If B$esi = LF
        ret
    Else_If B$esi = ','
        ret
    End_If
    jmp L0<

L1: While B$esi = ' ' | inc esi | End_While

    .If B$esi = '='
        If B$esi+1 = ' '
            mov B$UnfoldEqual &TRUE
            move D$UnfoldedMacro D$CodeSource
            dec D$UnfoldedMacro
            mov eax &TRUE | ret
        End_If
    .End_If
ret
____________________________________________________________________________________________

; Called by User DoubleLefClick. If the Block is a Label, the user can store it as
; BookMarked.

[BookMarks: ?    BookMarkLen: ?    BookMarkPtr: ?]
[ToBeBookMarked: ? #20]

IsItaLabel:
  ; If not a Label, we abort:
    mov esi D$BlockEndTextPtr
    If B$esi+1 <> ':'
        mov eax 0 | ret
    End_If

  ; Local Label, we abort:
    mov esi D$BlockStartTextPtr
    If B$esi+2 = ':'
        mov eax 0 | ret
    End_If

  ; If It is a Local Symbol, we extend. If it is a Local Label we abort:
    mov esi D$BlockStartTextPtr, edi ToBeBookMarked

    If B$esi-1 = '@'
        push edi | call SearchUpperMainLabel | pop edi | On eax = 0, ret
        mov esi eax
        While B$esi <> ':'
            movsb
        End_While
        mov B$edi '@' | inc edi
        mov esi D$BlockStartTextPtr
    End_If

    While esi < D$BlockEndTextPtr | movsb | End_While | movsb | mov B$edi 0

    sub edi ToBeBookMarked | mov D$BookMarkLen edi
    mov esi ToBeBookMarked

  ; If the Block is aready BookMarked, we enable the [UnBookMark] option:
    If D$BookMarks > 0
        mov edi D$BookMarks, ecx D$BookMarkLen
        inc edi
L0:     push esi, edi, ecx
            mov D$BookMarkPtr edi
L1:         mov al B$esi, bl B$edi | or al 020 | or bl 020 | inc esi | inc edi
            cmp al bl | jne L2>
            loop L1<
        pop ecx, eax, esi                       ; Found.
        cmp B$edi ' ' | ja L3>
        mov eax 2 | ret                         ; Return for [UnBookMark] option.
L2:     pop ecx, eax, esi                       ; Not yet found.
L3:     mov edi eax
L3:     cmp B$edi 0 | je L4>
            inc edi | jmp L3<
L4:     inc edi | cmp B$edi 0 | jne L0<         ; Not yet end of Stored BookMarks.
    End_If

  ; If here, the Label is a Main label and is not yet BookMarked:
    mov eax 1
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

SearchUpperMainLabel:
    mov edi D$BlockStartTextPtr
L0: dec edi
    While B$edi <> ':'
        dec edi
        If B$edi = '"'
            Do
                dec edi | on edi = D$CodeSource, jmp L7>
            Loop_Until B$edi = '"'
            dec edi
        Else_If B$edi = "'"
            Do
                dec edi | on edi = D$CodeSource, jmp L7>
            Loop_Until B$edi = "'"
            dec edi
        End_If
        on edi = D$CodeSource, jmp L7>
    End_While
    While B$edi > ' '
        dec edi | on edi = D$CodeSource, jmp L7>
    End_While
    inc edi
    If B$edi = '@'
        jmp L0<<
    Else_If B$edi+2 = ':'
        jmp L0<<
    Else
        mov eax edi | ret
    End_If

L7: mov eax 0
ret


[FullBookMarks: 'No more room to store BookMarks', 0
 BookMarksTitle: '               ----------- BookMarks -----------', 0
 EndBookMarks:   '                 ------------- Tree --------------', 0]
[NumberOfBookMarks: 0]

StoreBookMark:
    call CreateTreeViewList
    If D$BookMarks = 0
        VirtualAlloc BookMarks 01000 | mov D$NumberOfBookMarks 2
        call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle, 100, &LB_INSERTSTRING, 0,
                                          EndBookMarks
        call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle, 100, &LB_INSERTSTRING, 0,
                                          BookMarksTitle
    End_If
    mov edi D$BookMarks, al 0, ecx 01000
L0: repne scasb | cmp B$edi 0 | jne L0<
    push edi
        mov eax ToBeBookMarked
        While B$eax <> 0 | inc eax | End_While
        sub eax ToBeBookMarked
        If ecx <= eax
            call 'USER32.MessageBoxA' D$hwnd, FullBookMarks, Argh, &MB_SYSTEMMODAL
            pop edi | jmp L9>
        End_If
        mov ecx eax, esi ToBeBookMarked
        rep movsb
        mov al ' ' | stosb | mov al 0 | stosb
    pop edi

    call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 1 edi
    inc D$NumberOfBookMarks
L9: ret


ReInsertBookMarks:
    mov D$NumberOfBookMarks 2
    call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 0,
                                     EndBookMarks
    call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 0,
                                     BookMarksTitle

    mov esi D$BookMarks | inc esi
    .While B$esi > 0
        call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_INSERTSTRING 1 esi
        inc D$NumberOfBookMarks
        While B$esi <> 0
            inc esi
        End_While
        inc esi
    .End_While
ret


DeleteBookMark:
    call CreateTreeViewList
    call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_FINDSTRING 0-1 D$BookMarkPtr
    call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_DELETESTRING eax 0
    mov edi D$BookMarkPtr, esi edi
    add esi D$BookMarkLen | While B$esi > 0 | inc esi | End_While | inc esi
    mov ecx D$BookMarks | add ecx 01000 | sub ecx esi | rep movsb

  ; Delete the 2 added titles if no more BookMarks, delete the .BKM File and release Mem:
    dec D$NumberOfBookMarks
    If D$NumberOfBookMarks = 2
        call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_DELETESTRING 0 0
        call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle 100  &LB_DELETESTRING 0 0
        call DeleteBookMarkFile

        VirtualFree D$BookMarks
    End_If
ret


DeleteBookMarkFile:
    mov D$NumberOfBookMarks 0
    mov edi SaveFilter, al 0, ecx 0-1 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    push D$edi, edi
        mov D$edi '.BKM'

        call 'KERNEL32.FindFirstFileA' SaveFilter FindFile

        .If eax <> &INVALID_HANDLE_VALUE
            call 'KERNEL32.FindClose' eax
            call 'KERNEL32.DeleteFileA' SaveFilter
        .End_If
    pop edi, D$edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
           Unfolder's jobs.
           
  'ShowUnfoldMacro' ---> 'ShowUnfoldDialog' ---> 'UnfoldMacro' ---> 'AsmMain'
  
  Both 'AsmMain' and (after final RET), 'UnfoldMacro' call for 'UnfoldOutput'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[UnfoldedMacro: ?    InstructionToUnfold: ?    StackBeforeUnfolding: ?    WeAreUnfolding: ?]

[ShowUnfoldDialogHandle: ?]

ShowUnfoldMacro:
    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, ret
    End_If

    .If D$ShowUnfoldDialogHandle = 0
        mov B$CompileErrorHappend &FALSE
        call 'USER32.DialogBoxParamA' D$hInstance, 23000, &NULL, ShowUnfoldDialog, &NULL

    .Else
        Beep

    .End_If
ret
____________________________________________________________________________________________

[UnfoldTitle: B$ 'Macro Unfolding', 0]

; Tag Dialog 23000

Proc ShowUnfoldDialog:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If D@Message = &WM_COMMAND
         If W@wParam = &IDCANCEL
L0:         mov D$ShowUnfoldDialogHandle 0
            call 'USER32.EndDialog' D@Adressee, 0

         Else_If W@wParam = &IDOK
            jmp L0<

         End_If

    .Else_If D@Message = &WM_SIZE
        Call ResizeEditControl

    .Else_If D@Message = &WM_INITDIALOG
        move D$ShowUnfoldDialogHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

        call UnfoldMacro

        If B$UnfoldCompleted = &FALSE
            jmp L0<
        Else
            call 'USER32.SendMessageA' D@Adressee, &WM_SETTEXT, &NULL, UnfoldTitle
            mov B$FirstCTLCOLOREDIT &TRUE
        End_If

    .Else_If D@Message = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0, 0
            mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    .Else_If B$CompileErrorHappend = &TRUE
        mov D$ShowUnfoldDialogHandle 0
        call 'USER32.EndDialog' D@Adressee, 0

    .Else
        popad | mov eax &FALSE | jmp L9>

    .End_If

    popad | mov eax &TRUE

L9: EndP


Proc ResizeEditControl:
    Structure @RECT 16, @RECT_leftDis 0, @RECT_topDis 4, @RECT_rightDis 8, @RECT_bottomDis 12

        call 'USER32.GetClientRect' D$ShowUnfoldDialogHandle, D@RECT

        call 'USER32.GetDlgItem' D$ShowUnfoldDialogHandle, 101

        mov ebx D@RECT_rightDis | sub ebx D@RECT_leftDis
        mov ecx D@RECT_bottomDis | sub ecx D@RECT_topDis

        call 'USER32.MoveWindow' eax, 0, 0, ebx, ecx, &TRUE
EndP
____________________________________________________________________________________________

[UnfoldEqual: ?  UnfoldCompleted: ?]

UnfoldMacro:
    call 'USER32.SetCursor', D$WaitCursor | call AskForRedrawNow

    mov B$WeAreUnfolding &TRUE, B$UnfoldStepIndice '0'
    mov D$TrashPointer Trash3

    push D$SourceLen, D$SourceEnd

        mov D$StackBeforeUnfolding esp

        call AsmMain
      ; 'AsmMain' stops after the Macros jobs, in cases when "B$WeAreUnfolding = &TRUE".

        call 'USER32.SetCursor' D$ActualCursor

        call UnfoldOutput

        mov D$edi CRLF2, B$edi+4 0

      ; Show the result:
        mov eax D$CodeSourceB
        If D$eax <> 0
            call 'USER32.SetDlgItemTextA' D$ShowUnfoldDialogHandle, 101, Trash3
            mov B$UnfoldCompleted &TRUE

        Else
            mov B$UnfoldCompleted &FALSE

        End_If

L8: pop D$SourceEnd, D$SourceLen

    ;VirtualFree D$LabelList, D$MacroList, D$PlainLabelList,
    ;        D$StatementsTable, D$StatementsTable2, D$CodeSourceB, D$CodeSourceA
    call ReleaseAsmTables

    mov B$WeAreUnfolding &FALSE, B$ReadyToRun &FALSE

    call ReleaseAsmTables
ret

UnfoldingError:
    mov D$UnfoldErrorMessage eax, B$CompileErrorHappend &TRUE, B$UnfoldCompleted &FALSE

    call 'USER32.SetCursor' D$ActualCursor

    While esp <> D$StackBeforeUnfolding
        pop ebx
    End_While

    jmp L8<<
____________________________________________________________________________________________

[UnfoldErrorMessage: ?]

GetUnfoldStatement:
    push esi, ebx, ecx
        mov esi D$CodeSourceA, eax 0
        mov edx esi | add edx D$SourceLen

        While eax <> ecx
            .If B$esi = EOI
                If B$esi+1 = OpenBracket
                    ;
                Else_If B$esi+1 = OpenVirtual
                    ;
                Else
                    inc eax
                End_If

            .Else_If B$esi = OpenBracket
                inc eax

            .Else_If B$esi = OpenVirtual
                inc eax

            .End_If

            inc esi | On esi > edx, jmp L8>>
        End_While

      ; Translate into normal Ascii form:
        mov edi D$TrashPointer

        If B$esi-1 = OpenVirtual ; 016
            dec esi | call TranslateDeclarationToNormalAscii

        Else_If B$esi-1 = OpenBracket ; 014
            dec esi | call TranslateDeclarationToNormalAscii

        Else
            call TranslateCodeToNormalAscii

        End_If

L8: pop ecx, ebx, esi
ret
____________________________________________________________________________________________

TranslateDeclarationToNormalAscii:
L0: lodsb

    .If al = Space
        mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        mov al CR | stosb | mov al LF
    .Else_If al = TextSign
        mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | mov al '"'

    .Else_If al = MemMarker
        mov al '$'
    .Else_If al = OpenBracket
        mov al '['
    .Else_If al = CloseVirtual
        mov al ']' | stosb
        mov al CR | stosb | mov al LF | stosb | jmp L2>>
    .Else_If al = CloseBracket
        mov al ']' | stosb
        mov al CR | stosb | mov al LF | stosb | jmp L2>>
    .Else_If al = OpenVirtual
        mov al '['
    .Else_If al = AddSign
        mov al '+'
    .Else_If al = SubSign
        mov al '-'
    .Else_If al = MulSign
        mov al '*'
    .Else_If al = DivSign
        mov al '/'
    .Else_If al = numSign
        mov al '#'
    .Else_If al = colonSign
        mov al ':' | stosb | mov al ' ' | stosb
    .End_If

    stosb

    If al = LF
        mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: mov ax CRLF | stosw ;| mov al 0 | stosb
ret
____________________________________________________________________________________________

TranslateCodeToNormalAscii:
L0: lodsb

    .If al = Space
        mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        mov al CR | stosb | mov al LF
    .Else_If al = TextSign
        mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | mov al '"'
    .Else_If al = MemMarker
        mov al '$'
    .Else_If al = OpenBracket
        jmp L2>>
    .Else_If al = CloseVirtual
        mov al ']'
    .Else_If al = OpenVirtual
        mov al '['
    .Else_If al = AddSign
        mov al '+'
    .Else_If al = SubSign
        mov al '-'
    .Else_If al = MulSign
        mov al '*'
    .Else_If al = DivSign
        mov al '/'
    .Else_If al = numSign
        mov al '#'
    .Else_If al = colonSign
        mov al ':' | stosb | mov al CR | stosb | mov al LF
    .End_If

    stosb

    If al = LF
        mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: mov ax CRLF | stosw ;| mov al 0 | stosb
ret
____________________________________________________________________________________________

; pos '0' = 31
[UnfoldSteps: "
******************************
*    Macros-Engine Pass "

UnfoldStepIndice: "0    *
******************************

    ", 0]

UnfoldOutput:
    inc B$UnfoldStepIndice

    mov edi D$TrashPointer
    zCopy UnfoldSteps
    mov esi D$CodeSourceA
    mov D$TrashPointer edi

  ; Count how many Statements, in the 'StatementsTable', down to our Line:
    mov ebx D$BlockStartTextPtr, esi D$StatementsTable, ecx 1

  ; For "some reason", leading Labels must be included in the Statement:
    mov eax ebx | dec eax | While B$eax = ' ' | dec eax | End_While
    If B$eax = ':'
        While B$eax-1 > ' ' | dec eax | End_While
        mov ebx eax
    End_If

  ; Unfold upon an Equal Pre-Parser Statement:
    If B$ebx-1 = 024 ;'$' ; CharMessage
        sub ebx 2
    Else_If B$ebx-1 = 0A7 ;'$'
        sub ebx 2
    End_If

  ; Several Statements are possible. Example, in Data and in Code with a Para-Macro:
    While D$esi <> 0
        If D$esi = ebx
            call GetUnfoldStatement
            mov D$edi '    ' | add edi 4
            mov D$TrashPointer edi
        End_If

        add esi 4 | inc ecx
    End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

EncodeBoxError:
    call ErrorMessageBox 0, D$ErrorMessagePtr
    mov esp D$OldStackPointer | sub esp 4
ret
____________________________________________________________________________________________

MarginRightClick:
    On D$BreakPointsTables = 0, call InitBreakPointsTables

    call MouseTextPos
    push ebx
        call SearchTxtPtr | mov D$BreakPointLocation eax
    pop ebx

    mov D$PhysicalCaretRow eax, D$CaretRow eax, D$StartBlockCol eax, D$EndBlockCol eax,
        D$CaretLine ebx, D$StartBlockLine ebx, D$EndBlockLine ebx

    mov D$CaretRow 1

    mov eax D$BreakPointLocation | call IsEaxInBpOnTable

    call BpMenu
ret
____________________________________________________________________________________________

DoubleClick:
    call MouseTextPos

    mov D$PhysicalCaretRow eax, D$CaretRow eax, D$StartBlockCol eax, D$EndBlockCol eax,
        D$CaretLine ebx, D$StartBlockLine ebx, D$EndBlockLine ebx

    If D$DBPMenuOn = DOUBLE_CLICK_ACTION
        On B$ClickOnMargin = &TRUE, jmp DoubleClickMarginAction
    End_If

    call SearchTxtPtr

    mov al B$esi | call WordEdge | On B$Edge = &TRUE, ret

    push esi
        std
L0:       lodsb | call WordEdge | cmp B$Edge &TRUE | jne L0<         ; search start
        cld
        add esi 2 | mov D$BlockStartTextPtr esi
    pop esi

L0: lodsb | call WordEdge | cmp B$Edge &TRUE | jne L0<               ; search end

    sub esi 2 | mov D$BlockEndTextPtr esi

    mov B$BlockInside &TRUE | call SetCaret esi
    call AskForRedraw | call RightClickOnBlock
ret
____________________________________________________________________________________________


_________________________________________________________

; See comment for rotary BackTable at "SetBackTableMemory"

ClearBackTable:
    mov edi D$BackTable, D$BackTablePtr edi, eax 0, ecx 040 | rep stosd
ret


StorePosInBackTable:
    mov ebx D$BackTablePtr, eax D$UpperLine
    mov D$ebx eax | add bl 4 | mov D$ebx 0
    mov D$BackTablePtr ebx, B$MovingBack  &FALSE
ret


[MovingBack: ?]

BackClick:
    mov eax D$CodeSource | On D$SourceEnd = eax, ret

    If B$MovingBack = &FALSE              ; BackTable store old pos, not last new one.
      call StorePosInBackTable            ; here we add last new one to allow Forward
      sub bl 4                            ; moves completion.
      mov D$BackTablePtr ebx, B$MovingBack &TRUE
    End_If

    mov ebx D$BackTablePtr | sub bl 4
    If D$ebx = 0                          ; If Start pointer, lock on it
        call StartEdition | call AskForRedraw | ret
    End_If

L1: mov eax D$ebx
    mov D$BackTablePtr ebx, D$UpperLine eax
    mov D$CaretRow 1, D$CaretLine 0, D$CurrentWritingPos eax
    call TryToMove
    call ResetUpperline
    call AskForRedraw
L9: ret


ForwardClick:
    mov eax D$CodeSource | On D$SourceEnd = eax, ret

    mov ebx D$BackTablePtr | add bl 4
    mov eax D$ebx | cmp eax 0 | je L9>
      mov D$BackTablePtr ebx, D$UpperLine eax
      mov D$CaretRow 1, D$CaretLine 0, D$CurrentWritingPos eax
      call TryToMove | call ResetUpperline | call AskForRedraw
L9: ret


; If text lenght have change between two right-click moves, D$Upperline may point to
; any char in a line. We ensure start of line in D$Upperline:

ResetUpperline:
    mov esi D$Upperline
L0: cmp B$esi-1 LF | je L9>
        dec esi | jmp L0<
L9: mov D$Upperline esi
ret






