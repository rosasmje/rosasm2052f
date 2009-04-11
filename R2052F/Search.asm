TITLE Search
 _______________________________________________________________________________________
 _______________________________________________________________________________________

;                              Search / replace routines:
 _______________________________________________________________________________________
 _______________________________________________________________________________________


; Dialog Box for Find/Replace and simple search. writing
; "U$ 04 0 0 0CA 03D" at FRdialog+4 turns it simple search

[FRdialog: D$ 090C008C2 0      ; Style
 U$ 0C 0 0 0CB 06A             ; Dim  >>>   U$ 04 0 0 0CA 03D  ; Dim for search only
 0                             ; Menu (not yet)
 0                             ; Class(not yet)
 'Search what' 0               ; Title
 08 'Helv' 0]                  ; Font

[FRC0: D$ 050000202 0          ; Style
 U$ 02 0 0C8 093               ; Dim
 012C                          ; ID
 0FFFF 085                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[FRC1: D$ 050000001 0          ; Style
 U$ 089 02A 03F 011            ; Dim
 &FR_FINDNEXT                   ; ID
 0FFFF 080                     ; Class
 'Search' 0                    ; Title
 0]                            ; No creation data

[FRC2: D$ 050000000 0          ; Style
 U$ 089 013 03F 011            ; Dim
 &FR_DIALOGTERM                 ; ID
 0FFFF 080                     ; Class
 'Close' 0                     ; Title
 0]                            ; No creation data

[FRC3: D$ 050000003 0          ; Style
 U$ 047 029 036 010            ; Dim
 FR_WHOLEWORD                  ; ID
 0FFFF 080                     ; Class
 'Whole word' 0                 ; Title
 0]                            ; No creation data

[FRC4: D$ 050000007 0          ; Style
 U$ 02 0F 080 02C              ; Dim
 041                           ; ID
 0FFFF 080                     ; Class
 'Search Flags' 0              ; Title
 0]                            ; No creation data

[FRC5: D$ 050000009 0          ; Style
 U$ 07 019 038 010             ; Dim
 FR_UP                         ; ID
 0FFFF 080                     ; Class
 'Upward' 0                    ; Title
 0]                            ; No creation data

[FRC6: D$ 050000009 0          ; Style
 U$ 07 029 038 010             ; Dim
 FR_DOWN                       ; ID
 0FFFF 080                     ; Class
 'Downward' 0                  ; Title
 0]                            ; No creation data

[FRC7: D$ 050000003 0          ; Style
 U$ 047 01B 032 0E             ; Dim
 FR_MATCHCASE                  ; ID
 0FFFF 080                     ; Class
 'Case' 0                      ; Title
 0]                            ; No creation data    <<<<<<<<<<<< end of simple search

[FRC8: D$ 050000027 0          ; Style
 U$ 0 049 0CC 07               ; Dim
 0133                          ; ID
 0FFFF 080                     ; Class
 'Replace with' 0              ; Title
 0]                            ; No creation data

[FRC9: D$ 050000000 0          ; Style
 U$ 042 046 040 011            ; Dim
 FR_REPLACEALL                 ; ID
 0FFFF 080                     ; Class
 'Replace all' 0               ; Title
 0]                            ; No creation data

[FRC10: D$ 050000000 0         ; Style
 U$ 089 046 03F 011            ; Dim
 FR_REPLACE                    ; ID
 0FFFF 080                     ; Class
 'Find/Replace' 0              ; Title
 0]                            ; No creation data

[FRC11: D$ 050000202 0         ; Style
 U$ 01 05B 0C8 093             ; Dim
 0136                          ; ID
 0FFFF 085                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data



[FR_UP 150    FR_DOWN 151    FR_WHOLEWORD 154    FR_MATCHCASE 155
 FR_REPLACEALL 156    FR_REPLACE 157]

________________________________________________________________________________________

; these setting use the same Dialog Box data for both options:

SetFindReplaceBox:
    mov B$Replace &TRUE
    mov edi FRdialog | add edi 8
    mov ax 0C | stosw | mov eax 0 | stosd | mov eax 06A_00CB | stosd  ; U$ 0C 0 0 0CB 06A
    jmp L9>

SetSimpleSearchBox:
    mov B$Replace &FALSE
    mov edi FRdialog | add edi 8
    mov ax 08 | stosw | mov eax 0 | stosd | mov eax 0_3D_00CB | stosd  ; U$ 04 0 0 0CB 03D

L9: If D$FindReplaceHandle = 0
        call 'User32.CreateDialogIndirectParamA' D$hinstance, FRdialog, D$hwnd, FRproc, 0
    Else
        Beep
    End_If
ret


[UserSearchStringHandle: ?  UserReplaceStringHandle: ?  OnReplace: ?
 LenOfSearchedString: ?     LenOfReplaceString: ?]

[StringChange: ?]

GetUserSearchString:
    mov edi ControlString, esi SearchString, ecx 120 | rep movsb
    mov edi SearchString, al 0, ecx 120 | rep stosb
    call 'User32.SendMessageA' D$UserSearchStringHandle &WM_GETTEXTLENGTH 0 0
    mov D$LenOfSearchedString eax
    On eax = 0, ret
    mov D$LenOfSearchedString eax
 ; "WM_GETTEXTLENGTH" message does not include end mark. "WM_GETTEXT" does >>> inc eax
    inc eax
    call 'User32.SendMessageA' D$UserSearchStringHandle &WM_GETTEXT eax SearchString
    mov B$OnReplace &FALSE

  ; Control if user changed the string for FindOrReplace flag in FRproc. (if he changed
  ; between a Find and a Replace, we have to reset to search again):
    mov B$StringChange &FALSE
    mov edi ControlString, esi SearchString, ecx 120 | rep cmpsb | je L5>
      mov B$StringChange &TRUE

  ; add (only new) string to List and delete last one (limit=15 strings):

L5: call 'User32.SendMessageA' D$UserSearchStringHandle  &CB_FINDSTRINGEXACT 0-1 SearchString
    If eax = &CB_ERR
        call 'User32.SendMessageA' D$UserSearchStringHandle &CB_INSERTSTRING 0  SearchString
        call 'User32.SendMessageA' D$UserSearchStringHandle &CB_DELETESTRING 15 0
    End_If
ret

GetUserReplaceString:
    mov edi ReplaceWithString, al 0, ecx 120 | rep stosb
    call 'User32.SendMessageA' D$UserReplaceStringHandle, &WM_GETTEXTLENGTH, 0, 0
    mov D$LenOfReplaceString eax
    On eax = 0, ret
    inc eax
    call 'User32.SendMessageA' D$UserReplaceStringHandle, &WM_GETTEXT, eax, ReplaceWithString
    mov B$OnReplace &TRUE

  ; add (only new) string to List and delete last one (limit=15 strings):
    call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_FINDSTRINGEXACT, 0-1,
                               ReplaceWithString
    If eax = &CB_ERR
      call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_INSERTSTRING, 0,
                                 ReplaceWithString
      call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_DELETESTRING, 15, 0
    End_If
ret


; Set them all to previous state (third parameter) at initialisation time:

SetSearchFlagButtons:
    call 'User32.GetDlgItem' D$FindReplaceHandle, FR_DOWN
    call 'User32.SendMessageA' eax, &BM_SETCHECK, D$DownSearch, 0

    call 'User32.GetDlgItem' D$FindReplaceHandle, FR_UP
    mov ebx D$DownSearch | xor ebx &TRUE
    call 'User32.SendMessageA' eax, &BM_SETCHECK, ebx, 0

    call 'User32.GetDlgItem' D$FindReplaceHandle, FR_MATCHCASE
    call 'User32.SendMessageA' eax, &BM_SETCHECK, D$CaseSearch, 0

    call 'User32.GetDlgItem' D$FindReplaceHandle, FR_WHOLEWORD
    call 'User32.SendMessageA' eax, &BM_SETCHECK, D$WholeWordSearch, 0
ret


[STR01: B$ ? #120] [STR02: B$ ? #120] [STR03: B$ ? #120] [STR04: B$ ? #120] [STR05: B$ ? #120]
[STR06: B$ ? #120] [STR07: B$ ? #120] [STR08: B$ ? #120] [STR09: B$ ? #120] [STR10: B$ ? #120]
[STR11: B$ ? #120] [STR12: B$ ? #120] [STR13: B$ ? #120] [STR14: B$ ? #120] [STR15: B$ ? #120]
[STR16: ?]
[RTR01: B$ ? #120] [RTR02: B$ ? #120] [RTR03: B$ ? #120] [RTR04: B$ ? #120] [RTR05: B$ ? #120]
[RTR06: B$ ? #120] [RTR07: B$ ? #120] [RTR08: B$ ? #120] [RTR09: B$ ? #120] [RTR10: B$ ? #120]
[RTR11: B$ ? #120] [RTR12: B$ ? #120] [RTR13: B$ ? #120] [RTR14: B$ ? #120] [RTR15: B$ ? #120]
[RTR16: ?] [STR_RTR_len: RTR16-STR01]

StoreSearchStrings:
  ; ecx = [(120 bytes * 30) / 4] + 2 (edges) = 902
    mov edi STR01, eax 0, ecx D$STR_RTR_len | rep stosb                  ; clear all tables
    call 'User32.SendMessageA' D$UserSearchStringHandle, &CB_GETCOUNT, 0, 0
    On eax = &CB_ERR, ret

    mov edi STR01, ebx 0

    While eax > 0
      push eax, ebx, edi
        call 'User32.SendMessageA' D$UserSearchStringHandle, &CB_GETLBTEXT, ebx, edi
      pop edi, ebx, eax
      dec eax | inc ebx | add edi 120
    End_While

    call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_GETCOUNT, 0, 0
    On eax = &CB_ERR, ret

    mov edi RTR01, ebx 0

    While eax > 0
      push eax, ebx, edi
        call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_GETLBTEXT, ebx, edi
      pop edi, ebx, eax
      dec eax | inc ebx | add edi 120
    End_While
ret


RestoreSearchStrings:
    mov esi STR01, ebx 0
    While B$esi > 0
        push ebx, esi
            call 'User32.SendMessageA' D$UserSearchStringHandle, &CB_INSERTSTRING, ebx, esi
        pop esi, ebx
        inc ebx | add esi 120
    End_While

    mov esi RTR01, ebx 0
    While B$esi > 0
        push ebx, esi
            call 'User32.SendMessageA' D$UserReplaceStringHandle, &CB_INSERTSTRING, ebx, esi
        pop esi, ebx
        inc ebx | add esi 120
    End_While
ret

_____________________________________________________________________________________

[FinfOrReplace: ?    FindReplaceHandle: ?]

Proc FRproc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message  = &WM_INITDIALOG
        mov B$ShiftBlockInside &FALSE

        move D$FindReplaceHandle D@Adressee
        mov B$FinfOrReplace &FALSE                  ; flag 0 for Search / 1 Replace
        call 'User32.GetDlgItem' D@Adressee 012C     ; 012C = our Find Edit Box
        mov D$UserSearchStringHandle eax
        call 'User32.GetDlgItem' D@Adressee 0136     ; 0136 = our Replace Edit Box
        mov D$UserReplaceStringHandle eax
        call RestoreSearchStrings
        call SetSearchFlagButtons
        call 'User32.SetFocus' D$UserSearchStringHandle  ; return 0  to
        call 'USER32.SetClassLongA' D$FindReplaceHandle &GCL_HICON D$wc_hIcon
        jmp L8>>                                        ; keep focus to first edit control

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam
        .If eax = &FR_DIALOGTERM
L0:         call StoreSearchStrings
            mov D$FindReplaceHandle 0
            call 'User32.DestroyWindow' D@Adressee
        .Else_If eax = &FR_FINDNEXT
L1:         call GetUserSearchString
            On D$LenOfSearchedString = 0, jmp L8>>
                call RestoreRealSource
                    call StringSearch
                    On B$BlockInside = &TRUE, mov B$FinfOrReplace &TRUE ; ready for Replace if whished
                call SetPartialEditionFromPos
        .Else_If eax = &IDCANCEL
            jmp L0<
        .Else_If eax = &IDOK
            jmp L1<
        .Else_If eax = FR_DOWN
            mov B$DownSearch &TRUE, B$FinfOrReplace &FALSE
        .Else_If eax = FR_UP
           mov B$DownSearch &FALSE, B$FinfOrReplace &FALSE
        .Else_If eax = FR_MATCHCASE
           xor B$CaseSearch &TRUE | mov B$FinfOrReplace &FALSE
        .Else_If eax = FR_WHOLEWORD
           xor B$WholeWordSearch &TRUE | mov B$FinfOrReplace &FALSE
        .Else_If eax = FR_REPLACE
           call GetUserSearchString | call GetUserReplaceString
           ..If B$FinfOrReplace = &TRUE
                If B$StringChange = &TRUE   ; see "StringChange" comment in GetUserSearchString
                    mov B$FinfOrReplace &FALSE
                    call RestoreRealSource
                        call StringSearch
                    call SetPartialEditionFromPos
                Else
                  On D$LenOfSearchedString > 0, call StringReplace  ; &TRUE > Replace
                End_If
           ..Else
                call RestoreRealSource
                    call StringSearch                               ; false > Search
                call SetPartialEditionFromPos
           ..End_If
           xor B$FinfOrReplace &TRUE
        .Else_If eax = FR_REPLACEALL
           call RestoreRealSource
                call GetUserReplaceString | call GetUserSearchString
                On D$LenOfSearchedString > 0,  call StringReplaceAll
           call SetPartialEditionFromPos
        .Else
           jmp L8>
        .End_If

        call 'USER32.SetFocus' D@Adressee

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP



[SearchString: B$ ? #120]  [ReplaceWithString: B$ ? #120]  [ControlString: B$ ? #120]

[DownSearch:  ?  CaseSearch: ?  WholeWordSearch: ?   SkipDashLines: ?
 Replace: ?   StringFound: ?]
[StringNotFound: B$ 'String not found', 0]

[NextSearchPos: ?    SearchFirstChar: ?]


[Lowcase | cmp #1 'A' | jb C9> | cmp #1 'Z' | ja C9> | or #1 020 | C9: ]

; If user change the searched string inside a Find/Replace process, we have to reset
; the "FindOrReplace" flag in FRproc:

StringSearch:
    mov B$ShiftBlockInside &FALSE, B$StringFound &TRUE

    On D$LenOfSearchedString = 0, ret
    mov B$BlockInside &FALSE

L0: mov edi SearchString, esi edi, edx D$LenOfSearchedString
    dec edx                                                     ; - first tested char
    If B$DownSearch = &TRUE
        mov ah B$edi | inc edi | mov ebx 1
    Else
        add edi edx | mov ah B$edi | dec edi | std | mov ebx 0-1; ebx > inc/dec edi in L4 loop
    End_If

    On B$CaseSearch = &FALSE, LowCase ah

    If D$NextSearchPos = 0
        mov esi D$CurrentWritingPos
    Else
        mov esi D$NextSearchPos
        On esi > D$SourceEnd, mov esi D$CurrentWritingPos       ; case of massive block delete
    End_If

L2: cmp esi D$SourceEnd | ja L8>>                               ; search for fitting first Char:
    cmp esi D$CodeSource | jb L8>>
        lodsb | On B$CaseSearch = &FALSE, LowCase al
        cmp al ah | jne L2<

    ..If edx = 0                                                ; len = 1 > string found
        mov D$NextSearchPos esi
    ..Else
       mov ecx edx                                              ; first letter found:
       push eax, esi, edi
L4:      .If esi > D$SourceEnd
             pop edi, esi, eax | jmp L8>>
         .Else_If esi < D$CodeSource
             pop edi, esi, eax | jmp L8>>
         .Else
K0:          lodsb | mov ah al
             If B$SkipDashLines = &TRUE
                cmp ah '_' | je K0<
             End_If
K0:          mov al B$edi | add edi ebx
             If B$SkipDashLines = &TRUE
                cmp al '_' | je K0<
             End_If
             If B$CaseSearch = &FALSE
                LowCase ah
                LowCase al
             End_If
             If ah = al
                loop L4<
             Else
                 pop edi, esi, eax | jmp L2<<
             End_If
             mov D$NextSearchPos esi
         .End_If
L5:   pop edi, esi, eax
    ..End_If

    mov eax D$NextSearchPos                                 ; string found
    If B$DownSearch = &TRUE
        mov bl B$eax | dec eax | mov D$BlockEndTextPtr eax
        sub eax edx | mov D$CurrentWritingPos eax | mov D$BlockStartTextPtr eax
        mov bh B$eax-1
    Else
        mov bl B$eax | inc eax | mov D$CurrentWritingPos eax | mov D$BlockStartTextPtr eax
        mov D$NextSearchPos eax
        add eax edx | mov D$BlockEndTextPtr eax
        mov bh B$eax+1
    End_If

    .If B$WholeWordSearch = &TRUE
        mov al bl | call WordEdge
        If B$Edge = &TRUE
            mov al bh | call WordEdge
        End_If
        If B$Edge = &FALSE
            jmp L0<<
        End_If
    .End_If

    std | mov B$BlockInside &TRUE, D$RightScroll 0
L6: lodsb | cmp al LF | ja L6<
        dec esi | mov ebx esi | add ebx 2                     ; for caret h. Pos. count
L6: lodsb | cmp al LF | ja L6<                              ; start printing 2 lines upper
        dec esi
L6: lodsb | cmp al LF | ja L6<
        add esi 2

    If esi >= D$CodeSource
        mov D$UpperLine esi
    Else
        move D$UpperLine D$CodeSource
    End_If

    call SetCaret D$BlockEndtextPtr | jmp L9>

L8: cld
    If B$OnReplaceAll = &FALSE
        mov eax StringNotFound | call MessageBox            ; if not found
        mov B$StringFound &FALSE
    End_If
    mov D$NextSearchPos 0

L9: cld | On B$Disassembling = &FALSE, call AskForRedrawNow
ret


[ReplaceStart: ?]

StringReplace:
    mov B$ShiftBlockInside &FALSE

    .If B$BlockInside = &TRUE
        call ControlX
        mov ecx D$BlockEndTextPtr
        mov D$ReplaceStart ecx
        sub ecx D$BlockStartTextPtr | inc ecx
        dec D$CaretRow | dec D$PhysicalCaretRow
        mov esi ReplaceWithString
        While B$esi <> 0
            lodsb
            pushad
                movzx eax al | call InsertSource
            popad
            inc D$CaretRow | inc D$PhysicalCaretRow
            If al = LF
                mov D$CaretRow 1
            End_If
        End_While
    .Else
       jmp StringSearch
    .End_If

    If B$DownSearch = &TRUE
        move D$NextSearchPos D$CurrentWritingPos
    Else
        move D$NextSearchPos D$ReplaceStart
    End_If

    call AskForRedrawNow
ret


[AllDanger: "

  Are you sure you want 'replace all' ?

", 0  AllTitle: 'Danger:', 0]

[OnReplaceAll: SilentSearch: ?]

StringReplaceAll:
    call 'USER32.MessageBoxA' D$hwnd, AllDanger, AllTitle,
                              &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL

    ..If eax = &IDYES
        mov B$OnReplaceAll &TRUE

L0:     call RestoreRealSource
            call StringSearch
        call SetPartialEditionFromPos

        cmp B$BlockInside &TRUE | jne L9>

        call AskForRedrawNow | call StringReplace | jmp L0<

L9:     mov B$OnReplaceAll &FALSE
    ..End_If
ret

;StringReplaceAll:
    mov B$ShiftBlockInside &FALSE

    call 'USER32.MessageBoxA' D$hwnd, AllDanger, AllTitle,
                              &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL

    ..If eax = &IDYES
        mov B$OnReplaceAll &TRUE
L0:     call RestoreRealSource
        call StringSearch | cmp B$BlockInside &TRUE | jne L9>
        call StringReplace
        call SetPartialEditionFromPos | jmp L0<

L9:     mov B$OnReplaceAll &FALSE
        call SetPartialEditionFromPos
    ..End_If
ret








