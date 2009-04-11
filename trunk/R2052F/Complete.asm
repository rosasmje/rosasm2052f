TITLE Complete

;;
  Flag in [Configuration]: 'CompletionWanted'.

  'BuildCompletionTable' is called after each Compilation. It builds a Table of
  symbols (Labels, Macros and Equates), form the Assembler Lists: 'CompletionTable',
  Where each Record is a zero ended String.
  
  'CompletionPointers' is a Table of Pointers to the 'CompletionTable' Strings Table.
  
  The 'CodeComplete' Routines set the 'Underline' Flag on if one (and only one)
  fitting Symbol has been found.
  
  Once the 'Underline' Flag is set on, if the user hit [Ctrl]/[Space], the 'Completion'
  Routine is called.
  
  
  General Completion Checking: 'CodeComplete'
  
  'Completion' is the default (User symbols) and route to api or Equates when wanted.
  
  User symbols Routines:
  
  'UserSymbolsComplete' > 'CompletionCompare'  >'IsTheSmallerOnePartOfOthers'
  'Completion' > 'TakeCompleteModel' > 'CompleteSubstitute'
  
  Api Routine:
  
  'ApiComplete' > 'ApiCompletion' > 'CompletionListProc' > 'ApiSubstitute' > 'ReadApiForCompletion'

  OS Equates:
  
  'WinEquatesComplete'
  'EquatesCompletion'  > 'CompletionListProc' > 'InitListForEquates' > 'Win32EquatesSubstitute'
  
  'ListAllEquates'
;;
____________________________________________________________________________________________

[CompletionMode: ?] ; either 'Api', 'User', or 'Equ'

[CompletionTable: ?    CompletionPointers: ?]

BuildCompletionTable:
    VirtualFree D$CompletionTable, D$CompletionPointers

  ; Count how many Symbols:
    mov ecx 0, edx 0
    mov esi D$LabelList | add esi 5
    .While B$esi <> 0
        inc ecx | While B$esi > ' ' | inc esi | End_While | add esi 7
    .End_While
    add edx esi | sub edx D$LabelList

    mov esi D$MacroList | add esi 5
    .While B$esi <> 0
        inc ecx | While B$esi > ' ' | inc esi | End_While | add esi 11
    .End_While
    add edx esi | sub edx D$MacroList

    mov esi D$EquateList | add esi 5
    .While B$esi <> 0
        inc ecx | While B$esi > ' ' | inc esi | End_While | add esi 11
    .End_While
    add edx esi | sub edx D$EquateList

  ; Allocate mem for Strings ('CompletionTable')
  ; and for list of pointers to Strings ('CompletionPointers'):

    push edx
        inc ecx | shl ecx 2 | or ecx 1
        VirtualAlloc CompletionPointers ecx
    pop edx

    VirtualAlloc CompletionTable edx | inc D$CompletionTable

  ; Fill the Tables:
    mov edi D$CompletionTable, ebx D$CompletionPointers

    mov esi D$LabelList | add esi 5
    .While B$esi > ' '
        mov D$ebx edi | add ebx 4
        mov ecx 0
        While B$esi > ' '
            and B$esi 07F
            If B$esi = '@'
                sub ebx 4 | mov D$ebx 0
L0:             dec edi | cmp B$edi 0 | jne L0<
L0:             inc esi | cmp B$esi ' ' | ja L0<
                jmp L2>

            End_If
            movsb | inc ecx

        End_While
      ; Do not store all of Local Labels, and smaller than 4 Chars Labels
        If ecx < 4
            sub ebx 4 | mov D$ebx 0
L0:         dec edi | cmp B$edi 0 | jne L0<
        End_If

L2:     add esi 7
        mov B$edi 0 | inc edi
    .End_While

    mov esi D$MacroList | add esi 5
    .While B$esi > ' '
        mov D$ebx edi | add ebx 4
        While B$esi > ' ' | and B$esi 07F |  movsb | End_While | add esi 11
        mov B$edi 0 | inc edi
    .End_While

    mov esi D$EquateList | add esi 5
    .While B$esi > ' '
        mov D$ebx edi | add ebx 4
        While B$esi > ' '
            and B$esi 07F
            If B$esi = '@'
                sub ebx 4 | mov D$ebx 0
L0:             dec edi | cmp B$edi 0 | jne L0<
L0:             inc esi | cmp B$esi ' ' | ja L0<
                jmp L1>
            End_If
            movsb
        End_While
L1:     add esi 11
        mov B$edi 0 | inc edi
    .End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________


[CompletionWanted: ?    CompletionFound: ?    CompletionRuning: ?]

[CompletionMinimumInput: 3]

; Check what the user is typing in:

CodeComplete:
    mov D$CompletionFound 0, B$Underline &FALSE, B$CompletionRuning &TRUE

    mov eax D$CurrentWritingPos

    If B$eax <= ' '
        ; OK
    Else_If B$eax = ','
        ; OK
    Else_If B$eax = "'"
        ; OK
    Else_If B$eax = '"'
        ; OK
    Else_If B$eax = ';'
        ; OK
    Else_If B$eax = '|'
        ; OK
    Else
        jmp L9>>
    End_If

    mov esi D$CurrentWritingPos
    push D$esi, esi
        mov B$esi 0 | dec esi
        mov ecx 0 | While B$esi > ' ' | dec esi | inc ecx | End_While | inc esi
        On ecx < D$CompletionMinimumInput, jmp L5>

        If B$esi+1 = '$'
            add esi 2 | On B$esi = '@', inc esi
            call UserSymbolsComplete
        Else_If B$esi+1 = '@'
            add esi 2 | call UserSymbolsComplete
        Else_If B$esi = "'"
            call ApiComplete
        Else_If B$esi = '"'
            call ApiComplete
        Else_If B$esi = '&'
            call WinEquatesComplete
        Else_If B$esi > ' '
            call UserSymbolsComplete
        End_If
L5: pop esi, D$esi

    ..If D$CompletionFound = 1
        mov B$Underline &TRUE

    ..Else_If D$CompletionFound > 20
      ; Too much > nop

    ..Else_If D$CompletionFound > 1
        .If D$CompletionMode = 'User'
            call IsTheSmallerOnePartOfOthers

            If eax = &TRUE
                move D$CompletionPointer D$ShorterSymbolPointer
                mov B$Underline al
            End_If
        .End_If

    ..End_If

L9: ..If D$CompletionListHandle <> 0
        .If B$Underline = &TRUE
            If D$CompletionMode = 'Api'
                call ApiCompletion
            Else_If D$CompletionMode = 'Equ'
                call EquatesCompletion
            End_If
        .Else
            call 'USER32.DestroyWindow' D$CompletionListHandle
            mov D$CompletionListHandle 0
        .End_If
    ..End_If
ret


; Search if the user entered text fits with some registred compiled Symbol:

UserSymbolsComplete:
    On D$CompletionTable = 0, ret ; jmp L5>>

    mov ebx D$CompletionPointers, D$CompletionMode 'User'
    While D$ebx <> 0

        mov edi D$ebx | add ebx 4
        call CompletionCompare
        .If al = 0
            If D$CompletionFound = 1
                move D$CompletionPointersList D$CompletionPointer
            Else_If D$CompletionFound > 20
              ; Too much > nop
            Else_If D$CompletionFound > 1
                push ebx
                    mov eax D$CompletionFound, ebx D$CompletionPointer
                    dec eax | mov D$CompletionPointersList+eax*4 ebx
                pop ebx
            End_If
        .End_IF

    End_While
ret


CompletionCompare:
  ; esi > User Source // edi > CompletionTable
  ; (At esi, the user's text has been temporary zero ended)
    push esi, edi, ebx
        mov ecx 0
L0:     mov bl B$edi | inc edi
L1:     mov al B$esi | inc esi | cmp al '_' | je L1<

        .If al >= 'a'
            On al <= 'z', sub al 020
        .Else_If al = 0
            If bl > ' '
                inc D$CompletionFound
                pop ebx, edi, esi
                mov D$CompletionPointer edi | ret
            End_If
        .End_If

        cmp al bl | jne L9>

        jmp L0<
L9: pop ebx, edi, esi
ret


; Several fiting Symbols found:
; Take the smaller one if all others bigger ones have the same prefix:

[ShorterSymbolPointer: ?    ShorterSymbolSize: ?]

IsTheSmallerOnePartOfOthers:
  ; get the smaller one:
    mov esi CompletionPointersList

    mov edx D$CompletionFound, ebx 0, D$ShorterSymbolSize 0-1

    .While ebx < edx
        mov eax D$esi+ebx*4, ecx 0

        While B$eax > ' ' | inc eax | inc ecx | End_While
        If ecx < D$ShorterSymbolSize
            mov D$ShorterSymbolSize ecx
            move D$ShorterSymbolPointer D$esi+ebx*4
        End_If

        inc ebx
    .End_While

  ; Now, is this smaller one a valid Prefix for all fitting Symbols:
    mov ebx 0, edx CompletionPointersList

    .While ebx < D$CompletionFound
        mov edi D$ShorterSymbolPointer, esi D$edx, ecx D$ShorterSymbolSize
        repe cmpsb
        If ecx <> 0
            mov eax &FALSE | ret
        End_If
        inc ebx | add edx 4
    .End_While

    mov eax &TRUE
ret


[CompletionPointer: ?]

; 'Completion' is called when user hit [Ctrl][Space]

Completion:
    If D$CompletionMode = 'Api'
        jmp ApiCompletion
    Else_If D$CompletionMode = 'Equ'
        jmp EquatesCompletion
    End_If

  ; Default case for User Symbols Completion:

    mov D$CompletionMode 'User'

    call RestoreRealSource

    mov esi D$CodeSource, edx D$SourceEnd

    .While esi < edx
        ...If D$esi = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi >= edx, jmp L9>>
            End_While
            add esi 4

        ...Else_If B$esi = ';'
            While B$esi > CR | inc esi | End_While

        ...Else_If B$esi = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else_If B$esi = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else
          ; Not necesserary the Declaration. Any previous evocation will do as well:
            mov al B$esi-1
            If al = '['
                ; Good
            Else_If al <= ' '
                ; Good
            Else_If al = ','
                ; Good
            Else_If al = '$'
                ; Good
            Else_If al = '|'
                ; Good
            Else
                jmp L5>>
            End_If

          ; > edi > ebx > Edition Pointer  //  esi > ecx > Search Pointer
            mov edi D$CompletionPointer, ecx esi

L0:         mov al B$esi
            If al >= 'a'
                On al <= 'z', sub al 020
            Else_If al = '_'
                inc esi | jmp L0<
            End_If

            If al <> B$edi
                mov esi ecx | jmp L5>
            End_If

            inc esi | inc edi

            .If B$edi <= ' '
                If B$esi <= ' '
                    ; Good
                Else_If B$esi = ','
                    ; Good
                Else_If B$esi = ':'
                    ; Good
                Else_If B$esi = ';'
                    ; Good
                Else_If B$esi = '|'
                    ; Good
                Else
                    mov esi ecx | jmp L5>
                End_If

                xchg esi ecx | mov edi CompletionModel
                While esi < ecx | movsb | End_While | mov B$edi 0 | jmp L9>

            .Else
                jmp L0<

            .End_If
        ...End_If

L5:     inc esi
    .End_While

L9: call SetPartialEditionFromPos

    call CompleteSubstitute
ret


[CompletionPointersList: ? #20] [CompletionModel: ? #20]

; Substitution for User defined Symbols

CompleteSubstitute:
    mov B$Underline &FALSE, B$Keys+&VK_CONTROL &FALSE
    mov D$CompletionMode 0, B$CompletionRuning &TRUE

    mov edi D$CurrentWritingPos
    dec edi | While B$edi > ' ' | dec edi | End_While | inc edi
    If B$edi+1 = '$'
        add edi 2 | On B$edi = '@', inc edi
    Else_If B$edi+1 = '@'
        add edi 2
    End_If

    mov B$OldBlockInside &FALSE
    While D$CurrentWritingPos > edi
        push edi
            call EraseLastChar
            call AskForRedrawNow
        pop edi
    End_While

    mov esi CompletionModel

    While B$esi <> 0
        push esi
            movzx eax B$esi | call InsertSource
            call AskForRedrawNow
        pop esi
        inc esi
    End_While

    mov B$CompletionRuning &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Api Completion

[ApiInput: ? #20]

; Search if the user entered text fits with some Api Name:

ApiComplete: ; Win32Functions  WinApiFirstPass
    mov D$CompletionFound 0, D$CompletionMode 'Api'

    mov eax D$esi-5 | or eax 020202020 | On eax <> 'call', ret

    inc esi

  ; esi points now after the Text Separator. Is there some '.'?
    mov ebx D$CurrentWritingPos
    While ebx > esi
        dec ebx
        If B$ebx = '.'
            inc ebx | mov esi ebx | jmp L0>
        End_If
    End_While

  ; Make a Copy for the ListBox Edit: ;;; ???!!! What use now?
L0: push esi
        mov edi ApiInput
        While esi < D$CurrentWritingPos | movsb | End_While | mov B$edi 0
    pop esi

    mov ecx D$CurrentWritingPos | sub ecx esi | On ecx < D$CompletionMinimumInput, ret

    mov al B$esi, edi Win32Functions, ecx 0

    .While edi < EndOfFunctionsList
        ..If B$edi < al
          ; Next Line:
L2:         While B$edi <> LF | inc edi | End_While | inc edi

        ..Else_If B$edi > al
            ret

        ..Else
            mov D$CompletionPointer edi
            mov ebx esi | inc ebx | inc edi
            While ebx < D$CurrentWritingPos
                mov cl B$ebx | cmp cl B$edi | jne L2<
                inc ebx | inc edi
            End_While
            mov D$CompletionFound 1 | ret

        ..End_If

    .End_While
L9: ret

____________________________________________________________________________________________

ApiCompletion:
    mov D$CompletionMode 'Api'

    If D$CompletionListHandle <> 0
        call 'USER32.DestroyWindow' D$CompletionListHandle
    End_If

    call 'USER32.CreateDialogParamA' D$hinstance, 1200, D$hwnd, CompletionListProc, 0

    call 'USER32.SetFocus' D$hwnd

    mov B$Keys+&VK_CONTROL &FALSE
ret

____________________________________________________________________________________________

Proc ToCompletionList:
    Argument @Key

    call 'USER32.GetDlgItem' D$CompletionListHandle, 10
    call 'USER32.SetFocus' eax

    call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_GETCURSEL, 0, 0

    .If D@Key = &VK_UP
        If eax > 0
            dec eax
            call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10,
                                              &LB_SETCURSEL, eax, 0
        End_If

    .Else_If D@Key = &VK_DOWN
        inc eax
        call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_SETCURSEL, eax, 0

    .Else_If D@Key = CR
        call CompletionListProc D$CompletionListHandle, &WM_COMMAND, &IDOK, 0

    .End_If
EndP


[CompletionListHandle: ?]

; Tag Dialog 1200

Proc CompletionListProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        On D$CompletionListHandle <> 0,
            call 'USER32.EndDialog' D$CompletionListHandle, 0

        move D$CompletionListHandle D@Adressee | call SetCompletionPos

        .If D$CompletionMode = 'Api'
            call InitListForApi
        .Else_If D$CompletionMode = 'User'
            mov ecx 0
        .Else_If D$CompletionMode = 'Equ'
            call InitListForEquates
            If ecx = 0-1
                call 'USER32.EndDialog' D$CompletionListHandle, 0
                mov D$CompletionListHandle 0
            Else_If ecx = 0
                call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10,
                                                  &LB_GETTEXT, 0, CompletionModel
                mov ecx 0
            End_If
        .End_If

L1:     If ecx = 0
            mov D$CompletionPointer CompletionModel
            call AskForRedrawNow
            call SubstituteFromEitherList
            call 'USER32.EndDialog' D$CompletionListHandle, 0
            mov B$Underline &FALSE, D$CompletionListHandle, 0
        Else
            call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &LB_SETCURSEL, 0, 0
        End_If

        jmp L8>>

    ...Else_If D@Message = &WM_COMMAND
        If D@wParam = &IDCANCEL
            call 'USER32.EndDialog' D$CompletionListHandle, 0
            mov D$CompletionListHandle 0

        Else_If D@wParam = &IDOK
            call ListOk

        Else_If W@wParam+2 = &LBN_DBLCLK
            call ListOk

        End_If

    ...Else_If D@Message = &WM_VKEYTOITEM
        If W@wParam = &VK_RETURN
            call ListOk
            popad | mov eax 0-2 | jmp L9>

        Else_If W@wParam = &VK_ESCAPE
            call 'USER32.EndDialog' D$CompletionListHandle, 0
            mov D$CompletionListHandle 0
            popad | mov eax 0-2 | jmp L9>

        Else
            popad | mov eax 0-1 | jmp L9>

        End_If

    ...Else_If D@Message = &WM_CHARTOITEM
      ; For Chars inputs to the User's Source. Does not work under 95:
        call 'USER32.SetFocus' D$hwnd

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE
L9: EndP

[ListRectangle: @X1: ? @Y1: ? @X2: @W: ? @Y2: @H: ?
 HwndTitleHight: ?]

SetCompletionPos:
    call 'USER32.GetWindowRect' D$hwnd, WindowX
    mov eax D$WindowX | sub D$WindowW eax
    mov eax D$WindowY | sub D$WindowH eax

    call 'USER32.GetClientRect' D$hwnd, ListRectangle
    mov eax D$WindowH | sub eax D$ListRectangle@H | mov D$HwndTitleHight eax

    call 'USER32.GetWindowRect' D$CompletionListHandle, ListRectangle

  ; Write the ApiList Window Width and hight, instead of RECTright, RECTbottom:
    mov eax D$ListRectangle@X2 | sub eax D$ListRectangle@X1
    mov D$ListRectangle@W eax
    mov eax D$ListRectangle@Y2 | sub eax D$ListRectangle@Y1
    mov D$ListRectangle@H eax

  ; Count how many Chars in the Underlinement:
    mov esi D$CurrentWritingPos, ecx 0 | dec esi
    While B$esi > ' ' | dec esi | inc ecx | End_While

  ; Translate Caret Pos into Pixels Pos:
    mov eax D$CaretRow | sub eax ecx | call RowToX eax | add eax D$WindowX
    push eax
        call LineToY D$CaretLine
        add eax D$WindowY | add eax D$HwndTitleHight
        push eax

          ; Is the Caret in the upper of lower part of the Screen?
            call 'USER32.GetSystemMetrics' &SM_CYSCREEN
            mov ecx eax | shr ecx 1
    pop ebx, eax

    If ebx < ecx
      ; Upper half: Set ListBox Pos under the user writing Pos:
        add ebx  D$FontHeight
    Else
      ; Lower half: Set ListBox Pos above the user writing Pos:
        sub ebx D$FontHeight | sub ebx D$ListRectangle@H
        On ebx > 0FFFF, mov ebx 0
    End_If

    call 'USER32.MoveWindow' D$CompletionListHandle, eax, ebx,
                             D$ListRectangle@W, D$ListRectangle@H, &TRUE
ret


InitListForApi:
    mov esi D$CompletionPointer, ecx 0
L0: mov edi CompletionModel
    While B$esi > ' ' | movsb | End_While | mov B$edi 0
    push esi, ecx
        call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_ADDSTRING,
                                          0, CompletionModel
    pop ecx esi
    While B$esi <= ' ' | inc esi | End_While
    mov ebx esi, edi ApiInput
    While B$edi <> 0
        mov al B$ebx | cmp al B$edi | jne L9>
        inc ebx | inc edi
    End_While
    inc ecx | mov B$edi 0 | jmp L0<
L9: ret


ListOk:
    call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_GETCURSEL, 0, 0
    call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_GETTEXT, eax,
                                      CompletionModel
    mov D$CompletionPointer CompletionModel
    call SubstituteFromEitherList
    call 'USER32.EndDialog' D$CompletionListHandle, 0
    mov B$Underline &FALSE, D$CompletionListHandle, 0
ret


SubstituteFromEitherList:
    If D$CompletionMode = 'Api'
        call ApiSubstitute
    Else_If D$CompletionMode = 'User'

    Else_If D$CompletionMode = 'Equ'
        call Win32EquatesSubstitute
    End_If
Ret


ApiSubstitute:
    mov B$CompletionRuning &TRUE, B$Underline &FALSE
    mov esi D$CurrentWritingPos

    call ReadApiForCompletion | On B$ApiFileOK = &FALSE, ret

L0: call EraseLastChar | call AskForRedrawNow
    mov eax D$CurrentWritingPos
    .If B$ApiFound = &FALSE
        If B$eax-1 = '.'
            While B$eax-1 <> "'"
                dec eax | cmp B$eax-1 '"' | je L1>
            End_While
            jmp L1>
        End_If
    .End_If
    cmp B$eax-1 "'" | je L1>
    cmp B$eax-1 '"' | jne L0<

L1: movzx eax B$eax-1
    push eax
        mov esi D$CompletionPointer
        While B$esi > ' '
            push esi
                movzx eax B$esi | call InsertSource
                move D$PhysicalCaretRow D$CaretRow
            pop esi
            inc esi
        End_While
    pop eax

    mov esi D$CurrentWritingPos | On B$esi <> al, call InsertSource
    call AskForRedrawNow

    mov B$ApiFound &FALSE, B$CompletionRuning &FALSE, D$CompletionMode 0
ret


ReadApiForCompletion:
    mov B$ApiFound &FALSE
    call OpenApiFunctionsFile | On B$ApiFileOK = &FALSE, ret

    mov esi D$Win32ApiList, ebx D$CompletionPointer

    mov edx D$ApiFileSize | add edx esi

L1: .While esi < edx
        inc esi
        ...If B$esi = '.'
            mov edi ebx                 ; edi > Our model  // esi > Win32ApiList
            inc esi

            Do
                mov al B$esi, ah B$edi

                .If al = '('
                    If ah <= ' '
                        mov B$ApiFound &TRUE
                        While B$esi-1 > LF | dec esi | End_While
                        mov edi ebx

                        While B$esi <> '(' | movsb | End_While | mov B$edi 0 | jmp L9>
                    End_If
                .End_If

                inc esi | inc edi
            Loop_Until al <> ah

        ...End_If
    .End_While

L9: VirtualFree D$Win32ApiList
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; OS Equates Completion

____________________________________________________________________________________________
____________________________________________________________________________________________


; Search if the user entered text fits with some Win32 Equate:

WinEquatesComplete:
    mov D$CompletionFound 1, D$CompletionMode 'Equ'
ret


EquatesCompletion:
    call IsEquatesEquThere

    If B$IncludesOK = &TRUE
        call GetEquFilesMemory
        call ReadEquatesEqu
        call 'USER32.CreateDialogParamA' D$hinstance, 1200, D$hwnd, CompletionListProc, 0
        call 'USER32.SetFocus' D$hwnd
    End_If

    VirtualFree D$EquateIncMemory

    mov B$Keys+&VK_CONTROL &FALSE
ret


InitListForEquates:
    mov esi D$CurrentWritingPos, ecx 0

  ; Take an Upper Case copy of user input:
    While B$esi-1 > '&' | dec esi | End_While | mov edi CompletionModel
    While esi < D$CurrentWritingPos
        lodsb | On al > 'Z', and al 00_11011111
        stosb
    End_While
    mov B$edi 0

  ; Search fitting Items in Equates.equ:
    mov edi D$EquateIncMemory, ecx 0, edx edi | add edx D$EquatesIncFileSize

L0: mov esi CompletionModel, al B$esi
    .If B$edi = al
        While B$edi = al | inc esi | inc edi | mov al B$esi | End_While
        If al = 0
            While B$edi > ' ' | inc edi | End_While
            push edx, ecx, D$edi, edi
                mov B$edi 0 | dec edi
                While B$edi > LF
                    dec edi | On edi = D$EquateIncMemory, jmp L1>
                End_While
                inc edi
L1:             call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_ADDSTRING, 0, edi
            pop edi, D$edi, ecx, edx
            inc ecx
            jmp L2>

        Else
L2:         While B$edi > LF | inc edi | End_While | inc edi
            On edi >= edx, jmp L9>
            mov al B$CompletionModel
            On al >= B$edi, jmp L0<

        End_If

    .Else_If B$edi <= al
        jmp L2<

    .End_If

L9: dec ecx | On ecx = 0-1, call ListAllEquates
ret


; No matching Equate found > propose the full List:

ListAllEquates:
    call 'USER32.MessageBoxA', D$hwnd, {"Do you want the complete List?

(This may take some time to build the list)", 0}, {'No such OS Equate', 0},
&MB_YESNO__&MB_SYSTEMMODAL

    If eax = &IDNO
        mov ecx 0-1 | ret
    End_If

    On D$EquateIncMemory = 0, ret
    On D$EquatesIncFileSize = 0, ret

    mov esi D$EquateIncMemory, edx esi | add edx D$EquatesIncFileSize

L0: mov edi esi | While B$edi > ' ' | inc edi | End_While

    push edx, esi D$edi, edi
        mov B$edi 0
        call 'USER32.SendDlgItemMessageA' D$CompletionListHandle, 10, &LB_ADDSTRING, 0, esi
    pop edi, D$edi, esi, edx

    If eax <> &LB_ERR
        mov esi edi |  While B$esi-1 <> LF | inc esi | End_While
        On esi < edx, jmp L0<
    End_If
ret


Win32EquatesSubstitute:
    mov B$CompletionRuning &TRUE, B$Underline &FALSE, D$CompletionMode 0

    mov esi D$CurrentWritingPos

L0: call EraseLastChar | ;call AskForRedrawNow
    mov eax D$CurrentWritingPos
    cmp B$eax-1 '&' | jne L0<

L1: mov esi D$CompletionPointer
    While B$esi > ' '
        push esi
            movzx eax B$esi | call InsertSource
            move D$PhysicalCaretRow D$CaretRow
        pop esi
        inc esi
    End_While

    call AskForRedrawNow

    mov B$CompletionRuning &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  While the Completion ListBox is runing, there are many user actions that may
  require to close this ListBox (moves of Caret by KeyBoard or Mouse).
;;

KillCompletionList:
    If D$CompletionListHandle <> 0
        call 'USER32.SendMessageA' D$CompletionListHandle, &WM_COMMAND, &IDCANCEL, 0
    End_If
    mov B$Underline &FALSE
    call AskForRedraw
ret












