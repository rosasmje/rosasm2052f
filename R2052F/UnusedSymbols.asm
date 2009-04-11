TITLE UnusedSymbols
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
writer :  /\\o//\      - December the 31, 2004 -

 'LabelList'
 'SearchSortedRegularLabel'
 'LowSigns'
 'DoneFlag'
 'BuildPlainLabelList'

;;
____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________

RemoveLocalLabels:
    VirtualFree D$PlainLabelList

    mov eax D$LabelList, eax D$eax | inc eax

    VirtualAlloc PlainLabelList eax

    mov edx D$LabelList | add edx D$edx
    mov edi D$PlainLabelList | add edi 5
    mov esi D$LabelList | add esi 5

    .While esi < edx
        cmp B$esi+2 EOI | jne L1>
            cmp B$esi 'A' | jb L1>
            cmp B$esi 'Z' | ja L1>
                cmp B$esi+1 '0' | jb L1>
                cmp B$esi+1 '9' | ja L1>
                  ; |L0|dWord Byte| >>> 9
                    add esi 9 | jmp L2>

L1:     While B$esi <> EOI
            movsb
        End_While
        movsb   ; |
        movsd   ; Ptr
        movsb   ; Flag
        movsb   ; |
L2: .End_While

    mov eax edi | mov D$EndOfPlainLabelList eax | sub eax D$PlainLabelList
    mov edi D$PlainLabelList | stosd | mov al EOI | stosb
ret
____________________________________________________________________________________________

[WorkBuffer: ? #256]

[CodeLabelNameList: ?
 CodeLabelNameList.Current: ?
 DataLabelNameList: ?
 DataLabelNameList.Current: ?
 UnusedCodeAndDataDialogHandle: ?
 RecompileWanted: ?
 NumberOfUnusedDataLabels: ?
 NumberOfUnusedCodeLabels: ?]


;Task : Seperate CodeLabels from DataLabel, (Put into a list of their own)
Proc DisplayUnusedSymbolsDialog:
    Argument @Adressee
        call RemoveLocalLabels
        mov eax D$PlainLabelList | mov eax D$eax
        push eax
            VirtualAlloc CodeLabelNameList eax
        pop eax
            VirtualAlloc DataLabelNameList eax

        mov edi D$CodeLabelNameList, D$CodeLabelNameList.Current edi
        mov edi D$DataLabelNameList, D$DataLabelNameList.Current edi

        mov ecx D$PlainLabelList | add ecx D$ecx
        mov esi D$PlainLabelList | add esi 5
        mov edi WorkBuffer
        mov D$NumberOfUnusedCodeLabels 0, D$NumberOfUnusedDataLabels 0

        .While esi < ecx
L1:         While B$esi <> EOI
                movsb
            End_While

            Test B$esi+5 DoneFlag | jnz L1>>

            mov B$edi 0 | inc edi

            Test B$esi+5 CodeLabelFlag | jz L2>
                push esi
                    mov edi D$CodeLabelNameList.Current
                    mov esi WorkBuffer
                    While B$esi <> 0 | movsb | End_While
                    mov B$edi 0 | inc edi
                    mov D$CodeLabelNameList.Current edi
                    inc D$NumberOfUnusedCodeLabels
                pop esi
            jmp L1>

            L2: Test B$esi+5 DataLabelFlag | jz L1>
                push esi
                    mov edi D$DataLabelNameList.Current esi WorkBuffer
                    While B$esi <> 0 | movsb | End_While
                    mov B$edi 0 | inc edi
                    mov D$DataLabelNameList.Current edi
                    inc D$NumberOfUnusedDataLabels
                pop esi
            Jmp L1>
L1:         add esi 7
            mov edi WorkBuffer
L2:     .End_While
; Tag Dialog 4
        call 'USER32.EndDialog' D@Adressee, 0
        call 'USER32.DialogBoxParamA' D$hInstance, 4, 0, UnusedCodeAndDataDialogCallBack, 0

        VirtualFree D$CodeLabelNameList
        VirtualFree D$DataLabelNameList
EndP
____________________________________________________________________________________________

[CodeListBox 101
 DataListbox 102
 UnUsedSymbolsEditBox 111
 FindDeclaration 4
 FreeSearchCheckBox 5
 UnusedSymbolHelpbutton 9]

[TextToRetrive: B$ ? #256]
[FocusedChild: D$ ?
CurrentListBox: ?

CodeListBox.Handle: ?
DataListBox.Handle: ?
UnUsedSymbolsEditBox.Handle: ?
FindDeclaration.Handle: ?
FreeSearchCheckBox.Handle: ?
UnusedSymbolHelpbutton.Handle: ?]
DataAndCodeLabelListBoxNotification:
   movzx ebx ax | mov D$CurrentListBox ebx
   shr eax 16
   If ax = &LBN_DBLCLK
      call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, D$CurrentListBox,
                                        &LB_GETCARETINDEX 0 0
      call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, D$CurrentListBox,
                                        &LB_GETTEXT eax TextToRetrive
      call 'user32.SetDlgItemTextA' D$UnusedCodeAndDataDialogHandle, UnUsedSymbolsEditBox,
                                    TextToRetrive
      call SearchForUnusedSymbol
   Else_If ax = &LBN_SETFOCUS
        move D$FocusedChild D$CurrentListBox
   End_If
ret

[EM_SETFOCUS 0100]
UnUsedSymbolsEditBoxNotification:
   shr eax 16
   If ax = EM_SETFOCUS
        mov D$FocusedChild UnUsedSymbolsEditBox
   End_If
ret

[CharDistance 020]
SearchForTheSymbolInUnUsedSymbolsEditBox:
   call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, UnUsedSymbolsEditBox,
                                        &WM_GETTEXTLENGTH 0 0
   inc eax
   mov D$LenOfSearchedString eax



   call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, UnUsedSymbolsEditBox,
                                        &WM_GETTEXT eax TextToRetrive


   mov eax TextToRetrive

   While B$eax <> 0
      cmp B$eax 'Z' | jbe L0>
      cmp B$eax 'A' | ja L0>
      sub B$eax CharDistance
     L0:
     inc eax
   End_While

    mov eax TextToRetrive
    mov ecx D$LenOfSearchedString


    mov esi TextToRetrive, edi ControlString | rep movsb
    mov esi TextToRetrive, edi SearchString ecx D$LenOfSearchedString | rep movsb

    dec D$LenOfSearchedString
    push D$DownSearch, D$WholeWordSearch, D$SkipDashLines
        mov B$DownSearch &TRUE, B$WholeWordSearch &FALSE, B$SkipDashLines &TRUE
        mov D$NextSearchPos 0
        On B$RealSourceRestored = &FALSE, call RestoreRealSource
            move D$CurrentWritingPos D$CodeSource
            call StringSearch
            push D$CodeSourceA D$CodeSourceB
                mov esi D$CurrentWritingPos | sub esi 2 | call InternalRightClick
            pop D$CodeSourceB D$CodeSourceA
            mov B$FinfOrReplace &FALSE
        call SetPartialEditionFromPos
    pop D$SkipDashLines, D$WholeWordSearch, D$DownSearch
    On B$StringFound = &FALSE, call 'USER32.SetForegroundWindow' D$UnusedCodeAndDataDialogHandle


ret



____________________________________________________________________________________________

[FreeSearchActive: &FALSE]
SearchForUnusedSymbol:
    mov eax TextToRetrive, ecx 0

    While b$eax <> 0
        inc ecx | inc eax
    End_While
    inc ecx

    push eax ecx
        call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, FreeSearchCheckBox,
                                     &BM_GETCHECK, 0, 0

        mov D$FreeSearchActive eax
        cmp eax &TRUE
    pop ecx eax | je L0>

            mov B$eax ':', B$eax + 1 0 | inc ecx
    L0:
    mov D$LenOfSearchedString ecx

    mov eax TextToRetrive
    While B$eax <> 0
        cmp B$eax 'Z' | jbe L0>
        cmp B$eax 'A' | ja L0>
        sub B$eax CharDistance
     L0:
        inc eax
    End_While


PerformSearch:

    mov esi TextToRetrive, edi ControlString | rep movsb
    mov esi TextToRetrive, edi SearchString ecx D$LenOfSearchedString | rep movsb

    dec D$LenOfSearchedString
    push D$DownSearch, D$WholeWordSearch, D$SkipDashLines
        mov eax D$FreeSearchActive | sub eax 1 | neg eax
        mov B$DownSearch &TRUE, B$WholeWordSearch al, B$SkipDashLines &TRUE
        mov D$NextSearchPos 0
        On B$RealSourceRestored = &FALSE, call RestoreRealSource
            move D$CurrentWritingPos D$CodeSource
            call StringSearch
            push D$CodeSourceA D$CodeSourceB
                mov esi D$CurrentWritingPos | sub esi 2 | call InternalRightClick
            pop D$CodeSourceB D$CodeSourceA
            mov B$FinfOrReplace &FALSE
        call SetPartialEditionFromPos

    pop D$SkipDashLines, D$WholeWordSearch, D$DownSearch

    On B$StringFound = &FALSE, call 'USER32.SetForegroundWindow' D$UnusedCodeAndDataDialogHandle
ret


DesideSearchForUnusedSymbols:
  If D$FocusedChild = UnUsedSymbolsEditBox
      call SearchForTheSymbolInUnUsedSymbolsEditBox
  Else
     mov ax &LBN_DBLCLK | shl eax 16 | mov ax w$FocusedChild | call DataAndCodeLabelListBoxNotification
  End_If

  On B$StringFound = &FALSE, call 'USER32.SetForegroundWindow' D$UnusedCodeAndDataDialogHandle
ret
____________________________________________________________________________________________

[NoteForWorkerBee:"You can press F5 to compile, without losing this list" 0]

; Tag Dialog 4


[OldUnusedSymbolDialogSubClassProc:  ?
 OldUnusedSymbolDialogSubClassProc1: ?
 OldUnusedSymbolDialogSubClassProc2: ?
 OldUnusedSymbolDialogSubClassProc3: ?
 OldUnusedSymbolDialogSubClassProc4: ?
 OldUnusedSymbolDialogSubClassProc5: ?
 OldUnusedSymbolDialogSubClassProc6: ?]

Proc UnusedSymbolDialogSubClassProc:
Arguments @Adressee, @Message, @wParam, @lParam

    .if D@Message = &WM_KEYDOWN
        if D@Wparam = &VK_F5
            mov B$RecompileWanted &TRUE
            mov B$ShowStats &FALSE
            call SaveUnusedIndex
            push D$UnusedCodeAndDataDialogHandle
                mov D$UnusedCodeAndDataDialogHandle 0
                mov B$UnusedSymbolsWanted &TRUE
            pop eax
            call 'USER32.EndDialog' eax, 0
            ExitP
        end_if
    .end_if


   @DefaultProcessing:
   mov eax D@Adressee
   .if eax = D$CodeListBox.Handle
       call D$OldUnusedSymbolDialogSubClassProc1 D@Adressee, D@Message, D@wParam, D@lParam
   .else_if eax = D$DataListBox.Handle
       call D$OldUnusedSymbolDialogSubClassProc2 D@Adressee, D@Message, D@wParam, D@lParam
   .else_if eax = D$UnUsedSymbolsEditBox.Handle
       call D$OldUnusedSymbolDialogSubClassProc3 D@Adressee, D@Message, D@wParam, D@lParam
   .else_if eax = D$FindDeclaration.Handle
       call D$OldUnusedSymbolDialogSubClassProc4 D@Adressee, D@Message, D@wParam, D@lParam
   .else_if eax = D$UnusedSymbolHelpbutton.Handle
       call D$OldUnusedSymbolDialogSubClassProc5 D@Adressee, D@Message, D@wParam, D@lParam
   .else
       call D$OldUnusedSymbolDialogSubClassProc6 D@Adressee, D@Message, D@wParam, D@lParam
   .end_if
EndP

Proc InstallSubClassCallbacks:
Argument @Adressee
            call 'USER32.GetDlgItem' D@Adressee CodeListBox
            if eax <> 0
                mov D$CodeListBox.Handle eax
                call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                mov D$OldUnusedSymbolDialogSubClassProc1 eax
            end_if

            call 'USER32.GetDlgItem' D@Adressee DataListbox
            if eax <> 0
                mov D$DataListBox.Handle eax
                call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                mov D$OldUnusedSymbolDialogSubClassProc2 eax
            end_if

            call 'USER32.GetDlgItem' D@Adressee UnUsedSymbolsEditBox
            if eax <> 0
                mov D$UnUsedSymbolsEditBox.Handle eax
                call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                mov D$OldUnusedSymbolDialogSubClassProc3 eax
            end_if

            call 'USER32.GetDlgItem' D@Adressee FindDeclaration
            if eax <> 0
                mov D$FindDeclaration.Handle eax
                call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                mov D$OldUnusedSymbolDialogSubClassProc4 eax
            end_if
            mov D$FindDeclaration.Handle eax

            call 'USER32.GetDlgItem' D@Adressee UnusedSymbolHelpbutton
            if eax <> 0
                mov D$UnusedSymbolHelpbutton.Handle eax
                call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                mov D$OldUnusedSymbolDialogSubClassProc5 eax
            end_if

            call 'USER32.GetDlgItem' D@Adressee FreeSearchCheckBox
            if eax <> 0
                call 'USER32.SetWindowLongA' eax &GWL_WNDPROC UnusedSymbolDialogSubClassProc
                mov D$OldUnusedSymbolDialogSubClassProc6 eax
            end_if
EndP

Proc UnusedCodeAndDataDialogCallBack:
    Arguments @Adressee, @Message, @wParam, @lParam

       pushad

        mov eax &FALSE
        ..If D@Message = &WM_COMMAND
            If D@wParam = &IDHELP
                call Help, B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage
            End_if

            mov B$StringFound &TRUE
            mov eax D@wParam

            If ax = CodeListBox
                call DataAndCodeLabelListBoxNotification

            Else_If ax = DataListBox
                call DataAndCodeLabelListBoxNotification

            Else_If ax = FindDeclaration
                shr eax 16
                cmp ax &BN_CLICKED | jne L0>
                    call DesideSearchForUnusedSymbols
                L0:
            Else_If ax = &IDOK
                  call DesideSearchForUnusedSymbols

            Else_If ax = &IDCANCEL
                    mov B$UnusedSymbolsWanted &FALSE
                    mov D$UnusedSymbolsDialogWanted &FALSE
                    mov D$UnusedCodeAndDataDialogHandle 0
                    call 'USER32.EndDialog' D@Adressee, 0

            Else_If ax = UnUsedSymbolsEditBox

                call UnUsedSymbolsEditBoxNotification
            End_If

        ..Else_If D@Message = &WM_MOUSEMOVE

        ..Else_If D@Message = &WM_HELP
             call Help B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage

        ..Else_If D@Message = &WM_KEYDOWN
             call Help, B_U_AsmName, UnusedSymbolsHelp, ContextHlpMessage

        ..Else_If D@Message = &WM_INITDIALOG
            Call InstallSubClassCallbacks D@Adressee

            mov eax D@Adressee
            mov D$UnusedCodeAndDataDialogHandle eax
            call SetUnusedDialogPos D@Adressee
            mov ecx D$NumberOfUnusedCodeLabels | jecxz L9>
            mov eax D$CodeLabelNameList

L0:         push eax ecx
                push eax
                    While B$eax <> 0
                        cmp B$eax '@' | je L1>
                        inc eax
                    End_While
                pop eax
                push eax
                    call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle,
                                                      CodeListBox, &LB_ADDSTRING, 0, eax
L1:             pop eax
            pop ecx eax
            While B$eax <> 0 | inc eax | End_While
            inc eax | dec ecx | jnz L0<

L9:         mov ecx D$NumberOfUnusedDataLabels | jecxz L9>
            mov eax D$DataLabelNameList

L0:         push eax ecx
                push eax
                    While B$eax <> 0
                        cmp B$eax '@' | je L1>
                        inc eax
                    End_While
                pop eax
                push eax
                    call 'user32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle,
                                                      DataListbox, &LB_ADDSTRING, 0, eax
L1:             pop eax
            pop ecx eax
            While B$eax <> 0 | inc eax | End_While
            inc eax | dec ecx | jnz L0<

L9:         call 'user32.SetDlgItemTextA' D$UnusedCodeAndDataDialogHandle,
                                          UnUsedSymbolsEditBox, NoteForWorkerBee

            mov eax D$UnusedCodeIndex | or eax D$UnusedDataIndex
            On eax <> 0 call RestoreUnusedIndex

        ..Else
            popad | mov eax &FALSE | ExitP

        ..End_If

        popad | mov eax &TRUE
EndP
____________________________________________________________________________________________

[UnusedCodeIndex: ?   UnusedDataIndex: ?]

SaveUnusedIndex:
    call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, CODELISTBOX,
                                      &LB_GETTOPINDEX, 0, 0
    mov D$UnusedCodeIndex eax

    call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, DATALISTBOX,
                                      &LB_GETTOPINDEX, 0, 0
    mov D$UnusedDataIndex eax
ret

RestoreUnusedIndex:
    call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, CODELISTBOX,
                                      &LB_SETTOPINDEX, D$UnusedCodeIndex, 0

    call 'USER32.SendDlgItemMessageA' D$UnusedCodeAndDataDialogHandle, DATALISTBOX,
                                      &LB_SETTOPINDEX, D$UnusedDataIndex, 0
ret
____________________________________________________________________________________________

Proc SetUnusedDialogPos:
    Argument @Handle
    Structure @WINDOWPLACEMENT 44,
              WINDOWPLACEMENT.iLengthDis 0,
              WINDOWPLACEMENT.flagsDis 4,
              WINDOWPLACEMENT.showCmdDis 8,
              WINDOWPLACEMENT.ptMinPosition.xDis 12,
              WINDOWPLACEMENT.ptMinPosition.yDis 16,
              WINDOWPLACEMENT.ptMaxPosition.xDis 20,
              WINDOWPLACEMENT.ptMaxPosition.yDis 24,
              WINDOWPLACEMENT.rcNormalPosition.leftDis 28,
              WINDOWPLACEMENT.rcNormalPosition.topDis 32,
              WINDOWPLACEMENT.rcNormalPosition.rightDis 36,
              WINDOWPLACEMENT.rcNormalPosition.bottomDis 40

        mov D$WINDOWPLACEMENT.iLengthDis 44

        call 'USER32.GetWindowPlacement' D$UnusedCodeAndDataDialogHandle, D@WINDOWPLACEMENT

        call 'USER32.GetSystemMetrics' &SM_CXSCREEN | sub eax 5
        mov ecx D$WINDOWPLACEMENT.rcNormalPosition.rightDis
        sub ecx D$WINDOWPLACEMENT.rcNormalPosition.leftDis
        mov D$WINDOWPLACEMENT.rcNormalPosition.rightDis eax
        sub eax ecx | mov D$WINDOWPLACEMENT.rcNormalPosition.leftDis eax

        call 'USER32.GetSystemMetrics' &SM_CYSCREEN | sub eax 30
        mov ecx D$WINDOWPLACEMENT.rcNormalPosition.bottomDis
        sub ecx D$WINDOWPLACEMENT.rcNormalPosition.topDis
        mov D$WINDOWPLACEMENT.rcNormalPosition.bottomDis eax
        sub eax ecx | mov D$WINDOWPLACEMENT.rcNormalPosition.topDis eax

        call 'USER32.SetWindowPlacement' D$UnusedCodeAndDataDialogHandle, D@WINDOWPLACEMENT
EndP

____________________________________________________________________________________________

ReInitUnusedDialog:
    call SaveUnusedIndex
    call 'USER32.EndDialog' D$UnusedCodeAndDataDialogHandle, 0
    mov D$UnusedCodeAndDataDialogHandle 0
    mov B$UnusedSymbolsWanted &TRUE
    mov B$RecompileWanted &TRUE
    mov B$ShowStats &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
