TITLE DCBP

;;
  "DCBP" stands for "Double-Click Break-Points"
  
   Left-Margin Double-Left-Click relative Routines:
  
  * User Double-Left-Click in the left Margin:
  
    'MarginAction', 'IsEaxInBpOnTable', 'BpMenu', 'InitBreakPointsTables'
  
  * User choice in the BP Float Menu:
  
    'SetBreakpoint', ('SortBpOnTable', 'DoStoreBP'),
    'DeleteBreakpoint', 'DeleteAllBreakpoints'
  
  * Routines call by the Sources Editor (factual Edition):
  
    'AdjustBpTable' ('AdjustDownwardPointers' // 'DeleteBpInBlock') are called by:
    'DoStoreInsert', 'DoStoreCharDelete', 'DoStoreBlockPaste',
    'WriteUndoBlockFile', 'ControlZ'
  
  * Routines called by the Source Editor output:
  
    'DrawOneBreakPoint'
  
  * Temporary example for Pointers translation at the attention of Ludwig:
  
    'GetcodeBreakPointPosFromSourcePointer'
  
  
  The two Tables, 'BpOnTable' and 'BpOffTable' are 512 dWords each (No security
  needed with one 01000 Bytes page).
  Each dword is a Pointer to the user Source: Not to the Code 'CodeListPtr'.
  
  The call to 'WriteBpOffTable' is actualy commented out for this Static version. 
  I suppose you will need it, for BP deleted by the user during the Debug session.
;;
____________________________________________________________________________________________

; Run by 'DoubleClick'.

[BreakPointLocation: ?]

MarginAction:
    mov B$ReadyToRun &FALSE

    On D$BreakPointsTables = 0, call InitBreakPointsTables
;;
  eax and ebx have been set to 'MousePosX' and 'MousePosY' by a previous call to
  'MouseTextPos' in 'DoubleClick. 'SearchTxtPtr' is going to return the Source
  Pointer from these Screen Coordinates.
;;
    call SearchTxtPtr | mov D$BreakPointLocation eax | dec D$CaretRow
  ; ('Dec' because 'SearchTxtPtr' forces Row 0 to 1, for usual actions)
  ; eax now the Pointer to Row 1 (Source Pointer form). Is it already recorded or not?

    .If B$Keys+&VK_SHIFT = &TRUE
        call DeleteAllBreakpoints

    .Else_If B$Keys+&VK_CONTROL = &TRUE
        call IsEaxInBpOnTable

        call BpMenu

;;
  The BreakPoints Float Menu may generate call's to:
  'SetBreakpoint', 'DeleteBreakpoint', 'DeleteAllBreakpoints'
;;
        inc D$CaretRow

    .Else
        call IsEaxInBpOnTable

        If B$InsideBpTable = &TRUE
            call DeleteBreakPoint | call DoStoreRemoveBP
        Else
            call SetBreakPoint | call DoStoreBP
        End_If

     .End_If

     mov B$Keys+&VK_SHIFT &FALSE, B$Keys+&VK_CONTROL &FALSE
ret


DoubleClickMarginAction:
    On D$BreakPointsTables = 0, call InitBreakPointsTables
;;
  eax and ebx have been set to 'MousePosX' and 'MousePosY' by a previous call to
  'MouseTextPos' in 'DoubleClick. 'SearchTxtPtr' is going to return the Source
  Pointer from these Screen Coordinates.
;;
    call SearchTxtPtr | mov D$BreakPointLocation eax | dec D$CaretRow
  ; ('Dec' because 'SearchTxtPtr' forces Row 0 to 1, for usual actions)
  ; eax now the Pointer to Row 1 (Source Pointer form). Is it already recorded or not?

;;
  The Left Button Double-Click generates: WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK,
  and WM_LBUTTONUP. So forth, the 'LeftButton' stuff has already (wrongly) been
  executed and we have to reverse its effect:
;;
    call IsEaxInBpOnTable

    If B$InsideBpTable = &TRUE
        call DeleteBreakPoint | call DoStoreRemoveBP
    Else
        call SetBreakPoint | call DoStoreBP
    End_If

    xor B$InsideBpTable &TRUE

    mov D$CaretRow 1

    call BpMenu
;;
  The BreakPoints Float Menu may generate call's to:
  'SetBreakpoint', 'DeleteBreakpoint', 'DeleteAllBreakpoints'
;;
ret


IsBreakPointHere:
    call RestoreRealSource
        call SearchTxtPtr
        While B$eax-1 <> LF | dec eax | End_While
        mov D$BreakPointLocation eax
        call IsEaxInBpOnTable
    call SetPartialEditionFromPos
  ; Returns 'InsideBpTable' &TRUE or &FALSE.
ret

____________________________________________________________________________________________

[FloatSetBp 5600    FloatDelBp 5601    FloatDelAllBp 5602]
[FloatSetBpString: B$ 'Set BreakPoint [F4]', 0 0 0 0 0
FloatDelBpString: 'Delete BreakPoint [F4]', 0 0 0 0 0
FloatDelAllBpString: 'Delete all BreakPoints [Shift]/[F4]', 0]

[FirstStrike: ?  BpMousePos: ? ? BpMouseX: ? BpMouseY: ?]

Proc KeyBoardProc:
    Arguments @nCode, @wParam, @lParam

        Inc D$FirstStrike | On D$FirstStrike < 2, jmp L9>>

        pushad
            ..If D@nCode = &HC_NOREMOVE
                .If D@wParam = &VK_F4
L1:                 call KillBpMenu

                    If B$InsideBpTable = &FALSE
                        call SetBreakPoint | call DoStoreBP
                    Else
                        call DeleteBreakPoint | call DoStoreRemoveBP
                    End_If

                    popad | mov eax &TRUE | ExitP ; Not forwarding

                .Else_If D@wParam = &VK_RETURN
                    call 'USER32.GetMenuState' D$FloatHandle, 0, &MF_BYPOSITION
                    test eax &MF_HILITE | jnz L1<

                .End_If
            ..End_If
        popad

L9:     mov eax &FALSE ; Forwarding
EndP


KillBpMenu:
  ; Forcing the Float Menu to close

  ; Simulate a Left-Click in the middle of the Window:
    push D$CaretRow, D$CaretLine
        call 'USER32.GetClientRect' D$EditWindowHandle, BpMousePos
        shr D$BpMouseX 1 | shr D$BpMouseY 1
        call 'USER32.ClientToScreen' D$EditWindowHandle, BpMouseX
        call BpClick
    pop D$CaretLine, D$CaretRow

  ; Restore the Caret initial Position:
    call FromCaretPosToScreen | mov D$BpMouseX eax, D$BpMouseY ebx
    call BpClick
ret


; Reurns: eax = X // ebx = Y

FromCaretPosToScreen:
    push D$EditWindowX, D$EditWindowY
        call 'USER32.GetClientRect' D$EditWindowHandle, EditWindowX
        call 'USER32.ClientToScreen' D$EditWindowHandle, EditWindowX
        mov eax D$CaretLine, ecx D$FontHeight | mul ecx | mov ebx eax
        mov eax D$CaretRow | mov ecx D$FontWidth | mul ecx
        add eax D$EditWindowX | add ebx D$EditWindowY
    pop D$EditWindowY, D$EditWindowX
ret


BpClick:
    call 'USER32.SetCursorPos' D$BpMouseX, D$BpMouseY
    call 'USER32.mouse_event' &MOUSEEVENTF_LEFTDOWN, D$BpMouseX, D$BpMouseY, 0, 0
    call 'USER32.mouse_event' &MOUSEEVENTF_LEFTUP, D$BpMouseX, D$BpMouseY, 0, 0
ret


[BpMenuPOINT: ? ?]

BpMenu:
  ; Build the Foating Menu:
    call 'USER32.CreatePopupMenu' | mov D$FloatHandle eax

    .If B$InsideBpTable = &FALSE
        call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, FloatSetBp, FloatSetBpString
        mov eax D$BpOnTable
        If D$eax <> 0
            call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, FloatDelAllBp,
                                      FloatDelAllBpString
        End_If

    .Else
        call 'USER32.AppendMenuA' D$FloatHandle &MF_STRING, FloatDelBp, FloatDelBpString
        mov eax D$BpOnTable
        If D$eax+4 <> 0
            call 'USER32.AppendMenuA' D$FloatHandle, &MF_STRING, FloatDelAllBp,
                                      FloatDelAllBpString
        End_If

    .End_If

    call 'USER32.HiliteMenuItem' D$EditWindowHandle, D$FloatHandle, 0,
                                 &MF_BYPOSITION__&MF_HILITE

  ; Create the KeyBoard Hook:
    mov D$FirstStrike 0
    call 'KERNEL32.GetCurrentThreadId'
    call 'USER32.SetWindowsHookExA' &WH_KEYBOARD, KeyBoardProc, &NULL, eax ; <<<<<<<<<<<
    mov D$hHook eax

  ; Menu Position:
    push D$CaretRow
        mov D$CaretRow 1 | call FromCaretPosToScreen
    pop D$CaretRow
    call 'USER32.TrackPopupMenu' D$FloatHandle, &TPM_LEFTALIGN , eax, ebx, 0,
                                D$EditWindowHandle, &NULL

    call 'USER32.UnhookWindowsHookEx' D$hHook

    call 'USER32.DestroyMenu' D$FloatHandle
ret
____________________________________________________________________________________________

[InsideBpTable: ?]

IsEaxInBpOnTable:
    mov ebx D$BpOnTable

    While D$ebx <> 0
        If D$ebx = eax
            mov B$InsideBpTable &TRUE | ret
        End_If
        add ebx 4
    End_While

    mov B$InsideBpTable &FALSE
ret
____________________________________________________________________________________________

; 01000 Bytes (0400 dwords) >>> 2*512 dWords

[BreakPointsTables: BpOnTable: ?    BpOffTable: ?]

; No need to ever clear, as, each time a new file is loaded, the Table is released
; by 'ReInitUndo', at the same time other features are reset.

InitBreakPointsTables:
    pushad
        VirtualAlloc BreakPointsTables 01000
        add eax 0800 | mov D$BpOffTable eax
    popad
ret

____________________________________________________________________________________________
;;
  Routines called on user choice in the BP Float Menu
  
  We reuse 'DebugDialogHandle' (set to 1 in case of Mouse BP without Debugger Runing
  and to zero if none), in order to forbid editing Sources with Mouse defined BP inside
;;

SetBreakpoint:
    call RestoreRealSource

; >>> UPDATED
    If D$BPAnteroom <> 0
        call AddBPToAnteroom D$BreakPointLocation &TRUE
    EndIf
; <<< UPDATED

    ;call DoStoreBP

    mov eax D$BreakPointLocation, edi D$BpOnTable

    While D$edi <> 0
        add edi 4
    End_While

  ; What we are storing is the real pos, in the real Source:

    stosd

    mov B$ReadyToRun &FALSE

    call SortBpOnTable

    call SetPartialEditionFromPos
ret


DeleteBreakpoint:
    call RestoreRealSource

; >>> UPDATED
    If D$BPAnteroom <> 0
        call AddBPToAnteroom D$BreakPointLocation &FALSE
    EndIf
; <<< UPDATED

    mov eax D$BreakPointLocation
    mov esi D$BpOnTable

    While D$esi <> eax
        add esi 4 | On D$esi = 0, ret
    End_While

    mov edi esi | add esi 4
    While D$esi <> 0 | movsd | End_While
    mov D$edi 0
    mov B$ReadyToRun &FALSE

    call SetPartialEditionFromPos
ret


DeleteAllBreakpoints:
    mov edi D$BpOnTable

    While D$edi <> 0
        mov eax D$edi
        call StoreUserAction ACTION_DELDCBP, eax, &NULL
        mov D$edi 0 | add edi 4
    End_While

    mov B$ReadyToRun &FALSE
ret


; For the Undo Feature:

DoStoreBP:
    call StoreUserAction ACTION_DCBP, D$BreakPointLocation, &NULL
ret

DoStoreRemoveBP:
    call StoreUserAction ACTION_DELDCBP, D$BreakPointLocation, &NULL
ret

;;
  The Undo is done without any need of some 'UnDoStoreBP' Routine, as this is done by:
  'AdjustBpTable' and 'AdjustDownwardPointers'.
;;


; This first one is called from 'SetBreakpoint' so that the Pointers are always sorted:

SortBpOnTable:
  ; Sort BpOnTable:
    mov esi D$BpOnTable, ecx 0
    While D$esi <> 0 | add esi 4 | add ecx 4 | End_While
    call BubbleSort D$BpOnTable, ecx

  ; Init the Pointer to 'BpOnTable' for insertions:
  ;  move D$BpOnTablePtr D$BpOnTable
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  If the user mofifies the Source with BreakPoints inside, we have to modify the
  Table Pointers accordingly.
  
  We make use of the Undo Table ('UndoMemory') Records to achieve this.
  
  This 'AdjustBpTable' Routine is called from:
  
  'DoStoreInsert', 'DoStoreCharDelete'
  'DoStoreBlockPaste', 'WriteUndoBlockFile'
  'ControlZ'
;;

; Called from 'StoreUserAction', 'ControlZ, 'ControlShiftZ'.

Proc AdjustBpTable:
    Argument @Type

    pushad
        mov esi D$UndoPtr, eax D@Type

        If eax = ACTION_INSERT
            mov eax D$esi+RECORD_CURRENTWRITINGPOS
;;
  The calls to 'DoStoreInsert' are dones after the Insertion. So, we have to
  substract the Number of inserted Chars:
;;
            sub eax D$esi+RECORD_PARAM1

        Else_If eax = ACTION_BLOCKCOPY
            mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ACTION_BLOCKDELETE
            mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ACTION_DEL
            mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ControlZ
            mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else_If eax = ControlShiftZ
            mov eax D$esi+RECORD_CURRENTWRITINGPOS

        Else ; ACTION_OVERWRITE, ACTION_DCBP, ACTION_DELDCBP
            popad | ExitP

        End_If

        If D$ActualTitle <> 0
            add eax D$ActualTitle
        Else
            add eax D$CodeSource
        End_If

        mov edi D$BpOnTable

        While D$edi <> 0
            If eax < D$edi
                call AdjustDownwardPointers, D@Type | popad | ExitP
            End_If
            add edi 4
        End_While
    popad
EndP


Proc AdjustDownwardPointers:
    Argument @Type
      ; esi >>> last record in the UndoTable
      ; edi >>> first 'BpOnTable' Pointer coming downward from the 'CurrentWritingPos'
      ;
      ; Displacement Table are the ones from 'UndoPtr'

        mov eax D$esi+RECORD_FLAG, ecx 0

        .If eax = ACTION_INSERT
          ;' n Chars':
            mov ecx D$esi+RECORD_PARAM1

        .Else_If eax = ACTION_DEL
            call DeleteBpOnDelCRLF
            mov ecx 0-1

        .Else_If eax = ACTION_BLOCKDELETE
            call DeleteBpInBlock
           ; Selected Block length:
            mov ecx D$esi+RECORD_PARAM2 | sub ecx D$esi+RECORD_PARAM1 | inc ecx
            neg ecx

        .Else_If eax = ACTION_BLOCKCOPY
          ; 'ClipBoardLen' used in ControlV
            mov ecx D$esi+RECORD_PARAM2 | sub ecx D$esi+RECORD_PARAM1 | inc ecx
;;
        .Else_If eax = ACTION_DCBP
          ;; CurrentWritingPos:
          ;  mov eax D$esi+8
          ;  If D$TitleTable = 0
          ;      add eax D$CodeSource
          ;  Else
          ;      add eax D$ActualTitle
          ;  End_If
          ;  mov D$BreakPointLocation eax
          ;  call DeleteBreakpoint | ExitP
          
          ExitP
          
        .Else_If eax = ACTION_DELDCBP
            ExitP
;;
        .Else
            ExitP

        .End_If

      ; ControlZ cases:
        If D@Type = ControlZ
            neg ecx
            mov eax D$CurrentWritingPos | On B$eax-1 = LF, dec ecx
        End_If

        While D$edi <> 0
            add D$edi ecx | add edi 4
        End_While
EndP


DeleteBpInBlock:
    pushad
        mov ebx D$BlockStartTextPtr, edx D$BlockEndTextPtr | inc edx
;;
  The user is deleting a Selected Block with BreakPoints inside. Remove these BPs.
  
  The Selection Values are the real Edition ones. If TITLEs inside, we have to switch
  to the real Source. If no TITLE inside, the Values are the real Source ones and we
  have nothing to do, even though this Routine is called from:
  
  'StoreUserAction' >>> 'AdjustBpTable'  >>> 'AdjustDownwardPointers'
;;
        If D$TitleTable <> 0
            sub ebx D$CodeSource | add ebx D$ActualTitle
            sub edx D$CodeSource | add edx D$ActualTitle
        End_If

        mov esi D$BpOnTable

        .While D$esi <> 0
            lodsd
            .If eax >= ebx
                If eax <= edx
                    push esi
                      ; Direct Delete of concerned BP:
                        mov edi esi | sub edi 4
                        While D$esi <> 0 | movsd | End_While
                        mov D$edi 0
                    pop esi
                End_If
            .End_If
        .End_While
    popad
ret
____________________________________________________________________________________________

; If the user [Del]s a CRLF followed by a DCBP, we remove it:

DeleteBpOnDelCRLF:
    pushad
        mov ebx D$esi+RECORD_CURRENTWRITINGPOS

        If D$TitleTable <> 0
            add ebx D$ActualTitle
        Else
            add ebx D$CodeSource
        End_If

        inc ebx

        mov esi D$BpOnTable

        .While D$esi <> 0
            lodsd
            If eax = ebx
              ; Direct Delete of concerned BP:
                mov edi esi | sub edi 4
                While D$esi <> 0 | movsd | End_While
                mov D$edi 0 | jmp L9>
            End_If
        .End_While
L9: popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; These are the Routines called by the Source Editor (at the end of 'TextOutput')

[RedPlotSize: ?   RedPlotX: ?]

DrawTheRedPlots:
    mov eax D$FontHeight | shl eax 1 | add eax D$FontWidth | shr eax 2
    mov D$RedPlotSize eax
    mov eax D$BpMarginWidth | sub eax D$RedPlotSize | shr eax 1
    mov D$RedPlotX eax

    call 'User32.BeginPaint' D$BpWindowHandle, PAINTSTRUCT | mov D$hdc eax

    call 'GDI32.SelectObject' D$hdc D$RedBrushHandle
    call 'GDI32.SelectObject'  D$hdc D$Font1Handle | mov D$hfont eax
    call 'GDI32.SetBkColor' D$hdc D$NormalBackColor

    mov ecx D$LineNumber, D$Line 0 | inc ecx | inc ecx

L0: push ecx
        call 'GDI32.TextOutA' D$hdc, 0, D$Line, BlankLine, 2
    pop ecx
    mov eax D$FontHeight | add D$Line eax | dec ecx | cmp ecx 0 | jne L0<

    mov esi D$UpperLine, D$Line 0

    While esi < D$LastCharPosOnScreen
        If B$esi-1 = LF
            push esi
                call DrawIfEsiInBreakPointsTable
            pop esi
        End_If
        inc esi
        If B$esi = LF
            mov eax D$FontHeight | add D$line eax
        End_If
    End_While

    call DrawTheBpLine

    call 'USER32.EndPaint' D$BpWindowHandle, PAINTSTRUCT
ret


BlankMargin:
    call 'User32.BeginPaint' D$BpWindowHandle, PAINTSTRUCT | mov D$hdc eax

    call 'GDI32.SetBkColor' D$hdc D$NormalBackColor
    call 'GDI32.SelectObject' D$hdc D$Font1Handle

    mov ecx D$LineNumber, D$Line 0 | inc ecx | inc ecx

L0: push ecx
        call 'GDI32.TextOutA' D$hdc, 0, D$Line, BlankLine, 2
    pop ecx
    mov eax D$FontHeight | add D$Line eax | dec ecx | cmp ecx 0 | jne L0<

    call DrawTheBpLine

    call 'USER32.EndPaint' D$BpWindowHandle, PAINTSTRUCT
ret


DrawIfEsiInBreakPointsTable:
    mov ebx D$BpOnTable

    .If B$RealSourceRestored = &FALSE
        If D$ActualTitle <> 0
            sub esi D$CodeSource | add esi D$ActualTitle
        End_If
    .End_If

    .While D$ebx <> 0
        If D$ebx = esi
            push esi, ebx
              ; Draw one Break-Point:
                mov eax D$RedPlotX
                mov ebx D$Line
                mov ecx eax | add ecx D$RedPlotSize
                mov edx ebx | add edx D$RedPlotSize
                call 'GDI32.Ellipse' D$hdc, eax, ebx, ecx, edx
            pop ebx, esi
        End_If

        add ebx 4

    .End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  This is a fully _un-tested_ ;) example about the way to switch from a dword found in
  the BP Tables -in form of Pointer to User Source- to a dword pointing to the Debuggee
  App Address space Code.
;;

; UPDATED >>>

Proc GetcodeBreakPointPosFromSourcePointer:
    Argument @SourcePtr

        mov eax D@SourcePtr, esi D$StatementsTable, edx D$StatementsPtr
        mov ebx D@SourcePtr

      ; 'edx' (D$StatementsPtr) should still point to the last record of 'StatementsTable'
      ; it doesn't :?

        While D$esi <> 0
            On D$esi >= eax, jmp L1>
;;
  Why '>=', up there? This is because the registred Sources Pointers, in the BP Tables,
  may point, for example, to leading spaces, and such (cases of Indents, Blank lines,
  Square Brackets Declarations,...).
;;
            add esi 4
        End_While

;;
  At 'L1:', edi should point to the Record of the searched Location in the 
  'StatementsTable' dwords Pointers Table. Now, get the Matching Code one:
;;
L1:     sub esi D$StatementsTable | add esi D$IpTable | lodsd
;;
  'eax' should now be a Pointer to Code as viewed by the Encoder. There is an
  adjustement variable called 'DebugBaseOfCode' that should enable with the true
  location inside the Debuggee App real Address space. (It is computed by 'SetCodeRVA')
  To be verified by experiment, but i think you should now say:
;;
        add eax D$DebugBaseOfCode

      ; 'eax' should ready as Return value...
EndP

; <<<
____________________________________________________________________________________________
____________________________________________________________________________________________




