TITLE Tree
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
 Tree view
 _______________________________________________________________________________________
 _______________________________________________________________________________________

 Building the tree view:

 here we build a simple list of labels encounted in source: label declarations 'Label:'
 and label evocations ('call label'). A byte flag is used for declarations and for
 evocations. A Declaration record is:         Flag (1) / DoneFlag (0/1) / adr / name
                                              ..(Byte)....(Byte)........(dWord)(Bytes)
             An evocation record is:          Flag (2) / name
                                              ..(Byte)..(Bytes)
 Names Strings are NOT zero ended (No need as following byte is either 1 or 2).

 We store everything we find in read order. Local label are not considered; so that
 each main label is followed by its own labels evocations, without any check of
 'ret' statement (that would be too uncertain...).

 No error check here for open text / brackets. We pop the return adress and return
 to caller of 'CreateTreeViewList'.
;;

[TreeList: ?  TreeListPtr: ?  TreeListEnd: ?]

[DeclarationFlag 1  EvocationFlag 2  ListFlagMax 3]

BuildLabelTreeList:
    mov eax D$SourceEnd | add eax 400 | mov D$eax-4 0A0D0A0D
    mov D$TreeList eax, D$TreeListPtr eax
    mov esi D$CodeSource, edx D$SourceEnd

L0: lodsb | cmp esi edx | jae L9>>

    cmp al ';' | jne L1>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            Do
                inc esi | cmp esi edx | jae L9>>
            Loop_Until D$esi = MLC
            add esi 3 | jmp L0<
        Else
L2:         lodsb | cmp al LF | jne L2<
            jmp L0<
        End_If

L1:   cmp al "'" | jne L1>                   ; jump over 'text'
L2:     lodsb | cmp al CR | je L3>
        cmp al "'" | jne L2<
          jmp L0<
L3:   jmp AbortTree

L1:   cmp al '"' | jne L1>                   ; jump over "text"
L2:     lodsb | cmp al '"' | je L0<
          cmp esi edx | jb L2<
      jmp AbortTree

; __________________________    Inside [brackets]: ________________________________

L1:   cmp al '[' | jne L1>                   ; jump over [Brackets]

L2:     lodsb | cmp al ';' | jne L4>

        If D$esi-2 = MLC   ; (LF ; ; CR)
            Do
                inc esi | cmp esi edx | jae L9>>
            Loop_Until D$esi = MLC
            add esi 3 | cmp esi edx | jae L9>>
        Else
L3:         lodsb | cmp al LF | jne L3<
        End_If
        jmp L2<

L4:     cmp al "'" | jne L4>
L5:       lodsb | cmp al "'" | je L2<        ; 'text' in brackets
          cmp al CR | jne L5<
            jmp L8>

L4:     cmp al '"' | jne L4>
L5:       lodsb | cmp al '"' | je L2<        ; "text" in brackets
          cmp esi edx | jb L5<
            jmp L8>

L4:     cmp al ']' | je L0<<
        cmp esi edx | jb L2<
L8:   jmp AbortTree

L1: cmp al ':' | jne L1>                              ; Label declaration found ?
        cmp B$esi-4 32  | jbe L0<<                         ; local ?
        cmp B$esi-4 '|' | je L0<<

      ; End of Label:
        mov ecx esi | dec ecx

      ; Case of Exported Functions.
        On B$esi = ':', inc esi

        push esi
L2:       dec esi | cmp B$esi 32 | ja L2<
            inc esi | mov edi D$TreeListPtr
            mov al DeclarationFlag | stosb            ; declaration flag
            mov al 0 | stosb                          ; done flag
            mov eax esi | stosd                       ; adress in source
            sub ecx esi | rep movsb                   ; label name
            mov D$edi 0
            mov D$TreeListPtr edi
        pop esi
      jmp L0<<

L1:   or al 32 | cmp al 'a' | je IsItDialogApi        ; >>> down here
      cmp al 'c' | jne L0<<                           ; Label evocation found ?
      cmp B$esi-2 32 | jbe L2>
        cmp B$esi-2 '|' | jne L0<<
L2:   lodsd | or eax 020202020 | cmp eax 'all ' | je L3>
        sub esi 4 | jmp L0<<
L3:   cmp B$esi 32 | jne L4>                          ; strip double spaces
        inc esi | jmp L3<
L4:   If B$esi = "'"                                  ; direct api call (without 'api' macro)
        mov ebp esi | dec ebp | inc esi | jmp L3>>
      End_If

L8:           mov edi D$TreeListPtr                   ; jmp target of successfull 'IsItDialogApi'
                mov al EvocationFlag | stosb          ; evocation flag
L5:             lodsb | cmp al '0' | jb L6>
                        cmp al 'z' | ja L6>
                stosb | jmp L5<                       ; label name
L6:           mov D$edi 0
              mov D$TreeListPtr edi | dec esi
        jmp L0<<

L9: mov edi D$TreeListPtr
    mov al DeclarationFlag | stosb         ; security if 'Callback:' is last main label
    mov al 0 | stosb
    move D$TreeListEnd D$TreeListPtr
ret


IsItDialogApi:
    cmp B$esi-2 32 | jbe L2>
        cmp B$esi-2 '|' | jne L0<<
L2: mov ebp esi
    lodsd | or eax 02020 | cmp eax "pi '" | je L3>
L7:     mov esi ebp | jmp L0<<
L3: add esi 7 | lodsd | cmp eax 'Crea' | jne L4>      ; esi+7 > jumps over 'User32.'
      lodsd | cmp eax 'teDi' | jne L7<
        lodsd | cmp eax 'alog' | jne L7<
          jmp L5>
L4: cmp eax 'Dial' | jne L7<
      lodsd | cmp eax 'ogBo' | jne L7<

L5: ; We found either call 'CreateDialog/Indirect/paramA' or 'DialogBox/Indirect/ParamA'.
    ; the pointed Proc is always the fourth following parameter:

    mov ecx 4

P0: lodsb | cmp al ' ' | je P1>                      ; search a separator
            cmp al ',' | jne P0<

P1: lodsb | cmp al ' ' | jbe P1<                     ; search start of parameter
            cmp al ';' | jne P4>                     ; jmp over comments
P2:            lodsb | cmp al LF | jne P2<
                   jmp P1<
P4: loop P0<

L6: dec esi | jmp L8<<


; If Building tree fails, we abort: strip previous record in BackTable, pop the return
; adress of caller and return to Callback.

[TreeAborted: 0]
[AbortTreeMessage: "Unpaired Text delimiter or unpaired Bracket", 0]

AbortTree:
    mov ebx D$BackTablePtr | sub bl 4 | mov D$ebx 0 | mov D$BackTablePtr ebx
    call 'USER32.MessageBoxA' D$hwnd, AbortTreeMessage, Argh, &MB_SYSTEMMODAL
    mov B$TreeAborted &TRUE
ret


;;
 Now the list of labels declarations and of labels evocation (by call) is done. Let us
 take a short exemple: 'CallBack:' contains three calls (to SubA1/A2/A3); SubA1 contains
 two calls (to SubB1/B2); SubB1 again two calls (SubC1/C2); and at last, SubC2 has one
 call (to SubD1). The tree we want to print could be:

 > CallBack
 >      SubA1
 >          SubB1
 >              SubC1
 >              SubC2
 >                  SubD1
 >          SubB2
 >      SubA2
 >      SubA3

 The List could be (1 is DeclarationFlag, 2 EvocationFlag, adr is Ptr in source):

 / 1 / adr / CallBack / 2 / SubA1 / 2 / SubA2 / 2 / SubA3 /
 / 1 / adr / SubA3 /
 / 1 / adr / SubA1 / 2 / SubB1 / 2 / SubB2 /
 / 1 / adr / SubA2
 / 1 / adr / SubB1 / 2 / SubC1 / 2 / SubC2 /
 / 1 / adr / SubC1 /
 / 1 / adr / SubC2 / 2 / SubD1 /
 / 1 / adr / SubD1 /
 / 1 / adr / SubB2 /
 / 1 ......

 We are going to get the tree from this list simply with playing with PUSH/POP: We first
 push 0, search for 'MainWindowProc' in the list, read/write; we push a pointer to next
 evocation, search actual label declaration, write it, and so on until we find a label
 with a declaration flag. In this case, we pop previous adress and go on. So, we go
 forth and back in the list until the entirely job is done; this is to say, until we
 pop 0. The 'base' of search (edi) is always an evocation and 'what we search' (esi)
 is always a declaration.

 this entire job is done each time user click on 'Tree'.
;;

[FirstLabelToSearch: ? #20] [TreeLabelPtr: ?  DoNotWriteInTree: B$ ?]

SearchListDeclaration:  ; edi(ah) > what to find (evoc.) // esi(al) search ptr (> declar.)

    mov edx D$TreeListEnd, esi D$TreeList, B$DoNotWriteInTree &FALSE
    push edi
L0:     lodsb | cmp esi edx | jae L8>
L1:     cmp al DeclarationFlag | ja L0<           ; jump over evocations
        mov ebx esi | inc esi                     ; ebx = done flag ptr. Jump over
        mov D$TreeLabelPtr esi | add esi 4 | pop edi | push edi
L4:     lodsb | mov ah B$edi | inc edi
        and eax 00_11011111_11011111              ; ah, al > Upper Case
        cmp eax 0202 | je L7>
        cmp eax 0101 | je L7>
        cmp al 0 | je L8>>
        cmp al ah | je L4<

        cmp al ListFlagMax | ja L0<
L6:     cmp ah ListFlagMax | ja L1<

L7:     If B$ShowLabelsOnce = &TRUE
            On B$ebx = 1, mov B$DoNotWriteInTree &TRUE
        End_If
        mov B$ebx &TRUE                           ; write done flag
        dec esi | mov ecx esi                     ; ecx = size
        mov esi D$TreeLabelPtr
        lodsd | mov ebx eax                       ; eax, ebx = declaration ptr in source
        sub ecx esi | jmp L9>

L8:     mov ecx 0
L9: pop edi
ret                     ; >>> ecx = 0 if not found,  esi = pointer to name if found


; For RightClick (direct search in tree View), the edge of the text must be defined.

[TreeViewItemEdge: ? ] [TreeViewItem: ? #100]

; when In, ecx = size of label name.

WriteTreeView:           ; Page set up of the tree like in Win32 TreeView Lists.
                         ; Here one Item once.
    push esi, edi, ecx
        mov edi TreeViewItem
L1:     cmp D$TreeIndent 0 | je L1>
L0:         push ecx
                mov ecx D$TreeIndent, ebx ecx
L0:             mov eax '    ' | stosd
                mov eax '|   ' | stosd
                loop L0<
            pop ecx
L1:     rep movsb                   ; ecx still good from 'SearchLabelInTreeList'

L2:     mov al ' ' | stosb | mov al 0 | stosb
        call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle, 100, &LB_ADDSTRING,
                                          0, TreeViewItem
    pop ecx, edi, esi
ret

;;
 In next routine, reintrant procedure would produce infinite loop; This may append, for
 exemple when a DialogBox routine Closes the Dialog and re-run the same fresh Dialog to
 re-init its values (This is the case in RosAsm Dialog Editor source).

 For such cases, we store all Source Adresses of declarations at each increment of tree
 writting position, and check if new written declaration adress is up there in the same
 tree branching. It yes, we pop back and go on.

 The checked value (Adress of Declaration in source) is given in eax by "SearchListDeclaration".
 but on "WriteTreeView" exit it is POPed in ebx, so that here, ebx is used instead.

 These adresses are stored in "TreeLevels" table which grows and decreases with indentations.

 What we push and pop here until poping 0 is nothing but a pointer to TreeList records.
 What we search in "SearchListDeclaration" is always the source adress of a declaration.
 When we see several following Labels at the same indent in Tree View window, we have
 both push (for "If Next") and pop (for "Next was a Declaration")... Difficult to explain
 difficult to understand, ... difficult to write... PUSHing is like going down in Tree
 branching and POPing is like going up. At each leave, we both push and pop.
;;

[TreeIndent: ?]
[TreeRoot: ? #120]

[TreeLevels: ? #30]

ClearTreeLevels: mov edi TreeLevels, eax 0, ecx 30 | rep stosd | ret

BuildOneTree:
    push 0                                           ; end search mark on stack first.
    mov D$TreeIndent 0, edi TreeRoot                 ; Root String set by caller.
    call SearchListDeclaration
    If ecx = 0                                       ; zero lenght (not found) > abort
        pop eax | ret
    End_If
    mov D$TreeLevels ebx

L0: call WriteTreeView | add esi ecx
                                                     ; Testing NEXT RECORD flag:
L2: .If B$esi = EvocationFlag                        ; ________Evocation Case______________
        inc esi | mov edi esi
L3:     lodsb | cmp al ListFlagMax | ja L3<          ; Search end of evocation Name
        dec esi | push esi                           ; push next possible evocation record Ptr
        inc D$TreeIndent                             ; > esi points to next record Type Flag
    .Else                                            ; ________Declaration Case____________
L4:     dec D$TreeIndent                             ; > No more evocation in that branch
        pop esi | cmp esi 0 | ja L2<                 ; pop previous TreeList pointer
            jmp L9>>
    .End_If

    call SearchListDeclaration | cmp ecx 0 | je L4<  ; Evocation of a non existing Label
    cmp B$DoNotWriteInTree &TRUE | je L4<            ; If "First Call Only" selected

    push edi, ecx                                    ; Compare new Source Label Adress with
        mov edi TreeLevels, ecx D$TreeIndent         ; other adresses of the same branch
        inc ecx | repne scasd                        ; to prevent from infine loop in case
    pop ecx, edi                                     ; of imbricated re-intrant calls.
    je L4<

    mov eax D$TreeIndent, D$TreeLevels+eax*4 ebx | jmp L0<< ; store source Label Adresses.
                                                            ; for next time upper control
L9: ret


[ContinueOrphans: 'List Orphans Labels?', 0
 ManyOrphans: "
   After analyzes, the resulting Tree is found poorly organised,   
   and the amount of Labels is uge.
   
   Listing all of the orphan Labels may take a very long time,
   because the tree Builder will try to recreate a sub-Tree from
   each orphan...
 
                                    Go on listing?", 0]

ListOrphanLabels:
    mov D$TreeIndent 0, edx D$TreeListEnd, esi D$TreeList, ecx 0, ebx 0

  ; Count how many Orphan Labels to be listed:
L1: lodsb | cmp esi edx | jae L2>
    cmp al DeclarationFlag | ja L1<                       ; jump over evocations
      lodsb | add esi 4                                   ; jump over adress
        inc ebx
        cmp al &TRUE | je L1<                             ; done flag ?
            inc ecx | jmp L1<

  ; The number of orphans Labels may be uge with Disassemblies:
L2: shr ecx 2
    If ecx > ebx
            call 'USER32.MessageBoxA' D$hwnd, ManyOrphans, ContinueOrphans,
                                      &MB_SYSTEMMODAL__&MB_YESNO
        On eax = &IDNO, ret
    End_If

    mov D$TreeIndent 0, edx D$TreeListEnd, esi D$TreeList

L1: lodsb | cmp esi edx | jae L9>
    cmp al DeclarationFlag | ja L1<                       ; jump over evocations
      lodsb | add esi 4                                   ; jump over adress
        cmp al &TRUE | je L1<                             ; done flag ?
        push esi                                          ; start of name
L4:       lodsb | cmp al ListFlagMax | ja L4<             ; search end of name
          mov ecx esi
        pop eax
          sub ecx eax                                     ; ecx = size
          dec esi
        push esi, edx                                     ; next record
          mov edi TreeRoot, D$TreeIndent 0, esi eax       ; esi > start of name
          rep movsb | mov al 0 | stosb
          call BuildOneTree
        pop edx, esi                                      ; next record
      jmp L1<
L9: ret


[ShowOrphan: &FALSE  ShowLabelsOnce: &TRUE]    ; keep dWords (evocated both as Bytes and dWords)

BuildTree:             ; build the tree from the List

    mov D$TreeIndent 0
    mov esi CallBackName, ecx D$CallBackNameLen, edi TreeRoot
    rep movsb | mov al 0 | stosb

    call BuildOneTree

    mov esi EntryPointLabel, edi TreeRoot, D$TreeIndent 0
    While B$esi <> 0 | movsb | End_While | movsb
    call BuildOneTree

    On B$ShowOrphan = &TRUE, call ListOrphanLabels
L9: ret

 _______________________________________________________________________________________
 _______________________________________________________________________________________

; tree view main routines:

; Tag Dialog 22000

CreateTreeViewList:
    If D$ShowTreeHandle = 0
        call 'USER32.CreateDialogParamA' D$hInstance, 22000, D$hWnd, ShowTree, &NULL
        On D$BookMarks > 0, call ReInsertBookMarks
    Else
        call 'USER32.SetForegroundWindow' D$ShowTreeHandle
        call 'USER32.ShowWindow' D$ShowTreeHandle, &SW_RESTORE
    End_If
ret


; Called when loading a new file:

CloseTree:
    If D$ShowTreeHandle > 0
        call 'User32.DestroyWindow' D$ShowTreeHandle
        mov D$ShowTreeHandle 0
    End_If
ret


[ShowTreeHandle: ?]
[ActualWindow: ActualWindowX: ?  ActualWindowY: ?  ActualWindowW: ?  ActualWindowH: ?]

[TreeWP:
 TreeWP.iLength: D$ Len
 TreeWP.flags: D$ 0
 TreeWP.showCmd: D$ 0
 TreeWP.ptMinPosition.x: D$ 0
 TreeWP.ptMinPosition.y: D$ 0
 TreeWP.ptMaxPosition.x: D$ 0
 TreeWP.ptMaxPosition.y: D$ 0
 TreeWP.rcNormalPosition.left: D$ 0
 TreeWP.rcNormalPosition.top: D$ 0
 TreeWP.rcNormalPosition.right: D$ 0
 TreeWP.rcNormalPosition.bottom: D$ 0]

[ListKeyBoardInput: ?    AutoHideTreeView: ?    AutoRebuildTreeView: ?]

Proc ShowTree:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ..If D@Message = &WM_COMMAND
        .If D@wParam = &IDCANCEL
L7:        mov D$ShowTreeHandle 0
           call 'User32.EndDialog' D@Adressee 0   ; CloseTree

        .Else
            shr D@wParam 16

            If D@wParam = &LBN_KILLFOCUS
                On B$AutoHideTreeView = &TRUE,
                    call 'USER32.ShowWindow' D@Adressee &SW_MINIMIZE
            Else_If D@wParam = &LBN_SELCHANGE
                On B$ListKeyBoardInput = &FALSE, call SearchFromTreeListBox D@lParam
                mov B$ListKeyBoardInput &FALSE
            End_If
        .End_If

    ..Else_If D@Message = &WM_VKEYTOITEM    ; Prevents from searching hundreads of
        If W@wParam <> CR                   ; Label when user is moving through the
            mov B$ListKeyBoardInput &TRUE   ; List with the KeyBorad.
        Else                                ; But allow Search if [Return] Key depressed.
            call SearchFromTreeListBox D@lParam
        End_If
        popad | mov eax 0-1 | jmp L9>>

    ..Else_If D@Message = &WM_INITDIALOG
      ; Does not survive in the Minimized state...:
      ; call 'USER32.SetWindowLongA' D@Adressee, &GWL_EXSTYLE, &WS_EX_TOOLWINDOW
        call RestoreRealSource
        mov D$TreeViewItemEdge '    '
        move D$ShowTreeHandle D@Adressee
        call StorePosInBackTable
        call 'USER32.SetClassLongA' D$ShowTreeHandle &GCL_HICON D$wc_hIcon
        call ClearTreeLevels
        mov B$TreeAborted &FALSE | push ebp | call BuildLabelTreeList | pop ebp
        cmp B$TreeAborted &TRUE | je L7<<
        call BuildTree
        call SetPartialEditionFromPos

    ..Else_If D@Message = &WM_SIZE
      ; This is for adjusting the List Control to the Dialog new Size:
        call 'USER32.GetClientRect' D@Adressee, ListEditRect
        mov eax D$ListERX | sub D$ListERW eax
        mov eax D$ListERY | sub D$ListERH eax
        call 'USER32.GetDlgItem' D@Adressee, 100
        call 'USER32.MoveWindow' eax D$ListERX D$ListERY D$listERW D$ListERH &TRUE

      ; Save the user's Tree Dialog Width:
        call 'USER32.GetWindowRect' D@Adressee, ListEditRect
        mov eax D$ListERW | sub eax D$ListERX | mov D$TreeWidth eax

L1:     push D$TreeWP.ptMinPosition.x, D$TreeWP.ptMinPosition.y, D$TreeWP.flags
            call 'USER32.GetWindowPlacement' D$ShowTreeHandle TreeWP
        pop D$TreeWP.flags, D$TreeWP.ptMinPosition.y, D$TreeWP.ptMinPosition.x

    ..Else_If D@Message = &WM_MOVE
        jmp L1<

    ..Else_If D@Message = &WM_CTLCOLORLISTBOX
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ..Else_If D@Message = &WM_ACTIVATE
       ; Wanted by... Anvar if i remember, but conflicts with any wish of moving
       ; the reduced Tree Window, with AutoHide Flag set on. So commented out:

       ; On B$AutoHideTreeView = &TRUE, call 'USER32.ShowWindow' D@Adressee &SW_NORMAL

    ..Else
        popad | mov eax &FALSE | jmp L9>

    ..End_If

    popad | mov eax &TRUE

L9: EndP


[TreeCurrentSel: ?    TreeUpperMainLabelSel: ?]

Proc SearchFromTreeListBox:
    Argument @lParam

    call RestoreRealSource

    call 'USER32.SendMessageA' D@lParam, &LB_GETCURSEL, 0, 0
    mov D$TreeCurrentSel eax, D$TreeUpperMainLabelSel eax

    call 'USER32.SendMessageA' D@lParam, &LB_GETTEXT, D$TreeUpperMainLabelSel, TreeViewItem
    mov esi TreeViewItem | On B$esi > ' ', jmp L2>
L1: lodsb | cmp B$esi ' ' | je L1<
        cmp B$esi '|' | je L1<

L2: If B$esi = '@'
L0:     dec D$TreeUpperMainLabelSel | jc L9>>
        call 'USER32.SendMessageA' D@lParam, &LB_GETTEXT, D$TreeUpperMainLabelSel,
                                   TreeViewItem
        mov esi TreeViewItem | On B$esi > ' ', jmp L2>
L1:     lodsb | cmp B$esi ' ' | je L1<
            cmp B$esi '|' | je L1<
L2:             On B$esi = '@', jmp L0<
                    call InternalRightClick

    Else
        call InternalRightClick | jmp L9>>

    End_If
;;
  If here, after having a selected Item saying "@Label", we have found out the upper
  MainLabel, which is now a selected Block.  We search downward for the wanted relative
  "@Label":
;;
    call 'USER32.SendMessageA' D@lParam, &LB_GETTEXT, D$TreeCurrentSel, TreeViewItem
        mov esi TreeViewItem | On B$esi > ' ', jmp L2>
L1:     lodsb | cmp B$esi ' ' | je L1<
            cmp B$esi '|' | je L1<

L2: push D$LenOfSearchedString
        mov edi SearchString, D$LenOfSearchedString 1
        While B$esi > ' '
            movsb | inc D$LenOfSearchedString
        End_While
        mov al ':' | stosb | mov al 0 | stosb

        push D$DownSearch, D$CaseSearch, D$WholeWordSearch, D$CurrentWritingPos
            mov B$DownSearch &TRUE, B$CaseSearch &FALSE, B$WholeWordSearch &TRUE
            move D$CurrentWritingPos D$BlockEndTextPtr
            push D$NextSearchPos
                move D$NextSearchPos D$CurrentWritingPos
                call StringSearch
            pop D$NextSearchPos
        pop D$CurrentWritingPos, D$CurrentWritingPos, D$CaseSearch, D$DownSearch
    pop D$LenOfSearchedString

L9: call SetPartialEditionFromPos
EndP



; Set the Tree Window at left of Main Window:

SetTreeDialogPos:
    call GetTreePlacement

    call 'USER32.SetWindowPlacement' D$ShowTreeHandle, TreeWP

    call 'USER32.GetWindowLongA' D$ShowTreeHandle, &GWL_STYLE | or eax &WS_VISIBLE
    call 'USER32.SetWindowLongA' D$ShowTreeHandle, &GWL_STYLE, eax
ret


[TreePlacementDone: ?    TreeWidth: ?
 TreeMinX: ?   TreeMinY: ?]

[EditWindowXforTree: ? EditWindowYforTree: ?
 EditWindowX2forTree: ? EditWindowY2forTree: ?]

GetTreePlacement:
    On B$TreePlacementDone = &TRUE, ret

    mov B$TreePlacementDone &TRUE

    call 'USER32.GetWindowRect' D$EditWindowHandle, EditWindowXforTree

  ; Default Width if the user never redefined it:
    On D$TreeWidth = 0, mov D$TreeWidth 250

    mov eax D$EditWindowX2forTree | mov D$TreeWP.rcNormalPosition.right eax
    sub eax D$TreeWidth | mov D$TreeWP.rcNormalPosition.left eax
    mov eax D$EditWindowYforTree | mov D$TreeWP.rcNormalPosition.top eax
    mov eax D$EditWindowY2forTree | mov D$TreeWP.rcNormalPosition.bottom eax

    mov D$TreeWP.flags &WPF_SETMINPOSITION
    mov eax D$TreeWP.rcNormalPosition.right | sub eax 160    ; founded Minimized Width.
    mov D$TreeWP.ptMinPosition.x eax
    mov eax D$TreeWP.rcNormalPosition.top
    move D$TreeWP.ptMinPosition.Y eax
    mov D$TreeWP.flags &WPF_SETMINPOSITION

    mov D$TreeWP.showCmd &SW_SHOWNORMAL
ret


____________________________________________________________________________________________

;;
  Simplified version of the &WM_INITDIALOG case in 'ShowTree' Proc. Called from
  'AsmMain' when 'AutoRebuildTreeView' is TRUE.
;;

TreeUpDate:
    call 'USER32.SendDlgItemMessageA' D$ShowTreeHandle, 100, &LB_RESETCONTENT, 0, 0

    mov D$TreeViewItemEdge '    ' | call ClearTreeLevels

    mov B$TreeAborted &FALSE | push ebp | call BuildLabelTreeList | pop ebp

    On B$TreeAborted = &FALSE, call BuildTree
L9: ret




