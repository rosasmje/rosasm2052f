TITLE Undo

________________________________________________________________________________________
________________________________________________________________________________________
;;
  Undo feature; 2046 events are stored in UndoTable:
  
  Note: 'DoStoreBP' is in the DCBP TITLE and, as the 'UnDoStoreBP' job is done by
  'AdjustBpTable' and 'AdjustDownwardPointers', there is no 'UnDoStoreBP' Routine.
  
  ____________________
  Undo Table Routines:
  
  'InitUndo', 'KillUndo', 'ReInitUndo', 'ReadUndoRecord'
  
  ______________
  "DoStore" Routines:
  
  'DoStoreInsert', 'DoStoreOverWrite', 'DoStoreCharDelete'
  'DoStoreBlockPaste' (called from 'ControlV', 'AddWizardCode' -???!!!...-)
  'WriteUndoBlockFile' (caled from 'ControlX')
  
  'DoStoreBP' (in DCBP TITLE)
  
  ____________________
  Main "UnDo" Routine: 'ControlZ', calling for:
  
  'ReInsertDeletedChar', 'UndoBlockCopy', 'ReadUndoBlockFile'
  
  __________________________
  Files Managements Routines (for Blocks):
  
  'ResetUndoFileName', 'IncUndoFileName', 'DecUndoFileName', 'DeleteUndoFiles'
  'DeleteOldUndoFiles'
  _____________
  ReDo Routines:
  
  'ControlShiftZ' >>> 'ReInsertSource'

;;
________________________________________________________________________________________
________________________________________________________________________________________

[UndoPtr: 0    UndoMemory: 0] ; BuildTitleTable
;;
 The Undo Table is a rotary table 010000 sized for 2048 (0800h) records of 8 dWords:
;;

; Undo Flags:

[ACTION_INSERT 1, ACTION_OVERWRITE 2, ACTION_BLOCKCOPY 3, ACTION_BLOCKDELETE 4
 ACTION_DCBP 5, ACTION_DELDCBP 6, ACTION_DEL 7]

;[UNO_N_CHARS ecx  UNDOCHAR ebx  UNDOBLOCKBEGIN ebx  UNDOBLOCKEND edx  UNDOFLAG eax]

; Displacements Equates for pointing into one 'UndoMemory' Record:
; 0FFFF
[RECORD_CARET_ROW 0
 RECORD_CARET_LINE 4
 RECORD_CURRENTWRITINGPOS 8
 RECORD_ACTUALTITLE 12
 RECORD_UPPERLINE 16

 RECORD_PARAM1 20
 RECORD_PARAM2 24

 RECORD_FLAG 28

 UNDO_RECORD 32]
____________________________________________________________________________________________
____________________________________________________________________________________________

; The Undo Buffer Managements:

InitUndo:
    VirtualAlloc UndoMemory 010000 | move D$UndoPtr D$UndoMemory
ret


KillUndo:
   VirtualFree D$UndoMemory
   mov D$PreviousPartIndex 0-1
ret


ReInitUndo:
    On B$CompletionWanted = &TRUE, VirtualFree D$CompletionTable, D$CompletionPointers
    call ClearF11F12
    mov D$TiTleTable 0, D$ActualTitle 0
ReInitUndoOnly:
    call DeleteUndoFiles
    call KillUndo
    call InitUndo
    call CloseTree
    VirtualFree D$BreakPointsTables
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; The centralized Storing Routines

Proc StoreUserAction:
    Argument @Flag, @Param1, @Param2
    Uses eax, ecx, edi

        mov edi D$UndoPtr
    ; action_insert

        move D$edi+RECORD_CARET_ROW D$CaretRow
        move D$edi+RECORD_CARET_LINE D$CaretLine
        mov eax D$CurrentWritingPos | sub eax D$CodeSource
        mov D$edi+RECORD_CURRENTWRITINGPOS eax
        move D$edi+RECORD_ACTUALTITLE D$ActualTitle
        mov eax D$UpperLine | sub eax D$CodeSource | move D$edi+RECORD_UPPERLINE eax

        move D$edi+RECORD_PARAM1 D@Param1
        move D$edi+RECORD_PARAM2 D@Param2

        move D$edi+RECORD_FLAG D@Flag

        On D$BpOnTable <> 0, call AdjustBpTable D@Flag

      ; Prepare the Undo Table Pointer for next registration:
        mov edi D$UndoPtr | add di 32 | mov D$UndoPtr edi
      ; Clear the next Record:
        mov eax 0, ecx 8 | rep stosd
EndP


DoStoreInsert:
  ; The Callers set the Number of Chars into ebx. (Usefull for Tab Spaces):
    call StoreUserAction ACTION_INSERT, ebx, D$InsertedChar
ret


DoStoreOverWrite:
  ; The Caller sets the overwritten Char into cl, and the new Char in al:
    and ecx 0FF | and eax 0FF
    call StoreUserAction ACTION_OVERWRITE, ecx, eax
ret


DoStoreCharDelete:
  ; ebx set to BACK or FORTH by callers: 'BackSpace' 'StripOneChar'
    mov esi D$CurrentWritingPos | movzx eax B$esi-1
  ; eax is the deleted Char:
    call StoreUserAction ACTION_DEL, eax, ebx
ret


DoStoreBlockPaste:
    pushad
        mov ebx D$CurrentWritingPos                       ; block start
        mov edx ebx | add edx D$ClipBoardLen | dec edx    ; block end

        sub ebx D$CodeSource | sub edx D$CodeSource
        call StoreUserAction ACTION_BLOCKCOPY, ebx, edx

        call WriteUndoBlockFileFromClipBoard
    popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; The Undo Files Managements. (They are only used for the Blocks Selections Operations).

ResetUndoFileName:
    mov edi D$PointerToUndoNumber, al '0' | stosb | stosb | stosb
ret


IncUndoFileName:
    mov eax D$PointerToUndoNumber
    inc B$eax+2 | cmp B$eax+2 '9' | jbe L9>
    mov B$eax+2 '0'
    inc B$eax+1 | cmp B$eax+1 '9' | jbe L9>
    mov B$eax+1 '0'
    inc B$eax
L9: ;Showme D$PointerToUndoNumber

    ret

DecUndoFileName:
    ;call 'KERNEL32.DeleteFileA' UndoFile ; Keep it, now, for Redo!!!!
    mov eax D$PointerToUndoNumber
    dec B$eax+2 | cmp B$eax+2 '0' | jae L9>
    mov B$eax+2 '9'
    dec B$eax+1 | cmp B$eax+1 '0' | jae L9>
    mov B$eax+1 '9'
    dec B$eax
L9: ret


[UndoFileHandle: ?]

WriteUndoBlockFileFromBlock: ; ControlV ControlX
L0: call IncUndoFileName | mov eax UndoFile
    call 'KERNEL32.CreateFileA' eax, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, 0
    mov D$UndoFileHandle eax

    If eax = &INVALID_HANDLE_VALUE
        call KillUndo | call InitUndo
        call SetUndoDirectory | jmp L0<
    End_If

    mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr | inc ecx

    call 'KERNEL32.WriteFile' D$UndoFileHandle, D$BlockStartTextPtr, ecx, NumberOfReadBytes, 0
    call 'KERNEL32.CloseHandle' D$UndoFileHandle
ret


WriteUndoBlockFileFromClipBoard: ; ControlV ControlX
L0: call IncUndoFileName | mov eax UndoFile
    call 'KERNEL32.CreateFileA' eax, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, 0
    mov D$UndoFileHandle eax

    If eax = &INVALID_HANDLE_VALUE
        call KillUndo | call InitUndo
        call SetUndoDirectory | jmp L0<
    End_If
    call 'KERNEL32.WriteFile' D$UndoFileHandle, D$ClipBoardPtr, D$ClipBoardLen,
                              NumberOfReadBytes, 0

    call 'KERNEL32.CloseHandle' D$UndoFileHandle
ret


[UndoBlockLen: ?]
;;
  Called from 'ControlZ'. A Selection has been Delete, and save into an Undo File. 
  we re-paste this Selection:
;;
ReadUndoBlockFile: ; ControlX
    mov D$BlockStartTextPtr ebx, D$BlockEndTextPtr ecx

    call 'KERNEL32.CreateFileA' UndoFile, &GENERIC_READ, 0, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    .If eax <> &INVALID_HANDLE_VALUE
        mov D$UndoFileHandle eax, D$NumberOfReadBytes 0

        call 'KERNEL32.GetFileSize' eax, 0 | mov D$UndoBlockLen eax
        add D$SourceLen eax | add D$SourceEnd eax

        mov esi D$SourceEnd | add esi 400 | mov edi esi | add edi eax
        mov ecx edi | sub ecx D$CurrentWritingPos | std | rep movsb | cld
        call 'KERNEL32.ReadFile' D$UndoFileHandle, D$CurrentWritingPos,
                                 D$UndoBlockLen, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$UndoFileHandle
    .End_If
ret


StoreUserActionOfBlockDeletion:
    push D$CurrentWritingPos
        move D$CurrentWritingPos D$BlockStarttextPtr
        mov eax D$BlockStarttextPtr | sub eax D$CodeSource
        mov ebx D$BlockEndTextPtr | sub ebx D$CodeSource

        call StoreUserAction ACTION_BLOCKDELETE, eax, ebx
    pop D$CurrentWritingPos
ret


DeleteUndoFiles:
    mov eax D$PointerToUndoNumber | cmp D$eax '000.' | je L9>
L1:     call 'KERNEL32.DeleteFileA' UndoFile
            call DecUndoFileName
            mov eax D$PointerToUndoNumber | cmp D$eax '000.' | ja L1<
L9: ret


; Undo Files WIN32_FIND_DATA Structure:

[DelUndo:
 DelUndo.dwFileAttributes: D$ ?
 DelUndo.ftCreationTime.dwLowDateTime: D$ ?
 DelUndo.ftCreationTime.dwHighDateTime: D$ ?
 DelUndo.ftLastAccessTime.dwLowDateTime: D$ ?
 DelUndo.ftLastAccessTime.dwHighDateTime: D$ ?
 DelUndo.ftLastWriteTime.dwLowDateTime: D$ ?
 DelUndo.ftLastWriteTime.dwHighDateTime: D$ ?
 DelUndo.nFileSizeHigh: D$ ?
 DelUndo.nFileSizeLow: D$ ?
 DelUndo.dwReserved0: D$ ?
 DelUndo.dwReserved1: D$ ?]
[DelUndo.cFileName: B$ ? #260]
[DelUndo.cAlternate: B$ ? #14]

[DelUndoHandle: ?]

DeleteOldUndoFiles:
    call 'KERNEL32.FindFirstFileA' AllUndoFiles2 DelUndo

    If eax <> &INVALID_HANDLE_VALUE
        mov D$DelUndoHandle eax
L0:     call 'KERNEL32.DeleteFileA' DelUndo.cFileName
        call 'KERNEL32.FindNextFileA' D$DelUndoHandle DelUndo
        cmp eax &TRUE | je L0<

        call 'KERNEL32.FindClose' D$DelUndoHandle
    End_If

    mov edi D$PointerToUndoNumber, eax '000.' | stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Main undo routine:

[ControlZreRunForCRLF: 0]
[BackOrForth: ?]

ControlZ:
    mov B$BlockInside &FALSE

    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If

    mov ebx D$UndoPtr | sub bx UNDO_RECORD
    cmp D$ebx+RECORD_FLAG 0 | je L9>>   ; depend on flag > 0 > End
        mov D$UndoPtr ebx

    If D$BpOnTable <> 0
        pushad
            call AdjustBpTable, ControlZ
        popad
    End_If

    call ReadUndoRecord

  ; >>> eax = RECORD_FLAG, ebx = RECORD_PARAM1, ecx = RECORD_PARAM2

    .If eax = ACTION_INSERT
        mov ecx ebx, ebx D$CurrentWritingPos
        if B$ebx-1 = LF
            call StripBackSpace
            mov ebx D$UndoPtr | sub bx 32 | mov D$UndoPtr ebx
            call StripBackSpace
            dec D$CurrentWritingPos, D$CaretRow
        Else
L1:         push ecx
                call StripBackSpace
            pop ecx
            dec D$CaretRow
            loop L1<
        End_If

    .Else_If eax = ACTION_OVERWRITE
        dec D$CaretRow | mov edi D$CurrentWritingPos, eax ebx | stosb

    .Else_If eax = ACTION_DEL
; Why this complicated stuff around 'ControlZreRunForCRLF': We may have to undo a CR/LF
; arasement done either by [BackSpace] or by [Del]. With [backSpace], we have first arased
; the LF and then the CR. With [Del], we have arased in the other way.
        mov D$BackOrForth ecx
        If ebx = CR
            dec D$CurrentWritingPos | xor B$ControlZreRunForCRLF &TRUE
            call ReInsertDeletedChar
            On B$ControlZreRunForCRLF = &TRUE, jmp ControlZ

        Else_If ebx = LF
            dec D$CurrentWritingPos | xor B$ControlZreRunForCRLF &TRUE
            call ReInsertDeletedChar
            On B$ControlZreRunForCRLF = &TRUE, jmp ControlZ
        Else
            dec D$CurrentWritingPos, D$CaretRow | call ReInsertDeletedChar
        End_If

        On D$BackOrForth = FORTH, dec D$CurrentWritingPos, D$CaretRow

    .Else_If eax = ACTION_BLOCKDELETE
        add ebx D$CodeSource | add ecx D$CodeSource
        call ReadUndoBlockFile | call DecUndoFileName

    .Else_If eax = ACTION_BLOCKCOPY
      ; 'DoStoreBlockPaste'
        add ebx D$CodeSource | add ecx D$CodeSource
        call UndoBlockCopy | call DecUndoFileName

    .Else_If eax = ACTION_DCBP
      ; 'DoStoreBp', 'DoStoreInsert', 'ReadUndoRecord'
        mov D$BreakPointLocation ebx
        call DeleteBreakPoint

    .Else_If eax = ACTION_DELDCBP
        mov D$BreakPointLocation ebx
        call SetBreakPoint

    .End_If

    call AskForRedraw
L9: ret
____________________________________________________________________________________________
;;
All the Pointers in UndoPtr Table are turned into Displacements from the 'CodeSource'
origin. This origin is continuously moving when the Source increases or decreases.
;;
ReadUndoRecord:
    mov esi D$UndoPtr

  ; When storing BPs, the Sources Pos are the ones of the Real Source. So:
   ; If D$esi+RECORD_FLAG = ACTION_DCBP
   ;     mov ebx D$esi+RECORD_PARAM1, eax ACTION_DCBP | ret
   ; End_If

    mov eax D$ActualTitle

    If eax <> D$esi+RECORD_ACTUALTITLE
        push esi
            call RestoreRealSource
        pop esi

        mov eax D$esi+RECORD_CURRENTWRITINGPOS
        add eax D$esi+RECORD_ACTUALTITLE
        mov D$CurrentWritingPos eax
        move D$UpperLine D$esi+RECORD_UPPERLINE

        push esi
            call SetPartialEdition
        pop esi
    End_If

    move D$CaretRow D$esi+RECORD_CARET_ROW
    move D$CaretLine D$esi+RECORD_CARET_LINE
    mov eax D$esi+RECORD_CURRENTWRITINGPOS | add eax D$CodeSource | mov D$CurrentWritingPos eax
    mov eax D$esi+RECORD_UPPERLINE | add eax D$CodeSource | mov D$UpperLine eax

    mov eax D$esi+RECORD_FLAG, ebx D$esi+RECORD_PARAM1, ecx D$esi+RECORD_PARAM2
ret
____________________________________________________________________________________________

; Called by 'ControlZ':

ReInsertDeletedChar:
    RealCaretRow

    mov esi D$SourceEnd | add esi 400 | mov edi esi      ; 400 is security 13/10/...
    mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx

    mov eax ebx

    cmp al tab | jne L1>
        cmp D$TabIs &TRUE | jne L2>
L1:         inc edi
            std
                rep movsb | stosb
            cld
            mov ebx 1 | jmp L3>

L2:     mov ebx D$TabIs | add edi ebx
        std
          rep movsb | mov al ' ', ecx ebx | rep stosb
        cld

L3: add D$SourceLen ebx | add D$CurrentWritingPos ebx | add D$SourceEnd ebx

    mov eax D$ColNumber

    cmp D$CaretRow  eax | jae L4>
    add D$CaretRow ebx | jmp L9>

L4: AlignOn RIGHT_FEED ebx | add D$RightScroll ebx | sub D$CaretRow ebx | dec D$CaretRow

L9: ret


UndoBlockCopy:
    mov D$BlockStartTextPtr ebx, D$BlockEndTextPtr ecx
    mov B$BlockInside &TRUE
    call UndoControlV
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[BACK 1, FORTH 0]

ControlShiftZ: ; ControlZ
    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If

    mov ebx D$UndoPtr | cmp D$ebx+RECORD_FLAG 0 | je L9>>   ; depend on flag > 0 > End
        mov D$UndoPtr ebx

    If D$BpOnTable <> 0
        pushad
            call AdjustBpTable, ControlShiftZ
        popad
    End_If

    call ReadUndoRecord
  ; >>> eax = RECORD_FLAG, ebx = RECORD_PARAM1, ecx = RECORD_PARAM2

    ..If eax = ACTION_INSERT
      ; RECORD_PARAM1 = Number of Chars // RECORD_PARAM2 = Char
        dec D$CaretRow | sub D$CurrentWritingPos ebx
        mov al cl, ecx ebx

L0:     push eax, ecx
            If al = CR
                call ReInsertSource | mov al LF | add D$UndoPtr UNDO_RECORD
            End_If
            call ReInsertSource
        pop ecx, eax
        loop L0<

    ..Else_If eax = ACTION_OVERWRITE
      ; RECORD_PARAM1 = Overwritten Char // RECORD_PARAM2 = New Char
        mov edi D$CurrentWritingPos, B$edi cl

    ..Else_If eax = ACTION_DEL ; 'DoStoreCharDelete' <<< 'BackSpace" // 'StripOneChar'
      ; RECORD_PARAM1 = Char // RECORD_PARAM2 = BACK, or FORTH
        .If ecx = FORTH
            If B$CurrentWritingPos = CR
                call StripBackSpace
                call StripBackSpace
                add D$UndoPtr UNDO_RECORD
            Else
                call StripBackSpace
                dec D$CurrentWritingPos
            End_If
        .Else
            call StripBackSpace
            dec D$CurrentWritingPos
            mov eax D$CurrentWritingPos | dec eax
            If B$eax = CR
                mov D$CurrentWritingPos eax
                call StripBackSpace
                call SetCaret D$CurrentWritingPos
                add D$UndoPtr UNDO_RECORD
            End_If
        .End_If

    ..Else_If eax = ACTION_BLOCKDELETE
        add ebx D$CodeSource | add ecx D$CodeSource
        mov D$BlockStartTextPtr ebx, D$BlockEndTextPtr ecx
        call WriteUndoBlockFileFromBlock | call UndoControlV
        mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr | inc ecx
        sub D$CurrentWritingPos ecx | sub D$CaretRow ecx

    ..Else_If eax = ACTION_BLOCKCOPY
        add ebx D$CodeSource | add ecx D$CodeSource
        call IncUndoFileName | call ReadUndoBlockFile
        mov eax D$UndoBlockLen | add D$CurrentWritingPos eax | add D$CaretRow eax

    ..Else_If eax = ACTION_DCBP
        mov D$BreakPointLocation ebx
        call SetBreakpoint

    ..Else_If eax = ACTION_DELDCBP
        mov D$BreakPointLocation ebx
        call DeleteBreakpoint

    ..End_If

    add D$UndoPtr UNDO_RECORD

    call AskForRedraw
ret


; Simplified 'InsertSource':

ReInsertSource:
    mov esi D$SourceEnd | add esi 400 | mov edi esi      ; 400 is security 13/10/...
    mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx

L1: inc edi
    std
        rep movsb | stosb
    cld
    mov ebx 1 | jmp L3>

L3: add D$SourceLen ebx | add D$CurrentWritingPos ebx | add D$SourceEnd ebx

    mov eax D$ColNumber

    cmp D$CaretRow  eax | jae L4>
    add D$CaretRow ebx | jmp L9>
L4: push ebx
        AlignOn RIGHT_FEED ebx | add D$RightScroll ebx | sub D$CaretRow ebx | inc D$CaretRow
    pop ebx

L9: ret


