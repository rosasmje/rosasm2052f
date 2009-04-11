TITLE DisInter

;;
  With each successful Disassembly, the Disassembler outputs 4 Files:
  
  * 3 of them are simple copies of 'SectionsMap', 'RoutingMap' and 'SizesMap'.
  
  * The 4th one is the 'ForcedRecordsTable', storing, at least, a header for
    'DisImageBase', and followed by the user defined Records, as edited with
    the 'ForcedFlags' Dialog, if any.
    
  -----------------
  The 
  
  'ForcedFlags' >>> 'ForcedFlagsProc'
  
  is called from the Float menu
  ('RightClickOnBlock' >>> DisLabel >>> Float Menu)
  
  When this interface is called, 'DisLabelTypeWas' says if this was CODEFLAG or
  DATAFLAG, 'DisAddressWas' holds the dWord Value of the selected Label (ex: 040506B),
  and 'CopyOfLabelHexa' is the Hexa Text form of the Label Number. Same is it for
  'NextDisAddressWas' and 'CopyOfNextLabelHexa', that hold the next matching Label,
  to assume then end of a Chunk, in Strings Cases.
  
  'ForcedFlagsProc' is the main Procedure that:
  
  * Reads the Files: 'MyAppSection.map', 'MyAppRouting.map', 'MyAppSize.map',
                     and, also, the 'MyAppForced.map' File.
                     
    These 3 Files are images of the Disassembler Tables, saves at the end of each
    Disassembling. When 'ForcedFlagsProc' reads them, this is just to get the
    actual Flags of the Bytes targetted by the last Location the User selected
    in the Disassembled Source. These Flags are just read to initialize the Dialog.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________

; Main of the Forced Flag Box: 'ForcedFlags' >>> 'ForcedFlagsProc'.

ForcedFlags:
    call GetOriginalDisFileNameFromSource

    If B$SaveFilter <> 0
        call 'USER32.DialogBoxParamA' D$hInstance, 2500, D$hwnd, ForcedFlagsProc, &NULL

        On eax = &TRUE, call ReRunDisassembler
    Else
        call 'USER32.MessageBoxA', D$hwnd, {"The expected Original File Name was not found,
at the Top of this Source" 0}, {'Failure of Edition', 0}, 0
    End_If
ret


[ForcedFlagsProcHandle: ?]

; Tag Dialog 2500

Proc ForcedFlagsProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ..If D@Message = &WM_INITDIALOG
        move D$ForcedFlagsProcHandle D@Adressee
        mov B$ForcedFlagsModified &FALSE

        call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &EM_SETLIMITTEXT, 8, 0
        call 'USER32.SendDlgItemMessageA' D@Adressee, 11, &EM_SETLIMITTEXT, 8, 0

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        mov eax &TRUE

        call InitForcedFlagsDialog

        If D$ForcedRecordsTable = 0
            call 'USER32.MessageBoxA', D@Adressee,
            {'The Forced Records File was not found', 0}, {'Failure of Edition', 0}, 0
            call 'User32.EndDialog' D@Adressee, &FALSE
        End_If

    ..Else_If D@Message = &WM_CLOSE
        call 'User32.EndDialog' D@Adressee, &FALSE

    ..Else_If D@Message = &WM_COMMAND
        .If D@wParam = &IDCANCEL
            call 'User32.EndDialog' D@Adressee, &FALSE

        .Else_If D@wParam = &IDOK
            call RegisterUserFlags
            If B$BadUserFlag = &FALSE
                call WriteForcedRecordsFile  ; ReadForcedRecordsFile
                call 'User32.EndDialog' D@Adressee, &TRUE
            End_If

        .Else_If D@wparam = ID_HELP
            call Help, B_U_AsmName, DisMap, ContextHlpMessage

      ; [x] Code Flag
        .Else_If D@wparam = 10
            call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &BM_GETCHECK, 0, 0
            push eax
                xor eax &TRUE
                call 'USER32.SendDlgItemMessageA' D@Adressee, 11, &BM_SETCHECK, eax, 0
            pop eax
            If eax = &TRUE
                call DisableDisDataFlags
              ; Code cannot be 'POINTER':
                call 'USER32.SendDlgItemMessageA' D@Adressee, 23, &BM_SETCHECK, &FALSE, 0
              ; But should be 'INSTRUCTION'
                call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &BM_SETCHECK, &TRUE, 0
            Else
                call EnableDisDataFlags
              ; Data cannot be 'INSTRUCTION', 'EXPORTED':
                call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &BM_SETCHECK, &FALSE, 0
                call 'USER32.SendDlgItemMessageA' D@Adressee, 22, &BM_SETCHECK, &FALSE, 0
            End_If
      ; [x] Data Flag
        .Else_If D@wparam = 11
            call 'USER32.SendDlgItemMessageA' D@Adressee, 11, &BM_GETCHECK, 0, 0
            xor eax &TRUE
            push eax
                call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &BM_SETCHECK, eax, 0
            pop eax
            xor eax &TRUE
            If eax = &TRUE
                call EnableDisDataFlags
              ; Data cannot be 'INSTRUCTION', 'EXPORTED':
                call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &BM_SETCHECK, &FALSE, 0
                call 'USER32.SendDlgItemMessageA' D@Adressee, 22, &BM_SETCHECK, &FALSE, 0
            Else
                call DisableDisDataFlags
              ; Code cannot be 'POINTER':
                call 'USER32.SendDlgItemMessageA' D@Adressee, 23, &BM_SETCHECK, &FALSE, 0
            End_If
       ; [x] Instruction
         .Else_If D@wparam = 20
            call CheckForcedRouting 20
       ; [x] Label
         .Else_If D@wparam = 21
            call CheckForcedRouting 21
       ; [x] Exported
         .Else_If D@wparam = 22
            call CheckForcedRouting 22
       ; [x] Pointer
         .Else_If D@wparam = 23
            call CheckForcedRouting 23

      ; [x] Previous
        .Else_If D@wparam = 60
            call DecForcedRecord

      ; [x] Next
        .Else_If D@wparam = 62
            call IncForcedRecord

      ; [x] Delete This Record
        .Else_If D@wparam = 70
            call DeleteForcedRecord
            If D$DisForcedRecordIndice = 0
                call WriteForcedRecordsFile
                call 'User32.EndDialog' D@Adressee 0
            End_If

        .Else_If D@wparam < 30
          ; nop

      ; [x] Byte / Word / Dword / FPU4/8/10
        .Else_If D@wparam < 36
            call Disable D$ForcedFlagsProcHandle, 51

      ;; [x] String or Unicode String
        .Else_If D@wparam < 38
            call Enable D$ForcedFlagsProcHandle, 51
            call SetForcedNextAddressEditControl

        .End_If

    ..Else_If D@Message = &WM_CTLCOLOREDIT
        call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP

    ..Else
        popad | mov eax &FALSE | jmp L9>

    ..End_If

    popad | mov eax &TRUE

L9: EndP
____________________________________________________________________________________________

; For coherency of the Routing Flags:

Proc CheckForcedRouting:
    Argument @ID

        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, D@ID, &BM_GETCHECK, 0, 0

        .If eax = &TRUE
            If D@ID < 23
              ;  >>> Cannot be Pointer:
                call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23,
                                                  &BM_SETCHECK, &FALSE, 0
            Else
              ; 23 (Pointer) >>> Cannot be Instruction, Exported:
                call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20,
                                                  &BM_SETCHECK, &FALSE, 0
                call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 22,
                                                  &BM_SETCHECK, &FALSE, 0
              ; But must be a dWord (ID 32):
                call CheckForcedData 32

            End_If
        .End_If
EndP


; Tag Dialog 2500

Proc CheckForcedData:
    Argument @ID

        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_SETCHECK, &FALSE, 0

        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, D@ID, &BM_SETCHECK, &TRUE, 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[MapFileHandle: ?]

; Called from DisMain:

WriteMapFiles:
    pushad

    call TakeCopyOfDisName

    push edi
        mov D$edi 'Sect', D$edi+4 'ion.', D$edi+8 'map'

        call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        If eax <> &INVALID_HANDLE_VALUE
            mov D$MapFileHandle eax

            mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap

            call 'KERNEL32.WriteFile' D$MapFileHandle, D$SectionsMap, ecx,
                                      NumberOfReadBytes, 0

            call 'KERNEL32.CloseHandle' D$MapFileHandle
        End_If

    pop edi
    push edi
        mov D$edi 'Rout', D$edi+4 'ing.', D$edi+8 'map'

        call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        If eax <> &INVALID_HANDLE_VALUE
            mov D$MapFileHandle eax

            mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap

            call 'KERNEL32.WriteFile' D$MapFileHandle, D$SectionsMap, ecx,
                                      NumberOfReadBytes, 0

            call 'KERNEL32.CloseHandle' D$MapFileHandle
        End_If

    pop edi
    push edi
        mov D$edi 'Size', D$edi+4 '.map', B$edi+8 0

        call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE, 0, 0, &CREATE_ALWAYS,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        If eax <> &INVALID_HANDLE_VALUE
            mov D$MapFileHandle eax

            mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap

            call 'KERNEL32.WriteFile' D$MapFileHandle, D$SectionsMap, ecx,
                                      NumberOfReadBytes, 0

            call 'KERNEL32.CloseHandle' D$MapFileHandle
        End_If
    pop edi

    popad
ret
____________________________________________________________________________________________
;;
  Called from DisMain.
  
  Must be executed in all case. For example, a user can change the AddressBase from
  exe to dll, without having yet used any Forced Record...
;;
WriteForcedRecordsFileBase:
  ; Is this usefull? (Case of empty File?)
    call ReadForcedRecordsFile

    If D$ForcedRecordsTable = 0
        VirtualAlloc ForcedRecordsTable, 010
    End_If

  ; Really needed:
    mov edi D$ForcedRecordsTable, eax D$DisImageBase | stosd

    call WriteForcedRecordsFile

    VirtualFree D$ForcedRecordsTable
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

DecForcedRecord:
    If D$DisForcedRecordIndice > 1
        call GetForcedDialogFlags
        call RegisterForcedRecord
        dec D$DisForcedRecordIndice
        call SetForcedFlagsFromForcedRecord
        call SetSourceToForcedRecord
    End_If
ret

IncForcedRecord:
    call GetForcedRecordPointer | mov esi eax | add esi FORCED_RECORD_LENGHT

    If D$esi <> 0
        call GetForcedDialogFlags
        call RegisterForcedRecord
        inc D$DisForcedRecordIndice
        call SetForcedFlagsFromForcedRecord
        call SetSourceToForcedRecord
    End_If
ret

DeleteForcedRecord:
    call GetForcedRecordPointer | mov esi eax

    On D$DisForcedRecordIndice > 1, dec D$DisForcedRecordIndice

    mov edi esi | add esi FORCED_RECORD_LENGHT

L0: movsd | movsd | movsd | cmp D$esi 0 | ja L0<

    mov D$edi 0, D$edi+4 0, D$edi+8 0

    sub D$ForcedRecordsSize FORCED_RECORD_LENGHT

    call SetForcedFlagsFromForcedRecord
    call SetSourceToForcedRecord
ret

; Called by the Disassembler, when the 'WithForcedMapFile' is set to &FALSE:

DeleteForcedFile:
    call TakeCopyOfDisName
    mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    call 'KERNEL32.DeleteFileA' DisassemblyMapName
ret
____________________________________________________________________________________________

RegisterForcedRecord:
    call GetForcedRecordPointer | mov ebx eax

    move D$ebx+FORCED_RECORD_OFFSET1 D$DisAddressWas
    move D$ebx+FORCED_RECORD_OFFSET2 D$NextDisAddressWas
    call ForcedFlagsIntoEax | mov D$ebx+FORCED_RECORD_FLAGS eax
ret


; Returns a Pointer to the actual Forced Record:

GetForcedRecordPointer:
    push ecx
        mov eax D$DisForcedRecordIndice | dec eax
        mov ecx FORCED_RECORD_LENGHT | mul ecx
        add eax FORCED_FILE_HEADERLENGHT
        add eax D$ForcedRecordsTable
    pop ecx
ret
____________________________________________________________________________________________

[SearchCopyOfForcedLabel: ? #4]

; Moves the Disassembly Source to the actually edited Forced Label ([Previous]/[Next]):

SetSourceToForcedRecord:
    mov esi CopyOfLabelHexa, edi SearchCopyOfForcedLabel

    If B$UserSectionFlag = CODEFLAG
        mov eax 'Code'
    Else
        mov eax 'Data'
    End_If
    stosd
    mov ebx 4
    While B$esi <> 0 | movsb | inc ebx | End_While | mov B$edi 0

  ; We now have a Copy of the Edited Record Label in 'SearchCopyOfForcedLabel'.
  ; call for a Search:
    push ebx
        call RestoreRealSource
    pop ebx
    mov edx SearchCopyOfForcedLabel

    call InternSearch | call AskForRedrawNow

    If B$BlockInside = &TRUE
        mov B$BlockInside &FALSE
        mov esi D$CurrentWritingPos | dec esi | dec esi | dec esi | call InternalRightClick
        mov B$BlockInside &TRUE
    End_If

    call SetPartialEditionFromPos
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[DisLabelTypeWas: ?    DisAddressWas: ?  NextDisAddressWas: ?
 CopyOfLabelHexa: ? ? ? ? ? ? ? ? ? ?
 CopyOfNextLabelHexa: ? ? ? ? ? ? ? ? ? ?]

[DisassemblyMapName: B$ ? #&MAXPATH]

TakeCopyOfDisName:
    mov esi SaveFilter, edi DisassemblyMapName
    While B$esi <> 0 | movsb | End_While | mov B$edi 0
    While B$edi <> '.' | dec edi | End_While
  ; Returns with edi >>> '.'
ret

[OriginalFileComment: B$ "
; Do not remove this Comment. It is used by the Disassembler for the interactive
; Editions of the Disassemblies Flags. Without, it would fail to guess which was
; the original File Name:
;
; "

OriginalDisFilePath: 0]

WriteOriginalFileNameInSource:
    mov esi OriginalFileComment, ecx OriginalDisFilePath
    sub ecx OriginalFileComment | rep movsb

    mov esi SaveFilter | While B$esi <> 0 | movsb | End_While

    mov D$edi CRLF2 | add edi 4
ret


GetOriginalDisFileNameFromSource:
    call RestoreRealSource

  ; Search, for example, for '; E:\':, as written by 'WriteOriginalFileNameInSource'
    mov esi D$CodeSource
    mov ecx OriginalDisFilePath | sub ecx OriginalFileComment
    add esi ecx

    mov edi SaveFilter

    .If W$esi-2 = '; '
        If W$esi+1 = ':\'
            While B$esi > CR | movsb | End_While | mov B$edi 0
        Else
            mov B$edi 0
        End_If

    .Else
        mov B$edi 0
    .End_If

    call SetPartialEditionFromPos

    call 'KERNEL32.FindFirstFileA' SaveFilter, FindFile

    If eax = &INVALID_HANDLE_VALUE
        mov D$SaveFilter 0
    Else
        call 'KERNEL32.FindClose' eax
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

InitForcedFlagsDialog:
    call ReadForcedRecordsFile

    call Disable D$ForcedFlagsProcHandle, 51

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 50, &WM_SETTEXT, 0,
                                      CopyOfLabelHexa

    mov esi D$ForcedRecordsTable, eax D$esi+FORCED_FILE_IMAGEBASE, D$DisImageBase eax

    call GetUserSelectionIndice | On eax = 0, call RegisterForcedRecord

    call 'USER32.SetDlgItemInt' D$ForcedFlagsProcHandle, 61,
                                D$DisForcedRecordIndice, &FALSE

    call TakeCopyOfDisName

    mov ebx edi
    push ebx
        mov D$ebx 'Sect', D$ebx+4 'ion.', D$ebx+8 'map'
        call ReadMapFileByte D$DisAddressWas ; SectionsMap, EndOfSectionsMap
        .If D$DisassemblyMapHandle = &INVALID_HANDLE_VALUE
           ; mov eax {'Sections Map File not found', 0} | call MessageBox
L1:         pop ebx | call InitForcedFlagsFromSelection | ret

        .Else
            call InitForcedSectionsDialog

        .End_If
    pop ebx

    push ebx
        mov D$ebx 'Rout', D$ebx+4 'ing.', D$ebx+8 'map'
        call ReadMapFileByte D$DisAddressWas ; SectionsMap, EndOfSectionsMap
        .If D$DisassemblyMapHandle = &INVALID_HANDLE_VALUE
            mov eax {'Routing Map File not found', 0} | call MessageBox | jmp L1<<

        .Else
            call InitForcedRoutingDialog

        .End_If
    pop ebx

    push ebx
        mov D$ebx 'Size', D$ebx+4 '.map', B$ebx+8 0
        call ReadMapFileByte D$DisAddressWas

        ..If D$DisassemblyMapHandle = &INVALID_HANDLE_VALUE
            mov eax {'Sizes Map File not found', 0} | call MessageBox | jmp L1<<

        ..Else
            call InitForcedSizeDialog

        ..End_If
    pop ebx
ret


InitForcedSectionsDialog:
    If eax = CODEFLAG
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        call DisableDisDataFlags
    Else
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &TRUE, 0
        call EnableDisDataFlags
    End_If
ret


InitForcedRoutingDialog:
    test eax INSTRUCTION | jz L1>
        push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        pop eax
L1: test eax NODE | jz L1>
        push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        pop eax
L1: ;test eax LABEL | jz L1> ; We suppose the Label always wanted
        push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_SETCHECK, &TRUE, 0
        pop eax
L1: test eax EXPORTNODE | jz L1>
        push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 22, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &FALSE, 0
        pop eax
L1: test eax INDIRECT | jz L1>
        push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 11, &BM_SETCHECK, &TRUE, 0
        pop eax
L1: ret


InitForcedSizeDialog:
  ; Clear all Sizes Flags and disable the End Address Edit:
    push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_SETCHECK, &FALSE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_SETCHECK, &FALSE, 0
        call Disable D$ForcedFlagsProcHandle, 51
    pop eax

    .If eax = BYTE
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = WORD
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = DWORD
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = FP4
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = FP8
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = FP10
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = POINTER
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23, &BM_SETCHECK, &TRUE, 0
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_SETCHECK, &TRUE, 0
    .Else_If eax = STRINGS+BYTE
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_SETCHECK, &TRUE, 0
        call Enable D$ForcedFlagsProcHandle, 51
    .Else_If eax = STRINGS+WORD
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_SETCHECK, &TRUE, 0
        call Enable D$ForcedFlagsProcHandle, 51
    .End_If
ret
____________________________________________________________________________________________

; First Record, for a given Disassembly. We take the Infos from the user's selection:

InitForcedFlagsFromSelection:
    mov eax D$DisLabelTypeWas | call InitForcedSectionsDialog

    ..If D$DisLabelTypeWas = CODEFLAG
        mov eax D$BlockEndTextPtr
        If W$eax +1 = '::'
            mov eax EXPORTNODE
        Else
            mov eax 0
        End_If
        add eax INSTRUCTION+LABEL | call InitForcedRoutingDialog
        call DisableDisDataFlags

    ..Else
        mov eax D$BlockEndTextPtr, edx D$eax+3, bl B$eax+6, al B$eax+3

        If edx = 'Data'
            mov eax INDIRECT | call InitForcedRoutingDialog
            mov eax DWORD
        Else_If edx = 'Code'
            mov eax INDIRECT | call InitForcedRoutingDialog
            mov eax DWORD
        Else_If al = 'B'
            mov eax BYTE
            On bl = '"', add eax STRINGS
        Else_If al = 'W'
            mov eax WORD
        Else_If al = 'D'
            mov eax DWORD
        Else_If al = 'F'
            mov eax FP4
        Else_If al = 'R'
            mov eax FP8
        Else_If al = 'T'
            mov eax FP10
        Else_If al = 'U'
            mov eax STRINGS+WORD
        End_If

        call InitForcedSizeDialog

    ..End_If

    call Disable D$ForcedFlagsProcHandle, 60
    call Disable D$ForcedFlagsProcHandle, 62
ret


DisableDisDataFlags:
    call Disable D$ForcedFlagsProcHandle, 30
    call Disable D$ForcedFlagsProcHandle, 31
    call Disable D$ForcedFlagsProcHandle, 32
    call Disable D$ForcedFlagsProcHandle, 33
    call Disable D$ForcedFlagsProcHandle, 34
    call Disable D$ForcedFlagsProcHandle, 35
    call Disable D$ForcedFlagsProcHandle, 36
    call Disable D$ForcedFlagsProcHandle, 37
    call Disable D$ForcedFlagsProcHandle, 51
ret


EnableDisDataFlags:
    call Enable D$ForcedFlagsProcHandle, 30
    call Enable D$ForcedFlagsProcHandle, 31
    call Enable D$ForcedFlagsProcHandle, 32
    call Enable D$ForcedFlagsProcHandle, 33
    call Enable D$ForcedFlagsProcHandle, 34
    call Enable D$ForcedFlagsProcHandle, 35
    call Enable D$ForcedFlagsProcHandle, 36
    call Enable D$ForcedFlagsProcHandle, 37
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_GETCHECK, 0, 0
    push eax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_GETCHECK, 0, 0
    pop ebx
    or eax ebx
    If eax <> 0
        call Enable D$ForcedFlagsProcHandle, 51
    End_If
ret


SetForcedNextAddressEditControl:
    call GetForcedRecordPointer | mov eax D$eax+FORCED_RECORD_OFFSET2

    mov D$CopyOfNextLabelHexa 0

    If eax <> 0
        mov edi CopyOfNextLabelHexa | call WriteEax | mov B$edi 0
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_SETTEXT,
                                      0, CopyOfNextLabelHexa
ret
____________________________________________________________________________________________

[DisassemblyMapHandle: ?  DisassemblyMapPtr: ?  DisassemblyMapLen: ?]
;;
  Reads one Byte, in any Map (SectionsMap, , 'RoutingMap' or 'SizesMap'):
;;
Proc ReadMapFileByte:
    Argument @Ptr
        call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        mov D$DisassemblyMapHandle eax

        .If eax <> &INVALID_HANDLE_VALUE

            call 'KERNEL32.GetFileSize' eax, 0 | mov D$DisassemblyMapLen eax
                add eax 10

            VirtualAlloc DisassemblyMapPtr eax

            call 'KERNEL32.ReadFile' D$DisassemblyMapHandle, D$DisassemblyMapPtr,
                                     D$DisassemblyMapLen, NumberOfReadBytes, 0

            call 'KERNEL32.CloseHandle' D$DisassemblyMapHandle

            mov eax D@Ptr | sub eax D$DisImageBase | add eax D$DisassemblyMapPtr
            movzx eax B$eax
          ; Flags Byte in eax

            push eax
                VirtualFree D$DisassemblyMapPtr
            pop eax
        .End_If
EndP
____________________________________________________________________________________________

[ForcedRecordsTable: ?  ForcedRecordsSize: ?]
;;
  'ForcedRecordsTable' holds Records of 2 dWords.
  
  * The leading Record is only for storing the 'DisImageBase', and is written at
    the end of the Disassembly Process, by the Disassembler itself.
  
  * The next Records store the user Editions: ... // DWORD Offset / DWORD Flags // ...
    where "Offset" is the displacement inside any Flags Table ('SectionsMap',
    'RoutingMap' and 'SizesMap'), plus the 'DisImageBase', and where "Flags" holds
    the 3 Flags Types, in order:
    
    Low Byte = SizesMap (Example, if Data: STRINGS+BYTE)
    Next Byte = RoutingMap Flag (Example INSTRUCTION+LABEL+EXPORTNODE)
    Next Byte = SectionsMap Flag (Example CODEFLAG)
    
    ... that are managed by 'ForcedFlagsIntoEax', 'EaxIntoForcedFlags'
    
    An "empty" Forced Records Table holds 8 Bytes, at least, for the DisImageBase.
;;

[FORCED_FILE_IMAGEBASE 0, FORCED_FILE_HEADERLENGHT 4
 FORCED_RECORD_OFFSET1 0, FORCED_RECORD_OFFSET2 4, FORCED_RECORD_FLAGS 8,
 FORCED_RECORD_LENGHT 12]

ReadForcedRecordsFile: ; WriteForcedRecordsFile
    VirtualFree D$ForcedRecordsTable

    call TakeCopyOfDisName
    mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    If eax <> &INVALID_HANDLE_VALUE
        mov D$DisassemblyMapHandle eax

        call 'KERNEL32.GetFileSize' D$DisassemblyMapHandle, 0
        mov D$ForcedRecordsSize eax | add eax 010
        VirtualAlloc ForcedRecordsTable eax

        call 'KERNEL32.ReadFile' D$DisassemblyMapHandle, D$ForcedRecordsTable,
                                 D$ForcedRecordsSize, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$DisassemblyMapHandle

    Else
        mov D$ForcedRecordsTable 0

    End_If
ret


IsForcedMapFile:
    call TakeCopyOfDisName

    mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    call 'KERNEL32.FindFirstFileA' DisassemblyMapName, FindFile

    If eax <> &INVALID_HANDLE_VALUE
        call 'KERNEL32.FindClose' eax
        mov eax &TRUE
    Else
        mov eax &FALSE
    End_If
ret
____________________________________________________________________________________________

; Called from 'DisMain':

ForceRecordsToMaps:
    mov esi D$ForcedRecordsTable | mov edx D$esi+FORCED_FILE_IMAGEBASE
    add esi FORCED_FILE_HEADERLENGHT

    .While D$esi <> 0
        mov eax D$esi+FORCED_RECORD_FLAGS | call EaxIntoForcedFlags

      ; Take the Displacement to the Map Tables:
        mov ebx D$esi+FORCED_RECORD_OFFSET1
      ; edx is the previously saved 'DisImageBase':

        sub ebx edx

      ; Force the SectionsMap:
      ; Useless: Everything is empty at this stage:
      ;  mov edi D$SectionsMap | add edi ebx
      ;  mov ecx D$esi+FORCED_RECORD_OFFSET2 | sub ecx D$esi+FORCED_RECORD_OFFSET1 | jecxz L1>
      ;  mov al 0 | rep stosb

L1:     mov edi D$SectionsMap, al B$UserSectionFlag, B$edi+ebx al

      ; Force the RoutingMap:
        mov edi D$RoutingMap, al B$UserRoutingFlag
        On B$UserSectionFlag = CODEFLAG, or al LABEL+NODE+EVOCATED+INSTRUCTION+ACCESSED
        mov B$edi+ebx al

      ; Force the SizesMap:
        mov edi D$SizesMap, al B$UserSizeFlag
        .If B$UserSectionFlag = CODEFLAG
            mov B$edi+ebx 0
        .Else
;;
  If Data, we may have to Flag Chunks of Bytes, in the SizesMap, instead of one
  single Byte, and to also flag the SectionsMap as Data:
;;
            If al = BYTE
                call ForceDataFlag eax, 1
            Else_If al = WORD
                call ForceDataFlag eax, 2
            Else_If al = DWORD
                call ForceDataFlag eax, 4
            Else_If al = FP4
                call ForceDataFlag eax, 4
            Else_If al = FP8
                call ForceDataFlag eax, 8
            Else_If al = FP10
                call ForceDataFlag eax, 10
            Else_If al = POINTER
                call ForceDataFlag eax, 4
            Else_If al = STRINGS+BYTE
                call ForceDataFlag eax, 0-1
            Else_If al = STRINGS+WORD
                call ForceDataFlag eax, 0-1
            End_If
        .End_If

        add esi FORCED_RECORD_LENGHT
    .End_While
;map
ret

Proc ForceDataFlag:
    Argument @Flag, @n

        mov al B@Flag

        .If D@n = 0-1
          ; Flag Strings down to the next Label:
            mov ecx D$esi+FORCED_RECORD_OFFSET2 | sub ecx D$esi+FORCED_RECORD_OFFSET1

            push ebx, ecx
L0:             mov B$edi+ebx al | inc ebx | loop L0<
            pop ecx, ebx

            mov edi D$SectionsMap
L0:         mov B$edi+ebx DATAFLAG | inc ebx | loop L0<

        .Else
            push ebx
                mov ecx D@n
L0:             mov B$edi+ebx al | inc ebx | loop L0<
            pop ebx

            push ebx
                mov ecx D@n, edi D$SectionsMap
L0:             mov B$edi+ebx DATAFLAG | inc ebx | loop L0<
            pop ebx
        .End_If
EndP
____________________________________________________________________________________________

WriteForcedRecordsFile: ; ReadForcedRecordsFile
    call TakeCopyOfDisName
    mov D$edi 'Forc', D$edi+4 'ed.m', D$edi+8 'ap'

    call 'KERNEL32.CreateFileA' DisassemblyMapName, &GENERIC_WRITE,
                                &FILE_SHARE_READ, 0, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, 0
    mov D$DisassemblyMapHandle eax

    If eax <> &INVALID_HANDLE_VALUE
        mov esi D$ForcedRecordsTable, ecx FORCED_FILE_HEADERLENGHT
        add esi ecx

        While D$esi <> 0
            add esi FORCED_RECORD_LENGHT | add ecx FORCED_RECORD_LENGHT
        End_While

        call 'KERNEL32.WriteFile' D$DisassemblyMapHandle, D$ForcedRecordsTable,
                                  ecx, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$DisassemblyMapHandle
    End_If
ret
____________________________________________________________________________________________

[DisForcedRecordIndice: ?]

GetUserSelectionIndice:
  ; Substract the 'DisImageBase' from the selected Label dWord:
    mov eax D$DisAddressWas
;;
  Scan all Records, in 'ForcedRecordsTable'. In case a same Offest would already
  have been edited, we lock on that one. If none found, we start the Edition of
  a new Record, at the end.
;;
    mov esi D$ForcedRecordsTable | add esi FORCED_FILE_HEADERLENGHT

    mov ecx 1

    While D$esi <> 0
        cmp D$esi eax | je L1>
        add esi FORCED_RECORD_LENGHT | inc ecx
    End_While

  ; No matching Record found >>> Return 0
    mov eax 0

L1: mov D$DisForcedRecordIndice ecx
ret
____________________________________________________________________________________________
;;
  Job similar to the one already done by the Init of 'InitForcedFlagsDialog' (the caller),
  but done from the Values Recorded in 'ForcedRecordsTable', at the wanted Indice:
;;
SetForcedFlagsFromForcedRecord:
        call Disable D$ForcedFlagsProcHandle, 51

        call 'USER32.SetDlgItemInt' D$ForcedFlagsProcHandle, 61,
                                    D$DisForcedRecordIndice, &FALSE

      ; Take the DisImageBase:
        mov esi D$ForcedRecordsTable
        mov eax D$esi+FORCED_FILE_IMAGEBASE | mov D$DisImageBase eax

      ; Point with esi to the proper Record
        mov ecx D$DisForcedRecordIndice | dec ecx
        mov eax FORCED_RECORD_LENGHT | mul ecx
        add eax FORCED_FILE_HEADERLENGHT
        add esi eax

      ; Write the Address to the EditBox:
        mov eax D$esi+FORCED_RECORD_OFFSET1
        push esi
            mov edi CopyOfLabelHexa | call WriteEax | mov B$edi 0
            call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 50, &WM_SETTEXT,
                                              0, CopyOfLabelHexa
        pop esi

      ; Split the Flags onto the wanted Variables:
        mov eax D$esi+FORCED_RECORD_FLAGS | call EaxIntoForcedFlags
      ; And set the Radio and CheckBoxes:
        push esi
            mov eax D$UserSectionFlag | call InitForcedSectionsDialog
            mov eax D$UserRoutingFlag | call InitForcedRoutingDialog
            mov eax D$UserSizeFlag | call InitForcedSizeDialog
        pop esi

        Test B$UserSizeFlag STRINGS | jz L2>
            mov eax D$esi+FORCED_RECORD_OFFSET2

          ; And write it to the EditBox:
            mov edi CopyOfNextLabelHexa | call WriteEax | mov B$edi 0
            call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_SETTEXT,
                                              0, CopyOfNextLabelHexa
L2:
        If D$UserSectionFlag = CODEFLAG
            call DisableDisDataFlags
        End_If
ret
____________________________________________________________________________________________

[StartOfForcedSection: ? EndOfForcedSection: ?   ForcedFlagsModified: ?]
[ForcedStartAddressBuffer: ? #3][ForcedEndAddressBuffer: ? #3]

[UserSectionFlag: ?  UserRoutingFlag: ?   UserSizeFlag: ?   BadUserFlag: ?]

; The order, from High to low is: Empty/Section/Routing/Size.

ForcedFlagsIntoEax:
    mov eax D$UserSectionFlag | shl eax 8
    or al B$UserRoutingFlag | shl eax 8
    or al B$UserSizeFlag
ret

EaxIntoForcedFlags:
    mov B$UserSizeFlag al | shr eax 8
    mov B$UserRoutingFlag al | shr eax 8
    mov B$UserSectionFlag al
ret
____________________________________________________________________________________________

GetForcedDialogFlags:
    mov B$BadUserFlag &FALSE

  ; Get the targetted Address:
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 50, &WM_GETTEXT,
                                      10, ForcedStartAddressBuffer

    call GetHexaFromText ForcedStartAddressBuffer

    If B$GetHexaFromTextError = &TRUE
        mov eax {'The Address must be given in Hexa Format', 0} | call MessageBox
        mov B$BadUserFlag &TRUE | ret
    End_If

    mov D$DisAddressWas eax

    call GetForcedEndAddress | On eax <> 0, mov D$NextDisAddressWas eax

  ; Get the SectionsMap Flag:
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 10, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSectionFlag CODEFLAG
    Else
        mov B$UserSectionFlag DATAFLAG
    End_If

  ; Get the RoutingMap Flags:
  ; [x] Instruction
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 20, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserRoutingFlag INSTRUCTION
    Else
        mov B$UserRoutingFlag 0
    End_If
  ; [x] Label
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 21, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        or B$UserRoutingFlag LABEL+EVOCATED
    End_If
  ; [x] Exported
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 22, &BM_GETCHECK, 0, 0
    On eax = &TRUE, or B$UserRoutingFlag EXPORTNODE+NODE+EVOCATED+ACCESSED+LABEL
  ; [x] Pointer
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 23, &BM_GETCHECK, 0, 0
    On eax = &TRUE, or B$UserRoutingFlag INDIRECT

  ; Get the SizesMap Flags:
    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 30, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag BYTE | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 31, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag WORD | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 32, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag DWORD | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 33, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag FP4 | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 34, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag FP8 | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 35, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag FP10 | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 36, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag STRINGS+BYTE | ret
    End_If

    call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 37, &BM_GETCHECK, 0, 0
    If eax = &TRUE
        mov B$UserSizeFlag STRINGS+WORD | ret
    End_If
ret
____________________________________________________________________________________________

RegisterUserFlags:
    call GetForcedDialogFlags

    If B$BadUserFlag = &FALSE
        call GetForcedRecordPointer | mov edi eax

        mov eax D$DisAddressWas | mov D$edi+FORCED_RECORD_OFFSET1 eax
        mov eax D$NextDisAddressWas | mov D$edi+FORCED_RECORD_OFFSET2 eax

        call ForcedFlagsIntoEax | mov D$edi+FORCED_RECORD_FLAGS eax

        push edi
            call GetForcedEndAddress
        pop edi

        On eax <> 0, mov D$edi+FORCED_RECORD_OFFSET2 eax
    End_If
ret
____________________________________________________________________________________________

GetForcedEndAddress:
    call 'USER32.GetDlgItem' D$ForcedFlagsProcHandle, 51
    call 'USER32.IsWindowEnabled' eax

    .If eax = &TRUE
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_GETTEXT,
                                          10, ForcedStartAddressBuffer
        If eax = 0
            call 'USER32.MessageBoxA' D$ForcedFlagsProcHandle,
            {'You must give the End Address of the String (Excluded Label)', 0},
            {'Missing Address', 0}, &MB_OK

            mov B$BadUserFlag &TRUE | jmp L2>
        End_If

        call GetHexaFromText ForcedStartAddressBuffer

        If B$GetHexaFromTextError = &TRUE
            mov eax {'The Address must be given in Hexa Format', 0}
            call MessageBox

            mov B$BadUserFlag &TRUE | jmp L2>
        End_If
      ; eax = Value of the End Label

    .Else
L2:      mov eax 0

    .End_If
ret
____________________________________________________________________________________________

[CopyOfEndLabelHexa: ? ? ? ? ? ? ? ? ? ?]

SetForcedEndAddress:
    push esi, edi
        mov eax D$esi+FORCED_RECORD_OFFSET2, edi CopyOfEndLabelHexa | call WriteEax
        call 'USER32.SendDlgItemMessageA' D$ForcedFlagsProcHandle, 51, &WM_SETTEXT,
                                          0, CopyOfEndLabelHexa
    pop edi, esi
ret
____________________________________________________________________________________________

[AddressToBeForced: ?   ForcedFlag: ?
 ForcedUpperLine: ?    ForcedCurrentWritingPos: ?]

ReRunDisassembler:
    mov B$AddressToBeForced &TRUE

    call RestoreRealSource

    mov eax D$UpperLine | sub eax D$CodeSource
    mov D$ForcedUpperLine eax
    mov eax D$CurrentWritingPos | sub eax D$CodeSource
    mov D$ForcedCurrentWritingPos eax

    call SetPartialEditionFromPos | call AskForRedrawNow

    call ReInitUndo | call ClearBackTable | mov D$TiTleTable 0

    call ReleaseMainFile | StoreNameOnly SaveFilter | call ClearBackTable
    VirtualFree D$UserPeStart

    mov B$WithForcedMapFile &TRUE | call ReloadForDissassembler

    mov eax D$ForcedUpperLine | add eax D$CodeSource
    mov D$UpperLine eax
    mov eax D$ForcedCurrentWritingPos | add eax D$CodeSource
    mov D$CurrentWritingPos eax
    mov D$PreviousPartialSourceLen 0

    call SetPartialEditionFromPos
    call SetCaret D$CurrentWritingPos

    mov B$BlockInside &FALSE, B$AddressToBeForced &FALSE
ret
____________________________________________________________________________________________
____________________________________________________________________________________________











