TITLE MRU
____________________________________________________________________________________________
____________________________________________________________________________________________

;                            Most Recently Used Files job.
____________________________________________________________________________________________
____________________________________________________________________________________________

[MRU1: B$ ? #260][MRU2: B$ ? #260][MRU3: B$ ? #260][MRU4: B$ ? #260]


; Adds the menu Items in [File] Popup, between [OutPut] and [Exit]:

[FilePopUpHandle: ?    AddedMRUitems: ?]

SetMRUmenu:
    call DeleteMRUmenu

    If D$MRU1 > 0
        call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3001, MRU1
        inc D$AddedMRUitems
    End_If
    If D$MRU2 > 0
        call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3002, MRU2
        inc D$AddedMRUitems
    End_If
    If D$MRU3 > 0
        call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3003, MRU3
        inc D$AddedMRUitems
    End_If
    If D$MRU4 > 0
        call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND, 3004, MRU4
        inc D$AddedMRUitems
    End_If

  ; Separator:
    If D$MRU1 > 0
        call 'USER32.InsertMenuA' D$FilePopUpHandle, M00_Exit, &MF_BYCOMMAND__&MF_SEPARATOR, 0 0
        inc D$AddedMRUitems
    End_If
ret


DeleteMRUmenu:
    call 'USER32.GetSubMenu' D$MenuHandle 0 | mov D$FilePopUpHandle eax
    While D$AddedMRUitems > 0
        call 'USER32.DeleteMenu' D$FilePopUpHandle, 16, &MF_BYPOSITION
        dec D$AddedMRUitems
    End_While
ret


; Push down (out) one MRU record to make leading room for a new File:

PushOneMRUrecordDown:
    mov esi MRU3, edi MRU4, ecx,(260*3)
    add esi 259 | add edi 259
    std | rep movsb | cld
ret

;;
 The recently saved file was already in the MRU list, but not in first Pos. We
 reorganise the List. Before calling, 'RecordMRU' Routine set esi to 'DestinationFile'
 and edi to MRU1/2/... We reuse 'DestinationFile', just in case the Name Cases would
 have been changed by user in between. So, at first, we arase the old record (this
 let room in first Pos). I do not use directely 'DestinationFile', but edi, instead,
 in case this function would be extended in the futur.
;;
RePosMRU:
    push esi, edi
      ; First arase the record at edi:
        std
            mov esi edi | add edi 259 | dec esi
            mov ecx esi | sub ecx MRU1
            rep movsb
        cld
    pop edi, esi
  ; then write new record at first Pos:
    mov edi MRU1, ecx 260 | rep movsb
ret

;;
  Adding a new File in the MRU List if it is not already inside, or update the range
  if already there. The file is the last compiled one ('DestinationFile').
;;
RecordMRU:
    mov esi DestinationFile | call CheckMRUFile
    mov ecx DestinationFile | While B$ecx <> 0 | inc ecx | End_While
    sub ecx DestinationFile

    mov esi DestinationFile
    push esi, ecx
        mov edi MRU1 | repe cmpsb | je L9>
L1: pop ecx, esi

    push esi, ecx
        mov edi MRU2 | repe cmpsb | jne L1>
            mov edi MRU2 | jmp L8>
L1: pop ecx, esi

    push esi, ecx
        mov edi MRU3 | repe cmpsb | jne L1>
            mov edi MRU3 | jmp L8>
L1: pop ecx, esi

    push esi, ecx
        mov edi MRU4 | repe cmpsb | jne L1>
            mov edi MRU4 | jmp L8>
  ; If here, it is a new file name:
L1: pop ecx, esi

    call PushOneMRUrecordDown
    mov esi DestinationFile, edi MRU1, ecx 260 | rep movsb | call SetMRUmenu | ret

  ; If here, the File is already Listed, but not at first Pos:
L8: pop ecx, esi
    call RePosMRU | call SetMRUmenu | ret

  ; If here, the File is already Listed at first Pos >>> OK, exit:
L9: pop ecx, esi | ret


; Arase one record if the File do not exist (example, if user deleted the file):

[CheckMRUpointer: ?]

CheckMRUFile:
    mov D$CheckMRUpointer esi
    call 'KERNEL32.FindFirstFileA' esi, FindFile
    push eax

        .If eax = &INVALID_HANDLE_VALUE
            mov eax 0, esi D$CheckMRUpointer, edi esi | add esi 260
            mov ecx MRU4 | sub ecx edi
            jecxz L1>
                rep movsb
L1:         mov ecx 260, al 0 | rep stosb

        .Else                                           ; All this is only to retrieve
            mov edi D$CheckMRUpointer                   ; the Name case (upper/lower),
            mov al 0, ecx 0-1 | repne scasb | dec edi   ; in case the user modified it
            mov esi FindFile.cFileName, ecx 0           ; since last session.
            While B$esi <> 0
                inc ecx | inc esi
            End_While
            sub edi ecx
            mov esi FindFile.cFileName | rep movsb

        .End_If

    pop eax | On eax <> &INVALID_HANDLE_VALUE, call 'KERNEL32.FindClose' eax
ret


CheckAllMRUFile:
    mov esi MRU1 | call CheckMRUFile
    mov esi MRU2 | call CheckMRUFile
    mov esi MRU3 | call CheckMRUFile
    mov esi MRU4 | call CheckMRUFile
ret


[MRUfileHasBeenDeleted: 'This File has been deleted or renamed Since last MRU update', 0]

[SelectedMRUFileName: B$ ? #260]

; Called from Main Message Loop with Menu ID in eax.

LoadRMUfile:
    push eax
        If eax = 3001
            mov esi MRU1
        Else_If eax = 3002
            mov esi MRU2
        Else_If eax = 3003
            mov esi MRU3
        Else_If eax = 3004
            mov esi MRU4
        End_If

        mov edi SelectedMRUFileName, ecx 260 | rep movsb

        call CheckAllMRUFile | call SetMRUmenu
        call ReInitUndo
    pop eax

    If eax = 3001
        mov esi MRU1
    Else_If eax = 3002
        mov esi MRU2
    Else_If eax = 3003
        mov esi MRU3
    Else_If eax = 3004
        mov esi MRU4
    End_If

    push esi
        mov edi SelectedMRUFileName, ecx 260 | rep cmpsb | je L1>
            mov eax MRUfileHasBeenDeleted | call MessageBox | pop esi | mov eax &FALSE | ret
L1: pop esi

    mov edi SaveFilter, ecx 260 | rep movsb

    mov edi SaveFilter
    While B$edi <> 0 | inc edi | End_While
    While B$edi <> '\' | dec edi | End_While

    push D$edi, edi
        mov B$edi 0
        call 'KERNEL32.SetCurrentDirectoryA' SaveFilter  ; says TRUE.
    pop edi, D$edi

    If eax = &TRUE
        call DirectMRUload
        call UpdateTitlesFromIncludeFiles
        call AskForRedraw
        mov eax &TRUE
    End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
; Loading the last MRU File on Startup, if 'LoadMRU' Flag is set On.

[LoadMRU: ?]

LoadLastMRUFile:
    .If D$MRU1 <> 0
        StoreNameOnly MRU1

        mov esi MRU1, edi SaveFilter
        While B$esi <> 0 | movsb | End_While

        call LastMRULoading

        If D$SourceLen > 0
            call SetPartialEditionFromPos | call EnableMenutems
            call LoadBookMarks
        End_If

    .End_If
ret
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


  ; Loading the last MRU File on Startup, if 'LoadMRU' Flag is set On.
; RWE1 start of drag and drop modification
[LoadMRU: ?]

LoadLastMRUFile:
    call ProcessCL
  ; eax returned pointing to command line filename in CLbuffer:
  ;showme eax
    ..If eax = 0
        .If D$MRU1 <> 0 ; Do this if no command line parameters i.e. no drag and drop file
            StoreNameOnly MRU1 ; Do this if 'autoreload last file on start up' selected

            mov esi MRU1, edi SaveFilter
            While B$esi <> 0 | movsb | End_While ; string MRU1 saved to Savefilter

            call LastMRULoading

            If D$SourceLen > 0 ; If the file includes assembly language source
                call SetPartialEditionFromPos | call EnableMenutems
                call LoadBookMarks
            End_If

        .End_If
    ..Else
        StoreNameOnly CLBuffer ; Do this if e.g. a file has been drag / dropped onto RosASM icon

        mov esi CLBuffer, edi SaveFilter
        While B$esi <> 0 | movsb | End_While ; string MRU1 saved to Savefilter

        call LastMRULoading

        If D$SourceLen > 0 ; If the file includes assembly language source
            call SetPartialEditionFromPos | call EnableMenutems
            call LoadBookMarks
        End_If
    ..End_If
ret

______________________________________________________________________________________________________________________________________________

[CLBuffer: B$ 0 #&MAXPATH] ; Stores any filename drag and dropped to RosASM icon on desktop

ProcessCL:
    call 'Kernel32.GetCommandLineA'
    mov edx eax, edi eax, ecx 0FF, al 0 | repne scasb
    mov esi edi, edi CLBuffer

    .While esi > edx
        dec esi
        .If B$esi = '"'
            dec esi
            While B$esi <> '"' | dec esi | End_While
            mov eax edi
            If esi <> edx
                inc esi | While B$esi <> '"' | movsb | End_While
            Else
                mov eax 0
            End_If
            mov B$edi 0 | ret

        .Else_If B$esi = '.'
            While B$esi <> ':' | dec esi | End_While
            dec esi | mov eax edi
            While B$esi <> 0 | movsb | End_While
            mov B$edi 0 | ret

        .End_If
    .End_While

    mov eax 0
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[ActualDir: B$ ? #&MAXPATH] [ActualDirPointer: ?]
[DllScannerDir: B$ ? #&MAXPATH]

Proc GetDirectory:
    Argument @Dir

        call 'KERNEL32.GetCurrentDirectoryA' &MAXPATH, D@Dir
EndP


Proc SetDirectory:
    Argument @Dir

        mov eax D@Dir | On D$eax <> 0, call 'KERNEL32.SetCurrentDirectoryA' D@Dir
EndP




