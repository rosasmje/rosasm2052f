TITLE Loaders
________________________________________________________________________________________
________________________________________________________________________________________

                         ; Loaders for non-edited Resources
________________________________________________________________________________________
________________________________________________________________________________________

[WaveType: U$ 'WAVE'    AviType: 'AVI'] [WAVETYPELEN 4    AVITYPELEN 3] ; 'SOUND'

; read new open PE Resources:

ReadRosAsmWaves:
    mov edi WaveList, eax 0, ecx 300 | rep stosd
    mov edi WaveType, edx WAVETYPELEN | call SearchResourceNamedType | On eax = 0, ret
    mov D$WaveListPtr WaveList,  ebx WaveListPtr | call ReadResourcesRecord
ret

ReadRosAsmAvis:
    mov edi AviList, eax 0, ecx 300 | rep stosd
    mov edi AviType, edx AVITYPELEN | call SearchResourceNamedType | On eax = 0, ret
    mov D$AviListPtr AviList,  ebx AviListPtr | call ReadResourcesRecord
    mov esi AviList
ret

ReadRosAsmRCs:
    mov edi RcDataList, eax 0, ecx 300 | rep stosd
    mov ebx &RT_RCDATA | call SearchResourceType | On eax = 0, ret
    mov D$RcDataListPtr RcDataList,  ebx RcDataListPtr | call ReadResourcesRecord
ret


ReadRosAsmCursors:
    mov edi CursorList, eax 0, ecx 300 | rep stosd
    mov ebx &RT_CURSOR | call SearchResourceType | On eax = 0, ret
    mov D$CursorListPtr CursorList,  ebx CursorListPtr | call ReadResourcesRecord
ret


ReadRosAsmGroupCursors:
    mov edi GroupCursorList, eax 0, ecx 300 | rep stosd
    mov ebx RT_GROUP_CURSOR | call SearchResourceType | On eax = 0, ret
    mov D$GroupCursorListPtr GroupCursorList,  ebx GroupCursorListPtr
    call ReadResourcesRecord
ret

ReadRosAsmIcons:
    mov edi IconList, eax 0, ecx 300 | rep stosd

    mov ebx &RT_ICON | call SearchResourceType | On eax = 0, ret

    mov D$IconListPtr IconList,  ebx IconListPtr | call ReadResourcesRecord

  ; Arase the First Icon, Which is the Main One (elsewhere...)
    VirtualFree D$IconList+4

    mov esi IconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
    On D$IconListPtr > IconList, sub D$IconListPtr 12
ret


ReadRosAsmGroupIcons:
    mov edi GroupIconList, eax 0, ecx 300 | rep stosd
    mov ebx RT_GROUP_ICON | call SearchResourceType | On eax = 0, ret
    mov D$GroupIconListPtr GroupIconList,  ebx GroupIconListPtr
    call ReadResourcesRecord

  ; Arase the First GroupIcon, Which is the Main One (elsewhere...)
    VirtualFree D$GroupIconList+4

    mov esi GroupIconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
    On D$GroupIconListPtr > GroupIconList, sub D$GroupIconListPtr 12
ret

[ResourcePointersSecurity: ? #10]

; Each Record of these Lists is: ID / Ptr / Size.

[CursorList: ? #MAXCURSOR]      [CursorListPtr: CursorList]
[GroupCursorList: ? #MAXCURSOR] [GroupCursorListPtr: GroupCursorList]
[IconList: ? #MAXICON]          [IconListPtr: IconList]
[MenuList: ? #MAXMENU]          [MenuListPtr: MenuList]
[DialogList: ? #MAXDIALOG]      [DialogListPtr: DialogList]
[StringsList: ? #MAXSTRINGS]    [StringsListPtr: StringsList]
[GroupIconList: ? #MAXICON]     [GroupIconListPtr: GroupIconList]
[WaveList: ? #MAXWAVE]          [WaveListPtr: WaveList]
[AviList: ? #MAXAVI]            [AviListPtr: AviList]
[RCdataList: ? #MAXRCDATA]      [RCdataListPtr: RCdataList]
[BitMapList: ? #MAXBITMAP]      [BitMapListPtr: BitMapList]
[OtherList: 0    OtherListPtr: 0]

[FontFilesTitle:     B$ 'Choose a Font File', 0
 WaveFilesFilters:   B$ 'Wave Files' 0 '*.wav' 0 0
 WaveFilesTitle:     B$ 'Chose a Wave File', 0
 AviFilesFilters:    B$ 'Avi Files' 0   '*.avi', 0 0
 AviFilesTitle:      B$ 'Choose an Avi File', 0
 RCDataFilesFilters: B$ 'AnyThing' 0  '*.*' 0 0
 RCDataFilesTitle:   B$ 'Choose a File', 0
 CursorFilesFilters: B$ 'Cursor Files' 0   '*.cur' 0 0
 CursorFilesTitle:   B$ 'Choose a Cursor File' 0
 IconFilesFilters: B$ 'Icon Files' 0   '*.Ico' 0 0
 IconFilesTitle:   B$ 'Choose an Icon File' 0
 DialogFilesFilters: B$ 'Dlg Files', 0, '*.dlg', 0, 0
 BinDialogFilesFilters: B$ 'bdf Files', 0, '*.bdf', 0, 0
 BinMenuFilesFilters: B$ 'bmf Files', 0, '*.bmf', 0, 0
 DialogFilesTitle: B$ 'Choose a Dialog Template File' 0
 MenuFilesTitle: B$ 'Choose a Menu Template File' 0]

[OtherChoosenFile: B$ ? #260] [OtherFileFilter: B$ ? #260]
[OtherSourceFilterPtr: B$ ? #260][OtherSaveFilter: ? #260]

[OtherOpenStruc: len
 OtherhwndFileOwner: 0  OtherhInstance: 0
 OtherFilesFilters: 0  OtherFileFilter  260
 OtherSourceFilterPtr  OtherSaveFilter  260  OtherChoosenFile  260  0
 OpenOtherFileTitle: 0  OtherFlags: 0281804 ; for read
 0  0  0  0  0]                   ; 0280006 : for write

[OtherSourceHandle: ?    OtherFileLen: ?    OtherFilePtr: ?]

ReadRCData:
    mov D$OtherFilesFilters RCDataFilesFilters
    mov D$OpenOtherFileTitle RCDataFilesTitle
    mov D$OtherList RcDataList | move D$OtherListPtr D$RCDataListPtr
    call ReadOtherFile
        If D$OtherSaveFilter = 0
            mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If
        call AskForResID | add D$RCDataListPtr 12
    call CloseOtherFilesRead
ret


SaveRcData:
    If D$RCDataList = 0
        call 'USER32.MessageBoxA' D$hwnd, NoRcData, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    mov D$WhatDialogListPtr RcDataList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    call 'USER32.CreateDialogParamA' D$hInstance, 1000, &NULL, EmptyProc, &NULL
    mov D$EmptyDialogHandle eax


    .While B$OkDialogFlag = &FALSE
        call SetRcDataTitle | call ShowHexa
        call SetNextChoiceID
        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar, D$hwnd,
                                              ChoiceDialogBoxProc, RcDataList
        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < RCDataList
            add D$WhatDialogListPtr 12
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
                sub D$WhatDialogListPtr 12
                call SetNextChoiceID
            End_If
        .End_If
   .End_While

    call 'USER32.MessageBoxA' D$hwnd, SaveRc, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL

    push eax
        call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    pop eax

   .If eax = &IDYES
        mov D$OpenDlg.lpstrFilter RcFilesFilters
        call 'Comdlg32.GetSaveFileNameA' OpenDlg
        mov D$OpenDlg.lpstrFilter DlgFilesFilters
        On eax = &FALSE, ret

        call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                                &FILE_SHARE_READ, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        If eax = &INVALID_HANDLE_VALUE
            mov eax D$BusyFilePtr | call MessageBox | ret
        End_If

        mov D$DestinationHandle eax, D$NumberOfReadBytes 0
        mov esi D$WhatDialogListPtr, ecx D$esi+4, esi D$esi

        call 'KERNEL32.WriteFile' D$DestinationHandle, esi, ecx, NumberOfReadBytes  0

        call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
   .End_If

L9: ret


[NoRcData: 'No RC Data Resources in this PE', 0]
[KillRc: 'Delete this Rc Data Resource?', 0]
[SaveRc: 'Save this RC Resource to Disk?', 0]
[IdTitle: 'RC Data ID: ' IdTitleID: '       ', 0]

SetRcDataTitle:
    mov eax D$WhatDialogListPtr, eax D$eax-4, edi IdTitleID
    call TransDwordToAsciiDecimal
    call 'USER32.SendMessageA' D$EmptyDialogHandle &WM_SETTEXT 0 IdTitle
ret


ShowHexa:
    mov eax D$WhatDialogListPtr, esi D$eax, ecx D$eax+4
    shl ecx 3 | On ecx > 0FF00, mov ecx 0FF00
    push ecx, esi
        VirtualAlloc TempoMemPointer ecx | mov edi D$TempoMemPointer
    pop esi, ecx
    shr ecx 3 | sub ecx 001111 | or ecx 001111
    push edi
L0:     lodsb | shr al 4 | add al '0' | On al > '9', add al 7 | stosb
        dec esi | lodsb | and al 0F | add al '0' | On al > '9', add al 7 | stosb
        mov al ' ' | stosb
        test ecx 00111 | jnz L1>
            mov al ' ' | stosb
        test ecx 001111 | jnz L1>
           sub esi 16
           push ecx
               mov ecx 16
T0:            lodsb | On al < ' ', mov al '.' | stosb | loop T0<
           pop ecx
           mov ax 0A0D | stosw
L1:     loop L0<
    pop edi

    call 'USER32.SetDlgItemTextA' D$EmptyDialogHandle, 100, edi
ret


DeleteRcData:
    If D$RCDataList = 0
        call 'USER32.MessageBoxA' D$hwnd, NoRcData, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    mov D$WhatDialogListPtr RcDataList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4
; Tag Dialog 1000
    call 'USER32.CreateDialogParamA' D$hInstance, 1000, &NULL, EmptyProc, &NULL
    mov D$EmptyDialogHandle eax

    .While B$OkDialogFlag = &FALSE
        call SetRcDataTitle | call ShowHexa
        call SetNextChoiceID
        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar, D$hwnd, ChoiceDialogBoxProc, RcDataList

        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < RCDataList
            add D$WhatDialogListPtr 12
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
                sub D$WhatDialogListPtr 12 ;| mov esi D$WhatDialogListPtr
                call SetNextChoiceID
            End_If
        .End_If
   .End_While

   call 'USER32.MessageBoxA' D$hwnd, KillRc, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
   If eax = &IDYES
        sub D$WhatDialogListPtr 4
        mov esi D$WhatDialogListPtr, edi esi | add esi 12
        mov eax D$WhatDialogListPtr | sub eax RCDataList | shr eax 2 | mov ecx 300 | sub ecx eax
        rep movsd
        sub D$RCDataListPtr 12
   End_If

L9: call 'User32.EndDialog' D$EmptyDialogHandle &NULL
ret


ReadWaveFile:
    mov D$OtherFilesFilters WaveFilesFilters
    mov D$OpenOtherFileTitle WaveFilesTitle
    mov D$OtherList WaveList | move D$OtherListPtr D$WaveListPtr
    call ReadOtherFile
        If D$OtherSaveFilter = 0
            mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If
        call AskForResID | add D$WaveListPtr 12
    call CloseOtherFilesRead
ret


[KillWave: 'Delete this Wave Resource?', 0
 NoWave: 'No Wave Resources in this PE', 0]
[TempoWaveFileHandle: 0  TempoWaveFile: 'Wave$$$.Wav' 0]

DeleteWave:
    If D$WaveList = 0
        call 'USER32.MessageBoxA' D$hwnd, NoWave, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    mov D$WhatDialogListPtr WaveList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        call 'KERNEL32.CreateFileA' TempoWaveFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        mov D$TempoWaveFileHandle eax
        mov esi D$WhatDialogListPtr
        call 'KERNEL32.WriteFile' D$TempoWaveFileHandle D$esi D$esi+4 NumberOfReadBytes &NULL
        call 'Kernel32.CloseHandle' D$TempoWaveFileHandle
        call 'WINMM.PlaySound' TempoWaveFile &NULL  &SND_ASYNC__&SND_FILENAME
        call SetNextChoiceID
        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar, D$hwnd, ChoiceDialogBoxProc, WaveList
        call 'WINMM.PlaySound' &NULL &NULL &NULL
        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < WaveList
            add D$WhatDialogListPtr 12
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12 | mov esi D$WhatDialogListPtr
              call SetNextChoiceID
            End_If
        .End_If
   .End_While

    call 'USER32.MessageBoxA' D$hwnd, KillWave, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
    If eax = &IDYES
        sub D$WhatDialogListPtr 4
        mov esi D$WhatDialogListPtr, edi esi | add esi 12
        mov eax D$WhatDialogListPtr | sub eax WaveList | shr eax 2 | mov ecx 300 | sub ecx eax
        rep movsd
        sub D$WaveListPtr 12
    End_If

L9: call 'KERNEL32.DeleteFileA' TempoWaveFile
ret


ReadAviFile:
    mov D$OtherFilesFilters AviFilesFilters
    mov D$OpenOtherFileTitle AviFilesTitle
    mov D$OtherList AviList | move D$OtherListPtr D$AviListPtr
    call ReadOtherFile
        If D$OtherSaveFilter = 0
            mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

        call AskForResID | add D$AviListPtr 12
    call CloseOtherFilesRead
ret


[TempoCursorMem: ?    CursorHotSpot: ?]
[BadCurFile: 'Bad or multiple Cursor(s) file', 0
 BadIcoFile: 'Bad or multiple Icon(s) file', 0]

ReadCursor:
    mov D$OtherFilesFilters CursorFilesFilters
    mov D$OpenOtherFileTitle CursorFilesTitle
    mov D$OtherList CursorList | move D$OtherListPtr D$CursorListPtr

    move D$OtherhwndFileOwner D$hwnd, D$OtherhInstance D$hInstance

    mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
        If D$OtherSaveFilter = 0
            mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

    call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | ret  ; return to caller of caller
    Else
      mov D$OtherSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize'  eax 0
    mov D$OtherFileLen eax
    VirtualAlloc OtherFilePtr eax

    mov D$NumberOfReadBytes 0
    call 'KERNEL32.ReadFile' D$OtherSourceHandle D$OtherFilePtr,
                            D$OtherFileLen NumberOfReadBytes 0

    mov eax D$OtherFilePtr
    If D$eax+2 <> 010002
        call 'USER32.MessageBoxA' D$hwnd, BadCurFile, Argh, &MB_OK+&MB_SYSTEMMODAL

        VirtualFree D$OtherFilePtr | ret
    End_If

  ; We write both the RT_CURSOR and the RT_GROUP_CURSOR:
    VirtualAlloc TempoCursorMem D$OtherFileLen
    VirtualAlloc TempoMemPointer 20
    mov edi D$GroupCursorListPtr, eax D$TempoMemPointer, D$edi+4 eax, D$edi+8 20

    mov edi eax                                       ; edi > RT_GROUP_CURSOR mem

    mov esi D$OtherFilePtr

    mov ecx 20 | rep movsb
    move D$CursorHotSpot D$edi-10                     ; x/y hot spot
    push edi

    mov esi D$OtherFilePtr | add esi 016
    sub D$OtherFileLen 012 | mov ecx D$OtherFileLen
    mov eax D$CursorHotSpot                           ; PE cursors need the hot spot in data:
    mov edi D$TempoCursorMem | stosd | rep movsb

    VirtualFree D$OtherFilePtr

    move D$OtherFilePtr D$TempoCursorMem

    call AskForResID | call CloseOtherFilesRead

    mov esi D$OtherListPtr | lodsd                    ; rewrite ID to GroupCursor:
    mov edi D$GroupCursorListPtr | stosd
    pop edi
    mov W$edi-2 ax                           ; write the ID in RT_GROUP_CURSOR records
    add D$edi-6 4                            ; +4 size because upper added hot Spot in data

    add D$CursorListPtr 12 | add D$GroupCursorListPtr 12
ret


[TempoIconMem: ?]

ReadIcon:
    mov D$OtherFilesFilters IconFilesFilters
    mov D$OpenOtherFileTitle IconFilesTitle
    mov D$OtherList IconList | move D$OtherListPtr D$IconListPtr

    move D$OtherhwndFileOwner D$hwnd, D$OtherhInstance D$hInstance

    mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
        If D$OtherSaveFilter = 0
            mov D$OtherList 0,  D$OtherListPtr 0 | ret
        End_If

    call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | ret  ; return to caller of caller
    Else
      mov D$OtherSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize'  eax 0
    mov D$OtherFileLen eax
    VirtualAlloc OtherFilePtr eax

    mov D$NumberOfReadBytes 0
    call 'KERNEL32.ReadFile' D$OtherSourceHandle D$OtherFilePtr,
                            D$OtherFileLen NumberOfReadBytes 0

    mov eax D$OtherFilePtr
    If D$eax+2 <> 010001
        call 'USER32.MessageBoxA' D$hwnd, BadIcoFile, Argh, &MB_OK+&MB_SYSTEMMODAL

        VirtualFree D$OtherFilePtr | ret

    End_If

  ; We write both the RT_ICON and the RT_GROUP_ICON:
    VirtualAlloc TempoIconMem D$OtherFileLen
    VirtualAlloc TempoMemPointer 20
    mov edi D$GroupIconListPtr, eax D$TempoMemPointer, D$edi+4 eax, D$edi+8 20

    mov edi eax                                       ; edi > RT_GROUP_ICON mem

    mov esi D$OtherFilePtr

    mov ecx 20 | rep movsb
    push edi

    mov esi D$OtherFilePtr | add esi 016
    sub D$OtherFileLen 012 | mov ecx D$OtherFileLen
    mov edi D$TempoIconMem | rep movsb

    VirtualFree D$OtherFilePtr

    move D$OtherFilePtr D$TempoIconMem

    mov B$OnIconLoad &TRUE
        call AskForResID | call CloseOtherFilesRead
    mov B$OnIconLoad &FALSE

    mov esi D$OtherListPtr | lodsd                    ; rewrite ID to GroupCursor:
    mov edi D$GroupIconListPtr | stosd
    pop edi
    mov W$edi-2 ax                           ; write the ID in RT_GROUP_CURSOR records
    add D$edi-6 4                            ; +4 size because upper added hot Spot in data

    add D$IconListPtr 12 | add D$GroupIconListPtr 12
ret


[AnimateHandle: 0    TempoAviFileHandle: 0
 AnimateClass: 'SysAnimate32' 0    TempoAviFile: 'Avi$$$.avi', 0]
[KillAvi: 'Delete this Avi Resource?', 0
 NoAvi: 'No Avi Resources in this PE', 0]

[EmptyDialog: D$ 0900408C2 0    ; Style
 U$ 0 0 0 0DC 0C8              ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 '' 0                          ; Title
 08 'Helv' 0]                  ; Font

[EmptyDialogHandle: ?]

Proc EmptyProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        mov eax &TRUE

    Else_If D@Message = &WM_CLOSE
        mov B$OkDialogFlag &TRUE
       ; call 'User32.EndDialog' D$ChoiceDialogBoxHandle 0
        call 'User32.DestroyWindow' D@Adressee

    Else
        popad | mov eax &FALSE | jmp L9>

    End_If

    popad | mov eax &TRUE

L9: EndP


DeleteAviFile:
    If D$AviList = 0
        call 'USER32.MessageBoxA' D$hwnd, NoAvi, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    call 'USER32.CreateDialogIndirectParamA' D$hinstance EmptyDialog &NULL Emptyproc 0
    mov D$EmptyDialogHandle eax

    call 'USER32.CreateWindowExA' &WS_SIZEBOX__&WS_DLGFRAME  AnimateClass &NULL,  ; &WS_Border  &WS_BORDER
                                 &ACS_AUTOPLAY__&WS_VISIBLE__&WS_CHILD__&ACS_CENTER,
                                 4 4 200 100 D$EmptyDialogHandle,
                                 0 D$hInstance 0
    mov D$AnimateHandle eax

    mov D$WhatDialogListPtr AviList,  D$OkDialogFlag &FALSE
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        call 'KERNEL32.CreateFileA' TempoAviFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        mov D$TempoAviFileHandle eax
        mov esi D$WhatDialogListPtr | mov ecx D$esi+4
        call 'KERNEL32.WriteFile' D$TempoAviFileHandle D$esi ecx NumberOfReadBytes &NULL
        call 'Kernel32.CloseHandle' D$TempoAviFileHandle
        call 'USER32.SendMessageA' D$AnimateHandle &ACM_OPEN  &NULL TempoAviFile
        call SetNextChoiceID
        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar, D$hwnd, ChoiceDialogBoxProc, AviList
        call 'USER32.SendMessageA' D$AnimateHandle &ACM_STOP  &NULL &NULL
        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < AviList
            add D$WhatDialogListPtr 12
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12 | mov esi D$WhatDialogListPtr
              call SetNextChoiceID
            End_If
        .End_If
    .End_While

    call 'USER32.MessageBoxA' D$hwnd, KillAvi, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
    If eax = &IDYES
        sub D$WhatDialogListPtr 4
        mov esi D$WhatDialogListPtr, edi esi | add esi 12
        mov eax D$WhatDialogListPtr | sub eax AviList | shr eax 2 | mov ecx 300 | sub ecx eax
        rep movsd
        sub D$AviListPtr 12
    End_If

L9: call 'USER32.DestroyWindow' D$AnimateHandle
    call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    call 'KERNEL32.DeleteFileA' TempoAviFile
ret


[NoCursor: 'No Cursor Resources in this PE' 0
 KillCursor: 'Delete this Cursor?' 0
 KillIcon: 'Delete this Icon?' 0
 TempoCursorFile: 'Cursor.$$$' 0
 CurDataPtr: W$ 0   D$ 016]

[UserCursorHandle: ?    TempoCursorFileHandle: ?]

DeleteCursor:
    If D$CursorList = 0
        call 'USER32.MessageBoxA' D$hwnd, NoCursor, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    mov D$WhatDialogListPtr CursorList,  D$OkDialogFlag &FALSE, D$EmptyDialogHandle 0
    add D$WhatDialogListPtr 4

    .While B$OkDialogFlag = &FALSE
        On D$EmptyDialogHandle <> 0, call 'User32.EndDialog' D$EmptyDialogHandle &NULL
        call 'USER32.CreateDialogIndirectParamA' D$hinstance EmptyDialog &NULL Emptyproc 0
            mov D$EmptyDialogHandle eax
        call 'KERNEL32.CreateFileA' TempoCursorFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        mov D$TempoCursorFileHandle eax
        mov eax D$WhatDialogListPtr | sub eax CursorList | add eax GroupCursorList
        mov ecx D$eax | sub D$ecx+14 4 | mov ecx D$ecx+14  ; how much -4 for added Hot Spot
        push eax, ecx
            call 'KERNEL32.WriteFile' D$TempoCursorFileHandle D$eax 010 NumberOfReadBytes &NULL
            call 'KERNEL32.WriteFile' D$TempoCursorFileHandle CurDataPtr 6 NumberOfReadBytes &NULL
            mov esi D$WhatDialogListPtr, esi D$esi | add esi 4
        pop ecx, eax
        mov eax D$eax | add D$eax+14 4                     ; restore our 'how much'
        call 'KERNEL32.WriteFile' D$TempoCursorFileHandle esi ecx NumberOfReadBytes &NULL
        call 'Kernel32.CloseHandle' D$TempoCursorFileHandle
        call 'User32.LoadCursorFromFileA' TempoCursorFile
        If D$UserCursorHandle > 0
            pushad
               call 'USER32.DestroyCursor' D$UserCursorHandle
            popad
        End_If
        mov D$UserCursorHandle eax
            call 'USER32.GetDC' D$EmptyDialogHandle
            push eax
                call 'User32.DrawIcon' eax 10 10 D$UserCursorHandle
            pop eax
            call 'USER32.ReleaseDC' D$EmptyDialogHandle eax
            call SetNextChoiceID
        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar,
                                             D$hwnd, ChoiceDialogBoxProc, CursorList

        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < CursorList
            add D$WhatDialogListPtr 12
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12
              call SetNextChoiceID
            End_If
        .End_If
   .End_While

   call 'USER32.MessageBoxA' D$hwnd, KillCursor, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
   If eax = &IDYES
       sub D$WhatDialogListPtr 4
       mov esi D$WhatDialogListPtr, edi esi | add esi 12
       mov ebx D$edi                                                ; ebx = ID
       mov eax D$WhatDialogListPtr | sub eax CursorList | shr eax 2
       mov ecx MAXCURSOR | sub ecx eax | rep movsd
       sub D$CursorListPtr 12

       mov esi GroupCursorList, ecx MAXCURSOR
       While D$esi <> ebx
           add esi 12 | sub ecx 3
       End_While
       mov edi esi | add esi 12  | rep movsd
       sub D$GroupCursorListPtr 12
   End_If

L9: call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    call 'KERNEL32.DeleteFileA' TempoCursorFile
ret


[UserIconHandle: ?]

[ICONINFO:
 ICONINFO_fIcon: D$ &TRUE
 ICONINFO_xHotspot: D$ 0
 ICONINFO_yHotspot: D$ 0
 ICONINFO_hbmMask: D$ 0
 ICONINFO_hbmColor: D$ 0]


DeleteIcon:
    If D$IconList = 0
        call 'USER32.MessageBoxA' D$hwnd, NoIcon, Argh, &MB_OK+&MB_SYSTEMMODAL | ret
    End_If

    mov D$WhatDialogListPtr IconList,  D$OkDialogFlag &FALSE, D$EmptyDialogHandle 0
    add D$WhatDialogListPtr 4

   ; ID / Pointer / Size.

    .While B$OkDialogFlag = &FALSE
        On D$EmptyDialogHandle <> 0, call 'User32.EndDialog' D$EmptyDialogHandle &NULL
        call 'USER32.CreateDialogIndirectParamA' D$hinstance EmptyDialog &NULL Emptyproc 0
            mov D$EmptyDialogHandle eax
        call 'KERNEL32.CreateFileA' TempoCursorFile &GENERIC_WRITE, 0, 0,
                                    &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
        mov D$TempoCursorFileHandle eax
        mov eax D$WhatDialogListPtr | sub eax IconList | add eax GroupIconList

      ; May hang down there here with an ecx = 0. So, temporary:
        On D$eax = 0, jmp L9>>

        mov ecx D$eax | mov ecx D$ecx+14
        push ecx
            call 'KERNEL32.WriteFile' D$TempoCursorFileHandle D$eax 010 NumberOfReadBytes &NULL
            call 'KERNEL32.WriteFile' D$TempoCursorFileHandle CurDataPtr 6 NumberOfReadBytes &NULL
            mov esi D$WhatDialogListPtr
        pop ecx
        call 'KERNEL32.WriteFile' D$TempoCursorFileHandle D$esi D$esi+4 NumberOfReadBytes &NULL
        call 'Kernel32.CloseHandle' D$TempoCursorFileHandle
        call 'User32.LoadCursorFromFileA' TempoCursorFile
        If D$UserCursorHandle > 0
            pushad
               call 'USER32.DestroyCursor' D$UserCursorHandle
            popad
        End_If
        mov D$UserCursorHandle eax
            call 'USER32.GetDC' D$EmptyDialogHandle
            push eax
                call 'User32.DrawIcon' eax 10 10 D$UserCursorHandle
            pop eax
            call 'USER32.ReleaseDC' D$EmptyDialogHandle eax
            call SetNextChoiceID

        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar,
                                             D$hwnd, ChoiceDialogBoxProc, IconList

        .If D$OkDialogFlag = &VK_ESCAPE
            jmp L9>>
        .Else_If D$WhatDialogListPtr < IconList
            add D$WhatDialogListPtr 12
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
              sub D$WhatDialogListPtr 12
              call SetNextChoiceID
            End_If
        .End_If
   .End_While

    call 'USER32.MessageBoxA' D$hwnd, KillIcon, Sure, &MB_ICONQUESTION+&MB_YESNO+&MB_SYSTEMMODAL
    If eax = &IDYES
        sub D$WhatDialogListPtr 4
        mov esi D$WhatDialogListPtr, edi esi | add esi 12
        mov ebx D$edi                                                ; ebx = ID
        mov eax D$WhatDialogListPtr | sub eax IconList | shr eax 2
        mov ecx MAXICON | sub ecx eax | rep movsd
        sub D$IconListPtr 12

        mov esi GroupIconList, ecx MAXICON
        While D$esi <> ebx
            add esi 12 | sub ecx 3
        End_While
        mov edi esi | add esi 12  | rep movsd
        sub D$GroupIconListPtr 12
    End_If

L9: call 'User32.EndDialog' D$EmptyDialogHandle &NULL
    call 'KERNEL32.DeleteFileA' TempoCursorFile
ret


ReadOtherFile:
    move D$OtherhwndFileOwner D$hwnd, D$OtherhInstance D$hInstance

    mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc
      On D$OtherSaveFilter = 0,  ret

  ; Loading the entire file in memory:
    On D$OtherSourceHandle > 0, call 'KERNEL32.CloseHandle' D$OtherSourceHandle
    mov D$OtherSourceHandle 0

    call 'KERNEL32.CreateFileA' OtherSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | ret  ; return to caller of caller
    Else
        mov D$OtherSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize'  eax 0 | mov D$OtherFileLen eax

    VirtualAlloc OtherFilePtr eax

    mov D$NumberOfReadBytes 0
    call 'KERNEL32.ReadFile' D$OtherSourceHandle, D$OtherFilePtr,
                            D$OtherFileLen, NumberOfReadBytes 0
ret


CloseOtherFilesRead:
    On D$OtherSourceHandle > 0, call 'KERNEL32.CloseHandle' D$OtherSourceHandle
    mov D$OtherSourceHandle 0
ret



AskForResID:
    call 'USER32.DialogBoxIndirectParamA' D$hinstance, OtherIdTemplate, 0, OtherIDProc, 0
ret


[OtherEditHandle: 0    OtherID: '                   ', 0]

[OnIconLoad: ?]

Proc OtherIDProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If D@Message = &WM_COMMAND
       ..If D@wParam = &IDCANCEL
            call 'User32.EndDialog' D@Adressee 0
       ..Else_If D@wParam = &IDOK
           call 'User32.GetDlgItem' D@Adressee 3 | mov D$OtherEditHandle eax
           call 'User32.SendMessageA' D$OtherEditHandle &WM_GETTEXTLENGTH 0 0 | inc eax
           call 'User32.SendMessageA' D$OtherEditHandle &WM_GETTEXT eax OtherID
           TranslateAsciiToDword OtherID
           If eax > 0FFFF
             mov eax D$IdTooBigPtr | call MessageBox
           Else_If eax < 1
             mov eax D$IdTooSmallPtr | call MessageBox
           Else
             mov edi D$OtherListPtr | stosd
             mov eax D$OtherFilePtr | stosd
             mov eax D$OtherFileLen | stosd
             call 'User32.EndDialog' D@Adressee 0
           End_If
       ..End_If

    .Else_If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'User32.GetDlgItem' D@Adressee 3
        call 'User32.SendMessageA' eax &EM_SETLIMITTEXT 5  0
           mov esi D$OtherListPtr | On esi > D$OtherList, sub esi 12
           If D$esi = 0
             mov eax 1
             On B$OnIconLoad = &TRUE, inc eax
           Else
             lodsd | inc eax
           End_If
           call 'USER32.SetDlgItemInt' D@Adressee 3 eax 0
    .Else
       popad | mov eax &FALSE | jmp L9>

    .End_If

    popad | mov eax &TRUE

L9: EndP


[OtherIdTemplate: D$ 090C408C2 0    ; Style
 U$ 03 0 0 0B9 018             ; Dim
 0                             ; no Menu
 '' 0                          ; Class
 'What ID number for new Resource?' 0 ; Title
 08 'Helv' 0]                  ; Font

[OID0: D$ 050000000 0         ; Style
 U$ 07E 03 038 013             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[OID1: D$ 050000000 0         ; Style
 U$ 03 03 038 013              ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data

[OID2: D$ 050802000 0         ; Style
 U$ 040 05 038 0F              ; Dim
 03                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


