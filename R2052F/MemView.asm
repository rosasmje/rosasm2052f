TITLE MemView

; Maintainer: Guga june the 29th of 2006

____________________________________________________________________________________________

; Routine for: Menu > Tools > 'View RosAs mMems':

[MemTableView: D$ ? #(MEMTABLESIZE*5)]
[MemTableViewEnd: ?]
[MemTableViewTitle: 'VirtualAlloc Mems actually commited to RosAsm:', 0]


[IDD_VIEWMEMWARNINGMSG 10000]
[IDC_DISPLAYMEMWARNING 10]
[IDC_DISPLAYMEMDATA 20]
[IDC_DUMPMEM 30]
[IDC_SAVEMEMREPORT 35]
[IDC_DISPLAYMEMINFO 40]

ViewRosAsmMems:
    call 'USER32.DialogBoxParamA' D$hinstance, IDD_VIEWMEMWARNINGMSG, &NULL,
                                  MemViewWarning, &NULL
ret


[DumpMemWarnMsg: B$ 0 #256]

_______________________________________________________

; Tag Dialog 10000

Proc MemViewWarning:
    Arguments @Adressee, @Message, @wParam, @lParam

     pushad


    ..If D@Message = &WM_INITDIALOG
       ; Get the handle of the Static Image:
        call 'USER32.GetDlgItem' D@Adressee IDC_LIBWARNING_SHOWICON
        mov D$LibWarningStaticImage eax
        call 'USER32.LoadBitmapA' D$hInstance, IDB_LIBWARNING_BITMAP
        mov D$LibScanhIcon eax

        call 'USER32.SendMessageA' D$LibWarningStaticImage, &STM_SETIMAGE,
                                   &IMAGE_BITMAP, D$LibScanhIcon
        call 'USER32.SendDlgItemMessageA' D@Adressee, IDC_DISPLAYMEMWARNING,
                                          &WM_SETTEXT, 0, DumpMemWarnMsg
        call 'USER32.SendDlgItemMessageA' D@Adressee IDC_DISPLAYMEMWARNING,
                                          &WM_GETTEXT, 0, DumpMemWarnMsg

        call DisplayMemData D@Adressee
        call DisplayMemInfo D@Adressee

    ..Else_If D@Message = &WM_CLOSE
        call ClearBuffer MemTableView (MEMTABLESIZE*5*4)
       ; call ClearBuffer MemTable (MEMTABLESIZE*4)
        call ClearBuffer DumpMemWarnMsg 256
        call 'User32.EndDialog' D@Adressee &NULL

    ..Else_If D@Message = &WM_COMMAND
        .If D@wParam = &IDOK
            call ClearBuffer MemTableView (MEMTABLESIZE*5*4)
          ;  call ClearBuffer MemTable (MEMTABLESIZE*4)
            call ClearBuffer DumpMemWarnMsg 256
            call 'User32.EndDialog' D@Adressee &NULL
        .Else_If D@wParam = IDC_DUMPMEM
            mov eax MemTableEnd
            sub eax MemTable
            call DumpMemory D@Adressee, MemTable, eax
        .Else_If D@wParam = IDC_SAVEMEMREPORT
            call SaveMemoryReport D@Adressee, MemTableView, D$MemReportInfoSize
        .End_If

    ..Else

       popad | mov eax &FALSE | ExitP

    ..End_If

    popad
    mov eax &TRUE
EndP

_______________________________________________________

[DumpMemFileTitle: B$ 'Save Dumped memory as...', 0]
[MemSaveFileFilter: B$ 'RosAsm Memory File (*.mem)', 0  '*.mem', 0 0]
[MemSaveFilter: "Report" 0 #(&MAX_PATH-6)]

[DumpMem:
 DumpMem.lStructSize: D$ Len
 DumpMem.hwndOwner: D$ 0
 DumpMem.hInstance: D$ 0
 DumpMem.lpstrFilter: D$ MemSaveFileFilter
 DumpMem.lpstrCustomFilter: D$ CustomLibFileFilter
 DumpMem.nMaxCustFilter: D$ 260
 DumpMem.nFilterIndex: D$ 1
 DumpMem.lpstrFile: D$ MemSaveFilter
 DumpMem.nMaxFile: D$ 260
 DumpMem.lpstrFileTitle: D$ ChoosenLibFile
 DumpMem.nMaxFileTitle: D$ 260
 DumpMem.lpstrInitialDir: D$ 0
 DumpMem.lpstrTitle: D$ DumpMemFileTitle
 DumpMem.Flags: D$ &OFN_EXPLORER__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
 DumpMem.nFileOffset: W$ 0
 DumpMem.nFileExtension: W$ 0
 DumpMem.lpstrDefExt: D$ 0
 DumpMem.lCustData: D$ 0
 DumpMem.lpfnHook: D$ 0
 DumpMem.lpTemplateName: D$ 0]

Proc DumpMemory:
    Arguments @Adressee, @File, @FileSize
    pushad

    mov D$ChoosenLibFile 0
    move D$DumpMem.lpstrFile MemSaveFilter
    move D$DumpMem.hwndOwner D@Adressee
    move D$DumpMem.hInstance D$hInstance
    move D$DumpMem.lpstrFilter MemSaveFileFilter
    move D$DumpMem.lpstrTitle DumpMemFileTitle
    call 'Comdlg32.GetSaveFileNameA' DumpMem

    ..If eax <> 0
        .If D$ChoosenLibFile <> 0
            call ForceExtension MemSaveFilter, '.mem'
            call SaveLibFile, D@File, D@FileSize, MemSaveFilter
        .End_If
    ..End_If
    popad

EndP
_______________________________________________________

[MemSaveReportFilter: B$ 'RosAsm Memory Report (*.txt)', 0  '*.txt', 0 0]
[SaveReportMemFileTitle: B$ 'Save Report memory as...', 0]

Proc SaveMemoryReport:
    Arguments @Adressee, @File, @FileSize
    pushad

    mov D$ChoosenLibFile 0
    move D$DumpMem.lpstrFile MemSaveFilter
    move D$DumpMem.hwndOwner D@Adressee
    move D$DumpMem.hInstance D$hInstance
    move D$DumpMem.lpstrFilter MemSaveReportFilter
    move D$DumpMem.lpstrTitle SaveReportMemFileTitle
    call 'Comdlg32.GetSaveFileNameA' DumpMem

    ..If eax <> 0
        .If D$ChoosenLibFile <> 0
            call ForceExtension MemSaveFilter, '.txt'
            call SaveLibFile, D@File, D@FileSize, MemSaveFilter
        .End_If
    ..End_If
    popad

EndP
_______________________________________________________

Proc DisplayMemData:
    Arguments @Adressee

    pushad

    mov esi MemTable, edi MemTableView, edx 0

    While esi < MemTableEnd
        mov eax D$esi
        .If eax > 0
            If eax = D$esi+8
                mov B$edi 'X' ; Main committed with Reserved Blocks.
            Else
                mov B$edi 'x' ; Child (small and nested) committed Blocks.
            End_If
        .Else
            mov B$edi '_'
        .End_If
        add esi MEMRECORD | inc edi | inc edx

        ; Spaces and CR/LF each 4 / 8 / 32 Chunk.
        mov eax edx | and eax 00_11
        If eax = 0
            mov al ' ' | stosb
        End_If
            mov eax edx | and eax 00_111
        If eax = 0
            mov eax '    ' | stosd
        End_If
            mov eax edx | and eax 00_11111
        If eax = 0
            mov al CR | stosb | mov al LF | stosb
        End_If
    End_While
    mov B$edi 0
    call 'USER32.SetDlgItemTextA' D@Adressee, IDC_DISPLAYMEMDATA, MemTableView

    popad
EndP
_______________________________________________________

; Calculate the maximum size of the report....
;0000000: 023 069 066 06E  - Ascii: # i f n
;0403ED4: 073 079 073 02F  - Ascii: s y s /  Size = 43
;0403ED4: 073 079 073 02F  - Not used - Ascii: s y s /  Size = 54
;0403ED4: 073 079 073 02F  - Commited - Ascii: s y s /  Size = 54
;0403ED4: 073 079 073 02F  - Reserved - Ascii: s y s /  Size = 54
; MEMTABLESIZE = 10650 dwords = 42600 bytes. The displacement is from 12 to 12 dwords
; So we have 887,5 * 4 (10650/12*4). 3550 Lines.
; On Our case this means:
; Size*3550 = 54*3550 = 191700 bytes
; Multiplication Factor = 191700/42600 = 4,5. So, for safety we do
; [MemTableView: D$ ? #(MEMTABLESIZE*5)] = 213000 Bytes

[MemTitleDisplay1: B$ "_______________________________________________________________________________
MemTable Start Address: ", 0]

[MemTitleDisplay2: B$ "
The data is displayed as:
Address; 4 Bytes sequence, Memory Type, Ascii representation.

Address: The address of the MemTable where the data is found. Each address
         is related to the 1st 12 Bytes, but only the 1st four ones are displayed
         here to preserve space.

Bytes:   1st four bytes of the memory that are displayed.

Memory Type: Specifies what is happening with the memory. It can be:
             Commited: 'x' chars. Child (small and nested) committed Blocks.
             Reserved: 'X' chars. Main committed with Reserved Blocks.
             Not Used:  Zeroed Bytes.

Ascii: Ascii representation of the 1st four bytes.
_______________________________________________________________________________
", 0]

[MemReportInfoSize: D$ 0]

Proc DisplayMemInfo:
    Arguments @Adressee

    pushad

    mov esi MemTable, edi MemTableView, edx 0

    push esi | zCopy MemTitleDisplay1 | pop esi
    mov eax esi
    call Writeeax
    push esi | zCopy MemTitleDisplay2 | pop esi

    .While esi < MemTableEnd
        mov eax D$esi
        call WriteMemTableOffset esi
        call WriteMemDwordChunck eax
        .If eax > 0
            If eax = D$esi+8
                push esi | zCopy {" - Commited - Ascii: ", 0} | pop esi
            Else
                push esi | zCopy {" - Reserved - Ascii: ", 0} | pop esi
            End_If
        .Else
            push esi | zCopy {" - Not used - Ascii: ", 0} | pop esi
        .End_If
        call WriteMemAsciiChunck eax
        add esi MEMRECORD
        mov W$edi 0A0D | add edi 2

    .End_While
    mov B$edi 0
    mov eax edi
    sub eax MemTableView
    mov D$MemReportInfoSize eax
    call 'USER32.SetDlgItemTextA' D@Adressee, IDC_DISPLAYMEMINFO, MemTableView

    popad
EndP
________________________________________________________________________________________________________

Proc WriteMemTableOffset:
    Arguments @Value
    Uses eax, ebx

    mov eax D@Value
    call Writeeax
    mov W$edi ': ' | add edi 2

EndP

________________________________________________________________________________________________________

Proc WriteMemDwordChunck:
    Arguments @Value
    Uses eax, esi, edx, ecx, ebx

    lea esi D@Value
    mov ecx 4
    xor eax eax
    mov edx 0
    Do
        ; Arrange the Strings to the chars be displayed on the same row one below each other.
        ; Ex.:  09 055   0 014
        ;      055  01 0FF 075

        If B$esi = 0
            mov W$edi '  ' | add edi 2
        Else_If B$esi <= 0F
            mov B$edi ' ' | inc edi
        End_If

        lodsb
        call writeeax
        xor eax eax
        mov B$edi ' ' | inc edi
        dec ecx
    Loop_Until ecx = 0

EndP
________________________________________________________________________________________________________

Proc WriteMemAsciiChunck:
    Arguments @Value
    Uses eax, esi, edx, ecx, ebx

    lea esi D@Value
    mov ecx 4
    xor eax eax
    mov edx 0

    .Do
        lodsb
        If eax < ' '
            mov B$edi '.' | inc edi
        Else_If eax >= 127
            mov B$edi '.' | inc edi
        Else
            stosb
        End_If

        xor eax eax
        mov B$edi ' ' | inc edi
        dec ecx
    .Loop_Until ecx = 0

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________
































