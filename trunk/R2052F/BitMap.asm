TITLE BitMap
___________________________________________________________________________________________
___________________________________________________________________________________________
;;
                                 BitMaps jobs.

 All-in-One organisation have some limits: I do not see what interrest it could be to
 implement a full BitMap Editor inside RosAsm... Maybe one day. Now, as we NEED BitMaps
 and as there are so many BitMaps Editors available for free, i only implement a BitMaps
 Import feature.
;;
___________________________________________________________________________________________
___________________________________________________________________________________________

;Reads Bitmaps, if any, in new load RosAsm PE (just like "ReadRosAsmMenus" / "ReadRosAsmDialogs":

ReadRosAsmBitMaps:
    mov edi BitMapList, eax 0, ecx 300 | rep stosd
    mov ebx &RT_BITMAP | call SearchResourceType | On eax = 0, ret
    mov D$BitMapListPtr BitMapList, ebx BitMapListPtr | call ReadResourcesRecord
ret


[BmStartOfResources: 0  BmExePtr: 0  BmFileLen: 0  BmFileHandle: 0
 BmFilterPtr: 1
 BmFilesFilters:  B$ 'BitMap files'     0  '*.bmp'   0  0
 OpenBitMapFileTitle: 'Choose a BitMap File to open'  0]


[BmuFileFilter: ? #262] [BmSaveFilter: ? #262] [BmChoosenFile: ? #262]

[BmOpenStruc: len
 BmhwndFileOwner: 0  BmOpenInstance: 0  BmFilesFilters  BmuFileFilter  260
 BmFilterPtr  BmSaveFilter  260  BmChoosenFile  260  0
 OpenBitMapFileTitle  0281804
 0  0  0  0  0]

[PointerToData: ?]

LoadBitMap:
  ; Opening a .bmp file:
    call SearchEmptyBitMapListRecord

    mov edi BmSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' BmOpenStruc | On D$BmSaveFilter = 0,  ret

    On D$BmFileHandle > 0, call 'KERNEL32.CloseHandle' D$BmFileHandle

    call 'KERNEL32.CreateFileA' BmSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox | ret
    Else
        mov D$BmFileHandle eax
    End_If

    call 'KERNEL32.GetFileSize' eax 0 | sub eax 14
    mov edi D$BitMapListPtr | add edi 8 | stosd         ; write BitMap lenght in List
    add eax 14
    mov D$BmFileLen eax                                 ; > eax = adress for asked memory
    VirtualAlloc TempoMemPointer eax
    mov edi D$BitMapListPtr | add edi 4 | mov D$edi eax ; write BitMap mem adress in List
    push edi
        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$BmFileHandle D$TempoMemPointer,
                              14 NumberOfReadBytes 0    ; jump over BitMapFile header.
    pop edi

    mov esi D$edi
    lodsw | cmp ax 'BM' | jne BadBitMapFileHeader
    lodsd | cmp eax D$BmFileLen | jne BadBitMapFileHeader
    lodsd | cmp eax 0 | jne BadBitMapFileHeader
    lodsd | mov D$PointerToData eax

  ; Load BitMap Data in same table (overwrite no more use header):
    mov ecx D$BmFileLen | sub ecx 14  ; 14 Bytes = len of File Header
  ; (File header is: W$ Style // D$ Size // D$ 0 // D$ Ptr to Data).

    push edi
        call 'KERNEL32.ReadFile' D$BmFileHandle D$edi ecx NumberOfReadBytes 0
    pop edi

  ; Ajust image size if this record is missing:
    mov edi D$edi
    If D$edi+20 = 0
        mov eax D$BmFileLen | sub eax D$PointerToData
        mov D$edi+20 eax
    End_If

  ; Ask user for what BitMap ID number:
L1: call 'USER32.DialogBoxIndirectParamA' D$hinstance  BMIDDialog  0  BMIDDialogProc  0

    If B$UserValidateBitMap = &TRUE
        call ReOrderBitMapList
    Else
        mov edi D$BitMapListPtr, eax 0 | stosd | stosd | stosd
    End_If
ret


[ConflictIDs: B$ 'This ID number is already in Use', 0  ReorderFlag: 0]

ReOrderBitMapList:
    mov B$ReorderFlag &FALSE
    mov esi BitMapList, edi esi | add edi 12
    While D$edi > 0
        mov eax D$esi
        If eax > D$edi
            Exchange D$esi D$edi, D$esi+4 D$edi+4, D$esi+8 D$edi+8
            mov B$ReorderFlag &TRUE
        End_If
        add esi 12 | add edi 12
    End_While
    cmp B$ReorderFlag &TRUE | je ReOrderBitMapList
ret

SearchEmptyBitMapListRecord:
    push esi
        mov esi BitMapList
        While D$esi > 0
            add esi 12
        End_While
        mov D$BitMapListPtr esi
    pop esi
ret


[BadBitMapFile: 'Bad BitMap file header', 0]

BadBitMapFileHeader:
    call 'USER32.MessageBoxA' D$hwnd, BadBitMapFile, Argh, &MB_SYSTEMMODAL
L8: mov edi D$BitMapListPtr, eax 0, ecx 3 | rep stosd
ret


[BMIDDialog: D$ 090C408C2 0    ; Style
 U$ 03 0 0 0B9 018             ; Dim
 0                             ; no Menu
 '' 0                          ; Class
 'What ID number for new BitMap?' 0 ; Title
 08 'Helv' 0]                  ; Font

[BMID0: D$ 050000000 0         ; Style
 U$ 07E 03 038 013             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[BMID1: D$ 050000000 0         ; Style
 U$ 03 03 038 013              ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data

[BMID2: D$ 050802000 0         ; Style
 U$ 040 05 038 0F              ; Dim
 03                            ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


[BMIDeditHandle: 0  uBitMapID: '                   ', 0]

; Previously, i had set a test to prevent from IDs numbers smaller than 1000 and bigger than
; 32000. I do not remember why. May be an old wrong idea about unique IDs across Types.
; Suppress...

Proc BMIDDialogProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
       ..If D@wParam = &IDCANCEL
            call 'User32.EndDialog' D@Adressee 0

       ..Else_If D@wParam = &IDOK
           call 'User32.GetDlgItem' D@Adressee 3 | mov D$BMIDeditHandle eax
           call 'User32.SendMessageA' D$BMIDeditHandle &WM_GETTEXTLENGTH 0 0 | inc eax
           call 'User32.SendMessageA' D$BMIDeditHandle &WM_GETTEXT eax uBitMapID
           TranslateAsciiToDword uBitMapID
           mov D$uBitMapID 0                         ; just for abort tests in callers:
           .If eax > 0FFFF    ; 32000                 ; 'StoreMenuEdition' / 'MenuEditProc'
             mov eax D$IdTooBigPtr | call MessageBox
           .Else_If eax < 1   ; 000
             mov eax D$IdTooSmallPtr | call MessageBox
           .Else
                mov esi BitMapList
                While D$esi > 0
                    On D$esi = eax, mov eax 0
                    add esi 12
                End_While
                If eax = 0
                    call 'USER32.MessageBoxA' D$hwnd, ConflictIDs, Argh, &MB_SYSTEMMODAL
                Else
                    mov edi D$BitMapListPtr, D$edi eax
                    mov B$UserValidateBitMap &TRUE
                    call 'User32.EndDialog' D@Adressee 0
                End_If
           .End_If

       ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        mov B$UserValidateBitMap &FALSE
        call 'User32.GetDlgItem' D@Adressee 3
        call 'User32.SendMessageA' eax &EM_SETLIMITTEXT 5  0
           mov esi D$BitMapListPtr | On esi > BitMapList, sub esi 12
           If D$esi = 0
             mov eax 1   ; 30000
           Else
             lodsd | inc eax
           End_If
           call 'USER32.SetDlgItemInt' D@Adressee 3 eax 0

    ...Else
       popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP



[hMemDC: 0    BitmapHandle: 0    hBitmap: 0]

[BMP: 0    BMPw: 0    BMPh: 0    BMPline: 011C    BMPplane: 0    BMPpixBits: 0   BMPptr: 0
 BitMapInfoHeader: 0    FileHeaderOffset: 0    BipMapCopyPtr: 0]

BitMapViewer:
    .If D$BitMapListPtr > BitMapList
        mov W$BitMapDialogControlsNumber 2
        call 'USER32.DialogBoxIndirectParamA' D$hinstance BitMapDialog  0  BitMapProc  0
        If B$UserValidateBitMap = &FALSE
            mov edi D$BitMapListPtr, eax 0 | stosd | stosd | stosd
        End_If
    .End_If
ret


[NoBitMap: B$ 'No BitMap in This file', 0]

[DeleteBitMapTitle: U$ 'Delete'  ShowBitMapTitle: 'Exit  ']

DeleteBitMap:
    mov edi BMPEXIT, esi DeleteBitMapTitle, ecx 12 | rep movsb
    mov W$BitMapDialogControlsNumber 5 | call BitMapView

    If B$UserValidateBitMap = &TRUE
        mov edi D$BitMapListPtr, esi edi | add esi 12
        While D$edi > 0
            movsd | movsd |movsd
        End_While
    End_If
ret


ShowBitMapsIds:
    mov edi BMPEXIT, esi ShowBitMapTitle, ecx 12 | rep movsb
    mov W$BitMapDialogControlsNumber 4 | call BitMapView
ret


BitMapView:
    mov D$BitMapListPtr BitMapList, eax D$BitMapListPtr

    If D$eax = 0
        mov B$UserValidateBitMap &FALSE
        call 'USER32.MessageBoxA' D$hwnd, NoBitMap, Argh, &MB_SYSTEMMODAL
    Else
        call 'USER32.DialogBoxIndirectParamA' D$hinstance, BitMapDialog, 0, BitMapProc, 0
    End_If
ret


[UserValidateBitMap: ?] [BitMapIdText: ? ? ? ?]

Proc BitMapProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
         ..If D@wParam = &IDOK
             mov B$UserValidateBitMap &TRUE
             call 'User32.EndDialog' D@Adressee 0

         ..Else_If D@wParam = &IDCANCEL
             mov B$UserValidateBitMap &FALSE
             call 'User32.EndDialog' D@Adressee 0

         ..Else_If D@wParam = 3                        ; >>>>
             mov eax D$BitMapListPtr | add eax 12
             mov ebx MAXBITMAP | shl ebx 2 | add ebx BitMapList
             .If eax < ebx                             ; ebx = end of BitMapList
                If D$eax > 0
                    mov D$BitMapListPtr eax
                    call 'USER32.RedrawWindow' D@Adressee 0  0,
                                           &RDW_ERASE+&RDW_INVALIDATE+&RDW_INTERNALPAINT
                End_If
             .End_If

         ..Else_If D@wParam = 4                        ; <<<<
             If D$BitMapListPtr > BitMapList
                 sub D$BitMapListPtr 12
                 call 'USER32.RedrawWindow' D@Adressee 0  0,
                                           &RDW_ERASE+&RDW_INVALIDATE+&RDW_INTERNALPAINT
             End_If
       ..End_If

    ...Else_If D@Message = &WM_PAINT

         call 'User32.BeginPaint'  D@Adressee  PAINTSTRUCT
             mov D$hdc eax
             call 'GDI32.CreateCompatibleDC' D$hdc | mov D$hMemDC eax

             mov esi D$BitMapListPtr | lodsd          ; ID
             call SetBitMapIdText D@Adressee
             lodsd | mov edi eax                      ; > edi > adress
             lodsd                                    ; eax = lenght
             mov esi edi, ebx D$esi+20                ; ebx = image size
             sub eax ebx | add eax edi                ; eax > ptr to bmp data

             call 'GDI32.CreateDIBitmap' D$hdc  edi  &CBM_INIT  eax  edi  &DIB_RGB_COLORS
                 mov D$hBitmap eax
                 call 'GDI32.SelectObject' D$hMemDC D$hBitmap
                 call 'User32.GetClientRect' D@Adressee RECT
                 call 'GDI32.BitBlt' D$hdc 0 0 D$Rect_Right D$Rect_bottom D$hMemDC 0 0 &SRCCOPY
             call 'GDI32.DeleteDC' D$hMemDC
         call 'User32.EndPaint' D@Adressee PAINTSTRUCT
         call 'GDI32.DeleteObject' D$hBitmap

    ...Else_If D@Message = &WM_INITDIALOG
        mov D$BitMapListPtr BitMapList
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

    ...Else
       popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


Proc SetBitMapIdText:
    Argument @Adressee

    pushad
        push 0_FFFF_FFFF
        mov edi BitMapIdText, ecx 10
L0:     mov edx 0 | div ecx | cmp eax 0 | je L2>
            push edx | jmp L0<
L2:         push edx
L2:     pop eax | cmp eax 0_FFFF_FFFF | je L3>
            add al '0' | stosb | jmp L2<
L3:     mov al 0 | stosb
        call 'USER32.GetDlgItem' D@Adressee 5
        call 'USER32.SetWindowTextA' eax BitMapIdText
    popad
EndP


[BitMapDialog: D$ 0900408C2 0  ; Style
 BitMapDialogControlsNumber:
 U$ 02 0 0 0DC 0C8             ; Dim
 0                             ; Menu
 '' 0                           ; Class
 'New Dialog' 0                ; Title
 08 'Helv' 0]                  ; Font

[BMD0: D$ 050000001 0      ; Style
 U$ 0AB 0B9 030 0F             ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 BMPEXIT:
 'Delete' 0                        ; Title
 0]                            ; No creation data

[BMD2: D$ 050000000 0      ; Style
 U$ 04E 0B9 029 0F             ; Dim
 03                            ; ID
 0FFFF 080                     ; Class
 '>>>>' 0                      ; Title
 0]                            ; No creation data

[BMD3: D$ 050000000 0      ; Style
 U$ 0 0B8 029 010              ; Dim
 04                            ; ID
 0FFFF 080                     ; Class
 '<<<<' 0                      ; Title
 0]                            ; No creation data

[BMD4: D$ 050000307 0      ; Style
 U$ 02B 0B7 01F 010            ; Dim
 05                            ; ID
 0FFFF 080                     ; Class
 '65000' 0                     ; Title
 0]                            ; No creation data

[BMD1: D$ 050000000 0      ; Style
 U$ 079 0B9 030 0F             ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data





