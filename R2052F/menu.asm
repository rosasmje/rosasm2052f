TITLE menu
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
                                  Memus editor

'MenuEditProc'

 There are 2 types of menus for Win i use the newer (EX type) one, with MFT and
 MFS independant flags stored as dwords in Resources, plus ID dWord, plus PopUp
 and End Flags as Words.

 &MF_POPUP value is 010, but, in the Resources Word for this, this value is 01,
 what gives for LastPopUp, 081. As this 1 conflicts with &MF_GRAYED (01) or
 &MFS_GRAYED (3), the translation from 010 to 01 is only done when storing (and
 reverse when loading).

 As i use only one dWord per item to store all the various flags for the Editor
 internal use (MenuItemsFlags table).
;;
 _______________________________________________________________________________________
 _______________________________________________________________________________________


; 080=button / 081=Edit / 082=Static / 083=ListBox / 084=ScrollBar / 085=ComboBox


[MenuDialogData: D$ 090CC08C2 0        ; Style
 U$ 0B 0 0 0FC 0A8             ; Dim
 0                             ; Menu
 0                             ; Class
 'Menu Edition' 0               ; Title
 08 'Helv' 0]                      ; Font

; WS_BORDER|WS_TABSTOP|WS_HSCROLL|WS_VSCROLL| ES_WANTRETURN|ES_NOHIDESEL|ES_AUTOVSCROLL|ES_MULTILINE
[MDD0000: D$ 050B01144 0       ; Style
 U$ 0 0 0FB 07D                ; Dim
 065                           ; ID
 0FFFF 081                     ; Class  > Edit Control
 '' 0                          ; Title
 0]                            ; No creation data

[MDD0001: D$ 050000003 0      ; Style
 U$ 0 084 020 0B              ; Dim
 IDR_Grayed                           ; ID  06F
 0FFFF 080                     ; Class
 'Gray' 0                    ; Title
 0]                            ; No creation data

[MDD0011: D$ 050000003 0      ; Style
 U$ 028 084 020 0B              ; Dim
 IDR_Checked                           ; ID  070
 0FFFF 080                     ; Class
 'Check' 0                   ; Title
 0]                            ; No creation data

[MDD0111: D$ 050000009 0      ; Style
 U$ 050 084 020 0B              ; Dim
 IDR_Radio                           ; ID
 0FFFF 080                     ; Class
 'Radio' 0                      ; Title
 0]                            ; No creation data

[MDD1111: D$ 050000003 0      ; Style
 U$ 078 084 020 0B              ; Dim
 IDR_Right                           ; ID
 0FFFF 080                     ; Class
 'Right' 0                      ; Title
 0]                            ; No creation data

[MDD0002: D$ 050000000 0      ; Style
 U$ 01 098 033 010             ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Cancel' 0                    ; Title
 0]                            ; No creation data

[MDD0003: D$ 050000000 0      ; Style
 U$ 035 098 033 010            ; Dim
 07B                           ; ID
 0FFFF 080                     ; Class
 'Help' 0                      ; Title
 0]                            ; No creation data

[MDD0004: D$ 050000000 0      ; Style
 U$ 0C9 098 033 010            ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[MDD0005: D$ 050000000 0      ; Style
 U$ 069 098 05F 010            ; Dim
 078                           ; ID
 0FFFF 080                     ; Class
 'Store Equates to ClipBoard' 0 ; Title
 0]                            ; No creation data

[MDD0006: D$ 050802002 0      ; Style
 U$ 0C7 083 033 0C             ; Dim
 06E                           ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[MDD0007: D$ 050000000 0      ; Style
 U$ 0A0 086 020 0B             ; Dim
 016E                           ; ID
 0FFFF 082                     ; Class
 'Id_Menu=' 0                ; Title
 0]                            ; No creation data


[ID_MenuEdit 065
 IDR_Radio 072
 IDR_Right 071
 IDR_Checked 070
 IDR_Grayed 06F
 ID_IDedit 06E
 ID_EquToClip 078  ID_MenuHelp 07B]

[hPopup: 0  hItem: 0  hLastPopUp: 0  hLastItem: 0
 hGrayed: 0   hChecked: 0   hRight: 0  hRadio: 0
 FirstMenuID: ' 1000', 0 0 0 0 0 0 0 0     MenuIDsHandle: 0]

[MENU_DWORDS 4000]

[eMenu: ? #MENU_DWORDS]  ; temporary table for menu edition
[ceMenu: ? #MENU_DWORDS] ; temporary table for menu edition control routine
[EndOfeMenu: ?]

[MenuEditHandle: 0   PostInit: 0  MenuEditIDHandle: 0   mEditClass: 'EDIT', 0]

[EdgeOfMenuEdition: 0FFFFFFFF]
[MenuEdition: B$ ?  #10000] [MenuItemsFlags: ? #500 MenuItemsFlagsEnd: ]
[IndexToMenuItemsFlags: ?]

; MenuList, like 'DialogList', holds records for each menu to be used by
; 'TemporaryFillRsrcList' to prepare the Resource tree construction. Each record is:
; ID / Ptr / Size > 100 possible menus here:

 _________________________________

; These two basics routines are used for ID base holding (back and forth).
; Input dWord value in eax. Writes Ascii corresponding chain at edi:

[TenTable:  10000 1000  100  10  1 0]

TransDwordToAsciiDecimal:
    mov ebx TenTable, ecx D$ebx, B$edi '0'
L0: cmp eax ecx | jb L1>
        sub eax ecx | inc B$edi | jmp L0<
L1: inc edi |  mov B$edi '0' | add ebx 4 | mov ecx D$ebx | cmp ecx 0 | ja L0<
    mov B$edi 0
ret


[TranslateDwordToAscii | pushad | push #1 | pop eax | push #2 | pop edi
                                         call TransDwordToAsciiDecimal | popad]

TransDecimalAsciiToDword:
    While B$esi = ' '
        inc esi
    End_While
    mov ebx 0
L0: mov eax 0
    lodsb | cmp al 0 | je L9>
    sub al '0' | shl ebx 1 | add eax ebx | shl ebx 2 | add ebx eax    ; (EBX * 10) + AL
        jmp L0<
L9: mov eax ebx
ret

[TranslateAsciiToDword | push #1 | pop esi | call TransDecimalAsciiToDword]
 _________________________________

;;
 Here, we read a resource menu and translate it in formated text for Edit Control and
 in a dWords table of flags (at MenuItemsFlags). Down there, 'StoreMenuEdition' does
 the reverse operation. 'eMenu' is the resource menu, including header. 'MenuEdition'
 is the text table to send to Edit Control.
;;

[uMenu_ID: 1000] [MenuTabsCount: 0]

FillMenuEditData:
    mov D$MenuTabsCount 0
    mov esi eMenu, edi MenuEdition, ebx MenuItemsFlags
    add esi 8                                           ; skip header

L0: lodsd | mov D$ebx eax                               ; MFT Flags
    lodsd | or D$ebx eax                                ; MFS Flags

    lodsd                                               ; jump over ID

    mov eax 0 | lodsw                                   ; Pop Flags

    .If eax = PopUpFlag
        inc D$MenuTabsCount
        mov eax &MF_POPUP
    .Else_If eax = LastPopUpFlag
        inc D$MenuTabsCount
        mov eax &MF_POPUP__&MF_END
    .Else_If eax = LastItemFlag
        On D$MenuTabsCount > 0, dec D$MenuTabsCount
        push ebx, eax
            sub ebx 4
            .While ebx > MenuItemsFlags
                mov eax D$ebx | and eax POPMASK
                If eax = &MF_POPUP__&MF_END
                    On D$MenuTabsCount > 0, dec D$MenuTabsCount
                Else_If eax = &MF_POPUP
                   jmp L1>
                End_If
                sub ebx 4
            .End_While
L1:     pop eax, ebx
        mov eax &MF_END
    .End_If

    or D$ebx eax                                        ; All Flags

    lodsw                                               ; item first letter
    If ax = 0
      mov ax 0A0D | stosw                               ; Separator
    Else
      mov edx 0                                         ; simple counter for read alignement
L1:   stosb | inc edx
      lodsw | cmp ax 0 | jne L1<                        ; write item name
      mov ax 0A0D | stosw                               ; end mark
      test edx 1 | jz L2>
        lodsw                                           ; dWord align
L2:   test D$ebx &MF_POPUP
      jz L2>
        lodsd                                           ; + 1 dWord in case of popup
    End_If

L2: mov ecx D$MenuTabsCount, al tab
    while ecx > 0
        stosb | dec ecx
    End_While

    add ebx 4 | cmp esi D$EndOfeMenu | jb L0<<

; In case user hit some CR/LF at the end of menu items, some separators are added. This
; 'strip job' should be done at the end of 'StoreMenuEdition' but it is much more easier
; here, as some '0' analyzes are done here.

L8: cmp B$edi-1  ' ' | jae L9>
      dec edi | jmp L8<

L9: On B$edi = 0FF, inc edi                          ; edge in case of empty menu
    mov eax 0 | stosd                                   ; End mark


    mov esi MenuItemsFlags
ret

____________________________________________________________________________________________

; &MF_POPUP 010   &MF_END 080  > but in resources >>> word value = popup = 1 (my equates)
;
; &MFS_GRAYED 3   &MFS_CHECKED 8   &MF_HELP 04000   &MF_POPUP 010  &MF_END 080
; (grayed=1 + disable=2)
;
; &MFT_MENUBARBREAK  020   &MFT_MENUBREAK 040   &MFT_OWNERDRAW 0100
; &MFT_RADIOCHECK 0200     &MFT_RIGHTJUSTIFY 04000

[MFSMASK 00_1111    MFTMASK 0_FFFF_FF00     POPMASK 090]

[GrayedFlag 2  PopUpFlag 1  LastPopUpFlag 081  ItemFlag 0  LastItemFlag 080]

[SelectionStart: 0 SelectionEnd: 0  MenuFlagEnable: 0]


SetMenuItemFlag:
; When user double click on some item with possible spaces in it, we generalise
; one word selection to the whole item line (like in a List box):

    On B$OnMenuEdition = &FALSE, ret

    call 'User32.SendMessageA' D$MenuEditHandle &EM_GETSEL SelectionStart SelectionEnd
    mov ebx D$SelectionStart, eax D$SelectionEnd | sub eax ebx | cmp eax 0 | je L8>>
    call 'User32.SendMessageA' D$MenuEditHandle &EM_LINEFROMCHAR D$SelectionStart 0
  ; eax > line number
    call 'User32.SendMessageA' D$MenuEditHandle &EM_LINEINDEX    eax   0
  ; eax > first line char
        mov D$SelectionStart eax
    call 'User32.SendMessageA' D$MenuEditHandle &EM_LINELENGTH   eax   0
  ; eax > lenght
        add eax D$SelectionStart | mov D$SelectionEnd eax
    call 'User32.SendMessageA' D$MenuEditHandle &EM_SETSEL D$SelectionStart D$SelectionEnd

; Now, set the according flags:

    .If B$MenuFlagEnable = &FALSE
        call 'User32.EnableWindow' D$hRight &TRUE
        call 'User32.EnableWindow' D$hChecked &TRUE
        call 'User32.EnableWindow' D$hGrayed &TRUE
        call 'User32.EnableWindow' D$hRadio &TRUE
        call 'User32.SendMessageA' D$hRight &BM_SETCHECK 0 0
        call 'User32.SendMessageA' D$hChecked &BM_SETCHECK 0 0
        call 'User32.SendMessageA' D$hGrayed &BM_SETCHECK 0 0
        call 'User32.SendMessageA' D$hRadio &BM_SETCHECK 0 0
        mov B$MenuFlagEnable &TRUE

        call 'User32.SendMessageA' D$MenuEditHandle &EM_LINEFROMCHAR  D$SelectionStart 0
        shl eax 2 | mov D$IndexToMenuItemsFlags eax
        mov ebx MenuItemsFlags | add ebx eax | mov eax D$ebx
        test eax &MFS_GRAYED | jz L1>
            push eax
                call 'User32.SendMessageA' D$hGrayed &BM_SETCHECK 1 0
            pop eax
L1:     test eax &MFS_CHECKED | jz L1>
            push eax
                call 'User32.SendMessageA' D$hChecked &BM_SETCHECK 1 0
            pop eax
L1:   test eax &MFT_RADIOCHECK | jz L1>
            push eax
                call 'User32.SendMessageA' D$hRadio &BM_SETCHECK 1 0
            pop eax
L1:   test eax &MFT_RIGHTJUSTIFY | jz L9>>
            push eax
                call 'User32.SendMessageA' D$hRight &BM_SETCHECK 1 0
            pop eax
    .End_If
    jmp L9>

; sleep:

L8: If B$MenuFlagEnable = &TRUE
        call 'User32.EnableWindow' D$hRight &FALSE
        call 'User32.EnableWindow' D$hChecked &FALSE
        call 'User32.EnableWindow' D$hGrayed &FALSE
        call 'User32.EnableWindow' D$hRadio &FALSE
        mov B$MenuFlagEnable &FALSE
    End_If

L9: ret


; Read a menu in RosAsm PE. Basicaly same routine as the one for icons.

ReadRosAsmPeMenus:
    mov edi MenuList, eax 0, ecx 300 | rep stosd
    mov ebx RT_MENU | call SearchResourceType | On eax = 0, ret
    mov D$MenuListPtr MenuList, ebx MenuListPtr | call ReadResourcesRecord
ret
 ______________________________________

[PreviousEditProc: ?]

Proc InitMenuEdition:
    Argument @Adressee

    call 'User32.GetDlgItem' D@Adressee ID_MenuEdit
      mov D$MenuEditHandle eax

  ; subClassing Edit control for tab problem:
    call 'User32.SetWindowLongA' D$MenuEditHandle  &GWL_WNDPROC mEditProc
      mov D$PreviousEditProc eax

  ; Limit edition of Equate number to 5 chars and set text at 1000:
    call 'User32.GetDlgItem' D@Adressee ID_IDedit
      mov D$MenuIDsHandle eax

    call 'User32.SendMessageA' eax  &EM_SETLIMITTEXT 5  0

    call 'User32.SendMessageA' D$MenuIDsHandle  &WM_SETTEXT 0 FirstMenuID

  ; Disable all the CheckBoxes:
    call 'User32.GetDlgItem' D@Adressee IDR_Grayed | mov D$hGrayed eax
    call 'User32.EnableWindow' D$hGrayed &FALSE
    call 'User32.GetDlgItem' D@Adressee IDR_Checked | mov D$hChecked eax
    call 'User32.EnableWindow' D$hChecked &FALSE
    call 'User32.GetDlgItem' D@Adressee IDR_Right | mov D$hRight eax
    call 'User32.EnableWindow' D$hRight &FALSE
    call 'User32.GetDlgItem' D@Adressee IDR_Radio | mov D$hRadio eax
    call 'User32.EnableWindow' D$hRadio &FALSE
Endp

____________________________________________________________________________________________

[OneItemString: 160 ] [StringData: 0 #40]

GetuMenuID:
    call 'User32.SendMessageA' D$MenuIDsHandle &WM_GETTEXTLENGTH 0 0 | inc eax
    call 'User32.SendMessageA' D$MenuIDsHandle &WM_GETTEXT eax FirstMenuID
    TranslateAsciiToDword FirstMenuID
    mov D$FirstMenuId 0                           ; just for abort tests in callers:
    If eax > 32000                                ; 'StoreMenuEdition' / 'MenuEditProc'
      mov eax D$IdTooBigPtr | call MessageBox | mov D$FirstMenuId 0 | ret
    Else_If eax < 1   ;000
      mov eax D$IdTooSmallPtr | call MessageBox | mov D$FirstMenuId 0 | ret
    End_If
    mov D$uMenu_ID eax
    inc eax | mov D$FirstMenuId eax
ret

____________________________________________________________________________________________

EM_GETLINE_Comment:

;;
  EM_GETLINE Message, when applied to a Multi-Lines Edit Control, may fills the Destination
  Buffer with additional corrupted Characters. This seems to happend when a Logitech
  Mouse-Wheel Driver has been installed on the Computer (intellimouse 1.2a (Microsoft) 2 
  buttons + wheel // Win 2000).
  
  Hopefully, the (eax) Return Value, saying the number of written Chars seems good. This is 
  why i add this:
  
  > mov B$Destination+eax 0
  
  after retrieving a Line by EM_GETLINE Message.
;;




; Here, we save user definition for menu edition. We read Edit Control text and the
; flags table (at MenuItemsFlags). We write at 'eMenu:' a true resource conventional
; menu. Up there, 'FillMenuEditData' does the reverse operation.

[SeparatorsNumber: ?    PopUpNumber: ?]

StoreMenuEdition:
    call GetuMenuID | On D$FirstMenuId = 0, ret
   ; "EM_GETLINECOUNT" returns 1 if empty; so, ...:
    call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, 0, OneItemString
    mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'

    If eax = 0
        mov D$FirstMenuId 0 | ret
    End_If

    mov edi eMenu, ecx MENU_DWORDS, eax 0 | rep stosd

    call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINECOUNT, 0, 0

    mov ecx eax, ebx MenuItemsFlags, edi eMenu, edx 0
    mov D$SeparatorsNumber 0, D$PopUpNumber 0
    mov ax 1 | stosw | mov ax 4 | stosw | mov eax 0 | stosd     ; header (8 bytes)
L0: pushad
T0:     mov eax 0, edi OneItemString, ecx 40 | rep stosd        ; GETLINE not zero-ended
        mov W$OneItemString 160                                 ; max write for GETLINE
        call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, edx, OneItemString
      ; Copied Line may be corrupted at the end with some weird Drivers on Board:
        mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'

        On eax = 0, jmp T1>
        mov esi OneItemString
        While B$esi = tab
            inc esi | dec eax                                   ; no header tabs in chars count
        End_While
        On B$esi = 0 , mov eax 0
        .If eax = 0
T1:         popad
                mov eax 0 | stosd | stosd | stosd | stosd       ; Separator = 16 zero bytes
                inc edx | inc D$SeparatorsNumber | add ebx 4
                If edx < ecx
                    pushad | jmp T0<<
                Else
                    jmp L9>>
                End_If
        .End_If
    popad

    mov eax D$ebx | and eax MFTMASK | stosd

    mov eax D$ebx | and eax MFSMASK | stosd

    mov eax D$FirstMenuID | add eax edx
    sub eax D$SeparatorsNumber
    sub eax D$PopUpNumber
    test D$ebx &MF_POPUP | jz F0>
        inc D$PopUpNumber | mov eax 0                          ; No ID for PopUp
F0: stosd                                                      ; ID number
    mov eax D$ebx | and eax POPMASK
    If eax = &MF_POPUP
        mov eax PopUpFlag
    Else_If eax = &MF_POPUP__&MF_END
        mov eax LastPopUpFlag
    Else_If eax = &MF_END
        mov eax LastItemFlag
    End_If
    stosw | mov esi OneItemString

    push ecx
        mov ecx 0, eax 0                                      ; align counter
        While B$esi < ' '
           lodsb                                              ; clear tabs if any
        End_While
L1:     lodsb | stosw | inc ecx | cmp al 0 | ja L1<           ; name
        test ecx 1 | jnz L2>
          stosw                                               ; align
L2: pop ecx
    test D$ebx &MF_POPUP | jz L3>
        stosd                                                 ; PopUp padding

L3: inc edx | add ebx 4 | cmp edx ecx | jb L0<<

L9: mov eax 0 | stosd | stosd

    mov D$EndOfeMenu edi, eax &TRUE
ret


PackMenuInList:
    mov eax D$MenuListPtr | add eax 4
    push eax
        mov edi D$MenuListPtr | mov eax D$uMenu_ID | stosd  ; ID
        push edi
            mov eax D$EndOfeMenu | sub eax eMenu | inc eax
            push eax
                VirtualAlloc TempoMemPointer eax            ; New memory
                mov ebx D$TempoMemPointer, eax ebx
            pop ecx
        pop edi
        stosd                                               ; Ptr (eax from 'AskMem')
        mov eax ecx                                         ; Size
        stosd
        mov esi eMenu, edi ebx | rep movsb                  ; store
    pop eax
  ;  call FreeMemory                                        ; release previous Listed memory
ret


; Here, we save Memu IDs Equates in CiplBoard 'WriteClipIDvalue' is a SubRoutine of
; 'ClipEquates'

[DataForClipEquates: ? #1000] [StartOfItemsLine: ?]

WriteClipIDvalue:
    mov ax '  ' | stosw
    mov eax D$FirstMenuID | add eax edx
    sub eax D$SeparatorsNumber | sub eax D$PopUpNumber

    push ecx
    ; adapted version of 'TransDwordToAsciiDecimal' for ID number:
      mov ebx TenTable, ecx D$ebx, B$edi '0'
L1:   cmp eax ecx | jae L2>
            add ebx 4 | mov ecx D$ebx | jmp L1<
L2:   cmp eax ecx | jb L3>
            sub eax ecx | inc B$edi | jmp L2<
L3:   inc edi |  mov B$edi '0' | add ebx 4 | mov ecx D$ebx | cmp ecx 0 | ja L2<

      mov ecx edi | sub ecx D$StartOfItemsLine        ; align items text (3 per line):
      If ecx > 64                                     ; 32 chars each
            mov ax 0A0D | stosw | mov al ' ' | stosb
            mov D$StartOfItemsLine edi
      Else
            mov al ' ' | and ecx 00_11111 | xor ecx 00_11111 | inc ecx | rep stosb
      End_If
    pop ecx


ret

____________________________________________________________________________________________

[MenuEquateIndice: 'M00_']

ClipEquates:
    mov D$MenuEquateIndice 'M00_'
    mov eax D$MenulistPtr | sub eax MenuList
    mov ebx 12, edx 0 | div ebx                 ; > indice = 0, 1, 2, ...
    mov ebx 10, edx 0 | div ebx
    add B$MenuEquateIndice+2 dl
    mov edx 0 | div ebx
    add B$MenuEquateIndice+1 dl

    call GetuMenuID
    call 'User32.SendMessageA' D$MenuEditHandle &EM_GETLINECOUNT 0 0
    mov ecx eax, edi DataForClipEquates, edx 0, D$SeparatorsNumber 0, D$PopUpNumber 0
    mov al '[' | stosb | mov D$StartOfItemsLine edi

    mov eax D$MenuEquateIndice | stosd | mov eax 'Menu' | stosd
    dec D$FirstMenuID | call WriteClipIDvalue | inc D$FirstMenuID

L0:
L1: push edi, ecx
        mov eax 0, edi OneItemString, ecx 40 | rep stosd        ; GETLINE not zero-ended
    pop ecx, edi
    mov W$OneItemString 160                                 ; max write for GETLINE
    pushad
        call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, edx, OneItemString
        mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'
    popad
    mov esi OneItemString
    While B$esi = tab
        inc esi
    End_While
    If B$esi = 0
        inc D$SeparatorsNumber | inc edx | cmp edx ecx | je L9>>   ; No Separators
            jmp L1<<
    End_If
    mov eax D$MenuItemsFlags+edx*4 | test eax &MF_POPUP | jz T0>   ; No Popup  ; PopUpFlag
    inc D$PopUpNumber | inc edx | jmp L1<<

T0: mov esi OneItemString, eax 0, eax D$MenuEquateIndice | stosd
    While B$esi <= ' '
        lodsb                                           ; strip leading tabs and spaces
    End_While

L3: lodsb | cmp al 0 | je L4>
       ; On al = ' ', mov al '_'                         ; link separates words
        On al = '&', jmp L3<                            ; do not write '&'
        On al = tab  , jmp L4>                          ; do not write 'hot keys'
        On al < '0', mov al '_'
        If al = '_'
            On B$edi-1 = '_', jmp L3<                   ; only one '_' at a time
        End_If

        stosb | jmp L3<                                 ; name Char writing

L4: call WriteClipIDvalue

    inc edx | cmp edx ecx | jb L0<<

L9: dec edi | cmp B$edi ' ' | jbe L9<                   ; suppress lasting spaces / CR/LF
    inc edi | mov al ']' | stosb | mov al 0 | stosb     ; and close Bracket

    dec edi                                             ; reuse 'ControlC' for clip:
    push D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside
      mov D$BlockStartTextPtr DataForClipEquates, D$BlockEndTextPtr edi
      mov B$BlockInside &TRUE | call ControlC
    pop D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
ret
____________________________________________________________________________________________
; As i get no end problems with this damned Edit Controls when i need to know if there
; are some ending CR/LF or not at the end, i get the text, strip the CR/LF, reset the
; text:

[LenOfMenuText: ?    TempoMenuTextPtr: ?]

CleanMenuEnd:
    call 'User32.SendMessageA' D$MenuEditHandle &WM_GETTEXTLENGTH 0 0
    inc eax | mov D$LenOfMenuText eax
    VirtualAlloc TempoMenuTextPtr eax
    call 'User32.SendMessageA' D$MenuEditHandle, &WM_GETTEXT, D$LenOfMenuText,
                               D$TempoMenuTextPtr
    mov esi D$TempoMenuTextPtr

    If D$esi = 0
L1:     VirtualFree D$TempoMenuTextPtr
        mov eax &FALSE | ret
    End_If
    add esi D$LenOfMenuText | mov D$esi+1 0
    While B$esi <= ' '
        mov B$esi 0 | dec esi | cmp esi D$TempoMenuTextPtr | jb L1<
    End_While
    call 'User32.SendMessageA' D$MenuEditHandle &WM_SETTEXT 0 D$TempoMenuTextPtr

    VirtualFree D$TempoMenuTextPtr
    mov eax &TRUE
ret

____________________________________________________________________________________________
; Here, we set the menu organisation flags (Item / LasItem / PopUp / LastPopUp)
; We use the number of leading tabs to retrieve the tree.

[MAXMENUTABS 500]

[MenuItemTabsList: B$ ? #MAXMENUTABS] [MenuItemTabsListPtr: ?]

StoreMenuPopFlags:
    mov D$MenuItemTabsListPtr MenuItemTabsList

    call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, 0, OneItemString
    mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'

    On eax = 0, ret

    call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINECOUNT, 0, 0

    mov ecx eax, edx 0

L0: pushad
      mov eax 0, edi OneItemString, ecx 40 | rep stosd       ; GETLINE not zero-ended
      mov W$OneItemString 160                                ; max write for GETLINE

      call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, edx, OneItemString
      mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'

      mov esi OneItemString, edi D$MenuItemTabsListPtr, B$edi 1

      .If eax > 0
          While B$esi = tab
             lodsb | inc B$edi
          End_While
      .End_If
    popad

    inc D$MenuItemTabsListPtr | inc edx | cmp edx ecx | jb L0<<

    mov edi D$MenuItemTabsListPtr, D$edi 0

    ; Now, set the Flags in MenuItemsFlags:

    mov esi MenuItemTabsList, edi MenuItemsFlags

    .While B$esi > 0
        lodsb
        .If al > B$esi
            mov eax &MF_END
        .Else_If al = B$esi
            mov eax 0
        .Else
            push esi
                While B$esi > al
                    inc esi
                End_While
                If B$esi < al
                    mov eax &MF_POPUP__&MF_END
                Else
                    mov eax &MF_POPUP
                End_If
            pop esi
       .End_If
       or D$edi POPMASK | xor D$edi POPMASK | or D$edi eax
       add edi 4
    .End_While
ret


[Beep | On B$SoundsWanted = &TRUE, call BeepIt]

BeepIt: pushad | call 'USER32.MessageBeep' &MB_ICONHAND | popad | ret

call 'USER32.MessageBeep' &MB_ICONHAND
;;
0xFFFFFFFF Standard beep using the computer speaker
&MB_ICONASTERISK SystemAsterisk
&MB_ICONEXCLAMATION SystemExclamation
&MB_ICONHAND SystemHand
&MB_ICONQUESTION SystemQuestion
&MB_OK SystemDefault
;;

[ClickMessage: ?   OnMenuEdition: ?    MenuEditorHandle: ?]

Proc MenuEditProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    mov eax D@wParam | shr eax 16 | mov D$ClickMessage eax

    If D@Message = &WM_CTLCOLOREDIT
        call SetMenuItemFlag | mov B$OnMenuEdition &TRUE
    End_If

    ...If D@Message = &WM_COMMAND
        mov eax D@lParam
        .If eax = D$MenuEditHandle
            mov eax D@wParam | shr eax 16
            If eax = &EN_CHANGE
                call MenuLinesControl | popad | mov eax &TRUE | jmp L9>>
            End_If
        .End_If

        ..If D@wParam = &IDCANCEL
            mov B$OnMenuEdition &FALSE, D$MenuEditorHandle 0
            call 'User32.EndDialog' D@Adressee 0

        ..Else_If D@wParam = &IDOK
            call CleanMenuEnd
            If eax = &FALSE
                mov D$MenuEditorHandle 0 | call 'User32.EndDialog' D@Adressee 0
            End_If
            call StoreMenuPopFlags | call StoreMenuEdition
            If D$FirstMenuId <> 0
                call PackMenuInList
                mov B$OnMenuEdition &FALSE, D$MenuEditorHandle 0
                call 'User32.EndDialog' D@Adressee 0
            End_If

      ..Else_If D@wParam = ID_EquToClip
            call CleanMenuEnd
            If eax = &TRUE
                call StoreMenuPopFlags
                call StoreMenuEdition
                On D$FirstMenuId <> 0, call ClipEquates
            End_If

            On B$TagedEdition = &TRUE, mov B$TagedEdition 0-1

      ..Else_If D@wParam = ID_MenuHelp
            call Help, B_U_AsmName, MenuHelp, ContextHlpMessage

      ..Else_If D$ClickMessage = &BN_CLICKED
            mov eax D@lParam, ebx MenuItemsFlags               ; D$lParam = from what
            add ebx D$IndexToMenuItemsFlags

            If eax = D$hGrayed
                xor D$ebx &MFS_GRAYED
            Else_If eax = D$hChecked
               xor D$ebx &MFS_CHECKED
            Else_If eax = D$hRight
               xor D$ebx &MFT_RIGHTJUSTIFY
            Else_If eax = D$hRadio
                xor D$ebx &MFT_RADIOCHECK
            End_If

      ..Else
            jmp L8>>

      ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        call FillMenuEditData | call InitMenuEdition D@Adressee
        call 'User32.SetDlgItemTextA' D@Adressee, 065, MenuEdition
        call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINECOUNT, 0, 0
        mov D$NumberOfMenuLines eax
            call MenuLinesControl
        move D$MenuEditorHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

        If B$TagedEdition = &TRUE
            call 'USER32.SendDlgItemMessageA' D@Adressee, ID_EquToClip,
                                              &WM_SETTEXT, 0, {'Replace in Source', 0}
        End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:   popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP



NewMenu:
    mov esi MenuList                               ; (ID / Ptr / Size)
    While D$esi > 0
        mov eax D$esi | add esi 12
    End_While

    mov D$MenuListPtr esi

    If esi = MenuList
        mov eax 1000
    Else
        add eax 1000
    End_If
    mov D$uMenu_ID eax
    mov ebx 10, edi FirstMenuID | add edi 5
L0: mov edx 0 | div ebx
    dec edi | add dl '0' | mov B$edi dl | cmp edi FirstMenuID | ja L0<
    While B$edi = '0'
        mov B$edi ' ' | inc edi
    End_While

    mov eax 0, ecx MENU_DWORDS, edi eMenu | rep stosd     ; clear temporary table
    mov D$EndOfeMenu eMenu

    call MenuEditor
ret


SaveMenuBinaryFile:
  ; Prevent from Multiple Instances:
    If B$OnMenuEdition = &TRUE
        Beep | ret
    End_If

    mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
    While B$UserTellWhatMenu = &FALSE
        call WhatMenu
    End_While
    On D$MenuListPtr = 0, ret

    mov edi SaveFilter, eax 0, ecx 65 | rep stosd
    mov D$SaveDlgFilter 'New.', D$SaveDlgFilter+3 '.bmf', D$SaveDlgFilter+7 0

    call 'Comdlg32.GetSaveFileNameA' OpenDlg | On eax = &FALSE, ret

    call ForceExtension SaveDlgFilter, '.bmf'

    call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                               &FILE_SHARE_READ, 0,
                               &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox | ret
    End_If

    mov D$DestinationHandle eax, D$NumberOfReadBytes 0

    mov esi D$MenuListPtr | mov ecx D$esi+8, esi D$esi+4

    call 'KERNEL32.WriteFile' D$DestinationHandle, esi, ecx, NumberOfReadBytes  0

    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
ret


[BinMenuMemory: ?   BinMenuLength: ?]

LoadMenuBinaryFile:
    If B$OnMenuEdition = &TRUE
        Beep | ret
    End_If

    mov D$OtherFilesFilters BinMenuFilesFilters
    mov D$OpenOtherFileTitle DialogFilesTitle

    move D$OtherhwndFileOwner D$hwnd, D$OtherhInstance D$hInstance

    mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc

    On D$OtherSaveFilter = 0, ret

    call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | ret  ; return to caller of caller
    Else
      mov D$OtherSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize' eax, 0 | mov D$BinMenuLength eax

    If eax > 0
        VirtualAlloc BinMenuMemory eax

        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$OtherSourceHandle, D$BinMenuMemory,
                                 D$BinMenuLength, NumberOfReadBytes, 0
    Else
        ret
    End_If

    call 'KERNEL32.CloseHandle' D$OtherSourceHandle

    mov esi MenuList | While D$esi <> 0 | add esi 12 | End_While
    mov eax D$esi-12 | add eax 1000 | mov D$esi eax
    mov eax D$BinMenuMemory, D$esi+4 eax
    mov eax D$BinMenuLength, D$esi+8 eax
ret


ReplaceMenuBinaryFile:
    If B$OnMenuEdition = &TRUE
        Beep | ret
    End_If

    mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
    While B$UserTellWhatMenu = &FALSE
        call WhatMenu
    End_While
    On D$MenuListPtr = 0, ret

    mov D$OtherFilesFilters BinMenuFilesFilters
    mov D$OpenOtherFileTitle DialogFilesTitle

    move D$OtherhwndFileOwner D$hwnd, D$OtherhInstance D$hInstance

    mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc

    On D$OtherSaveFilter = 0, ret

    call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | ret  ; return to caller of caller
    Else
      mov D$OtherSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize' eax, 0 | mov D$BinMenuLength eax

    If eax > 0
        VirtualAlloc BinMenuMemory eax

        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$OtherSourceHandle, D$BinMenuMemory,
                                 D$BinMenuLength, NumberOfReadBytes, 0
    Else
        ret
    End_If

    call 'KERNEL32.CloseHandle' D$OtherSourceHandle

    mov esi D$MenuListPtr
    push esi
        VirtualFree D$esi+4
    pop esi
    mov eax D$BinMenuMemory, D$esi+4 eax
    mov eax D$BinMenuLength, D$esi+8 eax
ret


MenuEditor:
    If D$MenuEditorHandle = 0
        call 'User32.DialogBoxIndirectParamA' D$hinstance, MenuDialogData, D$hwnd,
                                              MenuEditProc, 0
    Else
        Beep
    End_If
ret


[NoResourceMenu: '   There is no Menu in Resources    ', 0]
[UserTellWhatMenu: ?]

ExistingMenu:
    mov esi MenuList                                ; (ID / Ptr / Size)
    If D$esi = 0
       call 'User32.MessageBoxA' D$hwnd, NoResourceMenu, Argh,
                                 &MB_ICONINFORMATION+&MB_SYSTEMMODAL
       ret
    Else_If D$esi+12 = 0
      mov D$MenuListPtr MenuList
    Else
      mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
      While B$UserTellWhatMenu = &FALSE
        call WhatMenu
      End_While

      On D$MenuListPtr = 0, ret
    End_If

    ; MenuListPtr > choosen or alone menu

ReEditExistingMenu:

    mov esi D$MenuListPtr
    lodsd | mov D$uMenu_ID eax
      mov ebx 10, edi FirstMenuID | add edi 5
L0:     mov edx 0 | div ebx
          dec edi | add dl '0' | mov B$edi dl | cmp edi FirstMenuID | ja L0<
          While B$edi = '0'
            mov B$edi ' ' | inc edi
          End_While
    lodsd | mov ebx eax                           ; Ptr
    lodsd | mov ecx eax                           ; Size
    mov esi ebx, edi eMenu | rep movsb
    mov D$EndOfeMenu edi

    call MenuEditor
ret


[DelteMenuQuestion: 'Suppress choosen Menu from resources?', 0
 Sure: 'Are you sure...', 0]

DeleteMenu:
    mov esi MenuList                                ; (ID / Ptr / Size)

    If D$esi = 0
       call 'User32.MessageBoxA' D$hwnd, NoResourceMenu, Argh,
                                 &MB_ICONINFORMATION+&MB_SYSTEMMODAL
       ret
    Else
        mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
        While B$UserTellWhatMenu = &FALSE
            call WhatMenu
        End_While

        On D$MenuListPtr = 0, ret
    End_If

    call 'User32.MessageBoxA' D$hwnd, DelteMenuQuestion, Sure,
                             &MB_ICONQUESTION+&MB_SYSTEMMODAL+&MB_YESNO
   .If eax = &IDYES
        call VerifyNotDialogMenu
        If B$CancelDeleteMenu = &FALSE
            mov esi D$MenuListPtr, edi esi | add esi 12
            mov ecx MAXMENU, eax esi | sub eax MenuList | sub ecx eax
            rep movsd
        End_If
   .End_If
ret


; User asked for delete a menu. If the menu is used by a Dialog we must ajust the Dialog
; data if user persist:

[CancelDeleteMenu: ?]

VerifyNotDialogMenu:
    mov B$CancelDeleteMenu &FALSE
    mov ecx 0, edx D$MenuListPtr, edx D$edx                  ; whished delete Menu ID
    mov esi DialogList | add esi 4 | mov D$DialogListPtr esi

    While D$esi > 0
      mov ebx D$DialogListPtr | mov ebx D$ebx    ; ebx > DialogData > menu (0 / 0FFFF)
      mov ax W$ebx+18

      .If ax = 0FFFF                             ; (see ex.: EditedDialogBoxData)
         mov ax W$ebx+20                         ; Menu ID
         If ax = dx
           call 'User32.DialogBoxIndirectParamA' D$hinstance DelMenuDialog D$hwnd,
                                                DelDialogMenuProc 0
           On B$CancelDeleteMenu = &TRUE, ret
         End_If
      .End_If

      add D$DialogListPtr 12 | mov esi D$DialogListPtr
    End_While
ret


Proc DelDialogMenuProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND

        .If D@wParam = &IDCANCEL
            mov D$CancelDeleteMenu &TRUE | call 'User32.EndDialog' D@Adressee 0

        .Else_If D@wParam = &IDOK
            mov edi D$DialogListPtr, edi D$edi, eax 0   ; scratch menu record in Dialog Data:
            add edi 18 | stosw                          ; replace 0FFFF by 0
            mov esi edi | add esi 2                     ; (see ex: EditedDialogBoxData)

          ; We can't copy simply, because of alignement to reset before first control.
            If W$esi = 0                                ; copy Class record
                movsw
            Else_If W$esi = 0FFFF
                movsd
            Else
                While W$esi <> 0 | movsw | End_While | movsw
            End_If

            If W$esi = 0                                ; copy Title record
                movsw
            Else
                While W$esi <> 0 | movsw | End_While | movsw
            End_If

            While W$esi <> 0 | movsw | End_While | movsw; we have strip one Word >

; If aligment is good now , it was not previously and had require the add of a zeroed word
            Test edi 00_111 | jz L1>
                mov eax 0 | stosw | jmp L2>

L1:         add esi 2

          ; copy remaining controls:
L2:         mov eax D$DialogListPtr                     ; > List recorded lenght of Dialog data
            add eax 4 | mov ecx D$eax                   ; lenght of data do NOT change
            sub ecx 26 | rep movsb                      ; 'about...'

            call 'User32.EndDialog' D@Adressee, 0
        .End_If

    ...Else_If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

    ...Else
        popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


[DelMenuDialog: D$ 090C408C2 0 ; Style
 U$ 03 0 0 09C 042             ; Dim  8c
 0                             ; no Menu
 0                             ; Class(not yet)
 'Delete Dialog Menu???...' 0  ; Title
 08 'Helv' 0]                  ; Font

[DMD0: D$ 050000000 0          ; Style
 U$ 014 015 0100 018           ; Dim
 022B                          ; ID
 0FFFF 082                     ; Class
 'This Menu is used by one or more Dialog(s).' 0 ; Title
 0]                            ; No creation data

[DMD1: D$ 050000000 0          ; Style
 U$ 02 02D 038 013             ; Dim
 02                            ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data

[DMD2: D$ 050000000 0          ; Style
 U$ 03C 02D 04D 013            ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'Delete in Dialog too' 0      ; Title
 0]                            ; No creation data

____________________________________________________________


[WhatMenuData: D$ 090CC08C2 0  ; Style
 U$ 04 0 0 01F9 080            ; Dim 88 > 80
 0                             ; Menu
 0                             ; Class(not yet)
 'WhatMenu Dialog' 0           ; Title
 08 'Helv' 0]                  ; Font

[WMD0000: D$ 050000000 0       ; Style
 U$ 0 060 044 013              ; Dim
 064                           ; ID
 0FFFF 080                     ; Class
 '<<<<' 0                      ; Title
 0]                            ; No creation data

[WMD0001: D$ 050000000 0       ; Style
 U$ 0167 060 091 014           ; Dim
 01                            ; IDOK
 0FFFF 080                     ; Class
 'Yeah! This is this one i was searching!' 0 ; Title
 0]                            ; No creation data

[WMD0002: D$ 050000000 0       ; Style
 U$ 046 060 047 013            ; Dim
 065                           ; ID
 0FFFF 080                     ; Class
 '>>>>' 0                      ; Title
 0]                            ; No creation data

[WMD0003: D$ 050000000 0       ; Style
 U$ 0B0 060 0B5 014            ; Dim 60 > 90 > 75
 02                            ; IDCancel
 0FFFF 080                     ; Class
 "I don't find it. Maybe i've lost it, i am so stupid..." 0 ; Title
 0]                            ; No creation data

 [IDNextMenu 3  IDPreviousMenu 4]


WhatMenu:
    call 'User32.DialogBoxParamA' D$hinstance, 500, D$hwnd, WhatMenuProc, 0
ret


[ActualMenutestID: ?]

; Tag Dialog 500

Proc WhatMenuProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND

        .If D@wParam = &IDCANCEL
            mov D$MenuListPtr 0, B$UserTellWhatMenu &TRUE
            call 'User32.EndDialog' D@Adressee 0

        .Else_If D@wParam = &IDOK
            mov B$UserTellWhatMenu &TRUE
            call 'User32.EndDialog' D@Adressee 0

        .Else_If D@wParam = IDNextMenu
            mov ebx D$MenuListPtr | add ebx 12
            If D$ebx > 0
                mov D$MenuListPtr ebx | call SetTestMenu D@Adressee
                call 'User32.EndDialog' D@Adressee 0
            End_If

        .Else_If D@wParam = IDPreviousMenu
            mov ebx D$MenuListPtr | sub ebx 12
            If ebx >= MenuList
            mov D$MenuListPtr ebx
            call 'User32.DestroyMenu' D$ActualMenutestID
            call SetTestMenu D@Adressee
            call 'User32.EndDialog' D@Adressee 0
            End_If

        .End_If

    ...Else_If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call SetTestMenu D@Adressee

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


Proc SetTestMenu:
    Argument @Adressee

      mov eax D$MenuListPtr | add eax 4
      call 'User32.LoadMenuIndirectA' D$eax
      mov D$ActualMenutestID eax
      call 'User32.SetMenu' D@Adressee eax
EndP


; We associate each user edited line for menu with a table of flag. In case user suppress
; or add one line, we have to ajust the flag table:

; Copy whole user edition to "eMenu" (reuse of final storage area):

MenuCopy:
    mov edx 0, edi ceMenu, eax 0, ecx MENU_DWORDS | rep stosd
    mov edi ceMenu
    While edx < D$NumberOfMenuLines
L0:     pushad
            mov W$edi 160
            push edi
                call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, edx, edi
            pop edi
            mov B$edi+eax 0  ; >>> 'EM_GETLINE_Comment'
        popad
        mov al 0, ecx 200 | repne scasb
        inc edx
    End_While
ret


[MenuEditCaretPos: ? ?]
SearchMenuLineChange:
    call 'USER32.GetCaretPos' MenuEditCaretPos
  ; Zero based X/Y in eax: Y >>> High Word // X >>> Low Word:
    mov eax D$MenuEditCaretPos+4 | shl eax 16 | or eax D$MenuEditCaretPos

    call 'USER32.SendMessageA' D$MenuEditHandle, &EM_CHARFROMPOS, 0, eax
  ; Line in the high Word:
    shr eax 16 | mov edx eax
ret

;;
    mov edx 0, esi ceMenu

L0: push edx, esi
        mov eax 0, edi OneItemString, ecx 40 | rep stosd       ; GETLINE not zero-ended
        mov W$OneItemString 160                                ; max write for GETLINE
        call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, edx, OneItemString
        mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'
    pop esi, edx

    cmp eax 0 | je L9>
    mov edi OneItemString, ecx eax | repe cmpsb | jne L9>
    cmp B$esi 0 | jne L9>
        inc edx | inc esi | jmp L0<<
L9: ret
;;

; edx > 0 based index of suppressed item.

DeleteMenuFlagRecord:
    inc edx | mov edi MenuItemsFlags | shl edx 2 | add edi edx
    lea esi D$edi+4
    While esi < MenuItemsFlagsEnd | movsb | End_While
ret

    mov ecx MenuItemsFlagsEnd | sub ecx MenuItemsFlags | shr ecx 2
    sub ecx edx    ; Number of dWords to move backward.

    shl edx 2
    mov esi MenuItemsFlags | add esi edx | mov edi esi | add esi 4
    rep movsd
ret


; edx > 0 based index of added item.

InsertMenuFlagRecord:
    mov edi MenuItemsFlagsEnd, esi edi | sub esi 4
    mov ecx MenuItemsFlagsEnd | sub ecx MenuItemsFlags | shr ecx 2
    sub ecx edx    ; Number of dWords to move forward
    std | rep movsd | cld
    mov eax 0 | stosd
ret


[NumberOfMenuLines: ?]

MenuLinesControl:
    call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINECOUNT, 0, 0

    If eax = D$NumberOfMenuLines
        call MenuCopy
    Else_If eax < D$NumberOfMenuLines   ; user has suppressed one item:
        mov D$NumberOfMenuLines eax
        call SearchMenuLineChange       ; edx > 0 based index of suppressed line
        call DeleteMenuFlagRecord
    Else                                ; user has added one item:
        mov D$NumberOfMenuLines eax
        call SearchMenuLineChange       ; edx > 0 based index of added line
        call InsertMenuFlagRecord
        call IndentMenuItem
    End_If
ret


[ItemLevel: ?] ; = how many tab in previous item

[ItemTab: B$ 9 0]

IndentMenuItem:
    push edx
        dec edx
        mov eax 0, edi OneItemString, ecx 40 | rep stosd       ; GETLINE not zero-ended
        mov W$OneItemString 160                                ; max write for GETLINE
        call 'User32.SendMessageA' D$MenuEditHandle, &EM_GETLINE, edx, OneItemString
        mov B$OneItemString+eax 0  ; >>> 'EM_GETLINE_Comment'
        mov D$ItemLevel 0, esi OneItemString
        While B$esi = tab
            inc D$ItemLevel | inc esi
            pushad
                call 'User32.SendMessageA' D$MenuEditHandle &EM_REPLACESEL &TRUE ItemTab
            popad

        End_While
    pop edx
ret


; Tab is used by edit controls/dialog box as a selector. We set in normal:

[mEditRet: ?  mEditAdressee: ?  mEditMessage: ?  mEditWparam: ?  mEditLparam: ?]

mEditProc:
    pop D$mEditRet, D$mEditAdressee, D$mEditMessage, D$mEditWparam, D$mEditLparam
    push D$mEditRet

    .If D$mEditMessage = &WM_KEYDOWN
        If D$mEditWparam = tab
            call StoreTabInClipBoard
            call 'User32.SendMessageA' D$MenuEditHandle &WM_PASTE 0  0
            mov eax &FALSE | ret
        End_If
    .End_If

L9: call 'User32.CallWindowProcA' D$PreviousEditProc D$mEditAdressee,
                                 D$mEditMessage, D$mEditWparam, D$mEditLparam
    ret


[TabForEditControl: B$ tab]

StoreTabInClipBoard:
    pushad
        push D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside
            mov D$BlockStartTextPtr TabForEditControl, D$BlockEndTextPtr TabForEditControl
            mov B$BlockInside &TRUE | call ControlC | mov B$BlockInside &FALSE
        pop D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
    popad
ret


