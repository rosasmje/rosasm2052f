TITLE Struct
____________________________________________________________________________________________
____________________________________________________________________________________________

; The Structures DialogBox ([Struct] Menu option):

[WinStructures: ?]
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 Data in the .str Files are like this:

     [POINT:|x D|y D
     [POINTS:|x W|y W
     [RECT:|left D|top D|right D|bottom D

 For multi-units records:

     [FONTSIGNATURE:|fsUsb4 D 4|fsCsb2 D 2
                              ^          ^
 Nested Structures are expanded:

     [RBHITTESTINFO:|pt.x D|pt.y D|flags D|iBand D
                     ^^^^^^^^^^^^^
 Equates Values are given as this:

     [MIXERCONTROLDETAILS_LISTTEXTA:|dwParam1 D|dwParam2 D|szName < MIXER_LONG_NAME_CHARS
                                                                    ^^^^^^^^^^^^^^^^^^^^^
 (Without the '&').
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[StructHelp: 'Structures', 0]

StructDialog:
    .If B$StructuresFileOK = &TRUE
        If D$StructHandle = 0
            call 'USER32.DialogBoxParamA' D$hinstance, 18000, &NULL, StrucProc, &NULL
        Else
            Beep
        End_If

    .Else
        call Help B_U_AsmName, IncludeFilesHelp, RosAsmHlpMessage

    .End_If
ret


[StructHandle: ?   StructComboHandle: ?    StructEditHandle: ?    StructTitleEditHandle: ?]

; Tag Dialog 18000

Proc StrucProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        move D$StructHandle D@Adressee

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

       ; call 'USER32.SetWindowLongA' D@Adressee &GWL_EXSTYLE &WS_EX_TOOLWINDOW

        call 'USER32.GetDlgItem' D@Adressee 10 | mov D$StructComboHandle eax
        call 'USER32.GetDlgItem' D@Adressee 11 | mov D$StructEditHandle eax
        call 'USER32.GetDlgItem' D@Adressee 30 | mov D$StructTitleEditHandle eax
        call InitStructListBox
        call InitSerFormFlag
        call 'USER32.GetDlgItem' D@Adressee 10
        call 'USER32.SetFocus' eax
        jmp L8>>

    ...Else_If D@Message = &WM_COMMAND
        .If W@wParam = 21
            mov D$StructHeadFlag NACKEDSTRUCT | call ReBuildStructForm
        .Else_If W@wParam = 22
            mov D$StructHeadFlag LOCALSTRUCT | call ReBuildStructForm
        .Else_If W@wParam = 23
            mov D$StructHeadFlag SEMISTRUCT | call ReBuildStructForm
        .Else_If W@wParam = 24
            mov D$StructHeadFlag DASHSTRUCT | call ReBuildStructForm
        .Else_If W@wParam = 40
            movzx eax W@wParam | call SetFormFlags | call ReBuildStructForm
        .Else_If W@wParam = 41
            movzx eax W@wParam | call SetFormFlags | call ReBuildStructForm
        .Else_If W@wParam = 42
            movzx eax W@wParam | call SetFormFlags | call ReBuildStructForm
        .Else_If W@wParam = 50
            If B$ZeroOrQuestionMark = '?'
                mov B$ZeroOrQuestionMark '0'
            Else
                mov B$ZeroOrQuestionMark '?'
            End_If
           call ReBuildStructForm
        .End_If

        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16
        .If eax = &CBN_SELCHANGE
            call InitStructureName | call BuildStructForm

        .Else_If eax = &EN_CHANGE
            If W@wParam = 30
                call GetStructureUserName | call ReBuildStructForm
            End_If

        .Else_If D@wParam = &IDCANCEL
            jmp L1>

        .Else_If D@wParam = &IDOK
            call ClipStructure
L1:         mov D$StructHandle 0
            VirtualFree D$WinStructures
            call 'USER32.EndDialog' D@Adressee 0

        .Else_If D@wParam = &IDHELP
            call Help, B_U_AsmName, StructHelp, ContextHlpMessage

        .Else_If eax = &BN_CLICKED
            mov eax D@wParam

       .End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@Message = &WM_CTLCOLORLISTBOX
L1:     call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP

____________________________________________________________________________________________

[StructTitle: ? # 50] [StructTitleLen: ?]
[UserStructTitle: ? #50]
[StructEditText: ? #1000]

; Fill the ComboBox with all 'WinStructures' Data:

InitStructListBox:
    call OpenStructureFile

    mov esi D$WinStructures
    While B$esi <> 0
        lodsb
        If al = '['
            mov edi StructTitle
L1:             lodsb | cmp al ':' | je L2>
                stosb | jmp L1<
L2:         mov B$edi 0
            call 'USER32.SendMessageA' D$StructComboHandle &CB_ADDSTRING 0 StructTitle
        End_If
    End_While
    mov D$StructTitle 0
ret


[MenuID: ?    SeveralStructuresFiles: ?    StructuresFileHandle: ?    StructuresFileSize: ?]
[ApiFileString: MenuItemString: B$ ? #&MAX_PATH]

OpenStructureFile:
    .If B$StructuresFileOK = &FALSE
        call Help, B_U_AsmName, IncludeFilesHelp, ContextHlpMessage | ret

    .Else_If B$SeveralStructuresFiles = &TRUE
        mov esi EquatesName, edi MenuItemString
        While B$esi <> 0 | movsb | End_While
        dec edi
        While B$edi <> '.' | dec edi | End_While

L0:     dec edi | cmp B$edi '\' | je L1>
                  cmp B$edi ':' | je L1>
                  cmp edi MenuItemString | ja L0<
                    jmp L2>
L1:     inc edi
L2:     mov ecx &MAX_PATH | add ecx MenuItemString | sub ecx edi
      ; Case when call from the ToolBar
        On D$MenuID = 0, mov D$MenuID 4001
        call 'USER32.GetMenuStringA' D$MenuHandle, D$MenuID, edi, ecx, &MF_BYCOMMAND

        mov esi MenuItemString
        While B$esi <> 0 | inc esi | End_While | mov D$esi '.str', B$esi+4 0

    .End_If

    call 'KERNEL32.CreateFileA' MenuItemString &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0
    mov D$StructuresFileHandle eax

    call 'KERNEL32.GetFileSize' eax 0 | mov D$StructuresFileSize eax

    VirtualAlloc WinStructures eax

    mov D$NumberOfReadBytes 0
    call 'KERNEL32.ReadFile' D$StructuresFileHandle, D$WinStructures,
                             D$StructuresFileSize, NumberOfReadBytes, 0

    call 'KERNEL32.CloseHandle' D$StructuresFileHandle
ret

____________________________________________________________________________________________

InitStructureName:
    call 'User32.SendMessageA' D$StructComboHandle &CB_GETCURSEL 0 0
    push eax
        call 'User32.SendMessageA' D$StructComboHandle &CB_GETLBTEXT eax StructTitle
        mov D$StructTitleLen eax
    pop eax
    call 'User32.SendMessageA' D$StructComboHandle &CB_GETLBTEXT eax UserStructTitle
    call 'User32.SendMessageA' D$StructTitleEditHandle &WM_SETTEXT 0 UserStructTitle
ret

____________________________________________________________________________________________

[OnModeHandle: ?   StructMode: ?] ; strucMode: 0 > Data    1 > Equates    2 > Stack Macro


InitSerFormFlag:
    mov eax D$StructHeadFlag | add eax 21
    call 'User32.SendDlgItemMessageA' D$StructHandle eax &BM_SETCHECK 1 0

    mov eax D$StructMode | add eax 40
    call 'User32.SendDlgItemMessageA' D$StructHandle eax &BM_SETCHECK 1 0
ret



SetFormFlags:
    mov D$StructMode eax | sub D$StructMode 40

    call 'User32.GetDlgItem' D$StructHandle eax | mov D$OnModeHandle eax

    call 'User32.GetDlgItem' D$StructHandle 40
    call 'User32.SendMessageA' eax &BM_SETCHECK 0 0
    call 'User32.GetDlgItem' D$StructHandle 41
    call 'User32.SendMessageA' eax &BM_SETCHECK 0 0
    call 'User32.GetDlgItem' D$StructHandle 42
    call 'User32.SendMessageA' eax &BM_SETCHECK 0 0

    call 'User32.SendMessageA' D$OnModeHandle &BM_SETCHECK 1 0
ret

____________________________________________________________________________________________


ReBuildStructForm:
    cmp B$StructTitle 0 | je L9>
        call SetStructHeadText | call BuildStructForm
L9: ret


BuildStructForm:
    mov edi StructEditText, eax 0, ecx 1000 | rep stosd

  ; First, search the structure inside 'WinStructures' list same for all forms):
    mov esi D$WinStructures, bl B$StructTitle, eax D$StructTitleLen

L1: inc esi
    While B$esi <> bl
        inc esi
    End_While
    cmp B$esi-1 '[' | jne L1<
    push esi
        mov ecx eax
        mov edi StructTitle | repe cmpsb
        mov dl B$esi
    pop esi | jne L1<
    cmp dl ':' | jne L1<
    add esi eax | inc esi

    If D$StructMode = 0
        call BuildStructDataForm
        sub edi 3 | mov al ']' | stosb |  mov B$edi 0
    Else_If D$StructMode = 1
       call BuildStructEquForm
       sub edi 3 | mov al ']' | stosb |  mov B$edi 0
    Else_If D$StructMode = 2
       call BuildStructStackForm
       sub edi 3 |  mov B$edi 0
    End_If

    call StripDoubleColon

    call 'User32.SendMessageA' D$StructEditHandle &WM_SETTEXT 0 StructEditText
ret


StripDoubleColon:
    mov esi StructEditText
    While B$esi > 0
        lodsb
        If al = ':'
            On B$esi = ':', mov B$esi ' '
        End_If
    End_While
ret

[ZeroOrQuestionMark: '?']

BuildStructDataForm:
  ; Write the Structure main name in the EditBox:
    mov edi StructEditText

    push esi
        mov esi UserStructTitle
        mov al '[' | stosb
        While B$esi <> 0
            movsb
        End_While
        mov al ':' | stosb
    pop esi
    mov al 13 | stosb | mov al 10 | stosb | mov al ' ' | stosb

; Write the items:
;
; [REBARBANDINFO:|cbSize D|fMask D|fStyle D|clrFore D|cl...
;                ^
    call SetStructHeadText

L0: call WriteStructHead
    inc esi                                           ; jmp over first '|'
    If B$esi+1 = '|'
        ; case of missing names: "[DDEUP:|D|D'
    Else_If B$esi+1 < ' '
        ; ... Same for: "[DDELN:|D"
    Else
        While B$esi <> ' '
        movsb
        End_While
        mov al ':' | stosb
        inc esi
    End_If

; [REBARBANDINFO:|cbSize D|fMa...
;                        ^
    ..If B$esi+1 = '|'
L3:     If B$esi = 'B'
            mov eax ' B$ '
        Else_If B$esi = 'W'
            mov eax ' W$ '
        Else_If B$esi = 'D'
            mov eax ' D$ '
        Else_If B$esi = 'Q'
            mov eax ' Q$ '
        Else_If B$esi = 'F'
            mov eax ' F$ '
        Else_If B$esi = 'U'
            mov eax ' U$ '
        Else_If B$esi = 'T'
            mov eax ' T$ '
        End_If
        stosd | mov eax 0200A0D30 | mov al B$ZeroOrQuestionMark | stosd
      ; 0200A0D30 =  '0' 13 10 ' '
        inc esi

    ..Else_If B$esi+1 = 13
        jmp L3<

    ..Else_If B$esi+1 = ' '
        add edi 2

L4:         dec edi | cmp B$edi 13 | jne L4<
L4:         dec esi | cmp B$esi-1 '|' | jne L4<
            On B$edi-1 = ']', dec edi
          ; No more need of Square Brackets for Multiple Data:
           ; mov eax 05B0A0D5D | stosd              ;  05B0A0D5D = ']' 13 10 '['
           mov W$edi CRLF, B$edi+2 ' ' | add edi 3
                call WriteStructHead
                While B$esi <> ' '
                    movsb
                End_While
            mov al ':' | stosb | inc esi

        If B$esi = 'B'
            mov eax ' B$ '
        Else_If B$esi = 'W'
            mov eax ' W$ '
        Else_If B$esi = 'D'
            mov eax ' D$ '
        Else_If B$esi = 'U'
            mov eax ' U$ '
        Else_If B$esi = 'Q'
            mov eax ' Q$ '
        Else_If B$esi = 'F'
            mov eax ' F$ '
        Else_If B$esi = 'T'
            mov eax ' T$ '
        End_If

        stosd | dec edi | mov eax ' 0 #' | stosd | mov al B$ZeroOrQuestionMark, B$edi-3 al
        add esi 2
        If B$esi >= 'A'
            mov al '&' | stosb  ; If it is a Win Equate instead of a value.
        End_If
        While B$esi <> '|'
            On B$esi = 13, jmp L5>
            movsb
        End_While
     ; No more need of Square Brackets for Multiple Data:
L5:   ;  mov eax 05B0A0D5D | stosd   ; 05B0A0D5D =  ']' 13 10 '['
      mov W$edi CRLF, B$edi+2 ' ' | add edi 3
        If B$esi <> '|'
          ; dec edi
        End_If
    ..Else
L6:     movsb | cmp B$esi '|' | je L7>
                cmp B$esi 13 | jne L6<
L7:     mov al 13 | stosb | mov al 10 | stosb | mov al ' ' | stosb
    ..End_If

L9: cmp B$esi '|' | je L0<<
ret


[StructDisplacement: ?]

BuildStructEquForm:
  ; Write the Structure main name in the EditBox:
  ; esi is on 'WinStructures'
    mov edi StructEditText, D$StructDisplacement 0

        mov al '[' | stosb

BuildFromEquRoutine:           ; reused (called by 'BuildStructStackForm')

; Write the items:
;
; [REBARBANDINFO:|cbSize D|fMask D|fStyle D|clrFore D|cl...
;                ^
    call SetStructHeadText

L0: call WriteStructHead

    inc esi                                           ; jmp over first '|'

    .If B$esi+1 = '|'
        ; case of missing names: "[DDEUP:|D|D'
    .Else_If B$esi+1 < ' '
        ; ... Same for: "[DDELN:|D"
    .Else
        While B$esi <> ' '
            movsb
            If W$esi = '::'
                inc esi | jmp L1>
            End_If
        End_While
L1:     mov eax 'Dis ' | stosd
    .End_If

    mov eax D$StructDisplacement
  ; Destination String pointed by edi. eax holds the value to translate in Ascii Decimal.
    mov dl 0FF | push edx                       ; Push stack end mark
    mov ecx 10
L1: mov edx 0
    div ecx | push edx | cmp eax 0 | ja L1<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L3>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L3: mov al 13 | stosb | mov al 10 | stosb | mov al ' ' | stosb

  ; Cases of '::', as found above:
    If B$esi = ':'
        jmp L0<
    End_If

    inc esi

; [REBARBANDINFO:|cbSize D|fMa|cl... |bReserved < 2|dwDa...|rcChild RECT<>|rcBand RECT<>...
;                        ^
    ..If B$esi+1 = '|'
        If B$esi = 'B'
            add D$StructDisplacement 1
        Else_If B$esi = 'W'
            add D$StructDisplacement 2
        Else_If B$esi = 'D'
            add D$StructDisplacement 4                                   ; WinStructures
        Else_If B$esi = 'Q'
            add D$StructDisplacement 8
        Else_If B$esi = 'F'
            add D$StructDisplacement 4
        End_If
        inc esi

    ..Else_If B$esi+1 = ' '
      ; Cases of multiple Values (#n)
        mov bl B$esi | add esi 2 | mov ecx 0, eax 0
L4:     lodsb
        cmp al '|' | je L6>>
        cmp al '9' | ja L4>
        cmp al '0' | jb L5>>

                sub al '0'                  ; convert Decimal to binary:
                lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
                lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
        jmp L4<
L4:
            mov D$imm32 0
            pushad
                mov edi DataLoopWinEquate
                ;mov al '&' | stosb |
                dec esi
                While B$esi > ' '
                    movsb | On B$esi = '|', jmp L4>
                End_While
L4:             mov al 0 | stosb
                mov esi DataLoopWinEquate
                mov B$ShowWinEquateError &FALSE

                call ReadWin32Equate | on B$EquateFound = &TRUE, mov D$imm32 eax
            popad

            If B$EquateFound = &TRUE
                mov ecx D$imm32
            Else
                mov ecx 1
            End_If

            mov B$ShowWinEquateError &TRUE, D$imm32 0
            While B$esi >= 'A'
                inc esi | On B$esi = '|', jmp L5>
            End_While
            If D$esi = ' + 1'
                inc ecx | add esi 4
            End_If

L5:   ; Error case

L6:     If bl = 'B'
            mov eax 1
        Else_If bl = 'W'
            mov eax 2
        Else
            mov eax 4
        End_If
        mul ecx | add D$StructDisplacement eax | dec esi

    ..End_If

        While B$esi <> '|'
            On B$esi = 13, jmp L9>
            inc esi
        End_While

L9: cmp B$esi '|' | je L0<<
ret


BuildStructStackForm:

    mov edi StructEditText | add edi 100 | mov D$StructDisplacement 0

    call BuildFromEquRoutine

    While B$esi <> '|'        ; Back one item to count the last one size
        dec esi
    End_While

    While B$esi <> ' '
        inc esi
    End_While
    inc esi

    .If B$esi+1 <= CR
        If B$esi = 'B'
            add D$StructDisplacement 1
        Else_If B$esi = 'W'
            add D$StructDisplacement 2
        Else_If B$esi = 'D'
            add D$StructDisplacement 4
        Else_If B$esi = 'Q'
            add D$StructDisplacement 8
        Else_If B$esi = 'F'
            add D$StructDisplacement 4
        End_If
        inc esi
;;
    .Else    ;_If B$esi+1 = ' '
  ; In fact(, it apears that, when the Structure is ended by a Table, this Table Length
  ; is already counted inside the D$StructDisplacement provided by 'BuildFromEquRoutine'.
        mov bl B$esi | add esi 2 | mov ecx 0
L4:     lodsb
        cmp al '9' | ja L5>
        cmp al '0' | jb L5>
        cmp al '|' | je L6>
                sub al '0'                  ; convert Decimal to binary:
                lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
                lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
        jmp L4<
L5:     ; Error case
L6:     If bl = 'B'
            mov eax 1
        Else_If bl = 'W'
            mov eax 2
        Else
            mov eax 4
        End_If
        mul ecx | add D$StructDisplacement eax | dec esi
;;

    .End_If

    mov edi StructEditText, esi UserStructTitle  ; Write "Structure @NAME"
    mov eax 'Stru' | stosd | mov eax 'ctur' | stosd | mov ax 'e ' | stosw | mov al '@' | stosb
        While B$esi <> 0
            movsb
        End_While
        mov al ' ' | stosb

    mov eax D$StructDisplacement
    Align_On 4 eax                              ; Stack must remain aligned, whatever.

  ; Destination String pointed by edi. eax holds the value to translate in Ascii Decimal.
    mov dl 0FF | push edx                       ; Push stack end mark
    mov ecx 10
L0: mov edx 0
    div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
       add al '0' | stosb | jmp L2<             ; Write
L9:
    mov ax ', ' | stosw

    mov esi StructEditText | add esi 100

    While B$esi <> 0
        lodsb
        If al = 13
            mov al ','
        Else_If al = 10
            mov al ' '
        End_If
        stosb
    End_While
ret

____________________________________________________________________________________________


[StructHeadText: ? #50]

WriteStructHead:
    push esi
        mov esi StructHeadText
        While B$esi > 0
            movsb
        End_While
    pop esi
ret

[NACKEDSTRUCT 0    LOCALSTRUCT 1    SEMISTRUCT 2    DASHSTRUCT 3]

[StructHeadFlag: ?]

; Defining What text will (or none) be added at the begining of each symbol:

SetStructHeadText:
    pushad
        mov edi StructHeadText

        If B$StructMode = 2
            mov al '@' | stosb | jmp L1>
        End_If

        .If D$StructHeadFlag = NACKEDSTRUCT
            ; 0

        .Else_If D$StructHeadFlag = LOCALSTRUCT
            mov al '@' | stosb

        .Else
L1:         call GetStructureUserName
            mov esi UserStructTitle
            While B$esi <> 0
                movsb
            End_While
            If D$StructHeadFlag = SEMISTRUCT
                mov al '.' | stosb
            Else
                mov al '_' | stosb
            End_If

        .End_If

        mov al 0 | stosb
    popad
ret
____________________________________________________________________________________________

[UserStructName: ? #50]

GetStructureUserName:
    call 'USER32.SendMessageA' D$StructTitleEditHandle &WM_GETTEXT 150 UserStructTitle
ret

____________________________________________________________________________________________

[ClipStructureMemory: ?]

ClipStructure:
    push D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside

        VirtualAlloc ClipStructureMemory 4000
        move D$BlockStartTextPtr D$ClipStructureMemory

        call 'USER32.SendMessageA' D$StructEditHandle, &WM_GETTEXT, 4000, D$ClipStructureMemory

        If eax > 0
            add eax D$BlockStartTextPtr
            mov B$BlockInside &TRUE, D$BlockEndTextPtr eax
            call ControlC

            VirtualFree D$ClipStructureMemory
        End_If

L9: pop D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
ret


