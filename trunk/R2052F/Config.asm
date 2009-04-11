TITLE Config
 _______________________________________________________________________________________
 _______________________________________________________________________________________

; Configuration tab:

[StaticClassName: B$ 'STATIC', 0
 ButtonClassName:    'BUTTON' 0]

[ConfigTabHandle: ?]

[FirstTab:   B$ 'Tree View', 0       FirstTabLen: len
 SecondTab:  B$ 'Sources Editor', 0  SecondTabLen: len
 ThirdTab:   B$ 'Bad Habits' 0       ThirdTabLen: len
 FourthTab:   B$ 'Colors' 0          FourthTabLen: len
 FifthTab:  B$ 'Help Files' 0        FifthTabLen: len
 SixthTab:   B$ 'Companion Files' 0  SixthTabLen: len
 SeventhTab:   B$ 'User Menu' 0      SeventhTabLen: len
 HeigthTab: B$ 'Pos' 0               HeigthTabLen: len
 NinethTab: B$ 'Lang' 0              NinethTabLen: len
 DBPMenuTab: B$ 'DBP' 0              DBPMenuLen: len
 APItab:     B$ 'API', 0             APItabLen: len]

[CalcName: 'Calc.exe', 0] [CalcNameTail: 0 #60]                        ; If same dir
[EquatesName: 'Equates.equ', 0] [EquatesNameTail: 0 #60]
[F2Name: ? #60]
[CalcLinkName: 'C:\Program Files\BaseCalc\BASECALC.EXE', 0  OnCalc: 0] ; If Link to...

;[Asm32TutName: 'Asm32Help.exe', 0]      [Asm32TutNameTail: 0 #60]
[Win32HlpName: 'Win32.hlp', 0]          [Win32HlpNameTail: 0 #60] ;&MAX_PATH 0104  / 4 > 65d
[MmediaHlpName: 'Mmedia.hlp', 0]        [MmediaHlpNameTail: 0 #60]
[OpenGlHlpName: 'OpenGl.hlp', 0]        [OpenGlHlpNameTail: 0 #60]
;[OpcodeHlpName: 'x86eas.hlp', 0]        [OpcodeHlpNameTail: 0 #60]
;[WinDataFileName: 'WindowsData.inc', 0]     [WinDataFileNameTail: 0 #60]
[DxHlpName:    'directx.chm', 0]        [DxHlpNameTail: 0 #60]
[WinsockHlpName: 'Winsock.hlp', 0]      [WinsockHlpNameTail: 0 #60]
[SDLRefName: 'SDLRef.chm', 0]           [SDLRefNameTail: 0 #60]
[sqliteName: 'sqlite.hlp', 0]           [sqliteNameTail: 0 #60]
[DevILName: 'DevIL.html', 0]            [DevILNameTail: 0 #60]
[ClipName: 'Clip.txt', 0]               [ClipNameTail: 0 #60]
[B_U_AsmName: 'B_U_Asm.exe', 0]      [B_U_AsmNameTail: 0 #60]

[EntryPointLabel: B$ 'MAIN' 0 0 0 0 0 0 0 0 0 0
 EntryPointLabelLen: len]       ; upper case needed
;;
  Take care, that since the implementation of the "Tag Main NewName" Feature, this 
  'EntryPointLabelLen' can no more be used as such, and must be re-computed:
;;
[CallBackName: B$ 'MainWindowProc'  CallBackNameLen: len]

[TC_ITEM:
 TC_imask: &TCIF_TEXT__&TCIF_IMAGE   TC_lpReserved1: 0   TC_lpReserved2: 0
 TC_pszText: 0   TC_cchTextMax: 0    TC_iImage: 0-1      TC_lParam: 0]

[ITB1: "

Tree view is build by analyze of main labels and of calls to these main labels® If you validate [List orphans], the list will be added all 'non-called' main labels.

If you validate [First call only],  and some label is called many times, it will be listed at first call occurence only.", 0 ]

 ________________________________________________________________________

[TabDialogHandle: ?     ConfigTabbedDialogHandle: ?    ConfigTabIndex: ?]
[TabIs: 4    CtrlYflag: 0    SecurityWanted: &FALSE    SoundsWanted: &TRUE]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Providing the required Path for "RosAsm Files" (Equates.equ and friends).

[FolderPath: B$ ? #&MAXPATH]
[PathInfo: B$ 'Choose a Path', 0]
[PathTitle: B$ 'For RosAsm Files', 0]

[BROWSEINFOA:
 BROWSEINFOA.hwndOwner: D$ 0
 BROWSEINFOA.pidlRoot: D$ 0
 BROWSEINFOA.pszDisplayName: D$ 0
 BROWSEINFOA.lpszTitle: D$ 0
 BROWSEINFOA.ulFlags: D$ &BIF_RETURNONLYFSDIRS__&BIF_DONTGOBELOWDOMAIN__&BIF_USENEWUI
 BROWSEINFOA.lpfn: D$ cbBrowse
 BROWSEINFOA.lParam: D$ PathInfo
 BROWSEINFOA.iImage: D$ 0]

[BrowseForFolderAborted: ?]

Proc BrowseForFolder:
    Arguments @hParent, @PathTitle

        mov D$FolderPath 0
        move D$BROWSEINFOA.hwndOwner D@hParent
        move D$BROWSEINFOA.lpszTitle D@PathTitle
        call 'SHELL32.SHBrowseForFolder' BROWSEINFOA

        If eax <> &FALSE
            push eax
                call 'SHELL32.SHGetPathFromIDList' eax, FolderPath
            pop eax
            mov B$BrowseForFolderAborted &FALSE

        Else
            mov B$BrowseForFolderAborted &TRUE

        End_If

        call 'OLE32.CoTaskMemFree' eax
Endp


Proc cbBrowse:
    Argument @hWin, @uMsg, @lParam, @lpData

    call 'USER32.SetWindowTextA'  D@hWin, D@lpData
EndP


; User has selected a Path for RosAsmFiles (Equates.equ *must* be there).

[MustHaveEquatesPath: "
You have not provided a Path to Equates.equ.

RosAsm is going to shut down, because it can't do anything       
whithout the OS Equates List.

", 0]


[BadEquatesPath: "
You have provided a Path for 'Equates.equ',
but this File is not there.

           Try again?


", 0]


VerifyEquatesPath:
    ..If D$FolderPath = 0
        If B$IncludesOK = &FALSE
L1:         call 'USER32.MessageBoxA', D$hwnd, MustHaveEquatesPath, Argh, &MB_SYSTEMMODAL
            call 'KERNEL32.ExitProcess' 0
        End_If

        mov eax 0-1

    ..Else
        mov eax FolderPath
        While B$eax <> 0 | inc eax | End_While
        mov D$eax '\Equ', D$eax+4 'ates', D$eax+8 '.equ', B$eax+12 0
        mov eax FolderPath

        call 'KERNEL32.FindFirstFileA' FolderPath, FindFile

        .If eax = &INVALID_HANDLE_VALUE
            call 'USER32.MessageBoxA' D$hwnd, BadEquatesPath, Argh,
                                      &MB_YESNO+&MB_SYSTEMMODAL+&MB_ICONQUESTION
            If eax = &IDNO
                On B$IncludesOK = &FALSE, jmp L1<<
                mov eax 0-1

            Else
                mov eax 0

            End_If

        .Else
            call 'KERNEL32.FindClose' eax

            mov eax FolderPath

        .End_If

    ..End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Tag Dialog 3000 >>> Main
; Tag Dialog 4000 >>> ListView
; Tag Dialog 4100 >>> Sources Editor
; Tag Dialog 4200 >>> Bad Habit
; Tag Dialog 4300 >>> Colors
; Tag Dialog 4400 >>> Help Files
; Tag Dialog 4500 >>> Companion Files
; Tag Dialog 4600 >>> User Menu
; Tag Dialog 4700 >>> Pos
; Tag Dialog 4800 >>> National Language
; Tag Dialog 4900 >>> Definition of the BP Menu

Configuration:
    If D$ConfigDialogHandle = 0
        call 'USER32.DialogBoxParamA' D$hinstance, 3000, &NULL, ConfigTabProc, &NULL
    Else
        Beep
    End_If
ret


InitConfigDialogTab:
    call 'USER32.GetDlgItem' D$ConfigDialogHandle, 100 | mov D$ConfigTabHandle eax

    mov D$TC_pszText FirstTab | move D$TC_cchTextMax, D$FirstTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 0, TC_ITEM

    move D$TC_pszText SecondTab | move D$TC_cchTextMax D$SecondTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 1, TC_ITEM

    move D$TC_pszText ThirdTab | move D$TC_cchTextMax, D$ThirdTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 2, TC_ITEM

    move D$TC_pszText FourthTab | move D$TC_cchTextMax D$FourthTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 3, TC_ITEM

    move D$TC_pszText FifthTab | move D$TC_cchTextMax D$FifthTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 4, TC_ITEM

    move D$TC_pszText SixthTab | move D$TC_cchTextMax D$SixthTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 5, TC_ITEM

    move D$TC_pszText SeventhTab | move D$TC_cchTextMax, D$SeventhTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 6, TC_ITEM

    move D$TC_pszText HeigthTab | move D$TC_cchTextMax D$HeigthTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 7, TC_ITEM

    move D$TC_pszText NinethTab | move D$TC_cchTextMax D$NinethTabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 8, TC_ITEM

    move D$TC_pszText DBPMenuTab | move D$TC_cchTextMax D$DBPMenuLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 9, TC_ITEM

    move D$TC_pszText APItab | move D$TC_cchTextMax D$APItabLen
    call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_INSERTITEM, 10, TC_ITEM
ret

____________________________________________________________________________________________

; The Main Proc for Configuration:

[ConfigDialogHandle: ?]

; Tag Dialog 4100 [Text Editor]

Proc ConfigTabProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        .If D$ConfigDialogHandle = 0
            move D$ConfigDialogHandle D@Adressee
            call InitConfigDialogTab
            call 'USER32.CreateDialogParamA' D$hinstance, 4000, D$ConfigTabHandle,
                                             ConfigTabProc, &NULL
            mov D$ConfigTabbedDialogHandle eax, D$ConfigTabIndex 0
            call InittabTreeView
            call 'USER32.SetClassLongA' D$ConfigDialogHandle &GCL_HICON D$wc_hIcon
        .End_If
        jmp L8>>

    ...Else_If D@Message = &WM_NOTIFY
        mov esi D@lParam, eax D$esi+8
        ..If eax = &TCN_SELCHANGE
            call 'User32.SendMessageA' D$ConfigTabHandle, &TCM_GETCURSEL, 0, 0
            mov D$ConfigTabIndex eax

            call 'User32.DestroyWindow' D$ConfigTabbedDialogHandle
            mov eax D$ConfigTabIndex, ecx 100 | mul ecx | add eax 4000
push D$StringsLanguage ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call 'USER32.CreateDialogParamA' D$hinstance, eax, D$ConfigTabHandle,
                                             ConfigTabProc, &NULL
            mov D$ConfigTabbedDialogHandle eax
pop D$StringsLanguage  ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            .If D$ConfigTabIndex = 0
                call InittabTreeView
            .Else_If D$ConfigTabIndex = 1
                call InitTabTextEditor
            .Else_If D$ConfigTabIndex = 2
                call InitBadHabits
            .Else_If D$ConfigTabIndex = 3
                call InitTabColors
            .Else_If D$ConfigTabIndex = 4
                call InitTabHelpFiles
            .Else_If D$ConfigTabIndex = 5
                call InitTabOtherFiles
            .Else_If D$ConfigTabIndex = 6
                call InitTabUserMenu
            .Else_If D$ConfigTabIndex = 7
                call InitTabPos
            .Else_If D$ConfigTabIndex = 8
                call InitTabLang
            .Else_If D$ConfigTabIndex = 9
                call InitTabdbp
            .Else_If D$ConfigTabIndex = 10
; Tag Dialog 5000
                call InitTabAPI
            .End_If

        ..End_If

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If D@wParam = &IDCANCEL
            jmp L1>

        ..Else_If D@wParam = &IDOK
            call UpdateRegistry

            If D$UserPopUpHandle > 0
                call 'USER32.DeleteMenu' D$MenuHandle 7 &MF_BYPOSITION
            End_If
            call AddUserMenu | call EnableMenutems | call EnableHelpMenutems
            call 'USER32.DrawMenuBar' D$hwnd
L1:         call 'User32.DestroyWindow' D@Adressee
            mov D$ConfigDialogHandle 0

        ..Else_If D@wParam = &IDHELP
            call Help, B_U_AsmName, ConfigHelp, ContextHlpMessage

        ..Else_If eax = &BN_CLICKED
            mov eax D@wParam
            .If D$ConfigTabIndex = 0                ; Tree View.
                If eax = 11                         ; Check Orphans
                    xor B$ShowOrphan &TRUE
                Else_If eax = 12                    ; Check First call only
                    xor B$ShowLabelsOnce &TRUE
                Else_If eax = 13                    ; Check Tree View Auto-Hide
                    call CloseTree
                    xor B$AutoHideTreeView &TRUE
                Else_If eax = 14                    ; Rebuild tree after each Compilation
                    xor B$AutoRebuildTreeView &TRUE
                End_If

            .Else_If D$ConfigTabIndex = 1           ; Text Editor.
                call GetEditorConfig D@Adressee


            .Else_If D$ConfigTabIndex = 2           ; Bad Habits.
                If eax = 20
                    xor B$CtrlYFlag &TRUE
                Else_If eax = 21
                    xor B$BlockAutoDelete &TRUE
                Else_If eax = 22
                    xor B$NoVirtualLimit &TRUE
                Else_If eax = 23
                    xor B$WithControlA &TRUE
                End_If

L2:         .Else_If D$ConfigTabIndex = 3           ; Colors.
                pushad
                    If eax = 20                     ;D$Color1Handle
                        mov eax D$NormalBackColor | call SetColor
                        mov D$NormalBackColor eax | call ResetBackGroundColors
                    Else_If eax = 10                ; Color 2
                        mov eax D$StatementColor | call SetColor | mov D$StatementColor eax
                    Else_If eax = 11                ; Color 3
                        mov eax D$CommentColor | call SetColor | mov D$CommentColor eax
                    Else_If eax = 12                ; Color 4
                        mov eax D$TextColor | call SetColor | mov D$TextColor eax
                    Else_If eax = 13                ; Color 5
                        mov eax D$BracketColor | call SetColor | mov D$BracketColor eax
                    Else_If eax = 21                ; Color of Edit Controls, List Boxes,...
                        mov eax D$DialogsBackColor | call SetColor
                        mov D$DialogsBackColor eax | call ResetBackGroundColors
                    Else_If eax = 30
                        xor B$WantSizeMarkerColor &TRUE
                    End_If

                    call AskForRedrawNow
                popad

            .Else_If D$ConfigTabIndex = 4           ; Help Files.
                If eax = 500
                    call Help, B_U_AsmName, HelPFiles, ContextHlpMessage
                Else
                    call ConfigHelpFiles
                End_If

            .Else_If D$ConfigTabIndex = 5           ; Other Files

                pushad
                    push eax
                        If eax = 14
L1:                         call BrowseForFolder D@Adressee, PathTitle
                            call VerifyEquatesPath | On eax = &FALSE, jmp L1<
                                                     On eax <> 0-1, jmp L1>
                            pop eax | jmp L2>
                        Else
                            call ConfigSearchFile       ; returns Ptr or 0 in eax
                        End_If
L1:                 pop ebx
                    On eax = 0, jmp L2>>
                    mov esi eax
                    If ebx = 10
                        mov edi CalcName
                    Else_If ebx = 11
                        mov edi ClipName
                    Else_If ebx = 12
                        mov edi B_U_AsmName
                    Else_If ebx = 13
                        mov edi F2Name
                    Else_If ebx = 14
                        mov edi EquatesName
                    Else
                        jmp L2>
                    End_If

                    Do
                        lodsb | stosb
                    Loop_Until al = 0

                    call InitTabOtherFiles
L2:             popad

            .Else_If D$ConfigTabIndex = 6           ; User Menu
                On eax < 310, jmp L1>
                    On eax > 320, jmp L1>
                        call StripOneMenuUserItem | jmp L7>>

L1:             pushad
                    call CheckUserMenuItemText | On eax = 0, jmp L2>>
                popad
                pushad
                    push eax
                        call ConfigSearchFile       ; returns Ptr or 0 in eax
                    pop ebx
                    On eax = 0, jmp L2>>
                    mov esi eax

                    If ebx = 110
                        mov edi UserMenu0Path
                    Else_If ebx = 111
                        mov edi UserMenu1Path
                    Else_If ebx = 112
                        mov edi UserMenu2Path
                    Else_If ebx = 113
                        mov edi UserMenu3Path
                    Else_If ebx = 114
                        mov edi UserMenu4Path | I9: jmp I9>    ; just because no more '.'If
                    Else_If ebx = 115
                        mov edi UserMenu5Path
                    Else_If ebx = 116
                        mov edi UserMenu6Path
                    Else_If ebx = 117
                        mov edi UserMenu7Path
                    Else_If ebx = 118
                        mov edi UserMenu8Path
                    Else_If ebx = 119
                        mov edi UserMenu9Path
                    Else
                        jmp L2>
                    End_If

                    Do
                        lodsb | stosb
                    Loop_Until al = 0
                    call SaveUserMenuTab | call InitTabUserMenu
L2:             popad

            .Else_If D$ConfigTabIndex = 7           ; Pos
                If eax = 50
                    On B$SaveMainPosFlag = &FALSE, call GetUserPosTab
                Else_If eax = 51
                    On B$SaveMainPosFlag = &FALSE, call SaveUserPosTab
                Else_If eax = 52
                    call SetMaximized
                Else_If eax = 20
                    xor B$SaveMainPosFlag &TRUE | call InitTabPos
                End_If

          ; Tag Dialog 4800

             .Else_If D$ConfigTabIndex = 8           ; Lang
                call SetNationalLanguage

             .Else_If D$ConfigTabIndex = 9           ; DBPMenu
                call SetDBPMenu

             .Else_If D$ConfigTabIndex = 10          ; API verifications
                sub eax 10 | mov D$ApiCheckFlag eax

             .End_If

        ..End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@Message = &WM_CTLCOLORLISTBOX
L1:     call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

L7: popad | mov eax &TRUE

L9: EndP
____________________________________________________________________________________________

[NATION_CHOOSEFONT: ; EditorCHOOSEFONT
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hDC: D$ 0
 @lpLogFont: D$ NATION_LOGFONT
 @iPointSize: D$ 0
 @Flags: D$ &CF_SCREENFONTS__&CF_INITTOLOGFONTSTRUCT
 @rgbColors: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0
 @hInstance: D$ 0
 @lpszStyle: D$ &SCREEN_FONTTYPE
 @nFontType: W$ 0
 @Alignment: W$ 0
 @nSizeMin: D$ 0
 @nSizeMax: D$ 0]

[NATION_LOGFONT: ; EditorLOGFONT
 @lfHeight: D$ 0
 @lfWidth: D$ 0
 @lfEscapement: D$ 0
 @lfOrientation: D$ 0
 @lfWeight: D$ 0
 @lfItalic: B$ 0
 @lfUnderline: B$ 0
 @lfStrikeOut: B$ 0
 @lfCharSet: B$ 0
 @lfOutPrecision: B$ 0
 @lfClipPrecision: B$ 0
 @lfQuality: B$ 0
 @lfPitchAndFamily: B$ 0
 @lfFaceName: B$ 0 #&LF_FACESIZE
 NATION_LOGFONTlen: len] ; NATION_LOGFONT@lfFaceName

[NationalFontHandle: ?]

GetNationalFont:
    move D$NATION_CHOOSEFONT@hwndOwner D$ConfigDialogHandle
    call 'Comdlg32.ChooseFontA' NATION_CHOOSEFONT

    If eax = &TRUE
        On D$NationalFontHandle <> 0, call 'GDI32.DeleteObject' D$NationalFontHandle
        call 'GDI32.CreateFontIndirectA' NATION_LOGFONT | mov D$NationalFontHandle eax
    End_If
ret


ConfigHelpFiles:
    pushad
        push eax
            call ConfigSearchFile       ; returns Ptr or 0 in eax
        pop ebx
        On eax = 0, jmp L2>>
        mov esi eax
        .If ebx = 10
        ;  mov edi Asm32TutName
        .Else_If ebx = 11
            mov edi Win32HlpName
        .Else_If ebx = 12
            mov edi MmediaHlpName
        .Else_If ebx = 13
            mov edi OpenGlHlpName
        .Else_If ebx = 14
        ; mov edi OpcodeHlpName
        .Else_If ebx = 15
        ;   mov edi WinDataFileName ;| I9: jmp I9>
    ; .Else_If ebx = 16
    ;     mov edi B_U_AsmName    ; Now only in [Other Files]
        .Else_If ebx = 17
            mov edi DxHlpName
        .Else_If ebx = 18
            mov edi WinsockHlpName
        .Else_If ebx = 19
            mov edi SDLRefName
        .Else_If ebx = 20
            mov edi sqliteName
        .Else_If ebx = 21
            mov edi DevILName
        .End_If

        Do
            lodsb | stosb
        Loop_Until al = 0
        call InitTabHelpFiles
L2: popad
ret


Proc GetEditorConfig:
    Argument @Parent

        .If eax = 11                    ; Tab is 2 spaces
            mov D$TabIs 2
        .Else_If eax = 12                    ; Tab is 4 spaces
            mov D$TabIs 4
        .Else_If eax = 13                    ; Tab is 8 spaces
            mov D$TabIs 8

        .Else_If eax = 50
            call XorScrollBar
        .Else_If eax = 51
            xor B$SecurityWanted &TRUE
        .Else_If eax = 52
            xor B$DollarOnly &TRUE
        .Else_If eax = 53                         ; Indent
            xor D$AutoIndentFlag &TRUE
            On B$AutoIndentFlag = &FALSE, mov D$AutoIndent 0
        .Else_If eax = 54
            xor B$LoadMRU &TRUE
           ; call 'USER32.CheckDlgButton' D@Parent 54 D$LoadMRU
        .Else_If eax = 55
            call XorToolBar
        .Else_If eax = 56
            xor B$SoundsWanted &TRUE
        .Else_If eax = 57
            xor B$CompletionWanted &TRUE
       ; .Else_If eax = 58
       ;     xor B$WriteCheckerWanted &TRUE
        .Else_If eax = 58
            If B$ParagraphChar = 167    ; Paragraph
                mov B$ParagraphChar 36  ; Dollar
            Else
                mov B$ParagraphChar 167
            End_If
        .Else_If eax = 201
            call GetMaxBackUp
        .Else
            call CheckFontAndBlink
        .End_If
EndP


GetMaxBackUp:
    mov D$MaxBackUp 0
    call 'USER32.GetDlgItemTextA' D$ConfigTabbedDialogHandle, 200,
                                  MaxBackUp, 5

    On B$MaxBackUp = 0, mov D$MaxBackUp '0000'

    While B$MaxBackUp+3 = 0
        shl D$MaxBackUp 8 | mov B$MaxBackUp '0'
    End_While

    call UpDateRegistry
ret


XorScrollBar:
    xor D$ScrollBarWanted &TRUE | call RedrawInterface
ret


XorToolBar:
    xor D$ToolBarWanted &TRUE | call RedrawInterface
ret


InittabTreeView:
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 10, &WM_SETTEXT, 0, ITB1
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 11, &BM_SETCHECK,
                                      D$ShowOrphan, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 12, &BM_SETCHECK,
                                      D$ShowLabelsOnce, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 13, &BM_SETCHECK,
                                      D$AutoHideTreeView, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 14, &BM_SETCHECK,
                                      D$AutoRebuildTreeView, 0
ret

[NotYet: ' Will be available for next release...' 0]

InitTabTextEditor:
    mov ebx D$TabIs | and ebx 2 | shr ebx 1
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 11, &BM_SETCHECK, ebx, 0
    mov ebx D$TabIs | and ebx 4 | shr ebx 2
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 12, &BM_SETCHECK, ebx, 0
    mov ebx D$TabIs | and ebx 8 | shr ebx 3
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 13, &BM_SETCHECK, ebx, 0

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 50, &BM_SETCHECK,
                                      D$ScrollBarWanted, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 51, &BM_SETCHECK,
                                      D$SecurityWanted, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 52, &BM_SETCHECK,
                                      D$DollarOnly, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 53, &BM_SETCHECK,
                                      D$AutoIndentFlag, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 54, &BM_SETCHECK,
                                      D$LoadMRU, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 55, &BM_SETCHECK,
                                      D$ToolBarWanted, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 56, &BM_SETCHECK,
                                      D$SoundsWanted, 0
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 57, &BM_SETCHECK,
                                      D$CompletionWanted, 0
   ; call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 58, &BM_SETCHECK,
   ;                                   D$WriteCheckerWanted, 0
    If B$ParagraphChar = 167
        mov eax &TRUE
    Else
        mov eax &FALSE
    End_If
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 58, &BM_SETCHECK,
                                      eax, 0


    mov eax MaxBackUp | While B$eax = '0' | inc eax | End_While
    On B$eax = 0, dec eax
    call 'USER32.SetDlgItemTextA' D$ConfigTabbedDialogHandle, 200, eax
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 200, &EM_SETLIMITTEXT, 4, 0

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 301, &BM_SETCHECK,
                                      D$BlinkingCaretWanted, 0
    call 'User32.SetDlgItemInt' D$ConfigTabbedDialogHandle, 302, D$CaretTime, 0
ret

____________________________________________________________________________________________

[BadHabittext: "

A Sources Editor is not a Text Editor. 


This is why i spend much work-time with writing the Sources Editor from scratch instead of doing what most other IDEs Authors do:

Making use of an Edit or Richedit Control.

If you disagree with my choices these Flags are under your own decision.


Again, my recommandation: 

Do not use these Flags. 
These actions are incompatible with Assembly Edition, because they are going to help to corrupt your Sources.", 0]

InitBadHabits:
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 10, &WM_SETTEXT,
                                      0, BadHabittext

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 20, &BM_SETCHECK,
                                      D$CtrlYFlag, 0

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 21, &BM_SETCHECK,
                                      D$BlockAutoDelete, 0

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 22, &BM_SETCHECK,
                                      D$NoVirtualLimit, 0

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 23, &BM_SETCHECK,
                                      D$WithControlA, 0
ret
____________________________________________________________________________________________


CheckFontAndBlink:
    .If eax > 13
        If eax = 20
            call SelectFont

        Else
            jmp CheckBlinkCaret

        End_If

    .End_If
ret

CheckBlinkCaret:
    If eax = 301
        call KillBlinkCursor
        xor B$BlinkingCaretWanted &TRUE
        On B$BlinkingCaretWanted = &TRUE, call InitBlinkCursor
        call AskForRedraw

    Else_If eax = 303
        call 'User32.GetDlgItemInt' D$ConfigTabbedDialogHandle, 302, 0, 0
        mov D$CaretTime eax
        call ResetBlinkCursor

    Else_If eax = 320
        xor B$CompletionWanted &TRUE

    End_If
ret


InitTabColors:
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 30, &BM_SETCHECK,
                                      D$WantSizeMarkerColor, 0
ret


[HelpFilesPathsPtrs: Win32HlpName, MmediaHlpName, OpenGlHlpName,
                     DxHlpName, WinsockHlpName, SDLRefName, sqliteName, DevILName, 0]

InitTabHelpFiles:
;    mov esi HelpFilesPathsPtrs, ebx 110
;L0: push ebx esi
;        call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle ebx &WM_SETTEXT 0 D$esi
;    pop esi ebx
;    add esi 4 | inc ebx | cmp D$esi 0 | ja L0<
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 111, &WM_SETTEXT 0,
                                      Win32HlpName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 112, &WM_SETTEXT 0,
                                      MmediaHlpName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 113, &WM_SETTEXT 0,
                                      OpenGlHlpName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 117, &WM_SETTEXT 0,
                                      DxHlpName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 118, &WM_SETTEXT 0,
                                      WinsockHlpName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 119, &WM_SETTEXT 0,
                                      SDLRefName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 120, &WM_SETTEXT 0,
                                      sqliteName
    call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 121, &WM_SETTEXT 0,
                                      DevILName
ret


[OthersFilesPathsPtrs: CalcName ClipName B_U_AsmName F2Name EquatesName 0]

InitTabOtherFiles:
    mov esi OthersFilesPathsPtrs, ebx 100
L0: push ebx esi
        If D$esi = EquatesName
            push esi
                mov esi EquatesName
                While B$esi <> 0 | inc esi | End_While
                While D$esi <> 'Equa' | dec esi | End_While
                mov B$esi 0
            pop esi
        End_If
        call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, ebx, &WM_SETTEXT, 0,
                                          D$esi
    pop esi ebx
    add esi 4 | inc ebx | cmp D$esi 0 | ja L0<

    mov esi EquatesName
    While B$esi <> 0 | inc esi | End_While
    On D$esi+1 = 'quat', mov B$esi 'E'
ret

;;
  Note: a Durty hack up there, because i don't want to rewrite all of the Routines
  concerned by 'EquatesName' (providing the Path to all RosAsm Files, now). Previously
  i did not know how to do what 'BrowseForFolder' actually does, and i was retrieving
  the Path by runing a usual 'GetOpenFileName' pointing to Equates.equ.
;;


[UserMenuItemsPtrs: UserMenu0String UserMenu1String UserMenu2String UserMenu3String
                    UserMenu4String UserMenu5String UserMenu6String UserMenu7String
                    UserMenu8String UserMenu9String 0

 UserMenuPathsPtrs: UserMenu0Path UserMenu1Path UserMenu2Path UserMenu3Path
                    UserMenu4Path UserMenu5Path UserMenu6Path UserMenu7Path
                    UserMenu8Path UserMenu9Path 0]

InitTabUserMenu:
    mov esi UserMenuItemsPtrs, edi UserMenuPathsPtrs, ebx 10, ecx 210

L0: push edi, ecx, esi, ebx
        call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, ecx, &WM_SETTEXT,
                                          0, D$edi
        pop ebx, esi | push esi, ebx
        call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, ebx, &WM_SETTEXT,
                                          0, D$esi
    pop ebx, esi, ecx, edi
    add esi 4 | add edi 4 | inc ebx | inc ecx
    cmp D$esi 0 | ja L0<
ret


; ebx = 310 Based ID of [x] Button.

StripOneMenuUserItem:
    sub eax 310
  ; 'UserMenu0String' length is 8 dWords (32 Bytes per Record)
  ; 'UserMenu0Path' is 64 dWords (256 Bytes per Record)
  ; (sizes for ease of pointing).

    shl eax 5 ; * 32  (8 * dWord)

    lea edi D$UserMenu0String+eax | mov esi edi | add esi (4*8)
    mov ecx (8*8) | rep movsd

    shl eax 3  ; this is 3 more times > * 256 (64 * dWord)

    lea edi D$UserMenu0Path+eax | mov esi edi | add esi (4*64)
    mov ecx (64*8) | rep movsd

    call InitTabUserMenu
ret


; Tag Dialog 4700 >>> Pos

InitTabPos:
     call 'USER32.SetDlgItemInt' D$ConfigTabbedDialogHandle, 10, D$WindowX, &NULL
     call 'USER32.SetDlgItemInt' D$ConfigTabbedDialogHandle, 11, D$WindowY, &NULL
     call 'USER32.SetDlgItemInt' D$ConfigTabbedDialogHandle, 12, D$WindowW, &NULL
     call 'USER32.SetDlgItemInt' D$ConfigTabbedDialogHandle, 13, D$WindowH, &NULL

     If B$IsMaximizedFlag = &SW_MAXIMIZE
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 52,
                                          &BM_SETCHECK &TRUE, 0
        mov eax &FALSE

     Else_If B$SaveMainPosFlag = &TRUE
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 20,
                                          &BM_SETCHECK &TRUE, 0
        mov eax &FALSE

     End_If

     push eax
     call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 10, &WM_ENABLE, eax, 0
     pop eax | push eax
     call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 11, &WM_ENABLE, eax, 0
     pop eax | push eax
     call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 12, &WM_ENABLE, eax, 0
     pop eax
     call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 13, &WM_ENABLE, eax, 0
ret


SetMaximized:
     If B$IsMaximizedFlag = &SW_SHOWNORMAL
        mov B$IsMaximizedFlag &SW_MAXIMIZE
     Else
        mov B$IsMaximizedFlag &SW_SHOWNORMAL
     End_If

     call 'USER32.ShowWindow'  D$hwnd, D$IsMaximizedFlag
ret

; Tag Dialog 4800

InitTabLang:
    .If D$StringsLanguage = '.en'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 10, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.fr'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 11, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.br'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 12, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.sp'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 13, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.zh'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 14, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.it'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 15, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.de'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 16, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.no'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 17, &BM_SETCHECK, 1, 0
    .Else_If D$StringsLanguage = '.ca'
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 18, &BM_SETCHECK, 1, 0
    .Else
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 10, &BM_SETCHECK, 1, 0
    .End_If

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 100, &EM_SETMARGINS,
                                      &EC_LEFTMARGIN__&EC_RIGHTMARGIN, 0100010

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 100, &WM_SETTEXT, 0,
    {"
This Tab is for the definition of the Language to be used for displaying the Errors Messages given at Compile-Time, by the Assembler. The method will be generalized to the other Messages progressively.

Before using, you must have the National Strings Files, in a '.../RosAsmFiles/Lang/' Folder. If you downloaded 'RosAsmFull.zip', you have nothing to do. If you dowloaded the partial Files, unzip 'LangFiles.zip', in such a Folder.

The [National Font] Button is for choosing the Font to be used for dispaying the Error Messages. For the 'Ascii Users', this is just an option, but for the 'Unicode Users', - actualy only the [Chinese] ones -, this Font MUST be defined.

If your Language is missing, in this list, and if you wish to do the translation work, go to your Directory of 'RosAsm Files', and, in the [Lang] Folder, take the 'RosAsmStrings.en' Files, in any Editor.

In this File:

The '$$$$1', '$$$$2', ... Markers are simple Strings IDs, that you have to leave the way they are. These IDs are used by RosAsm to point out what National String subsitution matches with the default one.

In case you would see some '#1', '#2', ... inside a String, let these special Markers, as they are. These ones are used by RosAsm to mark the Positions of some Components of the Strings. For example, if an Error Message includes a Variable Number, these Markers are replaced on the fly, by the wished Numbers.

When you are over with a Translation, send me your work by Mail, at < betov@free.fr >, so that i could include it, and implement the needed Radio Button is this Tab.

", 0}
ret
____________________________________________________________________________________________

[DOUBLE_CLICK_ACTION 2  RIGHT_CLICK_ACTION 3]

[DBPMenuOn: DOUBLE_CLICK_ACTION]

InitTabdbp:
    If D$DBPMenuOn = DOUBLE_CLICK_ACTION
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 11, &BM_SETCHECK, 1, 0
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 12, &BM_SETCHECK, 0, 0
    Else
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 11, &BM_SETCHECK, 0, 0
        call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 12, &BM_SETCHECK, 1, 0
    End_If
ret


[ApiCheckFlag: ?] ; 0 // &DONT_RESOLVE_DLL_REFERENCES // No Verifiction

[ApiCheckInfo: "
Beginners: Let this Flag [Normal].

When compiling a Source, RosAsm does several verifications on your API calls, in order to point you to typos, missing DLLs, and so on...

In some exeptional cases (WDM drivers, General Hocks, ...) this may be a problem. So, if you perfectly know what you are doing, two Flags let you define the behaviour of RosAsm:

With the DONT_RESOLVE_DLL_REFERENCES Flag, the verifications of your calls will not address the references inside the concerned DLLS.

The last Flag will completely skip your API calls verifications. You do not need to set it for WDM Drivers, because, in the case of .SYS files, no such verification, is done, anyway.", 0]

InitTabAPI:
    mov eax 10 | add eax D$ApiCheckFlag

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, eax, &BM_SETCHECK, 1, 0

    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 100, &WM_SETTEXT, 0, ApiCheckInfo
ret


SetDBPMenu:
    If eax = 11
        mov D$DBPMenuOn DOUBLE_CLICK_ACTION
    Else
        mov D$DBPMenuOn RIGHT_CLICK_ACTION
    End_If
ret
____________________________________________________________________________________________

[ConfigFilesFilters: B$ 'All'  0  '*.*'   0  0] [FullPathUserChoice: B$ ? #260]
[ConfigInitialDir: B$ ? #260]
[ConfigFileTitle: 'Choose the file to asign to the Menu', 0]

[CONFIGOPENFILENAME:
 @lStructSize: D$ LEN
 @hWndOwner: D$ 0
 @hInstance: D$ 0
 @lpstrFilter: D$ ConfigFilesFilters
 @lpstrCustomFilter: D$ 0
 @nMaxCustFilter: D$ 0
 @nFilterIndex: D$ 0
 @lpstrFile: D$ FullPathUserChoice
 @nMaxFile: D$ 260
 @lpstrFileTitle: D$ 0
 @nMaxFileTitle: D$ 0
 @lpstrInitialDir: D$ ConfigInitialDir
 @lpstrTitle: D$ ConfigFileTitle
 @Flags: D$ 0281804
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]

ConfigSearchFile:
    move D$CONFIGOPENFILENAME@hWndOwner D$ConfigDialogHandle
    move D$CONFIGOPENFILENAME@hInstance D$hInstance
    mov edi FullPathUserChoice, al 0, ecx 260 | rep stosb
    call 'COMDLG32.GetOpenFileNameA' CONFIGOPENFILENAME
    If B$FullPathUserChoice <> 0
        mov eax FullPathUserChoice
    Else
        mov eax 0
    End_If
ret

____________________________________________________________________________________________

; User ask for a 'ConfigSearchFile'. He must first set the Menu Item text:

[TitleErrorUserMenu: 'User Menu:', 0
 ErrorUserMenu:      'Enter the Menu item Text in the EditBox first.', 0]

CheckUserMenuItemText:
  ; > in: eax = ID of [File...] Button (first one = 110
    push eax
        sub eax 110 | shl eax 5
        lea edi D$UserMenu0String+eax
    pop eax
    push edi
        sub eax 100                             ; Menu Item Edit Control (First one = 10)
        call 'USER32.GetDlgItem' D$ConfigTabbedDialogHandle eax
    pop edi
    call 'USER32.SendMessageA' eax &WM_GETTEXT 30 edi
    If eax = 0
        call 'USER32.MessageBoxA' D$hwnd, ErrorUserMenu, TitleErrorUserMenu, &MB_SYSTEMMODAL
        mov eax &FALSE
    Else
        mov eax &TRUE
    End_If
ret


SaveUserMenuTab:
    pushad
        mov edi UserMenu0String, eax 10
L1:     push edi, eax
            call 'USER32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, eax,
                                              &WM_GETTEXT, 30, edi
        pop eax, edi
        inc eax | add edi 32 | cmp edi UserMenu9String | jbe L1<

        mov esi UserMenu0String, edi UserMenu0Path
L1:     On B$esi = 0, mov B$edi 0
        add esi 32 | add edi 256 | cmp edi UserMenu9Path | jbe L1<
    popad
ret



GetUserPosTab:
    call 'User32.GetWindowRect' D$hwnd WindowX
    If D$WindowX > 0FFF_FFFF
        mov D$WindowX 0
        mov D$WindowY 0
        call 'USER32.GetSystemMetrics' &SM_CXSCREEN | mov D$WindowW eax
        call 'USER32.GetSystemMetrics' &SM_CYSCREEN | mov D$WindowH eax
    Else
        mov eax D$WindowX | sub D$WindowW eax
        mov eax D$WindowY | sub D$WindowH eax
    End_If
    call InitTabPos
ret

SaveUserPosTab:
    call 'User32.SendDlgItemMessageA' D$ConfigTabbedDialogHandle, 20, &BM_GETCHECK, 0, 0
    mov D$SaveMainPosFlag eax
    call 'User32.GetDlgItemInt' D$ConfigTabbedDialogHandle, 10, &NULL, &NULL
    mov D$WindowX eax
    call 'User32.GetDlgItemInt' D$ConfigTabbedDialogHandle, 11, &NULL, &NULL
    mov D$WindowY eax
    call 'User32.GetDlgItemInt' D$ConfigTabbedDialogHandle, 12, &NULL, &NULL
    mov D$WindowW eax
    call 'User32.GetDlgItemInt' D$ConfigTabbedDialogHandle, 13, &NULL, &NULL
    mov D$WindowH eax

    call 'User32.MoveWindow' D$hwnd, D$WindowX, D$WindowY, D$WindowW, D$WindowH, &TRUE

    mov eax D$WindowX | or eax D$WindowY
    ..If eax = 0
        call 'USER32.GetSystemMetrics' &SM_CXSCREEN
        .If eax = D$WindowW
            call 'USER32.GetSystemMetrics' &SM_CYSCREEN
            If eax = D$WindowH
                call 'USER32.ShowWindow' D$hwnd, &SW_MAXIMIZE
            End_if
        .End_If
    ..End_If
ret

____________________________________________________________________________________________

[CustomColorsSet: ? #16]

[CHOOSECOLORAPI:
 CHOOSECOLORAPI_lStructSize: D$ len
 CHOOSECOLORAPI_hwndOwner: D$ 0
 CHOOSECOLORAPI_hInstance: D$ 0
 CHOOSECOLORAPI_rgbResult: D$ 0
 CHOOSECOLORAPI_lpCustColors: D$ CustomColorsSet
 CHOOSECOLORAPI_Flags: D$ &CC_RGBINIT__&CC_FULLOPEN
 CHOOSECOLORAPI_lCustData: D$ 0
 CHOOSECOLORAPI_lpfnHook: D$ 0
 CHOOSECOLORAPI_lpTemplateName: D$ 0]

SetColor:
        move D$CHOOSECOLORAPI_hwndOwner D$ConfigDialogHandle
        move D$CHOOSECOLORAPI_hInstance D$hInstance
        mov D$CHOOSECOLORAPI_rgbResult eax

        push eax                                          ; old color set by caller in eax
            call 'COMDLG32.ChooseColorA' CHOOSECOLORAPI
        pop ebx

        If eax = &FALSE
            mov eax ebx                                   ; restore old color if no choice.
        Else
            mov eax D$CHOOSECOLORAPI_rgbResult
        End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
; Registry jobs:

[hRegKey: 0    Result: 0    Datatype: 0    RegistryDataSize: 4]

[RosAsmConfigClass: B$ 'RosAsmConfigClass', 0
 RosAsmKey:      'Software\RosAsm', 0
 Color1:         'Color1', 0
 Color2:         'Color2', 0
 Color3:         'Color3', 0
 Color4:         'Color4', 0
 Color5:         'Color5', 0
 Color10:        'Color10', 0
 SizeColorFlag:  'SizeMarker Color Flag', 0

 UserLanguage:   'Lang', 0

 Blink:          'Blink', 0
 BlinkTime:      'BlinkTime', 0

 Complete:       'Complete', 0

 PrinterFont:    'PrinterFont', 0
 ScrollBar:      'ScrollBar', 0
 ToolBar:       'ToolBar', 0

 SourceEditorFont: 'SourceEditorFont', 0
 NationalLanguageFont: 'NationalLanguageFont', 0
 UnicodeEditorFont: 'UnicodeEditorFont', 0

 BackUpLimit: 'MaxBackUp', 0

 MainPosX:       'MainPosX', 0
 MainPosY:       'MainPosY', 0
 MainPosW:       'MainPosW', 0
 MainPosH:       'MainPosH', 0

 SaveMainPos:    'SaveMainPos', 0
 IsMaximized:    'IsMaximized', 0

 TreeViewReg:    'TreeViewReg', 0
 TreeViewReg2:   'TreeViewReg2', 0
 TreeViewAutoHide: 'TreeViewAutoHide', 0
 TreeRebuild:    'TreeRebuild', 0
 TreeW:          'Tree Width', 0

 AutoIndentReg:  'AutoIndentReg', 0
 TabReg:         'TabReg', 0
; Font:           'Font', 0
 CtrlYis:        'CtrlYis', 0
 LoadMRUis:      'LoadLastMRU', 0
 BlockAutoDeleteIs: 'BlockAutoDeleteIs', 0
 VirtualLimit:   'VirtualLimit', 0
 WithCtrlA:      'With Ctrl A', 0
 WithSecurity:   'WithSecurity', 0
 WithSounds:      'WithSounds', 0
 WithDollarOnly:  'Dollar only', 0
 WithChecker:     'Write Checker', 0
 ParagrafChar:    'ParagrafChar', 0

 CalcPath:       'CalcPath', 0
 EquatesPath:    'EquatesPath', 0
 F2Path:         'F2Path', 0
 RosAsmHelpPath:  'RosAsmHelpPath', 0
 Asm32HelpPath:  'Asm32HelpPath', 0
 WinApiPath:     'WinApiPath', 0
 AsmHelPath:     'AsmHelPath', 0
 MmediaPath:     'MmediaPath', 0
 OpenGlPath:     'OpenGlPath', 0
 DxPath:         'DxPath', 0
 WinSockPath:    'WinSockPath', 0
 SDLRefPath:     'SDLRefPath', 0
 sqlitePath:     'sqlitePath', 0
 DevILPath:      'DevILPath', 0
 ClipPath:       'ClipPath', 0
 StrucPath:      'StrucPath' 0

 LastUserPath:   'LastUserPath', 0

 UserMenu0:      'UserMenu0', 0
 UserMenu1:      'UserMenu1', 0
 UserMenu2:      'UserMenu2', 0
 UserMenu3:      'UserMenu3', 0
 UserMenu4:      'UserMenu4', 0
 UserMenu5:      'UserMenu5', 0
 UserMenu6:      'UserMenu6', 0
 UserMenu7:      'UserMenu7', 0
 UserMenu8:      'UserMenu8', 0
 UserMenu9:      'UserMenu9', 0
 UserPath0:      'UserMenuPath0', 0
 UserPath1:      'UserMenuPath1', 0
 UserPath2:      'UserMenuPath2', 0
 UserPath3:      'UserMenuPath3', 0
 UserPath4:      'UserMenuPath4', 0
 UserPath5:      'UserMenuPath5', 0
 UserPath6:      'UserMenuPath6', 0
 Userpath7:      'UserMenuPath7', 0
 UserPath8:      'UserMenuPath8', 0
 UserPath9:      'UserMenuPath9', 0

 MRU1Title:      'MRU1', 0
 MRU2Title:      'MRU2', 0
 MRU3Title:      'MRU3', 0
 MRU4Title:      'MRU4', 0
 DBPMenuString:  'DBPMenu', 0
 ApiCheckString: 'ApiCheck', 0
 UserConfigString: 'UserConfig', 0]

[UserMenu0String: ? #8] [UserMenu1String: ? #8] [UserMenu2String: ? #8]
[UserMenu3String: ? #8] [UserMenu4String: ? #8] [UserMenu5String: ? #8]
[UserMenu6String: ? #8] [UserMenu7String: ? #8] [UserMenu8String: ? #8]
[UserMenu9String: ? #8]
;&MAX_PATH = 0104   (0104/4) = 041 = 65
[UserMenu0Path: ? #64] [UserMenu1Path: ? #64] [UserMenu2Path: ? #64] [UserMenu3Path: ? #64]
[UserMenu4Path: ? #64] [UserMenu5Path: ? #64] [UserMenu6Path: ? #64] [UserMenu7Path: ? #64]
[UserMenu8Path: ? #64] [UserMenu9Path: ? #64]

[RegistryData:  Color1          &REG_DWORD  NormalBackColor
              ; NEVER INSERT ANYTHING !!!
                Color2          &REG_DWORD  StatementColor
                Color3          &REG_DWORD  CommentColor
                Color4          &REG_DWORD  TextColor
                Color5          &REG_DWORD  BracketColor
                Color10         &REG_DWORD  DialogsBackColor
                SizeColorFlag   &REG_DWORD  WantSizeMarkerColor
              ; NEVER INSERT ANYTHING !!!
                UserLanguage    &REG_DWORD  StringsLanguage
                NationalLanguageFont &REG_BINARY NATION_LOGFONT
                UnicodeEditorFont &REG_BINARY UNICODE_EDITION_LOGFONT
              ; NEVER INSERT ANYTHING !!!
                Blink           &REG_DWORD  BlinkingCaretWanted
                BlinkTime       &REG_DWORD  CaretTime
              ; NEVER INSERT ANYTHING !!!
                Complete        &REG_DWORD  CompletionWanted
              ; NEVER INSERT ANYTHING !!!
                PrinterFont     &REG_BINARY cbbuffer
                ScrollBar       &REG_DWORD  ScrollBarWanted
                ToolBar         &REG_DWORD  ToolBarWanted
              ; NEVER INSERT ANYTHING !!!
                SourceEditorFont &REG_BINARY EditorLOGFONT
              ; NEVER INSERT ANYTHING !!!
                BackUpLimit     &REG_DWORD  MaxBackUp
              ; NEVER INSERT ANYTHING !!!
                MainPosX        &REG_DWORD  WindowX
                MainPosY        &REG_DWORD  WindowY
                MainPosW        &REG_DWORD  WindowW
                MainPosH        &REG_DWORD  WindowH
              ; NEVER INSERT ANYTHING !!!
                SaveMainPos     &REG_DWORD  SaveMainPosFlag
                IsMaximized     &REG_DWORD  IsMaximizedFlag
              ; NEVER INSERT ANYTHING !!!
                TreeViewReg     &REG_DWORD  ShowOrphan
                TreeViewReg2    &REG_DWORD  ShowLabelsOnce
                TreeViewAutoHide &REG_DWORD AutoHideTreeView
                TreeRebuild     &REG_DWORD  AutoRebuildTreeView
                TreeW           &REG_DWORD  TreeWidth
              ; NEVER INSERT ANYTHING !!!
                AutoIndentReg   &REG_DWORD  AutoIndentFlag
                TabReg          &REG_DWORD  TabIs
               ; Font            &REG_DWORD  UserFont
                CtrlYis         &REG_DWORD  CtrlYflag
                LoadMRUis       &REG_DWORD  LoadMRU
                BlockAutoDeleteIs &REG_DWORD  BlockAutoDelete
                VirtualLimit    &REG_DWORD  NoVirtualLimit
                WithCtrlA       &REG_DWORD  WithControlA
                WithSecurity    &REG_DWORD  SecurityWanted
                WithSounds      &REG_DWORD  SoundsWanted
                WithDollarOnly  &REG_DWORD  DollarOnly
                WithChecker     &REG_DWORD  WriteCheckerWanted
                ParagrafChar    &REG_DWORD  ParagraphChar
              ; NEVER INSERT ANYTHING !!!
                CalcPath        &REG_SZ     CalcName
                EquatesPath     &REG_SZ     EquatesName
              ; NEVER INSERT ANYTHING !!!
                F2Path          &REG_SZ     F2Name
              ; NEVER INSERT ANYTHING !!!
                RosAsmHelpPath   &REG_SZ    B_U_AsmName

              ; NEVER INSERT ANYTHING !!!
                WinApiPath      &REG_SZ     Win32HlpName
              ;  AsmHelPath      &REG_SZ     OpcodeHlpName
                MmediaPath      &REG_SZ     MmediaHlpName
                OpenGlPath      &REG_SZ     OpenGlHlpName
                DxPath          &REG_SZ     DxHlpName
                WinSockPath     &REG_SZ     WinsockHlpName
                SDLRefPath      &REG_SZ     SDLRefName
                sqlitePath      &REG_SZ     sqliteName
                DevILPath       &REG_SZ     DevILName
                ClipPath        &REG_SZ     ClipName
              ; NEVER INSERT ANYTHING !!!
                UserMenu0       &REG_SZ     UserMenu0String
                UserMenu1       &REG_SZ     UserMenu1String
                UserMenu2       &REG_SZ     UserMenu2String
                UserMenu3       &REG_SZ     UserMenu3String
                UserMenu4       &REG_SZ     UserMenu4String
                UserMenu5       &REG_SZ     UserMenu5String
                UserMenu6       &REG_SZ     UserMenu6String
                UserMenu7       &REG_SZ     UserMenu7String
                UserMenu8       &REG_SZ     UserMenu8String
                UserMenu9       &REG_SZ     UserMenu9String
                UserPath0       &REG_SZ     UserMenu0Path
                UserPath1       &REG_SZ     UserMenu1Path
                UserPath2       &REG_SZ     UserMenu2Path
                UserPath3       &REG_SZ     UserMenu3Path
                UserPath4       &REG_SZ     UserMenu4Path
                UserPath5       &REG_SZ     UserMenu5Path
                UserPath6       &REG_SZ     UserMenu6Path
                UserPath7       &REG_SZ     UserMenu7Path
                UserPath8       &REG_SZ     UserMenu8Path
                UserPath9       &REG_SZ     UserMenu9Path
              ; NEVER INSERT ANYTHING !!!
                MRU1Title       &REG_SZ     MRU1
                MRU2Title       &REG_SZ     MRU2
                MRU3Title       &REG_SZ     MRU3
                MRU4Title       &REG_SZ     MRU4
              ; NEVER INSERT ANYTHING !!!
                DBPMenuString   &REG_DWORD  DBPMenuOn
              ; NEVER INSERT ANYTHING !!!
                ApiCheckString  &REG_DWORD  ApiCheckFlag

              ; ALWAYS ADD AT THE END !!!!!!!!!!!!!!!!!!!!

                0]

OpenRegistry:
    call 'ADVAPI32.RegCreateKeyExA' &HKEY_CURRENT_USER, RosAsmKey, 0,
                                    RosAsmConfigClass, 0,
                                    &KEY_READ__&KEY_WRITE__&KEY_QUERY_VALUE,
                                    0, hRegKey, Result
ret


ReadRegistry:
    mov esi RegistryData

    ...If D$Result = &REG_CREATED_NEW_KEY
      ; Case of new installation of the Registry:
L1:     push esi
            call GetFieldDataSize esi
            call 'ADVAPI32.RegSetValueExA' D$hRegKey, D$esi, 0, D$esi+4, D$esi+8, eax
        pop esi
        On eax <> &ERROR_SUCCESS, jmp AutoInit
        add esi 12 | cmp D$esi 0 | ja L1<<

        call AutoInit

    ...Else
      ; Case of existing Registry to be read:
L1:     push esi
            lea ebx D$esi+4 | mov D$RegistryDataSize 0FF
            call 'ADVAPI32.RegQueryValueExA' D$hRegKey, D$esi, 0, ebx, D$esi+8,
                                             RegistryDataSize
            .If eax <> &ERROR_SUCCESS
                pop esi | push esi
                call GetFieldDataSize esi
                call 'ADVAPI32.RegSetValueExA' D$hRegKey, D$esi, 0, D$esi+4, D$esi+8, eax
            .End_If
        pop esi
        add esi 12 | cmp D$esi 0 | ja L1<<
    ...End_If

    call CloseRegistry
ret


UpdateRegistry:
    On D$UserConfig = CONFIGFILE, jmp WriteConfigFile

    call OpenRegistry

    mov esi RegistryData
  ; either &REG_BINARY // &REG_DWORD // &REG_SZ
L1: push esi
        call GetFieldDataSize esi
        call 'ADVAPI32.RegSetValueExA' D$hRegKey, D$esi, 0, D$esi+4, D$esi+8, eax
    pop esi
    add esi 12 | cmp D$esi 0 | ja L1<<

    call CloseRegistry
ret


Proc GetFieldDataSize:
    Argument @Pointer
    Uses esi

        mov esi D@Pointer
      ; esi >>> One Key in 'RegistryData': PointerToName // &FLAG // RosAsmData
      ; &FLAG can be: &REG_BINARY // &REG_DWORD // &REG_SZ
        If D$esi+4 = &REG_DWORD
            mov eax 4
      ; cases of Fonts LOGFONT Structures:
        Else_If D$esi = PrinterFont
          ; Static size of cbbuffer (&REG_BINARY):
            mov eax 60
        Else_If D$esi = SourceEditorFont
            mov eax D$EditorLOGFONT.len
        Else_If D$esi = NationalLanguageFont
            mov eax D$NATION_LOGFONTlen
        Else_If D$esi = UnicodeEditorFont
            move eax D$UNICODE_EDITION_LOGFONTlen
      ; Cases of Strings (&REG_SZ):
        Else
            push edi
                mov edi D$esi+8, ecx 0FFFF, al 0 | repne scasb
                sub ecx 0FFFF | neg ecx | mov eax ecx
            pop edi
        End_If
      ; eax = Size of the Data to be saved into the Registry Key:
EndP


CloseRegistry:
    call 'ADVAPI32.RegCloseKey' D$hRegKey
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Auto initialize the companion Files registries if this is a first install done
  after unzipping the whole Package.
;;


AutoInit:  ; 'RegistryData'
    call GetDirectory ActualDir

    mov edi ActualDir | While B$edi <> 0 | inc edi | End_While

    On B$edi-1 <> '\', mov B$edi '\' | inc edi
  ; ...\RosAsmFiles\
    mov D$edi 'RosA', D$edi+4 'smFi', D$edi+8 'les\' | add edi 12
    mov D$ActualDirPointer edi

  ; 'Equates.inc'
    mov D$edi 'Equa', D$edi+4 'tes.', D$edi+8 'equ'
    call 'KERNEL32.FindFirstFileA' ActualDir, WIN32_FIND_DATA

    .If eax <> &INVALID_HANDLE_VALUE
        call 'KERNEL32.FindClose' eax
        mov esi ActualDir, edi EquatesName
        While B$esi <> 0 | movsb | End_While | mov B$edi 0

      ; 'B_U_Asm.exe'
        mov edi D$ActualDirPointer
        mov D$edi 'B_U_', D$edi+4 'Asm.', D$edi+8 'exe', B$edi+12 0
        call 'KERNEL32.FindFirstFileA' ActualDir, WIN32_FIND_DATA

        If eax <> &INVALID_HANDLE_VALUE
            call 'KERNEL32.FindClose' eax
            mov esi ActualDir, edi B_U_AsmName
            While B$esi <> 0 | movsb | End_While | mov B$edi 0
        End_If

      ; 'Clip.txt
        mov edi D$ActualDirPointer
        mov D$edi 'Clip', D$edi+4 '.txt', B$edi+8 0
        call 'KERNEL32.FindFirstFileA' ActualDir, WIN32_FIND_DATA
        If eax <> &INVALID_HANDLE_VALUE
            call 'KERNEL32.FindClose' eax
            mov esi ActualDir, edi ClipName
            While B$esi <> 0 | movsb | End_While | mov B$edi 0
        End_If

    .End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; add a User Menu if any.

[UserPopUpHandle: 0    UserPopMenu: 'User', 0]

[MENUITEMINFO:
 @cbSize: D$ len
 @fMask: D$ 0
 @fType: D$ &MFT_STRING
 @fState: D$ 0
 @wID: D$ 0
 @hSubMenu: D$ 0
 @hbmpChecked: D$ 0
 @hbmpUnchecked: D$ 0
 @dwItemData: D$ 0
 @dwTypeData: D$ 0
 @cch: D$ 0]


AddUserMenu:
    On B$UserMenu0Path = 0, jmp L9>>
    On B$UserMenu0String = 0, jmp L9>>

    call 'USER32.CreatePopupMenu' | mov D$UserPopUpHandle eax
    call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2000 UserMenu0String

    On B$UserMenu1Path = 0, jmp L8>>
    On B$UserMenu1String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2001 UserMenu1String
    On B$UserMenu2Path = 0, jmp L8>>
    On B$UserMenu2String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2002 UserMenu2String
    On B$UserMenu3Path = 0, jmp L8>>
    On B$UserMenu3String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2003 UserMenu3String
    On B$UserMenu4Path = 0, jmp L8>>
    On B$UserMenu4String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2004 UserMenu4String
    On B$UserMenu5Path = 0, jmp L8>>
    On B$UserMenu5String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2005 UserMenu5String
    On B$UserMenu6Path = 0, jmp L8>>
    On B$UserMenu6String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2006 UserMenu6String
    On B$UserMenu7Path = 0, jmp L8>>
    On B$UserMenu7String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2007 UserMenu7String
    On B$UserMenu8Path = 0, jmp L8>>
    On B$UserMenu8String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2008 UserMenu8String
    On B$UserMenu9Path = 0, jmp L8>>
    On B$UserMenu9String = 0, jmp L8>>
        call 'USER32.AppendMenuA' D$UserPopUpHandle &MF_STRING 2009 UserMenu9String

 L8: call 'USER32.InsertMenuA' D$MenuHandle 7 &MF_BYPOSITION__&MF_STRING__&MF_POPUP,
                              D$UserPopUpHandle  UserPopMenu

L9: ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Using a Config File instead of the Registry. Implemented by Eric, October the 19th of 2006

[ConfigBinaryName: B$ 'config.bin' 0]
[ConfigFileHandle: D$ 0 ConfigSizeReadWrite: 0]
[ConfigDataSize: D$ 0 ]
[ConfigFilePath: B$ 0 #&MAX_PATH]

SetMainConfigFilePath:
    push esi
        call 'KERNEL32.GetCurrentDirectoryA' &MAX_PATH, ConfigFilePath

        mov edi ConfigFilePath, eax 0, ecx 0-1
        repne scasb | dec edi

        On B$edi-1 <> '\', mov B$edi '\' | inc edi

        mov esi ConfigBinaryName, ecx 13
        rep movsb
    pop esi
ret


WriteConfigFile:
    push esi, edi

    call 'KERNEL32.CreateFileA' ConfigFilePath, &GENERIC_READ+&GENERIC_WRITE,
                                &FILE_SHARE_READ, &NULL,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_ARCHIVE, &NULL
    mov D$ConfigFileHandle eax

    mov esi RegistryData
L1:
    push esi
        call GetFieldDataSize esi
        mov D$ConfigDataSize eax
        call 'KERNEL32.WriteFile' D$ConfigFileHandle, ConfigDataSize, 4,
                                  ConfigSizeReadWrite, &NULL
        call 'KERNEL32.WriteFile' D$ConfigFileHandle, D$esi+8, D$ConfigDataSize,
                                  ConfigSizeReadWrite, &NULL
    pop esi
    add esi 12 | cmp D$esi 0 | ja L1<

    call 'KERNEL32.CloseHandle' D$ConfigFileHandle

    pop edi, esi
ret


ReadConfigFile:
    push esi

    call SetMainConfigFilePath

    call 'KERNEL32.CreateFileA' ConfigFilePath, &GENERIC_READ+&GENERIC_WRITE,
                                &FILE_SHARE_READ,
                                &NULL &OPEN_EXISTING, &FILE_ATTRIBUTE_ARCHIVE &NULL
    mov D$ConfigFileHandle eax

    mov esi RegistryData
L1:
    push esi
        ; The config is  structured like: [dword: sizeof data][data]
        ; So get the first dword and store it in >ConfigDataSize< :
        call 'KERNEL32.ReadFile' D$ConfigFileHandle, ConfigDataSize, 4,
                                 ConfigSizeReadWrite, &NULL
        ; Read the block of data the sizeof >ConfigDataSize< and copy into
        ; RegistryData:
        call 'KERNEL32.ReadFile' D$ConfigFileHandle, D$esi+8, D$ConfigDataSize,
                                 ConfigSizeReadWrite, &NULL
    pop esi
    add esi 12 | cmp D$esi 0 | ja L1<

    call 'KERNEL32.CloseHandle' D$ConfigFileHandle
    pop esi
ret
____________________________________________________________________________________________

;;
  When the Config.bin file is being used on an USB Memory Stick, if the user moves from
  Computer to computer, the Drive may be different, and force to re-initialize if the
  Drive Char is different.
  
  To avoid this problem, we give a try to substitute, with the actual one.
  
  'RegistryData'
;;

CheckPaths:
    mov al B$ConfigFilePath, bl B$EquatesName

    .If al <> bl
        If D$EquatesName <> 'Equa'
            mov B$EquatesName al
        End_If

        If D$B_U_AsmName <> 'B_U_'
            mov B$B_U_AsmName al
        End_If

        ;If D$CalcName <> 'Calc'
        ;    On B$CalcName <> 0, mov B$CalcName al
        ;End_If

        ;On B$F2Name <> 0, mov B$F2Name al

        ;If D$Win32HlpName <> 'Win3'
        ;    mov B$Win32HlpName al
        ;End_If

    .End_If
ret
____________________________________________________________________________________________


[WineDebugKey: 'Software\Windows NT\CurrentVersion\AeDebug', 0]
[WineDbg: 'winedbg %ld %ld', 0]
[WineDbgString: 'Debugger', 0]

WineKey:
    call 'ADVAPI32.RegCreateKeyExA' &HKEY_LOCAL_MACHINE, WineDebugKey, 0,
                                    0, 0,
                                    &KEY_READ__&KEY_WRITE__&KEY_QUERY_VALUE,
                                    0, hRegKey, Result

    call 'ADVAPI32.RegSetValueExA' D$hRegKey, WineDbgString, 0, &REG_SZ, WineDbg, 15
ret
____________________________________________________________________________________________

; Regedit.exe: HKEY_CURRENT_USER / Software / RosAsm

WhateverConfig:  ; 'Main'
    call SetMainConfigFilePath

    call 'KERNEL32.FindFirstFileA' ConfigFilePath, FindFile
    push eax
        call 'KERNEL32.FindClose' eax
    pop eax

    ..If eax = &INVALID_HANDLE_VALUE
        call OpenRegistry

        .If D$Result = &REG_CREATED_NEW_KEY
         ; Tag Dialog 3
            call 'USER32.DialogBoxParamA' D$hinstance, 3, &NULL,
                                           ConfigProc, &NULL
            If D$UserConfig = 0FF
                call CloseRegistry
                call 'ADVAPI32.RegDeleteKeyA' &HKEY_CURRENT_USER, RosAsmKey
                call 'KERNEL32.ExitProcess', 0

            Else_If D$UserConfig = REGISTRY
              ; "Read" assumes create when none:
                call ReadRegistry
                mov D$UserConfig REGISTRY

            Else
                call CloseRegistry
                call 'ADVAPI32.RegDeleteKeyA' &HKEY_CURRENT_USER, RosAsmKey
                mov D$UserConfig CONFIGFILE
                call AutoInit
                call WriteConfigFile

            End_If

        .Else
            call ReadRegistry
            mov D$UserConfig REGISTRY

        .End_If

    ..Else
        call ReadConfigFile
        call CheckPaths
        mov D$UserConfig CONFIGFILE

    ..End_If
ret

[ConfigMessage: "You can choose to save your Configuration either as a Registry Key, or under the form of a File: config.bin

You should choose the first option for regular work on your home computer.

The second option may be of interrest, for example, when working from an USB memory stick.

If you choose the second option, when doing a Copy of RosAsm to another Folder, you will have to also copy your 'config.bin' aside", 0]

[UserConfig: ?]

[REGISTRY 1, CONFIGFILE 2]
____________________________________________________________________________________________

; Tag Dialog 3

Proc ConfigProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ..If D@Message = &WM_COMMAND
        and D@wParam 0FFFF

        .If D@wParam = &IDCANCEL
            mov D$UserConfig 0FF
            call 'User32.EndDialog' D@Adressee, 0

        .Else_If D@wParam = &IDOK
            call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &BM_GETCHECK, 0, 0
            If eax = &TRUE
                mov D$UserConfig REGISTRY
            Else
                mov D$UserConfig CONFIGFILE
            End_If

            call 'User32.EndDialog' D@Adressee, 0
        .End_If

    ..Else_If D@Message = &WM_INITDIALOG
        move D$ShowApiDialogHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

        call 'User32.SetDlgItemTextA' D@Adressee, 10, ConfigMessage

        call 'USER32.SendDlgItemMessageA' D@Adressee, 20, &BM_SETCHECK, 1, 0

    ..Else_If D@Message = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0, 0
            mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ..Else
        popad | mov eax &FALSE | jmp L9>

    ..End_If

    popad | mov eax &TRUE

L9: EndP
EndP

____________________________________________________________________________________________

Create_Config_bin:
    call 'USER32.MessageBoxA' 0, Config.Bin_Message, Config.Bin_Title, &MB_YESNO

    If eax = &IDYES
        call SetMainConfigFilePath
        call WriteConfigFile
    End_If
ret

[Config.Bin_Message:
"Do you want to create a 'config.bin' file in the current
Directory?

When a 'config.bin' is found aside RosAsm, this file is
used, instead of the Registry.

If you mean, later to recover the Registry functionalities,    
you will just have to delete this file", 0

Config.Bin_Title: 'Configuration File', 0]
____________________________________________________________________________________________















