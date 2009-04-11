TITLE Tools_Exec
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
                               ; run "Calc.exe" and friends...
 _______________________________________________________________________________________
 _______________________________________________________________________________________

 Icons Links are not yet used by RosAsm. I had a look at Win32.hlp for how to use them
 from an app, in order to retrieve the true link. >>> 3 pages of C chiet (!!!!!!).

 So, i will try later to read the link file. It looks like this:


 D:\RosAsm\Calc.exe.lnk  334 bytes

00000000: 4C 00 00 00 01 14 02 00 - 00 00 00 00 C0 00 00 00
00000010: 00 00 00 46 13 00 00 00 - 20 00 00 00 C0 9C 12 13
00000020: 7C D1 BF 01 00 B0 92 B9 - CB D0 BF 01 00 8C FC C4
00000030: F8 CD BC 01 00 82 03 00 - 00 00 00 00 01 00 00 00
00000040: 00 00 00 00 00 00 00 00 - 00 00 00 00 90 00 14 00
00000050: 1F 00 E0 4F D0 20 EA 3A - 69 10 A2 D8 08 00 2B 30
00000060: 30 9D 19 00 23 43 3A 5C - 00 00 00 00 00 00 00 00
00000070: 00 00 00 00 00 00 00 00 - 00 11 EE 25 00 31 00 00
00000080: 00 00 00 6D 28 9A 65 11 - 00 50 72 6F 67 72 61 6D
00000090: 20 46 69 6C 65 73 00 50 - 52 4F 47 52 41 7E 31 00
000000A0: 20 00 31 00 00 00 00 00 - C8 28 4B 98 10 00 42 61
000000B0: 73 65 43 61 6C 63 00 42 - 41 53 45 43 41 4C 43 00
000000C0: 1C 00 32 00 00 82 03 00 - 3E 23 C0 BB 20 80 42 61
000000D0: 73 65 63 61 6C 63 2E 65 - 78 65 00 00 00 00 55 00
000000E0: 00 00 1C 00 00 00 01 00 - 00 00 1C 00 00 00 2D 00
000000F0: 00 00 00 00 00 00 54 00 - 00 00 11 00 00 00 03 00
00000100: 00 00 EE 15 26 16 10 00 - 00 00 00 43 3A 5C 50 72
                                              C  :  \  P  r
00000110: 6F 67 72 61 6D 20 46 69 - 6C 65 73 5C 42 61 73 65
           o  g  r  a  m --  F  i    l  =  s  \  <  a  s  e
00000120: 43 61 6C 63 5C 42 41 53 - 45 43 41 4C 43 2E 45 58
           C  a  l  c  \  <  A  S    =  C  A  L  C  .  =  X
00000130: 45 00 00 19 00 43 3A 5C - 50 72 6F 67 72 61 6D 20
           =              C  :  \    P  r  o  g  r  a  m --
00000140: 46 69 6C 65 73 5C 42 61 - 73 65 43 61 6C 63
           F  i  l  =  s  \  <  a    s  =  C  a  l  c

 So do i think it will be much easier to read this thing down to the end, make

 > std
 >   mov al ':', ecx 0FFFF | repne scasb | repne scasb   ; >>> edi > C:\Pr...
 > cld

 store a copy in place of default, and go on (3 simple lines instead of 3 uncertain pages).

 i'll see if it works like this later because i do not know if the links files are
 the same in all win32 platforms.... (i am affraid they are not...)

 Right now, if you use a calc that need installation at C:\somewhere\...
 just copy the full path and name down here instead of "CalcName" or "CalcLinkName".
 the "CalcLinkName" down here is the one i use on my computer... do it your own and
 recompile "CopyOfRosAsm.exe".
;;


[CalcMessage: "

    Copy your prefered Calc in this directory

    and rename it 'Calc.exe'

    or have a search for 'Calc.exe' in RosAsm source    
    and read the header comment...

     "  0

 FileNotFound: 'Requested file not found', 0]


[ProcessInfos: ? #4]

[CalcHandle: ?  CalcExitCode: ?  CalcWindow: ?]

;;
 Multi-instances Calc are a problem: I would have prefered to put the calc foreground
 in case user clic on RosAsm window without closing the calc. I have never been able to
 get the handle of it to transmit to 'SetForeground'. So i leave it like this: Each
 time [Calc] menu item is clicked, we first kill the calc (even if already dead). It
 works supringly well, but i do not understand why. It seams that Win doesn't reuse
 the same "handles"... help wanted!!!
;;

Calc:
    If B$OnCalc = &TRUE
      call 'KERNEL32.TerminateProcess' D$ProcessInfos D$CalcExitCode
    End_If

    call 'KERNEL32.CreateProcessA'  CalcName  0 0 0  0  0 0 0  STARTUPINFO  ProcessInfos

    On eax = 0,
      call 'KERNEL32.CreateProcessA'  CalcLinkName  0 0 0  0  0 0 0  STARTUPINFO  ProcessInfos

    If eax = 0
      call 'User32.MessageBoxA' D$hwnd, CalcMessage, FileNotFound,
                                &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    Else
      call 'KERNEL32.GetExitCodeProcess' D$ProcessInfos CalcExitCode
      mov B$OnCalc &TRUE
    End_If
ret
________________________________________________________________________________________
________________________________________________________________________________________

[open: 'open', 0]

; Same but for OpCodes.hlp / Win32.hlp
;;
[OpCodesMessage: " Copy 'x86eas.hlp' in this directory
or run [Config] menu option" 0]

Opcodes_Hlp:
    call 'Shell32.ShellExecuteA' D$hwnd open  OpcodeHlpName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      call 'User32.MessageBoxA' 0 OpCodesMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[WinHlpMessage: "Copy 'Win32.hlp' in this directory
or run [Config] menu option", 0]

Win32_Hlp:
    call Help Win32HlpName, NoParameter, WinHlpMessage
ret

;;
[Asm32HlpMessage: "Copy 'Asm32Tut.exe' in this directory
or run [Config] menu option", 0]

Asm32_Hlp:
    call 'Shell32.ShellExecuteA' D$hwnd open Asm32TutName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      call 'User32.MessageBoxA' 0 Asm32HlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[MmediaHlpMessage: "Copy 'Mmedia.hlp' in this directory
or run [Config] menu option", 0]

Mmedia_Hlp:
    call Help MmediaHlpName, NoParameter, MmediaHlpMessage
ret


[OpenGlHlpMessage: "Copy 'OpenGl.hlp' in this directory
or run [Config] menu option", 0]

OpenGl_Hlp:
    call Help OpenGlHlpName, NoParameter, OpenGlHlpMessage
ret


[WinSockHlpMessage: "Copy 'WinSock.hlp' in this directory
or run [Config] menu option", 0]

WinSock_Hlp:
    call Help WinSockHlpName, NoParameter, WinSockHlpMessage
ret


[SDLRefHlpMessage: "Copy 'SDL.chm' in this directory
or run [Config] menu option", 0]

SDL_Hlp:
    call Help SDLRefName, NoParameter, SDLRefHlpMessage
ret


[sqliteHlpMessage: "Copy 'sqlite.hlp' in this directory
or run [Config] menu option", 0]

sqlite_Hlp:
    call Help sqliteName, NoParameter, sqliteHlpMessage
ret


[DevILHlpMessage: "Copy 'DevIL.html' in this directory
or run [Config] menu option", 0]

DevIL_Hlp:
    call Help DevILName, NoParameter, DxHlpMessage
ret


[DxHlpMessage: "Copy 'DirectX.hlp' in this directory
or run [Config] menu option", 0]

Dx_Hlp:
    call Help DxHlpName, NoParameter, DxHlpMessage
ret

;;
[WinDataFileMessage: "Run [Config] menu option and define a File    ", 0]

WinDataFile:
    call 'Shell32.ShellExecuteA' D$hwnd open WinDataFileName NoParameter &NULL &SW_SHOWDEFAULT
    If eax <= 32
      call 'User32.MessageBoxA' 0 WinDataFileMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    End_If
ret
;;

[RosAsmHlpMessage: "
Copy 'B_U_Asm.exe' in this directory or run:

[Configuration] menu option.


If this is for a first run, select:

[Configuration][Companion Files] and provide the Path to:

* [B_U_Asm.exe]

Then provide the Path to:        

* [Equates Path]

After this, RosAsm will shut down, and will be ready
for a first Run.

", 0]

RosAsmHelp:
    call Help B_U_AsmName, B_U_AsmTop, RosAsmHlpMessage
ret


[F2HlpMessage: "The File you defined in the [Configuration][Other Files]
to be activated by [F2] is not available", 0]

F2Help:
    If D$F2Name <> 0
        call Help F2Name, NoParameter, F2HlpMessage
    End_If
ret


[RosAsmLicenceHelp: 'RosAsm_License', 0]

LicenseView:
    call Help, B_U_AsmName, RosAsmLicenceHelp, ContextHlpMessage

    call 'USER32.MessageBoxA' D$hwnd,
        {"
        RosAsm License viewed       
        
                   Accept?", 0},
        {'License', 0}, &MB_SYSTEMMODAL__&MB_YESNO

    On eax = &IDNO, call 'KERNEL32.ExitProcess' 0
ret


[GplHelp: 'GPL', 0]

GPLView:
    call Help, B_U_AsmName, GplHelp, ContextHlpMessage
ret






[NoParameter: B$ 0 ;
 B_U_AsmTop: 'Top', 0
 StringsHelp: 'Strings_Editor', 0
 IconHelp: 'Icon_Editor', 0
 MenuHelp: 'Menus_Editor', 0
 DialogHelp: 'Dialog_Editor', 0
 TempateHelp: 'Reusing_Code' 0
 ConfigHelp:  'Configuration_Tab', 0
 DisassemblerHelp: 'Disassembling', 0
 IncludeFilesHelp: 'Include_Files', 0
 SourceEditor: 'Source_Editor', 0
 StructuresHelp: 'Structures', 0
 HelPFiles: 'HelpFiles', 0
 DisMap: 'Disassembler_Flags', 0
 UnusedSymbolsHelp: 'Unused_symbols_Scanner', 0]

[ContextHlpMessage: 'Run [Configuration]/|Files Locations]/[RosAsm]     ' 0]

;[ContextHelp | call 'Shell32.ShellExecuteA' D$hwnd open, B_U_AsmName, #1, &NULL, &SW_SHOWNORMAL
; cmp eax 32 | ja M0>
;    call 'User32.MessageBoxA' D$hwnd ContextHlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
; M0: ]


Proc ContextHelp:
    Argument @Doc, @ErrorMessage

        call 'Shell32.ShellExecuteA' D$hwnd, open, B_U_AsmName, D@Doc, &NULL,
                                     &SW_SHOWNORMAL

        If eax <= 32
            call 'User32.MessageBoxA' D$hwnd, D@ErrorMessage, FileNotFound,
                                      &MB_ICONINFORMATION+&MB_SYSTEMMODAL
        End_If
EndP


Proc Help:
    Arguments @HelpFile, @Page, @ErrorMessage

        call 'Shell32.ShellExecuteA' D$hwnd, open, D@HelpFile, D@Page, &NULL,
                                     &SW_SHOWNORMAL
        If eax <= 32
            push eax
                call 'User32.MessageBoxA' D$hwnd, D@ErrorMessage, FileNotFound,
                                          &MB_ICONINFORMATION+&MB_SYSTEMMODAL
            pop eax
        End_If
EndP


; Menu is build at 'AddUserMenu'

UserDefinedAction:
    sub eax 2000  ; eax = 0 to 8
    mov ecx (64*4) | mul ecx
    add eax UserMenu0Path

    call 'Shell32.ShellExecuteA' D$hwnd, open, eax, NoParameter, &NULL, &SW_SHOWDEFAULT
ret

______________________________________________________________________________________

; Show a Box with Ascii codes in decimal and Hexa:

[AsciiData: B$ ? #2860]

[AsciiDialog: D$ 090CC08C2 0   ; Style
 U$ 01 0 0 01E6 0109           ; Dim
 0                             ; Menu
 0                             ; Class(not yet)
 "Ascii Table" 0                ; Title
 08 'Helv' 0]                  ; Font

[AC0: D$ 0508008C4 0           ; Style
 U$ 1 1 01E5 0108              ; Dim
 0100                          ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data


; Build a text table to send to Edit Control:

AsciiTable:
    If D$AsciiDialogHandle > 0
        Beep | ret
    End_If

    push ebp
        mov edi AsciiData, ebx 0, ecx 10, ebp 0
        mov al tab | stosb
        .While ebp < 256
            mov al bl | On al < 32, mov al 2 | stosb
            mov al tab | stosb
            mov eax ebx | add edi 2
            mov edx 0 | div ecx
            add dl '0' | mov B$edi dl
            dec edi
            mov edx 0 | div ecx
            add dl '0' | mov B$edi dl
            dec edi
            mov edx 0 | div ecx
            add dl '0' | mov B$edi dl
            add edi 3 | mov al tab | stosb

            mov al '0' | stosb
            mov eax ebx, edx eax | shr eax 4
            and eax 0F | add al '0' | On al > '9', add al 7
            stosb
            mov eax edx
            and eax 0F | add al '0' | On al > '9', add al 7
            stosb

            add ebx 32
            cmp ebx 255 | jna L2>
            sub ebx 255
            mov al CR | stosb | mov al LF | stosb
            mov al tab | stosb | jmp L3>
L2:         mov al tab | stosb | stosb
L3:         inc ebp
        .End_While

        mov eax 0 | stosd
       ; Show resulting box:
        call 'User32.DialogBoxIndirectParamA' D$hinstance, AsciiDialog, 0, AsciiProc, 0
    pop ebp
ret


[TabDimPtr: 10] [AsciiDialogHandle: ?]

Proc AsciiProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If D@Message = &WM_COMMAND
        If D@wParam = &IDCANCEL
            mov D$AsciiDialogHandle 0
            call 'User32.EndDialog' D@Adressee 0
        Else
            call 'User32.GetDlgItem' D@Adressee 0100
            call 'User32.SendMessageA' eax &EM_SETSEL  0-1 0
        End_If

    .Else_If D@Message = &WM_INITDIALOG
        move D$AsciiDialogHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'User32.GetDlgItem' D@Adressee 0100
        call 'User32.SendMessageA' eax &EM_SETTABSTOPS 1 TabDimPtr
        call 'User32.SetDlgItemTextA' D@Adressee 0100 AsciiData

    .Else
       popad | mov eax &FALSE | jmp L9>

    .End_If

    popad | mov eax &TRUE

L9: EndP

___________________________________________________________________________________________
___________________________________________________________________________________________

;             Printer job (here, with fixed pitch fonts for sources alignment)
___________________________________________________________________________________________
___________________________________________________________________________________________

; This Structure is for call 'Comdlg32.PrintDlgA'. We really use only 'PD_Flags' to tell
; if we want/propose a user selected block to print or not:

[PRINTDLG:
 PD_lStructSize: len             PD_hWndOwner: 0       PD_hDevMode: 0
 PD_hDevNames: 0                 PD_hDC: 0
 PD_Flags: &PD_RETURNDC+&PD_SELECTION
 PD_nFromPage: W$ 0              PD_nToPage: 0         PD_nMinPage: 0
 PD_nMaxPage: 0                  PD_nCopies: 1
 PD_hInstance: D$ 0              PD_lCustData: 0
 PD_lpfnPrintHook: 0             PD_lpfnSetupHook: 0   PD_lpPrintTemplateName: 0
 PD_lpPrintSetupTemplateName: 0  PD_hPrintTemplate: 0  PD_hSetupTemplate: 0]

; Needed for 'GDI32.StartDocA' (no interrest for us):

[DocName: ' - Poor text Printer -'  0]

[DOCINFO: DI_cbSize: len  DI_lpszDocName: DocName  DI_lpszOutput: 0  DI_lpszDatatype: 0
           DI_fwType: 0]

; This is for call 'Comdlg32.ChooseFontA'. Important:

;[CF_PRINTERFONTS 2  CF_FIXEDPITCHONLY 04000  CF_INITTOLOGFONTSTRUCT 040]


; Default font for choose-font-box (Courier new 14).
; LOGFONTA Structure (Pointed by record 4 in CHOOSEFONT):

[CbBuffer:
 lfHeight: D$ 0FFFF_FFED    ; (-19) (for details, see "LOGFONT" in win32.hlp)
 lfWidth: D$ 0
 lfEscapement: D$ 0
 lfOrientation: D$ 0
 lfWeight: D$ 0190
 lfItalic: B$ 0
 lfUnderline: B$ 0
 lfStrikeOut: B$ 0
 lfCharSet: B$ 0
 lfOutPrecision: B$ 03
 lfClipPrecision: B$ 02
 lfQuality: B$ 01
 lfPitchAndFamily: B$ 031
 lfFaceName: B$ 'Courier New' 0] [CBBsecurityTail: 0 #5] ; 32 Bytes > Total lenght = 60 Bytes

[PrintTextMetric: 0 #20]

[CHOOSEFONT: CF_lStructSize: Len     CF_hWndOwner: 0      CF_hDC: 0
             CF_lpLogFont: cbbuffer  CF_iPointSize: 0
             CF_Flags: &CF_PRINTERFONTS+&CF_FIXEDPITCHONLY+&CF_INITTOLOGFONTSTRUCT
             CF_rgbColors: 0         CF_lCustData: 0      CF_lpfnHook: 0
             CF_lpTemplateName: 0    CF_hInstance: 0      CF_lpszStyle: 0
             CF_nFontType: W$ 0      CF_Alignment:  0     CF_nSizeMin: D$ 3
             CF_nSizeMax: 0ff]

[UserChoosenPrinterFont: ?]

; Our pointers:
[PrintStartPtr: ?  PrintEndPtr: ?   CharHight: ?  PageHight: ?  PrinterDCavailable: ?]

Print:
    On D$SourceLen = 0, ret

    If B$PrinterDCavailable = &TRUE
        call 'GDI32.DeleteDC' D$PD_hDC
        call 'GDI32.DeleteObject' D$UserChoosenPrinterFont
    End_If

    If B$BlockInside = &TRUE
        mov D$PD_Flags &PD_RETURNDC+&PD_SELECTION
    Else
        mov D$PD_Flags &PD_RETURNDC+&PD_NOSELECTION
    End_If

    call 'Comdlg32.PrintDlgA' PRINTDLG | On eax = 0, ret    ; The Printer dialog sets
    move D$CF_hDC D$PD_hDC,  D$CF_hWndOwner D$hwnd          ; the DC handle in PRINTDLG struc
    If D$PD_Flags = &PD_RETURNDC+&PD_SELECTION
        move D$PrintStartPtr D$BlockStartTextPtr, D$PrintEndPtr D$BlockEndTextPtr
    Else
        mov eax D$UpperLine | mov D$PrintStartPtr eax
        add eax D$SourceLen | mov D$PrintEndPtr eax
    End_If

    call 'Comdlg32.ChooseFontA' CHOOSEFONT | On eax = 0, ret    ; user sets the font:
    call 'GDI32.CreateFontIndirectA' cbbuffer
        mov D$UserChoosenPrinterFont eax

    call UpdateRegistry                                 ; To save last used Printer Font.

StartControlP:
    call 'GDI32.GetDeviceCaps' D$PD_hDC &VERTRES | mov D$PageHight eax   ; we set the sizes:
    call 'GDI32.GetTextMetricsA' D$PD_hDC PrintTextMetric
        mov eax D$PrintTextMetric, D$CharHight eax
        shl eax 4 | sub D$PageHight eax                 ; sub 16 lines for top/bottom blanks

    call 'GDI32.StartDocA' D$PD_hDC  DOCINFO | On eax le 0, jmp L7>>; we print:
        call 'GDI32.StartPage' D$PD_hDC | On eax le 0, jmp L7>>
            call 'GDI32.SelectObject' D$PD_hDC D$UserChoosenPrinterFont

          mov esi D$PrintStartPtr, edx 0
L0:       mov ebx esi, ecx 0
L1:       cmp B$esi ' ' | jb L2>
            lodsb | inc ecx | cmp esi D$PrintEndPtr | jae L3>
              jmp L1<
L2:       add esi 2                                                      ; strip CR/LR
L3:       push esi, edx
            On ecx > 0
              call 'USER32.TabbedTextOutA'  D$PD_hDC 0 edx ebx ecx 0,0,0
            End_If
          pop edx esi
          add edx D$CharHight
          If edx >= D$PageHight                                          ; Next page?:
            pushad
              call 'GDI32.EndPage' D$PD_hDC | On eax le 0, jmp L7>>
                call 'GDI32.StartPage' D$PD_hDC | On eax le 0, jmp L7>>
                  call 'GDI32.SelectObject' D$PD_hDC D$UserChoosenPrinterFont
            popad
            mov edx 0
          End_If
          On esi < D$PrintEndPtr, jmp L0<<

        call 'GDI32.EndPage' D$PD_hDC | On eax le 0, jmp L7>>
      call 'GDI32.EndDoc' D$PD_hDC
    mov B$PrinterDCavailable &TRUE | ret

L7: call 'GDI32.AbortDoc' D$PD_hDC
    call 'GDI32.DeleteObject' D$UserChoosenPrinterFont
    call 'GDI32.DeleteDC' D$PD_hDC

    mov B$PrinterDCavailable &FALSE
ret


ControlP:
    If B$BlockInside = &FALSE
        ret
    Else_If B$PrinterDCavailable = &FALSE
        jmp Print
    Else
        move D$PrintStartPtr D$BlockStartTextPtr,  D$PrintEndPtr D$BlockEndTextPtr
        jmp StartControlP
    End_If

____________________________________________________________________________________________
____________________________________________________________________________________________

; Tool for viewing the System Resources and retrieving the IDs.

ViewSysResources:
    If D$ViewSysResourcesHandle = 0
        call 'USER32.DialogBoxParamA' D$hinstance 29000 &NULL ViewSysResourcesProc &NULL
    Else
        Beep
    End_If
ret


[ViewSysResourcesHandle: ?    SysBitMapsHandle: ?    SysIconsHandle: ?    SysCursorsHandle: ?]

Proc ViewSysResourcesProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        move D$ViewSysResourcesHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.GetDlgItem' D@Adressee 10 | mov D$SysBitMapsHandle eax
        call 'USER32.GetDlgItem' D@Adressee 11 | mov D$SysIconsHandle eax
        call 'USER32.GetDlgItem' D@Adressee 12 | mov D$SysCursorsHandle eax
        call InitViewSysResourcesListBoxes
        mov D$SysButtonText 0 | jmp L8>>

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        ..If eax = &LBN_SELCHANGE
            call 'USER32.SendDlgItemMessageA' D@Adressee D@wParam &LB_GETCURSEL 0 0

            add D@wParam 10     ; Listy Box ID + 10 >>> Static Control ID

            If D@wParam = 20
                SetOKButtonSysText SysBitMapsStrings SysBitMapsIDs
                lea ebx D$SysBitMapsIDs+eax*4
                call 'USER32.LoadBitmapA' &NULL D$ebx
                mov ecx &IMAGE_BITMAP
            Else_If D@wParam = 21
                SetOKButtonSysText SysIconsStrings SysIconsIDs
                lea ebx D$SysIconsIDs+eax*4
                call 'USER32.LoadIconA' &NULL D$ebx
                mov ecx &IMAGE_ICON
            Else_If D@wParam = 22
                SetOKButtonSysText SysCursorsStrings SysCursorsIDs
                lea ebx D$SysCursorsIDs+eax*4
                call 'USER32.LoadCursorA' &NULL D$ebx
                mov ecx &IMAGE_CURSOR
            End_If

            call 'USER32.SendDlgItemMessageA' D@Adressee D@wParam &STM_SETIMAGE ecx eax
            call 'USER32.SendDlgItemMessageA' D@Adressee 1 &WM_SETTEXT 0 SysButtonText

        ..Else_If D@wParam = &IDCANCEL
            jmp L1>

        ..Else_If D@wParam = &IDOK
            If B$SysButtonText <> 0
                mov esi SysButtonText, edi SysButtonText
                While B$esi > 0
                    movsb
                    On W$esi = '&&', inc esi
                End_While
                mov B$edi 0

                mov eax SysButtonText, D$BlockStartTextPtr eax
                While B$eax > 0 | inc eax | End_While
                dec eax | mov D$BlockEndTextPtr eax, B$BlockInside &TRUE
                call ControlC | mov B$BlockInside &FALSE
            End_If

L1:         mov D$ViewSysResourcesHandle 0
            call 'USER32.EndDialog' D@Adressee 0

        ..End_If

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


[SetOKButtonSysText | mov edi #1 | mov edx #2 | call OKButtonSysText]

; Example: edi > SysBitMapsStrings // eax = zero Based Indice

[SysButtonText: ? #12]
[SysBitMapApiCall: "call 'USER32.LoadBitmapA' &&NULL &&" 0
 SysIconApiCall: "call 'USER32.LoadIconA' &&NULL &&" 0
 SysCursorApiCall: "call 'USER32.LoadCursorA' &&NULL &&" 0]

OKButtonSysText:
    push eax, ebx
        .While eax > 0
            While B$edi <> 0 | inc edi | End_While
            inc edi | dec eax
        .End_While

        push edi
            mov edi SysButtonText
            If edx = SysBitMapsIDs
                mov esi SysBitMapApiCall
            Else_If edx = SysIconsIDs
                mov esi SysIconApiCall
            Else
                mov esi SysCursorApiCall
            End_If
            While B$esi <> 0 | movsb | End_While
        pop esi

      ; Copy the Equate in the Button Buffer:
        inc esi
        While B$esi <> 0 | movsb | End_While
        mov D$edi ' ; =', B$edi+4 ' ' | add edi 5

    pop ebx, eax
    push eax, ebx
        mov eax D$edx+eax*4 | call WriteEax | mov B$edi 0
    pop ebx, eax
ret


[SysStringPointer: ?]

[SysBitMapsStrings: B$
'&OBM_CLOSE' 0       '&OBM_UPARROW' 0     '&OBM_DNARROW' 0     '&OBM_RGARROW' 0
'&OBM_LFARROW' 0     '&OBM_REDUCE' 0      '&OBM_ZOOM' 0        '&OBM_RESTORE' 0
'&OBM_REDUCED' 0     '&OBM_ZOOMD' 0       '&OBM_RESTORED' 0    '&OBM_UPARROWD' 0
'&OBM_DNARROWD' 0    '&OBM_RGARROWD' 0    '&OBM_LFARROWD' 0    '&OBM_MNARROW' 0
'&OBM_COMBO' 0       '&OBM_UPARROWI' 0    '&OBM_DNARROWI' 0    '&OBM_RGARROWI' 0
'&OBM_LFARROWI' 0    '&OBM_SIZE' 0        '&OBM_BTSIZE' 0      '&OBM_CHECK' 0
'&OBM_CHECKBOXES' 0  '&OBM_BTNCORNERS' 0 0]

[SysBitMapsIDs:
 &OBM_CLOSE       &OBM_UPARROW     &OBM_DNARROW     &OBM_RGARROW
 &OBM_LFARROW     &OBM_REDUCE      &OBM_ZOOM        &OBM_RESTORE
 &OBM_REDUCED     &OBM_ZOOMD       &OBM_RESTORED    &OBM_UPARROWD
 &OBM_DNARROWD    &OBM_RGARROWD    &OBM_LFARROWD    &OBM_MNARROW
 &OBM_COMBO       &OBM_UPARROWI    &OBM_DNARROWI    &OBM_RGARROWI
 &OBM_LFARROWI    &OBM_SIZE        &OBM_BTSIZE      &OBM_CHECK
 &OBM_CHECKBOXES  &OBM_BTNCORNERS]

[SysIconsStrings: B$
'&IDI_APPLICATION' 0     '&IDI_HAND' 0    '&IDI_QUESTION' 0    '&IDI_EXCLAMATION' 0
'&IDI_ASTERISK' 0        '&IDI_WINLOGO' 0 0]

[SysIconsIDs: &IDI_APPLICATION  &IDI_HAND  &IDI_QUESTION  &IDI_EXCLAMATION
              &IDI_ASTERISK     &IDI_WINLOGO]

[SysCursorsStrings: B$
'&IDC_ARROW' 0      '&IDC_IBEAM' 0      '&IDC_WAIT' 0       '&IDC_CROSS' 0
'&IDC_UPARROW' 0    '&IDC_SIZENWS' 0    '&IDC_SIZENESW' 0   '&IDC_SIZEWE' 0
'&IDC_SIZENS' 0     '&IDC_SIZEALL' 0    '&IDC_NO' 0         '&IDC_APPSTARTING' 0
'&IDC_HELP' 0 0]

[SysCursorsIDs:
 &IDC_ARROW     &IDC_IBEAM         &IDC_WAIT       &IDC_CROSS     &IDC_UPARROW
 &IDC_SIZENWSE  &IDC_SIZENESW      &IDC_SIZEWE     &IDC_SIZENS    &IDC_SIZEALL
 &IDC_NO        &IDC_APPSTARTING   &IDC_HELP]

InitViewSysResourcesListBoxes:
    mov edi SysBitMapsStrings
    While B$edi <> 0
        push edi
        call 'USER32.SendMessageA' D$SysBitMapsHandle &LB_ADDSTRING 0 edi
        pop edi
        mov al 0, ecx 0FF | repne scasb
    End_While

    mov edi SysIconsStrings
    While B$edi <> 0
        push edi
        call 'USER32.SendMessageA' D$SysIconsHandle &LB_ADDSTRING 0 edi
        pop edi
        mov al 0, ecx 0FF | repne scasb
    End_While

    mov edi SysCursorsStrings
    While B$edi <> 0
        push edi
        call 'USER32.SendMessageA' D$SysCursorsHandle &LB_ADDSTRING 0 edi
        pop edi
        mov al 0, ecx 0FF | repne scasb
    End_While
ret

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

 Used DLL in the actual Source.
;;


ShowSourceImports:
    If D$ImportDialogHandle = 0
        call 'USER32.DialogBoxParamA' D$hinstance, 1100, &NULL,
                                      ViewSourceImportsProc, &NULL
    End_If
ret


ShowSourceExports:
    If D$ExportDialogHandle = 0
        call 'USER32.DialogBoxParamA' D$hinstance, 1101, &NULL,
                                      ViewSourceExportsProc, &NULL
    End_If
ret


[DLLsProcListHandle: ?    DLLsProcFunctionsListHandle: ?]

Proc Enable:
    Arguments @ParentHandle, @ID

        call 'User32.GetDlgItem' D@ParentHandle, D@ID
        call 'User32.EnableWindow' eax &TRUE
EndP

Proc Disable:
    Arguments @ParentHandle, @ID

        call 'User32.GetDlgItem' D@ParentHandle, D@ID
        call 'User32.EnableWindow' eax &FALSE
EndP


[ImportDialogHandle: ?]

; Tag Dialog 1100

Proc ViewSourceImportsProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            mov D$ImportDialogHandle 0
            call 'User32.EndDialog' D@Adressee, 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_SELCHANGE
                    call RestoreRealSource
                    call ViewDllFunctionList
                    call SetPartialEditionFromPos
                End_If
                call 'USER32.SetDlgItemInt' D@Adressee, 12, 0, &FALSE
                call Disable D@adressee, 3
                call Disable D@adressee, 4

            .Else_If W@wParam = 11
                If W@wParam+2 = &LBN_SELCHANGE
                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                               &LB_GETCURSEL, 0, 0

                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                               &LB_GETITEMDATA, eax, 0

                    call 'USER32.SetDlgItemInt' D@Adressee, 12, eax, &FALSE
                    call 'USER32.SendDlgItemMessageA' D@Adressee, 3, &WM_ENABLE,
                                                      &TRUE, 0
                    call Enable D@adressee, 3
                    call Disable D@adressee, 4

                End_If

            .Else_If W@wParam = 3
                call ImportFunctionFindFirst

                call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
                call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETITEMDATA,
                                           eax, 0
                On eax > 1, call Enable D@adressee, 4

            .Else_If W@wParam = 4
                call ImportFunctionFindNext

            .End_If
        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$ImportDialogHandle D@Adressee

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

        call 'USER32.GetDlgItem' D@Adressee, 10 | mov D$DLLsProcListHandle eax
        call 'USER32.GetDlgItem' D@Adressee, 11 | mov D$DLLsProcFunctionsListHandle eax

        call Disable D@adressee, 3
        call Disable D@adressee, 4

        call RestoreRealSource
        call InitImportsProcList
        call SetPartialEditionFromPos

        If B$DLLsFoundInSource = &FALSE
            call 'USER32.MessageBoxA' D$hwnd, {'No Import Function found in this Source', 0},
                                     {'Failure:', 0}, 0
            call 'USER32.EndDialog' D@Adressee, 0
        End_If

        popad | mov eax &FALSE | ExitP

    ...Else
        popad | mov eax &FALSE | ExitP

    ...End_If

L8: popad | mov eax &TRUE
EndP


ExportFunctionFindFirst:
    call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETCURSEL, 0, 0
    call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETTEXT, eax, SearchString

    ON eax = &LB_ERR, ret
    mov cl B$SearchString+eax-1 | cmp cl '"' | je L1> | cmp cl "'" | je L1>
    mov W$SearchString+eax '::' | add eax 2 | jmp L1>

ImportFunctionFindFirst:
    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETTEXT, eax, SearchString

    .If eax <> &LB_ERR
        mov D$SearchString+eax "'" | inc eax
L1:     mov D$LenOfSearchedString eax

        call RestoreRealSource

        push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            mov B$DownSearch &TRUE, B$CaseSearch &TRUE, B$WholeWordSearch &FALSE
            move D$CurrentWritingPos D$CodeSource
            call SetCaret D$CodeSource | move D$UpperLine D$CodeSource
            call AskForRedrawNow

                mov D$NextSearchPos 0

                call StringSearch

        pop D$WholeWordSearch, D$CaseSearch, D$DownSearch

        call SetPartialEditionFromPos

    .End_If
ret


ImportFunctionFindNext:
ExportFunctionFindNext:
    call RestoreRealSource
        push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            call StringSearch
        pop D$WholeWordSearch, D$CaseSearch, D$DownSearch
    call SetPartialEditionFromPos
ret


[DLLsFoundInSource: ?]

InitImportsProcList:
    mov esi D$CodeSource, edx D$SourceEnd, B$DLLsFoundInSource &FALSE

    .While esi < edx
        mov eax D$esi
        ..If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ..Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ..Else
            or eax 020202020
            If eax = 'call'
                add esi 4
            Else_If eax = 'jmp '
                add esi 3
            Else
                jmp L8>>
            End_If

            While B$esi = ' ' | inc esi | End_While
            mov al B$esi
            .If al = '"'
                jmp L1>
            .Else_If al = "'"
L1:             inc esi | mov ebx esi
                While B$esi <> al
                    On B$esi <= ' ', jmp L2>
                    If B$esi = '.'
                        mov B$DLLsFoundInSource &TRUE
                        push eax, edx, D$esi, esi
                            mov B$esi 0
                            push ebx
                             call Lb_FindString D$DLLsProcListHandle, ebx
         ; call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_FINDSTRING, 0, ebx
                            pop ebx
                            On eax = &LB_ERR,
                            call 'USER32.SendMessageA' D$DLLsProcListHandle,
                                                       &LB_ADDSTRING, 0, ebx
                        pop esi, D$esi, edx, eax
                        jmp L2>
                    End_If

                    inc esi
                End_While

L2:             While B$esi <> al
                    inc esi | On esi >= edx, jmp L9>
                End_While

            .End_If

        ..End_If

L8:     inc esi

    .End_While

L9: ret


InitExportsProcList:
    mov esi D$CodeSource, edx D$SourceEnd, B$DLLsFoundInSource &FALSE

    .While esi < edx
        mov eax D$esi
        ..If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ..Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ..Else_If ax = '::'
                        mov B$DLLsFoundInSource &TRUE
                        push edx, esi
                            mov edi esi
                            mov al B$esi+2 | cmp al '"' | je L0> | cmp al "'" | jne l2>
L0:                         lea edi D$esi+3 | mov ecx 0FF | repne scasb
L2:                         push D$edi | mov B$edi 0
                            While B$esi-1 = ' ' | dec esi | End_While
L0:                         cmp B$esi-1 ' ' | jbe L1>
                            cmp B$esi-1 '[' | je L1>
                            cmp B$esi-1 ']' | je L1>
                            cmp W$esi-2 '[<' | je L1>
                            cmp B$esi-1 '|' | je L1>
                            dec esi | jmp L0<
L1:
                            call 'USER32.SendMessageA' D$DLLsProcListHandle,
                                                       &LB_ADDSTRING, 0, esi
                            pop D$edi
                        pop esi, edx
        ..End_If

        inc esi

    .End_While
L9: ret


[CurrentSelectionInDLLsList: ?]

ViewDllFunctionList:
    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_RESETCONTENT, 0, 0

    call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETCURSEL, 0, 0
    mov D$CurrentSelectionInDLLsList eax

    mov esi D$CodeSource, edx D$SourceEnd

    ..While esi < edx
        mov eax D$esi
        On al = 'C', or eax 020202020 | On al = 'c', or eax 020202020
        On al = 'J', or eax 020202020 | On al = 'j', or eax 020202020
        ...If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ...Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ...Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ...Else_If eax = 'jmp '
            add esi 3 | jmp L1>

        ...Else_If eax = 'call'
            add esi 4
L1:         While B$esi = ' ' | inc esi | End_While
            mov al B$esi
            ..If al = "'"
L1:             inc esi | mov ebx esi
                .While B$esi <> al
                    On B$esi <= ' ', jmp L2>>
                    .If B$esi = '.'
                        push eax, ebx, edx, D$esi, esi
                            mov B$esi 0
                                call Lb_FindString D$DLLsProcListHandle, ebx
                            mov ecx eax
                        pop esi, D$esi, edx, ebx, eax

                        On ecx <> D$CurrentSelectionInDLLsList, jmp L2>>

                            inc esi | mov ebx esi

                            While B$esi <> al
                                inc esi | On esi >= edx, jmp L9>>
                            End_While
                            push D$esi, esi, edx
                                mov B$esi 0
                                push ebx
                                    call Lb_FindString D$DLLsProcFunctionsListHandle, ebx
                                pop ebx

                                If eax = &LB_ERR
                                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_ADDSTRING, 0, ebx
                                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_SETITEMDATA, eax, 1
                                Else
                                    push eax
                                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_GETITEMDATA, eax, 0
                                    inc eax
                                    pop edx
                                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle,
                                                               &LB_SETITEMDATA, edx, eax
                                End_If

                            pop edx, esi, D$esi
                            jmp L3>
                    .End_If

                    inc esi
                .End_While

L2:             While B$esi <> al
                    inc esi | On esi >= edx, jmp L9>
                End_While

            ..Else_If al = '"'
                jmp L1<<

            ..End_If

        ...End_If

L3:     inc esi

    ..End_While

L9: ret


Proc Lb_FindString:
    Argument @Lb_Handle, @Pointer
    Local @FirstFound
    Uses esi, edi, edx

        mov edx 0-1, D@FirstFound edx

L0:     push edx
            call 'USER32.SendMessageA' D@Lb_Handle, &LB_FINDSTRING, edx, D@Pointer
        pop edx

        .If eax <> &LB_ERR
            push eax
                call 'USER32.SendMessageA' D@Lb_Handle, &LB_GETTEXT, eax, SearchString
            pop edx

            mov esi D@Pointer, edi SearchString | lodsb
            and al UPPERCASEMASK | and B$edi UPPERCASEMASK

            While al = B$edi
                If al = 0
                    mov eax edx | ExitP
                End_If

                lodsb | inc edi
                and al UPPERCASEMASK | and B$edi UPPERCASEMASK
            End_While

            If D@FirstFound = 0-1
                mov D@FirstFound edx | inc edx | jmp L0<
            Else_If edx = D@FirstFound
                mov eax &LB_ERR
            Else
                inc edx | jmp L0<
            EndIf

        .End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[ExportsFoundInSource: ?  ExportDialogHandle: ?]

; Tag Dialog 1101

Proc ViewSourceExportsProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        mov eax D@wParam | and eax 0FFFF

        ..If eax = &IDCANCEL
            mov D$ExportDialogHandle 0
            call 'User32.EndDialog' D@Adressee, 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_SELCHANGE
                    call ExportFunctionFindFirst
                End_If

            .End_If

        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$ExportDialogHandle D@Adressee

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

        call 'USER32.GetDlgItem' D@Adressee, 10 | mov D$DLLsProcListHandle eax

        call RestoreRealSource
        call InitExportsProcList
        call SetPartialEditionFromPos

        If B$DLLsFoundInSource = &FALSE
            call 'USER32.MessageBoxA' D$hwnd, {'No Export Function found in this Source', 0},
                                     {'Failure:', 0}, 0
            call 'USER32.EndDialog' D@Adressee, 0
        End_If

        popad | mov eax &FALSE | ExitP

    ...Else
        popad | mov eax &FALSE | ExitP

    ...End_If

L8: popad | mov eax &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; DLLs Scan.

ExportScanner:
    call GetExportScannerFile

    ..If D$SfFileMemory <> 0
        push D$UserPeStart
            move D$UserPeStart D$SfFileMemory

            call StartScan

            .If eax <> 0
                call ReAlignPE | call GetExportDirPointer

                If D$NumberOfDisExportedFunctions <> 0
                    call 'USER32.DialogBoxParamA' D$hInstance, 30500, &NULL,
                                                  ViewExport, &NULL

                Else
                    jmp L7>

                End_If
            .Else
L7:             call 'USER32.MessageBoxA' D$hwnd, {'No Export found in this File', 0}, SfFile, 0

            .End_If

            VirtualFree D$UserPeStart
          ; D$UserPeStart and not D$SfFileMemory, because of the switch in 'ReAlignPE'

        pop D$UserPeStart

    ..End_If
ret


; Simplified version of 'StartNewDisFile', reuse the same Variables:

StartScan:
    mov edi FirstDisVirtualData, eax 0
    mov ecx LastDisVirtualData | sub ecx edi | shr ecx 2 | repe stosd
    mov D$DisPeOrigine 0 | GetPeHeader PeHeaderPointer
    mov eax D$eax | sub eax 080 | mov D$DisPeOrigine eax
  ; (080 is the RosAsm Data 'PeHeaderPointer')

    GetPeHeader PeHeader

    ..If eax < D$SfFileMemory
        mov eax 0

    ..Else_If eax > D$SfFileMemoryEnd
        mov eax 0

    ..Else
        mov eax D$eax

        .If eax = D$PeHeader
            GetPeHeader NumberOfSections | movzx eax W$eax

            If eax <> 0
                mov D$DisNumberOfSections eax
                GetPeHeader ImageBase | mov ebx D$eax, D$DisImageBase ebx
                move D$DisRvaSectionAlignment D$eax+4, D$DisFileSectionAlignment D$eax+8
            End_If

        .Else
            mov eax 0

        .End_If

    ..End_If
ret


; SImplified version of 'CheckExport':

GetExportDirPointer:
    GetPeHeader SectionTable | mov edx D$eax

    and D$NumberOfDisExportedFunctions 0

    If edx <> 0
        mov ecx D$DisNumberOfSections

        GetPeHeader SectionsHeaders

L0:     mov ebx D$eax+SECTION_RVA |  cmp edx ebx | jb L1>
            add ebx D$eax+SECTION_FILESIZE | cmp edx ebx | jb L2>

L1:     add eax SECTIONHEADERSIZE | loop L0<
            ret

L2:     add edx D$UserPeStart | mov eax D$edx+(5*4), ebx D$edx+(6*4), ecx D$edx+(4*4)
        mov D$NumberOfDisExportedFunctions eax, D$NumberOfDisExportNames ebx, D$DisExportOrdBase ecx
    End_If
ret


[SF:
 SF.lStructSize: D$ len
 SF.hwndOwner: D$ 0
 SF.hInstance: D$ 0
 SF.lpstrFilter: D$ SfFilter
 SF.lpstrCustomFilter: D$ 0
 SF.nMaxCustFilter: D$ 0
 SF.nFilterIndex: D$ 1
 SF.lpstrFile: D$ SfFile
 SF.nMaxFile: D$ &MAXPATH
 SF.lpstrFileTitle: D$ 0
 SF.nMaxFileTitle: D$ 0
 SF.lpstrInitialDir: D$ 0
 SF.lpstrTitle: D$ 0
 SF.Flags: D$ &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES__&OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 SF.nFileOffset: W$ 0
 SF.nFileExtension: W$ 0
 SF.lpstrDefExt: D$ 0
 SF.lCustData: D$ 0
 SF.lpfnHook: D$ 0
 SF.lpTemplateName: D$ 0]

[SfFilter: B$ 'Module Name', 0, '*.*', 0, 0]
[SfFile: B$ ? #&MAXPATH]
[SfFileHandle: ?    SfFileLen: ?    SfFileMemory: ?   SfFileMemoryEnd: ?]

GetExportScannerFile:
    mov D$SfFile 0, D$SfFileMemory 0
    move D$SF.hwndOwner D$hwnd ;, D$SF.hInstance D$hInstance
    call 'Comdlg32.GetOpenFileNameA' SF

    ..If D$SfFile <> 0
        call 'KERNEL32.CreateFileA' SfFile,
                                    &GENERIC_READ, &FILE_SHARE_READ,
                                    0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL

        .If eax = &INVALID_HANDLE_VALUE
            mov eax D$BusyFilePtr | call MessageBox | ret

        .Else
            mov D$SfFileHandle eax
            call 'KERNEL32.GetFileSize' eax, 0 | mov D$SfFileLen eax

            VirtualAlloc SfFileMemory D$SfFileLen
            add eax D$SfFileLen | mov D$SfFileMemoryEnd eax

            call 'KERNEL32.ReadFile' D$SfFileHandle, D$SfFileMemory D$SfFileLen,
                                     NumberOfReadBytes, 0

            call 'KERNEL32.CloseHandle' D$SfFileHandle

          ; Is it a PE?
            mov eax D$SfFileMemory | On W$eax <> 'MZ', jmp L7>
            add eax D$eax+03C | lea ecx D$eax+078
            On ecx > D$SfFileMemoryEnd, jmp L7>
            On ecx < D$SfFileMemory, jmp L7>
            On D$eax <> 'PE', jmp L7>

        .End_If
    ..End_If
ret

L7: VirtualFree D$SfFileMemory
    mov eax D$NotPeExePtr | call MessageBox
ret


[LV_COLUMN:
 @imask: D$ &LVCF_TEXT__&LVCF_WIDTH__&LVCF_FMT  ;__&LVCF_SUBITEM
 @fmt: D$ &LVCFMT_LEFT
 @cx: D$ 0
 @pszText: D$ 0
 @cchTextMax: D$ 0
 @iSubItem: D$ 0
 @iImage: D$ 0
 @iOrder: D$ 0]

[LV_ITEM:
 @imask: D$ &LVIF_TEXT
 @iItem: D$ 0
 @iSubItem: D$ 0
 @state: D$ 0
 @stateMask: D$ 0
 @pszText: D$ ExportViewBuffer
 @cchTextMax: D$ 0
 @iImage: D$ 0
 @lParam: D$ 0
 @iIndent: D$ 0]

[ScanExportPointer: ?    ScanPeOrigine: ?    SfListHandle: ?]

[ExportViewBuffer: ? #60] [AfterExportViewBuffer: ?]

; Tag Dialog 30500

Proc ViewExport:
    Arguments @Adressee, @Message, @wParam, @lParam
    Structure @RECT 16, @leftDis 0,  @topDis 4,  @rightDis 8,  @bottomDis 12

    pushad

    ...If D@Message = &WM_COMMAND
        mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            call 'User32.EndDialog' D@Adressee, 0

        ..Else
            call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &EM_SETSEL, 0-1, 0

        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG

        call 'USER32.GetDlgItem' D@Adressee, 10 | mov D$SfListHandle eax

        call 'USER32.GetClientRect' D$SfListHandle D@RECT
        mov eax D@rightDis | sub eax D@leftDis | mov D@bottomDis eax
        shr eax 3 | mov D$LV_COLUMN@cx eax | sub D@bottomDis eax

        push eax
            mov D$LV_COLUMN@pszText {'Ordinal', 0}
            call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTCOLUMN, 0, LV_COLUMN

            shl D$LV_COLUMN@cx 1 | mov eax D$LV_COLUMN@cx | sub D@bottomDis eax
            mov D$LV_COLUMN@pszText {'Relative Address', 0}
            call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTCOLUMN, 1, LV_COLUMN

            move D$LV_COLUMN@cx D@bottomDis
        pop eax

        mov D$LV_COLUMN@pszText {'Name', 0}
        call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTCOLUMN, 2, LV_COLUMN

      ; 'ExportSectionComments'

        GetPeHeader SectionTable | mov edi D$eax | add edi D$UserPeStart

            mov eax D$edi+(3*4) | add eax D$UserPeStart
            If eax > D$UserPeStart
                On eax < D$UserPeEnd,
                        call 'USER32.SendMessageA', D@Adressee, &WM_SETTEXT, 0, eax
            End_If

            call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEMCOUNT, D$edi+(5*4), 0

      ; Number of Functions:
        mov ecx D$edi+(5*4)
      ; Pointer to ExportAdressesTable:
        mov ebx D$edi+(7*4) | On ebx <> 0, add ebx D$UserPeStart
      ; Pointer to ExportNamesTable:
        mov esi D$edi+(8*4) | On esi <> 0, add esi D$UserPeStart
      ; Pointer to ExportOrdinals:
        mov edx D$edi+(9*4) | On edx <> 0, add edx D$UserPeStart

        If esi = edx
         mov esi 0, edx 0
        End_If

        mov D$LV_ITEM@iItem 0
L0:
        If D$ebx = 0
           pushad
           dec D$LV_ITEM@iItem
           jmp L5>>
        End_If
; Write the Ordinal:
        mov eax ebx | sub eax D$UserPeStart | sub eax D$edi+(7*4) | shr eax 2
        pushad
        add eax D$edi+(4*4) | mov edi ExportViewBuffer | call WriteEax | mov B$edi 0
        mov D$LV_ITEM@iSubItem 0
        call 'USER32.SendMessageA' D$SfListHandle, &LVM_INSERTITEM, 0, LV_ITEM
        popad
; Write the Relative Address:
        pushad
        mov eax D$ebx
        mov edi ExportViewBuffer | call WriteEax | mov B$edi 0
        mov D$LV_ITEM@iSubItem 1
        call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEM, 0, LV_ITEM
        popad

        pushad
        cmp edx 0 | je L5>>
        mov ecx D$edi+(6*4) | mov edi edx | repne scasw | jne L5>>
        sub edi 2 |sub edi edx | shl edi 1 | mov esi D$esi+edi | test esi esi | je L5>>
        add esi D$UserPeStart | mov edi ExportViewBuffer
        .If esi =< D$UserPeStart
            mov D$edi '???'
        .Else_If esi >= D$UserPeEnd
            mov D$edi '???'
        .Else
            While B$esi <> 0
               If B$esi < ' '
                  mov D$ExportViewBuffer '???' | jmp L2>
               End_If
               movsb
               If edi = AfterExportViewBuffer
                  mov D$edi-3 '...' | jmp L2>
               End_If
               If esi = D$UserPeEnd
                  mov D$edi '???' | jmp L2>
               End_If
            End_While | mov B$edi 0
        .End_If
L2:
        mov D$LV_ITEM@iSubItem 2
        call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEM, 0, LV_ITEM
L5:
        popad | add ebx 4 | inc D$LV_ITEM@iItem | dec ecx | jnz L0<<
        call 'USER32.SendMessageA' D$SfListHandle, &LVM_SETITEMCOUNT, D$LV_ITEM@iItem, 0
L7:     popad | mov eax &FALSE | ExitP

    ...Else
        popad | mov eax &FALSE | ExitP

    ...End_If

L8: popad | mov eax &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; GUIDs Stuff

[ShowGUIDsHandle: ?  GUDfileIsThere: ?]

; All of this GUIDs stuff and the associated GUD file is to be used with:
;
; [COMCall | mov eax D$#1 eax D$eax | push #L>3 | Call D$eax+#1.#2 D$#1]

ViewGUIDs:
    If D$ShowGUIDsHandle = 0
        call 'USER32.CreateDialogParamA' D$hInstance, 35, D$hWnd, ShowGUIDsProc, &NULL
    End_If
ret


[GUIDsHelp: 'GUIDs', 0]

; Tag Dialog 35

Proc ShowGUIDsProc:
     Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ..If D@Message = &WM_COMMAND
        .If D@wParam = &IDCANCEL
L7:        mov D$ShowGUIDsHandle 0
           VirtualFree D$GUIDsFileMemory
           call 'User32.EndDialog' D@Adressee, 0

        .Else_If D@wParam = &IDOK
L8:         call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &LB_GETCURSEL, 0, 0
            call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &LB_GETTEXT, eax,
                                              TrashString

            call GetGUID eax | On D$GUIDsPastingType <> 101, jmp L7<<

        .Else_If D@wParam = &IDHELP
            call Help, B_U_AsmName, GUIDsHelp, ContextHlpMessage

        .Else_If D@wParam = 100
            mov D$GUIDsPastingType 100

        .Else_If D@wParam = 101
            mov D$GUIDsPastingType 101

        .Else
            mov eax D@wParam | shr eax 16
            On eax = &LBN_DBLCLK, jmp L8<<

        .End_If

    ..Else_If D@Message = &WM_INITDIALOG
        move D$ShowGUIDsHandle D@Adressee
        call 'USER32.GetDlgItem' D@Adressee, 10
        call InitGUIDsView eax | On B$GUIDsInit = &FALSE, jmp L7<<

        If B$SourceReady = &FALSE
            call Disable D@Adressee 100
            call Disable D@Adressee 101
        End_If

        On D$GUIDsPastingType = 0, mov D$GUIDsPastingType 101

        If D$GUIDsPastingType = 100
            call 'User32.SendDlgItemMessageA' D@Adressee, 100, &BM_SETCHECK, &TRUE, 0
        Else
            call 'User32.SendDlgItemMessageA' D@Adressee, 101, &BM_SETCHECK, &TRUE, 0
        End_If

    ..Else_If D@Message = &WM_VKEYTOITEM
            If W@wParam = &VK_RETURN
                jmp L8<<

            Else_If W@wParam = &VK_ESCAPE
                jmp L7<<

            End_If

    ..Else_If D@Message = &WM_CTLCOLORLISTBOX
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ..Else
        popad | mov eax &FALSE | jmp L9>

    ..End_If

    popad | mov eax &TRUE
L9: EndP
____________________________________________________________________________________________

[GUIDsFilePath: B$ ? #&MAXPATH]

[GUIDsFileNamePointer: ?  GUIDsFileHandle: ?  GUIDsFileLength: ?
 GUIDsFileMemory: ?  GUIDsInit: ?]

[MissingGUIDsFile: B$ "
Before using this Tool,
you have to save the GUIDs File(s)
in the RosAsmFiles Directory
(aside Equates.equ)
", 0]

Proc InitGUIDsView:
    Argument @Handle

    mov esi IncludeFileName, edi GUIDsFilePath
    While B$esi <> 0 | movsb | End_While
    While B$edi-1 <> '\' | dec edi | End_While
    mov D$GUIDsFileNamePointer edi
    mov W$edi '*.', D$edi+2 'GUD'

    call 'KERNEL32.FindFirstFileA' GUIDsFilePath, FindFile

    ...If eax <> &INVALID_HANDLE_VALUE
        call 'KERNEL32.FindClose' eax

        mov esi FindFile.cFileName, edi D$GUIDsFileNamePointer
        While B$esi <> 0 | movsb | End_While | mov B$edi 0

        call 'KERNEL32.CreateFileA' GUIDsFilePath, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        mov D$GUIDsFileHandle eax

        call 'KERNEL32.GetFileSize' eax, 0 | mov D$GUIDsFileLength eax

        VirtualAlloc GUIDsFileMemory eax

        call 'KERNEL32.ReadFile' D$GUIDsFileHandle, D$GUIDsFileMemory,
                                 D$GUIDsFileLength, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$GUIDsFileHandle

        mov esi D$GUIDsFileMemory, edx esi | add edx D$GUIDsFileLength

        .While esi < edx
            .If B$esi = '['
                inc esi
                If B$esi = '&'
                    ;While B$esi <> '_' | inc esi | End_While |
                    inc esi
                    mov edi TrashString
                    While B$esi <> ':' | movsb | End_While | mov B$edi 0
                    push edx
                        call 'USER32.SendMessageA' D@Handle, &LB_ADDSTRING, 0, TrashString
                    pop edx
                End_If
            .End_If

            inc esi

        .End_While

        mov B$GUIDsInit &TRUE

    ...Else
        call 'USER32.MessageBoxA', 0, MissingGUIDsFile, {'File not found', 0}, 0

    ...End_If
EndP
____________________________________________________________________________________________

[GUID_MotherNamePointer: ?   SourceComment: ?  GUIDsPastingType: ?]

Proc GetGUID:
    Argument @Length

    mov D$SourceComment 0
    mov esi D$GUIDsFileMemory, edx esi | add edx D$GUIDsFileLength

    .While esi < edx
        .If B$esi = '['
            mov ebx esi
            inc esi
            If B$esi = '&' ;'IID_'
                ;While B$esi <> '_' | inc esi | End_While |
                inc esi
                mov D$GUID_MotherNamePointer TrashString
                mov edi TrashString, ecx D@Length | repe cmpsb | je L5>

            End_If

        .Else_If B$esi = ';'
          ; Example: "; SOURCE FILE -> strmf.h"
            If D$esi+2 = 'SOUR'
                On D$esi+6 = 'CE F', mov D$SourceComment esi
            End_If

        .End_If

        inc esi

    .End_While

    ExitP

L5:

  ; ebx pointing to the wanted GUID.
    mov esi ebx, edi Trash

    If D$SourceComment <> 0
        push esi
            mov esi D$SourceComment
            While B$esi <> CR | movsb | End_While
            mov D$edi CRLF2 | add edi 4
        pop esi
    End_If

  ; Copy the GUID Data, with the Sizes Markers:
    mov B$edi '[' | inc edi | add esi 2
    While B$esi <> ':' | movsb | End_While | Movsb

    mov D$edi ' D$ ' | add edi 4
    While B$esi = ' ' | inc esi | End_While
    call MovsbHexa

    mov D$edi ' W$ ' | add edi 4
    While B$esi = ' ' | inc esi | End_While
    call MovsbHexa

    mov D$edi ' W$ ' | add edi 4
    While B$esi = ' ' | inc esi | End_While
    call MovsbHexa

    mov D$edi ' B$ ' | add edi 4
    .While B$esi <> ']'
        While B$esi = ' ' | inc esi | End_While
        call MovsbHexa
        mov B$edi ' ' | inc edi
    .End_While
    mov B$edi-1 ']'
    While B$esi <> CR | inc esi | End_While

    mov D$edi CRLF2 | add edi 4
    While B$esi <= ' '  | inc esi | End_While

  ; Copy the other stuff:
    .While esi < edx
        If W$esi = '[.'
            push edx
              ; Skip the "IID_" thingies:
                mov eax D$GUID_MotherNamePointer
                While B$eax <> '_' | inc eax | End_While | inc eax
                mov D$GUID_MotherNamePointer eax
                call BuildGUIDvTable | call InsertGUIDsObjectHandle
            pop edx
        Else_If W$esi = '[&'
            jmp L5>
        Else_If B$esi = ';'
            While B$esi <> CR | inc esi | End_While | add esi 2
        Else
            movsb
        End_If
    .End_While

L5: mov B$edi 0

    push D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
        mov B$BlockInside &TRUE, D$BlockStartTextPtr Trash
        dec edi | mov D$BlockEndTextPtr edi

        call ControlC

        If D$GUIDsPastingType = 101
            mov B$BlockInside &FALSE
            call ControlV | call AskForRedrawNow
        End_If

    pop D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside
EndP


InsertGUIDsObjectHandle:
  ; Insert the Object Handle Declaration:
    mov D$edi CRLF2 | add edi 4

    mov B$edi '[' | inc edi
    push esi
        mov esi D$GUID_MotherNamePointer
        ;While B$esi <> '_' | inc esi | End_While | inc esi
        While B$esi <> 0 | movsb | End_While
        mov B$edi ':' | inc edi
    pop esi
    mov D$edi ' ?]' | add edi 3
ret
____________________________________________________________________________________________

; Just to turn all GUIDs' hexa numbers upper cases:

MovsbHexa:
L0: lodsb
    .If al > ' '
        If al = ']'
            dec esi | ret
        Else_If al < 'Z'

        Else
            sub al ' '
        End_If

        stosb | jmp L0<
    .End_If
ret
____________________________________________________________________________________________

[vTableQueryInterface: '.QueryInterface', 0
 vTableAddRef: '.AddRef', 0
 vTableRelease: '.Release', 0]

BuildGUIDvTable:
    mov D$GUIDDisplacementEquate 0
  ; Copy the '[':
    movsb
    push esi
        zCopy D$GUID_MotherNamePointer, vTableQueryInterface
        call WriteGUIDDisplacementEquate
        mov B$edi ' ' | inc edi
        zCopy D$GUID_MotherNamePointer, vTableAddRef
        call WriteGUIDDisplacementEquate
        mov B$edi ' ' | inc edi
        zCopy D$GUID_MotherNamePointer, vTableRelease
        call WriteGUIDDisplacementEquate
    pop esi

    .While B$esi <> ']'
        mov B$edi ' ' | inc edi
        push esi | zCopy D$GUID_MotherNamePointer | pop esi
        While B$esi <> CR
            mov al B$esi, B$edi al | inc edi | inc esi
            If al = ']'
                dec edi | call WriteGUIDDisplacementEquate
                sub edi 2 | mov B$edi ']' | inc edi | ret
            End_If
        End_While

        call WriteGUIDDisplacementEquate

        While B$esi <= ' ' | inc esi | End_While
    .End_While
ret
____________________________________________________________________________________________

[GUIDDisplacementEquate: ?]

WriteGUIDDisplacementEquate:
    push ebx, edx, esi
        mov B$edi ' ' | inc edi
        mov eax D$GUIDDisplacementEquate
        call WriteEax
        add D$GUIDDisplacementEquate 4
    pop esi, edx, ebx
    mov W$edi CRLF | add edi 2
ret
____________________________________________________________________________________________


































