TITLE Main
 _______________________________________________________________________________________
 _______________________________________________________________________________________


[WindowX: 5  WindowY: 2  WindowW: 790  WindowH: 595
 SaveMainPosFlag: 0  IsMaximizedFlag: &SW_SHOWNORMAL]

[ListEditRect: ListERX: 0   ListERY: 0   ListERW: 0   ListERH: 0]
[WindowStyle: &WS_OVERLAPPEDWINDOW
 WindowExStyle: &WS_EX_CLIENTEDGE]
[MenuHandle: 0    ScrollBarWanted: &TRUE]

[FindString: 'commdlg_FindReplace' 0  FindStringMessage: 0]

[RosAsmMutexName: B$ 'RosAsmIsRuning', 0   MultiInstance: &FALSE]

[ClassName: B$ 'RosAsmWindowClass' 0   EditClassName: 'EDIT' 0
 AppName: '  RosAsm, The Bottom-Up Assembler for ReactOS -V.2.052f-' 0]


[WndClassEx:
 wc_Size: len         wc_style: 11      WndProc: MainWindowProc   wc_ClsExtra: 0
 wc_WndExtra: 0       wc_hInstance: 0   wc_hIcon: 0               wc_hCursor: 0
 wc_hbrBackground: &COLOR_SCROLLBAR+1 ; 6
 wc_MenuName: 0    wc_ClassName: ClassName   wc_hIconSm: 0]

[Bp_hCursor: ?   ActualCursor: ?  WaitCursor: ?]
 __________________________________________________________________________________

; First message structure is for the main loop (Get-Translate-DispatchMessage)
; Second is for Callback. They can't be only one because Wparam for ExitProcess is
; not the same as Wparam previously stored for CallBack.

[FirstMsg: FAdressee: ?  FuMsg: ?  FWparam: ?  FLparam: ?  FTime: ?  FPoint: ? ?]

[hInstance: ?  hwnd: ?  hwndEdit: ?  FindHandle: ?]

; For mem tests:

[Meminfo: ? #20]

GetMemory:
    call 'KERNEL32.GlobalMemoryStatus' Meminfo
    hexprint D$Meminfo+12
ret
 __________________________________


[UserHaveClickDown: B$ 0]

; (UserHaveClickDown flag is to prevent block drawing when back from 'OpenFileName' api
 __________________________________

; Accelerators:

; [DRAWLINE 628]

;;
ACCELNUMBER 2    FLAGLAST 080]

;  ;  01  070  03E8  00    ;03E8 > about ID  (F1)
;  ;  01  071  03F4  00    ;03F4 > Save ID   (F2)
;  ;  01  072  03EC  00    ;03EC > Open ID   (F3)
;  ; 090  078  03ED  00    ;03ED > Exit ID   (Alt-X)  78 > 'x'

; &FCONTROL  &FNOINVERT  &FSHIFT  &FVIRTKEY  &FALT

[ACCELERATORS:
 U$ &FVIRTKEY__&FNOINVERT                        &VK_F1    M00_RosAsm
    &FVIRTKEY__&FCONTROL__&FNOINVERT+FLAGLAST    &VK_8     DRAWLINE]
;;

[IncludesOK: ?  RichEditHandle: ?]

Main:
  ; For the Resurces Editor:
    call 'KERNEL32.LoadLibraryA' {'riched20.dll',0} | mov D$RichEditHandle eax

  ; Install exception handler
    call 'KERNEL32.SetUnhandledExceptionFilter' FinalExceptionHandler

  ; Ensure mono-instance:
    call 'KERNEL32.CreateMutexA' &NULL &TRUE RosAsmMutexName
    call 'KERNEL32.GetLastError'
    On eax = &ERROR_ALREADY_EXISTS, mov B$MultiInstance &TRUE

    call 'Kernel32.GetModuleHandleA' 0
      mov D$hInstance eax, D$wc_hInstance eax, D$OSSInstance eax,
          D$OPESInstance eax, D$BmOpenInstance eax

    call 'User32.LoadIconA' eax 1
    mov D$wc_hIcon eax, D$wc_hIconSm eax

    call 'User32.LoadCursorA' 0, &IDC_ARROW  | mov D$Bp_hCursor eax
    call 'User32.LoadCursorA' 0, &IDC_IBEAM | mov D$wc_hCursor eax
    mov D$ActualCursor eax
    call 'User32.LoadCursorA' 0, &IDC_WAIT | mov D$WaitCursor eax

    call WineKey
    call WhateverConfig

    mov D$WriteCheckerWanted 0 ; Temporary locked. ('WriteChecker' ---> toDo as a Hook).

    call ResetBackGroundColors | move D$wc_hbrBackground D$BackGroundBrushHandle

    call 'User32.RegisterClassExA' WndClassEx

    call 'User32.LoadMenuA' D$hInstance M00_Menu | mov D$MenuHandle eax

    call GetWheelInfo

    call AddUserMenu

    call NewBuildWin32Equates

    If B$IncludesOK = &TRUE
        On D$EquatesName = 'Equa', call AppendToCurrentDirectory
        call PrepareStructuresFiles
    End_If

    call 'User32.CreateWindowExA' D$WindowExStyle, ClassName, AppName, D$WindowStyle,
                                  D$WindowX, D$WindowY, D$WindowW, D$WindowH, 0,
                                  D$MenuHandle, D$hInstance, 0

   ; for a 'full screen 'window's user area (ex: screen-saver):
   ;
   ; call 'User32.CreateWindowExA' 04030D0  ClassName  AppName  096000000,
   ;                               D$WindowX  D$WindowY  D$WindowW  D$WindowH  0,
   ;                               0  D$hInstance  0

    mov D$hwnd eax, D$hwndFileOwner eax, D$hwndPEFileOwner eax,
        D$PD_hWndOwner eax, D$BmhwndFileOwner eax

    On D$StringsLanguage <> '.en', call OpenStringsFile

    call EnableMenutems | call EnableHelpMenutems
    call EnableVisualTutsMenu | call EnableWizardsMenu
    call EnableClipMenu

    call CreateStatusBar
    On B$ToolBarWanted = &TRUE, call CreateToolBar
    On B$ScrollBarWanted = &TRUE, call CreateScrollBar
    call CreateEditWindow

  ; (D$IsMaximizedFlag = &SW_SHOWNORMAL or &SW_MAXIMIZE):
    call 'USER32.ShowWindow'  D$hwnd, D$IsMaximizedFlag

    call 'USER32.UpdateWindow' D$hwnd

    call 'USER32.GetClipCursor' FullRECT

    call 'USER32.RegisterWindowMessageA' FindString | mov D$FindStringMessage eax

  ; Rotary table for moving inside text:
    call SetBackTableMemory | call InitUndo

    call CreateFontForDialogEdition | call LoadFont

    On D$NATION_LOGFONT@lfWeight <> 0, call LoadNationalFont

    call SetUndoDirectory
    call DeleteOldUndoFiles

    call CheckAllMRUFile | call SetMRUmenu | On B$LoadMRU = &TRUE, call LoadLastMRUFile


  ; copying compilable version of icon in case user compiles vithout defining any icon:
    call StoreIcon

    On B$BlinkingCaretWanted = &TRUE, call InitBlinkCursor

    call InitExpressionBuffers

;    call 'USER32.CreateAcceleratorTableA' ACCELERATORS ACCELNUMBER
;    mov D$AccelHandle eax

    jmp L1>>
   ___________________________________________________________________________

   ; Our main loop: when 'DispatchMessageA' called, Win calls upper CallBack:
   ___________________________________________________________________________

L0: call 'User32.IsDialogMessageA' D$FindReplaceHandle Firstmsg  | On eax > 0, jmp L1>

    If B$IsDebugging = &TRUE
        call 'User32.TranslateAcceleratorA' D$DebugDialogHandle, D$DbgAccelHandle, FirstMsg
        cmp eax &TRUE | je L1>
        call 'User32.IsDialogMessageA' D$CurrentDataPageHandle, FirstMsg
        cmp eax &TRUE | je L1>
        call 'User32.IsDialogMessageA' D$DebugDialogHandle, FirstMsg
        cmp eax &TRUE | je L1>
    End_If

    call 'User32.TranslateMessage' Firstmsg
    call 'User32.DispatchMessageA' Firstmsg

L1: call 'User32.GetMessageA' FirstMsg 0 0 0

    cmp eax 0 | ja L0<<

  ; call ReleaseFonts
    call UpdateRegistry

    call 'KERNEL32.FreeLibrary' D$RichEditHandle

  ; call 'USER32.DestroyAcceleratorTable' D$AccelHandle

    call 'Kernel32.ExitProcess' D$FWparam
____________________________________________________________________________________________

ResetBackGroundColors:
    On D$BackGroundBrushHandle <> 0, call 'KERNEL32.CloseHandle' D$BackGroundBrushHandle
    call 'GDI32.CreateSolidBrush' D$NormalBackColor
    mov D$BackGroundBrushHandle eax

    On D$DialogsBackGroundBrushHandle, <> 0, call 'KERNEL32.CloseHandle' D$DialogsBackGroundBrushHandle
    call 'GDI32.CreateSolidBrush' D$DialogsBackColor
    mov D$DialogsBackGroundBrushHandle eax

    On D$CaretBrushHandle, <> 0, call 'KERNEL32.CloseHandle' D$CaretBrushHandle
    call 'GDI32.CreateSolidBrush' D$StatementColor
    mov D$CaretBrushHandle eax

    On D$RedBrushHandle, <> 0, call 'KERNEL32.CloseHandle' D$RedBrushHandle
    call 'GDI32.CreateSolidBrush' D$BracketColor
    mov D$RedBrushHandle eax
ret

[CaretTime: 600    ShowCaret: &TRUE    BlinkingCaretWanted: &FALSE]

ResetBlinkCursor:
    call KillBlinkCursor
InitBlinkCursor:
    call 'User32.SetTimer' D$hwnd, 1, D$CaretTime, BlinkProc
ret


[CaretOnlyRedraw: ?]

BlinkProc:
    .If B$BlockInside = &FALSE
L1:     xor B$ShowCaret &TRUE
        If D$CaretRectangle+8 <> 0
            mov B$CaretOnlyRedraw &TRUE | call AskForRedraw
        End_If
    .Else
        On B$ShowCaret = &FALSE, jmp L1<
    .End_If
ret 16


KillBlinkCursor:
    call 'User32.KillTimer' D$hwnd, 1
    mov B$ShowCaret &TRUE
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
; Init the Deleted Blocks Undo Folder and delete Undo Files if any:

[UndoDirectory: UndoFile: B$ ? #&MAX_PATH]
[PointerToUndoNumber: '000.'] ; Old nUndoFile
[AllUndoFiles2: B$ ? #&MAX_PATH]

;[UndoFile: 'RosAsmUndo' nUndoFile: '000.$$$' 0  ; 17 Bytes.
; AllUndoFiles: 'RosAsmUndo???.$$$' 0]
;
; The full name looks like this:
; 'E:\RosAsm3\RosAsmUndo\Undo000.$$$'
;
; 'PointerToUndoNumber' points to '000.$$$'
;
; 'AllUndoFiles', used to search for Files to be deleted, looks like this:
; 'E:\RosAsm3\RosAsmUndo\Undo???.$$$'

[UndoExist: "
Block-Delete Undo-Files have been found in the
Temporary Directory (...\RosAsmUndo\).

The existing Undo-Files are going to be deleted and
the previous instance of RosAsm will no more be able to
UnDelete its saved Blocks.
"

MultiUndo: "
You are runing several instances of RosAsm. Do not        
Delete/UnDelete Blocks of text [Ctrl][X] / [Ctrl][Z]
The results could be unwished.

" 0]

SetUndoDirectory:
    mov edi UndoDirectory, ecx &MAX_PATH, al 0 | rep stosb
    call 'KERNEL32.GetTempPathA' &MAX_PATH UndoDirectory

    mov edi UndoDirectory, al 0, ecx &MAX_PATH | repne scasb | dec edi
    On B$edi-1 = '\', dec edi
    mov eax '\Ros' | stosd | mov eax 'AsmU' | stosd | mov ax 'nd' | stosw | mov al 'o' | stosb

    push edi
        call 'KERNEL32.CreateDirectoryA' UndoDirectory &NULL | mov ebx eax
    pop edi

    If B$MultiInstance = &FALSE
        push edi
            mov D$edi '\Und', D$edi+4 'o*.$', W$edi+8 '$$', B$edi+9 0
            call 'KERNEL32.DeleteFileA' UndoDirectory
        pop edi
    End_If

    mov eax '\Und' | stosd | mov eax 'o001' | stosd | mov eax '.$$$' | stosd | mov B$edi 0
    sub edi 5
    mov D$PointerToUndoNumber edi | sub D$PointerToUndoNumber 2

    mov esi UndoDirectory, edi AllUndoFiles2, ecx &MAX_PATH | rep movsb
    mov eax D$PointerToUndoNumber | sub eax UndoDirectory
    mov edi AllUndoFiles2 | add edi eax | mov eax '???.' | stosd

    ..If ebx = 0   ; 'CreateDirectoryA failed to create a new Dir >>> Already exist.

        call 'KERNEL32.FindFirstFileA' AllUndoFiles2 FindFile
        .If eax <> &INVALID_HANDLE_VALUE
            If B$MultiInstance = &TRUE
                call 'USER32.MessageBoxA' D$hwnd, UndoExist, Argh, &MB_OKCANCEL__&MB_ICONHAND
                On eax = &IDCANCEL, call 'KERNEL32.ExitProcess' 0
            End_If

            call DeleteOldUndoFiles

        .Else
            call 'KERNEL32.FindClose' eax
            If B$MultiInstance = &TRUE
                call 'USER32.MessageBoxA' D$hwnd, MultiUndo, Argh, 0
            End_If

        .End_If
    ..End_If

    mov edi D$PointerToUndoNumber, eax '000.' | stosd
ret


;;
 BackTable is used for moving back and forward in text after tree view or right clicks
 moves. It is an 8 bytes rotary table; this is to say that we use only lower byte of
 BackTablePtr to ajust moves inside table, like this:

 > mov ebx D$BackTablePtr | add (or sub) BL 4

 As memory given by win is page aligned, if Bl = 0, "sub bl 4" points to end of table.
;;

[BackTable: ?  BackTablePtr: ?]

SetBackTableMemory:
    VirtualAlloc BackTable 0100
    move D$BackTablePtr D$BackTable
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[ToolBarHandle: ?]
[TOOLBUTTONS_NUMBER 18] ; (zero based).

[ToolBarButtons:
 D$  0  M00_Tree  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$  1  M00_Open  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  2  M00_New  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$  3  M00_Compile  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  4  M00_Run  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$ 14  M00_About_ToolBar  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0

 D$ 0 0 B$ &TBSTATE_ENABLED  &TBSTYLE_SEP 0 0  D$ 0 0
 D$  5  M00_Calc  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  6  M00_Clip_File  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  7  M00_Structures  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  8  M00_New_Dialog   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$  9  M00_Find  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 10  M00_Replace  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 11  M00_Ascii_Table   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 12  M00_Configuration   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 13  M00_B_U_Asm  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 14  M00_About   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 15  M00_Print   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 16  M00_Main_Icon   B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 17  M00_Save_Source_Only  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 18  M00_Exit  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 D$ 19 12  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 20 0CE  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 21 0CE  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
; D$ 22 0CE  B$ &TBSTATE_ENABLED  &TBSTYLE_BUTTON  0 0  D$ 0 0
 ]

[toolbar00_str_00: B$ 'Tree', 0
 toolbar00_str_01: '', 0
 toolbar00_str_02: 'Open', 0
 toolbar00_str_03: 'New', 0
 toolbar00_str_04: '', 0
 toolbar00_str_05: 'Compile', 0
 toolbar00_str_06: 'Run', 0
 toolbar00_str_07: '', 0
 toolbar00_str_08: 'About ToolBar', 0
 toolbar00_str_09: '', 0
 toolbar00_str_10: 'Calc', 0
 toolbar00_str_11: 'Clip', 0
 toolbar00_str_12: 'Structures', 0
 toolbar00_str_13: 'New Dialog', 0
 toolbar00_str_14: 'Find', 0
 toolbar00_str_15: 'Replace', 0
 toolbar00_str_16: 'Ascii Table', 0
 toolbar00_str_17: 'Configuration', 0
 toolbar00_str_18: 'B_U_Asm', 0
 toolbar00_str_19: 'Print', 0
 toolbar00_str_20: 'Main Icon', 0
 toolbar00_str_21: 'Save Source',0
 toolbar00_str_22: 'Exit', 0
 toolbar00_str_23: 'Wizards', 0, 0]

 [PointersToToolTipsStrings: toolbar00_str_00   toolbar00_str_01
                              toolbar00_str_02
                             toolbar00_str_03   toolbar00_str_04
                             toolbar00_str_05
                             toolbar00_str_06   toolbar00_str_07
                              toolbar00_str_08
                             toolbar00_str_09
                            toolbar00_str_10   toolbar00_str_11
                             toolbar00_str_12   toolbar00_str_13   toolbar00_str_14
                             toolbar00_str_15   toolbar00_str_16   toolbar00_str_17
                             toolbar00_str_18   toolbar00_str_19   toolbar00_str_20
                             toolbar00_str_21   toolbar00_str_22   toolbar00_str_23]

[TOOLTIPTEXT_NMHDR_hwndFrom 0            ; With WM_NOTIFY Message, Win sends in
 TOOLTIPTEXT_NMHDR_idfrom   4          ; lParam a pointer to a TOOLTIPTEXT
 TOOLTIPTEXT_NMHDR_code     8          ; Structure (which win GIVES to us).

 TOOLTIPTEXT_lpszText       12         ; Item
 TOOLTIPTEXT_szText         16         ; These Equates are to ease accesses to
 TOOLTIPTEXT_hInst          96         ; this Structure (which doesn't belong
 TOOLTIPTEXT_uFlags         100]       ; to our Data area).


[TB_NOTIFY_NMHDR_hwndFrom     0          ; And same for "TBN_GETBUTTONINFO" answers:
 TB_NOTIFY_NMHDR__idfrom      4
 TB_NOTIFY_NMHDR__code        8

 TB_NOTIFY_Item      12

 TB_NOTIFY_TBBUTTON_iBitmap   16
 TB_NOTIFY_TBBUTTON_idCommand 20
 TB_NOTIFY_TBBUTTON_fsState   24
 TB_NOTIFY_TBBUTTON_fsStyle   23
 TB_NOTIFY_TBBUTTON_dwData    28
 TB_NOTIFY_TBBUTTON_iString   32

 TB_NOTIFY_CharCount 36
 TB_NOTIFY_TextPtr   40]

[TbRECT: TbRECTleft: ?  TbRECTtop: ?  TbRECTright: ?  TbRECTbottom: ?]
[ToolBarPixelsHight: ?    ToolBarLinesHight: ?]
[ToolBarWanted: ?]

[HelpToolBar: 'Sure you need some help???!!!!', 0
 HelpToolBarTitle: 'Poor you!' 0]

[ToolBar_Registry:
 TB_hkr: &HKEY_CURRENT_USER
 TB_SubKey: ToolBarSubKey
 TB_ValueName: ToolBarValueName]

[ToolBarChange: B$ 0   ToolBarSubKey: 'Software\RosAsm\ToolBar' 0
                       ToolBarValueName: 'ToolBarState' 0]

CreateToolBar:
 ;&TBSTYLE_TOOLTIPS__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE,
    call 'COMCTL32.CreateToolbarEx' D$hWnd,
    &CCS_TOP__&TBSTYLE_TOOLTIPS__&WS_CHILD__&WS_VISIBLE__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE__&WS_BORDER,
    0300, TOOLBUTTONS_NUMBER, D$hInstance, 2, ToolBarButtons, 9, 0, 0, 20, 20, 20

; &WS_CHILD__&WS_VISIBLE__&WS_BORDER__&CCS_TOP__&TBSTYLE_ALTDRAG__&CCS_ADJUSTABLE,

    mov D$ToolBarHandle eax

    call 'USER32.SendMessageA' D$ToolBarHandle, &TB_AUTOSIZE, 0, 0

    call 'USER32.GetWindowRect' D$ToolBarHandle, TbRECT
    mov eax D$TbRECTbottom | sub eax D$TbRECTtop | dec eax
    mov D$ToolBarPixelsHight eax

    call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SAVERESTORE, &FALSE,  ; FALSE > restore
                               ToolBar_Registry
SaveToolBar:
    call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SAVERESTORE, &TRUE,   ; TRUE > save
                               ToolBar_Registry
ret



[StatusBarX: ?  StatusBarY: ? StatusBarW: ? StatusBarH: ?]
[StatusBarHight: ?]

CreateStatusBar:
    call 'COMCTL32.CreateStatusWindowA',
        &WS_CHILD__&WS_VISIBLE__&WS_DLGFRAME__&WS_BORDER__&SBARS_SIZEGRIP, 0, D$hwnd,
        STATUSBAR_ID
    mov D$StatusbarHandle eax

    call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETPARTS, 7, StatusPartsPos

    call 'USER32.GetWindowRect' D$StatusbarHandle, StatusBarX
    push D$StatusBarY
        call 'USER32.GetClientRect' D$hWnd, StatusBarX
        call 'USER32.ClientToScreen' D$hWnd, StatusBarW
    pop ebx
    mov eax D$StatusBarH | sub eax ebx | mov D$StatusBarHight eax
    call 'USER32.ShowWindow' D$StatusBarHandle &SW_SHOW
ret


[SbRECT: ScrollBarX: ?  ScrollBarY: ?  ScrollBarW: ?  ScrollBarH: ?]
[ScrollWindowHandle: ?]
[ScrollBarName: 'SCROLLBAR', 0]

CreateScrollBar:
    call 'USER32.GetClientRect' D$hWnd ScrollBarX
    push D$ScrollBarW
        If B$ToolBarWanted = &TRUE
            mov eax D$ToolBarPixelsHight | add D$ScrollBarY eax | sub D$ScrollBarH eax
        End_If
        call 'USER32.GetSystemMetrics' &SM_CXVSCROLL | mov D$ScrollBarW eax
    pop ebx | sub ebx eax | mov D$ScrollBarX ebx
    mov eax D$StatusBarHight | sub D$ScrollBarH eax

    call 'USER32.CreateWindowExA' 0, EditClassName, &NULL,
                                  &WS_CHILD__&WS_VISIBLE__&WS_VSCROLL,
                                  D$ScrollBarX, D$ScrollBarY, D$ScrollBarW, D$ScrollBarH,
                                  D$hWnd, &NULL, D$hInstance, 0
    mov D$ScrollWindowHandle eax

    call 'USER32.SetWindowLongA' D$ScrollWindowHandle, &GWL_WNDPROC, ScrollBarProc
ret


[EditWindowHandle: ?   BpWindowHandle: ?
 EditWindowX: ? EditWindowY: ? EditWindowW: EditWindowX2: ? EditWindowH: EditWindowY2: ?]

[BpMarginWidth: ?]

CreateEditWindow:
    mov eax D$FontWidth | shl eax 1 | mov D$BpMarginWidth eax

    call 'USER32.GetClientRect' D$hWnd EditWindowX
    mov eax D$BpMarginWidth | add D$EditWindowX eax

    If B$ToolBarWanted = &TRUE
        mov eax D$ToolBarPixelsHight
        add D$EditWindowY eax
        sub D$EditWindowH eax
    End_If

    If B$ScrollBarWanted = &TRUE
        mov eax D$ScrollBarW | sub D$EditWindowW eax
    End_If

    mov eax D$StatusBarHight | sub D$EditWindowH eax
    mov eax D$BpMarginWidth | sub D$EditWindowW eax

    call 'USER32.CreateWindowExA' 0, ClassName, &NULL,
                                  &WS_CHILD__&WS_VISIBLE,
                                  D$EditWindowX, D$EditWindowY, D$EditWindowW, D$EditWindowH,
                                  D$hWnd, &NULL, D$hInstance, 0
    mov D$EditWindowHandle eax

  ; Prepare the Client area for printing:
    ;call 'USER32.GetClientRect' D$EditWindowHandle, EditWindowX

  ; Create the BP Margin Window:
    call 'USER32.CreateWindowExA' 0, ClassName, &NULL,
                                  &WS_CHILD__&WS_VISIBLE,
                                  0, D$EditWindowY, D$EditWindowX, D$EditWindowH,
                                  D$hwnd, &NULL, D$hInstance, 0
    mov D$BpWindowHandle eax
ret
____________________________________________________________________________________________

[AraseBackEdit: ? #4]

Proc AraseBackGround:
    Argument @hdc
    call 'USER32.GetClientRect' D$EditWindowHandle, AraseBackEdit
    call 'USER32.FillRect' D@hdc, AraseBackEdit, D$BackGroundBrushHandle

    call AskForRedraw
EndP


MainResize:
  ; StatusBar:
    call 'USER32.GetClientRect' D$hWnd, StatusBarX

    mov eax D$StatusBarH | sub eax D$StatusBarHight | mov D$StatusBarY eax
    call 'USER32.MoveWindow' D$StatusBarHandle,
                             D$StatusBarX, D$StatusBarY, D$StatusBarW, D$StatusBarH,
                             &TRUE

  ; ToolBar:
    If B$ToolBarWanted = &TRUE
        call 'USER32.SendMessageA' D$ToolBarHandle, &TB_AUTOSIZE, 0, 0
    End_If

  ; ScrollBar:
    .If B$ScrollBarWanted = &TRUE
        call 'USER32.GetClientRect' D$hWnd ScrollBarX
        push D$ScrollBarW
            If B$ToolBarWanted = &TRUE
                mov eax D$ToolBarPixelsHight | add D$ScrollBarY eax | sub D$ScrollBarH eax
            End_If
            call 'USER32.GetSystemMetrics' &SM_CXVSCROLL | mov D$ScrollBarW eax
        pop ebx | sub ebx eax | mov D$ScrollBarX ebx
        mov eax D$StatusBarHight | sub D$ScrollBarH eax

        call 'USER32.MoveWindow' D$ScrollWindowHandle,
                                 D$ScrollBarX, D$ScrollBarY, D$ScrollBarW, D$ScrollBarH,
                                 &TRUE
    .End_If

  ; Edit:
    mov eax D$FontWidth | shl eax 1 | mov D$BpMarginWidth eax

    call 'USER32.GetClientRect' D$hWnd EditWindowX
    mov eax D$BpMarginWidth | add D$EditWindowX eax

    If B$ToolBarWanted = &TRUE
        mov eax D$ToolBarPixelsHight
        add D$EditWindowY eax
        sub D$EditWindowH eax
    End_If

    If B$ScrollBarWanted = &TRUE
        mov eax D$ScrollBarW | sub D$EditWindowW eax
    End_If

    mov eax D$StatusBarHight | sub D$EditWindowH eax
    mov eax D$BpMarginWidth | sub D$EditWindowW eax


;If D$TitleFontHandle <> 0
;    mov eax D$TitleTabHight | sub D$EditWindowH eax
;End_If

    call 'USER32.MoveWindow' D$EditWindowHandle,
                             D$EditWindowX, D$EditWindowY, D$EditWindowW, D$EditWindowH,
                             &TRUE
  ; Initialize the Edit Window dims as used by the Editor:
    ;call 'USER32.GetClientRect' D$EditWindowHandle, EditWindowX
    ;mov eax D$BpMarginWidth | add D$EditWindowX eax

  ; Bp Margin:
    call 'USER32.MoveWindow' D$BpWindowHandle,
                             0, D$EditWindowY, D$BpMarginWidth, D$EditWindowH,
                             &TRUE
ret


RedrawInterface:
  ; Use of 'ScrollBarProc' for killing the various windows because this Proc.
  ; (As MainWindowProc would close RosAsm)

  ; Edit:
    call 'USER32.SetWindowLongA' D$EditWindowHandle, &GWL_WNDPROC, ScrollBarProc
    call 'USER32.DestroyWindow' D$EditWindowHandle

  ; ScrollBar
    If D$ScrollWindowHandle <> 0
        call 'USER32.DestroyWindow' D$ScrollWindowHandle
        mov D$ScrollWindowHandle 0
    End_If

  ; ToolBar:
    If D$ToolBarHandle <> 0
        call 'USER32.SetWindowLongA' D$ToolBarHandle, &GWL_WNDPROC, ScrollBarProc
        call 'USER32.DestroyWindow' D$ToolBarHandle
        mov D$ToolBarHandle 0
    End_If

  ; StatusBar:
    call 'USER32.SetWindowLongA' D$StatusbarHandle, &GWL_WNDPROC, ScrollBarProc
    call 'USER32.DestroyWindow' D$StatusbarHandle

  ; BpWindow
    call 'USER32.SetWindowLongA' D$BpWindowHandle, &GWL_WNDPROC, ScrollBarProc
    call 'USER32.DestroyWindow' D$BpWindowHandle

    call CreateStatusBar
    On B$ToolBarWanted = &TRUE,  call CreateToolBar
    On B$ScrollBarWanted = &TRUE, call CreateScrollBar
    call CreateEditWindow
ret












