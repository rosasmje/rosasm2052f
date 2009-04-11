TITLE MainProc

 ______________________________________________________________________________________
 ______________________________________________________________________________________

; Equates given by the Menu Editor:

; Tag Menu 1000

[M00_Menu  1000                  M00_Open  1001                  M00_New  1002
 M00_New_Model  1003             M00_Paste_at_Pos  1004          M00_Change_Compile_Name  1005
 M00_Open_Source_Only  1006      M00_Replace_Source_Only  1007   M00_Save_Source_Only  1008
 M00_Print  1009                 M00_Output  1010                M00_Exit  1011
 M00_Tree  1012                  M00_Import  1013                M00_Export  1014
 M00_Find  1015                  M00_Replace  1016               M00_Undo  1017
 M00_Redo  1018                  M00_Copy  1019                  M00_Paste  1020
 M00_Delete  1021                M00_Cut  1022                   M00_Compile  1023
 M00_Run  1024                   M00_Optimize_Jumps_Sizes  1025  M00_Calc  1026
 M00_Ascii_Table  1027           M00_Show_RosAsm_Mems  1028      M00_Show_Symbols_Repartition  1029
 M00_Local_mem_Tests  1030       M00_Serial_Compilations  1031   M00_Data_to_Equates  1032
 M00_Encoding  1033              M00_DLLs_Scanner  1034          M00_Libs_Scanner  1035
 M00_Configuration  1036         M00_Create_Config_bin  1037     M00_Main_Icon  1038
 M00_Load_Icon  1039             M00_Delete_Icon  1040           M00_Icon_IDs  1041
 M00_Save_Icon  1042             M00_Load_BitMap  1043           M00_Delete_BitMap  1044
 M00_BitMaps_IDs  1045           M00_Save_BitMap  1046           M00_Load_Cursor  1047
 M00_Delete_Cursor  1048         M00_Cursors_IDs  1049           M00_Save_Cursor  1050
 M00_Load_Wave  1051             M00_Delete_Wave  1052           M00_Waves_IDs  1053
 M00_Save_Wave  1054             M00_Load_Avi  1055              M00_Delete_Avi  1056
 M00_Avi_IDs  1057               M00_Save_Avi  1058              M00_Load_RC  1059
 M00_Delete_RC  1060             M00_RCs_IDs  1061               M00_Save_RC  1062
 M00_New_Dialog  1063            M00_Load_from_Resources  1064   M00_Load_from_ClipBoard  1065
 M00_Load_from_File  1066        M00_Save_to_Binary_File  1067   M00_Load_from_Binary_File  1068
 M00_Replace_from_Binary_File  1069                              M00_Delete_Resources_Dialog  1070
 M00_Strings  1071               M00_New_Menu  1072              M00_Existing_Menu  1073
 M00_Delete_a_Menu  1074         M00_Save_to_Binary_Menu_File  1075
 M00_Load_Binary_Menu_File  1076 M00_Replace_from_Binary_Menu_File  1077
 M00_Clip_File  1078             M00_Structures  1079            M00_Api_Functions  1080
 M00_Sys_Resources  1081         M00_GUIDs  1082                 M00_Win32_Equates  1083
 M00_Win32_Data_Types  1084      M00_Wizards  1085               M00_B_U_Asm  1086
 M00_Sources_Editor  1087        M00_Visual_Tuts  1088           M00_Win32_hlp  1089
 M00_Mmedia_hlp  1090            M00_OpenGl_hlp  1091            M00_WinSock_hlp  1092
 M00_Dx_hlp  1093                M00_SDL  1094                   M00_sqlite  1095
 M00_DevIl  1096                 M00_About  1097                 M00_GPL_License  1098
 M00_RosAsm_License  1099        M00_<<<<  1100                  M00_>>>>  1101]

[M00_About_ToolBar 1200]
____________________________________________________________________________________________

[redrawFlag: 0, ReadyToRun: 0, ShowStats: &TRUE, UnusedSymbolsDialogWanted: &FALSE]

[NotReadyToRun L7>>    NoClient L8>>]

[CloseDebuggerOrIgnore
    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If]

; buffer for returned filename:
[lpszFile: B$ 0 #&MAXPATH]

; size of buffer for filename:
[cch: D$ &MAXPATH]

Proc MainWindowProc:
;;
    At the attention of all RosAsm contributors ---> 'Rules'
    
  'CreateTitleTab'
    
  'NewReplaceMacAndEqu', 'GetFileNameFromPath'
  
  'CheckMRUFile', 'RightClick', 'WheelMsg'
  'ShowUnfoldMacro', 'UnfoldMacro', 'ShowUnfoldDialog'
  'StructDialog'; 'NewFileNameDialog'
  'EncodeLines', 'StoreFlatData', 'UpdateTitlesFromIncludeFiles'
  
  'Main', 'AsmMain, 'checksum64' 'OutOnError'
  
  'NewReplaceMacAndEqu', 'zReplaceEquates'
  
  'KillTrailingSpaces', 'NewFileNameDialog', 'DataToStructureProc'
;;

    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    If D@Message = &WM_CREATE
        call 'SHELL32.DragAcceptFiles' D@Adressee, &TRUE | jmp L9>>

    Else_If D@Message = &WM_DROPFILES
        call 'SHELL32.DragQueryFile' D@wParam, 0, lpszFile, D$cch
        mov esi lpszFile, edi SaveFilter
        while B$esi <> 0 | movsb | End_While | movsb
        call DirectLoad
        call StartEdition
        call ReInitUndo
        call SetPartialEditionFromPos
        call EnableMenutems
        call LoadBookMarks
        call AskForRedraw
    End_If

    mov eax D@Adressee

    .If eax = D$hwnd
        ; OK
    .Else_If eax = D$EditWindowHandle
        mov eax D$wc_hCursor
        If D$ActualCursor <> eax
            mov D$ActualCursor eax
            call 'USER32.SetClassLongA' D$EditWindowHandle, &GCL_HCURSOR, eax
        End_If
    .Else_If eax = D$ScrollWindowHandle
        ; OK
    .Else_If eax = D$BpWindowHandle
        mov eax D$Bp_hCursor
        If D$ActualCursor <> eax
            mov D$ActualCursor eax
            call 'USER32.SetClassLongA' D$BpWindowHandle, &GCL_HCURSOR, eax
        End_If
    .Else
        jmp NoClient
    .End_If
  ; Yes, this may happend when [Run]ing. And this works because we do not hold anything
  ; at creation time.
 ____________________________________


; General purpose Callback:

    mov eax D@Message

    ...If eax = &WM_KEYDOWN
        .If B$SourceReady = &TRUE
            movzx eax B@Wparam | mov B$Keys+eax 1

            call KeyMessage ; CharMessage

            On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
            If B$KeyHasModifedSource = &TRUE

                mov B$SourceHasChanged &TRUE
                call AskForRedraw |
              jmp NotReadyToRun
           ; Else_If B$KeyHasMovedCaret = &TRUE
           ;     call AskForRedraw
            End_If

        .Else_If D@wParam = &VK_F1
            call RosAsmHelp

        .Else_If D@wParam = &VK_F2
            call F2Help

        .End_If

    ...Else_If eax = &WM_KEYUP
        movzx eax B@Wparam | mov B$Keys+eax 0
        On eax = &VK_MENU, mov B$keys+&VK_CONTROL &FALSE

    ...Else_If eax = &WM_CLOSE
        CloseDebuggerOrIgnore

        On B$OnDialogEdition = &TRUE, call CloseDialogEdition

        call Security | On eax = &IDCANCEL, jmp L9>>

        If B$SaveMainPosFlag = &TRUE
            call 'USER32.ShowWindow' D$hwnd, &SW_RESTORE
            call 'USER32.GetWindowRect' D$hwnd, WindowX
            mov eax D$WindowX | sub D$WindowW eax
            mov eax D$WindowY | sub D$WindowH eax
            call UpdateRegistry
        End_If

        On B$ToolBarChange = &TRUE, call SaveToolBar

        call LeftButtonUp                            ; ensure no confined mouse
        call CloseHelp

        On D$EditedDialogHandle <> 0, call 'User32.DestroyWindow' D$EditedDialogHandle
        On D$DialogEditorHandle <> 0 call 'User32.EndDialog' D$DialogEditorHandle

        call 'USER32.DestroyWindow' D$hwnd | jmp L9>> ; works too with 'jmp NoClient'

    ...Else_If eax = &WM_GETMINMAXINFO
        mov eax D@lParam
        mov D$eax+24 500, D$eax+28 300
        popad | mov eax &FALSE | ExitP

    ...Else_If eax = &WM_SIZE
        On D$TitleWindowHandle > 0, call KillTitleTab

        call MainResize

    ...Else_If eax = &WM_MOVING
        On D$TitleWindowHandle <> 0, call KillTitleTab

    ...Else_If eax = &WM_DESTROY
        On B$OnDialogEdition = &TRUE, call CloseDialogEdition
        call DeleteUndoFiles | call KillUndo
        call ReleaseResourceMemory
        call 'User32.PostQuitMessage' 0 | jmp L9>>

    ...Else_If D@Message = &WM_NOTIFY
        call KillCompletionList
      ; ebx > NMHDR // eax > hwndFrom.
        mov ebx D@lParam, eax D$ebx
      ; ToolBar Messages:
        .If D$ebx+TOOLTIPTEXT_NMHDR_code = &TTN_NEEDTEXT
            mov eax D$ebx+TOOLTIPTEXT_NMHDR_idfrom
          ; Pointing with esi to the Buttons List IDs:
            lea esi D$ToolBarButtons+4
            mov ecx 0
            While D$esi <> eax
                add esi 20 | inc ecx | On ecx > TOOLBUTTONS_NUMBER, ExitP
            End_While
            mov eax D$PointersToToolTipsStrings+ecx*4
            mov D$ebx+TOOLTIPTEXT_lpszText eax

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_QUERYINSERT ; May be inserted ?
            popad | mov eax &TRUE | ExitP                   ; > yes for all.

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_QUERYDELETE ; May be deleted?
                        popad | mov eax &TRUE | ExitP                   ; > yes for all.

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_FIRST  ; = &TBN_GETBUTTONINFO (missing?)
            mov ecx D$ebx+TB_NOTIFY_Item, edx ecx
            If ecx a TOOLBUTTONS_NUMBER
                popad | mov eax &FALSE | ExitP
            End_If
            lea ecx D$ecx*4+ecx | shl ecx 2                 ; ecx = ecx * 20 >>>
            add ecx ToolBarButtons                          ; Pointer to whished Button Data
            move D$ebx+TB_NOTIFY_TBBUTTON_iBitmap D$ecx
            move D$ebx+TB_NOTIFY_TBBUTTON_idCommand  D$ecx+4
            move D$ebx+TB_NOTIFY_TBBUTTON_fsState D$ecx+8
            move D$ebx+TB_NOTIFY_TBBUTTON_dwData D$ecx+12
            move D$ebx+TB_NOTIFY_TBBUTTON_iString D$ecx+16
            mov edi D$ebx+TB_NOTIFY_TextPtr
            mov edx D$ebx+TB_NOTIFY_Item | shl edx 2        ; Displacement to pointers
            mov eax PointersToToolTipsStrings | add eax edx ; Pointer
            mov esi D$eax                                   ; Source
            mov ecx 0-1                                     ; Counter for Non-zero-ended text
            Do
                lodsb | stosb | inc ecx
            Loop_Until al = 0
            mov D$ebx+TB_NOTIFY_CharCount ecx               ; Lenght of String    (+36)
            popad | mov eax &TRUE | ExitP

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_TOOLBARCHANGE
            mov B$ToolBarChange &TRUE                       ; For Saving at Exit if TRUE.

            ; Next instruction of no use usually, but, under some unknown circumstances
            ; the Buttons Size may be changed (Win98 without IE). So, as it can't hurt:
            call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SETBUTTONSIZE, 0, 014_0014

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_RESET
            call 'USER32.SendMessageA' D$ToolBarHandle, &TB_SAVERESTORE, &FALSE,
                                    TOOLBAR_REGISTRY        ; FALSE > restore

        .Else_If D$ebx+TOOLTIPTEXT_NMHDR_code = &TBN_CUSTHELP
            call 'USER32.MessageBoxA' D$hwnd, HelpToolBar, HelpToolBarTitle,
                                    &MB_OK__&MB_SYSTEMMODAL
        .End_If


 ____________________________________

; Main window CallBack: User choices open-file independant:

    ...Else_If eax = &WM_COMMAND
        call KillCompletionList

        mov eax D@wParam | and eax 0FFFF

        If B$IncludesOK = &FALSE
            call Configuration | call 'User32.SendMessageA' D$hwnd &WM_CLOSE 0 0
        End_If

        ..If eax = M00_Open
            CloseDebuggerOrIgnore

            call Security | On eax = &IDCANCEL, jmp L9>>

            call OpenRosAsmPE

            call UpdateTitlesFromIncludeFiles

            If D$SaveFilter <> 0
                call ReInitUndo
                call SetPartialEditionFromPos | call EnableMenutems
                call LoadBookMarks
            End_If

        ..Else_If eax = M00_Open_Source_Only
            CloseDebuggerOrIgnore

            call Security | On eax = &IDCANCEL, jmp L9>>

            call LooseResources
            .If B$KeepResources = &FALSE
                call ReInitUndo | call OpenSourceOnly
                call UpdateTitlesFromIncludeFiles
                If D$SourceLen > 0
                    call SetPartialEditionFromPos | call EnableMenutems
                    call LoadBookMarks
                End_If
            .End_If

        ..Else_If eax = M00_Exit
            call 'User32.SendMessageA' D$hwnd &WM_CLOSE 0 0

        ..Else_If eax = M00_Sources_Editor
            call Help, B_U_AsmName, SourceEditor, ContextHlpMessage

        ..Else_If eax = M00_Main_Icon
            CloseDebuggerOrIgnore
            call IconEdition | jmp NotReadyToRun

        ..Else_If eax = M00_New_Menu
            CloseDebuggerOrIgnore
            call NewMenu | mov D$ActualMenutestID 0 | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Existing_Menu
            CloseDebuggerOrIgnore
            call ExistingMenu | mov D$ActualMenutestID 0 | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_a_Menu
            CloseDebuggerOrIgnore
            call DeleteMenu | mov D$ActualMenutestID 0 | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Save_to_Binary_Menu_File
            CloseDebuggerOrIgnore
            call SaveMenuBinaryFile

        ..Else_If eax = M00_Load_Binary_Menu_File
            CloseDebuggerOrIgnore
            call LoadMenuBinaryFile | jmp NotReadyToRun

        ..Else_If eax = M00_Replace_from_Binary_Menu_File
            CloseDebuggerOrIgnore
            call ReplaceMenuBinaryFile | jmp NotReadyToRun

        ..Else_If eax = M00_About | call AboutBox

        ..Else_If eax = M00_About_ToolBar | call About_ToolBar

        ..Else_If eax = M00_GPL_License | call GPLView

        ..Else_If eax = M00_RosAsm_License | call LicenseView

        ..Else_If eax = M00_B_U_Asm | call RosAsmHelp

        ..Else_If eax = M00_Configuration | call Configuration

        ..Else_If eax = M00_Create_Config_bin | call Create_Config_bin

        ..Else_If eax = M00_Api_Functions | call ViewApiList

        ..Else_If eax = M00_New
            CloseDebuggerOrIgnore
            call Security | On eax = &IDCANCEL, jmp L9>>
            call AutoNew
            and D$RelocsWanted 0 ; jE!
            call ChangeName
            cmp D$SavingExtension '.DLL' | setz B$RelocsWanted ; jE! Relocs for Dll
            call StartNewFile

        ..Else_If eax = M00_New_Model
            CloseDebuggerOrIgnore
            call Security | On eax = &IDCANCEL, jmp L9>>
            call NewFileNameDialog

        ..Else_If eax = M00_Change_Compile_Name | call ChangeName

        ..Else_If eax = M00_Calc | call Calc

        ..Else_If eax = M00_DLLs_Scanner | call ExportScanner

        ..Else_If eax = M00_Libs_Scanner | call LibScanner

        ..Else_If eax = M00_New_Dialog
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                mov B$SameIdAllowed &FALSE
                On B$BlinkingCaretWanted = &TRUE, call KillBlinkCursor
                    call InitDialogEdition | call ReleaseDialogMemories
                On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
                call EnableMenutems | jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_ClipBoard
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                mov B$SameIdAllowed &FALSE
                On B$BlinkingCaretWanted = &TRUE, call KillBlinkCursor
                    call ReleaseDialogMemories | call LoadDialogFromClipBoard
                On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
                call EnableMenutems | jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_File
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                mov B$SameIdAllowed &FALSE
                On B$BlinkingCaretWanted = &TRUE, call KillBlinkCursor
                    call ReleaseDialogMemories | call LoadDialogFromFile
                On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
                call EnableMenutems | jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Load_from_Resources
            CloseDebuggerOrIgnore
            If B$OnDialogEdition = &FALSE
                mov B$SameIdAllowed &TRUE
                On B$BlinkingCaretWanted = &TRUE, call KillBlinkCursor
                    call ReleaseDialogMemories | call LoadFromResources
                On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
                jmp NotReadyToRun
            End_If

        ..Else_If eax = M00_Save_to_Binary_File
            CloseDebuggerOrIgnore
            call SaveToBinaryFile

        ..Else_If eax = M00_Load_from_Binary_File
            CloseDebuggerOrIgnore
            mov B$SameIdAllowed &TRUE
            call LoadFromBinaryFile | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Replace_from_Binary_File
            CloseDebuggerOrIgnore
            call ReplaceFromBinaryFile | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Resources_Dialog
            CloseDebuggerOrIgnore
            On B$BlinkingCaretWanted = &TRUE, call KillBlinkCursor
                call DeleteDialog | call EnableMenutems
            On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
            jmp NotReadyToRun

        ..Else_If eax = M00_Load_BitMap
            CloseDebuggerOrIgnore
            call LoadBitMap | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_BitMap
            CloseDebuggerOrIgnore
            call DeleteBitMap | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_BitMaps_IDs | call ShowBitMapsIds

        ..Else_If eax = M00_Load_Wave
            CloseDebuggerOrIgnore
            call ReadWaveFile | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Wave
            CloseDebuggerOrIgnore
            call DeleteWave | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Load_Avi
            CloseDebuggerOrIgnore
            call ReadAviFile | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Avi
            CloseDebuggerOrIgnore
            call DeleteAviFile | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Load_RC
            CloseDebuggerOrIgnore
            call ReadRcData | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Save_RC
            CloseDebuggerOrIgnore
            call SaveRcData | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_RC
            CloseDebuggerOrIgnore
            call DeleteRcData | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Ascii_Table | call AsciiTable

        ..Else_If eax = M00_Win32_hlp | call Win32_Hlp

        ..Else_If eax = M00_Mmedia_hlp | call Mmedia_Hlp

        ..Else_If eax = M00_OpenGl_hlp | call OpenGl_Hlp

        ..Else_If eax = M00_WinSock_hlp | call WinSock_Hlp

        ..Else_If eax = M00_Dx_hlp | call Dx_Hlp

        ..Else_If eax = M00_SDL | call SDL_hlp

        ..Else_If eax = M00_sqlite | call sqlite_Hlp

        ..Else_If eax = M00_DevIl | call DevIL_Hlp

        ..Else_If eax = M00_Win32_Data_Types | call ShowTypes

        ..Else_If eax = M00_Win32_Equates | call ShowEquates

        ..Else_If eax = M00_Strings
            CloseDebuggerOrIgnore
            call StringsResources | jmp NotReadyToRun

        ..Else_If eax = M00_Load_Cursor
            CloseDebuggerOrIgnore
            call ReadCursor | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Cursor
            CloseDebuggerOrIgnore
            call DeleteCursor | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Load_Icon
            CloseDebuggerOrIgnore
            call ReadIcon | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Delete_Icon
            CloseDebuggerOrIgnore
            call DeleteIcon | call EnableMenutems | jmp NotReadyToRun

        ..Else_If eax = M00_Clip_File | call Templates
           ; call SetQwordCheckSum String2

        ..Else_If eax = M00_Data_to_Equates | call DataToStructure

        ..Else_If eax = M00_Structures | call StructDialog

        ..Else_If eax = M00_Sys_Resources | call ViewSysResources

        ..Else_If eax = M00_GUIDs | call ViewGUIDs

        ..Else_If eax = M00_Show_RosAsm_Mems | call ViewRosAsmMems

        ..Else_If eax = M00_Show_Symbols_Repartition | call TestRepartition

        ..Else_If eax = M00_Local_mem_Tests | call TestLocals

        ..Else_If eax = M00_Serial_Compilations | call MultipleCompileTests

        ..Else_If eax = M00_Encoding
            CloseDebuggerOrIgnore

            call ViewEncoding

            jmp NotReadyToRun

        ..Else
            .If eax > 6999                      ; Clip Files
                call Templates

            .Else_If eax > 5999                  ; Wizards
                CloseDebuggerOrIgnore

                On B$SourceReady = &FALSE,
                    call 'USER32.SendMessageA', D$hwnd, &WM_COMMAND, M00_New, 0

                On B$SourceReady = &TRUE, call NewWizardForm

            .Else_If eax > 4999                  ; Visual Tuts
                If eax < 5100
                   call VisualTuts
                Else
                    jmp L5>>
                End_If

            .Else_If eax > 3999             ; Added FloatMenu under [Struct].
                If eax < 4010
                    mov D$MenuID eax | call StructDialog ; OpenStructureFile
                Else
                    jmp L5>>
                End_If

            .Else_If eax > 3000             ; Most Recently Used Files.
                If eax < 3005
                    CloseDebuggerOrIgnore
                    push eax
                        call Security       ; 'Security' also uses eax as return Value.
                    pop ebx
                    On eax = &IDCANCEL, jmp L9>>
                        mov eax ebx         ; ... and 'LoadRMUfile' as input.
                        call LoadRMUfile
                        On eax = &FALSE, jmp L5>
                        call SetPartialEditionFromPos
                        call LoadBookMarks | call EnableMenutems | call LoadBookMarks
                Else
                    jmp L5>
                End_If
            .Else_If eax > 1999             ; User defined menu:
                If eax < 2009
                    mov eax D@wParam | call UserDefinedAction
                Else
                    jmp L5>
                End_If
            .Else
                jmp L5>
            .End_If
        ..End_if
        jmp L9>>

    ...End_If

L5: If B$SourceReady = &FALSE
        On D@Message = &WM_PAINT, call SplashScreen
        jmp NoClient
    End_If
 __________________________________________

 ; Main window CallBack going on: With opened file only:

    mov eax D@Message

    ...If eax = &WM_PAINT
        .If D$CodeSource > 0
            call TextPos
            mov eax D@Adressee
            If eax = D$EditWindowHandle
                call PrintColorText
            Else_If eax = D$BpWindowHandle
                call PrintBp
            End_If

            If B$MovingBlock = &TRUE
                call SetBlock | call AskForRedraw
            End_If

            call StatusBar

            mov B$RedrawFlag &FALSE
        .End_If

        mov eax D$EditWindowHandle | On D@Adressee <> eax, jmp NoClient

    ...Else_If eax = &WM_CHAR
        ..If B$RedrawFlag = &FALSE
            mov B$RedrawFlag &TRUE

            call CharMessage D@wParam | or B$SourceHasChanged al

            call AskForRedraw | jmp NotReadyToRun
        ..End_If
;;
   Note from Betov: This 'WM_IME_CHAR' case and according Routine have been implemented
   by Liang Xianning. I am pretty sure that holding unicode oriental KeyBoards input
   can *not* be done this way for many reasons (what if one byte inside the unicode
   string is equal to Quote, Double-Quote,... and other critical Bytes values? What of
   BackSpace upon such a String, and so on. Also, all of the innumerous RosAsm Routines
   that perform any action on Source Text should be re-written too... What i would
   clearly refuse 'for many reasons...' too).

   The only solution i can imagine for inputing Oriental Unicode Strings would be to
   write an external little tool (a simple EditControl + a simple Routine to save the
   String Data in U$ RosAsm Format -in hexaDecimal Numbers- to be pasted in the Source
   Data through the ClipBoard. Very easy to do, but i can't do it. (I can't see any
   Oriental Char on my PC...).

   Another thing is that fully holding Oriental Char *inside* an Asm Source Editor
   does not make much sense, as the source would then be turned of no use for all other
   programmers. Do i write in french??? Oh yes,... Sometimes, when i am ashamed... :))
;;
    ...Else_If eax = &WM_IME_CHAR
        .If B$RedrawFlag = &FALSE
            CloseDebuggerOrIgnore

            mov B$RedrawFlag &TRUE
            mov eax D@Wparam | call InsertDoubleByte | call AskForRedraw
            jmp NotReadyToRun
        .End_If

    ...Else_If eax = &WM_MOUSEWHEEL
        mov eax D@wParam | call WheelMsg | call KillCompletionList

    ...Else_If eax = &WM_XBUTTONDOWN
        ..If W@Wparam+2 = &XBUTTON1
            mov B$BlockInside &FALSE
            call RestoreRealSource | call BackClick | call SetPartialEditionFromPos

        ..Else_If W@Wparam+2 = &XBUTTON2
            mov B$BlockInside &FALSE
            call RestoreRealSource | call ForwardClick | call SetPartialEditionFromPos
        ..End_If

        popad | mov eax &TRUE | ExitP

    ...Else_If eax = &WM_COMMAND
        mov eax D@Wparam | and eax 0FFFF

        ..If eax = M00_<<<<_
            mov B$BlockInside &FALSE
            call RestoreRealSource | call BackClick | call SetPartialEditionFromPos

        ..Else_If eax = M00_>>>>_
            mov B$BlockInside &FALSE
            call RestoreRealSource | call ForwardClick | call SetPartialEditionFromPos

        ..Else_If eax = M00_Compile
            mov D$ShowStats &TRUE
            call Compile

        ..Else_If eax = M00_Run
            call Run

        ..Else_If eax = M00_Optimize_Jumps_Sizes
            call Optimize

       ; ..Else_If eax = M00_Profile | call Profiler

        ..Else_If eax = M00_Paste_at_Pos
            On D$IsDebugging = &TRUE, jmp L9>>
            call RestoreRealSource | call IncludeSource | call SetPartialEditionFromPos
            mov B$SourceHasChanged &TRUE, B$FirstBlockDraw &FALSE
            call SetCaret D$CurrentWritingPos

        ..Else_If eax = M00_Find
            mov D$NextSearchPos 0 | call SetSimpleSearchBox

        ..Else_If eax = M00_Replace
            mov D$NextSearchPos 0 | On D$IsDebugging = &FALSE, call SetFindReplaceBox

        ..Else_If eax = M00_Undo
            call ControlZ

        ..Else_If eax = M00_Redo
            call ControlShiftZ

        ..Else_If eax = M00_Copy
            call ControlC

        ..Else_If eax = M00_Paste
            call ControlV | call AskForRedraw

        ..Else_If eax = M00_Delete
            call ControlD | call AskForRedraw

        ..Else_If eax = M00_Cut
            call ControlX | call AskForRedraw

        ..Else_If eax = M00_Save_Source_only
            call ControlS

        ..Else_If eax = M00_Replace_Source_Only
            On D$IsDebugging = &TRUE, jmp L9>>
            call ReInitUndoOnly
            call ReplaceSourceOnly
            call UpdateTitlesFromIncludeFiles
            On D$SourceLen > 0, call SetPartialEditionFromPos

        ..Else_If eax = M00_Tree
            call CreateTreeViewList | call SetTreeDialogPos

        ..Else_If eax = M00_Import
           call ShowSourceImports

        ..Else_If eax = M00_Export
           call ShowSourceExports

        ..Else_If eax = M00_Print | call Print

        ..Else_If eax = Float_Copy
            call CopyFromFloatMenu | call AskForRedraw
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Delete
            call ControlX | call AskForRedraw
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Replace
            call ControlD | call ControlV | call AskForRedraw
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SearchUp
            call RestoreRealSource
            call StorePosInBackTable | call SearchUpFromFloatMenu
            call SetPartialEditionFromPos
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SearchDown
            call RestoreRealSource
            call StorePosInBackTable | call SearchDownFromFloatMenu
            call SetPartialEditionFromPos
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SearchFromTop
            call RestoreRealSource
            call StorePosInBackTable | call SearchFromTopFromFloatMenu
            call SetPartialEditionFromPos
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Unfold
            call 'USER32.DestroyMenu' D$FloatHandle
            call RestoreRealSource | call ShowUnfoldMacro | call SetPartialEditionFromPos

        ..Else_If eax = Float_BookMark | call StoreBookMark
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_UnBookMark | call DeleteBookMark
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_Number | call ViewClickedNumber
            call 'USER32.DestroyMenu' D$FloatHandle

        ..Else_If eax = Float_SelReplace
            call 'USER32.DestroyMenu' D$FloatHandle | call BlockReplaceAll

        ..Else_If eax = FloatSetBp
            call 'USER32.DestroyMenu' D$FloatHandle
            call SetBreakPoint | call DoStoreBP | jmp NotReadyToRun

        ..Else_If eax = FloatDelBp
            call 'USER32.DestroyMenu' D$FloatHandle
            call DeleteBreakPoint | call DoStoreRemoveBP | jmp NotReadyToRun

        ..Else_If eax = FloatDelAllBp
            call 'USER32.DestroyMenu' D$FloatHandle
            call DeleteAllBreakpoints | jmp NotReadyToRun

        ..Else_If eax = Float_BadDisLabel
            call 'USER32.DestroyMenu' D$FloatHandle
            call ForcedFlags

        ..Else_If eax = M00_Output
            call OutputFormat | call EnableMenutems | jmp NotReadyToRun

        ..End_If

    ...Else_If eax = &WM_SETCURSOR
        mov eax D@wParam
        .If eax = D$StatusBarHandle
            If D$TitleWindowHandle = 0
                call ShowTitles
            End_If
        .End_If

        jmp NoClient

    ...Else_If eax = &WM_MOUSEMOVE
        .If D$TitleWindowHandle > 0
            mov ebx D$TabWindowY
            On W@lParam+2 < bx, call KillTitleTab
        .End_If

        If D@Wparam <> &MK_LBUTTON                          ; Is the Left Button Down?
            call 'USER32.ClipCursor' &NULL | jmp NoClient
        End_If

        On B$UserHaveClickDown = &FALSE, jmp NoClient
            On B$UserClickAfterEnd = &TRUE, jmp NoClient
                push D@Lparam | pop W$MousePosX, W$MousePosY

                call SetBlock

                If B$FirstBlockDraw = &FALSE
                    On B$BlockRedraw = &TRUE, call AskForRedrawNow
                End_If

    ...Else_If eax = &WM_LBUTTONDOWN
        mov B$ShiftBlockInside &FALSE

        call KillCompletionList

        On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
        push D@Lparam | pop W$MousePosX, W$MousePosY

        mov B$BlockInside &FALSE, eax D@Adressee | call LeftButton

        call AskForRedrawNow
        mov D$NextSearchPos 0, B$UserHaveClickDown &TRUE

    ...Else_If eax = &WM_LBUTTONUP
        On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
        On B$FirstBlockDraw = &TRUE, mov B$BlockInside &FALSE
        call LeftButtonUp | mov B$UserHaveClickDown &FALSE | call AskForRedrawNow

    ...Else_If eax = &WM_RBUTTONUP
        call KillCompletionList
        On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor

        mov eax D@Adressee
        .If eax = D$BpWindowHandle
            If D$DBPMenuOn = RIGHT_CLICK_ACTION
                mov B$ShiftBlockInside &FALSE

                call KillCompletionList

                On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
                call RestoreRealSource
                    push D@Lparam | pop W$MousePosX, W$MousePosY
                    call MarginRightClick
                call SetPartialEditionFromPos
            End_If

            jmp NoClient
        .End_If

        call RestoreRealSource
            push D@Lparam | pop W$MousePosX, W$MousePosY
            call RightClick
        call SetPartialEditionFromPos

    ...Else_If eax = &WM_LBUTTONDBLCLK
        mov B$ShiftBlockInside &FALSE

        call KillCompletionList

        On B$BlinkingCaretWanted = &TRUE, call ResetBlinkCursor
        call RestoreRealSource
            push D@Lparam | pop W$MousePosX, W$MousePosY
            call DoubleClick
        call SetPartialEditionFromPos

    ...Else_If eax = D$FindStringMessage
        call KillCompletionList

        call StringSearch | call AskForRedraw

    ...Else
        jmp NoClient

    ...End_If
 ____________________________________

L9: popad | mov eax &FALSE | jmp P9>>

L7: ; NotReadyToRun:
    .If B$ReadyToRun = &TRUE         ; (Something has just been modified).
        mov B$ReadyToRun &FALSE
        VirtualFree D$IpTable, D$StatementsTable, D$StatementsTable2
    .End_If

    mov B$SourceHasChanged &TRUE

    popad | mov eax &FALSE | jmp P9>

L8: ; NoClient:
    popad | call 'User32.DefWindowProcA' D@Adressee D@Message D@wParam D@lParam
EndP

____________________________________________________________________________________________

Compile:
        If D$UnusedCodeAndDataDialogHandle = &FALSE
            On B$Compiling = &TRUE, ret
            On D$IsDebugging = &TRUE, ret
        Else
            mov D$ShowStats &FALSE
        End_If

        mov B$RecompileWanted &FALSE, B$Compiling &TRUE
        call RestoreRealSource

        On D$UnusedCodeAndDataDialogHandle <> 0, call ReInitUnusedDialog

        call AsmMain | mov D$OldStackPointer 0

        If B$CompileErrorHappend = &FALSE
            mov B$ReadyToRun &TRUE, B$SourceHasChanged &FALSE
        End_If

        call SetPartialEditionFromPos
        call AskForRedraw

        mov B$Compiling &FALSE | call ResetKeys
ret


Run:
    If D$UnusedCodeAndDataDialogHandle <> &FALSE
      ; 'UnusedCodeAndDataDialogCallBack'
        call 'USER32.EndDialog' D$UnusedCodeAndDataDialogHandle, 0
        mov D$UnusedCodeAndDataDialogHandle 0
        mov B$Compiling &FALSE, B$UnusedSymbolsDialogWanted &FALSE, B$ShowStats &FALSE
        mov B$UnusedSymbolsDialogWanted &FALSE
    End_If

    On B$Compiling = &TRUE, ret
    On D$IsDebugging = &TRUE, ret

    mov B$ShowStats &FALSE, B$Compiling &TRUE, B$RecompileWanted &FALSE

    call RestoreRealSource

    If B$ReadyToRun = &FALSE
        mov B$ShowStats &FALSE
        call AsmMain
        mov D$OldStackPointer 0
        mov D$UnusedSymbolsDialogWanted &FALSE

        On B$CompileErrorHappend = &FALSE, mov B$ReadyToRun &TRUE
    End_If

    call SetPartialEditionFromPos

    If B$CompileErrorHappend = &FALSE
        mov B$SourceHasChanged &FALSE
        call Debugger
    End_If

    mov B$Compiling &FALSE | call ResetKeys
ret


Optimize:
    mov B$AlignFound &FALSE

    If D$UnusedCodeAndDataDialogHandle <> &FALSE
      ; 'UnusedCodeAndDataDialogCallBack'
        call 'USER32.EndDialog' D$UnusedCodeAndDataDialogHandle, 0
        mov D$UnusedCodeAndDataDialogHandle 0
        mov B$Compiling &FALSE, B$UnusedSymbolsDialogWanted &FALSE, B$ShowStats &FALSE
    End_If

    If B$ReadyToRun = &FALSE
        mov D$ShowStats &FALSE
        call Compile
    End_If

    .If B$ReadyToRun = &TRUE
        If B$AlignFound = &TRUE
            call 'USER32.MessageBoxA' &NULL, NoptimizeMessage,
                                      NoptimizeTitle, &MB_SYSTEMMODAL
        Else
            mov B$ShortenJumpsWanted &TRUE
            mov D$ShowStats &TRUE
            call Compile
            mov B$ShortenJumpsWanted &FALSE
        End_If
    .End_If
ret

[NoptimizeTitle: 'Optimizer', 0
 NoptimizeMessage: "The Assembler can not yet optimize the Jump Sizes  
 on a Source making use of the Align Statement:
 
 The Alignments would be broken.", 0]
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc ScrollBarProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    On B$SourceReady = &FALSE, jmp L8>>

    pushad

    ...If D@Message = &WM_VSCROLL
        call KillCompletionList

        call 'USER32.IsMenu' D$FloatHandle
        ..If eax = &FALSE
            .If W@wParam = &SB_THUMBTRACK
                If D$TotalNumberOfLines < 0FFFF
                    movzx edx W@wParam+2
                Else
                    call 'USER32.GetScrollInfo' D$ScrollWindowHandle, &SB_VERT, VScroll
                    mov edx D$VScroll.nTrackPos
                End_If
                call RePosFromScroll

            .Else_If W@wParam = &SB_LINEDOWN    ; SB > ScrollBar.
                call DownOneLine

            .Else_If W@wParam = &SB_LINEUP
                call UpOneLine

            .Else_If W@wParam = &SB_PAGEDOWN
                mov ecx D$LineNumber
L4:             push ecx | call DownOneLine | pop ecx | loop L4<

            .Else_If W@wParam = &SB_PAGEUP
                mov ecx D$LineNumber
L0:             call UpOneLine | loop L0<

            .End_If
            call AskForRedraw
        ..End_If

        popad | mov eax &FALSE | ExitP

     ...End_If

     On D$TitleWindowHandle > 0, call KillTitleTab

    popad
L8: call 'User32.DefWindowProcA' D@Adressee D@Message D@wParam D@lParam
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

[QuitTitle: 'Sure?', 0
QuitMessage: 'Source has changed. Save/Compile now?', 0]

Security:
    mov eax &IDNO

    On D$CodeSource = 0, ret

    ...If B$SecurityWanted = &TRUE
        ..If B$SourceHasChanged = &TRUE
            call 'MessageBoxA' D$hwnd, QuitMessage, QuitTitle, &MB_YESNOCANCEL
            .If eax = &IDYES
                call RestoreRealSource
                call AsmMain | mov D$OldStackPointer 0
                If B$CompileErrorHappend = &FALSE
                    mov B$ReadyToRun &TRUE, B$SourceHasChanged &FALSE, eax &IDNO
                Else
                    call SetPartialEditionFromPos | mov eax &IDCANCEL
                End_If
            .End_if
        ..End_If
    ...End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[OldBlockInside: ?]  [DollarOnly: &FALSE]

Proc CharMessage:
    Argument @wParam

    mov eax D@wParam | On eax = &VK_ESCAPE, ExitP

    On B$keys+&VK_CONTROL = &TRUE, jmp L2>>

    .If D$DebugDialogHandle <> 0
        push eax
        call KillDebugger
            If eax = &IDNO
                mov eax &FALSE | ExitP
            End_If
        pop eax
    .End_If

    move D$OldBlockInside D$BlockInside | mov B$BlockInside &FALSE

    If B$WriteCheckerWanted = &TRUE
        cmp eax 32 | je L1>
        cmp eax CR | je L1>
        cmp eax ',' | je L1>
        cmp eax 8 | je L1>
       ; cmp eax ':' | je L1>
        cmp eax Tab | jne L0>
L1:     call WriteChecker D$CurrentWritingPos, eax
    End_If

L0: cmp eax CR | jne L1>
        If D$CompletionListHandle <> 0
            call ToCompletionList CR | ExitP
        End_If
        call CarriageReturn | jmp P8>>
L1: cmp eax 8 | jne L1>
        call BackSpace | jmp P8>>
L1: cmp al 167 | jne L1>   ; 167 = Paragraph Char (no more available).
        On B$DollarOnly = &TRUE, mov al '$'
L1: mov B$SimulatedBlock &FALSE
    cmp B$keys+&VK_SHIFT &TRUE | jne L1>
        On B$OldBlockInside = &FALSE, call SimulateBlockForBackIndent
L1: cmp B$OldBlockInside &TRUE | jne L1>
        cmp al Tab | jne L1>
            call IsItBlockIndent
            .If B$SimulatedBlock = &TRUE
                mov B$BlockInside &FALSE
                mov esi D$CurrentWritingPos
                If B$esi-1 = LF
                    call StartOfLine
                Else
                    call StartOfLine | call AskForRedrawNow | call StartOfLine
                End_If
            .End_If
            On B$BlockIndent = &TRUE, jmp P8>>
L1: ;mov D$OldBlockInside &FALSE
    cmp B$Overwrite &TRUE | jne L1>
        call OverwriteSource | jmp P8>>
L1:     call InsertSource | jmp P8>>

L2: call GetCtrlKeyState

    .If eax = 03
        call ControlC | mov eax &FALSE
    .Else_If eax = 016
        ;mov B$BlockInside &FALSE
        call ControlV | mov eax &TRUE
        mov B$BlockInside &FALSE
    .Else_If eax = 018
        call ControlX | mov eax &TRUE
    .Else_If eax = 04
        call ControlD | mov eax &TRUE
    .Else_If eax = 01A
        If B$Keys+&VK_SHIFT = &TRUE
            call ControlShiftZ | mov eax &FALSE ; ??? What use is this, here ???
        Else
            call ControlZ | mov eax &TRUE  ; ???
        End_If
    .Else_If eax = 1
        call ControlA | mov eax &FALSE
    .Else_If eax = 010
        call ControlP | mov eax &FALSE
    .Else_If eax = 012
        mov D$NextSearchPos 0 | call SetFindReplaceBox
        mov B$keys+&VK_CONTROL &FALSE, eax &TRUE
    .Else_If eax = 013
        call ControlS | mov eax &FALSE
        mov B$keys+&VK_CONTROL &FALSE
    .Else_If eax = 0B
        call ControlK | mov eax &FALSE
        mov B$keys+&VK_CONTROL &FALSE
    .Else_If eax = 06
        mov D$NextSearchPos 0 | call SetSimpleSearchBox
        mov B$keys+&VK_CONTROL &FALSE
        mov eax &FALSE
        mov B$keys+&VK_CONTROL &FALSE
    .Else_If eax = 019
        If B$CtrlYFlag = &TRUE
            call ControlY | mov eax &TRUE
        End_If
    .Else_If eax = 020
        On B$Underline = &TRUE, call Completion
        mov eax &TRUE
        mov B$keys+&VK_CONTROL &FALSE
    .Else
        mov eax &FALSE
    .End_If

    push eax | call CheckCtrlKeyState | pop eax | mov D$OldBlockInside &FALSE | ExitP

P8: mov D$OldBlockInside &FALSE, eax &TRUE
EndP
____________________________________________________________________________________________

[CtrlKeyState: ?]

; The "and eax 0FFFF" makes it work as well under 95 and 2000

GetCtrlKeyState:
    push eax
        call 'USER32.GetKeyState' &VK_CONTROL
        and eax 0FFFF | mov D$CtrlKeyState eax
    pop eax
ret


CheckCtrlKeyState:
    call 'USER32.GetKeyState' &VK_CONTROL | and eax 0_FFFF
    On eax <> D$CtrlKeyState, mov B$keys+&VK_CONTROL &FALSE
ret
____________________________________________________________________________________________

[SimulatedBlock: ?]

SimulateBlockForBackIndent:
    push esi
        mov esi D$CurrentWritingPos
        While B$esi-1 <> LF | dec esi | End_While
        mov D$BlockStartTextPtr esi

        mov esi D$CurrentWritingPos
        While B$esi <> CR | inc esi | End_While | dec esi
        mov D$BlockEndTextPtr esi

        mov B$OldBlockInside &TRUE, B$SimulatedBlock &TRUE
    pop esi
ret


[BlockIndent: ?]

IsItBlockIndent:
    mov B$BlockIndent &FALSE

  ; Verify that the Block includes a start of Line (accept non included Labels):
    mov esi D$BlockStartTextPtr
    While B$esi <> LF
        dec esi
        If B$esi = ':'
            jmp L2>
        Else_If B$esi > ' '
            jmp L9>>
        End_If
    End_While

  ; Verify that the Block is not empty:
L2: mov esi D$BlockStartTextPtr
    While esi < D$BlockEndTextPtr
        On B$esi > ' ', jmp L2>
        inc esi
    End_While
    jmp L9>>

  ; Verify that the last selected line is complete (does not stop before CRLF):
L2: mov esi D$BlockEndTextPtr

    While W$esi+1 <> CRLF
        On esi < D$BlockStartTextPtr, jmp L9>>
        dec esi
    EndWhile

  ; OK, Block holds a full Lines content.  ; 'InsertSource'
  ; Insert or retrieve as many Tab as Lines (but preserve Labels):
L2: On B$keys+&VK_SHIFT = &TRUE, jmp RetrieveBlockIndent

L2: mov esi D$BlockStartTextPtr, B$FirstBlockLine &TRUE

    ..While esi < D$BlockEndTextPtr
      ; Go to Start of Line:
        mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= ' ' | inc ebx | End_While
        mov eax ebx
        .While B$ebx > ' '
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= ' '
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
          ; New Start of first memeber if first one was a label:
            mov eax ebx
            End_If
        .End_While
      ; eax > real first member to move. Adjust Block Start if necessary:
        If B$FirstBlockLine = &TRUE
            mov D$BlockStartTextPtr eax
            mov B$FirstBlockLine &FALSE
        End_If
        push eax, D$SourceLen
            call SetCaret eax | dec D$CaretRow | move D$PhysicalCaretRow D$CaretRow
            mov al Tab | call InsertSourceOnBlockIndent
        pop ecx, esi
      ; Next Line:
        While B$esi <> CR | inc esi | End_While | inc esi
        mov eax D$SourceLen | sub eax ecx | add D$BlockEndTextPtr eax
    ..End_While

    mov eax D$BlockStartTextPtr
    While B$eax = ' ' | inc eax | End_While
    mov D$BlockStartTextPtr eax

    call SetCaret D$BlockEndTextPtr | mov B$RightScroll 0

  ; 'KeyMessage'
  ;  move D$ShiftBlockCol D$CaretRow
  ;  move D$ShiftBlockLine D$CaretLine
  ;  move D$PhysicalCaretRow D$CaretRow
  ;  mov D$CaretEndOfLine &TRUE
  ;  call KeyMessage

L8: mov B$BlockIndent &TRUE, B$BlockInside &TRUE
L9: ret


[FirstBlockLine: ?    NewCaretBlockPos: ?]

RetrieveBlockIndent:
    mov B$BlockIndent &TRUE, D$NewCaretBlockPos 0
  ; Are there enough free Spaces to be deleted on each Line? If not, abort:
    mov esi D$BlockStartTextPtr
    ..While esi < D$BlockEndTextPtr
      ; Go to Start of Line:
        mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= ' ' | inc ebx | End_While
        mov eax ebx
      ; If Label, jmp over it:
        .While B$ebx > ' '
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= ' '
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
              ; New Start of first memeber if first one was a label:
                mov eax ebx
            End_If
        .End_While

      ; eax > now real first member to move. Are there enough Spaces in front of it?
        push eax
            mov ecx D$TabIs
L0:         dec eax
            If B$eax <> ' '
                pop eax | jmp L8>>
            End_If
            loop L0<
        pop esi
      ; OK, enough spaces > Next Line:
        While B$esi <> CR | inc esi | End_While | inc esi
    ..End_While

  ; OK, enough Spaces on each Line > Delete:
    mov esi D$BlockStartTextPtr, B$FirstBlockLine &TRUE
    ..While esi < D$BlockEndTextPtr
      ; Go to Start of Line:
        mov ebx esi | While B$ebx <> LF | dec ebx | End_While
      ; Go to Start of first member, and save in eax:
        While B$ebx <= ' ' | inc ebx | End_While
        mov eax ebx
        .While B$ebx > ' '
            inc ebx
            If B$ebx = ':'
                inc ebx
                While B$ebx <= ' '
                    inc ebx | On B$ebx = CR, jmp L8>>
                End_While
              ; New Start of first member if first one was a label:
                mov eax ebx
            End_If
        .End_While
      ; eax > real first member to move:
        On D$NewCaretBlockPos = 0, mov D$NewCaretBlockPos eax

        If B$FirstBlockLine = &TRUE
            mov D$BlockStartTextPtr eax, B$FirstBlockLine &FALSE
        End_If
        push eax, D$SourceLen
            call SetCaret eax | dec D$CaretRow | move D$PhysicalCaretRow D$CaretRow
            mov ecx D$TabIs
L0:         push ecx
                mov al Tab | call BackSpace | dec D$BlockEndTextPtr
            pop ecx | loop L0<
        pop ecx, esi
      ; Next Line:
        sub esi D$TabIs
        While B$esi <> CR | inc esi | End_While | inc esi
    ..End_While

    mov eax D$TabIs | sub D$BlockStartTextPtr eax
    mov eax D$NewCaretBlockPos | sub eax D$TabIs | dec eax | call SetCaret eax

   ; move D$ShiftBlockCol D$CaretRow
   ; move D$ShiftBlockLine D$CaretLine
   ; move D$PhysicalCaretRow D$CaretRow
   ; mov D$CaretEndOfLine &FALSE

L8: mov B$BlockIndent &TRUE, B$BlockInside &TRUE
L9: ret



[ShiftDown: ?    ShiftBlockCol: ?    ShiftBlockLine: ?]

[keys: B$ ? #0100]

[KeyHasModifedSource: ?   KeyHasMovedCaret: ?   SourceHasChanged: ?]

KeyMessage:
    On B$SourceReady = &FALSE, ret

    mov B$KeyHasModifedSource &FALSE, B$KeyHasMovedCaret &FALSE

    ..If B$BlockInside = &TRUE
        .If B$ShiftBlockInside = &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                move D$CaretRow D$ShiftBlockCol
                move D$CaretLine D$ShiftBlockLine
                move D$PhysicalCaretRow D$CaretRow
                mov D$CaretEndOfLine &FALSE
            End_If
        .Else
            mov D$ShiftDown 0
        .End_If
    ..End_If

  ; Under some configurations, it appears that [Atl-Gr] (Right [Alt]), may generate
  ; a Key Down Message not followed by a Key Up message, for the Control Key... So:
    ...If B$keys+&VK_MENU = &TRUE
        mov B$keys+&VK_MENU &FALSE, B$keys+&VK_CONTROL &FALSE

    ...Else_If B$keys+&VK_CONTROL = &TRUE
        .If B$keys+&VK_PGDN = &TRUE
            call FullDown | mov B$BlockInside &FALSE
            mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE

        .Else_If B$keys+&VK_PGUP = &TRUE
            call FullUp | mov B$BlockInside &FALSE
            mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE

        .Else_If B$keys+&VK_LEFT = &TRUE
            call StartOfWord | mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            mov B$keys+&VK_LEFT &FALSE

        .Else_If B$keys+&VK_RIGHT = &TRUE
            call EndOfWord | mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            mov B$keys+&VK_RIGHT &FALSE

        .Else_If B$keys+&VK_DOWN = &TRUE
            call DownOneLine | mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            mov B$keys+&VK_DOWN &FALSE

        .Else_If B$keys+&VK_UP = &TRUE
            call UpOneLine | mov B$KeyHasMovedCaret &TRUE
            If B$keys+&VK_SHIFT = &TRUE
                call AskForRedrawNow | jmp L1>>
            End_If

            On B$ShiftBlockInside = &TRUE, mov B$BlockInside &FALSE, B$ShiftBlockInside &FALSE
            mov B$keys+&VK_UP &FALSE

        .Else_If B$keys+&VK_DELETE = &TRUE
            On D$DebugDialogHandle <> 0, ret
            call ControlD | mov B$keys+&VK_DELETE &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_INSERT = &TRUE
            On D$DebugDialogHandle <> 0, ret
            call ControlC | mov B$keys+&VK_INSERT &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_BACK = &TRUE
            On D$DebugDialogHandle <> 0, ret
            call ControlX | mov B$keys+&VK_BACK &FALSE, B$KeyHasModifedSource &TRUE
            mov B$RedrawFlag &TRUE
          ; This 'RedrawFlag' is to kill the next coming WM_CHAR holding
          ; ([Ctrl][Back] sends a char).

        .Else_If B$keys+&VK_F4 = &TRUE
            call RestoreRealSource
                call SetCaret D$CurrentWritingPos
                mov eax 0, ebx D$CaretLine | call MarginAction
            call SetPartialEditionFromPos
            mov B$keys+&VK_F4 0

        .Else
            On B$keys+&VK_SHIFT = &TRUE, jmp L1>

        .End_If



    ...Else_If B$keys+&VK_SHIFT = &TRUE
L1:     .If B$keys+&VK_LEFT = &TRUE
            call SetPhysicalCaretRow
            On B$keys+&VK_CONTROL = &FALSE, call KeyLeft
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE
            move D$PhysicalCaretRow D$CaretRow

        .Else_If B$keys+&VK_RIGHT = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, call KeyRight
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_UP = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, call KeyUp
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_DOWN = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, call KeyDown
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_PGUP = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, call OnlyOnePageUp
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_PGDN = &TRUE
            On B$keys+&VK_CONTROL = &FALSE, call OnlyOnePageDown
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_HOME = &TRUE
            call StartOfLine
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_END = &TRUE
            call EndOfLine
            If D$ShiftDown <> 0
                call SetShiftBlock
            Else
                mov B$BlockInside &FALSE
            End_If
            mov B$KeyHasMovedCaret &TRUE

        .Else_If B$keys+&VK_INSERT = &TRUE
            On D$DebugDialogHandle <> 0, ret
            call ControlV | mov B$keys+&VK_INSERT &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_DELETE = &TRUE
            On D$DebugDialogHandle <> 0, ret
            call ControlX | mov B$keys+&VK_DELETE &FALSE, B$KeyHasModifedSource &TRUE

        .Else_If B$keys+&VK_F4 = &TRUE
            call RestoreRealSource
                call SetCaret D$CurrentWritingPos
                mov eax 0, ebx D$CaretLine | call MarginAction
            call SetPartialEditionFromPos
            mov B$keys+&VK_F4 0

        .Else
            If B$BlockInside = &FALSE
                move D$ShiftDown D$CurrentWritingPos | ret
           ; Else
           ;     mov B$BlockInside &FALSE | call AskForRedraw | ret
            End_If

        .End_If

    ...Else
        ..If eax = &VK_PGDN
            call OnlyOnePageDown | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_PGUP
            call OnlyOnePageUp | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_DOWN
            If D$CompletionListHandle <> 0
                mov B$Keys+eax 0
                call ToCompletionList &VK_DOWN
              ; To save from downward 'KeyHasMovedCaret' Flag modification:
                ret
            Else
                call KeyDown | mov B$BlockInside &FALSE
            End_If

        ..Else_If eax = &VK_UP
            If D$CompletionListHandle <> 0
                mov B$Keys+eax 0
                call ToCompletionList &VK_UP
              ; To save from downward 'KeyHasMovedCaret' Flag modification:
                ret
            Else
                call KeyUp | mov B$BlockInside &FALSE
            End_If

        ..Else_If eax = &VK_LEFT
            call KeyLeft | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_RIGHT
            call KeyRight | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_INSERT
            call KeyInsert | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_DELETE
            .If D$DebugDialogHandle <> 0
                mov B$Keys+eax 0
                call KillDebugger | On eax = &IDNO, ret
            .End_If
            call KeyDelete | mov B$BlockInside &FALSE, B$KeyHasModifedSource &TRUE

        ..Else_If eax = &VK_END
            call EndOfLine | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_HOME
            call StartOfLine | mov B$BlockInside &FALSE

        ..Else_If eax = &VK_ESCAPE
            If D$CompletionListHandle <> 0
                call 'USER32.SendMessageA' D$CompletionListHandle, &WM_COMMAND, &IDCANCEL, 0
                mov B$keys+&VK_ESCAPE &FALSE | ret
            End_If

        ..Else_If eax = &VK_F1
            call RosAsmHelp | mov B$keys+&VK_F1 &FALSE

        ..Else_If eax = &VK_F2
            call F2Help | mov B$keys+&VK_F2 &FALSE

        ..Else_If eax = &VK_F3
            call RestoreRealSource | call StringSearch | call SetPartialEditionFromPos
            mov B$keys+&VK_F3 0

        ..Else_If eax = &VK_F4  ; BpMenu / SetBreakPoint / DeleteBreakpoint
            call RestoreRealSource
                call SetCaret D$CurrentWritingPos
                mov eax 0, ebx D$CaretLine | call MarginAction
            call SetPartialEditionFromPos
            mov B$keys+&VK_F4 0

        ..Else_If eax = &VK_F5
            mov B$keys+&VK_F5 &FALSE
            mov D$ShowStats &TRUE
            call Compile

        ..Else_If eax = &VK_F6
            mov B$keys+&VK_F6 &FALSE

            If D$DebugDialogHandle <> 0
                ret
            Else
                call Run
            End_If

        ..Else_If eax = &VK_F8
            If D$DebugDialogHandle <> 0
                mov B$keys+&VK_F2 &FALSE
                call KillDebugger | On eax = &IDNO, ret
            End_If
            call DrawOneLine

        ..Else_If eax = &VK_F9
            mov B$Keys+eax 0
            ;mov eax D$BreakPointsTables | int3
            ret
;;
; Problem if someone want to implement a Key doinf the same job as Right-Click:
; How to get back after Editor moves?

            call KillCompletionList
  
            call RowToX D$CaretRow | mov D$MousePosY eax
            call LineToY D$CaretLine | mov D$MousePosY eax
            call 'USER32.SetCursorPos' D$MousePosX, D$MousePosY

            call RestoreRealSource
                call RightClick
            call SetPartialEditionFromPos
            mov B$keys+&VK_F9 &FALSE
;;
        ..Else_If eax = &VK_F11
            call SavePosOnF11

        ..Else_If eax = &VK_F12
            call SetPosOnF12

        ..Else
            mov B$Keys+eax 0 | ret

        ..End_If
      ; Necessity for clearing by hand because, with lengthy case (like upper 'StringSearch'),
      ; the OS clear the remaining Messages (here WM_KEYUP!!!) for the Message Flow.
       ; pop eax | mov B$Keys+eax 0
        mov B$KeyHasMovedCaret &TRUE

    ...End_If


    On B$KeyHasMovedCaret = &TRUE, call KillCompletionList
ret
____________________________________________________________________________________________

ResetKeys:
    mov edi keys, eax 0, ecx (0100/4) | rep stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

EnableMenutems:
   ; call 'USER32.EnableMenuItem' D$MenuHandle, M00_Profile, &MF_GRAYED

    .If B$SourceReady = &FALSE
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Tree, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste_at_Pos, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Change_Compile_Name, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_Source_Only, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Source_Only, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Output, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Print, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Find, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Undo, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Redo, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Copy, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cut, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Compile, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Import, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Export, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_GUIDs, &MF_GRAYED

    .Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Tree, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste_at_Pos, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Change_Compile_Name, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_Source_Only, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Source_Only, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Output, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Print, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Find, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Undo, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Redo, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Copy, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Paste, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cut, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Compile, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Import, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Export, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_GUIDs, &MF_ENABLED

    .End_If

    If D$SavingExtension = '.SYS'
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_GRAYED
    Else_If B$SourceReady = &TRUE
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Run, &MF_ENABLED
    End_If

    If D$IconList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Icon, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Icon_IDs, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Icon, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Icon, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Icon_IDs, &MF_GRAYED ; &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Icon, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$BitMapList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_BitMap, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_BitMaps_IDs, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_BitMap, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_BitMap, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_BitMaps_IDs, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_BitMap, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$CursorList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Cursor, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cursors_IDs, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Cursor, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Cursor, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Cursors_IDs, &MF_GRAYED ;&MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Cursor, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$WaveList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Wave, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Waves_IDs, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Wave, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Wave, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Waves_IDs, &MF_GRAYED ;&MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Wave, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$AviList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Avi, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Avi_IDs, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Avi, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Avi, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Avi_IDs, &MF_GRAYED ;&MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_Avi, &MF_GRAYED ;&MF_ENABLED
    End_If

    If D$RCDataList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_RC, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_RCs_IDs, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_RC, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_RC, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_RCs_IDs, &MF_GRAYED ;&MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_RC, &MF_ENABLED
    End_If

    If D$DialogList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Resources, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Resources_Dialog, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_File, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_File, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Resources, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_Resources_Dialog, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_File, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_File, &MF_ENABLED
    End_If

    If B$SourceReady = &FALSE
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Binary_File, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_from_Binary_File, &MF_ENABLED
    End_If

    If D$MenuList = 0
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Existing_Menu, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_a_Menu, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_Menu_File, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_Binary_Menu_File, &MF_GRAYED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_Menu_File, &MF_GRAYED
    Else
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Existing_Menu, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Delete_a_Menu, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Save_to_Binary_Menu_File, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Load_Binary_Menu_File, &MF_ENABLED
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Replace_from_Binary_Menu_File, &MF_ENABLED
    End_If

    call 'USER32.DrawMenuBar' D$hwnd
ret


[VisualTutsFindHandle: ?    VisualTutsMenuHandle: ?    VisualTutMenuID: ?]
[VisualTutsItem: 'Visual Tuts', 0]
[VisualTutPath: B$ ? #&MAXPATH]
[WizardPath: B$ ? #&MAXPATH]


EnableHelpMenutems:
    call EnableHelpMenutem B_U_AsmName, M00_B_U_Asm
    call EnableHelpMenutem Win32HlpName, M00_Win32_hlp
    call EnableHelpMenutem MmediaHlpName, M00_Mmedia_hlp
    call EnableHelpMenutem OpenGlHlpName, M00_OpenGl_hlp
    call EnableHelpMenutem DxHlpName, M00_Dx_hlp
    call EnableHelpMenutem WinsockHlpName, M00_WinSock_hlp
    call EnableHelpMenutem SDLRefName, M00_SDL
    call EnableHelpMenutem sqliteName, M00_sqlite
    call EnableHelpMenutem DevILName, M00_DevIL
ret


[PreviousIVT: ?] [IVTFilesNames1 Trash1][IVTFilesNames2 Trash2]

EnableVisualTutsMenu:
  ; Enable the 'Visual Tuts' Item and create the Pop-Up if some Visual Tuts are there:
    mov esi EquatesName, edi VisualTutPath, ecx (&MAXPATH / 4) | rep movsd

    mov esi VisualTutPath | While B$esi <> 0 | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While
    mov D$esi+1 'IVT*', D$esi+5 '.exe', B$esi+9 0

    call 'KERNEL32.FindFirstFileA' VisualTutPath, FindFile

    .If eax = &INVALID_HANDLE_VALUE
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Visual_Tuts, &MF_GRAYED

    .Else
        mov D$VisualTutsFindHandle eax

        call 'USER32.CreatePopupMenu' | mov D$VisualTutsMenuHandle eax

        mov edi IVTFilesNames1, ecx 0

L1:     mov esi FindFile.cFileName | inc ecx

        While B$esi <> 0 | movsb | End_While | mov B$edi 0 | inc edi

        push ecx
            call 'KERNEL32.FindNextFileA' D$VisualTutsFindHandle, FindFile
        pop ecx

        On eax = &TRUE, jmp L1<

        call zStringsSort IVTFilesNames1, IVTFilesNames2, ecx
        ____________________________________________________

        mov B$PreviousIVT '0', D$VisualTutMenuID 5000

        call 'USER32.CreatePopupMenu' | mov D$VisualTutsMenuHandle eax

        mov esi IVTFilesNames2

L1:     add esi 6

        push esi
            mov al B$esi-3
            If al <> B$PreviousIVT
                mov B$PreviousIVT al
                call 'USER32.AppendMenuA' D$VisualTutsMenuHandle, &MF_SEPARATOR, 0, 0
            End_If

            call 'USER32.AppendMenuA' D$VisualTutsMenuHandle, &MF_ENABLED__&MF_STRING,
                                      D$VisualTutMenuID, esi
        pop esi

        While B$esi <> 0 | inc esi | End_While | inc esi

        If B$esi <> 0
            inc D$VisualTutMenuID | jmp L1<
        End_If

        call 'USER32.InsertMenuA' D$MenuHandle, M00_Visual_Tuts,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$VisualTutsMenuHandle, VisualTutsItem

        call 'USER32.DeleteMenu' D$MenuHandle, M00_Visual_Tuts, &MF_BYCOMMAND

        call 'KERNEL32.FindClose' D$VisualTutsFindHandle
    .End_If
ret


VisualTuts:
    push eax
        .If D$DebugDialogHandle <> 0
            call KillDebugger
            If eax = &IDNO
                pop eax | ret
            End_If
        .End_If

        ...If B$SourceReady = &TRUE
            ..If B$ReadyToRun = &FALSE
                .If B$SecurityWanted = &TRUE
                    call 'USER32.MessageBoxA' D$hwnd, {'Close the Actual File ?', 0},
                                              {'Visual Tutorial', 0}, &MB_YESNO
                    If eax = &IDNO
                        pop eax | ret
                    End_If
                .End_If
            ..End_If
        ...End_If
    pop eax

    mov esi VisualTutPath | While D$esi <> '\IVT' | inc esi | End_While
    add esi 4 | mov D$esi '???' | add esi 3
    call 'USER32.GetMenuStringA' D$MenuHandle, eax, esi, 100, &MF_BYCOMMAND

    call 'KERNEL32.FindFirstFileA' VisualTutPath, FindFile

    .If eax <> &INVALID_HANDLE_VALUE
        mov D$VisualTutsFindHandle eax
        call 'KERNEL32.FindClose' D$VisualTutsFindHandle

        mov esi VisualTutPath, edi SaveFilter
        While D$esi <> '\IVT' | movsb | End_While | movsb
        mov esi FindFile.cFileName | While B$esi <> 0 | movsb | End_While | movsb

        call DirectLoad

        call ReInitUndo
        call SetPartialEditionFromPos | call EnableMenutems
        call LoadBookMarks
    .End_If
ret


Proc EnableHelpMenutem:
    Argument @FileName, @Item

    call 'KERNEL32.FindFirstFileA' D@FileName, FindFile
    push eax
        If eax = &INVALID_HANDLE_VALUE
            mov eax &MF_GRAYED
        Else
            mov eax &MF_ENABLED
        End_If

        call 'USER32.EnableMenuItem' D$MenuHandle, D@Item, eax
    pop eax
    call 'KERNEL32.FindClose' eax
EndP
____________________________________________________________________________________________

[WizardsFindHandle: ?    WizardsMenuHandle: ?    WizardMenuID: ?]
[PreviousWZRD: ?]

[WizardsItem: 'Wizards', 0]

EnableWizardsMenu:
    call ClearTrashTables

  ; Enable the 'Visual Tuts' Item and create the Pop-Up if some Visual Tuts are there:
    mov esi EquatesName, edi WizardPath, ecx (&MAXPATH / 4) | rep movsd

    mov esi WizardPath | While B$esi <> 0 | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While

    mov D$esi+1 'WZRD', D$esi+5 '*.*'

    call 'KERNEL32.FindFirstFileA' WizardPath, FindFile

    .If eax = &INVALID_HANDLE_VALUE
        call 'USER32.EnableMenuItem' D$MenuHandle, M00_Wizards, &MF_GRAYED

    .Else
        mov D$WizardsFindHandle eax

        call 'USER32.CreatePopupMenu' | mov D$WizardsMenuHandle eax

        mov edi Trash1, ecx 0

L1:     mov esi FindFile.cFileName | inc ecx

        While B$esi <> 0 | movsb | End_While | mov B$edi 0 | inc edi

        push ecx
            call 'KERNEL32.FindNextFileA' D$WizardsFindHandle, FindFile
        pop ecx

        On eax = &TRUE, jmp L1<

        call zStringsSort Trash1, Trash2, ecx

        ____________________________________________________

        mov B$PreviousWZRD '0', D$WizardMenuID 6000

        call 'USER32.CreatePopupMenu' | mov D$WizardsMenuHandle eax

        mov esi Trash2

L1:     add esi 4

        push esi
           ; mov al B$esi-3
           ; If al <> B$PreviousWZRD
           ;     mov B$PreviousWZRD al
           ;     call 'USER32.AppendMenuA' D$VisualTutsMenuHandle, &MF_SEPARATOR, 0, 0
           ; End_If

            call 'USER32.AppendMenuA' D$WizardsMenuHandle, &MF_ENABLED__&MF_STRING,
                                      D$WizardMenuID, esi
        pop esi

        While B$esi <> 0 | inc esi | End_While | inc esi

        If B$esi <> 0
            inc D$VisualTutMenuID | jmp L1<
        End_If

        call 'USER32.InsertMenuA' D$MenuHandle, M00_Wizards,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$WizardsMenuHandle, WizardsItem

        call 'USER32.DeleteMenu' D$MenuHandle, M00_Wizards, &MF_BYCOMMAND

        call 'KERNEL32.FindClose' D$WizardsFindHandle
    .End_If
ret
____________________________________________________________________________________________

[ClipMenuID: 7000    NumberOfClipFiles: 0]

[FindClipFilesHandle: ?    ClipFilePopUpMenuHandle: ?
 ClipFilesPath: B$ ? #&MAXPATH]

[ClipFilesItem: 'Clip Files', 0]

; Room for storing 20 Clip Files Names of 20 Chars each:

[ClipMenuStrings: ? #100]

Proc StoreClipMenuStrings:
    Argument @Source
    uses esi, edi

        mov edi ClipMenuStrings
L1:     While B$edi <> 0 | inc edi | End_While
        If edi <> ClipMenuStrings
            inc edi | On B$edi <> 0, jmp L1<
        End_If
        mov esi D@Source
        While B$esi <> 0 | movsb | End_While
EndP


EnableClipMenu: ; M00_Clip_File / 'LoadClipFile'
    call 'KERNEL32.FindFirstFileA' ClipName, FindFile
;;
  Assuming Multiple Clip Files had never been planed, and the whole implementation
  is supposed to work withnone single 'Clip.txt', which Path is given in 'ClipName',
  from the Configuration.
  
  So, if no Clip File found, we simple leave, and let the default way:
;;
    ...If eax = &INVALID_HANDLE_VALUE
        jmp L9>>

    ...Else
        call 'KERNEL32.FindClose' eax

        mov esi ClipName, edi ClipFilesPath
        While B$esi <> 0 | movsb | End_While
        While B$edi <> '\' | dec edi | End_While | inc edi
        mov D$edi '*Cli', D$edi+4 'p.tx', W$edi+8 't'

        call 'KERNEL32.FindFirstFileA' ClipFilesPath, FindFile

        ..If eax = &INVALID_HANDLE_VALUE
            jmp L9>>

        ..Else
            mov D$FindClipFilesHandle eax

L1:         If D$ClipFilePopUpMenuHandle = 0
                call 'USER32.CreatePopupMenu' | mov D$ClipFilePopUpMenuHandle eax
            End_If

            inc D$NumberOfClipFiles

            call 'USER32.AppendMenuA' D$ClipFilePopUpMenuHandle,
                                      &MF_ENABLED__&MF_STRING,
                                      D$ClipMenuID, FindFile.cFileName

            call StoreClipMenuStrings FindFile.cFileName

            inc D$ClipMenuID

            call 'KERNEL32.FindNextFileA' D$FindClipFilesHandle, FindFile
            On eax = &TRUE, jmp L1<<

            call 'KERNEL32.FindClose' D$FindClipFilesHandle

            .If D$ClipFilePopUpMenuHandle <> 0
                If D$NumberOfClipFiles > 1
                    call 'USER32.InsertMenuA' D$MenuHandle, M00_ClipFile,
                                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                              D$ClipFilePopUpMenuHandle, ClipFilesItem

                    call 'USER32.DeleteMenu' D$MenuHandle, M00_ClipFile, &MF_BYCOMMAND
                End_If
            .End_If

        ..End_If

    ...End_If
L9: ret
____________________________________________________________________________________________


SetShiftBlock:
    mov eax D$CaretRow, ebx D$CaretLine
    push eax, ebx

        call SearchTxtPtr               ; >>> eax = new Pos.

        .If B$BlockInside = &FALSE
            If eax > D$ShiftDown
                dec eax
            End_If

        .Else
            If eax = D$ShiftDown
                mov B$BlockInside &FALSE | jmp L9>
            Else_If eax > D$ShiftDown
                dec eax
            End_If

        .End_If

        If eax < D$ShiftDown
            move D$BlockStartTextPtr eax, D$BlockEndTextPtr D$ShiftDown
            dec D$BlockEndTextPtr
        Else
            move D$BlockStartTextPtr D$ShiftDown, D$BlockEndTextPtr eax
        End_If

        mov B$BlockInside &TRUE, B$ShiftBlockInside &TRUE

L9: pop D$ShiftBlockLine, D$ShiftBlockCol
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

SplashScreen:
    call 'USER32.BeginPaint' D$EditWindowHandle, PAINTSTRUCT | mov D$hdc eax

    call 'User32.GetClientRect' D$EditWindowHandle, RECT

    shr D$RECTright 1 | shr D$RECTbottom 1
    sub D$RECTright 75 | sub D$RECTbottom 75

    call 'USER32.LoadBitmapA' D$hInstance, 5 | mov D$BitMapHandle eax

    call 'GDI32.CreateCompatibleDC' D$hdc | mov D$hMemDC eax

    call 'GDI32.SelectObject' eax, D$BitMapHandle

    call 'GDI32.BitBlt' D$hdc, D$RECTright, D$RECTbottom, 150, 150,
                        D$hMemDC, 0, 0, &SRCCOPY

    call 'GDI32.DeleteDC' D$hMemDC

    call 'GDI32.DeleteObject' D$BitMapHandle

    call 'USER32.EndPaint' D$EditWindowHandle, PAINTSTRUCT
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  BackUps.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________






