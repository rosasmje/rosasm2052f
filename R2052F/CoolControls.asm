TITLE CoolControls

____________________________________________________________________________________________
____________________________________________________________________________________________

;              Cool Controls are a set of functinos that enhaces the Look of
;              RosAsm Controls, like Menus, ToolBars, Dialogs, Images etc

;              Author: Guga  - January/2.006
____________________________________________________________________________________________
____________________________________________________________________________________________

; ToolBar Controls
____________________________________________________________________________________________
____________________________________________________________________________________________

; CharMap dialog image list initialisation & finalization.


Proc CoolControlTB_CreateImageList:
    Arguments @OutPutHandle, @EnabledImage, @DisabledImage, @Cx, @Cy, @Flags, @CInitial, @CGrow
    Local @Image, @Mask, @TempBuff


    lea edi D@OutPutHandle
    move D@TempBuff D$edi

  ; Create the images
    call 'User32.LoadImageA' D$hInstance, D@EnabledImage, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        call ReportWinError {'CoolControl ToolBar CreateImageList: LoadImage (1 - Enabled)' 0}
    EndIf
    mov D@Image eax

    call 'User32.LoadImageA' D$hInstance, D@DisabledImage, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        call ReportWinError {'CoolControl ToolBar CreateImageList: LoadImage (2 - Disabled)' 0}
    EndIf
    mov D@Mask eax

    call 'ComCtl32.ImageList_Create' D@Cx, D@Cy, D@Flags, D@CInitial, D@CGrow
    mov D$edi eax       ; Copy the Value store in eax to the Data in Edi (That have the address of the Outputed Buffer)
    mov eax D@TempBuff  ; Copy our previously stored address of the outputed Buffer to eax
    move D$eax D$edi    ; Save the Data to be outputed in the outputed Buffer at the OutPutHandle that now is stored in eax

    call 'ComCtl32.ImageList_Add' D@OutPutHandle, D@Image, D@Mask
    If eax = 0-1
        call ReportWinError {'CoolControl ToolBar: ImageList_Add' 0}
    EndIf

    call 'GDI32.DeleteObject' D@Image
    call 'GDI32.DeleteObject' D@Mask
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CoolControlWin_CreateToolbar:
    Arguments @Addresse, @OutPutHandle, @ToolStructure, @TotalButtons, @ToolTipArray, @tbCmdStruc
    Local @TempBuff
    pushad

    call 'ComCtl32.InitCommonControlsEx' Init_Common_Controls

    ; Always initialize the ToolBar handle with 0

    lea edi D@OutPutHandle
    move D@TempBuff D$edi
    mov eax D@TempBuff
    mov D$eax 0

    ; Always initialize the Text Flag to FALSE

    mov edx D@tbCmdStruc
    mov edx D$edx+TBWIN_CMD.ShowTxtFlagDis
    mov D$edx &FALSE

    call CoolControlWin_CreateCommandTB D@Addresse, eax, D@ToolStructure, D@TotalButtons, D@ToolTipArray, D@tbCmdStruc
    popad
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
    CoolControlWin_CreateCommandTB
    
    This function builds the toolbar located inside a window handle.
    
    Parameters:
    
        Addresse: Address of the window handle
        
        OutPutHandle: Pointer to the handle of the ToolBar. When the function exits, it will output to the handle
                      of the toolbar.
                      
        ToolStructure: Pointer to a array of TBBUTTON Structure containing the Data to be used for the toolbar.
                       Each element of the array is a TBBUTTON Structure related to each one of button the user wants that
                       his toolbar to have.
        
        TotalButtons:  Total amount of Buttons of the created ToolBar.
        
        ToolTipArray:   Pointer to a array of Pointers (Dwords) to Null-Terminated Strings used for ToolTips or for displaying inside
                        each Button. The amount of elements of the array needs to be the same as the amount of buttons
                        to allow that each button have a ToolTip Text String.
        
        tbCmdStruc:     Pointer to the address of a TBWin_Cmd (ToolBar windows Command) Structure that contains the initialization values
                        of the toolbar to be created.
                        
                        
                        The TBWin_Cmd Structure have the following format:
                        
                        [TBWin_Cmd:
                            bWidth: D$ 0
                            dwStyle: D$ 0
                            hMenu: D$ 0
                            hIml: D$ 0
                            ShowTxtFlag: D$ 0]
                        
                        It member's information are related below:
                        
                            bWidth: Value of the width of each Button of the ToolBar.
                            
                            dwStyle: Styles of the ToolBar. The same ones defined for usage in CreateWindowExA (User32)
                                     when you are creating a 'ToolbarWindow32' class.
                                    
                            hMenu: Menu handle or child identifier of the window
                            
                            hIml: Pointer to a Buffer containing ImageList handle for this toolBar.
                                  The handle must be previously created from the function CoolControlTB_CreateImageList.
                            
                            ShowTxtFlag: Pointer to a Buffer that contains a Flag to display or not the Text inside a Button.
                                         If the Flag is settled to &TRUE the ToolBar displays the text inside the Button,
                                         otherwise it does not display the text.
;;


[CHARMAP_TOOLBAR_STYLE &WS_CHILD__&WS_VISIBLE__&TBSTYLE_FLAT__&TBSTYLE_LIST__&TBSTYLE_AUTOSIZE__&TBSTYLE_TRANSPARENT__&TBSTYLE_TOOLTIPS__&CCS_TOP]

[CHARMAPDLG_TOOLBAR 432]

[CharMapDialog_ImageList: ?]

[CharMapShowTBText: ?]


; Based on DebugDialog_CreateCommandTB
; CharMapToolbarHandle
[TBWin_Cmd:
 TBWin_Cmd.bWidth: D$ 20
 TBWin_Cmd.dwStyle: D$ CHARMAP_TOOLBAR_STYLE
 TBWin_Cmd.hMenu: D$ CHARMAPDLG_TOOLBAR
 TBWin_Cmd.hIml: D$ CharMapDialog_ImageList
 TBWin_Cmd.ShowTxtFlag: D$ CharMapShowTBText]

[TBWIN_CMD.bWidthDis 0
 TBWIN_CMD.dwStyleDis 4
 TBWIN_CMD.hMenuDis 8
 TBWIN_CMD.hImlDis 12
 TBWIN_CMD.ShowTxtFlagDis 16

 SizeOf_TBWIN_CMD 20]

[TBBUTTON.iBitmapDis 0
 TBBUTTON.idCommandDis 4
 TBBUTTON.fsStateDis 8
 TBBUTTON.fsStyleDis 9
 TBBUTTON._wPad1Dis 10
 TBBUTTON.dwDataDis 12
 TBBUTTON.iStringDis 16

 SizeOf_TBBUTTON 20]

Proc CoolControlWin_CreateCommandTB:
    Arguments @Addresse, @OutPutHandle, @ToolStructure, @TotalButtons, @ToolTipArray, @tbCmdStruc
    Local @TBWidth
    pushad

    mov ebx D@ToolStructure
    mov esi 0

    mov ecx D@OutPutHandle ; We need to keep track on ecx because this is the output handle.

    ; Save states & clear if toolbar is Recreated
    .If D$ecx <> 0
        Do
            push ecx | SendMessage D$ecx, &TB_GETSTATE, D$ebx+TBBUTTON.idCommandDis, 0 | pop ecx
            mov B$ebx+TBBUTTON.fsStateDis al
            inc esi
            add ebx SizeOf_TBBUTTON
        Loop_Until esi = D@TotalButtons

        push ecx | call 'USER32.DestroyWindow' D$ecx | pop ecx
    .EndIf

    ; Create toolbar
    push ecx
        mov edx D@tbCmdStruc
        mov esi D@tbCmdStruc
        ; eax = D@TBWidth * D@TotalButtons
        move D@TBWidth D$edx
        mov eax D@TotalButtons
        mul D@TBWidth

        call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0, D$esi+TBWIN_CMD.dwStyleDis,
                                      0, 0, eax, 0, D@Addresse, D$esi+TBWIN_CMD.hMenuDis, D$hInstance, 0

    pop ecx

    mov D$ecx eax
    push ecx | SendMessage D$ecx, &TB_BUTTONSTRUCTSIZE, SizeOf_TBBUTTON, 0 | pop ecx

    mov eax D$esi+TBWIN_CMD.hImlDis
    push ecx | SendMessage D$ecx, &TB_SETIMAGELIST, 0, D$eax | pop ecx

    mov ebx D@ToolStructure
    mov edx D@tbCmdStruc
    mov edx D$edx+TBWIN_CMD.ShowTxtFlagDis
    mov esi 0
    mov edi D@ToolTipArray

  ; Activate / Deactivate Text
    .If D$edx = &TRUE
        Do
            move D$ebx+TBBUTTON.iStringDis D$edi
            inc esi
            add ebx SizeOf_TBBUTTON
            add edi 4
        Loop_Until esi = D@TotalButtons

    .Else
        Do
            mov D$ebx+TBBUTTON.iStringDis 0
            inc esi
            add ebx SizeOf_TBBUTTON
        Loop_Until esi = D@TotalButtons

    .EndIf

    mov ebx D@ToolStructure
    push ecx | SendMessage D$ecx, &TB_ADDBUTTONS, D@TotalButtons, ebx | pop ecx

    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CoolControlTB_OnNotify:
    Arguments @Handle, @Notification, @ToolStructure, @TotalButtons, @ToolTipArray
    Uses ebx, edx, ecx, edi

        mov ebx D@Notification
        mov edx D$ebx+NMHDR.idFromDis
        mov eax D$ebx+NMHDR.codeDis

        mov edi D@ToolStructure

        ..If eax = &TTN_NEEDTEXT
            mov eax D$ebx+NMHDR.idFromDis
          ; Pointing with esi to the Buttons List IDs:
            lea esi D$edi+TBBUTTON.idCommandDis

            mov ecx 0
            While D$esi <> eax
                add esi SizeOf_TBBUTTON | inc ecx
                If ecx > D@TotalButtons
                    mov eax 0   ; mandatory for TCN_SELCHANGING !!!
                    ExitP
                End_If
            End_While

            mov edi D@ToolTipArray
            mov eax D$edi+ecx*4
            mov D$ebx+TOOLTIPTEXT_lpszText eax

        ..EndIf

        mov eax 0 ; mandatory for TCN_SELCHANGING !!!
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CoolControlTabToolTip_OnNotify:
    Arguments @Notification, @TotalButtons, @ToolTipArray
    Uses ebx, edx, ecx, edi

        mov ebx D@Notification
        mov edx D$ebx+NMHDR.idFromDis
        mov eax D$ebx+NMHDR.codeDis

        mov esi 0 ; The Tab Item always starts with 0

        ..If eax = &TTN_NEEDTEXT
            mov eax D$ebx+NMHDR.idFromDis
            mov ecx 0
            While esi <> eax
                inc esi | inc ecx
                If ecx > D@TotalButtons
                    mov eax 0   ; mandatory for TCN_SELCHANGING !!!
                    ExitP
                End_If
            End_While

            mov edi D@ToolTipArray
            mov eax D$edi+ecx*4
            mov D$ebx+TOOLTIPTEXT_lpszText eax

        ..EndIf

        mov eax 0 ; mandatory for TCN_SELCHANGING !!!
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CoolControlTabChange_OnNotify:
    Arguments @Notification, @TabSelected, @MainTab

        mov ebx D@Notification
        mov eax D$ebx+NMHDR.codeDis
        lea edi D@TabSelected
        mov edi D$edi

        ..If eax = &TCN_SELCHANGE

            ;Tab selection. We are sending the message on the Main Tab Control, in the main dialog.
            ; At ebx we have the handle of the MainTab = htab
            call 'USER32.SendMessageA' D$ebx &TCM_GETCURSEL 0 0
            .If eax <> D$edi

                push eax
                mov eax D$edi
                call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_HIDE
                pop eax
                mov D$edi eax
                call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_SHOWDEFAULT

            .End_If

        ..End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Tab Controls
____________________________________________________________________________________________
____________________________________________________________________________________________

[RECT.leftDis 0
 RECT.topDis 4
 RECT.rightDis 8
 RECT.bottomDis 12]

[Size_Of_RECT 16]

Proc CoolControlDialog_AdjustDataTabSize:
    Arguments @Handle, @nIDDlgItem, @hWndPage, @Rect
    Local @TabHandle

    pushad

    call 'USER32.GetDlgItem' D@Handle, D@nIDDlgItem
    mov D@TabHandle eax

    mov esi D@Rect
    call 'USER32.GetClientRect' D@TabHandle, esi
    SendMessage D@TabHandle, &TCM_ADJUSTRECT, &FALSE, esi
    mov eax D$esi+RECT.leftDis
    mov ebx D$esi+RECT.topDis
    mov ecx D$esi+RECT.rightDis
    mov edx D$esi+RECT.bottomDis
    sub ecx eax
    sub edx ebx
    call 'USER32.SetWindowPos' D@hWndPage, 0, eax, ebx, ecx, edx, &SWP_NOZORDER

    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; ToolBar Controls
____________________________________________________________________________________________
____________________________________________________________________________________________

[NM_LISTVIEW.hdr.hwndFromDis 0
 NM_LISTVIEW.hdr.idFromDis 4
 NM_LISTVIEW.hdr.codeDis 8
 NM_LISTVIEW.iItemDis 12
 NM_LISTVIEW.iSubItemDis 16
 NM_LISTVIEW.uNewStateDis 20
 NM_LISTVIEW.uOldStateDis 24
 NM_LISTVIEW.uChangedDis 28
 NM_LISTVIEW.ptAction.xDis 32
 NM_LISTVIEW.ptAction.yDis 36
 NM_LISTVIEW.lParamDis 40]

;;

    CoolControl_ListViewAlternateSort
    
    This function reorganizes (sorts) each one of the columns on a ListView Control. 
    
    Parameters:
    
        hLView:         Address of a Sort function where the user defines how each one of the ListView itens is displayed.
        
        Notification:   Pointer to the Notification message handle (lParam, for example)
                      
        SortItem:       Address of the Buffer responsable for the Sorting Itens
        
        hList:          Pointer to the handle of the ListView Control.
        
        LViewNumber:    Total amount of Columns used on the listview.


    Returned values: This function does not returns any value.

    Usage Example:
        call CoolControl_ListViewAlternateSort ListViewSort, D@Notification, SortDecimal, D$hCharMapList, 5    

    Note: The functionality of this function is exactly the same as if we defined each listview item by hand, like
          if when we have, for example 5 itens (columns) on a listview, defined as:
          
                .If D$ebx+NM_LISTVIEW.iSubItemDis = 0 ; decimal
                    call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 1, 2
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 1 ; hexadecimal
                    call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 3, 4
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 2 ; Char
                    call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 5, 6
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 3 ; Count
                    call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 7, 8
                .Else_If D$ebx+NM_LISTVIEW.iSubItemDis = 4 ; Percent
                    call AlternateSorting ListViewSort, SortDecimal, D$hCharMapList, 9, 10
                .End_If          
;;

Proc CoolControl_ListViewAlternateSort:
    Arguments @hLView, @Notification, @SortItem, @hList, @LViewNumber
    Local @InitValue, @NextValue

    pushad

    mov ebx D@Notification
    mov edx D$ebx+NMHDR.idFromDis
    mov eax D$ebx+NMHDR.codeDis

    mov D@InitValue 1
    mov D@NextValue 2

    mov ecx 0 ; Column Counter. Always starts at the 1st column.

    .While ecx <> D@LViewNumber
        If D$ebx+NM_LISTVIEW.iSubItemDis = ecx
            call AlternateSorting D@hLView, D@SortItem, D@hList, D@InitValue, D@NextValue
            jmp L1> ; Once we identify wich column we are working with, we go out of the loop
        End_If
        inc ecx
        add D@InitValue 2
        add D@NextValue 2
    .End_While

L1: popad
EndP
________________________________________________________________________________________

Proc AlternateSorting:
    Arguments @hLView, @SortItem, @hList, @InitValue, @NextValue
    pushad

    mov edi D@SortItem
    If D$edi = &FALSE
        call 'USER32.SendMessageA' D@hList, &LVM_SORTITEMS, D@InitValue, D@hLView
        call Resequence D@hList
        mov edi D@SortItem
        mov D$edi &TRUE
    Else
        call 'USER32.SendMessageA' D@hList, &LVM_SORTITEMS, D@NextValue, D@hLView
        call Resequence D@hList
        mov edi D@SortItem
        mov D$edi &FALSE
    End_If

    popad
EndP

________________________________________________________________________________________

Proc Resequence:
    Arguments @hList
    Structure @lviResequence 40, lviResequence.imaskDis 0, lviResequence.iItemDis 4, lviResequence.iSubItemDis 8, lviResequence.stateDis 12, lviResequence.stateMaskDis 16, lviResequence.pszTextDis 20, lviResequence.cchTextMaxDis 24, lviResequence.iImageDis 28, lviResequence.lParamDis 32, lviResequence.iIndentDis 36

    pushad

    call 'USER32.SendMessageA' D@hList, &LVM_GETITEMCOUNT, 0, 0
    mov edi eax
    mov D$lviResequence.imaskDis &LVIF_PARAM
    mov D$lviResequence.iSubItemDis 0
    mov D$lviResequence.iItemDis 0

    While edi <> 0
        push D$lviResequence.iItemDis
        pop D$lviResequence.lParamDis
        call 'USER32.SendMessageA' D@hList, &LVM_SETITEM, 0, D@lviResequence
        inc D$lviResequence.iItemDis
        dec edi
    End_While

    popad

EndP
________________________________________________________________________________________

;;

    CoolControl_LVBeginSort
    
    This function initializes the sorting state reorganizes (sorts) each one of the columns on a ListView Control. 
    
    Parameters:
    
        hLView:         Address of a Sort function where the user defines how each one of the ListView itens is displayed.
        
        SortItem:       Address of the Buffer responsable for the Sorting Itens
        
        hList:          Pointer to the handle of the ListView Control.
        
        InitValue:      Initial value of the item the user wants to start first. If the user wants to 
                        start sorting biased on the 1st column (value = 0), theInitValue is equal to 1.
                        If it is biased on the 2nd column, the initvalue is 3.
                        If it is biased on the 3rd column, the initvalue is 5
                        If it is biased on the 4th column, the initvalue is 7, and so on.
                        
                        Note: By default, the InitValue should be settled to 1.

    Returned values: This function does not returns any value.

    Usage Example:
        call CoolControl_LVBeginSort ListViewSort, SortDecimal, D$hCharMapList, 1

;;

Proc CoolControl_LVBeginSort:
    Arguments @hLview, @SortItem, @hList, @InitValue

    pushad
    call 'USER32.SendMessageA' D@hList, &LVM_SORTITEMS, D@InitValue, D@hLview
    call Resequence D@hList
    mov edi D@SortItem
    mov D$edi &TRUE
    popad

EndP


































































