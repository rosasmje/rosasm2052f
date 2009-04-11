TITLE Dialog

 _______________________________________________________________________________________
 _______________________________________________________________________________________

DialogMenuComment:
;;
 Main at: 'InitDialogEdition' / 'EditDialogBoxProc' / 'EditedDialogBoxProc'
          'CloseDialogEdition'
          'HelpDialog' / 'HelpDialogProc'

'InstallHook' 'MouseProc' 'ShowDialogResult'


                                  Dialog Editor

 Menus in Dialogs are stored either as "0" (no menu) or as, for exemple, "FFFF 0D8F"
 (menu with ID 0D8F). When Win runs a Dialog, it read the ID according menu in resources.
 As the "runing resources" in RosAsm Dialog Editor is nothing but RosAsm resources itselves
 (and not the futur resources of the futur PE we are writing), we can't set the wished
 value in the edited dialog template. Instead, we have to "LoadMenuIndirectA" / "SetMenu"
 inside the "EditedDialogBoxProc". (So do we in 'WhatMenu' > 'WhatMenuProc' > 'SetTestMenu'
 routine). So, we have a difficult job to do with 2 menus IDs instead of one (and hide any
 in "EditedDialogBoxData"):

 - When an existing Dialog is loaded, we save the true menu ID in D$DialogMenuTrueID.
   Then we ask Win for a handle for the menu template (for "SetMenu")
   The Dialog box template, then, has menu set at "0"...
 - In the Dialog-Main-Edit-Control (Editor), we show to the user the 'true' ID
 - As Menu are usually choosen by "WhatMenu" routine, we store the 'runing ID' in
   'D$ActualMenutestID'.

 So:  >  ActualMenutestID = Menu ID used by RosAsm to activate a menu.
      >  DialogMenuTrueID = Menu ID really saved in Editied PE Resources.

 A similar problem arises for Dialog Class. As soon as a class is written by user inside
 the template, the Dialog would desappear from screen (as not registered by RosAsm...).
 So, have we to hide this record from the editor internal Template and restore it before
 saving either in ClipBoard or in Resources. As the record is a string -and not a number-
 i choose to simply save it in a string table ('ClassRecord'), to restored it at saving
 time.
;;
 _______________________________________________________________________________________
 _______________________________________________________________________________________
 _______________________________________________________________________________________

[DialogMenuTrueID: ?]

[TypeFace: 'Arial', 0    Helv: 'Helv' 0]

[MyFontHandle: ?]

CreateFontForDialogEdition:
    call 'GDI32.CreateFontA' 10, 5, 0, 0, 400, 0, 0, 0, 1,   ;  DEFAULT_CHARSET 1  OEM_CHARSET 255
                             0, 0, 0, 0, TypeFace
    mov D$MyFontHandle eax
ret

_______________________________________________________________________________________
_______________________________________________________________________________________

[MaxTemplateText 0FFFF]

[D_button 080  D_Edit 081  D_Static 082  D_ListBox 083  D_ScrollBar 084  D_ComboBox 085]

[ID_Ilist 101  ID_IpopUpMenu 102]

; 080=button / 081=Edit / 082=Static / 083=ListBox / 084=ScrollBar / 085=ComboBox

; This is the text default template that user can see in in the main editor list:

[NewDialogTemplateText: D$ 0] [ClassRecord: ? #20]

[DefaultDialogTemplateText:
 B$
 'D$ 90C408C2 ; Style' 0                 ; style
 'D$ 00000000 ; ExStyle' 0               ; extended style
 'U$ 0000 0000 0000 00DC 00C8 ; Dim' 0   ; control-number, x, y, width, hight
 '0 ;      no Menu' 0                    ; no menu >>> 0. If Menu > 0FFFF ID
 '"" 0 ; Class' 0                        ; class 0 > default
 '"New Dialog" 0 ; Title' 0              ; title
 '08 "Helv" 0 ; Font'  0 0 255           ; font

 DefaultDialogTemplateTextLen: len]

[CommentDialogStyle:  ' ; Style' 0
 CommentDialogDim:    ' ; Dim' 0
 CommentDialogMenu:   ' ; Menu' 0
 CommentDialogClass:  ' ; Class' 0
 CommentDialogTitle:  ' ; Title' 0
 CommentDialogFont:   ' ; Font'  0
 CommentControlStyle: ' ; Style' 0
 CommentControlDim:   ' ; Dim' 0
 CommentControlID:    ' ; ID' 0
 CommentControlClass: ' ; Class' 0
 CommentControlTitle: ' ; Title' 0
 CommentControlCData: ' ; No creation data' 0]


; Default Visible text in Editor when adding/inserting a new Control:

[NewDialogControlText: B$
 'D$ 50000000 ; Style' 0          ; style
 'D$ 00000000 ; ExStyle' 0        ; extended style
 'U$ 0000 0000 0038 0018 ; Dim' 0   ; x y w h
 '0000 ; ID' 0                      ; ID
 'FFFF 0080 ; Class' 0              ; Predefined class / 080=button
 '"New Control" 0 ; Title' 0        ; title
 '0 ; No creation data' 0 0 255     ; no creation data
 DefaultControlLenght: len]
; len = 07E 126 Octets // 'D$NewDialogTemplateText' = 010000 // 010000/07E = 520

; This is the template for edition (THE Editor):

[DialogBoxData:
 DialogStyle:
 D$ &WS_VISIBLE+&WS_THICKFRAME+&DS_SYSMODAL+&DS_SETFONT+&DS_3DLOOK+&DS_MODALFRAME+&WS_POPUP+&WS_CAPTION
 DialogExtStyle: 0

 ControlsNumber: U$ 1
 DialogX: 3
 DialogY: 3
 DialogW: 100
 DialogH: 340                       ; control-number, x, y, width, hight
    0                               ; no menu
    0                               ; class 0 > default
 DialogTitle: 'Dialogs Editor' 0    ; title
 DialogFontSize: 8  DialogFont: 'Helv' 0

; controls:
; main editor list:

D$ &WS_CHILD&WS_VISIBLE+&LBS_HASSTRINGS+&LBS_NOTIFY+&WS_VSCROLL+&WS_HSCROLL+&ES_AUTOVSCROLL+&ES_AUTOHSCROLL+&WS_BORDER;+&WS_THICKFRAME

    0                        ; style / ext.style
   U$ 2 2  96 75             ; x y w h
     ID_Ilist                ; ID
     0FFFF                   ; Predefined class
     D_ListBox               ; List control for main editor
    '  ' 0 0                 ; button title
     0]


[DialogBoundingRectangle: DBRX1: ?  DBRY1: ?  DBRX2: ?  DBRY2: ?]
[BaseUnits: BaseUnitX1: ?   BaseUnitY1: ?   BaseUnitX2: ?   BaseUnitY2: ?]

[IDFstring: B$ "

    You must set the ID number of last created control.

    Unlike the Menu Editor, the Dialog Editor will not do this for you    
    and let you free of your equates choices.

    1) You are allowed to give the same ID number to several controls

    2) The Dialog Editor have no way to save your Equates Names
        nor to set them for you as the names MUST be unique.

    So >>> paper / pencil...

    ", 0]

;;
 Of no use now, but of some interrest:

 I wrote this at a time when the two dialogs (editior and edited) were redrawn after
 each modification. Now, only edited dialog is redrawn and all this is of no more use.
 But, as it have been some work..., i let it here (in case of need). This is a trick
 that gives the base unit (absolute need if we want to know where to set a dialog, but
 the direct function do NOT exist in Win api (!!!!!!!!)).
;;

;SaveDialogUserPosition:
;    ; MapDialog do this to BaseUnits values:
;    ;
;    ; left   = (left   * baseunitX) / 4
;    ; right  = (right  * baseunitX) / 4
;    ; top    = (top    * baseunitY) / 8
;    ; bottom = (bottom * baseunitY) / 8
;    ;
;    ; i only want to know baseUnits. So:
;
;    mov D$BaseUnitX1 4, D$BaseUnitY1 8, D$BaseUnitX2 0, D$BaseUnitY2, 0     ; <<< the
;    call 'User32.MapDialogRect' D$Adressee BaseUnits                        ; <<< trick
;
;    call 'User32.GetWindowRect' D$Adressee DialogBoundingRectangle
;   ; call 'User32.GetDialogBaseUnits'   ; of no need
;
;   ; dialogUnitX = (pixelX * 4) / baseunitX
;   ; dialogUnitY = (pixelY * 8) / baseunitY
;
;    mov eax D$DBRX1, edx 0
;    shl eax 2 | div D$BaseUnitX1
;    mov W$DialogX ax
;
;    mov eax D$DBRY1, edx 0
;    shl eax 3 | div D$BaseUnitY1
;    mov W$DialogY ax
;ret


[PreviousControlID: 0FFFF]

; If user selected a blank separator line, we do not add > we insert:

AddOneControl:
    If W$PreviousControlID = 0
        call 'USER32.MessageBoxA' D$hwnd, IDFstring, CCFtitle, &MB_SYSTEMMODAL | ret
    Else
        mov W$PreviousControlID 0            ; for next time test (filled -or not- by user)
    End_If

    call 'User32.SendMessageA' D$DialogListHandle &LB_GETCURSEL eax 0
    mov D$DialogListIndex eax

    On D$DialogListIndex > 0FFFF, mov D$DialogListIndex 0      ; no sel. > OK  > add

    mov eax D$DialogListIndex
    While eax >= Line_empty+1 | sub eax Line_empty+1 | End_While

    Push eax

    If eax = Line_empty
      ; Insert 'DefaultControlLenght' of 'NewDialogControlText':
        dec D$DefaultControlLenght                                  ; no 255 end mark
        call SearchDialogLine | inc edi | mov edx edi               ; > start of next control
        mov al 255, ecx MaxTemplateText | repne scasb | dec edi     ; actual end
        mov esi edi | add edi D$DefaultControlLenght                ; new end
        mov ecx esi | sub ecx edx | inc ecx                         ; count moveable chars
        std | rep movsb | cld                                       ; make room
        mov edi edx, esi NewDialogControlText                       ; ready for copy
        mov ecx D$DefaultControlLenght                              ; how much
        rep movsb                                                   ; copy default control
        inc D$DefaultControlLenght                                  ; restore full lenght
    Else
      ; add:
        mov edi D$NewDialogTemplateText, al 255, ecx MaxTemplateText
        repne scasb | dec edi
        mov esi NewDialogControlText, ecx D$DefaultControlLenght
        rep movsb
    End_If

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
    pop eax

    If eax = Line_empty                                             ; if "insert":
        call ScrollToInsertedControl
    Else                                                            ; if "add":
        call ScrollDownToLastControl
    End_If

    mov edi D$NewDialogTemplateText, al 0, ecx 200 | repne scasb
    mov al '0' | repne scasb | add edi 2 | inc B$edi
    On B$edi > '9', add B$edi 7
    .If B$edi > 'F'
        mov B$edi '0' | inc B$edi-1
        On B$edi-1 > '9', add B$edi-1 7
        If B$edi-1 > 'F'
            mov B$edi-1 '0' | inc B$edi-2
            On B$edi-2 > '9', add B$edi-2 7       ; >>> up to FFF (4095 controls -enough?-)
        End_If
    .End_If
ret


ShowDialogResult:
  ; Under 98, impossible to destroy the old Window *after* having created the new one.
  ; (Works fine under 2000... too bad... ).

    mov eax D$EditedDialogBoxData
    push D$eax
        and D$eax 0FFF_FFFF | or D$eax &WS_VISIBLE__&WS_POPUP

        call 'User32.DestroyWindow' D$EditedDialogHandle
        call 'User32.CreateDialogIndirectParamA' D$hinstance, D$EditedDialogBoxData,
                                                 D$hwnd, EditedDialogBoxProc, 0
        mov D$EditedDialogHandle eax

        mov eax D$EditedDialogBoxData
    pop D$eax
ret

________________________________________________________________________________________

; This is the default 'StartUp' Dialog Data you see when cliclng on [New Dialog]:
; WS_THICKFRAME turns it 'sizeable' (for editing only, striped from savings).

[EditedDialogBoxData: D$ ?]

[DefaultEditedDialogBoxData:
 D$ &WS_VISIBLE+&WS_THICKFRAME+&DS_SETFONT+&DS_SYSMODAL+&DS_MODALFRAME+&WS_POPUP+&WS_CAPTION+&DS_CENTER
    0                       ; style / extended style
 U$ 0 0 0 220 200           ; control-number, x, y, width, hight
    0                       ; no menu
    0                       ; class 0 > default
    "New Dialog" 0          ; title
    8 "Helv" 0              ; font

 DefaultEditedDialogBoxDataLenght: len]


;;
 Dialog proc for the result of edition. Clicking on a control select the dim record
 of the according Main list template part. Works only with controls that send
 some WM_COMMAND message > doesn't work on static controls. Another problem is that
 selection must not run at initialisation time > Nothing works at all until user didn't
 click on a Button. Exemple: if user first click on an Edit Control, nothing happends.
 see if we can do better later.
;;

[DialogEditorHandle: ?    EditedDialogHandle: ?    EditionInitFlag: ?]
[ArrowCursor: ?]

Proc EditedDialogBoxProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

  ; while opening and closing this dialog to show work progress to user, after
  ; some time, the menu seems destroyed (must be by Win -as bound to dialog-)
  ; > so, reinitialise. But then, the menu (visible) do not work any more
  ; in the editor. Saved data work fine....
    ...If D@Message = &WM_INITDIALOG
        move D$EditedDialogHandle D@Adressee
        call MakeControlIDList
        mov B$EditionInitFlag &TRUE

        call GetDialoBaseUnits

        If D$ActualMenutestID <> 0
            call 'User32.DestroyMenu' D$ActualMenutestID
            mov eax D$MenuListPtr | add eax 4
            call 'User32.LoadMenuIndirectA' D$eax
            mov D$ActualMenutestID eax
            call 'User32.SetMenu' D@Adressee eax
        End_If

  ;  ...Else_If D@Message = &WM_CLOSE
  ;      call 'User32.DestroyWindow' D@Adressee

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


[Text12345: '1234567890', 0] [DialogUnitX: ?    DialogUnitY: ?]

UpdateControlDims:
    mov eax D$Control.rcNormalPosition.left, edx 0
    shl eax 2 | div D$BaseUnitX1
    On edx >= 5, inc eax
    mov edi Text12345 | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$DialogControlsHandles+4, &WM_SETTEXT, 0, Text12345
    mov ecx D$DialogControlsHandles+4 | call WriteDimOnly

    mov eax D$Control.rcNormalPosition.Top, edx 0
    shl eax 3 | div D$BaseUnitY1
    On edx >= 5, inc eax
    mov edi Text12345 | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$DialogControlsHandles+12+4, &WM_SETTEXT, 0, Text12345
    mov ecx D$DialogControlsHandles+12+4 | call WriteDimOnly

    mov eax D$Control.rcNormalPosition.Right, edx 0
    sub eax D$Control.rcNormalPosition.left
    shl eax 2 | div D$BaseUnitX1
    On edx >= 5, inc eax
    mov edi Text12345 | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$DialogControlsHandles+24+4, &WM_SETTEXT, 0, Text12345
    mov ecx D$DialogControlsHandles+24+4 | call WriteDimOnly

    mov eax D$Control.rcNormalPosition.Bottom, edx 0
    sub eax D$Control.rcNormalPosition.Top
    shl eax 3 | div D$BaseUnitY1
    On edx >= 5, inc eax
    mov edi Text12345 | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$DialogControlsHandles+36+4, &WM_SETTEXT, 0, Text12345
    mov ecx D$DialogControlsHandles+36+4 | call WriteDimOnly

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret


;;
  For the Dialog itself, only Sizing modifications are allowed. We only set the Hight
  and width.
;;

UpdateDialogDims:
    mov B$ModifiedControl &FALSE

    call 'USER32.GetClientRect' D$EditedDialogHandle, Control.rcNormalPosition.left

  ; Result of '.GetClientRect' >>> right > Weidth // Bottom > Hight:
    mov eax D$Control.rcNormalPosition.Right, edx 0
    shl eax 2 | div D$BaseUnitX1
    On edx >= 5, inc eax
    mov edi Text12345 | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$DialogControlsHandles+24+4, &WM_SETTEXT, 0, Text12345
    mov ecx D$DialogControlsHandles+24+4 | call WriteDimOnly

    mov eax D$Control.rcNormalPosition.Bottom, edx 0
    shl eax 3 | div D$BaseUnitY1
    On edx >= 5, inc eax
    mov edi Text12345 | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$DialogControlsHandles+36+4, &WM_SETTEXT, 0, Text12345
    mov ecx D$DialogControlsHandles+36+4 | call WriteDimOnly

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret


;;
  This is the trick for retrieving the Values needed for translating Screen Coordinates
  into DialogBoxes coordinates. It take account of the Font.
  
  For X Dim, the formula is:   shl eax 2 | div D$BaseUnitX1
  For Y Dim, the formula is:   shl eax 3 | div D$BaseUnitY1
;;

GetDialoBaseUnits:
    mov D$BaseUnitX1 4, D$BaseUnitY1 8, D$BaseUnitX2 0, D$BaseUnitY2, 0
    call 'User32.MapDialogRect' D$EditedDialogHandle BaseUnits
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

HookComments:
;;
  'InstallHook' is called from the Init of 'ReInitDialogEdition'. Its purpose is to
  install a Mouse Hook Procedure, 'MouseProc', that enable the Direct Mouse Edition
  of Dialog Controls Coordinates.
  
  The Coordinates of the target Cotrol are directely modified by 'MouseProc' and, for
  avoiding blinckering they are made effective only when user release the Button.
  
  Left Button is for Pos. Right Button is for Dim.
  
  When moving a Control, the Cursor is confined to the Dialog Client Rectangle.
  (not when re-sizing).
  
  As this would not mean a thing, if user left-Click on the Dialog (instead of a Control)
  Right-Click is silently subtituted, in that case.
  
  'KickDims' is run when the user releases the Mouse Button. It first select the proper
  ListBox Item for Dims, according with the target Control or with the Dialog. Then it
  runs 'SetDialogTools', that shows the Size and Pos Edition Controls in the Editor,
  and finaly calls either for 'UpdateControlDims' or for 'UpdateDialogDims'.
  
  When drawing a Control, it is replaced by a Static Control with SS_BLACKFRAME Style.
  This is for viewing, for example, the real sizes of Radio Buttons.
;;

[Control:
 Control.iLength: D$ len
 Control.flags: D$ 0
 Control.showCmd: D$ 0
 Control.ptMinPosition.x: D$ 0
 Control.ptMinPosition.y: D$ 0
 Control.ptMaxPosition.x: D$ 0
 Control.ptMaxPosition.y: D$ 0
 Control.rcNormalPosition.left: D$ 0
 Control.rcNormalPosition.top: D$ 0
 Control.rcNormalPosition.right: D$ 0
 Control.rcNormalPosition.bottom: D$ 0]

[oControl:
 oControl.iLength: D$ len
 oControl.flags: D$ 0
 oControl.showCmd: D$ 0
 oControl.ptMinPosition.x: D$ 0
 oControl.ptMinPosition.y: D$ 0
 oControl.ptMaxPosition.x: D$ 0
 oControl.ptMaxPosition.y: D$ 0
 oControl.rcNormalPosition.left: D$ 0
 oControl.rcNormalPosition.top: D$ 0
 oControl.rcNormalPosition.right: D$ 0
 oControl.rcNormalPosition.bottom: D$ 0]

[hHook: ?  HookedHandle: ?
 UserClickedOnControl: ?    UserRightClickedOnControl: ?
 FromPointX: ?    FromPointY: ? ? ?
 ClickFromPointX: ?    ClickFromPointY: ?
 ModifiedControl: ?]

[EditedDialogRectangle: ? ? ? ?]

; 'HookComments'.


Proc MouseProc:
    Arguments @nCode, @wParam, @lParam
;;
  Win Doc: 
  
  * If nCode is less than zero, the hook procedure must pass the message 
  to the CallNextHookEx function without further processing and should return the 
  value returned by CallNextHookEx. 
  
  * To enable the system to process the message, the return value must be zero. 
  To discard the message, the return value must be a nonzero value. 
  
  * If the 'CallNextHookEx' function succeeds, the return value is the value returned
  by the next hook procedure in the chain. The current hook procedure must also
  return this value.
  __________________
  
  In fact, under 98, if the MouseProc processes the Message, it *must* return a non
  zero value. Otherwise, the App hangs randomaly.
;;
    pushad

    ..If D@nCode > 07FFF_FFFF
        jmp L9>>

    ..Else_If D$EditedDialogHandle = 0
      ; All of this should be no use. Just added security...
L0:     mov al B$UserClickedOnControl | or al B$UserRightClickedOnControl
        If al <> &FALSE
            mov B$UserClickedOnControl &FALSE, B$UserRightClickedOnControl &FALSE
            call 'USER32.ClipCursor' &NULL
        End_If
        jmp L9>>

    ..Else
        mov al B$UserClickedOnControl | or al B$UserRightClickedOnControl
        .If al = &FALSE
            mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            mov D$FromPointX eax, D$FromPointY ebx
            mov D$ClickFromPointX eax, D$ClickFromPointY ebx
            call 'USER32.WindowFromPoint' eax, ebx | On eax = &FALSE, jmp L0<<
            If eax <> D$EditedDialogHandle
                call 'USER32.IsChild' D$EditedDialogHandle, eax | On eax = &FALSE, jmp L0<<
            End_If
        .End_If

    ..End_If
    _______________________________

    If D@wParam = &WM_LBUTTONDOWN
        call SearchForWhatControl

    Else_If D@wParam = &WM_RBUTTONDOWN
        call SearchForWhatControl

    End_If

    ...If D$HookedHandle <> 0
        ..If D@wParam = &WM_LBUTTONDOWN
            mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            mov D$FromPointX eax, D$FromPointY ebx
            mov D$ClickFromPointX eax, D$ClickFromPointY ebx

            mov eax D$EditedDialogHandle
            If D$HookedHandle = eax
                mov B$UserRightClickedOnControl &TRUE
            Else
                mov B$UserClickedOnControl &TRUE
            End_If

            call 'USER32.GetWindowPlacement' D$HookedHandle, Control

            call ClipCursorInDialog

            popad | mov eax &TRUE | ExitP

        ..Else_If D@wParam = &WM_RBUTTONDOWN
            mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            mov D$FromPointX eax, D$FromPointY ebx
            mov D$ClickFromPointX eax, D$ClickFromPointY ebx

            mov B$UserRightClickedOnControl &TRUE

            call 'USER32.GetWindowPlacement' D$HookedHandle, Control

            call ClipCursorInSelected

            popad | mov eax &TRUE | ExitP

        ..Else_If D@wParam = &WM_LBUTTONUP
            mov B$UserClickedOnControl &FALSE, B$UserRightClickedOnControl &FALSE
            call KickDims
            mov D$HookedHandle 0
            call 'USER32.ClipCursor' &NULL

            popad | mov eax &TRUE | ExitP

        ..Else_If D@wParam = &WM_RBUTTONUP
L1:         mov B$UserRightClickedOnControl &FALSE, B$UserClickedOnControl &FALSE
            call KickDims
            mov D$HookedHandle 0
            call 'USER32.ClipCursor' &NULL

            popad | mov eax &TRUE | ExitP

        ..Else_If D@wParam = &WM_MOUSEMOVE
            mov ebx D@lParam, eax D$ebx, ebx D$ebx+4
            mov D$FromPointX eax, D$FromPointY ebx

            .If B$UserClickedOnControl = &TRUE
                mov eax D$FromPointX | sub eax D$ClickFromPointX
                add D$Control.rcNormalPosition.left eax
                add D$Control.rcNormalPosition.right eax
                add D$ClickFromPointX eax

                mov ebx D$FromPointY | sub ebx D$ClickFromPointY
                add D$Control.rcNormalPosition.Top ebx
                add D$Control.rcNormalPosition.Bottom ebx
                add D$ClickFromPointY ebx

                call 'USER32.SetWindowPlacement' D$HookedHandle, Control
                mov B$ModifiedControl &TRUE

            .Else_If B$UserRightClickedOnControl = &TRUE
                mov eax D$FromPointX | sub eax D$ClickFromPointX
                add D$Control.rcNormalPosition.right eax
                add D$ClickFromPointX eax

                mov eax D$FromPointY | sub eax D$ClickFromPointY
                add D$Control.rcNormalPosition.Bottom eax
                add D$ClickFromPointY eax

                call 'USER32.SetWindowPlacement' D$HookedHandle, Control
                mov B$ModifiedControl &TRUE

            .End_If

            popad | mov eax &TRUE | ExitP

        ..End_If

    ...End_If

L9: popad
    call 'USER32.CallNextHookEx' D$hHook D@nCode D@wParam D@lParam
EndP


; User Left-Clicked on the Edited Dialog. The 'Control' Structure contains the
; 'HookedHandle' Window dimentions.

ClipCursorInDialog:
    call 'USER32.GetClientRect' D$EditedDialogHandle RECT
    call 'USER32.ClientToScreen' D$EditedDialogHandle RECTleft
    call 'USER32.ClientToScreen' D$EditedDialogHandle RECTright

    mov eax D$HookedHandle
    If eax = D$EditedDialogHandle
      ; Target = Dialog >>> Bottom-Right limits = Screen limits, (for resizing Dialog):
        call 'USER32.GetSystemMetrics' &SM_CXSCREEN | mov D$RECTright eax
        call 'USER32.GetSystemMetrics' &SM_CYSCREEN | mov D$RECTbottom eax

    Else
      ; Target = Control
      ; >>> Bottom-Right limits = Dialog limits
      ; >>> Top-Left limits = Dialog limits - (Mouse Pos - Control Top-Left)
        move D$PointX D$Control.rcNormalPosition.left
        move D$PointY D$Control.rcNormalPosition.top
        call 'USER32.ClientToScreen' D$EditedDialogHandle POINT
        mov eax D$FromPointX | sub eax D$PointX
        mov ebx D$FromPointY | sub ebx D$PointY
        add D$RECTleft eax | add D$RECTtop ebx

    End_If

    call 'USER32.ClipCursor' RECT
ret


[POINT: PointX: ? PointY: ?] [EditedDialogX: ?   EditedDialogY: ?]
[SlideBarX: ?    SlideBarY: ?]

ClipCursorInSelected:
    mov eax D$HookedHandle | On eax = D$EditedDialogHandle, jmp ClipCursorInDialog

  ; Dialog Dims:
    call 'USER32.GetClientRect' D$EditedDialogHandle RECT
    move D$EditedDialogX D$RECTleft, D$EditedDialogY D$RECTtop
    call 'USER32.ClientToScreen' D$EditedDialogHandle EditedDialogX

  ; Control Left-Top Dims:
    move D$RECTleft D$Control.rcNormalPosition.left
    move D$RECTtop D$Control.rcNormalPosition.top
    call 'USER32.ClientToScreen' D$EditedDialogHandle RECTleft
    call 'USER32.ClientToScreen' D$EditedDialogHandle RECTright

  ; Target = Control >>> Limit resizing to 25/25:
    move D$PointX D$Control.rcNormalPosition.right
    move D$PointY D$Control.rcNormalPosition.bottom
    call 'USER32.ClientToScreen' D$EditedDialogHandle POINT
;;
  For sizing, if the user Right-Clicks rigth upon the lower right corner, OK, but, if
  he Clicks, say, in the middle of the Control, we have to substract that 'Half-Size'
  of the control from the upper-left limit of the ClipCursor call, to let the modification
  go down to the minimum allowed size:
;;
    mov ebx D$EditedDialogX
    mov eax D$PointX | sub eax D$FromPointX
    sub eax D$MinimumX
    sub D$RECTleft eax ;| add D$RECTleft 25
  ; Don't let the Mouse go outside the Dialog, in cases of compensation.
    On D$RECTleft < ebx, mov D$RECTleft ebx

    mov ebx D$EditedDialogY
    mov eax D$PointY | sub eax D$FromPointY
    sub eax D$MinimumY
    sub D$RECTtop eax ;| add D$RECTtop 25
  ; Don't let the Mouse go outside the Dialog, in cases of compensation.
    On D$RECTtop < ebx, mov D$RECTtop ebx

    call 'USER32.ClipCursor' RECT
ret

____________________________________________________________________________________________

; 'HookComments'

Proc InstallHook:
    call 'KERNEL32.GetCurrentThreadId'
    call 'USER32.SetWindowsHookExA' &WH_MOUSE, MouseProc, &NULL, eax
    mov D$hHook eax
EndP


Proc UninstallHook:
    On D$hHook <> 0, call 'USER32.UnhookWindowsHookEx' D$hHook
    mov D$hHook 0
EndP


;;
  Force the New Dims and Pos (by Mouse action) to be displayed and updated (by calling
  either 'UpdateControlDims' or 'UpdateDialogDims':
;;

KickDims:
    mov eax D$HookedHandle

    .If eax <> D$EditedDialogHandle
        call 'USER32.GetDlgCtrlID' D$HookedHandle

        mov edi ControlsIDlist, ecx ControlsIDlistdWords
        repne scasd

L1:     If ecx > 0
            mov eax ControlsIDlistdWords
            sub eax ecx                         ; eax = ID list position (in ControlsIDlist)
            mov ecx Line_empty+1 | imul eax ecx ; what control dim
            push eax
            call 'User32.SendMessageA' D$DialogListHandle &LB_SETTOPINDEX eax 0 ; Pos
            pop eax | add eax Line_Dim
            call 'User32.SendMessageA' D$DialogListHandle &LB_SETCURSEL eax 0   ; select
            call SetDialogTools
            On B$ModifiedControl = &TRUE, call UpdateControlDims
        End_If

    .Else
        call 'User32.SendMessageA' D$DialogListHandle &LB_SETTOPINDEX 0 0
        call 'User32.SendMessageA' D$DialogListHandle &LB_SETCURSEL Line_Dim 0

        call SetDialogTools

        On B$ModifiedControl = &TRUE, call UpdateDialogDims
    .End_If
ret

____________________________________________________________________________________________

;;
  User has Clicked down on a Control in the Edited Dialog. We search in 
  'D$EditedDialogBoxData' for what Control.
  
  D$FromPointX, D$FromPointY hold the Mouse coordinates.
;;

[HookedID: ?
 HookedControlStyle: ?    HookedControlClass: ?][ClassTail: ? #10]
[HookedControlStylePtr: ?    HookedControlClassPtr: ?
 ClassNameInside: ?]

SearchForWhatControl:
    push D$FromPointX, D$FromPointY

        call 'USER32.GetWindowRect' D$EditedDialogHandle EditedDialogRectangle

        mov eax D$FromPointX
        cmp eax D$EditedDialogRectangle | jb L9>>
        cmp eax D$EditedDialogRectangle+8 | ja L9>>
        mov eax D$FromPointY
        cmp eax D$EditedDialogRectangle+4 | jb L9>>
        cmp eax D$EditedDialogRectangle+12 | ja L9>>

        mov eax D$EditedDialogRectangle | sub D$FromPointX eax
        mov eax D$EditedDialogRectangle+4 | sub D$FromPointY eax
        mov ebx D$EditedDialogRectangle+12 | sub ebx eax
        push ebx
        call 'USER32.GetClientRect' D$EditedDialogHandle EditedDialogRectangle
        pop ebx
        sub ebx D$EditedDialogRectangle+12 | sub D$FromPointY ebx


        mov D$BaseUnitX1 4, D$BaseUnitY1 8, D$BaseUnitX2 0, D$BaseUnitY2, 0     ; <<< the

        call 'User32.MapDialogRect' D$EditedDialogHandle BaseUnits               ; <<< trick

        mov eax D$FromPointX, edx 0 | shl eax 2 | div D$BaseUnitX1 | mov D$FromPointX eax
        mov eax D$FromPointY, edx 0 | shl eax 3 | div D$BaseUnitY1 | mov D$FromPointY eax

        mov D$HookedID 0, D$HookedHandle 0

        mov esi D$EditedDialogBoxData

        add esi 8  ; Style // Extended Style.

        mov eax 0 | lodsw | mov ecx eax | cmp ecx 0 | je L8>>   ; How many Controls
        add esi (4*2)                                           ; Dims
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Menu.
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Class.
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Title.
        While W$esi <> 0 | add esi 2 | End_While | add esi 2    ; Font.

        Align_on 4, esi

      ; Parse Controls (last good one = top one at that coordinate):

L0:     add esi 8  ; Style // Extended Style.

      ; Here are the searched Dims: X, Y, W, H (Words).
        mov ax W$esi   | cmp D$FromPointX eax | jb L2>>
        mov ax W$esi+2 | cmp D$FromPointY eax | jb L2>>
        mov ax W$esi+4 | add ax W$esi | cmp D$FromPointX eax | ja L2>
        mov ax W$esi+6 | add ax W$esi+2 | cmp D$FromPointY eax | ja L2>
            move W$HookedID W$esi+8

            mov eax esi | sub eax 8 | mov D$HookedControlStylePtr eax
            move D$HookedControlStyle D$eax

            add eax 18 | mov D$HookedControlClassPtr eax
            move D$HookedControlClass D$eax

            mov B$ClassNameInside &FALSE
            If W$eax <> 0FFFF
                push esi
                    mov esi eax, edi HookedControlClass
                    While W$esi <> 0
                        movsw
                    End_While
                    movsw
                pop esi
                mov B$ClassNameInside &TRUE
            End_If

            mov eax 0

L2:     call NextControl | dec ecx | jnz L0<<

        If D$HookedID <> 0
            push ebx | call GetSmallerReSize | pop ebx

            mov ebx D$HookedControlStylePtr
            mov D$ebx &BS_PUSHBUTTON__&BS_BITMAP__&BS_FLAT__&WS_CHILD__&WS_VISIBLE
            mov ebx D$HookedControlClassPtr, D$ebx 080_FFFF ; Button.
            On B$ClassNameInside = &TRUE, call AdjustTitle
            call ShowDialogResult
            call 'USER32.GetDlgItem' D$EditedDialogHandle, D$HookedID
            mov D$HookedHandle eax

        Else
L8:         move D$HookedHandle D$EditedDialogHandle
            mov D$MinimumX 200, D$MinimumY 50

        End_If

L9:     pop D$FromPointY, D$FromPointX
ret


[MinimumX: ?    MinimumY: ?]

GetSmallerReSize:
    mov D$MinimumX 10, D$MinimumY 10

    mov esi D$HookedControlStylePtr

    .If D$esi+18 = 0_81_FFFF  ; +18 >>> Class = 0FFFF 081 >>> Edit Control
        test D$esi &WS_VSCROLL | jz L1>
            add D$MinimumX 30
L1:     test D$esi &WS_HSCROLL | jz L2>
            add D$MinimumY 30
    .Else_If D$esi+18 = 0_83_FFFF  ; +18 >>> Class = 0FFFF 083 >>> ListBox
        add D$MinimumX 30 | add D$MinimumY 30
    .Else_If D$esi+18 = 0_85_FFFF  ; +18 >>> Class = 0FFFF 083 >>> ComboBox
        add D$MinimumX 30 | add D$MinimumY 30
    .End_If

L2: ret



; jump over Dims and search for end of Control Data. Pointer in esi:

NextControl:
  ; esi > Dims: X, Y, W, H (Words)
  ;             ID
  ;             0FFFF 0080 ou "msctls_progress32", 0
  ;             "Title", 0
  ;             0
    add esi 10

    If W$esi = 0FFFF
        add esi 4
    Else
        While W$esi <> 0 | add esi 2 | End_While | add esi 2
    End_If

    While W$esi <> 0 | add esi 2 | End_While | add esi 4

    Align_on 4, esi
ret


; If Class Name, recover the whole length with a simulated Title (the old part of the
; Classe Name, is simply made one sigle Title with the real Title, by inserting an 'X'
; instead of the zero Class string termination:

AdjustTitle:
    mov ebx D$HookedControlClassPtr
    While W$ebx <> 0 | add ebx 2 | End_While | mov B$ebx 'X'
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; This is for the edited dialog proc. Used to know what control user clicked on, and
; after, make the according "dim" controls appear in the editor:

[ControlsIDlist: ? #1000] [ControlsIDlistdWords 1000]

MakeControlIDList:
    mov edi ControlsIDlist, eax 0, ecx ControlsIDlistdWords | rep stosd ; clear ID table
    mov ebx ControlsIDlist
    mov edi D$EditedDialogBoxData
    mov edx 0, dx W$edi+8                       ; number of controls in edx
    On edx = 0, ret
    add edi 22
    mov ax 0, ecx 100 | repne scasw
    mov ecx 100 | repne scasw                   ; edi > end of dialog data
L0: test edi 00_11 | jz L1>
        add edi 2                               ; > start of Control data
L1: On edx = 0, jmp L9>
    add edi 16                                  ; > ID number
    move W$ebx W$edi | add ebx 4 | add edi 2
    If W$edi = 0FFFF                            ; type by number
        add edi 4
    Else
        mov ax 0, ecx 100 | repne scasw         ; type by name
    End_If
    mov ax 0, ecx 100 | repne scasw             ; control title
    add edi 2                                   ; > end of Control data
    dec edx | jmp L0<
L9: ret

_______________________________________________________________________________

; Menu for Dialog-Edition Dialog:

[DialogMenuHandle: ? DialogPopUpMenuTemplate: ? DialogPopUpMenuExit: ?]

[DETemplates: 'Templates', 0
    DEadd: 'Add Control' 0
    DEdel: 'Delete Control' 0

    DEdelMenu: 'Delete Menu', 0
    DEreplaceMenu: 'Replace Menu', 0

    DEreset: 'New Dialog' 0

 DEExit: 'Exit', 0
    DEsaveToResources: 'Save to Resources and Exit', 0
    DEsaveToClipBoard: 'Save to Clipboard and Exit', 0
    DEsaveToDisk: 'Save to Disk and Exit', 0

    DEabort: 'Abort', 0

 DEhelp: 'Help', 0]

[ID_DETemplates 500
    ID_DEadd 501
    ID_DEdel 502

    ID_DEdelMenu 503
    ID_DEreplaceMenu 504

    ID_DEreset 505

 ID_DEExit 510
    ID_DEsaveToResources 511
    ID_DEsaveToClipBoard 512
    ID_DEsaveToDisk 513

    ID_DEabort 514

 ID_DEhelp 520]

; Creates and Dispatch the menu for Dialog Edition:

CreateDialogMenu:
  ; Main Menu:
    call 'USER32.CreateMenu' | mov D$DialogMenuHandle eax

  ; Templates PopUp:
    call 'USER32.CreatePopupMenu' | mov D$DialogPopUpMenuTemplate eax

    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEadd, DEadd
    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEdel, DEdel
    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, &MF_SEPARATOR, &NULL, &NUll
    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEdelMenu, DEdelMenu
    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEreplaceMenu, DEreplaceMenu
    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, &MF_SEPARATOR, &NULL, &NUll
    call 'USER32.AppendMenuA' D$DialogPopUpMenuTemplate, 0, ID_DEreset, DEreset

    call 'USER32.InsertMenuA' D$DialogMenuHandle, 0,
                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                              D$DialogPopUpMenuTemplate, DETemplates

  ; Exit PopUp:
    call 'USER32.CreatePopupMenu' | mov D$DialogPopUpMenuExit eax

    call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEsaveToResources, DEsaveToResources
    call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEsaveToClipBoard, DEsaveToClipBoard
    call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEsaveToDisk, DEsaveToDisk
    call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, &MF_SEPARATOR, &NULL, &NUll
    call 'USER32.AppendMenuA' D$DialogPopUpMenuExit, 0, ID_DEabort, DEabort

    call 'USER32.InsertMenuA' D$DialogMenuHandle, 1,
                              &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                              D$DialogPopUpMenuExit, DEExit

  ; Menu Help:
    call 'USER32.AppendMenuA' D$DialogMenuHandle, 0, ID_DEhelp, DEhelp

    call 'USER32.SetMenu' D$DialogEditorHandle, D$DialogMenuHandle
ret

_______________________________________________________________________________
________________________________________________________________________________


FillDialogListBox:
    ; preserv Pos and Selection
    call 'User32.SendMessageA' D$DialogListHandle &LB_GETTOPINDEX D$DialogListIndex 0
    push eax

    call 'User32.SendMessageA' D$DialogListHandle &LB_GETCURSEL D$DialogListIndex 0
    push eax

    call 'User32.SendMessageA' D$DialogListHandle &LB_RESETCONTENT 0 0     ; makes list empty

    mov edi D$NewDialogTemplateText

L1: push edi
      call 'User32.SendMessageA' D$DialogListHandle &LB_ADDSTRING  0  edi   ; fill
    pop edi
    mov al 0, ecx 200 | repne scasb
    cmp B$edi 255 | jne L1<

    ; restore Selection and Pos:
    pop ebx, eax
    push ebx
    call 'User32.SendMessageA' D$DialogListHandle &LB_SETTOPINDEX eax 0
    pop eax
    call 'User32.SendMessageA' D$DialogListHandle &LB_SETCURSEL eax 0
L9: ret


; Restore end mark, in case of second run:

ResetDialogListBox:
    mov edi D$NewDialogTemplateText, ecx Line_empty+1, al 0

L0: push ecx
        mov ecx 200 | repne scasb
    pop ecx
    loop L0<

    mov B$edi 255
ret

____________________________________________________________________________________
___________________________________________________________________________________

; Main routines for dialog edition.
_____________________________________________________________________________________
_____________________________________________________________________________________

ReleaseDialogMemories:
    VirtualFree D$NewDialogTemplateText, D$EditedDialogBoxData
ret


InitDialogMemory:
    VirtualFree D$NewDialogTemplateText
    VirtualAlloc NewDialogTemplateText 010000

    VirtualFree D$EditedDialogBoxData
    VirtualAlloc EditedDialogBoxData 010000
ret


; missing the &ICC_LINK_CLASS ?
InitDialogEdition:

    call 'ComCtl32.InitCommonControlsEx' CodeAddressFormClassName@Init_All_Common_Controls ; added by Guga

    If B$OnDialogEdition = &TRUE
        Beep | ret
    End_If
    call InitDialogMemory

    mov edi D$NewDialogTemplateText,
        esi DefaultDialogTemplateText,
        ecx D$DefaultDialogTemplateTextLen
    rep movsb
    mov D$edi 0FFFFFFFF                          ; End mark

    mov edi D$EditedDialogBoxData,
        esi DefaultEditedDialogBoxData,
        ecx D$DefaultEditedDialogBoxDataLenght
    rep movsb

    mov edi DialogList, eax 0, ecx 300
    repne scasd | sub edi 4
    mov D$DialogListPtr edi

    mov D$DialogMenutrueID 0, D$ActualMenutestID 0

ReInitDialogEdition:
    call InstallHook

    call FromTextToBinTemplate
;;
 Little difficulty: User can check either "POPUP" or "CHILD". Results visible dialog
 must remain "POPUP/VISIBLE" because created with "CreateDialogIndirectParamA"
 (either "CHILD" or not "VISIBLE" would make it unvisible for edition. So do we
 save true Dialog Style value and set the fitting one for edition:
;;
    mov eax D$EditedDialogBoxData
    push D$eax
        and D$eax 0FFF_FFFF | or D$eax 0_9000_0000

        call 'User32.CreateDialogIndirectParamA' D$hinstance, D$EditedDialogBoxData,
                                                 D$EditWindowHandle, EditedDialogBoxProc, 0
        mov D$EditedDialogHandle eax

        call 'User32.DialogBoxIndirectParamA' D$hinstance,      ; "create..." > modeless
                    DialogBoxData, 0, EditDialogBoxProc, 0      ; "Dialog..." > modal
      ; Editor Handle in 'DialogEditorHandle'.

        mov eax D$EditedDialogBoxData
    pop D$eax

    mov W$PreviousControlID 0FFFF

    call UninstallHook

    call SortDialogs
ret


[DoubleID: B$ ? #20]

SortDialogs:
  ; ID / Ptr / Size // ...
L0: mov esi DialogList, edi esi | add edi 12

    While D$edi <> 0
        mov eax D$esi
        If eax > D$edi
            Exchange D$esi D$edi
            Exchange D$esi+4 D$edi+4
            Exchange D$esi+8 D$edi+8 | jmp L0<
        End_If
        add esi 12 | add edi 12
    End_While
ret

; Verify that all Dialogs have unique IDs:

[SameIdAllowed: ?]

Proc CheckNotUsedId:
    Argument @ID, @Parent
    Uses esi

        On B$SameIdAllowed = &TRUE, ExitP

L0:     mov esi DialogList

        While D$esi <> 0
            mov eax D$esi
            If eax = D@ID
                call WriteDecimalID, eax, DoubleID
                call 'USER32.MessageBoxA' D@Parent, {'This Dialog ID already exist', 0},
                                          DoubleID, &MB_OK
                mov eax &IDCANCEL | ExitP
            Else
                add esi 12 | add edi 12
            End_If
        End_While

        mov eax &IDOK
EndP


Proc WriteDecimalID:
    Argument @Value, @Destination

    pushad
        mov eax D@Value, edi D@Destination, ecx 10

        mov D$edi ' ID ' | add edi 4

        push 0-1
L0:     mov edx 0 | div ecx | push edx | cmp eax 0 | ja L0<

L0:     pop eax | cmp eax 0-1 | je L9>
        add al '0' | stosb | jmp L0<

L9:     mov B$edi 0
    popad
EndP

____________________________________________________________________________________________



[DialogListHandle: ?    LastDialogListItem: ?]

ScrollDownToLastControl:
  ; Scroll full down to last new added control edition:
    call 'User32.SendMessageA' D$DialogListHandle &LB_GETCOUNT 0 0
    dec eax | mov D$LastDialogListItem eax
    push eax
        call 'User32.SendMessageA' D$DialogListHandle &LB_SETTOPINDEX eax 0
    pop eax
    sub eax 2
    call 'User32.SendMessageA' D$DialogListHandle &LB_SETCURSEL  eax 0
ret


ScrollToInsertedControl:
    call 'User32.SendMessageA' D$DialogListHandle &LB_GETCOUNT 0 0
    dec eax | mov D$LastDialogListItem eax
    mov eax D$DialogListIndex | add eax Line_Font
    call 'User32.SendMessageA' D$DialogListHandle &LB_SETCURSEL  eax 0  ; select title
ret

[UpDownEndScroll 8]

[UserModifiedDim: ?] [OnDialogEdition: B$ ?  DialogLoadedFromResources: ?]

; D$ControlIndex values (set by "SetDialogTools"):
;   0 > Style  1 > exStyle 2 > Dim   3 > ID   4 > Class   5 > Title   6 > Font
;   0_FFFF_FFFF if not yet or blank line.
[Line_Style 0 | Line_exStyle 1 | Line_Dim 2 | Line_ID 3 | Line_Class 4 | Line_Title 5 | Line_Font 6 | Line_empty 7]
[ProposedUpDowmChange: ?] ; The UDN_DELTAPOS WM_NOTIFY Message is sent before the
; Edit Control update. I use this Message to ease differenciating between all the
; Various Edit Controls. Immidiately after 'WriteDim' has used it, it reset it to
; zero. The Value is a signed dWord.

Proc EditDialogBoxProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If B$OnMenuEdition = &TRUE
        If D@Message = &WM_NOTIFY
            jmp L1>
        Else_If D@Message = &WM_VSCROLL
            jmp L1>
        Else_If D@Message = &WM_COMMAND
L1:         call 'User32.SendMessageA' D$MenuEditorHandle, &WM_COMMAND, &IDCANCEL, 0
        End_If
    .End_If

    If D$UserModifiedDim <> &FALSE
        mov ecx D$UserModifiedDim | call WriteDim
        mov D$UserModifiedDim &FALSE
    End_If

    ...If D@Message = &WM_NOTIFY
        mov ebx D@lParam, eax D$ebx+8     ; user click and hold on Updown in Dim edition
        If eax = &UDN_DELTAPOS
            mov eax D$ebx, edi DialogControlsHandles, ecx 12 | repne scasd
            sub edi 8 | mov ecx D$edi               ; each "dim": string / edit / UpDown
            move D$ProposedUpDowmChange D$ebx+16    ; This is the proposed change (signed dWord)
            call WriteDim
            call 'User32.SetForegroundWindow' D$DialogEditorHandle
            popad | mov eax &FALSE | jmp L9>>
        End_If

    ...Else_If D@Message = &WM_VSCROLL        ; user clicked and release Updown in Dim edition
        mov eax D@wParam | and eax UpDownEndScroll | jz C9>
          mov eax D@lParam, edi DialogControlsHandles, ecx 12 | repne scasd
          sub edi 8 | mov ecx D$edi           ; each "dim": string / edit / UpDown
          mov D$UserModifiedDim ecx
          call WriteDim
          call 'User32.SetForegroundWindow' D$DialogEditorHandle

; ID_DEadd 3  ID_DEdel 4  ID_DEreset 5  ID_DEexit 6  ID_DEhelp 7

; D$StyleHelpButtonsHandles > List of Styles Help
C9:
    ...Else_If D@Message = &WM_COMMAND
        .If D@wParam = ID_DEadd
            call AddOneControl
            call 'User32.SendMessageA' D$DialogListHandle, &LB_GETCURSEL, 0, 0
            sub eax 2
            call 'User32.SendMessageA' D$DialogListHandle, &LB_SETCURSEL, eax, 0
            call SetDialogTools
            call 'User32.SetForegroundWindow' D$DialogEditorHandle
            jmp L7>>

        .Else_If D@wParam = ID_DEdel
            call DelOneControl
            call 'User32.SetForegroundWindow' D$DialogEditorHandle
            jmp L7>>

        .Else_If D@wParam = ID_DEdel_Menu
            call SearchDialogLine
            If D$edi = 'FFFF'
                mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
                push esi | ZCopy {"0 ;      no Menu", 0} | pop esi
                call FromTextToBinTemplate | call ShowDialogResult
                call FillDialogListBox
            Else
                call NoDialogMenu
            End_If
            jmp L7>>

        .Else_If D@wParam = ID_DEreplaceMenu
            call SearchDialogLine
            If D$edi = 'FFFF'
                call AddMenuToDialog
            Else
                call NoDialogMenu
            End_If
            jmp L7>>

        .Else_If D@wParam = ID_DEreset
            call ResetDialogListBox | call FillDialogListBox
            call 'User32.SetForegroundWindow' D$DialogEditorHandle
            mov W$PreviousControlID 0FFFF
            call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
            jmp L7>>

        .Else_If D@wParam = ID_DEhelp
L1:         call Help, B_U_AsmName, DialogHelp, ContextHlpMessage | jmp L7>>

        .Else_If D@wParam = ID_DEsaveToResources
            mov ebx 4 | call ExitDialog | jmp L7>>

        .Else_If D@wParam = ID_DEsaveToClipBoard
            mov ebx 0 | call ExitDialog | jmp L7>>

        .Else_If D@wParam = ID_DEsaveToDisk
            mov ebx 8 | call ExitDialog | jmp L7>>

        .Else_If D@wParam = ID_DEabort
            mov ebx 12 | call ExitDialog | jmp L7>>

        .Else_If D@wParam = &IDCANCEL
            call 'User32.EndDialog' D@Adressee 0
            call 'User32.DestroyWindow' D$EditedDialogHandle
            mov D$DialogEditorHandle 0, D$EditedDialogHandle 0
            mov B$OnDialogEdition &FALSE, B$DialogLoadedFromResources &FALSE | jmp L7>>
        .End_If

        mov eax D@wParam, ebx eax, ecx D@lParam
      ; buttons handles in ecx
        shr eax 16 | and ebx 0FFFF

        ..If ecx = D$DialogListHandle
            On eax = &LBN_SELCHANGE, call SetDialogTools

        ..Else
            .If eax = &BN_CLICKED
              ; Check button?
                On ebx = 1, jmp L7>>
              ; case of user hit [Return]
                mov esi DialogControlsHandles, ebx 0
L0:             lodsd
                If eax = ecx
                   call WriteStyle
                Else_If eax <> 0
                  ; stop on 0 (user click, but not on a 'moveable' control)
                    add ebx 4 | jmp L0<
                End_If

                On ecx = 0, jmp L2>
                    mov esi StyleHelpButtonsHandles, ebx 0
L0:                 lodsd

              ; Ready for Styles Help ('RunDialogHelp' / 'HelpDialogProc'):
                    If eax = ecx
                        call ShowStyleInfo
                    Else_If eax <> 0      ; stop on 0 (user click, but not on a 'moveable' control)
                        add ebx 4 | jmp L0<
                    End_If
                ;  call 'User32.SetForegroundWindow' D$DialogEditorHandle

            .Else_If eax = &EN_CHANGE           ; Edit Box?
                If D$ControlIndex = Line_Title
                    call WriteTitle
                Else_If D$ControlIndex = Line_ID
                    call WriteID
                Else_If D$ControlIndex = Line_Class
                    call WriteTitle              ; reuse for Dialog Class
                End_If
L2:             call 'User32.SetForegroundWindow' D$DialogEditorHandle

            .Else_If eax = &LBN_SELCHANGE       ; List Box >>> font syze or type, or Class
                If D$DialogControlsHandles+4 = 0
                    call WriteClass
                Else_If ecx = D$DialogControlsHandles
                    call WriteFontType
                Else
                    call WriteFontSize
                End_If
                call 'User32.SetForegroundWindow' D$DialogEditorHandle

            .Else_If eax = &EN_UPDATE                  ; direct input in Dim edit control
                On D$ControlIndex = Line_Dim, call WriteDim
                call 'User32.SetForegroundWindow' D$DialogEditorHandle

            .End_If

        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$DialogEditorHandle D@Adressee

        call 'User32.GetDlgItem' D@Adressee ID_Ilist | mov D$DialogListHandle eax

        call CreateDialogMenu

        If B$OnClipDialog = &FALSE
            call ResetDialogListBox | call FillDialogListBox
            call ScrollDownToLastControl
        Else
            call FillDialogListBox | call ScrollDownToLastControl
            mov B$OnClipDialog &FALSE
        End_If
        mov B$OnDialogEdition &TRUE

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

    ...Else_If D@Message = &WM_CTLCOLORLISTBOX
        jmp L1>

    ...Else_If D@Message = &WM_CTLCOLOREDIT
L1:     call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>
    ...End_If

L7: popad | mov eax &TRUE

L9: EndP

_____________________________________________________________________________________
_____________________________________________________________________________________

; Routine to show particular controls on the editor dialog, depending on what main list
; line user click.
_____________________________________________________________________________________
_____________________________________________________________________________________

[DialogControlsHandles: ? #40] [StyleHelpButtonsHandles: ? #40]
[EditClass: 'EDIT', 0]

; these tables are used to set contents and pos of controls for style edition:

; Dialog window styles:

[DialogCheckingMask:
 0 0 0 0
 0 0
 0 0
 0 0 0 0
 0 0 0 0 0 0 0 0 0]

[DialogExcludeBitTable:
 0C0000000  0C0000000 0 0
 0 0
 0 0
 0 0 0 0
 0 0 0 0 0 0 0 0 0]

[DialogMustHaveBItTable:
 0 0 0 0
 0 0
 0 0
 0C00000 0C80000 0C80000 0C80000
 0 0 0 0 0 0 0 0 0]

[DialogTextTable: B$
 'WS_CHILD', 0     'WS_POPUP', 0        'WS_VISIBLE', 0       'WS_DISABLED', 0
 'WS_BORDER', 0    'WS_CAPTION', 0
 'WS_HSCROLL', 0  'WS_VSCROLL', 0
 'WS_SYSMENU', 0   'WS_MAXIMIZEBOX', 0  'WS_MINIMIZEBOX', 0   'DS_CONTEXTHELP', 0
 'DS_ABSALING', 0
 'DS_CENTER', 0
 'WS_THICKFRAME', 0
 'DS_CONTROL', 0
 'DS_MODALFRAME', 0
 'DS_NOFAILCREATE', 0
 'DS_NOIDLEMSG', 0
 'DS_SETFOREGROUND', 0
 'DS_SYSMODAL', 0 0]

[DialogBitTable:
 &WS_CHILD    &WS_POPUP        &WS_VISIBLE      &WS_DISABLED
 &WS_BORDER   &WS_CAPTION
 &WS_HSCROLL  &WS_VSCROLL
 &WS_SYSMENU  &WS_MAXIMIZEBOX  &WS_MINIMIZEBOX  &DS_CONTEXTHELP
 &DS_ABSALIGN
 &DS_CENTER
 &WS_THICKFRAME
 &DS_CONTROL
 &DS_MODALFRAME
 &DS_NOFAILCREATE
 &DS_NOIDLEMSG
 &DS_SETFOREGROUND
 &DS_SYSMODAL] ; &DS_CENTERMOUSE

[WS_CHILDhelp: "WS_CHILD:

> Child window.

Cannot be used with the WS_POPUP style.", 0
 WS_POPUPhelp: "WS_POPUP:

> Pop-up window.

Cannot be used with the WS_CHILD style.", 0
 WS_VISIBLEhelp: "WS_VISIBLE:

window is initially visible.", 0
 WS_DISABLEDhelp: "WS_DISABLED:

The window is initially disabled and can't receive input from the user.", 0
 WS_BORDERhelp: "WS_BORDER:

The window has a thin-line border.", 0
 WS_CAPTIONhelp: "WS_CAPTION:

The window has a title bar (includes WS_BORDER).", 0
 WS_HSCROLLhelp: "WS_HSCROLL:

The window has an horizontal scroll bar.", 0
 WS_VSCROLLhelp: "WS_VSCROLL:

The window has a vertical scroll bar.", 0
 WS_SYSMENUhelp: "WS_SYSMENU:

The window has a System-menu box in the title bar.

Must be WS_CAPTION too.", 0
 WS_MAXIMIZEBOXhelp: "WS_MAXIMIZEBOX:

Adds the Maximize button to the title bar.", 0
 WS_MINIMIZEBOXhelp: "WS_MINIMIZEBOX:

Adds the Minimize button to the title bar.", 0
 DS_CONTEXTHELPhelp: "DS_CONTEXTHELP:

Adds a question mark in the title bar", 0
 DS_ABSALINGhelp: "DS_ABSALIGN:

The coordinates of the dialog box will be screen coordinates instead of client area ones.", 0
 DS_CENTERhelp: "DS_CENTER:

Centers the dialog box in the working area.", 0
 WS_THICKFRAMEhelp: "WS_THICKFRAME:

The window has a sizing border.

Same as WS_SIZEBOX.", 0
 DS_CONTROLhelp: "DS_CONTROL:

The dialog will works as a child window of another dialog (example: A page in a Tabbed Dialog).", 0
 DS_MODALFRAMEhelp: "DS_MODALFRAME:

The dialog will have a modal dialog-box frame.", 0
 DS_NOFAILCREATEhelp: "DS_NOFAILCREATE:

Creates the dialog even if an error occur, for example, if a child window cannot be created.", 0
 DS_NOIDLEMSGhelp: "DS_NOIDLEMSG:

Blocks the WM_ENTERIDLE messages that Windows would otherwise send to the owner of the dialog box when the dialog box is displayed.", 0
 DS_SETFOREGROUNDhelp: "DS_SETFOREGROUND:

Brings the dialog box to the foreground, like it could be done with a call to SetForegroundWindow", 0
 DS_SYSMODALhelp: "DS_SYSMODAL:

> System-modal dialog box.

The dialog will have the WS_EX_TOPMOST style.", 0
 0]


[DialogStylesHelp: WS_CHILDhelp WS_POPUPhelp WS_VISIBLEhelp WS_DISABLEDhelp
 WS_BORDERhelp WS_CAPTIONhelp WS_HSCROLLhelp WS_VSCROLLhelp
 WS_SYSMENUhelp WS_MAXIMIZEBOXhelp WS_MINIMIZEBOXhelp DS_CONTEXTHELPhelp
 DS_ABSALINGhelp DS_CENTERhelp WS_THICKFRAMEhelp DS_CONTROLhelp DS_MODALFRAMEhelp
 DS_NOFAILCREATEhelp DS_NOIDLEMSGhelp DS_SETFOREGROUNDhelp DS_SYSMODALhelp 0]

___________________________________________________________________________________

ShowDialogStyleControl:
    ShowSetOfCheckBoxes DialogTextTable
ret

__________________________________________________________________________________

; very simplified version of TranslateHexa
; (esi >>> first text hexa number char after leading '0'):

TranslateDialogHexa:
    mov ebx 0
L0: lodsb | cmp al ' ' | je L9>
            cmp al ';' | je L9>
            cmp al CR | jbe L9>
      sub al '0' | On al > 9, sub al 7
      shl ebx 4 | or bl al | jmp L0<
L9: ret                                     ; >>> ebx = binary value

TranslateDialogText8:
    mov ecx 8
L0: mov eax ebx | and eax 0_F000_0000 | shr eax 28
    add al '0' | On al > '9', add al 7
    shl ebx 4
    stosb | loop L0<
ret

TranslateDialogText4:
    shl ebx 16
    mov ecx 4 | jmp L0<

TranslateDialogText2:
    shl ebx 24
    mov ecx 2 | jmp L0<

____________________________________________________________________________________

ShowDialogStyles:
    mov esi D$NewDialogTemplateText | add esi 3
    call TranslateDialogHexa | mov D$CheckActual ebx

    move D$CheckingMask DialogCheckingMask
    mov esi DialogBitTable

    push ebp
        call CheckControlStyles
    pop ebp
ret


[CheckBit: ?  CheckActual: ?  CheckExclude: ?  CheckMust: ?  CheckBitPtr: ?
 CheckResult: ?  CheckingMask: ?]

____________________________________________________________________________________


CheckControlStyles:
;;
 check / uncheck the controls:

 Set by caller (WriteStyle):

      D$CheckActual = Actual value for style (full value)
      D$CheckingMask = Bits table adress of checking masks (the main difficulty)

      esi = ptr to Style Bits table
;;
    mov edi DialogControlsHandles, ecx 0
    While D$edi > 0
       add edi 4 | inc ecx                ; number of check boxes at screen in ecx
    End_While
;;
 Very killing problem: This routine MUST be able to Check/UnCheck the edition CheckBoxes
 according with the value of "Style" (D$CheckActual). The fact is that we can have, for
 exemple, for buttons styles, values sets like:

 BS_3STATE 5         BS_AUTO3STATE 6     BS_AUTOCHECKBOX 3  BS_AUTORADIOBUTTON 9
 BS_CHECKBOX 2       BS_DEFPUSHBUTTON 1  BS_GROUPBOX 7      BS_PUSHBUTTON 0
 BS_RADIOBUTTON 4

 ... what makes it impossible to trust any bit value for checking. For exemple, if user
 click on "BS_GROUPBOX" (= 00_111), we must not check 00_001 (BS_DEFPUSHBUTTON), 00_010
 (BS_CHECKBOX), and so on... Sorry, next lines are very very difficult to understand. The
 only readable solution would be to write a no end cases selections routine for each
 control class, but i have choosen this shorter way. A second difficulty is that zero
 may be significant - and Check / UnCheck- (exemple: BS_PUSHBUTTON) and that we have to
 discriminate 00_10 (BS_CHECKBOX, for exemple) from 00_10... Killing:
;;
    mov edx D$CheckingMask, ebp 0              ; ebp = handles index for 'SendMessage'

L0: lodsd                                      ; loads one style value from Styles list
    pushad
      mov edx D$edx                            ; CheckingMask  !!!!!!!!!!!!!!!!!!!!!!!!
      ; eax Style bit value from list
        ; (Style and eax) <> eax >>> false?:
      mov ebx D$CheckActual | and ebx eax | cmp eax ebx | jne L6>
        ; (eax and CheckingMask) = 0  > next line
      mov ebx eax | and ebx edx | cmp ebx 0 | jne L4>
        ; check only if really 0 wanted:
        cmp eax 0 | jne L5>
          ; true zero?:
          mov eax D$CheckActual | and eax 00_111 | cmp eax 0 | je L5>
            jmp L6>
        ; last test for 'true', only if: bit value = (Bit value and CheckingMask):
L4:     mov eax D$CheckActual | and eax edx | cmp ebx eax | jne L6>

L5:   mov eax &TRUE | jmp L7>    ; check

L6:   mov eax &FALSE             ; UnCheck

L7:   call 'User32.SendMessageA' D$DialogControlsHandles+ebp &BM_SETCHECK eax 0
    popad | add ebp 4 | add edx 4 | loop L0<
ret


; Same as above, but for showing the checkboxes set at first time (user did'nt yet click
; at any check box, we just need actual style value and according checking mask table):

ShowControlStyles:
    call SearchDialogLine | mov esi edi | add esi 3
    call TranslateDialogHexa | mov D$CheckActual ebx

    add D$DialogListIndex Line_Class
        call SearchWhatControlClass             ; ebx = indice of class (0 / 1 / 2 / 3...)
    sub D$DialogListIndex Line_Class

    ..If edi = ControlClassByNumber             ; >>> class by Number
        .If ebx = 0
            move D$CheckingMask ButtonCheckingMask
            mov esi ButtonBitTable
        .Else_If ebx = 1
            move D$CheckingMask EditCheckingMask
            mov esi EditBitTable
        .Else_If ebx = 2
            move D$CheckingMask StaticCheckingMask
            mov esi StaticBitTable
        .Else_If ebx = 3
            move D$CheckingMask ListCheckingMask
            mov esi ListBitTable
        .Else_If ebx = 4
            move D$CheckingMask ScrollCheckingMask
            mov esi ScrollBitTable
        .Else_If ebx = 5
            move D$CheckingMask ComboCheckingMask
            mov esi ComboBitTable
        .End_If
    ..Else                                      ; >>> class by Name
        .If ebx = 0
            move D$CheckingMask UpDownCheckingMask
            mov esi UpDownBitTable
        .Else_If ebx = 1
            ret                                 ; msctls_progress32 (no controls)
        .Else_If ebx = 2
            move D$CheckingMask TrackCheckingMask
            mov esi TrackBitTable
        .Else_If ebx = 3
            move D$CheckingMask TreeCheckingMask
            mov esi TreeBitTable
        .Else_If ebx = 4
            move D$CheckingMask TabCheckingMask
            mov esi TabBitTable
        .Else_If ebx = 5
            move D$CheckingMask ListViewCheckingMask
            mov esi ListViewBitTable
        .Else_If ebx = 6
            move D$CheckingMask ToolBarCheckingMask
            mov esi ToolBarBitTable
        .Else_If ebx = 7
            move D$CheckingMask RichEdit20aCheckingMask
            mov esi RichEdit20aBitTable
        .Else_If ebx = 8
            move D$CheckingMask SysHeader32CheckingMask
            mov esi SysHeader32BitTable
        .Else_If ebx = 9
            move D$CheckingMask ReBarWindow32CheckingMask
            mov esi ReBarWindow32BitTable
        .Else_If ebx = 10
            move D$CheckingMask Tooltips_class32CheckingMask
            mov esi Tooltips_class32BitTable
        .Else_If ebx = 11
            move D$CheckingMask msctls_statusbar32CheckingMask
            mov esi msctls_statusbar32BitTable
        .Else_If ebx = 12
            move D$CheckingMask msctls_hotkey32CheckingMask
            mov esi msctls_hotkey32BitTable
        .Else_If ebx = 13
            move D$CheckingMask ComboBoxEx32CheckingMask
            mov esi ComboBoxEx32BitTable
        .Else_If ebx = 14
            move D$CheckingMask SysAnimate32CheckingMask
            mov esi SysAnimate32BitTable
        .Else_If ebx = 15
            move D$CheckingMask SysMonthCal32CheckingMask
            mov esi SysMonthCal32BitTable
        .Else_If ebx = 16
            move D$CheckingMask SysDateTimePick32CheckingMask
            mov esi SysDateTimePick32BitTable
        .Else_If ebx = 17
            move D$CheckingMask SysIPAddress32CheckingMask
            mov esi SysIPAddress32BitTable
        .Else_If ebx = 18
            move D$CheckingMask SysPagerCheckingMask
            mov esi SysPagerBitTable
        .Else_If ebx = 19
            move D$CheckingMask SysLinkCheckingMask
            mov esi SysLinkBitTable
        .End_If
    ..End_If

    call CheckControlStyles
ret

_______________________________________________________________________________________

[XdimText: 'X position:', 0  YdimText: 'Y position:', 0
 WdimText: 'Width:', 0       HdimText: 'Hight:', 0]

; This Table is an array of 2 UDACCEL structures. First dWord is the delay before next
; Array runing, dWord 2 is the displacement:
[UDACCEL: n1Sec: D$ 0    n1Inc: 1
          n2Sec:    2    n2Inc: 5]

[DialogDimTable: XdimText 200 0,  YdimText 240 0,  WdimText 280 0,  HdimText 320 0]
;;
 In "display edit box", ES_RIGHT (to align text number to the left, works only with
 ES_MULTILINE edit control style. (the box is de facto mono-line, of course). Without
 this, it sends a EN_CHANGE message at first display.
;;
ShowDimControls:
  ; First retrieve values from user text dim:
    call SearchDialogLine
    mov al ' ' | mov ecx 200 | repne scasb  ; space used as separator > count of data
    If D$DimIsForDialogWindow = &TRUE
        repne scasb                         ; jump over n (number of controls)
    End_If
    mov esi edi | inc esi
    call TranslateDialogHexa | mov D$DialogDimTable+ 8 ebx

    call TranslateDialogHexa | mov D$DialogDimTable+20 ebx

    call TranslateDialogHexa | mov D$DialogDimTable+32 ebx

    call TranslateDialogHexa | mov D$DialogDimTable+44 ebx

    mov ecx 4, esi DialogDimTable, ebx 0
L0: push ecx
      ; display text:
        push esi, ebx
            mov eax D$esi+4 | add eax 6
            call 'User32.CreateWindowExA'  0, StaticClassName, 0,
                                           &WS_CHILD+&WS_VISIBLE+&SS_SIMPLE, 2, eax,
                                           120 20, D$DialogEditorHandle 0 D$hInstance 0
        pop ebx, esi
        mov D$DialogControlsHandles+ebx eax
        push esi, ebx
            call 'User32.SendMessageA' eax &WM_SETFONT D$MyFontHandle &FALSE
            call 'User32.SetWindowTextA' D$DialogControlsHandles+ebx D$esi
        pop ebx esi

      ; display edit box:
        push esi, ebx
            call 'User32.CreateWindowExA'  0  EditClass  0,
                      &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&ES_NUMBER+&ES_RIGHT+&ES_MULTILINE,
                      80 D$esi+4  45 20, D$DialogEditorHandle 0 D$hInstance 0
        pop ebx, esi
        mov D$DialogControlsHandles+ebx+4 eax

      ; display Up and down control:                     +&UDS_ARROWKEYS
        push esi, ebx
            call 'Comctl32.CreateUpDownControl',
              &WS_CHILD+&WS_BORDER+&WS_VISIBLE+&UDS_SETBUDDYINT+&UDS_NOTHOUSANDS+&UDS_HORZ,
                      130 D$esi+4 20 20, D$DialogEditorHandle, 102, D$Hinstance,
                      eax  2000 0  D$esi+8
        pop ebx, esi
        mov D$DialogControlsHandles+ebx+8 eax

      ; Set the speed (repeat speeds) of the UpDown Controls:
        push esi, eax, ebx
            call 'User32.SendMessageA' eax &UDM_SETACCEL 2 UDACCEL
        pop ebx, eax, esi

        push esi, ebx
            call 'User32.SendMessageA' eax &UDM_SETBUDDY D$DialogControlsHandles+ebx+4 0
        pop ebx, esi

        add ebx 12 | add esi 12
    pop ecx

    sub ecx 1 | jnz L0<<
ret

____________________________________________________________________________________

[ActualFontName: B$ ? #20] [ActualFontSize: ? ?]

; +ES_MULTILINE because it work better at message flow point of view.
; (used as mono-line, of course). Without this, it sends a EN_CHANGE message
; at first display.

ShowTitleControl:
    call 'User32.CreateWindowExA' 0  EditClass  0,
                     &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&ES_AUTOHSCROLL+&ES_MULTILINE,
                                 2 200 145 20, D$DialogEditorHandle 0 D$hInstance 0
    mov D$DialogControlsHandles eax

  ; Copy data title (without quotes and comments) in TitleEditText:
    call SearchDialogLine | inc edi | mov esi edi
    mov al '"', ecx 200, ebx 200 | repne scasb                      ; search "Text lenght"
    sub ebx ecx | xchg ecx ebx | dec ecx
    mov edi ActualFontName | rep movsb | mov al 0 | stosb           ; copy + end mark

    call 'User32.GetDlgCtrlID' D$DialogControlsHandles
    call 'User32.SetDlgItemTextA' D$DialogEditorHandle eax ActualFontName  ; show edition text
    call 'User32.SetFocus' D$DialogControlsHandles
    call 'User32.SendMessageA' D$DialogControlsHandles  &EM_SETSEL  0  0-1
ret

[ComboClass: 'COMBOBOX', 0]  ; EditClass


; table of fonts names for dialog fonts:

[DialogFonts: B$ 'Arial', 0            'Arial Black', 0  'Comic Sans MS', 0
                 'Courier', 0          'Courier New', 0  'Fixedsys', 0
                 'Helv' 0
                 'Impact', 0           'Marlett', 0      'Modern', 0
                 'MS Sans Serif', 0    'MS Serif', 0     'Small Fonts', 0
                 'Symbol', 0           'System', 0       'Terminal', 0
                 'Times New Roman', 0  'Verdana', 0      'Webdings', 0
                 'Wingdings', 0 0

; Differents set of fonts sizes available for upper fonts list:

 T1F:  '08' 0  '09' 0  '0A' 0  '0B' 0  '0C' 0  '0E' 0  '10' 0  '12' 0
               '14' 0  '16' 0  '18' 0  '1A' 0  '1C' 0  '24' 0  '30' 0  '48' 0  0
 T2F:  '0A' 0  '0C' 0  '0F' 0 0
 T3F:  '09' 0  0
 T4F:  '08' 0  '0A' 0  '0C' 0  '0E' 0  '12' 0  '18' 0  0
 T5F:  '06' 0  '07' 0  '08' 0  '0A' 0  '0C' 0  '0E' 0  '12' 0  '18' 0  0
 T6F:  '02' 0  '03' 0  '04' 0  '05' 0  '06' 0  '07' 0  0
 T7F:  '0A' 0  0

; Table of pointers: font index of upper names >>> pointer to upper sizes table:

 FontSizesTable:
 D$ T1F T1F T1F T2F T1F T3F  T4F  T1F T1F T1F T4F T5F T6F T7F T3F T1F T1F T1F T1F]


; Search for the actual font in font name list to retrieve the index for sizes table:

SearchFontIndex:
    mov edi DialogFonts, edx 0-4

    While B$edi > 0
        mov esi ActualFontName | add edx 4
        push edi
            mov ecx 200, ebx 200, al 0 | repne scasb | sub ebx ecx | dec ebx
        pop edi
        If ebx = D$ComboFontNameLenght
            mov esi edi, ecx ebx | repe cmpsb | je L9>
                mov edi esi | add edi ebx | inc edi
        Else
            add edi ebx | inc edi
        End_If
    End_While
    mov eax 0 | ret

L9: mov eax edx | ret


[FontComboHandle: ?  FontComboID: ?  ComboFontNameLenght: ?]

ShowFontControls:
  ; Font Type edition combo box:
    call 'User32.CreateWindowExA'  0, ComboClass, 0,
&WS_CHILD+&WS_VISIBLE+&WS_BORDER+&CBS_HASSTRINGS+&CBS_AUTOHSCROLL+&CBS_DROPDOWNLIST+&WS_VSCROLL+&ES_AUTOVSCROLL,
                      2, 240, 145, 220, D$DialogEditorHandle, 0, D$hInstance, 0
    mov D$DialogControlsHandles eax

  ; Copy data font text (without quotes and comments) in TitleEditText:
    call SearchDialogLine
    mov esi edi, edi ActualFontSize
    while B$esi > ' '
        movsb
    End_While
    mov al 0 | stosb
    add esi 2 | mov edi esi
    mov al '"', ecx 200, ebx 200 | repne scasb                      ; search "Text lenght"
    sub ebx ecx | xchg ecx ebx | dec ecx
    mov D$ComboFontNameLenght ecx
    mov edi ActualFontName | rep movsb | mov al 0 | stosb           ; copy + end mark

    mov edi DialogFonts
    While B$edi > 0
        push edi
            call 'User32.SendMessageA' D$DialogControlsHandles &CB_ADDSTRING 0  edi
        pop edi
        mov al 0, ecx 200 | repne scasb
    End_While

    call 'User32.SendMessageA' D$DialogControlsHandles &CB_SELECTSTRING  0  ActualFontName

  ; Font size edition box:
    call 'User32.CreateWindowExA'  0, ComboClass  0,
    &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&CBS_HASSTRINGS+&CBS_AUTOHSCROLL+&CBS_DROPDOWNLIST+&WS_VSCROLL+&ES_AUTOVSCROLL,
                                  100, 200, 47, 150, D$DialogEditorHandle, 0, D$hInstance, 0
    mov D$DialogControlsHandles+4 eax

    call SearchFontIndex

    If eax > 0
        mov edi D$FontSizesTable+eax
        While B$edi > 0
            push edi
                call 'User32.SendMessageA' D$DialogControlsHandles+4 &CB_ADDSTRING 0  edi
            pop edi
            mov al 0, ecx 200 | repne scasb
        End_While
    End_If

    call 'User32.SendMessageA' D$DialogControlsHandles+4  &CB_SELECTSTRING  0  ActualFontSize
ret

________________________________________________________________________________________
________________________________________________________________________________________
;;
 All the controls specificaly used for one record edition have these data tables
 associated with: (one for equates), one for texts proposed as different choices, one
 for the bit values of each possible choice, one for the bit values that can not be
 fitting with a choice and one for the bits that must be associated with one choice.

 Some win equates are not bit values but ordinals values (1, 2, 3, ..., 0100, 0200, 0300,...)
 in one same set of equates. This is to say that, having the binary value for some 'Style',
 it is impossible to find the signification by simply analysing each bit of 'Style' value.
 So must we have an additionnal set of mask to be able to Check / UnCheck the check
 boxes, either at first screen show or after user modifications, according with
 exclusions and 'must have' rules. These additionnal tables are named 'xxxxCheckingMask'.

 "xxxxExcludeBitTable" is used by 'WriteStyle' to compute the style value (only one value
 per operation)
 "xxxxCheckingMask" is used by 'CheckControlStyles' to set the check boxes (all the set
 per operation).
;;
________________________________________________________________________________________
________________________________________________________________________________________
; Button tables:


[ButtonCheckingMask:
 0F 0F
 0F 0F 0F 0F
 0F 0F
 0F
 0F0
 &BS_CENTER &BS_CENTER &BS_CENTER
 &BS_VCENTER &BS_VCENTER &BS_VCENTER
 0F 0F0 0F0
 &BS_MULTILINE
 &BS_NOTIFY
 &BS_PUSHLIKE
 &BS_FLAT
 010000
 020000
 08000000
 &WS_CHILD &WS_VISIBLE &WS_BORDER &WS_CLIPSIBLINGS]

[ButtonExcludeBitTable:
 0FFFF  0FFFF
 0FFFF 0FFFF 0FFFF 0FFFF
 0FFFF 0FFFF
 0FFFF
 0
 &BS_CENTER &BS_CENTER &BS_CENTER
 &BS_VCENTER &BS_VCENTER &BS_VCENTER
 0F   0F0  0F0
 0 0 0 0 0 0 0
 0 0 0
 0]

[ButtonTextTable: B$
 'BS_PUSHBUTTON' 0   'BS_DEFPUSHBUTTON' 0
 'BS_CHECKBOX' 0     'BS_3STATE' 0           'BS_AUTOCHECKBOX' 0    'BS_AUTO3STATE' 0
 'BS_RADIOBUTTON' 0  'BS_AUTORADIOBUTTON' 0
 'BS_GROUPBOX' 0
 'BS_LEFTTEXT' 0
 'BS_LEFT' 0         'BS_RIGHT' 0            'BS_CENTER' 0
 'BS_BOTTOM' 0       'BS_TOP' 0              'BS_VCENTER' 0
 'BS_OWNERDRAW' 0    'BS_BITMAP' 0           'BS_ICON' 0
 'BS_MULTILINE' 0
 'BS_NOTIFY' 0
 'BS_PUSHLIKE' 0
 'BS_FLAT' 0
 'WS_TABSTOP' 0
 'WS_GROUP' 0
 'WS_DISABLED' 0
 'WS_CHILD' 0
 'WS_VISIBLE' 0
 'WS_BORDER',0
 'WS_CLIPSIBLINGS', 0 0]

[ButtonBitTable:
 &BS_PUSHBUTTON      &BS_DEFPUSHBUTTON
 &BS_CHECKBOX        &BS_3STATE              &BS_AUTOCHECKBOX    &BS_AUTO3STATE
 &BS_RADIOBUTTON     &BS_AUTORADIOBUTTON
 &BS_GROUPBOX
 &BS_LEFTTEXT
 &BS_LEFT            &BS_RIGHT               &BS_CENTER
 &BS_BOTTOM          &BS_TOP                 &BS_VCENTER
 &BS_OWNERDRAW       &BS_BITMAP              &BS_ICON
 &BS_MULTILINE
 &BS_NOTIFY
 &BS_PUSHLIKE
 &BS_FLAT
 &WS_TABSTOP &WS_GROUP &WS_DISABLED
 &WS_CHILD &WS_VISIBLE &WS_BORDER
 &WS_CLIPSIBLINGS] ; &BS_RIGHTBUTTON &BS_TEXT

[ButtonMustHaveBitTable: 0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0  0 0     0 0 0   0]

[BS_PUSHBUTTONhelp: "BS_PUSHBUTTON:

The button posts a WM_COMMAND message to its parent when pushed.", 0
 BS_DEFPUSHBUTTONhelp: "BS_DEFPUSHBUTTON:

The button behaves like with BS_PUSHBUTTON, but has a enhanced border and reacts as default OK, when user hits Return.", 0
 BS_CHECKBOXhelp: "BS_CHECKBOX: 

Empty check box with text.

The default text Pos (at right) may be changed with BS_LEFTTEXT.", 0
 BS_3STATEhelp: "BS_3STATE:

Same as check box, but it can be grayed, checked or unchecked.", 0
 BS_AUTOCHECKBOXhelp: "BS_AUTOCHECKBOX:

Same as check box, but the state automatically toggles on user action.", 0
 BS_AUTO3STATEhelp: "BS_AUTO3STATE:

Same as Three-state check box, but the state automatically toggles on user action.", 0
 BS_RADIOBUTTONhelp: "BS_RADIOBUTTON:

Small circle with text. The default text Pos (at right) may be changed with BS_LEFTTEXT.", 0
 BS_AUTORADIOBUTTONhelp: "BS_AUTORADIOBUTTON:

Same as a radio button, but the state automatically toggles on user action.", 0
 BS_GROUPBOXhelp: "BS_GROUPBOX:

Not really a button. Creates a rectangle in which other controls can be grouped, with eventually a common title.

If you want, for example to have several sets of Auto-Radio-Buttons, you may also need the WS_GROUP style", 0
 BS_LEFTTEXThelp: "BS_LEFTTEXT:

Places text on the left side of the radio button or check box.", 0
 BS_LEFThelp: "BS_LEFT:

Left-justified the text in the button rectangle.

Note: If the button is a check box or radio button without BS_RIGHTBUTTON, the text is left justified on the right side of the control.", 0
 BS_RIGHThelp: "BS_RIGHT:

Right-justified text in the button rectangle.

Note: If the button is a check box or radio button without BS_RIGHTBUTTON, the text is right justified on the right side of the control.", 0
 BS_CENTERhelp: "BS_CENTER:

Centers the text horizontally in the button rectangle.", 0
 BS_BOTTOMhelp: "BS_BOTTOM:

Places the text at the bottom of the button rectangle.", 0
 BS_TOPhelp: "BS_TOP:

Places the text at the top of the button rectangle.", 0
 BS_VCENTERhelp: "BS_VCENTER:

Centers vertically the text on the button rectangle.", 0
 BS_OWNERDRAWhelp: "BS_OWNERDRAW:

The owner window will receives a WM_MEASUREITEM message at creation time and a WM_DRAWITEM message after the visual aspect change is done.

Can't be combined with any other button styles.", 0
 BS_BITMAPhelp: "BS_BITMAP:

The button will display a bitmap. You set it in your Proc.", 0
 BS_ICONhelp: "BS_ICON:

The button will display an Icon. You set it in your Proc.", 0
 BS_MULTILINEhelp: "BS_MULTILINE:

Allows multiple lines if the text string is too long to fit with the button width.", 0
 BS_NOTIFYhelp: "BS_NOTIFY:

The button will send BN_DBLCLK / BN_KILLFOCUS / BN_SETFOCUS notification messages to its parent. 

Note: BN_CLICKED is always send, even without this style. ", 0
 BS_PUSHLIKEhelp: "BS_PUSHLIKE:

Makes a check box or a radio button look like a button, with a state holding behaviour.", 0
 BS_FLAThelp: "BS_FLAT:

The button borders are visible only when activated", 0

WS_GROUPhelp: "WS_GROUP:

To be used with BS_GROUPBOX.

The next coming Controls, for example, a serie of Radio Buttons will behave as a group", 0]

[ButtonStylesHelp: BS_PUSHBUTTONhelp BS_DEFPUSHBUTTONhelp BS_CHECKBOXhelp
 BS_3STATEhelp BS_AUTOCHECKBOXhelp BS_AUTO3STATEhelp BS_RADIOBUTTONhelp
 BS_AUTORADIOBUTTONhelp BS_GROUPBOXhelp BS_LEFTTEXThelp BS_LEFThelp BS_RIGHThelp
 BS_CENTERhelp BS_BOTTOMhelp BS_TOPhelp BS_VCENTERhelp BS_OWNERDRAWhelp
 BS_BITMAPhelp BS_ICONhelp BS_MULTILINEhelp BS_NOTIFYhelp BS_PUSHLIKEhelp
 BS_FLAThelp WS_TABSTOPhelp WS_GROUPhelp WS_DISABLEDhelp WS_CHILDhelp
 WS_VISIBLEhelp WS_BORDERhelp WS_CLIPSIBLINGShelp]


____________________________________

; Edit control tables:



[EditCheckingMask: 03 03 03 0   0 0 0 0   0 0 0 0   0 0 0 0   0 0]

[EditTextTable: B$
 'ES_LEFT' 0         'ES_CENTER' 0        'ES_RIGHT' 0       'ES_MULTILINE' 0
 'ES_AUTOVSCROLL' 0  'ES_AUTOHSCROLL' 0   'ES_LOWERCASE' 0   'ES_UPPERCASE' 0
 'ES_PASSWORD' 0     'ES_OEMCONVERT' 0    'ES_NOHIDESEL' 0   'ES_READONLY' 0
 'ES_NUMBER' 0       'ES_WANTRETURN' 0    'WS_VSCROLL' 0     'WS_HSCROLL' 0
 'WS_TABSTOP' 0      'WS_BORDER' 0 0]

[EditBitTable:
 &ES_LEFT             &ES_CENTER            &ES_RIGHT           &ES_MULTILINE
 &ES_AUTOVSCROLL      &ES_AUTOHSCROLL       &ES_LOWERCASE       &ES_UPPERCASE
 &ES_PASSWORD         &ES_OEMCONVERT        &ES_NOHIDESEL       &ES_READONLY
 &ES_NUMBER           &ES_WANTRETURN        &WS_VSCROLL         &WS_HSCROLL
 &WS_TABSTOP          &WS_BORDER]

[EditExcludeBitTable:
 03  02   01   0
 0   0   08  010
 0   0   0   0
 0   0   0   0
 0   0]

[EditMustHaveBitTable:
 0 0 0 0
 &WS_VSCROLL &WS_HSCROLL 0 0
 0 0 0 0
 0 0 0 0
 0 0]

[ES_LEFThelp: "ES_LEFT:

Text will be Left-aligned.", 0
 ES_CENTERhelp: "ES_CENTER:

For multiline edit control:

Text will be centered", 0
 ES_RIGHThelp: "ES_RIGHT:

For multiline edit control:

Text wil be right-aligned.", 0
 ES_MULTILINEhelp: "ES_MULTILINE:

Multiline edit control. Default is single-line. Whith multi-lines, by default, when pressing ENTER, the default button is activated. To use the ENTER key as a real carriage return, add the ES_WANTRETURN style.

With the ES_AUTOHSCROLL style, the multiline edit control automatically scrolls horizontally when the caret reaches the right edge. To start a new line, the user must press the ENTER key. Without ES_AUTOHSCROLL, the control automatically wraps the words to the beginning of the next line when needed. A new line is also started if the user presses the ENTER key. The word wrap vary with the window size.

With multiline edit you can have scroll bars, and the edit control will process its own scroll bar messages.", 0
 ES_AUTOVSCROLLhelp: "ES_AUTOVSCROLL:

Automatically scrolls text up or down on user action.", 0
 ES_AUTOHSCROLLhelp: "ES_AUTOHSCROLL:

Automatically scrolls text to the right by 10 chars when needed (on user action).", 0
 ES_LOWERCASEhelp: "ES_LOWERCASE:

Converts all imputs to lowercase.", 0
 ES_UPPERCASEhelp: "ES_UPPERCASE:

Converts all Inputs to uppercase.", 0
 ES_PASSWORDhelp: "ES_PASSWORD:

Shows asterisks instead of imputed chars. You can change the (*)s to something else with the EM_SETPASSWORDCHAR message.", 0
 ES_OEMCONVERThelp: "ES_OEMCONVERT:

Converts text entered, from the Windows character set to the OEM character set and then back to the Windows set. This ensures proper character conversion when the application calls the CharToOem function to convert a string to OEM. Useful for retrieving filenames.", 0
 ES_NOHIDESELhelp: "ES_NOHIDESEL:

The selected text will remain selected, even if the control looses the focus. (as opposed to the default behaviour).", 0
 ES_READONLYhelp: "ES_READONLY:

User can't modify the actual text.", 0
 ES_NUMBERhelp: "ES_NUMBER:

Allows only decimal digits inputs.", 0
 ES_WANTRETURNhelp: "ES_WANTRETURN:

For multi-lines edit: The carriage return be inserted in response to ENTER.

Otherwise, ENTER would be considered as a default push button actions.", 0
; WS_VSCROLLhelp: '', 0
; WS_HSCROLLhelp: '', 0
 WS_TABSTOPhelp: "WS_TABSTOP:

Pressing the TAB key changes the keyboard focus to the next control having this style. 

The order of Controls is the one in the Dialog Data.", 0]

; WS_BORDERhelp: '', 0


[EditStylesHelp: ES_LEFThelp ES_CENTERhelp ES_RIGHThelp ES_MULTILINEhelp
  ES_AUTOVSCROLLhelp
 ES_AUTOHSCROLLhelp ES_LOWERCASEhelp ES_UPPERCASEhelp ES_PASSWORDhelp
 ES_OEMCONVERThelp ES_NOHIDESELhelp ES_READONLYhelp ES_NUMBERhelp ES_WANTRETURNhelp
 WS_VSCROLLhelp WS_HSCROLLhelp WS_TABSTOPhelp WS_BORDERhelp]
____________________________________


; Static Controls tables:

[StaticCheckingMask:
 0F 0FFF 0FFF  0FFF
 0FFF 0FFF 0FFF 0FFF
 0FFF 03 03 03
 0 0 0 0
 0 0]

[StaticTextTable: B$
 'SS_SIMPLE' 0        'SS_BITMAP' 0           'SS_ICON' 0        'SS_BLACKRECT' 0
 'SS_GRAYRECT' 0      'SS_WHITERECT' 0        'SS_BLACKFRAME' 0  'SS_GRAYFRAME' 0
 'SS_WHITEFRAME' 0    'SS_CENTER' 0           'SS_LEFT' 0        'SS_RIGHT' 0
 'SS_CENTERIMAGE' 0   'SS_LEFTNOWORDWRAP' 0   'SS_NOPREFIX' 0    'SS_NOTIFY' 0
 'WS_TABSTOP' 0       'WS_BORDER' 0 0]

[StaticBitTable:
 &SS_SIMPLE            &SS_BITMAP             &SS_ICON          &SS_BLACKRECT
 &SS_GRAYRECT          &SS_WHITERECT          &SS_BLACKFRAME    &SS_GRAYFRAME
 &SS_WHITEFRAME        &SS_CENTER             &SS_LEFT          &SS_RIGHT
 &SS_CENTERIMAGE       &SS_LEFTNOWORDWRAP     &SS_NOPREFIX      &SS_NOTIFY
 &WS_TABSTOP           &WS_BORDER] ; &SS_METAPICT &SS_RIGHTIMAGE

[StaticExcludeBitTable:
 0FFF  0FF  0FFF  0FF
 0FF   0FF  0FF   0FF
 0FF   03   03    03
 0      0   0     0
 0 0]

[StaticMustHaveBitTable:
 0  0  0  0
 0  0  0  0
 0  0  0  0
 0  0  0  0
 0  0]

[SS_SIMPLEhelp: "SS_SIMPLE:

A simple short line of left-aligned text in a rectangle.", 0
 SS_BITMAPhelp: "SS_BITMAP:

Room where to display a bitmap.

The Name (Title), when given is for targetting a Bitmap Resource save by Name. Actually RosAsm does not assume this way for saving Resources (only Resources by Number).

You have to defined your BitMap at Run Time from your CallBack Initialisation.

The width and Hight of the Control are dummy: The control automatically resizes to accommodate to the bitmap size.", 0
 SS_ICONhelp: "SS_ICON:

Room where to display an icon.

The Name (Title), when given is for targetting a Bitmap Resource save by Name. Actually RosAsm does not assume this way for saving Resources (only Resources by Number).

You have to defined your BitMap at Run Time from your CallBack Initialisation.

The width and Hight of the Control are dummy: The control automatically resizes to accommodate to the bitmap size", 0
 SS_BLACKRECThelp: "SS_BLACKRECT:

A rectangle filled with the current window frame color.

Default is black.", 0
 SS_GRAYRECThelp: "SS_GRAYRECT:

A rectangle filled with the current screen background color.

Default is gray.", 0
 SS_WHITERECThelp: "SS_WHITERECT:

A rectangle filled with the current window background color.

Defauilt is white.", 0
 SS_BLACKFRAMEhelp: "SS_BLACKFRAME:

A box with a frame drawn in the color of window frames.

Default is black.", 0
 SS_GRAYFRAMEhelp: "SS_GRAYFRAME:

A box with a frame drawn in the color of screen background.

Default is gray.", 0
 SS_WHITEFRAMEhelp: "SS_WHITEFRAME:

A box with a frame drawn with the color of window backgrounds.

Default is white.", 0
 SS_CENTERhelp: "SS_CENTER: 

A rectangle with centered text inside.

Includes wordwrap.", 0
 SS_LEFThelp: "SS_LEFT:

A rectangle with left-aligned text inside.

Includes wordwrap.", 0
 SS_RIGHThelp: "SS_RIGHT:

A rectangle with right-aligned text inside.

Includes wordwrap.", 0
 SS_CENTERIMAGEhelp: "SS_CENTERIMAGE:

With SS_BITMAP or SS_ICON, when resizing to accomodate the image size, the control pos will refer to its center instead of its upper rigth corner.", 0
 SS_LEFTNOWORDWRAPhelp: "SS_LEFTNOWORDWRAP:

Clips past extended text instead of wrapping when too long", 0
 SS_NOPREFIXhelp: "SS_NOPREFIX:

By default ampersand (&) indicates the next Char as a hot key (accelerator). This style negates this behaviour.

You can do the same by simply stating 2 ampersands (&&) instead.", 0
 SS_NOTIFYhelp: "SS_NOTIFY:

Sends STN_CLICKED and STN_DBLCLK notification messages to the parent when the user clicks or double clicks the control.", 0
 ;WS_BORDERhelp: '', 0
]
;
[StaticStylesHelp: SS_SIMPLEhelp SS_BITMAPhelp SS_ICONhelp SS_BLACKRECThelp
 SS_GRAYRECThelp SS_WHITERECThelp SS_BLACKFRAMEhelp SS_GRAYFRAMEhelp
 SS_WHITEFRAMEhelp SS_CENTERhelp SS_LEFThelp SS_RIGHThelp SS_CENTERIMAGEhelp
 SS_LEFTNOWORDWRAPhelp SS_NOPREFIXhelp SS_NOTIFYhelp WS_TABSTOPhelp WS_BORDERhelp]
____________________________________


; List box tables:

[ListCheckingMask:
 0 0 0
 0_FF_FFFF 0 0
 0 0 0
 0
 0 0 0
 0 030 030
 0 0 0 0]

[ListTextTable: B$
 'LBS_HASSTRINGS' 0         'LBS_NOTIFY' 0          'LBS_SORT' 0
 'LBS_STANDARD' 0           'LBS_USETABSTOPS' 0     'LBS_WANTKEYBOARDINPUT' 0
 'LBS_DISABLENOSCROLL' 0    'LBS_EXTENDEDSEL' 0     'LBS_MULTICOLUMN' 0
 'LBS_MULTIPLESEL' 0        'LBS_NODATA' 0          'LBS_NOINTEGRALHEIGHT' 0
 'LBS_NOSEL' 0
 'LBS_NOREDRAW' 0           'LBS_OWNERDRAWFIXED' 0  'LBS_OWNERDRAWVARIABLE' 0
 'WS_TABSTOP' 0             'WS_BORDER' 0           'WS_VSCROLL' 0
 'WS_HSCROLL' 0 0]

[ListBitTable:
 &LBS_HASSTRINGS             &LBS_NOTIFY             &LBS_SORT
 &LBS_STANDARD               &LBS_USETABSTOPS        &LBS_WANTKEYBOARDINPUT
 &LBS_DISABLENOSCROLL        &LBS_EXTENDEDSEL        &LBS_MULTICOLUMN
 &LBS_MULTIPLESEL            &LBS_NODATA             &LBS_NOINTEGRALHEIGHT
 &LBS_NOSEL
 &LBS_NOREDRAW               &LBS_OWNERDRAWFIXED     &LBS_OWNERDRAWVARIABLE
 &WS_TABSTOP                 &WS_BORDER              &WS_VSCROLL        &WS_HSCROLL]

[ListExcludeBitTable:
 02000      0    0
 0FF_FFFF 0    0
 0          0    0
 0          042  0
 0808
 0          030  030
 0            0  0   0]

[ListMustHaveBitTable:
 0  0  0
 0  0  0
 0  0  0
 0  0  0
 0
 0  0  0
 0  0  0 0]

[LBS_HASSTRINGShelp: "LBS_HASSTRINGS:

List of strings.", 0
 LBS_NOTIFYhelp: "LBS_NOTIFY:

Notifies the parent, when the user clicks or double-clicks on a listed item.", 0
 LBS_SORThelp: "LBS_SORT:

Auto-Sorts the strings in alphabetic order.", 0
 LBS_STANDARDhelp: "LBS_STANDARD:

Auto-Sorts the strings in alphabetic order.

Notify parent of user cicks and double-clicks.

Plus side borders", 0
 LBS_USETABSTOPShelp: "LBS_USETABSTOPS:

Tab are drawn with the string, if any.", 0
 LBS_WANTKEYBOARDINPUThelp: "LBS_WANTKEYBOARDINPUT:

Send the owner of the box a WM_VKEYTOITEM messages when user depresses a key.

Enables an application to control keyboard inputs.", 0
 LBS_DISABLENOSCROLLhelp: "LBS_DISABLENOSCROLL:

When the vertical scroll bar is not required for viewing all of the ListBox Items, this style negates the default behaviour, which is to *not* display the scroll bar.", 0
 LBS_EXTENDEDSELhelp: "LBS_EXTENDEDSEL:

Allows multiple items selections by use of the SHIFT key or of the mouse.", 0
 LBS_MULTICOLUMNhelp: "LBS_MULTICOLUMN:

Multicolumn list box, scrolled horizontally.

You set the columns width with LB_SETCOLUMNWIDTH message.", 0
 LBS_MULTIPLESELhelp: "LBS_MULTIPLESEL:

Turns string selection on or off at each clicks on a string. 

The user can select several strings.", 0
 LBS_NODATAhelp: "LBS_NODATA:

Does not work with LBS_SORT or LBS_HASSTRINGS style. A no-data list box is to be used when the number of items is over 1000. Must have the LBS_OWNERDRAWFIXED style. 

See Win Help for more information. You should not use this.", 0
 LBS_NOINTEGRALHEIGHThelp: "LBS_NOINTEGRALHEIGHT:

Block the adaption of the list box size on the Items length.", 0
 LBS_NOSELhelp: "LBS_NOSEL:
 
 The Items viewed in list box cannot be selected", 0
 LBS_NOREDRAWhelp: "LBS_NOREDRAW:

Prevents from updating when changes are made. May be changed with WM_SETREDRAW message.", 0
 LBS_OWNERDRAWFIXEDhelp: "LBS_OWNERDRAWFIXED:

The items are fixed height.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages.", 0
 LBS_OWNERDRAWVARIABLEhelp: "LBS_OWNERDRAWVARIABLE:

Items are variable heights.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages.", 0
; WS_BORDERhelp: ' ', 0
; WS_VSCROLLhelp: ' ', 0
; WS_HSCROLLhelp: ' ', 0
]

[ListBoxStyles: LBS_HASSTRINGShelp LBS_NOTIFYhelp LBS_SORThelp LBS_STANDARDhelp
 LBS_USETABSTOPShelp LBS_WANTKEYBOARDINPUThelp LBS_DISABLENOSCROLLhelp
 LBS_EXTENDEDSELhelp LBS_MULTICOLUMNhelp LBS_MULTIPLESELhelp LBS_NODATAhelp
 LBS_NOINTEGRALHEIGHThelp
 LBS_NOSELhelp
 LBS_NOREDRAWhelp LBS_OWNERDRAWFIXEDhelp LBS_OWNERDRAWVARIABLEhelp
 WS_TABSTOPhelp WS_BORDERhelp WS_VSCROLLhelp WS_HSCROLLhelp]

____________________________________


; Scroll Controls tables:

[ScrollCheckingMask:
 01 06 06
 01 06 06
 010 08 06 06
 0]

[ScrollTextTable: B$
 'SBS_HORZ' 0      'SBS_TOPALIGN' 0    'SBS_BOTTOMALIGN' 0
 'SBS_VERT' 0      'SBS_LEFTALIGN' 0   'SBS_RIGHTALIGN' 0
 'SBS_SIZEBOX' 0   'SBS_SIZEGRIP' 0    'SBS_SIZEBOXBOTTOMRIGHTALIGN' 0
 'SBS_SIZEBOXTOPLEFTALIGN' 0
 'WS_TABSTOP' 0 0]

[ScrollBitTable:
 &SBS_HORZ       &SBS_TOPALIGN     &SBS_BOTTOMALIGN
 &SBS_VERT       &SBS_LEFTALIGN    &SBS_RIGHTALIGN
 &SBS_SIZEBOX    &SBS_SIZEGRIP     &SBS_SIZEBOXBOTTOMRIGHTALIGN
 &SBS_SIZEBOXTOPLEFTALIGN
 &WS_TABSTOP]

[ScrollExcludeBitTable:
 019    019  019
 018    018  018
 010     08    0 0 0]

[ScrollMustHaveBitTable:
 0 0 0
 0 1 1
 0 0 8
 8 0]

[SBS_HORZhelp: "SBS_HORZ:

The scroll bar will be horizontal.", 0
 SBS_TOPALIGNhelp: "SBS_TOPALIGN:

To be used with SBS_HORZ, to align the scroll bar top edge with the bounding rectangle.", 0
 SBS_BOTTOMALIGNhelp: "SBS_BOTTOMALIGN:

To be used with SBS_HORZ, to align the scroll bar bottom edge with the bounding rectangle.", 0
 SBS_VERThelp: "SBS_VERT:

The scroll bar will be vertical.", 0
 SBS_LEFTALIGNhelp: "SBS_LEFTALIGN:

To be used with SBS_VERT, to align the scroll bar left edge with the bounding rectangle.", 0
 SBS_RIGHTALIGNhelp: "SBS_RIGHTALIGN:

To be used with SBS_VERT, to align the scroll bar right edge with the bounding rectangle.", 0
 SBS_SIZEBOXhelp: "SBS_SIZEBOX:

Features the scroll bar with a size box style.

Does not seem to work, at least under 95.

See SBS_SIZEGRIP.", 0
 SBS_SIZEGRIPhelp: "SBS_SIZEGRIP:

Adds a resize bitmap left bottom corner of the scroll bar.", 0
 SBS_SIZEBOXBOTTOMRIGHTALIGNhelp: "SBS_SIZEBOXBOTTOMRIGHTALIGN:

to be used with SBS_SIZEBOX.

Like SBS_SIZEBOX, does not seem to work.", 0
 SBS_SIZEBOXTOPLEFTALIGNhelp: "SBS_SIZEBOXBOTTOMRIGHTALIGN:

To be used with SBS_SIZEBOX.

Like SBS_SIZEBOX, does not seem to work.", 0
 ]

[ScrollStylesHelp: SBS_HORZhelp SBS_TOPALIGNhelp SBS_BOTTOMALIGNhelp SBS_VERThelp SBS_LEFTALIGNhelp
 SBS_RIGHTALIGNhelp SBS_SIZEBOXhelp SBS_SIZEGRIPhelp SBS_SIZEBOXBOTTOMRIGHTALIGNhelp
 SBS_SIZEBOXTOPLEFTALIGNhelp WS_TABSTOPhelp]
____________________________________


; Combo Box tables:

[ComboCheckingMask:
 &CBS_DROPDOWNLIST 0 0
 0 0 0F000
 0F000 &CBS_DROPDOWNLIST &CBS_DROPDOWNLIST
 0 0 0F0 0F0 0 0 0]

[ComboTextTable: B$
 'CBS_SIMPLE' 0            'CBS_HASSTRINGS' 0         'CBS_SORT' 0
 'CBS_AUTOHSCROLL' 0       'CBS_DISABLENOSCROLL' 0    'CBS_LOWERCASE' 0
 'CBS_UPPERCASE' 0         'CBS_DROPDOWN' 0           'CBS_DROPDOWNLIST' 0
 'CBS_NOINTEGRALHEIGHT' 0  'CBS_OEMCONVERT' 0         'CBS_OWNERDRAWFIXED' 0
 'CBS_OWNERDRAWVARIABLE' 0 'WS_VSCROLL' 0             'WS_HSCROLL' 0
 'WS_TABSTOP' 0 0]

[ComboBitTable:
 &CBS_SIMPLE                 &CBS_HASSTRINGS            &CBS_SORT
 &CBS_AUTOHSCROLL            &CBS_DISABLENOSCROLL       &CBS_LOWERCASE
 &CBS_UPPERCASE              &CBS_DROPDOWN              &CBS_DROPDOWNLIST
 &CBS_NOINTEGRALHEIGHT       &CBS_OEMCONVERT            &CBS_OWNERDRAWFIXED
 &CBS_OWNERDRAWVARIABLE      &WS_VSCROLL                &WS_HSCROLL
 &WS_TABSTOP]

[ComboExcludeBitTable:
 0FF     0   0
 0       0   &CBS_UPPERCASE
 &CBS_LOWERCASE  &CBS_DROPDOWNLIST   0
 0       0   &CBS_OWNERDRAWVARIABLE
 &CBS_OWNERDRAWFIXED     0   0
 0]

[ComboMustHaveBitTable:
 0     0     0
 0     &CBS_AUTOHSCROLL  &CBS_HASSTRINGS
 &CBS_HASSTRINGS  0     0
 0     0     0
 0     0     0
 0]

[CBS_SIMPLEhelp: "CBS_SIMPLE:

The list box is always displayed.

The current selection is displayed in the edit control.", 0
 CBS_HASSTRINGShelp: "CBS_HASSTRINGS:

The combo box items are strings.

The application makes use of CB_GETLBTEXT for retrieving one text item.", 0
 CBS_SORThelp: "CBS_SORT:

When you will fill the list box, the items will be Automatically sorted.", 0
 CBS_AUTOHSCROLLhelp: "CBS_AUTOHSCROLL:

In the edit control, the text will scroll Automatically when needed.", 0
 CBS_DISABLENOSCROLLhelp: "CBS_DISABLENOSCROLL:

Does not hide the scroll bar when it is useless.", 0
 CBS_LOWERCASEhelp: "CBS_LOWERCASE:

All Chars entered in the edit control will be turned lower case.", 0
 CBS_UPPERCASEhelp: "CBS_UPPERCASE:

All Chars entered in the edit control will be turned upper case.", 0
 CBS_DROPDOWNhelp: "CBS_DROPDOWN:

Same as CBS_SIMPLE, but the list box will only be displayed after the user selects the down arrow of the edit control.", 0
 CBS_DROPDOWNLISThelp: "CBS_DROPDOWNLIST:

Same as CBS_SIMPLE, but the list box will only be displayed after the user selects the down arrow of the edit control, just like with CBS_DROPDOWN, but the Edit Control is replaced by a static control.", 0
 CBS_NOINTEGRALHEIGHThelp: "CBS_NOINTEGRALHEIGHT:

Blocks automatic resizing depending on the size of items.", 0
 CBS_OEMCONVERThelp: "CBS_OEMCONVERT:

Converts text entered, from the Windows character set to the OEM character set and then back to the Windows set. This ensures proper character conversion when the application calls the CharToOem function to convert a string to OEM.

Useful for retrieving filenames.", 0
 CBS_OWNERDRAWFIXEDhelp: "CBS_OWNERDRAWFIXED:

All items are same hight.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages.", 0
 CBS_OWNERDRAWVARIABLEhelp: "CBS_OWNERDRAWVARIABLE:

The items are variable hights.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages.", 0
; WS_VSCROLLhelp:
; WS_HSCROLLhelp:
 ]

[ComboStylesHelp: CBS_SIMPLEhelp CBS_HASSTRINGShelp CBS_SORThelp CBS_AUTOHSCROLLhelp
 CBS_DISABLENOSCROLLhelp CBS_LOWERCASEhelp CBS_UPPERCASEhelp CBS_DROPDOWNhelp
 CBS_DROPDOWNLISThelp CBS_NOINTEGRALHEIGHThelp CBS_OEMCONVERThelp CBS_OWNERDRAWFIXEDhelp
 CBS_OWNERDRAWVARIABLEhelp WS_VSCROLLhelp WS_HSCROLLhelp WS_TABSTOPhelp]

____________________________________


; UpDown controls tables:

[UpDownCheckingMask:
 0 0 0 0   0 0 0 0  0]

[UpDownTextTable: B$
 'UDS_ALIGNLEFT' 0  'UDS_ALIGNRIGHT' 0  'UDS_SETBUDDYINT' 0  'UDS_NOTHOUSANDS' 0
 'UDS_ARROWKEYS' 0  'UDS_HORZ' 0        'UDS_WRAP' 0         'UDS_AUTOBUDDY', 0
 'WS_TABSTOP' 0 0]

[UpDownBitTable:
 &UDS_ALIGNLEFT      &UDS_ALIGNRIGHT   &UDS_SETBUDDYINT   &UDS_NOTHOUSANDS
 &UDS_ARROWKEYS      &UDS_HORZ         &UDS_WRAP          &UDS_AUTOBUDDY
 &WS_TABSTOP]

[UpDownExcludeBitTable:
 4  8  0  0
 0  0  0  0
 0]

[UpDownMustHaveBitTable:
 0  0  0  0
 0  0  0  0
 0]

[UDS_ALIGNLEFThelp: "UDS_ALIGNLEFT:

The up-down control will be at the left edge of the buddy window.", 0
 UDS_ALIGNRIGHThelp: "UDS_ALIGNRIGHT:

The up-down control will be at the right edge of the buddy window.", 0
 UDS_SETBUDDYINThelp: "UDS_SETBUDDYINT:

The up-down control will set the text of the buddy window when needed (numbers only).

The buddy window may be set by UDS_AUTOBUDDY.", 0
 UDS_NOTHOUSANDShelp: "UDS_NOTHOUSANDS:

Reverses default, which is to insert a thousands'separator between each three digits.", 0
 UDS_ARROWKEYShelp: "UDS_ARROWKEYS:

Allows keyboard UP ARROW and DOWN ARROW keys alternate inputs.", 0
 UDS_HORZhelp: "UDS_HORZ:

Default is vertical...", 0
 UDS_WRAPhelp: "UDS_WRAP:

Restarts at other end, when moving out of range.", 0
 UDS_AUTOBUDDYhelp: "UDS_AUTOBUDDY:
 
Automatically selcts the previous Z order window as the associated buddy window.
The 'previous Z order Window' is the previous one in the Template.", 0]

[UpDownStylesHelp: UDS_ALIGNLEFThelp UDS_ALIGNRIGHThelp UDS_SETBUDDYINThelp
 UDS_NOTHOUSANDShelp UDS_ARROWKEYShelp UDS_HORZhelp UDS_WRAPhelp UDS_AUTOBUDDYhelp
 WS_TABSTOPhelp]
____________________________________


; Progress bar have nothing to edit:

[ProgressTextTable: 0 0]
____________________________________

; Track Bars controls tables:

[TrackCheckingMask:
 0 0 0   0 0 0  0 0 0  0 0 0
 0]

[TrackTextTable: B$
 'TBS_HORZ' 0            'TBS_BOTTOM' 0        'TBS_TOP' 0
 'TBS_VERT' 0            'TBS_RIGHT' 0         'TBS_LEFT' 0
 'TBS_AUTOTICKS' 0       'TBS_NOTICKS' 0       'TBS_BOTH' 0
 'TBS_ENABLESELRANGE' 0  'TBS_FIXEDLENGTH' 0   'TBS_NOTHUMB' 0
 'WS_TABSTOP' 0 0]

[TrackBitTable:
 &TBS_HORZ                 &TBS_BOTTOM            &TBS_TOP
 &TBS_VERT                 &TBS_RIGHT             &TBS_LEFT
 &TBS_AUTOTICKS            &TBS_NOTICKS           &TBS_BOTH
 &TBS_ENABLESELRANGE       &TBS_FIXEDLENGTH       &TBS_NOTHUMB
 &WS_TABSTOP]

[TrackExcludeBitTable:
 2  2  2
 0  0  0
 0  0  0
 0  0  0
 0]

[TrackMustHaveBitTable:
 0  0  0
 0  2  2
 0  0  0
 0  0  0
 0]

[TBS_HORZhelp: "TBS_HORZ:

The trackbar will be horizontal.", 0
 TBS_BOTTOMhelp: "TBS_BOTTOM:

Position of the tick marks in an horizontal trackbar.", 0
 TBS_TOPhelp: "TBS_TOP:

Position of the tick marks in an horizontal trackbar.", 0
 TBS_VERThelp: "TBS_VERT:

The trackbar will be vertical.", 0
 TBS_RIGHThelp: "TBS_RIGHT:

Position of the tick marks in a vertical trackbar.", 0
 TBS_LEFThelp: "TBS_LEFT:

Position of the tick marks in a vertical trackbar.", 0
 TBS_AUTOTICKShelp: "TBS_AUTOTICKS:

The tick mark will be added automatically when your application will send the TBM_SETRANGE message.

Otherwise, you can send the TBM_SETTIC and TBM_SETTICFREQ, to specify these positions.", 0
 TBS_NOTICKShelp: "TBS_NOTICKS:

Does not display the tick marks.", 0
 TBS_BOTHhelp: "TBS_BOTH:

Displays tick marks on both side of the track bar (left/right with TBS_VERT.Down top with TBS_HORZ).", 0
 TBS_ENABLESELRANGEhelp: "TBS_ENABLESELRANGE:

Displays edge ticks as triangles and the selection range is highlighted.", 0
 TBS_FIXEDLENGTHhelp: "TBS_FIXEDLENGTH:

The slider size will not vary with the size of range, as it does by default.", 0
 TBS_NOTHUMBhelp: "TBS_NOTHUMB:

Disables the slider.", 0]

[TrackStylesHelp: TBS_HORZhelp TBS_BOTTOMhelp TBS_TOPhelp TBS_VERThelp TBS_RIGHThelp
 TBS_LEFThelp TBS_AUTOTICKShelp TBS_NOTICKShelp TBS_BOTHhelp TBS_ENABLESELRANGEhelp
 TBS_FIXEDLENGTHhelp TBS_NOTHUMBhelp WS_TABSTOPhelp]
____________________________________

; TreeView Controls tables:

[TreeCheckingMask:
 0 0 0 0
 0 0 0 0
 0 0 0 0
 0 0 0 0
 0]

[TreeTextTable: B$
 'TVS_HASLINES' 0  'TVS_LINESATROOT' 0  'TVS_HASBUTTONS' 0  'TVS_EDITLABELS' 0
 'TVS_SHOWSELALWAYS' 0 'TVS_DISABLEDRAGDROP ', 0 'TVS_RTLREADING ', 0 'TVS_NOTOOLTIPS ', 0
 'TVS_CHECKBOXES ', 0 'TVS_TRACKSELECT ', 0 'TVS_SINGLEEXPAND ', 0 'TVS_INFOTIP ', 0
 'TVS_FULLROWSELECT ', 0 'TVS_NOSCROLL ', 0 'TVS_NONEVENHEIGHT ', 0 'TVS_NOHSCROLL ', 0
 'WS_TABSTOP' 0, 0]

[TreeBitTable: &TVS_HASLINES  &TVS_LINESATROOT  &TVS_HASBUTTONS  &TVS_EDITLABELS
 &TVS_SHOWSELALWAYS &TVS_DISABLEDRAGDROP &TVS_RTLREADING &TVS_NOTOOLTIPS
 &TVS_CHECKBOXES &TVS_TRACKSELECT &TVS_SINGLEEXPAND &TVS_INFOTIP
 &TVS_FULLROWSELECT &TVS_NOSCROLL &TVS_NONEVENHEIGHT &TVS_NOHSCROLL
 &WS_TABSTOP]

 [TreeExcludeBitTable: &TVS_FULLROWSELECT 0 0 0
 0 0 0 &TVS_INFOTIP
 0 0 0 &TVS_NOTOOLTIPS
 &TVS_HASLINES 0 0 0
 0]

[TreeMustHaveBitTable: 0 0 0 0
 0 0 0 0
 0 0 0 0
 0 0 0 0
 0]

[TVS_HASLINEShelp: "TVS_HASLINES:

Draws lines showing the items hierarchy.", 0
 TVS_LINESATROOThelp: "TVS_LINESATROOT:

Draws lines between root and items.

Does not work with TVS_HASLINES.", 0
 TVS_HASBUTTONShelp: "TVS_HASBUTTONS:

Displays plus and minus buttons aside the parent items.

They are used to open / close the lists of  child items.", 0
 TVS_EDITLABELShelp: "TVS_EDITLABELS:

Allows edition of the items by user.", 0
 TVS_SHOWSELALWAYShelp: "TVS_SHOWSELALWAYS:

When lossing the focus, the tree-view will maintain the actual selection.", 0

TVS_DISABLEDRAGDROPhelp: "TVS_DISABLEDRAGDROP:

Prevents the tree-view control from sending TVN_BEGINDRAG notification messages. ", 0

TVS_RTLREADINGhelp: "TVS_RTLREADING:

Version 4.70. Causes text to be displayed from right-to-left (RTL).
Usually, windows display text left-to-right (LTR). Windows can be mirrored to display languages such
as Hebrew or Arabic that read RTL. Typically, tree-view text is displayed in the same direction as the text
in its parent window. If TVS_RTLREADING is set, tree-view text reads in the opposite direction from the text in
the parent window.", 0

TVS_NOTOOLTIPShelp: "TVS_NOTOOLTIPS:

Version 4.70. Disables ToolTips.", 0

TVS_CHECKBOXEShelp: "TVS_CHECKBOXES:

Version 4.70. Enables check boxes for items in a tree-view control. A check box is displayed only if an image is
associated with the item. When set to this style, the control effectively uses DrawFrameControl to create and set
a state image list containing two images. State image 1 is the unchecked box and state image 2 is the checked box.
Setting the state image to zero removes the check box altogether.

Version 5.80. Displays a check box even if no image is associated with the item. Once a tree-view control is created
with this style, the style cannot be removed. Instead, you must destroy the control and create a new one in its place.
Destroying the tree-view control does not destroy the check box state image list. You must destroy it explicitly.
Get the handle to the state image list by sending the tree-view control a TVM_GETIMAGELIST message.
Then destroy the image list with ImageList_Destroy.
     
If you want to use this style, you must set the TVS_CHECKBOXES style with SetWindowLong after you create the
treeview control, and before you populate the tree. Otherwise, the checkboxes might appear unchecked, depending
on timing issues.", 0

TVS_TRACKSELECThelp: "TVS_TRACKSELECT:

Version 4.70. Enables hot tracking in a tree-view control.", 0

TVS_SINGLEEXPANDhelp: "TVS_SINGLEEXPAND:

Version 4.71. Causes the item being selected to expand and the item being unselected to collapse upon selection
in the tree view. If the mouse is used to single-click the selected item and that item is closed, it will be expanded.
If the user holds down the CTRL key while selecting an item, the item being unselected will not be collapsed.

Version 5.80. Causes the item being selected to expand and the item being unselected to collapse upon selection in
the tree view. If the user holds down the CTRL key while selecting an item, the item being unselected will not be
collapsed.", 0

TVS_INFOTIPhelp: "TVS_INFOTIP:

Version 4.71. Obtains ToolTip information by sending the TVN_GETINFOTIP notification.", 0

TVS_FULLROWSELECThelp: "TVS_FULLROWSELECT:

Version 4.71. Enables full-row selection in the tree view. The entire row of the selected item is highlighted,
and clicking anywhere on an item's row causes it to be selected. This style cannot be used in conjunction with
the TVS_HASLINES style.", 0

TVS_NOSCROLLhelp: "TVS_NOSCROLL:

Version 5.80. Disables horizontal scrolling in the control. The control will not display any horizontal scroll bars.", 0

TVS_NONEVENHEIGHThelp: "TVS_NONEVENHEIGHT:

Version 4.71 Sets the height of the items to an odd height with the TVM_SETITEMHEIGHT message. By default, the height
of items must be an even value.", 0

TVS_NOHSCROLLhelp: "TVS_NOHSCROLL:

Version 5.80. Disables horizontal scrolling in the control. The control will not display any horizontal scroll bars.", 0]

[TreeViewStylesHelp: TVS_HASLINEShelp TVS_LINESATROOThelp  TVS_HASBUTTONShelp
 TVS_EDITLABELShelp TVS_SHOWSELALWAYShelp TVS_DISABLEDRAGDROPhelp TVS_RTLREADINGhelp
 TVS_NOTOOLTIPShelp TVS_CHECKBOXEShelp TVS_TRACKSELECThelp TVS_SINGLEEXPANDhelp TVS_INFOTIPhelp
 TVS_FULLROWSELECThelp TVS_NOSCROLLhelp TVS_NONEVENHEIGHThelp TVS_NOHSCROLLhelp
 WS_TABSTOPhelp]

____________________________________

; Tabs Controls tables:

[TabCheckingMask:   0 0 0
                    0 0 0
                    0 0 0
                    0 0 0
                    0 0 0
                    0 0 0
                    0]

[TabTextTable:  B$ 'TCS_SCROLLOPPOSITE' 0  'TCS_RIGHT/TCS_BOTTOM' 0   'TCS_MULTISELECT' 0
                'TCS_FLATBUTTONS' 0     'TCS_FORCEICONLEFT' 0   'TCS_FORCELABELLEFT' 0
                'TCS_HOTTRACK' 0        'TCS_VERTICAL' 0        'TCS_BUTTONS' 0
                'TCS_MULTILINE' 0       'TCS_FIXEDWIDTH' 0      'TCS_RAGGEDRIGHT' 0
                'TCS_FOCUSONBUTTONDOWN' 0   'TCS_OWNERDRAWFIXED' 0      'TCS_TOOLTIPS' 0
                'TCS_FOCUSNEVER' 0      'WS_TABSTOP' 0      'WS_CHILD' 0
                'WS_VISIBLE' , 0, 0]

[TabBitTable:   &TCS_SCROLLOPPOSITE     &TCS_RIGHT              &TCS_MULTISELECT
                &TCS_FLATBUTTONS        &TCS_FORCEICONLEFT      &TCS_FORCELABELLEFT
                &TCS_HOTTRACK           &TCS_VERTICAL           &TCS_BUTTONS
                &TCS_MULTILINE          &TCS_FIXEDWIDTH         &TCS_RAGGEDRIGHT
                &TCS_FOCUSONBUTTONDOWN  &TCS_OWNERDRAWFIXED     &TCS_TOOLTIPS
                &TCS_FOCUSNEVER         &WS_TABSTOP             &WS_CHILD
                &WS_VISIBLE]


[TabExcludeBitTable:    0 0 0
                        0 &TCS_FORCELABELLEFT 0
                        0 0 0
                        0 0 0
                        &TCS_FOCUSNEVER 0 0
                        &TCS_FOCUSONBUTTONDOWN 0 0
                        0]

[TabMustHaveBitTable:   0 0 0
                        &TCS_BUTTONS &TCS_FIXEDWIDTH &TCS_FIXEDWIDTH__&TCS_FORCEICONLEFT
                        0 0 0
                        0 &TCS_FORCELABELLEFT 0
                        &TCS_BUTTONS 0 0
                        0 0 0
                        0]


[TCS_SCROLLOPPOSITEhelp: " TCS_SCROLLOPPOSITE:

Scrolls the Tab Control", 0

TCS_RIGHThelp: "TCS_RIGHT:

Places the Tab control vertically at the right corner of the control. It must be used with TCS_VERTICAL to display the tabs at the right corner.

This equate is the same as TCS_BOTTOM. But if it is used alone, the tab itens are placed horizontally at the bottom of the control.", 0

TCS_MULTISELECThelp: "TCS_MULTISELECT:

Enables multi-selection mode of the tab itens", 0

TCS_FLATBUTTONShelp: "TCS_FLATBUTTONS:

The tab itens are in Flat mode", 0

TCS_FORCEICONLEFThelp: "TCS_FORCEICONLEFT:

Only with TCS_FIXEDWIDTH.

Icons will be aligned to the left of each tab.", 0
TCS_FORCELABELLEFThelp: "TCS_FORCELABELLEFT:

Only with TCS_FIXEDWIDTH.

Labels will be be aligned to the left of each tab.", 0

TCS_HOTTRACKhelp: "TCS_HOTTRACK:

Highlite the Text Label of the Tab.", 0

TCS_VERTICALhelp: "TCS_VERTICAL:

Places the Tab itens vertically. By default the itens are places in the left corner of the Control. To display them in the right corner, use this equate with TCS_RIGHT.", 0


TCS_BUTTONShelp: "TCS_BUTTONS:

Tabs are shown in the form of buttons.", 0


TCS_MULTILINEhelp: "TCS_MULTILINE:

Allows multiple rows of tabs, when needed.", 0


TCS_FIXEDWIDTHhelp: "TCS_FIXEDWIDTH:

Cannot be combined with TCS_RIGHTJUSTIFY.

All tabs will be the same width.", 0

TCS_RAGGEDRIGHThelp: "TCS_RAGGEDRIGHT:

By default, Tabs are extended to entire fill the with of their control.

TCS_RAGGEDRIGHT prevents from this if unwanted.", 0

TCS_FOCUSONBUTTONDOWNhelp: "CS_FOCUSONBUTTONDOWN:

Tabs receive the focus when clicked.", 0

TCS_OWNERDRAWFIXEDhelp: "TCS_OWNERDRAWFIXED:

The parent window will draw the tabs.", 0

TCS_TOOLTIPShelp: "TCS_TOOLTIPS:

The tabs control will display tooltips.

See Tooltip Controls in Win Help.", 0


TCS_FOCUSNEVERhelp: "TCS_FOCUSNEVER:

Tabs never receive the focus (the Dialog does).", 0

CCS_BOTTOMhelp: "CCS_BOTTOM:

Common Controls style:

Inverts the vertical apearance of the control.", 0]


[TabStylesHelp: TCS_SCROLLOPPOSITEhelp  TCS_RIGHThelp   TCS_MULTISELECThelp
                TCS_FLATBUTTONShelp     TCS_FORCEICONLEFThelp   TCS_FORCELABELLEFThelp
                TCS_HOTTRACKhelp        TCS_VERTICALhelp        TCS_BUTTONShelp
                TCS_MULTILINEhelp       TCS_FIXEDWIDTHhelp      TCS_RAGGEDRIGHThelp
                TCS_FOCUSONBUTTONDOWNhelp       TCS_OWNERDRAWFIXEDhelp  TCS_TOOLTIPShelp
                TCS_FOCUSNEVERhelp      WS_TABSTOPhelp          WS_CHILDhelp
                WS_VISIBLEhelp]

____________________________________


; ListView Controls tables:

;[ListViewCheckingMask: &LVS_LIST &LVS_LIST &LVS_LIST &LVS_LIST 0
[ListViewCheckingMask: 0 03 03 03 0
 0F0 0F0 0
 0F000
 0 0
 0 0
 0
 0 0
 ;0F0 0
 0
 0F000
 0]

[ListViewTextTable: B$
 'LVS_ICON' 0  'LVS_SMALLICON' 0  'LVS_LIST' 0  'LVS_REPORT' 0  'LVS_NOCOLUMNHEADER' 0
 'LVS_SORTASCENDING' 0  'LVS_SORTDESCENDING' 0  'LVS_NOSORTHEADER' 0
 'LVS_AUTOARRANGE' 0
 'LVS_OWNERDRAWFIXED' 0 'LVS_OWNERDATA' 0
 'LVS_EDITLABELS' 0 'LVS_NOLABELWRAP' 0
 'LVS_NOSCROLL' 0
 'LVS_SINGLESEL' 0 'LVS_SHOWSELALWAYS' 0
 'LVS_SHAREIMAGELISTS' 0
 'LVS_ALIGNLEFT' 0
 'WS_TABSTOP' 0 0]

[ListViewBitTable: &LVS_ICON  &LVS_SMALLICON  &LVS_LIST  &LVS_REPORT  &LVS_NOCOLUMNHEADER
 &LVS_SORTASCENDING &LVS_SORTDESCENDING &LVS_NOSORTHEADER
 &LVS_AUTOARRANGE
 &LVS_OWNERDRAWFIXED &LVS_OWNERDATA
 &LVS_EDITLABELS &LVS_NOLABELWRAP
 &LVS_NOSCROLL
 &LVS_SINGLESEL &LVS_SHOWSELALWAYS
 &LVS_SHAREIMAGELISTS
 &LVS_ALIGNLEFT
 &WS_TABSTOP]
; &LVS_ALIGNLEFT &LVS_ALIGNTOP &LVS_AUTOARRANGE &LVS_BUTTON &LVS_EDITLABELS &LVS_NOLABELWRAP
; &LVS_NOSCROLL &LVS_NOSORTHEADER &LVS_OWNERDRAWFIXED &LVS_SHAREIMAGELISTS &LVS_SHOWSELALWAYS
; &LVS_SINGLESEL &LVS_SORTASCENDING &LVS_SORTDESCENDING

;[ListViewExcludeBitTable: &LVS_LIST  &LVS_LIST  &LVS_LIST  &LVS_LIST__&LVS_ALIGNLEFT  0
[ListViewExcludeBitTable:
 &LVS_SMALLICON__&LVS_LIST__&LVS_REPORT
 &LVS_LIST__&LVS_REPORT__&LVS_ICON
 &LVS_SMALLICON__&LVS_REPORT__&LVS_ICON
 &LVS_ICON__&LVS_SMALLICON__&LVS_LIST__&LVS_ALIGNLEFT  0

 &LVS_SORTDESCENDING__&LVS_NOSORTHEADER__&LVS_AUTOARRANGE &LVS_SORTASCENDING__&LVS_NOSORTHEADER__&LVS_AUTOARRANGE &LVS_SORTASCENDING__&LVS_SORTDESCENDING
 &LVS_SORTASCENDING__&LVS_SORTDESCENDING
 0 0
 0 0
 0
 0 0
 0
 &LVS_REPORT
 0]

[ListViewMustHaveBitTable: 0 0 0 0 0
 0 0 0
 0
 0 0
 0 0
 0
 0 0
 0
 0
 0]

[LVS_ICONhelp: "LVS_ICON:

Indicates an Icon view.", 0
 LVS_SMALLICONhelp: "LVS_SMALLICON:

Indicates a small Icon view.", 0
 LVS_LISThelp: "LVS_LIST:

Simple list view.", 0
 LVS_REPORThelp: "LVS_REPORT:

Report view: The first column is always left-aligned, and you cannot use LVCFMT_RIGHT to change this.", 0
 LVS_NOCOLUMNHEADERhelp: "LVS_NOCOLUMNHEADER:

A column header is not displayed (as it would be by default).", 0

 LVS_SORTASCENDINGhelp: "LVS_SORTASCENDING:

Sorts the List View in ascendent order accordying to the 1st item of your list view.", 0

 LVS_SORTDESCENDINGhelp: "LVS_SORTDESCENDING:

Sorts the List View in descendent order.", 0

 LVS_NOSORTHEADERhelp: "LVS_NOSORTHEADER:

Disables the Sort ordering of the header of the ListView Control.", 0

 LVS_AUTOARRANGEhelp: "LVS_AUTOARRANGE:

Auto-Organize the List View Control Itens. This is used by default.", 0

 LVS_OWNERDRAWFIXEDhelp: "LVS_OWNERDRAWFIXED:
The items are fixed height.

The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages.", 0

 LVS_OWNERDATAhelp: "LVS_OWNERDATA:
The owner of the list box redraws its contents when it receives the WM_MEASUREITEM (on creation) or WM_DRAWITEM (on change) messages.", 0

 LVS_EDITLABELShelp: "LVS_EDITLABELS:
Allows editing the item Label. To allow edition all you have to do is press the left button of the mouse over a selected item for a couple of seconds to allow the edition mode", 0

 LVS_NOLABELWRAPhelp: "LVS_NOLABELWRAP:
Disables the wrapping mode of the item label", 0

 LVS_NOSCROLLhelp: "LVS_NOSCROLL:
Disables the scrollbar of the ListView Control", 0

 LVS_SINGLESELhelp: "LVS_SINGLESEL:
Allows selection of a row", 0

 LVS_SHOWSELALWAYShelp: "LVS_SHOWSELALWAYS:
Always displays the selection mode", 0

 LVS_SHAREIMAGELISTShelp: "LVS_SHAREIMAGELISTS:
Allows using images or icons with the ListView Control", 0]

[ListViewStylesHelp: LVS_ICONhelp LVS_SMALLICONhelp LVS_LISThelp LVS_REPORThelp
 LVS_NOCOLUMNHEADERhelp LVS_SORTASCENDINGhelp LVS_SORTDESCENDINGhelp LVS_NOSORTHEADERhelp
 LVS_AUTOARRANGEhelp LVS_OWNERDRAWFIXEDhelp LVS_OWNERDATAhelp LVS_EDITLABELShelp
 LVS_NOLABELWRAPhelp LVS_NOSCROLLhelp LVS_SINGLESELhelp LVS_SHOWSELALWAYShelp
 LVS_SHAREIMAGELISTShelp WS_TABSTOPhelp]


____________________________________


; ToolBar Control Tables:

[ToolBarCheckingMask:
0F  0F  0F  0F
0F  0F  0F0 0F0
0F0 0F0 0   0
0   0   0   0
0   0   0F  0F
0F0 0F0 0F  0F
0F  0F  0F  0F
0F  0F  0F  0F
0F  0   0   0
0   0   0   0]

[ToolBarTextTable: B$
'TBSTYLE_BUTTON' 0  'TBSTYLE_SEP' 0 'TBSTYLE_CHECK' 0   'TBSTYLE_GROUP' 0
'TBSTYLE_CHECKGROUP' 0  'TBSTYLE_DROPDOWN' 0    'TBSTYLE_AUTOSIZE' 0    'TBSTYLE_NOPREFIX' 0
'BTNS_SHOWTEXT' 0   'BTNS_WHOLEDROPDOWN' 0  'TBSTYLE_TOOLTIPS' 0    'TBSTYLE_WRAPABLE' 0
'TBSTYLE_ALTDRAG' 0 'TBSTYLE_FLAT' 0    'TBSTYLE_LIST' 0    'TBSTYLE_CUSTOMERASE' 0
'TBSTYLE_REGISTERDROP' 0    'TBSTYLE_TRANSPARENT' 0
'TBSTYLE_EX_DRAWDDARROWS' 0 'TBSTYLE_EX_MIXEDBUTTONS' 0 'TBSTYLE_EX_HIDECLIPPEDBUTTONS' 0   'TBSTYLE_EX_DOUBLEBUFFER' 0
'CCS_TOP' 0 'CCS_NOMOVEY' 0 'CCS_BOTTOM' 0  'CCS_NORESIZE' 0
'CCS_NOPARENTALIGN' 0   'CCS_ADJUSTABLE' 0  'CCS_NODIVIDER' 0   'CCS_VERT' 0
'CCS_LEFT' 0    'CCS_NOMOVEX' 0 'CCS_RIGHT' 0

'WS_TABSTOP' 0  'WS_GROUP' 0    'WS_DISABLED' 0 'WS_CHILD' 0
'WS_VISIBLE' 0  'WS_BORDER',0   'WS_CLIPSIBLINGS', 0 0]

[ToolBarBitTable:
&TBSTYLE_BUTTON     &TBSTYLE_SEP        &TBSTYLE_CHECK      &TBSTYLE_GROUP
&TBSTYLE_CHECKGROUP &TBSTYLE_DROPDOWN   &TBSTYLE_AUTOSIZE   &TBSTYLE_NOPREFIX
&BTNS_SHOWTEXT      &BTNS_WHOLEDROPDOWN &TBSTYLE_TOOLTIPS   &TBSTYLE_WRAPABLE
&TBSTYLE_ALTDRAG    &TBSTYLE_FLAT       &TBSTYLE_LIST       &TBSTYLE_CUSTOMERASE
&TBSTYLE_REGISTERDROP   &TBSTYLE_TRANSPARENT
&TBSTYLE_EX_DRAWDDARROWS    &TBSTYLE_EX_MIXEDBUTTONS    &TBSTYLE_EX_HIDECLIPPEDBUTTONS  &TBSTYLE_EX_DOUBLEBUFFER
&CCS_TOP    &CCS_NOMOVEY    &CCS_BOTTOM &CCS_NORESIZE
&CCS_NOPARENTALIGN  &CCS_ADJUSTABLE &CCS_NODIVIDER  &CCS_VERT
&CCS_LEFT   &CCS_NOMOVEX    &CCS_RIGHT

&WS_TABSTOP &WS_GROUP   &WS_DISABLED    &WS_CHILD
&WS_VISIBLE &WS_BORDER  &WS_CLIPSIBLINGS]

[ToolBarExcludeBitTable:
0FFFF   0FFFF   0FFFF   0FFFF
0FFFF   0FFFF   0FFFF   0FFFF
0FFFF   0FFFF   0   0
0   0   0   0
0   0
0FFFF   0FFFF   0FFFF   0FFFF
0FFFF   0FFFF   0FFFF   0FFFF
0FFFF   0FFFF   0FFFF   0FFFF
0FFFF   0FFFF   0FFFF
0   0   0   0
0   0   0]

[ToolBarMustHaveBitTable:
0   0   0   0
0   0   0   0
&TBSTYLE_LIST+&TBSTYLE_EX_MIXEDBUTTONS  0   0   0
&CCS_ADJUSTABLE 0   0   0
0   0   0   0
0   0   0   0
0   0   0   0
0   0   0   0
0   0   0   0
0   0   0   0]


[TBSTYLE_BUTTONhelp: "TBSTYLE_BUTTON:

Default ToolBar Style. Initialize the Control as a Button.", 0]

[CCS_TOPhelp: "CCS_TOP:

Common Controls style:

Inverts the Horizontal appearance of the control.", 0]

[TBSTYLE_CHECKhelp: "TBSTYLE_CHECK:

Allows using a CheckBox on the ToolBar.", 0]
;;
CCS_BOTTOMhelp: "CCS_BOTTOM:
 
Common Controls style:

Inverts the vertical appearance of the control.", 0
;;
[TBSTYLE_GROUPhelp: "TBSTYLE_GROUP:

Uses the ToolBar as Group.", 0]

[TBSTYLE_CHECKGROUPhelp: "TBSTYLE_CHECKGROUP:

Uses the ToolBar as a CheckBox Group.", 0]

[TBSTYLE_DROPDOWNhelp: "TBSTYLE_DROPDOWN:

 Enables the dropdown mode on the ToolBar.", 0]

[TBSTYLE_AUTOSIZEhelp: "TBSTYLE_AUTOSIZE:

AutoSize the ToolBar to fit the window width.", 0]

[CCS_ADJUSTABLEhelp: "CCS_ADJUSTABLE:
 
Common Controls style:

Enables the customization mode of the control.", 0]

[CCS_LEFThelp: "CCS_LEFT:
 
Common Controls style:

Place the Control at the Left Corner of the window.", 0]

[CCS_RIGHThelp: "CCS_RIGHT:
 
Common Controls style:

Place the Control at the Right Corner of the window.", 0]

[TBSTYLE_TOOLTIPShelp: "TBSTYLE_TOOLTIPS:
 
Enables showing tooltips on the ToolBar.", 0]

[TBSTYLE_WRAPABLEhelp: "TBSTYLE_WRAPABLE:
 
Allows warp mode on the ToolBar.", 0]

[TBSTYLE_ALTDRAGhelp: "TBSTYLE_ALTDRAG:
 
Allows dragging and drop the ToolBar.

To do this, press the key Alt and the left mouse button and drag a certain Button in the ToolBar.", 0]

[TBSTYLE_FLAThelp: "TBSTYLE_FLAT:

Shows the ToolBar as Flat.", 0]

[TBSTYLE_LISThelp: "TBSTYLE_LIST:
 
Enables using a List Control on the ToolBar.", 0]

[TBSTYLE_CUSTOMERASEhelp: "TBSTYLE_CUSTOMERASE:
 
Allows customizatino of the ToolBar.", 0]

[TBSTYLE_REGISTERDROPhelp: "TBSTYLE_REGISTERDROP:


Allows registering the drag and drop mode.", 0]


[TBSTYLE_TRANSPARENThelp: "TBSTYLE_TRANSPARENT:
 

Shows the toolbar button as transparent to insert masked images.", 0]


[TBSTYLE_SEPhelp: "TBSTYLE_SEP:


version 4.72 and earlier. This is equivalent to BTNS_SEP (for version 5.80 and later).

Creates a separator, providing a small gap between button groups.

A button that has this style does not receive user input.", 0]


[TBSTYLE_NOPREFIXhelp: "TBSTYLE_NOPREFIX:


version 4.72 and earlier. This is equivalent to BTNS_NOPREFIX  (for version 5.80 and later).

Specifies that the button text will not have an accelerator prefix associated with it.", 0]


[BTNS_SHOWTEXThelp: "BTNS_SHOWTEXT:


Version 5.81. Specifies that button text should be displayed.

All buttons can have text, but only those buttons with the BTNS_SHOWTEXT button style will display it.

This button style must be used with the TBSTYLE_LIST style and the TBSTYLE_EX_MIXEDBUTTONS extended style.

If you set text for buttons that do not have the BTNS_SHOWTEXT style, the toolbar control will automatically display it as a ToolTip when the cursor hovers over the button.

This feature allows your application to avoid handling the TBN_GETINFOTIP notification for the toolbar.", 0]


[BTNS_WHOLEDROPDOWNhelp: "BTNS_WHOLEDROPDOWN:


Version 5.80. Specifies that the button will have a drop-down arrow, but not as a separate section.

Buttons with this style behave the same, regardless of whether the TBSTYLE_EX_DRAWDDARROWS extended style is set.", 0]


[TBSTYLE_EX_DRAWDDARROWShelp: "TBSTYLE_EX_DRAWDDARROWS:


Version 4.71. This style allows buttons to have a separate dropdown arrow.

Buttons that have the BTNS_DROPDOWN style will be drawn with a drop-down arrow in a separate section, to the right of the button.

If the arrow is clicked, only the arrow portion of the button will depress, and the toolbar control will send a TBN_DROPDOWN notification to prompt the application to display the dropdown menu.

If the main part of the button is clicked, the toolbar control sends a WM_COMMAND message with the button's ID. The application normally responds by launching the first command on the menu.

There are many situations where you may want to have only some of the dropdown buttons on a toolbar with separated arrows. To do so, set the TBSTYLE_EX_DRAWDDARROWS extended style. Give those buttons that will not have separated arrows the BTNS_WHOLEDROPDOWN style. Buttons with this style will have an arrow displayed next to the image.

However, the arrow will not be separate and when any part of the button is clicked, the toolbar control will send a TBN_DROPDOWN notification.

To prevent repainting problems, this style should be set before the toolbar control becomes visible.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message.", 0]


[TBSTYLE_EX_HIDECLIPPEDBUTTONShelp: "TBSTYLE_EX_HIDECLIPPEDBUTTONS:

Version 5.81. This style hides partially clipped buttons.

The most common use of this style is for toolbars that are part of a rebar control. If an adjacent band covers part of a button, the button will not be displayed.
However, if the rebar band has the RBBS_USECHEVRON style, the button will be displayed on the chevron's dropdown menu.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message.", 0]

[TBSTYLE_EX_DOUBLEBUFFERhelp: "TBSTYLE_EX_DOUBLEBUFFER:

Version 6. This style requires the toolbar to be double buffered. Double buffering is a mechanism that detects when the toolbar has changed.
Comctl32.dll version 6 is not redistributable but it is included in Microsoft Windows XP or later. To use Comctl32.dll version 6, specify it in a manifest.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message.", 0]

[TBSTYLE_EX_MIXEDBUTTONShelp: "TBSTYLE_EX_MIXEDBUTTONS:


Version 5.81. This style allows you to set text for all buttons, but only display it for those buttons with the BTNS_SHOWTEXT button style.

The TBSTYLE_LIST style must also be set. Normally, when a button does not display text, your application must handle TBN_GETINFOTIP to display a ToolTip.

With the TBSTYLE_EX_MIXEDBUTTONS extended style, text that is set but not displayed on a button will automatically be used as the button's ToolTip text.

Your application only needs to handle TBN_GETINFOTIP if it needs more flexibility in specifying the ToolTip text.

Note :

    To set an extended style, send the toolbar control a TB_SETEXTENDEDSTYLE message. To determine what extended styles are currently set, send a TB_GETEXTENDEDSTYLE message.", 0]

[CCS_NOMOVEYhelp: "CCS_NOMOVEY:

Causes the control to resize and move itself horizontally, but not vertically, in response to a WM_SIZE message.

Header windows have this style by default. This style does not apply if your control has the CCS_NORESIZE style.", 0]

[CCS_NORESIZEhelp: "CCS_NORESIZE:
 
Prevents the control from using the default width and height when setting its initial size or a new size.

Instead, the control uses the width and height that is specified in the request for creation or sizing.", 0]

[CCS_NOPARENTALIGNhelp: "CCS_NOPARENTALIGN:
 
Prevents the control from automatically moving to the top or bottom of the parent window.

Instead, the control keeps its position within the parent window despite changes to the size of the parent.
If the application also uses the CCS_TOP or CCS_BOTTOM styles, it adjusts the height to the default, but does not change the position and width of the control.", 0]

[CCS_NODIVIDERhelp: "CCS_NODIVIDER:

Prevents a 2-pixel highlight from being drawn at the top of the control.", 0]

[CCS_VERThelp: "CCS_VERT:

Causes the control to display vertically.", 0]

[CCS_NOMOVEXhelp: "CCS_NOMOVEX:

Version 4.70. Causes the control to resize and move itself vertically, but not horizontally, in response to a WM_SIZE message.

If CCS_NORESIZE is used, this style does not apply.", 0]


[WS_CLIPSIBLINGShelp: "WS_CLIPSIBLINGS:

Clips child windows relative to each other; that is, when a particular child window receives a paint message, the WS_CLIPSIBLINGS style clips all other overlapped child windows out of the region of the child window to be updated.
(If WS_CLIPSIBLINGS is not given and child windows overlap, when you draw within the client area of a child window, it is possible to draw within the client area of a neighboring child window.) For use with the WS_CHILD style only.", 0]


[ToolBarStylesHelp:
TBSTYLE_BUTTONhelp  TBSTYLE_SEPhelp TBSTYLE_CHECKhelp   TBSTYLE_GROUPhelp
TBSTYLE_CHECKGROUPhelp  TBSTYLE_DROPDOWNhelp    TBSTYLE_AUTOSIZEhelp    TBSTYLE_NOPREFIXhelp
BTNS_SHOWTEXThelp   BTNS_WHOLEDROPDOWNhelp  TBSTYLE_TOOLTIPShelp    TBSTYLE_WRAPABLEhelp
TBSTYLE_ALTDRAGhelp TBSTYLE_FLAThelp    TBSTYLE_LISThelp    TBSTYLE_CUSTOMERASEhelp
TBSTYLE_REGISTERDROPhelp    TBSTYLE_TRANSPARENThelp TBSTYLE_EX_DRAWDDARROWShelp TBSTYLE_EX_MIXEDBUTTONShelp
TBSTYLE_EX_HIDECLIPPEDBUTTONShelp   TBSTYLE_EX_DOUBLEBUFFERhelp CCS_TOPhelp CCS_NOMOVEYhelp
CCS_BOTTOMhelp  CCS_NORESIZEhelp    CCS_NOPARENTALIGNhelp   CCS_ADJUSTABLEhelp
CCS_NODIVIDERhelp   CCS_VERThelp    CCS_LEFThelp    CCS_NOMOVEXhelp
CCS_RIGHThelp   WS_TABSTOPhelp  WS_GROUPhelp    WS_DISABLEDhelp
WS_CHILDhelp    WS_VISIBLEhelp  WS_BORDERhelp   WS_CLIPSIBLINGShelp]


; &CCS_VERT We don't need this because when it is set to Left or Right, it already uses the CCS_VERT
; &TBSTYLE_SEP The same as TBSTYLE_SEP
; &TBSTYLE_NOPREFIX iS THE SAME AS &CCS_ADJUSTABLE

____________________________________

; RichEdit20a control tables:



[RichEdit20aCheckingMask: 03 03 03 0   0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0]

[RichEdit20aTextTable: B$
 'ES_LEFT' 0         'ES_CENTER' 0        'ES_RIGHT' 0       'ES_MULTILINE' 0
 'ES_AUTOVSCROLL' 0  'ES_AUTOHSCROLL' 0   'ES_LOWERCASE' 0   'ES_UPPERCASE' 0
 'ES_PASSWORD' 0     'ES_OEMCONVERT' 0    'ES_NOHIDESEL' 0   'ES_READONLY' 0
 'ES_NUMBER' 0       'ES_WANTRETURN' 0    'WS_VSCROLL' 0     'WS_HSCROLL' 0
 'WS_TABSTOP' 0      'WS_BORDER' 0        'WS_VISIBLE' 0     'WS_CHILD' 0 0]

[RichEdit20aBitTable:
 &ES_LEFT             &ES_CENTER            &ES_RIGHT           &ES_MULTILINE
 &ES_AUTOVSCROLL      &ES_AUTOHSCROLL       &ES_LOWERCASE       &ES_UPPERCASE
 &ES_PASSWORD         &ES_OEMCONVERT        &ES_NOHIDESEL       &ES_READONLY
 &ES_NUMBER           &ES_WANTRETURN        &WS_VSCROLL         &WS_HSCROLL
 &WS_TABSTOP          &WS_BORDER            &WS_VISIBLE         &WS_CHILD]

[RichEdit20aExcludeBitTable:
 03  02   01   0
 0   0   08  010
 0   0   0   0
 0   0   0   0
 0   0   0   0]

[RichEdit20aMustHaveBitTable:
 0 0 0 0
 &WS_VSCROLL &WS_HSCROLL 0 0
 0 0 0 0
 0 0 0 0
 0 0 0 0]

[RichEdit20aStylesHelp: ES_LEFThelp ES_CENTERhelp ES_RIGHThelp ES_MULTILINEhelp
  ES_AUTOVSCROLLhelp
 ES_AUTOHSCROLLhelp ES_LOWERCASEhelp ES_UPPERCASEhelp ES_PASSWORDhelp
 ES_OEMCONVERThelp ES_NOHIDESELhelp ES_READONLYhelp ES_NUMBERhelp ES_WANTRETURNhelp
 WS_VSCROLLhelp WS_HSCROLLhelp WS_TABSTOPhelp WS_BORDERhelp  WS_VISIBLEhelp WS_CHILDhelp]

____________________________________

; SysHeader32 control tables:

; use this on: ShowControlStyles ,  ShowControlStyleControl , ControlClassByNames , SearchWhatControlClass , WriteStyle,
; ShowStyleInfo
  ;&WS_GROUP &WS_DISABLED
  ; &BS_RIGHTBUTTON &BS_TEXT

[SysHeader32CheckingMask:
    0 0 0 0
    0 0 0 0
    0 0 0
    0 0 0 0
    0 0 0 0]


[SysHeader32TextTable: B$
'HDS_BUTTONS' 0 'HDS_DRAGDROP' 0 'HDS_FILTERBAR' 0 'HDS_FLAT' 0
'HDS_FULLDRAG' 0 'HDS_HIDDEN' 0  'HDS_HORZ' 0 'HDS_HOTTRACK' 0
'HDS_CHECKBOXES' 0 'HDS_NOSIZING' 0     'HDS_OVERFLOW' 0
'WS_VSCROLL' 0 'WS_HSCROLL' 0 'WS_TABSTOP' 0 'WS_BORDER' 0
'WS_VISIBLE' 0 'WS_CHILD' 0 'WS_GROUP' 0 'WS_DISABLED' 0 0]

[SysHeader32BitTable:
 &HDS_BUTTONS       &HDS_DRAGDROP   &HDS_FILTERBAR  &HDS_FLAT
 &HDS_FULLDRAG      &HDS_HIDDEN     &HDS_HORZ       &HDS_HOTTRACK
 &HDS_CHECKBOXES    &HDS_NOSIZING   &HDS_OVERFLOW
 &WS_VSCROLL        &WS_HSCROLL     &WS_TABSTOP     &WS_BORDER
 &WS_VISIBLE        &WS_CHILD       &WS_GROUP       &WS_DISABLED]

[SysHeader32ExcludeBitTable:
    &HDS_CHECKBOXES 0 0 0
    0 0 0 0
    &HDS_BUTTONS 0 0
    0 0 0 0
    0 0 0 0]

[SysHeader32MustHaveBitTable:
    0 0 0 0
    0 0 0 0
    0 0 0
    0 0 0 0
    0 0 0 0]

[SysHeader32StylesHelp:
HDS_BUTTONShelp     HDS_DRAGDROPhelp    HDS_FILTERBARhelp   HDS_FLAThelp
HDS_FULLDRAGhelp    HDS_HIDDENhelp      HDS_HORZhelp        HDS_HOTTRACKhelp
HDS_CHECKBOXEShelp  HDS_NOSIZINGhelp    HDS_OVERFLOWhelp
WS_VSCROLLhelp      WS_HSCROLLhelp      WS_TABSTOPhelp      WS_BORDERhelp
WS_VISIBLEhelp      WS_CHILDhelp        WS_GROUPhelp        WS_DISABLEDhelp]


[HDS_BUTTONShelp: "HDS_BUTTONS:

Each item in the control looks and behaves like a push button.
This style is useful if an application carries out a task when the user clicks an item in the header control.
For example, an application could sort information in the columns differently depending on which item the user clicks.", 0]

[HDS_DRAGDROPhelp: "HDS_DRAGDROP:

Version 4.70. Allows drag-and-drop reordering of header items.", 0]

[HDS_FILTERBARhelp: "HDS_FILTERBAR:

Version 5.80. Include a filter bar as part of the standard header control.
This bar allows users to conveniently apply a filter to the display.
Calls to HDM_LAYOUT will yield a new size for the control and cause the list view to update.", 0]

[HDS_FLAThelp: "HDS_FLAT:

Version 6.0. Causes the header control to be drawn flat when Microsoft Windows XP is running in classic mode.

Note:  Comctl32.dll version 6 is not redistributable but it is included in Windows XP or later.
To use Comctl32.dll version 6, specify it in a manifest windows style", 0]

[HDS_FULLDRAGhelp: "HDS_FULLDRAG:

Version 4.70. Causes the header control to display column contents even while the user resizes a column.", 0]

[HDS_HIDDENhelp: "HDS_HIDDEN:

Indicates a header control that is intended to be hidden.
This style does not hide the control.
Instead, when you send the HDM_LAYOUT message to a header control with the HDS_HIDDEN style, the control returns zero in the cy member of the WINDOWPOS structure.
You would then hide the control by setting its height to zero.
This can be useful when you want to use the control as an information container instead of a visual control.", 0]

[HDS_HORZhelp: "HDS_HORZ:

Creates a header control with a horizontal orientation.", 0]

[HDS_HOTTRACKhelp: "HDS_HOTTRACK:

Version 4.70. Enables hot tracking.", 0]

[HDS_CHECKBOXEShelp: "HDS_CHECKBOXES:

(0x0400)", 0]

[HDS_NOSIZINGhelp: "HDS_NOSIZING:

(0x0800)", 0]

[HDS_OVERFLOWhelp: "HDS_OVERFLOW:

(0x1000)", 0]


____________________________________

; ReBarWindow32 controls

[ReBarWindow32CheckingMask:
 &CCS_BOTTOM  &CCS_VERT__&CCS_BOTTOM
 0  0
 0  0   0
 0  0   0   &CCS_VERT

 &CCS_RIGHT &CCS_LEFT

 0  0   0]


[ReBarWindow32TextTable: B$
 'CCS_TOP' 0  'CCS_BOTTOM' 0
 'RBS_AUTOSIZE' 0   'CCS_ADJUSTABLE' 0
 'RBS_BANDBORDERS' 0 'RBS_DBLCLKTOGGLE' 0    'RBS_FIXEDORDER' 0
 'RBS_REGISTERDROP' 0    'RBS_TOOLTIPS' 0        'RBS_VARHEIGHT' 0   'RBS_VERTICALGRIPPER' 0

 'CCS_LEFT' 0  'CCS_RIGHT' 0

 'WS_BORDER' 0  'WS_VISIBLE' 0  'WS_CHILD' 0 0]

[ReBarWindow32BitTable:
&CCS_TOP &CCS_BOTTOM
&RBS_AUTOSIZE   &CCS_ADJUSTABLE
&RBS_BANDBORDERS    &RBS_DBLCLKTOGGLE   &RBS_FIXEDORDER
&RBS_REGISTERDROP   &RBS_TOOLTIPS   &RBS_VARHEIGHT  &RBS_VERTICALGRIPPER

&CCS_LEFT &CCS_RIGHT

&WS_BORDER &WS_VISIBLE &WS_CHILD]


[ReBarWindow32ExcludeBitTable:
 &CCS_VERT__&CCS_BOTTOM  &CCS_VERT__&CCS_TOP
 0  0
 0  0   0
 0  0   0   0

 &CCS_RIGHT+&CCS_BOTTOM+&CCS_TOP  &CCS_LEFT+&CCS_TOP+&CCS_BOTTOM

 0  0   0]

[ReBarWindow32MustHaveBitTable:
 0  0
 0  0
 0  0   0
 0  0   0   0

 0  0

 0  0   0]

[ReBarWindow32StylesHelp:
CCS_TOPhelp CCS_BOTTOMhelp  RBS_AUTOSIZEhelp    CCS_ADJUSTABLEhelp
RBS_BANDBORDERShelp RBS_DBLCLKTOGGLEhelp        RBS_FIXEDORDERhelp  RBS_REGISTERDROPhelp
RBS_TOOLTIPShelp    RBS_VARHEIGHThelp   RBS_VERTICALGRIPPERhelp CCS_LEFThelp
CCS_RIGHThelp   WS_BORDERhelp   WS_VISIBLEhelp  WS_CHILDhelp]


[RBS_AUTOSIZEhelp: "RBS_AUTOSIZE:

Version 4.71. The rebar control will automatically change the layout of the bands when the size or position of the control changes. An RBN_AUTOSIZE notification will be sent when this occurs.", 0]

[RBS_BANDBORDERShelp: "RBS_BANDBORDERS:

Version 4.71. The rebar control displays narrow lines to separate adjacent bands.", 0]

[RBS_DBLCLKTOGGLEhelp: "RBS_DBLCLKTOGGLE:

Version 4.71. The rebar band will toggle its maximized or minimized state when the user double-clicks the band. Without this style, the maximized or minimized state is toggled when the user single-clicks on the band.", 0]

[RBS_FIXEDORDERhelp: "RBS_FIXEDORDER:

Version 4.70. The rebar control always displays bands in the same order. You can move bands to different rows, but the band order is static.", 0]

[RBS_REGISTERDROPhelp: "RBS_REGISTERDROP:

Version 4.71. The rebar control generates RBN_GETOBJECT notification messages when an object is dragged over a band in the control. To receive the RBN_GETOBJECT notifications, initialize OLE with a call to OleInitialize or CoInitialize.", 0]

[RBS_TOOLTIPShelp: "RBS_TOOLTIPS:

Version 4.71. Not yet supported.", 0]

[RBS_VARHEIGHThelp: "RBS_VARHEIGHT:

Version 4.71. The rebar control displays bands at the minimum required height, when possible. Without this style, the rebar control displays all bands at the same height, using the height of the tallest visible band to determine the height of other bands.", 0]

[RBS_VERTICALGRIPPERhelp: "RBS_VERTICALGRIPPER:

Version 4.71. The size grip will be displayed vertically instead of horizontally in a vertical rebar control. This style is ignored for rebar controls that do not have the CCS_VERT style.", 0]

____________________________________

; tooltips_class32
;note: A ToolTip control always has the WS_POPUP and WS_EX_TOOLWINDOW window styles,
; regardless of whether you specify them when creating the control.

[Tooltips_class32CheckingMask:
 0  0   0   0
 0  0   0
 0  0   0
 0  0]
 ;&WS_POPUP  &WS_EX_TOOLWINDOW]

[Tooltips_class32TextTable: B$
 'TTS_ALWAYSTIP', 0     'TTS_BALLOON', 0        'TTS_NOANIMATE', 0      'TTS_NOFADE', 0
 'TTS_NOPREFIX', 0      'TTS_USEVISUALSTYLE', 0     'TTS_CLOSE', 0
 'WS_BORDER', 0         '&WS_VISIBLE', 0     'WS_CHILD', 0
 'WS_POPUP', 0          'WS_EX_TOOLWINDOW', 0 0]



[Tooltips_class32BitTable:
 &TTS_ALWAYSTIP     &TTS_BALLOON    &TTS_NOANIMATE  &TTS_NOFADE
 &TTS_NOPREFIX      &TTS_USEVISUALSTYLE     &TTS_CLOSE
 &WS_BORDER         &WS_VISIBLE     &WS_CHILD
 &WS_POPUP          &WS_EX_TOOLWINDOW]


[Tooltips_class32ExcludeBitTable:
 0  0   0   0
 0  0   0
 0  0   0
 0  0]


[Tooltips_class32MustHaveBitTable:
 &WS_POPUP+&WS_EX_TOOLWINDOW  &WS_POPUP+&WS_EX_TOOLWINDOW   &WS_POPUP+&WS_EX_TOOLWINDOW   &WS_POPUP+&WS_EX_TOOLWINDOW
 &WS_POPUP+&WS_EX_TOOLWINDOW  &WS_POPUP+&WS_EX_TOOLWINDOW   &WS_POPUP+&WS_EX_TOOLWINDOW
 0  0   0
 0  0];&WS_POPUP  &WS_EX_TOOLWINDOW];  0]

[Tooltips_class32StylesHelp:
 TTS_ALWAYSTIPHelp  TTS_BALLOONHelp TTS_NOANIMATEHelp   TTS_NOFADEHelp
 TTS_NOPREFIXHelp    TTS_USEVISUALSTYLEHelp  TTS_CLOSEHelp
 WS_BORDERhelp      WS_VISIBLEhelp WS_CHILDhelp
 WS_POPUPhelp       WS_EX_TOOLWINDOWhelp]


[TTS_ALWAYSTIPHelp: "TTS_ALWAYSTIP:

Indicates that the ToolTip control appears when the cursor is on a tool, even if the ToolTip control's owner window is inactive.
Without this style, the ToolTip appears only when the tool's owner window is active.", 0]

[TTS_BALLOONHelp: "TTS_BALLOON:

Version 5.80. Indicates that the ToolTip control has the appearance of a cartoon 'balloon,' with rounded corners and a stem pointing to the item.", 0]

[TTS_NOANIMATEHelp: "TTS_NOANIMATE:

Version 5.80. Disables sliding ToolTip animation on Microsoft Windows 98 and Windows 2000 systems.
This style is ignored on earlier systems.", 0]

[TTS_NOFADEHelp: "TTS_NOFADE:

Version 5.80. Disables fading ToolTip animation on Windows 2000 systems.
This style is ignored on earlier Microsoft Windows NT systems, and on Windows 95 and Windows 98.", 0]

[TTS_NOPREFIXHelp: "TTS_NOPREFIX:

Prevents the system from stripping the ampersand character from a string.
Without this style, the system automatically strips ampersand characters.
This allows an application to use the same string as both a menu item and as text in a ToolTip control.", 0]

[TTS_USEVISUALSTYLEHelp: "TTS_USEVISUALSTYLE:

Uses themed hyperlinks. The theme will define the styles for any links in the tooltip.
This style always requires TTF_PARSELINKS to be set.", 0]

[TTS_CLOSEHelp: "TTS_CLOSE:

Displays a Close button on the tooltip.", 0]

[WS_EX_TOOLWINDOWhelp: "WS_EX_TOOLWINDOW:

Creates a tool window; that is, a window intended to be used as a floating toolbar.
A tool window has a title bar that is shorter than a normal title bar, and the window title is drawn using a smaller font.
A tool window does not appear in the taskbar or in the dialog box that appears when the user presses ALT+TAB.
If a tool window has a system menu, its icon is not displayed on the title bar.
However, you can display the system menu by typing ALT+SPACE.", 0]


____________________________________

; msctls_statusbar32

[msctls_statusbar32CheckingMask:
 0  0
 0  0   0
 0]

[msctls_statusbar32TextTable: B$
 'SBARS_SIZEGRIP', 0     'SBT_TOOLTIPS', 0
 'WS_BORDER', 0         '&WS_VISIBLE', 0     'WS_CHILD', 0
 'WS_POPUP', 0 0]



[msctls_statusbar32BitTable:
 &SBARS_SIZEGRIP    &SBT_TOOLTIPS
 &WS_BORDER         &WS_VISIBLE     &WS_CHILD
 &WS_POPUP]


[msctls_statusbar32ExcludeBitTable:
 0  0
 0  0   0
 0]


[msctls_statusbar32MustHaveBitTable:
 0  0
 0  0  0
 0]

[msctls_statusbar32StylesHelp:
 SBARS_SIZEGRIPHelp  SBT_TOOLTIPSHelp
 WS_BORDERhelp      WS_VISIBLEhelp WS_CHILDhelp
 WS_POPUPhelp]


[SBARS_SIZEGRIPHelp: "SBARS_SIZEGRIP:

The status bar control will include a sizing grip at the right end of the status bar.
A sizing grip is similar to a sizing border; it is a rectangular area that the user can click and drag to resize the parent window.", 0]

[SBT_TOOLTIPSHelp: "SBT_TOOLTIPS:

Version 4.71.Use this style to enable ToolTips.
This is exactly the same as SBARS_TOOLTIPS", 0]


____________________________________

; msctls_hotkey32

[msctls_hotkey32CheckingMask:
 0  0]

[msctls_hotkey32TextTable: B$
 'WS_BORDER', 0         'WS_TABSTOP', 0 0]


[msctls_hotkey32BitTable:
 &WS_BORDER         &WS_TABSTOP]


[msctls_hotkey32ExcludeBitTable:
 0  0]


[msctls_hotkey32MustHaveBitTable:
 0  0]

[msctls_hotkey32StylesHelp:
 WS_BORDERhelp      WS_TABSTOPhelp]


____________________________________

; ComboBoxEx32

[ComboBoxEx32CheckingMask:
 0
 0  0
 0  0
 0  ;0
 0  0   0]

[ComboBoxEx32TextTable: B$

'CBS_OWNERDRAWFIXED', 0

'CBES_EX_CASESENSITIVE', 0         'CBES_EX_NOEDITIMAGE', 0

'CBES_EX_NOEDITIMAGEINDENT', 0         'CBES_EX_NOSIZELIMIT', 0

'CBES_EX_PATHWORDBREAKPROC', 0         ;'CBES_EX_TEXTENDELLIPSIS', 0

 'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[ComboBoxEx32BitTable:
&CBS_OWNERDRAWFIXED
&CBES_EX_CASESENSITIVE  &CBES_EX_NOEDITIMAGE
&CBES_EX_NOEDITIMAGEINDENT  &CBES_EX_NOSIZELIMIT
&CBES_EX_PATHWORDBREAKPROC  ;  &CBES_EX_TEXTENDELLIPSIS (For Windows Vista. we need this equate later :) )
&WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[ComboBoxEx32ExcludeBitTable:
 0
 0  0
 0  0
 0  ;0
 0  0  0]


[ComboBoxEx32MustHaveBitTable:
 0
 0  0
 0  0
 0  ;0
 0  0  0]

[ComboBoxEx32StylesHelp:
 CBS_OWNERDRAWFIXEDhelp
 CBES_EX_CASESENSITIVEhelp      CBES_EX_NOEDITIMAGEhelp
 CBES_EX_NOEDITIMAGEINDENThelp   CBES_EX_NOSIZELIMIThelp
 CBES_EX_PATHWORDBREAKPROChelp ; CBES_EX_TEXTENDELLIPSIShelp
 WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]


[CBES_EX_CASESENSITIVEhelp: "CBES_EX_CASESENSITIVE:

BSTR searches in the list will be case sensitive.
This includes searches as a result of text being typed in the edit box and the CB_FINDSTRINGEXACT message.", 0]

[CBES_EX_NOEDITIMAGEhelp: "CBES_EX_NOEDITIMAGE:

The edit box and the dropdown list will not display item images.", 0]

[CBES_EX_NOEDITIMAGEINDENThelp: "CBES_EX_NOEDITIMAGEINDENT:

The edit box and the dropdown list will not display item images.", 0]

[CBES_EX_NOSIZELIMIThelp: "CBES_EX_NOSIZELIMIT:

Allows the ComboBoxEx control to be vertically sized smaller than its contained combo box control.
If the ComboBoxEx is sized smaller than the combo box, the combo box will be clipped.", 0]

[CBES_EX_PATHWORDBREAKPROChelp: "CBES_EX_PATHWORDBREAKPRO:

Microsoft Windows NT only.
The edit box will use the slash (/), backslash (\), and period (.) characters as word delimiters.
This makes keyboard shortcuts for word-by-word cursor movement () effective in path names and URLs.", 0]

[CBES_EX_TEXTENDELLIPSIShelp: "CBES_EX_TEXTENDELLIPSIS:

Windows Vista and later.
Causes items in the drop-down list and the edit box (when the edit box is read only) to be truncated with an ellipsis ('...') rather than just clipped by the edge of the control.
This is useful when the control needs to be set to a fixed width, yet the entries in the list may be long.", 0]

____________________________________

;SysAnimate32


[SysAnimate32CheckingMask:
 0  0  0  0
 0  0  0  0]

[SysAnimate32TextTable: B$
 'ACS_AUTOPLAY', 0   'ACS_CENTER', 0  'ACS_TIMER', 0  'ACS_TRANSPARENT', 0
 'WS_BORDER', 0      'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[SysAnimate32BitTable:
 &ACS_AUTOPLAY   &ACS_CENTER  &ACS_TIMER   &ACS_TRANSPARENT
 &WS_BORDER &WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[SysAnimate32ExcludeBitTable:
 0  0  0  0
 0  0  0  0]


[SysAnimate32MustHaveBitTable:
 0  0  0  0
 0  0  0  0]

[SysAnimate32StylesHelp:
 ACS_AUTOPLAYhelp   ACS_CENTERhelp  ACS_TIMERhelp   ACS_TRANSPARENThelp
 WS_BORDERhelp WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]


[ACS_AUTOPLAYhelp: "ACS_AUTOPLAY:

Starts playing the animation as soon as the AVI clip is opened.", 0]

[ACS_CENTERhelp: "ACS_CENTER:

Centers the animation in the animation control's window.", 0]

[ACS_TIMERhelp: "ACS_TIMER:

By default, the control creates a thread to play the AVI clip.
If you set this flag, the control plays the clip without creating a thread; internally the control uses a Win32 timer to synchronize playback.
Comctl32.dll version 6 and later: This style is not supported. By default, the control plays the AVI clip without creating a thread.
Note:  Comctl32.dll version 6 is not redistributable, but it is included in Microsoft Windows XP.
To use Comctl32.dll version 6, specify it in a manifest.", 0]

[ACS_TRANSPARENThelp: "ACS_TRANSPARENT:

Allows you to match an animation's background color to that of the underlying window, creating a 'transparent' background.
The parent of the animation control must not have the WS_CLIPCHILDREN style.
The control sends a WM_CTLCOLORSTATIC message to its parent.
Use SetBkColor to set the background color for the device context to an appropriate value.
The control interprets the upper-left pixel of the first frame as the animation's default background color.
It will remap all pixels with that color to the value you supplied in response to WM_CTLCOLORSTATIC.", 0]

____________________________________

; SysMonthCal32

[SysMonthCal32CheckingMask:
 0  0  0  0
 0  ;0  0  0
 0  0  0  0]

[SysMonthCal32TextTable: B$
 'MCS_DAYSTATE', 0   'MCS_MULTISELECT', 0  'MCS_WEEKNUMBERS', 0  'MCS_NOTODAYCIRCLE', 0
 'MCS_NOTODAY', 0   ; 'MCS_NOTRAILINGDATES', 0 'MCS_SHORTDAYSOFWEEK', 0     'MCS_NOSELCHANGEONNAV', 0
 'WS_BORDER', 0      'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[SysMonthCal32BitTable:
 &MCS_DAYSTATE   &MCS_MULTISELECT  &MCS_WEEKNUMBERS  &MCS_NOTODAYCIRCLE
 &MCS_NOTODAY   ; &MCS_NOTRAILINGDATES &MCS_SHORTDAYSOFWEEK     &MCS_NOSELCHANGEONNAV (these are Vista equates.. need to get them later)
 &WS_BORDER &WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[SysMonthCal32ExcludeBitTable:
 0  0  0  0
 0  ;0  0  0
 0  0  0  0]


[SysMonthCal32MustHaveBitTable:
 0  0  0  0
 0  ;0  0  0
 0  0  0  0]

[SysMonthCal32StylesHelp:
 MCS_DAYSTATEhelp   MCS_MULTISELECThelp  MCS_WEEKNUMBERShelp  MCS_NOTODAYCIRCLEhelp
 MCS_NOTODAYhelp    ;MCS_NOTRAILINGDATEShelp MCS_SHORTDAYSOFWEEKhelp     MCS_NOSELCHANGEONNAVhelp
 WS_BORDERhelp WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]


[MCS_DAYSTATEhelp: "MCS_DAYSTATE:

Version 4.70. The month calendar will send MCN_GETDAYSTATE notifications to request information about which days should be displayed in bold.", 0]

[MCS_MULTISELECThelp: "MCS_MULTISELECT:

Version 4.70. The month calendar will allow the user to select a range of dates within the control.
By default, the maximum range is one week. You can change the maximum range that can be selected by using the MCM_SETMAXSELCOUNT message.", 0]

[MCS_WEEKNUMBERShelp: "MCS_WEEKNUMBERS:

Version 4.70. The month calendar control will display week numbers (1-52) to the left of each row of days.
Week 1 is defined as the first week that contains at least four days.", 0]

[MCS_NOTODAYCIRCLEhelp: "MCS_NOTODAYCIRCLE:

Version 4.70. The month calendar control will not circle the 'today' date.", 0]

[MCS_NOTODAYhelp: "MCS_NOTODAY:

Version 4.70.The month calendar control will not display the 'today' date at the bottom of the control.", 0]

[MCS_NOTRAILINGDATEShelp: "MCS_NOTRAILINGDATES:

Microsoft Windows Vista. This flag disables displaying the dates from the previous/next month in the current calendar.", 0]

[MCS_SHORTDAYSOFWEEKhelp: "MCS_SHORTDAYSOFWEEK:

Microsoft Windows Vista. This flag uses the CAL_SSHORTESTDAYNAME* names to display for the day of the week column header.", 0]

[MCS_NOSELCHANGEONNAVhelp: "MCS_NOSELCHANGEONNAV:

Microsoft Windows Vista. This flag does not change the selection when the user navigates next or previous in the calendar.
This allows the user to select a range larger than what they can currently see.", 0]

____________________________________

; SysDateTimePick32

[SysDateTimePick32CheckingMask:
 &DTS_APPCANPARSE
 &DTS_LONGDATEFORMAT
 &DTS_RIGHTALIGN
 &DTS_SHOWNONE
 &DTS_SHORTDATEFORMAT
 &DTS_SHORTDATECENTURYFORMAT
 &DTS_TIMEFORMAT
 &DTS_UPDOWN

 0  0  0  0]

[SysDateTimePick32TextTable: B$
'DTS_APPCANPARSE', 0
'DTS_LONGDATEFORMAT', 0
'DTS_RIGHTALIGN', 0
'DTS_SHOWNONE', 0
'DTS_SHORTDATEFORMAT', 0
'DTS_SHORTDATECENTURYFORMAT', 0
'DTS_TIMEFORMAT', 0
'DTS_UPDOWN', 0
'WS_BORDER', 0      'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[SysDateTimePick32BitTable:
 &DTS_APPCANPARSE
 &DTS_LONGDATEFORMAT
 &DTS_RIGHTALIGN
 &DTS_SHOWNONE
 &DTS_SHORTDATEFORMAT
 &DTS_SHORTDATECENTURYFORMAT
 &DTS_TIMEFORMAT
 &DTS_UPDOWN
 &WS_BORDER &WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[SysDateTimePick32ExcludeBitTable:
 &DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_TIMEFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_UPDOWN
 &DTS_APPCANPARSE+&DTS_LONGDATEFORMAT+&DTS_RIGHTALIGN+&DTS_SHOWNONE+&DTS_SHORTDATEFORMAT+&DTS_SHORTDATECENTURYFORMAT+&DTS_TIMEFORMAT
 0  0  0  0]

[SysDateTimePick32MustHaveBitTable:
 0
 0
 0
 0
 0
 0
 0
 0
 0  0  0  0]

[SysDateTimePick32StylesHelp:
 DTS_APPCANPARSEhelp    DTS_LONGDATEFORMAThelp  DTS_RIGHTALIGNhelp  DTS_SHOWNONEhelp
 DTS_SHORTDATEFORMAThelp DTS_SHORTDATECENTURYFORMAThelp  DTS_TIMEFORMAThelp DTS_UPDOWNhelp
 WS_BORDERhelp WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]

[DTS_APPCANPARSEhelp: "DTS_APPCANPARSE:

Allows the owner to parse user input and take necessary action.
It enables users to edit within the client area of the control when they press the F2 key.
The control sends DTN_USERSTRING notification messages when users are finished.", 0]

[DTS_LONGDATEFORMAThelp: "DTS_LONGDATEFORMAT:

Displays the date in long format.
The default format string for this style is defined by LOCALE_SLONGDATEFORMAT, which produces output like 'Friday, April 19, 1996'.", 0]

[DTS_RIGHTALIGNhelp: "DTS_RIGHTALIGN:

The drop-down month calendar will be right-aligned with the control instead of left-aligned, which is the default.", 0]

[DTS_SHOWNONEhelp: "DTS_SHOWNONE:

It is possible to have no date currently selected in the control.
With this style, the control displays a check box that users can check once they have entered or selected a date.
Until this check box is checked, the application will not be able to retrieve the date from the control because, in essence, the control has no date.
This state can be set with the DTM_SETSYSTEMTIME message or queried with the DTM_GETSYSTEMTIME message.", 0]

[DTS_SHORTDATEFORMAThelp: "DTS_SHORTDATEFORMAT:

Displays the date in short format.
The default format string for this style is defined by LOCALE_SSHORTDATE, which produces output like '4/19/96'.", 0]

[DTS_SHORTDATECENTURYFORMAThelp: "DTS_SHORTDATECENTURYFORMAT:

Version 5.80. Similar to the DTS_SHORTDATEFORMAT style, except the year is a four-digit field.
The default format string for this style is based on LOCALE_SSHORTDATE. The output looks like: '4/19/1996'.", 0]

[DTS_TIMEFORMAThelp: "DTS_TIMEFORMAT:

Displays the time. The default format string for this style is defined by LOCALE_STIMEFORMAT, which produces output like '5:31:42 PM'.", 0]

[DTS_UPDOWNhelp: "DTS_UPDOWN:

Places an up-down control to the right of the DTP control to modify date-time values.
This style can be used in place of the drop-down month calendar, which is the default style.", 0]

____________________________________

; SysIPAddress32 . No ipaddress style exists.. we must set the minimum of WS_CHILD.

[SysIPAddress32CheckingMask:
 0
 0  0  0  0]

[SysIPAddress32TextTable: B$
 'WS_CHILD', 0
 'WS_BORDER', 0      'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[SysIPAddress32BitTable:
 &WS_CHILD
 &WS_BORDER &WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[SysIPAddress32ExcludeBitTable:
 0
 0  0  0  0]


[SysIPAddress32MustHaveBitTable:
 0
 0  0  0  0]

[SysIPAddress32StylesHelp:
 WS_CHILDhelp
 WS_BORDERhelp WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]

____________________________________

; SysPager

[SysPagerCheckingMask:
 0
 0
 0
 0
 0
 0  0  0  0]

[SysPagerTextTable: B$
 'PGS_AUTOSCROLL', 0
 'PGS_DRAGNDROP', 0
 'PGS_HORZ', 0
 'PGS_VERT', 0

 'WS_CHILD', 0
 'WS_BORDER', 0      'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[SysPagerBitTable:
 &PGS_AUTOSCROLL
 &PGS_DRAGNDROP
 &PGS_HORZ
 &PGS_VERT

 &WS_CHILD
 &WS_BORDER &WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[SysPagerExcludeBitTable:
 0
 0
 &PGS_VERT
 &PGS_HORZ
 0
 0  0  0  0]


[SysPagerMustHaveBitTable:
 0
 0
 0
 0
 0
 0  0  0  0]

[SysPagerStylesHelp:
 PGS_AUTOSCROLLhelp
 PGS_DRAGNDROPhelp
 PGS_HORZhelp
 PGS_VERThelp

 WS_CHILDhelp
 WS_BORDERhelp WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]

[PGS_AUTOSCROLLhelp: "PGS_AUTOSCROLL:

The pager control will scroll when the user hovers the mouse over one of the scroll buttons.", 0]

[PGS_DRAGNDROPhelp: "PGS_DRAGNDROP:

The contained window can be a drag-and-drop target.
The pager control will automatically scroll if an item is dragged from outside the pager over one of the scroll buttons.", 0]

[PGS_HORZhelp: "PGS_HORZ:

Creates a pager control that can be scrolled horizontally.
This style and the PGS_VERT style are mutually exclusive and cannot be combined.", 0]

[PGS_VERThelp: "PGS_VERT:

Creates a pager control that can be scrolled vertically.
This is the default direction if no direction style is specified.
This style and the PGS_HORZ style are mutually exclusive and cannot be combined.", 0]

____________________________________

; SysLink

; Guga note:
; missing the &ICC_LINK_CLASS ? . The dialog is not showing.
; InitDialogEdition:

[SysLinkCheckingMask:
 ;0  0  0
 0  0
 0  0  0  0]

[SysLinkTextTable: B$
; 'LIS_FOCUSED', 0  'LIS_ENABLED', 0   'LIS_VISITED', 0
 'WS_VISIBLE', 0    'WS_CHILD', 0
 'WS_BORDER', 0      'WS_TABSTOP', 0    'WS_VSCROLL' 0   'WS_HSCROLL' 0 0]


[SysLinkBitTable:
 ;&LIS_FOCUSED   &LIS_ENABLED   &LIS_VISITED
 &WS_VISIBLE    &WS_CHILD
 &WS_BORDER &WS_TABSTOP   &WS_VSCROLL   &WS_HSCROLL]

[SysLinkExcludeBitTable:
 ;0  0   0
 0  0
 0  0  0  0]


[SysLinkMustHaveBitTable:
 ;0  0   0
 0  0
 0  0  0  0]

[SysLinkStylesHelp:
 ;LIS_FOCUSEDhelp     LIS_ENABLEDhelp     LIS_VISITEDhelp
 WS_VISIBLEhelp WS_CHILDhelp
 WS_BORDERhelp WS_TABSTOPhelp     WS_VSCROLLhelp  WS_HSCROLLhelp]

[LIS_FOCUSEDhelp: "LIS_FOCUSED:

The link is surrounded by a dashed box. Pressing ENTER launches the link.", 0]

[LIS_ENABLEDhelp: "LIS_ENABLED:

 The link is displayed in blue or purple text, depending on LIS_VISITED. Clicking the link launches it.", 0]

[LIS_VISITEDhelp: "LIS_VISITED:
 
 The link is displayed in purple text. The user has already visited the URL represented by the link.", 0]
____________________________________

[QuestionMark: B$ '?' 0]
; General routine for showing the checkboxes under the main edition:

SSCkeckBoxes:
    mov eax 170, ebx 0

    .While B$esi > 0
        push eax, ebx, esi
            call 'User32.CreateWindowExA' 0, ButtonClassName, esi,
                           &WS_CHILD__&WS_VISIBLE__&BS_AUTOCHECKBOX,
                           16, eax, 140, 10, D$DialogEditorHandle, 0, D$hInstance, 0
            pop esi, ebx | push ebx, esi
            mov D$DialogControlsHandles+ebx eax
            call 'User32.SendMessageA' eax &WM_SETFONT D$MyFontHandle &TRUE
        pop esi, ebx, eax

        push eax, ebx, esi
          ; This is the little Style [?] Buttons:
            call 'User32.CreateWindowExA' 0, ButtonClassName, QuestionMark,
                           &WS_CHILD__&WS_VISIBLE,
                           2, eax, 12, 12, D$DialogEditorHandle, 0, D$hInstance, 0
            mov D$StyleHelpButtonsHandles+ebx eax
            call 'User32.SendMessageA' eax &WM_SETFONT D$MyFontHandle &TRUE
        pop edi
            mov al 0, ecx 200 | repne scasb | mov esi edi
        pop ebx, eax

        add eax 14 | add ebx 4
    .End_While
ret

____________________________________________________________________________________________

; Little Dialog for viewing the Styles Help:

[HelpDialog: D$ 090CC00C2 0   ;  090CC00C2 0 ; Style
 U$ 01 09 08 0DB 033        ; Dim 01 040 090
 0                          ;      no Menu
 0                          ; Class
 0                          ; Title
 08 'Helv' 0]               ; Font

[HelpDialogEdit: D$
 &WS_VISIBLE__&WS_CHILD__&ES_CENTER__&ES_MULTILINE__&ES_AUTOVSCROLL__&WS_VSCROLL__&WS_BORDER
 0

 U$ 0 0 0DB 033             ; Dim
 01                         ; ID
 0FFFF 081                  ; Class
 0                          ; TitleV.
 0]                         ; No creation data

____________________________________________________________________________________________

Proc HelpDialogProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If D@Message = &WM_INITDIALOG
      ; lParam >>> InitValue >>> set Text...
        call 'USER32.SetDlgItemTextA' D@Adressee, 1, D@lParam

    .Else_If D@Message = &WM_CTLCOLOREDIT
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        call 'USER32.SendMessageA' D@lParam &EM_SETSEL 0 0
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>>

    .Else_If D@Message = &WM_PARENTNOTIFY
        jmp L4>

    .Else_If D@Message = &WM_LBUTTONDOWN
        jmp L4>

    .Else_If D@Message = &WM_RBUTTONDOWN
        jmp L4>

    .Else_If D@Message = &WM_COMMAND
        mov eax D@wParam, ebx eax | shr eax 16 | and ebx 0FFFF

        If ebx = &IDCANCEL
            jmp L4>
        Else_If eax = &EN_CHANGE
            jmp L4>
        Else_If eax = &EN_UPDATE
L4:         call 'USER32.SetFocus' D$DialogListHandle
            jmp L5>
        Else_If eax = &EN_KILLFOCUS
L5:         call 'User32.EndDialog' D@Adressee 0
        End_If

    .Else
L8:     popad | mov eax &FALSE | jmp L9>
    .End_If

    popad | mov eax &TRUE

L9: EndP


[ShowSetOfCheckBoxes | mov esi #1 | call SSCkeckBoxes]

[CCFstring: B$
"

  Style settings depend on what Class you choose    
  (line 4 of each control).

  Set the Class first

  ", 0

    CCFtitle: "Arghhhhhh!!!!... " 0]


ControlClassFirst:
    call 'USER32.MessageBoxA' D$hwnd, CCFstring, CCFtitle, &MB_SYSTEMMODAL
ret

; from line pointed by user, searches what class and return in edi:ebx
; adress_of_table: index_of_string:

ShowControlStyleControl:
    push D$DialogListIndex
       add D$DialogListIndex Line_Class | call SearchWhatControlClass

       ..If edi = ControlClassByNumber
            If ebx = 0      | ShowSetOfCheckBoxes ButtonTextTable
            Else_If ebx = 1 | ShowSetOfCheckBoxes EditTextTable
            Else_If ebx = 2 | ShowSetOfCheckBoxes StaticTextTable
            Else_If ebx = 3 | ShowSetOfCheckBoxes ListTextTable
            Else_If ebx = 4 | ShowSetOfCheckBoxes ScrollTextTable
            Else_If ebx = 5 | ShowSetOfCheckBoxes ComboTextTable
            Else
                call ControlClassFirst
            End_If
       ..Else                                                   ; (edi = ControlClassByNames)
            .If ebx = 0      | ShowSetOfCheckBoxes UpDownTextTable
            .Else_If ebx = 1 | ShowSetOfCheckBoxes ProgressTextTable
            .Else_If ebx = 2 | ShowSetOfCheckBoxes TrackTextTable
            .Else_If ebx = 3 | ShowSetOfCheckBoxes TreeTextTable
            .Else_If ebx = 4 | ShowSetOfCheckBoxes TabTextTable
            .Else_If ebx = 5 | ShowSetOfCheckBoxes ListViewTextTable
            .Else_If ebx = 6 | ShowSetOfCheckBoxes ToolBarTextTable
            .Else_If ebx = 7 | ShowSetOfCheckBoxes RichEdit20aTextTable
            .Else_If ebx = 8 | ShowSetOfCheckBoxes SysHeader32TextTable
            .Else_If ebx = 9 | ShowSetOfCheckBoxes ReBarWindow32TextTable
            .Else_If ebx = 10 | ShowSetOfCheckBoxes Tooltips_class32TextTable
            .Else_If ebx = 11 | ShowSetOfCheckBoxes msctls_statusbar32TextTable
            .Else_If ebx = 12 | ShowSetOfCheckBoxes msctls_hotkey32TextTable
            .Else_If ebx = 13 | ShowSetOfCheckBoxes ComboBoxEx32TextTable
            .Else_If ebx = 14 | ShowSetOfCheckBoxes SysAnimate32TextTable
            .Else_If ebx = 15 | ShowSetOfCheckBoxes SysMonthCal32TextTable
            .Else_If ebx = 16 | ShowSetOfCheckBoxes SysDateTimePick32TextTable
            .Else_If ebx = 17 | ShowSetOfCheckBoxes SysIPAddress32TextTable
            .Else_If ebx = 18 | ShowSetOfCheckBoxes SysPagerTextTable
            .Else_If ebx = 19 | ShowSetOfCheckBoxes SysLinkTextTable
            .End_If
       ..End_If

    pop D$DialogListIndex
ret

[ID_Message: "==== Set the ID first ====





==== Mouse Edition ====


    Move: Left button.

    Resize: Right Button.", 0]

ShowIDcontrols:
    call SearchDialogLine | mov esi edi | call TranslateDialogHexa
    push ebx
        call 'USER32.CreateWindowExA' 0, EditClass, 0,
             &WS_CHILD+&WS_VISIBLE+&WS_BORDER+&ES_NUMBER+&ES_RIGHT+&ES_MULTILINE,
             80, 200, 45, 20, D$DialogEditorHandle, 0, D$hInstance, 0
        mov D$DialogControlsHandles eax
        call 'User32.GetDlgCtrlID' eax
    pop ebx

    call 'USER32.SetDlgItemInt' D$DialogEditorHandle, eax, ebx, &FALSE

    call 'USER32.CreateWindowExA' 0, EditClass, 0,
             &WS_CHILD+&WS_VISIBLE+&ES_MULTILINE+&ES_READONLY,
             4, 250, 140, 200, D$DialogEditorHandle, 0, D$hInstance, 0
    mov D$DialogControlsHandles+4 eax
    call 'USER32.SendMessageA' eax, &WM_SETTEXT, 0, ID_Message
    call 'USER32.SendMessageA' D$DialogControlsHandles+4, &WM_SETFONT, D$MyFontHandle, &TRUE

    call 'USER32.SetFocus' D$DialogControlsHandles
    call 'USER32.SendMessageA' D$DialogControlsHandles ,&EM_SETSEL, 0, 0-1
ret


[ControlClassByNumber: B$ 'Button' 0,  'Edit control' 0,  'Static Control' 0,  'ListBox' 0
                          'ScrollBar' 0,  'ComboBox' 0 0]
;[D_button 080  D_Edit 081  D_Static 082  D_ListBox 083  D_ScrollBar 084  D_ComboBox 085]

[ControlClassByNames: B$ 'msctls_updown32' 0,
                         'msctls_progress32' 0,
                         'msctls_trackbar32' 0,
                         'SysTreeView32' 0,
                         'SysTabControl32' 0,
                         'SysListView32' 0,
                         'ToolbarWindow32' 0,
                         'RichEdit20A' 0,
                         'SysHeader32' 0,
                         'ReBarWindow32' 0,
                         'tooltips_class32' 0,
                         'msctls_statusbar32' 0,
                         'msctls_hotkey32' 0,
                         'ComboBoxEx32' 0,
                         'SysAnimate32' 0,
                         'SysMonthCal32' 0,
                         'SysDateTimePick32' 0,
                         'SysIPAddress32' 0,
                         'SysPager' 0,
                         'SysLink' 0 0]
[ActualClassName: ? #10]

; From line pointed by user, searches what class and return in edi:ebx
; adress_of_table: index_of_string. ebx value is 0 to 5, edi either "ControlClassByNumber"
; or "ControlClassByNumber" (6 classes 'by number', 6 classes 'by name'):

SearchWhatControlClass:
    call SearchDialogLine
    ..If D$edi = 'FFFF'
        mov esi edi | add esi 5 | call TranslateDialogHexa  ; > ebx = 080 / 081 / ...
        sub ebx 080 | mov edi ControlClassByNumber          ; > ebx = 0 / 1 / 2...
    ..Else
        mov eax D$edi+8
        .If D$edi+8 = 'updo'         ; msctls_updown32
            mov ebx 0
        .Else_If D$edi+8 = 'prog'    ; msctls_progress32
            mov ebx 1
        .Else_If D$edi+8 = 'trac'    ; msctls_trackbar32
            mov ebx 2
        .Else_If D$edi+4 = 'Tree'    ; SysTreeView32
            mov ebx 3
        .Else_If D$edi+4 = 'List'    ; SysListView32
            mov ebx 5
        .Else_If D$edi+8 = 'ontr'    ; SysTabControl32
            mov ebx 4
        .Else_If D$edi+1 = 'Tool'    ; ToolbarWindow32
            mov ebx 6
        .Else_If D$edi+8 = 't20A'    ; RichEdit20a
            mov ebx 7
        .Else_If D$edi+1 = 'SysH'    ; SysHeader32
            mov ebx 8
        .Else_If D$edi+1 = 'ReBa'   ; ReBarWindow32
            mov ebx 9
        .Else_If D$edi+5 = 'tips'  ;tooltips_class32
            mov ebx 10
        .Else_If D$edi+8 = 'stat' ; msctls_statusbar32
            mov ebx 11
        .Else_If D$edi+8 = 'hotk'; msctls_hotkey32
            mov ebx 12
        .Else_If D$edi+1 = 'Comb' ;ComboBoxEx32
            mov ebx 13
        .Else_If D$edi+1 = 'SysA'; SysAnimate32
            mov ebx 14
        .Else_If D$edi+1 = 'SysM' ;SysMonthCal32
            mov ebx 15
        .Else_If D$edi+1 = 'SysD' ;SysDateTimePick32
            mov ebx 16
        .Else_If D$edi+1 = 'SysI' ;SysIPAddress32
            mov ebx 17
        .Else_If D$edi+1 = 'SysP'  ;SysPager
            mov ebx 18
        .Else_If D$edi+1 = 'SysL'  ; SysLink
            mov ebx 19
        .End_If

      mov edi ControlClassByNames
    ..End_If
ret


ShowClassControls:
    call 'User32.CreateWindowExA'  0, ComboClass, 0,
&WS_CHILD+&WS_VISIBLE+&WS_BORDER+&CBS_HASSTRINGS+&CBS_AUTOHSCROLL+&CBS_DROPDOWNLIST+&WS_VSCROLL+&ES_AUTOVSCROLL,
                                  2, 200, 145, 320, D$DialogEditorHandle, 0, D$hInstance, 0
    mov D$DialogControlsHandles eax

    ; Copy data font text (without quotes and comments) in TitleEditText:

    call SearchWhatControlClass

    mov al 0, ecx 0FFFF
    While ebx > 0                       ; setting edi > start of whatever class
        repne scasb | dec ebx           ; name, either by number or by name
    End_While
    mov esi edi, ecx 200 , al 0
    mov edi ActualClassName

L0: lodsb | stosb | cmp al 0 | ja L0<   ; copying actual name for next 'CB_SELECTSTRING'

    mov edi ControlClassByNumber        ; build list name from class by numbers:
    While B$edi > 0
        push edi
            call 'User32.SendMessageA' D$DialogControlsHandles &CB_ADDSTRING 0  edi
        pop edi
        mov al 0, ecx 200 | repne scasb
    End_While

    mov edi ControlClassByNames         ; build list name from class by names:
    While B$edi > 0
        push edi
            call 'User32.SendMessageA' D$DialogControlsHandles &CB_ADDSTRING 0  edi
        pop edi
        mov al 0, ecx 200 | repne scasb
    End_While

  ; setting actual choice in edit control of ComboBox:
    call 'User32.SendMessageA' D$DialogControlsHandles &CB_SELECTSTRING  0  ActualClassName
ret
____________________________________________________________________________________

; used by many 'user modifications' writting inside the edited template:

SearchDialogLine:
    mov ecx MaxTemplateText, edi D$NewDialogTemplateText, ebx 0, al 0

    While ebx < D$DialogListIndex
        repne scasb | inc ebx
    End_While
ret


NoDialogMenu:
    call 'USER32.MessageBoxA', 0, {'There is no Menu in this Dialog', 0},
                              {' Dialog Menu', 0}, &MB_SYSTEMMODAL
ret


; At same time: reset the table of controls handles and close these controls:

KillPreviousDialogControls:
    mov esi DialogControlsHandles
    While D$esi > 0
        lodsd | mov D$esi-4 0
        push esi
            call 'USER32.DestroyWindow' eax
        pop esi
    End_While

    mov esi StyleHelpButtonsHandles
    While D$esi > 0
        lodsd | mov D$esi-4 0
        push esi
            call 'USER32.DestroyWindow' eax
        pop esi
    End_While
ret

;;
;[NewMenuForDialog: "
;    Do you want to create a new menu?    
;     ", 0]

OldAddMenuToDialog:
    mov esi MenuList

   ..If D$esi = 0                               ; no Menu:
        call 'USER32.MessageBoxA'  0  NewMenuForDialog  argh,
                                &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO
        If eax = &IDYES
            call NewMenu
            mov eax D$MenuListPtr               ; either 0 or ID
        Else
            mov eax 0
        End_If
   ..Else                                       ; menu(s) exist in rsrc:
        call 'USER32.MessageBoxA'  0  NewMenuForDialog  argh,
                                &MB_SYSTEMMODAL+&MB_ICONQUESTION+&MB_YESNO
        .If eax = &IDYES
            call NewMenu | mov D$ActualMenutestID 0
            mov esi D$MenuListPtr
            If D$esi = 0
                mov eax 0
            Else
                mov eax D$MenuListPtr
            End_If
        .Else
            mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
            While B$UserTellWhatMenu = &FALSE   ; even if only 1 menu because we do
                call WhatMenu                   ; not set here the true user menu ID but
            End_While                           ; intead the 'D$ActualMenutestID'
            If D$MenuListPtr = 0
                mov eax 0
            Else
                mov eax D$MenuListPtr
            End_If
        .End_If
   ..End_If

   ; now, eax = Menu ID or 0.
   .If eax = 0
        mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
   .Else
        move D$DialogMenuTrueID D$eax
        If D$DialogMenuTrueID <> 0              ; happends if user abort menu edition
            add eax 4 | call 'User32.LoadMenuIndirectA' D$eax ; menu Id (from resources)
            mov D$ActualMenutestID eax
            call SearchDialogLine
            mov eax 'FFFF' | stosd | mov al ' ' | stosb
            mov ebx D$DialogMenuTrueID
            call TranslateDialogText4 | mov al ' ' | stosb | mov ax '; ' | stosw
            call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
        End_If
   .End_If
ret
;;

AddMenuToDialog:
    call 'USER32.DialogBoxParamA' D$hinstance 31555 &NULL NewOrExistingMenuProc &NULL

    .If B$NewOrExistingMenuChoice = 6           ; New.
        call NewMenu | mov D$ActualMenutestID 0
        mov esi D$MenuListPtr
        If D$esi = 0
            mov eax 0
        Else
            mov eax D$MenuListPtr
        End_If

    .Else_If B$NewOrExistingMenuChoice = 5       ; Existing.
        mov D$MenuListPtr MenuList,  B$UserTellWhatMenu &FALSE
        While B$UserTellWhatMenu = &FALSE   ; even if only 1 menu because we do
            call WhatMenu                   ; not set here the true user menu ID but
        End_While                           ; intead the 'D$ActualMenutestID'
        If D$MenuListPtr = 0
            mov eax 0
        Else
            mov eax D$MenuListPtr
        End_If

    .End_If

 ; now, eax = Menu ID or 0.
   .If eax = 0
        mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
   .Else
        move D$DialogMenuTrueID D$eax
        If D$DialogMenuTrueID <> 0              ; happends if user abort menu edition
            add eax 4 | call 'User32.LoadMenuIndirectA' D$eax ; menu Id (from resources)
            mov D$ActualMenutestID eax
            call SearchDialogLine
            mov eax 'FFFF' | stosd | mov al ' ' | stosb
            mov ebx D$DialogMenuTrueID
            call TranslateDialogText4 | mov al ' ' | stosb | mov ax '; ' | stosw
            call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
        End_If
   .End_If
ret


[NewOrExistingMenuChoice: ?]

Proc NewOrExistingMenuProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
      ; If no Menu >>> Disable the [Load Existing] Button:
        mov esi MenuList
        If D$esi = 0
            call 'User32.GetDlgItem' D@adressee, 5
            call 'User32.EnableWindow' eax &FALSE
        End_If

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        If D@wParam = &IDCANCEL
            mov B$NewOrExistingMenuChoice 0
            call 'User32.DestroyWindow' D@Adressee

        Else_If D@wParam = 5            ; Existing.
            mov B$NewOrExistingMenuChoice 5
            call 'User32.DestroyWindow' D@Adressee

        Else_If D@wParam = 6            ; New.
            mov B$NewOrExistingMenuChoice 6
            call 'User32.DestroyWindow' D@Adressee

        Else_If D@wParam = &IDHELP
            call Help, B_U_AsmName, DialogHelp, ContextHlpMessage

        End_If

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE
L9: EndP
____________________________________________________________________________________________



EditDialogMenu:
    push D$DialogMenuTrueID
        mov esi MenuList, eax D$DialogMenuTrueID
        While D$esi <> eax
            add esi 12
        End_While
        mov D$MenuListPtr esi | call ReEditExistingMenu
    pop eax

    If D$uMenu_ID <> eax
      move D$DialogMenuTrueID D$uMenu_ID
    End_If

    mov eax D$MenuListPtr | add eax 4          ; ptr to menu data
    call 'User32.LoadMenuIndirectA' D$eax | mov D$ActualMenutestID eax
    call SearchDialogLine
    mov eax 'FFFF' | stosd | mov al ' ' | stosb
    mov ebx D$DialogMenuTrueID
    call TranslateDialogText4 | mov al ' ' | stosb | mov ax '; ' | stosw
    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret

______________________________________________________________________________________
;;
 Setting of the wished controls under main list box for editing user dialog. (User
 have click on some item >>> which one >>> what controls to be set). As these controls
 may be many or few (or none), i choose a dynamic storage for the handles, whatever
 they are: One simgle table ("DialogControlsHandles") is used to store all the handles
 as they come, without any visible symbolic name for further identification. To know
 what user is doing, we use the index (selected item) of the main list. In some cases
 (Dialog Callback checking), we scan the number of handles written in the List.
;;

[DialogListIndex: ?           ; true 0 based line number in main list
 ControlIndex: ?]             ; 0 based index of line in one control

[NoDialogControl | mov D$DialogListIndex 0_FFFF_FFFF | mov D$ControlIndex 0_FFFF_FFFF]

[EditingMenuFromDialog: ?]

SetDialogTools:
    call KillPreviousDialogControls
    NoDialogControl
    call 'User32.SendMessageA' D$DialogListHandle, &LB_GETCURSEL, eax, 0
    mov D$DialogListIndex eax

    ..If eax < Line_empty
        mov D$ControlIndex eax
        .If eax = Line_Style
            call ShowDialogStyleControl | call ShowDialogStyles
        .Else_If eax = Line_Dim
            mov D$DimIsForDialogWindow &TRUE | call ShowDimControls
        .Else_If eax = Line_ID
            call SearchDialogLine
            If D$edi = 'FFFF'
                call EditDialogMenu
            Else
                call AddMenuToDialog
            End_If
        .Else_If eax = Line_Class
            call ShowTitleControl          ; reuse for dialog class
         ;  NoDialogControl  ; not yet
        .Else_If eax = Line_Class
            NoDialogControl  ; not yet
        .Else_If eax = Line_Title
            call ShowTitleControl
        .Else_If eax = Line_Font
            call ShowFontControls
        .End_If
    ..Else_If eax = Line_empty
        NoDialogControl    ; separator
    ..Else_If eax < D$LastDialogListItem
L1:     sub eax Line_empty+1 | cmp eax Line_empty+1 | jae L1<
        mov D$ControlIndex eax
        If eax = Line_Style
            call ShowControlStyleControl | call ShowControlStyles
        Else_If eax = Line_ExStyle
            NoDialogControl   ; not yet
        Else_If eax = Line_Dim
            mov D$DimIsForDialogWindow &FALSE | call ShowDimControls
        Else_If eax = Line_ID
            call ShowIDcontrols
        Else_If eax = Line_Class
            call ShowClassControls
        Else_If eax = Line_Title
            call ShowTitleControl
        Else_If eax = Line_Font
            NoDialogControl   ; not yet
        End_If
    ..End_If
ret

____________________________________________________________________________________
____________________________________________________________________________________

; Showing user modifications result:
____________________________________________________________________________________
____________________________________________________________________________________


[NextTemplateLine | xchg esi edi | mov al 0 | mov ecx 200 | repne scasb | xchg esi edi]


[ControlNumberPtr: 0]            ; user doesn't have to set this record (before Dims)

; Translate the text template (visible in Editor Edit Control) into a Binary Template
; run by RosAsm (without the menu and the class, if any). (see 'DialogMenuComment')
;
; 'FromBinToTextTemplate' doesn't do the reverse operation...

FromTextToBinTemplate:
    mov esi D$NewDialogTemplateText, edi D$EditedDialogBoxData

    add esi 3 | call TranslateDialogHexa | mov eax ebx | stosd  ; Style
    NextTemplateLine
    add esi 3 | call TranslateDialogHexa | mov eax ebx | stosd  ; extended style
    NextTemplateLine | add esi 3
    mov D$ControlNumberPtr edi                                  ; used down there to
    call TranslateDialogHexa | mov eax 0 | stosw                ; set dummy control number
    call TranslateDialogHexa | mov eax ebx | stosw              ; X pos
    call TranslateDialogHexa | mov eax ebx | stosw              ; Y pos
    call TranslateDialogHexa | mov eax ebx | stosw              ; width
    call TranslateDialogHexa | mov eax ebx | stosw              ; hight
    NextTemplateLine | call TranslateDialogHexa
  ; Why the commented out lines? >>> DialogMenuComment
  ;  If ebx = 0
      mov ax 0 | stosw                                          ; no menu
  ;  Else
  ;    mov eax ebx | stosw                                      ; 0FFFF in ebx > menu
  ;    call TranslateDialogHexa
  ;    mov eax ebx | stosw                                      ; menu ID
  ;  End_If
    NextTemplateLine | inc esi
    mov eax 0 | stosw                                           ; no class in edition
  ;  mov al 0FF
    push edi
        mov edi ClassRecord
L0:     cmp B$esi '"' | je L1>
        cmp B$esi "'" | je L1>
            lodsb | stosb                                       ; preserve Class for savings
        jmp L0<
L1:     mov eax 0 | stosd                                       ; end mark at our string
    pop edi
    NextTemplateLine | inc esi | mov eax 0
L0: cmp B$esi '"' | je L1>
    cmp B$esi "'" | je L1>
        lodsb | stosw                                            ; title
    jmp L0<
L1: mov ax 0 | stosw |  NextTemplateLine
    call TranslateDialogHexa | mov eax ebx | stosw               ; font size
    inc esi | mov ax 0
L0: cmp B$esi '"' | je L1>
    cmp B$esi "'" | je L1>
        lodsb | stosw                                            ; font name
    jmp L0<
L1: mov ax 0 | stosw
    Test edi 00_11 | jz L1>                                      ; Dword aligned?
        stosw                                                    ; no > align!
L1: NextTemplateLine | NextTemplateLine                          ; + 1 blank line in text

    .While B$esi <> 255                                          ; Controls data:
        mov ebx D$ControlNumberPtr | inc W$ebx
        add esi 3
        call TranslateDialogHexa | mov eax ebx | stosd           ; style
        NextTemplateLine | add esi 3
        call TranslateDialogHexa | mov eax ebx | stosd           ; extended style
        NextTemplateLine | add esi 3

        call TranslateDialogHexa | mov eax ebx | stosw           ; X pos
        call TranslateDialogHexa | mov eax ebx | stosw           ; Y pos
        call TranslateDialogHexa | mov eax ebx | stosw           ; width
        call TranslateDialogHexa | mov eax ebx | stosw           ; hight
        NextTemplateLine
        call TranslateDialogHexa | mov eax ebx | stosw           ; ID
        NextTemplateLine
        If D$esi = 'FFFF'                                        ; class by number:
            mov ax 0FFFF | stosw | add  esi 5
            call TranslateDialogHexa | mov eax ebx | stosw       ; class
        Else
            inc esi | mov eax 0
L0:         cmp B$esi '"' | je L1>
            cmp B$esi "'" | je L1>
            lodsb | stosw
            jmp L0<
L1:         mov ax 0 | stosw
        End_If
        NextTemplateLine | inc esi | mov ax 0
L0:     cmp B$esi '"' | je L1>
        cmp B$esi "'" | je L1>
            lodsb | stosw                                       ; title
        jmp L0<
L1:     mov ax 0 | stosw                                        ; end mark
        NextTemplateLine | stosw                                ; no creation dat
        Test edi 00_11 | jz L1>                                 ; Dword aligned?
            stosw                                               ; no > align!
L1:     NextTemplateLine  | NextTemplateLine                    ; + 1 blank line in text

    .End_While
ret
______________________________________________________________________________________

; User have click some 'Style' CheckBox. Called by 'EditDialogBoxProc' with
;  buttons handles in ecx, tables index in ebx

[ClickedCheckBoxHandle: ?  FlagCheck: ?]

WriteStyle:
    mov eax D$DialogListIndex, D$ClickedCheckBoxHandle ecx

    ...If eax = D$ControlIndex              ; >>> this is for Dialog Style:
        move D$CheckMust D$DialogMustHaveBitTable+ebx
        move D$CheckExclude D$DialogExcludeBitTable+ebx
        move D$CheckingMask DialogCheckingMask
        move D$CheckBit D$DialogBitTable+ebx
        mov esi DialogBitTable

    ...Else                                  ; >>> this is for Control Style:
        push ebx                                ; table indice (0 / 4 / 8 / 12...)
            add D$DialogListIndex Line_Class
            call SearchWhatControlClass             ; ebx = indice of class (0 / 1 / 2 / 3...)
            sub D$DialogListIndex Line_Class
        pop eax                                       ; table indice in eax
        ..If edi = ControlClassByNumber               ; >>> class by Number
            .If ebx = 0
                move D$CheckMust D$ButtonMustHaveBitTable+eax      ; Button
                move D$CheckExclude D$ButtonExcludeBitTable+eax
                move D$CheckingMask ButtonCheckingMask
                move D$CheckBit D$ButtonBitTable+eax
                mov esi ButtonBitTable
            .Else_If ebx = 1
                move D$CheckMust D$EditMustHaveBitTable+eax        ; Edit control
                move D$CheckExclude D$EditExcludeBitTable+eax
                move D$CheckingMask EditCheckingMask
                move D$CheckBit D$EditBitTable+eax
                mov esi EditBitTable
            .Else_If ebx = 2
                move D$CheckMust D$StaticMustHaveBitTable+eax      ; Static Control
                move D$CheckExclude D$StaticExcludeBitTable+eax
                move D$CheckingMask StaticCheckingMask
                move D$CheckBit D$StaticBitTable+eax
                mov esi StaticBitTable
            .Else_If ebx = 3
                move D$CheckMust D$ListMustHaveBitTable+eax        ; ListBox
                move D$CheckExclude D$ListExcludeBitTable+eax
                move D$CheckingMask ListCheckingMask
                move D$CheckBit D$ListBitTable+eax
                mov esi ListBitTable
            .Else_If ebx = 4
                move D$CheckMust D$ScrollMustHaveBitTable+eax      ; ScrollBar
                move D$CheckExclude D$ScrollExcludeBitTable+eax
                move D$CheckingMask ScrollCheckingMask
                move D$CheckBit D$ScrollBitTable+eax
                mov esi ScrollBitTable
            .Else_If ebx = 5
                move D$CheckMust D$ComboMustHaveBitTable+eax       ; ComboBox
                move D$CheckExclude D$ComboExcludeBitTable+eax
                move D$CheckingMask ComboCheckingMask
                move D$CheckBit D$ComboBitTable+eax
                mov esi ComboBitTable
            .End_If
        ..Else                                         ; >>> class by Name
            .If ebx = 0
                move D$CheckMust D$UpDownMustHaveBitTable+eax      ; msctls_updown32
                move D$CheckExclude D$UpDownExcludeBitTable+eax
                move D$CheckingMask UpDownCheckingMask
                move D$CheckBit D$UpDownBitTable+eax
                mov esi UpDownBitTable
            .Else_If ebx = 1
                ret                                    ; msctls_progress32 (no controls)
            .Else_If ebx = 2
                move D$CheckMust D$TrackMustHaveBitTable+eax       ; msctls_trackbar32
                move D$CheckExclude D$TrackExcludeBitTable+eax
                move D$CheckingMask TrackCheckingMask
                move D$CheckBit D$TrackBitTable+eax
                mov esi TrackBitTable
            .Else_If ebx = 3
                move D$CheckMust D$TreeMustHaveBitTable+eax        ; SysTreeView32
                move D$CheckExclude D$TreeExcludeBitTable+eax
                move D$CheckingMask TreeCheckingMask
                move D$CheckBit D$TreeBitTable+eax
                mov esi TreeBitTable
            .Else_If ebx = 4
                move D$CheckMust D$TabMustHaveBitTable+eax         ; SysTabControl32
                move D$CheckExclude D$TabExcludeBitTable+eax
                move D$CheckingMask TabCheckingMask
                move D$CheckBit D$TabBitTable+eax
                mov esi TabBitTable
            .Else_If ebx = 5
                move D$CheckMust D$ListViewMustHaveBitTable+eax    ; SysListView32
                move D$CheckExclude D$ListViewExcludeBitTable+eax
                move D$CheckingMask ListViewCheckingMask
                move D$CheckBit D$ListViewBitTable+eax
                mov esi ListViewBitTable
            .Else_If ebx = 6
                move D$CheckMust D$ToolBarMustHaveBitTable+eax    ; ToolbarWindow32
                move D$CheckExclude D$ToolBarExcludeBitTable+eax
                move D$CheckingMask ToolBarCheckingMask
                move D$CheckBit D$ToolBarBitTable+eax
                mov esi ToolBarBitTable

            .Else_If ebx = 7
                move D$CheckMust D$RichEdit20aMustHaveBitTable+eax    ; RichEdit20a
                move D$CheckExclude D$RichEdit20aExcludeBitTable+eax
                move D$CheckingMask RichEdit20aCheckingMask
                move D$CheckBit D$RichEdit20aBitTable+eax
                mov esi RichEdit20aBitTable

            .Else_If ebx = 8
                move D$CheckMust D$SysHeader32MustHaveBitTable+eax    ; SysHeader32
                move D$CheckExclude D$SysHeader32ExcludeBitTable+eax
                move D$CheckingMask SysHeader32CheckingMask
                move D$CheckBit D$SysHeader32BitTable+eax
                mov esi SysHeader32BitTable

            .Else_If ebx = 9
                move D$CheckMust D$ReBarWindow32MustHaveBitTable+eax    ; ReBarWindow32
                move D$CheckExclude D$ReBarWindow32ExcludeBitTable+eax
                move D$CheckingMask ReBarWindow32CheckingMask
                move D$CheckBit D$ReBarWindow32BitTable+eax
                mov esi ReBarWindow32BitTable

            .Else_If ebx = 10
                move D$CheckMust D$tooltips_class32MustHaveBitTable+eax    ; tooltips_class32
                move D$CheckExclude D$tooltips_class32ExcludeBitTable+eax
                move D$CheckingMask tooltips_class32CheckingMask
                move D$CheckBit D$tooltips_class32BitTable+eax
                mov esi tooltips_class32BitTable

            .Else_If ebx = 11
                move D$CheckMust D$msctls_statusbar32MustHaveBitTable+eax    ; msctls_statusbar32
                move D$CheckExclude D$msctls_statusbar32ExcludeBitTable+eax
                move D$CheckingMask msctls_statusbar32CheckingMask
                move D$CheckBit D$msctls_statusbar32BitTable+eax
                mov esi msctls_statusbar32BitTable

            .Else_If ebx = 12
                move D$CheckMust D$msctls_hotkey32MustHaveBitTable+eax    ; msctls_hotkey32
                move D$CheckExclude D$msctls_hotkey32ExcludeBitTable+eax
                move D$CheckingMask msctls_hotkey32CheckingMask
                move D$CheckBit D$msctls_hotkey32BitTable+eax
                mov esi msctls_hotkey32BitTable

            .Else_If ebx = 13
                move D$CheckMust D$ComboBoxEx32MustHaveBitTable+eax    ; ComboBoxEx32
                move D$CheckExclude D$ComboBoxEx32ExcludeBitTable+eax
                move D$CheckingMask ComboBoxEx32CheckingMask
                move D$CheckBit D$ComboBoxEx32BitTable+eax
                mov esi ComboBoxEx32BitTable

            .Else_If ebx = 14
                move D$CheckMust D$SysAnimate32MustHaveBitTable+eax    ; SysAnimate32
                move D$CheckExclude D$SysAnimate32ExcludeBitTable+eax
                move D$CheckingMask SysAnimate32CheckingMask
                move D$CheckBit D$SysAnimate32BitTable+eax
                mov esi SysAnimate32BitTable

            .Else_If ebx = 15
                move D$CheckMust D$SysMonthCal32MustHaveBitTable+eax    ; SysMonthCal32
                move D$CheckExclude D$SysMonthCal32ExcludeBitTable+eax
                move D$CheckingMask SysMonthCal32CheckingMask
                move D$CheckBit D$SysMonthCal32BitTable+eax
                mov esi SysMonthCal32BitTable

            .Else_If ebx = 16
                move D$CheckMust D$SysDateTimePick32MustHaveBitTable+eax    ; SysDateTimePick32
                move D$CheckExclude D$SysDateTimePick32ExcludeBitTable+eax
                move D$CheckingMask SysDateTimePick32CheckingMask
                move D$CheckBit D$SysDateTimePick32BitTable+eax
                mov esi SysDateTimePick32BitTable

            .Else_If ebx = 17
                move D$CheckMust D$SysIPAddress32MustHaveBitTable+eax    ; SysIPAddress32
                move D$CheckExclude D$SysIPAddress32ExcludeBitTable+eax
                move D$CheckingMask SysIPAddress32CheckingMask
                move D$CheckBit D$SysIPAddress32BitTable+eax
                mov esi SysIPAddress32BitTable

            .Else_If ebx = 18
                move D$CheckMust D$SysPagerMustHaveBitTable+eax    ; SysPager
                move D$CheckExclude D$SysPagerExcludeBitTable+eax
                move D$CheckingMask SysPagerCheckingMask
                move D$CheckBit D$SysPagerBitTable+eax
                mov esi SysPagerBitTable

            .Else_If ebx = 19
                move D$CheckMust D$SysLinkMustHaveBitTable+eax    ; SysLink
                move D$CheckExclude D$SysLinkExcludeBitTable+eax
                move D$CheckingMask SysLinkCheckingMask
                move D$CheckBit D$SysLinkBitTable+eax
                mov esi SysLinkBitTable
            .End_If
        ..End_If

    ...End_If

    push esi
        call SearchDialogLine | add edi 3           ; edi > "NewDialogTemplateText": Style
        mov esi edi | call TranslateDialogHexa      ; Style value in ebx
    pop esi
;;
 Now: D$CheckBit = bit value of the clicked checkbox
      ebx            = previous value for style
      D$CheckExclude = Excluded Bits
      D$CheckMust    = Must Have Bits
      esi            = ptr to Bit table
      edi            = "NewDialogTemplateText" concerned line +3 ("D$ "xxxxxxxx)

 We take care of "required" / "exclude" bits only if we are setting the bits:
;;
    push esi, edi, ebx
        call 'User32.SendMessageA' D$ClickedCheckBoxHandle &BM_GETCHECK 0 0
        mov D$FlagCheck eax
    pop ebx, edi, esi


    On B$FlagCheck = &FALSE, jmp L1>                    ; Only if user is "checking"
        or ebx D$CheckExclude | xor ebx D$CheckExclude  ; excluded bit(s)
        or ebx D$CheckMust                              ; required bit(s)
L1: xor ebx D$CheckBit                                  ; new set bit

; or ebx &WS_THICKFRAME  (for my tests)

    mov D$CheckActual ebx
    pushad
        call TranslateDialogText8  ; write ebx (text) at "NewDialogTemplateText": Style
    popad

    call CheckControlStyles

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret

____________________________________________________________________________________________

; esi just found a valid [?] Help Butoon Handle:

ShowStyleInfo:
    .If D$CheckingMask = DialogCheckingMask
        mov eax DialogStylesHelp
    .Else_If D$CheckingMask = ButtonCheckingMask
        mov eax ButtonStylesHelp
    .Else_If D$CheckingMask = EditCheckingMask
        mov eax EditStylesHelp
    .Else_If D$CheckingMask = StaticCheckingMask
        mov eax StaticStylesHelp
    .Else_If D$CheckingMask = ListCheckingMask
        mov eax ListBoxStyles
    .Else_If D$CheckingMask = ScrollCheckingMask
        mov eax ScrollStylesHelp
    .Else_If D$CheckingMask = ComboCheckingMask
        mov eax ComboStylesHelp
    .Else_If D$CheckingMask = UpDownCheckingMask
        mov eax UpDownStylesHelp
    .Else_If D$CheckingMask = TrackCheckingMask
        mov eax TrackStylesHelp
    .Else_If D$CheckingMask = TreeCheckingMask
        mov eax TreeViewStylesHelp
    .Else_If D$CheckingMask = TabCheckingMask
        mov eax TabStylesHelp
    .Else_If D$CheckingMask = ListViewCheckingMask
        mov eax ListViewStylesHelp
    .Else_If D$CheckingMask = ToolBarCheckingMask
        mov eax ToolBarStylesHelp
    .Else_If D$CheckingMask = RichEdit20aCheckingMask
        mov eax RichEdit20aStylesHelp
    .Else_If D$CheckingMask = SysHeader32CheckingMask
        mov eax SysHeader32StylesHelp
    .Else_If D$CheckingMask = ReBarWindow32CheckingMask
        mov eax ReBarWindow32StylesHelp
    .Else_If D$CheckingMask = tooltips_class32CheckingMask
        mov eax tooltips_class32StylesHelp
    .Else_If D$CheckingMask = msctls_statusbar32CheckingMask
        mov eax msctls_statusbar32StylesHelp
    .Else_If D$CheckingMask = msctls_hotkey32CheckingMask
        mov eax msctls_hotkey32StylesHelp
    .Else_If D$CheckingMask = ComboBoxEx32CheckingMask
        mov eax ComboBoxEx32StylesHelp
    .Else_If D$CheckingMask = SysAnimate32CheckingMask
        mov eax SysAnimate32StylesHelp
    .Else_If D$CheckingMask = SysMonthCal32CheckingMask
        mov eax SysMonthCal32StylesHelp
    .Else_If D$CheckingMask = SysDateTimePick32CheckingMask
        mov eax SysDateTimePick32StylesHelp
    .Else_If D$CheckingMask = SysIPAddress32CheckingMask
        mov eax SysIPAddress32StylesHelp
    .Else_If D$CheckingMask = SysPagerCheckingMask
        mov eax SysPagerStylesHelp
    .Else_If D$CheckingMask = SysLinkCheckingMask
        mov eax SysLinkStylesHelp
    .End_If

    sub esi 4 | sub esi StyleHelpButtonsHandles | add esi eax

    call 'USER32.CreateDialogIndirectParamA' D$hinstance, HelpDialog, ecx,
                                             HelpDialogProc, D$esi

ret
____________________________________________________________________________________

; Called by next routine only (to suppress the class by number before wirting a new
; class by name. edi, ecx set by caller:

StripFFFF0080:
    mov B$edi '"' | inc edi
    push edi
        add edi 8                                   ; edi after 'FFFF 0080'
        push edi
            mov al 255 | repne scasb | mov ecx edi  ; edi > End of template
        pop esi
        sub ecx esi                                 ; how many char to move backward
    pop edi                                         ; esi > first char
    rep movsb
ret


[DefaultNumberClass: 'xxxx', 0]

WriteClass: ;need LINES_!!
    call 'User32.SendMessageA' D$DialogControlsHandles, &CB_GETCURSEL, 0, 0 | mov ebx eax

    mov edx D$DialogListIndex, edi D$NewDialogTemplateText, al 0, ecx MaxTemplateText
    sub edx Line_Class

L0: repne scasb | dec edx | jnz L0<                          ; to point to upper Style
    add edi 4 | mov B$edi-1 '5', al '0', ecx 7 | rep stosb   ; reset Style record to default
    mov edx Line_Class, al 0, ecx 0FFFFFFFF
L0: repne scasb | dec edx | jnz L0<                          ; point to Class record

    ..If D$edi = 'FFFF'
        .If ebx < 6
            add edi 8 | mov B$edi '0' | add B$edi bl         ; new 'FFFF 008x' value
        .Else
          ; strip "text", make 9 chars room, copy 'FFFF 008x' value:
            push edi
                push ebx
                    call StripFFFF0080
                pop ebx
                sub ebx 6
                mov edi ControlClassByNames, ecx 0FFFF, al 0
                While ebx > 0
                    repne scasb | dec ebx
                End_While                                ; edi > choosen Class by name text
            pop esi
            inc esi                     ; "StripFFFF0080" have written first quote
            call ResetTemplateClass
        .End_If
    ..Else                              ; edi point to "msctls_...."
      ; Strip "text":
        push ebx, edi
            inc edi | call StripTemplateText
        pop edi, ebx
        .If ebx < 6
          ; make 9 chars room (including <" 0>, copy 'FFFF 008x' value):
            push ebx
                mov esi edi, edi DefaultNumberClass | call ResetTemplateClass
                sub edi 7 | mov D$edi 'FFFF', D$edi+4 ' 008'
            pop ebx
            add bl '0' | mov B$edi+8 bl, B$edi+9 ' '
        .Else
            push edi
                sub ebx 6
              ; make whished lenght room, copy new Class text
                mov edi ControlClassByNames, ecx 0FFFF, al 0
                While ebx > 0
                    repne scasb | dec ebx
                End_While
            pop esi
            inc esi
            call ResetTemplateClass
        .End_If
    ..End_If

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret
_________________________________________________________________________________

WriteFontSize:
    mov edi D$NewDialogTemplateText al 0, ecx MaxTemplateText
    repne scasb | repne scasb | repne scasb | repne scasb | repne scasb
    push edi
        call 'User32.SendMessageA' D$DialogControlsHandles+4  &CB_GETCURSEL  0  0
    pop edi
    push edi
        call 'User32.SendMessageA' D$DialogControlsHandles+4 &CB_GETLBTEXT eax edi
    pop edi
    mov B$edi+2 ' '
    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret
_____________________________________________________________________________________

; 2 routines to replace some text inside template:

StripTemplateText:
    push edi
        mov al '"', ecx MaxTemplateText | repne scasb | dec edi ; edi > lasting '"'
        push edi
            mov al 255 | repne scasb                            ; edi > End of template
            mov ecx edi
        pop esi
        sub ecx esi                                    ; how many char to move backward
    pop edi                                                     ; esi > first char
    rep movsb
ret


ResetTemplateText:
    push edi
        push esi                                                ; > where to write
            mov al 0, ecx 120, ebx 120 | repne scasb
            sub ecx 120 | neg ecx | mov ebx ecx                 ; new string lenght
            mov edi D$NewDialogTemplateText, al 255, ecx MaxTemplateText
            repne scasb | mov esi edi | add edi ebx             ; edi, esi ready for room
        pop eax
        push eax
            mov ecx esi | sub ecx eax | dec edi
            std
                rep movsb                   ; make empty room
            cld
        pop edi
    pop esi                                 ; adress of new string

L0: lodsb | cmp al 0 | je L9>
    stosb | jmp L0<                         ; write new string

L9: mov al '"' | stosb
ret
______________________________________________________________________________________

ResetTemplateClass:
    push edi
        push esi                                                   ; > where to write
            mov al 0, ecx 120, ebx 120 | repne scasb
            sub ecx 120 | neg ecx | mov ebx ecx                    ; new string lenght
            add ebx 2                                              ; for ' 0'
            mov edi D$NewDialogTemplateText, al 255, ecx MaxTemplateText
            repne scasb | mov esi edi | add edi ebx                ; edi, esi ready for room
        pop eax
        push eax
            mov ecx esi | sub ecx eax | dec edi
            std
                rep movsb                ; make empty room
            cld
        pop edi
    pop esi                              ; adress of new string set in edi by caller.

L0: lodsb | cmp al 0 | je L9>
    stosb | jmp L0<                      ; write new string

L9: mov al '"' | stosb
    mov ax ' 0' | stosw
ret

__________________________________________________________________________________

[LBbuffer: ? #30] ;[LBbufferLen 120]

ClearLBbuffer:
    mov eax 0, ecx 30, edi LBbuffer | rep stosd
ret


WriteFontType:
    call ClearLBbuffer
    mov edi D$NewDialogTemplateText al 0, ecx MaxTemplateText
    repne scasb | repne scasb | repne scasb | repne scasb | repne scasb
    add edi 4
    push edi
        call StripTemplateText
        call 'User32.SendMessageA' D$DialogControlsHandles  &CB_GETCURSEL  0  0
        call 'User32.SendMessageA' D$DialogControlsHandles &CB_GETLBTEXT eax LBbuffer
    pop esi
    mov edi LBbuffer | call ResetTemplateText
    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret
________________________________________________________________________________________
;;
 For some unknown reason, none of the usual working messages to retrieve text or
 value from an edit control (what work everywhere else in this source) doesn't
 work with these boxes. So i have to use "WM_GETTEXT" instead, translate it to Bin,
 then to hexa, then to hexa text !!!!!!!!............ These Edit boxes no not seam to
 have a valid ID. They are created by:

     call 'User32.CreateWindowExA'  0  EditClass  0,
             WS_CHILD+WS_VISIBLE+WS_BORDER+ES_NUMBER+ES_RIGHT+ES_MULTILINE,
             80 D$esi+4  45 20, D$EDBPadressee 0 D$hInstance 0

 ... just like any other ones...
;;

[DimIsForDialogWindow: ?] ; used as +0 / +1 (true / false) to jump over n (number of controls)

WriteDimOnly:
    push ecx                          ; edit control handle ( > edx )
        call 'User32.SendMessageA' ecx, &WM_GETTEXT, 0F, LBbuffer
    pop edx
    call SearchDialogLine           ; Edi is set there.
    mov esi DialogControlsHandles, ebx 1
    while D$esi <> edx | add esi 4 | inc ebx | End_While  ; search for what control
  ; 3 handles (text, edit, UpDown). So: 2 > 1   5 > 2   8 > 3   11 > 4
    shr ebx 1 | On ebx > 2, dec ebx
    add ebx D$DimIsForDialogWindow                       ; jump over n (number of controls)
    mov al ' '
    While ebx > 0 | repne scasb | dec ebx | End_While    ; edi > dim to overwrite
    push edi

      ; Translate decimal to binary (simplified version):
        mov eax 0, ecx 0, esi LBbuffer
L2:     mov cl B$esi | inc  esi              ; (eax used for result > no lodsb)
        cmp cl 0 | jbe  L9>
            mov edx 10 | mul edx | sub  ecx '0'
            add  eax ecx | jmp  L2<          ; >>> number in EAX
L9:         mov ebx eax
            add ebx D$ProposedUpDowmChange | mov D$ProposedUpDowmChange 0
      ; Store text hexa at "DimTempo":
        mov edi LBbuffer | call TranslateDialogText8 | mov al 0 | stosb
    pop edi
    mov eax '0000' | stosd | dec edi | mov esi LBbuffer
    While B$esi+1 > 0 | inc esi | End_While
    std | mov ecx 4
L0:     lodsb
        stosb | On esi < LBbuffer, jmp L9>
        loop L0<
L9: cld
ret

WriteDim:
    call WriteDimOnly
    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret
__________________________________________________________________________________

WriteTitle:
    call SearchDialogLine | inc edi        ; points to Class record
    push edi
        call StripTemplateText
        call 'User32.GetDlgCtrlID' D$DialogControlsHandles
        call 'User32.GetDlgItemTextA' D$DialogEditorHandle eax LBbuffer 100
                                        ; fix limit (100) here
                                        ; (sendmessage for text limitation doesn't work)
    pop esi

    mov edi LBbuffer | call ResetTemplateText

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret
_____________________________________________________________________________________

WriteID:
    call SearchDialogLine

    push edi
      call 'User32.GetDlgCtrlID' D$DialogControlsHandles
      call 'User32.GetDlgItemInt' D$DialogEditorHandle eax  0  0
    pop edi

    mov ebx eax, ecx 4  | On eax > 0, mov W$PreviousControlID ax

L0: mov eax ebx | and eax 0_F000 | shr eax 12
    add al '0' | On al > '9', add al 7
    shl ebx 4
    stosb | loop L0<

    call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox
ret
____________________________________________________________________________________

[DelMessage:"

    To delete a control:

      Select any record in the control you want to delete.       
      Do not select a separator empty line.
      Do not select a Dialog box record.
    ", 0

 Wahoo: ' Whahooo!!!...', 0]

OutDelOneControl:
    call 'USER32.MessageBoxA' D$hwnd, DelMessage, Wahoo, &MB_SYSTEMMODAL
ret


DelOneControl:
    call 'User32.SendMessageA' D$DialogListHandle &LB_GETCURSEL eax 0
    mov D$DialogListIndex eax

    .If D$DialogListIndex < Line_empty+1
        jmp OutDelOneControl

    .Else_If D$DialogListIndex > 0FFFF
        jmp OutDelOneControl

    .Else
        call SearchDialogLine
        On B$edi = 0, jmp OutDelOneControl

        mov al 0, ecx MaxTemplateText          ; edi > one record inside one control
        While B$edi > 0
            repne scasb
        End_While
        mov esi edi | sub edi 2                ; esi > end of control records
        std
            While B$edi > 0
                repne scasb
            End_While
        cld
        add edi 2                              ; edi > start of control records

; If user delete some control before defining the ID, we reset the flag for new "Add":
        push edi
            repne scasb | repne scasb
            On D$edi = '0000', mov W$PreviousControlID 0FFFF  ; ID at +52 octets from start

            mov al 255, edi esi | repne scasb  ; search for the end
            mov ecx edi
        pop edi | dec edi | sub ecx edi        ; ecx = lenght of data to move upward
        rep movsb
        call FromTextToBinTemplate | call ShowDialogResult | call FillDialogListBox

    .End_If
ret
____________________________________________________________________________________

; Called by Dialog Edition CallBack with buttons handles in ecx, controls table index in ebx:

ExitDialog:
    .If ebx = 0
        call SaveClipDialog | call CloseDialogEdition
    .Else_If ebx = 4
        If B$DialogLoadedFromResources = &TRUE
            call SaveResourceDialog | call CloseDialogEdition
        Else
            call SaveNewResourceDialog | call CloseDialogEdition
        End_If
        mov B$SourceHasChanged &TRUE
    .Else_If ebx = 8
       call CloseDialogEdition |  call SaveDialogToDisk
    .Else   ;_If ebx = 12
       call CloseDialogEdition
    .End_If
ret

CloseDialogEdition:
    call 'USER32.ClipCursor' &NULL
    call UninstallHook
    call 'User32.EndDialog' D$DialogEditorHandle 0
    call 'User32.DestroyWindow' D$EditedDialogHandle
    mov D$DialogEditorHandle 0, D$EditedDialogHandle 0
    mov B$OnDialogEdition &FALSE, B$DialogLoadedFromResources &FALSE
ret

_______________________________________________________________________________________
;;
 saving the template in ClipBoard. The main job is to make it clean and pretty source.
 (aligned comments, formated hexa numbers, ...). "B$InsideText" is used to know where
 to set leading hexa zeros (to preserve both texts and comments).
;;

[ClipTemplate: ?   ClipTemplateLength: ?]
[DialogName: 'Dialog: ' 0  ControlName: 'Control0' 0]


BuildDialogTemplate:
    VirtualAlloc ClipTemplate 60_000

    mov edi D$ClipTemplate , esi D$NewDialogTemplateText
    mov al '[' | stosb
    mov eax 'Dial' | stosd | mov eax 'og: ' | stosd
    mov ebx 0, edx 8, B$InsideText &FALSE

L0: lodsb
    ..If al = 0                                     ; line end:
        mov edx 0
        .If B$esi = 0                               ; block end (dialog or control)
            inc esi
            push edi
                std
                    mov al ';', ecx 200 | repne scasb      ; write ']' before comment
                    mov al ' ' | repe scasb | add edi 2    ; search last non space char
                    mov al ']' | stosb
                cld
            pop edi
            mov al CR, ah LF | stosw | stosw
            On B$esi = 255, jmp L9>>
            mov al '[' | stosb
            mov eax 'Cont' | stosd | mov eax 'rol0' | stosd | dec edi
            push ebx
                call TranslateDialogText4
            pop ebx
            inc ebx
            mov ax ': ' | stosw | add edx 10
        .Else
            mov al CR, ah LF | stosw | mov al ' ' | stosb
        .End_If
        mov B$InsideText &FALSE

    ..Else
        .If al = '"'
            mov al "'" | mov B$InsideText &TRUE
        .End_If
        .If al <> '0'
            If al = ';'
                mov ecx 30 | cmp edx 28 | ja L2>
                sub ecx edx | mov al ' ' | rep stosb
                mov al ';', B$InsideText &TRUE
            End_If
            On B$InsideText = &TRUE, jmp L2>
            If B$esi <> '$'                         ; set a leading '0'
                cmp B$edi-1 ' ' | jne L2>           ; for naked hexa
                cmp al '1' | jb L2>
                cmp al 'F' | ja L2>
                    mov B$edi '0' | inc edi | inc edx  ; numbers (not before 'U$'/'D$')
            End_If
L2:         stosb | inc edx
        .Else
            If W$edi-2 <> ' 0'
                stosb | inc edx                     ; no double zeros for hexa numbers
            End_If
        .End_If

    ..End_If
    jmp L0<<

L9: sub edi D$ClipTemplate | mov D$ClipTemplateLength edi
ret


SaveClipDialog:
    push D$BlockStartTextPtr, D$BlockEndTextPtr, D$BlockInside

        call BuildDialogTemplate
        move D$BlockStartTextPtr D$ClipTemplate

L9:     mov eax D$BlockStartTextPtr | add eax D$ClipTemplateLength | dec eax
        mov D$BlockEndTextPtr eax

        mov B$BlockInside &TRUE | call ControlC | mov B$BlockInside &FALSE

        VirtualFree D$ClipTemplate
        mov B$InsideText &FALSE

    pop D$BlockInside, D$BlockEndTextPtr, D$BlockStartTextPtr
ret


[DlgFilesFilters: B$ 'RosAsm Dialog Template',  0,   '*.dlgl', 0, 0]
[RcFilesFilters: B$ 'RosAsm RC Template',  0,   '*.*', 0, 0]
[SaveDlgFilter: ? #&MAX_PATH] [ChoosenDlgFile: ? #&MAX_PATH]

[OpenDlg:
 OpenDlg.lStructSize: D$ len
 OpenDlg.hwndOwner: D$ 0
 OpenDlg.hInstance: D$ 0
 OpenDlg.lpstrFilter: D$ DlgFilesFilters

 OpenDlg.lpstrCustomFilter: D$ &NULL ; uFileFilter
 OpenDlg.nMaxCustFilter: D$ 0 ; 260
 OpenDlg.nFilterIndex: D$ 1
 OpenDlg.lpstrFile: D$ SaveDlgFilter
 OpenDlg.nMaxFile: D$ 260
 OpenDlg.lpstrFileTitle: D$ ChoosenDlgFile
 OpenDlg.nMaxFileTitle: D$ 260
 OpenDlg.lpstrInitialDir: D$ 0
 OpenDlg.lpstrTitle: D$ SaveDlgNameTitle
 OpenDlg.Flags: D$ &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES__&OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 ; 0281804
 ; &OFN_CREATEPROMPT__&OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_LONGNAMES
 ; &OFN_NONETWORKBUTTON__&OFN_OVERWRITEPROMPT__&OFN_PATHMUSTEXIST
 ;  0_2000
 ; 08_0000
 ;       4
 ; 20_0000
 ;  2_0000
 ;       2
 ;     800
 OpenDlg.nFileOffset: W$ 0
 OpenDlg.nFileExtension: W$ 0
 OpenDlg.lpstrDefExt: D$ 0
 OpenDlg.lCustData: D$ 0
 OpenDlg.lpfnHook: D$ 0
 OpenDlg.lpTemplateName: D$ 0]

;[OpenPEStruc: len
; hwndPEFileOwner: 0  OPESInstance: 0  PEFilesFilters  uFileFilter  260
; 1 SaveFilter  260  ChoosenFile  260  0
; OpenPEFileTitle  OpenPEStrucFlags: 0281804
; 0  0  0  0  0]

SaveDialogToDisk:
    call BuildDialogTemplate

    mov edi SaveFilter, eax 0, ecx 65 | rep stosd
    mov D$SaveDlgFilter 'New.', D$SaveDlgFilter+3 '.dlg', D$SaveDlgFilter+7 0

    call 'Comdlg32.GetSaveFileNameA' OpenDlg | On eax = &FALSE, jmp L9>>

    call ForceExtension SaveDlgFilter, '.dlg'

    call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                               &FILE_SHARE_READ, 0,
                               &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox | jmp L9>>
    End_If

    mov D$DestinationHandle eax, D$NumberOfReadBytes 0

    call 'KERNEL32.WriteFile' D$DestinationHandle, D$ClipTemplate, D$ClipTemplateLength,
                              NumberOfReadBytes  0

    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0

L9: VirtualFree D$ClipTemplate
    mov B$InsideText &FALSE
ret


Proc ForceExtension:
    Argument @FileName, @Ext

        mov eax D@FileName, ebx D@Ext
        While B$eax <> 0 | inc eax | End_While | sub eax 4

        ..If D$eax <> ebx
            .If B$eax = '.'
L1:             mov D$eax ebx, B$eax+4 0
            .Else
                If B$eax+1 = '.'
                    inc eax | jmp L1<
                Else_If B$eax+2 = '.'
                    add eax 2 | jmp L1<
                Else_If B$eax+3 = '.'
                    add eax 3 | jmp L1<
                Else
                    add eax 4 | jmp L1<
                End_If
            .End_If
        ..End_If
EndP

; Reuse of the ClipBoard Naming because reuse of the Routine for Loading from ClipBoard.

OpenDlgFile:
    mov D$OtherFilesFilters DialogFilesFilters
    mov D$OpenOtherFileTitle DialogFilesTitle

    move D$OtherhwndFileOwner D$hwnd, D$OtherhInstance D$hInstance

    mov edi OtherSaveFilter, ecx 260, eax 0 | rep stosd
    call 'Comdlg32.GetOpenFileNameA' OtherOpenStruc

    If D$OtherSaveFilter = 0
        pop eax | ret
    End_If

    call 'KERNEL32.CreateFileA' OtherSaveFilter &GENERIC_READ,
                                &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | pop eax | ret  ; return to caller of caller
    Else
      mov D$OtherSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize'  eax 0 | mov D$ClipBoardLen eax

    If eax > 0
        VirtualAlloc ClipBoardPTR eax

        mov eax D$ClipBoardPTR | add eax D$ClipBoardLen | mov D$ClipBoardEnd eax

        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$OtherSourceHandle D$ClipBoardPTR,
                                 D$ClipBoardLen NumberOfReadBytes 0
    End_If

    call 'KERNEL32.CloseHandle' D$OtherSourceHandle
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[argh: 'Arghhh!!!...', 0]

[ClipBoardEnd: ?  OnClipDialog: ?]

___________________________________________________________________________________
;;
 Saving the template for .rsrc:

 'DialogList' table holds the records for each Dialog resource (ID / Ptr / Size). This
 table is used at building time by 'TemporaryFillRsrcList' to prepare the Resource tree
 construction.
;;

SearchLenghtOfDialogData:
    mov ebx D$LastDialogListItem, esi D$EditedDialogBoxData
    inc ebx                                     ; ex: 7+7+7+6 (last line not account)
    lodsd | lodsd                               ; styles
    lodsw | lodsw | lodsw | lodsw | lodsw       ; n, X, Y, W, H
    lodsw                                       ; menu?
    On ax > 0, lodsw                            ; menu ID
    lodsw                                       ; class? > 0 for edition

    mov edi esi
        mov ax 0, ecx 200 | repne scasw | repne scasw   ; title / font
    mov esi edi

    sub ebx Line_empty+1
    while ebx > 0
        test esi 00_11 | jz L1>
            lodsw                               ; align
L1:     lodsd | lodsd                           ; styles
        lodsw | lodsw | lodsw | lodsw           ; X, Y, W, H
        lodsw | lodsw                           ; ID / Class
        If ax <> 0FFFF
            mov edi esi
                mov ax 0, ecx 200 | repne scasw ; Text form Class (zero ended)
            mov esi edi
        End_If
        mov edi esi
            mov ax 0, ecx 200 | repne scasw     ; title
        mov esi edi
        lodsw

        sub ebx Line_empty+1
    End_While

    mov ecx esi | sub ecx D$EditedDialogBoxData ; lenght of dialog data

    mov esi ClassRecord
    While B$esi > 0
        lodsb | add ecx 2
    End_While
ret


[WhatDialogIDData: D$ 090C408C2 0  ; Style
 U$ 03 0 0 09C 01A             ; Dim
 0                             ; Menu
 0                             ; Class
 'Give an ID number for this Dialog' 0 ; Title
 08 'Helv' 0]                  ; Font

[WDIDC1: D$ 050042000 0        ; Style
 U$ 02A 05 049 0F              ; Dim
 0DE                           ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[WDIDC2: D$ 050000001 0        ; Style
 U$ 075 05 023 0E              ; Dim
 0DF                           ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[WDIDC3: D$ 050000000 0        ; Style
 U$ 03 05 024 0F               ; Dim
 0E0                           ; ID
 0FFFF 080                     ; Class
 'Abort' 0                     ; Title
 0]                            ; No creation data


[OK_ID 0DF  IDNumberEdit 0DE  Abort_ID 0E0]


[EIDNMessage: "

    Dialog ID numbers can't be greater than 0FFFF (65535)    

", 0]


ErrorIDnumber:
    call 'USER32.MessageBoxA' D$hwnd, EIDNMessage, Wahoo, &MB_SYSTEMMODAL
ret


[UserAbortID: 0] [IDstring: '        ' 0]

Proc WhatDialogIdProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        mov B$UserAbortID &FALSE
        call 'User32.GetDlgItem' D@Adressee, IDNumberEdit
        push eax
            call 'User32.SendMessageA' eax, &EM_SETLIMITTEXT, 5, 0
        pop eax
        If B$DialogLoadedFromResources = &TRUE
            mov eax D$WhatDialogListPtr, edi IDString, ebx 10          ; translate ID to
            sub eax 4 | mov eax D$eax
            add edi 9                                                    ; decimal text
L0:         mov edx 0 | div ebx
            dec edi | add dl '0' | mov B$edi dl | cmp eax 0 | ja L0<
            call 'User32.SetDlgItemTextA' D@Adressee, IDNumberEdit, edi

        Else
            mov esi DialogList
            While D$esi+12 > 0
                add esi 12
            End_While
            mov D$DialogListPtr esi | On D$esi <> 0, add D$DialogListPtr 12

            lodsd | add eax 10
            mov edi IDString, ebx 10 | add edi 9 | jmp L0<
        End_If

        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

    ...Else_If D@Message = &WM_COMMAND
        ..If W@wParam = OK_ID
            call 'USER32.GetDlgItemInt' D@Adressee, IDNumberEdit, 0, 0

            .If eax > 0FFFF
                call ErrorIDnumber
            .Else
                If B$DialogLoadedFromResources = &FALSE
                    push eax
                        call CheckNotUsedId eax, D@Adressee
                    pop ebx
                    On eax = &IDCANCEL, jmp L5>
                    mov eax ebx
                End_If

                If B$DialogLoadedFromResources = &TRUE
                    mov edi D$WhatDialogListPtr | sub edi 4 | stosd
                Else
                    mov edi D$DialogListPtr | stosd | add D$DialogListPtr 4
                End_If
                call 'User32.EndDialog' D@Adressee, 0
            .End_If

        ..Else_If W@wParam = Abort_ID
            mov B$UserAbortID &TRUE
            call 'User32.EndDialog' D@Adressee, 0
        ..End_If

    ...Else
        popad | mov eax &FALSE | jmp L9>

    ...End_If

L5: popad | mov eax &TRUE

L9: EndP
____________________________________________________________________________________________
;;
 this does only save temporary a resource template in a fitting memory. It will
 remain there until compile time fills the resource tree. "ExitDialog" clear the
 editions chunks of memory. "ReleaseResourceDialogMemory" will be called by "MainWindowProc"
 exit case
;;

SaveNewResourceDialog:
    call SearchLenghtOfDialogData
    push ecx
        VirtualAlloc TempoMemPointer ecx                ; new memory for dialog data

        call 'User32.DialogBoxIndirectParamA' D$hinstance,
               WhatDialogIDData, 0, WhatDialogIdProc, 0 ; write ID in list <<<<<<<<

        If B$UserAbortID = &TRUE
            VirtualFree D$TempoMemPointer
            pop eax, eax  | ret                 ; return to caller of 'ExitDialog'
        End_If

        mov eax D$TempoMemPointer
        mov edi D$DialogListPtr | stosd         ; write ptr to data in list <<<<<<<<
        add D$DialogListPtr 4
        mov edi eax                             ; ready for fill
    pop ecx
    mov esi D$DialogListPtr | mov D$esi ecx     ; write size in list <<<<<<<<
    add D$DialogListPtr 4
    call SaveDialogDataToResources
ret

____________________________________________________________________________________________
; In with ecx = Length as given by 'SearchLenghtOfDialogData'.
;         edi = Ptr to a Mem in DialogList.

SaveDialogDataToResources:
    mov esi D$EditedDialogBoxData

    movsd | movsd                           ; Style / Ext.Style

    movsw | movsw | movsw | movsw | movsw   ; n / X / Y / W / H

    lodsw                                   ; +2 for '0' menu in edition

    sub ecx 20                              ; >>> 20 Bytes read / 18 Bytes written

    If D$DialogMenuTrueID = 0
        stosw                               ; '0' menu
    Else
        mov eax 0FFFF | stosw
        mov eax D$DialogMenuTrueID | stosw  ; See comments at 'DialogMenuComment'
    End_If

    lodsw
    push esi
        mov esi ClassRecord, eax 0
        Do
            lodsb | stosw                   ; write preserved Class
        loop_Until al = 0
    pop esi
    sub ecx 2

    Do
        lodsw | stosw | sub ecx 2           ; write Title
    loop_Until ax = 0

    Do
        lodsw | stosw | sub ecx 2           ; write Font
    loop_Until ax = 0

;;
    Now, if edi is un-aligned, we have to align for the next Control.

    But: If esi is also un-aligned, the required alignement has already be done previously
        (at least for viewing the Dialog, when Editing). So:

    * esi un-aligned // edi un-aligned  >>> do nothing.
    * esi aligned    // edi aligned     >>> do nothing.
    * esi unaligned  // edi aligned     >>> kill previous alignment.
    * esi aligned    // edi un-aligned  >>> do the alignment.

   (The difference between esi and edi may come from addition of the Class
    Record, which is cleared off from the 'EditedDialogBoxData' for edition).
;;
    jecxz L9>

    mov eax esi, ebx edi | and eax 00_11 | and ebx 00_11
    .If eax <> ebx
        If eax = 0
            mov W$edi 0 | add edi 2
        Else
            lodsw | sub ecx 2 | jecxz L9>
        End_If
    .End_If

    rep movsb                               ; Write all Controls at once.
L9: ret

____________________________________________________________________________________________


SaveResourceDialog:
    call 'User32.DialogBoxIndirectParamA' D$hinstance,
              WhatDialogIDData  0  WhatDialogIdProc 0   ; write ID in list <<<<<<<<

    If B$UserAbortID = &TRUE
          pop eax | ret
    End_If

    mov eax D$WhatDialogListPtr, eax D$eax
    VirtualFree eax                                     ; free previous mem ptr in List

    call SearchLenghtOfDialogData
    push ecx
        VirtualAlloc TempoMemPointer ecx
        mov eax D$TempoMemPointer
        mov edi D$WhatDialogListPtr | stosd             ; write ptr to data in list <<<<<<<<
        mov edi eax                                     ; ready for fill
    pop ecx
    mov esi D$WhatDialogListPtr | add esi 4
    mov D$esi ecx                                       ; write size in list <<<<<<<<

    call SaveDialogDataToResources
ret


ReleaseResourceMemory:
  ; All Lists are: ID / Pointer to Mem / Size.

    mov esi DialogList, D$DialogListPtr DialogList
    call ReleaseOneResourceMemory

    mov esi MenuList, D$MenuListPtr MenuList
    call ReleaseOneResourceMemory

    mov esi IconList, D$IconListPtr IconList
    call ReleaseOneResourceMemory

    mov esi GroupIconList, D$GroupIconListPtr GroupIconList
    call ReleaseOneResourceMemory

    mov esi CursorList, D$CursorListPtr CursorList
    call ReleaseOneResourceMemory

    mov esi GroupCursorList, D$GroupCursorListPtr GroupCursorList
    call ReleaseOneResourceMemory

    mov esi BitMapList, D$BitMapListPtr BitMapList
    call ReleaseOneResourceMemory

    mov esi WaveList, D$WaveListPtr WaveList
    call ReleaseOneResourceMemory

    mov esi AviList, D$AviListPtr AviList
    call ReleaseOneResourceMemory

    mov esi RcDataList, D$RcDataListPtr RcDataList
    call ReleaseOneResourceMemory

 ;   mov esi OtherList, D$OtherListPtr OtherList
 ;   call ReleaseOneResourceMemory

    mov edi uRsrcList, D$uRsrcListPtr uRsrcList, eax 0, ecx 1000
    rep | stosd                             ; Clear uRsrcList
ret


ReleaseOneResourceMemory:
L0: lodsd | On eax = 0, ret                 ; ID
        mov D$esi-4 0                       ; (and clear BitMapList table)
        lodsd | mov D$esi-4 0               ; ptr
        push esi
            VirtualFree eax
        pop esi
        lodsd | mov D$esi-4 0 | jmp L0<     ; size

____________________________________________________________________________________

SearchResourceType:  ; in:  ebx = type (&RT_DIALOG, &RT_MENU, ...)
    mov esi D$UserPEStartOfResources | cmp esi 0 | je L8>

    add esi 14                          ; > points to number of resources
    mov eax 0 | lodsw | mov ecx eax     ; > in ecx
    On eax = 0, ret                     ; if no resources at all

  ; search RT_MENU, ... in resource general header:

L0: lodsd | cmp eax ebx | je L1>
        lodsd | loop L0<
            jmp L8>                     ; no whished resource found (possible naked PE)

L1: lodsd                               ; menu "Level2Rt_Menu-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF                    ; strip node flag (0_80000000)
    add eax D$UserPEStartOfResources

  ; edx will be the Number of Resources in a Type:
    add eax 14 | mov esi eax | movzx edx W$esi
  ; If the Resource is registered by name instead of by ID:

    ;.If edx = 0

    .If W$esi-2 <> 0
        call StoreNameToID esi
        mov dx W$esi-2      ; Number of Resource for a given Type
        push esi, edx
            dec dx | add esi 8
            While dx > 0 | call StoreNameToID esi | dec dx | add esi 8 | End_While
        pop edx, esi

        movzx edx W$esi-2 | add dx W$esi
  ;  .Else
  ;      If ebx = &RT_ICON
  ;          On B$FirstIconDone = &FALSE, call StoreNameToID esi
  ;      End_If

    .End_If
    add esi 2 | ret

L8: mov eax 0 | ret


; 'NamedIdList' Records are: [NumberID / StringLength with 2 "'" / 'String'] / [...] / ...

[NamedIdList: ?] [NamedIdListPtr: ?    FirstIconDone: ?]

Proc StoreNameToID:
    Argument @Pointer
    Uses esi, edi, ebx, ecx, edx

        If D$NamedIDList = 0
            pushad
                VirtualAlloc NamedIDList D$ResourcesSize
                mov D$NamedIdListPtr eax
            popad
        End_If

        mov eax D@Pointer | add eax 2 ; Points now to Name Pointer (+ 0_8000_0000 mark)
        mov eax D$eax | and eax 0_FFFF
       ; .If ebx = RT_ICON
       ;     If B$FirstIconDone = &FALSE
       ;         mov eax 1, B$FirstIconDone &TRUE
       ;     End_If
       ; .End_If
        mov edi D$NamedIdListPtr | stosd                        ; ID


        mov esi eax | add esi D$UserPEStartOfResources

        lodsw | movzx eax ax | mov ecx eax | add eax 2 | stosd  ; Length

        mov B$edi '"' | inc edi

L0:     lodsw | stosb | loop L0<
        mov B$edi '"' | inc edi | mov D$edi 0                   ; String

        mov D$NamedIdListPtr edi
EndP

____________________________________________________________________________________________

;;
  1) Search the Data 'Name'
  2) Get Back and read the Data Label
  3) Replace all Evocations of this Label by the Direct Numbered ID
;;
NamedIdSubstitution:  ; 'SymbolicAnalyzes' 'FRproc' 'StringReplaceAll'
    ...If D$NamedIdList <> 0
        mov esi D$NamedIdList
        mov B$DownSearch &TRUE, B$CaseSearch &FALSE, B$SilentSearch &TRUE

        ..While D$esi <> 0
          ; Create a Copy of the NumberID in Text form:
            lodsd | mov edi ReplaceWithString | call WriteEax | mov B$edi 0

          ; Copy the User Source IDName into the 'SearchString' Buffer:
            lodsd | mov D$LenOfSearchedString eax, ecx eax
            mov edi SearchString | rep movsb | mov al 0 | stosb

          ; esi ready for next Record:
            add esi ebx
            push esi
                mov D$NextSearchPos 0
                move D$CurrentWritingPos D$CodeSource
                call StringSearch
                ..If B$BlockInside = &TRUE
                  ; The Text of the IdName has been found. Search back for the Data Label:
                    mov esi D$BlockStartTextPtr
                    While D$esi <> 'Data' | dec esi | End_While

                  ; Copy the 'DATAXXXXXX' into the 'SearchString' Buffer:
                    mov edi SearchString, ecx 0
                    While B$esi <> ':' | movsb | inc ecx | End_While | mov al 0 | stosb
                    mov D$LenOfSearchedString ecx

                  ; Search Evocations of 'DATAXXXXXX', replace by the ID Number Text:
L0:                 call StringSearch
                    If B$BlockInside = &TRUE
                        mov eax D$BlockEndTextPtr
                        On B$eax+1 <> ':', call IDReplace
                        jmp L0<
                    End_If

                ..End_If
            pop esi
        ..End_While

        VirtualFree D$NamedIDList
    ...End_If
ret


IDReplace:
    mov esi ReplaceWithString, edi D$BlockStartTextPtr

    While B$esi <> 0 | movsb | End_While
    While B$edi > ' ' | mov B$edi ' ' | inc edi | End_While
ret


SearchResourceNamedType:   ; in:  edi = Named Type pointer ('WAVE', 'AVI', ...), edx = len
    mov esi D$UserPEStartOfResources | cmp esi 0 | je L8>>  ; retire >>

    add esi 14                            ; > points to number of resources
    mov eax 0 | lodsw | mov ecx eax       ; > in ecx
    On eax = 0, ret                    ; if no resources at all

  ; search Ptr to Name, ... in resource general header:

L0: lodsd | test eax 0_8000_0000 | jz L2>
    pushad
        xor eax 0_8000_0000 | add eax D$UserPEStartOfResources
        mov ecx edx, esi eax | lodsw | cmp al dl | jne L1>
        repe cmpsw | je L3>
L1: popad
L2: lodsd | loop L0<
      jmp L8>                             ; no whished resource found (possible naked PE)

L3: popad
    lodsd                                 ; menu "Level2Rt_Menu-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF                      ; strip node flag (0_80000000)
    add eax D$UserPEStartOfResources

    add eax 14 | mov esi eax, edx 0, dx W$esi
    If edx = 0
        call StoreNameToID esi
        mov dx W$esi-2
    End_If
    add esi 2 | ret

L8: mov eax 0 | ret


; Read all Dialogs in RosAsm PE. Same routine as the ones for icon / menu.

ReadRosAsmPeDialogs:
    mov edi DialogList, eax 0, ecx 300 | rep stosd
    mov ebx RT_DIALOG | call SearchResourceType | On eax = 0, ret
    mov D$DialogListPtr DialogList,  ebx DialogListPtr | call ReadResourcesRecord
  ret

 ______________________________________

  ; resource TYPEs dir:

  ; Resources (ID / Ptr / Size)

; in: ebx = Prt variable to a List (DialogListPtr, MenuListPtr, ...)
; (ebx is the adress, not the value -we move it in here-)
; edx is the Number of Resources in a Type

ReadResourcesRecord:
    lodsd | and eax 0FFFF | mov edi D$ebx | stosd   ; Write ID.
    mov D$ebx edi                                   ; Adjust D$XxxxxxListPtr.
    lodsd                                  ; "Level3Rt_Menu-StartOfRsrc+NodeFlag" in eax

    push esi, edx
        test eax 08000_0000 | jnz L1>
            add eax D$UserPEStartOfResources | mov esi eax | jmp L5>>

L1:     and eax 0FFFFFFF | add eax D$UserPEStartOfResources | add eax 20 | mov esi eax
      ; Language. dir:
        lodsd                    ; "Level4Rt_Menu-StartOfRsrc" in eax (no NodeFlag here
                                 ; next one is leafe ptr to true resources)
        add eax D$UserPEStartOfResources
        mov esi eax

      ; Records of each resource:
L5:     lodsd                                   ; ptr to menu data (but RVA - startOfResource)
L1:     mov ecx D$esi
        sub eax D$ResourcesRVA                  ; - RVA
        add eax D$UserPEStartOfResources        ; eax now points to true menu data

        If eax < D$UserPeStart
            jmp DisFail
        Else_If eax > D$UserPeEnd
            jmp DisFail
        End_If

        push eax
            push ebx, ecx
                VirtualAlloc TempoMemPointer ecx
            pop ecx, ebx
        pop esi

        mov edi D$TempoMemPointer
        push edi, ecx
            rep movsb                           ; copy bin template to temporary mem
        pop ecx, eax

        mov edi D$ebx | stosd
        mov eax ecx | stosd | mov D$ebx edi

    pop edx, esi
    dec edx | cmp edx 0 | ja ReadResourcesRecord; next resource TYPEs dir record > ID ready
L9: mov eax &TRUE
ret
_____________________________________________________________________________________
;;
 All routines for dialog edition are first based on text version of templates. Here,
 we got first a bin template > translation back to text needed. All dialog resources
 have been uploaded in memories chuncks stored in "DialogList" (/ID/Ptr/Size). The
 Naming "LoadFromResouce" is only to echoe what user clicked on in main menu, but,
 in fact, upload job was previously done by "ReadRosAsmPeDialogs" when file is open.
 We now simply translate to text version before runing edition:
;;

[NoResourceDialog: "

    There is no Resources Dialog in this PE      

", 0]


LoadFromResources:
    If B$OnDialogEdition = &TRUE
        Beep | ret  ; prevents from multi-runs
    End_If
    call InitDialogMemory

    mov esi DialogList
    .If D$esi  = 0                    ; empty? > out
        call 'USER32.MessageBoxA' D$hwnd, NoResourceDialog, Argh, &MB_SYSTEMMODAL | ret

    .Else_If D$esi+12 = 0               ; only one resource? > OK
        mov D$WhatDialogListPtr DialogList+4

        mov ebx D$WhatDialogListPtr, ebx D$ebx
        If W$ebx+18 = 0
            mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
        Else
            movzx eax W$ebx+20 | mov D$DialogMenuTrueID eax
            mov esi MenuList
            While D$esi <> eax
                add esi 12
            End_While
            mov D$MenuListPtr esi
            add esi 4
            call 'User32.LoadMenuIndirectA' D$esi | mov D$ActualMenutestID eax
        End_If

    .Else                               ; several resources? > wich one?
        mov D$DialogListHandle 0, D$DialogEditorHandle 0
        call WhatResourceTemplate       ; > D$WhatDialogListPtr > Data pointer

    .End_If

    mov B$DialogLoadedFromResources &TRUE
    call FromBinToTextTemplate
    call ReInitDialogEdition
ret

____________________________________________________________________________________________

; The following routines have been rewritten to loose the dependency on the old routines.
; They are not rewritten in an intelligent way. They are just rewritten so they will do
; exactly the same thing as before (no matter how stupid that was/is).
____________________________________________________________________________________________

[DialogFromFile: ?]

LoadDialogFromFile:
    If B$OnDialogEdition = &TRUE
        Beep | ret
    End_If

    mov B$DialogFromFile &TRUE

    call InitDialogMemory

    call OpenDlgFile | jmp L0>


LoadDialogFromClipBoard:
    If B$OnDialogEdition = &TRUE
        Beep | ret
    End_If

    mov B$DialogFromFile &FALSE

    call InitDialogMemory

    call OpenClipBoard


L0: On D$ClipBoardPtr = 0, ret
    On D$ClipBoardlen = 0, ret

    mov B$WeAreInTheCodeBox &TRUE
    mov eax esp, D$OldStackPointer eax

    push D$CodeSourceA, D$CodeSourceB

        mov ecx D$ClipBoardlen | add ecx 010
        VirtualAlloc CodeSourceA, ecx | add D$CodeSourceA 010
        mov ecx D$ClipBoardlen | add ecx 010
        VirtualAlloc CodeSourceB, ecx | add D$CodeSourceB 010

        call NewCopyToCodeSourceA D$ClipBoardPtr, D$ClipBoardlen

        If B$DialogFromFile = &TRUE
            VirtualFree D$ClipBoardPTR
        Else
            call CloseClipBoard
        End_If

        mov esi D$CodeSourceA | While B$esi > 0 | inc esi | End_While
        mov W$esi CRLF | add esi 2 | add D$StripLen 2

        call ClearQwordCheckSum

        call CoolParsers

        call NewCountStatements | On B$CompileErrorHappend = &TRUE, jmp L9>>

        call HotParsers | On B$CompileErrorHappend = &TRUE, jmp L9>>

        call InitIndex1 | call InitIndex2

        Exchange D$CodeSourceA D$CodesourceB

        push D$SourceLen
            move D$SourceLen D$StripLen
            move D$AsmTablesLength D$SourceLen
            call ReuseSourceAForCodeList
        pop D$SourceLen

        ;call ClearQwordCheckSum

        call InitIndex3

        call SaveCheckSumTable

        mov eax D$CodeListPtr | mov D$DataList eax | mov D$DataListPtr eax
        call StoreDatas

        call RestoreCheckSumTable

L9:     mov B$WeAreInTheCodeBox &FALSE | On B$CompileErrorHappend = &TRUE, jmp L9>>

        mov B$OnClipDialog &TRUE

        mov esi D$DataList, edi D$NewDialogTemplateText, D$ActualEditedDialogID 0
        mov eax D$DataListPtr | sub eax D$DataList | inc eax
        mov D$ResourceDialogSize eax

        .If W$esi+18 = 0FFFF
            movzx eax W$esi+20 | mov D$DialogMenuTrueID eax
            push esi
                mov esi MenuList
                While D$esi <> eax
                    add esi 12
                    If D$esi = 0
                        pop esi | call NoSuchMenu | jmp L9>
                    End_If
                End_While
            pop esi
        .End_If

        call FromClipBoardBinToText | call ReInitDialogEdition

L9:     VirtualFree D$CodeSourceA, D$CodeSourceB
    pop D$CodeSourceB, D$CodeSourceA
ret
____________________________________________________________________________________________

[NoMenuMessage: "
This Dialog Template cannot be loaded because it    
includes a Menu that is not found in the actual
Resources.

                            Menu"

MissingMenuID: "         ", 0]

NoSuchMenu:
    call WriteDecimalID eax, MissingMenuID
    call 'USER32.MessageBoxA' D$hwnd, NoMenuMessage, Argh, &MB_OK
ret
____________________________________________________________________________________________


[WhatDialogListPtr: ?  OkDialogFlag: ?  ChoiceDialogHandle: ?]

WhatResourceTemplate:
  ; 'DialogList' structure is (dWords): ID / Ptr / Size // ...
  ; D$WhatDialogListPtr >>> Ptr to Dialog Mem in the form of 'DefaultDialogTemplateText').
    mov D$OkDialogFlag &FALSE, D$WhatDialogListPtr DialogList+4
    .While B$OkDialogFlag = &FALSE

        mov ebx D$WhatDialogListPtr, ebx D$ebx
        If W$ebx+18 = 0
            mov D$ActualMenutestID 0, D$DialogMenuTrueID 0
        Else
          ; If Menu, W$ebx+18 = 0FFFF // W$ebx+20 = ID Number.
            movzx eax W$ebx+20 | mov D$DialogMenuTrueID eax
            mov esi MenuList
            While D$esi <> eax
                add esi 12
            End_While
            mov D$MenuListPtr esi | add esi 4
            call 'User32.LoadMenuIndirectA' D$esi | mov D$ActualMenutestID eax
        End_If

        call FromBinToTextTemplate | call FromTextToBinTemplate

        call 'User32.CreateDialogIndirectParamA' D$hinstance, D$EditedDialogBoxData,
                                                 D$EditWindowHandle, EditedDialogBoxProc, 0
        mov D$ChoiceDialogHandle eax

        call SetNextChoiceID
        call 'User32.DialogBoxIndirectParamA' D$hinstance, ChoiceBar, D$hwnd, ChoiceDialogBoxProc, DialogList
      ; This is the deletion of the viewed Dialog (not of the Choice Bar Dialog):
        call 'User32.EndDialog' D$ChoiceDialogHandle 0

        .If B$OkDialogFlag = &VK_ESCAPE
            pop eax | ret                          ; abort "LoadFromResources" caller
        .Else_If D$WhatDialogListPtr < DialogList
            mov D$WhatDialogListPtr DialogList+4
            call SetNextChoiceID
        .Else
            mov esi D$WhatDialogListPtr
            If D$esi = 0
                sub D$WhatDialogListPtr 12 | mov esi D$WhatDialogListPtr
                call SetNextChoiceID
            End_If
        .End_If
    .End_While
ret
____________________________________________________________________________________________
;                       _______   _______   _______
; The tool:            [  <<<  ] [  OK   ] [  >>>  ]       at 'ChoiceBar'
;                       -------   -------   -------

[ChoiceDialogBoxHandle: ?    UserIsChoosing: ?]
[ChoiceBackHandle: ?    ChoiceOKHandle: ?    ChoiceForthHandle: ?]

SetNextChoiceID:
L1: mov eax D$WhatDialogListPtr, eax D$eax-4
    mov ecx 10, edi ChoiceDecimalID+10, D$edi 0200020, D$edi+4 0200020,
    D$edi+8 0200020, D$edi+12 0200020
    add edi 18
L2: mov edx 0 | div ecx | dec edi | add dl '0'
    mov B$edi 0 | dec edi | mov B$edi dl | cmp eax 0 | ja L2<
ret

[ListOrigin: ?]
Proc ChoiceDialogBoxProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        mov eax D@wParam | and eax 0FFFF
        .If eax = CHOICEFORTH
            add D$WhatDialogListPtr 12 | call SetNextChoiceID

        .Else_If eax = CHOICEBACK
            sub D$WhatDialogListPtr 12 | call SetNextChoiceID

        .Else_If eax = CHOICEFIRST
            move D$WhatDialogListPtr D$ListOrigin  ;DialogList+4 |
            add D$WhatDialogListPtr 4
            call SetNextChoiceID

        .Else_If eax = CHOICELAST
L0:         add D$WhatDialogListPtr 12 | mov eax D$WhatDialogListPtr
            cmp D$eax 0 | ja L0<
            sub D$WhatDialogListPtr 12 | call SetNextChoiceID

        .Else_If eax = CHOICEOK
            mov B$OkDialogFlag &TRUE

        .Else_If eax = &IDCANCEL
            mov B$OkDialogFlag &VK_ESCAPE

        .Else
            jmp L8>>
        .End_If

        mov D$ChoiceDialogBoxHandle 0
        call 'User32.EndDialog' D@Adressee 0

    ...Else_If D@Message = &WM_INITDIALOG
        move D$ChoiceDialogBoxHandle D@Adressee, D$ListOrigin D@lParam

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.GetWindowPlacement' D@Adressee Control
        mov eax D$Control.rcNormalPosition.top | shr eax 2
        add D$Control.rcNormalPosition.top eax
        add D$Control.rcNormalPosition.bottom eax
        call 'USER32.SetWindowPlacement' D@Adressee Control

    ...Else
        If D$ChoiceDialogBoxHandle <> 0
            call 'USER32.GetFocus'
            On eax <> D@Adressee, call 'User32.SetForegroundWindow' D@Adressee
        End_If
        popad | mov eax &FALSE | jmp L9>

    ...End_If

L8: popad | mov eax &TRUE

L9: EndP



[SureDeleteDialog: 'Kill this Dialog?', 0]

DeleteDialog:
    If B$OnDialogEdition = &TRUE
        Beep | ret                    ; prevents from multi-runs
    End_If
    call InitDialogMemory
    mov esi DialogList
    .If D$esi  = 0                                          ; empty? > out
        call 'USER32.MessageBoxA' D$hwnd, NoResourceDialog, Argh, &MB_SYSTEMMODAL | ret
    .Else                                                   ; Wich resources to kill?
        call WhatResourceTemplate
    .End_If
    mov eax D$WhatDialogListPtr
    call 'User32.CreateDialogIndirectParamA' D$hinstance, D$eax, D$hwnd, EditedDialogBoxProc 0
        mov D$ChoiceDialogHandle eax

    call 'USER32.MessageBoxA' D$hwnd, SureDeleteDialog, Argh, &MB_SYSTEMMODAL+&MB_YESNO
    push eax
        call 'User32.DestroyWindow' D$ChoiceDialogHandle
    pop eax

    If eax = &IDYES
        sub D$WhatDialogListPtr 4                       ; > ID / Ptr /Size
        mov esi D$WhatDialogListPtr, edi esi
        add esi 12
        mov eax esi | sub eax DialogList | shr eax 2

        mov ecx MAXDIALOG | sub ecx eax                 ; tail to move
        rep movsd                                       ; scratch
    End_If

    VirtualFree D$NewDialogTemplateText, D$EditedDialogBoxData
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

SimplyGetDialog:
    call InitDialogMemory

    mov esi DialogList
    If D$esi  = 0
        call 'USER32.MessageBoxA' D$hwnd, NoResourceDialog, Argh, &MB_SYSTEMMODAL
        mov eax 0 | ret
    Else
        call WhatResourceTemplate
    End_If
ret


SaveToBinaryFile:
  ; Prevent from Multiple Instances:
    If B$OnDialogEdition = &TRUE
        Beep | ret
    End_If

    call SimplyGetDialog | On B$OkDialogFlag = &VK_ESCAPE, ret

  ; Similar to SaveDialogToDisk:

    mov edi SaveFilter, eax 0, ecx 65 | rep stosd
    mov D$SaveDlgFilter 'New.', D$SaveDlgFilter+3 '.dlf', D$SaveDlgFilter+7 0

    call 'Comdlg32.GetSaveFileNameA' OpenDlg | On eax = &FALSE, ret

    call ForceExtension SaveDlgFilter, '.bdf'

    call 'KERNEL32.CreateFileA' SaveDlgFilter &GENERIC_WRITE,
                               &FILE_SHARE_READ, 0,
                               &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox | ret
    End_If

    mov D$DestinationHandle eax, D$NumberOfReadBytes 0

    mov esi D$WhatDialogListPtr | sub esi 4 | mov ecx D$esi+8, esi D$esi+4

    call 'KERNEL32.WriteFile' D$DestinationHandle, esi, ecx, NumberOfReadBytes  0

    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
ret


[BinDialogMemory: ? BinDialogLength: ?]

LoadFromBinaryFile:
    If B$OnDialogEdition = &TRUE
        Beep | ret
    End_If

    mov D$OtherFilesFilters BinDialogFilesFilters
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

    call 'KERNEL32.GetFileSize' eax, 0 | mov D$BinDialogLength eax

    If eax > 0
        VirtualAlloc BinDialogMemory eax

        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$OtherSourceHandle, D$BinDialogMemory,
                                 D$BinDialogLength, NumberOfReadBytes, 0
    Else
        ret
    End_If

    call 'KERNEL32.CloseHandle' D$OtherSourceHandle

    mov esi DialogList | While D$esi <> 0 | add esi 12 | End_While
    mov D$DialogListPtr esi
    mov eax D$BinDialogMemory, D$esi+4 eax
    mov eax D$BinDialogLength, D$esi+8 eax

    mov B$DialogLoadedFromResources &FALSE
    call 'User32.DialogBoxIndirectParamA' D$hinstance, WhatDialogIDData, 0,
                                          WhatDialogIdProc, 0

    If B$UserAbortID = &TRUE
        mov esi DialogList | While D$esi+4 <> 0 | add esi 12 | End_While
        mov D$esi-4 0, D$esi-8 0, D$esi-12 0
        VirtualFree D$BinDialogMemory
    End_If
ret


ReplaceFromBinaryFile:
    If B$OnDialogEdition = &TRUE
        Beep | ret
    End_If

    call SimplyGetDialog | On B$OkDialogFlag = &VK_ESCAPE, ret

    mov D$OtherFilesFilters BinDialogFilesFilters
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

    call 'KERNEL32.GetFileSize' eax, 0 | mov D$BinDialogLength eax

    If eax > 0
        VirtualAlloc BinDialogMemory eax

        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$OtherSourceHandle, D$BinDialogMemory,
                                 D$BinDialogLength, NumberOfReadBytes, 0
    Else
        ret
    End_If

    call 'KERNEL32.CloseHandle' D$OtherSourceHandle

    mov edi D$WhatDialogListPtr | sub edi 4
    VirtualFree D$edi+4
    mov eax D$BinDialogMemory, D$edi+4 eax
    mov eax D$BinDialogLength, D$edi+8 eax
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[SureReplaceDialog: B$ 'Replace with this Dialog ?', 0]

ReplaceDialogFromFile:  ; DeleteDialog // LoadDialogFromFile


    ____________________________

  ; LoadDialogFromFile

    call OpenDlgFile | On D$ClipBoardPtr = 0, ret
                       On D$ClipBoardlen = 0, ret

    call 'USER32.MessageBoxA' D$hwnd, SureReplaceDialog, Argh, &MB_SYSTEMMODAL+&MB_YESNO

    If eax = &IDYES
        mov esi D$ClipBoardPtr, edi D$WhatDialogListPtr, ecx D$ClipBoardlen
        mov D$edi+4 ecx, edi D$edi
        rep movsb
    End_If

    call CloseClipBoard

    mov B$DialogLoadedFromResources &TRUE
    call FromBinToTextTemplate
    call ReInitDialogEdition
ret
____________________________________________________________________________________________


[CHOICEBACK 0136  CHOICEOK 01  CHOICEFORTH 0138  CHOICEID 010   CHOICEFIRST 037
 CHOICELAST 038]

[ChoiceBar: D$ 0900408C2 0        ; Style
 U$ 06 0 0 0C7 017             ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 'New Dialog' 0                ; Title
 08 'Helv' 0]                  ; Font

[CGroup: D$ 050000307 0      ; Style
 U$ 0 0 0C8 019                ; Dim
 010                           ; ID
 0FFFF 080                     ; Class
 ChoiceDecimalID:
 'ID =          ' 0            ; Title
 0]                            ; No creation data

[CBack: D$ 050000000 0      ; Style
 U$ 0E 09 038 0E               ; Dim
 0136                          ; ID
 0FFFF 080                     ; Class
 '<<<<<' 0                     ; Title
 0]                            ; No creation data

[COK: D$ 050000000 0      ; Style
 U$ 047 09 038 0E              ; Dim
 01                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[CForth: D$ 050000000 0      ; Style
 U$ 080 09 038 0E              ; Dim
 0138                          ; ID
 0FFFF 080                     ; Class
 '>>>>>' 0                     ; Title
 0]                            ; No creation data

[Cfirst: D$ 050000000 0      ; Style
 U$ 02 09 0B 0E                ; Dim
 037                           ; ID
 0FFFF 080                     ; Class
 '[<' 0                        ; Title
 0]                            ; No creation data

[Clast: D$ 050000000 0      ; Style
 U$ 0B9 09 0B 0E               ; Dim
 038                           ; ID
 0FFFF 080                     ; Class
 '>]' 0                        ; Title
 0]                            ; No creation data


;;
"Load_Dialog_from_Resources" effective Translation for bin to text template.
 Called only in case of 'Load Existing Resource Dialog' and 'Search for what Dialog '.
 So we can find in the Dialog data some menu ID. We set it in 'D$DialogMenuTrueID'
 (to see DialogMenuComment, < Right Click).

 'FromTextToBinTemplate' doesn't do the reverse operation...
;;

[ActualEditedDialogID: 0  ResourceDialogSize: 0  DialogNcontrol: 0]
[isDLGEX: B$ 0 InjectFont: 0]
FromBinToTextTemplate:
    call InitDialogMemory
    mov B$OnClipDialog &TRUE              ; reuse of this flag for initialisation in proc

    mov eax D$WhatDialogListPtr, esi D$eax, edi D$NewDialogTemplateText  ; esi > ptr
    move D$ActualEditedDialogID D$eax-4                                  ; ID
    move D$ResourceDialogSize D$eax+4                                    ; size

FromClipBoardBinToText:
    cmp D$esi 0FFFF0001 | setz B$isDLGEX

    mov eax 'D$  ' | stosd | dec edi
    If B$isDLGEX = &TRUE
       mov ebx D$esi+0C
    Else
       mov ebx D$esi
    End_If
    test ebx &DS_SETFONT | setz B$InjectFont | or ebx &DS_SETFONT | call TranslateDialogText8
    mov eax ' ; S' | stosd | mov eax 'tyle' | stosd | mov al 0 | stosb
    mov eax 'D$  ' | stosd | dec edi
    mov ebx D$esi+08
    If B$isDLGEX = &TRUE
       mov ebx D$esi+08
    Else
       mov ebx D$esi+04
    End_If
    call TranslateDialogText8
    mov eax ' ; E' | stosd | mov eax 'xSty' | stosd |mov eax 'le' | stosd | dec edi
    If B$isDLGEX = &TRUE
;      mov eax 'D$ 0' | stosd | dec edi
;      mov ebx D$esi+04 | call TranslateDialogText8; HelpID
;      mov eax ' ; H' | stosd | mov eax 'elpI' | stosd| mov ax 'D' | stosw
       add esi 08
    End_If
    add esi 08

    mov eax 'U$  ' | stosd | dec edi
    lodsw | mov W$DialogNcontrol ax
            mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb  ; n
    lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb  ; X
    lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb  ; Y
    lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb  ; W
    lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb  ; H
    mov eax ' ; D' | stosd | mov ax 'im' | stosw | mov al 0 | stosb

    lodsw    ; always 0 in RosAsm edition but 0 or 'FFFF ID' in resources:

    If D$DialogMenuTrueID = 0
        mov eax '0 ; ' | stosd | mov eax '    ' | stosd |
        mov eax ' no ' | stosd | mov eax 'Menu' | stosd | mov al 0 | stosb  ; No menu
      ; beware: this is room for "FFFF ID ; menu" when menu added!
    Else
        mov ebx 0FFFF | call TranslateDialogText4 | mov al ' ' | stosb       ; Menu
        lodsw
        mov ebx D$DialogMenuTrueID |  call TranslateDialogText4
        mov eax ' ; M' | stosd | mov ax 'en' | stosw | mov al 'u' | stosb | mov al 0 | stosb
    End_If

    mov al '"' | stosb | mov bx W$esi | inc bx | jne L0> | add esi 4 | jmp L1> ; can Id
L0: lodsw | cmp ax 0 | je L1>
        stosb | jmp L0<
L1: mov al '"' | stosb
    mov eax ' 0 ;' | stosd | mov eax ' Cla' | stosd | mov ax 'ss' | stosw
    mov al 0 | stosb

    mov al '"' | stosb
L0: lodsw | cmp ax 0 | je L1>
        stosb | jmp L0<
L1: mov al '"' | stosb
    mov eax ' 0 ;' | stosd | mov eax ' Tit' | stosd | mov ax 'le' | stosw
    mov al 0 | stosb

    cmp B$InjectFont 0 | je F0>
    mov eax '08 "' |stosd| mov eax 'Helv' |stosd| jmp L1> ; Inject font!
F0:
    lodsw | mov ebx eax | call TranslateDialogText2 ;pointsize
    If B$isDLGEX = &TRUE
;       mov al 020 | stosb
;       lodsw | mov ebx eax | call TranslateDialogText4 ;weight
;       mov al 020 | stosb
;       lodsw | mov ebx eax | call TranslateDialogText4 ;bItalic
       add esi 04 ; or above
    End_If
    mov ax ' "' | stosw
L0: lodsw | cmp ax 0 | je L1>
        stosb | jmp L0<
L1: mov al '"' | stosb
    mov eax ' 0 ;' | stosd | mov eax ' Fon' | stosd | mov al 't' | stosb
    mov al 0 | stosb | stosb

    ALIGN_ON 4 esi

L1: .While W$DialogNcontrol > 0
       dec W$DialogNcontrol
       mov eax 'D$  ' | stosd | dec edi
       If B$isDLGEX = &TRUE
          mov ebx D$esi+08
       Else
          mov ebx D$esi
       End_If
       call TranslateDialogText8
       mov eax ' ; S' | stosd | mov eax 'tyle' | stosd | mov al 0 | stosb
       mov eax 'D$  ' | stosd | dec edi
       mov ebx D$esi+04
       call TranslateDialogText8
       mov eax ' ; E' | stosd | mov eax 'xSty' | stosd |mov eax 'le' | stosd | dec edi
       If B$isDLGEX = &TRUE
;         mov eax 'D$  ' | stosd | dec edi
;         mov ebx D$esi | call TranslateDialogText8; HelpID
;         mov eax ' ; H' | stosd | mov eax 'elpI' | stosd| mov ax 'D' | stosw
          add esi 04
       End_If
       add esi 08

       mov eax 'U$  ' | stosd | dec edi
       lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb ; X
       lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb ; Y
       lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb ; W
       lodsw | mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb ; H
       mov eax ' ; D' | stosd | mov eax 'im' | stosd | dec edi

       lodsw | mov ebx eax | call TranslateDialogText4
       If B$isDLGEX = &TRUE
          add esi 2
       End_If
       mov eax ' ; I' | stosd | mov ax 'D' | stosw

       lodsw
       If ax = 0FFFF
          mov ebx eax | call TranslateDialogText4 | mov al ' '| stosb
          lodsw | mov ebx eax | call TranslateDialogText4
       Else
          sub esi 2 | mov al '"' | stosb
L0:       lodsw | cmp ax 0 | je L1>
          stosb | jmp L0<
L1:       mov al '"' | stosb | mov ax ' 0' | stosw
       End_If
       mov eax ' ; C' | stosd | mov eax 'lass' | stosd | mov al 0 | stosb

      mov al '"' | stosb | mov bx W$esi | inc bx | jne L0> | add esi 4 | jmp L1> ; can ResId
L0:   lodsw | cmp ax 0 | je L1>
      stosb | jmp L0<
L1:   mov al '"' | stosb
      mov eax ' 0 ;' | stosd | mov eax ' Tit' | stosd | mov ax 'le' | stosw
      mov al 0 | stosb

      movzx eax W$esi
      If B$isDLGEX = &FALSE
         mov ah 0
      End_If
      add esi eax ; skip CreationData; NoAlign!
L1:
      mov eax '0 ; ' | stosd | mov eax 'No c' | stosd | mov eax 'reat' | stosd
      mov eax 'ion ' | stosd | mov eax 'data' | stosd | mov al 0 | stosb | stosb

      add esi 5 | and esi 0-4;ALIGN_ON 4 esi
L1:
    .End_While
    mov al 255 | stosb
ret
;DLGTEMPLATEEX
;[0FFFF0001, D$helpID, D$EXSTYLE, D$STYLE, W$Ncntrls, W$X, W$Y, W$Width, W$Height
;{W$0 | W$-1_W$MENUID | uMENU} {W$0 | W$-1_W$CLASS | uSTRING} {W$0 | uTITLE}
;if &DS_SETFONT {W$pointsize, W$weight, W$bItalic, uFontName}] ALIGN 4
;DLGITEMTEMPLATEEX
;[D$helpID, D$exStyle, D$style, W$x, W$Y, W$w, W$h, W$id
;{W$-1_W$CLASS | uSTRING} {W$0 | W$-1_W$ResID | uTITLE}
;W$extraCount] ALIGN 4


; Set a working ID for a Menu in Resources Upload Menu (see: DialogMenuComment):

SetMenuToLoadWithDialog:
    pushad
        mov eax D$DialogMenuTrueID, esi MenuList
        While D$esi <> eax
            If D$esi = 0
                mov D$DialogMenuTrueID 0, D$ActualMenutestID 0
                popad | mov eax 0 | ret
            End_If
            add esi 12
        End_While
        add esi 4
        call 'User32.LoadMenuIndirectA' D$esi
        mov D$ActualMenutestID eax
    popad
ret






