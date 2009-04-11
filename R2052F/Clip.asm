TITLE Clip
____________________________________________________________________________________________
____________________________________________________________________________________________
;                                     Templates.
;
; A simple bank of Templates to be copied inside the user ClipBoard trough a friendly
; Dialog interface ([Clip] Menu Option).

[GenericName: 0 #10]     ; 40 bytes for a name to add to symbols.

; 'TemplateChoice' 'AddGroup' 'DeleteGroup' 'AddTemplate' 'DeleteTemplate'
;
;
; Proc modified is: 'ShowClip'
; Additional procs added are: 'Clip_Disable_Customization_Controls' and
; 'Clip_Enable_Customization_Controls'
____________________________________________________________________________________________
____________________________________________________________________________________________

[ClipMessage: " Copy 'Clip.txt' in this directory
or run [Config] menu option" 0]

Templates:
    If D$ClipperHandle > 0
        Beep | ret
    End_If

    On eax = M00_Clip_File, jmp L1>

    If D$NumberOfClipFiles > 1
        call LoadSelectedClipFile
    Else
L1:     call LoadClipFile
    End_If

    If B$ClipFileReady = &TRUE
      ; Tag Dialog 2000
        call 'USER32.DialogBoxParamA' D$hInstance, 2000, &NULL, TemplateChoice, &NULL
        call ReleaseClipFile
    Else_If B$ClipFileReady = &FALSE
        call 'User32.MessageBoxA' D$hwnd, ClipMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
    Else_If B$ClipFileReady = 0-1
        call 'USER32.MessageBoxA' D$hwnd, ClipTitleWanted, UnvalidClipTitle, &MB_SYSTEMMODAL
        call ReleaseClipFile
    End_If
ret


[ClipFileHandle: ?   ClipFileSize: ?    ClipFileMemoryPointer: ?    ClipMemoryEnd: ?
 ClipFileReady: ?]

[CLIPRESERVATION 010000]

LoadClipFile:
    call 'KERNEL32.CreateFileA' ClipName, &GENERIC_READ, 0, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, &NULL
    If eax = &INVALID_HANDLE_VALUE
        mov B$ClipFileReady &FALSE | ret
    Else
        mov B$ClipFileReady &TRUE, D$ClipFileHandle eax
    End_If

    call 'KERNEL32.GetFileSize' D$ClipFileHandle, &NULL | mov D$ClipFileSize eax

    add eax CLIPRESERVATION | VirtualAlloc ClipFileMemoryPointer, eax

    add eax D$ClipFileSize | add eax CLIPRESERVATION | mov D$ClipMemoryEnd eax

    call 'KERNEL32.ReadFile' D$ClipFileHandle, D$ClipFileMemoryPointer, D$ClipFileSize,
                            NumberOfReadBytes, &NULL
    call ClipCRLFs
ret


LoadSelectedClipFile:
  ; eax stil hold the ID: 7000, 7001, ...
    sub eax 7000

    mov esi ClipMenuStrings
    .While eax > 0
        While B$esi <> 0 | inc esi | End_While | inc esi
        dec eax
    .End_While

    mov edi ClipName
    While B$edi <> 0 | inc edi | End_While
    While B$edi <> '\' | dec edi | End_While | inc edi
    While B$esi <> 0 | movsb | End_While | movsb

    call LoadClipFile
ret


WriteClipFile:
    call 'KERNEL32.CloseHandle' D$ClipFileHandle

    call 'KERNEL32.CreateFileA' ClipName, &GENERIC_READ__&GENERIC_WRITE, 0, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, &NULL
    mov D$ClipFileHandle eax, D$NumberOfReadBytes 0

    call 'KERNEL32.WriteFile' D$ClipFileHandle, D$ClipFileMemoryPointer, D$ClipFileSize,
                              NumberOfReadBytes, 0
ret


ReleaseClipFile:
    VirtualFree D$ClipFileMemoryPointer
    call 'KERNEL32.CloseHandle' D$ClipFileHandle
ret


[OneClipName: ? #40]

ReadClipsections:
    call 'USER32.SendMessageA' D$TemplateList1, &LB_RESETCONTENT, 0, 0

    mov esi D$ClipFileMemoryPointer

    .While esi < D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi = '/'
                ..If B$esi-2 < ' '
                    inc esi | mov edi OneClipName
                    While B$esi >= ' '
                        movsb
                    End_While
                    mov al 0 | stosb

                    call 'USER32.SendMessageA' D$TemplateList1, &LB_ADDSTRING,
                                               0, OneClipName
                ..End_If
            .End_If
        End_If
    .End_While
ret


; in eax: 0 based index of the first ListBox (Sections)

[SectionPointer: ?]

SearchSection:
    inc eax | mov ebx eax
    mov esi D$ClipFileMemoryPointer

    .While esi < D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi = '/'
                ..If B$esi-2 < ' '
                    dec ebx | jnz L1>
                        While B$esi <> LF
                            inc esi
                        End_While
                        inc esi | mov D$SectionPointer esi | ret
L1:             ..End_If
            .End_If
        End_If
    .End_While

    mov D$SectionPointer 0
ret


ReadClipTitles:
    mov esi D$SectionPointer
    mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize

    .While esi < edx ;D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi-2 < ' '
                ..If B$esi = '/'
                    ret
                ..Else
                    mov edi OneClipName
                    While B$esi >= ' '
                        movsb
                    End_While
                    mov al 0 | stosb
                    push edx
                        call 'USER32.SendMessageA' D$TemplateList2  &LB_ADDSTRING,
                                                   0 OneClipName
                    pop edx
                ..End_If
            .End_If
        End_If
    .End_While
ret


; in eax: 0 based index of the second ListBox (Titles)

[ClipPointer: ?   ClipGroupIndex: ?   ClipIndex: ?]

SearchTheClip:
    mov D$TheClipLenght 0, D$ClipPointer 0
    call 'USER32.SendMessageA' D$TemplateList2, &LB_GETCURSEL, 0, 0 | cmp eax &LB_ERR | je L9>>
    mov D$ClipIndex eax
    mov esi D$SectionPointer | inc eax | mov ebx eax

    .While esi < D$ClipMemoryEnd
        lodsb
        If al = '/'
            .If B$esi-2 < ' '
                dec ebx | jnz L1>
                    While B$esi <> LF
                            inc esi
                    End_While

                    inc esi | mov D$ClipPointer esi | call SearchTheClipLenght | jmp L9>
L1:         .End_If
        End_If
    .End_While
L9: ret


[TheClipLenght: ?]

SearchTheClipLenght:
    .While esi < D$ClipMemoryEnd
        lodsb | inc D$TheClipLenght
        .If al = '/'
            If B$esi-2 < ' '
                dec D$TheClipLenght | ret
            End_If
        .Else_If al = 0
            dec D$TheClipLenght | ret
        .End_If
    .End_While
ret
____________________________________________________________________________________________


[TempoClipmemoryPtr: ?]
[ControlID108: ?] ; Handle to Generic Name textbox
[ControlID104: ?] ; These are the 4 handles to the radio buttons.
[ControlID105: ?]
[ControlID106: ?]
[ControlID107: ?]


ShowClip:
    call RetrieveGenericName
    call SearchTheClip | On D$TheClipLenght = 0, ret

    mov ecx D$TheClipLenght | shl ecx 2               ; *4 > room for Generic Names
    push ecx
        VirtualAlloc TempoClipmemoryPtr ecx
        mov edi D$TempoClipmemoryPtr, esi D$ClipPointer
    pop ecx
    shr ecx 2                                                   ; restore true data size

  ; If the very first Char after the first CR/LF is '@', we load the Clip 'As is':
    mov ebx esi | add ebx 2
    While B$ebx <= ' ' | inc ebx | End_While

    .If B$ebx = '@'
        call Clip_Disable_Customization_Controls
        call SearchTheClip | On D$TheClipLenght = 0, ret
        mov esi D$ClipPointer, ecx D$TheClipLenght
        While B$esi <> '@'
            inc esi | dec ecx | jz L8>
        End_While | inc esi | dec ecx | jz L8>
        rep movsb

    .Else_If B$WithData = &TRUE
        If B$GlobalScope = &TRUE
            call WithDataGlobalScope
        Else
            call WithDataLocalScope
        End_If
        call Clip_Enable_Customization_Controls
    .Else
        If B$GlobalScope = &TRUE
            call WithoutDataGlobalScope
        Else
            call WithoutDataLocalScope
        End_If
        call Clip_Enable_Customization_Controls
    .End_If

    mov eax 0 | stosd

    call 'USER32.SendMessageA' D$TemplateList3  &WM_SETTEXT 0 D$TempoClipmemoryPtr
L8:
ret


Clip_Disable_Customization_Controls:
   call 'USER32.EnableWindow' D$ControlID108 &FALSE
   call 'USER32.EnableWindow' D$ControlID104 &FALSE
   call 'USER32.EnableWindow' D$ControlID105 &FALSE
   call 'USER32.EnableWindow' D$ControlID106 &FALSE
   call 'USER32.EnableWindow' D$ControlID107 &FALSE
ret

Clip_Enable_Customization_Controls:
   call 'USER32.EnableWindow' D$ControlID108 &TRUE
   call 'USER32.EnableWindow' D$ControlID104 &TRUE
   call 'USER32.EnableWindow' D$ControlID105 &TRUE
   call 'USER32.EnableWindow' D$ControlID106 &TRUE
   call 'USER32.EnableWindow' D$ControlID107 &TRUE
ret

____________________________________________________________________________________________


[TemplateList1: ?    TemplateList2: ?    TemplateList3: ?]

[TPointer: ?    RadioButtonID: ?    ClipperHandle: ?]

[DEL_TEMPLATE 5    AD_TEMPLATE 6    DEL_GROUP 7   AD_GROUP 8   DEL_GROUP_TEMPLATES 4
 CLIP_MANAGER 25]

[ClipDialogIsExtended: ?]

ExtendClipDialog:
    call 'USER32.GetWindowPlacement' D$ClipperHandle Control

    If B$ClipDialogIsExtended = &FALSE
        add D$Control.rcNormalPosition.bottom 66
    Else
        sub D$Control.rcNormalPosition.bottom 66
    End_If

    call 'USER32.SetWindowPlacement' D$ClipperHandle Control

    xor B$ClipDialogIsExtended &TRUE
ret


Proc TemplateChoice:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
         ..If D@wParam = &IDCANCEL
            mov D$ClipperHandle 0
            call 'User32.EndDialog' D@Adressee 0

         ..Else_If D@wParam = &IDOK
             call RetrieveGenericName
             call SearchTheClip
             call TemplateToClipBoard
             mov D$ClipperHandle 0
             call 'User32.EndDialog' D@Adressee 0

         ..Else_If D@wParam = &IDHELP
             call Help, B_U_AsmName TempateHelp, ContextHlpMessage

         ..Else_If D@wParam = CLIP_MANAGER
            call ExtendClipDialog

         ..Else_If D@wParam = AD_GROUP
            call AddGroup

         ..Else_If D@wParam = DEL_GROUP
            call DeleteGroup

         ..Else_If D@wParam = DEL_GROUP_TEMPLATES
            call DeleteGroupAndTemplates

         ..Else_If D@wParam = AD_TEMPLATE
            call AddTemplate

         ..Else_If D@wParam = DEL_TEMPLATE
            call DeleteTemplate

         ..Else_If W@wParam = 101                     ; 101 > First ListBox
             shr D@wParam 16
             .If D@wParam = &LBN_SELCHANGE
                call ViewTemplatesItems
             .End_If

         ..Else_If W@wParam = 102                     ; 102 > Second ListBox
             shr D@wParam 16
             .If D@wParam = &LBN_SELCHANGE
                 call ShowClip
             .End_If

         ..Else_If W@wParam = 108                   ; Generic Name Edit Control
            call ShowClip
         ..Else_If W@wParam >= 104                   ; 104/105/106/107 > Radio Buttons
             move D$RadioButtonID D@wParam
             and D$RadioButtonID 0FFFF
             .If W@wParam <= 107
                 shr D@wParam 16
                 If D@wParam = &BN_CLICKED
                     call CheckTemplateRadioButtons
                     call ShowClip
                 End_If
             .End_If
         ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$ClipperHandle D@Adressee
        mov B$ClipDialogIsExtended &FALSE
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.GetDlgItem' D@Adressee 101 | mov D$TemplateList1 eax
        call 'USER32.GetDlgItem' D@Adressee 102 | mov D$TemplateList2 eax
        call 'USER32.GetDlgItem' D@Adressee 110 | mov D$TemplateList3 eax
        call 'USER32.GetDlgItem' D@Adressee 108 | mov D$ControlID108 eax
        call 'USER32.GetDlgItem' D@Adressee 104 | mov D$ControlID104 eax
        call 'USER32.GetDlgItem' D@Adressee 105 | mov D$ControlID105 eax
        call 'USER32.GetDlgItem' D@Adressee 106 | mov D$ControlID106 eax
        call 'USER32.GetDlgItem' D@Adressee 107 | mov D$ControlID107 eax
        call ReadClipsections
        call InitTemplateRadioButtons D@Adressee
        call 'USER32.SendMessageA' D$TemplateList1  &LB_SETCURSEL 0 0
        call ViewTemplatesItems
       ; call 'USER32.SendDlgItemMessageA' D@Adressee, 110, &WM_SETFONT, D$Font1Handle, &FALSE

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        jmp L1>

   ; ...Else_If D@Message = &WM_CTLCOLORBTN ; Never received for Radio/Check Buttons !!!???
   ;     jmp L1>

    ...Else_If D@Message = &WM_CTLCOLORLISTBOX
L1:     call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>>

    ...Else
       ; mov eax D$BlockEndTextPtr | sub eax D$BlockStartTextPtr
       ; mov ebx D$ClipMemoryEnd | sub ebx D$ClipFileMemoryPointer
       ; mov ecx D$BlockInside | On eax >= ebx, mov ecx &FALSE
       ; EnableControl D@Adressee, AD_TEMPLATE, ecx

        popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: Endp


ViewTemplatesItems:
    call 'USER32.SendMessageA' D$TemplateList2, &LB_RESETCONTENT, 0, 0
    call 'USER32.SendMessageA' D$TemplateList3, &WM_SETTEXT, 0, 0

    call 'USER32.SendMessageA' D$TemplateList1  &LB_GETCURSEL 0 0
    call SearchSection | On D$SectionPointer <> 0, call ReadClipTitles
ret

____________________________________________________________________________________________

[GetNewGroupNameHandle: ?]

AddGroup: ; 'LoadClipFile'
    call SaveClipSelections

    On D$GetNewGroupNameHandle <> 0,
        call 'User32.EndDialog' D$GetNewGroupNameHandle, 0
    call 'USER32.DialogBoxParamA' D$hInstance 20001, &NULL, GetNewGroupName, &NULL
    On D$NewGroupName = 0, ret

    ...If B$GroupInsert = &TRUE
      ; Go to the selected Group:
        mov esi D$ClipFileMemoryPointer, edx D$ClipGroupIndex | inc edx

        While esi < D$ClipMemoryEnd
            lodsb
            ..If al = '/'
                .If B$esi = '/'
                    If B$esi-2 < ' '
                        dec edx | jz L1>
                    End_If
                .End_If
            ..End_If
        End_While

L1:     dec esi

        push esi
          ; Make room: ('D$NewGroupNameLen'+6) Bytes:
            mov ecx D$ClipFileMemoryPointer | add ecx D$ClipFileSize
            mov edi ecx     ; edi > End.
            sub ecx esi     ; ecx = How many Bytes to be moved downward.
            mov esi edi | add edi D$NewGroupNameLen | add edi 8 ; 6 = 3 CRLF + '//'
          ; esi > End // edi > (End+room). Copy BackWard:
            inc ecx | std | rep movsb | cld
        pop edi

        add edi 2 ; (old '//' yet there).
        mov esi NewGroupName, ecx D$NewGroupNameLen | rep movsb
        mov D$edi CRLF2, W$edi+4 CRLF

        mov eax D$NewGroupNameLen | add eax 8 | add D$ClipFileSize eax

    ...Else ; B$GroupInsert = &FALSE >>> Append:
        mov edi D$ClipFileMemoryPointer | add edi D$ClipFileSize
        While B$edi < ' ' | dec edi | dec D$ClipFileSize | End_While
        inc edi | inc D$ClipFileSize
        mov D$edi CRLF2, W$edi+4 CRLF, W$edi+6 '//' | add edi 8
        mov esi NewGroupName
        While B$esi <> 0 | movsb | inc D$ClipFileSize | End_While
        mov D$edi CRLF2 | add edi 4
        add D$ClipFileSize 12 ; (5 CRLF + '//')

    ...End_If

    call ClipUpdate
ret


[GroupInsert: ?]

Proc GetNewGroupName:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

        .If D@Message = &WM_COMMAND
            If D@wParam = &IDOK         ; [Append]
                mov D$GroupInsert &FALSE
L1:             call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_GETTEXTLENGTH, 0, 0
                mov D$NewGroupNameLen eax
                call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_GETTEXT, 38,
                                                  NewGroupName
                call 'USER32.EndDialog' D@Adressee, 0 | popad | mov eax &TRUE | ExitP

            Else_If D@wParam = 20       ; [Insert]
                mov D$GroupInsert &TRUE | jmp L1<

            Else_If D@wParam = &IDCANCEL
                mov D$GetNewGroupNameHandle 0
                call 'USER32.EndDialog' D@Adressee, 0 | popad | mov eax &TRUE | ExitP

            End_If

        .Else_If D@Message = &WM_INITDIALOG
            mov B$WithCustomisation &FALSE
            move D$GetNewGroupNameHandle D@Adressee
            mov edi NewGroupName, eax 0, ecx 10 | rep stosd

            call 'USER32.SendDlgItemMessageA' D@Adressee, 21,  &BM_SETCHECK, &TRUE, 0
            call 'USER32.GetDlgItem', D@Adressee, 10
            call 'USER32.SetFocus' eax

        .End_If

    popad | mov eax &FALSE
EndP


; Deleting a Group name only.

[SureDelGroupName: 'Delete "                                ', 0]

DeleteGroup:
    call SaveClipSelections

    mov esi D$ClipFileMemoryPointer, ecx D$ClipGroupIndex | inc ecx

L0: inc esi | cmp D$esi 02F2F0A0D | jne L0<     ; "CR LF //" = 02F2F0A0D
    dec ecx | jnz L0<

    push esi
        add esi 4 | lea edi D$SureDelGroupName+8
        While B$esi > CR | movsb | End_While
        mov D$edi '" ? ', B$edi+3 0
        call 'USER32.MessageBoxA' D$hwnd, SureDelGroupName, SureDelTemplateTitle,
                                  &MB_SYSTEMMODAL__&MB_YESNO__&MB_ICONEXCLAMATION
    pop esi

    .If eax = &IDYES
        mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
        add esi 2 | mov edi esi | add esi 2 | sub D$ClipFileSize 2
      ; edi > '//'. esi > Name. Skip over Line:
        While B$esi > CR | inc esi | dec D$ClipFileSize | End_While
      ; Skip over coming CRLF:
        While W$esi = CRLF | add esi 2 | sub D$ClipFileSize 2 | End_While
        While esi < edx | movsb | End_While

        call ClipUpdate
    .End_If
ret


DeleteGroupAndTemplates:
    call SaveClipSelections

    mov esi D$ClipFileMemoryPointer, ecx D$ClipGroupIndex | inc ecx

L0: inc esi | cmp D$esi 02F2F0A0D | jne L0<     ; "CR LF //" = 02F2F0A0D
    dec ecx | jnz L0<

    push esi
        add esi 4 | lea edi D$SureDelGroupName+8
        While B$esi > CR | movsb | End_While
        mov D$edi '" ? ', B$edi+3 0
        call 'USER32.MessageBoxA' D$hwnd, SureDelGroupName, SureDelTemplateTitle,
                                  &MB_SYSTEMMODAL__&MB_YESNO__&MB_ICONHAND
    pop esi

    .If eax = &IDYES
        mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
        mov edi esi | add esi 4 | sub D$ClipFileSize 4
        While D$esi <> 02F2F0A0D
            inc esi | dec D$ClipFileSize
            On esi >= edx, jmp L2>
        End_While

        While esi < edx | movsb | End_While

L2:     call ClipUpdate
    .End_If
ret


[GetNewTemplateNameHandle: ?]

[NoClipSelection: B$ 'No selection found in the Source Editor  ', 0
 SelectionTooBig: B$ 'The Selection is too big  ', 0]

AddTemplate: ; 'LoadClipFile'
    mov eax D$BlockEndTextPtr | sub eax D$BlockStartTextPtr
    mov ebx D$ClipMemoryEnd | sub ebx D$ClipFileMemoryPointer

    If D$BlockInside = &FALSE
        call 'USER32.MessageBoxA' D$hwnd, NoClipSelection, Argh, &MB_SYSTEMMODAL | ret
    Else_If eax >= ebx
        call 'USER32.MessageBoxA' D$hwnd, SelectionTooBig, Argh, &MB_SYSTEMMODAL | ret
    End_If

    call SaveClipSelections

    On D$GetNewTemplateNameHandle <> 0,
        call 'User32.EndDialog' D$GetNewTemplateNameHandle, 0
    call 'USER32.DialogBoxParamA' D$hInstance 20000, &NULL, GetNewTemplateName, &NULL

    On D$NewTemplateName = 0, ret

    call SearchTheClip

    mov eax D$ClipFileSize | add eax CLIPRESERVATION
    VirtualAlloc TempoClipFileMemoryPointer, eax

    If D$TheClipLenght = 0
      ; No Item yet, or no Item selected >>> paste at end:
        mov ebx D$SectionPointer, edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
L0:     inc ebx | cmp ebx edx | jae L1>
        cmp W$ebx '//' | jne L0<
L1:         mov D$ClipPointer ebx

        mov esi D$ClipFileMemoryPointer, edi D$TempoClipFileMemoryPointer
        While esi < D$ClipPointer | movsb | End_While

    Else
      ; Item selected >>> Insert:
        mov esi D$ClipFileMemoryPointer, edi D$TempoClipFileMemoryPointer
        While esi < D$ClipPointer | movsb | End_While

L0:     dec esi | dec edi | cmp B$esi '/' | jne L0<
        If B$esi-1 = '/'
            dec esi | dec edi
        End_If
        cmp W$esi-2 CRLF | jne L0<

    End_If

    mov B$edi '/' | inc edi | inc D$ClipFileSize
    push esi
        mov esi NewTemplateName
        While B$esi <> 0 | movsb | inc D$ClipFileSize | End_While
    pop esi
    mov D$edi CRLF2 | add edi 4 | add D$ClipFileSize 4

    If B$WithCustomisation = &FALSE
        mov B$edi '@', W$edi+1 CRLF | add edi 3 | add D$ClipFileSize 3
    End_If

    mov W$edi CRLF | add edi 2 | add D$ClipFileSize 2

    push esi
        mov esi D$BlockStartTextPtr
        While esi < D$BlockEndTextPtr
            movsb | inc D$ClipFileSize
        End_While | movsb | inc D$ClipFileSize
        mov D$edi CRLF2, W$edi+4 CRLF | add edi 6 | add D$ClipFileSize 6
    pop esi

    mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
    While esi < edx | movsb | End_While
    Exchange D$ClipFileMemoryPointer D$TempoClipFileMemoryPointer
    VirtualFree D$TempoClipFileMemoryPointer
    call ClipUpdate
ret


SaveClipSelections:
    call 'USER32.SendMessageA' D$TemplateList1  &LB_GETCURSEL 0 0 | mov D$ClipGroupIndex eax
    call 'USER32.SendMessageA' D$TemplateList2  &LB_GETCURSEL 0 0 | mov D$ClipIndex eax
ret

ClipUpdate:
    call ClipCRLFs | call WriteClipFile | call ReleaseClipFile
    call LoadClipFile | call ReadClipsections



    call 'USER32.SendMessageA' D$TemplateList2, &LB_SETCURSEL, D$ClipIndex, 0
    If eax = &LB_ERR
        call 'USER32.SendMessageA' D$TemplateList2, &LB_SETCURSEL, 0-1, 0
    End_If

    call 'USER32.SendMessageA' D$TemplateList1, &LB_SETCURSEL, D$ClipGroupIndex, 0
    If eax = &LB_ERR
        call 'USER32.SendMessageA' D$TemplateList1, &LB_SETCURSEL, 0, 0
    End_If

    call ViewTemplatesItems
ret
____________________________________________________________________________________________

[UnvalidClipTitle: 'Unvalid Clip File', 0
 ClipTitleWanted: "A Clip File must contain some text before the first //Group", 0]

;;
  Ensure that all '/Name', '//Name' are preceeded and ended by 3 CRLF and than the
  whole thing is ended by 2 CRLF.
;;

ClipCRLFs:
    mov esi D$ClipFileMemoryPointer
    While W$esi = CRLF | add esi 2 | End_While
    If B$esi = '/'
        mov B$ClipFileReady 0-1 | ret
    End_If

    mov eax D$ClipFileSize | add eax CLIPRESERVATION
    VirtualAlloc TempoClipFileMemoryPointer, eax

    mov eax D$TempoClipFileMemoryPointer
    add eax D$ClipFileSize | add eax CLIPRESERVATION | mov D$ClipMemoryEnd eax

    mov esi D$ClipFileMemoryPointer, edi D$TempoClipFileMemoryPointer
    mov edx esi | add edx D$ClipFileSize

L0: .While esi < edx
        lodsb

        .If al = '/'
            If W$esi-3 = CRLF
              ; Skip the CRLF Back:
                While W$edi-2 = CRLF | sub edi 2 | End_While
              ; Re-Write the wanted 3 CRLFs:
                mov D$edi CRLF2, W$edi+4 CRLF | add edi 6
              ; Write the '/Name', '//Name':
                dec esi | While B$esi <> CR | movsb | End_While
              ; Write the wanted 3 CRLFs:
                mov D$edi CRLF2, W$edi+4 CRLF | add edi 6
              ; Arase the existing source CRLF:
                While W$esi = CRLF | add esi 2 | End_While | jmp L0<
            End_If
        .End_If

        stosb
    .End_While

  ; Arase existing CRLF at End:
    While W$edi-2 = CRLF | sub edi 2 | End_While
  ; And rewrite the 2 wanted ones:
    mov D$edi CRLF2 | add edi 4

    sub edi D$TempoClipFileMemoryPointer | mov D$ClipFileSize edi
    Exchange D$ClipFileMemoryPointer D$TempoClipFileMemoryPointer

    mov eax D$ClipMemoryEnd | sub eax D$ClipFileMemoryPointer

    VirtualFree D$TempoClipFileMemoryPointer
ret


[NewTemplateName: NewGroupName: ? #10] [WithCustomisation: ?    NewGroupNameLen: ?]

Proc GetNewTemplateName:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

        .If D@Message = &WM_COMMAND
            If D@wParam = &IDOK
                call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_GETTEXTLENGTH, 0, 0
                inc eax | On eax > 38, mov eax 38
                call 'USER32.SendDlgItemMessageA' D@Adressee, 10, &WM_GETTEXT, eax,
                                                   NewTemplateName

                call 'User32.EndDialog' D@Adressee 0 | popad | mov eax &TRUE | ExitP

            Else_If D@wParam = &IDCANCEL
                mov D$GetNewTemplateNameHandle 0
                call 'User32.EndDialog' D@Adressee 0 | popad | mov eax &TRUE | ExitP

            Else_If D@wParam = 20
                xor B$WithCustomisation &TRUE

            End_If

        .Else_If D@Message = &WM_INITDIALOG
            mov B$WithCustomisation &FALSE
            move D$GetNewTemplateNameHandle D@Adressee
            mov edi NewTemplateName, eax 0, ecx 10 | rep stosd

        .End_If

    popad | mov eax &FALSE
EndP


[NoClipSelected: 'Select a Template in the second List Box, if you want to delete it', 0
 SureDelTemplateTitle: 'Sure?', 0
 SureDelTemplate: 'Delete "                                ']

DeleteTemplate:
    call SaveClipSelections

    call SearchTheClip

    .If D$TheClipLenght = 0
      ; No Item yet, or no Item selected:
        call 'USER32.MessageBoxA' D$hwnd, NoClipSelected, Argh, &MB_SYSTEMMODAL

    .Else
      ; Item selected >>> Delete:
        mov esi D$ClipPointer, edi esi

      ; Go to start of Template to be deleted:
        While B$edi <> '/' | dec edi | End_While | mov D$ClipPointer edi

        push esi, edi
            lea edi D$SureDelTemplate+8 | mov esi D$ClipPointer | inc esi
            While B$esi <> CR | movsb | End_While | mov D$edi '"  ?', B$edi+4 0

            call 'USER32.MessageBoxA' D$hwnd, SureDelTemplate, SureDelTemplateTitle,
                                      &MB_SYSTEMMODAL__&MB_YESNO__&MB_ICONEXCLAMATION
        pop edi, esi

        .If eax = &IDYES
            mov edx D$ClipFileMemoryPointer | add edx D$ClipFileSize
            mov esi D$ClipPointer | inc esi | dec D$ClipFileSize
            While B$esi <> '/'
L0:             inc esi | dec D$ClipFileSize | On esi >= edx, jmp L1>
            End_While
            cmp B$esi-1 LF | jne L0<
            mov ecx D$ClipMemoryEnd | sub ecx esi | rep movsb

L1:         call ClipUpdate
        .End_If

    .End_If
ret

[TempoClipFileMemoryPointer: ?]

____________________________________________________________________________________________

Proc InitTemplateRadioButtons:
    Argument @Adressee

    mov eax 105 | xor al B$WithData
    call 'USER32.GetDlgItem' D@Adressee eax
    call 'USER32.SendMessageA' eax &BM_SETCHECK eax &TRUE
    mov eax 107 | xor al B$GlobalScope
    call 'USER32.GetDlgItem' D@Adressee eax
    call 'USER32.SendMessageA' eax &BM_SETCHECK eax &TRUE
EndP


[WithData: B$ &TRUE    GlobalScope: &TRUE]

CheckTemplateRadioButtons:
    call 'USER32.GetDlgItem' D$ClipperHandle D$RadioButtonID
    push eax
        call 'USER32.SendMessageA' eax &BM_GETCHECK 0 0 | xor eax &TRUE
    pop ebx
    call 'USER32.SendMessageA' ebx &BM_SETCHECK eax 0
    xor D$RadioButtonID 1                                    ; 104 <> 105 // 106 <> 107
    call 'USER32.GetDlgItem' D$ClipperHandle D$RadioButtonID
    push eax
        call 'USER32.SendMessageA' eax &BM_GETCHECK 0 0 | xor eax &TRUE
    pop ebx
    call 'USER32.SendMessageA' ebx &BM_SETCHECK eax 0
    If D$RadioButtonID < 106
        xor B$WithData &TRUE
    Else
        xor B$GlobalScope &TRUE
    End_If
ret


TemplateToClipBoard:
    On D$TheClipLenght = 0, ret
    call 'USER32.OpenClipboard' D$hwnd | cmp eax 0 | je L9>>
    call 'USER32.EmptyClipboard' | cmp eax 0 | je L8>>

    mov ecx D$TheClipLenght | shl ecx 2               ; *4 > room for Generic Names
    push ecx
        call 'KERNEL32.GlobalAlloc' &GMEM_DDESHARE ecx | cmp eax 0 | jne L1>  ; > eax = handle
        pop eax | jmp L8>>
L1:     mov D$hBlock eax
        call 'KERNEL32.GlobalLock' eax                                       ; > eax = adress
    pop ecx
    shr ecx 2                                                   ; restore true data size
    mov edi eax, esi D$ClipPointer

  ; If the very first Char after the first CR/LF is '@', we load the Clip 'As is':
    mov ebx esi | add ebx 2
    While B$ebx <= ' ' | inc ebx | End_While

    .If B$ebx = '@'
        call SearchTheClip | On D$TheClipLenght = 0, ret
        mov esi D$ClipPointer, ecx D$TheClipLenght
        While B$esi <> '@'
            inc esi | dec ecx | jz L8>
        End_While | inc esi | dec ecx | jz L8>
        rep movsb

    .Else_If B$WithData = &TRUE
        If B$GlobalScope = &TRUE
            call WithDataGlobalScope
        Else
            call WithDataLocalScope
        End_If
    .Else
        If B$GlobalScope = &TRUE
            call WithoutDataGlobalScope
        Else
            call WithoutDataLocalScope
        End_If
    .End_If

    mov eax 0 | stosd

    call 'KERNEL32.GlobalUnlock' D$hBlock
    call 'USER32.SetClipboardData' &CF_TEXT  D$hBlock
    call 'USER32.SendMessageA' D$TemplateList3  &WM_SETTEXT 0 D$hBlock
L8: call 'USER32.CloseClipboard'
L9: ret


[GenericNameHandle: ?]

RetrieveGenericName:
    mov edi GenericName, ecx 10, eax 0 | rep stosd
    call 'USER32.GetDlgItem' D$ClipperHandle 108  ; 108 = ID of Generic Name Edit Control.
    mov D$GenericNameHandle eax
    call 'USER32.SendMessageA' D$GenericNameHandle &EM_LINELENGTH 0 0
    If eax > 0
        mov W$GenericName ax
        call 'USER32.SendMessageA' D$GenericNameHandle, &EM_GETLINE, 0, GenericName
        mov B$GenericName+eax 0  ; >>> 'EM_GETLINE_Comment'
    End_If
ret


WithDataGlobalScope:
L0: lodsb
    .If al = '@'
        If B$GenericName > 0
            push esi
                mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else
        stosb
    .End_If
    loop L0<
ret


WithDataLocalScope:
L0: lodsb
    .If al = '@'
        stosb
        If B$GenericName > 0
            push esi
                mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else_If al = '['
        If B$esi = '@'
            mov eax 'Loca' | stosd | mov ax 'l ' | stosw
            call CopyTemplateLabelsOnly
        Else
            stosb
        End_If
    .Else_If al = '$'
      ; Strip '$' in 'D$@Value', but not in D$Value.
        On B$esi <> '@', stosb
    .Else_If al = '$'
      ; Strip '$' in 'D$@'Value', but not in D$Value.
        On B$esi <> '@', stosb
    .Else
        stosb
    .End_If
    loop L0<
ret


CopyTemplateLabelsOnly:
L0:
    lodsb
        .If al = '@'
            stosb
            If B$GenericName > 0
                push esi
                    mov esi GenericName | lodsb
L1:                 stosb | lodsb | cmp al 0 | ja L1<
                pop esi
            End_If
L1:         lodsb | dec ecx | stosb
            If B$esi = ' '
                dec ecx | jmp L2>
            Else_If B$esi <> ':'
                jmp L1<
            End_If

L2:         mov ax ', ' | stosw
        .Else_If al = ']'
            sub edi 2 | mov D$edi 0 | ret       ; strip ending ' ,'
        .End_If
    Loop L0<


WithoutDataGlobalScope:
L0: lodsb
    .If al = '['
        On B$esi <> '@', jmp L2>
        While al <> ']'
            lodsb | dec ecx         ; Strip Data if unwished
        End_While
    .Else_If al = '@'
        If B$GenericName > 0
            push esi
                mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else
L2:     stosb
    .End_If
    loop L0<
ret


WithoutDataLocalScope:
L0: lodsb
    .If al = '['
        On B$esi <> '@', jmp L2>
        While al <> ']'
            lodsb | dec ecx         ; Strip Data if unwished
        End_While
    .Else_If al = '@'
        stosb
        If B$GenericName > 0
            push esi
                mov esi GenericName | lodsb
L1:             stosb | lodsb | cmp al 0 | ja L1<
            pop esi
        End_If
    .Else_If al = '$'
      ; Strip '$' in 'D$@'
        On B$esi <> '@', stosb
    .Else_If al = '$'
      ; Strip '$' in 'D$@'
        On B$esi <> '@', stosb
    .Else
L2:     stosb
    .End_If
    loop L0<
ret







