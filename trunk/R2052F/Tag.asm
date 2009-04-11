TITLE Tag


;;
  Tag Feature:
  
  When a commented out 'Tag' KeyWord is found out (Right-Click feature), these Parsers
  are run and branced to the according associated action.
  
  First implementation, for test, if:
_________________

; Tag Dialog 1000
_________________

  'WizardTag' 'NewWizardForm'

  is written in some Source, when Right-Clicking on 'Tag', and if the Edited PE Resources
  contain a Dialog with an ID 1000 (Decimal only), the Dialog Editor should be run.
;;

____________________________________________________________________________________________
____________________________________________________________________________________________

; General purpose Routines: 'IsItTag', 'TagParser', 'NoSuchTag', 'TagDecimalToBin', 'NewWizardForm', 'WizardFormTag'

; When called from 'RightClick' > 'InternalRightClick', esi+1 point to 'Tag '

IsItTag:
    push esi
  ; Is it a Comment?
L0:     If B$esi = ';'
            ; OK
        Else_If B$esi = ' '
            dec esi | jmp L0<
        Else
            pop esi | mov eax &FALSE | ret
        End_If

    pop esi
    mov eax &TRUE
ret
____________________________________________________________________________________________

TagParser:
  ; Check for the Tag KeyWords:
  ; 'Dialog' or 'Wizard'

  ; esi > 'tag '
    add esi 4 | While B$esi = ' ' | inc esi | End_While
    mov eax D$esi | or eax 020202020
  ; "Tag Dialog DialogID":
    ...If eax = 'dial'
        mov ax W$esi+4 | or ax 02020
        .If ax = 'og'
            If B$esi+6 = ' '
                mov B$ReadyToRun &FALSE
                add esi 7 | jmp DialogTag
            End_If
        .End_If
  ; "Tag Wizard WizardName 'Path...........' ":
    ...Else_If eax = 'wiza'
        mov ax W$esi+4 | or ax 02020
        .If ax = 'rd'
            If B$esi+6 = ' '
                mov B$ReadyToRun &FALSE
                add esi 6 | jmp WizardTag
            End_If
        .End_If
  ; "Tag Unicode LabelName":
    ...Else_If eax = 'unic'
        mov eax D$esi+3 | or eax 020202020
        .If eax = 'code'
            If B$esi+7 = ' '
                mov B$ReadyToRun &FALSE
                add esi 8 | jmp UnicodeTag
            End_If
        .End_If
  ; "Tag Menu MenuID":
    ...Else_If eax = 'menu'
        If B$esi+4 = ' '
            mov B$ReadyToRun &FALSE
            add esi 5 | jmp MenuTag
        End_If

    ...End_If

    call 'USER32.MessageBoxA' D$hwnd, {'Unknown Tag KeyWord', 0}, {'No such Tag', 0}, 0
ret


TagDecimalToBin:
    mov ecx 0, eax 0

L0: lodsb
    cmp al ' ' | jbe L9>
    cmp al '9' | ja L8>
    cmp al '0' | jb L8>
        sub al '0'                  ; convert Decimal to binary:
        lea ecx D$ecx+ecx*4         ;     ecx = ecx * 5
        lea ecx D$eax+ecx*2         ;     ecx = eax + old ecx * 10
    jmp L0<

L8: call 'USER32.MessageBoxA' D$hwnd, {"Tags' Numbers are to be provided in Decimal notation", 0},
                              {'Bad Number', 0}, 0
    mov eax &FALSE | ret

L9: mov eax &TRUE | ret

____________________________________________________________________________________________

; "Tag Dialog" Routines:

; "; Tag Dialog " found // esi on next possible ID

DialogTag:
    While B$esi = ' ' | inc esi | End_While

    call TagDecimaltoBin

    .If eax = &TRUE
        mov eax ecx

        If eax = 0
            ; nope
        Else_If eax > 0_FFFF
            ; nope
        Else
            call GetTagedDialog
        End_If
    .End_If
ret

____________________________________________________________________________________________

; Simplified version of the 'LoadFromResources' executed by Menu Selection:

GetTagedDialog:
    If B$OnDialogEdition = &TRUE
        Beep | ret  ; prevents from multi-runs
    End_If

    push eax
        call ReleaseDialogMemories | call InitDialogMemory
    pop eax

    mov esi DialogList, D$MenuListPtr esi      ; 1 record:  ID / Ptr / Size

L0: .If D$esi = eax
        mov D$WhatDialogListPtr esi | add D$WhatDialogListPtr 4  ;DialogList+4

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

        mov B$DialogLoadedFromResources &TRUE
        call FromBinToTextTemplate
        call ReInitDialogEdition

    .Else_If D$esi = 0
        call 'USER32.MessageBoxA' D$hwnd, {'No Dialog found with matching ID Number', 0},
                                  {'Bad ID', 0}, 0
    .Else
        add esi 12 | jmp L0<<
    .End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
[WizardCommandLine: ?  #512]
[ApplicationPath:   ? #&MAX_PATH]
[NewFormPath:       ? #&MAX_PATH]
[ProducedCode_BeginWrite: ?    ProducedCode_Written: ?
 ProducedCodeHandle:      ?    ProducedCode:         ?
 WizardSearchHandle:      ?]

[WizardName: 'WZRD010Form.exe', 0]

[WizardSaving:
 @dwFileAttributes: D$ 0
 @ftCreationTime.dwLowDateTime: D$ 0
 @ftCreationTime.dwHighDateTime: D$ 0
 @ftLastAccessTime.dwLowDateTime: D$ 0
 @ftLastAccessTime.dwHighDateTime: D$ 0
 @ftLastWriteTime.dwLowDateTime: D$ 0
 @ftLastWriteTime.dwHighDateTime: D$ 0
 @nFileSizeHigh: D$ 0
 @nFileSizeLow: D$ 0
 @dwReserved0: D$ 0
 @dwReserved1: D$ 0]
[@cFileName: B$ 0 #&MAX_PATH]
[@cAlternate: B$ 0 #14]

; tag example :
; >  ; Tag Wizard Form "C\MyPath\MyFolder\MyFile.wwf" -R

; esi points on the space after "; Tag wizard"
WizardTag:

    If D$DebugDialogHandle <> 0
          push esi | call KillDebugger | pop esi | On eax = &IDNO, ret
    End_If

    ;push D$CurrentWritingPos
    mov D$CurrentWritingPos esi
    call SetPartialEditionFromPos
    mov esi D$CurrentWritingPos

    ; selects the wizard
    mov eax D$esi+1
    or eax 020202020
    If eax = 'form'

        call WizardFormTag
    ;  ; Temporary add, because of the new 'ControlZ', that does not work on the Full Source:
    ;    call ReInitUndo
    Else_If eax = 'test'
        ;jmp WizardFormTest
    End_If

    call RestoreRealSource

    call ReInitUndoOnly
ret
____________________________________________________________________________________________
WizardFormTest:
    call CheckTagEndPos | On eax = &FALSE, ret
    call DeleteLastWizardCode esi
ret
____________________________________________________________________________________________
WizardFormTag:

    call CheckTagEndPos | On eax = &FALSE, ret

    ; retrieves Wizard Path :
    push esi
        call GetRosAsmFilesPath
        mov esi RosAsmFilesPath | mov edi WizardPath
        While B$esi <> 0 | movsb | End_While
        mov esi WizardName
        While B$esi <> 0 | movsb | End_While | mov B$edi 0
    pop esi

    ; set esi on the character after the first double quote following 'Tag Wizard Form'
    While B$esi <> '"' | inc esi | End_While | inc esi

    ; => command line writing:
    mov edi WizardCommandLine

    ;    * Wizard FileName
    mov B$edi '"' | inc edi
    push esi
        mov esi WizardPath
        While B$esi <> 0 | movsb | End_While
    pop esi
    mov B$edi '"' | inc edi

    ;    * File location
    mov D$edi '-f "'  | add edi 4
    While B$esi <> '"' | movsb | End_While
    mov W$edi '" ' | add edi 2

    ;    * RosAsm ID
    mov W$edi '-R'  | add edi 2 | mov B$edi 0 | inc edi

    call LoadWizardAndPasteCode esi

ret
____________________________________________________________________________________________
Proc DeleteLastWizardCode:
    Argument @TagBegin
    Uses edx

    mov esi D@TagBegin
    While B$esi <> LF | dec esi | End_While | inc esi | mov edx esi
    While esi <= D$SourceEnd
        .If W$esi = ((59 shl 8) or LF)
            mov eax D$esi+3 | or eax 0202020
            If eax = 'tag '
                mov eax D$esi+6 | or eax 020202000
                On eax = ' end', jmp L1>
                ExitP
            End_If
        .End_If
        inc esi
    End_While
    ; should not be accessed ( checked before)
    ExitP

L1: While B$esi <> CR | inc esi | End_While | dec esi | mov D$BlockEndTextPtr esi
    mov D$BlockStartTextPtr edx
    mov B$BlockInside &TRUE
    mov D$CaretRow 1              ; the caret is set at the beginning of the block
    mov D$PhysicalCaretRow 1      ;
    mov eax D$CaretRow, ebx D$CaretLine | call SearchTxtPtr
    mov D$CurrentWritingPos eax
    call ControlD

    ;ControlZ

EndP
____________________________________________________________________________________________
Proc CheckTagEndPos:
    Uses esi

    While B$esi <> LF | dec esi | End_While | inc esi | mov edx esi
    While esi <= D$SourceEnd
        .If W$esi = ((59 shl 8) or LF)
            mov eax D$esi+3 | or eax 0202020
            If eax = 'tag '
                mov eax D$esi+6 | or eax 020202000
                On eax = ' end', jmp L1>
                jmp L2>
            End_If
        .End_If
        inc esi
    End_While

L2: call 'USER32.MessageBoxA' D$hwnd,
{"Can't find '; Tag End' mark.
Write it back and try again ;o)", 0},  {"Error", 0}, &MB_ICONERROR
    mov eax &FALSE
    ExitP

L1: mov eax &TRUE

EndP
____________________________________________________________________________________________
Proc AddWizardCode:
    Argument @WizardCodePtr @WizardCodeLen
    Uses D$ClipBoardLen

    move D$ClipBoardLen D@WizardCodeLen

    call ReMapSourceMemoryIfNeeded D$ClipBoardLen | On eax = &IDNO, ret

    call DoStoreBlockPaste

    mov esi D$SourceEnd | add esi 400                       ; make room inside our text:
    mov edi esi | add edi D$ClipBoardLen
    mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx
    std | rep movsb | cld | inc esi
                                                            ; write from clipboard:
    mov edi esi, esi D@WizardCodePtr, ecx D$ClipBoardLen
    pushad | rep movsb | popad
                                                            ; position:
    mov esi edi, ebx D$CaretLine
L0: lodsb | inc D$CaretRow | cmp al CR | jne L1>
        inc ebx | mov D$CaretRow 1 | lodsb | dec ecx | jz L0>
L1: loop L0<

L0: cmp ebx D$LineNumber | jna L6>
        mov esi D$UpperLine | mov ecx ebx | sub ecx D$CaretLine
L1:     lodsb | cmp al LF | jne L1<
            mov D$UpperLine esi | dec ebx | jmp L0<

L6: mov D$CaretLine ebx

    mov eax D$ClipBoardLen
    add D$SourceLen eax | add D$SourceEnd eax | add D$CurrentWritingPos eax


EndP
____________________________________________________________________________________________
NewWizardForm:

    .If D$DebugDialogHandle <> 0
          call KillDebugger | If eax = &IDNO | pop esi | ret | End_If
    .End_If

    call 'User32.MessageBoxA' &NULL {'Set the caret where you want the code to be pasted, and click OK.',0},
                              {'Wait a minute...',0}    &MB_ICONEXCLAMATION+&MB_SYSTEMMODAL


    call GetRosAsmFilesPath
    mov esi RosAsmFilesPath | mov edi WizardPath
    While B$esi <> 0 | movsb | End_While
    mov esi WizardName
    While B$esi <> 0 | movsb | End_While | mov B$edi 0


    call 'Kernel32.GetCurrentDirectoryA' &MAX_PATH NewFormPath
    mov esi NewFormPath | add esi eax
    mov D$esi '\Wiz' | add esi 4 | mov D$esi 'ardF' | add esi 4 | mov D$esi 'iles' | add esi 4
    mov B$esi 0

    push esi | call 'Kernel32.CreateDirectoryA' NewFormPath &NULL | pop esi

    mov D$esi '\WZR' | add esi 4 | mov D$esi 'DFor' | add esi 4 | mov D$esi 'm*.w' | add esi 4
    mov W$esi 'wf'   | add esi 2 | mov B$esi 0

  push esi
    ; looks for existing wizard files in the '\WizardFiles\' sub-directory of the program's directory
    call 'Kernel32.FindFirstFileA' NewFormPath WizardSaving
    mov D$WizardSearchHandle eax
    xor ecx ecx
    If eax <> &INVALID_HANDLE_VALUE
L1:     push ecx | call 'Kernel32.FindNextFileA' D$WizardSearchHandle WizardSaving | pop ecx
        inc ecx
        cmp eax 0 | jnz L1<
        push ecx | call 'Kernel32.FindClose' D$WizardSearchHandle | pop ecx
    End_If
  pop esi

    ; creates the new file name according to the number of already existing files
    mov ebx ecx | mov edi esi | sub edi 2
    std
        mov ecx, 4
L1:     mov al bl | and al 0F | On al >= 0A, add al 7
        add al, '0' | stosb | shr ebx, 4 | loop L1
    cld
    mov D$esi-1 '.wwf' | mov B$esi+3 0


    ; => command line writing:
    mov edi WizardCommandLine

    ;    * Wizard FileName
    mov B$edi '"' | inc edi
    mov esi WizardPath
    While B$esi <> 0 | movsb | End_While
    mov B$edi '"' | inc edi

    ;    * File location
    mov D$edi '-f "'  | add edi 4
    mov esi NewFormPath
    While B$esi <> 0 | movsb | End_While
    mov W$edi '" ' | add edi 2

    ;    * create the Wizard File
    mov W$edi '-c'  | add edi 2 | mov B$edi 020 | inc edi

    ;    * RosAsm ID
    mov W$edi '-R'  | add edi 2 | mov B$edi 0 | inc edi

    call LoadWizardAndPasteCode &FALSE

    call ReInitUndoOnly
ret

____________________________________________________________________________________________
Proc LoadWizardAndPasteCode:
    Argument @TagBegin

    call 'Kernel32.CreateEventA' &NULL &FALSE &FALSE {'ProducedCode_BeginWrite',0}
    mov D$ProducedCode_BeginWrite eax

    call 'Kernel32.CreateProcessA' WizardPath WizardCommandLine 0 0  0  0 0 0  STARTUPINFO  ProcessInfos
    If eax = 0
        call 'User32.MessageBoxA' &NULL {"The Wizard cannot be loaded.",0},
                                        ErrorMessageTitle   &MB_ICONEXCLAMATION+&MB_SYSTEMMODAL
        jmp L9>>
    End_If
    call 'Kernel32.WaitForSingleObject' D$ProducedCode_BeginWrite &INFINITE  ; wait while the user uses the wizard
    ; open the shared memory where the code has been written.
    ; the first DWord of this data is the code length.
    call 'Kernel32.OpenFileMappingA' &FILE_MAP_ALL_ACCESS 0 {'RosAsmWizardProducedCode',0}
    mov D$ProducedCodeHandle eax
    call 'Kernel32.MapViewOfFile' D$ProducedCodeHandle &FILE_MAP_ALL_ACCESS 0 0 0 ; jE!
    mov D$ProducedCode eax | add D$ProducedCode 4

    If D$eax <> 0
        On D@TagBegin <> &FALSE, call DeleteLastWizardCode D@TagBegin
        mov eax D$ProducedCode | mov edx D$eax-4
        call AddWizardCode eax edx
    Else
        On D@TagBegin = &FALSE, call 'Kernel32.DeleteFileA' NewFormPath
    End_If

    sub D$ProducedCode 4
    call 'Kernel32.UnmapViewOfFile' D$ProducedCode
    call 'Kernel32.CloseHandle' D$ProducedCodeHandle
    call 'Kernel32.OpenEventA' &EVENT_MODIFY_STATE &FALSE {'ProducedCode_Written',0}
    mov D$ProducedCode_Written eax
    call 'Kernel32.SetEvent' D$ProducedCode_Written
    call 'Kernel32.CloseHandle' D$ProducedCode_Written
    call 'Kernel32.CloseHandle' D$ProcessInfos+4
    call 'Kernel32.CloseHandle' D$ProcessInfos
L9: call 'Kernel32.CloseHandle' D$ProducedCode_BeginWrite

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

 ; Tag Unicode MyUnicodeString

[MyUnicodeString: W$ 06D, 06C, 06B, 06D, 06C, 06B, 06D, 06C, 06B, 0D, 0A,
                     06D, 06D, 06C, 06B, 06D, 06C, 06B, 0D, 0A, 06C, 06B,
                     06A, 06C, 06A]

UnicodeTag:
  ; esi points to 'MyUnicodeString', inside "; Tag Unicode MyUnicodeString":
    call 'USER32.DialogBoxParamW' D$hinstance, 600, &NULL, UnicodeEditorProc, esi
ret


[UnicodeEditorHandle: ?, UnicodeDataPointer: ?]

; Tag Dialog 600

Proc UnicodeEditorProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        move D$UnicodeEditorHandle D@Adressee, D$UnicodeDataPointer D@lParam

        call 'USER32.SetClassLongW' D@Adressee, &GCL_HICON, D$wc_hIcon

        call LoadUnicodeEditorFont

        On D$UnicodeEditorFontHandle <> 0,
            call 'USER32.SendDlgItemMessageW' D@Adressee, 10, &WM_SETFONT,
                                              D$UnicodeEditorFontHandle, &TRUE

        call SetUnicodeDialogContent | jmp L8>>

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        .If D@wParam = &IDCANCEL
            If D$UnicodeEditorFontHandle <> 0
                call 'GDI32.DeleteObject' D$UnicodeEditorFontHandle
                mov D$UnicodeEditorFontHandle 0
            End_If

            call 'User32.DestroyWindow' D@Adressee

        .Else_If D@wParam = &IDOK
          ; The call is from inside the Right-Click, with full Source restored. So:
            call SetPartialEditionFromPos
                call PasteUnicodeDialogContent
            call RestoreRealSource

            If D$UnicodeEditorFontHandle <> 0
                call 'GDI32.DeleteObject' D$UnicodeEditorFontHandle
                mov D$UnicodeEditorFontHandle 0
            End_If

            call 'User32.DestroyWindow' D@Adressee

        .Else_If D@wParam = 3
            call GetUnicodeEditorFont

            If D$UnicodeEditorFontHandle <> 0
                call 'USER32.SendDlgItemMessageW' D@Adressee, 10, &WM_SETFONT,
                                              D$UnicodeEditorFontHandle, &TRUE
                call UpdateRegistry
            End_If
;;
; Does not work, at all:

        .Else_If D@wParam = 4
            call 'USER32.GetDlgItem' D@Adressee, 10
            push eax
                call 'USER32.GetWindowLongW' eax, &GWL_STYLE
                xor eax &ES_AUTOHSCROLL__&WS_HSCROLL
            pop ebx
            call 'USER32.SetWindowLongW' ebx, &GWL_STYLE, eax
;;
        .End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP


    ...Else
L8:     popad | mov eax &FALSE | ExitP

    ...End_If

    popad | mov eax &TRUE
EndP


[UNICODE_EDITION_CHOOSEFONT: ; EditorCHOOSEFONT
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hDC: D$ 0
 @lpLogFont: D$ UNICODE_EDITION_LOGFONT
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

[UNICODE_EDITION_LOGFONT:
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
 @lfFaceName: U$ 0 #&LF_FACESIZE
 UNICODE_EDITION_LOGFONTlen: len]

[UnicodeEditorFontHandle: ?]

LoadUnicodeEditorFont:
    If D$UnicodeEditorFontHandle = 0
        call 'GDI32.CreateFontIndirectA' UNICODE_EDITION_LOGFONT
        mov D$UnicodeEditorFontHandle eax
    End_If
ret


GetUnicodeEditorFont:
    move D$UNICODE_EDITION_CHOOSEFONT@hwndOwner D$UnicodeEditorHandle
    call 'Comdlg32.ChooseFontW' UNICODE_EDITION_CHOOSEFONT

    If eax = &TRUE
        On D$UnicodeEditorFontHandle <> 0, call 'GDI32.DeleteObject' D$UnicodeEditorFontHandle
        call 'GDI32.CreateFontIndirectW' UNICODE_EDITION_LOGFONT
        mov D$UnicodeEditorFontHandle eax
    End_If
ret


[AddedCRLF: ?, UnicodeDataInsertionPoint: ?, UnicodeValuesAlignment: ?
 NumberOfUnicodeChars: ?]

PasteUnicodeDialogContent:
    mov esi D$UnicodeDataPointer
    call InternalRightClick
    If B$BlockInside = &TRUE
        mov esi D$BlockStartTextPtr
        While B$esi <> '[' | dec esi | End_While
        mov D$BlockStartTextPtr esi, D$UnicodeDataInsertionPoint esi

        mov esi D$BlockEndTextPtr
        While B$esi <> ']' | inc esi | End_While
        mov D$BlockEndTextPtr esi
        call ControlD

        mov B$AddedCRLF &FALSE

    Else
        mov B$AddedCRLF &TRUE

        mov esi D$UnicodeDataPointer
        While B$esi >= ' ' | inc esi | End_While
        mov D$UnicodeDataInsertionPoint esi

    End_If

    ;    mov ebx D$UnicodeDataPointer
    ;    While B$ebx > CR | inc ebx | End_While | add ebx 2

    call 'USER32.SendDlgItemMessageW' D$UnicodeEditorHandle, 10, &WM_GETTEXTLENGTH,
                                      D$UnicodeEditorFontHandle, &FALSE
    add eax 100
    push eax
        shl eax 1 | VirtualAlloc Trash1 eax
    pop eax

    push eax
        call 'USER32.SendDlgItemMessageW' D$UnicodeEditorHandle, 10, &WM_GETTEXT,
                                          eax, D$Trash1
    pop eax

  ; Max = 7 Char ("0FFFF, ") >>> 8 + Alignements (....) >>>> 32
    shl eax 5 | VirtualAlloc Trash2 eax

    mov edi D$Trash2

    If B$AddedCRLF = &TRUE
        mov D$edi CRLF2 | add edi 4
    End_If

    mov B$edi '[', ecx edi | inc edi

  ; Write the Data Label to the Tempo Buffer:
    mov esi D$UnicodeDataPointer
    While B$esi > ' ' | movsb | End_While
    mov D$edi ': U$', B$edi+4 ' '| add edi 5

  ; Alignement count:
    sub ecx edi | neg ecx | mov D$UnicodeValuesAlignment ecx

    mov esi D$Trash1, ecx 0, D$NumberOfUnicodeChars 0
    .While W$esi <> 0
        movzx eax W$esi| add esi 2 | call WriteEax | mov W$edi ', ' | add edi 2 | inc ecx
        inc D$NumberOfUnicodeChars
      ; New Line when wished:
        If ecx = 11
            mov ecx 0
            mov W$edi CRLF | add edi 2
            mov ecx D$UnicodeValuesAlignment
            While ecx > 0 | mov B$edi ' ' | dec ecx | inc edi | End_While
        End_If
    .End_While

  ; Remove the possible CRLF and ',', at the end of the buffer:
    While B$edi <= ' ' | dec edi | End_While | On B$edi = ',', dec edi
    inc edi

  ; Cases of empty Edition:
    If W$edi-2 = 'U$'
        mov W$edi ' 0' | add edi 2
    Else
        mov D$edi ', 0]' | add edi 3
    End_If

    mov W$edi CRLF | add edi 2
    mov esi D$UnicodeDataPointer
    mov B$edi ' ' | inc edi
    While B$esi > ' ' | movsb | End_While
    mov D$edi 'Ncha', D$edi+4 'rs: ' | add edi 8
    mov D$edi 'D$  ' | add edi 3
    mov eax D$NumberOfUnicodeChars | call WriteEax

    mov B$edi ']' | inc edi

    move D$BlockStartTextPtr D$Trash2, D$BlockEndTextPtr edi
    mov B$BlockInside &TRUE
    call ControlC

    mov B$BlockInside &FALSE
    call SetCaret D$UnicodeDataInsertionPoint
    call ControlV

    VirtualFree D$Trash2, D$Trash1
ret


SetUnicodeDialogContent:
    mov esi D$UnicodeDataPointer | call InternalRightClick

    .If B$BlockInside = &TRUE
        mov esi D$BlockEndTextPtr
        While B$esi <> '0'
            inc esi | On B$esi = ']', ret
        End_While
        push esi
        ; Count the number of Words (at least...):
          mov ecx 0
          While B$esi <> ']'
            On B$esi = '0', inc ecx
            inc esi
          End_While
          inc ecx | shl ecx 1
        ; Get a mem:
          VirtualAlloc Trash1, ecx
        pop esi

      ; esi >>> first '0' of the first hexa data.
      ; Translate each Hexa Word to Binary:
        mov edi D$Trash1
      ; strip the leading '0'
L0:     lodsb
        mov ebx 0
L1:     lodsb | cmp al ',' | je L8>
                cmp al ']' | je L8>
                    sub al '0' | cmp al 9 | jbe L2>
                        sub al 7
                        cmp al 0F | ja L9>
L2:     shl ebx 4 | or bl al | jmp L1<

L8:     mov W$edi bx | add edi 2

        If al <> ']'
            While B$esi < '0'
                inc esi | On esi >= D$SourceEnd, jmp L9>
            End_While
            On B$esi = '0', jmp L0<
        End_If

L9:     mov W$edi 0
        call 'USER32.SendDlgItemMessageW' D$UnicodeEditorHandle, 10, &WM_SETTEXT, 0, D$Trash1

        VirtualFree D$Trash1
    .End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

MenuTag: ; 'ExistingMenu'
    While B$esi = ' ' | inc esi | End_While

    call TagDecimalToBin

    If eax = &TRUE
        mov eax ecx | call GetTagedMenu
    End_If
ret


[TagedEdition: ?]

GetTagedMenu:
    mov esi MenuList
  ; (ID / Ptr / Size)
    While D$esi <> 0
        If D$esi = eax
            mov B$TagedEdition &TRUE
            mov D$MenuListPtr esi
            call ReEditExistingMenu | jmp L9>
        Else
            add esi (4*3)
        End_If
    End_While

L9: .If B$TagedEdition = 0-1
      ; Delete the Previous Menu Equates Block, and paste the New Menu Equates Block:
        Lea esi D$DataForClipEquates+3 | call InternalRightClick

        If D$BlockInside = &TRUE
            mov esi D$BlockStartTextPtr
            While B$esi <> '[' | dec esi | End_While
            mov D$BlockStartTextPtr esi
            inc esi
            While B$esi <> ']' | inc esi | End_While
            mov D$BlockEndTextPtr esi
            call ControlD | call ControlV
        End_If
    .End_If

    mov B$TagedEdition &FALSE
ret









