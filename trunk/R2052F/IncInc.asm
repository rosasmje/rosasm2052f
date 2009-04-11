TITLE IncInc

;;
  Purpose: Modifying at once a TITLE for several RosAsm PEs. This Title must have
  an identical .inc Version on the Disk. Modifying the File.inc will automatically
  modify the Title(s) in all concerned PEs when re-Compiling them.

  When reading:

  >PREPARSE IncInclude

  the Parser parses, for example, the:

  >INCINCLUDE E:\RosAsm\Macros.inc

  Statement, opens the indicated File and compares it with the actual
  
  TITLE Macros

  If identical, it does nothing. If different, it replaces the concerned Title, both
  in the user Source, and in the actually computed 'Cooked Source'.
;;


;;
    Added by /\\o//\(Half). updated 01/10
    
    Add resource 10, a dialog, that shows all includes, as a checklist for inclusion
    (if incincludes are present)
    
    Wanted really a CHECKLIST instead of a ListBox, 
    but added a list box, and fills it with yes no, when clicked instead.
    --Plus one list saying MOD for the files that differs from current source
    A button toggles between yes and no to include everything.
    
;;

[IncludeListBox                     101
 IncludeCheckListBox                102
 IncludeChangedCheckListBox         103
 SelectAllButton                    10
 id_IncludeListCompilationDialog    30]

[IncludeUncheckedString:"No  " 0
 IncludeCheckedString:  "Yes " 0
 IncludeChangedString:  "Mod" 0
 IncludeUnChangedString:" " 0]

[IncInc_MaxFiles 100]

[IncludeListCompilationDialog.Handle:  ?

 Incinc.FileList:                       ?
;;
   The file list is formated as :
   Count, SizeString1, String1, SizeString2, String2 ....ect... (Temp virtual mem)
   where SizeString is a "packed" dword, where the upper word is 0FF00 before
   the dialog is shown, to indicate MODIFIED titles. And it is 0FFFF on exit
   to tell which file has been selected. The lower word is the Length of the
   string.
;;

 EnableSubstitution:                    ?
;;
 This is a boolean variable that when TRUE, will include changed files and when 
 false will only evaluate state.
;;

 IncInc.TitleChanged:                   ?
 ; Set to true per file, when the file differs from source

 IncInc.SomeTitleHasChanged:            ?]
;;
    Set to true if ANY of the titles has changed
    and skips showing the dialog if no titles 
    changed.
;;

;;
   Uses the Incinc.FileList memory
   to display 3 lists [FILENAME, SELECTSTATUS, CHANGEDSTATUS]
;;
FillIncludeListBox:
    mov ebx D$Incinc.FileList
    mov edx D$Incinc.FileList
    push D$edx
        add ebx 4
      L0:
        add ebx 4
        push ebx edx
            push ebx
                mov eax D$ebx-4
                movzx eax ax
                add ebx eax
                while b$ebx <> '\' | dec ebx | end_while
                call 'user32.SendDlgItemMessageA',
                    D$IncludeListCompilationDialog.Handle,
                    IncludeListBox,
                    &LB_ADDSTRING,
                    0,
                    ebx
                call 'user32.SendDlgItemMessageA',
                    D$IncludeListCompilationDialog.Handle,
                    IncludeCheckListBox,
                    &LB_ADDSTRING,
                    0,
                    IncludeUncheckedString
            pop ebx
            mov eax D$ebx-4 | shr eax 16 | cmp ax 0ff00 | jne L1>
                call 'user32.SendDlgItemMessageA',
                    D$IncludeListCompilationDialog.Handle,
                    IncludeChangedCheckListBox,
                    &LB_ADDSTRING,
                    0,
                    IncludeChangedString

                jmp L2>
            L1:
                call 'user32.SendDlgItemMessageA',
                    D$IncludeListCompilationDialog.Handle,
                    IncludeChangedCheckListBox,
                    &LB_ADDSTRING,
                    0,
                    IncludeUnChangedString
            L2:
        pop edx ebx
        mov eax D$ebx-4





        movzx eax ax
        add ebx eax
        dec D$edx | jnz L0<<
    pop D$edx

ret

[SelItem: ?]

;;
   Retrive the current selected listbox item, 
   and mark the corresponding file as selected in the Incinc.FileList
;;

ToggleIncludeFileList:
  call 'user32.SendDlgItemMessageA',
    D$IncludeListCompilationDialog.Handle,
    IncludeListBox,
    &LB_GETCURSEL,
    0,
    0

  mov D$SelItem eax

  ;*bininc.filename* is available, because we will rewrite it later, so we borrow it
  call 'user32.SendDlgItemMessageA',
        D$IncludeListCompilationDialog.Handle,
        IncludeCheckListBox,
        &LB_GETTEXT,
        eax,
        bininc.Filename

  mov ebx bininc.Filename

  cmp D$ebx 'Yes ' | je L0>
    call SetIncludeCheckList IncludeCheckedString
    jmp L1>
  L0:
    call SetIncludeCheckList IncludeUnCheckedString
  L1:

ret

;;
    Marks the selected file as included, exludes and
    updates the list. 
;;

Proc SetIncludeCheckList:
Argument @Value

  mov ecx D$SelItem
  mov edx D$Incinc.FileList
  add edx 4
  while ecx > 0
      mov eax D$edx
      and eax 0_FFFF
      add edx eax
      add edx 4
      dec ecx
  end_while

  if D@Value = IncludeCheckedString
    or D$edx 0_FFFF_0000
  else
    and D$edx 0_FFFF
  end_if

  call 'user32.SendDlgItemMessageA' D$IncludeListCompilationDialog.Handle,

                                    IncludeCheckListBox, &LB_INSERTSTRING D$SelItem, D@Value
  inc D$SelItem
  call 'user32.SendDlgItemMessageA' D$IncludeListCompilationDialog.Handle,

                                    IncludeCheckListBox, &LB_DELETESTRING D$SelItem, 0
EndP

;;
    Tests the first listbox item and sets all item to the oposite value
;;
ToggleAll:
  call 'user32.SendDlgItemMessageA' D$IncludeListCompilationDialog.Handle,
                                      IncludeCheckListBox, &LB_GETTEXT 0, bininc.Filename
  mov ebx bininc.Filename
  cmp D$ebx 'Yes ' | je L0>

  call 'user32.SendDlgItemMessageA' D$IncludeListCompilationDialog.Handle,
                                    IncludeCheckListBox, &LB_GETCOUNT 0, 0
  mov D$SelItem 0
  While D$SelItem < eax
     push eax | call SetIncludeCheckList IncludeCheckedString | pop eax
  End_While
ret
  L0:

  call 'user32.SendDlgItemMessageA' D$IncludeListCompilationDialog.Handle,
                                    IncludeCheckListBox, &LB_GETCOUNT 0, 0
  mov D$SelItem 0
  While D$SelItem < eax
     push eax | call SetIncludeCheckList IncludeUnCheckedString | pop eax
  End_While
ret

;;
    Callback for the dialog.
;;
Proc IncludeListCompilationDialogCallBack:
    Arguments @Adressee, @Message, @wParam, @lParam

        pushad

        mov eax &FALSE
        ..If D@Message = &WM_COMMAND
            mov eax D@wParam
            If D@wParam = &IDCANCEL
                call 'USER32.EndDialog' D@Adressee, 0
            else_if,
               ax = IncludeListBox
               shr eax 16
               cmp ax &LBN_DBLCLK | jne L0>
                  call ToggleIncludeFileList
               L0:
            else_if,
               ax = SelectAllButton
                  call ToggleAll
            End_If

            On D@wParam = &IDOK, call 'USER32.EndDialog',
                                      D$IncludeListCompilationDialog.Handle,
                                      1



        ..Else_If D@Message = &WM_INITDIALOG
            mov eax D@Adressee | mov D$IncludeListCompilationDialog.Handle eax
            call FillIncludeListBox

        ..Else

            popad | mov     eax &FALSE | ExitP
        ..End_if

        popad | mov eax &TRUE
EndP


;;
    called to display the dialog
    the  Incinc.FileList must be initialized.
;;
DisplayIncludeListCompilationDialog:
  call 'USER32.DialogBoxParamA',
   D$hInstance,
   id_IncludeListCompilationDialog ,
   0,
   IncludeListCompilationDialogCallBack,
   0
ret
____________________________________________________________________________________________

[incinc.errornotfound: B$ 'IncIncluder: File not found!' 0
 incinc.errorsize: B$ 'IncIncluder: File size is greater than 1MB!' 0
 incinc.errorSyntax: B$ 'Bad INCINCLUDE syntax', 0]


;;
    This is the original code. Basically locates and
    compares the TITLE in this app, against the loaded title
    (loaded at D$bininc.mem)
    
    
    My additions is just to compile a list of the changed and unchanged files
    and show to the user so he can select the once to update.
;;

IncParser: UpdateTitlesFromIncludeFiles:
    mov D$OldStackPointer esp

    call GetResourcesSize
    add eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation
    mov D$AsmTablesLength eax
    VirtualAlloc CodeSourceA eax | add D$CodeSourceA 010
    call NewCopyToCodeSourceA D$CodeSource, D$SourceLen

    call CheckTextDelimitersPairing
    call KillMultiLineComments ; and Comments
   ; call KillSingleLineComments
    call NewKillVirtualCRLF
    call KillMeaninglessCommas
    call CheckandKillPreParsers


    mov B$ErrorLevel 9
    mov D$IncInc.SomeTitleHasChanged &FALSE
    mov D$EnableSubstitution &FALSE
    VirtualAlloc Incinc.FileList (&MAXPATH*IncInc_MaxFiles)

    mov esi D$CodeSourceA
    mov ecx esi | add ecx D$StripLen | mov D$EcxSave ecx

    .While esi < D$EcxSave
        cmp D$esi   'INCI' | jne L8>>
        cmp D$esi+4 'NCLU' | jne L8>>
        cmp W$esi+8 'DE'   | jne L8>>
        cmp B$esi-1 LF | jne L8>>
        cmp B$esi+10 ' ' | ja L8>>

        add esi 10
L3:     inc esi | cmp B$esi ' ' | jbe L3<

        mov edx bininc.filename
L3:         mov al B$esi | mov B$edx al
            inc esi | inc edx
        cmp B$esi ' ' | ja L3<
        mov B$edx 0

      ; Clear the 'INCLUDE xxxxxxx' Statement:
        mov ebx esi | While D$esi <> 'INCI' | dec esi | End_While
        While esi < ebx | mov B$esi ' ' | inc esi | End_While

____________________________________________________________________________________________
;;
    Generate a file list, and show to user before including any of those
;;
        mov ebx D$Incinc.FileList | inc D$ebx | add ebx 4

        While D$ebx <> 0
            mov eax D$ebx | and eax 0_FFFF
            add ebx eax | add ebx 4 |
        End_While

        add ebx 4
        mov edx bininc.filename
        push ebx
            While b$edx <> 0
                push w$edx | pop w$ebx
                cmp b$edx+1 0 | add edx 2
                add ebx 2 | je L0> |
            End_While
            inc ebx
            jmp L1>
            L0:

            L1:
        pop ecx
____________________________________________________________________________________________

        pushad
            call ReadIncFile
            If eax = &TRUE
                mov D$IncInc.TitleChanged &FALSE
                call CompareIncToTitle
                VirtualFree D$bininc.mem
            Else
            End_If
        popad
____________________________________________________________________________________________

        sub ebx ecx | xchg ebx ecx
        mov D$ebx-4 ecx
        if D$IncInc.TitleChanged = &TRUE
           or D$ebx-4 0_ff00_0000
           mov D$IncInc.SomeTitleHasChanged &TRUE
        end_if


L8:     inc esi
    .End_While

    mov edx D$Incinc.FileList
    cmp D$edx 0 | je L9>>


    cmp D$IncInc.SomeTitleHasChanged &TRUE | jne L9>>

    call DisplayIncludeListCompilationDialog

    ..if eax = &IDOK


       mov edx D$Incinc.FileList
       mov ecx D$edx
       add edx 4
       .while ecx > 0
            mov eax d$edx
            and D$edx 0_FFFF
            shr eax 16
            add edx 4
            .If ax = 0FFFF
              pushad
                mov edi bininc.filename
                mov ecx &MAXPATH | mov eax 0 | shr ecx 2 | rep stosd
                mov eax bininc.filename
                while b$edx <> 0
                      push w$edx | pop w$eax
                      cmp b$edx+1 0 | je L0>
                      add edx 2 | add eax 2
                End_While
                L0:
                call ReadIncFile
                If eax = &TRUE
                    mov D$EnableSubstitution &TRUE
                    mov D$IncInc.TitleChanged &FALSE
                    call CompareIncToTitle
                    VirtualFree D$bininc.mem
                Else
                End_If
              popad

            .End_if
            add edx D$edx-4
            dec ecx
       .end_while




    ..End_If


  L9:
    VirtualFree D$Incinc.FileList
    VirtualFree D$CodeSourceA | mov D$CodeSourceA 0
ret

ClearIncludeStateMentsFromSource:
 mov esi D$CodeSourceA
 mov ecx esi | add ecx D$StripLen | mov D$EcxSave ecx
 .While esi < D$EcxSave
        cmp D$esi   'INCI' | jne L8>>
        cmp D$esi+4 'NCLU' | jne L8>>
        cmp W$esi+8 'DE'   | jne L8>>

        add esi 10

L3:     inc esi | cmp B$esi ' ' | jbe L3<

        mov edx bininc.filename
L3:         mov al B$esi | mov B$edx al
            inc esi | inc edx
        cmp B$esi ' ' | ja L3<
        mov B$edx 0

        mov ebx esi | While D$esi <> 'INCI' | dec esi | End_While
        While esi < ebx | mov B$esi ' ' | inc esi | End_While

L8:     inc esi
 .End_While


ret
____________________________________________________________________________________________

ReadIncFile:
    pushad
        call 'KERNEL32.CreateFileA' bininc.filename,
                                    &GENERIC_READ,
                                    &FILE_SHARE_READ,
                                    &NULL,
                                    &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL,
                                    &NULL
        mov D$bininc.filehandle eax

        .If eax = &INVALID_HANDLE_VALUE
            mov eax incinc.errornotfound | call MessageBox
            popad
            mov eax &FALSE | ret
        .End_If

        call 'KERNEL32.GetFileSize' eax, 0 | mov D$bininc.filesize eax | add eax 4
;;
        .If eax > (1024*1024*2) ; 1MB
            mov eax incinc.errorsize | call MessageBox
            call 'KERNEL32.CloseHandle' D$bininc.filehandle
            popad
            jmp L8>>
        .End_If
;;
        VirtualAlloc bininc.mem eax
        mov ebx D$bininc.mem, D$ebx CRLF2 | add D$bininc.mem 4

        call 'KERNEL32.ReadFile' D$bininc.filehandle, D$bininc.mem, D$bininc.filesize,
                                 NumberOfReadBytes, 0
        call 'KERNEL32.CloseHandle' D$bininc.filehandle
    popad

    mov eax &TRUE
ret
____________________________________________________________________________________________

[INcSuccess: ?    IncTitleName: ?    StartOfChunk: ?    EndOfChunk: ?]

CompareIncToTitle:
    mov B$IncSuccess &FALSE

    call CoolParsersOnInc

    mov edi bininc.filename
    While B$edi <> 0 | inc edi | End_While
    While B$edi <> '\'
        dec edi | On edi = bininc.filename, jmp L1>
        On B$edi = '.', mov B$edi 0
    End_While
    inc edi

L1: mov D$IncTitleName edi

L1: mov esi D$CodeSourceA, edx D$StripLen | add edx esi

    .While esi < edx
        ...If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ...Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ...Else_If D$esi = 'TITL'
            ..If B$esi-1 = LF
                On W$esi+4 <> 'E ', jmp L0>>
                    mov D$StartOfChunk esi | add esi 5
                    While B$esi = ' ' | inc esi | End_While
                    push edi
                        mov ecx 0FF | repe cmpsb
                        .If B$edi-1 = 0
                            push esi, edi
                                mov edi D$bininc.mem,  ecx D$bininc.filesize
                                While D$esi <> 'TITL' | dec esi | End_While

                                L5: repe cmpsb
                                cmp b$esi-1 '$' | je L5<

                                While D$esi  <> 'TITL'
                                    On esi = edx, jmp L1>
                                    On B$esi > ' ', inc ecx
                                    inc esi
                                End_While
L1:                             mov D$EndOfChunk esi
                            pop edi, esi

ininc_ChangesBegin:
______________________________________________
                            If ecx <> 0
;;
  This was used as a small security against overwriting NEW code
  earlier. It still works, but isnt needed.
  
  
                                push ecx
                                  call 'USER32.MessageBoxA' 0, D$IncTitleName,
                                  IncDoneTitle, 
                                  &MB_SYSTEMMODAL + &MB_YESNOCANCEL + &MB_ICONQUESTION
                                pop ecx
                                cmp eax &IDYES|jne @SkipIncludingFile
                                cmp eax &IDCANCEL|jne @IncludeIt

                                mov B$INcSuccess &FALSE| pop edi | jmp L9>
;;
                               @IncludeIt:
                                mov D$IncInc.TitleChanged &TRUE
                                cmp D$EnableSubstitution &TRUE | jne @SkipIncludingFile
                                call SubstituteTitleByCookedInc
                                call SubstituteUserTitleByInc
                                mov esi D$IncTitleName, edi IncDoneTitle | add edi 22
                                While B$esi <> 0 | movsb | End_While | mov B$edi 0


                            End_If
 ______________________________________________


                            @SkipIncludingFile:

                            mov B$INcSuccess &TRUE| pop edi | jmp L9>


                        .End_If
                    pop edi
            ..End_If
        ...End_If

L0:     inc esi
    .End_While


[NoIncTitle: B$ 'Include File.inc without the required TITLE                           ', 0]
[IncDoneTitle: B$ "You current include file is diffrent from shared source." ,
                  " Overwrite current TITLE ???? " , 0]

L9: If B$INcSuccess = &FALSE
        mov esi D$IncTitleName, edi NoIncTitle | add edi 44
        While B$esi <> 0 | movsb | End_While | mov B$edi 0

        error NoIncTitle
    End_If
ret
____________________________________________________________________________________________

[ModifiedSourceSize: ?]

SubstituteTitleByCookedInc:  ; ControlD asmmain
    mov ebx D$CodeSourceA | add ebx D$StripLen
    mov edx D$CodeSourceA | add edx D$SourceLen | add edx 1_000_000
    mov ecx D$bininc.mem | add ecx D$bininc.filesize

    mov eax D$EndOfChunk | sub eax D$StartOfChunk
    sub eax ecx | add eax D$bininc.mem
    neg eax | mov D$ModifiedSourceSize eax

    call ChunkReplace D$StartOfChunk, D$EndOfChunk, ebx, edx, D$bininc.mem, ecx

    mov eax D$ModifiedSourceSize | add D$StripLen eax
ret


SubstituteUserTitleByInc:
    VirtualFree D$bininc.mem

  ; Restore the File.inc extension:
    mov esi bininc.filename | While B$esi <> 0 | inc esi | End_While | mov B$esi '.'
    call ReadIncFile

    mov eax D$bininc.mem | add eax D$bininc.filesize | mov ecx eax

    mov eax D$StartOfChunk | sub eax D$CodeSourceA | add eax D$CodeSource
    mov D$StartOfChunk eax

    mov ebx D$EndOfChunk | sub ebx D$CodeSourceA | add ebx D$CodeSource
    mov D$EndOfChunk ebx

    call ChunkReplace D$StartOfChunk, D$EndOfChunk, D$SourceEnd, D$EndOfSourceMemory,
                      D$bininc.mem, ecx

    mov eax D$ModifiedSourceSize | add D$SourceEnd eax | add D$SourceLen eax

    mov edi D$SourceEnd, D$edi CRLF2
ret









