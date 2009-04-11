TITLE String

 _______________________________________________________________________________________
 _______________________________________________________________________________________

                            ; Strings Resources Management.
 _______________________________________________________________________________________
 _______________________________________________________________________________________

; Read resources when loading a new RosAsm PE:

ReadRosAsmStrings:
    mov edi StringsList, eax 0, ecx 300 | rep stosd
    mov ebx &RT_STRING | call SearchResourceType | On eax = 0, ret
    mov D$StringsListPtr StringsList,  ebx StringsListPtr | call ReadResourcesRecord
ret


; Editing Resources Strings. A simple EditBox holds the Strings. Each String begins with
; leading '#n ' 'n' being decimal Ascii. CR/LF are allowed inside Strings:

StringsResources:
    If D$StringsEditorHandle = 0
        call 'USER32.CreateDialogIndirectParamA' D$hinstance StringsDialog D$hwnd StringsProc 0
    Else
        Beep
    End_If
ret


[ID_HELP 3    ID_EDITSTRINGS 4]

[StringsDialog: D$ 090CC08C2 0        ; Style
 U$ 04 0 0 0DC 0C8             ; Dim
 0                             ;      no Menu
 '' 0                          ; Class
 'Strings Resources' 0         ; Title
 08 'Helv' 0]                  ; Font

[StringC0: D$ 050B000C4 0      ; Style
 U$ 0 0 0DB 0B2                ; Dim
 ID_EDITSTRINGS                ; ID
 0FFFF 081                     ; Class
 '' 0                          ; Title
 0]                            ; No creation data

[strngC1: D$ 050000001 0      ; Style
 U$ 0A1 0B4 039 013            ; Dim
 &IDOK                            ; ID
 0FFFF 080                     ; Class
 'OK' 0                        ; Title
 0]                            ; No creation data

[StringC2: D$ 050000000 0      ; Style
 U$ 065 0B4 03A 013            ; Dim
 &IDCANCEL                    ; ID
 0FFFF 080                     ; Class
 'Cancel' 0                    ; Title
 0]                            ; No creation data

[StringC3: D$ 050000000 0      ; Style
 U$ 0 0B4 038 013              ; Dim
 ID_HELP                       ; ID
 0FFFF 080                     ; Class
 'Help' 0                      ; Title
 0]                            ; No creation data

____________________________________________________________________________________________
; Same as CleanMenuEnd.
; As i get no end problems with this damned Edit Controls when i need to know if there
; are some ending CR/LF or not at the end, i get the text, strip the CR/LF, reset the
; text:

[LenOfStringsText: ?    TempoStringsTextPtr: ?    StringsEditHandle: ?]

CleanStrings:
    mov B$ErrorString &FALSE
    call 'User32.GetDlgItem' D$StringsEditorHandle ID_EDITSTRINGS
    mov D$StringsEditHandle eax
    call 'User32.SendMessageA' D$StringsEditHandle &WM_GETTEXTLENGTH 0 0
    inc eax | mov D$LenOfStringsText eax

    VirtualAlloc TempoStringsTextPtr eax

    call 'User32.SendMessageA' D$StringsEditHandle &WM_GETTEXT,
                              D$LenOfStringsText D$TempoStringsTextPtr

    mov ecx eax | On ecx = 0, jmp L8>>

  ; Clean ending bad chars:
    mov esi D$TempoStringsTextPtr | add esi D$LenOfStringsText | mov D$esi+1 0
    While B$esi < ' '
        mov B$esi 0 | dec esi
    End_While

  ; Clean bad first chars too:
    mov esi D$TempoStringsTextPtr | add ecx esi      ; ecx = end of data
    While B$esi < '#'
        lodsb | cmp esi ecx | ja L1>
    End_While

L1: If B$esi <> '#'
L8:     call 'USER32.MessageBoxA' D$hwnd, BadStringID, ErrorMessageTitle, &MB_SYSTEMMODAL
        mov B$ErrorString &TRUE
    Else
        mov D$TempoStringsTextPtr esi
    End_If

L1: call 'User32.SendMessageA' D$StringsEditHandle &WM_SETTEXT 0 D$TempoStringsTextPtr
    VirtualFree D$TempoStringsTextPtr
ret
____________________________________________________________________________________________


[StringsEditorHandle: ?     StringsListHandle: ?    StringEditFirstRun: ?]

Proc StringsProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        mov B$StringEditFirstRun &TRUE
        move D$StringsEditorHandle D@Adressee
        call 'USER32.GetDlgItem' D@Adressee ID_EDITSTRINGS | mov D$StringsListHandle eax
        call InitStringsList
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

    ...Else_If D@Message = &WM_CTLCOLOREDIT                           ; Un-selected text at first show:
        .If B$StringEditFirstRun = &TRUE
            mov eax D$StringsListHandle
            If D@lParam = eax
                call 'User32.SendMessageA' D$StringsListHandle &EM_SETSEL 0 0
                mov B$StringEditFirstRun &FALSE
            End_If
        .End_If
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>>

    ...Else_If D@Message = &WM_COMMAND
        ..If D@wParam = &IDCANCEL
L1:         mov D$StringsEditorHandle 0
            call 'User32.DestroyWindow' D@Adressee

        ..Else_If D@wParam = ID_HELP
            call Help, B_U_AsmName, StringsHelp, ContextHlpMessage

        ..Else_If D@wParam = &IDOK
            call CleanStrings
            On B$ErrorString = &FALSE, call StoreStringsList
            If B$ErrorString = &FALSE
                call ReorderStringsList | call Group16Strings | jmp L1<<
            End_If

        ..End_If

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


[TempoAsciiStrings: ?    TempoUnicodeStrings: ?    OneStringID: ?]

TransDwordToAsciiNakedDecimal:
    mov dl 0FF | push edx
    mov ecx 10
L0: mov edx 0 | div ecx | push edx | cmp eax 0 | ja L0<
L2: pop eax
    cmp al 0FF | je L9>
       add al '0' | stosb | jmp L2<
L9: ret


InitStringsList:
    mov esi StringsList | On D$esi = 0, ret

    VirtualAlloc TempoAsciiStrings 0FFFF
    mov ebx StringsList, edi eax                           ; ebx points to 'StringsList' ID
    mov D$StringsListPtr StringsList

    .While D$ebx > 0
        mov esi D$StringsListPtr
        mov al '#' | stosb
        lodsd | dec eax | shl eax 4 | mov D$OneStringID eax
        call TransDwordToAsciiNakedDecimal | mov al ' ' | stosb
        add D$StringsListPtr 4
        mov esi D$StringsListPtr, esi D$esi
        While W$esi > 0
            lodsw | movzx ecx ax
L0:         lodsw | stosb | loop L0<
            mov al CR | stosb | mov al LF | stosb
            If W$esi > 0
                mov al '#' | stosb
                inc D$OneStringID | mov eax D$OneStringID
                call TransDwordToAsciiNakedDecimal | mov al ' ' | stosb
            End_If
        End_While
        add D$StringsListPtr 8                             ; next record
        mov ebx D$StringsListPtr
    .End_While
    mov al 0 | stosb

    call 'User32.SendMessageA' D$StringsListHandle &WM_SETTEXT 0 D$TempoAsciiStrings

    VirtualFree D$TempoAsciiStrings
ret


[OneStringBuffer: ?    IndexToStringsLines: ?    ErrorString: ?    NumberOfStrings: ?]

[BadStringID: "Bad or missing ID number encounted    
                         or
   missing space separator or String", 0]

StoreStringsList:
    mov edi StringsList,  eax 0, ecx MAXSTRINGS | rep stosd

    mov B$ErrorString &FALSE, D$StringsListPtr, StringsList, D$IndexToStringsLines 0
    VirtualAlloc OneStringBuffer 0FFFF

    call 'User32.SendMessageA' D$StringsListHandle &EM_GETLINECOUNT 0 0
    mov D$NumberOfStrings eax

  ; Read one Line (supposed beginning with '#n ':
L0: mov edi D$OneStringBuffer, ecx 0FFFF, al 0 | rep stosb
    mov edi D$OneStringBuffer, W$edi 0FFFF
    push edi
    call 'User32.SendMessageA' D$StringsListHandle, &EM_GETLINE, D$IndexToStringsLines, edi
    pop edi
    mov B$edi+eax 0  ; >>> 'EM_GETLINE_Comment'
    cmp eax 0 | je L9>>
L1: mov edi D$OneStringBuffer, B$edi+eax 0

  ; Read next Line(s), in Case it (they) would not begin with '#', this would be the same
  ; String:
L1: mov esi D$OneStringBuffer | inc D$IndexToStringsLines         ; Allowing CR/LF:
    while B$esi <> 0
        lodsb
    End_While
    mov W$esi 0A0D | add esi 2
    mov W$esi 0FFFF
    push esi
        call 'User32.SendMessageA' D$StringsListHandle, &EM_GETLINE, D$IndexToStringsLines,
                                   esi
    pop esi
    mov B$esi+eax 0  ; >>> 'EM_GETLINE_Comment'
    mov ebx D$NumberOfStrings | cmp D$IndexToStringsLines ebx | je L2>
    mov B$esi+eax 0                     ; clear remaining 0FF / 0FFFF when 1 / 0 char
    cmp B$esi '#' | jne L1<
  ; '#' encounted >>> stop appending:
L2: sub esi 2 | mov B$esi 0 | dec D$IndexToStringsLines

    mov esi D$OneStringBuffer
    lodsb | cmp al '#' | jne L8>
    cmp B$esi ' ' | je L8>
    mov ecx 0
    While B$esi > ' '                   ; Compute #n:
        lodsb
        If al > '9'
L8:         call 'USER32.MessageBoxA' D$hwnd, BadStringID, ErrorMessageTitle, &MB_SYSTEMMODAL
            mov B$ErrorString &TRUE | ret
        Else_If al < '0'
            jmp L8<
        End_If
        sub al '0'                      ; convert Decimal to binary:
        lea ecx D$ecx+ecx*4             ;     ecx = ecx * 5
        lea ecx D$eax+ecx*2             ;     ecx = eax + old ecx * 10
    End_While

    On B$esi <> ' ', jmp L8<            ; Suppress > Allow Tab and CR/LF
                                        ; > Down there test ecx=0 instead
    mov eax ecx
    On eax > 0FFFF, jmp L8<
    mov edi D$StringsListPtr
    stosd                               ; store ID in 'StringsList'
    inc esi | mov ecx 0
    push esi
        While B$esi > 0
            lodsb | inc ecx             ; compute lenght
        End_While
        If ecx = 0
            pop esi | jmp L8<<
        End_If
        inc ecx
        push ecx
            inc ecx | shl ecx 1         ; Unicode length
            push ecx
                push edi, esi
                    VirtualAlloc TempoMemPointer ecx | mov eax D$TempoMemPointer
                pop esi, edi
                mov ebx eax | stosd     ; store Pointer
            pop eax | stosd             ; store Size
        pop ecx
        mov edi ebx, eax 0
    pop esi
L1: lodsb | stosw | loop L1<            ; Fill Unicode String mem
    add D$StringsListPtr 12
    inc D$IndexToStringsLines | jmp L0<<

    VirtualFree D$OneStringBuffer
ret


[Reordered: ?]

ReorderStringsList:
  ; First, numerical order:
L0: mov B$Reordered &FALSE
    mov esi StringsList, edi esi | add edi 12   ; esi > ID  // edi next ID
    While D$edi+4 > 0
        mov eax D$esi
        If eax > D$edi
            Exchange D$esi D$edi                ; exchange IDs
            Exchange D$esi+4 D$edi+4            ; exchange Ptrs
            Exchange D$esi+8 D$edi+8            ; exchange Sizes
            mov B$Reordered &TRUE
        End_If
        add esi 12 | add edi 12
   End_While
   cmp B$Reordered &TRUE | je L0<

  ; Now, ensure no identical IDs. If yes, auto_ajust:
L0: mov esi StringsList, B$Reordered &FALSE
    While D$esi+16 > 0
       mov eax D$esi
       If D$esi+12 = eax
           mov B$Reordered &TRUE | inc D$esi+12 | jmp L0<
       End_If
       add esi 12
    End_While
    cmp B$Reordered &TRUE | je L0<
ret


[NextIdNode: ?    Group16StringsLen: ?    StringsGroupBoudary: ?    GroupStringsID: ?]

; Resource tree IDs are not user defined IDs. They are ((User_ID shr 4) +1) and attached
; Resource Pointers point to groups of 16 Stings. Writes the ID high bits:

[StoreStringID | push eax | shr eax 4 | inc eax | mov D$GroupStringsID eax | pop eax]

Group16Strings:
    VirtualAlloc OneStringBuffer 0FFFF

    mov esi StringsList, D$StringsListPtr esi

L0: mov edi D$OneStringBuffer | cmp D$esi+4 0 | je L9>>

    mov eax D$esi | StoreStringID | mov D$Group16StringsLen 0
    move D$NextIdNode D$GroupStringsID | shl D$NextIdNode 4  ; '+16' is done by (shr 4)+1

  ; Store dummy Strings if the first ID is not at a 00_1_0000 boundary or if Strings IDs
  ; are missing in between:
    mov ecx D$esi | and ecx 00_1111 | jmp M1> ; first_case in new group, jE!
L1: mov ecx D$esi | and ecx 00_1111 ; jE!
    .If esi > StringsList
        mov ebx D$esi-12
        and ebx 00_1111 ; jE!
        If ecx > ebx
            dec ecx | sub ecx ebx
        End_If
    .End_If
M1:
    jecxz L2>
    mov eax 0200001                                 ; 01 00 020 00 (1 wChar: 1 Space).
M0: add D$Group16StringsLen 4 | stosd | loop M0<

  ; Store one String to "OneStringBuffer": >>jE! optimiZZed > copyU_with_count
L2: push esi
        mov esi D$esi+4, edx edi | sub ecx ecx | sub eax eax | add edi 2
X0:     lodsw | test eax eax | je X1> | inc ecx | stosw | jmp X0<
X1:     mov W$edx cx | lea ecx D$ecx*2+2 | add D$Group16StringsLen ecx
    pop esi

  ; Stop grouping if:
  ; - Next ID is 00_10000 aligned (ebx)
  ; - Previous ID includes 00_1111 (eax)
  ; - Next record is greater than next 16 Strings Node
  ; - Next record is empty
    mov eax D$esi, ecx eax | inc ecx                ; ecx: Next ID should be ID+1
    add esi 12 | mov ebx D$esi
    and eax 00_1111

    mov B$StringsGroupBoudary &FALSE
    If D$esi+4 = 0
        mov B$StringsGroupBoudary &TRUE             ; Next = empty
    Else_If eax = 00_1111
        mov B$StringsGroupBoudary &TRUE             ; Actual = End of Set
    Else_If ebx >= D$NextIdNode
        mov B$StringsGroupBoudary &TRUE             ; Next >= Start of Set
    End_If

    .If B$StringsGroupBoudary = &TRUE
        push esi
            push edi
                mov eax D$StringsListPtr | mov eax D$eax+4 | VirtualFree eax
                VirtualAlloc TempoMemPointer D$Group16StringsLen
                mov eax D$TempoMemPointer
                push eax
                    mov edi D$StringsListPtr
                    mov eax D$GroupStringsID | stosd    ; Write Splitted ID
                pop eax
                push eax
                    stosd                               ; Write Ptr
                    mov eax D$Group16StringsLen | stosd ; Write Lenght
                pop edi
                mov ecx eax, esi D$OneStringBuffer | rep movsb; Write Strings group Data
            pop edi
        pop esi
        add D$StringsListPtr 12 | jmp L0<<
    .Else_If D$esi = ecx
        jmp L2<<                            ; OK > group
    .Else
        jmp L1<<                            ; Missing records > Pad and group
    .End_If

L9: mov esi D$StringsListPtr
    While D$esi+4 > 0
        mov eax D$esi+4
        push esi
            VirtualFree eax                 ; Release possible tail mems.
        pop esi
        mov D$esi 0, D$esi+4 0, D$esi+8 0   ; Clear Record
        add esi 12
    End_While

    VirtualFree D$OneStringBuffer           ; Release temporary storage.
ret


