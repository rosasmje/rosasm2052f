TITLE Rsrc

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

       Resources with customized Types and Names and Languages support
       Version 1.0
____________________________________________________________________________________________
____________________________________________________________________________________________
 
 Author name: Diego Fraga
 
 Email: < diegfrag@yahoo.com >
 
 Started: September 2004
 
____________________________________________________________________________________________

;;

; CustomList > Type/Name/Lang/Pointer/Size

[CustomList: ? #MAXRESOURCE]

[RsrcType: 0    RsrcTypeStringLen: 0    RsrcTypeString: B$ 0 #33]
[RsrcName: 0    RsrcNameStringLen: 0    RsrcNameString: B$ 0 #33]
[AviTypeStrLen: 4     AviTypeStr: B$ 'AVI', 0]
[WaveTypeStrLen: 5    WaveTypeStr: B$ 'WAVE', 0]

ClearCustomList:
    mov edi CustomList, eax 0, ecx MAXRESOURCE | rep stosd
ret

;;

[CustomList:
 CustomList.Type: D$ 0
 CustomList.Name: D$ 0
 CustomList.Lang: D$ 0
 CustomList.Pointer: D$ 0
 CustomList.Size: D$ 0]
;;

[CustomList.TypeDis 0
 CustomList.NameDis 4
 CustomList.LangDis 8
 CustomList.PointerDis 12
 CustomList.SizeDis 16]

[Size_Of_CustomList 20]

Proc ReadRosAsmResources:

    pushad
        call ClearCustomList
        mov D$RsrcType 0
        call FillCustomListFromResourceTree D$UserPEStartOfResources, CustomList
        call CopyStandarTypeResources
    popad

EndP

Proc FillCustomListFromResourceTree: ; 'ResourcesStub' For infos.
    Argument @Pointer, @Output
    Local @Array
    Uses esi, ebx, eax

    mov esi D@Pointer, eax 0, edi D@Output

    push esi
      ; add ImgResDir.NumberOfNamedEntries to ImgResDir.NumberOfIdEntries
      ; and copy the result to N:
        add esi ImgResDir.NumberOfNamedEntriesDis
        lodsw | mov D@Array eax
        lodsw | add D@Array eax
    pop esi

    ; esi and ebx now points to the IMAGE_RESOURCE_DIRECTORY_ENTRY.
    add esi Size_Of_IMAGE_RESOURCE_DIRECTORY
    add ebx Size_Of_IMAGE_RESOURCE_DIRECTORY

          ; We need to see if we have a Unicode String Name or a ID
L0:
            lodsd ; load the name ID to eax
            mov D$RsrcTypeStringLen 0
            mov edx eax

            mov D$edi+CustomList.TypeDis edx
            If D$RsrcType = 0
                mov D$RsrcType edx
            End_If

            add edi 4

            Test_If eax 08000_0000 ; If it is a named ID, load th resource strings
                push esi, edi
                    mov esi eax, edi RsrcTypeString, ebx RsrcTypeStringLen
                    call LoadRsrcIDString
                pop edi, esi

            Test_End

            ; Now, load the OffsetToData and save it´s value at eax
            lodsd

            .Test_If eax 08000_0000 ; If the high bit (0x80000000) is set this is a node
                xor eax 08000_0000 | add eax D$UserPEStartOfResources
                call FillCustomListFromResourceTree eax edi

                .If D@Array > 1
                  ; let´s search from the rest of the array IMAGE_RESOURCE_DIRECTORY_ENTRY.
                  ; point to the previous good datatype
                    add edi 4
                    dec D@Array
                    If D$RsrcType <> 0
                        move D$edi+CustomList.TypeDis D$RsrcType
                        add edi 4
                    Else
                        move D$RsrcType D$esi
                        move D$edi+CustomList.TypeDis D$RsrcType
                    End_If
                    jmp L0<<
                .Else_If D@Array = 1
                    mov D$RsrcType 0
                .End_If

            .Test_Else ; If the high bit (0x80000000) is not set this is a leaf
                ; Get the size and address of the data
                add eax D$UserPEStartOfResources
                mov esi eax
                lodsd | sub eax D$ResourcesRVA| add eax D$UserPEStartOfResources
                add edi 4
                mov ecx D$esi
                mov D$edi ecx ; copy it to CustomList.SizeDis
                pushad
                    call ReadResource
                    mov D$edi-4 eax ; copy the read address to CustomList.PointerDis
                popad
            .Test_End


EndP

____________________________________________________________________________________________
; Copy Standard Type resources from CustomList to each list (AviList, BitmapList,MenuList, etc)

[AviTypeDir: 0 WaveTypeDir: 0]

Proc CopyStandarTypeResources:

    mov esi CustomList, edi esi, D$AviTypeDir 0, D$WaveTypeDir 0
L1: cmp D$esi 0 | je l5>>
    lodsd
    mov D$RsrcTypeStringLen 0
    test eax 08000_0000 | jz L4>
    push esi, edi
        mov esi eax, edi RsrcTypeString, ebx RsrcTypeStringLen
        call LoadRsrcIDString
    pop edi, esi

    if D$AviTypeDir = 0
        push esi, edi, ecx
            mov esi AviTypeStr, edi RsrcTypeString, ecx D$AviTypeStrLen
            repe cmpsb
        pop ecx, edi, esi
        je L2>
    endif

    if D$WaveTypeDir = 0
        push esi, edi, ecx
            mov esi WaveTypeStr, edi RsrcTypeString, ecx D$WaveTypeStrLen
            repe cmpsb
        pop ecx, edi, esi
        je L3>
    endif

L4: add esi 16 | jmp L1<
L2: mov D$AviTypeDir eax | jmp L4<
L3: mov D$WaveTypeDir eax | jmp L4<
L5:

    call FillResourceTypeList CustomList, AviList, AviListPtr, D$AviTypeDir, MAXAVI
    call FillResourceTypeList CustomList, WaveList, WaveListPtr, D$WaveTypeDir, MAXWAVE
    call FillResourceTypeList CustomList, CursorList, CursorListPtr, &RT_CURSOR, MAXCURSOR
    call FillResourceTypeList CustomList, BitmapList, BitmapListPtr, &RT_BITMAP, MAXBITMAP
    call FillResourceTypeList CustomList, MenuList, MenuListPtr, &RT_MENU, MAXMENU
    call FillResourceTypeList CustomList, DialogList, DialogListPtr, &RT_DIALOG, MAXDIALOG
    call FillResourceTypeList CustomList, StringsList, StringsListPtr, &RT_STRING, MAXSTRINGS
    call FillResourceTypeList CustomList, RCdataList, RCdataListPtr, &RT_RCDATA, MAXRCDATA
    call FillResourceTypeList CustomList, GroupCursorList, GroupCursorListPtr, &RT_GROUP_CURSOR, MAXCURSOR
    call FillResourceTypeList CustomList, IconList, IconListPtr, &RT_ICON, MAXICON
    ; Erase the First Icon, Which is the Main One (elsewhere...)
    If D$IconList = 1
        VirtualFree D$IconList+4
        mov esi IconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
        On D$IconListPtr > IconList, sub D$IconListPtr 12
    End_If
    call FillResourceTypeList CustomList, GroupIconList, GroupIconListPtr, &RT_GROUP_ICON, MAXICON
    ; Erase the First GroupIcon, Which is the Main One (elsewhere...)
    If D$GroupIconList = 1
        VirtualFree D$GroupIconList+4
        mov esi GroupIconList, edi esi, ecx MAXICON-3 | add esi 12 | rep movsd
        On D$GroupIconListPtr > GroupIconList, sub D$GroupIconListPtr 12
    End_If

____________________________________________________________________________________________
; Remove "standard" resources from CustomList:
    mov esi CustomList, edi esi
L1: cmp D$esi 0 | je L5>>
    lodsd
    mov D$RsrcTypeStringLen 0
    cmp eax 16 | jg L2>
    test eax 08000_0000 | jz L3>

    cmp eax D$AviTypeDir | je L4>
    cmp eax D$WaveTypeDir | je L4>

L2: stosd | movsd | movsd | movsd | movsd | jmp L1<

L3: cmp eax &RT_FONTDIR | je L2<
    cmp eax &RT_FONT | je L2<
    cmp eax &RT_ACCELERATOR | je L2<
    cmp eax &RT_MESSAGETABLE | je L2<
    cmp eax 13 | je L2<
    cmp eax 15 | je L2<
    cmp eax &RT_VERSION | je L2<

L4: add esi 16 | jmp L1<<

L5: mov eax 0, ecx 5 | rep stosd

EndP
____________________________________________________________________________________________

Proc FillResourceTypeList:
    Arguments @InputList, @OutputList, @OutputListPtr, @ResType, @MaxResource

    pushad
    ; Clear the output list
    xor eax eax
    mov edi D@OutputList, ecx D@MaxResource | rep stosd

    mov esi D@InputList, edi D@OutputList
    While D$esi <> 0
        mov edx D@ResType
        If D$esi+CustomList.TypeDis = edx
            move D$edi D$esi+CustomList.NameDis ; copy Name only
            move D$edi+4 D$esi+CustomList.PointerDis ; copy Pointer only
            move D$edi+8 D$esi+CustomList.SizeDis ; copy size only
            add edi 12
        End_If
        add esi Size_Of_CustomList
    End_While

    mov edx D@OutputListPtr
    mov D$edx edi

    popad

EndP

____________________________________________________________________________________________
LoadRsrcIDString:
; In esi: source Unicode string entry ptr
; In edi: destination ptr
; In ebx: dest String size ptr (including null)

    push eax, ecx
        and esi 07ff_ffff | add esi D$UserPEStartOfResources
        mov eax 0
        lodsw
        mov ecx eax, D$ebx eax | inc D$ebx
L1:     cmp ecx 0 | je L2>
            lodsw | stosb
            dec ecx
        jmp L1<
L2: mov eax &NULL | stosb
    pop ecx, eax

ret

____________________________________________________________________________________________

[MissingResource: ?]

ReadResource:
; In eax: ptr to resource to read
; In ecx: size of data
; Out eax: Pointer to loaded data

    mov B$MissingResource &FALSE


    If eax < D$UserPeStart
        ;mov B$MissingResource &TRUE | ret
        jmp DisFail
    Else_If eax > D$UserPeEnd
        ;mov B$MissingResource &TRUE | ret
        jmp DisFail
    End_If

    push esi, edi
      ; Allocate space for data+id's strings
        push eax
            push ebx, ecx
                add ecx D$RsrcTypeStringLen
                add ecx D$RsrcNameStringLen
                VirtualAlloc TempoMemPointer ecx
            pop ecx, ebx
        pop esi

      ; Copy data
        mov edi D$TempoMemPointer
        push edi, ecx
            rep movsb
        pop ecx, eax

      ; Copy strings
        mov esi RsrcTypeString, ecx D$RsrcTypeStringLen | rep movsb
        mov esi RsrcNameString, ecx D$RsrcNameStringLen | rep movsb
    pop edi, esi

ret

____________________________________________________________________________________________

NewTemporaryFillRsrcList:
  ; All the resources lists are in format Name/Pointer/Size except:
  ; CustomList, wich is Type/Name/Lang/Pointer/Size
  ; Now, we will fill uRsrcList with format Type/Name/Lang/Pointer/Size
  ; and then will sort it.

    mov edi uRsrcList, D$TypeByName 0

    mov esi CustomList
    If D$esi > 0
        While D$esi > 0
        mov ecx 5 | rep movsd
        End_While
    End_If

    mov esi AviList
    If D$esi > 0
        While D$esi > 0
            mov eax RT_AVI | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi WaveList
    If D$esi > 0
        While D$esi > 0
            mov eax RT_WAVE | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi CursorList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_CURSOR | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi BitMapList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_BITMAP | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

  ; Store default icon if user didn't edit any (or user defined if any):
    If B$NoMainIcon = &FALSE
        mov eax &RT_ICON | stosd | mov eax ID_Icon | stosd | mov eax Language | stosd
        mov eax uIcon | stosd | mov eax uIconEnd | sub eax uIcon | stosd
    End_If

    mov esi IconList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_ICON | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi MenuList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_MENU | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi DialogList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_DIALOG | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi StringsList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_STRING | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi RcDataList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_RCDATA | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    mov esi GroupCursorList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_GROUP_CURSOR | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

    If B$NoMainIcon = &FALSE
        mov eax &RT_GROUP_ICON | stosd | mov eax ID_Group_Icon | stosd
        mov eax Language | stosd
        mov eax uGroupIcon | stosd
        mov eax uGroupIconEnd | sub eax uGroupIcon | stosd
    End_If

    mov esi GroupIconList
    If D$esi > 0
        While D$esi > 0
            mov eax &RT_GROUP_ICON | stosd | movsd | mov eax Language | stosd
            movsd | movsd
        End_While
    End_If

  ; Close this List, because, in case user would have deleted some Resources, there
  ; could be old Records here, from a previous [Compile]:
    mov eax 0, ecx 5 | rep stosd | sub edi (5*4)

  ; -4 > ready for backward read
    sub edi 4 | mov D$uRsrcListPtr edi
____________________________________________________________________________________________
; Sort uRsrcList Table by Type, then by name and then by lang:
    mov esi uRsrcList
    push edi, esi

; First, sort by Type:
L0:     mov edx &FALSE
        .While D$esi > 0
            mov eax D$esi | cmp eax D$esi+20 | je L4>>
            test eax BIT31 | jnz L2>
L1:       ; Type's Id by number:
            cmp eax D$esi+20 | jle L4>>
                jmp L3>
L2:       ; Type's Id by string:
            test D$esi+20 BIT31 | jz L4>>
            push esi
                mov eax esi
                if D$eax = RT_AVI
                    mov esi AviTypeStr
                elseif D$eax = RT_WAVE
                    mov esi WaveTypeStr
                else
                    mov esi D$eax+12 | add esi D$eax+16
                endif
                add eax 20
                if D$eax = RT_AVI
                    mov edi AviTypeStr
                elseif D$eax = RT_WAVE
                    mov edi WaveTypeStr
                else
                    mov edi D$eax+12 | add edi D$eax+16
                endif
                mov ecx 32 | repe cmpsb
                mov al B$esi-1
            pop esi
            cmp al B$edi-1 | jbe L4>
L3:       ; Exchage Resource
            if D$esi+20 <> 0
                Exchange D$esi D$esi+20, D$esi+4 D$esi+24, D$esi+8 D$esi+28,
                    D$esi+12 D$esi+32, D$esi+16 D$esi+36
                mov edx &TRUE
            endif
L4:       ; Next Resource
            add esi 20
        .End_While
     pop esi | push esi
        cmp edx &TRUE | je L0<<

; Then sort by name:
L0:     mov edx &FALSE
        .While D$esi > 0
            mov eax D$esi | cmp eax D$esi+20 | jne L4>>
            mov eax D$esi+4 | cmp eax D$esi+24 | je L4>>
            test eax BIT31 | jnz L2>
L1:       ; Name's Id by number:
            cmp eax D$esi+24 | jle L4>>
                jmp L3>
L2:       ; Name's Id by string:
            test D$esi+24 BIT31 | jz L4>>
            push esi
                mov ebx esi, eax 0
                mov esi D$ebx+12 | add esi D$ebx+16
                mov edi D$ebx+32 | add edi D$ebx+36
                if D$ebx+20 <s 0
                    mov ecx 32 | repne scasb | add esi 32 | sub esi ecx
                endif
                mov ecx 32 | repe cmpsb
                mov al B$esi-1
            pop esi
            cmp al B$edi-1 | jbe L4>
L3:       ; Exchage Resource
            if D$esi+20 <> 0
                Exchange D$esi D$esi+20, D$esi+4 D$esi+24, D$esi+8 D$esi+28,
                    D$esi+12 D$esi+32, D$esi+16 D$esi+36
                mov edx &TRUE
            endif
L4:       ; Next Resource
            add esi 20
        .End_While
   pop esi | push esi
        cmp edx &TRUE | je L0<<

; And finally, sort by lang:
L0:     mov edx &FALSE
        .While D$esi > 0
            mov eax D$esi | cmp eax D$esi+20 | jne L1>
            mov eax D$esi+4 | cmp eax D$esi+24 | jne L1>
          ; Lang's Id are always by number, so:
            mov eax d$esi+8 | cmp eax D$esi+28 | jbe L1>
          ; Exchage Resource
            if D$esi+20 <> 0
                Exchange D$esi D$esi+20, D$esi+4 D$esi+24, D$esi+8 D$esi+28,
                    D$esi+12 D$esi+32, D$esi+16 D$esi+36
                mov edx &TRUE
            endif
L1:       ; Next Resource
            add esi 20
        .End_While
    pop esi | push esi
        cmp edx &TRUE | je L0<

    pop esi, edi


ret

____________________________________________________________________________________________
[BIT31 0_8000_0000]
; Resources Head Line Macro. In: #1: Entries by ID, #2: Entries by name
[RsrcHeadLine | add edi 2 | mov eax #1 | stosw | mov eax #2 | stosw
    sub edi 2 | mov eax 0 | stosd | stosd | stosd]

NewBuildResourceTree:
  ; Initialisation of tree pointers:
    mov eax D$CodeListPtr, D$RsrcHeadPtr eax, D$RsrcTypePtr eax,
      D$RsrcLangPtr eax, D$RsrcPtrPtr eax, D$RsrcSectionPtr eax,
      D$RsrcSectionOrigine eax

  ; Search for the tree size (evaluation begins at second record):
    mov ecx 1                            ; how many resources in ecx
    mov edx 1                            ; how many different resources types in edx
    mov ebx 1                            ; how many langage in ebx
    mov esi uRsrcList+20                 ; start comparisons at second record
L0: mov eax D$esi | cmp eax 0 | je L3>
        inc ecx                          ; count resources
        cmp eax D$esi-20 | je L1>
            inc edx                      ; count different types
            inc ebx | jmp L2>            ; if new type >>> count language
L1: mov eax D$esi+4
      On eax <> D$esi-16, inc ebx        ; same type: different name?>>> count language
L2: add esi 20 | jmp L0<
;;
    down > top:
    - as many ptr record as resources records in 'RsrcList' (ecx * 16)
    - as many lang headers as ebx + as many lang records as ptrs ((ebx * 16)+(ecx * 8))
    - as many type headers as edx + as many types records as lang ((edx * 16)+(ebx*8))
    - records in root as types (16 + (edx * 8))
    >>>  (ecx * 16)+(ebx * 16)+(ecx * 8)+(edx * 16)+(ebx*8)+16+(edx * 8)
    >>>  (ecx * (16+8)) + (ebx * (16+8)) + (edx * (16+8)) + 16
    >>>  ((ecx+ebx+edx) * (16+8)) + 16
;;
L3: add ecx ebx | add ecx edx | mov eax ecx | shl eax 4 | shl ecx 3
    add eax ecx | add eax 16

____________________________________________________________________________________________

    mov ebx uRsrcList | add ebx 12 | mov edi D$CodeListPtr

  ; Clear the header (may bee corrupt by previous use of same memory)
    mov ecx eax, al 0 | rep stosb

   push edi

____________________________________________________________________________________________
  ; Strings for Types ID's by Name
    [RsrcAVIString: U$ 3 'AVI' RsrcWAVEString: U$ 4 'WAVE']
    push ebx
      ; Type ID in uRsrcList
        sub ebx 12
        mov edx 0
        .While D$ebx > 0
          ; If bit 31 = 0 skip resource
            test D$ebx BIT31 | jz L4>>
                cmp edx D$ebx | jne L1>
                  ; If same Type by name that last one, Copy RVA
                    move D$ebx D$ebx-20 | jmp L4>>

L1:       ; New type by name
            mov edx D$ebx

          ; Avi Type
            cmp D$ebx RT_AVI | jne L2>
                mov eax edi, esi RsrcAVIString, ecx 8 | rep movsb
              ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
                sub eax D$RsrcSectionOrigine | or eax BIT31 | mov D$ebx eax
                jmp L4>

          ; Wave Type
L2:         cmp D$ebx RT_WAVE | jne L3>
                mov eax edi, esi RsrcWAVEString, ecx 10 | rep movsb
              ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
                sub eax D$RsrcSectionOrigine | or eax BIT31 | mov D$ebx eax
                jmp L4>

          ; Custom Type
          ; esi points String in uRsrcList
L3:         mov esi D$ebx+12 | add esi D$ebx+16
          ; skip 'String Size' for now
            push edi | add edi 2
                mov eax 0, ecx 0
              ; Put String in Unicode format
                While B$esi <> 0
                    movsb | stosb
                    inc ecx
                End_While
            pop eax
          ; now put 'String Size'
            mov W$eax cx
          ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
            sub eax D$RsrcSectionOrigine | or eax BIT31 | mov D$ebx eax

L4:         add ebx 20
        .End_While
    pop ebx

  ; Strings for Name ID's by Name
    push ebx
        sub ebx 8 ; Name ID in uRsrcList
        mov edx 0
        .While D$ebx > 0
            test D$ebx BIT31 | jnz L1>
              ; bit 31 = 0, skip this resource
                mov edx 0 | jmp L4>
          ; Last Type
L1:         mov eax D$ebx-24
          ; If not same Type that last one jump
            cmp D$ebx-4 eax | jne L2>
                cmp edx D$ebx | jne L2>
                  ; If same ID by name that last one, Copy RVA
                    move D$ebx D$ebx-20 | jmp L4>
L2:       ; New id by name
            mov edx D$ebx
          ; esi points String in uRsrcList
            mov esi D$ebx+8 | add esi D$ebx+12
            test D$ebx-4 BIT31 | jz L3>
              ; Type by name, skip Type string:
                xchg edi esi | mov ecx 32 | mov al 0 | repne scasb | xchg edi esi
          ; skip 'String Size' for now
L3:         push edi | add edi 2
                mov eax 0, ecx 0
              ; Put String in Unicode format
                While B$esi <> 0
                    movsb | stosb
                    inc ecx
                End_While
            pop eax
          ; Now put 'String Size'
            mov W$eax cx
          ; Change ID in uRsrcList to RVA and set the 'ID by name' flag:
            sub eax D$RsrcSectionOrigine | or eax BIT31 | mov D$ebx eax
L4:     add ebx 20
        .End_While
    pop ebx

    Align_On 16 edi
____________________________________________________________________________________________
  ; Write all data resources in .rsrc.
  ; Before action, the pointers in 'uRsrcList' point to each resource's raw data in memory
  ; After action, same pointers point to each resource in .rsrc section:
    While D$ebx > 0
      ; This is the ptr
        mov esi D$ebx
        mov eax edi | sub eax D$RsrcSectionOrigine
      ; change Ptrs in uRsrcList to RVA
        add eax D$uBaseOfRsrc | mov D$ebx eax
      ; Size
        mov ecx D$ebx+4
        rep movsb
        mov eax edi | Align_on 010 eax | mov edi eax
        add ebx 20
    End_While

    mov eax edi | sub eax D$RsrcSectionOrigine | mov D$uRsrcSize eax

    mov eax edi | Align_on 0200 eax
    mov D$CodeListPtr eax
    pop edi | sub edi 4

____________________________________________________________________________________________

  ; Write the tree backward:
____________________________________________________________________________________________
  ; Pointers directory:

  ; end of uRsrcList (> size)
    mov esi D$uRsrcListPtr
  ; BackWard
    std
    Do
      ; write ptrs records
        mov eax 0 | stosd | stosd
      ; size / RVA ptr to true data
        movsd | movsd
      ; adress of record start
        mov eax edi | add eax 4
      ; displacement from start of .rsrc
        sub eax D$RsrcSectionOrigine
      ; ptrs-dir pointer in next level up
        mov D$esi+4 eax
        sub esi 12
    Loop_until D$esi = 0

____________________________________________________________________________________________
  ; Language directory

    mov esi D$uRsrcListPtr
  ; Write Languages dirs
    sub esi 4 | mov ecx 0

    Do
      ; pointers to data Ptrs / Lang
        movsd | movsd
      ; Name
        mov ebx D$esi
      ; records counter for lang header
        inc ecx
      ; Check if there are more languages (same type and name that next resource):
      ; Compare Names
        mov eax D$esi | cmp eax D$esi-20 | jne L1>
      ; Compare Types
        mov eax D$esi-4 | cmp eax D$esi-24 | je L2>
      ; If not equal write headLine
L1:     RsrcHeadLine ecx, 0 | mov ecx 0
        mov eax edi | add eax 4
        sub eax D$RsrcSectionOrigine
      ; node flag
        or eax BIT31
      ; ptrs-dir pointer in next level up
        mov D$esi+8 eax
      ; next ptr record in uRsrcList
L2:     sub esi 12
     Loop_until D$esi = 0

____________________________________________________________________________________________
  ; we do not need any more sizes and Lang values. We do not need any more double
  ; records for Languages. So, we rewrite uRsrcList:
    cld
    push edi
        mov esi uRsrcList, edi uRsrcList
        Do
          ; keep Type / keep ID / skip Lang / keep Ptr / skip size
            movsd | movsd | lodsd | movsd | lodsd
        Loop_Until D$esi = 0
        mov D$edi 0, esi uRsrcList, edi esi
      ; type / ID??? / ptr
L0:     movsd | Lodsd | stosd | movsd
      ; Compare Names
L1:     mov eax D$esi-8 | cmp eax D$esi+4 | jne L2>
      ; Compare Types
        mov eax D$esi-12 | cmp eax D$esi | jne L2>
      ; Skip if equal
        add esi 12 | jmp L1<
L2:
        cmp D$esi 0 | jne L0<
      ; > last record ptr
        mov esi edi | sub esi 4
        mov eax 0 | stosd | stosd | stosd
    pop edi

____________________________________________________________________________________________
  ; Types directory
    std
        mov ecx 0, edx 0
      ; Pointer
L1:     movsd
        lodsd
      ; edx = ID by name count; ecx = ID by number count
        test eax BIT31 | jz L2>
            inc edx
            jmp L3>
L2:         inc ecx
      ; Name / Type
L3:     stosd | lodsd
        cmp eax D$esi-8 | je L4>

        RsrcHeadLine ecx edx | mov ecx 0, edx 0
        mov eax edi | add eax 4
        sub eax D$RsrcSectionOrigine
      ; node flag
        or eax BIT31
      ; ptrs-dir pointer in next level up
        mov D$esi+12 eax
L4:     cmp D$esi 0 | jne L1<

  ; We do not need any more ID. So, we rewrite uRsrcList:
    cld
    push edi
        mov esi uRsrcList, edi uRsrcList
      ; keep Type / skip ID / keep Ptr
L0:     movsd | lodsd | movsd
        cmp D$esi 0 | jne L0<
            mov D$edi 0, esi uRsrcList, edi esi
      ; type??? / ptr
L0:     Lodsd | stosd | movsd
L1:     cmp D$esi eax | jne L2>
            add esi 8 | jmp L1<
L2:     cmp D$esi 0 | jne L0<
        mov esi edi | sub esi 4
        mov eax 0 | stosd | stosd
    pop edi

____________________________________________________________________________________________
  ; Root Directory:
    std
        mov ecx 0, edx 0
      ; Ptr
L1:     movsd
        lodsd
      ; edx = ID by name count; ecx = ID by number count
        test eax BIT31 | jz L2>
            inc edx | jmp L3>
L2:         inc ecx
      ; Type
L3:     stosd
        cmp D$esi 0 | jne L1<
        RsrcHeadLine ecx, edx
    cld

ret


____________________________________________________________________________________________
;[NoResources: ?    NoMainIcon: ?]

BuildRsrc:
    mov B$NoResources &FALSE, B$NoMainIcon &FALSE

    .If D$SavingExtension = '.DLL'
        mov B$NoMainIcon &TRUE
    .Else_If D$SavingExtension = '.SYS'
        mov B$NoMainIcon &TRUE
    .Else_If W$SubSystem = 3             ; Console > no Main Icon
        mov B$NoMainIcon &TRUE
    .End_If


  ; THIS LINE SHOULD BE COMMENTED FOR TESTING WITH RsrcSTUB:
    call NewTemporaryFillRsrcList

  ; THIS LINE SHOULD BE UNCOMMENTED FOR TESTING WITH RsrcSTUB:
  ;  mov esi RsrcStub, edi uRsrcList, ecx D$RsrcStubLen | rep movsd | sub edi 4 | mov D$uRsrcListPtr edi


    If D$uRsrcList = 0
        mov B$NoResources &TRUE
    Else
        call NewBuildResourceTree
    End_If
ret


; TESTING STUB
; This will serve until we create a Resources menu item to add Custom Type Resources
[RsrcStub:
;  Type         Name        Lang    Pointer         Size
   BIT31+10     BIT31       02c0a   RsrcData1       4
   BIT31+10     BIT31       040c    RsrcData2       4
   BIT31+10     BIT31+1     02c0a   RsrcData3       4
   BIT31+11     1           0409    RsrcData4       4
   100          BIT31       0409    RsrcData5       4
   100          BIT31+1     0409    RsrcData6       4
   200          1           02c0a   RsrcData7       4
RsrcStubLen: 35]; Resources * 5

[RsrcData1:  D$01111_1111 B$'TypeB' 0 'NameB' 0
 RsrcData2:  D$02222_2222 B$'TypeB' 0 'NameB' 0
 RsrcData3:  D$03333_3333 B$'TypeB' 0 'NameA' 0
 RsrcData4:  D$04444_4444 B$'TypeA' 0
 RsrcData5:  D$05555_5555 B$'NameC' 0
 RsrcData6:  D$06666_6666 B$'Abcd' 0
 RsrcData7:  D$07777_7777]



