TITLE Optimize
____________________________________________________________________________________________

;;
  Release 2: Betov November 2006.
  _______________________________
    SetShortenJmpFlag
    
    SetJMPShortenJmpFlag
  
    JmpsOptimize
        ScanShortenJmpsTable
            InitScanShortenJmpsTable
            TryToShortenLong
                ShortenLongDown
                ShortenLongUp
        CompactLabelListAndCodeRef
            InitLabelListScanDwordPointer
            AdjustAboveListPointers
            AdjustLastListPointers
        CompactCodeList
        CompactShortenJmpsTable
       
        ScanCouples
            InitScanShortenJmpsTable
            IsJumpDownOrUp
            CouldBothBeShorten
                GetDisplacementDown
                GetDisplacementUp
            SetBothShort
  _______________________________
  
  Process:
  
  * A 'ShortenJmpsTable' Map is created by a cal from 'AsmMain': Empty Table,
    representing the Code area.
  
  * Jcc Encodage (above 'JMPmnemo') calls for 'SetShortenJmpFlag': 'LONG_Jcc' at the
    matching Byte of the 'ShortenJmpsTable'.
     
    JMP Encodage ('JMPmnemo') calls for 'SetJMPShortenJmpFlag': 'LONG_JMP' at the
    matching Byte of the 'ShortenJmpsTable'.
     
    Case of Plain Labels are not considered: They are always Long in RosAsm Syntax
    and the Jumps Sizes apply to Local Labels only.
  
  * After the Encodage, a "Long to Short" Optimization is computed, here.
;;
____________________________________________________________________________________________

[ShortenJumpsWanted: &FALSE]

[ShortenJmpsTable: ?   CodeListOrigine: ?]

InitShortenJmpsTable:
    On D$ShortenJmpsTable <> 0, call ReleaseShortenJmpsTable

    VirtualAlloc ShortenJmpsTable, D$SourceLen

    move D$CodeListOrigine D$CodeListPtr
ret


ReleaseShortenJmpsTable:
    VirtualFree D$ShortenJmpsTable
ret
____________________________________________________________________________________________

;;
  Called from 'Encode' // 'Letter_J' // Cases of Jcc and JMP (DownLong or UpLong only):
  
  The Plain Labels are not considered (always long in RosAsm Syntax).
;;

[EndOfShortenJmpsTable: ?]

[LONG_JMP 1, LONG_Jcc 2
 SHORTEN 3
 SHORTEN_LONG_JMP 4, SHORTEN_LONG_Jcc 5
 ALIGN_FLAG 010]

SetShortenJmpFlag:
    push edi
        sub edi D$CodeListOrigine | add edi D$ShortenJmpsTable | add edi 2
        mov B$edi LONG_Jcc, D$EndOfShortenJmpsTable edi
    pop edi
    inc D$NumberOfLongToShort
ret


SetJMPShortenJmpFlag:
    push edi
        sub edi D$CodeListOrigine | add edi D$ShortenJmpsTable | add edi 1
        mov B$edi LONG_JMP, D$EndOfShortenJmpsTable edi
    pop edi
    inc D$NumberOfLongToShort
ret


[AlignFound: ?]

SetAlignFlag:
    mov B$AlignFound &TRUE
ret
    push edi
        sub edi D$CodeListOrigine | add edi D$ShortenJmpsTable
        mov B$edi ALIGN_FLAG, D$EndOfShortenJmpsTable edi

        mov ecx D$imm32
    pop edi
ret
____________________________________________________________________________________________

[CodeRefScan: ?  NumberOfLongToShort: ?  JumpSizeStat: ?]

;;
  On RosAsm itself, the Optimization adds 7% to the Compile time, and resolves
  7,619 Long to Short. The main "While" is run 4 times.
;;

JmpsOptimize:
    mov eax D$CodeListPtr | dec eax

    call ScanShortenJmpsTable
    mov eax D$NumberOfLongToShort, D$JumpSizeStat eax

    While D$NumberOfLongToShort <> 0
L1:     call CompactLabelListAndCodeRef

        call CompactCodeList

        call CompactShortenJmpsTable

        call ScanShortenJmpsTable
        mov eax D$NumberOfLongToShort | add D$JumpSizeStat eax
    End_While

    call ScanCouples
    mov eax D$NumberOfLongToShort | add D$JumpSizeStat eax
    On D$NumberOfLongToShort <> 0, jmp L1<
ret
____________________________________________________________________________________________

[LongInstruction: ?] ; will be either 'LONG_JMP' or 'LONG_Jcc'

InitScanShortenJmpsTable:
    mov ebx D$CodeRef | add ebx 5

  ; Case of Api Calls: ...|0FF dWordCodeAddress|:
  ;                   ....|.....|
    While B$ebx = 0FF | add ebx 6 | End_While

    mov D$CodeRefName ebx

    While B$ebx > EOI | inc ebx | End_While | inc ebx

    mov D$CodeRefScan ebx
  ; 'CodeRefScan' points to the first Char of a LabelName, in 'CodeRef'.

    mov D$NumberOfLongToShort 0

    call InitLabelListScanDwordPointer

    mov esi D$ShortenJmpsTable
ret
____________________________________________________________________________________________

[CodeRefName: ?  CodeRefName1: ?  CodeRefName2: ?]

ScanShortenJmpsTable:
;;
  Job: The 'ShortenJmpsTable' has been filled with 'LONG_JMP' or 'LONG_Jcc' at each
  location of a relative jump address (parallel to 'CodeListOrigine'). If JUMPs can
  be shorten, it turns them into 'SHORTEN_LONG_JMP' or 'SHORTEN_LONG_Jcc'.
  
  Calls for: 'TryToShortenLong' >>> 'ShortenLongDown' and 'ShortenLongUp'
;;

    call InitScanShortenJmpsTable

  ; Scanning 'ShortenJmpsTable' 0 and 1 (esi ---> 'ShortenJmpsTable'):
L0: mov edx D$EndOfShortenJmpsTable

    While B$esi = 0
        inc esi | cmp esi edx | ja L9>>
    End_While

  ; Here we have a Long Displacement that will be computed in 'FillCodeSymbols'.
    mov al B$esi, B$LongInstruction al ; Either 'LONG_JMP' or 'LONG_Jcc'

  ; Translate into a 'CodeList' Pointer:
    mov eax esi | sub eax D$ShortenJmpsTable | add eax D$CodeListOrigine

  ; Scan CodeRef:

  ; >>> Two Pointers (One in CodeList -eax-, and one in CodeRef -ecx-):
L1: mov ecx D$ebx | and ecx (not relativeFlag)

    ..If ecx = eax
      ; Found matching Record, in CodeRef. Take a copy of the Name in 'CodeRefName':
        mov eax ebx | sub eax 5 | move D$CodeRefName D$eax

            call TryToShortenLong

            .If B$CanBeShorten = &TRUE
                If B$LongInstruction = LONG_JMP
                    mov B$esi SHORTEN_LONG_JMP
                Else_If B$LongInstruction = LONG_Jcc
                    mov B$esi SHORTEN_LONG_Jcc
                End_If

              ; For the lasting 'FillCodeSymbols' to know it is a special "Short":
                mov B$ebx-2 '.'
            .End_If

          ; Next CodeRef Record:
            add ebx 9 | inc esi
          ; Skip the Api calls:
            While B$ebx = 0FF | add ebx 6 | End_While
          ; Skip the "labelName|":
            While B$ebx > EOI | inc ebx | End_While | inc ebx

            jmp L0<<

        ..Else_If ecx <> 0
          ; Next CodeRef Record::
            add ebx 9
          ; Skip the Api calls:
            While B$ebx = 0FF | add ebx 6 | End_While
          ; Skip the "labelName|":
            While B$ebx > EOI | inc ebx | End_While | inc ebx

            jmp L1<<

        ..End_If
L9: ret
____________________________________________________________________________________________

[FirstDownCodeRefScan: ?  SecondUpCodeRefScan: ?]
[FirstDownShortenJmp: ?  SecondUpShortenJmp: ?]

ScanCouples:
    call InitScanShortenJmpsTable
  ; >>> ebx ---> 'CodeRefScan' ---> 'CodeRef'.
  ; >>> esi ---> 'ShortenJmpsTable'.
    mov edx D$EndOfShortenJmpsTable

    .While esi <= edx
        ...If B$esi <> 0
            push esi, edx
            call IsJumpDownOrUp

            ..If B$JumpIs = DOWN
                mov D$CodeRefScan eax
                mov D$FirstDownCodeRefScan eax, D$FirstDownShortenJmp esi
                move W$CodeRefName1 W$eax-5
                mov ebx esi | add esi 4
                push esi, edx
                    mov eax esi | add eax 133-4 | On eax < edx, mov edx eax

                    While esi <= edx
                        .If B$esi <> 0
                            call IsJumpDownOrUp ; >>> eax ---> 'CodeRefScan'

                            If B$JumpIs = UP
                                mov D$SecondUpCodeRefScan eax
                                move W$CodeRefName2 W$eax-5
                                mov D$SecondUpShortenJmp esi
                                call CouldBothBeShorten
                                On eax = &FALSE, jmp L2>
                                call SetBothShort | jmp L4>
                            End_If

                        .End_If

L2:                     inc esi
                    End_While
L4:             pop edx, esi

            ..End_If
            pop edx, esi
        ...End_If

L5:     inc esi
    .End_While
ret
____________________________________________________________________________________________

[JumpIs: ?]

[Down 1, Up 2]

IsJumpDownOrUp:
    push ebx, ecx
        mov eax esi | sub eax D$ShortenJmpsTable | add eax D$CodeListOrigine
        mov ebx D$CodeRefScan

    ; >>> Two Pointers (One to CodeList -eax-, and one to CodeRef -ecx-):
L1:     mov ecx D$ebx | and ecx (not relativeFlag)

        .If ecx = eax
            If W$ebx-3 = '>>'
                mov B$JumpIs DOWN
                mov eax ebx

            Else_If W$ebx-3 = '<<'
                mov B$JumpIs UP
                mov eax ebx

            Else
                mov B$JumpIs 0

            End_If

        .Else_If ecx = 0
            mov B$JumpIs 0

        .Else
          ; Next Record in 'CodeRef':
            add ebx 9
          ; No API call:
            While B$ebx = 0FF | add ebx 6 | End_While
          ; Skip the "labelName|":
            While B$ebx > EOI | inc ebx | End_While | inc ebx | jmp L1<<

        .End_If

L9: pop ecx, ebx
ret


CouldBothBeShorten:
;;
  When reducing from (5 or 6) to 2, we reduce of (3 or 4)
  From Flag to Label:
  
  Down: (07F): 1 + 127 +(3 or 4) =
                129  +    3 or 4      = 132 or 133
                
  Up:   (080): 128 -(1 or 0) +(3 or 4) =
                128 or 129     3 or 4    = 130, 131, 132
;;

  ; In between Distance, in the 'ShortenJmpsTable' Table:
    push ebx, ecx, edx
        mov ebx D$FirstDownShortenJmp, edx D$SecondUpShortenJmp
        mov ecx edx | sub ecx ebx

        mov eax &FALSE

        If B$ebx = LONG_Jcc
            mov ecx 135 | On B$edx = LONG_JMP, dec ecx
        Else
            mov ecx 134 | On B$edx = LONG_Jcc, inc ecx
        End_If
        inc ecx

        call GetDisplacementDown

        .If B$CanBeShorten = &TRUE
            If B$edx = LONG_Jcc
                mov ecx 132 | On B$ebx = LONG_JMP, dec ecx
            Else
                mov ecx 130 | On B$ebx = LONG_Jcc, inc ecx
            End_If
            inc ecx

            call GetDisplacementUp

            If B$CanBeShorten = &TRUE
                mov eax &TRUE
            Else
                mov eax &FALSE
            End_If

        .Else
            mov eax &FALSE

        .End_If

L9: pop edx, ecx, ebx
ret
____________________________________________________________________________________________

SetBothShort:
    mov eax D$FirstDownShortenJmp

    If B$eax = LONG_JMP
        mov B$eax SHORTEN_LONG_JMP
    Else
        mov B$eax SHORTEN_LONG_Jcc
    End_If

    mov eax D$SecondUpShortenJmp

    If B$eax = LONG_JMP
        mov B$eax SHORTEN_LONG_JMP
    Else
        mov B$eax SHORTEN_LONG_Jcc
    End_If

    mov eax D$FirstDownCodeRefScan, B$eax-2, '.'
    mov eax D$SecondUpCodeRefScan,  B$eax-2, '.'

    add D$NumberOfLongToShort 2
ret
____________________________________________________________________________________________

[CanBeShorten: ?, ScanSize: ?]

TryToShortenLong:
    pushad
;;
  ecx is a Pointer to CodeRef, to a Long jmp Displacement. Could it be Short?
  
  1) Is it Long Up or Long Down? >>> Read the '<<' or '>>', at D$CodeRefScan
;;
        mov B$CanBeShorten &FALSE

        If W$CodeRefName+2 = '>>'
            mov D$ScanSize (07F+4+1)
;;
  '+4' is the size of the long jump Displacement (We are pointing to the first Byte ot it).
  Either B$LongInstruction = LONG_Jcc, or LONG_JMP do not change a thing:
  Just moving it all upward 1 Byte, later, if LONG_Jcc.
  Why '+1'? Because the Processor will start from the Byte after the jump.
;;
            call ShortenLongDown

        Else_If W$CodeRefName+2 = '<<'
            mov D$ScanSize 080
          ; If an Instruction is a Jcc the Opcode length will switch from 2 to 1:
            On B$LongInstruction = LONG_Jcc, inc D$ScanSize

            call ShortenLongUp

        End_If
    popad
ret
____________________________________________________________________________________________

[ScanLabelPointer: ?]

ShortenLongDown:
  ; ebx = D$CodeRefScan >>> Label Name // ecx = 'CodeRef' Pointer to 'CodeList'

  ; Search a Label Declaration Pointer bigger that the CodeRef Evocation Pointer:
  ; eax = 'LabelList' Pointer to 'CodeList' // ecx = 'CodeRef' Pointer to 'CodeList'
  ; We search for a Pointer, in Label List, such as "ecx < D$esi < edx":
    mov esi D$LabelListScanPointer

    While D$esi > ecx
        PreviousLabelListPointer esi
    End_While

    While D$esi < ecx
        NextLabelListPointer esi
    End_While

    mov D$LabelListScanPointer esi

  ; Search the next same Label, in LabelList, as the one pointed by ebx.
  ; Store, for example 'K9', of "K9>>", in ebx:
    mov bx W$CodeRefName
  ; Scan-Down Limit (07F (127) is the limit for positive signed bytes):
    mov edx ecx | add edx D$ScanSize

  ; Search for the matching Label in the matching range, if any:
    While D$esi < edx
      ; Does 'esi-3' point to "K9", in "|K9|....f|" ?
      ; A LabelList Record is: // EOI, LabelName, EOI, Pointer, Flag, // EOI, ...
        .If W$esi-3 = bx
            cmp B$esi-4 EOI | jne L5>
            cmp B$esi-1 EOI | jne L5>
              ; Found a matching Label, that is Long, and that can be made Short:
                mov B$CanBeShorten &TRUE
                inc D$NumberOfLongToShort | jmp L9>

        .Else
L5:         NextLabelListPointer esi

        .End_If

    End_While

    mov B$CanBeShorten &FALSE
L9: ret
____________________________________________________________________________________________

GetDisplacementDown:
;;
  'FirstDownCodeRefScan' has kept track of the Reference. That is, the pointer to Codelist
  found in the CodeRef Table.
  
  We search for a Pointer, in LabelList, that would be bigger than 'FirstDownCodeRefScan',
  with a matching Name.
  
  Abort if bigger than 'FirstDownCodeRefScan' + 133.
;;
    pushad
      ; Go to the first Label closer to 'FirstDownShortenJmp'
        mov esi D$LabelListScanPointer
        mov eax D$FirstDownCodeRefScan, eax D$eax | and eax (not relativeFlag)

        While D$esi > eax
            PreviousLabelListPointer esi ; 'LabelList'
        End_While

        While D$esi < eax
            NextLabelListPointer esi ; 'LabelList'
        End_While
        PreviousLabelListPointer esi
        mov D$LabelListScanPointer esi

      ; Search the next same Label, in LabelList, as the one pointed by ebx.
      ; Store, for example 'K9', of "K9>>", in ebx:
        mov bx W$CodeRefName1
      ; Scan-Down Limit:
        mov edx eax | add edx ecx

      ; Search for the matching Label in the matching range, if any:
        While D$esi < edx
            .If W$esi-3 = bx
                cmp B$esi-4 EOI | jne L5>
                cmp B$esi-1 EOI | jne L5>
                  ; Found a matching Label, that is Long, and that could be made Short:
                    mov B$CanBeShorten &TRUE | jmp L9>

            .Else
L5:             NextLabelListPointer esi

            .End_If

        End_While

        mov B$CanBeShorten &FALSE
L9: popad
ret
____________________________________________________________________________________________

; A Jump can be short Up if it is > 0FFFF_FF80

ShortenLongUp:
  ; ebx = D$CodeRefScan >>> Label Name // ecx = 'CodeRef' Pointer to 'CodeList'

  ; Search the next same Label, in LabelList, as the one pointed by ebx.
  ; Store, for example 'K9', of "K9>>", in ebx:
    mov bx W$CodeRefName
    mov edx ecx | sub edx D$ScanSize

  ; Search a Label Declaration Pointer smaller that the CodeRef Evocation Pointer:
  ; eax = 'LabelList' Pointer to 'CodeList' // ecx = 'CodeRef' Pointer to 'CodeList'
  ; We search for a Pointer, in Label List, such as "ecx < D$esi < edx":
    mov esi D$LabelListScanPointer

    While D$esi > ecx
        PreviousLabelListPointer esi
    End_While

    While D$esi < ecx
        NextLabelListPointer esi
        On D$esi = 0, jmp L7>
    End_While

L7: PreviousLabelListPointer esi
    mov D$LabelListScanPointer esi

  ; Search for the matching Label in the matching range, if any:
    While D$esi > edx
      ; Does 'esi-7' point to "K9", in "|K9|....f|
      ; A LabelList Record is: // EOI, LabelName, EOI, Pointer, Flag, // EOI, ...
        .If W$esi-3 = bx
            cmp B$esi-4 EOI | jne L5>
            cmp B$esi-1 EOI | jne L5>
          ; Found a matching Label, that is Long, and that can be made Short:
                mov B$CanBeShorten &TRUE
                inc D$NumberOfLongToShort | jmp L9>

        .Else
L5:         PreviousLabelListPointer esi

        .End_If

    End_While

    mov B$CanBeShorten &FALSE
L9: ret
____________________________________________________________________________________________

GetDisplacementUp:
;;
  'FirstDownCodeRefScan' has kept track of the Reference. That is, the pointer to Codelist
  found in the CodeRef Table.
  
  We search for a Pointer, in LabelList, that would be bigger than 'FirstDownCodeRefScan',
  with a matching Name.
;;
    pushad
      ; Go to the first Label closer to 'FirstDownShortenJmp'
        mov esi D$LabelListScanPointer
        mov eax D$SecondUpCodeRefScan, eax D$eax | and eax (not relativeFlag)

        While D$esi < eax
            nextLabelListPointer esi
        End_While

        While D$esi > eax
            PreviousLabelListPointer esi
        End_While
        NextLabelListPointer esi

        mov D$LabelListScanPointer esi

      ; Search the next same Label, in LabelList, as the one pointed by ebx.
      ; Store, for example 'K9', of "K9>>", in ebx:
        mov bx W$CodeRefName2
      ; Scan-Down Limit:
        mov edx eax | sub edx ecx

      ; Search for the matching Label in the matching range, if any:
        mov eax D$FirstDownCodeRefScan
        While D$esi > edx
            If W$esi-3 = bx
                ;On D$esi > eax, jmp L8>
                cmp B$esi-4 EOI | jne L5>
                cmp B$esi-1 EOI | jne L5>
                  ; Found a matching Label, that is Long, and that could be made Short:
                    mov B$CanBeShorten &TRUE | jmp L9>

            Else
L5:             PreviousLabelListPointer esi

            End_If
        End_While

L8:     mov B$CanBeShorten &FALSE
L9: popad
ret
____________________________________________________________________________________________

; LabelListScanPointer points to one of the dWord Pointers, in LabelList:

[LabelListScanPointer: ?]

InitLabelListScanDwordPointer:
  ; Dword | ... // ...| Name | Dword1 Byte | // ....  ;;; 'StoreDataLabel'
  ;    ...|LabelName|....f|LabelName|....f|
    mov esi D$LabelList | add esi 5
L0: While B$esi > EOI | inc esi | End_While | inc esi

    While B$esi+4 < CodeLabelFlag
        NextLabelListPointer esi
    End_While

    mov D$LabelListScanPointer esi
  ; >>> D$LabelListScanPointer points to the dWord (Pointer to CodeList)
ret

[NextLabelListPointer
 add #1 6
 N4: cmp B$#1 EOI | je N5> | cmp D$#1 0 | je N5> | inc #1 | jmp N4<
 N5: inc #1]

[PreviousLabelListPointer
 sub #1 2
 N4: cmp B$#1 EOI | je N5> | cmp #1 LabelList | je N5> | dec #1 | jmp N4<
 N5: sub #1 5]

[CodeRefScanPointer: ?]
____________________________________________________________________________________________

;;
  'SHORTEN_LONG_JMP' or 'SHORTEN_LONG_Jcc' found in 'ShortenJmpsTable':
  
  * add 3 to eax.
  
  * All LabelList References to CodeList, downward this Pos, must be substract
    with eax.
    
  * All CodeRef Pointers to a downward Pos must also be substract with eax.
;;

CompactLabelListAndCodeRef:
    mov ebx D$ShortenJmpsTable, edx D$EndOfShortenJmpsTable

    call InitLabelListScanDwordPointer
  ; >>> esi ---> D$LabelListScanPointer

  ; Edi = Will point to the dWord1 of 'CodeRef':
    mov edi D$CodeRef | add edi 5

  ; Case of Api Calls: ...|0FF dWordCodeAddress|:
  ;                   ....|.....|
    While B$edi = 0FF | add edi 6 | End_While

  ; Pointing to the 'CodeRef' Pointer to 'CodeList';
    While B$edi <> EOI | inc edi | End_While | inc edi

    mov eax 0

    While ebx <= edx
        If B$ebx > SHORTEN
            mov cl B$ebx, B$LongInstruction cl
            call AdjustAboveListPointers
            add eax 3
        End_If

        inc ebx
    End_While

    call AdjustLastListPointers
ret
____________________________________________________________________________________________

AdjustAboveListPointers:
  ; ("Above" means in between the previous access and the actual access).

    push ebx
      ; esi ---> LabelList / edi ---> CodeRef.

      ; ecx = Offset matching the actual Shorten Jump Displacement, in 'CodeList':
        mov ecx ebx | sub ecx D$ShortenJmpsTable | add ecx D$CodeListOrigine

      ; LabelList adjustments of the Pointers to Code:
      ; ("<" because the Label is necessary _before_ the Instruction).
        While D$esi < ecx
            sub D$esi eax
            NextLabelListPointer esi
            On D$esi = 0, jmp L1>
        End_While
;;
  CodeRef adjustments of the Pointers to Code:
  
  2 Problems: 
  
  1) The Records for Api calls are irregular. We just skip over them.
  
  2) The Pointer to 'CodeList', in 'CodeRef' (dWord1) the 'RelativeFlag' is [On].
;;
L1:     mov ebx D$edi | and ebx (not RelativeFlag)
      ; "<=", because the Evocation Reference and the Shorten Jump match exactly:
        .While ebx <= ecx
            .If ebx = ecx
                If B$LongInstruction = SHORTEN_LONG_Jcc
                    inc eax
                End_If
            .End_If
            sub ebx eax
            test D$edi RelativeFlag | jz L2>
                or ebx RelativeFlag
L2:         mov D$edi ebx

          ; Next CodeRef Record:
            add edi 9
          ; Cases of Api calls. Comments in 'FillCodeSymbols'. We just skip over:
            While B$edi = 0FF | add edi 6 | End_While
            While B$edi <> EOI | inc edi
                On D$edi = 0, jmp L9>
            End_While
            inc edi
            mov ebx D$edi | and ebx (not RelativeFlag)
        .End_While

L9: pop ebx
ret
____________________________________________________________________________________________

AdjustLastListPointers:
  ; Same as above 'AdjustAboveListPointers', but just assume the lasting Records:

  ; LabelList adjustments:
    While D$esi <> 0
        sub D$esi eax
        NextLabelListPointer esi
    End_While

  ; CodeRef adjustments:
    .While D$edi <> 0
        mov ebx D$edi | and ebx (not RelativeFlag)
        sub ebx eax | Test D$edi RelativeFlag | jz L1>
            or ebx RelativeFlag

L1:     mov D$edi ebx
      ; Next CodeRef Record:
        add edi 9
      ; Cases of Api calls. Comments in 'FillCodeSymbols'. We just skip over:
        While B$edi = 0FF | add edi 6 | End_While
        While B$edi <> EOI
            inc edi | On D$edi = 0, jmp L9>
        End_While | inc edi
    .End_While
L9: ret
____________________________________________________________________________________________

CompactCodeList:
    mov ebx D$ShortenJmpsTable, edx D$EndOfShortenJmpsTable
    mov esi D$CodeListOrigine, edi esi

    While ebx <= edx
        If B$ebx = SHORTEN_LONG_JMP
          ; jmp Long = 0E9 >>> jmp short = 0EB
            mov B$edi-1 0EB, B$edi 0
            add esi 4 | add ebx 4 | inc edi

        Else_If B$ebx = SHORTEN_LONG_Jcc
          ; Example: B$esi-2 >>> 0F, 084 (JE long) >>> 074 (JE short), 0:
            mov al B$esi-1 | sub al 010 | mov B$edi-2 al, B$edi-1 0
            add esi 4 | add ebx 4

        Else
            movsb | inc ebx

        End_If

    End_While

  ; Copy the Bytes coming after the last modified CodeRef Pointer:
    While esi < D$CodeListPtr | movsb | End_While

  ; Cosmetic clean-up of the trailing Bytes in CodeList:
    push edi
        mov al 0 | While edi < D$CodeListPtr | stosb | End_While
    pop edi
    mov D$CodeListPtr edi
ret
____________________________________________________________________________________________

CompactShortenJmpsTable:
    mov esi D$ShortenJmpsTable, edx D$EndOfShortenJmpsTable
    mov edi esi, eax 0

    While esi <= edx
        If B$esi = SHORTEN_LONG_JMP
            mov B$esi 0 | add esi 3

        Else_If B$esi = SHORTEN_LONG_Jcc
            mov B$esi 0 | add esi 4

        Else
            movsb

        End_If

    End_While

    mov D$EndOfShortenJmpsTable edi
ret
____________________________________________________________________________________________









