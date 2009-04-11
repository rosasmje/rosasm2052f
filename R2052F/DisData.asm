TITLE DisData

;;
  Write all Data and VirtualData, at once, at the Top of Source.

  3Dfun:
  
  [Data0404040: D$ 0 #03
                 D$ Data0404093]
  [<Data040404D: B$ "@@"]
  [<Data040404F: B$ 0]
  [Data0404050: B$ "Software\Chris Dragan\"]
  
  0408290 non flaged Resources 'IMAGE_RESOURCES_DATA_ENTRY'
;;

;;
; Obsolete
WriteAllDisData:
    mov esi D$SectionsMap | add esi D$FirstSection
    mov edx D$EndOfSectionsMap

L0: .While esi < edx
        test B$esi DATAFLAG+VIRTUALFLAG | jz L9>
            mov cl B$esi
            mov ebx esi, eax esi
            sub ebx D$SectionsMap | add ebx D$RoutingMap

L1:         inc ebx | inc esi | cmp esi edx | jae L2>
            If B$esi = cl
                test B$ebx LABEL+EVOCATED | jz L1<
            End_If

L2:         sub eax D$SectionsMap | add eax D$SizesMap
            sub ebx D$RoutingMap | add ebx D$SizesMap

            If cl = VIRTUALFLAG
                call WriteOneVirtualDataChunk, eax, ebx
            Else
                call WriteOneDataChunk eax, ebx
            End_If

            jmp L0<

L9:     inc esi
    .End_While
ret
;;

Proc WriteOneDataChunksAsFound:
    Argument @Section
    Local @ChunkSize, @Type

      ; Reset the output to the start of Line (first row):
        While B$edi-1 <= ' ' | dec edi | End_While
        mov D$edi CRLF2 | add edi 4

      ; esi points into 'UserPeStart'.

      ; Consider DATA or VIRTUALDATA Flags, in SectionsMap:
        mov eax D@Section, edx eax, cl B$eax, D@ChunkSize 0, B@Type cl

        While B$edx = cl
            inc edx | inc D@ChunkSize | On edx = D$EndOfSectionsMap, jmp L0>
        End_While
;;
  This 'ChunSize' is the whole size of a 'Chunk'; that may hold several
  Labels, and so forth, several "[Chunk: XXXX]".
  
  'eax:edx' is now the 'Start:End' of the whole 'Chunk', inside 'SectionsMap'.
;;
L0:     .While eax < edx
            mov ebx eax | sub ebx D$SectionsMap | add ebx D$RoutingMap
            mov ecx edx | sub ecx D$SectionsMap | add ecx D$RoutingMap

L1:         inc ebx | cmp ebx ecx | jae L2>
            test B$ebx LABEL+EVOCATED | jz L1<

L2:         sub eax D$SectionsMap | add eax D$SizesMap
            sub ebx D$RoutingMap | add ebx D$SizesMap

          ; call BuildCommentedDataReference eax ; made by Guga

            push ebx, edx
                If B@Type = VIRTUALFLAG
                    call WriteOneVirtualDataChunk, eax, ebx
                Else
                    call WriteOneDataChunk eax, ebx
                End_If
            pop edx, eax

            sub eax D$SizesMap | add eax D$SectionsMap
        .End_While

        add esi D@ChunkSize
EndP


[NextDataOutputBreak: ?]
;;
  Here, 'Chunk' stands for 'Chunk from Label to Label'.
;;

[NextDataChunkStart: ?]

; Writes one Data Chunk, in between two 'LABEL+EVOCATED':

Proc WriteOneDataChunk:
    Arguments @SizesmapStart, @SizesmapEnd
    Uses esi, edx

        InitDataLineBreak | mov B$edi '[' | inc edi

        test D@SizesmapStart 00_11 | jz L0>
            mov eax D@SizesmapStart
            If eax = D$NextDataChunkStart
                mov B$edi '<' | inc edi
            Else
                mov D$edi '<2 ' | add edi 3
            End_If

L0:     test D@SizesmapStart 00_1111 | jnz L0>
                mov D$edi '<16 ' | add edi 4

L0:     mov eax D@SizesmapStart | sub eax D$SizesMap | add eax D$SectionsMap
        call WriteOneDataLabel eax

        mov esi D@SizesmapStart, edx D@SizesmapEnd, cl B$esi
        mov D$NextDataChunkStart edx

        mov B$RepetitiveBytesDone &FALSE | call DisDataTypeRouter

        While B$edi-1 <= ' ' | dec edi | End_While
        On B$edi-1 = ',' dec edi

        mov B$edi ']', W$edi+1 CRLF | add edi 3
EndP


[ActualSizeFlag: ?  LastSizeFlag: ?  RealDataChunkEdx: ?]

DisDataTypeRouter:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag

 ; mov eax esi | sub eax D$SizesMap | add eax D$DisImageBase
 ; On eax = 0403000, int3 ;map

    mov B$ActualSizeFlag cl, D$RealDataChunkEdx edx

    call AlignSizeOn ecx

    If edx = esi
        mov edx D$RealDataChunkEdx
        mov cl BYTE, B$ActualSizeFlag cl | call WriteDisBytes | jmp L9>>
    End_If

    On cl = 0, mov cl BYTE

    .If cl = BYTE
        mov eax edx | sub eax esi | and eax 00_11
        If eax = 00_10
            mov cl WORD
        Else_If eax = 00_00
            mov cl DWORD
        End_If
    .End_If

    .If cl = BYTE
        call WriteDisBytes
    .Else_If cl = WORD
        call WriteDisWords
    .Else_If cl = DWORD
        call WriteDisPointers ; WriteDisdWords
    .Else_If cl = POINTER
        call WriteDisPointers
    .Else_If cl = POINTER+DWORD
        call WriteDisPointers
    .Else_If cl = STRINGS+BYTE
        call WriteDisAscii
    .Else_If cl = STRINGS+WORD
        call WriteDisUnicode
    .Else_If cl = FP4
        call WriteDisFP4
    .Else_If cl = FP8
        call WriteDisFP8
    .Else_If cl = FP10
        call WriteDisFP10
    .Else_If cl = FP4+POINTER
        call WriteDisFP4
    .Else_If cl = FP8+POINTER
        call WriteDisFP8
    .Else_If cl = FP10+POINTER
        call WriteDisFP10
    .Else_If cl = STRINGS+BYTE+POINTER
        mov cl STRINGS+BYTE, B$ActualSizeFlag cl
        call WriteDisAscii
    .Else_If cl = STRINGS+WORD+POINTER
        mov cl STRINGS+WORD, B$ActualSizeFlag cl
        call WriteDisUnicode
    .Else_If cl = STRINGS+POINTER
        mov cl STRINGS+BYTE, B$ActualSizeFlag cl
        call WriteDisAscii
    .Else
        test cl STRINGS | jz L1>
            If B$LastSizeFlag = STRINGS+BYTE
                mov cl STRINGS+BYTE, B$ActualSizeFlag cl | call WriteDisAscii
            Else_If B$LastSizeFlag = STRINGS+WORD
                mov cl STRINGS+WORD, B$ActualSizeFlag cl | call WriteDisUnicode
            Else
                mov cl STRINGS+BYTE, B$ActualSizeFlag cl | call WriteDisAscii
            End_If
            jmp L5>>

L1:     mov eax edx | sub eax esi
        cmp eax 4 | jb L1>
            call IsPointerCandidate

            If eax = &TRUE
                mov cl POINTER, B$ActualSizeFlag cl
                call AlignSizeOn ecx
                call WriteDisPointers | jmp L5>>
            End_If

L1:   ; edx (end) has been aligned: Unalign:
        mov edx D$RealDataChunkEdx | call IsStringCandidate

        If eax = &TRUE
           ; mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
           ; push esi, edx
                mov cl STRINGS+BYTE, B$ActualSizeFlag cl | call WriteDisAscii
           ; pop edx, esi
           ; mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
        Else
            mov cl BYTE, B$ActualSizeFlag cl | call WriteDisBytes
        End_If

    .End_If

  ; Trailing Bytes may remain there because of the above call to 'AlignSizeOn'. So:
L5: .If esi < D$RealDataChunkEdx
        mov edx D$RealDataChunkEdx
        mov W$edi '  ' | add edi 2
        mov eax edx | sub eax esi
        If eax < 4
            mov cl BYTE, B$ActualSizeFlag cl | call WriteDisBytes
        Else
            jmp DisDataTypeRouter
        End_If
    .End_If

L9: mov cl B$ActualSizeFlag, B$LastSizeFlag cl
ret


Proc WriteOneDataLabel: ; 'WriteDisCodeLabel', 'WriteExportedFunctionLabel'
; called by 'WriteOneDataChunk' and 'WriteOneVirtualDataChunk'
    Argument @SectionsMapPtr

        mov eax D@SectionsMapPtr | sub eax D$SectionsMap | add eax D$RoutingMap

        Test B$eax EXPORTNODE | jz L1>  ; 'CheckExport'
            mov ebx eax, eax D@SectionsMapPtr
            sub eax D$SectionsMap | add eax D$DisImageBase
            call WriteExportedFunctionLabel

L1:     mov eax D@SectionsMapPtr

        If B$eax = DATAFLAG
            mov D$edi 'Data' | add edi 4
        Else_If B$eax = VIRTUALFLAG
            mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
        End_If

        sub eax D$SectionsMap | add eax D$DisImageBase
        push eax
            call WriteEax
        pop eax

        ToStringsMapFrom DisImageBase, eax
        If D$eax <> 0
            push esi
                zCopy D$eax
            pop esi
        End_If

        mov W$edi ': ' | add edi 2
EndP


Proc AlignSizeOn:
    Argument @Unit
    Uses ecx

      ; esi = SizesmapStart, edx = SizesmapEnd
        mov ecx D@Unit | and ecx (not STRINGS)

        .If cl = POINTER
            mov ecx 4
        .Else
            and ecx (not POINTER)
            If cl = 0
                mov ecx BYTE | ExitP
            Else_If cl = BYTE
                ExitP
            Else_If cl = WORD
                mov ecx 2
            Else_If cl = DWORD
                mov ecx 4
            Else_If cl = FP4
                mov ecx 4
            Else_If cl = FP8
                mov ecx 8
            Else_If cl = FP10
                mov ecx 10
            End_If
        .End_If

        mov eax edx | sub eax esi
        mov edx 0 | div ecx | mul ecx
        mov edx esi | add edx eax
EndP


[RepetitiveBytesDone: ?  NumberOfRepetitiveData: ?]

IsRepetitiveBytes:
    If B$RepetitiveBytesDone = &TRUE
        mov D$NumberOfRepetitiveData 0 | ret
    End_If

    push esi, ecx, edx
        sub edx D$SizesMap | add edx D$UserPeStart

        mov esi ebx, al B$esi, ecx 0

        While B$esi = al
            lodsb | inc ecx | cmp esi edx | jae L2>>
        End_While

L2:     If ecx > 1
            mov D$NumberOfRepetitiveData ecx
        Else
            mov D$NumberOfRepetitiveData 0
        End_If

    pop edx, ecx, esi

    mov B$RepetitiveBytesDone &TRUE
ret


IsRepetitiveWords:
    If B$RepetitiveBytesDone = &TRUE
        mov D$NumberOfRepetitiveData 0 | ret
    End_If

    push esi, ecx, edx
        sub edx D$SizesMap | add edx D$UserPeStart

        mov esi ebx, ax W$esi, ecx 0

        While W$esi = ax
            lodsw | inc ecx | cmp esi edx | jae L2>>
        End_While

L2:     If ecx > 1
            mov D$NumberOfRepetitiveData ecx
        Else
            mov D$NumberOfRepetitiveData 0
        End_If

    pop edx, ecx, esi

    mov B$RepetitiveBytesDone &TRUE
ret


IsRepetitivedWords:
    If B$RepetitiveBytesDone = &TRUE
        mov D$NumberOfRepetitiveData 0 | ret
    End_If

    push esi, ecx, edx
        sub edx D$SizesMap | add edx D$UserPeStart

        mov esi ebx, eax D$esi, ecx 0

        While D$esi = eax
            lodsd | inc ecx | cmp esi edx | jae L2>>
        End_While

L2:     If ecx > 1
            mov D$NumberOfRepetitiveData ecx
        Else
            mov D$NumberOfRepetitiveData 0
        End_If

    pop edx, ecx, esi

    mov B$RepetitiveBytesDone &TRUE
ret


WriteDisBytes: ; 'WriteBytesData', 'WriteAsciiData'
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3
    mov D$edi 'B$ ' | add edi 3

    mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

    call IsRepetitiveBytes

    If D$NumberOfRepetitiveData > 0
        movzx eax B$ebx | push ebx | call WriteEax | pop ebx
        mov W$edi ' #' | add edi 2
        mov eax D$NumberOfRepetitiveData | push ebx | call WriteEax | pop ebx
        add esi D$NumberOfRepetitiveData
        add ebx D$NumberOfRepetitiveData | cmp esi edx | jae L9>
        call NextDisDataLine
    End_If

L0: movzx eax B$ebx | push ebx | call WriteEax | pop ebx

    inc esi | inc ebx | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                call NextDisDataLine
                mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

    jmp L0<
L9: ret


WriteDisWords:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
   On B$edi-2 = '$', sub edi 3
    mov D$edi 'W$ ' | add edi 3

    mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

    call IsRepetitiveWords

    If D$NumberOfRepetitiveData > 0
        movzx eax W$ebx | push ebx | call WriteEax | pop ebx
        mov W$edi ' #' | add edi 2
        mov eax D$NumberOfRepetitiveData | push ebx | call WriteEax | pop ebx
        mov eax D$NumberOfRepetitiveData | shl eax 1
        add esi eax | add ebx eax | cmp esi edx | jae L9>
        call NextDisDataLine
    End_If

L0: mov eax edx | sub eax esi
    If eax < 2
        mov cl BYTE | ret
    End_If

    movzx eax W$ebx | push ebx | call WriteEax | pop ebx

    add esi 2 | add ebx 2 | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                call NextDisDataLine
                mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

    jmp L0<
L9: ret


WriteDisdWords:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3
    mov D$edi 'D$ ' | add edi 3

    mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

    call IsRepetitivedWords

    If D$NumberOfRepetitiveData > 0
        mov eax D$ebx | push ebx | call WriteEax | pop ebx
        mov W$edi ' #' | add edi 2
        mov eax D$NumberOfRepetitiveData | push ebx | call WriteEax | pop ebx
        mov eax D$NumberOfRepetitiveData | shl eax 2
        add esi eax | add ebx eax | cmp esi edx | jae L9>>
        call NextDisDataLine
    End_If

L0: mov eax D$ebx | push ebx | call WriteEax | pop ebx

    add esi 4 | add ebx 4 | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                call NextDisDataLine
                mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

    jmp L0<<
L9: ret


[WasValidPointer: ?]

WriteDisPointers:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3
    mov D$edi 'D$ ' | add edi 3

    mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

L0: mov eax edx | sub eax esi
    If eax < 4
        mov cl BYTE | ret
    End_If

    mov eax D$ebx

    ;sub eax D$DisImageBase | add eax D$RoutingMap
    ;test B$eax LABEL | jz WriteDisdWords
    ;mov eax D$ebx
;;
  This case of zeroed POINTERs should be turned DWORDs, in 'CheckFlagsCoherency'
;;
    .If eax = 0
        call IsRepetitivedWords

        If D$NumberOfRepetitiveData > 0
            mov eax D$ebx | push ebx | call WriteEax | pop ebx
            mov W$edi ' #' | add edi 2
            mov eax D$NumberOfRepetitiveData | push ebx | call WriteEax | pop ebx
            mov eax D$NumberOfRepetitiveData | shl eax 2
            add esi eax | add ebx eax | cmp esi edx | jae L9>>
            call NextDisDataLine | jmp L0<
        End_If

    .End_If

    mov B$RepetitiveBytesDone &TRUE

    push eax
        sub eax D$DisImageBase | add eax D$SectionsMap
        ..If eax > D$SectionsMap
            .If eax < D$EndOfSectionsMap
                sub eax D$SectionsMap | add eax D$RoutingMap
                test B$eax LABEL+INSTRUCTION+EVOCATED | jz L2>
L1:             sub eax D$RoutingMap | add eax D$SectionsMap
                If B$eax = CODEFLAG
                    sub eax D$SectionsMap | add eax D$RoutingMap
                    test B$eax LABEL+INSTRUCTION | jz L2>
                    mov D$edi 'Code' | add edi 4
                Else_If B$eax = DATAFLAG
                    mov D$edi 'Data' | add edi 4
                Else_If B$eax = VIRTUALFLAG
                    mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                End_If
            .End_If
        ..End_If
L2: pop eax

    .If D$edi-4 = 'tual'
        mov B$WasValidPointer &TRUE
    .Else_If D$edi-4 = 'Data'
        mov B$WasValidPointer &TRUE
    .Else_If D$edi-4 = 'Code'
        mov B$WasValidPointer &TRUE
    .Else
        mov B$WasValidPointer &FALSE
        push eax
            mov edx D$RealDataChunkEdx | call IsStringCandidate
            If eax = &TRUE
                pop eax
                mov cl STRINGS+BYTE, B$ActualSizeFlag cl | call WriteDisAscii | ret
            End_If
        pop eax
    .End_If

    push ebx, eax | call WriteEax | pop eax, ebx

    ..If B$WasValidPointer = &TRUE
        ToStringsMapFrom DisImageBase, eax
        .If D$eax <> 0
            push esi
                If D$eax = MainWindowProcName
                    While D$edi <> 'Code' | dec edi | End_While
                End_If
                zCopy D$eax
            pop esi
        .End_If
    ..End_If

    add esi 4 | add ebx 4 | cmp esi edx | jae L9>

        .If B$esi <> 0
            If B$esi <> cl
                call NextDisDataLine
                mov cl B$esi | ret
            End_If
        .End_If

        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

    jmp L0<<
L9: ret


[InsideQuotes: ?]

WriteDisAscii: ; 'WriteAsciiData'
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    mov B$RepetitiveBytesDone &TRUE
    On B$edi-2 = '$', sub edi 3
    mov D$edi 'B$ ' | add edi 3
    mov B$InsideQuotes &FALSE

    mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

L0: movzx eax B$ebx
;On D$ebx = '3D F', int3
    push ebx
        mov ebx D$TruthAsciiTable

        ...If B$ebx+eax = GOODASCII
;;
  Isolated cases of 13 or 10 might be miss-interpretated as part of a CRLF:
;;
            If al = CR
                pop ebx | push ebx
                On B$ebx <> LF, jmp L2>
            Else_If al = LF
                pop ebx | push ebx
                On B$esi-2 <> CR, jmp L2>
            End_If

            On al = '"', jmp L2>

            If B$InsideQuotes = &FALSE
                mov B$edi '"' | inc edi
                mov B$InsideQuotes &TRUE
            End_If
            stosb

        ...Else
L2:         If B$InsideQuotes = &TRUE
                mov B$edi '"' | inc edi
                mov B$InsideQuotes &FALSE
            End_If
            .If W$edi-2 <> ', '
                If W$edi-3 <> 'B$'
                    mov W$edi ', ' | add edi 2
                End_If
            .End_If
            call WriteEax
            inc esi
            ..If esi < edx
                .If W$edi-2 <> ', '
                    If W$edi-3 <> 'B$'
                        mov D$edi ', ' | add edi 2
                    End_If
                .End_If
            ..End_If
            dec esi
        ...End_If
    pop ebx

    inc esi | inc ebx | cmp esi edx | jae L9>

        ..If B$esi <> 0
            .If B$esi <> cl
                If B$InsideQuotes = &TRUE
                    mov B$edi '"' | inc edi
                    mov B$InsideQuotes &FALSE
                End_If
                call NextDisDataLine
                mov cl B$esi | ret
            .End_If
        ..End_If

        jmp L0<<

L9: If B$InsideQuotes = &TRUE
        mov B$edi '"' | inc edi
        mov B$InsideQuotes &FALSE
    End_If
ret


WriteDisUnicode:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    mov B$RepetitiveBytesDone &TRUE
    On B$edi-2 = '$', sub edi 3
    mov D$edi 'U$ ' | add edi 3
    mov B$InsideQuotes &FALSE

    mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart

L0: movzx eax B$ebx

    push ebx, ecx
        mov ecx ebx, ebx D$TruthAsciiTable
        ..If B$ebx+eax = GOODASCII
            On al = '"', jmp L2>
            If al = CR
                On B$ecx+2 <> LF, jmp L2>
            Else_If al = LF
                On B$ecx-2 <> CR, jmp L2>
            End_If
            If B$InsideQuotes = &FALSE
                mov B$edi '"' | inc edi
                mov B$InsideQuotes &TRUE
            End_If
            stosb
        ..Else
L2:         If B$InsideQuotes = &TRUE
                mov B$edi '"' | inc edi
                mov B$InsideQuotes &FALSE
            End_If
            .If W$edi-2 <> ', '
                If W$edi-3 <> 'U$'
                    mov W$edi ', ' | add edi 2
                End_If
            .End_If
            call WriteEax
            inc esi
            .If esi < edx
                .If W$edi-2 <> ', '
                    If W$edi-3 <> 'U$'
                        mov W$edi ', ' | add edi 2
                    End_If
                .End_If
            .End_If
            dec esi
        ..End_If
    pop ecx, ebx

    add esi 2 | add ebx 2 | cmp esi edx | jae L9>

        ..If B$esi <> 0
            .If B$esi <> cl
                If B$InsideQuotes = &TRUE
                    mov B$edi '"' | inc edi
                    mov B$InsideQuotes &FALSE
                End_If
                call NextDisDataLine
                mov cl B$esi | ret
            .End_If
        ..End_If

        jmp L0<<

L9: If B$InsideQuotes = &TRUE
        mov B$edi '"' | inc edi
        mov B$InsideQuotes &FALSE
    End_If
ret


WriteDisFP4: ; WriteDisdWords
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3

L0: mov eax edx | sub eax esi
    If eax < 4
        mov cl BYTE | ret
    End_If

    push esi, edx
        mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart
        call WriteFP4
    pop edx, esi

    add esi 4

    .If B$esi <> 0
        If B$esi <> cl
            call NextDisDataLine
            mov cl B$esi | ret
        End_If
    .End_If

    .If esi < edx
        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

        jmp L0<
    .End_If

    mov B$RepetitiveBytesDone &TRUE
ret


WriteDisFP8:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3

L0: mov eax edx | sub eax esi
    If eax < 8
        mov cl BYTE | ret
    End_If

    push esi, edx
        mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart
        call WriteFP8
    pop edx, esi

    add esi 8

    .If B$esi <> 0
        If B$esi <> cl
            call NextDisDataLine
            mov cl B$esi | ret
        End_If
    .End_If

    .If esi < edx
        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

        jmp L0<
    .End_If

    mov B$RepetitiveBytesDone &TRUE
ret


WriteDisFP10:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    On B$edi-2 = '$', sub edi 3

L0: mov eax edx | sub eax esi
    If eax < 10
        mov cl BYTE | ret
    End_If

    push esi, edx
        mov ebx esi | sub ebx D$Sizesmap | add ebx D$UserPeStart
        call WriteFP10
    pop edx, esi

    add esi 10

    .If B$esi <> 0
        If B$esi <> cl
            call NextDisDataLine
            mov cl B$esi | ret
        End_If
    .End_If

    .If esi < edx
        If edi > D$NextDataOutputBreak
            call NextDisDataLine
        Else_If esi < edx
            mov D$edi ', ' | add edi 2
        End_If

        jmp L0<
    .End_If

    mov B$RepetitiveBytesDone &TRUE
ret


NextDisDataLine:
    InitDataLineBreak
    mov D$edi 020200A0D, D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    ', D$edi+16 '    '
    add edi 19
ret


NextCommentedDisDataLine:
    InitDataLineBreak
    mov D$edi 020200A0D, D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    ', D$edi+16 '  ; '
    add edi 19
ret


[InitDataLineBreak | mov D$NextDataOutputBreak edi | add D$NextDataOutputBreak 70]


IsPointerCandidate:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    push ecx, esi
        mov ecx edx | sub ecx esi | shr ecx 2

        sub esi D$Sizesmap | add esi D$Routingmap

L0:     test B$esi INDIRECT | jz L1>
            mov eax esi
            sub eax D$Routingmap | add eax D$UserPeStart
            mov eax D$eax
            sub eax D$DisImageBase | add eax D$RoutingMap

            If eax < D$Routingmap
                ;
            Else_If eax < D$EndOfRoutingMap
                test B$eax LABEL | jz L1>
                    mov eax &TRUE | jmp L9>
            End_If

L1:         add esi 4 | loop L0<

            mov eax &FALSE
L9: pop esi, ecx
ret


IsStringCandidate:
  ; esi = SizesmapStart, edx = SizesmapEnd, cl = SizeFlag
    push esi, ecx, edx
L0:     mov ecx edx | sub ecx esi
        sub esi D$Sizesmap | add esi D$UserPeStart | On B$esi = 0, jmp L8>

        mov eax 0, edx D$TruthAsciiTable
L0:     lodsb | On B$edx+eax = GOODASCII, loop L0<
        jecxz L9>

L1:         If al = 0
                lodsb | loop L1<
                jecxz L9>
            Else_If B$edx+eax = GOODASCII
                jecxz L9>
                loop L0<
            End_If

L8:     mov eax &FALSE
    pop edx, ecx, esi
ret

L9:     mov eax &TRUE
    pop edx, ecx, esi
ret


IsDwordString:
  ; esi >>> SizesMap // ebx >>> RoutingMap // B$esi = DWORD
    If B$esi = DWORD

    End_If


ret
____________________________________________________________________________________________

Proc WriteOneVirtualDataChunk:
    Arguments @SizesmapStart, @SizesmapEnd
    Uses esi, edx

        InitDataLineBreak | mov B$edi '[' | inc edi

        test D@SizesmapStart 00_11 | jz L0>
            mov B$edi '<' | inc edi

L0:     mov eax D@SizesmapStart | sub eax D$SizesMap | add eax D$SectionsMap
        call WriteOneDataLabel eax

        mov esi D@SizesmapStart, ecx D@SizesmapEnd | sub ecx esi

        ..If ecx = 1
            mov D$edi 'B$ ?' | add edi 4
        ..Else_If ecx = 2
            mov D$edi 'W$ ?' | add edi 4
        ..Else_If ecx = 4
            If B$esi = FP4
                mov D$edi 'F$ ?'
            Else
                mov D$edi 'D$ ?'
            End_If
            add edi 4
        ..Else
        ; LOOPVDATAMAX
L1:         mov eax ecx | and eax 0011
            .If eax = 0
                If B$esi = FP4
                    mov D$edi 'F$ ?'
                Else
                    mov D$edi 'D$ ?'
                End_If
                mov W$edi+4 ' #'  | add edi 6
                mov eax ecx | shr eax 2
                On eax > LOOPVDATAMAX, mov eax LOOPVDATAMAX
                call WriteEax
            .Else
                mov D$edi 'B$ ?', W$edi+4 ' #'  | add edi 6
                mov eax ecx | On eax > LOOPVDATAMAX, mov eax LOOPVDATAMAX
                call WriteEax
            .End_If
        ..End_If
        mov B$edi ']' | inc edi

      ; Case of set sizes > RosAsm Max, split into as many sub-sets as wanted:
        If ecx > LOOPVDATAMAX
            sub ecx LOOPVDATAMAX | NextDisLine
            push esi
                mov esi edi
                While B$esi <> '[' | dec esi | End_While
                While B$esi <> ':' | movsb | End_While
                mov B$edi 'X' | inc edi
                While B$esi <> '$' | movsb | End_While | dec edi
            pop esi
            jmp L1<<
        End_If

        mov D$edi CRLF2 | add edi 4
EndP





































