TITLE Status
____________________________________________________________________________________________
____________________________________________________________________________________________

;                                   Status Bar Job.
;
; Mainly done by Pit (Peter Tuente).
____________________________________________________________________________________________
____________________________________________________________________________________________

[StatusbarHandle: ?    OnStatusBar: ?]

[STATUSBAR_ID 999]

[StatusPartsPos: 0 100 200 300 400 500 600 700 800 900 1000]

[SbLine: ? #10]
[SbLineTotalLineText: B$ ' Total Lines', 0
 SbLineOctetsText:       ' Bytes', 0
 SbLineModified:         'Modified', 0
 SbLineUnModified:       'Ready to Run', 0
 SbLineLinePos:          'Line: '
 SbLineLinePosNumber:    '           '
 SbLineRowPos:           'Row: '
 SbLineRowPosNumber:     '           '
 SbF9Off:                '[F9] Inspector Off', 0
 SbF9On:                 '[F9] Inspector On', 0 ]
[SbLine2: '12345', 0]


; > input: eax = Number / edi = String Pointer

FromBinaryToDecimalAscii:
    If eax = 0
        mov B$edi '0', B$edi+1 0 | ret
    Else
        mov ecx 10, edx 0-1
L0:     push edx | mov edx 0 | div ecx | cmp eax 0 | ja L0<
        mov al dl
L0:     add eax '0' | stosb | pop eax | cmp eax 0-1 | jne L0<
        mov al 0 | stosb
    End_If
ret


SbLineAppend:
    mov edi sbLine, al 0, ecx 40 | repne scasb | dec edi
    While B$esi <> 0
        movsb
    End_While
    mov al 0 | stosb
ret

____________________________________________________________________________________________

[PointZeroed: ?]

StatusBar:
    mov eax D$CaretRowValue | shr eax 3 | add eax D$RightScroll | mov D$StatusCol eax

  ; Name of the File, or of the TITLE:
    mov esi D$ActualTitle

    ..If esi <> 0
        On D$esi <> 'TITL', jmp L2>>

        While B$esi > ' ' | inc esi | End_While
        While B$esi = ' ' | inc esi | End_While
        mov edi esi
        While B$edi > ' ' | inc edi | End_While
        push D$edi, edi
            mov B$edi 0
            call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 1, esi
        pop edi, D$edi
    ..Else
      ; eax: Simple flag for '.', because under some OS the .ext may be missing:
        mov eax &FALSE, B$PointZeroed &FALSE
        mov esi SaveFilter | On D$esi = 0, ret

        While B$esi <> 0 | inc esi | End_While
        While B$esi-1 <> '\'
            dec esi | On esi = SaveFilter, jmp L1>
            .If B$esi = '.'
                If B$PointZeroed = &FALSE
                    push D$esi, esi
                    mov B$esi 0
                    mov eax &TRUE, B$PointZeroed &TRUE
                End_If
            .End_If
        End_While
L1:     push eax
            call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 1, esi
        pop eax
        If eax = &TRUE
            pop edi, D$edi
        End_If
    ..End_If
L2:
  ; Total Number of Sources Lines:
    mov eax D$TotalNumberOfLines, edi SbLine | call FromBinaryToDecimalAscii
    mov esi SbLineTotalLineText | call SbLineAppend
    call 'USER32.SendMessageA' D$StatusbarHandle &SB_SETTEXT 2 SbLine

  ; Total Number of Bytes:
    mov eax D$SourceEnd | sub eax D$CodeSource
    mov edi SbLine | call FromBinaryToDecimalAscii
    mov esi SbLineOctetsText | call SbLineAppend
    call 'USER32.SendMessageA' D$StatusbarHandle &SB_SETTEXT 3 SbLine

  ; Line Pos:
    mov eax D$StatusLine, edi SbLineLinePosNumber | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 4, SbLineLinePos

  ; Row Pos:
    mov eax D$StatusCol, edi SbLineRowPosNumber | call FromBinaryToDecimalAscii
    call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 5, SbLineRowPos

  ; Source modified of ready to run:
    If B$ReadyToRun = &TRUE
        call 'USER32.SendMessageA' D$StatusbarHandle &SB_SETTEXT 6 SbLineUnModified
    Else
        call 'USER32.SendMessageA' D$StatusbarHandle, &SB_SETTEXT, 6, SbLineModified
    End_If
ret


[ModifiedRect: ? ? ? ?]














