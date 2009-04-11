TITLE DisEngine

____________________________________________________________________________________________

; This is the only one Table used in the Disassembler.
; Table of Pointers to each primary Opcode computation Routine:

[DisOp1:
 Op00 Op01 Op02 Op03 Op04 Op05 Op06 Op07 Op08 Op09 Op0A Op0B Op0C Op0D Op0E Op0F
 Op10 Op11 Op12 Op13 Op14 Op15 Op16 Op17 Op18 Op19 Op1A Op1B Op1C Op1D Op1E Op1F
 Op20 Op21 Op22 Op23 Op24 Op25 Op26 Op27 Op28 Op29 Op2A Op2B Op2C Op2D Op2E Op2F
 Op30 Op31 Op32 Op33 Op34 Op35 Op36 Op37 Op38 Op39 Op3A Op3B Op3C Op3D Op3E Op3F
 Op40 Op41 Op42 Op43 Op44 Op45 Op46 Op47 Op48 Op49 Op4A Op4B Op4C Op4D Op4E Op4F
 Op50 Op51 Op52 Op53 Op54 Op55 Op56 Op57 Op58 Op59 Op5A Op5B Op5C Op5D Op5E Op5F
 Op60 Op61 Op62 Op63 Op64 Op65 Op66 Op67 Op68 Op69 Op6A Op6B Op6C Op6D Op6E Op6F
 Op70 Op71 Op72 Op73 Op74 Op75 Op76 Op77 Op78 Op79 Op7A Op7B Op7C Op7D Op7E Op7F
 Op80 Op81 Op82 Op83 Op84 Op85 Op86 Op87 Op88 Op89 Op8A Op8B Op8C Op8D Op8E Op8F
 Op90 Op91 Op92 Op93 Op94 Op95 Op96 Op97 Op98 Op99 Op9A Op9B Op9C Op9D Op9E Op9F
 OpA0 OpA1 OpA2 OpA3 OpA4 OpA5 OpA6 OpA7 OpA8 OpA9 OpAA OpAB OpAC OpAD OpAE OpAF
 OpB0 OpB1 OpB2 OpB3 OpB4 OpB5 OpB6 OpB7 OpB8 OpB9 OpBA OpBB OpBC OpBD OpBE OpBF
 OpC0 OpC1 OpC2 OpC3 OpC4 OpC5 OpC6 OpC7 OpC8 OpC9 OpCA OpCB OpCC OpCD OpCE OpCF
 OpD0 OpD1 OpD2 OpD3 OpD4 OpD5 OpD6 OpD7 OpD8 OpD9 OpDA OpDB OpDC OpDD OpDE OpDF
 OpE0 OpE1 OpE2 OpE3 OpE4 OpE5 OpE6 OpE7 OpE8 OpE9 OpEA OpEB OpEC OpED OpEE OpEF
 OpF0 OpF1 OpF2 OpF3 OpF4 OpF5 OpF6 OpF7 OpF8 OpF9 OpFA OpFB OpFC OpFD OpFE OpFF]

[AMDassumed: &FALSE]
;;
  Prefixes: Op0F (EscapePrefix) , Op66 (OperandSizeOverride), Op67 (AddressSizeOverride)
;;


; Jcc Prefixes (02E / 03E) implemented as UTJ LTJ (Unlikely / Likely Taken Jump).
; Op2E > UTJ // Op3E > LTJ Followed by Jcc / (+JECXz ???).

;;
  Bad TD Files:

  115, impossible: Trick with create Window STATIC + Resource Name !!!!
  170, Menu? + Does not show the BitMaps (???...). Hang
  710, Strings IDs? BitMaps?

  810, Tricky (Auto-Write ?) Code (not Writeable in [Output].

* The Short Jumps Sizes adjustements do not seem to work (TD 170 example).
* Menu IDs replacement to be implemented.
* Some detail os wrong in the Menus until they be reloaded and resaved by the Menu Editor...
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; 256 Routines for the primary Opcodes:
____________________________________________________________________________________________
____________________________________________________________________________________________

Op00:
    On D$esi-5 = 0, add D$UnLikelyCode 4
    On D$esi-1 = 0, add D$UnLikelyCode 1 ; ???...

; This hangs here, because a valid "add reg8 reg8" is found somewhere and
; produces something wrong in the Sections recognitions...
;
; To be analyzed on Calc.exe:
;
; RoutingMap differences with or without this added 'UnLikelyCode', in order
; to understand the failure point of the Recogitions...

   ; push eax
   ;     mov eax esi | add eax 7
   ;     If eax < D$UserPeEnd
   ;        ; On D$esi = 0,
;           add D$UnLikelyCode 1
   ;     End_If
   ; pop eax

    .If B$EscapePrefix = &FALSE           ; add r/m8 r8
        mov B$LockPrefix &FALSE
        mov D$edi 'add ' | add edi 4 | jmp Dis_rm8_r8

    .Else
        mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0       ; 0F 00 /0 SLDT r/m16 ; 0F 00 /0 SLDT r/m32
            mov D$edi 'sldt', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16
        Else_If al = 1  ; 0F 00 /1 STR r/m16
            mov D$edi 'str ' | add edi 4
        Else_If al = 2  ; 0F 00 /2 LLDT r/m16
            mov D$edi 'lldt', B$edi+4 ' ' | add edi 5
        Else_If al = 3  ; 0F 00 /3 > LTR r/m16
            mov D$edi 'ltr ' | add edi 4 | jmp EndWith.W.mem
        Else_If al = 4  ; 0F 00 /4 VERR r/m16
            mov D$edi 'verr', B$edi+4 ' ' | add edi 5 ; VERW
        Else_If al = 5  ; 0F 00 /5 VERW r/m16
            mov D$edi 'verw', B$edi+4 ' ' | add edi 5
        Else
            dec esi | ret
        End_If
        inc D$UnLikelyCode
        jmp EndWith.W.mem
    .End_If


Op01:
    ..If B$EscapePrefix = &FALSE
        mov B$LockPrefix &FALSE
        mov D$edi 'add ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16
        inc D$LikelyCode

    ..Else_If W$esi = 0C801 ; 0F,01,C8 MONITOR
        inc D$LikelyCode
        add esi 2
        mov D$edi 'moni', D$edi+4 'tor ' | add edi 8
        mov B$DisFlag DISDONE+DISLINEOVER | ret

    ..Else_If W$esi = 0C901 ; 0F,01,C9 MWAIT
        inc D$LikelyCode
        add esi 2
        mov D$edi 'mwai', D$edi+4 't ' | add edi 6
        mov B$DisFlag DISDONE+DISLINEOVER | ret

    ..Else
        mov bl B$esi | inc esi | DigitMask bl To al
        inc D$UnLikelyCode

        .If al = 0          ; 0F 01 /0 SGDT m
            mov D$edi 'sgdt', B$edi+4 ' ' | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 1     ; 0F 01 /1 SIDT m
            mov D$edi 'sidt', B$edi+4 ' ' | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 2     ; LGDT m16&32
            mov D$edi 'lgdt', B$edi+4 ' ' | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 3     ; LIDT m16&32
            mov D$edi 'lidt', B$edi+4 ' ' | add edi 5 | jmp EndWith.X.mem
        .Else_If al = 4     ; 0F 01 /4 SMSW r/m16 ; 0F 01 /4 SMSW r32/m16
            mov D$edi 'smsw', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16
        .Else_If al = 6     ; LMSW r/m16
            mov D$edi 'lmsw', B$edi+4 ' ' | add edi 5 | jmp EndWith.W.mem
        .Else_If al = 7     ; INVLPG m
            mov D$edi 'invl', D$edi+4 'pg  ' | add edi 7 | jmp EndWith.X.mem
        .Else
            dec esi | ret
        .End_If
    ..End_If


Op02:
    If B$EscapePrefix = &FALSE
        mov B$LockPrefix &FALSE
        mov D$edi 'add ' | add edi 4 | jmp Dis_r8_rm8
    Else       ; LAR r16,r/m16
        mov D$edi 'lar ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
    End_If


Op03:
    If B$EscapePrefix = &FALSE
        mov D$edi 'add '
    Else     ; LSL r32,r/m32 ; LSL r16,r/m16
        mov D$edi 'lsl '
        inc D$UnLikelyCode
    End_If
    add edi 4 | jmp Dis_r32_r16__rm32_rm16


Op04: ; add al imm8
    If D$esi-1 = 4
        inc D$UnlikelyCode
    Else
        inc D$LikelyCode
    End_If
    mov D$edi 'add ' | add edi 4 | jmp Dis_al_imm8


Op05: ; add eax/ax imm32/imm16
    mov D$edi 'add ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


Op06: ; clts
    inc D$UnLikelyCode
    If B$EscapePrefix = &TRUE
        mov D$edi 'clts' | add edi 4 | mov B$DisFlag DISDONE+DISLINEOVER
    Else        ; 06 PUSH ES
        mov D$edi 'push', D$edi+4 ' es ' | add edi 7 | mov B$DisFlag DISDONE+DISLINEOVER
    End_If
ret


Op07: ; 07 POP ES
    inc D$UnLikelyCode
    mov D$edi 'pop ', W$edi+4 'es' | add edi 6 | mov B$DisFlag DISDONE+DISLINEOVER
ret


Op08:
    If B$EscapePrefix = &FALSE        ; OR r/m8,r8
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'or  ' | add edi 3 | jmp Dis_rm8_r8
    Else     ; INVD
        inc D$UnLikelyCode
        mov D$edi 'invd' | add edi 4 | mov B$DisFlag DISDONE+DISLINEOVER
    End_If
ret


Op09:
    If B$EscapePrefix = &FALSE    ; OR r/m16,r16 // OR r/m32,r32
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'or  ' | add edi 3 | jmp Dis_rm32_rm16__r32_r16
    Else    ; 0F 09 WBINVD
        inc D$UnLikelyCode
        mov D$edi 'wbin', D$edi+4 'vd  ' | add edi 6
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op0A: ; OR r8,r/m8
    inc D$LikelyCode
    mov B$LockPrefix &FALSE
    mov D$edi 'or  ' | add edi 3 | jmp Dis_r8_rm8


Op0B:
    If B$EscapePrefix = &FALSE    ; OR r32,r/m32 // OR r16,r/m16
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'or  ' | add edi 3 | jmp Dis_r32_r16__rm32_rm16
    Else    ; 0F 0B UD2
        inc D$UnLikelyCode
        mov D$edi 'ud2 ' | add edi 3
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op0C: ; OR AL,imm8
    inc D$LikelyCode
    mov D$edi 'or  ' | add edi 3 | jmp Dis_al_imm8


Op0D: ; OR AX,imm16 ; OR EAX,imm32
    .If B$AMDassumed = &TRUE
         If B$EscapePrefix = &TRUE ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Looki
            mov D$edi 'PREF', D$edi+4 'ETCH', W$edi+8 'W ' | add edi 10
            jmp Dis_rm8
        Endif
    .End_If
    inc D$LikelyCode
    mov D$edi 'or  ' | add edi 3 | jmp Dis_eax_ax__imm32_imm16


Op0E: ; 0E PUSH CS
    .If B$AMDassumed = &TRUE
        If B$EscapePrefix = &TRUE ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Looki
            mov D$edi 'FEMM', B$edi+4 'S' | add edi 5
            mov B$DisFlag DISDONE+DISLINEOVER
            ret
        Endif
    .End_If

    inc D$UnLikelyCode
    mov D$edi 'push', D$edi+4 ' cs ' | add edi 7
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op0F:
    ...If B$AMDassumed = &TRUE
        ..If B$EscapePrefix = &TRUE ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< new
            .If B$esi+1 = 0AE ; 0F 0F xx 0AE - PFACC
                mov D$edi 'PFAC', W$edi+4 'C ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 09E ; 0F 0F xx 09E - PFADD
                mov D$edi 'PFAD', W$edi+4 'D ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 09A ; 0F 0F xx 09A - PFSUB
                mov D$edi 'PFSU', W$edi+4 'B ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0B4 ; 0F 0F xx 0B4 - PFMUL
                mov D$edi 'PFMU', W$edi+4 'L ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 096 ; 0F 0F xx 096 - PFRCP
                mov D$edi 'PFRC', W$edi+4 'P ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 097 ; 0F 0F xx 097 - PFRSQRT <<<<<< until here all are common
                mov D$edi 'PFRS', D$edi+4 'QRT ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0BF ; 0F 0F xx 0BF - PAVGUSB
                mov D$edi 'PAVG', D$edi+4 'USB ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 01D ; 0F 0F xx 01D - PF2ID
                mov D$edi 'PF2I', W$edi+4 'D ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 090 ; 0F 0F xx 090 - PFCMPGE
                mov D$edi 'PFCM', D$edi+4 'PGE ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0A0 ; 0F 0F xx 0A0 - PFCMPGT
                mov D$edi 'PFCM', D$edi+4 'PGT ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0B0 ; 0F 0F xx 0B0 - PFCMPEQ
                mov D$edi 'PFCM', D$edi+4 'PEQ ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 094 ; 0F 0F xx 094 - PFMIN
                mov D$edi 'PFMI', W$edi+4 'N ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0A4 ; 0F 0F xx 0A4 - PFMAX
                mov D$edi 'PFMA', W$edi+4 'X ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0A6 ; 0F 0F xx 0A6 - PFRCPIT1
                mov D$edi 'PFRC', D$edi+4 'PIT1', B$edi+8 ' ' | add edi 9
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0A7 ; 0F 0F xx 0A7 - PFRSQIT1
                mov D$edi 'PFRS', D$edi+4 'QIT1', B$edi+8 ' ' | add edi 9
                jmp Dis_mmx1__mmx2_m64_v2
    ;
            .ElseIf B$esi+1 = 0B6 ; 0F 0F xx 0B6 - PFRCPIT2
                mov D$edi 'PFRC', D$edi+4 'PIT2', B$edi+8 ' ' | add edi 9
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0AA ; 0F 0F xx 0AA - PFSUBR
                mov D$edi 'PFSU', W$edi+4 'BR', B$edi+6 ' ' | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0D ; 0F 0F xx 0D - PI2FD
                mov D$edi 'PI2F', W$edi+4 'D ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0B7 ; 0F 0F xx 0B7 - PMULHRW
                mov D$edi 'PMUL', D$edi+4 'HRW ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0C ; 0F 0F xx 0C - PI2FW*
                mov D$edi 'PI2F', W$edi+4 'W ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 01C ; 0F 0F xx 01C - PF2IW*
                mov D$edi 'PF2I', W$edi+4 'W ' | add edi 6
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 08E ; 0F 0F xx 08E - PFPNACC*
                mov D$edi 'PFPN', D$edi+4 'ACC ' | add edi 8
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 08A ; 0F 0F xx 08A - PFNACC*
                mov D$edi 'PFNA', W$edi+4 'CC', B$edi+6 ' ' | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 0BB ; 0F 0F xx 0BB - PSWAPD*
                mov D$edi 'PSWA', W$edi+4 'PD', B$edi+6 ' ' | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 086 ; 0F 0F xx 086 - PFRCPV**
                mov D$edi 'PFRC', W$edi+4 'PV', B$edi+6 ' ' | add edi 7 ; ??????
                jmp Dis_mmx1__mmx2_m64_v2
            .ElseIf B$esi+1 = 087 ; 0F 0F xx 087 - PFRSQRTV**
                mov D$edi 'PFRS', D$edi+4 'QRTV', B$edi+8 ' ' | add edi 9
    ;* Enhanced 3DNow! or Extended 3DNow! or 3DNow!+ (Athlon/XP+-Doc[22466.pdf])
    ;** 3DNow! Professional or 3DNow! Pro (Geode LX/GX)
            .Else
                inc D$UnLikelyCode
            .Endif
        ..Endif
    ...End_If

    On B$EscapePrefix = &TRUE, inc D$UnLikelyCode
    mov B$EscapePrefix &TRUE, B$DisFlag DISDONE
    inc D$Prefixes
ret


Op10:
    .If B$EscapePrefix = &FALSE            ; adc r/m8 m8
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'adc ' | add edi 4 | jmp Dis_rm8_m8

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'movu'
        If B$OperandSizeOverride = &TRUE   ; 66 0F 10 /r MOVUPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd  '
        Else       ; 0F 10 /r MOVUPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__xmm2_m128

    .End_If


Op11:
    .If B$EscapePrefix = &FALSE         ; adc r/m32//r/m16 r32/r16
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'adc ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'movu'
        If B$OperandSizeOverride = &TRUE   ; 66 0F 11 /r MOVUPD xmm2/m128, xmm
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd  '
        Else       ; 0F 11 /r MOVUPS xmm2/m128, xmm1
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm2_m128__xmm1

    .End_If


Op12:
    ..If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        .If B$OperandSizeOverride = &TRUE        ; 66 0F 12 /r MOVLPD xmm, m64
            call MarkSSEdata SSE_1_R
            mov bl B$esi | inc esi
            mov D$edi 'movl', D$edi+4 'ps  ' | add edi 7 | jmp Dis_xmm_m64
        .Else        ; OF 12 /r MOVHLPS xmm1, xmm2 // 0F 12 /r MOVLPS xmm, m64
            mov bl B$esi | inc esi | ModMask bl to al

            If al = 3
                mov D$edi 'movh', D$edi+4 'lps ' | add edi 8
                jmp Dis_xmm1_xmm2
            Else
                dec esi | call MarkSSEdata SSE_2_F | inc esi
                mov D$edi 'movl', D$edi+4 'ps  ' | add edi 7
                jmp Dis_xmm_m64
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If
        .End_If

    ..Else    ; adc r8 r/m8
        inc D$LikelyCode
        mov D$edi 'adc ' | add edi 4 | jmp Dis_r8_rm8

    ..End_If


Op13:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        mov D$edi 'movl'
        If B$OperandSizeOverride = &TRUE        ; 66 0F 13 /r MOVLPD m64, xmm
            call MarkSSEdata SSE_1_R
            mov D$edi+4 'pd  '
        Else    ; 0F 13 /r MOVLPS m64, xmm
            call MarkSSEdata SSE_2_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | mov bl B$esi | inc esi | jmp Dis_m64_xmm

    .Else       ; adc r32/r16 r/m32//r/m16
        inc D$LikelyCode
        mov D$edi 'adc ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    .End_If


Op14:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        mov D$edi 'unpc'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 14 /r UNPCKLPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'klpd'
        Else        ; 0F 14 /r UNPCKLPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'klps'
        End_If
        mov B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__xmm2_m128

    .Else ; adc al imm8
        inc D$LikelyCode
        mov D$edi 'adc ' | add edi 4 | jmp Dis_al_imm8

    .End_If
ret


Op15:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        mov D$edi 'unpc'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 15 /r UNPCKHPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'khpd'
        Else        ; 0F 15 /r UNPCKHPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'khps'
        End_If
        mov B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__xmm2_m128

    .Else   ; adc eax/ax imm32/imm16
        inc D$LikelyCode
        mov D$edi 'adc ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16

    .End_If


Op16:

    ..If B$EscapePrefix = &TRUE
        .If B$OperandSizeOverride = &TRUE    ; 66 0F 16 /r MOVHPD xmm, m64
            call MarkSSEdata SSE_1_R
            mov bl B$esi | inc esi
            mov D$edi 'movh', D$edi+4 'pd  ' | add edi 7 | jmp Dis_xmm_m64

        .Else            ; 0F 16 /r MOVHPS xmm, m64 // OF 16 /r MOVLHPS xmm1, xmm2
            mov bl B$esi | inc esi | ModMask bl to al

            If al = 3
                mov D$edi 'movl', D$edi+4 'hps ' | add edi 8
                jmp Dis_xmm1_xmm2
            Else
                dec esi | call MarkSSEdata SSE_2_F | inc esi
                mov D$edi 'movh', D$edi+4 'ps  ' | add edi 7
                jmp Dis_xmm_m64
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If
        .End_If

    ..Else  ; 16 PUSH SS
        mov D$edi 'push', D$edi+4 ' ss ' | add edi 7

    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op17:
    .If B$EscapePrefix = &TRUE
        mov D$edi 'movh'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 17 /r MOVHPD m64, xmm
            call MarkSSEdata SSE_1_R
            mov D$edi+4 'pd  '
        Else            ; 0F 17 /r MOVHPS m64, xmm
            call MarkSSEdata SSE_2_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | mov bl B$esi | inc esi | jmp Dis_m64_xmm

    .Else       ; 17 POP SS
        inc D$UnLikelyCode
        mov D$edi 'pop ', W$edi+4 'ss' | add edi 6

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op18:
    .If B$EscapePrefix = &TRUE

        mov D$edi 'pref', D$edi+4 'etch' | add edi 8
        mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0           ; 0F 18 /0 PREFETCHNTA m8
            mov D$edi 'nta ' | add edi 4
        Else_If al = 1      ; 0F 18 /1 PREFETCHT1 m8
            mov D$edi 't0  ' | add edi 3
        Else_If al = 2      ; 0F 18 /1 PREFETCHT2 m8
            mov D$edi 't1  ' | add edi 3
        Else_If al = 3      ; 0F 18 /1 PREFETCHT3 m8
            mov D$edi 't2  ' | add edi 3
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
        jmp EndWith.B.mem

    .Else       ; 18 /r SBB r/m8,r8
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'sbb ' | add edi 4 | jmp Dis_rm8_r8

    .End_If


Op19: ; 19 /r SBB r/m16,r16 ; 19 /r SBB r/m32,r32
    inc D$LikelyCode
    mov B$LockPrefix &FALSE
    mov D$edi 'sbb ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16


Op1A: ; 1A /r SBB r8,r/m8
    inc D$LikelyCode
    mov D$edi 'sbb ' | add edi 4 | jmp Dis_r8_rm8


Op1B: ; 1B /r SBB r16,r/m16 ; 1B /r SBB r32,r/m32
    inc D$LikelyCode
    mov D$edi 'sbb ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16


Op1C: ; 1C ib SBB AL,imm8
    inc D$LikelyCode
    mov D$edi 'sbb ' | add edi 4 | jmp Dis_al_imm8


Op1D: ; 1D iw SBB AX,imm16  ; 1D id SBB EAX,imm32
    inc D$LikelyCode
    mov D$edi 'sbb ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


Op1E:   ; 1E PUSH DS
    inc D$UnLikelyCode
    mov D$edi 'push', D$edi+4 ' ds ' | add edi 7
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op1F: ; 1F POP DS
    inc D$UnLikelyCode
    mov D$edi 'pop ', W$edi+4 'ds' | add edi 6
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op20:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'and ' | add edi 4 | jmp Dis_rm8_r8

    Else        ; MOV r32,CR0 // CR1...
        inc D$UnLikelyCode
        mov bl B$esi | inc esi | BaseMask bl to al | and eax 0FF
        mov D$edi 'mov ' | add edi 4
        move D$edi D$dWordRegsTable+eax*4 | add edi 4
        mov D$edi 'CR0 ' | DigitMask bl To al | add B$edi+2 al | add edi 3
        mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op21:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'and ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    Else        ; MOV r32, DR0-DR7
        inc D$UnLikelyCode
        mov bl B$esi | inc esi | BaseMask bl to al | and eax 0FF
        mov D$edi 'mov ' | add edi 4
        move D$edi D$dWordRegsTable+eax*4 | add edi 4
        mov D$edi 'DR0 ' | DigitMask bl To al | add B$edi+2 al | add edi 3
        mov B$DisFlag DISDONE+DISLINEOVER

    EndiF
ret


Op22:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        mov D$edi 'and ' | add edi 4 | jmp Dis_r8_rm8

    Else        ; MOV CR0,r32 // CR1...
        inc D$UnLikelyCode
        mov bl B$esi | inc esi | DigitMask bl To al
        mov D$edi 'mov ', D$edi+4 'CR0 '
        add B$edi+6 al | add edi 8
        BaseMask bl to al | and eax 0FF
        move D$edi D$dWordRegsTable+eax*4 | add edi 3
        mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op23:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        mov D$edi 'and ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    Else        ; MOV DR0-DR7,r32
        inc D$UnLikelyCode
        mov bl B$esi | inc esi | DigitMask bl To al
        mov D$edi 'mov ', D$edi+4 'DR0 '
        add B$edi+6 al | add edi 8
        BaseMask bl to al | and eax 0FF
        move D$edi D$dWordRegsTable+eax*4 | add edi 3
        mov B$DisFlag DISDONE+DISLINEOVER

    End_If
ret


Op24: ; and al imm8
    inc D$LikelyCode
    mov D$edi 'and ' | add edi 4 | jmp Dis_al_imm8


Op25: ; 25 iw AND AX,imm16 ; 25 id AND EAX,imm32
    inc D$LikelyCode
    mov D$edi 'and ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


Op26:
    inc D$UnLikelyCode
    inc D$Prefixes
    mov D$SegmentOverride 'es: ', B$DisFlag DISDONE
ret


Op27: ; DAA
    inc D$LikelyCode
    mov D$edi 'daa ' | add edi 3 | mov B$DisFlag DISDONE+DISLINEOVER
ret


Op28: ; 66 0F 28 /r > MOVAPD xmm1, xmm2/m128
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'mova'
        If B$OperandSizeOverride = &TRUE
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd  '
        Else        ; 0F 28 /r MOVAPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__mmx2_m128

    .Else   ; 28 /r SUB r/m8,r8
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'sub ' | add edi 4 | jmp Dis_rm8_r8

    .End_If


Op29:   ; 66 0F 29 /r MOVAPD xmm2/m128, xmm1
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'mova'
        If B$OperandSizeOverride = &TRUE
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd  '
        Else        ; 0F 29 /r MOVAPS xmm2/m128, xmm1
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_mmx2_m128__xmm1

    .Else   ; 29 /r SUB r/m16,r16 ; 29 /r SUB r/m32,r32
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'sub ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    .End_If


Op2A:
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE  ; ; CVTPI2PD xmm, mm/m64
            call MarkSSEdata SSE_2_D
            mov D$edi+4 'i2pd'
        Else                                ; CVTPI2PS xmm, mm/m64
            call MarkSSEdata SSE_2_D
            mov D$edi 'cvtp', D$edi+4 'i2ps'
        End_If
        mov B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__mmx2_m64

    .Else       ; 2A /r SUB r8,r/m8
        inc D$LikelyCode
        mov D$edi 'sub ' | add edi 4 | jmp Dis_r8_rm8

    .End_If


Op2B:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'movn'
        If B$OperandSizeOverride = &TRUE        ; 66 0F 2B /r MOVNTPD m128, xmm
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'tpd '
        Else        ; 0F 2B /r MOVNTPS m128, xmm
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'tps '
        End_If
        add edi 8 | mov bl B$esi | inc esi | jmp Dis_m128_xmm

    .Else   ; 2B /r SUB r16,r/m16 ; 2B /r SUB r32,r/m32
        inc D$LikelyCode
        mov D$edi 'sub ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    .End_If


Op2C: ; CVTTPD2PI mm, xmm/m128
    .If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        mov D$edi 'cvtt'
        If B$OperandSizeOverride = &TRUE    ; CVTTPD2PI mm, xmm/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd2p', W$edi+8 'i ' | add edi 10
            jmp Dis_mmx1__xmm2_m128
        Else  ; CVTTPS2PI mm, xmm/m64
            call MarkSSEdata SSE_2_F
            mov D$edi 'cvtt', D$edi+4 'ps2p', W$edi+8 'i ' | add edi 10
            jmp Dis_mmx1__xmm2_m64
        End_If

    .Else       ; 2C ib SUB AL,imm8
        If D$esi-1 = 02C
            inc D$UnlikelyCode
        Else
            inc D$LikelyCode
        End_If
        mov D$edi 'sub ' | add edi 4 | jmp Dis_al_imm8

    .End_If


Op2D:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE    ; CVTPD2PI mm, xmm/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'd2pi', B$edi+8 ' ' | add edi 9
            jmp Dis_mmx1__xmm2_m128
        Else                                ; CVTPS2PI mm, xmm/m64
            call MarkSSEdata SSE_2_F
            mov D$edi+4 's2pi', B$edi+8 ' ' | add edi 9
            jmp Dis_mmx1__xmm2_m64
        End_If

    .Else       ; 2D iw SUB AX,imm16 ; 2D id SUB EAX,imm32
        inc D$LikelyCode
        mov D$edi 'sub ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16

    .End_If


; Unikely Taken Jump: This is a Mnemonic non defined by Intel, which purpose is to
; reverse the Jump Prediction. UTJ / LTJ have been defined in collaboration with
; the NASM developers group. UTJ / LTJ are prefixes to Jcc Instructions. I have no
; info about if it is active or not before JECXZ. As i think it can't hurt, i allow
; it in RosAsm assembling.

Op2E:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
        mov D$edi 'ucom'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 2E /r UCOMISD xmm1, xmm2/m64
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'isd ' | add edi 8 | jmp Dis_xmm1__xmm2_m64
        Else    ; 0F 2E /r UCOMISS xmm1, xmm2/m32
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'iss ' | add edi 8 | jmp Dis_xmm1__xmm2_m32
        End_If

    .Else
        ; UTJ if Jcc:
        ;      op70 71 72 73 74 75 76 77 Op78 79 7A 7B 7C 7D 7E 7F  E3 (E3 is JCXZ JECXZ...)
        ; Of / 80 81 82 83 84 85 86 86 88 89 8A 8B 8C 8D 8E 8F
        mov al B$esi
        .If al = 0F
            mov al B$esi+1
            cmp al 080 | jb L5>
                cmp al 08F | ja L5>
                    inc esi | mov B$EscapePrefix &TRUE | jmp L3>

        .Else
            cmp al 0E3 | je L3>
                cmp al 070 | jb L5>
                    cmp al 07F | ja L5>
L3:                     mov D$edi 'utj ' | add edi 4
                            movzx eax B$esi
                            inc esi | call D$DisOp1+eax*4
                        mov B$DisFlag DISDONE+DISLINEOVER | ret

        .End_If

L5:     mov D$SegmentOverride 'cs: '
        inc D$Prefixes
      ; Watcom-C incodes the Api calls in the form of call D$cs:apiname:
        If W$esi <> 015FF
            inc D$UnLikelyCode
        Else
            mov eax D$esi+2
            sub eax D$DisImageBase | add eax D$UserPeStart | mov eax D$eax
            On eax < D$ApiBuffer, inc D$UnLikelyCode
            On eax >= D$EndOfApiBuffer, inc D$UnLikelyCode
        End_If

    .End_If

L9: mov B$DisFlag DISDONE
ret


Op2F:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'comi'
        If B$OperandSizeOverride = &TRUE
            call MarkSSEdata SSE_1_R
            mov D$edi+4 'sd  ' | add edi 7
            jmp Dis_xmm1__xmm2_m64     ; COMISD xmm1, xmm2/m64
        Else
            call MarkSSEdata SSE_1_F
            mov D$edi+4 'ss  ' | add edi 7
            jmp Dis_xmm1__xmm2_m32     ; COMISS xmm1, xmm2/m32
        End_If

    .Else ; DAS
        inc D$LikelyCode
        mov D$edi 'das ' | add edi 3 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op30:
    If B$EscapePrefix = &FALSE  ; 30 /r XOR r/m8,r8
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'xor ' | add edi 4 | jmp Dis_rm8_r8
    Else         ; 0F 30 WRMSR
        inc D$UnLikelyCode
        mov D$edi 'wrms', B$edi+4 'r' | add edi 5
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op31: ; 0F 31 RDTSC
    If B$EscapePrefix = &FALSE  ; 31 /r XOR r/m16,r16 ; 31 /r XOR r/m32,r32
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'xor ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16
    Else
        ;inc D$UnLikelyCode
        mov D$edi 'rdts', B$edi+4 'c' | add edi 5  ; rdtsc
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op32:
    If B$EscapePrefix = &FALSE  ; 32 /r XOR r8,r/m8
        inc D$LikelyCode
        mov D$edi 'xor ' | add edi 4 | jmp Dis_r8_rm8
    Else     ; 0F 32 RDMSR
        ;inc D$UnLikelyCode
        mov D$edi 'rdms', B$edi+4 'r' | add edi 5
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op33:
    If B$EscapePrefix = &FALSE; 33 /r XOR r16,r/m16 ; 33 /r XOR r32,r/m32
        inc D$LikelyCode
        mov D$edi 'xor ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
    Else        ; 0F 33 RDPMC
        ;inc D$UnLikelyCode
        mov D$edi 'rdpm', B$edi+4 'c' | add edi 5
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op34:
    If B$EscapePrefix = &FALSE        ; 34 ib XOR AL,imm8
        inc D$LikelyCode
        mov D$edi 'xor ' | add edi 4 | jmp Dis_al_imm8
    Else         ; 0F 34 SYSENTER
        inc D$UnLikelyCode
        mov D$edi 'syse', D$edi+4 'nter' | add edi 8
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op35:
    If B$EscapePrefix = &FALSE        ; 35 iw XOR AX,imm16 ; 35 id XOR EAX,imm32
        inc D$LikelyCode
        mov D$edi 'xor ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16
    Else             ; 0F 35 SYSEXIT
        inc D$UnLikelyCode
        mov D$edi 'syse', D$edi+4 'xit ' | add edi 7
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op36:
    inc D$UnLikelyCode
    inc D$Prefixes
    mov D$SegmentOverride 'ss: ', B$DisFlag DISDONE
ret


Op37: ; aaa
    inc D$LikelyCode
    mov D$edi 'aaa ', B$DisFlag DISDONE+DISLINEOVER | add edi 3
ret


Op38: ; CMP rm8 r8
    inc D$LikelyCode
    mov D$edi 'cmp ' | add edi 4 | jmp Dis_rm8_r8
ret


Op39: ; cmp rm32/rm16 r32/r16
    inc D$LikelyCode
    mov D$edi 'cmp ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16
ret


Op3A: ; CMP r8 rm8

    inc D$LikelyCode
    mov D$edi 'cmp ' | add edi 4 | jmp Dis_r8_rm8
ret


Op3B: ; r32/r16 cmp rm32/rm16
    inc D$LikelyCode
    mov D$edi 'cmp ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
ret


Op3C: ; cmp al imm8
    inc D$LikelyCode
    mov D$edi 'cmp ' | add edi 4 | jmp Dis_al_imm8


Op3D: ; cmp eax // ax,  imm32 // imm16
    inc D$LikelyCode
    mov D$edi 'cmp ' | add edi 4 | jmp Dis_eax_ax__imm32_imm16


; Likely Taken Jump: This is a Mnemonic non defined by Intel, which purpose is to
; reverse the Jump Prediction. UTJ / LTJ have been defined in collaboration with
; the NASM developers group. UTJ / LTJ are prefixes to Jcc Instructions. I have no
; info about if it is active or not before JECXZ. As i think it can't hurt, i allow
; it in RosAsm assembling.

Op3E: ; Op2E for UTJ

    ; LTJ if Jcc:
  ;      70 71 72 73 74 75 76 77 78 79 7A 7B 7C 7D 7E 7F E3
  ; Of / 80 81 82 83 84 85 86 86 88 89 8A 8B 8C 8D 8E 8F

    mov al B$esi
        .If al = 0F
            mov al B$esi+1
            cmp al 080 | jb L5>
                cmp al 08F | ja L5>
                    inc esi | mov B$EscapePrefix &TRUE | jmp L3>

        .Else
            cmp al 0E3 | je L3>
                cmp al 070 | jb L5>
                    cmp al 07F | ja L5>
L3:                     mov D$edi 'ltj ' | add edi 4
                        push edi
                            movzx eax B$esi
                            inc esi | add edi 5 | call D$DisOp1+eax*4
                        pop eax
                        mov B$eax ' ' | mov B$DisFlag DISDONE+DISLINEOVER | ret

        .End_If

L5: mov D$SegmentOverride 'ds: '
    inc D$Prefixes
    inc D$UnLikelyCode

L9: mov B$DisFlag DISDONE
ret


Op3F: ; aas
    inc D$LikelyCode
    mov D$edi 'aas ', B$DisFlag DISDONE+DISLINEOVER | add edi 4
ret


Op40: ; cmovo
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'cmov', W$edi+4 'o ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        inc D$LikelyCode
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'ax' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op41: ; cmovno
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'cmov', D$edi+4 'no  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'cx' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER
    .End_If
ret


Op42: ; cmovb / cmovc / cmovnae
    .If B$EscapePrefix = &TRUE
        mov D$edi 'cmov', W$edi+4 'c ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'dx' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op43: ; cmovae / cmovnb / cmovnc
    .If B$EscapePrefix = &TRUE
        mov D$edi 'cmov', D$edi+4 'ae  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'bx' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op44: ; cmove / cmovz
    .If B$EscapePrefix = &TRUE
        mov D$edi 'cmov', W$edi+4 'e ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'sp' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op45: ; cmovne / cmonnz
    .If B$EscapePrefix = &TRUE
        mov D$edi 'cmov', D$edi+4 'ne  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'bp' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op46: ; cmovbe : cmovna
    .If B$EscapePrefix = &TRUE
        mov D$edi 'cmov', D$edi+4 'be  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'si' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op47: ; cmova / cmovnbe
    .If B$EscapePrefix = &TRUE
        mov D$edi 'cmov', W$edi+4 'a ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16

    .Else
        mov D$edi 'inc ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov W$edi 'di' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER

    .End_If
ret


Op48: ; cmovs
    .If B$EscapePrefix = &FALSE
        mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov  W$edi 'ax' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        mov D$edi 'cmov', W$edi+4 's ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op49: ; cmovns
    .If B$EscapePrefix = &FALSE
        mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov  W$edi 'cx' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        mov D$edi 'cmov', D$edi+4 'ns  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op4A: ; cmovp / cmovpe
    .If B$EscapePrefix = &FALSE
        mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov  W$edi 'dx' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        mov D$edi 'cmov', W$edi+4 'p ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op4B: ; cmovnp / cmovpo
    .If B$EscapePrefix = &FALSE
        mov D$edi 'dec ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov B$edi 'e' | inc edi
        End_If
        mov  W$edi 'bx' | add edi 2 | mov B$DisFlag DISDONE+DISLINEOVER
    .Else
        mov D$edi 'cmov', D$edi+4 'np  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    .End_If
ret


Op4C: ; cmovl / cmovnge
    If B$EscapePrefix = &FALSE
        inc D$UnLikelyCode
        mov D$edi 'dec ', D$edi+4 'esp ' | add edi 7 | mov B$DisFlag DISDONE+DISLINEOVER
    Else
        mov D$edi 'cmov', W$edi+4 'l ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op4D: ; cmovge / cmovnl
    If B$EscapePrefix = &FALSE
        mov D$edi 'dec ', D$edi+4 'ebp ' | add edi 7 | mov B$DisFlag DISDONE+DISLINEOVER
    Else
        mov D$edi 'cmov', D$edi+4 'ge  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op4E: ; cmovle / cmovng
    If B$EscapePrefix = &FALSE
        mov D$edi 'dec ', D$edi+4 'esi ' | add edi 7 | mov B$DisFlag DISDONE+DISLINEOVER
    Else
        mov D$edi 'cmov', D$edi+4 'le  ' | add edi 7 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op4F: ; cmovg / cmovnle
    If B$EscapePrefix = &FALSE
        mov D$edi 'dec ', D$edi+4 'edi ' | add edi 7 | mov B$DisFlag DISDONE+DISLINEOVER
    Else
        mov D$edi 'cmov', W$edi+4 'g ' | add edi 6 | jmp Dis_r32_r16__rm32_rm16
    End_If
ret


Op50:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' eax' | add edi 4
        Else
           ; inc D$UnLikelyCode
            mov D$edi ' ax ' | add edi 3
        End_If

     .Else
        ;inc D$UnLikelyCode
        mov D$edi 'movm'
        If B$OperandSizeOverride = &TRUE ; 66 0F 50 /r MOVMSKPD r32, xmm
            mov D$edi+4 'skpd'
        Else            ; 0F 50 /r MOVMSKPS r32, xmm
            mov D$edi+4 'skps'
        End_If
        mov B$edi+8 ' ' | add edi 9 | jmp Dis_r32_xmm

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op51:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' ecx' | add edi 4
        Else
            ;inc D$UnLikelyCode
            mov D$edi ' cx ' | add edi 3
        End_If

    .Else
       ; inc D$UnLikelyCode
        mov D$edi 'sqrt'
        If B$OperandSizeOverride = &TRUE   ; 66 0F 51 /r SQRTPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd  '
        Else        ; 0F 51 /r SQRTPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op52:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' edx' | add edi 4
        Else
            ;inc D$UnLikelyCode
            mov D$edi ' dx ' | add edi 3
        End_If

    .Else   ; 0F 52 /r RSQRTPS xmm1, xmm2/m128
        call MarkSSEdata SSE_4_F
        mov D$edi 'rsqr', D$edi+4 'tps ' | add edi 8 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op53:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' ebx' | add edi 4
        Else
           ; inc D$UnLikelyCode
            mov D$edi ' bx ' | add edi 3
        End_If

    .Else  ; 0F 53 /r RCPPS xmm1, xmm2/m128
        call MarkSSEdata SSE_4_F
        mov D$edi 'rcpp', W$edi+4 's ' | add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op54:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            mov D$edi ' esp' | add edi 4
        Else
            inc D$UnlikelyCode
            mov D$edi ' sp ' | add edi 3
        End_If

    .Else
        mov D$edi 'andp' | add edi 4
        If B$OperandSizeOverride = &TRUE                ; 066 0F 054 ...
            call MarkSSEdata SSE_2_R    ; ANDPD 66 0F 54 /r
            mov W$edi 'd '
        Else
            call MarkSSEdata SSE_4_F
            mov W$edi 's '              ; ANDPS 0F 54 /r
        End_If
        add edi 2 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op55:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' ebp' | add edi 4
        Else
            inc D$UnLikelyCode
            mov D$edi ' bp ' | add edi 3
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'andn' | add edi 4
        If B$OperandSizeOverride = &TRUE
            call MarkSSEdata SSE_2_R
            mov D$edi 'pd  '    ; ANDNPD 66 0F 55 /r
        Else
            call MarkSSEdata SSE_4_F
            mov D$edi 'ps  '    ; ANDNPS 0F 55 /r
        End_If
        add edi 3 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op56:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' esi' | add edi 4
        Else
            inc D$UnLikelyCode
            mov D$edi ' si ' | add edi 3
        End_If

    .Else
        ;inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE    ; ORPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi 'orpd'
        Else        ; ORPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi 'orps'
        End_If
        mov B$edi+4 ' ' | add edi 5 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op57:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'push' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi ' edi' | add edi 4
        Else
            inc D$UnLikelyCode
            mov D$edi ' di ' | add edi 3
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'xorp'
        If B$OperandSizeOverride = &TRUE  ; 66 0F 57 /r XORPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '
        Else        ; 0F 57 /r XORPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op58:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'eax ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            mov W$edi 'ax' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'addp'
        If B$OperandSizeOverride = &TRUE       ; 066 0F 058...
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '                   ; ADDPD
        Else
            call MarkSSEdata SSE_4_F
            mov W$edi+4 's '                   ; ADDPS
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op59:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'ecx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            mov D$edi 'cx' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'mulp'
        If B$OperandSizeOverride = &TRUE        ; 66 0F 59 /r MULPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '
        Else        ; 0F 59 /r MULPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5A:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'edx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            mov W$edi 'dx' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE    ; CVTPD2PS xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'd2ps', B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__xmm2_m128
        Else                                ; CVTPS2PD xmm1, xmm2/m64
            call MarkSSEdata SSE_2_F
            mov D$edi+4 's2pd', B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__xmm2_m64
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5B:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'ebx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            mov W$edi 'bx' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode

        If B$OperandSizeOverride = &TRUE ; CVTPS2DQ xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov D$edi 'cvtp', D$edi+4 's2dq'
        Else                             ; CVTDQ2PS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_D
            mov D$edi 'cvtd', D$edi+4 'q2ps'
        End_If
        mov B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5C:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'esp ' | add edi 3
        Else
            inc D$UnLikelyCode
            mov W$edi 'sp' | add edi 2
        End_If

    .Else
        mov D$edi 'subp'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 5C /r SUBPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '
        Else    ; 0F 5C /r SUBPS xmm1 xmm2/m128
            call MarkSSEdata SSE_4_F
            mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5D:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'ebp ' | add edi 3
        Else
            inc D$UnLikelyCode
            mov W$edi 'bp' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'minp'
        If B$OperandSizeOverride = &TRUE ; MINPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '
        Else        ; MINPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5E:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'esi ' | add edi 3
        Else
            inc D$UnLikelyCode
            mov W$edi 'si' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'divp'
        If B$OperandSizeOverride = &TRUE    ; DIVPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '
        Else                                ; DIVPS xmm1, xmm2/m128
            call MarkSSEdata SSE_4_F
            mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op5F:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'edi ' | add edi 3
        Else
            inc D$UnLikelyCode
            mov W$edi 'di' | add edi 2
        End_If

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'maxp'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 5F /r MAXPD xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov W$edi+4 'd '
        Else                                ; MAXPS xmm1, xmm2/m128
            mov W$edi+4 's '
        End_If
        add edi 6 | jmp Dis_xmm1__xmm2_m128

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op60:
    .If B$EscapePrefix = &FALSE   ; 60 PUSHA 60 PUSHAD
        inc D$LikelyCode
        mov D$edi 'push', B$edi+4 'a' | add edi 5
        If B$OperandSizeOverride = &FALSE
            mov B$edi 'd' | inc edi
        End_If

    .Else; 0F 60 /r PUNPCKLBW mm, mm/m32   ; 66 0F 60 /r PUNPCKLBW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'punp', D$edi+4 'cklb', W$edi+8 'w ' | add edi 10
        If B$OperandSizeOverride = &TRUE
            jmp Dis_xmm1__xmm2_m128
        Else
            jmp Dis_mmx1__mmx2_m64
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op61:
    .If B$EscapePrefix = &FALSE   ; 61 POPA     ; 61 POPAD
        inc D$LikelyCode
        mov D$edi 'popa' | add edi 4
        If B$OperandSizeOverride = &FALSE
            mov B$edi 'd' | inc edi
        End_If

    .Else ; 0F 61 /r PUNPCKLWD mm, mm/m32   ; 66 0F 61 /r PUNPCKLWD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'punp', D$edi+4 'cklw', W$edi+8 'd ' | add edi 10
        If B$OperandSizeOverride = &TRUE
            jmp Dis_xmm1__xmm2_m128
        Else
            jmp Dis_mmx1__mmx2_m64
        End_If
    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op62:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
      ; 0F 62 /r PUNPCKLDQ mm, mm/m32   ; 66 0F 62 /r PUNPCKLDQ xmm1, xmm2/m128
        mov D$edi 'punp', D$edi+4 'ckld', W$edi+8 'q ' | add edi 10
        If B$OperandSizeOverride = &TRUE
            jmp Dis_xmm1__xmm2_m128
        Else
            jmp Dis_mmx1__mmx2_m64
        End_If
    .Else
        inc D$UnLikelyCode
        mov D$edi 'boun', W$edi+4 'd ' | add edi 6  ; BOUND
        jmp Dis_r32_r16__rm32_rm16
    .End_If



Op63:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
        mov D$edi 'pack', D$edi+4 'sswb', B$edi+8 ' ' | add edi 9
        If B$OperandSizeOverride = &TRUE    ; 66 0F 63 /r PACKSSWB xmm1, xmm2/m128
            jmp Dis_xmm1__xmm_m128
        Else        ; 0F 63 /r PACKSSWB mm1, mm2/m64
            jmp Dis_mmx1__mmx2_m64
        End_If

    .Else
        inc D$UnlikelyCode
        mov D$edi 'arpl', B$edi+4 ' ' | add edi 5 ; ARPL
        mov B$OperandSizeOverride &TRUE             ; to avoid one more case for 16/16

        jmp Dis_rm32_rm16__r32_r16

    .End_If


Op64:
    .If B$EscapePrefix = &TRUE
      ;0F 64 /r PCMPGTB mm, mm/m64 ;66 0F 64 /r PCMPGTB xmm1, xmm2/m128
        mov D$edi 'pcmp', D$edi+4 'gtb ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        mov D$SegmentOverride 'fs: ', B$DisFlag DISDONE
        inc D$Prefixes

    .End_If
ret


Op65:
    .If B$EscapePrefix = &TRUE
      ; 0F 65 /r PCMPGTW mm, mm/m64   ; 66 0F 65 /r PCMPGTW xmm1, xmm2/m128
        mov D$edi 'pcmp', D$edi+4 'gtw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        mov D$SegmentOverride 'gs: ', B$DisFlag DISDONE
        inc D$Prefixes
        inc D$UnLikelyCode
    .End_If
ret


Op66:
    .If B$EscapePrefix = &TRUE  ; 0F 66 /r PCMPGTD mm, mm/m64
        ;inc D$UnLikelyCode
        mov D$edi 'pcmp', D$edi+4 'gtd ' | add edi 8
        jmp Dis_mmx1__mmx2_m64

    .Else_If W$esi = 0660F      ; 66 0F 66 /r PCMPGTD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2
        mov D$edi 'pcmp', D$edi+4 'gtd ' | add edi 8
        jmp Dis_xmm1__xmm2_m128

    .Else_If W$esi = 07C0F      ; 66,0F,7C,/r HADDPD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2 | call MarkSSEdata SSE_2_R
        mov D$edi 'hadd', D$edi+4 'pd ' | add edi 7
        jmp Dis_xmm1__xmm2_m128

    .Else_If W$esi = 07D0F      ; 66,0F,7D,/r HSUBPD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2 | call MarkSSEdata SSE_2_R
        mov D$edi 'hsub', D$edi+4 'pd ' | add edi 7
        jmp Dis_xmm1__xmm2_m128

    .Else_If W$esi = 0D00F      ; 66,0F,D0,/r ADDSUBPD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        add esi 2
        call MarkSSEdata SSE_2_R
        mov D$edi 'adds', D$edi+4 'ubpd', B$edi+8 ' ' | add edi 9
        jmp Dis_xmm1__xmm2_m128

    .Else
        On B$OperandSizeOverride = &TRUE, inc D$UnLikelyCode
        mov B$OperandSizeOverride &TRUE, B$DisFlag DISDONE, W$DisSizeMarker 'W$'
        inc D$Prefixes
    .End_If
ret


Op67:
    .If B$EscapePrefix = &FALSE
        On B$AddressSizeOverride = &TRUE, inc D$UnLikelyCode
        mov B$AddressSizeOverride &TRUE, B$DisFlag DISDONE
        inc D$Prefixes

    .Else   ; 66 0F 67 /r PACKUSWB xmm1, xmm2/m128 ; 0F 67 /r PACKUSWB mm, mm/m64
        ;inc D$UnLikelyCode
        mov D$edi 'pack', D$edi+4 'uswb', B$edi+8 ' ' | add edi 9
        jmp Dis_xmmx1__xmmx2_m64_128

    .End_If
ret


Op68:
    .If B$EscapePrefix = &FALSE   ; 68 PUSH imm16 / 32
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'push', B$edi+4 ' ' | add edi 5
        Else
            mov D$edi 'push', W$edi+4 'W ' | add edi 6
        End_If
        jmp Dis_imm32_16

    .Else ; 0F 68 /r PUNPCKHBW mm, mm/m64   ; 66 0F 68 /r PUNPCKHBW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'punp', D$edi+4 'ckhb', W$edi+8 'w ' | add edi 10
        jmp Dis_xmmx1__xmmx2_m64_128
    .End_If
ret


Op69:
    .If B$EscapePrefix = &TRUE
      ; 0F 69 /r PUNPCKHWD mm, mm/m64   ; 66 0F 69 /r PUNPCKHWD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'punp', D$edi+4 'ckhw', W$edi+8 'd ' | add edi 10
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
      ; IMUL r16,r/m16,imm16 (word register r/m16 * immediate word) (+ 32)
        inc D$LikelyCode
        mov D$edi 'imul', B$edi+4 ' ' | add edi 5
        jmp Dis_r32_r16__rm32_rm16_OrNone__SignedImm16_32
    .End_If
ret


Op6A:
    .If B$EscapePrefix = &TRUE
      ; 0F 6A /r PUNPCKHDQ mm, mm/m64   ; 66 0F 6A /r PUNPCKHDQ xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'punp', D$edi+4 'ckhd', W$edi+8 'q ' | add edi 10
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else       ; 6A PUSH imm8
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'push', B$edi+4 ' ' | add edi 5 | jmp Dis_Imm8
        Else
            mov D$edi 'push', W$edi+4 'W ' | add edi 6 | jmp Dis_Imm8
        End_If
    .End_If
ret


Op6B:
    .If B$EscapePrefix = &TRUE
      ; 66 0F 6B /r PACKSSDW xmm1, xmm2/m128 ; 0F 6B /r PACKSSDW mm1, mm2/m64
        ;inc D$UnLikelyCode
        mov D$edi 'pack', D$edi+4 'ssdw', B$edi+8 ' ' | add edi 9
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else  ; 6B /r ib > IMUL r16,r/m16,imm8
        inc D$LikelyCode
        mov D$edi 'imul', B$edi+4 ' ' | add edi 5
        jmp Dis_r32_r16__rm32_rm16_OrNone__SignedImm8

    .End_If
ret


Op6C:
    .If B$EscapePrefix = &TRUE
      ; 66 0F 6C /r PUNPCKLQDQ xmm1, xmm2/m128
        mov D$edi 'punp', D$edi+4 'cklq', D$edi+8 'dq  ' | add edi 11
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
      ; INS m8, DX (ins 'B$es:edi dx')
        inc D$UnLikelyCode
        mov D$edi 'insb' | add edi 4

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op6D:

    .If B$EscapePrefix = &TRUE
      ; 66 0F 6D /r PUNPCKHQDQ xmm1, xmm2/m128
        mov D$edi 'punp', D$edi+4 'ckhq', D$edi+8 'dq  ' | add edi 11
        jmp Dis_xmm1__xmm2_m128

    .Else ; INS m32, DX +16
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE
            mov D$edi 'insw'
        Else
            mov D$edi 'insd'
        End_If
        add edi 4

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op6E:
    .If B$EscapePrefix = &TRUE
        inc D$LikelyCode
        mov D$edi 'movd', B$edi+4 ' ' | add edi 5
        If B$OperandSizeOverride = &TRUE    ; 66 0F 6E /r MOVD xmm, r/m32
            jmp Dis_xmm_rm32
        Else         ; 0F 6E /r MOVD mm, r/m32
            jmp Dis_mmx_rm32
        End_If

    .Else       ; OUTS DX, m8
        inc D$UnLikelyCode
        mov D$edi 'outs', B$edi+4 'B' | add edi 5

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op6F:
    .If B$EscapePrefix = &TRUE
        inc D$LikelyCode
        If B$OperandSizeOverride = &TRUE        ; MOVDQA xmm1, xmm2/m128
            mov D$edi 'movd', D$edi+4 'qa  ' | add edi 7 | jmp Dis_xmm1__xmm2_m128
        Else        ; 0F 6F /r MOVQ mm, mm/m64
            mov D$edi 'movq', B$edi+4 ' ' | add edi 5 | jmp Dis_mmx1__mmx2_m64
        End_If

    .Else       ; OUTSW OUTSD
        inc D$UnLikelyCode
        mov D$edi 'outs' | add edi 4
        If B$OperandSizeOverride = &TRUE
            mov D$edi 'W'
        Else
            mov D$edi 'D'
        End_If
        inc edi

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op70:
    .If B$EscapePrefix = &FALSE        ; JO rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'o ' | add edi 6
        jmp EndWithDisByteRelative

    .Else
        ;inc D$UnLikelyCode
        mov D$edi 'pshu'
        If B$OperandSizeOverride = &TRUE    ; 66 0F 70 /r ib PSHUFD xmm1, xmm2/m128, imm8
            mov D$edi+4 'fd  '
            add edi 7 | call Dis_xmm1__xmm2_m128
        Else        ; 0F 70 /r ib PSHUFW mm1, mm2/m64, imm8
            mov D$edi+4 'fw  '
            add edi 7 | call Dis_mm1__mm2_m128
        End_If
        mov D$edi ' ' | inc edi | call WriteImm8

    .End_If
     mov B$DisFlag DISDONE+DISLINEOVER
ret


Op71:
    .If B$EscapePrefix = &FALSE        ; JNO rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'no  ' | add edi 7
        jmp EndWithDisByteRelative

    .Else
        mov bl B$esi | inc esi | DigitMask bl To al

        If al = 2   ; 0F 71 /2 ib PSRLW mm, imm8    ; 66 0F 71 /2 ib PSRLW xmm1, imm8
            mov D$edi 'psrl'
        Else_If al = 4   ; 0F 71 /4 ib PSRAW mm, imm8   ; 66 0F 71 /4 ib PSRAW xmm1, imm8
            mov D$edi 'psra'
        Else_If al = 6   ; 0F 71 /6 ib PSLLW mm, imm8 ; 66 0F 71 /6 ib PSLLW xmm1, imm8
            mov D$edi 'psll'
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
        mov W$edi+4 'w ' | add edi 6 | jmp Dis_xmmx_imm8

    .End_If


Op72:
    .If B$EscapePrefix = &FALSE ; JB rel8 ; JNAE rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'b ' | add edi 6
        jmp EndWithDisByteRelative

    .Else
        mov bl B$esi | inc esi | DigitMask bl To al

        If al = 2      ; 0F 72 /2 ib PSRLD mm, imm8  ; 66 0F 72 /2 ib PSRLD xmm1, imm8
            mov D$edi 'psrl'
        Else_If al = 4 ; 0F 72 /4 ib PSRAD mm, imm8  ; 66 0F 72 /4 ib PSRAD xmm1, imm8
            mov D$edi 'psra'
        Else_If al = 6 ; 0F 72 /6 ib PSLLD mm, imm8  ; 66 0F 72 /6 ib PSLLD xmm1, imm8
            mov D$edi 'psll'
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
        mov W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx_imm8

    .End_If



Op73:
    ...If B$EscapePrefix = &TRUE
        mov bl B$esi | inc esi | DigitMask bl To al

        .If al = 2   ; 0F 73 /2 ib PSRLQ mm, imm8  ; 66 0F 73 /2 ib PSRLQ xmm1, imm8
            mov D$edi 'psrl', W$edi+4 'q ' | add edi 6
            jmp Dis_xmmx_imm8
        .Else_If al = 3    ; 66 0F 73 /3 ib PSRLDQ xmm1, imm8
            If B$OperandSizeOverride = &TRUE
                mov D$edi 'psrl', D$edi+4 'dq  ' | add edi 7
                jmp Dis_xmm_imm8
            End_If
            inc D$UnLikelyCode
        .Else_If al = 6
              ; 0F 73 /6 ib PSLLQ mm, imm8 ; 66 0F 73 /6 ib PSLLQ xmm1, imm8
            mov D$edi 'psll', W$edi+4 'q ' | add edi 6 | jmp Dis_xmmx_imm8
        .Else_If al = 7    ; 66 0F 73 /7 ib PSLLDQ xmm1, imm8
            If B$OperandSizeOverride = &TRUE
                mov D$edi 'psll', D$edi+4 'dq  ' | add edi 7
                jmp Dis_xmm_imm8
            End_If
            inc D$UnLikelyCode
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

    ...Else        ; JAE rel8 ; JNB rel8 ; JNC rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'ae  ' | add edi 7
        jmp EndWithDisByteRelative

    ...End_If
ret


Op74:
    If B$EscapePrefix = &FALSE  ; JE rel8 ; JZ rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'e ' | add edi 6
        jmp EndWithDisByteRelative

    Else
      ; 0F 74 /r PCMPEQB mm, mm/m64    ; 66 0F 74 /r PCMPEQB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'pcmp', D$edi+4 'eqb ', B$edi+8 ' ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    End_If


Op75:
    If B$EscapePrefix = &FALSE  ; JNE rel8 ; JNZ rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'ne  ' | add edi 7
        jmp EndWithDisByteRelative

    Else
      ; 0F 75 /r PCMPEQW mm, mm/m64     ; 66 0F 75 /r PCMPEQW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'pcmp', D$edi+4 'eqw ', B$edi+8 ' ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    End_If


Op76:
    If B$EscapePrefix = &FALSE        ; JBE rel8 ; JNA rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'be  ' | add edi 7
        jmp EndWithDisByteRelative

    Else
      ; 0F 76 /r PCMPEQD mm, mm/m64     ; 66 0F 76 /r PCMPEQD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'pcmp', D$edi+4 'eqd ', B$edi+8 ' ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    End_If


Op77:
    If B$EscapePrefix = &FALSE        ; JA rel8 ; JNBE rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'a ' | add edi 6 | jmp EndWithDisByteRelative

    Else     ; EMMS
        mov D$edi 'emms' | add edi 4

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op78: ; JS rel8
    inc D$LikelyCode
    mov B$CALLInstruction &TRUE
    SubEdi6 | mov D$edi ' | j', W$edi+4 's ' | add edi 6 | jmp EndWithDisByteRelative
ret


Op79: ; JNS rel8
    inc D$LikelyCode
    mov B$CALLInstruction &TRUE
    SubEdi6 | mov D$edi ' | j', D$edi+4 'ns  ' | add edi 7 | jmp EndWithDisByteRelative
ret


Op7A: ; JP rel8 ; JPE rel8
    inc D$LikelyCode
    mov B$CALLInstruction &TRUE
    SubEdi6 | mov D$edi ' | j', W$edi+4 'p ' | add edi 6 | jmp EndWithDisByteRelative
ret


Op7B: ; JNP rel8 ; JPO rel8
    inc D$LikelyCode
    mov B$CALLInstruction &TRUE
    SubEdi6 | mov D$edi ' | j', D$edi+4 'np  ' | add edi 7 | jmp EndWithDisByteRelative
ret


Op7C: ; JL rel8 ; JNGE rel8
    inc D$LikelyCode
    mov B$CALLInstruction &TRUE
    SubEdi6 | mov D$edi ' | j', W$edi+4 'l ' | add edi 6 | jmp EndWithDisByteRelative
ret


Op7D: ; JGE rel8 ; JNL rel8
    inc D$LikelyCode
    mov B$CALLInstruction &TRUE
    SubEdi6 | mov D$edi ' | j', D$edi+4 'ge  ' | add edi 7 | jmp EndWithDisByteRelative
ret


Op7E:
    inc D$LikelyCode
    If B$EscapePrefix = &FALSE     ; JLE rel8 ; JNG rel8
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'le  ' | add edi 7
        jmp EndWithDisByteRelative

    Else
       ; 66 0F 7E /r MOVD r/m32, xmm ; 0F 7E /r MOVD r/m32, mm
         mov D$edi 'movd', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_xmmx

    End_If


Op7F:
    .If B$EscapePrefix = &FALSE  ; JG rel8 ; JNLE rel8
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'g ' | add edi 6
        jmp EndWithDisByteRelative

    .Else
        If B$OperandSizeOverride = &TRUE    ; 66 0F 7F /r MOVDQA xmm2/m128, xmm1
            mov D$edi 'movd', D$edi+4 'qa  ' | add edi 7
            jmp Dis_xmm2_m128__xmm1
        Else        ; 0F 7F /r MOVQ mm/m64, mm
            mov D$edi 'movq', B$edi+4 ' ' | add edi 5
            jmp Dis_mmx1_m64__mmx2
        End_If

    .End_If


Op80:
    inc D$LikelyCode
    ..If B$EscapePrefix = &TRUE       ; 0F 80 cw/cd JO rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'o ' | add edi 6
        jmp EndWithDisWordDwordRelative

    ..Else
        mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /2 ?

        .If al = 0
            mov B$LockPrefix &FALSE
            mov D$edi 'add ' | add edi 4
        .Else_If al = 1  ; OR r/m8,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'or  ' | add edi 3
        .Else_If al = 2  ; adc r/m8 imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'adc ' | add edi 4
        .Else_If al = 3  ; 80 /3 ib SBB r/m8,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'sbb ' | add edi 4
        .Else_If al = 4
            mov B$LockPrefix &FALSE
            mov D$edi 'and ' | add edi 4
        .Else_If al = 5  ; 80 /5 ib SUB r/m8,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'sub ' | add edi 4
        .Else_If al = 6  ; 80 /6 ib XOR r/m8,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'xor ' | add edi 4
        .Else_If al = 7
            mov D$edi 'cmp ' | add edi 4
        .End_If

        jmp Dis_rm8_imm8

    ..End_If


Op81:
    inc D$LikelyCode
    ..If B$EscapePrefix = &TRUE      ; JNO rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'no  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    ..Else
      ; adc r/m32//r/m16 imm32/imm16
        mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /2 ?

        .If al = 0
            mov B$LockPrefix &FALSE
            mov D$edi 'add ' | add edi 4
        .Else_If al = 1      ; OR r/m16,imm16 // OR r/m32,imm32
            mov B$LockPrefix &FALSE
            mov D$edi 'or  ' | add edi 3
        .Else_If al = 2
            mov B$LockPrefix &FALSE
            mov D$edi 'adc ' | add edi 4
        .Else_If al = 3  ; 81 /3 iw SBB r/m16,imm16  ; 81 /3 id SBB r/m32,imm32
            mov B$LockPrefix &FALSE
            mov D$edi 'sbb ' | add edi 4
        .Else_If al = 4
            mov B$LockPrefix &FALSE
            mov D$edi 'and ' | add edi 4
        .Else_If al = 5  ; 81 /5 iw SUB r/m16,imm16 ; 81 /5 id SUB r/m32,imm32
            mov B$LockPrefix &FALSE
            mov D$edi 'sub ' | add edi 4
        .Else_If al = 6  ; 81 /6 iw XOR r/m16,imm16 ; 81 /6 id XOR r/m32,imm32
            mov B$LockPrefix &FALSE
            mov D$edi 'xor ' | add edi 4
        .Else_If al = 7
            mov D$edi 'cmp ' | add edi 4
        .End_If

        jmp Dis_rm32_rm16__imm32_imm16

    ..End_If


Op82:
    If B$EscapePrefix = &TRUE       ; JB rel16/32 ; JC rel16/32 ; JNAE rel16/32
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'b ' | add edi 6
        jmp EndWithDisWordDwordRelative
    End_If
ret


Op83:
    inc D$LikelyCode

    ..If B$EscapePrefix = &TRUE      ; JAE rel16/32 ; JNB rel16/32 ; JNC rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'ae  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    ..Else

        mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /2 ?

        .If al = 0
            mov B$LockPrefix &FALSE
            mov D$edi 'add ' | add edi 4
        .Else_If al = 1      ; OR r/m16,imm8 // OR r/m32,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'or  ' | add edi 3
        .Else_If al = 2   ; adc r/m32//r/m16 imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'adc ' | add edi 4
        .Else_If al = 3  ; 83 /3 ib SBB r/m16,imm8   ; 83 /3 ib SBB r/m32,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'sbb ' | add edi 4
        .Else_If al = 4
            mov B$LockPrefix &FALSE
            mov D$edi 'and ' | add edi 4
        .Else_If al = 5  ; 83 /5 ib SUB r/m16,imm8 ; 83 /5 ib SUB r/m32,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'sub ' | add edi 4
        .Else_If al = 6  ; 83 /6 ib XOR r/m16,imm8 ; 83 /6 ib XOR r/m32,imm8
            mov B$LockPrefix &FALSE
            mov D$edi 'xor ' | add edi 4
        .Else_If al = 7
            mov D$edi 'cmp ' | add edi 4
        .End_If

        jmp Dis_rm32_rm16__imm8

    ..End_If


Op84:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE       ; JE rel16/32 ; JZ rel16/32 ; JZ rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'e ' | add edi 6
        jmp EndWithDisWordDwordRelative

    Else    ; 84 /r TEST r/m8,r8
        mov D$edi 'test', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8_r8

    End_If


Op85:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE    ; JNE rel16/32 ; JNZ rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'ne  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else    ; 85 /r TEST r/m16,r16 ; 85 /r TEST r/m32,r32
        mov D$edi 'test', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__r32_r16

    End_If


Op86:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE    ; JBE rel16/32 ; JNA rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'be  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else        ; 86 /r XCHG r/m8, r8
        mov B$LockPrefix &FALSE
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8_r8

    End_If


Op87:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE    ; 0F 87 cw/cd > JA rel16/32 ; JNBE rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'a ' | add edi 6

        jmp EndWithDisWordDwordRelative

    Else        ; 87 /r XCHG r/m16, r16 ; 87 /r XCHG r/m32, r32 ( and reverse)
        mov B$LockPrefix &FALSE
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_r32_r16__rm32_rm16

    End_If


Op88:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JS rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 's ' | add edi 6
        jmp EndWithDisWordDwordRelative

    Else    ; MOV r/m8,r8
        mov D$edi 'mov ' | add edi 4 | jmp Dis_rm8_r8

    End_If


Op89:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JNS rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'ns  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else        ; MOV r/m16,r16 ; MOV r/m32,r32
        mov D$edi 'mov ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    End_If


Op8A:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JP rel16/32 ; JPE rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'p ' | add edi 6
        jmp EndWithDisWordDwordRelative

    Else        ; MOV r8,r/m8
        mov D$edi 'mov ' | add edi 4 | jmp Dis_r8_rm8

    End_If


Op8B:
    inc D$LikelyCode

    If B$EscapePrefix = &TRUE           ; JNP rel16/32 ; JPO rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'np  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    Else        ; MOV r16,r/m16 ; MOV r32,r/m32
        mov D$edi 'mov ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16

    End_If


Op8C:
    .If B$EscapePrefix = &TRUE           ; JL rel16/32 ; JNGE rel16/32
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'l ' | add edi 6
        jmp EndWithDisWordDwordRelative

    .Else        ; MOV r/m16,Sreg** (there was a 066...)
        inc D$UnLikelyCode
        mov D$edi 'mov ' | add edi 4
        If B$OperandSizeOverride = &TRUE
            jmp Dis_rm16_Sreg
        Else
            jmp Dis_rm32_Sreg
        End_If
    .End_If
ret


Op8D:
    inc D$LikelyCode

    .If B$EscapePrefix = &TRUE           ; JGE rel16/32 ; JNL rel16/32
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'ge  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    .Else            ; LEA r16,m
        If B$esi > 00_10_111_111      ; Never: lea eax ebx
            inc D$UnLikelyCode
            ret
        Else

        ;If B$TestNow = 1
        ;    On D$esi+1 = 04398C0, int3
        ;End_If

            mov B$LeaInstruction &TRUE
            mov D$edi 'lea ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16
        End_If
    .End_If


Op8E:
    .If B$EscapePrefix = &TRUE           ; JLE rel16/32 ; JNG rel16/32
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', D$edi+4 'le  ' | add edi 7
        jmp EndWithDisWordDwordRelative

    .Else        ; MOV Sreg,r/m16** (there was a 066...)
        inc D$UnLikelyCode
        mov D$edi 'mov ' | add edi 4
        If B$OperandSizeOverride = &TRUE
            jmp Dis_Sreg_rm16
        Else
            jmp Dis_Sreg_rm32 ;jE! ALLOWED!!
        End_If

    .End_If
ret


Op8F:
    .If B$EscapePrefix = &TRUE           ; JG rel16/32 ; JNLE rel16/32
        inc D$LikelyCode
        mov B$CALLInstruction &TRUE
        SubEdi6 | mov D$edi ' | j', W$edi+4 'g ' | add edi 6
        jmp EndWithDisWordDwordRelative

    .Else
        mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0       ; 8F /0 POP m16
            mov D$edi 'pop ' | add edi 4 | jmp EndWithModRm
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If
    .End_If


Op90:
    If B$EscapePrefix = &FALSE  ; NOP ;  ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'nop ' | add edi 4

    Else     ; 0F 90 SETO r/m8
       ; inc D$UnLikelyCode
        mov D$edi 'seto', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op91: ; 0F 91 SETNO r/m8
    If B$EscapePrefix = &TRUE
       ; inc D$UnLikelyCode
        mov D$edi 'setn', W$edi+4 'o ' | add edi 6 | jmp Dis_rm8

    Else    ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    End_If


Op92: ; 0F 92 SETB r/m8 ; 0F 92 SETC r/m8  ; 0F 92 SETNAE r/m8
    If B$EscapePrefix = &FALSE    ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
        mov D$edi 'setc', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    End_If


Op93:
    If B$EscapePrefix = &FALSE   ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
      ; 0F 93 SETAE r/m8    ; 0F 93 SETNB r/m8  ; 0F 93 SETNC r/m8
        mov D$edi 'seta', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    End_If


Op94: ; 0F 94 SETE r/m8 ; 0F 94 SETZ r/m8
    If B$EscapePrefix = &FALSE    ; ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
        mov D$edi 'sete', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    End_If


Op95: ; 0F 95 SETNE r/m8 ; 0F 95 SETNZ r/m8
    If B$EscapePrefix = &FALSE    ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else
       ; inc D$UnLikelyCode
        mov D$edi 'setn', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    End_If


Op96:
    If B$EscapePrefix = &FALSE    ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else         ; 0F 96 SETBE r/m8    ; 0F 96 SETNA r/m8
       ; inc D$UnLikelyCode
        mov D$edi 'setb', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    End_If


Op97:
    If B$EscapePrefix = &FALSE    ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
        inc D$LikelyCode
        mov D$edi 'xchg', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__rd_rw

    Else         ; 0F 97 SETA r/m8 ; 0F 97 SETNBE r/m8
       ; inc D$UnLikelyCode
        mov D$edi 'seta', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    End_If


Op98:
    .If B$EscapePrefix = &TRUE   ; 0F 98 SETS r/m8
        mov D$edi 'sets', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    .Else ; cbw / cwde
        If B$OperandSizeOverride = &TRUE
            mov D$edi 'cbw ' | add edi 3
        Else
            mov D$edi 'cwde' | add edi 4
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op99:
    .If B$EscapePrefix = &TRUE   ; 0F 99 SETNS r/m8
        mov D$edi 'setn', W$edi+4 's ' | add edi 6 | jmp Dis_rm8

    .Else    ; CWD / CDQ
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'cdq ' | add edi 3
        Else
            mov D$edi 'cwd ' | add edi 3
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9A:

    ..If B$EscapePrefix = &TRUE      ; 0F 9A SETP r/m8 ; 0F 9A SETPE r/m8
        mov D$edi 'setp', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    ..Else
        add D$UnLikelyCode 0FF
        If B$OperandSizeOverride = &TRUE        ; call far ptr16:selector16 ; callF16
            mov D$edi 'call', D$edi+4 'F W$' | add edi 8 ; jE! fix
            push D$esi, esi
                Exchange W$esi, W$esi+2
                call WriteImm16 | mov B$edi ' ' | inc edi | call WriteImm16
            pop eax, D$eax
        Else                                ;   call far ptr32:selector16
            mov D$edi 'call', D$edi+4 'F D$' | add edi 8 ; jE! fix
            push D$esi, D$esi+4, esi
                mov eax D$esi, bx W$esi+4, W$esi bx, D$esi+2 eax
                call WriteImm16 | mov B$edi ' ' | inc edi | call WriteImm32
            pop eax D$eax+4, D$eax
        End_If

        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8
    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9B:
    ..If B$EscapePrefix = &TRUE     ; 0F 9B SETNP r/m8 ; 0F 9B SETPO r/m8
        ;inc D$UnLikelyCode
        mov D$edi 'setn', W$edi+4 'p ' | add edi 6 | jmp Dis_rm8

    ..Else
        .If W$esi = 0E2DB           ; 9B DB E2 > FCLEX
            mov D$edi 'fcle', W$edi+4 'x ' | add edi 5 | add esi 2

        .Else_If W$esi = 0E3DB      ; 9B DB E3 >FINIT
            mov D$edi 'fini', W$edi+4 't ' | add edi 5 | add esi 2

        .Else_If W$esi = 0E0DF      ; 9B DF E0 FSTSW AX
            mov D$edi 'fsts', D$edi+4 'w ax' | add edi 8 | add esi 2

        .Else_If B$esi = 0DD
            mov bl B$esi+1 | DigitMask bl to al

            If al = 6               ; 9B DD /6 > FSAVE m94/108byte
                mov D$edi 'fsav', W$edi+4 'e ' | add edi 6 | inc esi | lodsb
                jmp EndWith.X.mem
            Else_If al = 7          ; 9B DD /7 FSTSW m2byte
                mov D$edi 'fsts', W$edi+4 'w ' | add edi 6 | inc esi | lodsb
                jmp EndWith.W.mem
            Else
                jmp L5>
            End_If

        .Else_If B$esi = 0D9 ; opD9
            mov bl B$esi+1 | DigitMask bl to al

            If al = 6               ; 9B D9 /6 FSTENV m14/28byte
                mov D$edi 'fste', D$edi+4 'nv  ' | add edi 7 | inc esi | lodsb
                jmp EndWith.X.mem
            Else_If al = 7          ; 9B DD /6 > FSTCW m2byte
                mov D$edi 'fstc', W$edi+4 'w ' | add edi 6 | inc esi | lodsb
                jmp EndWith.W.mem
            Else
                jmp L5>
            End_If

        .Else                       ; 9B WAIT
L5:         mov D$edi 'wait' | add edi 4
        .End_If

    ..End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9C: ; setl

    .If B$EscapePrefix = &TRUE   ; 0F 9C SETL r/m8 ; 0F 9C SETNGE r/m8
        mov D$edi 'setl', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    .Else       ; 9C PUSHF 9C PUSHFD
        ;inc D$UnLikelyCode
        mov D$edi 'push', B$edi+4 'f' | add edi 5
        If B$OperandSizeOverride = &TRUE
            mov B$edi 'd' | inc edi
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9D:

    .If B$EscapePrefix = &TRUE   ; 0F 9D SETGE r/m8 ; 0F 9D SETNL r/m8
        mov D$edi 'setg', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    .Else    ; 9D POPF  ; 9D POPFD
        ;inc D$UnLikelyCode
        mov D$edi 'popf' | add edi 4
        If B$OperandSizeOverride = &TRUE
            mov B$edi 'd' | inc edi
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9E:
    .If B$EscapePrefix = &TRUE   ; 0F 9E SETLE r/m8 ; 0F 9E SETNG r/m8
        mov D$edi 'setl', W$edi+4 'e ' | add edi 6 | jmp Dis_rm8

    .Else       ; 9E SAHF
        ;inc D$UnLikelyCode
        mov D$edi 'sahf' | add edi 4

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Op9F:

    If B$EscapePrefix = &TRUE   ; 0F 9F SETG r/m8 ; 0F 9F SETNLE r/m8
        mov D$edi 'setg', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8

    Else    ; LAHF
        ;inc D$UnLikelyCode
        mov D$edi 'lahf' | add edi 4

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA0:
    If B$EscapePrefix = &TRUE   ; 0F A0 PUSH FS
        inc D$UnLikelyCode
        mov D$edi 'push', D$edi+4 ' fs ' | add edi 7

    Else ; MOV AL,moffs8*
        mov D$edi 'mov ', D$edi+4 'al B', B$edi+8 '$' | add edi 9 | call WriteImm32

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret



OpA1:
    .If B$EscapePrefix = &TRUE ; 0F A1 POP FS
        inc D$UnLikelyCode
        mov D$edi 'pop ', W$edi+4 'fs' | add edi 6

    .Else        ; MOV EAX,moffs32* ; MOV AX,moffs16*
        mov D$edi 'mov ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'eax ', W$edi+4 'D$' | add edi 6
        Else
            mov D$edi 'ax W', B$edi+4 '$' | add edi 5
        End_If
        If D$SegmentOverride <> 0
            move D$edi D$SegmentOverride | add edi 3
        End_If
        call WriteDis32 ; WriteImm32

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA2:
    If B$EscapePrefix = &TRUE ; CPUID
        mov D$edi 'cpui', B$edi+4 'd' | add edi 5

    Else        ; MOV moffs8*,AL
        mov D$edi 'mov ', D$edi+4 'B$  ' | add edi 6
        call WriteImm32 | mov D$edi ' al ' | add edi 3

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA3:
    .If B$EscapePrefix = &TRUE
        mov D$edi 'bt  ' | add edi 3 | jmp Dis_rm32_rm16__r32_r16

    .Else        ; MOV moffs32*,EAX ; MOV moffs16*,AX
        mov D$edi 'mov '
        If B$OperandSizeOverride = &FALSE
            mov D$edi+4 'D$  '
        Else
            mov D$edi+4 'W$  '
        End_If
        add edi 6
        If D$SegmentOverride <> 0
            move D$edi D$SegmentOverride | add edi 3
        End_If

        call WriteDis32

        If B$OperandSizeOverride = &FALSE
            mov D$edi ' eax' | add edi 4
        Else
            mov D$edi ' ax ' | add edi 3
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA4:
    inc  D$LikelyCode
    If B$EscapePrefix = &TRUE   ; 0F A4 SHLD r/m16, r16, imm8 ; 0F A4 SHLD r/m32, r32, imm8
        mov D$edi 'shld', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__r32_r16__imm8

    Else ; MOVSB
        mov D$edi 'movs', B$edi+4 'b' | add edi 5

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA5:
    inc D$LikelyCode
    .If B$EscapePrefix = &TRUE   ; 0F A5 SHLD r/m16, r16, CL ; 0F A5 SHLD r/m32, r32, CL
        mov D$edi 'shld', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__r32_r16__cl

    .Else
        If B$OperandSizeOverride = &FALSE  ; MOVSD
            mov D$edi 'movs', B$edi+4 'd'
        Else  ; MOVSW
            mov D$edi 'movs', B$edi+4 'w'
        End_If
        add edi 5

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA6: ; cmpsb
    inc D$LikelyCode
    mov D$edi 'cmps', B$edi+4 'b' | add edi 5
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA7: ; cmpsw / d
    inc D$LikelyCode
    mov D$edi 'cmps' | add edi 4
    If B$OperandSizeOverride = &TRUE
        mov B$edi 'w'
    Else
        mov B$edi 'd'
    End_If
    inc edi
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA8:
    If B$EscapePrefix = &TRUE   ; 0F A8 PUSH GS
        inc D$UnLikelyCode
        mov D$edi 'push', D$edi+4 ' gs ' | add edi 7

    Else    ; A8 ib TEST AL,imm8
        inc D$LikelyCode
        mov D$edi 'test', B$edi+4 ' ' | add edi 5 | jmp Dis_al_imm8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpA9: ; 0F A9 POP GS
    If B$EscapePrefix = &TRUE
        inc D$UnLikelyCode
        mov D$edi 'pop ', W$edi+4 'gs' | add edi 6

    Else   ; A9 iw TEST AX,imm16 ; A9 id TEST EAX,imm32
        inc D$LikelyCode
        mov D$edi 'test', B$edi+4 ' ' | add edi 5 | jmp Dis_eax_ax__imm32_imm16

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAA:
    If B$EscapePrefix = &TRUE    ; 0F AA RSM
        inc D$UnLikelyCode
        mov D$edi 'rsm ' | add edi 3

    Else       ; AA STOSB
        inc D$LikelyCode
        mov D$edi 'stos', B$edi+4 'b' | add edi 5

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAB:
    If B$EscapePrefix = &TRUE
        mov B$LockPrefix &FALSE
        mov D$edi 'bts ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16

    Else        ; AB STOSW ; AB STOSD
        inc D$LikelyCode
        mov D$edi 'stos', B$edi+4 'd' | add edi 5
        On B$OperandSizeOverride = &TRUE, mov B$edi-1 'w'

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAC:
    If B$EscapePrefix = &TRUE   ; 0F AC SHRD r/m16, r16, imm8 ; 0F AC SHRD r/m32, r32, mm8
        mov D$edi 'shrd', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__r32_r16__imm8

    Else ; LODSB
        inc D$LikelyCode
        mov D$edi 'lods', B$edi+4 'b' | add edi 5

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAD:
    .If B$EscapePrefix = &TRUE   ; 0F AD SHRD r/m16, r16, CL ; 0F AD SHRD r/m32, r32, CL
        mov D$edi 'shrd', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__r32_r16__cl

    .Else    ; LODSD ; LODSW
        inc D$LikelyCode
        mov D$edi 'lods', B$edi+4 'd'
        On B$OperandSizeOverride = &TRUE, mov B$edi+4 'w'
        add edi 5

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAE:
    ..If B$EscapePrefix = &TRUE
        movzx ebx B$esi | inc esi | DigitMask bl to al

        .If al = 0          ; FXSAVE m512byte
            mov D$edi 'fxsa', D$edi+4 've  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 1     ; FXRSTOR m512byte
            mov D$edi 'fxrs', D$edi+4 'tor ' | add edi 8 | jmp EndWith.X.mem
        .Else_If al = 2     ; LDMXCSR m32
            mov D$edi 'ldmx', D$edi+4 'csr ' | add edi 8 | jmp EndWith.D.mem
        .Else_If al = 3     ; 0F AE /3 STMXCSR m32
            mov D$edi 'stmx', D$edi+4 'csr ' | add edi 8 | jmp EndWith.D.mem
        .Else_If al = 5     ; LFENCE
            mov D$edi 'lfen', W$edi+4 'ce' | add edi 6
        .Else_If al = 6     ; MFENCE
            mov D$edi 'mfen', W$edi+4 'ce' | add edi 6
        .Else_If al = 7     ; 0F AE /7 CLFLUSH   ; 0F AE /7 SFENCE
            ModMask bl to al
            If al = 3
                mov D$edi 'sfen', D$edi+4 'ce  '  | add edi 6
            Else
                mov D$edi 'clfl', D$edi+4 'ush ' | add edi 8 | jmp Dis_m8
            End_If
        .Else
            dec esi | ret
        .End_If

    ..Else  ; AE SCASB
        mov D$edi 'scas', B$edi+4 'b' | add edi 5
    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpAF:  ; 0F AF /r > IMUL r16,r/m16 ou 32
    inc D$LikelyCode
    If B$EscapePrefix = &TRUE
        mov D$edi 'imul', B$edi+4 ' ' | add edi 5 | jmp Dis_r32_r16__rm32_rm16

    Else        ; AF SCASD AF SCASW
         mov D$edi 'scas', B$edi+4 'd' | add edi 5
         On B$OperandSizeOverride = &TRUE, mov B$edi-1 'w'

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB0:
    If B$EscapePrefix = &FALSE  ; MOV r8,imm8
        inc D$LikelyCode
        mov D$edi 'mov ', D$edi+4 'al  ' | add edi 7 | call WriteImm8

    Else     ; CMPXCHG r/m8,r8
        ;inc D$UnLikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'cmpx', D$edi+4 'chg ' | add edi 8 | jmp Dis_rm8_r8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB1: ; CMPXCHG r/m32,r32 (+16)
    If B$EscapePrefix = &FALSE
        mov D$edi 'mov ', D$edi+4 'cl  ' | add edi 7 | call WriteImm8

    Else
        mov B$LockPrefix &FALSE
        mov D$edi 'cmpx', D$edi+4 'chg ' | add edi 8 | jmp Dis_rm32_rm16__r32_r16

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB2: ; 0F B2 > LSS r16,m16:16 ; LSS r32,m16:32
    If B$EscapePrefix = &FALSE
        mov D$edi 'mov ', D$edi+4 'dl  ' | add edi 7 | call WriteImm8

    Else
        inc D$UnLikelyCode
        mov D$edi 'lss ' | add edi 4 | call Dis_r32_r16__rm32_rm16 ; LSL
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB3:
    If B$EscapePrefix = &FALSE
        mov D$edi 'mov ', D$edi+4 'bl  ' | add edi 7 | call WriteImm8

    Else
        ;inc D$UnLikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'btr ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16 ; BTR

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB4:
    If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        mov D$edi 'mov ', D$edi+4 'ah  ' | add edi 7 | call WriteImm8

    Else     ; 0F B4 > LFS r16,m16:16 ; LFS r32,m16:32
        inc D$UnLikelyCode
        mov D$edi 'lfs ' | add edi 4 | call Dis_r32_r16__rm32_rm16
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB5:
    If B$EscapePrefix = &FALSE
        mov D$edi 'mov ', D$edi+4 'ch  ' | add edi 7 | call WriteImm8

    Else     ; LGS r16,m16:16 ; LGS r32,m16:32
        inc D$UnLikelyCode
        mov D$edi 'lgs ' | add edi 4 | call Dis_r32_r16__rm32_rm16
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB6:
    If B$EscapePrefix = &FALSE
        mov D$edi 'mov ', D$edi+4 'dh  ' | add edi 7 | call WriteImm8

    Else        ; 0F B6 /r MOVZX r16,r/m8 ; 0F B6 /r MOVZX r32,r/m8
        inc D$LikelyCode
        mov D$edi 'movz', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_r16__rm8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB7:
    If B$EscapePrefix = &FALSE
        mov D$edi 'mov ', D$edi+4 'bh  ' | add edi 7 | call WriteImm8

    Else        ; 0F B7 /r MOVZX r32,r/m16
        inc D$LikelyCode
        mov D$edi 'movz', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_rm16

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB8:
    inc D$LikelyCode
    If B$OperandSizeOverride = &FALSE
        mov D$edi 'mov ', D$edi+4 'eax ' | add edi 8 | call WriteImm32
    Else
        mov D$edi 'mov ', D$edi+4 'ax  ' | add edi 7 | call WriteImm16
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpB9:
    inc D$LikelyCode
    If B$OperandSizeOverride = &FALSE
        mov D$edi 'mov ', D$edi+4 'ecx ' | add edi 8 | call WriteImm32

    Else
        mov D$edi 'mov ', D$edi+4 'cx  ' | add edi 7 | call WriteImm16

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBA:
    .If B$EscapePrefix = &TRUE
        movzx ebx B$esi | inc esi | DigitMask bl to al

        If al = 4
            mov D$edi 'bt  ' | add edi 3 ; bts, ...
        Else_If al = 5
            mov B$LockPrefix &FALSE
            mov D$edi 'bts ' | add edi 4
        Else_If al = 6
            mov B$LockPrefix &FALSE
            mov D$edi 'btr ' | add edi 4
        Else_If al = 7
            mov B$LockPrefix &FALSE
            mov D$edi 'btc ' | add edi 4
        Else
            dec esi | ret
        End_If
        jmp Dis_rm32_rm16__imm8

    .Else
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'mov ', D$edi+4 'edx ' | add edi 8 | call WriteImm32
        Else
            mov D$edi 'mov ', D$edi+4 'dx  ' | add edi 7 | call WriteImm16
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBB:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'btc ' | add edi 4 | jmp Dis_rm32_rm16__r32_r16 ; BTC

    .Else
        inc D$LikelyCode
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'mov ', D$edi+4 'ebx ' | add edi 8 | call WriteImm32
        Else
            mov D$edi 'mov ', D$edi+4 'bx  ' | add edi 7 | call WriteImm16
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBC:
    .If B$EscapePrefix = &TRUE
        mov D$edi 'bsf ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16 ; bsf

    .Else
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'mov ', D$edi+4 'esp ' | add edi 8 | call WriteImm32
        Else
            inc D$UnLikelyCode
            mov D$edi 'mov ', D$edi+4 'sp  ' | add edi 7 | call WriteImm16
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBD:
    .If B$EscapePrefix = &TRUE
        mov D$edi 'bsr ' | add edi 4 | jmp Dis_r32_r16__rm32_rm16 ; bsr

    .Else
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'mov ', D$edi+4 'ebp ' | add edi 8 | call WriteImm32
        Else
            inc D$UnLikelyCode
            mov D$edi 'mov ', D$edi+4 'bp  ' | add edi 7 | call WriteImm16
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBE:
    inc D$LikelyCode
    .If B$EscapePrefix = &FALSE
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'mov ', D$edi+4 'esi ' | add edi 8 | call WriteImm32
        Else
            mov D$edi 'mov ', D$edi+4 'si  ' | add edi 7 | call WriteImm16
        End_If

    .Else
      ; 0F BE ¯r MOVSX r32,r/m8  ; 0F BE /r MOVSX r16,r/m8
        mov D$edi 'movs', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_r16__rm8

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpBF:
    inc D$LikelyCode
    .If B$EscapePrefix = &FALSE
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'mov ', D$edi+4 'edi ' | add edi 8
            call WriteImm32
        Else
            mov D$edi 'mov ', D$edi+4 'di  ' | add edi 7
            call WriteImm16
        End_If

    .Else       ; 0F BF /r MOVSX r32,r/m16
        mov D$edi 'movs', W$edi+4 'x ' | add edi 6 | jmp Dis_r32_rm16

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC0:
    .If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0   ; C0 /0 ib ROL r/m8, imm8
            mov D$edi 'rol '
        Else_If al = 1  ; C0 /1 ib ROR r/m8, imm8
            mov D$edi 'ror '
        Else_If al = 2  ; C0 /2 ib RCL r/m8, imm8
            mov D$edi 'rcl '
        Else_If al = 3  ; C0 /3 ib RCR r/m8, imm8
            mov D$edi 'rcr '
        Else_If al = 4   ; C0 /4 ib SAL r/m8,imm8   ; C0 /4 ib SHL r/m8,imm8
            mov D$edi 'shl '
        Else_If al = 5  ; C0 /5 ib SHR r/m8,imm8
           mov D$edi 'shr '
        Else_If al = 7  ; C0 /7 ib SAR r/m8,imm8
            mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm8_imm8

    .Else   ; 0F C0 /r XADD r/m8, r8
        mov B$LockPrefix &FALSE
        mov D$edi 'xadd', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8_r8

    .End_If


OpC1:
    .If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        movzx ebx B$esi | inc esi | DigitMask bl to al

        If al = 0   ; C1 /0 ib ROL r/m16, imm8  C1 /0 ib ROL r/m32, imm8
            mov D$edi 'rol '
        Else_If al = 1    ; C1 /1 ib ROR r/m16, imm8    ; C1 /1 ib ROR r/m32, imm8
            mov D$edi 'ror '
        Else_If al = 2    ; C1 /2 ib RCL r/m16, imm8 ; C1 /2 ib RCL r/m32,i mm8
            mov D$edi 'rcl '
        Else_If al = 3  ; C1 /3 ib RCR r/m16, imm8  C1 /3 ib RCR r/m32, imm8
            mov D$edi 'rcr '
        Else_If al = 4  ; C1 /4 ib SAL r/m16,imm8   ; C1 /4 ib SAL r/m32,imm8 ; C1 /4 ib SHL r/m32,imm8  ; C1 /4 ib SHL r/m16,imm8
            mov D$edi 'shl '
        Else_If al = 5  ; C1 /5 ib SHR r/m16,imm8   ; C1 /5 ib SHR r/m32,imm8
            mov D$edi 'shr '
        Else_If al = 7  ; C1 /7 ib SAR r/m16,imm8   ; C1 /7 ib SAR r/m32,imm8
            mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm32_rm16__imm8

    .Else   ; 0F C1 /r XADD r/m16, r16 ; 0F C1 /r XADD r/m32, r32
        mov B$LockPrefix &FALSE
        mov D$edi 'xadd', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__r32_r16

    .End_If


OpC2:
    .If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'cmp_'
        If B$OperandSizeOverride = &TRUE
          ; 66 0F C2 /r ib CMPPD xmm1, xmm2/m128, imm8
          ; 66 0F C2 _1D_ 010 34 42 00 01
            call MarkSSEdata SSE_2_R    ; CMPPD 66 0F C2 /r ib
            mov D$edi+4 'pd  '
        Else
          ; cmpps xmm1, xmm2/m128, imm8
            call MarkSSEdata SSE_4_F    ; CMPPS 0F C2 /r ib
            mov D$edi+4 'ps  '
        End_If

        add edi 7
        call Dis_xmm1__xmm2_m128 | jmp WritePacketCondition

    .Else       ; C2 iw RET imm16
        inc D$LikelyCode
        mov D$edi 'ret ' | add edi 4 | call WriteImm16
        mov al CR | stosb | mov al LF | stosb
        mov B$DisEndOfChunk &TRUE

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC3:
    If B$EscapePrefix = &FALSE  ; C3 RET
        inc D$LikelyCode
        mov D$edi 'ret ' | add edi 3
        mov al CR | stosb | mov al LF | stosb
        mov B$DisEndOfChunk &TRUE

    Else        ; 0F C3 /r MOVNTI m32, r32
        ;inc D$UnLikelyCode
        mov D$edi 'movn', D$edi+4 'ti  ' | add edi 7 | jmp Dis_m32_r32

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC4:
    .If B$EscapePrefix = &TRUE
      ; 66 0F C4 /r ib PINSRW xmm, r32/m16, imm8 ; 0F C4 /r ib PINSRW mm, r32/m16, imm8
        mov D$edi 'pins', D$edi+4 'rw  ' | add edi 7
        jmp Dis_PINSRW

    .Else        ; LES r16,m16:16 ; LES r32,m16:32
        mov D$edi 'les ' | add edi 4 | call Dis_r32_r16__rm32_rm16
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8
        inc D$UnLikelyCode

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC5:
    .If B$EscapePrefix = &TRUE
      ; 66 0F C5 /r ib PEXTRW r32, xmm, imm8 ; 0F C5 /r ib PEXTRW r32, mm, imm8
        mov D$edi 'pext', D$edi+4 'rw  ' | add edi 7
        jmp Dis_r32_xmmx_imm8
       ; call Dis_r32_xmmx | mov B$edi ' ' | inc edi | call Writeimm8
;Dis_xmmx_r32
    .Else        ; LDS r16,m16:16
        inc D$UnLikelyCode
        mov D$edi 'lds ' | add edi 4 | call Dis_r32_r16__rm32_rm16
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC6:
    ..If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
      ; 66 0F C6 /r ib SHUFPD xmm1, xmm2/m128, imm8 ; 0F C6 /r ib SHUFPS xmm1, xmm2/m128, imm8
        mov D$edi 'shuf'
        If B$OperandSizeOverride = &TRUE
            call MarkSSEdata SSE_2_R
            mov D$edi+4 'pd  '
        Else
            call MarkSSEdata SSE_4_F
            mov D$edi+4 'ps  '
        End_If
        add edi 7 | jmp Dis_xmm1__xmm2_m128__imm8

    ..Else
        mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0     ; MOV r/m8,imm8
            inc D$LikelyCode
            mov D$edi 'mov ' | add edi 4 | jmp Dis_rm8_imm8
        Else
            dec esi | ret
        End_If

    ..End_If


OpC7:
    movzx ebx B$esi | inc esi | DigitMask bl to al

    .If B$EscapePrefix = &FALSE
        inc D$LikelyCode
        If al = 0       ; MOV r/m32,imm32 ; MOV r/m16,imm16
            mov B$MovOrJmpImmInstruction &TRUE
            mov D$edi 'mov ' | add edi 4 | jmp Dis_rm32_rm16__imm32_imm16
        End_If

    .Else    ; 0F C7 /1 m64 CMPXCHG8B m64
        If al = 1
            ;inc D$UnLikelyCode
            mov B$LockPrefix &FALSE
            mov D$edi 'cmpx', D$edi+4 'chg8', W$edi+8 'b ' | add edi 10 | jmp Dis_m64
        End_If

    .End_If

    dec esi
ret


OpC8:
    .If B$EscapePrefix = &FALSE ; ENTER imm16,imm8
        mov D$edi 'ente', W$edi+4 'r ' | add edi 6
      ; Enter Max is 0FFFC and must be 4 Bytes Aligned:
        If W$esi > 0FFFC
            add D$UnlikelyCode 5
        Else
            test W$esi 00_11 | jz L1>
                add D$UnlikelyCode 5
        End_If

L1:     On B$esi+2 > 31, add D$UnlikelyCode 5
        call WriteImm16 | mov B$edi ' ' | inc edi
        call WriteImm8

    .Else
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'eax ' | add edi 9

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpC9:
    If B$EscapePrefix = &FALSE
        mov D$edi 'leav', B$edi+4 'e' | add edi 5

    Else
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'ecx ' | add edi 9

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCA:
    If B$EscapePrefix = &FALSE        ; CA iw RET imm16
        inc D$UnLikelyCode
        mov D$edi 'retf', B$edi+4 ' ' | add edi 5 | call WriteImm16
        mov B$DisEndOfChunk &TRUE

    Else
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'edx ' | add edi 9

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCB:
    If B$EscapePrefix = &FALSE        ; CB RET
        inc D$UnLikelyCode
        mov D$edi 'retF' | add edi 4
        mov B$DisEndOfChunk &TRUE

    Else
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'ebx ' | add edi 9

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCC:
    .If B$EscapePrefix = &TRUE
        inc D$UnLikelyCode
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'esp ' | add edi 9

    .Else        ; INT 3
        mov D$edi 'int ', B$edi+4 '3' | add edi 5
        If B$No0CC = &FALSE
            mov B$DisEndOfChunk &TRUE
        Else
            inc D$UnLikelyCode
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCD:
    If B$EscapePrefix = &TRUE
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'ebp ' | add edi 9

    Else        ; INT imm8
        inc D$UnLikelyCode
        mov D$edi 'int ' | add edi 4 | call WriteImm8

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCE:
    If B$EscapePrefix = &TRUE
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'esi ' | add edi 9

    Else        ; INTO
        inc D$UnLikelyCode
        mov D$edi 'into' | add edi 4

    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpCF:
    .If B$EscapePrefix = &TRUE
        mov D$edi 'bswa', W$edi+4 'p ', D$edi+6 'edi ' | add edi 9

    .Else       ; IRET IRETD
        inc D$UnLikelyCode
        mov D$edi 'iret' | add edi 4
        If B$OperandSizeOverride = &FALSE
            mov B$edi 'd' | inc edi
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpD0:
    inc D$LikelyCode
    mov bl B$esi | inc esi | DigitMask bl To al

    If al = 0       ; D0 /0 ROL r/m8, 1
        mov D$edi 'rol '
    Else_If al = 1  ; D0 /1 ROR r/m8, 1
        mov D$edi 'ror '
    Else_If al = 2  ; D0 /2 RCL r/m8, 1
        mov D$edi 'rcl '
    Else_If al = 3  ; D0 /3 RCR r/m8, 1
        mov D$edi 'rcr '
    Else_If al = 4  ; D0 /4 SAL r/m8,1  ; D0 /4 SHL r/m8,1
        mov D$edi 'shl '
    Else_If al = 5  ; D0 /5 SHR r/m8,1
        mov D$edi 'shr '
    Else_If al = 7  ; D0 /7 SAR r/m8,1
        mov D$edi 'sar '
    Else
        dec esi | ret
    End_If

    add edi 4 | jmp Dis_rm8_1



OpD1:
    .If B$EscapePrefix = &TRUE ; Op0F
        ;inc D$UnLikelyCode
      ; 0F D1 /r PSRLW mm, mm/m64       ; 66 0F D1 /r PSRLW xmm1, xmm2/m128
        If B$esi-3 <> 0F
            mov D$edi 'psrl', W$edi+4 'w ' | add edi 6
            jmp Dis_xmmx1__xmmx2_m64_128
        End_If

    .Else
        inc D$LikelyCode
        mov bl B$esi | inc esi | DigitMask bl To al

        If al = 0   ; D1 /0 ROL r/m16, 1    D1 /0 ROL r/m32, 1
            mov D$edi 'rol '
        Else_If al = 1   ; D1 /1 ROR r/m16, 1   ; D1 /1 ROR r/m32, 1
            mov D$edi 'ror '
        Else_If al = 2   ; D1 /2 RCL r/m16, 1 D1 /2 RCL r/m32, 1
            mov D$edi 'rcl '
        Else_If al = 3  ; D1 /3 RCR r/m16, 1    ; D1 /3 RCR r/m32, 1
            mov D$edi 'rcr '
        Else_If al = 4  ; D1 /4 SAL r/m16,1 ; D1 /4 SAL r/m32,1 ; D1 /4 SHL r/m16,1; D1 /4 SHL r/m32,1
            mov D$edi 'shl '
        Else_If al = 5  ; D1 /5 SHR r/m16,1 ; D1 /5 SHR r/m32,1
            mov D$edi 'shr '
        Else_If al = 7  ; D1 /7 SAR r/m16,1 ; D1 /7 SAR r/m32,1
            mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm32_rm16__1

    .End_If


OpD2:
    .If B$EscapePrefix = &TRUE
      ; 0F D2 /r PSRLD mm, mm/m64   ; 66 0F D2 /r PSRLD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psrl', W$edi+4 'd ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$LikelyCode
        mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0       ; D2 /0 ROL r/m8, CL
            mov D$edi 'rol '
        Else_If al = 1      ;     D2 /1 ROR r/m8, CL
             mov D$edi 'ror '
        Else_If al = 2      ; D2 /2 RCL r/m8, CL
            mov D$edi 'rcl '
        Else_If al = 3 ; D2 /3 RCR r/m8, CL
            mov D$edi 'rcr '
        Else_If al = 4   ; D2 /4 SAL r/m8,CL    ; D2 /4 SHL r/m8,CL
            mov D$edi 'shl '
        Else_If al = 5  ; D2 /5 SHR r/m8,CL
            mov D$edi 'shr '
        Else_If al = 7  ; D2 /7 SAR r/m8,CL
            mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm8_cl

    .End_If


OpD3:
    .If B$EscapePrefix = &TRUE
      ; 0F D3 /r PSRLQ mm, mm/m64   ; 66 0F D3 /r PSRLQ xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psrl', W$edi+4 'q ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$LikelyCode
        mov bl B$esi | inc esi | DigitMask bl to al

        If al = 0   ; D3 /0 ROL r/m16, CL   D3 /0 ROL r/m32, CL
            mov D$edi 'rol '
        Else_If al = 1      ; D3 /1 ROR r/m16, CL    ; D3 /1 ROR r/m32, CL
            mov D$edi 'ror '
        Else_If al = 2      ; D3 /2 RCL r/m16, CL    D3 /2 RCL r/m32, CL
            mov D$edi 'rcl '
        Else_If al = 3  ; D3 /3 RCR r/m16, CL   D3 /3 RCR r/m32, CL
            mov D$edi 'rcr '
        Else_If al = 4  ; D3 /4 SAL r/m16,CL    ; D3 /4 SAL r/m32,CL  ;D3 /4 SHL r/m32,CL  ; D3 /4 SHL r/m16,CL
            mov D$edi 'shl '
        Else_If al = 5  ; D3 /5 SHR r/m16,CL    ; D3 /5 SHR r/m32,CL
            mov D$edi 'shr '
        Else_If al = 7  ; D3 /7 SAR r/m16,CL    ; D3 /7 SAR r/m32,CL
            mov D$edi 'sar '
        Else
            dec esi | ret
        End_If

        add edi 4 | jmp Dis_rm32_rm16__cl

    .End_If


OpD4:
    .If B$EscapePrefix = &TRUE
        ; 66 0F D4 /r PADDQ xmm1,xmm2/m128       ; 0F D4 /r PADDQ mm1,mm2/m64
        ;inc D$UnLikelyCode
        mov D$edi 'padd', W$edi+4 'q ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        mov D$edi 'aam ', B$DisFlag DISDONE+DISLINEOVER | add edi 4
        lodsb
        If al <> 0A
            call LoadedOpToHexa | stosw
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpD5:
    .If B$EscapePrefix = &TRUE
      ; 0F D5 /r PMULLW mm, mm/m64      ; 66 0F D5 /r PMULLW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'pmul', D$edi+4 'lw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        mov D$edi 'aad ', B$DisFlag DISDONE+DISLINEOVER | add edi 4
        lodsb
        If al <> 0A
            call loadedOpToHexa | stosw
        End_If

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER

ret


OpD6:
    .If B$EscapePrefix = &TRUE
        If B$OperandSizeOverride = &TRUE    ; 66 0F D6 MOVQ xmm2/m64, xmm1
            mov D$edi 'movq', B$edi+4 ' ' | add edi 5 | jmp Dis_xmm2_m64__xmm1
        End_If
    .Else
        mov D$edi 'salc'  | add edi 4
        mov B$DisFlag DISDONE+DISLINEOVER
    .End_If
ret


OpD7:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
      ; 66 0F D7 /r PMOVMSKB r32, xmm ; 0F D7 /r PMOVMSKB r32, mm
        mov D$edi 'pmov', D$edi+4 'mskb', B$edi+8 ' ' | add edi 9 | jmp Dis_r32_xmmx

    .Else       ; D7 XLATB
        mov D$edi 'xlat', B$edi+4 'b' | add edi 5

    .End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpD8:
    ..If B$EscapePrefix = &TRUE
      ; 0F D8 /r PSUBUSB mm, mm/m64 ; 66 0F D8 /r PSUBUSB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psub', D$edi+4 'usb ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    ..Else
        mov bl B$esi | inc esi | DigitMask bl To al

        .If al = 0          ; D8 /0 FADD m32fp ; D8 C0+i FADD ST(0), ST(i)
            mov D$edi 'fadd', B$edi+4 ' ' | add edi 5
        .Else_If al = 1     ; D8 /1 FMUL m32fp ; D8 C8+i FMUL ST(0), ST(i)
            mov D$edi 'fmul', B$edi+4 ' ' | add edi 5
        .Else_If al = 2  ; FCOM m32fp ; FCOM ST(i)
            mov D$edi 'fcom', B$edi+4 ' ' | add edi 5
        .Else_If al = 3     ; FCOMP m32fp; FCOMP ST(i)
           mov D$edi 'fcom', W$edi+4 'p ' | add edi 6
        .Else_If al = 4     ; FSUB m32fp ; FSUB ST(0), ST(i)
            mov D$edi 'fsub', B$edi+4 ' ' | add edi 5
        .Else_If al = 5     ; FSUBR m32fp ; FSUBR ST(0), ST(i)
            mov D$edi 'fsub', W$edi+4 'r ' | add edi 6
        .Else_If al = 6     ; FDIV m32fp ; FDIV ST(0), ST(i)
            mov D$edi 'fdiv', B$edi+4 ' ' | add edi 5
        .Else_If al = 7     ; FDIVR m32fp ; FDIVR ST(0), ST(i)
            mov D$edi 'fdiv', W$edi+4 'r ' | add edi 6
        .End_If

        jmp Dis_St0Sti_or_Fmem

    ..End_If
ret


OpD9:
    If B$EscapePrefix = &TRUE
      ; 0F D9 /r PSUBUSW mm, mm/m64     ; 66 0F D9 /r PSUBUSW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psub', D$edi+4 'usw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al  ; 0EE 0011_101_110

    ..If al = 3
        mov al bl
        .If al = 0E4       ; D9 E4 FTST
            mov D$edi 'ftst' | add edi 4
        .Else_If al = 0E5  ; FXAM
            mov D$edi 'fxam' | add edi 4
        .Else_If al = 0F0  ; F2XM1
            mov D$edi 'f2xm', B$edi+4 '1' | add edi 5
        .Else_If al = 0F1  ; FYL2X
            mov D$edi 'fyl2', B$edi+4 'x' | add edi 5
        .Else_If al = 0F2  ; FPTAN
            mov D$edi 'fpta', B$edi+4 'n' | add edi 5
        .Else_If al = 0F3  ; FPATAN
            mov D$edi 'fpat', W$edi+4 'an' | add edi 6
        .Else_If al = 0F4  ; FXTRACT
            mov D$edi 'fxtr', D$edi+4 'act ' | add edi 7
        .Else_If al = 0F5  ; FPREM1
            mov D$edi 'fpre', W$edi+4 'm1' | add edi 6
        .Else_If al = 0F8  ; FPREM
            mov D$edi 'fpre', B$edi+4 'm' | add edi 5
        .Else_If al = 0F9  ; FYL2XP1
            mov D$edi 'fyl2', D$edi+4 'xp1 ' | add edi 7
        .Else_If al = 0FA  ; FSQRT
            mov D$edi 'fsqr', B$edi+4 't' | add edi 5
        .Else_If al = 0FB  ; FSINCOS
            mov D$edi 'fsin', D$edi+4 'cos ' | add edi 7
        .Else_If al = 0FC  ; FRNDINT
            mov D$edi 'frnd', D$edi+4 'int ' | add edi 7
        .Else_If al = 0FD  ; FSCALE
            mov D$edi 'fsca', W$edi+4 'le' | add edi 6
        .Else_If al = 0FE  ; FSIN
            mov D$edi 'fsin' | add edi 4
        .Else_If al = 0D0  ; FNOP
            mov D$edi 'fnop' | add edi 4
        .Else_If al = 0E1  ; FABS
            mov D$edi 'fabs' | add edi 4
        .Else_If al = 0E0  ; FCHS
            mov D$edi 'fchs' | add edi 4
        .Else_If al = 0E8  ; FLD1
            mov D$edi 'fld1' | add edi 4
        .Else_If al = 0E9  ; FLDL2T
            mov D$edi 'fldl', W$edi+4 '2t' | add edi 6
        .Else_If al = 0EA  ; FLDL2E
            mov D$edi 'fldl', W$edi+4 '2e' | add edi 6
        .Else_If al = 0EB  ; FLDPI
            mov D$edi 'fldp', B$edi+4 'i' | add edi 5
        .Else_If al = 0EC  ; FLDLG2
            mov D$edi 'fldl', W$edi+4 'g2' | add edi 6
        .Else_If al = 0ED  ; FLDLN2
            mov D$edi 'fldl', W$edi+4 'n2' | add edi 6
        .Else_If al = 0EE  ; FLDZ
            mov D$edi 'fldz' | add edi 4
        .Else_If al = 0F6  ; FDECSTP
            mov D$edi 'fdec', D$edi+4 'stp ' | add edi 7
        .Else_If al = 0F7  ; FINCSTP
            mov D$edi 'finc', D$edi+4 'stp ' | add edi 7
        .Else_If al = 0FF  ; FCOS
            mov D$edi 'fcos' | add edi 4
        .Else
            and eax (not 7)
            If al = 0C0         ; D9 C0+i > FLD ST(i)
                mov D$edi 'fld ' | add edi 4
            Else_If al = 0C8    ; D9 C8+i FXCH ST(i)
                mov D$edi 'fxch', B$edi+4 ' ' | add edi 5
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

            call WriteSti

        .End_If

    ..Else
        DigitMask bl To al ; 0EE

        .If al = 0            ; D9 /0 > FLD m32fp
            mov D$edi 'fld ' | add edi 4 | jmp EndWith.F.mem
        .Else_If al = 2       ; FST m32fp
            mov D$edi 'fst ' | add edi 4 | jmp EndWith.F.mem
        .Else_If al = 3       ; FSTP m32fp
            mov D$edi 'fstp', B$edi+4 ' ' | add edi 5 | jmp EndWith.F.mem
        .Else_If al = 4       ; FLDENV m14/28byte
            mov D$edi 'flde', D$edi+4 'nv  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 5       ; D9 /5 > FLDCW m2byte
            mov D$edi 'fldc', W$edi+4 'w ' | add edi 6
        .Else_If al = 6       ; D9 /6FNSTENV* m14/28byte
            mov D$edi 'fnst', D$edi+4 'env ' | add edi 8 | jmp EndWith.X.mem
        .Else_If al = 7       ; D9 /7 > FNSTCW m2byte
            mov D$edi 'fnst', D$edi+4 'cw  ' | add edi 7
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

        jmp EndWith.W.mem

    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDA:
    If B$EscapePrefix = &TRUE
      ; 0F DA /r PMINUB mm1, mm2/m64      ; 66 0F DA /r PMINUB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'pmin', D$edi+4 'ub  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        mov al bl

        .If al = 0E9     ; FUCOMPP
            mov D$edi 'fuco', D$edi+4 'mpp ' | add edi 7
            mov B$DisFlag DISDONE+DISLINEOVER | ret

        .Else
            and eax (not 7)
            If al = 0C0         ; DA C0+i : FCMOVB ST(0), ST(i)
                mov D$edi 'fcmo', D$edi+4 'vb  ' | add edi 7
            Else_If al = 0C8    ; FCMOVE ST(0), ST(i)
                mov D$edi 'fcmo', D$edi+4 've  ' | add edi 7
            Else_If al = 0D0    ; FCMOVBE ST(0), ST(i)
                mov D$edi 'fcmo', D$edi+4 'vbe ' | add edi 8
            Else_If al = 0D8    ; FCMOVU ST(0), ST(i)
                mov D$edi 'fcmo', D$edi+4 'vu  ' | add edi 7
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

        .End_If

        call WriteSt0Sti

    ..Else
        DigitMask bl To al

        .If al = 0          ; FIADD m32int
            mov D$edi 'fiad', W$edi+4 'd ' | add edi 6
        .Else_If al = 1     ; FIMUL m32int
            mov D$edi 'fimu', W$edi+4 'l ' | add edi 6
        .Else_If al = 2     ; FICOM m32int
            mov D$edi 'fico', W$edi+4 'm ' | add edi 6
        .Else_If al = 3     ; FICOMP m32int
            mov D$edi 'fico', D$edi+4 'mp  ' | add edi 7
        .Else_If al = 4     ; FISUB m32int
            mov D$edi 'fisu', W$edi+4 'b ' | add edi 6
        .Else_If al = 5     ; FISUBR m32int
            mov D$edi 'fisu', D$edi+4 'br  ' | add edi 7
        .Else_If al = 6     ; FIDIV m32int
            mov D$edi 'fidi', W$edi+4 'v ' | add edi 6
        .Else_If al = 7     ; FIDIVR m32int
            mov D$edi 'fidi', D$edi+4 'vr  ' | add edi 7
        .End_If

        jmp EndWith.D.mem

    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret




OpDB:
    If B$EscapePrefix = &TRUE
        ; 66 0F DB /r PAND xmm1, xmm2/m128      ; 0F DB /r PAND mm, mm/m64
        mov D$edi 'pand', B$edi+4 ' ' | add edi 5
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        mov al bl

        If al = 0E3      ; DB E3 FNINIT >>> E3 DigitBit = 4
            mov D$edi 'fnin', D$edi+4 'it  '
L0:         add edi 6 | mov B$DisFlag DISDONE+DISLINEOVER | ret
        Else_If al = 0E2    ; DB E2 FNCLEX*
            mov D$edi 'fncl', D$edi+4 'ex  ' | jmp L0<
        End_If

        and eax (not 7)

        .If al = 0C0        ; DA C0+i : FCMOVNB ST(0), ST(i)
            mov D$edi 'fcmo', D$edi+4 'vnb ' | add edi 8
        .Else_If al = 0C8   ; FCMOVNE ST(0), ST(i)
            mov D$edi 'fcmo', D$edi+4 'vne ' | add edi 8
        .Else_If al = 0D0   ; FCMOVNBE ST(0), ST(i)
            mov D$edi 'fcmo', D$edi+4 'vnbe', B$edi+8 ' ' | add edi 9
        .Else_If al = 0D8   ; FCMOVNU ST(0), ST(i)
            mov D$edi 'fcmo', D$edi+4 'vnu ' | add edi 8
        .Else_If al = 0E8   ; FUCOMI ST, ST(i)
            mov D$edi 'fuco', D$edi+4 'mi  ' | add edi 7
        .Else_If al = 0F0   ; FCOMI ST, ST(i)
            mov D$edi 'fcom', W$edi+4 'i ' | add edi 6
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

        call WriteSt0Sti

    ..Else
        DigitMask bl To al

        .If al = 0     ; FILD m32int
            mov D$edi 'fild', B$edi+4 ' ' | add edi 5 | jmp EndWith.D.mem
        .Else_If al = 1        ; FISTTP  DB /1 FISTTP m32int
            mov D$edi 'fist', D$edi+4 'tp ' | add edi 7 | jmp EndWith.D.mem
        .Else_If al = 2        ; FIST m32int
            mov D$edi 'fist', B$edi+4 ' ' | add edi 5 | jmp EndWith.D.mem
        .Else_If al = 3        ; FISTP m32int
            mov D$edi 'fist', W$edi+4 'p ' | add edi 6 | jmp EndWith.D.mem
        .Else_If al = 5        ; FLD m80fp
            mov D$edi 'fld ' | add edi 4 | jmp EndWith.T.mem
        .Else_If al = 7        ; FSTP m80fp
            mov D$edi 'fstp', B$edi+4 ' ' | add edi 5 | jmp EndWith.T.mem
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

     ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDC:
    If B$EscapePrefix = &TRUE
        ; 66 0F DC /r PADDUSB xmm1, xmm2/m128     ; 0F DC /r PADDUSB mm, mm/m64
        ;inc D$UnLikelyCode
        mov D$edi 'padd', D$edi+4 'usb ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        mov al bl | and eax (not 7)

        If al = 0C0             ; FADD ST(i), ST(0)
            mov D$edi 'fadd', B$edi+4 ' ' | add edi 5
        Else_If al = 0C8        ; FMUL ST(i), ST(0)
            mov D$edi 'fmul', B$edi+4 ' ' | add edi 5
        Else_If al = 0E0        ; FSUBR ST(i), ST(0)
            mov D$edi 'fsub', W$edi+4 'r ' | add edi 6  ; fsubr
        Else_If al = 0E8        ; FSUB ST(i), ST(0)
            mov D$edi 'fsub', B$edi+4 ' ' | add edi 5
        Else_If al = 0F0        ; FDIVR ST(i), ST(0)
            mov D$edi 'fdiv', W$edi+4 'r ' | add edi 6
        Else_If al = 0F8        ; FDIV ST(i), ST(0)
            mov D$edi 'fdiv', B$edi+4 ' ' | add edi 5
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If

        call WriteStiSt0

    ..Else
        DigitMask bl To al

        .If al = 0          ; FADD m64fp
            mov D$edi 'fadd', B$edi+4 ' ' | add edi 5
        .Else_If al = 1     ; FMUL m64fp
            mov D$edi 'fmul', B$edi+4 ' ' | add edi 5
        .Else_If al = 2     ; FCOM m64fp
            mov D$edi 'fcom', B$edi+4 ' ' | add edi 5
        .Else_If al = 3     ; FCOMP m64fp
            mov D$edi 'fcom', W$edi+4 'p ' | add edi 6
        .Else_If al = 4     ; FSUB m64fp
            mov D$edi 'fsub', B$edi+4 ' ' | add edi 5
        .Else_If al = 5      ; FSUBR m64fp
            mov D$edi 'fsub', W$edi+4 'r ' | add edi 6
        .Else_If al = 6     ; FDIV m64fp
            mov D$edi 'fdiv', B$edi+4 ' ' | add edi 5
        .Else_If al = 7     ; FDIVR m64fp
            mov D$edi 'fdiv', W$edi+4 'r ' | add edi 6
        .End_If

        jmp EndWith.R.mem

    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDD:
    If B$EscapePrefix = &TRUE
        ; 66 0F DD /r PADDUSW xmm1, xmm2/m128     ; 0F DD /r PADDUSW mm, mm/m64
        mov D$edi 'padd', D$edi+4 'usw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al

    ..If al = 3
        mov al bl | and eax (not 7)

        If al = 0C0     ; C0+i :  FFREE ST(i)
            mov D$edi 'ffre', W$edi+4 'e ' | add edi 6
        Else_If al = 0D0    ; FST ST(i)
            mov D$edi 'fst ' | add edi 4
        Else_If al = 0D8        ; FSTP ST(i)
            mov D$edi 'fstp', B$edi+4 ' ' | add edi 5
        Else_If al = 0E0        ; FUCOM ST(i)
            mov D$edi 'fuco', W$edi+4 'm ' | add edi 6
        Else_If al = 0E8        ; FUCOMP ST(i)
            mov D$edi 'fuco', D$edi+4 'mp  ' | add edi 7
        Else
            inc D$UnLikelyCode | dec esi | ret
        End_If

        call WriteSti

    ..Else
        DigitMask bl To al

        .If al = 0          ; FLD m64fp
            mov D$edi 'fld ' | add edi 4 | jmp EndWith.R.mem
        .Else_If al = 1     ; FISTTP  DD /1 FISTTP m64int
            mov D$edi 'fst ', D$edi+4 'tp '| add edi 7 | jmp EndWith.R.mem
        .Else_If al = 2     ; FST m64fp
            mov D$edi 'fst ' | add edi 4 | jmp EndWith.R.mem
        .Else_If al = 3     ; FSTP m64fp
            mov D$edi 'fstp', B$edi+4 ' ' | add edi 5 | jmp EndWith.R.mem
        .Else_If al = 4     ; FRSTOR m94/108byte
            mov D$edi 'frst', D$edi+4 'or  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 6     ; FNSAVE* m94/108byte
            mov D$edi 'fnsa', D$edi+4 've  ' | add edi 7 | jmp EndWith.X.mem
        .Else_If al = 7     ; DD /7 FNSTSW* m2byte
            mov D$edi 'fnst', D$edi+4 'sw  ' | add edi 7 | jmp EndWith.W.mem
        .Else
            inc D$UnLikelyCode | dec esi | ret
        .End_If

    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDE:
    If B$EscapePrefix = &TRUE
        ; 0F DE /r PMAXUB mm1, mm2/m64      ; 66 0F DE /r PMAXUB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'pmax', D$edi+4 'ub  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al

    ...If al = 3
        mov al bl

        .If al = 0D9        ; FCOMPP
            mov D$edi 'fcom', W$edi+4 'pp' | add edi 6
        .Else_If al = 0E9   ; FSUBP
            mov D$edi 'fsub', W$edi+4 'p ' | add edi 6
            call WriteStiSt0
        .Else
            and eax (not 7)

            If al = 0C0             ; FADDP ST(0), ST(i)
                mov D$edi 'fadd', W$edi+4 'p ' | add edi 6 | jmp WriteStiSt0
            Else_If al = 0C8        ; FMULP ST(i), ST(0)
                mov D$edi 'fmul', W$edi+4 'p ' | add edi 6
            Else_If al = 0E0        ; FSUBRP ST(i), ST(0)
                mov D$edi 'fsub', D$edi+4 'rp ' | add edi 7
            Else_If al = 0E8        ; FSUBP ST(i), ST(0)
                mov D$edi 'fsub', W$edi+4 'p ' | add edi 6
            Else_If al = 0F0        ; FDIVRP ST(i), ST(0)
                mov D$edi 'fdiv', D$edi+4 'rp ' | add edi 7
            Else_If al = 0F8        ; FDIVP ST(i), ST(0)
                mov D$edi 'fdiv', W$edi+4 'p ' | add edi 6
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

            call WriteStiSt0

        .End_If

    ...Else
        DigitMask bl To al

        .If al = 0             ; FIADD m16int
            mov D$edi 'fiad', W$edi+4 'd ' | add edi 6
        .Else_If al = 1        ; FIMUL m16int
            mov D$edi 'fimu', W$edi+4 'l ' | add edi 6
        .Else_If al = 2        ; FICOM m16int
            mov D$edi 'fico', W$edi+4 'm ' | add edi 6
        .Else_If al = 3        ; FICOMP m16int
            mov D$edi 'fico', D$edi+4 'mp  ' | add edi 7
        .Else_If al = 4        ; FISUB m16int
            mov D$edi 'fisu', W$edi+4 'b ' | add edi 6
        .Else_If al = 5        ; FISUBR m16int
             mov D$edi 'fisu', D$edi+4 'br  ' | add edi 7
        .Else_If al = 6        ; FIDIV m16int
            mov D$edi 'fidi', W$edi+4 'v ' | add edi 6
        .Else_If al = 7        ; FIDIVR m16int
            mov D$edi 'fidi', D$edi+4 'vr  ' | add edi 7
        .End_If

        jmp EndWith.W.mem

    ...End_If

L9: mov B$DisFlag DISDONE+DISLINEOVER
ret


OpDF:
    If B$EscapePrefix = &TRUE
        ; 66 0F DF /r PANDN xmm1, xmm2/m128     ; 0F DF /r PANDN mm, mm/m64
        ;inc D$UnLikelyCode
        mov D$edi 'pand', W$edi+4 'n ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If

    mov bl B$esi | inc esi | ModMask bl to al

    ...If al = 3
        mov al bl

        .If al = 0E0       ; DF E0 FNSTSW* AX
            mov D$edi 'fnst', D$edi+4 'sw a', B$edi+8 'x' | add edi 9
        .Else
            and eax (not 7)

            If al = 0F0        ; FCOMIP ST, ST(i)
                mov D$edi 'fcom', D$edi+4 'ip  ' | add edi 7
            Else_If al = 0C0
                mov D$edi 'ffre', D$edi+4 'ep  ' | add edi 7 | jmp WriteSti
            Else_If al = 0E8       ; FUCOMIP ST, ST(i)
                mov D$edi 'fuco', D$edi+4 'mip ' | add edi 8
            Else
                inc D$UnLikelyCode | dec esi | ret
            End_If

            jmp WriteSt0Sti

        .End_If

    ...Else
        DigitMask bl To al

        ..If al = 0     ; FILD m16int
            mov D$edi 'fild', B$edi+4 ' ' | add edi 5 | jmp EndWith.W.mem
        ..Else_If al = 1        ; FISTTP  DF /1 FISTTP m16int
            mov D$edi 'fist', D$edi+4 'tp ' | add edi 7 | jmp EndWith.W.mem
        ..Else_If al = 2        ; FIST m16int
            mov D$edi 'fist', B$edi+4 ' ' | add edi 5 | jmp EndWith.W.mem
        ..Else_If al = 3        ; FISTP m16int
            mov D$edi 'fist', W$edi+4 'p ' | add edi 6 | jmp EndWith.W.mem
        ..Else_If al = 4           ; /4 > FBLD m80 dec
            mov D$edi 'fbld', B$edi+4 ' ' | add edi 5 | jmp EndWith.T.mem
            ; This is an m80 Binary coded decimal chunk.
        ..Else_If al = 5        ; FILD m64int
            mov D$edi 'fild', B$edi+4 ' ' | add edi 5 | jmp EndWith.Q.mem
        ..Else_If al = 6      ; /6 > FBSTP m80bcd
            mov D$edi 'fbst', W$edi+4 'p ' | add edi 6 | jmp EndWith.T.mem
            ; This is an m80 Binary coded decimal chunk.
        ..Else_If al = 7        ; FISTP m64int
            mov D$edi 'fist', W$edi+4 'p ' | add edi 6 | jmp EndWith.Q.mem
        ..Else
            inc D$UnLikelyCode | dec esi | ret
        ..End_If
    ...End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE0:
    If B$EscapePrefix = &TRUE
        ; 66 0F E0, /r PAVGB xmm1, xmm2/m128     ; 0F E0 /r PAVGB mm1, mm2/m64
        ;inc D$UnLikelyCode
        mov D$edi 'pavg', W$edi+4 'b ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    Else        ; LOOPNE rel8 ; LOOPNZ rel8
        inc D$LikelyCode
        mov D$edi 'loop', D$edi+4 'ne  ' | add edi 7
        mov B$CALLInstruction &TRUE
        test B$esi 080 | jnz L1>
            add D$UnLikelyCode 0FF
L1:     jmp EndWithDisByteRelativeBack

    End_If


OpE1:
    If B$EscapePrefix = &TRUE
      ; 0F E1 /r PSRAW mm, mm/m64       ; 66 0F E1 /r PSRAW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psra', W$edi+4 'w ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    Else       ;  ; LOOPE rel8 ; LOOPZ rel8
        inc D$LikelyCode
        mov D$edi 'loop', W$edi+4 'e ' | add edi 6
        mov B$CALLInstruction &TRUE
        test B$esi 080 | jnz L1>
            add D$UnLikelyCode 0FF
L1:     jmp EndWithDisByteRelativeBack

    End_If


OpE2:
    If B$EscapePrefix = &TRUE
      ; 0F E2 /r PSRAD mm, mm/m64   ; 66 0F E2 /r PSRAD xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psra', W$edi+4 'd ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    Else        ; LOOP rel8
        inc D$LikelyCode
        mov D$edi 'loop', B$edi+4 ' ' | add edi 5
        mov B$CALLInstruction &TRUE
        test B$esi 080 | jnz L1>
            add D$UnLikelyCode 0FF
L1:     jmp EndWithDisByteRelativeBack

    End_If


OpE3:
    .If B$EscapePrefix = &TRUE
        ; 66 0F E3 /r PAVGW xmm1, xmm2/m128 ; 0F E3 /r PAVGW mm1, mm2/m64
        ;inc D$UnLikelyCode
        mov D$edi 'pavg', W$edi+4 'w ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        mov B$CALLInstruction &TRUE
        If B$OperandSizeOverride = &FALSE       ; JECXZ rel8
            inc D$LikelyCode
            SubEdi6 | mov D$edi ' | j', D$edi+4 'ecxz', B$edi+8 ' ' | add edi 9
        Else                                    ; JCXZ rel8
            inc D$UnLikelyCode
            SubEdi6 | mov D$edi ' | j', D$edi+4 'cxz ' | add edi 8
        End_If
        jmp EndWithDisByteRelative

    .End_If


OpE4:
    .If B$EscapePrefix = &TRUE
      ; 0F E4 /r PMULHUW mm1, mm2/m64       ; 66 0F E4 /r PMULHUW xmm1, xmm2/m128
        mov D$edi 'pmul', D$edi+4 'huw ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; IN AL,imm8
        inc D$UnLikelyCode
        mov D$edi 'in a', W$edi+4 'l ' | add edi 6 | call WriteImm8

    .End_If

     mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE5:

    .If B$EscapePrefix = &TRUE
      ; 0F E5 /r PMULHW mm, mm/m64      ; 66 0F E5 /r PMULHW xmm1, xmm2/m128
        mov D$edi 'pmul', D$edi+4 'hw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE   ; IN AX,imm8 / IN EAX,imm8
            mov D$edi 'in e', D$edi+4 'ax  ' | add edi 7
        Else
            mov D$edi 'in a', W$edi+4 'x ' | add edi 6
        End_If
        call WriteImm8

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE6: ; CVTTPD2DQ xmm1, xmm2/m128

    .If B$EscapePrefix = &TRUE
        If B$OperandSizeOverride = &TRUE    ; CVTTPD2DQ xmm1, xmm2/m128
            call MarkSSEdata SSE_2_R
            mov D$edi 'cvtt', D$edi+4 'pd2d', W$edi+8 'q ' | add edi 10
            jmp Dis_mmx1__xmm2_m128
        Else
            ret
        End_If
    .Else       ; OUT imm8, AL
        inc D$UnLikelyCode
        mov D$edi 'out ' | add edi 4
        call WriteImm8 | mov D$edi ' al ' | add edi 3

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE7:
    .If B$EscapePrefix = &TRUE
        mov bl B$esi | inc esi
        If B$OperandSizeOverride = &TRUE        ; 66 0F E7 /r MOVNTDQ m128, xmm
            mov D$edi 'movn', D$edi+4 'tdq ' | add edi 8
            jmp Dis_m128_xmm
        Else        ; 0F E7 /r MOVNTQ m64, mm
            mov D$edi 'movn', D$edi+4 'tq  ' | add edi 7
            jmp Dis_m64_mmx
        End_If

    .Else       ; OUT imm8, AX // OUT imm8, EAX
        inc D$UnLikelyCode
        mov D$edi 'out ' | add edi 4
        call WriteImm8
        If B$OperandSizeOverride = &TRUE
            mov D$edi ' ax ' | add edi 3
        Else
            mov D$edi ' eax' | add edi 4
        End_If

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpE8:
    .If B$EscapePrefix = &TRUE
      ; 0F E8 /r PSUBSB mm, mm/m64  ; 66 0F E8 /r PSUBSB xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psub', D$edi+4 'sb  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; call rel 16/32
        mov D$edi 'call', B$edi+4 ' ' | add edi 5
        mov B$CALLInstruction &TRUE

        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            lodsd
        Else
            inc D$UnLikelyCode
            movsx eax W$esi | add esi 2
        End_If

        mov D$LastCodeRef eax
        call RelativeToAbsolute | call WriteDisRelative

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret

OpE9: ; jmp rel 16/32
    .If B$EscapePrefix = &TRUE
      ; 0F E9 /r PSUBSW mm, mm/m64  ; 66 0F E9 /r PSUBSW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
        mov D$edi 'psub', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; jmp rel 16/32
        On B$edi-2 = ';', sub edi 4
        mov D$edi 'jmp ' | add edi 4
        mov B$CALLInstruction &TRUE, B$DisEndOfChunk &TRUE

        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            lodsd
        Else
            inc D$UnLikelyCode
            movsx eax W$esi | add esi 2
        End_If

        mov D$LastCodeRef eax
        call RelativeToAbsolute | call WriteDisRelative

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpEA:
    ;inc D$UnLikelyCode
    .If B$EscapePrefix = &TRUE
      ; 0F EA /r PMINSW mm1, mm2/m64        ; 66 0F EA /r PMINSW xmm1, xmm2/m128
        mov D$edi 'pmin', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE    ; JMP ptr16:32, JMP ptr16:16 (jmp inter-segment).
            mov D$edi 'jmpF', D$edi+4 ' W$ ' | add edi 7 ; jE! fix
            push D$esi, esi
                Exchange W$esi, W$esi+2
                call WriteImm16 | mov B$edi ' ' | inc edi | call WriteImm16 ; jE! fix
            pop eax, D$eax
        Else                                ;   JMP far ptr32:selector16
            mov D$edi 'jmpF', D$edi+4 ' D$ ' | add edi 7 ; jE! fix
            push D$esi, D$esi+4, esi
                mov eax D$esi, bx W$esi+4, W$esi bx, D$esi+2 eax
                call WriteImm16 | mov B$edi ' ' | inc edi | call WriteImm32 ; jE! fix
            pop eax, D$eax+4, D$eax
        End_If
        mov B$DisEndOfChunk &TRUE
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpEB:
    If B$EscapePrefix = &TRUE
      ; 0F EB /r POR mm, mm/m64     ; 66 0F EB /r POR xmm1, xmm2/m128
        mov D$edi 'por ' | add edi 4
        jmp Dis_xmmx1__xmmx2_m64_128

    Else        ; jmp rel 8
        inc D$LikelyCode
        On B$edi-2 <> ';', sub edi 4
        mov D$edi 'jmp ' | add edi 4
        mov B$CALLInstruction &TRUE, B$DisEndOfChunk &TRUE | jmp EndWithDisByteRelative

    End_If


OpEC:
    If B$EscapePrefix = &TRUE
        ; 66 0F EC /r PADDSB xmm1,xmm2/m128       ; 0F EC /r PADDSB mm, mm/m64
        mov D$edi 'padd', D$edi+4 'sb  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    Else           ; IN AL,DX
        inc D$UnLikelyCode
        mov D$edi 'in a', D$edi+4 'l dx' | add edi 8

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpED:
    .If B$EscapePrefix = &TRUE
        ; 66 0F ED /r PADDSW xmm1, xmm2/m128    ; 0F ED /r PADDSW mm, mm/m64
        mov D$edi 'padd', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else        ; IN AX,DX / IN EAX,DX
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'in e', D$edi+4 'ax d', B$edi+8 'x' | add edi 9
        Else
            mov D$edi 'in a', D$edi+4 'x dx' | add edi 8
        End_If

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret

OpEE:

    If B$EscapePrefix = &TRUE
        ; 0F EE /r PMAXSW mm1, mm2/m64      ; 66 0F EE /r PMAXSW xmm1, xmm2/m128
        mov D$edi 'pmax', D$edi+4 'sw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    Else          ; OUT DX, AL
        inc D$UnLikelyCode
        mov D$edi 'out ', D$edi+4 'dx a', B$edi+8 'l' | add edi 9

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpEF:
    .If B$EscapePrefix = &TRUE
      ; 0F EF /r PXOR mm, mm/m64    ; 66 0F EF /r PXOR xmm1, xmm2/m128
        mov D$edi 'pxor', B$edi+4 ' ' | add edi 5
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$UnLikelyCode
      ; OUT DX, AX // OUT DX, EAX
        mov D$edi 'out ', W$edi+4 'dx' | add edi 6
        If B$OperandSizeOverride = &FALSE
            mov D$edi ' eax' | add edi 4
        Else
            mov D$edi ' ax ' | add edi 3
        End_If

    .End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret

[LockPrefix: ?]

; ADD, ADC, AND, BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC, INC, NEG, NOT, OR,
; SBB, SUB, XOR, XADD, XCHG

OpF0:
    mov B$LockPrefix &TRUE
    inc D$Prefixes
    mov D$edi 'lock', B$edi+4 ' ', B$DisFlag DISDONE | add edi 5
    mov B$DisFlag DISDONE ;+DISLINEOVER
ret


OpF1:
    ;inc D$UnLikelyCode
    If B$EscapePrefix = &TRUE
      ; 0F F1 /r PSLLW mm, mm/m64  ; 66 0F F1 /r PSLLW xmm1, xmm2/m128
        mov D$edi 'psll', W$edi+4 'w ' | add edi 6
        jmp Dis_xmmx1__xmmx2_m64_128
    End_If
ret


OpF2:
    ..If B$esi = 0F
        .If B$esi+1 = 010       ; F2 0F 10 /r MOVSD xmm1, xmm2/m64 ; op10
            add esi 2 | mov D$edi 'movs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 011       ; F2 0F 11 /r MOVSD xmm2/m64, xmm
            add esi 2 | mov D$edi 'movs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm2_m64__xmm1
        .Else_If B$esi+1 = 012       ; F2,0F,12,/r MOVDDUP xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_1_Q
            mov D$edi 'movd', D$edi+4 'dup ' | add edi 8
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 02A       ; CVTSI2SD xmm, r/m32
            add esi 2 | call MarkSSEdata SSE_1_D
            mov D$edi 'cvts', D$edi+4 'i2sd', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__rm32
        .Else_If B$esi+1 = 02C       ; CVTTSD2SI r32, xmm/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'cvtt', D$edi+4 'sd2s', W$edi+8 'i ' | add edi 10
            jmp Dis_r32__xmm_m64
        .Else_If B$esi+1 = 02D       ; CVTSD2SI r32, xmm/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'cvts', D$edi+4 'd2si', B$edi+8 ' ' | add edi 9
            jmp Dis_r32__xmm_m64
        .Else_If B$esi+1 = 051      ; F2 0F 51 /r SQRTSD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_2_R
            mov D$edi 'sqrt', D$edi+4 'sd  ' | add edi 7
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 058      ; ADDSD
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'adds', W$edi+4 'd ' | add edi 6 | call Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 059      ; F2 0F 59 /r MULSD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'muls', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05A       ; CVTSD2SS xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'cvts', D$edi+4 'd2ss', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05C   ; F2 0F 5C /r SUBSD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_2_R
            mov D$edi 'subs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05D       ; MINSD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'mins', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05E         ; DIVSD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'divs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 05F      ; F2 0F 5F /r MAXSD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'maxs', W$edi+4 'd ' | add edi 6
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 070      ; F2 0F 70 /r ib PSHUFLW xmm1, xmm2/m128, imm8
            add esi 2 | mov D$edi 'pshu', D$edi+4 'flw ' | add edi 8
            call Dis_xmm1__xmm2_m128 | mov B$edi ' ' | inc edi
            call WriteImm8
        .Else_If B$esi+1 = 07C      ; F2,0F,7C,/r HADDPS xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'hadd', D$edi+4 'ps ' | add edi 7
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 07D      ; F2,0F,7D,/r HSUBPS xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'hsub', D$edi+4 'ps ' | add edi 7
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 0C2       ; CMPSD xmm1, xmm2/m64, imm8
            add esi 2 | mov D$edi 'cmps', W$edi+4 'd ' | add edi 6
            call Dis_xmm1__xmm2_m64 | jmp WritePacketCondition
        .Else_If B$esi+1 = 0D0          ; F2,0F,D0,/r ADDSUBPS xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'adds', D$edi+4 'ubps', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 0D6          ; F2 0F D6 MOVDQ2Q mm, xmm
            add esi 2 | mov D$edi 'movd', D$edi+4 'q2q ' | add edi 8
            jmp Dis_mmx_xmm
        .Else_If B$esi+1 = 0E6       ; CVTPD2DQ xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_2_R
            mov D$edi 'cvtp', D$edi+4 'd2dq', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 0F0       ; F2,0F,F0,/r LDDQU xmm, mem
            add esi 2 | mov D$edi 'lddq', D$edi+4 'u ' | add edi 6
            jmp Dis_xmm1__xmm2_m128  ; Xmm2 is dummy, here.
        .Else
            inc D$UnLikelyCode | ret
        .End_If

    ..Else_If B$esi = 0A6   ; F2 A6 REPNE CMPS m8, m8
        inc esi | mov D$edi 'repn', D$edi+4 'e cm', D$edi+8 'psb ' | add edi 11
    ..Else_If W$esi = 0A766   ; F2 A7 REPNE CMPS m16, m16 ; F2 A7 REPNE CMPS m32, m32
        add esi 2 | mov D$edi 'repn', D$edi+4 'e cm', D$edi+8 'psw ' | add edi 11
    ..Else_If B$esi = 0A7   ; F2 A7 REPNE CMPS m16, m16 ; F2 A7 REPNE CMPS m32, m32
        inc esi | mov D$edi 'repn', D$edi+4 'e cm', D$edi+8 'psd ' | add edi 11
    ..Else_If B$esi = 0AE   ; F2 AE REPNE SCAS m8
        inc esi | mov D$edi 'repn', D$edi+4 'e sc', D$edi+8 'asb ' | add edi 11
    ..Else_If W$esi = 0AF66   ; F2 AF REPNE SCAS m16 ; F2 AF REPNE SCAS m32
        add esi 2 | mov D$edi 'repn', D$edi+4 'e sc', D$edi+8 'asw ' | add edi 11
    ..Else_If B$esi = 0AF   ; F2 AF REPNE SCAS m16 ; F2 AF REPNE SCAS m32
        inc esi | mov D$edi 'repn', D$edi+4 'e sc', D$edi+8 'asd ' | add edi 11
    ..Else
        .If B$EscapePrefix = &TRUE
          ; 0F F2 /r PSLLD mm, mm/m64   ;  66 0F F2 /r PSLLD xmm1, xmm2/m128
            mov D$edi 'psll', W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128
        .Else
            inc D$UnLikelyCode | ret
        .End_If
    ..End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF3:
    ..If B$esi = 0F
        .If B$esi+1 = 010       ; F3 0F 10 /r MOVSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'movs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32F
        .Else_If B$esi+1 = 011       ; F3 0F 11 /r MOVSS xmm2/m32, xmm
            add esi 2 | call MarkSSEdata SSE_1_R
            mov D$edi 'movs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm2_m32F__xmm1
        .Else_If B$esi+1 = 012       ; F3,0F,12,/r MOVSLDUP xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'movs', D$edi+4 'ldup', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 016       ; F3,0F,16,/r MOVSHDUP xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'movs', D$edi+4 'hdup', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 051      ; F3 0F 51 /r SQRTSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'sqrt', D$edi+4 'ss  ' | add edi 7 | jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 052      ; F3 0F 52 /r RSQRTSS xmm1,xmm2/m32
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'rsqr', D$edi+4 'tss ' | add edi 8 | jmp Dis_xmm1__xmm2_m32
        .Else_If al = 053; F3 0F 53 /r RCPSS xmm1, xmm2/m32; F3 0F 53 /r RCPSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'rcpp', W$edi+4 's ' | add edi 6 | jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 058 ; F3 0F 58 /r ADDSS
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'adds', W$edi+4 's ' | add edi 6 | jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 059       ; F3 0F 59 /r MULSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'muls', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 05C   ; F3 0F 5C /r SUBSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'subs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 05D       ; MINSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'mins', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 05F      ; F3 0F 5F /r MAXSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'maxs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 070       ; F3 0F 70 /r ib PSHUFHW xmm1, xmm2/m128, imm8
            add esi 2 | mov D$edi 'pshu', D$edi+4 'fhw ' | add edi 8
            call Dis_xmm1__xmm2_m128 | mov B$edi ' ' | inc edi
            call WriteImm8
        .Else_If B$esi+1 = 07E      ;  F3 0F 7E MOVQ xmm1, xmm2/m64
            add esi 2 | mov D$edi 'movq', B$edi+4 ' ' | add edi 5
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 0C2      ; CMPSD xmm1, xmm2/m64, imm8
            add esi 2 | mov D$edi 'cmps', W$edi+4 's ' | add edi 6
            call  Dis_xmm1__xmm2_m64 | jmp WritePacketCondition
        .Else_If B$esi+1 = 0D6        ; F3 0F D6 MOVQ2DQ xmm, mm
            add esi 2 | mov D$edi 'movq', D$edi+4 '2dq ' | add edi 8
            jmp Dis_xmm_mmx
        .Else_If B$esi+1 = 0E6      ; CVTDQ2PD xmm1, xmm2/m64
            add esi 2 | call MarkSSEdata SSE_2_D
            mov D$edi 'cvtd', D$edi+4 'q2pd', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m64
        .Else_If B$esi+1 = 02A      ; CVTSI2SS xmm, r/m32
            add esi 2 | call MarkSSEdata SSE_1_D
            mov D$edi 'cvts', D$edi+4 'i2ss', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__rm32
        .Else_If B$esi+1 = 02C      ; CVTTSS2SI r32, xmm/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'cvtt', D$edi+4 'ss2s', W$edi+8 'i ' | add edi 10
            jmp Dis_r32__xmm_m32
        .Else_If B$esi+1 = 05A      ; CVTSS2SD xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'cvts', D$edi+4 's2sd', B$edi+8 ' ' | add edi 9
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 02D      ; CVTSS2SI r32, xmm/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'cvts', D$edi+4 's2si', B$edi+8 ' ' | add edi 9
            jmp Dis_r32__xmm_m32
        .Else_If B$esi+1 = 05B      ; CVTTPS2DQ xmm1, xmm2/m128
            add esi 2 | call MarkSSEdata SSE_4_F
            mov D$edi 'cvtt', D$edi+4 'ps2d', W$edi+8 'q ' | add edi 10
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 05E      ; DIVSS xmm1, xmm2/m32
            add esi 2 | call MarkSSEdata SSE_1_F
            mov D$edi 'divs', W$edi+4 's ' | add edi 6
            jmp Dis_xmm1__xmm2_m32
        .Else_If B$esi+1 = 06F      ; F3 0F 6F /r MOVDQU xmm1, xmm2/m128
            add esi 2 | mov D$edi 'movd', D$edi+4 'qu  ' | add edi 7
            jmp Dis_xmm1__xmm2_m128
        .Else_If B$esi+1 = 07F      ; F3 0F 7F /r MOVDQU xmm2/m128, xmm1
            add esi 2 | mov D$edi 'movd', D$edi+4 'qu  ' | add edi 7
            jmp Dis_xmm2_m128__xmm1
        .Else
            ret
        .End_If

    ..Else_If B$esi = 090   ; F3 90 PAUSE
        inc esi | mov D$edi 'paus', B$edi+4 'e' | add edi 5
    ..Else_If W$esi = 0A766 ; F3 66 17
        add esi 2 | mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'w' | add edi 9
      ;  add esi 2 | mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'w' | add edi 9
    ..Else_If W$esi = 0AF66 ; F3 66 17
        add esi 2 | mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'w' | add edi 9
    ..Else_If W$esi = 0A566 ; F3 66 17
        add esi 2 | mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'w' | add edi 9
      ;!!!!  add esi 2 | mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 06C       ; F3 6C REP INS r/m8, DX
        inc D$UnLikelyCode
        inc esi | mov D$edi 'rep ', D$edi+4 'insb' | add edi 8 ; | jmp Dis_rm8_dx
    ..Else_If B$esi = 06D       ; F3 6D REP INS r/m16, DX ; F3 6D REP INS r/m32, DX
        inc D$UnLikelyCode
        inc esi | mov D$edi 'rep ', D$edi+4 'insd' | add edi 8 ; | jmp Dis_rm32_rm16__dx
    ..Else_If W$esi = 06D66       ; F3 6D REP INS r/m16, DX ; F3 6D REP INS r/m32, DX
        inc D$UnLikelyCode
        add esi 2 | mov D$edi 'rep ', D$edi+4 'insw' | add edi 8 ; | jmp Dis_rm32_rm16__dx
    ..Else_If B$esi = 06E       ; F3 6E REP OUTS DX, r/m8
        inc D$UnLikelyCode
        inc esi | mov D$edi 'rep ', D$edi+4 'outs', B$edi+8 'b' | add edi 9 ;| jmp Dis_dx_rm8
    ..Else_If W$esi = 06F66       ; F3 6F REP OUTS DX, r/m16 ; F3 6F REP OUTS DX, r/m32
        inc D$UnLikelyCode
        add esi 2 | mov D$edi 'rep ', D$edi+4 'outs', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 06F       ; F3 6F REP OUTS DX, r/m16 ; F3 6F REP OUTS DX, r/m32
        inc D$UnLikelyCode
        inc esi | mov D$edi 'rep ', D$edi+4 'outs', B$edi+8 'd' | add edi 9 ;| jmp Dis_dx__rm32_rm16
    ..Else_If B$esi = 0A4       ; F3 A4 REP MOVS m8, m8
        inc esi | mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0A566       ; F3 A5 REP MOVS m16, m16 ; F3 A5 REP MOVS m32, m32
        add esi 2 | mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0A5       ; F3 A5 REP MOVS m16, m16 ; F3 A5 REP MOVS m32, m32
        inc esi | mov D$edi 'rep ', D$edi+4 'movs', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0A6       ; F3 A6 REPE CMPS m8, m8
        inc esi | mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0A766       ; F3 A7 REPE CMPS m16, m16 ; F3 A7 REPE CMPS m32, m32
        add esi 2 | mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0A7       ; F3 A7 REPE CMPS m16, m16 ; F3 A7 REPE CMPS m32, m32
        inc esi | mov D$edi 'rep ', D$edi+4 'cmps', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0AA       ; F3 AA REP STOS m8
        inc esi | mov D$edi 'rep ', D$edi+4 'stos', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0AB66       ; F3 AB REP STOS m16 ; F3 AB REP STOS m32
        add esi 2 | mov D$edi 'rep ', D$edi+4 'stos', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0AB       ; F3 AB REP STOS m16 ; F3 AB REP STOS m32
        inc esi | mov D$edi 'rep ', D$edi+4 'stos', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0AC       ; F3 AC REP LODS AL
        inc esi | mov D$edi 'rep ', D$edi+4 'lods', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0AD66       ; F3 AD REP LODS AX ; F3 AD REP LODS EAX
        add esi 2 | mov D$edi 'rep ', D$edi+4 'lods', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0AD       ; F3 AD REP LODS AX ; F3 AD REP LODS EAX
        inc esi | mov D$edi 'rep ', D$edi+4 'lods', B$edi+8 'd' | add edi 9
    ..Else_If B$esi = 0AE       ; F3 AE REPE SCAS m8
        inc esi | mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'b' | add edi 9
    ..Else_If W$esi = 0AF66       ; F3 AF REPE SCAS m16 ; F3 AF REPE SCAS m32
        add esi 2 | mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'w' | add edi 9
    ..Else_If B$esi = 0AF       ; F3 AF REPE SCAS m16 ; F3 AF REPE SCAS m32
        inc esi | mov D$edi 'rep ', D$edi+4 'scas', B$edi+8 'd' | add edi 9
    ..Else
        If B$EscapePrefix = &TRUE
            ; 0F F3 /r PSLLQ mm, mm/m64     ; 66 0F F3 /r PSLLQ xmm1, xmm2/m128
            mov D$edi 'psll', W$edi+4 'q ' | add edi 6
            jmp Dis_xmmx1__xmmx2_m64_128
        Else
            ret
        End_If
    ..End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF4: ; HLT
    If B$EscapePrefix = &TRUE
      ; 0F F4 /r PMULUDQ mm1, mm2/m64       ; 66 OF F4 /r PMULUDQ xmm1, xmm2/m128
        mov D$edi 'pmul', D$edi+4 'udq ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    Else
        inc D$UnLikelyCode
        mov D$edi 'hlt ' | add edi 3

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF5: ; cmc
    If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
      ; 0F F5 /r PMADDWD mm, mm/m64       ; 66 0F F5 /r PMADDWD xmm1, xmm2/m128
        mov D$edi 'pmad', D$edi+4 'dwd ' | add edi 8
        jmp Dis_xmmx1__xmmx2_m64_128

    Else
        mov D$edi 'cmc ' | add edi 3  ; cmc

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF6:
    .If B$EscapePrefix = &TRUE
      ; 0F F6 /r PSADBW mm1, mm2/m64        ; 66 0F F6 /r PSADBW xmm1, xmm2/m128
        ;inc D$UnLikelyCode
         mov D$edi 'psad', D$edi+4 'bw  ' | add edi 7
        jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        inc D$LikelyCode
        mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /6 ?

        If al = 0   ; F6 /0 ib TEST r/m8,imm8
            mov D$edi 'test', B$edi+4 ' ' | add edi 5 | jmp Dis_rm8_imm8
        Else_If al = 2       ; F6 /2 NOT r/m8
            mov B$LockPrefix &FALSE
            mov D$edi 'not ' | add edi 4
        Else_If al = 3       ; F6 /3 NEG r/m8
            mov B$LockPrefix &FALSE
            mov D$edi 'neg ' | add edi 4
        Else_If al = 4           ; F6 /4 MUL r/m8
            mov D$edi 'mul ' | add edi 4
        Else_If al = 5           ; IMUL r/m8
            mov D$edi 'imul', B$edi+4 ' ' | add edi 5
        Else_If al = 6      ; DIV r/m8
            mov D$edi 'div ' | add edi 4
        Else_If al = 7      ; IDIV r/m8
            mov D$edi 'idiv', B$edi+4 ' ' | add edi 5
        Else
            inc D$UnLikelyCode | ret
        End_If

        jmp EndWith.B.mem

    .End_If
ret


OpF7:
    ..If B$EscapePrefix = &TRUE
        ;inc D$UnLikelyCode
        mov D$edi 'mask' | mov bl B$esi | inc esi
        If B$OperandSizeOverride = &TRUE    ; 66 0F F7 /r MASKMOVDQU xmm1, xmm2
            mov D$edi+4 'movd', D$edi+8 'qu  ' | add edi 11
            jmp Dis_xmm1_xmm2
        Else                                ; 0F F7 /r MASKMOVQ mm1, mm2
            mov D$edi+4 'movq', B$edi+8 ' ' | add edi 9
            jmp Dis_mmx1_mmx2
        End_If


    ..Else
        inc D$LikelyCode
        mov bl B$esi | inc esi | DigitMask bl To al              ; ModRm with /6 ?

        .If al = 0  ; F7 /0 iw TEST r/m16,imm16 ; F7 /0 id TEST r/m32,imm32
            mov D$edi 'test', B$edi+4 ' ' | add edi 5 | jmp Dis_rm32_rm16__imm32_imm16
        .Else_If al = 2      ; F7 /2 NOT r/m16
            mov B$LockPrefix &FALSE
            mov D$edi 'not ' | add edi 4
        .Else_If al = 3       ; F7 /3 NEG r/m16
            mov B$LockPrefix &FALSE
            mov D$edi 'neg ' | add edi 4
        .Else_If al = 4           ; F7 /4 MUL r/m16
            mov D$edi 'mul ' | add edi 4
        .Else_If al = 5           ; IMUL r/m16
            mov D$edi 'imul', B$edi+4 ' ' | add edi 5
        .Else_If al = 6      ; DIV r/m32 r/m16
            mov D$edi 'div ' | add edi 4
        .Else_If al = 7      ; IDIV r/m32 // r/m16
            mov D$edi 'idiv', B$edi+4 ' ' | add edi 5
        .Else
            inc D$UnLikelyCode | ret
        .End_If

        On B$OperandSizeOverride = &TRUE, jmp EndWith.W.mem
        jmp EndWith.D.mem

    ..End_If
ret



OpF8:
    If B$EscapePrefix = &TRUE
      ; 0F F8 /r PSUBB mm, mm/m64   ; 66 0F F8 /r PSUBB xmm1, xmm2/m128
        mov D$edi 'psub', W$edi+4 'b ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else; clc
        mov D$edi 'clc ' | add edi 3

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpF9:
    If B$EscapePrefix = &TRUE
      ; 0F F9 /r PSUBW mm, mm/m64   ; 66 0F F9 /r PSUBW xmm1, xmm2/m128
        mov D$edi 'psub', W$edi+4 'w ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else       ; F9 STC
        mov D$edi 'stc ' | add edi 3

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFA:
    If B$EscapePrefix = &TRUE
      ; 0F FA /r PSUBD mm, mm/m64   ; 66 0F FA /r PSUBD xmm1, xmm2/m128
        mov D$edi 'psub', W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else
        inc D$UnLikelyCode
        mov D$edi 'cli ' | add edi 3
    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFB:
    If B$EscapePrefix = &TRUE
      ; 0F FB /r PSUBQ mm1, mm2/m64 ; 66 0F FB /r PSUBQ xmm1, xmm2/m128
        mov D$edi 'psub', W$edi+4 'q ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else       ; FB STI
        inc D$UnLikelyCode
        mov D$edi 'sti ' | add edi 3

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFC:
    If B$EscapePrefix = &TRUE
        ; 66 0F FC /r PADDB xmm1,xmm2/m128    ; 0F FC /r PADDB mm, mm/m64
        mov D$edi 'padd', W$edi+4 'b ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else               ; cld
        mov D$edi 'cld ' | add edi 3

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFD:
    If B$EscapePrefix = &TRUE
        ; 66 0F FD /r PADDW xmm1, xmm2/m128     ;  0F FD /r PADDW mm, mm/m64
        mov D$edi 'padd', W$edi+4 'w ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    Else       ; FD STD
        mov D$edi 'std ' | add edi 3

    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


OpFE:
    .If B$EscapePrefix = &TRUE
        ; 66 0F FE /r PADDD xmm1, xmm2/m128    ; 0F FE /r PADDD mm, mm/m64
        ;inc D$UnLikelyCode
        mov D$edi 'padd', W$edi+4 'd ' | add edi 6 | jmp Dis_xmmx1__xmmx2_m64_128

    .Else
        movzx ebx B$esi | inc esi | DigitMask bl to al
        If al = 0           ; INC r/m8
            mov B$LockPrefix &FALSE
            mov D$edi 'inc '
        Else_If al = 1      ; DEC r/m8
            mov B$LockPrefix &FALSE
            mov D$edi 'dec '
        Else
            inc D$UnLikelyCode | ret
        End_If

        add edi 4 | jmp EndWith.B.mem
    .End_If
ret

;075 00_01_110_101

OpFF:
    movzx ebx B$esi | inc esi | DigitMask bl to al

    .If al = 0           ; INC r/m16 / r/m32
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'inc ' | add edi 4 | jmp EndWith.WD.mem

    .Else_If al = 1      ; DEC r/m32 // DEC r/m16
        inc D$LikelyCode
        mov B$LockPrefix &FALSE
        mov D$edi 'dec ' | add edi 4 | jmp EndWith.WD.mem

    .Else_If al = 2     ;  ; FF /2 CALL r/m16 ; FF /2 CALL r/m32
        inc D$LikelyCode
        mov D$edi 'call', B$edi+4 ' ' | add edi 5
      ; add here the DLL Functions calls.
      ; If BL = 15 and D$esi inside .import Address Table
      ;             or D$esi points to a JMP Pointer To .import Address Table
        jmp L5> ;;;call Dis_rm32_rm16

    .Else_If al = 3
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &TRUE
            mov D$edi 'call', D$edi+4 'F W$' | add edi 6 ; jE! fix
        Else
            mov D$edi 'call', D$edi+4 'F D$' | add edi 6 ; jE! fix
        End_If
        call WriteEffectiveAddressFromModRm         ; Mem pointing to 16:16/32
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .Else_If al = 4      ; JMP r/m16 ; ; JMP r/m32
        inc D$LikelyCode
        mov B$DisEndOfChunk &TRUE
        mov D$edi 'jmp ' | add edi 4

L5:     ;push D$LastCodeRef
            call Dis_rm32_rm16
;       ;pop eax
;;
        cmp eax D$LastCodeRef | je L9>>
      ; This 'D$LastCodeRef' may be, for example, 'mov eax D$Codexxxx' or 'D$Dataxxxx'.
            mov ebx D$LastCodeRef
            cmp ebx D$DisCodeMin | jb L9>>
            cmp ebx D$DisCodeMax | ja L9>>
                call ExtendToSideCodePointers

L9:         cmp ebx D$DisDataMin | jb L9>>
            cmp ebx D$DisDataMax | ja L9>>
                call ExtendToSideDataPointers
;;

  ; OpEA: ; JMP ptr16:32, JMP ptr16:16 (jmp inter-segment).
    .Else_If al = 5      ; JMP m16:32
        inc D$UnLikelyCode
        If B$OperandSizeOverride = &FALSE
            mov D$edi 'jmpF', D$edi+4 ' W$ ' | add edi 5 ; jE! fix
        Else
            mov D$edi 'jmpF', D$edi+4 ' D$ ' | add edi 5 ; jE! fix
        End_If
        mov B$DisEndOfChunk &TRUE
        call WriteEffectiveAddressFromModRm
        mov D$edi ' ; !', D$edi+4 '!!!!' | add edi 8

    .Else_If al = 6     ; FF /6 PUSH r/m16  ; FF /6 PUSH r/m32
        inc D$LikelyCode
        mov D$edi 'push', B$edi+4 ' ' | add edi 5 | jmp Dis_m32_16

    .Else
        inc D$UnLikelyCode
        ret

    .End_If

L9: mov B$DisFlag DISDONE+DISLINEOVER
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Routines usable by several encodings:
____________________________________________________________________________________________
____________________________________________________________________________________________

Dis_rm8_r8:
    mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteByteRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_al_imm8:
    mov D$edi 'al  ' | add edi 3 | call WriteImm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_eax_ax__imm32_imm16:
    If B$OperandSizeOverride = &TRUE
        mov D$edi 'ax  ' | add edi 3 | call WriteImm16
    Else
        mov D$edi 'eax ' | add edi 4 | call WriteImm32
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_eax_ax__rd_rw:
  ; See: ; 90+rw XCHG AX, r16 ; 90+rd XCHG EAX, r32
  ;
  ; the Reg is given in the last Opcode Byte, in the BaseMask

    movzx eax B$esi-1 | and eax 00_000_111

    If B$OperandSizeOverride = &TRUE
        mov D$edi 'ax  ' | add edi 3
        mov eax D$WordRegsTable+eax*4 | stosd | dec edi
    Else
        mov D$edi 'eax ' | add edi 4
        mov eax D$dWordRegsTable+eax*4 | stosd
    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8_m8:
    mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteByteRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__r32_r16:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WritedWordRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_rm32_rm16__r32_r16__imm8:
    call Dis_rm32_rm16__r32_r16 | mov B$edi ' ' | inc edi
    call WriteImm8      ; not signed this is for SHLD / SHRD
ret

Dis_rm32_rm16__r32_r16__cl:
    call Dis_rm32_rm16__r32_r16 | mov D$edi ' cl ' | add edi 3
ret

Dis_m32_r32:
    mov W$DisSizeMarker 'D$'
    movzx ebx B$esi | inc esi
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WritedWordRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r8_rm8:
    mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    call WriteByteRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWithModRm ;EndWith.D.mem
ret

Dis_r32_r16__rm32_rm16:
    On  B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    call WritedWordRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.D.mem
ret

Dis_r32_r16__rm32_rm16_orNone__SignedImm8:
    On  B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    call WritedWordRegsFromRegBits | mov B$edi ' ' | inc edi
    Test bl 00_11_000_000 | jz L2>
        RegMask bl to al | RmMask bl to cl | ;on al = cl jmp L3>
            call WriteEffectiveAddressFromModRm
            mov B$edi ' ' | inc edi | jmp L3>
L2: call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
L3: call WriteSignedImm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_r16__rm32_rm16_OrNone__SignedImm16_32:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    call WritedWordRegsFromRegBits | mov B$edi ' ' | inc edi
    Test bl 00_11_000_000 | jz L2>
        RegMask bl to al | RmMask bl to cl | ;on al = cl jmp L3>
            call WriteEffectiveAddressFromModRm
            mov B$edi ' ' | inc edi | jmp L3>
L2: call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
L3: If B$OperandSizeOverride = &TRUE
        call WriteSignedImm16
    Else
        call WriteSignedImm32
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_rm8_1:
    mov W$DisSizeMarker 'B$' | jmp Dis_rm32_1
Dis_rm16_1:
    mov W$DisSizeMarker 'W$'
Dis_rm32_1:
  ;  movzx ebx B$esi | inc esi
    call WriteEffectiveAddressFromModRm | mov W$edi ' 1' | add edi 2
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8:
    mov W$DisSizeMarker 'B$'
    movzx ebx B$esi | inc esi
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8_dx:
    mov W$DisSizeMarker 'B$' | jmp L1>
Dis_rm32_rm16__dx:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
L1: movzx ebx B$esi | inc esi
    call WriteEffectiveAddressFromModRm | mov D$edi ' dx ' | add edi 3
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_dx_rm8:
    mov W$DisSizeMarker 'B$' | jmp L1>
Dis_dx__rm32_rm16:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
L1: mov D$edi ' dx ' | add edi 3
    movzx ebx B$esi | inc esi
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__1:
    If B$OperandSizeOverride = &FALSE
        jmp Dis_rm32_1
    Else
        jmp Dis_rm16_1
    End_If

Dis_rm8_cl:
    mov W$DisSizeMarker 'B$' | jmp Dis_rm32_cl
Dis_rm16_cl:
    mov W$DisSizeMarker 'W$'
Dis_rm32_cl:
   ; movzx ebx B$esi | inc esi
    call WriteEffectiveAddressFromModRm | mov D$edi ' cl ' | add edi 3
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__cl:
    If B$OperandSizeOverride = &FALSE
        jmp Dis_rm32_cl
    Else
        jmp Dis_rm16_cl
    End_If

;Dis_rm16_imm8:
;    mov W$DisSizeMarker 'W$'
;    movzx ebx B$esi | inc esi
;    call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
;    call WriteImm8
;    mov B$DisFlag DISDONE+DISLINEOVER
;ret

Dis_rm16_Sreg:
    mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteSregsFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_Sreg:
    mov W$DisSizeMarker 'D$'
    movzx ebx B$esi | inc esi
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteSregsFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_Sreg_rm16:
    mov W$DisSizeMarker 'W$'
    movzx ebx B$esi | inc esi
    call WriteSregsFromModRm | mov B$edi ' ' | inc edi
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_Sreg_rm32:
    mov W$DisSizeMarker 'D$'
    movzx ebx B$esi | inc esi
    call WriteSregsFromModRm | mov B$edi ' ' | inc edi
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm8_imm8:
    mov W$DisSizeMarker 'B$'
    call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    call WriteImm8
    mov B$DisFlag DISDONE+DISLINEOVER

ret

Dis_Imm8:
    call WriteSignedImm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_imm32_16:
    If B$OperandSizeOverride = &FALSE
        call WriteImm32
    Else
        call WriteImm16
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__imm32_imm16:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    If B$OperandSizeOverride = &FALSE
        call WriteImm32
    Else
        call WriteImm16
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_rm16__imm8:
    On  B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    call WriteSignedImm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_r16__rm8:
    On B$OperandSizeOverride = &TRUE, jmp Dis_r16_rm8
Dis_r32_rm8:
    mov bl B$esi | inc esi
    call WriteEregsFromRegBits | mov B$edi ' ' | inc edi
    mov W$DisSizeMarker 'B$' | call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r16_rm8:
    mov bl B$esi | inc esi
    call WriteWordregsFromRegBits | mov B$edi ' ' | inc edi
    mov W$DisSizeMarker 'B$' | call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_rm16:
    mov bl B$esi | inc esi
    call WriteEregsFromRegBits | mov B$edi ' ' | inc edi
    mov W$DisSizeMarker 'W$' | call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_xmmx:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx
Dis_r32_xmm:
    mov bl B$esi | inc esi
    call WriteEregsFromRegBits | mov B$edi ' ' | inc edi
    call WriteXmmRegsFromRmBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_mmx:
    mov bl B$esi | inc esi
    call WriteEregsFromRegBits | mov B$edi ' ' | inc edi
    call WriteMMXRegsFromRmBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

; Some encodings like MOVMSKPD are in the form: 11 r32 mm
; Some like PMOVMSKB are: 11 mm r32.
; This 'Rev'ersed  form is for this second case:

; Was wronbg Doce of Intel-2001. Correct in Intel-2004
;;
Dis_r32_xmmx_Rev:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx_Rev
Dis_r32_xmmRev:
    mov bl B$esi | inc esi
    call WriteEregsFromRmBits | mov B$edi ' ' | inc edi
    call WriteXmmRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_r32_mmx_Rev:
    mov bl B$esi | inc esi
    call WriteEregsFromRmBits | mov B$edi ' ' | inc edi
    call WriteMMXRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret
;;

Dis_r32_xmmx_imm8:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx_imm8
Dis_r32_xmm_imm8:
    mov bl B$esi | inc esi
    call WriteEregsFromRmBits | mov B$edi ' ' | inc edi
    call WriteXmmRegsFromRegBits | jmp L9>

Dis_r32_mmx_imm8:
    mov bl B$esi | inc esi
    call WriteEregsFromRmBits | mov B$edi ' ' | inc edi
    call WriteMMXRegsFromRegBits
L9: mov B$edi ' ' | inc edi | call Writeimm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmmx_r32_imm8:
    On B$OperandSizeOverride = &FALSE, jmp Dis_r32_mmx_imm8
Dis_xmm_r32_imm8:
    mov bl B$esi | inc esi
    call WriteXmmRegsFromRmBits | mov B$edi ' ' | inc edi
    call WriteEregsFromRegBits | jmp L9>

Dis_mmx_r32_imm8:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRmBits | mov B$edi ' ' | inc edi
    call WriteEregsFromRegBits
L9: mov B$edi ' ' | inc edi | call Writeimm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_PINSRW:
    mov W$DisSizeMarker 'D$'
    mov bl B$esi | inc esi

    On B$OperandSizeOverride = &FALSE, jmp L2>

    call WriteXmmRegsFromRegBits | mov B$edi ' ' | inc edi
    push edi ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        call WriteEffectiveAddressFromModRm | jmp L9>

L2:     call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi
    push edi ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        call WriteEffectiveAddressFromModRm

L9:     mov B$edi ' ' | inc edi | call Writeimm8
    pop eax ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    On B$eax = 'D', mov B$eax 'W' ; <<<<<<<<<< Old EDI.
    mov B$DisFlag DISDONE+DISLINEOVER
ret


Dis_r32__xmm_m64:
    mov bl B$esi | inc esi
    call WriteEregsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_r32__xmm_m32:
    mov bl B$esi | inc esi
    call WriteEregsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm1__xmm2_m128:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi
    ModMask bl to al
    If al = 3
        call WriteXMMRegsFromRmBits
    Else
        call EndWith.X.XMMmem
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mm1__mm2_m128:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi
    ModMask bl to al
    If al = 3
        call WriteMMXRegsFromRmBits
    Else
        call EndWith.Q.mem
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1__xmm2_m128__imm8:
    call Dis_xmm1__xmm2_m128 | mov B$edi ' ' | inc edi
    call WriteImm8
ret

Dis_xmm2_m128__xmm1:
    mov W$DisSizeMarker 'X$'
    mov bl B$esi | inc esi
    push ebx
        call WriteEffectiveXMMAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1__xmm2_m64:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm1__xmm2_m32:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm1__xmm2_m32F:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.F.mem
ret

Dis_mmx_rm32:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.D.mem
ret

Dis_xmm_rm32:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.D.mem
ret

Dis_rm32_xmmx:
    On B$OperandSizeOverride = &TRUE, jmp Dis_rm32_xmm
Dis_rm32_mmx:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'D$'
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteMMXRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_rm32_xmm:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'D$'
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx1__xmm2_m128:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_mmx1__xmm2_m64:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_xmm2_m64__xmm1:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'X$'
    push ebx
        call WriteEffectiveXMMAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm2_m32__xmm1:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'D$'
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm2_m32F__xmm1:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'F$'
    push ebx
        call WriteEffectiveAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx1__mmx2_m64:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.Q.MMXmem

Dis_mmx1__mmx2_m64_v2: ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Looki
    mov bl B$esi | inc esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.Q.MMXmem

Dis_xmmx1__xmmx2_m64_128:
    If B$OperandSizeOverride = &TRUE
        jmp Dis_xmm1__xmm2_m128
    Else
        jmp Dis_mmx1__mmx2_m64
    End_If

Dis_xmm1__xmm_m128:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem

Dis_mmx1__mmx2_m128:
    mov bl B$esi | inc esi
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.MMXmem

Dis_mmx_xmm:
    mov bl B$esi | inc esi
    RegMask bl to al
    mov D$edi 'MMX0', B$edi+4 ' ' | add B$edi+3 al | add edi 5
    BaseMask bl to al
    mov D$edi 'XMM0', B$edi+4 ' ' | add B$edi+3 al | add edi 4
ret

Dis_xmm_mmx:
    mov bl B$esi | inc esi
    RegMask bl to al
    mov D$edi 'XMM0', B$edi+4 ' ' | add B$edi+3 al | add edi 5
    BaseMask bl to al
    mov D$edi 'MM0 ' | add B$edi+2 al | add edi 3
ret

Dis_xmm1__mmx2_m64:
Dis_xmm1__mmx2_m128:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.X.XMMmem
ret

Dis_mmx2_m128__xmm1:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'X$'
    push ebx
        call WriteEffectiveXMMAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx1_m64__mmx2:
    mov bl B$esi | inc esi
    mov W$DisSizeMarker 'Q$'
    push ebx
        call WriteEffectiveMMXAddressFromModRm | mov B$edi ' ' | inc edi
    pop ebx
    call WriteMMXRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1__rm32:
    mov bl B$esi | inc esi
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi | jmp EndWith.D.mem
ret

Dis_xmmx_imm8:
    On B$OperandSizeOverride = &FALSE, jmp Dis_mmx_imm8

Dis_xmm_imm8:
   ; mov bl B$esi | inc esi
    call WriteXMMRegsFromRmBits | mov B$edi ' ' | inc edi
    call WriteImm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_mmx_imm8:
   ; mov bl B$esi | inc esi
    call WriteMMXRegsFromRmBits | mov B$edi ' ' | inc edi
    call WriteImm8
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m64:
    mov W$DisSizeMarker 'Q$'
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m32_16:
    On B$OperandSizeOverride = &TRUE, mov W$DisSizeMarker 'W$'
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m8:
    mov W$DisSizeMarker 'B$'
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret


WritePacketCondition:
    mov B$edi ' ' | inc edi | lodsb | and eax 0FF

    push eax
        call WriteEax
    pop eax

    mov D$edi ' ; (' | add edi 4

    If al = 0
        mov D$edi 'EQ) ' | add edi 3
    Else_If al = 1
        mov D$edi 'LT) ' | add edi 3
    Else_If al = 2
        mov D$edi 'LE) ' | add edi 3
    Else_If al = 3
        mov D$edi 'UNOR', W$edi 'D)' | add edi 6
    Else_If al = 4
        mov D$edi 'NEQ)' | add edi 4
    Else_If al = 5
        mov D$edi 'NLT)' | add edi 4
    Else_If al = 6
        mov D$edi 'NLE)' | add edi 4
    Else_If al = 7
        mov D$edi 'ORD)' | add edi 4
    Else
        add D$UnlikelyCode 5
    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret


EndWith.B.mem:
    mov W$DisSizeMarker 'B$' | jmp EndWithModRm
ret

EndWith.W.mem:
    mov W$DisSizeMarker 'W$'
EndWith.D.mem:
EndWithModRm:
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWith.WD.mem:
    On B$OperandSizeOverride = &TRUE, jmp EndWith.W.mem
    call WriteEffectiveAddressFromModRm
    mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWith.Q.mem:
    mov W$DisSizeMarker 'Q$' | jmp EndWithModRm
ret

EndWith.F.mem:
    mov W$DisSizeMarker 'F$' | jmp EndWithModRm
ret

EndWith.R.mem:
    mov W$DisSizeMarker 'R$' | jmp EndWithModRm
ret

EndWith.T.mem:
    mov W$DisSizeMarker 'T$' | jmp EndWithModRm
ret

EndWith.X.mem:
    mov W$DisSizeMarker 'X$' | jmp EndWithModRm
ret

EndWith.X.XMMmem:
EndWith.X.MMXmem:
    mov W$DisSizeMarker 'X$'
    ModMask bl to al
    If al = 3
        call WriteXMMregsFromRmBits
    Else
        call EndWithModRm
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWith.Q.MMXmem:
    mov W$DisSizeMarker 'Q$'
    ModMask bl to al
    If al = 3
        call WriteMMXregsFromRmBits
    Else
        call EndWithModRm
    End_If
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmmx1_xmmx2:
    On B$OperandSizeOverride = &TRUE, jmp Dis_xmm1_xmm2

Dis_mmx1_mmx2:
    call WriteMMXRegsFromRegBits | mov B$edi ' ' | inc edi
    call WriteMMXRegsFromRmBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm1_xmm2:
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi
    call WriteXMMRegsFromRmBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_xmm_m64:
    call WriteXMMRegsFromRegBits | mov B$edi ' ' | inc edi
    mov W$DisSizeMarker 'X$' | jmp EndWithModRm
ret

Dis_m64_xmm:
    mov W$DisSizeMarker 'X$' | call WriteEffectiveAddressFromModRm
    mov B$edi ' ' | inc edi
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m64_mmx:
    mov W$DisSizeMarker 'Q$' | call WriteEffectiveAddressFromModRm
    mov B$edi ' ' | inc edi
    call WriteMMXRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

Dis_m128_xmm:
    mov W$DisSizeMarker 'X$' | call WriteEffectiveAddressFromModRm
    mov B$edi ' ' | inc edi
    call WriteXMMRegsFromRegBits
    mov B$DisFlag DISDONE+DISLINEOVER
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Now come the terminal writing Routines.
____________________________________________________________________________________________
____________________________________________________________________________________________


[NextDisLine | push eax | mov eax 020200A0D | stosd | mov eax '    ' | stosw | pop eax]

[DisFlag: ?    SegmentOverride: ?
 OperandSizeOverride: ?    AddressSizeOverride: ?
 EscapePrefix: ?]

[DISDONE 1    DISLINEOVER 2    DISFAILED 0]

; Reads a Byte, returns the Hexa in ax. If The Hexa is greater than '0F', writes the
; wanted leading '0'.

[HexaTable: '0123456789ABCDEF']

OpToHexa:
    movzx eax B$esi | inc esi
LoadedOpToHexa:             ; If the Byte has already been loaded for some caller test.
    mov ebx eax | shr ebx 4
    and eax 0F | and ebx 0F
    mov al B$HexaTable+eax, bl B$HexaTable+ebx
    If ebx > '0'
        mov B$edi '0' | inc edi
    End_If
    shl eax 8 | or eax ebx
ret


[ModMask | mov #3 #1 | and #3 0011_000_000 | shr #3 6]
[DigitMask | mov #3 #1 | and #3 00_111_000 | shr #3 3]
[RegMask   | mov #3 #1 | and #3 00_111_000 | shr #3 3]
[RmMask    | mov #3 #1 | and #3 00_000_111]

[ScaleMask | mov #3 #1 | and #3 00_11_000_000 | shr #3 6]
[IndexMask | mov #3 #1 | and #3 00_111_000 | shr #3 3]
[BaseMask  | mov #3 #1 | and #3 00_000_111]
;;
 'DisSizeMarker' is set to 'D$' by default. If a 066 OpCode is encouneted, 'DisSizeMarker'
 is set to 'W$' at that time. So, All the OpCode Routines have the overwrite this Marker
 only in case of Bytes (which is always specific to one given OpCode).
;;
[DisSizeMarker: ?]


[dWordRegsTable: 'eax ecx edx ebx esp ebp esi edi '
 WordRegsTable:  'ax  cx  dx  bx  sp  bp  si  di  '
 ByteRegsTable:  'al  cl  dl  bl  ah  ch  dh  bh  '
 SregsTable:     'es  cs  ss  ds  fs  gs          ']

WriteEregsFromRmBits:
    RmMask bl To al | and eax 0FF
    move D$edi D$dWordRegsTable+eax*4 | add edi 3
ret

WriteEregsFromRegBits:
    RegMask bl To al | and eax 0FF
    move D$edi D$dWordRegsTable+eax*4 | add edi 3
ret

WritedWordRegsFromRegBits:
    On B$OperandSizeOverride = &TRUE, jmp WriteWordRegsFromRegBits
    RegMask bl To al | and eax 0FF
    move D$edi D$dWordRegsTable+eax*4 | add edi 3
Ret

WriteSregsFromModRm:
    RegMask bl To al | and eax 0FF
    move D$edi D$SregsTable+eax*4 | add edi 2  ; 0010001110 08C 00111
ret

WriteWordRegsFromRegBits:
    RegMask bl To al | and eax 0FF
    move D$edi D$WordRegsTable+eax*4 | add edi 2

Ret

WriteMMXRegsFromRegBits:
    RegMask bl To al
    mov D$edi 'MM0 ' | add B$edi+2 al | add edi 3
Ret

WriteMMXRegsFromRmBits:
    RmMask bl To al
    mov D$edi 'MM0 ' | add B$edi+2 al | add edi 3
Ret

WriteXMMRegsFromRegBits:
    RegMask bl To al
    mov D$edi 'XMM0' | add B$edi+3 al | add edi 4
Ret

WriteXMMRegsFromRmBits:
    RmMask bl To al
    mov D$edi 'XMM0' | add B$edi+3 al | add edi 4
Ret

WriteWordRegsFromRmBits:
    RmMask bl To al | and eax 0FF
    move D$edi D$WordRegsTable+eax*4 | add edi 2
ret

WriteByteRegsFromRmBits:
    RmMask bl To al | and eax 0FF
    move D$edi D$ByteRegsTable+eax*4 | add edi 2
ret

WriteByteRegsFromRegBits:
    RegMask bl To al | and eax 0FF
    move D$edi D$ByteRegsTable+eax*4 | add edi 2
ret


; Writes, for example: 'D$', 'CS:B$cs:', ...
StartEffectiveAddress:
    move W$edi W$DisSizeMarker | add edi 2

    If D$SegmentOverride <> 0
        move D$edi D$SegmentOverride | add edi 3
    End_If
ret


WriteFromSib: ; 085  00_10_000_101
    ModMask bl To cl                                ; Saves the MOD Bits for Base5 case.
    lodsb | mov bl al | BaseMask bl To al ; 025 00_00_100_101

    If al = 0      | mov D$edi 'eax+' | add edi 4
    Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
    Else_If al = 2 | mov D$edi 'edx+' | add edi 4
    Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
    Else_If al = 4 | mov D$edi 'esp+' | add edi 4
    Else_If al = 5 | call Base5                     ; No Base or ebp with dis 8 or 32
    Else_If al = 6 | mov D$edi 'esi+' | add edi 4
    Else           | mov D$edi 'edi+' | add edi 4
    End_If

    IndexMask bl To al

    .If al = 0      | mov D$edi 'eax ' | add edi 3
    .Else_If al = 1 | mov D$edi 'ecx ' | add edi 3
    .Else_If al = 2 | mov D$edi 'edx ' | add edi 3
    .Else_If al = 3 | mov D$edi 'ebx ' | add edi 3
    .Else_If al = 4
        If cl = 0FF
          ; Example: cmp ah B$078
            On W$DisSizeMarker <> 'D$', inc D$UnlikelyCode
        End_If
        On B$edi-1 = '+', dec edi             ; None / Strip the '+'
        jmp L9>
    .Else_If al = 5 | mov D$edi 'ebp ' | add edi 3
    .Else_If al = 6 | mov D$edi 'esi ' | add edi 3
    .Else           | mov D$edi 'edi ' | add edi 3
    .End_If

    ScaleMask bl To al

    If al = 0                                       ; no need *1
    Else_If al = 1 | mov W$edi '*2' | add edi 2
    Else_If al = 2 | mov W$edi '*4' | add edi 2
    Else_If al = 3 | mov W$edi '*8' | add edi 2
    End_If
L9: ret


Base5:          ; No Base or ebp with dis 8 or 32 (cl is the MOD bits from previous ModRm).
    If cl <> 0
        mov D$edi 'ebp+' | add edi 4
    Else
        mov cl 0FF
    End_If
ret


[ToJumpsTable: ?] ; Might now be removed entirely.

WriteBase5dis32:
    If B$edi-1 <> '+'
        mov B$edi '+' | inc edi
    End_If

WriteDis32:
    If B$AddressSizeOverride = &FALSE
        lodsd
    Else
WriteDis16:
        lodsw | and eax 0FFFF | call WriteEax | ret
    End_If

WriteDisRelative:
    mov D$LastCodeRef eax | On eax = 0, jmp L8>>

L0: If B$SimpleScan = &TRUE
        mov B$DisFlag DISDONE+DISLINEOVER | ret
    End_If

L0: On B$WeAreInTheCodeBox = &TRUE, jmp L8>>
;On eax = 01013E38, int3
    sub eax D$DisImageBase | add eax D$SectionsMap

    On eax >= D$EndOfSectionsMap, jmp L8>>
    On eax <= D$SectionsMap, jmp L8>>

    mov B$ToJumpsTable &FALSE
    mov al B$eax | and al DATAFLAG+VIRTUALFLAG+IMPORTFLAG+CODEFLAG
;map
    ..If al = 0
        mov eax D$LastCodeRef

        mov ebx eax | sub ebx D$DisImageBase | add ebx D$SizesMap

            .If B$LeaInstruction = &TRUE
                mov B$LeaInstruction &FALSE
                sub ebx D$SizesMap | add ebx D$SectionsMap | mov B$ebx DATAFLAG
            .Else_If W$edi-2 = 'B$'
                or B$ebx BYTE
                sub ebx D$SizesMap | add ebx D$SectionsMap | mov B$ebx DATAFLAG
            .Else_If W$edi-2 = 'W$'
                or B$ebx WORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov B$ebx DATAFLAG, B$ebx+1 DATAFLAG
            .Else_If W$edi-2 = 'D$'
                or B$ebx DWORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'F$'
                or B$ebx FP4
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'R$'
                or B$ebx FP8
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS
            .Else_If W$edi-2 = 'T$'
                or B$ebx FP10
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS, D$ebx+6 FOURDATAFLAGS
            .Else
                jmp L8>>
               ; or B$ebx POINTER
            .End_If

    ..Else_If al = DATAFLAG
        mov eax D$LastCodeRef | call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        mov D$edi 'Data' | add edi 4 | jmp L8>>

    ..Else_If al = CODEFLAG
      ; Is it a call to a Jumps Table?
        mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$UserPeStart

        If W$eax = 025FF ; Code of jmp relative long
            mov ebx D$eax+2 | sub ebx D$DisImageBase | add ebx D$SectionsMap
            On ebx > D$EndOfSectionsMap, jmp L1>
            On ebx < D$SectionsMap, jmp L1>
                On B$ebx <> IMPORTFLAG, jmp L1>

                    mov B$ApiCommentWanted &TRUE, ebx D$eax+2, D$PtrToApiName ebx
        End_If

L1:     mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$RoutingMap
        test B$eax INSTRUCTION | jz L8>>
        or B$eax NODE+LABEL | mov D$edi 'Code' | add edi 4 | jmp L8>>

    ..Else_If al = VIRTUALFLAG
        mov eax D$LastCodeRef | call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        mov D$edi 'Virt', D$edi+4 'ual ' | add edi 7 | jmp L8>>

    ..Else_If al = IMPORTFLAG
        mov ebx D$LastCodeRef
L5:     sub ebx D$DisImageBase | add ebx D$UserPeStart | mov ebx D$ebx
      ; May be a wrong pointing inside the .Import!
      ; Add a Pointer test!
        On ebx < D$ApiBuffer, ret
        On ebx >= D$EndOfApiBuffer, ret

        push esi

            On W$edi-2 = 'D$', sub edi 2

            mov esi ebx

            .If D$edi-4 = 'jmp '    ; Jumps Table?
                call WriteApiJmpTableLabel

                While B$esi <> 0 | movsb | End_While

            .Else_If D$edi-4 = 'all '  ; call api?
                call FlagNoReturnApi
                call FlagApiProcedures
                While B$esi <> 0 | movsb | End_While

            .Else_If D$edi-4 = '$cs:'
                sub edi 5 | jmp L6>

          ; Other case: Either "mov eax D$ApiCall" or "mov D$eax ApiCall"
            .Else
                push edi
                    mov al "'"
                    While B$edi > LF
                        dec edi
                        On B$edi = '$', mov al 0
                    End_While
                pop edi
L6:             mov esi ebx
                If al = 0
                    While B$esi <> '.' | inc esi | End_While | inc esi
                End_If
                While B$esi <> 0 | movsb | End_While
                On al = 0, dec edi

            .End_If
        pop esi

     ..End_If
ret

L8: On B$WeAreInTheCodeBox = &FALSE, jmp L8>
    On D$LibFileMemory = 0, jmp L8>
        push esi
            mov esi D$LastCodeRef
            .If esi > D$LibFileMemory
                mov eax D$LibFileMemory | add eax D$LibFileLength
                If esi < eax
                    While B$esi <> 0 | movsb | End_While
                   ; mov D$edi ' ; <', D$edi+4 '<<<<' | add edi 8
                Else
                    pop esi | jmp L8>
                End_If
            .Else
                pop esi | jmp L8>
            .End_If
        pop esi
        ret

L8: ;On D$LastCodeRef = 0438E28, int3

    .If B$edi-1 = '+'
        If W$edi-3 = '*2'
            call TryWithIndice 2
        Else_If W$edi-3 = '*4'
            call TryWithIndice 4
        Else_If W$edi-3 = '*8'
            call TryWithIndice 8
        End_If
    .End_If

    If W$DisplacementFromLabel = 0
        mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SizesMap
        mov ebx D$SizesMap | add ebx 4
        On eax < ebx, jmp L8>
        On eax > D$EndOfSizesMap, jmp L8>
        test B$eax-4 FP8 | jz L8>
            sub D$LastCodeRef 4 | mov W$DisplacementFromLabel '+4'
    End_If

L8: push 0-1

    mov ebx D$LastCodeRef

L0: mov eax ebx | shr ebx 4 | and eax 0F
    add eax '0' | On eax > '9', add eax 7
    push eax
    cmp ebx 0 | ja L0<

    mov B$edi '0' | inc edi
L0: pop eax | cmp eax 0-1 | je L9>
    mov B$edi al | inc edi | jmp L0<

L9: mov ebx D$LastCodeRef | sub ebx D$DisImageBase | add ebx D$UserPeStart

    ..If ebx > D$UserPeStart
        .If ebx < D$UserPeEnd
            mov ebx D$LastCodeRef | ToStringsMapFrom DisImageBase, ebx
            If D$ebx <> 0
                push esi
                ;On D$LastCodeRef = 04037EC, showme D$ebx
                    zCopy D$ebx
                pop esi
            End_If
        .End_If
    ..End_If

    If W$DisplacementFromLabel <> 0
        mov ax W$DisplacementFromLabel | stosw
        mov W$DisplacementFromLabel 0
    End_If

    mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SectionsMap

    .If eax < D$SectionsMap
        ;
    .Else_If eax < D$EndOfSectionsMap
        If B$eax = DATAFLAG
            sub eax D$SectionsMap | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        End_If
    .End_If

    On B$ApiCommentWanted = &TRUE, call WriteApiLabelComment
ret


[DisplacementFromLabel: ?]

Proc TryWithIndice:
    Argument @Indice

        mov eax D$LastCodeRef | add eax D@Indice
        sub eax D$DisImageBase | add eax D$SectionsMap

        ..If eax < D$SectionsMap
            ;
        ..Else_If eax < D$EndOfSectionsMap
            .If B$eax = DATAFLAG
                sub eax D$SectionsMap | add eax D$RoutingMap
                test B$eax LABEL+EVOCATED | jz L9>
                    mov eax D@Indice | add D$LastCodeRef eax
                    If eax = 2
                        mov W$DisplacementFromLabel '-2'
                    Else_If eax = 4
                        mov W$DisplacementFromLabel '-4'
                    Else_If eax = 8
                        mov W$DisplacementFromLabel '-8'
                    End_If

                    mov D$edi 'Data' | add edi 4
            .End_If
        ..End_If
L9:
EndP
____________________________________________________________________________________________

WriteApiLabel:
    While B$esi <> 0
        lodsb
;;
  ; No need as long as they are all Comments, now:
        If al = '$'
            mov al 'S'
        Else_If al = '$'
            mov al 'S'
        Else_If al = '@'
            mov al 'a'
        End_If
;;
        stosb
    End_While
ret

[ApiCommentWanted: ? PtrToApiName: ?]

WriteApiLabelComment:
    push esi
        mov B$ApiCommentWanted &FALSE

        mov D$edi ' ; ' | add edi 3
        mov esi D$PtrToApiName
        sub esi D$DisImageBase | add esi D$UserPeStart
        mov esi D$esi
        push esi
            call WriteApiLabel
        pop esi
        call FlagApiProcedures
    pop esi
ret


WriteApiJmpTableLabel:
    While B$edi-1 > LF | dec edi | End_While

    push esi
        mov W$edi '; ' | add edi 2

        mov esi ebx | inc esi

        call WriteApiLabel
    pop esi

    mov B$edi-1 ':' | NextDisLine
    mov D$edi 'jmp ' | add edi 4
ret
____________________________________________________________________________________________

FlagNoReturnApi:
    push esi
        inc esi
;;
      ; MSVBVM50.064 ???
        ...If D$esi = 'MSVB'
            ..If D$esi+4 = 'VM50'
                .If D$esi+8 = '.064'
                    If B$esi+12 = "'"
                        mov B$DisEndOfChunk &TRUE
                    End_If
                .End_If
            ..End_If
        ...End_If
;;
        While B$esi <> '.' | inc esi | End_While | inc esi

      ; ExitProcess
        ...If D$esi = 'Exit'
            ..If D$esi+4 = 'Proc'
                If D$esi+8 = "ess'"
                    mov B$DisEndOfChunk &TRUE
                End_If
            ..End_If

      ; __vbaErrorOverflow
        ...Else_If D$esi = '__vb'
            ..If D$esi+4 = 'aErr'
                .If D$esi+8 = 'orOv'
                    If D$esi+12 = 'erfl'
                        On W$esi+16 = 'ow', mov B$DisEndOfChunk &TRUE
                    End_If
                .End_If
            ..End_If

        ...Else
          ; For freezing the 'IsItNoReturnCall' test:
            mov B$CallInstruction &FALSE

        ...End_If
    pop esi
ret
____________________________________________________________________________________________

;;
  The 4 Dialogs Creations Functions:
  
        'USER32.CreateDialogParamA'         >>> second above push
        'USER32.CreateDialogindirectParamA' >>> second above push
        
        'USER32.DialogBoxParamA'            >>> second above push
        'USER32.DialogBoxindirectParamA'    >>> second above push

  The 'USER32.SetWindowLong' Function, with the "GWL_WNDPROC" Parameter
  GWL_WNDPROC >>> second above push
  Procedure >>> first above push

  The 'KERNEL32.CreateThread' Function.     >>> fourth above push

  The 'USER32.SetWindowsHookExA' Function.          >>> third above push
  
  List of Api with CallBacks:
  
    USER32.CreateDialogParamA
    USER32.CreateDialogindirectParamA
    USER32.DialogBoxParamA
    USER32.DialogBoxindirectParamA
    USER32.SetWindowLongA
    KERNEL32.CreateThread
    USER32.SetWindowsHookExA
    USER32.EnumChildWindows
;;

FlagApiProcedures:
    push esi

    While B$esi <> '.' | inc esi | End_While | inc esi

      ; ExitProcess
        ...If D$esi = 'Crea'
            ..If D$esi+4 = 'teDi'
                .If D$esi+8 = "alog"
                    If D$esi+12 = 'Para'
                      ; 'USER32.CreateDialogParamA'
                        call GetDialogProcedure

                    Else_If D$esi+12 = 'indi'
                      ; 'USER32.CreateDialogindirectParamA'
                        call GetDialogProcedure

                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'Dial'
            ..If D$esi+4 = 'ogBo'
                If D$esi+8 = 'xPar'
                  ; 'USER32.DialogBoxParamA'
                    call GetDialogProcedure

                Else_If D$esi+8 = 'indi'
                  ; 'USER32.DialogBoxindirectParamA'
                    call GetDialogProcedure

                End_If
            ..End_If

        ...Else_If D$esi = 'SetW'
            ..If D$esi+4 =  'indo'
                .If D$esi+8 = 'wLon'
                  ; 'USER32.SetWindowLongA'
                    call GetApiPush 2
                    If eax = &GWL_WNDPROC
                        call GetApiPush 3
                        On eax <> 0, call SetApiProcedure eax, WindowProcName
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'Crea'
            ..If D$esi+4 = 'teTh'
                If D$esi+8 = 'read'
                  ; 'KERNEL32.CreateThread'
                    call GetApiPush 3
                    On eax <> 0, call SetApiProcedure eax, ThreadProcName
                End_If
            ..End_If

        ...Else_If D$esi = 'SetW'
            ..If D$esi+4 = 'indo'
                .If D$esi+8 = 'wsHo'
                    If D$esi+12 = 'okEx'
                      ; 'USER32.SetWindowsHookExA'
                        call GetApiPush 2
                        On eax <> 0, call SetApiProcedure eax, HookProcName
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'Enum'
            ..If D$esi+4 = 'Chil'
                .If D$esi+8 = 'dWin'
                    If D$esi+12 = 'dows'
                      ; 'USER32.EnumChildWindows'
                        call GetApiPush 2
                        On eax <> 0, call SetApiProcedure eax, EnumChildWindowsProcName
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'lstr'
            ..If D$esi+4 = 'cpyA'
              ; 'KERNEL32.lstrcpyA'
                call GetApiPush 1
                On eax <> 0, call SetApiData eax
                call GetApiPush 2
                On eax <> 0, call SetApiData eax
            ..End_If

        ...Else_If D$esi = 'Regi'
            ..If D$esi+4 = 'ster'
                .If D$esi+8 = 'Clas'
                    If D$esi+12 = 'sExA'
                      ; 'USER32.RegisterClassExA/W'
L1:                     call GetApiPush 1 | On eax = 0, jmp L9>
                        push eax
                            call SelectMainWindowProc eax, 12
                        pop eax
                        call SetWINDCLASSEXData eax

                    Else_If D$esi+12 = 'sExW'
                        jmp L1<

                    Else_If B$esi+12 = 's'
                      ; 'USER32.RegisterClassA/W'
                        call GetApiPush 1 | On eax = 0, jmp L9>
                        push eax
                            call SelectMainWindowProc eax, 10
                        pop eax
                        call SetWINDCLASSData eax
                    End_If

                .End_If
            ..End_If
        ...End_If
L9: pop esi
ret


[DisMainWindowProc: ?]

Proc SelectMainWindowProc:
    Argument @WNDCLASS, @N
    Uses edi, ebx, ecx

        mov eax D@WNDCLASS | sub eax D$DisImageBase | add eax D$UserPeStart

        mov eax D$eax+8

        ..If eax <> 0
            .If D$DisMainWindowProc = 0
                mov D$DisMainWindowProc eax
                call SetApiProcedure eax, MainWindowProcName

            .Else
              ; Several candidates: Take the one closer to 'Main':
                mov ebx D$DisMainWindowProc | sub ebx D$DisEntryPoint | Absolute ebx
                mov ecx eax | sub ecx D$DisEntryPoint | Absolute ecx
                If ecx < ebx
                    call SetApiProcedure D$DisMainWindowProc, WindowProcName
                    call SetApiProcedure eax, MainWindowProcName
                    mov D$DisMainWindowProc eax
                Else_If ecx > ebx
                    call SetApiProcedure eax, WindowProcName
                End_If
            .End_If
        ..End_If

        mov edi D@WNDCLASS | sub edi D$DisImageBase | add edi D$SectionsMap

        mov ecx D@N, eax FOURDATAFLAGS | rep stosd
EndP


[WNDCLASS.cbSize.Name: B$'_cbSize', 0

 WNDCLASS.style.Name: B$ '_style', 0
 WNDCLASS.lpfnWndProc.Name: '_lpfnWndProc', 0
 WNDCLASS.cbClsExtra.Name: '_cbClsExtra', 0
 WNDCLASS.cbWndExtra.Name: '_cbWndExtra', 0
 WNDCLASS.hInstance.Name: '_hInstance', 0
 WNDCLASS.hIcon.Name: '_hIcon', 0
 WNDCLASS.hCursor.Name: '_hCursor', 0
 WNDCLASS.hbrBackground.Name: '_hbrBackground', 0
 WNDCLASS.lpszMenuName.Name: '_lpszMenuName', 0
 WNDCLASS.lpszClassName.Name: '_lpszClassName', 0

 WNDCLASS.hIconSm.Name: B$'_hIconSm', 0]

Proc SetWINDCLASSData:
    Argument @Pointer
    Uses edi

        mov eax D@Pointer

        ToStringsMapFrom DisImageBase eax
        mov D$eax WNDCLASS.style.Name | add eax 16
        mov D$eax WNDCLASS.lpfnWndProc.Name | add eax 16
        mov D$eax WNDCLASS.cbClsExtra.Name | add eax 16
        mov D$eax WNDCLASS.cbWndExtra.Name | add eax 16
        mov D$eax WNDCLASS.hInstance.Name | add eax 16
        mov D$eax WNDCLASS.hIcon.Name | add eax 16
        mov D$eax WNDCLASS.hCursor.Name | add eax 16
        mov D$eax WNDCLASS.hbrBackground.Name | add eax 16
        mov D$eax WNDCLASS.lpszMenuName.Name | add eax 16
        mov D$eax WNDCLASS.lpszClassName.Name

        mov edi D@Pointer | sub edi D$DisImageBase | add edi D$RoutingMap
        mov eax LABEL, ecx 10 | rep stosd

        mov edi D@Pointer | sub edi D$DisImageBase | add edi D$SizesMap
        mov eax DWORD, ecx 10 | rep stosd
EndP


Proc SetWINDCLASSEXData:
    Argument @Pointer
    Uses edi

        mov eax D@Pointer | ToStringsMapFrom DisImageBase, eax

        mov D$eax WNDCLASS.cbSize.Name | add eax 16
        mov D$eax WNDCLASS.style.Name | add eax 16
        mov D$eax WNDCLASS.lpfnWndProc.Name | add eax 16
        mov D$eax WNDCLASS.cbClsExtra.Name | add eax 16
        mov D$eax WNDCLASS.cbWndExtra.Name | add eax 16
        mov D$eax WNDCLASS.hInstance.Name | add eax 16
        mov D$eax WNDCLASS.hIcon.Name | add eax 16
        mov D$eax WNDCLASS.hCursor.Name | add eax 16
        mov D$eax WNDCLASS.hbrBackground.Name | add eax 16
        mov D$eax WNDCLASS.lpszMenuName.Name | add eax 16
        mov D$eax WNDCLASS.lpszClassName.Name | add eax 16
        mov D$eax WNDCLASS.hIconSm.Name

        mov edi D@Pointer | sub edi D$DisImageBase | add edi D$RoutingMap
        mov eax LABEL+EVOCATED, ecx 12 | rep stosd

        mov edi D@Pointer | sub edi D$DisImageBase | add edi D$SizesMap
        mov eax DWORD, ecx 12 | rep stosd
EndP


Proc GetApiPush:
    Argument @Indice

        mov D$PossibleApiPointer 0

        mov esi D$TestLastLineLocation | On esi = 0, jmp L9>>

        dec esi | mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap

L0:     movzx eax B$ebx
        test eax INSTRUCTION | jz L2>>
        test eax NODE+EXPORTNODE+PUSH_EBP | jnz L9>>

            mov al B$esi
          ; IsItPush    >>>>>>>>>   Op68 !!!!!!!!!!!
            .If al = 060 ; pushad
                jmp L9>>
            .Else_If al = 0FF ; OpFF // /6   ; 75
                DigitMask B$esi+1 To al
                If al = 6
                    mov eax &TRUE
                Else
                    mov eax &FALSE
                End_If

            .Else_If al = 06A
                mov eax &TRUE
            .Else_If al = 068
                mov eax &TRUE
            .Else_If al < 050
                mov eax &FALSE
            .Else_If al < 058
                mov eax &TRUE       ; push reg (050 to 057)
            .Else
                mov eax &FALSE
            .End_If

            ...If eax = &TRUE
                dec D@Indice

                ..If D@Indice = 0
                    If B$esi = 0FF ; OpFF, 035 = 00_110_101
                        add esi 2 | lodsd
                        cmp eax D$UserPeStart | jb L9>>
                        cmp eax D$UserPeEnd | ja L9>>
                            mov eax D$eax | ExitP

                    Else_If B$esi = 068 ; Op68
                        cmp B$esi-1 0F | je L9>  ; Op0F: Escape Prefix
                        cmp B$esi-1 066 | je L9>;   Op66 >>> Operand Size Override
                            inc esi | lodsd | ExitP
                    End_If

                ..End_If
            ...End_If

L2:     dec ebx | dec esi | jmp L0<<

L9:     mov eax 0
EndP


[PossibleApiPointer: ?  PossibleApiPointerIndice: ?]

Proc GetPossibleApiPointer:
    Argument @Pointer, @Indice

        mov eax D@Pointer

        sub eax D$DisImageBase | add eax D$UserPeStart | mov eax D$eax
      ; May be a wrong pointing inside the .Import!
      ; Add a Pointer test!
        If eax < D$ApiBuffer
            mov D$PossibleApiPointer 0
        Else_If eax >= D$EndOfApiBuffer
            mov D$PossibleApiPointer 0
        Else
            mov D$PossibleApiPointer eax
            mov eax D@Indice, D$PossibleApiPointerIndice eax
        End_If
EndP


GetDialogProcedure:
    call GetApiPush 4

    .If eax <> 0
        call SetApiProcedure eax, DialogProcName

    .Else_If D$PossibleApiPointer <> 0
;;
  There are case of:
  
        call 'KERNEL32.GetModuleHandleA'  ; FF 15 80 C1 43 00 
        push eax
        
        used inside the PUSHes flow, to set-up the 'hInstance' Member.
;;
        If D$PossibleApiPointerIndice = 3
            call GetApiPush 5
            On eax <> 0, call SetApiProcedure eax, DialogProcName
        End_If

    .End_If
ret


[MainWindowProcName: "MainWindowProc", 0

WindowProcName: '_WindowProc', 0
ThreadProcName: '_ThreadProc', 0
HookProcName: '_HookProc', 0
EnumChildWindowsProcName: '_EnumChildWindowsProc', 0
DialogProcName: '_DialogProc', 0]

Proc SetApiProcedure:
    Argument @Pointer, @Name
    Uses eax

        mov eax D@Pointer

        sub eax D$DisImageBase | add eax D$SectionsMap

        .If eax > D$SectionsMap
            If eax < D$EndOfSectionsMap
                mov B$eax CODEFLAG

                sub eax D$SectionsMap | add eax D$RoutingMap
                mov B$eax LABEL+NODE+EVOCATED+INSTRUCTION+ACCESSED

                ToStringsMapFrom RoutingMap, eax
                move D$eax D@Name
            End_If
        .End_If
EndP


Proc SetApiData:
    Argument @Pointer

        mov eax D@Pointer

        sub eax D$DisImageBase | add eax D$SectionsMap

        .If eax > D$SectionsMap
            If eax < D$EndOfSectionsMap
                mov B$eax DATAFLAG

                sub eax D$SectionsMap | add eax D$RoutingMap
                or B$eax LABEL+EVOCATED
            End_If
        .End_If

EndP


____________________________________________________________________________________________

; Storing the SizeOf the accessed element into 'SizesMap'. Before eax holds D$LastCodeRef.

StoreDisSize:
    mov ebx eax | sub ebx D$DisImageBase | add ebx D$SizesMap

    .If B$LeaInstruction = &TRUE
        or B$ebx POINTER | mov B$LeaInstruction &FALSE
    .Else_If W$edi-2 = 'B$'
        or B$ebx BYTE | call FlagData 1
    .Else_If W$edi-2 = 'W$'
        or B$ebx WORD | call FlagData 2
    .Else_If W$edi-2 = 'D$'
        or B$ebx DWORD | call FlagPointerToPointer| call FlagData 4
    .Else_If W$edi-2 = 'F$'
        or B$ebx FP4 | call FlagData 4
    .Else_If W$edi-2 = 'R$'
        or B$ebx FP8 | call FlagData 8
    .Else_If W$edi-2 = 'T$'
        or B$ebx FP10 | call FlagData 10
    .Else
        ;or B$ebx POINTER
        call FlagPointerToPointer
    .End_If
ret


Proc FlagData:
    Argument @N
    Uses edi, eax, ecx

        mov edi ebx | sub edi D$SizesMap | add edi D$SectionsMap
        On B$edi = VIRTUALFLAG, Exitp
        mov ecx D@N, al DATAFLAG | rep stosb
EndP


FlagPointerToPointer:
   push ebx
      ; If it is a Pointer to Pointer, LABEL the final Location:
        mov ebx eax | sub ebx D$DisImageBase | add ebx D$UserPeStart
      ; Here, eax still is 'D$LastCodeRef'
        ..If ebx < D$UserPeStart
            ;
        ..Else_If ebx < D$UserPeEnd
            sub ebx D$UserPeStart | add ebx D$RoutingMap | or B$ebx EVOCATED+LABEL

            sub ebx D$RoutingMap | add ebx D$UserPeStart
            mov ebx D$ebx | sub ebx D$DisImageBase | add ebx D$UserPeStart
            .If ebx < D$UserPeStart
                ;
            .Else_If ebx < D$UserPeEnd
                push eax
                    sub ebx D$UserPeStart | add ebx D$RoutingMap
                    mov eax ebx | sub eax D$RoutingMap | add eax D$SectionsMap
                    If B$eax = CODEFLAG
                        or B$ebx EVOCATED;+LABEL
                    Else
                        or B$ebx EVOCATED+LABEL
                    End_If
                pop eax
            .End_If

        ..End_If
    pop ebx
ret

____________________________________________________________________________________________

[DisShortRef: B$ ?    EncreasedLocalJmp: ?]

EndWithDisByteRelative:
    ;On B$esi = 0, add B$UnlikelyCode 5 ; Absurd, but really found in some Code.

    If B$SimpleScan = &TRUE
        inc esi | mov B$DisFlag DISDONE+DISLINEOVER | ret
    End_If

    mov B$EncreasedLocalJmp &FALSE
    movsx eax B$esi | inc esi | mov D$LastCodeRef eax
    call RelativeToAbsolute
    push eax
        call WriteLocalLabelFromEax
    pop eax
    If eax > D$LastCodeRef
        mov B$edi-1 '>'
    Else
        mov B$edi-1 '<'
    End_If

    On B$WeAreInTheCodeBox = &TRUE, ret
;jmp L7>>
  ; Is the short jump out of range because of the replacement of a DLL call Direct
  ; instead of Indirect?
    push esi, eax
        mov ecx 0
        mov al B$esi-1, bl al, B$DisShortRef al | and bl 00_1000_0000

      ; Negatif short?
        ..If bl = 00_1000_0000
            sub esi 3 | neg al
            .While al <> 0
                dec esi
              ; Code of Call Indirect (To Jumps Table?).
                .If B$esi = 0E8
                  ; Yes, but is it an Instruction ?
                    mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap
                    test B$ebx INSTRUCTION | jz L1>
                  ; Relative to absolute:
                    mov ebx esi | add ebx D$esi+1 | add ebx 5
                    cmp ebx D$UserPeStart | jb L1>
                    cmp ebx D$UserPeEnd | ja L1>
                        If W$ebx = 025FF
                            mov ebx D$ebx+2
                            sub ebx D$DisImageBase | add ebx D$SectionsMap

                            On ebx > D$EndOfSectionsMap, jmp L1>
                            On ebx < D$SectionsMap, jmp L1>
                                On B$ebx = IMPORTFLAG, inc ecx
L1:                     End_If
                .End_If
                dec al
            .End_While

        ..Else
            .While al <> 0
              ; Code of Call Direct (To Jumps Table?).
                .If B$esi = 0E8
                  ; Yes, but is it an Instruction ?
                    mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap
                    test B$ebx INSTRUCTION | jz L1>
                  ; Relative to absolute:
                    mov ebx esi | add ebx D$esi+1 | add ebx 5
                    cmp ebx D$UserPeStart | jb L1>
                    cmp ebx D$UserPeEnd | ja L1>
                        If W$ebx = 025FF  ; FF 25 XX XX XX XX > jmp D$xxxx
                            mov ebx D$ebx+2
                            sub ebx D$DisImageBase | add ebx D$SectionsMap
                            cmp ebx D$SectionsMap | jb L1>
                            cmp ebx D$EndOfSectionsMap | ja L1>
                                On B$ebx = IMPORTFLAG, inc ecx
L1:                     End_If
                .End_If
                dec al | inc esi
            .End_While

        ..End_If

L2:     .If ecx <> 0
            mov al B$DisShortRef
            If B$edi-1 = '>'
                add al cl | test al 00_1000_0000 | jz L5>
                mov B$edi '>' | inc edi
            Else
                sub al cl | test al 00_1000_0000 | jnz L5>
                mov B$edi '<' | inc edi
            End_If
            mov B$EncreasedLocalJmp &TRUE
        .End_If

L5: pop eax, esi
L7:
    mov D$edi '  ; ' | add edi 4

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;mov D$edi 'Code' | add edi 4
    call RelativeToAbsolute | mov D$LastCodeRef eax
    ;push eax
    ;    call WriteEax
    ;pop eax
    mov ebx D$LastCodeRef
    sub ebx D$DisImageBase | add ebx D$SectionsMap | mov B$ebx CODEFLAG
    call WriteDisRelative
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    If B$EncreasedLocalJmp = &TRUE
        NextDisLine
        mov D$edi "; >>", D$edi+4 "> 'C", D$edi+8 "omme", D$edi+12 "nt1'"
        add edi 16
    End_If

    mov B$DisFlag DISDONE+DISLINEOVER
ret

; Same as upper, but, in cases of Data in code, a LOOP L2>, might be generated. Not
; that important, but what is important is that this could mess up the targeted Code
; when re-aligning because of Code reference.

EndWithDisByteRelativeBack:
    movsx eax B$esi | On eax < 3, add B$UnlikelyCode 5

    inc esi | mov D$LastCodeRef eax
    call RelativeToAbsolute
    push eax
        call WriteLocalLabelFromEax
    pop eax
    If eax > D$LastCodeRef
        mov B$edi-1 '>'; ; ', D$edi+3 '!!!!' | add edi 7 ; jE! >>Agree DOWN_Loop
        ;mov D$LastCodeRef 0, B$DisFlag DISDONE+DISLINEOVER | ret
    Else
        mov B$edi-1 '<'
    End_If
    mov D$edi '  ; ' | add edi 4
    call RelativeToAbsolute | mov D$LastCodeRef eax
    call WriteDisRelative
    mov B$DisFlag DISDONE+DISLINEOVER
ret

EndWithDisWordDwordRelative:
    If B$OperandSizeOverride = &TRUE
        movsx eax W$esi | add esi 2
    Else
        lodsd
    End_If
    mov D$LastCodeRef eax
    call RelativeToAbsolute | call WriteDisRelative
    mov B$DisFlag DISDONE+DISLINEOVER
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; This is the Chunks of Macros to be saved at Top of the Disassembly Source:

[DisMacros2: "____________________________________________________________________________________________

;;
  Used Macros Strings Variables:
  
    &1, &2, &3              Proc
    &9, &10 to &19          For
 
  Used Macros Counters Variables:
 
    &&0, &&1                If
    &&2, &&3                While
    &&4, &&5                Do
    &&6, &&7                For
    
  Local Labels Attributions:
  
    O1                      On
    P9                      Proc
    I0,... I9, J0,... J9    If
    W0,... W9               While
    D0,... D9               Do
    F0,... F9               For
;;
____________________________________________________________________________________________

[= e   < b    > a    <s l    >s g    =< be    <= be    => ae    >= ae    <> ne]
____________________________________________________________________________________________

; Multi push, pop, mov, move, inc, and dec  Macros:

[push | push #1 | #+1]
[pop | pop #1 | #+1]
[mov | mov #1 #2 | #+2]
[move | push #2 | pop #1 | #+2]
[inc | inc #1 | #+1]
[dec | dec #1 | #+1]
____________________________________________________________________________________________

[Exchange | push #1 | push #2 | pop #1 | pop #2 | #+2]
____________________________________________________________________________________________

[On | cmp #1 #3 | jn#2 O1> | #4>L | O1: ]
____________________________________________________________________________________________

[call | #If #1=str
            ApiPush #L>2
        #Else
            push #L>2
        #End_If
        call #1]

[ApiPush | #If #1=str
                push {#1, 0}
           #Else
                push #1
           #End_If
           #+1]
____________________________________________________________________________________________

; C calling convention:

[ccall
    push #L>2 | call #1
    #If #N>1
        add esp ((#N-1)*4)
    #EndIf]
____________________________________________________________________________________________

[.If
    #If &&0<>0
        #If &&0<> '0'
            #Error 'Unpaired If'
        #End_If
    #End_If
    &&0= '0' | &&1=Pos
    AndCmp I&&0>>, #1>L]

[.End_If | I&&0: | J&&0:
    #If &&0<> '0'
        #ErrorPos &&1 'Unpaired If'
    #End_If
    &&0=0]

[If
    #If &&0=0
        &&0= '0'
    #Else
        &&0=&&0+1
    #End_If

    AndCmp I&&0>>, #1>L]

[Else_If | jmp J&&0>> | I&&0:
    AndCmp I&&0>>, #1>L]

[Else | jmp J&&0>> | I&&0: ]

[End_If | I&&0: | J&&0:
    #If &&0= '0'
        &&0=0
    #Else
        &&0=&&0-1
    #End_If]

[AndCmp | cmp #2 #4 | jn#3 #F | #+3]
____________________________________________________________________________________________

[.While
    #If &&2<>0
        #If &&2<> '0'
                #Error 'Unpaired While'
        #End_If
    #End_If

    &&2= '0' | &&3=Pos

    W&&2: cmp #1 #3 | jn#2 W&&2>>]

[.End_While
    #If &&2<> '0'
        #ErrorPos  &&3 'Unpaired While'
    #End_If
    jmp W&&2<< | w&&2:
    &&2=0]

[While
    #If &&2=0
        &&2= '0'
    #Else
        &&2=&&2+1
    #End_If
    W&&2: cmp #1 #3 | jn#2 W&&2>>]

[End_While | jmp W&&2<< | w&&2:
             #If &&2= '0'
                &&2=0
             #Else
                &&2=&&2-1
             #End_If]
____________________________________________________________________________________________

[.Do
    #If &&4<>0
        #If &&4<> '0'
                #Error 'Unpaired Do'
        #End_If
    #End_If

    &&4= '0' | &&5=Pos

    D&&4: ]

[.Loop_Until
    #If &&4<> '0'
        #ErrorPos  &&5 'Unpaired Do Until'
    #End_If

    cmp #1 #3 | jn#2 D&&4<<
    &&4=0]

[.Loop_While
    #If &&4<> '0'
        #ErrorPos  &&5 'Unpaired Do While'
    #End_If

    cmp #1 #3 | j#2 D&&4<<
    &&4=0]

[Do
    #If &&4= 0
        &&4= '0'
    #Else
        &&4=&&4+1
    #End_If

    D&&4: ]

[Loop_Until | cmp #1 #3 | jn#2 D&&4<<  | D&&4:
 #If &&4= '0'
    &&4=0
 #Else
    &&4=&&4-1
 #End_If]

[Loop_While | cmp #1 #3 | j#2 D&&4<<  | D&&4:
 #If &&4= '0'
    &&4=0
 #Else
    &&4=&&4-1
 #End_If]
 
[Do_Loop | jmp D&&4<<]
____________________________________________________________________________________________

[.For
    #If &&6<>0
        #If &&6<> '0'
            #Error 'Unpaired For'
        #End_If
    #End_If
    &&6= '0' | &&7=Pos

    #If #3=imm
        mov #1 (#3-1)
    #Else
        mov #1 #3
        dec #1
    #EndIf
 F&&6:
    inc #1 | cmp #1 #5 | ja F&&6>> ]

[.Next | jmp F&&6<<
 F&&6:
    #If &&6<> '0'
        #ErrorPos &&7 'Unpaired For'
    #End_If
    &&6=0]

[For
    #If &&6=0
        &&6= '0'
    #Else
        &&6=&&6+1
    #EndIf

    #If #3=imm
        mov #1 (#3-1)
    #Else
        mov #1 #3
        dec #1
    #EndIf
 F&&6:
    inc #1 | cmp #1 #5 | ja F&&6>> ]


[Next | jmp F&&6<< | F&&6: &&6=&&6-1]

[Break | jmp F&&6>>]
[Continue | jmp F&&6<<]
[ExitF | jmp F0>>]
____________________________________________________________________________________________

;;
  &1 = Size of arguments 
  &2 = Size of local data (local+structures) 
  &3 = preserved regs 
;;

[Proc | &1=0 | &2=0 | &3= | #1 | push ebp | mov ebp esp]

[Arguments | {#1 ebp+((#x*4)+4)} | #+1 | &1=(#N*4)]

[Local | {#1 ebp-(#x*4)} | #+1 | &2=(#N*4) | sub esp &2]

[GetMember | {#3 ebp-(#F-#2)} | #+2]

[Structure | {#1 ebp-(&2+#2+4)} | sub esp #2 | push esp | GetMember &2+#2 #L>3 | &2=&2+#2+4]

[Uses | push #1>L | &3=pop #L>1]

[Return | #If #N=1 | mov eax #1 | #EndIf | jmp P9>>]

[ExitP | jmp P9>>]

[EndP | P9: | &3 | mov esp ebp | pop ebp | ret &1]

[.EndP | P9: | &3 | mov esp ebp | pop ebp | ret &1]
_____________________________________________________________________________________________

; little message routines for values tests (dWords only / text pointer) to be called with:
; > Hexprint D$esi / showme esi+5, for exemple:

[InfoTitle: 'Application Base', 0]

[HexprintString: B$ '        h' 0
 MessageTitle:      'HihoHiho' 0]


Proc HexPrnt:
    Arguments @N

    pushad
        mov ebx D@N, ecx 8, edi HexPrintString | add edi 7
        std
            Do
                mov al bl | and al 0F | add al '0'
                On al > '9', add al 7
                stosb | shr ebx 4
            Do_Loop
        cld
        call 'User32.MessageBoxA'  0  HexPrintString  MessageTitle  &MB_OK__&MB_SYSTEMMODAL
    popad
EndP

[Hexprint | call Hexprnt #1 | #+1]


Proc ShowYou:
    Arguments @Pointer

    pushad
        call 'MessageBoxA'  &NULL  D@Pointer  MessageTitle  &MB_SYSTEMMODAL__&MB_OK
    popad
EndP

[Showme | push eax | lea eax D$#1 | call ShowYou eax | pop eax]

_______________________________________________________________________________________

____________________________________________________________________________________________

Comment1:
;;
  Many Compilers encode the DLLs calls under the form of a CALL to JUMPs Table. This is
  to say that each call to a DLL Function is encoded with two instructions: a CALL plus
  a JMP.
  
  RosAsm performs these calls in one single Instruction (a direct call to the .Import
  Section record).
  
  Unfortunately, the direct RosAsm form is one Byte longer than the indirect call to a
  Jumps Table (the Instruction Opcode for 'call D$Location' is two Bytes, whereas the
  one for 'call Label' is one Byte).
  
  It may happend that, when a short JMP jumps over one or more DLLs calls, the shorter
  form found in the Disassembly becomes out of range because of these added Bytes.
  
  In such cases, the Disassembler replaces, for example, 'je K5>', by 'je K5>>'.
  
  All occurences of '>>' and '<<' found in a disassembly Source fall under that case.
;;
",

DisTitle: B$ CR, LF ; ('1' is at DisTitle+12).
"TITLE Part01 

_____________________________________________________________________________________________

", 0]


WriteMacros:
    mov esi DisMacros2 | While B$esi <> 0 | movsb | End_While
ret
____________________________________________________________________________________________

[EvocatedOnly: ?]

RemoveNonAccessedEvocatedData:
    mov esi D$RoutingMap, edx D$EndOfRoutingMap | add esi D$FirstSection
    mov edi D$SectionsMap | add edi D$FirstSection
mov D$EvocatedOnly 0
    While esi < edx
        If B$esi = LABEL
          ; When here, this '0' means that it is going to be considered Data:
          ;  On B$edi = 0, mov B$esi 0
        Else_If B$esi = EVOCATED
          ;  On B$edi = 0, mov B$esi 0
          inc D$EvocatedOnly
        End_If

        inc esi | inc edi
    End_While

    hexprint D$EvocatedOnly
ret
____________________________________________________________________________________________

[DisReadyForTable: ?    DisCloseBracket: ?    DisVirtual: ?]

[PeWithoutData: B$ "
; Weird PE without .Data Section inside !!!" PeWithoutDataLen: Len
 AndVirtualIsRunable: "
; And a Virtual Section is runable!!!

" AndVirtualIsRunableLen: Len]




[TargetSize: ?]     ; May be BYTE, WORD, DWORD, FP4, FP8, FP10, POINTER.




; check if a Data set may be represented by [Data: 0 #XXX]
; (B$ / W$ / D$ // 0 or other):




[FirstDisDataByte: ?]

WriteBytesData:
    push ebx, ecx
        mov edx 0, D$edi 'B$  ' | add edi 3

L5:     movzx eax B$ebx | push ebx | call WriteEax | pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 16
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', B$edi+12 ' '
                add edi 13 | mov edx 0
            End_If
        .End_If
        inc ebx | loop L5<
        mov B$edi-1 ']'
    pop ecx, ebx
ret


WriteWordsData:
    push ebx, ecx
        shr ecx 1

        NextDisLine
        mov D$edi '    ', D$edi+4 '    '
        add edi 7 | mov D$edi '    ' | add edi 3
        mov edx 0, D$edi 'W$  ' | add edi 3

L5:     movzx eax W$ebx | push ebx | call WriteEax | pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 11
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | mov D$edi '    ' | add edi 3 | mov edx 0
            End_If
        .End_If
        add ebx 2 | On ebx < D$UserPeEnd, loop L5<
        mov B$edi-1 ']'
    pop ecx, ebx
ret


WriteUnicodeData:
    push ebx, ecx
        shr ecx 1

        NextDisLine
        mov D$edi '    ', D$edi+4 '    '
        add edi 7 | mov D$edi '    ' | add edi 3
        mov edx 0, D$edi 'U$  ' | add edi 3

L5:     movzx eax W$ebx | push ebx | call WriteEax | pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 11
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | mov D$edi '    ' | add edi 3 | mov edx 0
            End_If
        .End_If
        add ebx 2 | loop L5<
        mov B$edi-1 ']'
    pop ecx, ebx
ret


L9: pop ecx, ebx
ret

WriteCommentedWordsData:
    push ebx, ecx
        shr ecx 1 | jecxz L9<

        NextDisLine
        mov D$edi '    ', D$edi+4 '    '
        add edi 7 | mov D$edi ';   ' | add edi 3
        mov edx 0, D$edi 'W$  ' | add edi 3

L5:     movzx eax W$ebx | push ebx | call WriteEax | pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 11
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | mov D$edi ';   ' | add edi 3 | mov edx 0
            End_If
        .End_If
        add ebx 2 | loop L5<
        mov B$edi-1 ']'
L9: pop ecx, ebx
ret


WritedWordPointers:
    push ebx, ecx
        shr ecx 2 | mov edx 0

L0:     mov eax D$ebx

        push ebx
            mov ebx eax | sub ebx D$DisImageBase | add ebx D$RoutingMap
            cmp ebx D$RoutingMap | jb L2>>
                cmp ebx D$EndOfRoutingMap | jae L2>>

                    push ebx
                        sub ebx D$RoutingMap | add ebx D$SectionsMap
                        mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                    pop ebx

                    test B$ebx LABEL | jnz L2>>
                    mov cl B$ebx | and cl EVOCATED+INSTRUCTION
                    cmp cl EVOCATED+INSTRUCTION | je L2>>
                    If cl = INSTRUCTION
                        or B$ebx EVOCATED | jmp L2>>
                    End_If
                        mov ecx 0
L1:                     dec ebx
                        .If ebx <= D$RoutingMap
                            call EraseSectionName
                            add eax ecx | jmp L2>>
                        .Else_If ecx > 8
                            call EraseSectionName
                            add eax ecx | jmp L2>>
                        .End_If
                        dec eax | inc ecx
                        test B$ebx LABEL | jz L1<
                            sub ebx D$RoutingMap | add ebx D$SectionsMap
                            push edx
                                mov dl B$ebx, dh B$ebx+ecx
                                .If dl <> dh
                                    call EraseSectionName
                                    If dl = CODEFLAG
                                        sub ebx D$SectionsMap | add ebx D$RoutingMap
                                        test B$ebx INSTRUCTION | jz L1<<
                                        mov D$edi 'Code' | add edi 4
                                    Else_If dl = DATAFLAG
                                        mov D$edi 'Data' | add edi 4
                                    Else_If dl = VIRTUALFLAG
                                        mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                                    End_If
                                .End_If
                            pop edx
                            call WriteEax | mov B$edi '+' | inc edi
                            mov eax ecx

L2:                 call WriteEax
L3:     pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 6
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 ;| mov D$edi ';   ' | add edi 3 |
                mov edx 0
            End_If
        .End_If
        add ebx 4 | dec ecx | cmp ecx 0 | ja L0<<
        mov B$edi-1 ']'
    pop ecx, ebx
ret


EraseSectionName:
    If D$edi-4 = 'tual'
        sub edi 7
    Else_If D$edi-4 = 'Code'
        sub edi 4
    Else_If D$edi-4 = 'Data'
        sub edi 4
    End_If
ret


WritedWordsData:
    push ebx, ecx
        shr ecx 2

L0:     mov edx 0, D$edi 'D$  ' | add edi 3

L0:     mov eax D$ebx
        push ebx
            mov ebx eax | sub ebx D$DisImageBase | add ebx D$RoutingMap
            cmp ebx D$RoutingMap | jb L2>>
                cmp ebx D$EndOfRoutingMap | jae L2>>
                    test B$ebx NODE | jz L1>
                        sub ebx D$RoutingMap | add ebx D$SectionsMap

                        mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        jmp L2>

L1:                 test B$ebx Evocated | jz L2>
                        push ebx, eax
                            call WriteEax | mov D$edi '  ; ' | add edi 4
                        pop eax, ebx
                        sub ebx D$RoutingMap | add ebx D$SectionsMap
                        mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        call WriteEax
                        mov D$edi '??? ' | add edi 3 | mov W$edi CRLF | add edi 2
                        mov edx 6 | jmp L3>


L2:                 call WriteEax
L3:     pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 7
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | mov D$edi '    ' | add edi 3 | mov edx 0
            End_If
        .End_If
        add ebx 4 | dec ecx | cmp ecx 0 | ja L0<<
        mov B$edi-1 ']'
    pop ecx, ebx
ret


L9: pop ecx, ebx
ret

WritedCommentedWordsData:
    push ebx, ecx
        shr ecx 2 | jecxz L9<

        NextDisLine
        mov D$edi '    ', D$edi+4 '    '
        add edi 7 | mov D$edi ';   ' | add edi 3
L0:     mov edx 0, D$edi 'D$  ' | add edi 3

L0:     mov eax D$ebx
        push ebx
            mov ebx eax | sub ebx D$DisImageBase | add ebx D$RoutingMap
            cmp ebx D$RoutingMap | jb L2>>
                cmp ebx D$EndOfRoutingMap | jae L2>>
                    test B$ebx NODE | jz L1>
                        sub ebx D$RoutingMap | add ebx D$SectionsMap

                        mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        jmp L2>

L1:                 test B$ebx Evocated | jz L2>
                        push ebx, eax
                            call WriteEax | mov D$edi '  ; ' | add edi 4
                        pop eax, ebx
                        sub ebx D$RoutingMap | add ebx D$SectionsMap
                        mov bl B$ebx | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG
                        If bl = CODEFLAG
                            mov D$edi 'Code' | add edi 4
                        Else_If bl = DATAFLAG
                            mov D$edi 'Data' | add edi 4
                        Else_If bl = VIRTUALFLAG
                            mov D$edi 'Virt', D$edi+4 'ual' | add edi 7
                        End_If
                        call WriteEax
                        mov D$edi '??? ' | add edi 3 | mov edx 6 | jmp L3>


L2:                 call WriteEax
L3:     pop ebx
        mov B$edi ' ' | inc edi | inc edx
        .If edx = 7
            If ecx > 1
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
                add edi 10 | mov D$edi ';   ' | add edi 3 | mov edx 0
            End_If
        .End_If
        add ebx 4 | dec ecx | cmp ecx 0 | ja L0<<
        mov B$edi-1 ']'
L9: pop ecx, ebx
ret


[ControlFloat: ? ? ? ?]



; ecx = how many Bytes
[ST0trash: T$ ?]
[SourceOfSpecialFPU: ?  SourceOfSpecialFPUSize: ?]

WriteFP4:
    mov D$SourceOfSpecialFPU ebx, D$SourceOfSpecialFPUSize 4
    finit
    fld F$ebx | add ebx 4 | mov D$edi 'F$  ' | jmp L0>
WriteFP8:
    mov D$SourceOfSpecialFPU ebx, D$SourceOfSpecialFPUSize 8
    finit
    fld R$ebx | add ebx 8 | mov D$edi 'R$  ' | jmp L0>
WriteFP10:
    mov D$SourceOfSpecialFPU ebx, D$SourceOfSpecialFPUSize 10
    finit
    fld T$ebx | add ebx 10 | mov D$edi 'T$  '

L0: add edi 3
    pushad
        push edi
            mov eax 0, ecx 10 | rep stosd
        pop edi

        fstp T$ST0trash | call DisassemblerFloatToUString ST0trash edi
    popad

    While B$edi > 0 | inc edi | End_While

    .If B$SpecialFPU = &TRUE
        push ecx
            While B$edi <> '$' | dec edi | End_While | dec edi
            mov D$edi 'B$  ' | add edi 3
            mov ecx D$SourceOfSpecialFPUSize, ebx D$SourceOfSpecialFPU

L0:         mov al B$ebx
            mov B$edi '0' | inc edi
            If al = 0
              ; Done above
            Else_If al > 0F
                shr al 4 | and eax 0F | mov al B$HexaTable+eax | stosb
                mov al B$ebx
                and eax 0F | mov al B$HexaTable+eax | stosb
            Else
                and eax 0F | mov al B$HexaTable+eax | stosb
            End_If

            mov W$edi ', ' | add edi 2

            inc ebx | loop L0<

            sub edi 2
        pop ecx

    .End_If
ret


[FpModeTroncated 00_00001100_00000000
 FpModeNearest   00_00000000_00000000
 FpModeDown      00_00000100_00000000
 FpModeUp        00_00001000_00000000]

Proc FpuRounding:
    Argument @Mode
    local @ControlWord
        fstcw W@ControlWord
        and W@ControlWord 00_11110011_11111111
        mov eax D@Mode | or W@ControlWord ax
        fldcw W@ControlWord
EndP


Proc qWordToAscii:
    Arguments @qWordPointer, @StringPointer
    local @Divisor, @Remainder

        call FpuRounding FpModeTroncated

        push 0-1 ; End mark on the Stack.

        mov D@Divisor 10 | fild D@Divisor
        mov ebx D@qWordPointer
        fild Q$ebx
L0:     fprem | fistp D@Remainder | push D@Remainder
        fild Q$ebx | fdiv
        fld ST0 | fistp Q$ebx
        mov eax D$ebx | or eax D$ebx+4 | cmp eax 0 | jne L0<

        mov edi D@StringPointer
L0:     pop eax | cmp eax 0-1 | je L7>
            add al '0' | stosb | jmp L0<
L7:     mov al 0 | stosb
EndP
____________________________________________________________________________________________

NextDataDisLine:
    NextDisLine
    mov D$edi '    ', D$edi+4 '    ', D$edi+8 '  ; ' | add edi 12
ret
____________________________________________________________________________________________

[TruthAsciiTable: ?]
[BADASCII 2    GOODASCII 1]

BuildTruthAsciiTable:
    VirtualAlloc TruthAsciiTable 256
    mov edi D$TruthAsciiTable, al BADASCII, ecx 256 | rep stosb

    mov edi D$TruthAsciiTable, B$edi 0
    mov B$edi+LF GOODASCII, B$edi+CR GOODASCII,
        B$edi+0A7 GOODASCII,    ; $
        B$edi+025 GOODASCII,    ; %
        B$edi+0A9 GOODASCII,    ; ®
        B$edi+02F GOODASCII     ; /

    mov ebx 32, ecx 127
    While ebx < ecx
        mov B$edi+ebx GOODASCII | inc ebx
    End_While
ret


[GoodAsciiSum: ?    BadAsciiSum: ?    ZeroAsciiSum: ?]

;;
 Ascii recognition: The 3 upper Variables are used to store the number of 'good', 'bad'
 and 'zeroed' Bytes. When several zero-Bytes are consecutive, they count for 1 zero
 Bytes (possible cases of Strings Alignements). The truth Table is build by
 'BuildTruthAsciiTable' (called from 'DisInitialise'). As the intermediate Zeros are reduced
 to 1 when consecutive, the final number of zeros in 'D$ZeroAsciiSum' tells the possible
 Number of Strings in the parsed Chunk. Dividing the overall Number of 'D$GoodAsciiSum'
 by the number gives the average length of string(s). If too small, interpretation is
 statistically impossible. I set the limit, here to '< 4', what seems to me very risky.
 5 good Ascii Chars on 5 Bytes is only around 1 chance on 2*2*2*2*2 (32). Much too low
 to consider it 100% ensured. I must add a MAYBE Flag, for cases between 32 and ... ???
 The MAYBE case, would allow arasing the simple Bytes interpretation (same > no hurt),
 but should not forbid Words and dWords interpretations.
;;

CheckAsciiData:
    push ebx, ecx, edx, edi

        mov edx D$TruthAsciiTable, eax 0
        mov D$GoodAsciiSum 0, D$BadAsciiSum 0, D$ZeroAsciiSum 0
        mov B$ItWasReallyAscii &FALSE

L0:     mov al B$ebx | inc ebx
        If al = 0
            While B$ebx = 0
                inc ebx | cmp ebx D$UserPeEnd | jae L1>
                dec ecx | jz L1>
            End_While
            inc D$ZeroAsciiSum      ; Counts how many separated Strings, in fact;
        End_If

      ; 'GOODASCII' is 1, 'BADASCII' is 2. So:
        mov al B$edx+eax | shr al 1 | adc D$GoodAsciiSum 0 | add D$BadAsciiSum eax | loop L0<

      ; Extend the Ascii Recognition if Bytes coming after the parsed ones are also Ascii:
        mov al B$ebx, al B$edx+eax
        ..If al = GOODASCII
            inc D$GoodAsciiSum | inc ebx | mov al B$ebx, al B$edx+eax
            .If al = GOODASCII
                inc D$GoodAsciiSum | inc ebx | mov al B$ebx, al B$edx+eax
                If al = GOODASCII
                    inc D$GoodAsciiSum
                End_If
            .End_If
        ..End_If

L1:     .If D$ZeroAsciiSum > 1
            If D$GoodAsciiSum < 10
                mov edx 0, eax D$GoodAsciiSum, ecx D$ZeroAsciiSum | div ecx
                mov D$GoodAsciiSum eax
            End_If
        .End_If

        If D$BadAsciiSum = 0
            On D$GoodAsciiSum > 3, mov B$ItWasReallyAscii &TRUE
        End_If

    pop edi, edx, ecx, ebx
ret


[ItWasReallyUnicode: ?    NumberOfStringChars: ?]

CheckUnicodeData:
    push ebx, ecx, edx, edi

        mov edx D$TruthAsciiTable, eax 0
        mov D$GoodAsciiSum 0, D$BadAsciiSum 0, D$ZeroAsciiSum 0
        mov B$ItWasReallyAscii &FALSE

L0:     mov al B$ebx, al B$edx+eax | cmp al BADASCII | je L2>
        If B$ebx+1 = 0
            add ebx 2 | dec ecx | jz L2>
                        dec ecx | jz L2>
                            jmp L0<
        End_If

L2:     jecxz L3>

        While W$ebx = 0
            add ebx 2
            dec ecx | jz L3>
            dec ecx | jz L3>
        End_While

L3:     mov D$NumberOfStringChars ecx

        If ecx = 0
            mov B$ItWasReallyUnicode &TRUE
        Else
            mov B$ItWasReallyUnicode &FALSE
        End_If

    pop edi, edx, ecx, ebx
ret
____________________________________________________________________________________________

[SectionsMapForStrings: ?   EndOfSectionsMapForStrings: ?]

GetSectionsMapForStrings:
    mov ecx D$EndOfSectionsMap | sub ecx D$SectionsMap
    push ecx
        VirtualAlloc SectionsMapForStrings ecx
    pop ecx
    add ecx D$SectionsMapForStrings | mov D$EndOfSectionsMapForStrings ecx
ret

ReleaseSectionsMapForStrings:
    VirtualFree D$SectionsMapForStrings
ret
____________________________________________________________________________________________
;;
  Winner in matter of bad Strings Recognition:
  
  [Data0453031: B$ "=.midtd=.aift]=.au", 0,
                   "tV=.sndtO=.wmatH=.jpgtA=.bmpt:=.gift3=.pict,=.imgt%=.tift"]
  [<Data045307D: B$ 01E]
  [<Data045307E: B$ "=.tgat"]
  [Data0453084: B$ 017]
  [<Data0453085: B$ "=.pcxt"]
  [<Data045308B: B$ 010]
  [Data045308C: B$ "=.jpgt"]
  [<Data0453092: B$ 09]
  [<Data0453093: B$ "=.jpet"]
  
  ... which is real Code... :)))))))
;;

[Forced: ?]

Proc AsciiRecognition:
    Argument @Length
    Uses ecx

      ; First, for all Ascii Char, write a TEMPOFLAG into the relative SectionsMap Bytes:
        mov esi D$UserPeStart | add esi D$FirstSection
        mov ebx D$SectionsMap | add ebx D$FirstSection
        mov edx D$TruthAsciiTable, eax 0

        sub D$UserPeEnd 3

        While esi < D$UserPeEnd
            test B$ebx IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG+CODEFLAG | jnz L2>
            mov al B$esi

            .If B$edx+eax = GOODASCII
                If B$Forced = &TRUE
                    mov B$ebx TEMPOFLAG
                Else
                    On B$ebx = 0, mov B$ebx TEMPOFLAG
                End_If
            .End_If

L2:         inc esi | inc ebx
        End_While

      ; Append zero-ends:
        mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            .If B$esi = TEMPOFLAG
              ; Consider case of Strings Length >= D@length/2 :
                mov ecx 0
                While B$esi = TEMPOFLAG | inc esi | inc ecx | End_While
                shl ecx 1 | cmp ecx D@Length | jb L2>

                If B$esi = 0
                  ; Write a TEMPOFLAG on the trailing zero, if any:
                    mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                    On B$ebx = 0, mov B$esi TEMPOFLAG
                End_If
            .End_If

L2:         inc esi

        .End_While

      ; Delete too small Strings Chunks:
        mov esi D$SectionsMap | add esi D$FirstSection
        mov edx D$EndOfSectionsMap | sub edx 4

        .While esi < edx
            ..If B$esi = TEMPOFLAG
                mov ecx 0, ebx esi
L0:             While B$esi = TEMPOFLAG
                    inc esi | inc ecx | On esi >= edx, jmp L0>
                End_While

              ; Append any following small Chunk (down to 5 uChars):
                .If B$esi+1 = TEMPOFLAG                 ; 1 Byte in between!
                    If D$esi+2 = FOURTEMPOFLAGS
                        add esi 6 | add ecx 5 | jmp L0<
                    End_If
                .Else_If B$esi+2 = TEMPOFLAG            ; 1 Word in between!
                    If D$esi+3 = FOURTEMPOFLAGS
                        add esi 7 | add ecx 5 | jmp L0<
                    End_If
               ; .Else_If B$esi+3 = TEMPOFLAG            ; 3 Bytes >>> unlikely!
                   ; If D$esi+4 = FOURTEMPOFLAGS
                   ;     add esi 8 | add ecx 5 | jmp L0<
                   ; End_If
              ;  .Else_If B$esi+4 = TEMPOFLAG            ; 1 dWord in between!
                   ; If D$esi+5 = FOURTEMPOFLAGS
                   ;     add esi 8 | add ecx 5 | jmp L0<
                   ; End_If
                .End_If

L0:             If ecx < D@Length
                  ; Too small > arase the TEMPOFLAGs
                    push esi
L0:                     dec esi | mov B$esi 0 | cmp esi ebx | ja L0<
                    pop esi
                End_IF
            ..End_If

            inc esi

        .End_While

      ; Now flag the long enough Chunks:
        mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
                mov ebx esi
                While B$esi = TEMPOFLAG
                    inc esi | On esi >= D$EndOfSectionsMap, jmp L0>
                End_While

L0:             sub ebx D$SectionsMap | add ebx D$RoutingMap | or B$ebx EVOCATED
                sub ebx D$RoutingMap | add ebx D$SizesMap | mov B$ebx STRINGS+BYTE
              ; and include trailing zeros in Data:
                mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                On ebx >= D$UserPeEnd, jmp L2>
                If W$ebx = 0
                    mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax+2 EVOCATED
                    While B$ebx+1 = 0
                        mov B$esi TEMPOFLAG | inc esi | inc ebx
                        On ebx >= D$UserPeEnd, jmp L2>
                    End_While
                Else
                    mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax EVOCATED
                End_If

            ..End_If

            inc esi
        .End_While

L2:     call ReplaceTempoFlagBy DATAFLAG, STRINGS+BYTE | add D$UserPeEnd 3
EndP
____________________________________________________________________________________________

Proc UnicodeRecognition:
    Argument @Length
    Uses ecx

      ; First, for all Ascii Char, write a TEMPOFLAG into the relative SectionsMap Bytes:
        mov esi D$UserPeStart | add esi D$FirstSection
        mov ebx D$SectionsMap | add ebx D$FirstSection
        mov edx D$TruthAsciiTable, eax 0

        sub D$UserPeEnd 3

        While esi < D$UserPeEnd
            test B$ebx IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG+CODEFLAG | jnz L2>
            mov al B$esi

            ..If B$edx+eax = GOODASCII
              ; If No Section Flag yet:
                On B$Forced = &TRUE, jmp L1>
                .If W$ebx = 0
L1:                 If B$esi+1 = 0
                        mov B$ebx TEMPOFLAG, B$ebx+1 TEMPOFLAG
                        inc esi | inc ebx
                    End_If
                .End_If
            ..End_If

L2:         inc esi | inc ebx
        End_While

      ; Append zero-ends:
        mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
              ; Consider case of Strings Length >= D@length/2 :
                mov ecx 0
                While B$esi = TEMPOFLAG | inc esi | inc ecx | End_While
                cmp ecx D@Length | jb L2>

                If W$esi = 0
                  ; Write a TEMPOFLAG on the trailing zero, if any:
                    mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                    On W$ebx = 0, mov W$esi TWOTEMPOFLAGS
                End_If
            ..End_If

L2:         inc esi

        .End_While

      ; Delete too small Strings Chunks:
        mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
                mov ecx 0, ebx esi
L0:             While B$esi = TEMPOFLAG
                    inc esi | inc ecx | On esi = D$EndOfSectionsMap, jmp L0>
                End_While

              ; Append any following small Chunk (down to 5 uChars):
                .If B$esi+1 = TEMPOFLAG                 ; 1 Byte in between!
                    If D$esi+2 = FOURTEMPOFLAGS
                        add esi 6 | add ecx 5 | jmp L0<
                    End_If
                .Else_If B$esi+2 = TEMPOFLAG            ; 1 Word in between!
                    If D$esi+3 = FOURTEMPOFLAGS
                        add esi 7 | add ecx 5 | jmp L0<
                    End_If
                .Else_If B$esi+3 = TEMPOFLAG            ; 3 Bytes >>> unlikely!
                   ; If D$esi+4 = FOURTEMPOFLAGS
                   ;     add esi 8 | add ecx 5 | jmp L0<
                   ; End_If
                .Else_If B$esi+4 = TEMPOFLAG            ; 1 dWord in between!
                    If D$esi+5 = FOURTEMPOFLAGS
                        add esi 8 | add ecx 5 | jmp L0<
                    End_If
                .End_If

L0:             shr ecx 1

                If ecx < D@Length
                  ; Too small > arase the TEMPOFLAGs
                    push esi
L0:                     dec esi | mov B$esi 0 | cmp esi ebx | ja L0<
                    pop esi
                End_IF
            ..End_If

            inc esi

        .End_While

      ; Now flag the long enough Chunks:
        mov esi D$SectionsMap | add esi D$FirstSection

        .While esi < D$EndOfSectionsMap
            ..If B$esi = TEMPOFLAG
                mov ebx esi
                While B$esi = TEMPOFLAG
                    inc esi | On esi = D$EndOfSectionsMap, jmp L0>
                End_While

L0:             sub ebx D$SectionsMap | add ebx D$RoutingMap | or B$ebx EVOCATED
                sub ebx D$RoutingMap | add ebx D$SizesMap | mov B$ebx STRINGS+WORD
              ; and include trailing zeros in Data:
                mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart
                On ebx >= D$UserPeEnd, jmp L2>
                If W$ebx = 0
                    mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax+2 EVOCATED
                    While W$ebx+2 = 0
                        mov B$esi TEMPOFLAG | inc esi | inc ebx
                        mov B$esi TEMPOFLAG | inc esi | inc ebx
                        On ebx >= D$UserPeEnd, jmp L2>
                    End_While
                Else
                    mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax EVOCATED
                End_If

            ..End_If

            inc esi
        .End_While

L2:     call ReplaceTempoFlagBy DATAFLAG, STRINGS+WORD | add D$UserPeEnd 3
EndP
____________________________________________________________________________________________

Proc ReplaceTempoFlagBy:
    Argument @SectionFlag, @FLAG

      ; replace, for example, all TEMPOFLAGs by DATAFLAGs:
L2:     mov esi D$SectionsMap | add esi D$FirstSection
        mov eax D@SectionFlag

        mov ebx esi | sub ebx D$SectionsMap | add ebx D$SizesMap

        mov ecx D@FLAG

        While esi < D$EndOfSectionsMap
            If B$esi = TEMPOFLAG
                mov B$esi al
                or B$ebx cl
            End_If

            inc esi, ebx
        End_While
EndP

;;
  Small Strings before and after identified Strings are much likely also Strings.
  Not if this is inside Code that could as well be read as String.
;;
ExtendStrings: ret ; 'AsciiRecognition'
    push ebp

    mov esi D$SizesMap | add esi D$FirstSection
    mov edx D$EndOfSizesMap, eax 0, ebp D$TruthAsciiTable

    mov ebx esi | sub ebx D$SizesMap | add ebx D$SectionsMap
    mov edx esi | sub edx D$SizesMap | add edx D$RoutingMap
    mov edi esi | sub edi D$SizesMap | add edi D$UserPeStart

; esi > SizesMap // ebx > SectionsMap // edx > RoutingMap // edi > UserPeStart

    .While esi < edx
        test B$esi STRINGS | jz L5>
          ; What is above? Data or Code?
            .If B$ebx-1 = CODEFLAG
              ; Are we at the end of a Code Chunk?
                If B$edx = CHUNKEND
                  ; Yes > do nothing.
                Else
                    mov ecx 0
L0:                 inc ecx | dec edx | dec ebx
                    On B$ebx-1 <> CODEFLAG, jmp L2>
                        mov al B$edi, al B$ebp+eax | On al = BADASCII, jmp L5>
                        test B$edx CHUNKEND | jz L0<

L2:
                End_If

            .Else_If B$ebx-1 = DATAFLAG

            .End_If


L5:     inc esi | inc ebx | inc edi
    .End_While

    pop ebp
ret


FillStringsSizes:
    mov esi D$SizesMap | add esi D$FirstSection
    mov ecx D$EndOfSizesMap

    mov ebx esi | sub ebx D$SizesMap | add ebx D$UserPeStart

    mov edx D$TruthAsciiTable, eax 0

    .While esi < ecx
        test B$esi STRINGS | jz L2>
            If B$esi = STRINGS+BYTE
                mov al B$ebx
                While D$edx+eax = GOODASCII
                    or B$esi STRINGS
                    inc esi, ebx
                End_While

                On B$esi = 0, or B$esi STRINGS

            Else_If B$esi = STRINGS+WORD
                mov al B$ebx
                While D$edx+eax = GOODASCII
                    cmp B$esi+1 0 | jne L2>
                    or W$esi ((STRINGS shl 8)+STRINGS)
                    inc esi, ebx
                    inc esi, ebx
                End_While

                On W$esi = 0, or W$esi ((STRINGS shl 8)+STRINGS)
            End_If

L2:     inc esi, ebx
    .End_While
ret
____________________________________________________________________________________________

;;
  Repetitive Bytes may be real Instructions. Example:
  
  05, 05, 05, 05, 05   ; add eax 05050505
  C7,80, BC,00,00,00, 00,00,00,00  ; mov D$eax+0BC 0
;;

MarkRepetitiveDataBytes:
    mov esi D$UserPeStart | add esi D$FirstSection
    mov edx D$UserPeEnd | sub edx 8

    mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

    .While esi < edx
L0:     cmp D$ebx 0 | jne L8>
        cmp D$ebx+4 0 | jne L8>

            mov al B$esi | cmp al 090 | je L8> ; Op90 nop
                           cmp al 0CC | je L8> ; OpCC int3
                           cmp al 0A4 | je L8> ; OpA4 movsb
                           cmp al 0A5 | je L8> ; OpA5 movsd
                           cmp al 0C3 | je L8> ; OpC3 ret

            cmp B$esi+1 al | jne L8>
            cmp B$esi+2 al | jne L8>
            cmp B$esi+3 al | jne L8>
            cmp B$esi+4 al | jne L8>
            cmp B$esi+5 al | jne L8>
            cmp B$esi+6 al | jne L8>
            cmp B$esi+7 al | jne L8>

              ; Case of dummy "Push/pop eax" for all Registers >>> 9 cases:
                cmp al 050 | je L1>     ; Op50 push eax
                cmp al 058 | jne L2>    ; Op58 pop eax
L1:                 cmp B$esi+8 al | jne L8>

L2:             While B$esi = al
                    mov B$ebx DATAFLAG
                    inc esi | inc ebx | On esi = edx, ret
                    cmp B$ebx 0 | jne L8>
                End_While

                jmp L0<

L8:     inc esi | inc ebx
    .End_While
ret


MarkVeryRepetitiveDataBytes:
    mov esi D$UserPeStart | add esi D$FirstSection
    mov edx D$UserPeEnd | sub edx 15

    mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

    .While esi < edx
L0:     cmp D$ebx 0 | jne L2>
        cmp D$ebx+4 0 | jne L2>
        cmp D$ebx+8 0 | jne L2>

            movzx eax B$esi | cmp al 090 | je L2> ; Op90 nop
                              cmp al 0CC | je L2> ; OpCC int3
                              cmp al 0A4 | je L2> ; OpA4 movsb
                              cmp al 0A5 | je L2> ; OpA5 movsd
                              cmp al 0C3 | je L2> ; OpC3 ret

                mov ecx eax | shl ecx 8 | or ecx eax | shl ecx 16 | or ecx eax
                cmp D$esi ecx | jne L2>
                cmp D$esi+4 ecx | jne L2>
                cmp D$esi+8 ecx | jne L2>

                    While B$esi = al
                        mov B$ebx DATAFLAG
                        inc esi | inc ebx | On esi = edx, ret
                        cmp B$ebx 0 | jne L2>
                    End_While

                    jmp L0<

L2:     inc esi | inc ebx
    .End_While
ret

____________________________________________________________________________________________

MarkEvocatedSizes:
    mov esi D$SizesMap, edx D$EndOfSizesMap | add esi D$FirstSection

    .While esi < edx
        mov al B$esi

        ..If al <> 0
            .If al = POINTER
L1:             mov ebx esi | sub ebx D$SizesMap | add ebx D$UserPeStart
                mov ecx ebx | add ecx 100
                push esi, edx, ebx
                    call IsItCode ebx, ecx, 25
                pop ebx, edx, esi

                sub ebx D$UserPeStart | add ebx D$SectionsMap
                If eax = &TRUE
                    On B$ebx = 0, mov B$ebx CODEFLAG
                Else
                    On D$ebx = 0, mov D$ebx FOURDATAFLAGS
                End_If

            .Else_If al = DWORD
                jmp L1<

            .Else
              ; Bytes, Words, qWords, FP >>> Data:
                If al < FP4     ; BYTE 1, WORD 00_10, ; // ;DWORD  00_100
                    movzx ecx al
                Else_If al = FP4
                    mov ecx 4
                Else_If al = FP8
                    mov ecx 8
                Else_If al = FP10
                    mov ecx 10
                Else
                    jmp L5>
                End_If

                mov ebx esi | sub ebx D$SizesMap | add ebx D$SectionsMap
               ; Force it:
L0:             cmp B$ebx VIRTUALFLAG | je L5>
                    mov B$ebx DATAFLAG | inc ebx | loop L0<

            .End_If
        ..End_If

L5:     inc esi
    .End_While
ret

____________________________________________________________________________________________

[FlowNumberOfPointers: ?]

MarkPointersFlows:
    mov esi D$UserPeStart | add esi D$FirstSection
    mov edx D$UserPeEnd | sub edx 4
    mov D$FlowNumberOfPointers 0

L0:     .While esi < edx
            mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
            test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>

            mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart
            .If eax > D$UserPeStart
                If eax < D$UserPeEnd
                    sub eax D$UserPeStart | add eax D$SectionsMap
                    test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>

                    inc D$FlowNumberOfPointers | add esi 4 | jmp L0<
                End_If
            .End_If

          ; Minimum number of Pointers in the flow: 4
            .If D$FlowNumberOfPointers > 3
                mov ecx D$FlowNumberOfPointers | shl ecx 2 | sub esi ecx
                mov edi esi | sub edi D$UserPeStart | add edi D$SizesMap
                mov ebx edi | sub ebx D$SizesMap | add ebx D$RoutingMap
                push edi
                    mov ecx D$FlowNumberOfPointers

                    mov D$edi POINTER | add edi 4
                    mov D$ebx INDIRECT+LABEL+EVOCATED | add ebx 4
                    dec ecx

L1:                 mov D$edi POINTER | add edi 4
                    mov D$ebx INDIRECT | add ebx 4 | loop L1<
                pop edi

                mov ecx D$FlowNumberOfPointers | shl ecx 2
                sub edi D$SizesMap | add edi D$SectionsMap
                mov al DATAFLAG | rep stosb
              ; Give a chance to downward Code Analyzes:
                sub edi D$SectionsMap | add edi D$RoutingMap | or B$edi EVOCATED

              ; Flag the Locations pointed to by the Pointers Flow:
                mov ecx D$FlowNumberOfPointers
L1:             lodsd | sub eax D$DisImageBase | add eax D$RoutingMap
                or B$eax EVOCATED+LABEL | loop L1<

                mov D$FlowNumberOfPointers 0 | jmp L0<<
            .End_If

L5:         mov ecx 0

            inc esi | mov D$FlowNumberOfPointers 0
        .End_While
ret


[NumberOfAlternatedPointers: ?]

MarkAlternatedPointersFlows:
    mov esi D$UserPeStart | add esi D$FirstSection
    mov edx D$UserPeEnd | sub edx 4
    mov D$NumberOfAlternatedPointers 0

L0: .While esi < edx
        mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
        test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>
;mov eax esi | sub eax D$UserPeStart | add eax D$ImageBase
;On eax = 077ED83D4, int3
        mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart
        .If eax > D$UserPeStart
            If eax < D$UserPeEnd
                sub eax D$UserPeStart | add eax D$SectionsMap
                test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>
                call IsNBytesInstruction 4 | On eax = &TRUE, jmp L5>>
                ;hexprint D$NumberOfAlternatedPointers
                inc D$NumberOfAlternatedPointers | add esi 8 | jmp L0<<
            End_If
        .End_If

      ; Minimum number of Pointers in the flow: 4
        .If D$NumberOfAlternatedPointers > 3
        ;hexprint D$NumberOfAlternatedPointers
            mov ecx D$NumberOfAlternatedPointers | shl ecx 3 | sub esi ecx
            mov edi esi | sub edi D$UserPeStart | add edi D$SizesMap
            mov ebx edi | sub ebx D$SizesMap | add ebx D$RoutingMap
          ; ebx > RoutingMap // edi >> SizesMap
            push edi
                mov ecx D$NumberOfAlternatedPointers

                mov D$edi POINTER | add edi 8
                mov D$ebx INDIRECT+LABEL+EVOCATED | add ebx 8
                dec ecx

L1:             mov D$edi POINTER | add edi 8
                mov D$ebx INDIRECT | add ebx 8 | loop L1<
            pop edi

            mov ecx D$NumberOfAlternatedPointers | shl ecx 3 | sub ecx 4
            sub edi D$SizesMap | add edi D$SectionsMap
            mov al DATAFLAG | rep stosb
          ; Give a chance to downward Code Analyzes:
            sub edi D$SectionsMap | add edi D$RoutingMap | or B$edi EVOCATED

          ; Flag the Locations pointed to by the Pointers Flow:

            mov ecx D$NumberOfAlternatedPointers
L1:         lodsd | add esi 4 | sub eax D$DisImageBase | add eax D$RoutingMap
            or B$eax EVOCATED | loop L1<

            mov D$NumberOfAlternatedPointers 0 | jmp L0<<
        .End_If

L5:     mov D$NumberOfAlternatedPointers 0

        inc esi
    .End_While
ret
____________________________________________________________________________________________

Proc IsNBytesInstruction:
    Argument @N
    Local @Pointer, @End
    Uses esi, edx

      ; @N: Number of Byte before the four bytes Address.
      ; esi: Points to the Four Bytes Address.

      ; Like for 'DecodeOnly':
        mov B$WeAreInTheCodeBox &TRUE, B$SimpleScan &TRUE

        mov D@Pointer esi
      ; D@Pointer points to the first Byte of the Adress Candidate.
        lea eax D$esi+4 | mov D@End eax
      ; D@End points to the next Byte after the Address Candidate.
        sub esi D@N
      ; Esi points to the start Address of an Instruction Candidate, to be now tested.

      ; Disassemble: (DisMain)
L0:     mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
        mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
        mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE

        mov edi DecodeText
L1:     movzx eax B$esi | inc esi | call D$DisOp1+eax*4
        On B$DisFlag = DISDONE, jmp L1<

        .If esi = D@End
            mov eax &TRUE
        .Else_If esi < D@Pointer
            jmp L0<
        ;.Else_If esi = D@Pointer
          ; The Pointer candidate could, itself, also be an Instruction:
        ;    mov eax &TRUE
        .Else
            mov eax &FALSE
        .End_If

        mov B$WeAreInTheCodeBox &FALSE, B$SimpleScan &FALSE
EndP

____________________________________________________________________________________________

;;
  On a big File of 1 Mega, parsing the yet zeroed SectionsMap Bytes may be evaluated
  to parsing 500 Ko. (We execute this Recognition in between the positive and the
  megative Recognitions).
  ___________________________
  Probablity rude evaluations:
  
  * 500,000 on 4,294,967,295 (0_FFFF_FFFF) >>> say, 1 random chance on 2000.
  
    Two consecutive Pointer >>> say, 1 random chance on 4,000,000.
    Three consecutive Pointer >>> say, 1 random chance on 16,000,000,000,000.
    (Four consecutive Pointer and more are already identified erlier, by
    MarkPointersFlows'
  
  * For isolated Pointers (1 random chance on 2000) we also execute:
  
    'IsIsolatedPointer' >>> 'IsNBytesInstruction' that encreases a little bit
    the 1 on 2000 probablity when saying that the Pointer Candidate cannot be
    member of an Instruction (not guaranted at all, but better than nothing).
    
  * For Pointer to identified Code we also ensure that the Pointer does not
    point into the middle of a valid Instruction
;;

[PointerInData: ?]

MarkIsolatedPointers: ; MarkPointersFlows
    mov esi D$UserPeStart | add esi D$FirstSection
    mov edx D$UserPeEnd | sub edx 4
    mov D$FlowNumberOfPointers 0

L0: .While esi < edx
        mov B$PointerInData &FALSE

        mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
        If B$eax = 0
            ;
        Else_If B$eax = DATAFLAG
            mov B$PointerInData &TRUE
        Else
            jmp L5>>
        End_If

        mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart

        .If eax > D$UserPeStart
            If eax < D$UserPeEnd
                sub eax D$UserPeStart | add eax D$SectionsMap
                test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>
                test B$eax+1 IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>
                test B$eax+2 IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>
                test B$eax+3 IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>

                inc D$FlowNumberOfPointers | add esi 4 | jmp L0<<
            End_If
        .End_If

        .If D$FlowNumberOfPointers = 0
            ;
        .Else_If D$FlowNumberOfPointers = 1
            If B$PointerInData = &TRUE
                lea eax D$esi-4 | sub eax D$UserPeStart | add eax D$SizesMap
               ; test B$eax POINTER | jnz L1>
               ; test B$eax DWORD | jnz L1>
                On B$eax = POINTER, jmp L1>
                On B$eax = DWORD, jmp L1>
                mov eax D$esi-4 | sub eax D$DisImageBase | add eax D$RoutingMap
                Test B$eax EVOCATED+LABEL+INDIRECT | jnz L1>

                On B$eax <> 0, jmp L5>>

L1:             mov eax D$esi-4 | sub eax D$DisImageBase | add eax D$SectionsMap
                On B$eax <> CODEFLAG, jmp L1>>
                    sub eax D$SectionsMap | add eax D$RoutingMap
                    test B$eax INSTRUCTION | jnz L1>
            End_If
;;
  There is no logic that would tell if an isolated Pointer Candidate is or not
  a Pointer, by scaning weither or not, it could be some Code.
;;
            jmp L5>>
            ;sub esi 4 | call IsIsolatedPointer | add esi 4

            ;If eax = &TRUE
            ;    mov eax D$esi-4 | sub eax D$DisImageBase | add eax D$SectionsMap
            ;    On D$eax = 0, jmp L1>
            ;End_If

        .Else_If D$FlowNumberOfPointers = 2
            If B$PointerInData = &TRUE
                lea eax D$esi-4 | sub eax D$UserPeStart | add eax D$SizesMap
                test B$eax STRINGS | jnz L5>>

                jmp L1>
            End_If

            ;sub esi 8 | call IsIsolatedPointer | add esi 8
            ;On eax = &TRUE, jmp L1>

        .Else
          ; Restore esi to the first Pointer:
L1:         mov ecx D$FlowNumberOfPointers | shl ecx 2 | sub esi ecx
            mov edi esi | sub edi D$UserPeStart | add edi D$SizesMap
            mov ebx edi | sub ebx D$SizesMap | add ebx D$RoutingMap
            push edi
                mov ecx D$FlowNumberOfPointers
              ; First Pointer of a list:
                mov D$edi POINTER | add edi 4
                mov D$ebx INDIRECT+LABEL+EVOCATED | add ebx 4
                dec ecx | jz L2>
              ; Other Pointers if any:
L1:             mov D$edi POINTER | add edi 4
                and D$ebx 0_FF | or D$ebx INDIRECT | add ebx 4 | loop L1<
L2:         pop edi

            mov ecx D$FlowNumberOfPointers | shl ecx 2
            sub edi D$SizesMap | add edi D$SectionsMap
            mov al DATAFLAG | rep stosb
          ; Give a chance to downward Code Analyzes:
            sub edi D$SectionsMap | add edi D$RoutingMap | or B$edi EVOCATED

          ; Flag the Locations pointed to by the Pointers Flow:
            mov ecx D$FlowNumberOfPointers

L1:         lodsd | sub eax D$DisImageBase | add eax D$RoutingMap
            or B$eax EVOCATED+LABEL | loop L1<

            mov D$FlowNumberOfPointers 0 | jmp L0<<
        .End_If

L5:     inc esi | mov D$FlowNumberOfPointers 0
    .End_While
ret


IsIsolatedPointer:
    push esi, edx
      ; esi: UserPeStart (Four Bytes Pointer Adress)

;If D$esi = 04293E8 ; <<< Value of the Pointer Candidate
;    push esi | sub esi D$UserPeStart | add esi D$DisImageBase
;        On esi = 041F1C0, int3 ; <<< Location
;    pop esi
;End_If

        mov ecx 0, ebx esi
        sub ebx D$UserPeStart | add ebx D$SectionsMap

        While B$ebx-1 = 0
        ; "mov D$eax*4+ecx+01234 Data": 11 Bytes Instruction - 4 = 7 Bytes before Address
            inc ecx | dec ebx
            On ecx = 7, jmp L0>
        End_While

        ..If ecx <> 0
L0:         push ecx
                mov D$LastCodeRef 0 | call IsNBytesInstruction ecx
            pop ecx
            .If eax = &TRUE
                jmp L9>
            .Else
                mov eax D$LastCodeRef
                If eax = D$esi
                    mov eax &TRUE | jmp L9>
                End_If
            .End_If
            dec ecx | jnz L0<
            ;mov D$LastCodeRef 0 | call IsNBytesInstruction 0 | jmp L9>

        ..End_If

        mov eax &FALSE

L9: pop edx, esi

    xor eax &TRUE
ret
____________________________________________________________________________________________

;;
  When all possible Recognitions are done we generalize the found Flags in all
  the coming Sections. Based on the 'DisRvaSectionAlignment'.
;;

ExtendSections:
    mov esi D$SectionsMap, edx D$EndOfSectionsMap | add esi D$FirstSection

  ; Ensure, first that the first Section starts with a valid Flag. If not, extend
  ; by the next coming one:
    If B$esi = 0
        mov edi esi, ecx 0
        While B$esi = 0 | inc esi | inc ecx | End_While
        mov al B$esi | rep stosb
    End_If

  ; Now unicize the Flags depending on the previous one. But, in case the next
  ; Flag is Data and the previous oneis  Code with CHUNKEND, we extend to Data:
    mov esi D$SectionsMap, edx D$EndOfSectionsMap | add esi D$FirstSection
    .While esi < edx
        mov al B$esi
        While B$esi = al
            inc esi | cmp esi edx | je L9>
        End_While
        mov ebx esi
        While B$ebx = 0
            inc ebx | cmp ebx edx | je L1>
        End_While
        .If al = CODEFLAG
            If B$ebx = DATAFLAG
                mov ebx esi | sub ebx D$SectionsMap | add ebx D$RoutingMap
                Test B$ebx CHUNKEND | jz L1>
                    mov al DATAFLAG
            End_If
        .End_If
L1:     While B$esi = 0
            mov B$esi al | inc esi | cmp esi edx | je L9>
        End_While
    .End_While
L9: ret


StripSectionsZeroEnd:
    mov esi D$UserPeStart, edx D$UserPeEnd | add esi D$FirstSection
    add esi D$DisRvaSectionAlignment | dec esi

    .While esi < edx
        push esi

            mov ecx 0
            While B$esi = 0
                mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
                On B$eax <> 0, jmp L1>
                inc ecx | dec esi | On ecx = D$DisRvaSectionAlignment, jmp L1>
            End_While
            inc esi

            .If ecx > 0
                mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

                If B$ebx = DATAFLAG
                    sub ebx D$SectionsMap | add ebx D$RoutingMap
                    inc ebx | On ecx > 4, Align_On 4 ebx
                    mov B$ebx EVOCATED

                Else_If B$ebx = VIRTUALFLAG

                Else_If B$ebx <> KILLFLAG
                    mov edi ebx, al 0 | rep stosb

                End_If

            .End_If

L1:     pop esi

        add esi D$DisRvaSectionAlignment
    .End_While
ret
____________________________________________________________________________________________

[ItWasReallyAscii: ?]

WriteCommentedAsciiData:
    ;call CheckAsciiData |
    On D$BadAsciiSum = ecx, ret

    push ebx, ecx, edi
        NextDisLine

        mov D$NextStringBreak edi | add D$NextStringBreak 70

        .If B$ItWasReallyAscii = &TRUE
            If D$FirstDisDataByte <> 0
                mov edi D$FirstDisDataByte | mov B$edi '"' | inc edi | jmp L0>
            End_If
        .End_If

        mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    '
        add edi 7 | mov D$edi ';  B', D$edi+4 '$ " ' | add edi 7

K0:     mov al B$ebx | inc ebx
            jmp L1>
L0:             loop K0<
                    jmp L9>>

L1:     ...If al = CR
            cmp B$ebx  LF | jne L2>
                 inc ebx | dec ecx | cmp ecx 0 | je L9>>
L1:         If B$ItWasReallyAscii = &TRUE
                On D$FirstDisDataByte = 0, jmp L1>
                NextDisLine
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                add edi 13
            Else
L1:             call NextDataDisLine
            End_If
            jmp L0<

        ...Else_If al = LF
L2:         mov al '.' | inc edx

        ...Else_If al = 0
            inc edx
            ..If D$edi-4 = 'B$ "'
                mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If B$edi-1 = '0'
                If D$edi-4 = 'B$ 0'
                    mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = '" '
                    mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = ', '
                    mov D$edi ', 0 ' | add edi 3
                Else
                    mov D$edi '", 0' | add edi 4
                End_If

            ..Else
                mov D$edi '" 0 ' | add edi 3

            ..End_If

            cmp ecx 1 | je L8>>
            .If B$ebx <> 0
                If B$ItWasReallyAscii = &TRUE
                    On D$FirstDisDataByte = 0, jmp L1>
                    NextDisLine
                    mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                    add edi 13
                Else
L1:                 call NextDataDisLine
                End_If
                mov B$edi '"' | inc edi
            .End_If
            cmp ecx 1 | je L8>>
            jmp L0<<

        ...Else_If al = 9
            mov eax '    ' | stosd

        ...Else_If al = '"'
            mov D$edi '",02', D$edi+4 '2, "' | add edi 8 | jmp L0<<

        ...Else_If al < ' '
L2:         mov al '.' | inc edx


        ...Else_If al > 127
            ; ... ???

        ...End_If

        .If B$edi-1 = '0'
            If W$edi-3 = '" '
                mov D$edi ', " ' | add edi 3
            Else_If W$edi-3 = ', '
                mov D$edi ', " ' | add edi 3
            Else
                ; ????
            End_If
        .End_If

        stosb

        If edi = D$NextStringBreak
            mov W$edi '",', W$edi+2 CRLF | add edi 4
            mov eax '    ' | stosd | stosd | stosd | stosd
            mov W$edi-2 ';"'

            add D$NextStringBreak 88
        End_If

        jmp L0<<

L9:     mov W$edi '" ' | add edi 2
L8:     mov B$edi ']' | inc edi
    pop eax, ecx, ebx
ret


[NextStringBreak: ?    InsideDataText: ?]

Proc WriteAsciiData:
    Argument @Format
    Uses ebx, ecx

        mov D$NextStringBreak edi | add D$NextStringBreak 70

        If D@Format = 1
            mov D$edi 'B$ "'
        Else
            mov D$edi 'U$ "'
        End_If
        add edi 4
        mov B$InsideDataText &TRUE

L0:     mov al B$ebx | add ebx D@Format

L1:     ...If al = CR
            mov D$NextStringBreak edi | add D$NextStringBreak 70
            mov eax D@Format | dec eax
            cmp B$ebx+eax LF | jne L2>
                 add ebx D@Format | sub ecx D@Format | cmp ecx 0 | jle L9>>
L1:         If B$ItWasReallyAscii = &TRUE
                On D$FirstDisDataByte = 0, jmp L1>
             ;   NextDisLine
             ;   mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
             ;   add edi 13
                mov W$edi CRLF | add edi 2
            Else
L1:             call NextDataDisLine
            End_If

            jmp L5>>

        ...Else_If al = LF
L2:         mov al '.' | inc edx

        ...Else_If al = 0
           ; If B$InsideDataText = &TRUE
           ;     mov D$edi '",  ' | add edi 3
           ;     mov B$InsideDataText &FALSE
           ; End_If


L1:         ..If D$edi-4 = 'B$ "'
                mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If D$edi-4 = 'U$ "'
                mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If B$edi-1 = '0'
                If D$edi-4 = 'B$ 0'
                    mov D$edi ', 0 ' | add edi 3
                Else_If D$edi-4 = 'U$ 0'
                    mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = '" '
                    mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = ', '
                    mov D$edi ', 0 ' | add edi 3
                Else
L1:                 mov D$edi '", 0' | add edi 4
                End_If

          ; Case of 'NextDisLine' in the previous pass:
            ..Else_If D$edi-4 = '    '
                On B$InsideDataText = &TRUE, jmp L1<
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 ', 0' | add edi 11

            ..Else
                mov D$edi '" 0 ' | add edi 3

            ..End_If

            mov B$InsideDataText &FALSE

            cmp ecx D@Format | jle L8>>

            ..If B$ebx <> 0
                .If D@Format = 2
                    If B$ItWasReallyUnicode = &TRUE
                      jmp L1>
                       ; NextDisLine
                       ; mov D$NextStringBreak edi | add D$NextStringBreak 70
                    End_If
                .Else_If B$ItWasReallyAscii = &TRUE
L1:                 On D$FirstDisDataByte = 0, jmp L1>
                    NextDisLine
                    mov D$NextStringBreak edi | add D$NextStringBreak 70
                    mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                    add edi 13
                .Else
L1:                 call NextDataDisLine
                    mov D$NextStringBreak edi | add D$NextStringBreak 70
                .End_If
                mov B$edi '"' | inc edi
                mov B$InsideDataText &TRUE
            ..End_If

            jmp L5>>

        ...Else_If al = 9
            mov eax '    ' | stosd | jmp L5>>

        ...Else_If al = '"'  ; 022, 34
            mov D$edi '",02', D$edi+4 '2, "' | add edi 8 | jmp L5>>

        ...Else_If al < ' '
L2:         mov al '.'

        ...Else_If al > 127
            ; ... ???

        ...End_If

        .If B$InsideDataText = &FALSE
            If D$edi-4 = '", 0'
                mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'B$ 0'
                mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'U$ 0'
                mov D$edi ', " ' | add edi 3
            Else_If D$edi-3 = ', 0 '
                mov D$edi ', " ' | add edi 3
            End_If
        .End_If

        stosb

L5:     .If edi > D$NextStringBreak
            If B$InsideDataText = &TRUE
                mov W$edi '",', W$edi+2 CRLF, B$edi+4 '"' | add edi 5
            Else
                NextDisLine
            End_If
           ; mov B$InsideDataText &TRUE
            mov D$NextStringBreak edi |
            add D$NextStringBreak 70 ;88
        .End_If

L5:     sub ecx D@Format | jg L0<<

L9:     mov W$edi '" ' | add edi 2
L8:     mov B$edi ']' | inc edi
EndP

Proc NewWriteAsciiData:
    Argument @Format
    Uses ebx, ecx

        mov D$NextStringBreak edi | add D$NextStringBreak 70

        If D@Format = 1
            mov D$edi 'B$ "'
        Else
            mov D$edi 'U$ "'
        End_If
        add edi 4
        mov B$InsideDataText &TRUE

L0:     mov al B$ebx | add ebx D@Format

L1:     ...If al = CR
            mov D$NextStringBreak edi | add D$NextStringBreak 70
            mov eax D@Format | dec eax
            cmp B$ebx+eax LF | jne L2>
                 add ebx D@Format | sub ecx D@Format | cmp ecx 0 | jle L9>>
L1:         If B$ItWasReallyAscii = &TRUE
                On D$FirstDisDataByte = 0, jmp L1>
             ;   NextDisLine
             ;   mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
             ;   add edi 13
                mov W$edi CRLF | add edi 2
            Else
L1:             call NextDataDisLine
            End_If

            jmp L5>>

        ...Else_If al = LF
L2:         mov al '.' | inc edx

        ...Else_If al = 0
           ; If B$InsideDataText = &TRUE
           ;     mov D$edi '",  ' | add edi 3
           ;     mov B$InsideDataText &FALSE
           ; End_If


L1:         ..If D$edi-4 = 'B$ "'
                mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If D$edi-4 = 'U$ "'
                mov B$edi-1 '0'   ; > 'B$ 0'

            ..Else_If B$edi-1 = '0'
                If D$edi-4 = 'B$ 0'
                    mov D$edi ', 0 ' | add edi 3
                Else_If D$edi-4 = 'U$ 0'
                    mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = '" '
                    mov D$edi ', 0 ' | add edi 3
                Else_If W$edi-3 = ', '
                    mov D$edi ', 0 ' | add edi 3
                Else
L1:                 mov D$edi '", 0' | add edi 4
                End_If

          ; Case of 'NextDisLine' in the previous pass:
            ..Else_If D$edi-4 = '    '
                On B$InsideDataText = &TRUE, jmp L1<
                mov D$edi '    ', D$edi+4 '    ', D$edi+8 ', 0' | add edi 11

            ..Else
                ;mov D$edi '" 0 ' | add edi 3 ; <<<<<<<<<<<<<<<<<<<<<<<<<<<
                mov D$edi '", 0' | add edi 4
                NextDisLine
                ;mov B$edi ' ' | inc edi

            ..End_If

            mov B$InsideDataText &FALSE

            cmp ecx D@Format | jle L8>>

            ..If B$ebx <> 0
                .If D@Format = 2
                    If B$ItWasReallyUnicode = &TRUE
                      jmp L1>
                       ; NextDisLine
                       ; mov D$NextStringBreak edi | add D$NextStringBreak 70
                    End_If
                .Else_If B$ItWasReallyAscii = &TRUE
L1:                 On D$FirstDisDataByte = 0, jmp L1>
                    NextDisLine
                    mov D$NextStringBreak edi | add D$NextStringBreak 70
                    mov D$edi '    ', D$edi+4 '    ', D$edi+8 '    ', D$edi+12 '    '
                    add edi 13
                .Else
L1:                 ;call NextDataDisLine
                    NextDisLine
                    mov D$NextStringBreak edi | add D$NextStringBreak 70
                .End_If
                mov B$edi '"' | inc edi
                mov B$InsideDataText &TRUE
            ..End_If

            jmp L5>>

        ...Else_If al = 9
            mov eax '    ' | stosd | jmp L5>>

        ...Else_If al = '"'  ; 022, 34
            mov D$edi '",02', D$edi+4 '2, "' | add edi 8 | jmp L5>>

        ...Else_If al < ' '
L2:         mov al '.'

        ...Else_If al > 127
            ; ... ???

        ...End_If

        .If B$InsideDataText = &FALSE
            If D$edi-4 = '", 0'
                mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'B$ 0'
                mov D$edi ', " ' | add edi 3
            Else_If D$edi-4 = 'U$ 0'
                mov D$edi ', " ' | add edi 3
            Else_If D$edi-3 = ', 0 '
                mov D$edi ', " ' | add edi 3
            End_If
        .End_If

        stosb

L5:     .If edi > D$NextStringBreak
            If B$InsideDataText = &TRUE
                mov W$edi '",', W$edi+2 CRLF, B$edi+4 '"' | add edi 5
            Else
                NextDisLine
            End_If
           ; mov B$InsideDataText &TRUE
            mov D$NextStringBreak edi |
            add D$NextStringBreak 70 ;88
        .End_If

L5:     sub ecx D@Format | jg L0<<

L9:     mov W$edi '" ' | add edi 2
L8:     mov B$edi ']' | inc edi
EndP


WriteEcxByte:
L0: movzx eax B$esi | inc esi
    mov ebx eax | shr ebx 4
    and eax 0F | and ebx 0F
    mov al B$HexaTable+eax, bl B$HexaTable+ebx
    shl eax 8 | or eax ebx | or eax 020200000 | mov D$edi eax | add edi 3 | loop L0<

ret

WriteImm8:
    movzx ebx B$esi | inc esi | jmp L3>
WriteImm16:
    movzx ebx W$esi | add esi 2 | jmp L3>
WriteImm32:  ; WriteDis32
    On B$OperandSizeOverride = &TRUE, jmp WriteImm16

    lodsd | On B$SimpleScan = &TRUE, jmp WriteEax

    mov ebx eax | sub ebx D$DisImageBase | add ebx D$SectionsMap
    If ebx >= D$EndOfSectionsMap
        ; >>> WriteEax
    Else_If ebx <= D$SectionsMap
        ; >>> WriteEax
    Else
        mov bl B$ebx
        and bl DATAFLAG+VIRTUALFLAG+CODEFLAG | jnz WriteDisRelative
    End_If

WriteEax:
    mov ebx eax

L3: If ebx = 0
        mov B$edi '0' | inc edi | ret
    End_If

    push 0-1

L0: mov eax ebx | shr ebx 4 | and eax 0F

    mov al B$HexaTable+eax
    push eax
    cmp ebx 0 | ja L0<
    mov B$edi '0' | inc edi
L0: pop eax | cmp eax 0-1 | je L9>
    mov B$edi al | inc edi | jmp L0<
L9: ret


WriteSignedImm32:
    mov ebx D$esi | add esi 4 | jmp L0>
WriteSignedImm16:
    movsx ebx W$esi | add esi 2 | jmp L0>
WriteSignedImm8:
    movsx ebx B$esi | inc esi
L0: push 0-1
    test ebx dWordHighbit | jz L0>
        If B$edi-1 = '+'
            mov B$edi-1 '-'
        Else
            mov W$edi '0-' | add edi 2
        End_If
        neg ebx
L0: mov eax ebx | shr ebx 4 | and eax 0F
    mov al B$HexaTable+eax
    push eax
    cmp ebx 0 | ja L0<
    mov B$edi '0' | inc edi
L0: pop eax | cmp eax 0-1 | je L9>
    mov B$edi al | inc edi | jmp L0<
L9: ret

UnlikelyOut:
    push eax
        mov eax D$esi
        sub eax D$DisImageBase | add eax D$SectionsMap
        On eax >= D$EndOfSectionsMap, jmp L8>
        On eax <= D$SectionsMap, jmp L8>
    pop eax
ret

L8: If D$SegmentOverride = 0
      ; In case, for example of 'fs:0', this would be normal
        add B$UnlikelyCode 0F
    End_If
    pop eax | ret

;;
UnlikelyOut:
    push eax
        mov eax D$esi
        sub eax D$DisImageBase | add eax D$SectionsMap
        On eax >= D$EndOfSectionsMap, jmp L8>
        On eax <= D$SectionsMap, jmp L8>
    pop eax
ret

L8: add B$UnlikelyCode 0FF
    pop eax | ret
;;

; Input: bl = Mod (0 / 1 / 2.  - 3 is already done before calling here by a call to
; WriteEregsFromRmBits -)

WriteEffectiveAddressFromModRm:  ; 044 00_100_100    015 00_010_101
    On B$AddressSizeOverride = &TRUE, jmp WriteEffectiveAddressFromModRm16
    ModMask bl To al

    .If al = 0
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax ' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx ' | add edi 4
        Else_If al = 2 | mov D$edi 'edx ' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx ' | add edi 4
        Else_If al = 4 | call WriteFromSib
            ..If cl = 0FF
                call WriteBase5dis32
            ..End_If
        Else_If al = 5
            call UnlikelyOut
            call Writedis32
        Else_If al = 6 | mov D$edi 'esi ' | add edi 4
        Else           | mov D$edi 'edi ' | add edi 4
        End_If

    .Else_If al = 1
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib
            ..If B$edi-1 <> '+'
                mov B$edi '+' | inc edi
            ..End_If
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        call WriteSignedImm8 ;| mov W$edi ax | add edi 2  ; OpToHexa

    .Else_If al = 2
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib
            ..If B$edi-1 <> '+'
                mov B$edi '+' | inc edi
            ..End_If
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        call Writedis32

    .Else ; bl = 3
        If W$DisSizeMarker = 'D$'
            call WriteEregsFromRmBits
        Else_If W$DisSizeMarker = 'B$'
            call WriteByteRegsFromRmBits
        Else_If W$DisSizeMarker = 'W$'
            call WriteWordRegsFromRmBits
        End_If

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret


WriteEffectiveAddressFromModRm16:
    inc B$UnlikelyCode

    ModMask bl To al

    .If al = 0
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'bx+s', D$edi+4 'i ' | add edi 6
        Else_If al = 1 | mov D$edi 'bx+d', D$edi+4 'i ' | add edi 6
        Else_If al = 2 | mov D$edi 'bp+s', D$edi+4 'i ' | add edi 6
        Else_If al = 3 | mov D$edi 'bp+d', D$edi+4 'i ' | add edi 6
        Else_If al = 4 | mov D$edi 'si ' | add edi 3
        Else_If al = 5 | mov D$edi 'di ' | add edi 3
        Else_If al = 6 | call Writedis16
        Else           | mov D$edi 'bx ' | add edi 3
        End_If

    .Else_If al = 1
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'bx+s', D$edi+4 'i ' | add edi 6
        Else_If al = 1 | mov D$edi 'bx+d', D$edi+4 'i ' | add edi 6
        Else_If al = 2 | mov D$edi 'bp+s', D$edi+4 'i ' | add edi 6
        Else_If al = 3 | mov D$edi 'bp+d', D$edi+4 'i ' | add edi 6
        Else_If al = 4 | mov D$edi 'si ' | add edi 3
        Else_If al = 5 | mov D$edi 'di ' | add edi 3
        Else_If al = 6 | mov D$edi 'bp ' | add edi 3
        Else           | mov D$edi 'bx ' | add edi 3
        End_If

        call WriteSignedImm8 ;| mov W$edi ax | add edi 2  ; OpToHexa

    .Else_If al = 2
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'bx+s', D$edi+4 'i ' | add edi 6
        Else_If al = 1 | mov D$edi 'bx+d', D$edi+4 'i ' | add edi 6
        Else_If al = 2 | mov D$edi 'bp+s', D$edi+4 'i ' | add edi 6
        Else_If al = 3 | mov D$edi 'bp+d', D$edi+4 'i ' | add edi 6
        Else_If al = 4 | mov D$edi 'si ' | add edi 3
        Else_If al = 5 | mov D$edi 'di ' | add edi 3
        Else_If al = 6 | mov D$edi 'bp ' | add edi 3
        Else           | mov D$edi 'bx ' | add edi 3
        End_If

        call WriteSignedImm16

    .Else ; bl = 3
        If W$DisSizeMarker = 'D$'
            call WriteEregsFromRmBits
        Else_If W$DisSizeMarker = 'B$'
            call WriteByteRegsFromRmBits
        Else_If W$DisSizeMarker = 'W$'
            call WriteWordRegsFromRmBits
        End_If

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret


WriteEffectiveXMMAddressFromModRm:
    ModMask bl To al

    .If al = 0
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax ' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx ' | add edi 4
        Else_If al = 2 | mov D$edi 'edx ' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx ' | add edi 4
        Else_If al = 4 | call WriteFromSib | On cl = 0FF, call WriteBase5dis32
        Else_If al = 5 | call UnlikelyOut | call Writedis32
        Else_If al = 6 | mov D$edi 'esi ' | add edi 4
        Else           | mov D$edi 'edi ' | add edi 4
        End_If

    .Else_If al = 1
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib | mov B$edi '+' | inc edi
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        call WriteSignedImm8 ;;;OpToHexa | mov W$edi ax | add edi 2

    .Else_If al = 2
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib | mov B$edi '+' | inc edi
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        call Writedis32

    .Else ; bl = 3
        call WriteXMMregsFromRmBits

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret


WriteEffectiveMMXAddressFromModRm: ; 04  00_000_100
    ModMask bl To al

    .If al = 0
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax ' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx ' | add edi 4
        Else_If al = 2 | mov D$edi 'edx ' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx ' | add edi 4
        Else_If al = 4 | call WriteFromSib | On cl = 0FF, call WriteBase5dis32
        Else_If al = 5 | call UnlikelyOut | call Writedis32
        Else_If al = 6 | mov D$edi 'esi ' | add edi 4
        Else           | mov D$edi 'edi ' | add edi 4
        End_If

    .Else_If al = 1
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib | mov B$edi '+' | inc edi
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        call WriteSignedImm8 ;;;OpToHexa | mov W$edi ax | add edi 2

    .Else_If al = 2
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib | mov B$edi '+' | inc edi
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        call Writedis32

    .Else ; bl = 3
        call WriteMMXregsFromRmBits

    .End_If

    If W$edi-2 = '00'
        On B$edi-3 = '+', sub edi 3
    End_If
ret
____________________________________________________________________________________________

[NoDirectAccess: B$ ' ; Not accessed by direct CALL or JMP.', 0]

[ActualDisCodePointer: ?    BackStep: ?    LabelWritten: ?   LastWrittenLabel: ?]

WriteDisCodeLabel: ; 'WriteOneDataLabel'
    On esi = D$LastWrittenLabel, ret
    mov D$LastWrittenLabel esi

    mov eax esi | ToStringsMapFrom UserPeStart, eax
    .If D$eax <> 0
        push esi
            If D$eax = EntryPointName
                zCopy D$eax
            Else_If D$eax = MainWindowProcName
                mov W$edi CRLF | add edi 2
                zCopy D$eax
                mov B$edi ':' | inc edi
            End_If
        pop esi
    .End_If

    mov eax esi | sub eax D$UserPeStart | add eax D$DisImageBase
  ; >>> eax = RVA. ebx = 'RoutingMap' pointer.

   ; call BuildCommentedCodeReference ebx ; made by Guga

    test B$ebx EXPORTNODE | jz L0>
        call WriteExportedFunctionLabel

L0: mov ecx ebx, ebx eax
    push ebx
        If W$edi-3 <> '::'
            mov W$edi CRLF | add edi 2
        End_If

        mov D$edi 'Code' | add edi 4
        push 0-1

L0:     mov eax ebx | shr ebx 4 | and eax 0F
        add eax '0' | On eax > '9', add eax 7
        push eax
        cmp ebx 0 | ja L0<
        mov B$edi '0' | inc edi
L0:     pop eax | cmp eax 0-1 | je L1>
        mov B$edi al | inc edi | jmp L0<

L1: pop eax | push eax

    mov eax esi | ToStringsMapFrom UserPeStart, eax
    .If D$eax <> 0
        If D$eax = EntryPointName
            ;
        Else_If D$eax = MainWindowProcName
            ;
        Else
            push esi
                zCopy D$eax
            pop esi
        End_If
    .End_If

    mov W$edi ': ' | add edi 2

L2: pop eax
    mov ecx 10, edx 0 | div ecx | add dl '0'
    and al 001111 | add al 'A'
    mov B$edi al, B$edi+1 dl, B$edi+2 ':' | add edi 3

L9: ret


WriteExportedFunctionLabel:
  ; >>> eax = RVA. ebx = 'RoutingMap' pointer.       'ExportSectionComments' 'CheckExport'

    push eax, ebx, esi
        sub eax D$DisImageBase

      ; May be a wrong Flag resulting from a DLL Name pointer saved inside the
      ; 'RoutingMap'. If so, the relative 'SectionsMap' Byte is 'IMPORTFLAG':
        mov ecx D$NumberOfDisExportedFunctions | cmp ecx 0 | je L9>>

        mov esi D$DisExportFunctionsPointers, ebx D$DisExportNamesPointers

        .If esi < D$UserPeStart
            ; nop
        .Else_If esi > D$UserPeEnd
            ; nop
        .Else
          ; Scan the Functions Pointers, to get the Ordinal Indice:
            mov edx D$UserPeEnd | sub edx 4
L0:         cmp esi edx | jae L9>>
            cmp D$esi eax | je L1>
K0:             add esi 4 | add ebx 4 | loop L0<
                    jmp L9>>

L1:         push eax, ebx, ecx, edx, esi

            mov eax D$NumberOfDisExportedFunctions | sub eax ecx | mov ebx eax
; EAX= raw-ORD
          ; Search the Ordinal Number, in the Ordinal Table
            mov esi D$DisExportOrdinal, ecx D$NumberOfDisExportNames

L2:         cmp esi edx | jae L8>>
            cmp W$esi ax | je L3>
              ; Ordinal are Words:
                add esi 2 | loop L2<
                sub esi esi| jmp L4> ; Only ORD

L3:         mov eax D$NumberOfDisExportNames | sub eax ecx

          ; Get the parallel Pointer, in the Functions Pointers Table:
            mov esi D$DisExportNamesPointers
            mov esi D$esi+eax*4 | add esi D$UserPeStart

            On esi < D$UserPeStart, jmp L8>>
                On esi > D$UserPeEnd, jmp L8>>
                    ;sub edi 4
L4:
            While B$edi-1 = ' ' | dec edi | End_While
            If B$edi-3 > ' '
               mov W$edi CRLF | add edi 2
            End_If

            add ebx D$DisExportOrdBase | mov eax 'ORD0' | stosd
            xchg bl bh | rol bl 4 | rol bh 4 | mov ecx 4
B5:         mov al bl | and al 0F | jne B0>
            shr bx 4 | dec ecx | jne B5< | jmp B4>
B0:         mov al bl | and al 0F | or al 030 | cmp al 03A | jb B1> | add al 07
B1:         stosb | shr bx 4 | dec ecx | jne B0<
B4:         mov eax '::' | stosw | test esi esi | je L6>

            mov al "'" | stosb
L0:         lodsb | test al al | je L0> | stosb | jmp L0<
L0:         mov al "'" | stosb
            mov W$edi CRLF | add edi 2
L6:
            pop esi, edx, ecx, ebx, eax | jmp K0<<

L8:         pop esi, edx, ecx, ebx, eax
        .End_If

L9: pop esi ebx, eax
ret
____________________________________________________________________________________________

; Provides Local Labels, depending on the Code Pointer Value, in the range of
; A0-A9 to P0-P9:

WriteLocalLabelFromEax:
    mov ecx 10, edx 0 | div ecx | add dl '0'
    and al 001111 | add al 'A'
    mov B$edi al, B$edi+1 dl, B$edi+2 ':' | add edi 3
ret


[BadDecodeText: B$ "
 ; Downward Chunk would be bad if previous one was good. On Reference, aligning back: "
 BackSteps: "          
"
 BadDecodeTextLen: Len
 CommentItHexa: "; Get the Bytes Values with the Dialog option: [With Commented Hexa Code].
 ", CommentItHexaLen: Len]

BadDecode:
    push eax, ebx, ecx, esi
        push edi
            mov D$BackSteps '    ', D$BackSteps+4 '    '
            mov edi BackSteps
            mov eax esi | sub eax ebx | call WriteEax
        pop edi

        mov esi BadDecodeText, ecx D$BadDecodeTextLen | rep movsb

        If B$WithCommentedHexa = &FALSE
            mov esi CommentItHexa, ecx D$CommentItHexaLen | rep movsb
        End_If
    pop esi, ecx, ebx, eax
ret


[CALLInstruction: ?    MovOrJmpImmInstruction: ?]

RelativeToAbsolute:
 ; When called, eax = relative Dis
    mov eax D$LastCodeRef
    add eax esi | sub eax D$UserPeStart | add eax D$DisImageBase

    push eax
        sub eax D$DisImageBase | add eax D$SectionsMap

        .If eax < D$EndOfSectionsMap
            If eax > D$SectionsMap
                jmp L1>
            End_If
        .End_If

        add D$UnlikelyCode 0F
L1: pop eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

Dis_St0Sti_or_Fmem:
    ModMask bl to al | On al <> 3, jmp EndWith.F.mem

WriteSt0Sti: ; (Byte in bl).
    mov D$edi 'ST0 ', D$edi+4 'ST0 '
    RmMask bl to al
    add B$edi+6 al
    add edi 7
    mov B$DisFlag DISDONE+DISLINEOVER
ret


WriteStiSt0:
    mov D$edi 'ST0 ', D$edi+4 'ST0 '
    RmMask bl to al
    add B$edi+2 al
    add edi 7
    mov B$DisFlag DISDONE+DISLINEOVER
ret


WriteSti:
    mov D$edi 'ST0 '
    RmMask bl to al
    add B$edi+2 al
    add edi 3
    mov B$DisFlag DISDONE+DISLINEOVER
ret
____________________________________________________________________________________________



;;; 'Dis_mmx2_m128__xmm1'
; When called, ESI position is on the Mod/Reg/Rm Byte.

Proc MarkSSEdata:
    Arguments @Type
    Uses esi

        On B$WeAreInTheCodeBox = &TRUE, ExitP

      ; Remove the 'reg' bits from the mod/r/m byte:
        lodsb | and al 00_11_000_111 | On al <> 00_000_101, ExitP

        lodsd | sub eax D$DisImageBase | add eax D$RoutingMap

        .If eax < D$RoutingMap
            mov B$UnlikelyCode 5

        .Else_If eax > D$EndOfRoutingMap
            mov B$UnlikelyCode 5

        .Else
            or B$eax LABEL+EVOCATED

            sub eax D$RoutingMap | add eax D$SizesMap

            If D@Type = SSE_4_F
                mov D$eax FP4, D$eax+4 FP4, D$eax+8 FP4, D$eax+12 FP4

L4:             sub eax D$SizesMap | add eax D$SectionsMap
                On D$eax <> VIRTUALFLAG,
                    mov D$eax FOURDATAFLAGS, D$eax+4 FOURDATAFLAGS,
                        D$eax+8 FOURDATAFLAGS, D$eax+12 FOURDATAFLAGS

            Else_If D@Type = SSE_2_R
                mov D$eax FP8, D$eax+8 FP8 | jmp L4<

            End_If

        .End_If
EndP

[SSE_4_F 1, SSE_2_R 2, SSE_1_R 3, SSE_1_F 4, SSE_2_F 5,
 SSE_4_D 6, SSE_2_D 7, SSE_1_D 8, SSE_1_Q 9]

;;
  List of SSE Mnemonics >>> Types:  (call MarkSSEdata )
  
  addpd     call MarkSSEdata SSE_2_R
  addps     call MarkSSEdata SSE_4_F
  addsd     call MarkSSEdata SSE_1_R
  addss     call MarkSSEdata SSE_1_F
  addsubpd  call MarkSSEdata SSE_2_R
  addsubps  call MarkSSEdata SSE_4_F
  andnpd    call MarkSSEdata SSE_2_R
  andnps    call MarkSSEdata SSE_4_F
  andpd     call MarkSSEdata SSE_2_R
  andps     call MarkSSEdata SSE_4_F
  cmp..ps   call MarkSSEdata SSE_4_F opc2
  cmp..ss   call MarkSSEdata SSE_2_F
  comisd    call MarkSSEdata SSE_1_R
  comiss    call MarkSSEdata SSE_1_F
  cvtdq2pd  call MarkSSEdata SSE_2_D
  cvtdq2ps  call MarkSSEdata SSE_4_D
  cvtpd2dq  call MarkSSEdata SSE_2_R   ; F2 0F E6 /r opf2
  cvtpd2pi  call MarkSSEdata SSE_2_R
  cvtpd2ps  call MarkSSEdata SSE_2_R
  cvtpi2pd  call MarkSSEdata SSE_2_D
  cvtpi2ps  call MarkSSEdata SSE_2_D
  cvtps2dq  call MarkSSEdata SSE_4_F
  cvtps2pd  call MarkSSEdata SSE_2_F
  cvtps2pi  call MarkSSEdata SSE_2_F
  cvtsd2si  call MarkSSEdata SSE_1_R
  cvtsd2ss  call MarkSSEdata SSE_1_R
  cvtsi2sd  call MarkSSEdata SSE_1_D
  cvtsi2ss  call MarkSSEdata SSE_1_D
  cvtss2sd  call MarkSSEdata SSE_1_F
  cvtss2si  call MarkSSEdata SSE_1_F
  cvttpd2dq call MarkSSEdata SSE_2_R
  cvttpd2pi call MarkSSEdata SSE_2_R
  cvttps2dq call MarkSSEdata SSE_4_F
  cvttps2pi call MarkSSEdata SSE_2_F
  cvttsd2si call MarkSSEdata SSE_1_R
  cvttss2si call MarkSSEdata SSE_1_F
  divpd     call MarkSSEdata SSE_2_R
  divps     call MarkSSEdata SSE_4_F
  divsd     call MarkSSEdata SSE_1_R
  divss     call MarkSSEdata SSE_1_F
  haddpd    call MarkSSEdata SSE_2_R
  haddps    call MarkSSEdata SSE_4_F
  hsubpd    call MarkSSEdata SSE_2_R
  hsubps    call MarkSSEdata SSE_4_F
  maxpd     call MarkSSEdata SSE_2_R
  maxsd     call MarkSSEdata SSE_1_R
  maxss     call MarkSSEdata SSE_1_F
  minpd     call MarkSSEdata SSE_2_R
  minps     call MarkSSEdata SSE_4_F
  minsd     call MarkSSEdata SSE_1_R
  minss     call MarkSSEdata SSE_1_F
  movapd    call MarkSSEdata SSE_2_R
  movaps    call MarkSSEdata SSE_4_F
  movddup   call MarkSSEdata SSE_1_Q
  movhpd    call MarkSSEdata SSE_1_R
  movhps    call MarkSSEdata SSE_2_F
  movlpd    call MarkSSEdata SSE_1_R
  movlps    call MarkSSEdata SSE_2_F
  movshdup  call MarkSSEdata SSE_4_F
  movsldup  call MarkSSEdata SSE_4_F
  movntpd   call MarkSSEdata SSE_2_R
  movntps   call MarkSSEdata SSE_4_F
  movss     call MarkSSEdata SSE_1_R
  movupd    call MarkSSEdata SSE_2_R
  movups    call MarkSSEdata SSE_4_F
  mulpd     call MarkSSEdata SSE_2_R
  mulps     call MarkSSEdata SSE_4_F
  mulsd     call MarkSSEdata SSE_1_R
  mulss     call MarkSSEdata SSE_1_F
  orpd      call MarkSSEdata SSE_2_R
  orps      call MarkSSEdata SSE_4_F
  rcpps     call MarkSSEdata SSE_4_F
  rcpss     call MarkSSEdata SSE_4_F
  rsqrtps   call MarkSSEdata SSE_4_F
  rsqrtss   call MarkSSEdata SSE_4_F
  shufpd    call MarkSSEdata SSE_2_R
  shufps    call MarkSSEdata SSE_4_F
  sqrtpd    call MarkSSEdata SSE_2_R
  sqrtps    call MarkSSEdata SSE_4_F
  sqrtsd    call MarkSSEdata SSE_2_R
  sqrtss    call MarkSSEdata SSE_4_F
  subpd     call MarkSSEdata SSE_2_R
  subps     call MarkSSEdata SSE_4_F
  subsd     call MarkSSEdata SSE_2_R
  subss     call MarkSSEdata SSE_4_F
  ucomisd   call MarkSSEdata SSE_2_R
  ucomiss   call MarkSSEdata SSE_4_F
  unpckhpd  call MarkSSEdata SSE_2_R
  unpckhps  call MarkSSEdata SSE_4_F
  unpcklpd  call MarkSSEdata SSE_2_R
  unpcklps  call MarkSSEdata SSE_4_F
  xorpd     call MarkSSEdata SSE_2_R
  xorps     call MarkSSEdata SSE_4_F
;;


____________________________________________________________________________________________
____________________________________________________________________________________________

; The Encode / Decode Dialog (Tool Menu).
____________________________________________________________________________________________
____________________________________________________________________________________________

ViewEncoding:
    call 'USER32.DialogBoxParamA' D$hinstance, 28000, &NULL, EncodingProc, &NULL
ret

[EncodeEditHandle: ?    EncodingDialogHandle: ?] [ EncodeHelp: 'Code_Viewer' 0]
[ZeroString: ?]

; This is the Source For Encoding (Upper Edite Control);
[EncodeSourceMargin: B$ CR LF CR LF CR LF CR LF CR LF CR LF]
[EncodeSource: B$ "[DATA: 0 0 0 0]
LABEL0:
L0: ", EncodeText: "                                                                  
L1:
Label1:
"]
; This is to ensure that the Assembler do not overwrite the downward Data when ajusting,
; for example, the Asm Source ending CR/LF(s):
[EncodeSecurity: 0 #10]

; This is the second Edit Control showing Text Hexa Code
[HexaCodeText: ? #10]

; This is the third read only EditBox for Disassembly:
[DummyDecodeText: ? ?][DecodeText: ? #40] ; 40+2 >>> 168

; This is for storing Binary Hexa of Code when only Disassembling:
[DecodeOnlyHexa: ? #10]

Proc EncodingProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
        move D$EncodingDialogHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.GetDlgItem' D@Adressee 13 | mov D$EncodeEditHandle eax
        call 'USER32.SetFocus' eax
        jmp L8>>

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16
        ..If eax = &CBN_SELCHANGE

        ..Else_If D@wParam = &IDCANCEL
            mov D$EncodingDialogHandle 0
            call 'USER32.EndDialog' D@Adressee 0

        ..Else_If D@wParam = &IDOK
            call 'USER32.GetFocus' | call 'USER32.GetDlgCtrlID' eax
            On eax = 16, jmp L3>

            mov eax '    ', edi EncodeText, ecx 15 | rep stosd
            call 'USER32.GetDlgItemTextA' D@Adressee, 13, EncodeText, 60
            On eax = 0, jmp L3>

            mov B$EncodeText+eax ' ' | call EncodeDecode

            call 'USER32.SetDlgItemTextA' D@Adressee, 16, HexaCodeText
            call 'USER32.SetDlgItemTextA' D@Adressee, 17, DecodeText

        ..Else_If D@wParam = 3
L3:         call 'USER32.GetDlgItemTextA' D@Adressee, 16, HexaCodeText, 80

            call DecodeOnly

            call 'USER32.SetDlgItemTextA' D@Adressee, 13, ZeroString
            call 'USER32.SetDlgItemTextA' D@Adressee, 17, DecodeText
            call 'USER32.SetDlgItemTextA' D@Adressee, 16, HexaCodeText


        ..Else_If D@wParam = &IDHELP
            call Help, B_U_AsmName, EncodeHelp, ContextHlpMessage

        ..End_If

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        jmp L1>

    ...Else_If D@Message = &WM_CTLCOLORLISTBOX
L1:     call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


[WeAreInTheCodeBox: ?]

DecodeOnly:
    mov B$WeAreInTheCodeBox &TRUE, B$SimpleScan &TRUE
    mov esi HexaCodeText

    While B$esi > 0
        On B$esi <= ' ', mov B$esi Space
        inc esi
    End_While
    mov B$esi Space, D$esi+1 0

    mov esi HexacodeText, edi DecodeOnlyHexa

    .While B$esi > 0
        mov ebx 0,  edx 0, ecx 0, eax 0

        While B$esi < '0'
            inc esi | On B$esi = 0, jmp L9>
        End_While
        While B$esi = '0'
            inc esi
            On B$esi <= Space, jmp L8>
            On B$esi = 0, jmp L9>
        End_While

L0:     lodsb | On al > 'Z', and eax (not 32) | cmp al Space | jbe L8>
            sub al '0' | cmp al 9 | jbe L2>
                sub al 7
L2:     shld edx ebx 4 | shl ebx 4 | or bl al
        cmp edx ecx | jb L7>
        mov ecx edx
        cmp al 0F | jbe L0<

L7:     Beep | ret

L8:     mov eax ebx
        cmp eax 0FF | ja L7<

        stosb
    .End_While
L9:  mov D$edi 0

  ; Disassemble: (DisMain)
    mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    mov esi DecodeOnlyHexa, edi DecodeText
L0: movzx eax B$esi | inc esi | call D$DisOp1+eax*4
; WriteImm32 WeAreInTheCodeBox WriteDisRelative
    On B$DisFlag = DISDONE, jmp L0<
    mov D$edi 0

  ; Re-write the Hexa Code Text (simply for clean up and striping extra-Bytes):
    mov edx esi, esi DecodeOnlyHexa, edi HexacodeText
    While esi < edx
        movzx eax B$esi | inc esi
        mov ebx eax | shr ebx 4
        and eax 0F | and ebx 0F
        mov al B$HexaTable+eax, bl B$HexaTable+ebx
        shl eax 8 | or eax ebx | or eax 020200000 | stosd
    End_While
    mov D$edi 0

  ; In case of text organisation (sub edi 6, for example), we reset:
    If D$DummyDecodeText+4 <> 0
        mov eax DecodeText
        While B$eax-1 > 0
            dec eax
        End_While
        mov ecx DecodeText | sub ecx eax
        mov esi DecodeText | add esi 160-1 | mov edi esi | sub esi ecx
        std
L0:         movsb | cmp edi DecodeText | jae L0<
        cld
        mov D$DummyDecodeText 0, D$DummyDecodeText+4 0
    End_If

    mov B$WeAreInTheCodeBox &FALSE, B$SimpleScan &FALSE
ret


[LastDisVirtualData: ?]

____________________________________________________________________________________________
____________________________________________________________________________________________



