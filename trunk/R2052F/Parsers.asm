TITLE Parsers
____________________________________________________________________________________________
; Maintainer: Rikkert Wiggerink (EvilBro)
; Started: Somewhere in april 2003
; Email: r.wiggerink@student.utwente.nl
____________________________________________________________________________________________
;;
29-4-2003:
 This is the first version of the rewritten parsers in an official RosAsm release.
 This is by no means the end of the revision. :)
  
 The philosophy behind this revision:
    The routines that previously cleaned up the source have been replaced and split up into 
    many different routines (duplicate actions were removed). This is done for maintainance 
    purposes. Anyone who has taken a look at the old routines will see the importance of this 
    split up (from a maintainance point of view anyway :) )
    
    There will be some people who will think the clean up needs to be done in a single routine
    (Yes, I mean Kenny :) ). This will not work. It is what Betov started with when the old
    routines were written (and we all know the hell that followed from that... at least I do).
    There is also no need for the extra performance a single clean up routine would bring.
    On my Pentium 90 compilation of RosAsm takes 39.8 seconds with the new routines and 
    38.5 seconds with the old routines (average timing values used). That means that the new 
    routines are roughly a second slower (on a source like RosAsm). However, this second is 
    completely insignificant compared to the total compile time.

 All parsers I've written expect the source to be in CodeSourceA. They will either modify the 
 source in CodeSourceA directly or copy/modify the source to CodeSourceB. On exiting a routine
 the source will be in CodeSourceA. This is done for simplicity.

 Anyway, I expect that now this version is implemented in an official RosAsm release, errors 
 are bound to turn up. Just post them on the RosAsm forum or mail them directly to me and 
 I'll fix them.

 BTW 'Kill' means 'replace by spaces'. 'Strip' means 'replace by nothing'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; NewCopyToCodeSourceA replaces CopyToCodeSourceA which always used CodeSource as the <Source>.
; usage: call NewCopyToCodeSourceA <Source> <SourceLength>

Proc NewCopyToCodeSourceA:
    Arguments @Pointer, @Length

        mov esi D@Pointer, ecx D@Length, edi D$CodeSourceA, D$StripLen ecx
        rep movsb
EndP

Proc InjectedCopyToCodeSourceA:
    Arguments @Pointer, @Length

        mov esi InjectedTIME_COUNT, edi D$CodeSourceA, ecx D$InjectedTIME_COUNT_Len
        mov D$StripLen ecx | rep movsb

        mov esi D@Pointer, ecx D@Length | add D$StripLen ecx | rep movsb
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; CoolParsers are all parsers that do not change the position of statements in the source.

CoolParsers:
    call CheckTextDelimitersPairing
    call KillMultiLineComments ; and Comments
   ; call KillSingleLineComments
    call NewKillVirtualCRLF
    call KillMeaninglessCommas
    call CheckandKillPreParsers
        On B$ParseIncInclude = &TRUE, call ClearIncludeStateMentsFromSource ;call IncParser
        On B$ParseAlternates = &TRUE, call AlternatesPreparsers
      ; +0.2 seconds (2.850 >>> 3.650) on RosAsm 4 Megas, with a Celeron 1.3.
    call KillTitles ; + Old 'ConvertTextSigns'
   ; call CheckBracketsPairing
    ;call CheckNestedBracketsPairing
    call CheckPairings
    call ReplaceParaMacrosBrackets
   ; call CheckOpenCloseSignPairing
ret
;;
CoolParsersOnInc:
        call CheckTextDelimitersPairing
        call KillMultiLineComments ; and Comments
       ; call KillSingleLineComments
        call NewKillVirtualCRLF
        call KillMeaninglessCommas
;;

CoolParsersOnInc: ; CoolParsers
    push D$CodeSourceA, D$StripLen

        move D$CodeSourceA D$bininc.mem, D$StripLen D$bininc.filesize

        call CheckTextDelimitersPairing
        call KillMultiLineComments ; and Comments
       ; call KillSingleLineComments
        call NewKillVirtualCRLF
        call KillMeaninglessCommas

    pop D$StripLen, D$CodeSourceA
ret

____________________________________________________________________________________________
; HotParsers are parsers that can change the position of statements in the source.

HotParsers:
    call TranslateAsciiToMyAscii

    call StripUnderscore
    On B$ProfilerFlag = &TRUE, call InjectDashLines

    call StripUnneededSpaces
    call ConvertCommasToSpace
    call StripUnneededEOI
    call ConvertEOIinBracketsTOmeEOI
   ; call ConvertTextSigns    ; This one 'needs' to be done sooner. Would simplify earlier routines
    call ExtendLocalSymbols
    call IdentifyVirtualData
    call ReorderSource
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
CheckTextDelimitersPairing:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$esi-1 LF      ; for MultiLineComment starting on the first line

    .While esi < ecx
        .If B$esi = '"'
            mov edx esi
            Do
                inc esi | cmp esi ecx | je L9>  ; Error: no closing delimiter found inside source.
            Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            mov edx esi
            Do
                inc esi
                cmp esi ecx | je L9>        ; Error: no closing delimiter found inside source.
                cmp B$esi CR | je L9>       ; Error: ' isn't allowed to be multiline.
            Loop_Until B$esi = "'"
        .Else_If B$esi = ';'
            If D$esi-1 = MLC
                Do
                    inc esi | cmp esi ecx | je L8>
                Loop_Until D$esi = MLC
                inc esi
            Else
                Do
                    inc esi
                Loop_Until B$esi < ' '
            End_If
        .End_If
        inc esi
    .End_While
L8: ret

L9: ;ERROR! Unpaired textdelimiter.
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$OpenTextPtr
ret
____________________________________________________________________________________________
; Multiline comments are converted to spaces.

KillMultiLineComments:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$esi-1 LF      ; for MultiLineComment starting on the first line

    .While esi < ecx
        ..If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ..Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ..Else_If B$esi = ';'
            .If D$esi-1 = MLC
                Do
                    mov B$esi ' '
                    inc esi | On esi >= ecx, ret
                Loop_Until D$esi = MLC
                mov D$esi 0D202020  ; Replace 'LF ; ; CR' with 'Space Space Space CR'.
                add esi 3
            .Else
                ;On D$esi+1 = ' Tag', call AssemblyTag
                Do | mov B$esi ' ' | inc esi | Loop_Until B$esi < ' '
            .End_If
        ..End_If
        inc esi
    .End_While
L8: ; KillMultiLineComments might have killed the closing CRLF, thus it is restored.
    mov W$esi-2 0A0D
ret
________________________________________________________________________________________________
; Singleline comments are converted to spaces.
; Warning: This routine will also kill ";;", so if it is run before MultiLineComment are killed
; then you can be pretty sure, you won't be able to strip MultiLineComments correctly.

KillSingleLineComments:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = ';'
            Do | mov B$esi ' ' | inc esi | Loop_Until B$esi < ' '
        .End_If
        inc esi
    .End_While
L8: ret
________________________________________________________________________________________________
; Titles are converted to spaces.

KillTitles: ; ConvertTextSigns
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = '"'
            mov B$esi TextSign
        .Else_If B$esi = "'"
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = "'"
            mov B$esi TextSign
        .Else_If D$esi = 'TITL'
            If B$esi-1 = LF
                On W$esi+4 <> 'E ', jmp L0>
                    Do | mov B$esi ' ' | inc esi | Loop_Until B$esi < ' '
L0:
            End_If
        .End_If

        inc esi

    .End_While

L8: ret

________________________________________________________________________________________________
; VirtualCRLFs are converted to spaces.

[DisableWarning: B$ 0]

NewKillVirtualCRLF:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    ..While esi < ecx
        ...If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ...Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ...Else_If B$esi = ','
            push esi
            .While W$esi-1 <> CRLF
                dec esi
                    ..If_Or B$esi = '+', B$esi = '-', B$esi = '/', B$esi = '*', B$esi = '^'
                        .If B$DisableWarning = &FALSE
                            SyntaxErrorInMacro1 D$BadSyntaxBeforeCommaPtr esi
                            pushad
                            call 'USER32.MessageBoxA' 0, {"Disable previous warning ?", 0}, {'Syntax Error Found', 0}, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO
                            If eax = &IDYES
                                mov B$DisableWarning &TRUE
                            End_If
                            popad

                        .End_If
                        mov B$esi ' '
                    ..Else_If B$esi = ' '
                    ..Else ; any other char, exit
                        jmp L5>
                    ..End_If
            .End_While
         L5: | pop esi
        ...Else_If B$esi = CR
            mov edi esi, al ' '
            While B$edi <= ' '
                dec edi | On edi < D$CodeSourceA, jmp L7>
            End_While
            If B$edi = ','
                Do
                    stosb | cmp edi ecx | ja L8>
                Loop_Until B$edi > ' '
            End_If
        ...End_If

L7:     inc esi

    ..End_While
L8: ret
________________________________________________________________________________________________
; Meaningless commas are converted to spaces.
; Note: This routine will probably move to HotParsers in the future.

KillMeaninglessCommas:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = ','
            mov edi esi
            Do
                inc edi
            Loop_Until B$edi <> ' '

            If B$edi = '+'
                mov esi edi
            Else_If B$edi = '-'
                mov esi edi
            Else
                mov B$esi ' '
            End_If
        .End_If

        inc esi

    .End_While
ret
____________________________________________________________________________________________
; Checks for PREPARSE statements, sets the right flags and then kills the PREPARSE statements.
; Note that any amount of PREPARSE statements can be used in the source with this new routine.

[ParseAlternates: B$ ?    ParseEqual: ?    ParseOOA: ?    ParseNew: ?
 ParseBinInclude: ?       ParseIncInclude: ?]

CheckandKillPreParsers:
    mov B$ParseAlternates &FALSE, B$ParseEqual &FALSE, B$ParseOOA &FALSE, B$Dynamic &FALSE

    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If D$esi = 'PREP'             ; PREPARSE
            ..If D$esi+4 = 'ARSE'
                If B$esi-1 = LF
                    On B$esi+8 <> ' ', jmp L6>
                    call NewCheckPreparser
                    Do
                        mov B$esi ' '
                        inc esi
                    Loop_Until B$esi < ' '
L6:
                End_If
            ..End_If
        .End_If

L7:     inc esi

    .End_While
L8: ret
_______________________________________________________________________________________________
; This routine is not really altered at this point compared to the old routine. This will be
; done in a future version as this version is really unreadable (and thus hard to maintain).

[Dynamic: ?  MemReservation: ?]

NewCheckPreparser:
    mov D$MemReservation 0

    mov edi esi
    add edi 9

L1: While B$edi = ' ' | inc edi | End_While

    mov eax D$edi | and eax (not 020202020)     ; Convert to uppercase.
    ...If eax = 'ALTE'                          ; ALTERNATES
        mov eax D$edi+4 | and eax (not 020202020)
        ..If eax = 'RNAT'
            mov ax W$edi+8 | and eax (not 02020)
            .If ax = 'ES'
                If B$edi+10 <= ' '
                    mov B$ParseAlternates &TRUE | add edi 11 | jmp L8>>
                End_If
            .End_If
    ...Else_If eax = 'EQUA'                     ; EQUAL
        mov al B$edi+4 | and eax (not 020)
        .If al = 'L'
            If B$edi+5 <= ' '
                mov B$ParseEqual &TRUE | add edi 6 | jmp L8>>
            End_If
        .End_If
    ...Else_If eax = 'BINI'                     ; BinIncluder
        mov eax D$edi+4 | and eax (not 020202020)
        .If eax = 'NCLU'
            mov eax D$edi+7 | and eax (not 020202020)
            If eax = 'UDER'
                mov B$ParseBinInclude &TRUE | add edi 12 | jmp L8>>
            End_If
        .End_If
    ...Else_If eax = 'INCI'                     ; IncIncluder
        mov eax D$edi+4 | and eax (not 020202020)
        .If eax = 'NCLU'
            mov eax D$edi+7 | and eax (not 020202020)
            If eax = 'UDER'
                mov B$ParseIncInclude &TRUE | add edi 12 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'OO'                        ; OOA
        mov al B$edi+2 | and eax (not 020)
        .If al = 'A'
            If B$edi+3 <= ' '
                mov B$ParseOOA &TRUE | add edi 4 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'NE'                        ; Prepare New
        mov al B$edi+2 | and eax (not 020)
        .If al = 'W'
            If B$edi+3 <= ' '
                mov B$ParseNew &TRUE | add edi 4 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'EN'                        ; Preparse EntryPoint
        mov eax D$edi+2 | and eax (not 020202020)
        .If eax = 'TRYP'
            mov eax D$edi+6 | and eax (not 020202020)
            If eax = 'OINT'
                call TakeNewEntryPoint | ret   ; Must be separated on one line
            End_If
        .End_If
    ...Else_If eax = 'DYNA'                        ; Preparse Dynamic
        mov eax D$edi+3 | and eax (not 020202020)
        If eax = 'AMIC'
            mov B$Dynamic &TRUE | add edi 8 | jmp L8>
        End_If
;;
  ; Must be run before this time...
  
    ...Else_If eax = 'RESE'                        ; Preparse Reserve
        mov eax D$edi+3 | and eax (not 020202020)
        If eax = 'ERVE'
            call ReadMemoryReservation
            add edi 8 | jmp L8>
        End_If
;;

    ...End_If

    mov B$edi-1 0, esi edi
    While B$edi > ' ' | inc edi | End_While | mov B$edi 0
    mov B$ErrorLevel 9 | error D$BadPreParsePtr, esi

L8:  On B$edi >= ' ', jmp L1<<

; Possible add of multiple Pre-Parsers conflicts checking...
ret


ReadMemoryReservation:
    push esi
        mov esi edi
        While B$esi > ' ' | inc esi | End_While
        While B$esi = ' ' | inc esi | End_While
        If B$esi = '0'
            call TranslateHexa
        Else
            call TranslateDecimal
        End_If

        mov D$MemReservation eax
    pop esi
ret


[BadEntryDef: 'Bad definition of EntryPoint in "Preparse EntryPoint"', 0]

TakeNewEntryPoint: ; Preparse EntryPoint Name
    push esi, edi, ecx
        mov esi edi | add esi 11 | mov edi EntryPointLabel, ecx 0

        While B$esi > ' '
            lodsb | and eax (not 020) | stosb | inc ecx
        End_While
        mov B$edi 0

        mov B$ErrorLevel 9
        mov D$EntryPointLabelLen ecx | On ecx = 0, error BadEntryDef
    pop ecx, edi, esi
ret
________________________________________________________________________________________________
[InsideBrackets: B$ ?]

CheckBracketsPairing:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = '['
            If B$InsideBrackets = &TRUE
                jmp L9>
            Else
                mov edx esi
                mov B$InsideBrackets &TRUE
            End_If
        .Else_If B$esi = ']'
            If B$InsideBrackets = &TRUE
                mov B$InsideBrackets &FALSE
            Else
                mov edx esi
                jmp L9>
            End_If
        .End_If
        inc esi
    .End_While

    On B$InsideBrackets = &TRUE, jmp L9>

L8: ret

L9: ;ERROR! Unpaired bracket
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$OrphanBracketPtr
ret


L8: ;ERROR! Unpaired open/close-sign
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$ParenthesisPtr
ret
________________________________________________________________________________________________

[VirtualBracketsCount: ?
 FirstBracket: ?         LastBracket: ?
 FirstVirtualBracket: ?  LastVirtualBracket: ?
 FirstParenthesis: ?     LastParenthesis: ?]

CheckPairings:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE, edx esi
    mov D$OpenSignsCount 0, D$VirtualBracketsCount 0

    .While esi < ecx
        mov al B$esi

        .If al = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        .Else_If al = '['
            If B$InsideBrackets = &TRUE
                jmp L9>>
            Else_If D$VirtualBracketsCount <> 0
                jmp L9>>
            Else_If D$OpenSignsCount <> 0
                jmp L9>>
            Else
                mov B$InsideBrackets &TRUE, D$FirstBracket esi
            End_If

        .Else_If al = ']'
            mov D$LastBracket esi
            If D$VirtualBracketsCount <> 0
                jmp L9>>
            Else_If B$OpenSignsCount <> 0
                jmp L9>>
            Else_If B$InsideBrackets = &FALSE
                jmp L9>>
            End_If
            mov B$InsideBrackets &FALSE

        .Else_If al = '{'
            mov D$FirstVirtualBracket esi
            If B$InsideBrackets = &FALSE
                On D$VirtualBracketsCount > 0, jmp L9>> ; <<<<<<<<<<<<<<<<<<<<
            End_If
            inc D$VirtualBracketsCount

        .Else_If al = '}'
            mov D$LastVirtualBracket esi
            dec D$VirtualBracketsCount | On D$VirtualBracketsCount = 0-1, jmp L9>>

        .Else_If al = '('
            On B$OpenSignsCount = 0, mov D$FirstParenthesis esi
            inc B$OpenSignsCount

        .Else_If al = ')'
            mov D$LastParenthesis esi
            dec D$OpenSignsCount | On D$OpenSignsCount = 0-1, jmp L9>>
        .End_If

        inc esi
    .End_While

    If B$InsideBrackets = &TRUE
        mov B$esi '[' | jmp L9>
    Else_If D$VirtualBracketsCount <> 0
        mov B$esi '{' | jmp L9>
    Else_If B$OpenSignsCount <> 0
        mov B$esi ')' | jmp L9>
    End_If
ret

L9: ; Pointing the unpairing error:
    push esi
        .If B$esi = '['
            If D$VirtualBracketsCount <> 0
                mov esi D$FirstVirtualBracket
            Else_If D$OpenSignsCount <> 0
                mov esi D$FirstParenthesis
            Else
                mov esi D$FirstBracket
            End_If

        .Else_If B$esi = ']'
            If D$VirtualBracketsCount <> 0
                mov esi D$FirstVirtualBracket
            Else_If D$OpenSignsCount <> 0
                mov esi D$FirstParenthesis
            Else
                ;
            End_If

        .Else_If B$esi = '{'
            mov esi D$FirstVirtualBracket
        .Else_If B$esi = '}'
            ;
        .Else_If B$esi = '('
            mov esi D$FirstParenthesis
        .Else_If B$esi = ')'
            ;
        .End_If

        sub esi D$CodeSourceA | add esi D$CodeSource
        mov eax esi
        While B$esi > LF | dec esi | End_While | inc esi
        While B$eax > LF | inc eax | End_While | dec eax
        mov D$BlockStartTextPtr esi, D$BlockEndTextPtr eax, B$BlockInside &TRUE
        mov D$UpperLine esi
        call UpOneLine | call UpOneLine | call UpOneLine
    pop esi

  ; Set the Error Message Text:
    .If B$esi = '['
        If D$VirtualBracketsCount <> 0
            mov eax UnPairedNestedBrackets
        Else_If D$OpenSignsCount <> 0
            mov eax D$ParenthesisPtr
        Else
            mov eax D$OrphanBracketPtr
        End_If

    .Else_If B$esi = ']'
        If D$VirtualBracketsCount <> 0
            mov eax D$UnPairedNestedBracketsPtr
        Else_If D$OpenSignsCount <> 0
            mov eax D$ParenthesisPtr
        Else
            mov eax D$OrphanBracketPtr
        End_If

    .Else_If B$esi = '{'
        mov eax D$UnPairedNestedBracketsPtr

    .Else_If B$esi = '}'
        mov eax D$UnPairedNestedBracketsPtr

    .Else_If B$esi = '('
        mov eax D$ParenthesisPtr

    .Else_If B$esi = ')'
        mov eax D$ParenthesisPtr

    .Else
        mov eax D$unknownPtr

    .End_If

    mov edi CookedErrorMessage, esi D$BlockStartTextPtr
    While esi < D$BlockEndTextPtr
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While

L2: mov B$edi 0

    mov B$Errorlevel 9 | error eax
ret
____________________________________________________________________________________________

[InsideParaMacro: ?]

ReplaceParaMacrosBrackets:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE, B$InsideParaMacro &FALSE, ebx 0, edx 0

    .While esi < ecx
        lodsb

        ..If al = TextSign
            While B$esi <> TextSign | inc esi | End_While | inc esi
        ..Else_If al = '['
            mov B$InsideBrackets &TRUE
        ..Else_If al = ']'
            mov B$InsideBrackets &FALSE
        ..Else_If al = '{'
            .If B$InsideBrackets = &FALSE
                mov B$esi-1 OpenParaMacro
                mov B$InsideParaMacro &TRUE
            .Else
                mov ebx esi | dec ebx
                While B$ebx <> '['
                    dec ebx
                    On B$ebx = '|', jmp L2>
                    On B$ebx = LF, jmp L2>
                    If B$ebx > ' '
                        mov B$esi-1 OpenParaMacro
                        mov B$InsideParaMacro &TRUE | jmp L2>
                    End_If
                End_While
            .End_If

        ..Else_If al = '}'
            If B$InsideParaMacro = &TRUE
                mov B$esi-1 CloseParaMacro
                mov B$InsideParaMacro &FALSE
            End_If

        ..End_If
L2: .End_While
ret
________________________________________________________________________________________________
; This routine will be moved to Hotparsers at a later time.
[OpenSignsCount: B$ ?]

CheckOpenCloseSignPairing:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$OpenSignsCount 0

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = '('
            If B$OpenSignsCount = 0
                mov edx esi
            End_If
            inc B$OpenSignsCount
        .Else_If B$esi = ')'
            If B$OpenSignsCount = 0
                jmp L9>
            End_If
            dec B$OpenSignsCount
        .End_If
        inc esi
    .End_While
L8: ret

L9: ;ERROR! Unpaired open/close-sign
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$ParenthesisPtr
ret
____________________________________________________________________________________________
________________________________________________________________________________________________
NewCountStatements:
  ; How many statements:
    mov B$ErrorLevel 0
    mov esi D$CodeSourceA, D$StatementsCounter 0, D$LinesCounter 0
    mov B$DontCountNext &FALSE
    mov ebx esi | add ebx D$StripLen

    .While esi < ebx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = LF
            mov B$DontCountNext &FALSE
        .Else_If B$esi = '|'
            mov B$DontCountNext &FALSE
        .Else_If B$esi = '['
            inc D$LinesCounter
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = ']'
            mov B$DontCountNext &FALSE
        .Else_If B$esi > ' '
            ..If B$esi <> '_'
                If B$DontCountNext = &FALSE
                    inc D$LinesCounter
                    mov B$DontCountNext &TRUE
                End_If
            ..End_If
        .End_If

        inc esi
    .End_While

; set mem tables:

L9: If D$LinesCounter = 0
        call CloseProgressBar
        call 'USER32.MessageBoxA' 0, {"RosAsm can't compile empty files", 0},
                                     {' Sorry', 0}, 0
        mov B$CompileErrorHappend &TRUE
        mov esp D$OldStackPointer | ret ; direct error
       ; pop eax | ret                  ; Abort, pop caller and return to Message Loop
    End_If

    mov eax D$LinesCounter | add eax 20 | shl eax 3  ; 2 > dword +1 > security
    push eax
        VirtualAlloc StatementsTable eax
    pop eax
    VirtualAlloc StatementsTable2 eax

;StoreStatements:

    mov ecx D$CodeSource | sub ecx D$CodeSourceA    ; Ajust from CodeSource to CodeSourceA.
    mov esi D$CodeSourceA, edi D$StatementsTable
    mov B$DontCountNext &FALSE
    move D$StatementsPtr D$StatementsTable | move D$edi esi | add D$edi ecx
    mov ebx esi | add ebx D$StripLen

    .While esi < ebx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = LF
            mov B$DontCountNext &FALSE
        .Else_If B$esi = '|'
            mov B$DontCountNext &FALSE
        .Else_If B$esi = '['

            mov eax esi | add eax ecx | stosd | add D$StatementsPtr 4

            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If

                inc esi
            .Loop_Until B$esi = ']'

            mov B$DontCountNext &FALSE

        .Else_If B$esi > ' '
            ..If B$esi <> '_'
                If B$DontCountNext = &FALSE
                    mov eax esi | add eax ecx | stosd | add D$StatementsPtr 4
                    mov B$DontCountNext &TRUE
                End_If
            ..End_If
        .End_If

        inc esi
    .End_While

    mov eax 0 | stosd
ret
________________________________________________________________________________________________
________________________________________________________________________________________________
; As of now a '_' outside a Win_equate is the same '' like it is supposed to be according to
; RosAsm help.

StripUnderscore:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign
        .Else_If B$esi = '&'
            If B$esi+1 > '9'
                Do | movsb | Loop_Until B$esi <= ' '
            End_If
        .Else_If B$esi = '_'
            Do | inc esi | Loop_Until B$esi <> '_'
        .End_If
        movsb
    .End_While

    mov ecx edi
    sub ecx D$CodeSourceB
    mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
; To simplify operations, the source is converted to a special format called MyAscii.

[LowSigns            31
    TextSign            30

  NoSpaceAfterThis    29
    numSign             28   ; #  01C
    IfNumSign           27   ; Substitute of # for the Conditional macros #If, ... 01B

    OpenParaMacro       26   ; { for ParaMacros  01A
  NoSpaceBeforeThis   25
    CloseParaMacro      24   ; } for ParaMacros

    CommaSign           23   ; ,

    OpenVirtual         22   ; [   016 (Macros expanded '[' -{-)
    CloseVirtual        21   ; ]   015 (Macros expanded ']' -}-) 019
    OpenBracket         20   ; [   014
    CloseBracket        19   ; ]   013
; 18, 17 >>> NewOpenBracket / NewCloseBracket
  PartEnds            16
    memMarker           15   ; $ or $  exemple: MOV B$MYVALUE 1
    colonSign           14   ; :
    openSign            13   ; (
    closeSign           12   ; )

  OperatorSigns       11
    addSign             10   ; +
    subSign              9   ; -
    mulSign              8   ; *
    divSign              7   ; /
    expSign              6   ; ^
; 5
  Separators          4
   ; Statement           0FF
    Space               3    ; space
    EOI                 2    ; |  End Of Instruction (separator)
    meEOI               1]   ; |  End Of Instruction in macro expansion
                             ; 0 is used as erase sign inside treatements


[MyAsciiTable: B$ 0,1,2,3,4,5,6,7,8,Space,EOI,11,12,EOI,14,15,16,17,18,19,20,21,
 22,23,24,25,26,27,28,29,30,31,Space,'!','"',NumSign,memMarker,'%','&',39,OpenSign,
 CloseSign, MulSign,AddSign,CommaSign,SubSign,'.',DivSign,48,49,50,51,52,53,54,
 55,56,57, ColonSign,';','<',61,'>','?','@'
 65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90 ; (A > Z)
 Openbracket,'\',Closebracket,expSign,95,96
 65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90 ; (A > Z)
 '{',EOI,'}',126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143
 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164
 165,166,memMarker,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,
 184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,
 205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,
 226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,
 247,248,249,250,251,252,253,254,255]

TranslateAsciiToMyAscii:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen, ebx 0
    add ecx D$CodeSourceA

    .While esi < ecx
        If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        Else
            mov bl B$esi
            mov al B$ebx+MyAsciiTable
            mov B$esi al

        End_If

        inc esi
    .End_While
ret
________________________________________________________________________________________________

StripUnneededSpaces:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If B$esi = Space
            ..If B$esi+1 = OpenSign
                If B$edi-1 = CloseSign
                    movsb
                Else_If B$edi-1 = CloseParaMacro
                    movsb
                Else_If B$edi-1 < NoSpaceAfterThis
                    inc esi
                Else
                    movsb
                End_If
            ..Else_If B$esi+1 < NoSpaceBeforeThis
                inc esi
            ..Else_If B$edi-1 < NoSpaceAfterThis
                If B$edi-1 = CloseSign
                    movsb
                Else_If B$edi-1 = CloseParaMacro
                    movsb
                Else
                    inc esi
                End_If
            ..Else_If B$esi+1 = '}'
                inc esi
            ..Else_If B$esi-1 = '{'
                inc esi
            ..Else
                movsb
            ..End_If
        .Else
            movsb
        .End_If
    .End_While

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
____________________________________________________________________________________________

; Remaining some valid Comma. Example "mov eax, -1".

ConvertCommasToSpace:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    While esi < ecx
        On B$esi = CommaSign, mov B$esi Space
        inc esi
    End_While
ret
________________________________________________________________________________________________
StripUnneededEOI:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

; Ensure that the source starts with an EOI
    If B$esi <> EOI
        mov B$edi EOI
        inc edi
    End_If

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If B$esi = EOI
            ..If B$esi+1 = EOI
                inc esi
            ..Else_If B$esi+1 = CloseBracket
                inc esi
            ..Else_If B$esi+1 = OpenBracket
                If B$edi-1 = CloseBracket
                    inc esi
                Else
                    movsb
                End_If
            ..Else
                movsb
            ..End_If
        .Else
            movsb
        .End_If
    .End_While

    mov ecx edi
    sub ecx D$CodeSourceB
    mov D$StripLen ecx

    mov B$edi EOI,  B$edi+1 EOI | add D$Striplen 2          ; write end mark '||'

    Exchange D$CodeSourceA D$CodeSourceB
ret
____________________________________________________________________________________________

ConvertEOIinBracketsTOmeEOI:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen, ebx 0 | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_if B$esi = OpenBracket
            mov B$InsideBrackets &TRUE
        .Else_if B$esi = CloseBracket
            mov B$InsideBrackets &FALSE
        .Else_if B$esi = EOI
            If B$InsideBrackets = &TRUE
                mov B$esi meEOI
            End_If
        .End_If
        inc esi
    .End_While
ret
________________________________________________________________________________________________
ConvertTextSigns:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = '"'
            mov B$esi TextSign
        .ElseIf B$esi = "'"
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = "'"
            mov B$esi TextSign
        .End_If
        inc esi
    .End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Automatic Labels (created by the Assembler, for example, with the "&0" Macro Key,
  are 8 Bytes Long. Example: ZZZZZZZZ: (See 'NoMeanLabel').
  
  The user cannot make use of this form of Labels.
;;

NoAutomaticLabel:
    mov esi D$CodeSourceA, edx esi | add edx D$Striplen

    .While esi < edx
        If B$esi = TextSign
            inc esi
            While B$esi <> TextSign | inc esi | End_While
        End_If

        ..If B$esi = 'Z'
            .If B$esi-1 < LowSigns
                If D$esi = 'ZZZZ'
                  ; We have something begining with 'ZZZZ'. Must 8 chars long: 'ZZZZZZZ'
                    cmp B$esi+8 LowSigns | ja L2>

                    cmp B$esi+7 LowSigns | jb L2>
                    cmp B$esi+6 LowSigns | jb L2>
                    cmp B$esi+5 LowSigns | jb L2>
                    cmp B$esi+4 LowSigns | jb L2>

                        mov B$esi+9 0, B$Errorlevel 9 | error ZZZZreserved, esi

                End_If
            .End_If
        ..End_If

L2:     inc esi
    .End_While
ret
________________________________________________________________________________________________
; The way local symbols are defined in the RosAsm syntax needs to be reviewed. Until that is
; properly done (somewhere in the future) this routine will have to do. :)

[LastFoundLabel: B$ ? #80]
;;
  Beware: This Routine is called twice:
  
  First time from inside the HotParsers, to expand the 'normal' @Locals
  
  A second time for 'AsmMain', after the Equates and Macros Jobs, to expand the
  @Locals inserted by Macros Evocations.
;;
ExtendLocalSymbols:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$LastFoundLabel 0

    .While esi < ecx
        ...If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign

        ...Else_If B$esi = ColonSign
          ; On '::' don't process (as the label is already stored):
            ..If B$esi-1 < LowSigns
                ; Error holded downward.

            ..Else_If B$esi-1 <> ColonSign
              ; nonlocal label, as local labels are always 2 characters:
                .If B$esi-3 > LowSigns
                    push esi
                    mov al 0
                    Do | dec esi | On B$esi = '@', mov al 1 | Loop_Until B$esi < LowSigns
                    If al = 1
                        pop esi

                    Else
                        pop eax
                        inc esi
                        mov ebx LastFoundLabel
                        Do | lodsb | mov B$ebx al | inc ebx | Loop_Until B$esi = ColonSign
                        mov B$ebx 0
                    End_If

                .End_If
            ..End_If

        ...Else_If B$esi = OpenBracket
            mov ebx esi | inc ebx
            While B$ebx > LowSigns | inc ebx | End_While
            If B$ebx = meEOI
                While B$esi <> CloseBracket | movsb | End_While
            End_If

        ...Else_If B$esi = '@'
            If B$esi-1 < LowSigns
                mov ebx LastFoundLabel
                While B$ebx <> 0
                    mov al B$ebx | inc ebx | stosb
                End_While
            Else_if B$esi-2 < LowSigns
                mov al memMarker | stosb
                mov ebx LastFoundLabel
                While B$ebx <> 0
                    mov al B$ebx | inc ebx | stosb
                End_While
            End_If

            Do | movsb | Loop_Until B$esi < LowSigns
        ...End_If

        movsb
    .End_While

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
[VirtualDataFlag: B$ ?]

IdentifyVirtualData:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = OpenBracket
            mov edx esi, B$VirtualDataFlag &FALSE
            .Do
                inc esi
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                Else_If B$esi = '{'
                    Do | inc esi | Loop_Until B$esi = '}'
                Else_If B$esi = '?'
                    mov B$VirtualDataFlag &TRUE
                End_If
            .Loop_Until B$esi = CloseBracket

            If B$VirtualDataFlag = &TRUE
                mov B$edx OpenVirtual, B$esi CloseVirtual
            End_If
        .End_If
        inc esi
    .End_While
ret
________________________________________________________________________________________________
; The source in CodeSourceA is reorders to: Data/Equates/Macro, Virtual Data, Code..

ReorderSource:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    move D$StatementsPtr D$StatementsTable, D$StatementsPtr2  D$StatementsTable2

; Copy Brackets to CodeSourceB:

  ; this might be needed to skip first EOI if present.
    On B$esi = EOI, inc esi

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign | inc esi

        .Else_If B$esi = OpenBracket
            lea edx D$esi+1

            .Do
                If B$esi = TextSign
                    Do | movsb | Loop_Until B$esi = TextSign
                End_If
                movsb

            .Loop_Until B$esi = CloseBracket
            movsb

            mov eax D$StatementsPtr, eax D$eax
            mov ebx D$StatementsPtr2, D$ebx eax
            add D$StatementsPtr2 4 | On D$edx <> 'ZZZZ', add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseVirtual
            inc esi

            add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                add D$StatementsPtr 4
            EndIf
            inc esi
        .Else
            inc esi
        .End_If
    .End_While

    mov esi D$CodeSourceA

  ; Copy Virtual to CodeSourceB.
  ; this might be needed to skip first EOI if present.
    If B$esi = EOI
        inc esi
    End_If

    move D$StatementsPtr D$StatementsTable

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign | inc esi

        .Else_If B$esi = OpenBracket
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseBracket
            inc esi

            add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            lea edx D$esi+1

            .Do
                If B$esi = TextSign
                    Do | movsb | Loop_Until B$esi = TextSign
                End_If
                movsb
            .Loop_Until B$esi = CloseVirtual
            movsb

            mov eax D$StatementsPtr, eax D$eax
            mov ebx D$StatementsPtr2, D$ebx eax
            add D$StatementsPtr2 4 | On D$edx <> 'ZZZZ', add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                add D$StatementsPtr 4
            EndIf
            inc esi

        .Else
            inc esi

        .End_If
    .End_While


    mov B$edi EOI
    inc edi

    mov esi D$CodeSourceA

  ; Copy the other statements to CodeSourceB.
  ; this might be needed to skip first EOI if present.
    If B$esi = EOI
        inc esi
    End_If

    move D$StatementsPtr D$StatementsTable

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb

        .Else_If B$esi = OpenBracket
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseBracket
            inc esi
            add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseVirtual
            inc esi
            add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                mov eax D$StatementsPtr, eax D$eax
                mov ebx D$StatementsPtr2
                mov D$ebx eax

                add D$StatementsPtr 4
                add D$StatementsPtr2 4
            EndIf
            movsb

        .Else
            movsb

        .End_If
    .End_While

    mov ecx edi
    sub ecx D$CodeSourceB
    mov D$StripLen ecx

    mov eax D$StatementsPtr2, D$eax 0

    Exchange D$CodeSourceA D$CodeSourceB
    Exchange D$StatementsTable D$StatementsTable2

    call StripNewlyAddedUnneededEOI
ret
____________________________________________________________________________________________

; called by 'ReorderSource' only. 'StripUnneededEOI' ('HotParsers') is a bit similar.

StripNewlyAddedUnneededEOI:

    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb

        .Else_If B$esi = EOI
            ..If B$esi+1 = EOI
                inc esi
            ..Else
                movsb
            ..End_If

        .Else
            movsb

        .End_If
    .End_While

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

  ; Write the end mark '||':
    mov B$edi EOI | inc D$Striplen
    If B$edi-1 <> EOI
        mov B$edi+1 EOI | inc D$Striplen
    End_If

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
____________________________________________________________________________________________

NewPrepareExport:
    VirtualFree D$ExportListAPtr, D$ExportListBPtr, D$ExpOrdArray ;D$ExportListPointers
;int3
    call NewHowManyExport

    If B$ExportsectionWanted = &TRUE
        call NewStoreToExportListA
    End_If

    If D$NumberOfExportedFunctions = 0
        mov B$ExportsectionWanted &FALSE
        VirtualFree D$ExportListAPtr, D$ExportListBPtr, D$ExpOrdArray ;, D$ExportListPointers
    End_If
ret


NewHowManyExport:
    mov B$ExportsectionWanted &FALSE
    mov B$ErrorLevel 0
    mov esi D$CodeSourceA, ecx esi, ebx 0, edx 0 | add ecx D$StripLen

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If W$esi = '::'
            If_Or  B$esi-1 = '|', B$esi-1 <= ' '
                mov B$esi+2 EOI | jmp OnNoSpaceBeforeColonError
            End_If
            If B$esi+2 > ' '
                mov B$esi+3 EOI | jmp OnNeedSpaceAfterColonError
            End_If
            cmp B$esi-2 '|' | je L0> ; case A:
            cmp B$esi-2 ' ' | jbe L0>
            cmp B$esi-3 '|' | je L2> ; case L9:: and FS::
            cmp B$esi-3 '[' | je L2>
            cmp B$esi-3 ']' | je L2>
            cmp B$esi-3 ' ' | ja L0>
L2:         mov ax W$esi-2
            cmp ah '9' | ja L2> | cmp ah '0' | jae E1>
L2:         or eax 02020 | cmp ah 's' | jne L0>
            cmp al 'c' | je E1> | cmp al 'd' | je E1> | cmp al 'e' | je E1>
            cmp al 's' | je E1> | cmp al 'f' | je E1> | cmp al 'g' | je E1>
            jmp L0>
E1:         sub esi 2| jmp OnLocalExpotingError
           ; if full Export-Name, count it
L0:         If B$esi+2 = TextSign
               push esi | add esi 2
               Do | inc esi | inc ebx | Loop_Until B$esi = TextSign
               inc ebx | pop esi
            End_If

L0:         cmp B$esi-1 ' ' | jbe L1>
            cmp B$esi-1 '[' | je L1>
            cmp B$esi-1 ']' | je L1>
            cmp W$esi-2 '[<' | je L1>
            cmp B$esi-1 '|' | je L1>
            dec esi | jmp L0<
L1:
            While W$esi <> '::' | inc esi | inc ebx | End_While
            add ebx 2 | inc edx | inc esi

        .Else_If B$esi = ':'
            If_Or  B$esi-1 = '|', B$esi-1 <= ' '
                mov B$esi+1 EOI | jmp OnNoSpaceBeforeColonError
            End_If
            cmp B$esi-2 '|' | je L0> ; case A:
            cmp B$esi-2 ' ' | jbe L0>
            cmp B$esi-2 '&' | je L8> ; case in Macros J&&:
            cmp B$esi-3 ' ' | jbe L8> ; case L9: and FS:D$
            cmp B$esi-3 '|' | je L8> ; case L9
            cmp B$esi-3 '$' | je L8> ; case D$FS:
L0:         If B$esi+1 > ' '
E0:             mov B$esi+2 EOI | jmp OnNeedSpaceAfterColonError
            End_If
            cmp B$ExportALL 0 | je L8>
;            While B$esi-1 > ' ' | dec esi | End_While
L0:         cmp B$esi-1 ' ' | jbe L1>
            cmp B$esi-1 '[' | je L1>
            cmp B$esi-1 ']' | je L1>
            cmp W$esi-2 '[<' | je L1>
            cmp B$esi-1 '|' | je L1>
            cmp B$esi-1 ':' | je E0< ; prevents loop in case A:B:
            dec esi | jmp L0<
L1:
            mov eax D$esi | or eax 0202020 | cmp eax 'ord0' | je OnBadExportOrdinal
            While B$esi <> ':' | inc esi | inc ebx | End_While
            add ebx 2 | inc edx
L8:
        .End_If
        inc esi
    .End_While

    If edx > 0
        mov B$ExportsectionWanted &TRUE
        mov D$NumberOfExportedFunctions edx
        mov D$ExportSectionLen ebx
        shl edx 2 | VirtualAlloc ExpOrdArray edx
        VirtualAlloc ExportListAPtr D$ExportSectionLen
    End_If
ret


OnNoSpaceBeforeColonError:
L0: cmp B$esi-1 ' ' | jbe L1> | cmp B$esi-1 '|' | je L1> | dec esi | jmp L0<
L1: mov B$esi-1, EOI
    mov eax esi | sub eax D$CodeSourceA | add eax D$CodeSource
    mov ecx D$StatementsPtr | mov D$ecx eax
    Error D$NoSpaceBeforeColonPtr, esi

OnNeedSpaceAfterColonError:
L0: cmp B$esi-1 ' ' | jbe L1> | cmp B$esi-1 '|' | je L1> | dec esi | jmp L0<
L1: mov B$esi-1, EOI
    mov eax esi | sub eax D$CodeSourceA | add eax D$CodeSource
    mov ecx D$StatementsPtr | mov D$ecx eax
    Error D$NeedSpaceAfterColonPtr, esi

OnLocalExpotingError:
    mov B$esi-1 EOI
    mov eax esi | sub eax D$CodeSourceA | add eax D$CodeSource
    mov ecx D$StatementsPtr | mov D$ecx eax
    While B$esi <> ':' | inc esi | End_While | mov B$esi+2 EOI
    Error BadLabel, esi


NewStoreToExportListA:
    mov esi D$CodeSourceA, edi D$ExportListAPtr;, ebx D$ExportListPointers
    mov ecx esi | add ecx D$StripLen
    and D$NumberOfJunkBytes 0 | and D$NumberOfExportedFunctions 0; count again accepted ones
    and D$OnlyOrdinalExport 0 | and D$OrdinalExport 0 | mov B$ErrorLevel 0

    ..While esi < ecx
        ..If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        ..Else_If W$esi = '::'
;            While B$esi-1 > ' ' | dec esi | End_While
L0:         cmp B$esi-1 ' ' | jbe L1>
            cmp B$esi-1 '|' | je L1>
            cmp B$esi-1 ']' | je L1>
            dec esi | jmp L0<
L1:
            If B$esi = '@'
                jmp OnLocalExpotingError
            End_If
            mov ebx edi ; mov D$ebx edi | add ebx 4 ; store Pointer jE!
            mov eax D$esi | or eax 0202020 | cmp eax 'ord0' | jne L0>
            call CheckExportOrdinal | add D$NumberOfJunkBytes eax
L0:         While B$esi <> ':' | movsb | End_While | inc esi
            mov al EOI | stosb
           ; if full Export-Name, append it
            If B$esi+1 = TextSign
               mov eax edi | sub eax ebx | inc eax ; D$ebx-4
               add D$NumberOfJunkBytes eax | inc esi
               Do | movsb | mov B$esi-1 ' ' | Loop_Until B$esi = TextSign
               movsb | mov B$esi-1 ' ' | dec esi
            End_If
            inc D$NumberOfExportedFunctions
        ..Else_If B$esi = ':'
            cmp B$ExportALL 0 | je L8>
            cmp B$esi-2 '&' | je L8>
            cmp B$esi-3 ' ' | jbe L8>
            cmp B$esi-3 '|' | je L8>
            cmp B$esi-3 '$' | je L8>
;            While B$esi-1 > ' ' | dec esi | End_While
L0:         cmp B$esi-1 ' ' | jbe L1>
            cmp B$esi-1 '|' | je L1>
            cmp B$esi-1 ']' | je L1>
            dec esi | jmp L0<
L1:
            If B$esi = '@'
                While B$esi <> ':' | inc esi | End_While
                jmp L8>
            End_If
            ;mov D$ebx edi | add ebx 4 ; store Pointer jE!
            While B$esi <> ':' | movsb | End_While
            mov al EOI | stosb
            inc D$NumberOfExportedFunctions
L8:
        ..Else_If B$esi = '['
            .Do
                .If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                .Else_If W$esi = '::'
L0:                 ;While B$esi-1 > ' ' was incorrect! needs '[' check
                    cmp B$esi-1 ' ' | jbe L1>
                    cmp B$esi-1 '[' | je L1>
                    cmp W$esi-2 '[<' | je L1>
                    cmp B$esi-1 '|' | je L1>
                    dec esi | jmp L0<
L1:
;                    On B$esi = '[', inc esi
                    If B$esi = '@'
                        jmp OnLocalExpotingError
                    End_If
                    mov ebx edi ;mov D$ebx edi | add ebx 4 ; store Pointer jE!
                    mov eax D$esi | or eax 0202020 | cmp eax 'ord0' | jne L0>
                    call CheckExportOrdinal | add D$NumberOfJunkBytes eax
L0:                 While B$esi <> ':' | movsb | End_While | inc esi
                    mov al colonSign ; mark DataLABEL
                    mov ah EOI | stosw
                    mov B$esi Space
                  ; if full Export-Name, append it
                    If B$esi+1 = TextSign
                       mov eax edi | sub eax ebx ;inc eax ;D$ebx-4
                       add D$NumberOfJunkBytes eax | inc esi
                       Do | movsb | mov B$esi-1 ' ' | Loop_Until B$esi = TextSign
                       movsb | mov B$esi-1 ' ' | dec esi
                    End_If
                    inc D$NumberOfExportedFunctions, D$NumberOfJunkBytes ; for SUB markers
                .Else_If B$esi = ':'
                    cmp B$ExportALL 0 | je L8>
                    cmp B$esi-2 '&' | je L8>
                    cmp B$esi-3 ' ' | jbe L8>
                    cmp B$esi-3 '|' | je L8>
L0:
                    cmp B$esi-1 ' ' | jbe L1>
                    cmp B$esi-1 '[' | je L1>
                    cmp W$esi-2 '[<' | je L1>
                    cmp B$esi-1 '|' | je L1>
                    dec esi | jmp L0<
L1:
;                    On B$esi = '[', inc esi
                    If B$esi = '@'
                        While B$esi <> ':' | inc esi | End_While
                        jmp L8>
                    End_If
                    ;mov D$ebx edi | add ebx 4 ; store Pointer jE!
                    While B$esi <> ':' | movsb | End_While
                    mov al colonSign ; mark DataLABEL
                    mov ah EOI | stosw
                    inc D$NumberOfExportedFunctions, D$NumberOfJunkBytes
L8:
                .End_If
                inc esi
            .Loop_Until B$esi = ']'

        ..End_If
        inc esi
    ..End_While
;int3
    ON D$NumberOfExportedFunctions = 0, ret
    cmp D$NumberOfExportedFunctions 010000 | jb L4>
    mov eax D$TooManyExportsPtr | jmp OutOnError
L4: ; calculate Export size
    sub edi D$ExportListAPtr ;| mov ecx edi
    sub edi D$NumberOfJunkBytes | mov D$ExportSectionLen edi

    mov ecx 1 | mov eax D$NumberOfExportedFunctions | cmp D$OrdinalExport 0 | je L4>>

    mov edx D$ExpOrdArray, ecx D$OrdinalExport | shl ecx 2 | call BubbleSort edx, ecx
    sub ebx ebx | mov ecx D$OrdinalExport

    mov edi D$edx+ecx*4-4 | mov ecx D$edx
; can other Exports fit up   from Lo-Ord >> 0FFFF?
L1: mov edx D$NumberOfExportedFunctions | dec edx | add edx ecx | cmp edx 010000 | jae L1>
    mov eax D$NumberOfExportedFunctions | mov edx edi | sub edx ecx | inc edx
    cmp eax edx | jae L4> | mov eax edx | jmp L4>
; fit down from Hi-Ord >> 1?
L1: mov edx edi | inc edx | sub edx D$NumberOfExportedFunctions | jbe L1>
    cmp edx ecx | jae L0> | mov ecx edx
L0: mov eax edi | sub eax ecx | inc eax | jmp L4>
; at last, start from 1..
L1: mov ecx 1 | mov eax D$NumberOfExportedFunctions
    cmp eax edi | jae L4> | mov eax edi | jmp L4>

L4: ; eax=full_exp_num - ecx=Ordinal_Base
    mov ebx D$ExportSectionLen | add ebx 40 ; strings_len+headers
    lea ebx D$ebx+eax*4 ; + full-num of exports*4
    mov edx D$NumberOfExportedFunctions  | sub edx D$OnlyOrdinalExport
    lea edx D$edx+edx*2 | lea ebx D$ebx+edx*2 ; + num of export-names*6
    pushad
    mov edi ChoosenFile | sub eax eax | or ecx 0-1 | repne scasb | not ecx
    add ebx ecx | add ebx 4 ; +possible_extension
    mov D$ExportSectionLen ebx
    VirtualAlloc ExportListBPtr ebx
    popad
    mov edi D$ExportListBPtr | mov D$edi+EXPnBase ecx | mov D$edi+EXPNumberOfFunctions eax
    mov eax D$NumberOfExportedFunctions | sub eax D$OnlyOrdinalExport
    mov D$edi+EXPNumberOfNames eax
    add edi 40 | mov esi D$ExpOrdArray | mov edx ecx | mov ecx D$OrdinalExport
; Reserve in ExportFunctionList ORD_own place by ORD_num
L0: dec ecx | js L0> | lodsd | mov ebx eax | sub ebx edx | mov D$edi+ebx*4 eax | jmp L0<
L0: VirtualFree D$ExpOrdArray
ret


CheckExportOrdinal:
    push ecx, edi
    lea edi D$esi+4 | mov al ':' | mov ecx 5 | repne scasb | jne E0> ; no-more 4HEX
    lea edi D$esi+4 | sub edx edx
L1:
    mov al B$edi | inc edi | cmp al ':' | je L4> | or al 020
    cmp al 'f' | ja E0>  | cmp al '0' | jb E0>
    cmp al '9' | jbe L3> | cmp al 'a' | jb E0> | sub al 027
L3: sub al 030 | shl edx 4 | or dl al | jmp L1<
L4: test edx edx | je E0> | sub eax eax | cmp B$edi+1 TextSign | je L0>
    sub edi esi | mov eax edi | inc D$OnlyOrdinalExport | jmp L0>
OnBadExportOrdinal:
E0: mov edx D$BadExportOrdinalPtr
E1: mov B$esi-1 EOI
    mov eax esi | sub eax D$CodeSourceA | add eax D$CodeSource
    mov ecx D$StatementsPtr | mov D$ecx eax
    While B$esi <> ':' | inc esi | End_While | mov B$esi+2 EOI
    Error edx, esi
L0:
    xchg eax edx | mov edi D$ExpOrdArray | mov ecx D$OrdinalExport | mov D$edi+ecx*4 eax
    jecxz L0> | repne scasd | jne L0> | mov edx D$SameExportOrdinalPtr | jmp E1<
L0: xchg eax edx
    inc D$OrdinalExport
    pop edi, ecx
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
; This shouldn't be here, but is now for development purposes.

FromDataToStructure:
    mov D$DisScale 4, D$EquateValue 0

    call 'User32.GetDlgItemTextA' D$DataToStructureDialogHandle, 10, D$DataTextTable, 01000
    On eax < 10, ret

    mov B$WeAreInTheCodeBox &TRUE
;    push D$CodeSource, D$SourceLen, D$SourceEnd
        mov eax esp, D$OldStackPointer eax, B$CompileErrorHappend &FALSE

        mov eax D$DataTextTable
        While B$eax > 0
            inc eax
        End_While
        mov B$eax CR, B$eax+1 LF | add eax 2
        inc eax

        sub eax D$DataTextTable
        push eax
            call GetAsmTables
        pop eax
        call NewCopyToCodeSourceA D$DataTextTable, eax ; D$DataTextTableLen

        call Coolparsers

        call NewCountStatements

        On B$CompileErrorHappend = &TRUE, jmp L9>>

        call Hotparsers | On B$CompileErrorHappend = &TRUE, jmp L9>>


        mov esi D$CodeSourceA, edi D$StructureTextTable, D$FirstDataLabel 0

        On B$esi = OpenBracket, inc esi
        mov B$edi '[' | inc edi

L0:     .While B$esi > EOI
            mov ebx esi
            While B$ebx > LowSigns | inc ebx | End_While
            .If B$ebx = ColonSign
                On D$FirstDataLabel = 0, mov D$FirstDataLabel esi
                While B$esi <> ColonSign | movsb | End_While
                mov B$edi ' ' | inc edi
                mov eax D$EquateValue | call WriteEax
                mov B$edi CR, B$edi+1 LF, B$edi+2 ' ' | add edi 3
            .Else_If B$ebx = MemMarker
                If B$ebx-1 = 'D'
                    mov D$DisScale 4
                Else_If B$ebx-1 = 'W'
                    mov D$DisScale 2
                Else_If B$ebx-1 = 'U'
                    mov D$DisScale 2
                Else_If B$ebx-1 = 'B'
                    mov D$DisScale 1
                Else_If B$ebx-1 = 'Q'
                    mov D$DisScale 8
                Else_If B$ebx-1 = 'R'
                    mov D$DisScale 8
                Else_If B$ebx-1 = 'F'
                    mov D$DisScale 4
                Else_If B$ebx-1 = 'T'
                    mov D$DisScale 10
                End_If
            .Else_If B$ebx = NumSign
                inc esi
                If B$esi = '0'
                    call TranslateHexa
                Else
                    call TranslateDecimal
                End_If
                mul D$DisScale | sub eax D$DisScale | add D$EquateValue eax | jmp L1>>
            .Else_If B$esi = '?'
                mov eax D$DisScale | add D$EquateValue eax
            .Else_If B$esi < '0'

            .Else_If B$esi > '9'

            .Else
                mov eax D$DisScale | add D$EquateValue eax
                While B$esi > LowSigns | inc esi | End_While
            .End_If
            inc esi
L1:     .End_While
        On D$FirstDataLabel = 0, jmp L9>

        inc esi | cmp B$esi EOI | ja L0<<

L2:     mov B$edi CR, B$edi+1 LF | add edi 2
        mov esi D$FirstDataLabel
        While B$esi <> ColonSign
            On B$esi = 0, jmp L9>
            movsb
        End_While
        mov D$edi 'SIZE', B$edi+4 ' ' | add edi 5
        mov eax D$EquateValue | call WriteEax
        mov B$edi ']', B$edi+1 0
        call 'User32.SetDlgItemTextA' D$DataToStructureDialogHandle, 11, D$StructureTextTable

L9:     call ReleaseAsmTables

;    pop D$SourceEnd, D$SourceLen, D$CodeSource
    mov B$WeAreInTheCodeBox &FALSE
ret

____________________________________________________________________________________________

EncodeDecode:
    mov B$WeAreInTheCodeBox &TRUE
;    push D$CodeSource, D$SourceLen, D$SourceEnd
      ; ('AsmMain' 'OutOnError')
        mov eax esp, D$OldStackPointer eax, B$CompileErrorHappend &FALSE

; What on earth is EncodeSource???
        mov eax EncodeSource ;, D$CodeSource eax
        While B$eax > 0
            inc eax
        End_While
        mov B$eax CR, B$eax+1 LF | add eax 2
        inc eax

;        mov D$SourceEnd eax |
[EncodeSourceLen: D$ ?]
        sub eax EncodeSource | mov D$EncodeSourceLen eax

        call GetAsmTables
        call NewCopyToCodeSourceA EncodeSource D$EncodeSourceLen

        call Coolparsers

        call NewCountStatements

        call ClearQwordCheckSum

        On B$CompileErrorHappend = &TRUE, jmp L9>>

        call Hotparsers

        On B$CompileErrorHappend = &TRUE, jmp L9>>
        call InitIndex1 | call InitIndex2

        Exchange D$CodeSourceA D$CodesourceB
        push D$SourceLen
            move D$SourceLen D$EncodeSourceLen
            move D$AsmTablesLength D$SourceLen
            call ReuseSourceAForCodeList
        pop D$SourceLen
        call InitIndex3

        call BuildData                          ; result 'CodeSourceB' > 'CodeSourceB'

        On B$CompileErrorHappend = &TRUE, jmp L9>>
        call InitDebugIpTable
        mov B$ErrorLevel 7                      ; For outOnError, Error

        call ReCodeLine | On B$CompileErrorHappend = &TRUE, jmp L9>>

      ; Prepare Text to show in the Code Hexa view:
        mov esi D$CodeOrigine, edi HexaCodeText
        While esi < D$CodeListPtr
            movzx eax B$esi | inc esi
            mov ebx eax | shr ebx 4
            and eax 0F | and ebx 0F
            mov al B$HexaTable+eax, bl B$HexaTable+ebx
            shl eax 8 | or eax ebx | or eax 020200000 | stosd
        End_While
        mov D$edi 0

      ; Disassemble: (DisMain)
        mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
        mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
        mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
        mov esi D$CodeOrigine, edi DecodeText
L0:     movzx eax B$esi | inc esi | call D$DisOp1+eax*4
        On B$DisFlag = DISDONE, jmp L0<
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
L0:             movsb | cmp edi DecodeText | jae L0<
            cld
            mov D$DummyDecodeText 0, D$DummyDecodeText+4 0
        End_If

L9:     call ReleaseAsmTables

;    pop D$SourceEnd, D$SourceLen, D$CodeSource
    mov B$WeAreInTheCodeBox &FALSE

    mov D$EncodeSecurity 0 ; Ensure the Buffer never overflows
ret


EncodeError:
L0: mov ebx, esp | cmp ebx, D$OldStackPointer | jnb L1>
    pop ebx | jmp L0<
L1: sub esp 8 | call ErrorMessageBox 0, D$ErrorMessagePtr
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; Some routines I haven't got round to deleting. :)

[StripLen: ?  TextDelimiter: ?  TextGoingOn: ?]

IsItFirstText:
    cmp B$TextGoingOn &FALSE | je L1>            ; if OFF > test if ON  needed
L0:   cmp al, B$TextDelimiter | jne L9>         ; if ON  > test if OFF needed
        Jmp L3>
L1: cmp al '"' | je L2>
      cmp al "'" | jne L9>
L2: mov B$TextDelimiter al
L3: mov al, TextSign | xor B$TextGoingOn &TRUE
L9: cmp B$TextGoingOn &TRUE
ret

IsItText:                                       ; called by many routines after cleaner
    cmp al, TextSign | jne L9>
L2: xor B$TextGoingOn &TRUE
L9: cmp B$TextGoingOn &TRUE
    ret

[IfItIsText | cmp al TextSign | jne M9>         ; macro a bit faster than 'call IsIttext'
 M0: stosb | lodsb | cmp al TextSign | jne M0<     ; when it fits
    jmp #1
 M9: ]


; same as IsItFirstText, but simplified: no error check, no modification of AL. This
; is for other text ruling routines:

____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________


