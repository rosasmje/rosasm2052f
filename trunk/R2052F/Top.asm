
;;

                               R O S A S M    S O U R C E

_____________________________________________________________________________________________
_____________________________________________________________________________________________


 RosAsm: The Top Down Assembler for ReactOS PE files.

 This program is free open source.

 First Author: René Tournois.
 
 (i begun to work on SpAsm, the ancestor of RosAsm, in September 1998)

 Actual maintainer: René Tournois.

____________________________________________________________________________________________
____________________________________________________________________________________________

         Copyright (C) 1998, René Tournois

         This program is free software; you can redistribute it and/or modify
         it under the terms of the GNU General Public License as published by
         the Free Software Foundation.

         This program is distributed in the hope that it will be useful,
         but WITHOUT ANY WARRANTY; without even the implied warranty of
         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
         GNU General Public License included in B_U_Asm.exe for more details.

         You should have received a copy of the GNU General Public License
         along with this program; if not, write to the Free Software
         Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

Rules:

;;
  Developments rules for the volunteers contributing to RosAsm itself


  * In order to give the Source a bit of shape, it is wishable to apply a consistent
    writing Style, where each part would try to follow up, with a couple of rules.
    See an example of the actualy choosen Style with, 'CompactLabelListAndCodeRef':
    
    - Indent must be 4 Spaces.
    
    - The Text of the Multi-Lines Comments are two spaces away from the left margin.
    
    - Mono-Lines Comments start two Spaces before the next (or previous Statement).
      Never: Comment at the end of lines.
      A comment for the next Statement(s) end(s) with a column (":")
      A comment for the previous Statement ends with a "."
      Comments must be "english only".
      Comments start with a Space after the ";".
      
    - Old fashion vertical alignements of the Statements members: NEVER.
      
    - Symbolic Names: Never: "Routine_1", "Buffer", "Variable", and so on. Never,
      ever. The Symbols must be in the form of 'InitLabelListScanDwordPointer'
      (no '_', upper case for each leading char of a component), and should be as
      self-descriptive as possible. English required.
    
    - Equates must be UPPER_CASE. ['_' tolerated inside Equates when wishable for
      readability].
      
    - Separators: Make use of [F8] only. Avoid "---------" and friends.
    
    - There still exist users with a small screen and low resolution. So, the
      Source must not be too wide. 80 Chars is a good base.
      
    - For the same reasons as above, do not use anything like "MMX and above"
    
    - Verticaly, try to make your source optimal with the most proper usage
      of blank lines.
      
    - A multi-Instructions line should "make sense": Try to not group instructions
      which have completely different purposes, and to group the ones which concure
      to a given action, such as you could give it a kind of 'name', if you were
      making it a Routine.


  * Macros:
  
    - Do not implement your own Macros.
    
  
  * At top of your TITLE, we should found, at least:
  
    - The Name of the maintainer (yours).
      
    - The Date of the first release.
      
    - The Data of the actual release.
      
    - An organized list of the TITLE Routines. See an example at the top
      of the TITLE [Optimize], above 'InitShortenJmpsTable'.


  * For exchanging (updates) with the main maintainer:
  
    - For the TITLE you make a [Ctrl][S], and save your "MyTitle.asm". RosAsm is
      full featured for this. Sending a zip of RosAsm.exe is never necessary.
      
    - For Dialogs, make use of Binary Saving Format (Menu).
      
    - The zips of these Files are all that should be necessary for the updates.
      
    - When a new implementation requires to modify something in the main Source
      (branching, typically), this is the job of the main maintainer to do that.
  
  
  * Developments "property":

    At any given time, there can only be _one_ volunteer "touching" a TITLE.
    So, the very first step, before anything else, is about defining a new TITLE
    with the main maintainer. Once done, this TITLE is your "private area", and
    nobody else is allowed to touch it as long as you are the active maintainer.
    
    If two volunteers wish to work upon the same TITLE, the TITLE must be "splitted"
    to create "each one, each private room, each provate task", even if the two TITLEs
    will be merged, later. This simple method is very helpfull for defining "who does
    what".
;;
_____________________________________________________________________________________________
_____________________________________________________________________________________________
; Colors:

[DialogsBackColor: 0_FF_FC_F2]         ; For Edit Controls, List Boxes (Default: Light Blue)

                                       ; Source Editor colors:
[NormalBackColor:  0_FF_FF_FF          ; white BackGround color (Default: light yellow)
 StatementColor: 0                     ; black for instructions
 BracketColor: 0A0                     ; [red] for Data / Equates / Macros
 TextColor: 0_64_00                    ; 'green' for text
 CommentColor: 0_82_00_00 ]            ; blue for comments


[DRAWLINELEN 92]    ; This is the number of '_' in a Line drawn by [Ctrl][_]

[UPPERCASEMASK 0DF] ; (not 020) applied on a Byte.

;;
 To modify the default font used by the [Print] feature, Right Click on >>>  cbbuffer  <<<
 and do what you can there. Be aware that specifying a font for a Printer under Win32
 is absolutely crazy; i won't help you... Welcome to the one who could rewrite this
 in RosAsm.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; Displacements inside a 'SectionsHeaders': ; 'PeHeader'

[SECTION_NAME 0
 SECTION_RVASIZE 8
 SECTION_RVA 12
 SECTION_FILESIZE 16
 SECTION_FILEPOINTER 20

 SECTION_RELOC_PTR 24
 SECTION_LINES_NUMBER_PTR 28
 SECTION_RELOC_NUMBER 32
 SECTION_NUMBER_OF_LINES_NUMBER 34

 SECTION_FLAG 36

 SECTIONHEADERSIZE 40]
_____________________________________________________________________________________________
_____________________________________________________________________________________________

;;
 Used abreviations:

 EOI,      End Of Instruction
 meEOI,    Macro Expension EOI
 CR,       Carriage Returrn
 LF,       Line Feed
 ALEOD,    Api Lists End Of Data
 RVA,      Relative Virtual Adress
 Len,      Lenght

 Usual Tintel abreviations:

 SIB,      Scale_Base_Index Byte
 Mod/RM,   Mode and Register/Memory Byte
 tttnBits  condition encodage


 MLC       Multi-Lines Comments
;;

_____________________________________________________________________________________________
_____________________________________________________________________________________________

; General purpose macros:

[push | push #1 | #+1]  [pop | pop #1 | #+1]

[mov | mov #1 #2 | #+2]

[inc | inc #1 | #+1]    [dec | dec #1 | #+1]

[On | cmp #1 #3 | jn#2 M1> | #4>L | M1: ]

[call | push #L>2 | call #1]

[move | push #2 | pop #1 | #+2]      ; (for mem to mem moves, for exemple)

[If | #=3 | cmp #1 #3 | jn#2 I1>]
[Else_if | #=3 | jmp I9> | I1: | cmp #1 #3 | jn#2 I1>]
[Else | Jmp I9> | I1: ]
[End_if | I1: | I9: ]

[.If | #=3 | cmp #1 #3 | jn#2 J1>>]
[.Else_if | #=3 | jmp J9>> | J1: | cmp #1 #3 | jn#2 j1>>]
[.Else | Jmp j9>> | j1: ]
[.End_if | j1: | j9: ]

[..If | #=3 | cmp #1 #3 | jn#2 K1>>]
[..Else_if | #=3 | jmp K9>> | K1: | cmp #1 #3 | jn#2 K1>>]
[..Else | Jmp K9>> | K1: ]
[..End_if | K1: | K9: ]

[...If | #=3 | cmp #1 #3 | jn#2 Z1>>]
[...Else_if | #=3 | jmp Z9>> | Z1: | cmp #1 #3 | jn#2 Z1>>]
[...Else | Jmp Z9>> | Z1: ]
[...End_if | Z1: | Z9: ]

[While | #=3 | W0: | cmp #1 #3 | jn#2 W9>]
[End_While | jmp W0< | W9: ]

[.While | #=3 | X0: | cmp #1 #3 | jn#2 X9>>]
[.End_While | jmp X0<< | X9: ]

[..While | #=3 | Y0: | cmp #1 #3 | jn#2 Y9>>]
[..End_While | jmp Y0<< | Y9: ]

[Do | D0: ]
[Loop_Until | #=3 | cmp #1 #3 | jn#2 D0<]
[Do_Loop | jmp D0<<]

[.Do | E0: ]
[.Loop_Until | #=3 | cmp #1 #3 | jn#2 E0<<]

[Exchange | push #1 | push #2 | pop #1 | pop #2 | #+2]


[Agree | cmp #1 #3 | j#2 A9> | #+3]
[Reject | cmp #1 #3 | j#2 A8> | #+3 | jmp A9> | A8: | ret | A9: ]

_________________________________________________________________________________________
_________________________________________________________________________________________
; Proc Macros and Equates. Internal storages are:
;
; &1 <<< Size of Argument(s) (for ending Ret n, in EndP). Set by Argument(s)
; &2 <<< Size of Local (for Stack Management). Set by Local
; &3 <<< What to pop before ret. Set by Uses.

[Proc | &1=0 | &2=0 | &3= | #1 | push ebp | mov ebp esp]

[ExitP | jmp P9>>]

[Arguments | {#1 ebp+4+(#x shl 2)} | #+1 | &1=(#N shl 2)]
[Argument  | {#1 ebp+4+(#x shl 2)} | #+1 | &1=(#N shl 2)]

[Local | {#1 ebp-(#x shl 2)} | #+1 | &2=(#N shl 2) | sub esp &2]

[StrucPtrs | {#3 ebp+#2+#F} | #+2]

[Structure | {#1 ebp-&2-4} | sub esp #2+4 | mov D$#1 esp | StrucPtrs 0-&2-#2-4 #L>3]

[Uses | push #1>L | &3=pop #L>1]

[EndP | P9: | &3 | mov esp ebp | pop ebp | ret &1]

; For pointing to transmitted parameters (upper "Arg#x" fall here):

;[Arg1 ebp+8    Arg2 ebp+12    Arg3 ebp+16    Arg4 ebp+20   Arg5 ebp+24
; Arg6 ebp+28   Arg7 ebp+32    Arg8 ebp+36    Arg9 ebp+40   Arg10 ebp+44]

; For pointing Local Stack declared data (upper "Local#x" fall here):

;[Local1 ebp-4     Local2 ebp-8     Local3 ebp-12    Local4 ebp-16    Local5 ebp-20
; Local6 ebp-24    Local7 ebp-28    Local8 ebp-32    Local9 ebp-36    Local10 ebp-40]

; To help preventing from stack sizes' mistakes (upper "SizeOf#x" fall here):

;[SizeOf1 4     SizeOf2 8     SizeOf3 12    SizeOf4 16    SizeOf5 20
; SizeOf6 24    SizeOf7 28    SizeOf8 32    SizeOf9 36    SizeOf10 40]

____________________________________________________________________________________________
____________________________________________________________________________________________
; Equates for HLL comparisons (with 'If' and friends):

[= e   < b    > a    <s l    >s g    =< be    <= be    => ae    >= ae    <> ne]

_____________________________________________________________________________________________
_____________________________________________________________________________________________

; Some basic System calls:

[PrintErrorCode | call 'KERNEL32.GetLastError' | hexprint eax]

 ________________________________________________________________________________________

; Messages:

; in: eax = string adress

MessageBox:
    call 'USER32.MessageBoxA' D$hwnd,               ; hwin
                             eax,                   ; Message
                             D$ErrorMessageTitlePtr,     ; Message-Window-Title
                             &MB_SYSTEMMODAL        ; Style (0 to 4) 0 > 'OK'
ret
 ________________________________________________________________________________________

[HexprintString: '        h', 0]

HexPrn:
    mov ebx, eax | mov edi HexPrintString | add edi 7
    std
        mov ecx, 8
L1:     mov al bl | and al 0F | On al >= 0A, add al 7
        add al, '0' | stosb | shr ebx, 4 | loop L1
    cld
    call 'USER32.MessageBoxA' D$hwnd, HexPrintString, ErrorMessageTitle, &MB_SYSTEMMODAL
ret

[HexPrint | pushad | push #1 | pop eax | call hexprn | popad | #+1]
[ShowMe | pushad  | call 'USER32.MessageBoxA' D$hwnd, #1, argh, &MB_SYSTEMMODAL | popad]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Replaces a Chunk in an existing Data set, by another Chunk.

Proc ChunkReplace:
    Argument @TargetA,          ; Original Buffer insertion start Point (first Byte)
             @TargetB,          ; Original Buffer insertion End Point (after last Byte)
             @TargetEnd,        ; Original Buffer end (Byte after last valid content)
             @TargetMemEnd,     ; Original Buffer Memory limit (after last Byte)
             @SourceA,          ; Substitute Start Point (first Byte)
             @SourceB           ; Substitute End Point (after last Byte)
    Local @NewEnd
    Uses esi, edi, ecx, edx

        mov ecx D@SourceB | sub ecx D@SourceA
        mov edx D@TargetB | sub edx D@TargetA
        move D@NewEnd D@TargetEnd | add D@NewEnd ecx | sub D@NewEnd edx
        .If ecx > edx
          ; If the new Chunk is bigger than the old one:
            sub ecx edx
            mov edi D@TargetEnd | dec edi | mov esi edi | add edi ecx

            If edi >= D@TargetMemEnd
                mov eax &FALSE | ExitP
            End_If

            mov ecx D@TargetEnd | sub ecx D@TargetB | inc ecx | std | rep movsb | cld

        .Else_If ecx < edx
          ; If the new Chunk is smaller than the old one:
            xchg ecx edx | sub ecx edx
            mov edi D@TargetB, esi edi | sub edi ecx

            mov ecx D@TargetEnd | sub ecx D@TargetB | rep movsb
        .End_If

      ; Now, Copy the Chunk:
        mov esi D@SourceA, edi D@TargetA
        mov ecx D@SourceB | sub ecx D@SourceA | jecxz L9>
            mov edx ecx | shr ecx 2 | rep movsd
            mov ecx edx | and ecx 00_11 | jecxz L9>
                rep movsb

L9:     mov eax D@NewEnd, B$eax 0 | mov eax &TRUE
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[NewMen: ?]

[Surprise: U$ 048, 06F, 077, 020, 061, 072, 065, 020, 079, 06F, 075,
              020, 04D, 061, 073, 074, 065, 072, 020, 050, 064, 066,
              03F, 0
 SurpriseNchars: D$ 017]

[TABLES_SECURITY 100]

; Tag Unicode Surprise

Proc ExtendTableMemory:
    Argument @Mem, @Pointer, @End
    Uses ecx, ebx, esi, edi

        If B$Dynamic = &FALSE
            call 'USER32.MessageBoxW' D$hwnd, Surprise, Surprise, &MB_OK
            call CloseProgressBar
            mov esp D$OldStackPointer | ret
        End_If

        mov eax D@Mem, eax D$eax
        push eax
            mov ecx D@Pointer, ecx D$ecx
          ; Get the actual Size, multiply by 2, and add the security:
            sub ecx eax | shl ecx 1
            push ecx
                add ecx TABLES_SECURITY | VirtualAlloc NewMen ecx
              ; Copy:
                mov edi eax
                mov esi D@Mem, esi D$esi
                mov ecx D@End, ecx D$ecx | sub ecx esi | Align_On 01000, ecx
                shr ecx 2 | rep movsd
            pop ecx
          ; Save the new Mem Limit:
            add ecx D$NewMen | mov eax D@End, D$eax ecx
          ; Adjust and Save the new follow-up Pointer:
            mov ebx D@Mem, ebx D$ebx
            mov eax D@Pointer, eax D$eax
            sub eax ebx | add eax D$NewMen
            mov ebx D@Pointer, D$ebx eax
          ; Save the new Mem:
            mov eax D$NewMen | mov ebx D@Mem, D$ebx eax
        pop eax
        VirtualFree eax
Endp
____________________________________________________________________________________________
____________________________________________________________________________________________

; Enough for small Tables:

Proc BubbleSort:
    Arguments @Array, @Size ; In Bytes!
    Uses eax, ebx, ecx, edi

        mov edi D@Array, ecx D@Size | shr ecx 2 | jecxz L9>

L0:     lea ebx D$edi+ecx*4 | mov eax D$edi

L1:     sub ebx 4 | cmp eax D$ebx | jle L2>
            xchg eax D$ebx

L2:         cmp ebx edi | jne L1<

        stosd | loop L0<
L9: EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc zStringsSort:
    Argument @Source, @Destination, @Number

        mov ecx D@Number, edi D@Destination

L0:     push ecx
            mov esi D@Source, ecx D@Number, edx 0, bl 0FF

L1:         lodsb

            .If al = 0FF
                ; nop
            .Else_If al < bl
                mov bl al | lea edx D$esi-1
            .Else_If al = bl
                push ebx
                    push edx, esi
                        While al = bl
                            lodsb | inc edx | mov bl B$edx
                            cmp al 0 | je L2>
                        End_While
L2:                 pop esi, edx
                    On al < bl, lea edx D$esi-1
                pop ebx
            .End_If

            While B$esi <> 0 | inc esi | End_While | inc esi | loop L1<

            If edx > 0
                mov esi edx
                While B$esi <> 0
                    movsb | mov B$esi-1 0FF
                End_While

                mov B$edi 0 | inc edi
            End_If

        pop ecx | dec ecx | cmp ecx 0 | ja L0<<
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

WaitForUserAction:
L0: call 'USER32.PeekMessageA' FirstMSG, D$hwnd, 0, 0FFFF, &PM_REMOVE
    cmp D$FuMsg &WM_LBUTTONUP | je L9>
    cmp D$FuMsg &WM_RBUTTONUP | je L9>
    cmp D$FuMsg &WM_KEYDOWN | jne L0<
L9: ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[RosAsmFilesPath: ? #&MaxPath]

GetRosAsmFilesPath:
    push esi, edi
        mov esi EquatesName, edi RosAsmFilesPath
        While B$esi <> 0 | movsb | End_While
        While B$edi <> '\' | dec edi | End_While
        mov B$edi+1 0
    pop edi, esi
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Trash Buffers:

[TrashPointer: ?]
[TrashString: ? #80]
[Trash: Trash1: ? #10000] [Trash2: ? #10000] [Trash3: ? #10000]

ClearTrashTables:
    push edi, ecx, eax
        mov edi TrashString, ecx 20080, eax 0 | rep stosd
    pop eax, ecx, edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[GetHexaFromTextError: ?]

Proc GetHexaFromText:
    Argument @Buffer
    Uses esi

        mov esi D@Buffer, edx 0, B$GetHexaFromTextError &FALSE

        While B$esi > 0
            lodsb | On al > 'Z', sub al 020
            sub al '0' | On al > 9, sub al 7

            If al > 0F
                mov eax {'The Number should be HexaDecimal', 0}
                call MessageBox
                mov B$GetHexaFromTextError &TRUE | ExitP
            End_If
            shl edx 4 | or dl al
        End_While

        mov eax edx
EndP


[zCopy | mov esi #1 | While B$esi <> 0 | movsb | End_While | #+1]

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc ClearBuffer: ; (Guga: Instead of ClearCharMapData)
    Arguments @Buffer, @Size
    Uses edi, ecx

        mov edi D@Buffer, ecx D@Size, eax 0

        rep stosb
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

[RGB | (#1 or (#2 shl 8) or (#3 shl 16))]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Computes the Length of a zero-ended Ascii String. Regs under caller responsability:

StrLen: ; edi -> String // StrLenProc
    mov ecx 0-1, al 0
    repne scasb
    mov eax 0-2 | sub eax ecx      ; Lenght in eax.
ret
____________________________________________________________________________________________________________

; Same with regs under Proc responsability:

Proc StrLenProc:
    Arguments @Pointer
    Uses edi, ecx

        mov edi D@Pointer, ecx 0-1, al 0
        repne scasb
        mov eax 0-2 | sub eax ecx      ; Lenght in eax
EndP








