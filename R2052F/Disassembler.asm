TITLE Disassembler

;;
  'StripSectionsZeroEnd', 'FlagApiProcedures'
  
  'DisDataTypeRouter', 'MarkSSEdata'
 
 'HLL_Strings_Table_notes'
 'FlagApiProcedures', 'SetApiProcedure', 'WriteDisCodeLabel', 
 'WriteOneDataLabel', 'WriteDisPointers'

 'SelectMainWindowProc'
 
 'ForcedFlagsProc' 'DisassembleProc' 'WriteForcedMapFiles'
 
 'ConditionalJumpsAnalyzes' 'IsConditionalShortJump' 'IsLoop'
 
 'DisImageBase' 'UnlikelyOut'
 
 'WriteOneDataChunk' >>> 'DisDataTypeRouter' 'FlagPointerToPointer'
  
 'Map'
 
 'DisMain' 'ForceMenusExType' 'WriteDisRelative' 'EndWithDisByteRelative'
 'WritedWordPointers'
 'DisFail'
 'MarkEvocated' 'StoreDisSize' <<< for Size Markers
 'IsItCode' 'CheckAsciiData'
 'DisassembleForCodeRouting' 'TryToDisassembleEvocated' 'DisassembleAndWrite'
 'WriteDBandData' 'AlignRecognition' 'WriteOneDataChunk' >>> 'DisDataTypeRouter'
 'SearchMainWindowProc'
 'WriteDisRelative', 'FlagApiProcedures', 'StoreDisSize'
 'IsItCode'
 
  For Menus: EnableMenuItem
  
 'NamedIdSubstitution'  'WriteDisMenuIDs'
 
 'ST0ToAscii'
 
 'DisOp1'
____________________________________________________________________________________________
 
 For volunteers: 'OpenDev' / 'HLLAnalyzes' / ...

 Strategy:
 
   Once loaded, the target PE is re-mapped from File Alignement (usually 0200) to Memory
   alignement (usually 01000). The Name of this Target Table is yet 'UserPeStart'

   Then Tables are created, with the same length as the mapped PE. They are 'Flag Images'
   of the Target:
   
 * The 'SectionsMap' Table is for storing Sections Flags.
 * The 'RoutingMap' Table is for storing the routing Flags.
 * The 'SizesMap' Table is for the Sizes of evocated Data.
 
   These Table are duplicated , for the Negative Strategy, in 'InitDisTablesCopies'
   and friends.
 
 ________________ Positive Strategy ____________________
 (This is to say, searching, first, for all very highly probable Code and Data).
  
 * Mark 'Main'.
 
 * Search the EVOCATED Procedure (PUSH_EBP)
 
 * 'DisassembleForCodeRouting' for main and identified procedures
 
 * Search for Data Strings, Unicode Strings, PointersFlows, ...
 
 * (Sure Code marked CODEFLAG // Data Strings, Pointers and Sizes Marked >>> DATAFLAG)
 
 
 ________________ Negative Strategy ____________________
 (This is to say, everything left unflaged, that could be considerd Code will
 much likely kill something already identified valid, either as Code or as Data
 if it would not be interpreted as Data).

 * 'TryToDisassembleEvocated' >>> ItsItCode >>> Yes (it looks like valid Codes
   >>>> 'DisassemblingAttempt' >>> No Code broken // No Data in Disassembly 
   >>> Valid Code >>> Run 'DisassembleForCodeRouting' on all Evocated Location
   not yet Flaged as a Section.
 
 * All remaining zeroed Sections Flags to Data.
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

[FirstDisVirtualData: ?]

; re-mapped target File:

;;
  All of these Tables are the same size as the PE to be disassembled.
  
  'SectionsMap' is simply for flaging the various sections. The Flags are the
  ones found at 'CODEFLAG'
  
  'RoutingMap' is mainly for flaging the Code Flow, as found by r=the Recognitions
  and by the various Disassembling passes. The Flags are the ones found at 'INSTRUCTION'.
  In this List of Equates, 'EVOCATED' is not only for Code, but is used as well for
  Data.
  
  'SizesMap' is for flaging the Memory accesses. The purpose is for pointing Data
  out. The Flags are the one at 'BYTE'
  
  'StringsMap' is a Table of dWords, where each dWord is a mirror of a Byte, in
  the PE, and where each dWord may be a Pointer to a Symbol String, located
  anywhere else.
  
  A Macro 'ToStringsMapFrom' is provided to help switching from any other Map, to
  the Strings Table. This Macro is used each time a Label is being written, so
  that the Symbolic Name could be given, in the outputed Source, with the usual
  synthetic Label.
;;

[SectionsMap: ?    EndOfSectionsMap: ?
 RoutingMap: ?     EndOfRoutingMap: ?
 SizesMap: ?       EndOfSizesMap: ?
 StringsMap: ?     EndOfStringsMap: ?]

;;
  Macro for switching from any Map to StringsMap:
  
  ToStringsMapFrom Map reg ; example:
  
  ToStringsMapFrom SectionsMap, ebx
;;

[ToStringsMapFrom | sub #2 D$#1 | shl #2 2 | add #2 D$StringsMap]

;;
  ToStringsMapFrom is evocated from: 'MarkEntryPoint', 'WriteOneDataLabel',
  'WriteDisPointers', 'SetWINDCLASSData', 'SetWINDCLASSEXData', 'SetApiProcedure',
  'WriteDisCodeLabel', 'SetProcStrings'
;;

; ('EndOf...' points the Byte *after* // All the Length are 'D$UserPeLen')

; Sections Flags:
[CODEFLAG 1, DATAFLAG 2, VIRTUALFLAG 4, IMPORTFLAG 8, RESOURCESFLAG 010
 EXPORTFLAG 040,  KILLFLAG 080, TEMPOFLAG 0FF]

[TWOTEMPOFLAGS 0FFFF TWODATAFLAGS 0202
 FOURCODEFLAGS 01010101    FOURDATAFLAGS 02020202    FOURTEMPOFLAGS 0FFFF_FFFF
 FOUR_RESOURCESFLAG 010101010]
; 02 0A 01
; Routing Flags:  ; 01A (0A=EVOCATED+LABEL) 012 01F 010 09F 04A
[INSTRUCTION         1  ; > Start of an Instruction.
 EVOCATED        00_10  ; > little weight marker for any supposed evocation.
 NODE           00_100  ; > Directely CALLed or JMPed Location (+INSTRUCTION+LABEL)
 LABEL         00_1000  ; > Labelled Data Location.
 CHUNKEND      00_1000  ; > Location after a RET or a JMP (should be LABEL+NODE)
 ACCESSED    00_1_0000  ; > Marks a chunk of Instructions by direct call or jump.
 EXPORTNODE 00_10_0000  ; > Used for direct write of Exported Function Labels Names.
 INDIRECT  00_100_0000  ; > Data Reference to Code. No effect if not Instruction aligned.
 PUSH_EBP 00_1000_0000] ; > Futur NODE (typical CallBack entry-Point).

; 04A >  00_100_1010 EVOCATED LABEL INDIRECT
; 01F
; 016 >>> 10110
; 17    >>>   001_0001  ; Instruction / Accessed  //  0A EVOCATED+LABEL 012

; Sizes Flags: ; 014 045
[BYTE 1, WORD 00_10, DWORD  00_100
 FP4 00_1000, FP8 00_1_0000, FP10 00_10_0000
 POINTER 00_100_0000, STRINGS 00_1000_0000]

[LeaInstruction: &FALSE]

Proc BuildCommentedCodeReference:
    Arguments @DataPointer
    Local @DataPos
    Uses edx, esi, ebx, eax

        mov ebx D@DataPointer

      ; Get the actual position of the data label.
        mov eax ebx
        sub eax D$RoutingMap
        mov D@DataPos eax

        Zcopy {W$ CRLF, B$ "; Referenced Routing Flags: Hex = ", 0}
        movzx eax B$ebx

        push eax
            call Writeeax
        pop eax

L1:     cmp al CHUNKEND | jnz L1>
            zCopy {' CHUNKEND +', 0} | jmp L2>>
L1:
        test eax INSTRUCTION | jz L1>
            zCopy {' INSTRUCTION +', 0}
L1:     test eax EVOCATED | jz L1>
            zCopy {' EVOCATED +', 0}
L1:     test eax NODE | jz L1>
            zCopy {' NODE +', 0}
L1:     test eax LABEL | jz L1>
            zCopy {' LABEL +', 0}
;L1:    test eax CHUNKEND | jz L1>
    ;       zCopy {' CHUNKEND +', 0}
L1:     test eax ACCESSED | jz L1>
            zCopy {' ACCESSED +', 0}
L1:     test eax EXPORTNODE | jz L1>
            zCopy {' EXPORTNODE +', 0}
L1:     test eax INDIRECT | jz L1>
            zCopy {' INDIRECT +', 0}
L1:     test eax PUSH_EBP | jz L1>
            zCopy {' PUSH_EBP +', 0}
L1:

L2:

    If W$edi-2 = ' +'
        sub edi 2
    End_If

    mov W$edi CRLF | add edi 2
EndP


Proc BuildCommentedDataReference:
    Arguments @DataPointer
    Local @DataPos
    Uses edx, esi, ebx, eax

        mov ebx D@DataPointer

      ; Get the actual position of the data label:
        mov eax ebx
        sub eax D$RoutingMap
        mov D@DataPos eax

        Zcopy {W$ CRLF, B$ "; Referenced Size Flag for next data: Hex = ", 0}
        movzx eax B$ebx

        push eax
            call WriteEax
        pop eax

        test eax STRINGS | jz L1>
            zCopy {' STRINGS +', 0}
L1:     test eax POINTER | jz L1>
            zCopy {' POINTER +', 0}
L1:     test eax FP10 | jz L1>
            zCopy {' FP10 +', 0}
L1:     test eax FP8 | jz L1>
            zCopy {' FP8 +', 0}
L1:     test eax FP4 | jz L1>
            zCopy {' FP4 +', 0}
L1:     test eax DWORD | jz L1>
            zCopy {' DWORD +', 0}
L1:     test eax WORD | jz L1>
            zCopy {' WORD +', 0}
L1:     test eax BYTE | jz L1>
            zCopy {' BYTE +', 0}
L1:

        If W$edi-2 = ' +'
            sub edi 2
        End_If

        mov W$edi CRLF | add edi 2
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; For Disassembler developpements: Shows the Hexa of the various Dis Tables:

Proc ShowMaping:
    Argument @Map, @MapEnd, @Close

        ;mov D$DisViewPos 0

        mov esi D@Map, ecx D@MapEnd | sub ecx D@Map | shl ecx 6
        VirtualAlloc TempoMemPointer ecx

S0:     mov edi D$TempoMemPointer, esi D@Map, ecx D@MapEnd
        sub ecx esi | dec ecx

        mov eax esi | sub eax D@Map | add eax D$DisImageBase
        call WriteEax | mov D$edi '    ' | add edi 4

L0:     lodsb | shr al 4 | add al '0' | On al > '9', add al 7 | stosb
        dec esi | lodsb | and al 0F | add al '0' | On al > '9', add al 7 | stosb
        mov al ' ' | stosb

        test ecx 00111 | jnz L1>
            mov al ' ' | stosb

        test ecx 001111 | jnz L1>
            sub esi 16
            push ecx
                mov ecx 16
T0:             lodsb | On al < ' ', mov al '.' | stosb | loop T0<
            pop ecx
            mov ax 0A0D | stosw
            mov eax esi | sub eax D@Map | add eax D$DisImageBase
            call WriteEax | mov D$edi '    ' | add edi 4
L1:     loop L0<
; Tag Dialog 1001
        call 'USER32.DialogBoxParamA' D$hInstance, 1001, &NULL, DisViewProc, &NULL

        .If D$ViewCommand = 11
            mov eax D$SectionsMap, D@Map eax
            mov eax D$EndOfSectionsMap, D@MapEnd eax
            jmp S0<<
        .Else_If D$ViewCommand = 12
            mov eax D$RoutingMap, D@Map eax
            mov eax D$EndOfRoutingMap, D@MapEnd eax
            jmp S0<<
        .Else_If D$ViewCommand = 13
            mov eax D$Sizesmap, D@Map eax
            mov eax D$EndOfSizesMap, D@MapEnd eax
            jmp S0<<
        .Else_If D$ViewCommand = 14
            mov eax D$UserPeStart, D@Map eax
            mov eax D$UserPeEnd, D@MapEnd eax
            jmp S0<<
        .Else_If D$ViewCommand = 15
            If D$JumpsMap <> 0
                mov eax D$JumpsMap, D@Map eax
                mov eax D$EndOfJumpsMap, D@MapEnd eax
                jmp S0<<
            End_If
        .End_If

        VirtualFree D$TempoMemPointer

        mov B$SilentMap &TRUE
        On B@Close = &TRUE, jmp DisFail
EndP


[DisViewPos: ?    ViewCommand: ?]

Proc DisViewProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If D@Message = &WM_INITDIALOG
        mov D$EmptyDialogHandle eax
        call 'USER32.SetDlgItemTextA' D@Adressee, 100, D$TempoMemPointer
        If D$DisViewPos <> 0
            call 'User32.SendDlgItemMessageA' D@Adressee,100, &EM_LINESCROLL, 0, D$DisViewPos
        End_If

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        mov eax &TRUE

    .Else_If D@Message = &WM_CLOSE
        mov D$DisViewPos 0, D$ViewCommand 0
        call 'User32.DestroyWindow' D@Adressee

    .Else_If D@Message = &WM_COMMAND
        If D@wparam < 11
            ;
        Else_If D@wparam > 15
            ;
        Else
            move D$ViewCommand D@wParam
            call 'User32.SendDlgItemMessageA' D@Adressee, 100, &EM_GETFIRSTVISIBLELINE, 0, 0
            mov D$DisViewPos eax
            call 'User32.DestroyWindow' D@Adressee
        End_If

    .Else_If D@Message = &WM_CTLCOLOREDIT
        call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor

    .Else
        popad | mov eax &FALSE | jmp L9>

    .End_If

    popad | mov eax &TRUE

L9: EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 Initialisation of the Disassembly:
 
 We clear all of the Disassembler internal data, from 'FirstDisVirtualData'
 to 'LastDisVirtualData', and free all of the Data that could have been for
 a previously loaded PE.
 
 We intialize The Truth Ascii Table, for Ascii and Unicode Recognitions, and
 initialize the ProgreesBar, for the Disassembly time.
;;

DisInitialise:
  ; Clear all Disassembler job Virtual Data:
    mov edi FirstDisVirtualData, eax 0

    mov ecx LastDisVirtualData | sub ecx edi | shr ecx 2 | repe stosd

    Align_On 4 D$UserPeLen

    mov B$Disassembling &TRUE

    call CloseTree | call ReInitUndo | call ReInitHeaderFormat
    VirtualFree D$BookMarks | call ClearBackTable | call ReleaseResourceMemory

    call BuildTruthAsciiTable

    call InitDisProgressBar
;;
  The steping of the Progress Bar is just a simple evaluation. The Range is set
  (in 'InitDisProgressBar') at 0 to 128. So that we have to shr 7 for having the
  step size. But, as the Disassembler is run at least 4 times (2 or more for Pass1 /
  1 for Pass2 / 1 for Pass3), we estimate the number of Passes at 8 (> shl 3).
  So 7-3 > 4.
;;
    mov eax D$UserPeLen | shr eax 4 | or eax 1
    mov D$DisBarStep eax | move D$NextDisBarPos D$UserPeStart

    mov eax D$UserPeStart
    If D$eax+059 = 'spit'
        On D$eax+069 = ' wiz', mov D$CompiledBy 'ROSA'
    End_If

    mov D$DisMainWindowProc 0
ret
____________________________________________________________________________________________

; 'DisMain' is called by 'OpenRosAsmPe' if no Source found inside and if the
; user wants it.

[DisWarningMessage: ' ... with Un-modified File Name.           ', 0
 DisWarningTitle: 'Take care!!!...', 0]

[WritingData: 'Writing Data...', 0
 DisPasses: 'Analyze of the Code flow...', 0
 PointersAnalyzes: 'Analyzes of pointers to Code. Wait...', 0
 NegativeAnalyze: 'Negative Analysis of Code. May be slow...', 0
 SymbolsWriting: 'Symbolic Analyze of Api calls Parameters...', 0]

[EndOfDisData: ?    FromUserPeStartToMap: ?]
____________________________________________________________________________________________

MarkRosAsmPeSections:
    GetPeHeader SectionsHeaders | mov esi eax

    mov ecx D$DisNumberOfSections

L0: push ecx
        mov edi D$esi+SECTION_RVA | add edi D$SectionsMap
        mov ecx D$esi+SECTION_RVASIZE | Align_On 01000 ecx

        mov eax D$esi

        .If eax = '.ida'
            mov al IMPORTFLAG
        .Else_If eax = '.rsr'
            mov al RESOURCESFLAG
        .Else_If eax = '.dat'
            mov al DATAFLAG, ecx D$esi+SECTION_RVASIZE, edx D$esi+SECTION_FILESIZE
            On edx < ecx, xchg ecx edx
            Align_On 32 ecx
            push ecx
                rep stosb
            pop eax
            mov ecx D$esi+SECTION_RVASIZE, edx D$esi+SECTION_FILESIZE
            On edx > ecx, xchg ecx edx
            Align_On 01000 ecx | sub ecx eax
            mov al VIRTUALFLAG
        .Else_If eax = '.tex'
          ; Not CODEFLAG. It would kill the 'TryToDisassembleEvocated' job:
            mov al 0, ecx D$esi+SECTION_RVASIZE, edx D$esi+SECTION_FILESIZE
            On edx < ecx, xchg ecx edx
            add ecx 7
            push ecx
                rep stosb
            pop eax
            mov ecx D$esi+SECTION_RVASIZE, edx D$esi+SECTION_FILESIZE
            On edx > ecx, xchg ecx edx
            Align_On 01000 ecx | sub ecx eax
            mov al KILLFLAG
        .Else_If eax = '.eda'
            mov al EXPORTFLAG
        .Else
            mov al KILLFLAG
        .End_If

        rep stosb
    pop ecx

    add esi SECTIONHEADERSIZE | dec ecx | jnz L0<<
ret

____________________________________________________________________________________________

[OriginalDisPe: B$ ? #&MAX_PATH] [UserDisPeName: B$ ? #&MAX_PATH]

Proc SaveDisPeName:
    Argument @Destination

        mov esi MainName, edi D@Destination

        While B$esi <> 0 | movsb | End_While

        mov eax D$SavingExtension | stosd | mov B$edi 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

;call ShowMaping D$SectionsMap, D$EndOfSectionsMap, 1
;call ShowMaping D$RoutingMap, D$EndOfRoutingMap, 1
;call ShowMaping D$ApiBuffer, D$EndOfApiBuffer, 1
;call ShowMaping D$UserPeStart, D$UserPeEnd, 1
;call ShowMaping D$SizesMap, D$EndOfSizesMap, 1

[MapingBase: 050A007] ;407967]

[Map | call MapCall]

MapCall: MapView

[MapView |  If D$MapingBase <> 0
                mov eax D$MapingBase | sub eax D$DisImageBase | shr eax 4
                mov D$DisViewPos eax
            Else
                mov D$DisViewPos 0
            End_If

call ShowMaping D$SectionsMap, D$EndOfSectionsMap, 1]

[CompiledBy: ?    LastDisassemblyRoutingPass: ?]

DisMain: ; 'MSVBVM' 'OpenRosAsmPe', 'DisassembleProc'
    mov B$LastDisassemblyRoutingPass &FALSE

    call SaveDisPeName OriginalDisPe
    ___________________________
  ; Initialise the Disassembly:
;;
  When loading a PE without Source, the 'OpenRosAsmPE' Routine jumps here.
  'UserPeStart' point to the Start of the PE load into Memory, and 'UserPeLen'
  holds the length of the PE. All of this is the PE as found on Disk.
;;
  ; For 'DisFail' short exit:
    mov eax esp, D$OldStackPointer eax, D$CompiledBy 0

    call DisInitialise | call FixMzParagraphsNumber

  ; 'ReadResourcesRecord' seems to fail at pointing out GoAsm Resources.
  ; Not sure: To be verified.

    call LoadDisResources

    call StartNewDisFile | call ReAlignPE | call AllocateDisTables

; 'OutputFormat'
; Hexprint D$SavingExtension | jmp DisFail
    ____________________________________________________________
;;
  The PE in Memory is now ready for analyzes.
;;
  ; We don't need .reloc and .debug:
    call KillPeHeader | call KillSection RelocSectionTable | call KillSection DebugDir
;map
  ; Check the secure Sections:
    call CheckImport | call CheckExport
;map
    call CheckResources
;map
    call CheckVirtualData | call CheckExtendedVirtual

    call SmallBlanksToSameFlag
;map
    call KillSectionsExtensions
;map
    _____________________________________________________________

  ; If called from 'TryDisassembly' or from 'ReRunDisassembler':
    If B$WithForcedMapFile = &TRUE
        call ReadForcedRecordsFile
        On D$ForcedRecordsTable <> 0, call ForceRecordsToMaps
    Else
        call DeleteForcedFile
        VirtualFree D$ForcedRecordsTable
    End_If

  ; Sniff Recognition should come here...
    If D$CompiledBy = 'ROSA'
        call MarkRosAsmPeSections
;map
    Else_If D$CompiledBy = 'MSVB'
        call MarkVbPe
    Else
      ; ...
    End_If
;map
    ;
    ____________________________________________________________
;;
    Guga D.I.S. Identification Plan:
  
    call GetDisPath | call LoadOneDisFile {'Pe', 0}, '.dis'
    
    ; LoadPeDisFile
    
    .If B$DisFileOk = &TRUE
        VirtualAlloc PeDisMatches 01000 | move D$PeDisMatchesPointer D$PeDisMatches
  
        call ParsePeDis
        call GetBiggerPeDisID
        
        VirtualFree D$PeDisMatches
        jmp DisFail
    .End_If
;;
    ____________________________________________________________
  ; Here we go for Code vs Data Recognitions:
    ______________________________________
  ; Positive Recognition for Data and Code:

  ; 'MarkEvocated' _MUST_ be kept _first_ Recognition.
    call MarkEvocated | call ConditionalJumpsAnalyzes
;map
    call MarkEntryPoint
    call MarkProcedures | call MarkJumpsTables
;map
    call MarkPointersFlows | call MarkAlternatedPointersFlows
;map
    call DisassembleForCodeRouting
;map
    call MarkVeryRepetitiveDataBytes ; (12 identical Bytes >>> Data)
;Map
    call MarkEvocatedSizes
;map
    call MarkRepetitiveDataBytes ; (8 identical Bytes >>> Data)
;;;map
; Probably obsolete. Anyway, should not be at thins place:
    ;call ExtendStrings | call FillStringsSizes
;Map
    call UnEvocatedProcedures

    call DisassembleForCodeRouting

    mov B$Forced &TRUE
    call AsciiRecognition 25 | call UnicodeRecognition 25
    mov B$Forced &FALSE
;Map
    call SmallBlanksToSameFlag
;Map
    mov B$LastDisassemblyRoutingPass &TRUE
    call DisassembleForCodeRouting
    mov B$LastDisassemblyRoutingPass &FALSE

    call FlagTrueDataSection
;map
    call MarkIsolatedPointers
;map
    call SmallBlanksToSameFlag
;map
    call FlagsCleaner
;map
    ;call FlagTrueDataSection ; Better 4 lines above.
;map

    _________________________________________________________________
  ; Try&See recognitions of left Code // Negative Recognition of Data:
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, PointersAnalyzes
;
    call CodeFromPointers

    If B$AttemptSuccess = &TRUE
        call DisassembleForCodeRouting
        call SmallBlanksToSameFlag
    End_If

    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, NegativeAnalyze

    call GetBiggerSectionsBlank | mov ecx D$BiggerZeroedSectionsChunk
;map

    .Do
L0:     push ecx
            call TryToDisassembleEvocated ecx

            If B$AttemptSuccess = &TRUE
                call DisassembleForCodeRouting
                call SmallBlanksToSameFlag
;;
  This inner loop is Bound to "jmp L9>", in 'TryToDisassembleEvocated'. When these
  are uncommented, the results tend to be a little bit better. The problem is that
  it all becomes way too slow:
;;
                ;pop ecx | jmp L0<
;;
  Therefore, we can expect that a more sophisticated way for choosing the biggest
  Chunks could perhaps be implemented.
;;
            End_If
        pop ecx
;;
  To be watched: The tuning of these Strings jobs have impact on 3DFUN.EXE. Example,
  reducing the last (smaller) length, breaks the rebuilt. Remark: There is no need of
  scaning smaller Chunks here, as they should be assumed later, by 'DisDataTypeRouter'.
;;
        push ecx
            .If ecx < 100
                If ecx > 50
                    call AsciiRecognition 20 | call UnicodeRecognition 20 | jmp L9>
                End_If
            .End_If

            .If ecx <= 50
                If ecx > 25
                    call AsciiRecognition 15 | call UnicodeRecognition 15 | jmp L9>
                End_If
            .End_If

            .If ecx <= 25
                If ecx > 13
                    call AsciiRecognition 12 | call UnicodeRecognition 12 | jmp L9>
                End_If
            .End_If
L9:     pop ecx

        shr ecx 1 | mov eax ecx | shr eax 1 | add ecx eax
  ; Let the ecx = 0. This is to say down to one _Instruction_:
    .Loop_Until ecx = 0
;map
    call FlagsCleaner

    call DisassembleForCodeRouting

    call SmallBlanksToSameFlag
;Map
    call DisAlign
;Map
    ;call RemoveNonAccessedEvocatedData
;Map
  ; All Code pointed out >>> Not flaged Chunks to Data:
    call FillDataSection | call StripSectionsZeroEnd

    call MarkIsolatedPointers ; Second call really helpfull?
;map
    mov B$LastDisassemblyRoutingPass &TRUE

    call DisassembleForCodeRouting
;Map
    call CheckPointersInData

    call SplitBigData
;map
    call FlagsCoherency

    call FlagsCleaner
;map

    _________________________________
    _________________________________

  ; Recognitions over: The real Disassembly output begins here.
    ________________________________________________________
  ; Now we write the Source. We need the original File Name. Save it first:

    mov edi D$CodeSource | call WriteOriginalFileNameInSource

  ; then, Default Macros, and Data:

    mov D$NextDisTITLE edi | add D$NextDisTITLE (TITLE_MAX/2) | mov W$DisTitle+12 '01'

    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, WritingData
;Map
    If B$WithMacros = &TRUE
        On D$SavingExtension <> '.SYS', call WriteMacros
    End_If

    mov D$EndOfDisData edi

  ; ... and then Code:
;Map
    mov D$NextDisBarPos 0 | call DisassembleAndWrite  ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;call TestStringsTable
    pushad
        call WriteMapFiles ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        call WriteForcedRecordsFileBase
    popad

    ________________________________
  ; Preparing for the Source Editor:
    call SetDisSourceVariables

  ; Searching for 'MainWindowProc' Location:
   ;On D$SavingExtension <> '.DLL', call SearchMainWindowProc

    If B$MenuInside = &TRUE
         ; called from 'MenuIdsSubstitutions', 'PrepareDisMenuIDs' Builds the
           ; Table of Menu Ids Declarations
            call MenuIdsSubstitutions
            On B$WM_COMMAND_Found = &TRUE, call ResetMenusIDs
        End_If
    End_If

    call NamedIdSubstitution

    VirtualFree D$TruthAsciiTable, D$ApiBuffer
    VirtualFree D$UserPeStart | move D$UserPeStart D$CodeSource
    VirtualFree D$SectionsMap, D$RoutingMap, D$SizesMap, D$StringsMap
;;
  Because the 'normal' way to load a PE, in RosAsm, implements the 'CodeSource'
  pointer into the 'UserPeStart' Mem Chunk. Here, we do not define the Mem size
  depending on the File Source size, but we evaluate the size from the Code
  length after analyze of the PE.

  + If Resources saved by Name: Set ID Numbers ?????
;;
    SetWindowText

    mov D$DestinationFile 0

    mov D$OpenPEStruc+(12*4) ChangeNameTitle
    mov esi SaveFilter
    While B$esi <> 0 | inc esi | End_While
    dec esi | lea edi D$esi+2
    std
        Do
            movsb | On B$esi = '\', jmp L1>
        Loop_Until esi < SaveFilter
L1:     inc esi | mov W$esi 'My'
    cld

    .If D$WithForcedMapFile = &FALSE
        call ChangeName | mov D$OpenPEStruc+(12*4) OpenPEFileTitle

        If eax = &FALSE
            call 'USER32.MessageBoxA' D$hwnd, DisWarningMessage, DisWarningTitle,
                                    &MB_SYSTEMMODAL__&MB_ICONSTOP
        End_If

        call SaveDisPeName UserDisPeName

    .Else
        call SaveNewFileName | mov D$OpenPEStruc+(12*4) OpenPEFileTitle
        call SaveDisPeName UserDisPeName
        SetWindowText

    .End_If

    call StartEdition | move D$UserPeStart D$CodeSource

    mov B$Disassembling &FALSE, B$SourceReady &TRUE

    call DestroyDisProgressBar

    mov B$ThisSourceIsDisassembled &TRUE
ret


DestroyDisProgressBar:
    On D$ProgressInst <> 0, call 'User32.DestroyWindow' D$ProgressInst
    On D$hwndForBar <> 0, call 'User32.DestroyWindow' D$hwndForBar
    mov D$ProgressInst 0, D$hwndForBar 0
ret
____________________________________________________________________________________________

; Simple Debug-Test for viewing what is inside:

TestStringsTable:
    pushad
        mov esi D$StringsMap

        .While esi < D$EndOfStringsMap
            If D$esi <> 0
                mov eax esi | sub eax D$StringsMap | shr eax 2 | add eax D$DisImageBase
                mov edi Trash | call WriteEax | mov D$edi ' -> ' | add edi 4
                push esi
                    mov esi D$esi
                    While B$esi <> 0 | movsb | End_While
                pop esi
                mov B$edi 0
                Showme Trash
            End_If

            add esi 4

        .End_While
    popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[MenuInside: ?]

LoadDisResources:
    __________________________________________________________________________
  ; Next Lines are the same as the ones in 'OpenRosAsmPE' after having read the
  ; included Source:

    mov B$MenuInside &FALSE

    call SearchDisPEstartOfResource | On B$NoResourcesPE = &TRUE, jmp L9>>

  ; Here, 'UserPEStartOfResources' is the IMAGE_OPTIONAL_HEADER Sections aligned Pointer
  ; to Header.
    mov eax D$UserPEStartOfResources, ebx 0 | On eax = 0, jmp L9>>

L0: shl ebx 1 | or ebx 1 | test eax ebx | jz L0<
    shr ebx 1 | and eax ebx

    push eax
        call SearchPEstartOfResource
      ; Here 'UserPEStartOfResources' is the Sections Header File Aligned Section Pointer.
      ; We need this one but with the eventual Dis to Header (header not necessary at
      ; first Section Byte.
    pop eax

    .If D$UserPEStartOfResources <> 0
        or D$UserPEStartOfResources eax

        move D$iExePtr D$UserPeStart

        mov eax D$UserPEStartOfResources, D$iStartOfResources eax
        call DisReadMainIcon

        If B$PeIconFound = &TRUE
            mov esi eax | mov edi iIcon | rep movsb ; Copying to icon editor buffer
            call StoreIcon                          ; and copy it to 'compilable' image
        End_If

        call ReadRosAsmResources

        If D$MenuList+4 <> 0
            call ForceMenusExType | mov B$MenuInside &TRUE
        End_If
    .End_If
L9: ret


SearchDisPEstartOfResource:
    mov B$NoResourcesPE &TRUE, D$UserPEStartOfResources 0
    mov esi D$DisPeTagPointer | On esi = 0, ret

  ; RVA of resources from "Image Data Dir...":
    add esi 136 | lodsd
    If eax <> 0
        add eax D$UserPeStart | mov D$UserPEStartOfResources eax
        move D$ResourcesSize D$esi
        mov B$NoResourcesPE &FALSE
    End_If
ret
____________________________________________________________________________________________

; There are 2 types of Win Menus: Menu and MenuEx. RosAsm work only with MenuEx Type.
; In case an old type is encounted we need a translation:

ForceMenusExType:  ; ExistingMenu
    mov ebx MenuList                                ; (ID / Ptr / Size)
    While D$ebx+4 <> 0
        mov esi D$ebx+4
        If W$esi = 0

            call TurnThisMenuToExType     ; First Header Word is 0 (old) / 1 (EX)

        End_If

        add ebx 12
    End_While
ret


[DisExMenuMemoryPointer: ?]

TurnThisMenuToExType:
    mov D$PopUpNumber 1

    mov ecx D$ebx+8 | mov edx ecx | shl edx 3
  ; esi > Menu Template // ecx Length of Menu Template (*8 estimation > edx).
    push ebx, esi
        VirtualAlloc DisExMenuMemoryPointer edx
    pop esi, ebx

  ; Menu Header:
    mov edi D$DisExMenuMemoryPointer
    mov W$edi 1 | add edi 2     ; Type.
    mov W$edi 4 | add edi 2     ; Displacement to next Item.
    mov D$edi 0 | add edi 4     ; Help ID.
    movzx eax W$esi+2 | add eax 4 | add esi eax

  ; Items:
  ; Split MF_Flags into MFT_Flags / MFS_Flags / MFR_Flags.
L0:
  ; Security in case of perverted Menu: Old Menus begin by W$ Flags W$ ID. So the first
  ; dWord can't be zeroed, even in case of PopUp -with no ID-:
  ;;;  On D$esi = 0, jmp L9>>
  ;;; Commented out because it troncates the Menus after the first separator...
    movzx eax W$esi
    and eax &MFT_BITMAP__&MFT_MENUBARBREAK__&MFT_MENUBREAK__&MFT_RIGHTJUSTIFY__&MFT_SEPARATOR
    stosd
    mov ax W$esi
    and eax &MFS_CHECKED__&MFS_DEFAULT__&MFS_GRAYED__&MFS_HILITE
    stosd
    mov ax W$esi
    and eax &MF_END__&MF_POPUP
    add esi 2

    mov D$edi 0
    Test eax &MF_POPUP | jz L1>
        and eax (not &MF_POPUP) | or eax 1      ; 1 = My MF_POPUP substitution.
        inc D$PopUpNumber
      ; No ID in old Menu for POPUP item:
        add edi 4 | jmp L2>

L1: movsw | add edi 2      ; ID

L2: stosw       ; My MF_END / &MF_POPUP >>> ax yet holds the MF_Flags.
    Test eax &MF_END | jz L1>
        dec D$PopUpNumber
L1:
;;
  ; Menu: Items                  //     MenuEx Items
    W$     ; Menu Item Flags            D$      ; Type >>> 0
    W$     ; ID (none for PopUp)        D$      ; State >>> 0
    US     ; 'String', 0                W$      ; ID
                                        B$      ; END / POPUP / 0
                                        B$ 0    ; Align
                                        US      ; 'String', 0
                                        Align 4
                                      ; If PopUp:
                                        D$      ; HelpID
;;
    While W$esi <> 0 | movsw | End_While | movsw | Align_On 4, edi

L1: Test eax 1 | jz L1>             ; 1 = My MF_POPUP substitution.
        mov D$edi 0 | add edi 4

L1: cmp D$PopUpNumber 0 | ja L0<<

L9: mov eax edi | sub eax D$DisExMenuMemoryPointer | mov D$ebx+8 eax
    push ebx, esi
        VirtualFree D$ebx+4
    pop esi, ebx

    move D$ebx+4 D$DisExMenuMemoryPointer
    mov ecx edi | sub ecx D$DisExMenuMemoryPointer
ret


[BasemenuId: ?]

ResetMenusIDs:
    mov ebx MenuList                                ; (ID / Ptr / Size)
    While D$ebx+4 > 0
        move D$BasemenuId D$ebx
        push ebx
            call ResetThisMenuIDs
        pop ebx
        add ebx 12
    End_While
ret


ResetThisMenuIDs:
    mov esi D$ebx+4, edx D$ebx+8 | add edx esi

;[MENUEX_TEMPLATE_HEADER:
; wVersion: W$ 0
; wOffset: W$ 0
; dwHelpId: D$ 0]

    add esi 8

;[MENUEX_TEMPLATE_ITEM:
; dwType: D$ 0
; dwState: D$ 0
; uId: D$ 0
; bResInfo: W$ 0
; szText:U1
; dwHelpId: D$ 0]

L0: add esi 8 | movzx eax W$esi+4     ; Save Pos of possible POPUP Flag
    and eax (not &MF_END)
    .If eax = 0    ; &MF_END  080 // &MF_POPUP 010   ; Menu
        If W$esi+6 <> 0     ; 0 > Separator
            inc D$BasemenuId | move D$esi D$BasemenuId
        End_If
    .End_If

L2: add esi 6
  ; Name (including 2 zeros if Separators):
    While W$esi <> 0 | add esi 2 | End_While | add esi 2 | Align_On 4, esi

    If eax <> 0
      ; Help ID:
        add esi 4
    End_If

L2: cmp esi edx | jb L0<
ret
____________________________________________________________________________________________

SetDisSourceVariables:
    mov D$SourceEnd edi
    mov D$SourceLen edi, eax D$CodeSource | sub D$SourceLen eax
    mov eax 0A0D0A0D, ecx 100 | rep stosd                            ; end security tail

    mov eax D$CodeSource
    mov D$CurrentWritingPos eax, D$CaretLine 0, D$CaretRow 1

    move D$UpperLine D$CodeSource
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

StartHllAnalyze:
    mov eax D$SourceLen | add eax 1_000_000 | VirtualAlloc SymbolicCodeSource eax

    mov esi D$CodeSource, edi D$SymbolicCodeSource
    mov eax 0A0D0A0D | stosd | stosd | add D$SymbolicCodeSource 8
ret


CloseHllAnalyze:
    Exchange D$SymbolicCodeSource D$CodeSource
    VirtualFree D$SymbolicCodeSource
    call SetDisSourceVariables
ret


; Main of HllAnalyzes.

HLLAnalyzes:
  ;;; Analyze1.
    ; call StartHllAnalyze | call Analyze1 | call CloseHllAnalyze

  ;;; Analyze2.
    ; call StartHllAnalyze | call Analyze2 | call CloseHllAnalyze
  ;;; ...
    ; ...
ret

;;
 Example of specific Analyzes to be implemented.

 In: esi > D$CodeSource // edi > D$SymbolicCodeSource.

 A couple of Routines, Macros are already implemented for Source organisations (paging?):
 NextdisLine (Macro) / NextDataDisLine (Routine) / ...
;;

Analyze1:
  ; Must first write the required Macro(s) into the Source!!!

    .While esi < D$SourceEnd
        ..If D$esi = 'call'
            If W$esi+4 = " '"
                call BuildApiCallMacro | On B$ApiMacroSuccess = &FALSE, movsb
            End_If

        ..Else
            movsb

        ..End_If

    .End_While
ret

[ApiMacroSuccess: ?]

BuildApiCallMacro:
    mov B$ApiMacroSuccess &FALSE
;;
   Good luck EveryBody :))

   Must ajust esi Pointer right after the original api call.
   Otherwise > infinite loop!!!

   Must set the 'B$ApiMacroSuccess' to &TRUE if performed!!!
;;
ret

____________________________________________________________________________________________
;;
  We read all through the PE and search for Pointers to PE Locations.
  3 Processes are done:
  * Simple Evocations >>> EVOCATED
  * Pointers to Code saying 'Push ebp | mov ebp esp' / 'Enter' >>> PUSH_EBP+EVOCATED
  * Pointer Lists >>> EVOCATED+NODE+ACCESSED
;;

[EntryPointName: "
Main:", 0]

MarkEntryPoint:
    mov eax D$DisEntryPoint | sub eax D$DisImageBase | add eax D$RoutingMap
    or B$eax NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
    sub eax D$RoutingMap | add eax D$SectionsMap | mov B$eax CODEFLAG

    ToStringsMapFrom SectionsMap, eax | mov D$eax EntryPointName
ret
____________________________________________________________________________________________

MarkEvocated:
; We are going to analyze the PE up to the last dWord (mov eax D$esi >>> error!!!). So:
    sub D$UserPeEnd 4
;;
 EVOCATED is a 'little weight' Flag, just saying that some reference has been found
 to this Location. Nothing more. It may come from hazard. NODE / LABEL are stronger
 meaning (considered definitive significant Locations).
 
 Search for any reference (evocation) to a Code, Data or Virtual Data Location all 
 along the File being disassembled. When found, we mark the Routing Map Byte pointed
 by the evocation with EVOCATED:
;;

    mov esi D$UserPeStart | add esi D$FirstSection
  ; Relative:
    .While esi < D$UserPeEnd
        mov eax esi | add eax 4 | add eax D$esi
;push eax
;    mov eax esi | sub eax D$UserPeStart | add eax D$DisImageBase
;    On eax = 0440899, int3
;pop eax
        ;sub eax D$DisImageBase | add eax D$UserPeStart

        ...If eax > D$UserPeStart
            ..If eax < D$UserPeEnd
                sub eax D$UserPeStart | add eax D$SectionsMap
                mov bl B$eax | and bl IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG
                .If bl = 0
                    sub eax D$SectionsMap | add eax D$RoutingMap
                  ; eax >>> "Pointed to" Location
                    If B$esi-1 = 0E8
                        test B$eax EVOCATED | jz L1>
                      ; 1 chance on 400,000 * (2 or more) for both case:

                      ; 2 or more CALL Instructions to the same location:
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
                        mov B$ebx CODEFLAG

                    Else_If B$esi-1 = 0E9
                        test B$eax EVOCATED | jz L1>
                      ; 2 or more JMP Instructions to the same location:
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
                        mov B$ebx CODEFLAG

L1:                     or B$eax EVOCATED

                    End_If

                .End_If
            ..End_If
        ...End_If

L5:     inc esi
    .End_While

    mov esi D$UserPeStart | add esi D$FirstSection
  ; Absolute:
    While esi < D$UserPeEnd
        mov eax D$esi | sub eax D$DisImageBase | add eax D$UserPeStart
        ..If eax > D$UserPeStart
            .If eax < D$UserPeEnd
                sub eax D$UserPeStart | add eax D$SectionsMap
                mov bl B$eax | and bl IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG
                If bl = 0
                    sub eax D$SectionsMap | add eax D$RoutingMap
                    or B$eax EVOCATED
                End_If
            .End_If
        ..End_If

L5:     inc esi
    End_While

    add D$UserPeEnd 4
ret

____________________________________________________________________________________________

MarkProcedures:
;;
 'Enter' and 'push ebp | mov ebp esp', Locations in Code, are Flaged PUSH_EBP, in
 the 'RoutingMap' Table. But, if they also are found as EVOCATED, we consider them
 valid and accessed Code Nodes:
;;
    sub D$UserPeEnd 4

    mov esi D$UserPeStart | add esi D$FirstSection

    .While esi < D$UserPeEnd
        mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
        Test B$eax EVOCATED | jz L5>>

        mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
        Test B$eax VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+DATAFLAG | jnz L5>>

      ; 1 miss-interpretation chance on 0FFFFFF (16,777,215)
      ; * by number of "push ebp | mov ebp esp":
        ...If B$esi = 055               ; OpCode for "push ebp"  Op55
            ..If B$esi-1 <> 0F          ; No Escape Prefix wanted.
                .If W$esi+1 = 0EC8B      ; 08B 0EC >  "mov ebp esp"
L1:                 sub eax D$SectionsMap | add eax D$SectionsMap
                    If B$eax = 0
                        mov B$eax CODEFLAG
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                    Else
L2:                     sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax EVOCATED+LABEL
                    End_If

                .Else_If W$esi+1 = 0E589 ; Alternate for "mov ebp esp" op89 / op8B
                    jmp L1<
                .End_If
            ..End_If
      ; 1 miss-interpretation chance on 0FF*(0FF-32) [* (256/8) for IsItPush case]
      ; 0FF-32 = 0CD // 0CD*FF = 0CC33 // (256/8) = 020 // 0CC33*020 = 1 on 1,672,800
      ; * by number of "Enter":
        ...Else_If B$esi = 0C8          ; OpCode for 'enter'  OpC8 Op0F
            ..If B$esi-1 <> 0F          ; No Escape Prefix wanted.
                If B$esi+3 < 32         ; Nested Levels Byte is from 0 to 31.
                    sub eax D$SectionsMap | add eax D$RoutingMap
                    lea ebx D$eax+4 | call IsItPush ebx | cmp eax &FALSE | je L5>>
                        mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
                        On B$eax <> 0, jmp L2>
                            mov B$eax CODEFLAG
                            sub eax D$SectionsMap | add eax D$RoutingMap
                            or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL | jmp L5>>

L2:                     sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax EVOCATED+LABEL
                End_If
            ..End_If

       ; 1 miss-interpretation chance on 0FF * (0FF/2) * (0FF/7) = 1 on 1,165,860
         ...Else_If B$esi = 0EC
             .If B$esi-1 = 083         ; 083 0EC: OPcode for sub esp imm8
                sub eax D$SectionsMap | add eax D$RoutingMap
                lea ebx D$eax+2 | call IsItPushRegister ebx | cmp eax &FALSE | je L5>>
                    mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
                    If B$eax = 0
                        mov B$eax CODEFLAG
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                    Else
L2:                     sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax EVOCATED+LABEL
                    End_If

             .Else_If B$esi-1 = 081    ; 081 EC: OPcode for sub esp imm32
                sub eax D$SectionsMap | add eax D$RoutingMap
                lea ebx D$eax+5 | call IsItPushRegister ebx | cmp eax &FALSE | je L5>
                    mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
                    If B$eax = 0
                        mov B$eax CODEFLAG
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                    Else
L2:                     sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax EVOCATED+LABEL
                    End_If
             .End_If

        ...End_If

L5:     inc esi
    .End_While

    add D$UserPeEnd 4
ret
____________________________________________________________________________________________

UnEvocatedProcedures:
;;
 'Enter' and 'push ebp | mov ebp esp', Locations in Code, are Flaged PUSH_EBP, in
 the 'RoutingMap' Table. But, if they also are found as EVOCATED, we consider them
 valid and accessed Code Nodes:
;;
    sub D$UserPeEnd 4

    mov esi D$UserPeStart | add esi D$FirstSection

    .While esi < D$UserPeEnd
        mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
        Test B$eax VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG ;+DATAFLAG |
        jnz L5>>

      ; 1 miss-interpretation chance on 0FFFFFF (16,777,215)
      ; * by number of "push ebp | mov ebp esp":
        ...If B$esi = 055               ; OpCode for "push ebp"  Op55
            ..If B$esi-1 <> 0F          ; No Escape Prefix wanted.
                .If W$esi+1 = 0EC8B      ; 08B 0EC >  "mov ebp esp"
L1:                 push esi
                        mov eax esi | add eax 50
                        call IsItCode esi, eax, 8 ;20
                    pop esi

                    cmp eax &FALSE | je L5>

                    mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap
                    If B$eax = 0
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        sub eax D$RoutingMap | add eax D$SectionsMap | mov B$eax CODEFLAG
                    Else_If B$eax = CODEFLAG
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                    Else
                      ; Case of Map File reuse with Flag forced to Data (to follow up):
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        mov B$eax EVOCATED+LABEL
                    End_If

                .Else_If W$esi+1 = 0E589 ; Alternate for "mov ebp esp" op89 / op8B
                    jmp L1<

                .End_If
            ..End_If

        ...End_If

L5:     inc esi
    .End_While

    add D$UserPeEnd 4
ret


____________________________________________________________________________________________

; There is such a JMPs Table at Data043E188, in Teste.exe.

MarkJumpsTables:
    mov esi D$UserPeStart | add esi D$FirstSection
    mov edx D$UserPeEnd | sub edx 4
    mov ecx 0

    .While esi < edx
       ; OpFF, 'Dis_rm32_rm16', 'WriteEffectiveAddressFromModRm'
        ...If B$esi = 0FF  ; 1 on 256
            mov bl B$esi+1 | DigitMask bl to al
          ; 0FF /2 >>> CALL r/m32 // 0FF /4 >>> JMP r/m32
            ..If al = 2    ; 2 on 8
                jmp L1>
            ..Else_If al = 4
              ; Dis32 inside ? With SIB ?
              ; For JMP or CALL "D$Table+eax*4" and friends, the ModRm Byte must be:
              ; mod = 0 and r/m = 4 (SIB + dis32)
L1:             ModMask bl To al
                .If al = 0 ; 1 on 4
                    RmMask bl To al
                    If al = 4 ; 1 on 4
                        call CheckPointersTable
                        On eax = &TRUE, add esi 7
                    End_If
                .End_If
            ..End_If
        ...End_If

        inc esi

    .End_While
ret

;;
  A flow of Bytes that could be, for example, for 'call D$Table+eax*4' has been found.
  Is this 'Table' really a flow of Pointers. If true, the Instruction is valid Code,
  the target is valid Data, the data contents are valid Pointers to Code and the Code
  pointed by these Pointers are valid entry-points.
;;
CheckPointersTable:
    push esi, edx

    sub D$UserPeEnd 4 | mov eax D$esi+3
    sub eax D$DisImageBase | add eax D$UserPeStart

    ..If eax > D$UserPeStart
        .If eax < D$UserPeEnd
            mov ebx eax
            sub eax D$UserPeStart | add eax D$SectionsMap
            test B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG | jnz L5>>
          ; OK: 'Label' is a valid Pointer to a supposed Jumps Table.
          ; Now, are the two first dWords valid Pointers ?
            mov eax ebx, ebx D$eax | sub ebx D$DisImageBase | add ebx D$UserPeStart
            cmp ebx D$UserPeStart | jb L5>>
            cmp ebx D$UserPeEnd | ja L5>>

            mov ebx D$eax+4 | sub ebx D$DisImageBase | add ebx D$UserPeStart
            cmp ebx D$UserPeStart | jb L5>>
            cmp ebx D$UserPeEnd | ja L5>>

              ; OK, this is a Table of Code Pointers. Flag everything:
L0:             mov ebx D$eax | sub ebx D$DisImageBase | add ebx D$UserPeStart
                cmp ebx D$UserPeStart | jb L2>
                cmp ebx D$UserPeEnd | ja L2>
                    mov edx eax | sub edx D$UserPeStart | add edx D$SizesMap
                    or B$edx POINTER
                    sub edx D$SizesMap | add edx D$SectionsMap
                    mov D$edx FOURDATAFLAGS

                    sub ebx D$UserPeStart | add ebx D$SectionsMap
                    mov B$ebx CODEFLAG
                    sub ebx D$SectionsMap | add ebx D$RoutingMap
                    mov B$ebx PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL

                add eax 4 | jmp L0<
        .End_If
    ..End_If

L2: pop edx, esi | add D$UserPeEnd 4 | mov eax &TRUE | ret

L5: pop edx, esi | add D$UserPeEnd 4 | mov eax &FALSE | ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Analyzes of the Code Conditional Jumps
;;

[JumpsMap: ? EndOfJumpsMap: ?]

ConditionalJumpsAnalyzes:
  ; Create a Table, where to flag the possible Addresses pointed to, by Conditional Jumps:
    mov eax D$EndOfSectionsMap | sub eax D$SectionsMap
    mov D$EndOfJumpsMap eax
    VirtualAlloc JumpsMap eax
    add D$EndOfJumpsMap eax

    mov esi D$UserPeStart | add esi D$FirstSection
    mov ebx D$SectionsMap | add ebx D$FirstSection
    mov edx D$JumpsMap | add edx D$FirstSection
    mov eax 0

    .While esi < D$UserPeEnd
        ...If B$ebx = 0
            ..If B$esi <> 0
                call IsConditionalShortJump esi

                .If eax = &TRUE
L1:                 inc esi, ebx, edx | movsx eax B$esi | inc esi, ebx, edx
                    add eax edx
                    push esi, eax, ebx, edx
                        sub eax D$JumpsMap | add eax D$UserPeStart
                        mov ebx eax | add ebx 50
                        call IsItCode eax, ebx, 0
                        If eax = &TRUE
                            pop edx, ebx, eax, esi
                            inc B$eax
                        Else
                            pop edx, ebx, eax, esi
                        End_If
                .Else
                     call IsLoop esi
                     On eax = &TRUE, jmp L1<
                .End_If
            ..End_If
        ...End_If

        inc esi, ebx, edx
    .End_While
;;
  16 Chance on 256 (1 on 16) for a given Byte to be a Jcc.
  
  This is to say, that in a Flow of Random Bytes, one given Byte has 1 chance on
  16 to be the target of a possible Jcc. [In fact, less than that, because the
  cases of "070", ... , "07F", are way less frequent than, say "0", in a real PE]
  
  So, randomaly, there would 1 chance on 256, for a given Byte, for being the target
  of 2 JCCs, if these Jcc would not be limited to a 256 Bytes scope.
  
  Plus 3 chances on 256*2 to be a loop.
  
  Then, with the IsItCode Identification, this is considered reliable.
  
  We now cross all of this with the previously done analyzes of 'MarkEvocated',
  that flaged any Absolute or Relative Evocation of a Location.
;;
    mov esi D$RoutingMap, edi D$JumpsMap, edx D$EndOfRoutingMap

    While esi < edx
        Test B$esi EVOCATED | jz L2>
            On B$edi <> 0, inc B$edi
L2:     inc esi, edi
    End_While


    mov esi D$JumpsMap | add esi D$FirstSection
    mov edi D$SectionsMap | add edi D$FirstSection
    mov ebx D$RoutingMap | add ebx D$FirstSection
    mov edx D$EndOfSectionsMap

    While edi < edx
        If B$esi > 3
            On B$edi = 0, mov B$edi CODEFLAG
            On B$edi = CODEFLAG, mov B$ebx INSTRUCTION+EVOCATED+LABEL+ACCESSED+PUSH_EBP
            inc ecx
        End_If
        inc esi, edi, ebx
    End_While
;map
    VirtualFree D$JumpsMap
ret


[BadCode: ?    GoodCode: ?]

;[LongJmpsOp: 0E8    ; Long call 'OpE8'
;             0E9]   ; Long jmp    'OpE9'
;           ; 0EB    ; jmp short 'OpEB'
;;'loop' ; E0, E1, E2     OpE3 >>> jecxz
;[ShortJccOp:
;    070, 071, 072, 073, 074, 075, 076, 077, 078, 079, 07A, 07B, 07C, 07D, 07E, 07F, 0E3]
;; 0F Prefix >>> Long
;[LongJccOp:
;    080 > 08F    Op80 Op8F

;[DisCodePointer: ?]

Proc IsConditionalShortJump:    ; 21 random chances on 256
    Argument @Pointer
    Uses esi, edi, ebx

        mov esi D@Pointer, al B$esi, ebx &FALSE
;;
  Op78, Op79, Op7A, Op7B, Op7C, Op7D
  
  If these next ones have a 0F Escape Prefix, they are other Mnemonics:
  
  Op70, Op71, Op72, Op73, Op74, Op75, Op76, Op77, (....above.....)  Op7E, Op7F
  
  OpE3 is JECXZ
;;
        ...If al >= 070
            ..If al <= 07D
              ; This is can be a Conditional Short Jump
                mov ebx &TRUE
              ; No Escape Prefix for some:
                If al <= 077
                    On B$esi-1 = 0F, mov ebx &FALSE
                Else_If al >= 07E
                    On B$esi-1 = 0F, mov ebx &FALSE
                End_If

              ; A bit less than 16 chances on 256, randomaly.

            ..Else_If al = 0E3
              ; OpE3 is JECXZ // DEC ecx: 049 // sub ecx 5: 083 0E9 05
                If B$esi-1 = 049
                    mov ebx &TRUE
                Else_If W$esi-3 = 0E983
                    mov ebx &TRUE
                Else
                    mov ebx &FALSE
                End_If

              ; Max: (1 chance on 256) * (1 chance on 256) = 1 on 65,536
            ..End_If
        ...End_If

        If ebx = &TRUE
          ; It cannot be a jump to 0, -1, -2, -3, -4
            mov al B$esi+1 | neg al
            On al < 5, mov ebx &FALSE
        End_If

        mov eax ebx
EndP


Proc IsLoop:     ; 18 random chances on 256 / by the 1 on 256 of the 0F Jcc prefix.
    Argument @Pointer
    Uses esi

        mov esi D@Pointer, eax D$UserPeEnd | dec eax | On esi <= eax, jmp L2>

        mov al B$esi

      ; Must be negative:
        Test B$esi+1 080 | jz L2>

        .If al = 0E0
            mov eax &TRUE
        .Else_If al = 0E1
            mov eax &TRUE
        .Else_If al = 0E2
            mov eax &TRUE
        .Else
L2:         mov eax &FALSE
        .End_If

  ; 3 chances on 256*2 to be a loop
EndP
____________________________________________________________________________________________

Proc IsItPush:
    Argument @Location
    uses esi
      ; 8 chances on 256
        mov esi D@Location
        sub esi D$RoutingMap | add esi D$UserPeStart | mov al B$esi

        If al = 060
            mov eax &TRUE       ; pushad
        Else_If al < 050
            mov eax &FALSE
        Else_If al < 058
            mov eax &TRUE       ; push reg (050 to 057)
        Else
            mov eax &FALSE
        End_If
EndP

Proc IsItPushRegister:
    Argument @Location
    uses esi
      ; 7 chances on 256
        mov esi D@Location
        sub esi D$RoutingMap | add esi D$UserPeStart | mov al B$esi

        If al < 050
            mov eax &FALSE
        Else_If al < 058
            mov eax &TRUE       ; push reg (050 to 057)
        Else
            mov eax &FALSE
        End_If
EndP
____________________________________________________________________________________________

; Force the small (smaller than 4) zeroed Bytes inside a Code/Data Section (SectionsMap)
; to CODEFLAG/DATAFLAAG:

SmallBlanksToSameFlag:
    mov esi D$SectionsMap | add esi D$FirstSection
    mov edx D$EndOfSectionsMap | sub edx 4

    While esi < edx
        .If B$esi = 0
            mov edi esi, bl B$esi-1, ecx 0
            lodsb
            lodsb | inc ecx | cmp al bl | je L1>
                              cmp al 0 | ja L1>
            lodsb | inc ecx | cmp al bl | je L1>
                              cmp al 0 | ja L1>
            lodsb | inc ecx | cmp al bl | jne L2>
L1:         If al = bl
                rep stosb | dec esi
            End_If
        .End_If

L2:     inc esi
    End_While
ret
____________________________________________________________________________________________

;;
  Search for Chunks with non zeroed Bytes in UserPeStart and zeroed Flags in SectionsMap.
  Check if this is Code or not.
;;
[PreviousFlag: ?    NextFlag: ?    NewCodeChunkFound: ?]


____________________________________________________________________________________________

[SymCallApi: "call '", 0    SymTrue: "&TRUE", 0]

;;
 This the Main of the Symbolic Analyze. We search for all occurences of "call '...."
 with 'DisSearch'. If found, we call for 'ApiAnalyze', in order to seek for the called
 Function in our internal List. If found again, instead of zero, the 'DisApiSymbolsTable'
 holds Pointer(s) to the Api Parameter(s) Name(s). Example, for:

>KERNEL32._hwrite(
> HFILE hFile, ; handle to file
> LPCSTR lpBuffer, ; write data buffer
> long lBytes ; number of bytes to write
>)

 'DisApiSymbolsTable' will hold 3 dWords pointing to:
 'hFile', 'lpBuffer', 'lBytes'.

 Instead of overwriting the Source all along the Ananyles (what would take no end time),
 'MarkDisSymbols' simply overwrite 'Dataxxxxxx' with a 1 Flag first (1ataxxxxxx) and
 store the Pointer to the found Parameter name after this Ascii 1 Char.

 Only when the whole File parsing is done, 'ReWriteTheSymbolicSource' does the real
 replacement writing job at once, searching for all "1", loading the Pointer to Symbolic
 Name and writing it.
;;

SymbolicAnalyzes:
    mov esi SymCallApi, edi D$EndOfDisData | call FirstDisSearch

    While edi < D$SourceEnd
        push edi
            call ApiAnalyze
        pop edi

        On D$DisApiSymbolsTable <> 0, call MarkDisSymbols

        mov esi SymCallApi | call NextDisSearch

      ; Progress Bar:
        If edi > D$NextDisBarPos
            push edi
                mov eax edi | add eax D$DisBarStep | mov D$NextDisBarPos eax
                call BarProgress
            pop edi
        End_If
    End_While

    call ReWriteTheSymbolicSource
ret


; Searches esi zero ended String along edi (no reuse of available Search RosAsm features
; because here, the search is much simpler (perfectely ensured size, Case,... > faster):

[DisSearchLength: ?]

FirstDisSearch:
    mov ecx 0
    push esi
        While B$esi <> 0
            inc ecx | inc esi
        End_While
    pop esi
    sub ecx 4 | mov D$DisSearchLength ecx

NextDisSearch:
    mov eax D$esi | add esi 4

L1: While edi < D$SourceEnd
        cmp D$edi eax | je L2>
            inc edi
    End_While
    ret

L2: push esi, edi
        add edi 4 | mov ecx D$DisSearchLength | repe cmpsb | jne L3>

    pop eax, eax               ; Found: edi at end of searhced String.
    ret

L3: pop edi, esi
    inc edi | jmp L1<
ret


[ApiCallCopy: ? #40]

; Searching for the encounted Api call inside SpÂsm 'Win32ApiList'.
; When in: edi is pointing to the First Char of DLL Name in an api:
; call 'DLLNAME.FunctionNAME'
; ------^

ApiAnalyze:
  ; First Make a zero ended copy:
    mov esi edi, edi ApiCallCopy

    While B$esi <> "'"
        movsb
    End_While
    movsb | mov al 0 | stosb

L0: mov D$DisApiSymbolsTable 0

    mov edi D$Win32ApiList

L0: mov esi ApiCallCopy, eax D$esi | cmp eax D$edi | jne L1>

        mov ecx 80 | repe cmpsb
        On B$esi-1 = 'A', inc esi
        On B$esi-1 = 'W', inc esi
        cmp B$edi-1 '(' | jne L1>
        cmp B$esi-1 "'" | jne L1>

          ; >>> Found:

          ; Writes a 'Done Byte' in the List for searching once only per Function.
          ; (We reload the 'Functions.api' File at each Api Analyze).
            mov B$edi-1 0FF

            call RegisterApiParametersPointers | ret

L1: While B$edi <> ')'
        inc edi
    End_While
    add edi 3 | cmp B$edi 0 | jne L0<
ret


; Fitting Api Function found in 'Win32ApiList' by 'ApiAnalyze'. edi-1 is at the '('.

[DisApiSymbolsTable: ? #20]

; A fitting Api has been found inside 'Win32ApiList'. We now store in 'DisApiSymbolsTable',
; as many Pointer to the Api Parameters Names, as discribed in the 'SymbolicAnalyzes' top
; comment:

RegisterApiParametersPointers:
    mov esi edi, edi DisApiSymbolsTable
    If D$esi = 'VOID'
        mov D$edi 0 | ret
    End_If
    add esi 2

L0: add esi 2
    While B$esi > ' '
        inc esi
        If B$esi = ')'
            mov D$edi 0 | ret
        End_If
    End_While
    inc esi

  ; This happends with unknown optional parameters (like for: wnsprintf, in 'Win32ApiList'):
    If B$esi = ';'
        mov D$edi 0 | ret
    End_If

    mov D$edi esi | add edi 4       ; <<<< Write the Pointer in 'DisApiSymbolsTable'.

    While B$esi > LF | inc esi | End_While
    While B$esi <= ' ' | inc esi | End_While
    cmp B$esi ')' | jne L0<
    mov D$edi 0
ret


[OneDisLineUp | While B$edi <> LF | dec edi | End_While
                While B$edi < ' ' | dec edi | End_While
                While B$edi <> LF | dec edi | End_While | inc edi
                While B$edi = ' ' | inc edi | End_While]

;;
 Here, we are at an Api call, and Valid Parameters Names have been found in 'Win32ApiList'.
 We step back line after line and search for possible, for example, "push D$Data0403024'.
 At eaxh Parameter push found, we call for 'DisMarkAll' to mark all of this Label
 evocations with the 1 Flag and with the Symbol Name Pointer:
;;

MarkDisSymbols:
    push edi
        mov ebx DisApiSymbolsTable

L0:     OneDisLineUp | cmp D$edi 'push' | jne L9>

            add edi 5 | On W$edi = 'D$', add edi 2

            move D$PointerToApiList D$ebx

            .If D$edi = 'Code'
                call DisMarkAll
            .Else_If D$edi = 'Data'
                call DisMarkAll
            .End_If

            sub edi 5 | add ebx 4 | cmp D$ebx 0 | ja L0<
L9: pop edi
ret


[LengthOfDisReplaceString: ?    PointerToApiList: ?]
;;
 Mark all the occurences of the found Api Parameter with Ascii 1 + dWord Pointer to
 to Parameter Name inside 'Win32ApiList':
;;
DisMarkAll:
    pushad
    push D$DisSearchLength
        mov esi edi, edi SearchString, ecx 11
        rep movsb | mov B$edi 0

        mov edi D$CodeSource, D$DisSearchLength 7
      ; 11-4 (as we know the length, we don't need to call for 'FirstDisSearch' at all).

L0:     mov esi SearchString | call NextDisSearch
        While edi < D$SourceEnd

            sub edi 11 |  mov al 1 | stosb
            mov eax D$PointerToApiList | stosd
          ; 'Data0404204' is now replaced by:
          ; 'xYYYY404204', where x is the Byte value 1, YYYY the Pointer to the Parameter
          ; found inside RosAsm 'Win32ApiList'.

            mov esi SearchString | call NextDisSearch
        End_While

L9:     mov B$OnReplaceAll &FALSE
    pop D$DisSearchLength
    popad
ret


[SymbolicCodeSource: ?]

; General rewrite of all the Source with the real symbolic Names at each Flag 1 + dWord
; Pointer, instead:

ReWriteTheSymbolicSource:
    mov ecx D$EndOfSourceMemory | sub ecx D$CodeSource

    VirtualAlloc SymbolicCodeSource ecx
    mov D$eax CRLF2, D$eax+4 CRLF2

    add D$SymbolicCodeSource 8

    mov esi D$CodeSource, edi D$SymbolicCodeSource

    mov D$StartOfDisLine edi, D$AlignedComment edi | add D$AlignedComment 38

    .While esi < D$SourceEnd
        lodsb
        .If al = 1
            lodsd
            push esi
                mov esi eax
                While B$esi > ' ' | movsb | End_While
                On B$edi-1 = ',', dec edi

                mov W$edi '_0' | add edi 2
            pop esi

        .Else_If al = ':'
            stosb

        .Else
            stosb
            If al = LF
                mov D$StartOfDisLine edi, D$AlignedComment edi | add D$AlignedComment 38
            Else_If al = ';'
                dec edi | On D$edi-3 <> '   ;', jmp L2>
                While edi > D$AlignedComment
                    On B$edi-2 <> ' ', jmp L2>
                    dec edi
                End_While
L2:             mov B$edi ';' | inc edi
            End_If

        .End_If
    .End_While

L9:
;;
  There is some Variable somewhere, that i am unable to point out, and that produces
  a hang, if i 'Exchange' the usual way, and restore all the wished Variables. So, i
  do it the very stupid way: Copying:
;;
    mov ecx edi | sub ecx D$SymbolicCodeSource
    mov esi D$SymbolicCodeSource, edi D$CodeSource

    rep movsb

    mov D$SourceEnd edi | sub edi D$CodeSource | mov D$SourceLen edi

    push edi
        VirtualFree D$SymbolicCodeSource
    pop edi
ret

____________________________________________________________________________________________
;;
  Some PEs may have a wrong Paragraphs Number, in the MZ header. Fix It now to the
  correct values, because the Routines called for loading the various Resources
  make use of those values to point, through the various PE Headers, to the Resources
  Tree.
  
  We also get a Pointer to the Pe Tag (a Pointer to 'PE', in the PE header, as you
  can see, for example, at 'PeHeader'.
  
  A DOS 'Paragraph' is 16 Bytes. See Records at 'DosHeader':
  
  * At DosHeader+8 > Size of the Dos header in Paragraph units.
  
;;

[DisPeTagPointer: ?]

FixMzParagraphsNumber:
    mov D$DisPeTagPointer 0

  ; Standard manner: parag. size of dos header end > PE header address:
    mov esi D$UserPeStart | movzx eax W$esi+8 | shl eax 4 | sub eax 4
    If eax < D$UserPeLen
        add esi D$esi+eax
    Else
        mov eax 0
    End_If

    ..If eax <> 0
        mov edx D$UserPeStart | add edx D$UserPeLen | sub edx 4
        .If esi < edx
            If D$esi = 'PE'
                mov D$DisPeTagPointer esi | ret ; (No fix needed, in that case)
            End_If
        .End_If
    ..End_If

  ; Fix needed for these two other methods:
    call GetPeTagMethod2
    If esi < edx
        On D$esi = 'PE', jmp L1>
    End_If

    call GetPeTagMethod3 | On D$esi <> 'PE', jmp DisFail

L1: mov D$DisPeTagPointer esi

  ; Now, esi point to the PE Tag. Fix the MZ Paragraph Number for 'SearchPEstartOfResource':
    mov ecx esi | sub ecx D$UserPeStart

  ; Overwrite the Dos header according Displacement ('2' is for a minimum Dos Header):
    mov edi D$UserPeStart | mov W$edi+8 2, D$edi+(16+12) ecx

;  ; Test: Make sure it is now good for the default internal method:
;    mov esi D$UserPeStart | movzx eax W$esi+8 | shl eax 4 | sub eax 4 | add esi D$esi+eax
;    On D$esi <> 'PE', jmp DisFail

; With such an error, the Debugger fails, when compiling RosAsm with RosAsm...
; mov esi 0 | On D$esi <> 'PE', jmp DisFail
ret


GetPeTagMethod2:
  ; Method 2 for searching the PE Tag:
  ; If Origin >= 040 > PE header adress at 03C
    mov eax D$UserPeStart | add eax 018
    If W$eax >= 040
        sub eax 018 | add eax 03C | movzx esi W$eax | add esi D$UserPeStart
    End_If
ret

GetPeTagMethod3:
  ; Method 3. Stupid desesparated search:
    mov esi D$UserPeStart, ecx 0200
L0: inc esi | On D$esi <> 'PE', loop L0<
ret
____________________________________________________________________________________________

[DisNumberOfSections: ?
 DisDataMin: ?   DisDataMax: ?
 DisVirtualMin: ?   DisVirtualMax: ?
 DisCodeMin: ?   DisCodeMax: ?
 DisApiMin:  ?   DisApiMax:  ?
 LastCodeRef: ?    DisImageBase: ?    DisEntryPoint: ?    DisPeOrigine: ?
 DisRvaSectionAlignment: ?    DisFileSectionAlignment: ?
 DisBaseOfRsrc: ?]

[Disassembling: ?    ThisSourceIsDisassembled: ?]


; Used to switch from a RosAsm Data Label (for example, 'AppBaseOfCode') to the target
; File corresponding location:

[GetPeHeader | mov eax #1 | sub eax DosHeader | add eax D$DisPeOrigine
 add eax D$UserPeStart]

;;
  Here, we store several Data we need along Disassembling:
  The equivalents, in the UserPE of the Displacements found in our internal Stub;
  'PeHeader', 'SubSystem', 'NumberOfSections', 'ImageBase', Sections Alignment,
  EntryPoint ('AppRVAentryPoint').
;;

StartNewDisFile:
    mov D$DisPeOrigine 0 | GetPeHeader PeHeaderPointer

    mov eax D$eax | sub eax 080 | mov D$DisPeOrigine eax
  ; (080 is the RosAsm Data 'PeHeaderPointer')

    GetPeHeader PeHeader | mov eax D$eax | On eax <> D$PeHeader, jmp DisFail
  ; Pe Header found.

  ; Like in 'ReadHeaderFormat'.
  ; Copy / SubSystem / DllCharacteristics / AppStackMax / .... :
    GetPeHeader SubSystem | mov esi eax, edi SubSystem, ecx 5 | rep movsd

    GetPeHeader NumberOfSections | movzx eax W$eax | On eax = 0, jmp DisFail
    mov D$DisNumberOfSections eax

    GetPeHeader ImageBase | mov ebx D$eax, D$DisImageBase ebx, D$LinkerDllDefault ebx

    move D$DisRvaSectionAlignment D$eax+4, D$DisFileSectionAlignment D$eax+8

    GetPeHeader AppRVAentryPoint | mov eax D$eax | add eax D$DisImageBase
    mov D$DisEntryPoint eax
ret

____________________________________________________________________________________________

[TempoUserPeStart: ?    DisFlagsImage: ?    DisFlagsImageEnd: ?    DisRelocPointer: ?]

[FirstSection: ?    EndOfLastSection: ?]

;;
  Targetted File Mapping. Usually:

  The PE File alignment is on 0200 Bytes / Memory Image alignement is on 01000.
  In other words: On disk the Section are aligned on 0200h Boundaries // In
  Memory, the runing PE Sections are aligned on 01000h Boudaries, by the OS
  Launcher.
  
  The analyzes are made a bit easier by realigning the Sections the same way
  they are when runing in Memory.
;;

ReAlignPE:
    mov eax D$UserPeStart | sub D$UserPEStartOfResources eax ; Gona switch...

    mov ecx D$DisNumberOfSections, D$FirstSection 0-1, D$EndOfLastSection 0
    GetPeHeader SectionsHeaders

  ; Search for the First Section RVA:
L0: mov ebx D$eax+SECTION_RVA
    On ebx < D$FirstSection, mov D$FirstSection ebx
  ; Search for the last Section RVA and adds its RVA Size:
    push ecx
        mov ecx D$eax+SECTION_RVASIZE
      ; Some compiler (Watcom-C) may set the RVA to zero. So... :
        If ecx < D$eax+SECTION_FILESIZE
            mov ecx D$eax+SECTION_FILESIZE
            Align_On_Variable D$DisRvaSectionAlignment ecx
          ; Fix it (just in case this would be needed later...):
            mov D$eax+SECTION_RVASIZE ecx
        End_If

        On ebx > D$EndOfLastSection, mov D$EndOfLastSection ebx, edx ecx
    pop ecx
    add eax SECTIONHEADERSIZE | loop L0<

    add edx ebx | Align_On_Variable D$DisRvaSectionAlignment edx | mov D$UserPeLen edx

    VirtualAlloc TempoUserPeStart edx ;D$UserPeLen

    mov esi D$UserPeStart, edi D$TempoUserPeStart

  ; Copy the PE headers down to (including) 'SectionsHeaders':
    GetPeHeader SectionsHeaders
    mov ecx eax | sub ecx D$UserPeStart | rep movsb

    mov ecx D$DisNumberOfSections

L0: push ecx
        mov ecx SECTIONHEADERSIZE | rep movsb
    pop ecx
    loop L0<

  ; Want to skip 'RelocSectionTable', if any:
    GetPeHeader RelocSectionTable | move D$DisRelocPointer D$eax
  ; Copy all Sections with Memory alignment:
    GetPeHeader SectionsHeaders | mov edx D$DisNumberOfSections

    While D$eax+SECTION_RVA <> 0
        mov esi D$eax+SECTION_FILEPOINTER | add esi D$UserPeStart
        mov edi D$eax+SECTION_RVA | On edi = D$DisRelocPointer, jmp L1>
        add edi D$TempoUserPeStart
        mov ecx D$eax+SECTION_FILESIZE | Align_On 4 ecx | shr ecx 2 | rep movsd
L1:     add eax SECTIONHEADERSIZE | dec edx | jz L2>
    End_While

L2: Exchange D$UserPeStart D$TempoUserPeStart
    mov eax D$UserPeStart | add eax D$UserPeLen | mov D$UserPeEnd eax
    VirtualFree D$TempoUserPeStart
ret

____________________________________________________________________________________________
;;
  Allocations of 'SectionsMap', 'RoutingMap', 'SizesMap' (more comments ther) Tables
  (All same length as the mapped File):
;;

AllocateDisTables:
    VirtualAlloc SectionsMap D$UserPeLen,
                 RoutingMap D$UserPeLen,
                 SizesMap D$UserPeLen
    mov eax D$SectionsMap | add eax D$UserPeLen | mov D$EndOfSectionsMap eax
    mov eax D$RoutingMap | add eax D$UserPeLen | mov D$EndOfRoutingMap eax
    mov eax D$SizesMap | add eax D$UserPeLen | mov D$EndOfSizesMap eax

    If D$StringsMap = 0
        mov eax D$UserPeLen | shl eax 2
        push eax
            VirtualAlloc StringsMap eax
            move D$EndOfStringsMap D$StringsMap
        pop ecx
        add D$EndOfStringsMap ecx
    End_If

  ; And widely evaluated disassembly Source:
    mov ecx D$UserPeLen | shl ecx 4 | add ecx 10_000_000
    push ecx
        VirtualAlloc CodeSource ecx
        mov D$eax CRLF2, D$eax+4 CRLF2 | add D$CodeSource 8
    pop ecx
    add ecx D$CodeSource | mov D$EndOfSourceMemory ecx

  ; restore the true length (without the security tail):
    sub D$UserPeLen 01000

    mov edi D$CodeSource, eax CRLF2, ecx 100 | rep stosd

    mov eax D$UserPeStart | add D$UserPEStartOfResources eax  ; Switch done...
ret

HLL_Strings_Table_notes:

;;
  * Create one another paralel Table: 'SymbolsMap'
  
  * Symbol wanted at 'SomeRoom': Set the 'STRINGS' Flag.
  
  * Store the String, zero-ended, in a StringsTable.
  
  * Store the Pointer to the zero-ended String in a 'StringsPointersTable':
    Record: [... / SomeRoom, StringPointer / ...]
  
  'STRINGS' Flag encouted in 'SymbolsMap'
  >>> Search for the 'SomeRoom' Displacement, in the 'StringsPointersTable'
  >>> take the 'StringPointer'.
  
  Length? Why not using directly the String origine? Example the "Functions.api"
  File. Just a Pointer to the Name (>= ' ' ending).
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[DisImportPtr: ?    DisImportSize: ?]

;;
 Import Section looks like this:

[DirectoryTable:
 LookUpRVA: D$ 03050 TimeDate: 0 ForwarderChain: 0 NameRVA: 03136 AdressRVA: 030A8
 LookUpRVA: D$ 03060 TimeDate: 0 ForwarderChain: 0 NameRVA: 03164 AdressRVA: 030B8
 ...
 LookUp1: (03050:)
;;

[DisDllName: ?   StartOfDisImport: ?    EndOfDisImport: ?   FirstDisImportName: ?
 PointerToDisImportDirectory: ?    PointerToDisImportAddressTable: ?
 DisImportRVA: ?    DisImportFilePointer: ?    LastPointerInOurApiTable: ?
 DllMin: ?    DllMax: ?]

[DisNumberOfFunctions: ?    ApiBuffer: ?    EndOfApiBuffer: ?]

CheckImport:
    GetPeHeader AppImportSize | mov eax D$eax | On eax = 0, ret ;jmp DisFail

  ; !!! 'AppImportSize' is _NOT_ the size of Import, but the one of the Import
  ; Header only !!! We can't use it for defining the 'ApiBuffer' size !!!
  ; Done at the end, based on the number of found Functions.

    GetPeHeader AppBaseOfImport | mov edx D$eax | On edx = 0, jmp DisFail

    add edx D$UserPeStart
;;
  The Import Directory is a flow of blocks of 5 dWords (+ zero ended with 5 dWords):
  
  Import LookUp Table RVA
  Time and Date Stamp
  Forwarder Chain
  Name RVA                    ; <<<<<  MODULE.dll
  Import Addresses Table RVA  ; <<<<<  Function
  
  Import Addresses Table RVA is flow of dWords Pointer to Functions Names for each DLL.
  (Zero ending dWord).
  
  We first fill all the SectionsMap part of this Tree-like Structure with IMPORTFLAG:

  If either 'Import LookUp Table RVA' or 'Import Addresses Table RVA' are zeroed,
  we fill the one by the other, in order to save from no end Checking:
;;
    mov eax edx

    While D$eax+(4*3) <> 0          ; At least 'Name RVA' should be there ;)
        If D$eax = 0
            move D$eax D$eax+(4*4)
        Else_If D$eax+(4*4) = 0
            move D$eax+(4*4) D$eax
        End_If
        add eax (5*4)
    End_While

    push edx
        mov eax IMPORTFLAG+(IMPORTFLAG shl 8)+(IMPORTFLAG shl 16)+(IMPORTFLAG shl 24)

        mov edi edx | sub edi D$UserPeStart | add edi D$SectionsMap

        ..While D$edx <> 0
          ; Flag the Directory (dWords):
            mov edi edx, ecx 5
            sub edi D$UserPeStart | add edi D$SectionsMap | rep stosd
          ; Flag the LookUp Table (dWords):
            mov edi D$edx, ebx edi | add edi D$SectionsMap | add ebx D$UserPeStart
            While D$ebx <> 0
                ;call FlagImportCalls ebx
                stosd | add ebx 4
            End_While | stosd
          ; Flag the DLL Name (Bytes):
            mov edi D$edx+(3*4), ebx edi | add edi D$SectionsMap | add ebx D$UserPeStart
            While B$ebx <> 0 | stosb | inc ebx | End_While | stosb
          ; Flag the Address Table (dWords):
            mov edi D$edx+(4*4), ebx edi | add edi D$SectionsMap | add ebx D$UserPeStart
            While D$ebx <> 0
               ; call FlagImportCalls ebx
                stosd | add ebx 4
            End_While | stosd
          ; The Import Address Table may be either empty or filled up with any value.
          ; we first recopy the LoockUp Table upon it:
            push esi, edi
                mov esi D$edx, edi D$edx+(4*4)
                add esi D$UserPeStart | add edi D$UserPeStart
                While D$esi <> 0 | movsd | End_While
            pop edi, esi
          ; Flag the Functions String (Bytes, pointed by LookUp // Address Tables):
            mov ebx D$edx | add ebx D$UserPeStart
            .While D$ebx <> 0
                push ebx
                    mov ebx D$ebx, edi ebx, ecx ebx | and ecx 0_8000_0000
                    If ecx = 0_8000_0000 ; By Number

                    Else                 ; By Name
                        add ebx D$UserPeStart | add edi D$SectionsMap
                        stosw | add ebx 2
                        While B$ebx <> 0 | stosb | inc ebx | End_While | stosb
                    End_If
                pop ebx
                add ebx 4
            .End_While

            add edx (5*4)
        ..End_While
      ; Flag the Directory zero ending (5 dWords, too):
        mov edi edx, ecx 5
        sub edi D$UserPeStart | add edi D$SectionsMap | rep stosd
    pop edx
;;
  We fill the relative Flags image with the Pointers to DLL Names (instead of a Flag),
  at each Function Name Address, so that, when the Flag Image will hold a Pointer,
  instead of a Flag, this will be the Pointer to the DLL Name, and the Pointer to the
  Function Name will appear in the relative Pos in UserPeStart:
;;
    push edx
      ; (edx keeps track of the .import Base)
        .While D$edx <> 0
            mov eax D$edx+12 | add eax D$UserPeStart    ; Pointer to DLL Name
            On D$eax = 'MSVB', mov D$CompiledBy 'MSVB'
            mov ebx D$edx+16 | add ebx D$UserPeStart    ; Pointer to Functions Names List.
            mov edi D$RoutingMap | add edi D$edx+16
        ; Write the Pointer to DLL Name upon all 'RoutingMap' Pointers to Functions Names:
            While D$ebx <> 0
                stosd | add ebx 4
            End_While

            mov ebx D$edx+16 | add ebx D$UserPeStart
            mov edi D$SectionsMap | add edi D$edx+16
            mov eax IMPORTFLAG
        ; Write again IMPORTFLAG upon all 'SectionsMap' Pointers to Functions Names:
            While D$ebx <> 0
                stosb | inc ebx
            End_While

            add edx 20
        .End_While

    pop edx
;;
  Once done, when we encount a Pointer, and see 'IMPORTFLAG' in the relative 'SectionsMap'
  dWord / >>> We have the DLL Name Pointer in the relative 'RoutingMap' dWord and the
  the Pointer to the Function Name in the relative 'UserPeStart' dWord.
  
  Now, we make a Copy of 'DllName.FunctionName' into 'ApiBuffer', and replace all
  Address Table and LookUp Table dWords by Pointers to these new Strings:
  
  Search the Number of Functions, first, in order to guess what size for 'ApiBuffer'.
;;
    push esi, edx

        mov D$DisNumberOfFunctions 0

      ; Pointer to Functions Names Address Table in edx:
L0:     mov esi D$edx+(4*4) | add esi D$UserPeStart
        While D$esi <> 0
            add esi 4 | inc D$DisNumberOfFunctions
        End_While

        add edx 20 | cmp D$edx 0 | jne L0<<

    pop edx, esi

  ; Large estimation: 128 Chars per Function:
    mov eax D$DisNumberOfFunctions | shl eax 7
    pushad
        push eax
            VirtualAlloc ApiBuffer eax
        pop ecx
        add ecx eax | mov D$EndOfApiBuffer ecx
    popad

  ; Now, fill the ApiBuffer:
    mov edi D$ApiBuffer

L0: mov ebx D$edx+(3*4) | add ebx D$UserPeStart    ; Pointer to DLL Name.
    mov esi D$edx+(4*4) | add esi D$UserPeStart    ; Pointer to Functions Names Address Table.

    While D$esi <> 0
        mov ecx edi

        mov B$edi "'" | inc edi
        push ebx
L1:         mov al B$ebx | stosb | inc ebx | cmp al 0 | jne L1<   ; 'DllName.
        pop ebx
        mov eax D$edi-5 | or eax 020202020
        If eax = '.dll'
            sub edi 4
        Else
            mov B$edi-1 "."
        End_If

        lodsd | test eax 08000_0000 | jz L1>
            xor eax 08000_0000 | push ebx | call WriteEax | pop ebx | jmp L2>

L1:     push ebx
            mov ebx eax | add ebx D$UserPeStart | add ebx 2
L1:         mov al B$ebx | stosb | inc ebx | cmp al 0 | jne L1<   ; ....FunctionName'
            dec edi
        pop ebx

L2:     mov B$edi "'" | inc edi | mov B$edi 0 | inc edi

      ; Overwrite Address Table and LookUp Table dWords by Pointers to 'ApiBuffer':
        mov D$esi-4 ecx
        mov eax esi | sub eax D$edx+(4*4) | add eax D$edx | mov D$eax-4 ecx
    End_While

    add edx 20 | cmp D$edx 0 | jne L0<<

  ; ... and clear the temporary used 'RoutingMap'
    mov edi D$RoutingMap, ecx D$EndOfRoutingMap, eax 0
    sub ecx edi | shr ecx 2 | rep stosd

    call KillBlankBytes AppImportSize, IMPORTFLAG
ret

____________________________________________________________________________________________

[ResourcesTypesNumber: ?    DisResourcesOrigine: ?]

; Resources have be loaded by LoadDisResources. We now simply Flag the Resources Section.
; (Only the real Resources -not the whole Section that main contain anything else-).

CheckResources:
    GetPeHeader AppBaseOfRsrc | On D$eax = 0, ret

    mov eax D$eax | add eax D$UserPeStart | mov D$DisResourcesOrigine eax

    call FlagResourceTree eax

    call KillBlankBytes AppBaseOfRsrc, RESOURCESFLAG
ret

;;
  'FlagResourceTree' is a good example of Procedure calling itself, for moving along
  all branches of a tree.
;;

Proc OldFlagResourceTree: ; 'ResourcesStub' For infos. 'NewFlagResourceTree'
    Argument @Pointer
    Local @N
    Uses esi

        mov esi D@Pointer | add esi 12
      ; By Name references:
        lodsw | mov W@N ax
      ; By ID number references:
        lodsw | add W@N ax

        mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap
        mov D$ebx-16 FOUR_RESOURCESFLAG,
            D$ebx-12 FOUR_RESOURCESFLAG,
            D$ebx-8 FOUR_RESOURCESFLAG,
            D$ebx-4 FOUR_RESOURCESFLAG,
            D$ebx FOUR_RESOURCESFLAG,
            D$ebx+4 FOUR_RESOURCESFLAG

          ; esi now points to the Type/ID/Lang and Displacement dWords pairs.
          ; We Skip the ID and retrieve the Pointer:
L0:         add esi 4 | lodsd

          ; If the Value include the High Bit (example: 0_80000020), this is a node)
          ; No High Bit, this is a leaf.

            .If eax >= 08000_0000
                xor eax 08000_0000 | add eax D$DisResourcesOrigine
                call FlagResourceTree eax

                If W@N > 1
                  ; Flag the next coming Type/ID/Lang and Displacement dWords pair:
                    mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap
                    mov D$ebx FOUR_RESOURCESFLAG, D$ebx+4 FOUR_RESOURCESFLAG
                    dec W@N | jmp L0<
                End_If

            .Else
              ; Flag the leaf Pointer and size, and then the true Resources Data:
                add eax D$DisResourcesOrigine
                mov edi D$eax, ecx D$eax+4
                add edi D$DisResourcesOrigine
                GetPeHeader AppBaseOfRsrc | sub edi D$eax
                sub edi D$UserPeStart | add edi D$SectionsMap
                mov al RESOURCESFLAG | rep stosb

            .End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Guga proposition:

;;

Resource structures

[IMAGE_RESOURCE_DIRECTORY:
 Characteristics: D$ 0
 TimeDateStamp: D$ 0
 MajorVersion: W$ 0
 MinorVersion: W$ 0
 NumberOfNamedEntries: W$ 0
 NumberOfIdEntries: W$ 0]

[ImgResDir.CharacteristicsDis 0
 ImgResDir.TimeDateStampDis 4
 ImgResDir.MajorVersionDis 8
 ImgResDir.MinorVersionDis 10
 ImgResDir.NumberOfNamedEntriesDis 12
 ImgResDir.NumberOfIdEntriesDis 14]

[Size_Of_IMAGE_RESOURCE_DIRECTORY 16]

; followed by an array of XX IMAGE_RESOURCE_DIRECTORY_ENTRY. The total amount of elements
 on theg array are the sum of NumberOfNamedEntries + NumberOfIdEntries.

[IMAGE_RESOURCE_DIRECTORY_ENTRY:
 Name1: Id: D$ 0
 OffsetToData: D$ 0]
 
 Name1 = This field contains either an integer ID or a pointer to a structure that contains a string name.
         If the high bit (0x80000000) is zero, this field is interpreted as an integer ID.
         Th ID is the type of the resource, such as a dialog, an icon, an bitmap image etc. It can be one of the
         following equates (This member does not allow combination of the equates):
         &RT_CURSOR, &RT_BITMAP, &RT_ICON, &RT_MENU, &RT_DIALOG, &RT_STRING, &RT_FONTDIR, &RT_FONT, &RT_ACCELERATOR,
         &RT_RCDATA, &RT_GROUP_CURSOR, &RT_GROUP_ICON, &RT_MESSAGETABLE, &RT_VERSION, &RT_DLGINCLUDE, &RT_PLUGPLAY,
         &RT_VXD, &RT_ANICURSOR

            Ex.: 05 = the ID of the data resource. It is a dialog (&RT_DIALOG)
 
         If the high bit is nonzero, the lower 31 bits are an offset (relative to the start of the resources)
            to an IMAGE_RESOURCE_DIR_STRING_U structure.
            Ex.: 080000688 = 080000000+DataOffset-IMAGE_RESOURCE_DIRECTORY_ENTRY (The main one, that is the 1st found in the section)
                So, on the example we are at byte 0688 from the start of the resources section.
                At byte 0688 we will have a IMAGE_RESOURCE_DIR_STRING_U structure
            This structure contains a WORD character count, followed by a UNICODE string with the resource name.
            Yes, even PE files intended for non-UNICODE Win32 implementations use UNICODE here.
            To convert the UNICODE string to an ANSI string, use the WideCharToMultiByte function.
                [IMAGE_RESOURCE_DIR_STRING_U:
                    Length1: W$ 0   The length of the string
                    NameString: W$ 0] The unicode string. This string is non null terminated, but an additional word
                                      may be inserted after the string to make next field start on a dword boundary.

OffsetToData = This field is either an offset to another resource directory or a pointer to information about
               a specific resource instance.
               
               If the high bit (0x80000000) is set, this directory entry refers to a subdirectory.
                The lower 31 bits are an offset (relative to the start of the resources) to another IMAGE_RESOURCE_DIRECTORY.
                Ex.: 080000468 = 080000000+DataOffset-IMAGE_RESOURCE_DIRECTORY_ENTRY (The main one, that is the 1st found in the section)
                So, on the example we are at byte 0468 from the start of the resources section.
                At byte 0468 we will have another IMAGE_RESOURCE_DIRECTORY structure
               
               If the high bit isn't set, the lower 31 bits point to an IMAGE_RESOURCE_DATA_ENTRY structure.
               This is called as "leaf node".
                Ex.: 0EA0 = DataOffset-IMAGE_RESOURCE_DIRECTORY_ENTRY (The main one, that is the 1st found in the section)
                     At byte 0EA0 we will have an IMAGE_RESOURCE_DATA_ENTRY structure
                The IMAGE_RESOURCE_DATA_ENTRY structure contains the location of the resource's raw data, its size, and its code page.
                    [IMAGE_RESOURCE_DATA_ENTRY:
                        OffsetToData: D$ 0
                        Size1: D$ 0
                        CodePage: D$ 0
                        Reserved: D$ 0]

                OffsetToData = location of the actual resource data. Since this information is used primarily
                               by functions once the application has been loaded,
                               it makes more sense to make the OffsetToData field a relative virtual address.

                                This is precisely the case.
               
                                Interestingly enough, all other offsets, such as pointers from directory entries
                                to other directories, are offsets relative to the location of the root node.
                                Ex.: 0E1608 = DataOffset-ImageBase (The start of thge PE file)
               
                Size1 = size of the actual resource data.
               
                CodePage = Code page is the traditional IBM term used for a specific character encoding table:
                           a mapping in which a sequence of bits, usually a single octet representing integer values
                           0 through 255, is associated with a specific character. IBM and Microsoft often allocate a
                           code page number to a character set even if that charset is better known by another name.

                           Whilst the term code page originated from IBM's EBCDIC-based mainframe systems, the term is
                           most commonly associated with the IBM PC code pages. Microsoft, a maker of PC operating systems,
                           refers to these code pages as OEM code pages, and supplements them with its own "ANSI" code pages.

                           Most well-known code pages, excluding those for the CJK languages and Vietnamese, represent character
                           sets that fit in 8 bits and don't involve anything that can't be represented by mapping each code to a
                           simple bitmap, such as combining characters, complex scripts, etc.

                           The text mode of standard (VGA compatible) PC graphics hardware is built around using an 8 bit
                           code page, though it is possible to use two at once with some color depth sacrifice, and up to
                           8 may be stored in the display adaptor for easy switching).
                           
                           There were a selection of code pages that could be loaded into such hardware.
                           
                           However, it is now commonplace for operating system vendors to provide their own character encoding
                           and rendering systems that run in a graphics mode and bypass this system entirely.
                           
                           The character encodings used by these graphical systems (particularly Windows) are sometimes
                           called code pages as well.
                           
                            - Relationship to ASCII. -
                           The basis of the IBM PC code pages is ASCII, a 7-bit code representing 128 characters and control
                           codes. In the past, 8-bit extensions to the ASCII code often either set the top bit to zero,
                           or used it as a parity bit in network data transmissions.
                           When this bit was instead made available for representing character data, another 128 characters
                           and control codes could be represented. IBM used this extended range to encode characters used
                           by various languages.
                           No formal standard existed for these 'extended character sets'; IBM merely referred to the variants
                           as code pages, as it had always done for variants of EBCDIC encodings.
               
                            - IBM PC (OEM) code pages -

                            These code pages are most often used under MS-DOS-like operating systems;
                            they include a lot of box drawing characters. Since the original IBM PC code page (number 437)
                            was not really designed for international use, several incompatible variants emerged.
                            Microsoft refers to these as the OEM code pages. Examples include:

                                * 437 — The original IBM PC code page
                                * 737 — Greek
                                * 850 — "Multilingual (Latin-1)" (Western European languages)
                                * 852 — "Slavic (Latin-2)" (Eastern European languages)
                                * 855 — Cyrillic
                                * 857 — Turkish
                                * 858 — "Multilingual" with euro symbol
                                * 860 — Portuguese
                                * 861 — Icelandic
                                * 863 — French Canadian
                                * 865 — Nordic
                                * 866 — Cyrillic
                                * 869 — Greek

                            - Other code pages of note -

                                * 10000 — Macintosh Roman encoding (followed by several other Mac character sets)
                                * 10007 — Macintosh Cyrillic encoding
                                * 10029 — Macintosh Central European encoding
                                * 932 — Supports Japanese
                                * 936 — GBK Supports Simplified Chinese
                                * 949 — Supports Korean
                                * 950 — Supports Traditional Chinese
                                * 1200 — UCS-2LE Unicode little-endian
                                * 1201 — UCS-2BE Unicode big-endian
                                * 65001 — UTF-8 Unicode
                                * ASMO449+ — Supports Arabic

                            In modern applications, operating systems and programming languages, the IBM code pages
                            have been rendered obsolete by newer & better international standards, such as ISO 8859-1
                            and Unicode.

                            - Windows (ANSI) code pages -

                            Microsoft defined a number of code pages known as the ANSI code pages (as the first one, 1252
                            was based on an ansi draft of what became ISO 8859-1). Code page 1252 is built on ISO 8859-1
                            but uses the range 0x80-0x9F for extra printable characters rather than the C1 control codes
                            used in ISO-8859-1.
                            Some of the others are based in part on other parts of ISO 8859 but often rearranged to make
                            them closer to 1252.

                                * 1250 — East European Latin
                                * 1251 — Cyrillic
                                * 1252 — West European Latin
                                * 1253 — Greek
                                * 1254 — Turkish
                                * 1255 — Hebrew
                                * 1256 — Arabic
                                * 1257 — Baltic
                                * 1258 — Vietnamese

                Reserved = Reserved data. Do not use.
;;

;;
  'FlagResourceTree' is a good example of Procedure calling itself, for moving along
  all branches of a tree.
;;

; IMAGE_RESOURCE_DIRECTORY structure

[ImgResDir.CharacteristicsDis 0
 ImgResDir.TimeDateStampDis 4
 ImgResDir.MajorVersionDis 8
 ImgResDir.MinorVersionDis 10
 ImgResDir.NumberOfNamedEntriesDis 12
 ImgResDir.NumberOfIdEntriesDis 14]

[Size_Of_IMAGE_RESOURCE_DIRECTORY 16]

; IMAGE_RESOURCE_DIRECTORY_ENTRY structure

[ImgResDirEntry.Name1Dis 0
 ImgResDirEntry.OffsetToDataDis 4]

[Size_Of_IMAGE_RESOURCE_DIRECTORY_ENTRY 8]

; IMAGE_RESOURCE_DIR_STRING_U structure

[ImgResDirStringU.Length1Dis 0]
; ImgResDirStringU.NameString the size of the unicode string depends of the value of Length1

; IMAGE_RESOURCE_DATA_ENTRY structure

[ImgResDataEntry.OffsetToDataDis 0
 ImgResDataEntry.Size1Dis 4
 ImgResDataEntry.CodePageDis 8
 ImgResDataEntry.ReservedDis 12]

[Size_Of_IMAGE_RESOURCE_DATA_ENTRY 16]

Proc FlagResourceTree: ; 'ResourcesStub' For infos.
    Argument @Pointer
    Local @Array
    Uses esi, ebx, eax

    mov esi D@Pointer, eax 0

    push esi
      ; add ImgResDir.NumberOfNamedEntries to ImgResDir.NumberOfIdEntries
      ; and copy the result to N:
        add esi ImgResDir.NumberOfNamedEntriesDis
        lodsw | mov D@Array eax
        lodsw | add D@Array eax
    pop esi

    ; now we flag all IMAGE_RESOURCE_DIRECTORY
    mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap
    mov eax RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)
    mov D$ebx+ImgResDir.CharacteristicsDis eax, D$ebx+ImgResDir.TimeDateStampDis eax,
    W$ebx+ImgResDir.MajorVersionDis ax, W$ebx+ImgResDir.MinorVersionDis ax,
    W$ebx+ImgResDir.NumberOfNamedEntriesDis ax, W$ebx+ImgResDir.NumberOfIdEntriesDis ax

    ; esi and ebx now points to the IMAGE_RESOURCE_DIRECTORY_ENTRY.
    add esi Size_Of_IMAGE_RESOURCE_DIRECTORY
    add ebx Size_Of_IMAGE_RESOURCE_DIRECTORY

          ; We need to see if we have a Unicode String Name or a ID
L0:         lodsd ; load the name ID to eax
            ; flag the Name1Dis member
            mov D$ebx+ImgResDirEntry.Name1Dis RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)
            ;add ebx ImgResDirEntry.OffsetToDataDis ; update ebx to it points to the offsettodata in the sectionmap

            Test_If eax 08000_0000 ; If it is a named ID, flag all the IMAGE_RESOURCE_DIR_STRING_U structure
                xor eax 08000_0000
                add eax D$DisResourcesOrigine ; eax is now at IMAGE_RESOURCE_DIR_STRING_U
                movzx ecx W$eax ; ecx now contains the lenght of our unicode string
                sub eax D$userpestart ; let´s point it to the proper location
                add eax D$SectionsMap ; in the section map
                ; flag the lenght member
                mov W$eax+ImgResDirStringU.Length1Dis RESOURCESFLAG+(RESOURCESFLAG shl 8)
                add eax 2 ; points to the begginning of the unicode string
                ; flag the unicode string
                mov edi eax
                mov ax RESOURCESFLAG+(RESOURCESFLAG shl 8) | rep stosw
            Test_End

            ; Now, load the OffsetToData and save it´s value at eax
            lodsd
            ; flag the OffsetToDataDis member
            mov D$ebx+ImgResDirEntry.OffsetToDataDis RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)
            add ebx Size_Of_IMAGE_RESOURCE_DIRECTORY_ENTRY ; update ebx

            .Test_If eax 08000_0000 ; If the high bit (0x80000000) is set this is a node
                xor eax 08000_0000 | add eax D$DisResourcesOrigine
                call FlagResourceTree eax

                If D@Array > 1
                  ; let´s flag the rest of the array IMAGE_RESOURCE_DIRECTORY_ENTRY.
                    dec D@Array | jmp L0<<
                End_If

            .Test_Else ; If the high bit (0x80000000) is not set this is a leaf
                ; Flag all the IMAGE_RESOURCE_DATA_ENTRY and their internal pointers
                add eax D$DisResourcesOrigine
                mov ebx eax
                sub ebx D$UserPeStart
                add ebx D$SectionsMap

                mov D$ebx+ImgResDataEntry.OffsetToDataDis RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)
                mov D$ebx+ImgResDataEntry.Size1Dis RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)
                mov D$ebx+ImgResDataEntry.CodePageDis RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)
                mov D$ebx+ImgResDataEntry.ReservedDis RESOURCESFLAG+(RESOURCESFLAG shl 8)+(RESOURCESFLAG shl 16)+(RESOURCESFLAG shl 24)

                ; flag the data contents
                mov edi D$eax+ImgResDataEntry.OffsetToDataDis
                mov ecx D$eax+ImgResDataEntry.Size1Dis
                add edi D$SectionsMap; edi points to the offset of the data in the sectinosmap
                mov al RESOURCESFLAG | rep stosb

            .Test_End
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 If no Virtual Data Section is found, the Application is allowed (RosAsm works like
 this, and many other Compilers), to consider as Virtual (uninitialized) Data the
 room left between the End of Data and the Virtual End of .Data section. In
 'SectionsHeaders', this is the difference between the first dWord (after the name)
 and the third dWord.

 Here, if we find no Virtual Data Section, we simply exit and let in the Values
 defined in upper 'ReadDataSection'.
;;

[VirtualIsRunable: ?]

CheckVirtualData:
    GetPeHeader SectionsHeaders | mov ecx D$DisNumberOfSections

L0: test D$eax+SECTION_FLAG &IMAGE_SCN_CNT_UNINITIALIZED_DATA | jz L3>
        test D$eax+SECTION_FLAG &IMAGE_SCN_MEM_READ | jz L3>
            test D$eax+SECTION_FLAG &IMAGE_SCN_MEM_WRITE | jz L3>

                test D$eax &IMAGE_SCN_MEM_EXECUTE | jz L2>
                    mov B$VirtualIsRunable &TRUE            ; See this pb later.

  ; Regular Virtual Data Section found. Check the Bytes in 'SectionMap':
L2: mov ebx D$eax+SECTION_RVA | add ebx D$SectionsMap
    mov edi D$SectionsMap | add edi D$eax+SECTION_RVA
    mov edx edi | add edx D$eax+SECTION_FILESIZE
    push edi
        While edi < edx | mov B$edi VIRTUALFLAG | inc edi | End_While
    pop edi
  ; Force the First Virtual Byte EVOCATED:
    sub edi D$SectionsMap | add edi D$RoutingMap | mov B$edi EVOCATED

L3: add eax SECTIONHEADERSIZE | loop L0<
ret


CheckExtendedVirtual:
    GetPeHeader SectionsHeaders | mov ecx D$DisNumberOfSections
    push ecx ;jE! fixing incorrect loop; bcoz if header is dirty, then will bad..
L0: mov ecx D$eax+SECTION_RVASIZE, ebx D$eax+SECTION_FILESIZE
    Align_On_Variable D$DisRvaSectionAlignment ecx
    Align_On_Variable D$DisFileSectionAlignment ebx
    .If ecx > ebx
        test D$eax+SECTION_FLAG &IMAGE_SCN_MEM_EXECUTE | jz L2>
            mov B$VirtualIsRunable &TRUE            ; See this pb later.

L2:     mov edi D$SectionsMap | add edi D$eax+SECTION_RVA | add edi D$eax+SECTION_FILESIZE
      ; Don't touch Reloc and DebugInfo:
        If B$edi <> KILLFLAG
          ; Force the First Virtual Byte EVOCATED:
            mov edx edi | sub edx D$SectionsMap | add edx D$RoutingMap | mov B$edx EVOCATED

            push eax
                mov ecx D$eax+SECTION_RVASIZE
                Align_On_Variable D$DisRvaSectionAlignment ecx
                sub ecx ebx | mov al VIRTUALFLAG | rep stosb
            pop eax
        End_If
    .End_If

L3: dec D$esp | jle L4> ;jE!
    add eax SECTIONHEADERSIZE | cmp D$eax+SECTION_RVA 0 | ja L0<<
    jmp L3< ;jE! VirtualSize can be 0!
L4: add esp 4 ;jE!
ret
____________________________________________________________________________________________

[NumberOfDisExportedFunctions: ?  NumberOfDisExportNames: ?  DisExportFunctionsPointers: ?
 DisExportNamesPointers: ? DisExportInside: ? DisExportOrdinal: ? DisExportOrdBase: ?]

; 'ExportSectionComments'.

[NumberOfForwardedExport: ?]

[ForwardedMessage: '     ', Forwarded: "Forwarded Exports found in this Module. 
   
 RosAsm Assembler does not assume this method.    
 The rebuilt Module will therefore not work
 like the original (missing Functions).", 0]

CheckExport:
    GetPeHeader SectionTable | On D$eax = 0, ret

    mov D$NumberOfForwardedExport 0

    push eax
        mov edi D$eax, ecx D$eax+4, al EXPORTFLAG
        add edi D$SectionsMap | rep stosb
    pop eax

    mov edx D$eax | add edx D$UserPeStart
    mov eax D$edx+(5*4), ebx D$edx+(6*4), ecx D$edx+(4*4)
    ;On ebx > eax, mov eax ebx
    add edx (6*4)
    mov D$NumberOfDisExportedFunctions eax, D$NumberOfDisExportNames ebx, D$DisExportOrdBase ecx
; 0476 = 1142 in wsock32.dll !!!   04B  Forwarded Functions (forwarded to other DLLs) !!!
    On D$NumberOfDisExportedFunctions = 0, jmp L9>>
  ; 'ExportSectionComments'
    add edx 4 | mov eax D$edx | add eax D$UserPeStart
    mov D$DisExportFunctionsPointers eax

    add edx 4 | mov eax D$edx | add eax D$UserPeStart
    mov D$DisExportNamesPointers eax

    add edx 4 | mov eax D$edx | add eax D$UserPeStart
    mov D$DisExportOrdinal eax

  ; Mark the Exported Functions as Nodes in the Code Routing Table:
    mov esi D$DisExportFunctionsPointers, ecx D$NumberOfDisExportedFunctions

L0: lodsd
    .If eax = 0
        loop L0<
    .Else
        add eax D$SectionsMap
        If B$eax = EXPORTFLAG
            inc D$NumberOfForwardedExport | loop L0<
        Else
            sub  eax D$SectionsMap | add eax D$RoutingMap
            or B$eax EXPORTNODE+ACCESSED+EVOCATED+LABEL
            sub eax D$RoutingMap | add eax D$SectionsMap | mov B$eax 0 | loop L0<
        End_If
    .End_If

    If D$NumberOfForwardedExport <> 0
        mov eax D$NumberOfForwardedExport, edi ForwardedMessage
        call WriteEaxDecimal
        While edi < Forwarded | mov B$edi ' ' | inc edi | End_While
        call 'USER32.MessageBoxA', 0, ForwardedMessage,
                                  {' Warning', 0}, 0
    End_If
ret

; Old flaging method:

  ; Flag the SectionsMap with EXPORTFLAG (only the real Export Table. Not
  ; the whole Section, that may contain anything else -Import / Data /...-):
L1: GetPeHeader SectionTable | mov esi D$eax | add esi D$UserPeStart

    mov al EXPORTFLAG

  ; First, the Header (10 dWords).
    mov ecx (10*4)
    mov edi esi | sub edi D$UserPeStart | add edi D$SectionsMap
    rep stosb

  ; 'ExportSectionComments'.
  ;
  ; DllName:
    mov edi D$esi+(3*4), ebx edi
    ..If edi <> 0
        add edi D$SectionsMap | add ebx D$UserPeStart
        .If edi > D$SectionsMap
            If edi < D$EndOfSectionsMap
                While B$ebx <> 0 | stosb | inc ebx | End_While | stosb
            End_If
        .End_If
    ..End_If

  ; ExportAdressesTable:
    mov edi D$esi+(7*4)
    ..If edi <> 0
        add edi D$SectionsMap
        .If edi > D$SectionsMap
            If edi < D$EndOfSectionsMap
                mov ecx D$NumberOfDisExportedFunctions | shl ecx 2 | rep stosb
            End_If
        .End_If
    ..End_If

  ; ExportNamesTable:
    mov edi D$esi+(8*4)
    ..If edi <> 0
        add edi D$SectionsMap
        .If edi > D$SectionsMap
            If edi < D$EndOfSectionsMap
                mov ecx D$NumberOfDisExportNames | shl ecx 2 | rep stosb
            End_If
        .End_If
    ..End_If

  ; ExportOrdinals
    mov edi D$esi+(9*4)
    ..If edi <> 0
        add edi D$SectionsMap
        .If edi > D$SectionsMap
            If edi < D$EndOfSectionsMap
                mov ecx D$NumberOfDisExportNames | shl ecx 1 | rep stosb
            End_If
        .End_If
    ..End_If

  ; Function1Name, Function2Name, ...
    mov esi D$esi+(8*4), ecx D$NumberOfDisExportNames | On esi = 0, jmp L1>
    add esi D$UserPeStart
    ...If esi > D$UserPeStart
        ..If esi < D$UserPeEnd
L0:         mov edi D$esi, ebx edi
            add edi D$SectionsMap | add ebx D$UserPeStart
            .If edi > D$SectionsMap
                If edi < D$EndOfSectionsMap
                    While B$ebx <> 0 | stosb | inc ebx | End_While | stosb
                    add esi 4 | loop L0<
                End_If
            .End_If
        ..End_If
    ...End_If

L1: mov B$DisExportInside &TRUE

L9: call KillBlankBytes SectionTable, EXPORTFLAG
ret
____________________________________________________________________________________________

;;
  The 'KILLFLAG' is simply for _not_ analysing these Sections that are no use for
  the Disassembling Process.
;;

KillPeHeader:
    mov edi D$SectionsMap, ecx D$FirstSection, al KILLFLAG
    rep stosb
ret


Proc KillSection:
    Argument @OptionalHeaderTable

        GetPeHeader D@OptionalHeaderTable | mov edi D$eax | On edi = 0, ExitP

    ; Example: Is it a clean .reloc with nothing else inside?
        test edi 0FFF | jnz L2>

            mov esi edi | add esi D$UserPeStart
            mov ecx D$eax+4 | Align_On_Variable D$DisRvaSectionAlignment ecx
            mov edx esi | add edx ecx | add esi D$eax+4

            While esi < edx
                On B$esi <> 0, jmp L2>
                inc esi
            End_While

            ; Example: Seems to be a clean .reloc. Fill it all:
                add edi D$SectionsMap | mov al KILLFLAG | rep stosb | ExitP

L2:     mov ecx D$eax+4 | add edi D$SectionsMap | mov al KILLFLAG | rep stosb
EndP


Proc KillBlankBytes:
    Arguments @OptionalHeaderTable, @FLAG

        mov eax D@OptionalHeaderTable, esi D$eax, ecx D$eax+4
        Align_On_Variable D$DisRvaSectionAlignment ecx

        add esi D$UserPeStart | mov edx esi | add edx ecx
        mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

        While esi < edx
            On ebx >= D$EndOfSectionsMap, ExitP

            If B$ebx = 0
                On B$esi <> 0, ExitP
            End_If
            inc esi | inc ebx
        End_While

      ; OK. Only blank Bytes found, other than, for example, the normal Export Data:
        mov eax D@OptionalHeaderTable, esi D$eax, ecx D$eax+4
        Align_On_Variable D$DisRvaSectionAlignment ecx

        add esi D$UserPeStart | mov edx esi | add edx ecx
        mov ebx esi | sub ebx D$UserPeStart | add ebx D$SectionsMap

        While esi < edx
            On B$ebx = 0, mov B$ebx KILLFLAG
            inc esi | inc ebx
        End_While
EndP


KillSectionsExtensions:
    GetPeHeader SectionsHeaders | mov esi eax

    mov ecx D$DisNumberOfSections

L0: push ecx
        mov edi D$esi+SECTION_RVA | add edi D$SectionsMap
        mov ecx D$esi+SECTION_RVASIZE
        mov eax D$esi+SECTION_FILESIZE
        On ecx > eax, mov eax ecx
        Align_On_Variable D$DisFileSectionAlignment eax
        mov ecx eax
        Align_On_Variable D$DisRvaSectionAlignment ecx
        sub ecx eax | jz L2>

            add edi eax | mov al KILLFLAG | rep stosb
L2: pop ecx

    add esi SECTIONHEADERSIZE | loop L0<
ret
____________________________________________________________________________________________

[DisFailText: 'Failure of Disassembly', 0]
[SilentMap: ?]

DisFail:
    call DestroyDisProgressBar

    VirtualFree D$UserPeStart, D$CodeSource, D$TruthAsciiTable, D$RoutingMap, D$SectionsMap,
                D$SizesMap, D$ApiBuffer

    mov B$Disassembling &FALSE
    If B$SilentMap = &FALSE
        mov eax DisFailText | call MessageBox
    End_If
    mov B$SilentMap &FALSE

L0: mov ebx, esp | cmp ebx, D$OldStackPointer | jnb L1>
        pop ebx | jmp L0<

L1: jmp StartNewFile

____________________________________________________________________________________________
____________________________________________________________________________________________

[ProgressWindowStyle: 084 ] ;&WS_EX_TOOLWINDOW] ; 084

;;
   Attempt to having a ProgressBar that would be possible to close.
   
   Should be the routing center of all of the Assembler or Disassembler stuff.
   
   later...
;;

;InitDisProgressBar:
  ; Tag Dialog 25
    call 'USER32.DialogBoxParamA' D$hinstance, 25, &NULL, ProgressProc, &NULL
ret

Proc ProgressProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ..If D@Message = &WM_COMMAND
        If D@wParam = &IDCANCEL
            call 'USER32.EndDialog' D@Adressee, 0
        End_If

    ..If D@Message = &WM_CLOSE
        call 'USER32.EndDialog' D@Adressee, 0

    ..Else_If D@Message = &WM_INITDIALOG
        move D$hwndForBar D@Adressee
        call 'USER32.GetDlgItem' D@Adressee 5 | mov D$ProgressInst eax

        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon
      ; Set steping and Title:
        call 'User32.SendMessageA' D$ProgressInst, &PBM_SETRANGE, 0, (128 shl 16)
        call 'User32.SendMessageA' D$ProgressInst, &PBM_SETSTEP, 1, 0  ; 1/100
        call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, DisPasses

    ..Else
        popad | mov eax &FALSE | ExitP

    ..End_If

    popad | mov eax &TRUE
EndP
____________________________________________________________________________________________

InitDisProgressBar:
  ; Center the bar:
    call 'User32.GetSystemMetrics' &SM_CXSCREEN
      sub eax D$PBarWindowW | shr eax 1 | mov D$PBarWindowX eax
    call 'User32.GetSystemMetrics' &SM_CYSCREEN
      sub eax D$PBarWindowH | shr eax 1 | mov D$PBarWindowY eax

  ; WindowExStyle > 084:    80 > tool  4 > no parent notify
    call 'User32.CreateWindowExA' 084, ClassName, &NULL, &WS_OVERLAPPEDWINDOW,
                                  D$PBarWindowX, D$PBarWindowY, D$PBarWindowW, D$PBarWindowH,
                                  D$hwnd, 0, D$hInstance, 0
    mov D$hwndForBar eax

    call 'User32.ShowWindow' D$hwndForBar, &SW_SHOWNORMAL
    call 'User32.UpdateWindow' D$hwndForBar
_____________________________

    call 'User32.CreateWindowExA' 0, ProgressClassName, 0, 050000000,
                                  D$PWindowX, D$PWindowY, D$PWindowW, D$PWindowH,
                                  D$hwndForBar, 1, D$hInstance, 0
    mov D$ProgressInst eax

  ; Set steping and Title:
    call 'User32.SendMessageA' D$ProgressInst, &PBM_SETRANGE, 0, (128 shl 16)
    call 'User32.SendMessageA' D$ProgressInst, &PBM_SETSTEP, 1, 0  ; 1/100
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, DisPasses
ret

;InitDisProgressBar:
  ; Center the bar:
    call 'User32.GetSystemMetrics' &SM_CXSCREEN
      sub eax D$PBarWindowW | shr eax 1 | mov D$PBarWindowX eax
    call 'User32.GetSystemMetrics' &SM_CYSCREEN
      sub eax D$PBarWindowH | shr eax 1 | mov D$PBarWindowY eax

  ; WindowExStyle > 084:    80 > tool  4 > no parent notify
    call 'User32.CreateWindowExA' D$ProgressWindowStyle, ClassName, &NULL, &WS_OVERLAPPEDWINDOW,
                                  D$PBarWindowX, D$PBarWindowY, D$PBarWindowW, D$PBarWindowH,
                                  D$hwnd, 0, D$hInstance, 0
    mov D$hwndForBar eax

    call 'User32.ShowWindow' D$hwndForBar, &SW_SHOWNORMAL
    call 'User32.UpdateWindow' D$hwndForBar

    call 'User32.SetWindowLongA' D$hwndForBar, &GWL_WNDPROC, ProgressProc
    mov D$PreviousProgressProc eax
_____________________________

    call 'User32.CreateWindowExA' 0, ProgressClassName, 0, 050000000,
                                  D$PWindowX, D$PWindowY, D$PWindowW, D$PWindowH,
                                  D$hwndForBar, 1, D$hInstance, 0
    mov D$ProgressInst eax

  ; Set steping and Title:
    call 'User32.SendMessageA' D$ProgressInst, &PBM_SETRANGE, 0, (128 shl 16)
    call 'User32.SendMessageA' D$ProgressInst, &PBM_SETSTEP, 1, 0  ; 1/100
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, DisPasses
ret


[PreviousProgressProc: ?
 ProgressRet: ?
 ProgressAdressee: ?  ProgressMessage: ?  ProgressWparam: ?  ProgressLparam: ?]

;ProgressProc:
    pop D$ProgressRet
    pop D$ProgressAdressee, D$ProgressMessage, D$ProgressWparam, D$ProgressLparam
    push D$ProgressRet

    .If D$ProgressMessage = &WM_COMMAND
        If D$mEditWparam = &IDCANCEL
           ; mov eax &FALSE | ret
        End_If
    .End_If

L9: call 'User32.CallWindowProcA' D$PreviousProgressProc D$ProgressAdressee,
                                  D$ProgressMessage, D$ProgressWparam, D$ProgressLparam
    ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[SubEdi6 | On B$WithCommentedHexa = &FALSE, call SubEdi6IfNoComment]

;;
 Try a minimal organisation of Jcc Instructions writing, under the form of:

 > cmp eax 2 | je K2<

 In case "Cmp eax 2" is followed by comments (either "Code0407050" or Commented
 Hexa Code), we abort the attempt:
;;

SubEdi6IfNoComment:
    push ebx
        mov ebx edi | sub ebx 7
        While B$ebx >= ' '
            dec ebx | On B$ebx = ';', jmp L9>
        End_While
        sub edi 6
L9: pop ebx
ret
____________________________________________________________________________________________

; The main Passes Analyzes of Disassembly.
____________________________________________________________________________________________

[DisEndOfChunk: ?]
[DisCodeDisplacement: ?    DisCodeBytes: ?    DisBarStep: ?    NextDisBarPos: ?
 NewAccessedLocations: ?]

[TestLastLocation: ?  TestLastLineLocation: ?]

DisassembleForCodeRouting:
    mov edi D$CodeSource, esi D$UserPeStart
    add esi D$FirstSection
    mov D$NewAccessedLocations &FALSE, D$LastCodeRef 0

    NextDisLine

L0: mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    mov B$CALLInstruction &FALSE, B$LeaInstruction &FALSE

  ; Parse onlyCODEFLAGed (in SectionsMap) and ACCESSED (in RoutingMap) Locations:
L1: mov eax esi | sub eax D$UserPestart | add eax D$SectionsMap
    Test B$eax CODEFLAG | jnz L2>
        inc esi | On esi = D$UserPeEnd, jmp L9>>
            jmp L1<

L2: sub eax D$SectionsMap | add eax D$RoutingMap
    test B$eax ACCESSED | jnz L3>
        inc esi | On esi = D$UserPeEnd, jmp L9>>
            jmp L1<

      ; Call for the Disassembly Routines:
L3:     or B$eax INSTRUCTION
L3:     movzx eax B$esi

        mov D$TestLastLineLocation esi, D$TestLastLocation esi

        inc esi | call D$DisOp1+eax*4

      ; Loop immidiately in case of simple Prefix:
        While B$DisFlag = DISDONE
            mov D$TestLastLocation esi
            movzx eax B$esi | inc esi | call D$DisOp1+eax*4
        End_While

        mov eax D$TestLastLineLocation | sub eax D$UserPeStart | add eax D$DisImageBase

      ; Clear any LABEL from inside the valid parsed Code:
        mov eax D$TestLastLineLocation, edx esi | inc eax
        sub eax D$UserPeStart | add eax D$RoutingMap
        sub edx D$UserPeStart | add edx D$RoutingMap
L3:     and B$eax (not LABEL)
        On B$LastDisassemblyRoutingPass = &TRUE,
            and B$eax (not LABEL+INSTRUCTION+EVOCATED+NODE)
        inc eax | cmp eax edx | jb L3<

      ; If a CALL or a JMP was Disassembed, the decoding Routines return this Flag set on.
      ; We mark the RoutingMap's according Byte as a new Entry Point:
        .If B$CALLInstruction = &TRUE
            mov B$CALLInstruction &FALSE

            If B$EscapePrefix <> &TRUE
                mov eax D$TestLastLocation
                On B$eax = 0E8, call IsItNoReturnCall  ; OpE8
            End_If

            mov eax D$LastCodeRef
            sub eax D$DisImageBase | add eax D$SectionsMap
            On eax < D$SectionsMap, jmp L4>>
            On eax >= D$EndOfSectionsMap, jmp L4>>
            test B$eax VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+DATAFLAG | jnz L4>>
            ;test B$eax VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG | jnz L4>>
                mov B$eax CODEFLAG
                sub eax D$SectionsMap | add eax D$RoutingMap
                test B$eax ACCESSED | jnz L3>
                    mov B$NewAccessedLocations &TRUE
L3:             or B$eax NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
        .End_If

      ; CHUNKEND Flag marks the first Byte *after* a RET or a JMP:
L4:     mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
        If B$DisEndOfChunk = &TRUE
            or B$eax CHUNKEND | dec eax | mov B$DisEndOfChunk &FALSE
        End_If

      ; Now Flaged *backward* 'ACCESSED' the Bytes of the new disassembled Instruction,
      ; including the very first Byte of the new coming Instruction, if this one is
      ; not a CHUNKEND (we have 'dec eax' up there in such cases):
L4:     test B$eax ACCESSED | jnz L4>
            mov D$NewAccessedLocations &TRUE

L4:     mov ecx D$TestLastLineLocation | sub ecx D$UserPeStart | add ecx D$RoutingMap

L4:     or B$eax ACCESSED
        mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
        test B$ebx VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+DATAFLAG | jnz L5>
       ; test B$eax VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG | jnz L5>
        mov B$ebx CODEFLAG
L5:     dec eax

        On eax >= ecx, jmp L4<

L6:   ; Adjust the ProgressBar is wanted:
        If esi > D$NextDisBarPos
            mov eax esi | add eax D$DisBarStep | mov D$NextDisBarPos eax
            call BarProgress
        End_If

    On esi < D$UserPeEnd, jmp L0<<

  ; Loop it all until no more new accessed Code Chunks are found:
L9: mov D$NextDisBarPos 0
   ; map
    cmp B$NewAccessedLocations &TRUE | je DisassembleForCodeRouting
L9:ret

____________________________________________________________________________________________

IsItNoReturnCall:
  ; Escape, if the pointed Byte is already flaged valid Code, by some Code Reference:
    mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
    test B$eax LABEL | jz L1>
        sub eax D$RoutingMap | add eax D$SectionsMap
        On B$eax = CODEFLAG, ret
;sub eax D$SectionsMap | add eax D$DisImageBase | On eax = 040188B, mov B$TestNow 1
L1: push esi, edi
        push D$DisEndOfChunk, D$LastCodeRef
            lea eax D$esi+50
            mov B$StopAtCall &TRUE
                call IsItCode esi, eax, 0
            mov B$StopAtCall &FALSE
        pop D$LastCodeRef, D$DisEndOfChunk
    pop edi, esi

;If B$TestNow = 1
;    mov eax D$LastUnAccessed | sub eax D$UserPeStart | add eax D$DisImageBase
;    hexprint eax
;End_If

    If eax = &FALSE
        mov eax esi | sub eax D$UserPeStart | add eax D$SectionsMap | mov B$eax DATAFLAG
        sub eax D$SectionsMap | add eax D$RoutingMap | or B$eax EVOCATED
        mov D$DisEndOfChunk &TRUE | ;mov B$esi-5 0E9
        mov eax &FALSE

    Else
        mov D$DisEndOfChunk &FALSE

    End_If
ret
____________________________________________________________________________________________

[FollowedByCode: ?  No0CC: ?]

CodeFromPointers:
    call InitDisTablesCopies

    mov B$AttemptSuccess &FALSE, B$No0CC &TRUE, B$StopAtEndOfChunk &TRUE

    mov esi D$SizesMap | add esi D$FirstSection
    sub edx 4

    .While esi < D$EndOfSizesMap
        test B$esi POINTER | jz L8>>
          ; Get the Pointer:
            mov ebx esi | sub ebx D$SizesMap | add ebx D$UserPeStart
            mov eax D$ebx |; On eax = 047E3D4, int3
            sub eax D$DisImageBase | add eax D$UserPeStart

            ...If eax < D$UserPeStart
                xor B$esi POINTER

            ...Else_If eax > D$UserPeEnd
                xor B$esi POINTER

            ...Else
              ; If the Bytes flow begins with zero, this is probably not Code:
                On B$eax = 0, jmp L8>>

              ; Kill POINTERs to the special Sections:
                sub eax D$UserPeStart | add eax D$SectionsMap
                Test_If B$eax IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+KILLFLAG
                    xor B$esi POINTER
                    jmp L8>>
                Test_End

              ; The Section must not be flaged, yet:
                ..If B$eax = 0
                  ; Compute the size of the zeored Chunk:
                    mov ecx 0 | While B$eax+ecx = 0 | inc ecx | End_While
                    If B$eax+ecx = CODEFLAG
                        mov B$FollowedByCode &TRUE
                    Else
                        mov B$FollowedByCode &FALSE
                    End_If
                    push esi, ebx
                        lea ebx D$eax+ecx
                        sub eax D$SectionsMap | add eax D$UserPeStart
                        sub ebx D$SectionsMap | add ebx D$UserPeStart
                        call IsItCode eax, ebx, 1
                    pop ebx, esi

                    .If eax = &TRUE
                        If B$FollowedByCode = &FALSE
                            On B$DisEndOfChunkEncounted = &FALSE, jmp L7>>
                        End_If

                      ; Possible Code Candidate found. Give it a try:
L4:                     push esi, ebx
                            mov esi D$ebx ;| On eax = 047E3D4, int3
                            sub esi D$DisImageBase | add esi D$SectionsMap
                            If B$esi <> 0
                                pop edx, ebx, esi | jmp L8>>
                            End_If
                          ; Save copies the Tables states for cases of failure:
                            call SetDisTablesCopies

                            mov B$esi CODEFLAG
                            sub esi D$SectionsMap | add esi D$RoutingMap
                            or B$esi NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL

                            sub esi D$RoutingMap | add esi D$SectionsMap
                            call DisassemblingAttempt
                        pop ebx, esi

                      ; Restore the previous Tables versions on failure cases:
                        If B$DisFailure = &TRUE
                      ; Arase the effect of the previous 'SetDisTablesCopies':
                            sub esi D$SizesMap | add esi D$RoutingMap
                            call ExchangeDisTables
                            sub esi D$RoutingMap | add esi D$SizesMap

                            jmp L7>

                        Else
                            mov B$AttemptSuccess &TRUE

                        End_If

                    .Else
                       ; Kills the rebuilt of 3DFUN.exe:

L7:                    ; mov eax D$ebx | sub eax D$DisImageBase | add eax D$SectionsMap
                       ; mov B$eax DATAFLAG

                    .End_If
                ..End_If
            ...End_If
L8:     inc esi
    .End_While

    call ReleaseDisTablesCopies
    mov B$No0CC &FALSE, B$StopAtEndOfChunk &FALSE
ret
____________________________________________________________________________________________

[BiggerZeroedSectionsChunk: ?]

GetBiggerSectionsBlank:
    mov D$BiggerZeroedSectionsChunk 1

    mov esi D$SectionsMap, edx D$EndOfSectionsMap | add esi D$FirstSection
    mov ebx esi | sub ebx D$SectionsMap | add ebx D$UserPeStart

    .While esi < edx
        .If B$esi = 0
            mov ecx 0, eax &FALSE

            While B$esi = 0
                inc esi | inc ecx | On esi >= edx, jmp L2>
                On B$ebx <> 0, mov eax &TRUE
            End_While

            If eax = &TRUE
              ; (Don't consider zeroed Chunks, in the PE):
                mov eax &FALSE
L2:             On ecx > D$BiggerZeroedSectionsChunk, mov D$BiggerZeroedSectionsChunk ecx
            End_If
        .End_If

        inc esi, ebx

    .End_While
ret

____________________________________________________________________________________________
;;
  Everything EVOCATED but not yet Recognized may be either Code or Data.
  
  We first call for 'IsItCode' that says if, 'physicaly', a Chunk could be Code or not.
  
  If yes, run a Try&See Disassembly ('DisassemblingAttempt') on duplicated Dis Tables.
  
  If this attempt is _not_ valid Code, it will __much__ probably break something in the
  already found out Code and/or Data. Nothing broken >>> Run 'DisassembleForCodeRouting'
  for flaging Code.
;;

Proc CodeAttempt:

EndP

[TestNow: ?   LastIdentifiedCandidate: ?]

[Switch | sub #1 D$#2 | add #1 D$#2]

Proc TryToDisassembleEvocated:
    Argument @Required
  ; This 'Required' is the Number of Instructions. Not the number of Bytes.
        call InitDisTablesCopies

        mov B$AttemptSuccess &FALSE

        mov esi D$RoutingMap | add esi D$FirstSection

L0:     .While esi < D$EndOfRoutingMap
L1:         test B$esi EVOCATED+LABEL | jz L5>>

            mov ebx esi | sub ebx D$RoutingMap | add ebx D$SectionsMap
            cmp B$ebx 0 | jne L5>>
            sub ebx D$SectionsMap | add ebx D$SizesMap
            cmp B$ebx 0 | jne L5>>

            push esi, ebx
                sub ebx D$SizesMap | add ebx D$UserPeStart
                mov ecx ebx | add ecx 100
                call IsItCode ebx, D$UserPeEnd, D@Required
            pop ebx, esi

            move D$LastIdentifiedCandidate D$LastUnAccessed

            ...If eax = &TRUE
              ; Possible Code Candidate found. Give it a try:
L4:             push esi
                  ; The copies are for preserving the Tables states in cases of failure:
                    call SetDisTablesCopies
                    or B$esi NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                    sub esi D$RoutingMap | add esi D$SectionsMap
                    mov B$esi CODEFLAG

                    call DisassemblingAttempt
                pop esi

              ; Restore the previous Tables versions on failure cases:
                .If B$DisFailure = &TRUE
                  ; Arase the effect of the previous 'SetDisTablesCopies':
                    call ExchangeDisTables

                .Else
                    mov B$AttemptSuccess &TRUE
                    ;jmp L9> ; (Bound to "pop ecx | jmp L0<", in the main calling loop)

                .End_If

            ...End_If

L5:     inc esi

    .End_While

L9:  call ReleaseDisTablesCopies
EndP


[SectionsMapCopy: ?    RoutingMapCopy: ?    SizesMapCopy: ?
 EndOfSectionsMapCopy: ?    EndOfRoutingMapCopy: ?    EndOfSizesMapCopy: ?]
[DisTableLength: ?]

InitDisTablesCopies:
    mov eax D$EndOfSectionsMap | sub eax D$SectionsMap
  ; Align_On_Variable D$DisRvaSectionAlignment eax |
    mov D$DisTableLength eax

    VirtualAlloc SectionsMapCopy D$DisTableLength
    VirtualAlloc RoutingMapCopy D$DisTableLength
    VirtualAlloc SizesMapCopy D$DisTableLength

    mov ecx D$DisTableLength
    mov eax D$SectionsMapCopy | add eax ecx | mov D$EndOfSectionsMapCopy eax
    mov eax D$RoutingMapCopy | add eax ecx | mov D$EndOfRoutingMapCopy eax
    mov eax D$SizesMapCopy | add eax ecx | mov D$EndOfSizesMapCopy eax
ret


SetDisTablesCopies:
    push esi, edi, ecx
        mov esi D$SectionsMap, edi D$SectionsMapCopy, ecx D$DisTableLength
        shr ecx 2 | rep movsd
        mov esi D$RoutingMap, edi D$RoutingMapCopy, ecx D$DisTableLength
        shr ecx 2 | rep movsd
        mov esi D$SizesMap, edi D$SizesMapCopy, ecx D$DisTableLength
        shr ecx 2 | rep movsd
    pop ecx, edi, esi
ret

ExchangeDisTables:
    sub esi D$RoutingMap
    Exchange D$SectionsMap D$SectionsMapCopy
    Exchange D$RoutingMap D$RoutingMapCopy
    Exchange D$SizesMap D$SizesMapCopy
    Exchange D$EndOfSectionsMap D$EndOfSectionsMapCopy
    Exchange D$EndOfRoutingMap D$EndOfRoutingMapCopy
    Exchange D$EndOfSizesMap D$EndOfSizesMapCopy
    add esi D$RoutingMap
ret

ReleaseDisTablesCopies:
    VirtualFree D$SectionsMapCopy, D$RoutingMapCopy, D$SizesMapCopy
ret


; Code Recognitions over > Everything not yet Flaged is Data:

FillDataSection:
    mov esi D$SectionsMap | add esi D$FirstSection
    mov edx D$EndOfSectionsMap

;mov eax esi | sub eax D$SectionsMap | add eax D$DisImageBase
;    hexprint eax

    While esi < edx

        ;mov eax esi | sub eax D$SectionsMap | add eax D$UserPeStart | sub eax D$FirstSection
        ;On eax = 04081E4, hexprint DATAFLAG


        .If B$esi = 0
            mov B$esi DATAFLAG

          ; Force a dummy EVOCATED, at start of any Data Chunk, for the output:
            If B$esi-1 <> DATAFLAG
                mov ebx esi | sub ebx D$SectionsMap | add ebx D$RoutingMap
                or B$ebx EVOCATED+LABEL
            End_IF

        .End_If

        inc esi

    End_While

   ; mov eax esi | sub eax D$SectionsMap | add eax D$DisImageBase
   ; hexprint eax
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
    Code Recognition of a [Start --- End] un-accessed Code Chunk. Positive recognition
    occur when:
    
    * begins with a 'PUSH_EBP' Routing Flaged Instruction ('@PUSH_EBP')
    * end with a CHUNKEND ('@CHUNKEND')
    * not produce any 'DB' in Disassembly ('@DB')
    * have a positive Likely/Unlikely Code weight Value ('LikelyCode', 'UnLikelyCode')
    * have less than 20% of zeroed Bytes ('@Zeros')
    * be big enough for any bet ('@Size')
    
    Terrific problem of 'tuning', with all of this. So, the recognition tends to say
    it is really Code when it is very likely Code (better failing at flaging valid
    Code than flaging wrong Code as valid...).
;;

[LikelyCode: ?  UnLikelyCode: ? SimpleScan: ?    LastUnAccessed: ?
 StopAtCall: ?  StopAtEndOfChunk: ?  DisEndOfChunkEncounted: ? UnlikelyCodeFoundAt: ?]

Proc IsItCode:
    Arguments @Start, @End, @Required
    Local @OpNumber

    mov B$SimpleScan &TRUE ; Prevents 'WriteDisRelative' from modifying Routing Flags.
    mov B$DisEndOfChunkEncounted &FALSE

    mov D$LikelyCode 0, D$UnLikelyCode 0, B$DisEndOfChunk &FALSE, D@OpNumber 0

    mov eax D@Start | sub eax D$UserPeStart | add eax D$RoutingMap

    mov ecx D@End | On ecx > D$UserPeEnd, move D@End D$UserPeEnd

    mov esi D@Start, edi D$CodeSource

    .If B$esi = 0
        mov ecx esi
        add ecx 2
        On ecx >= D$UserPeEnd, jmp L2>
        If W$esi+1 = 0
L2:            mov eax &FALSE | jmp L9>>
        End_If
    .End_If


    .While esi < D@End
        mov B$DisFlag 0

        mov D$SegmentOverride 0, B$AddressSizeOverride 0, B$OperandSizeOverride 0
        mov W$DisSizeMarker 'D$', B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
        mov D$LastCodeRef 0, B$LeaInstruction &FALSE

        mov D$LastUnAccessed esi
;If B$TestNow = 1
;    mov eax esi | sub eax D$UserPeStart | add eax D$DisImageBase
;    On eax = 0401899, int3
;End_If

L0:     movzx eax B$esi | inc esi | call D$DisOp1+eax*4 | inc D@OpNumber

        ;mov ecx D@Required | On D@OpNumber >= ecx, jmp L2>

        .If B$StopAtCall = &TRUE
            If B$CALLInstruction = &TRUE
                mov eax D$LastUnAccessed | On B$eax = 0E8, mov B$DisEndOfChunk &TRUE
            End_If
        .End_If

        If D$UnLikelyCode > 0
L1:         mov eax D$LastUnAccessed | sub eax D$UserPeStart | add eax D$DisImageBase
            mov D$UnlikelyCodeFoundAt eax
            mov eax &FALSE | jmp L9>>
        Else_If B$DisFlag = DISFAILED
            mov eax &FALSE | jmp L9>
        Else_If B$DisEndOfChunk = &TRUE
            mov B$DisEndOfChunkEncounted &TRUE
            On B$StopAtEndOfChunk = &TRUE, jmp L2>
            mov ecx D@Required | On D@OpNumber >= ecx, jmp L2>
        End_If

        If B$DisFlag = DISDONE
            jmp L0<<
        Else_If B$LockPrefix = &TRUE
          ; DISDONE+DISLINEOVER, but LOCK not taken by a valid LOCKable Instruction:
            mov B$LockPrefix &FALSE | jmp L1<
        End_If

    .End_While

L2: mov eax &TRUE

L9: mov B$SimpleScan &FALSE

    On eax = &TRUE, mov D$LastUnAccessed esi
EndP
____________________________________________________________________________________________

[StartOfDisLine: ?    AlignedComment: ?    NonAccessedByteWritten: ?]

[DisFailure: ?  DisFailureType: ?   EncountedFlag: ?  AttemptSuccess: ?]

DisassemblingAttempt:
    mov B$DisFailure &FALSE, D$DisFailureType 0, B$NewAccessedLocations &FALSE
    mov B$LockPrefix &FALSE

  ; esi > 'SectionsMap' new CODEFLAG attempt when called. So:
    sub esi D$SectionsMap | add esi D$UserPeStart

L0: mov edi D$CodeSource, D$LastCodeRef 0
    mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    mov B$CALLInstruction &FALSE, B$DisEndOfChunk &FALSE

  ; Parse onlyCODEFLAGed (in SectionsMap) and ACCESSED (in RoutingMap) Locations:
L1: mov eax esi | sub eax D$UserPestart | add eax D$SectionsMap

    Test B$eax CODEFLAG | jnz L2>
        If B$eax <> 0
            ;On B$TestNow = 1, hexprint 1
            mov D$DisFailureType 1
            mov B$DisFailure &TRUE | ret
        End_If
        inc esi | On esi = D$UserPeEnd, jmp L9>>
            jmp L1<

L2:
  ; Is it still a new Location when comparing to the original Table? Yes > Quit:
    sub eax D$SectionsMap | add eax D$SectionsMapCopy | On B$eax = CODEFLAG, ret

    sub eax D$SectionsMapCopy | add eax D$RoutingMap
    test B$eax ACCESSED | jnz L3>
        inc esi | On esi = D$UserPeEnd, jmp L9>>
            jmp L1<

      ; Call for the Disassembly Routines:
L3:     or B$eax INSTRUCTION

        movzx eax B$esi | mov D$TestLastLocation esi | inc esi

        call D$DisOp1+eax*4

        While B$DisFlag = DISDONE
            movzx eax B$esi | inc esi | call D$DisOp1+eax*4
        End_While

        If B$LockPrefix = &TRUE
            mov B$LockPrefix &FALSE | add B$UnlikelyCode 50
        End_If

      ; The instruction must not cover a valid Label:
        mov eax D$TestLastLocation | inc eax
        mov ebx eax | sub ebx D$UserPeStart | add ebx D$RoutingMap
        While eax < esi
            test B$ebx INSTRUCTION+NODE+LABEL | jz L3>
                mov D$DisFailureType 2 | mov B$DisFailure &TRUE | ret
L3:         inc eax | inc ebx
        End_While

      ; If a CALL or a JMP was Disassembed, the decoding Routines return this Flag set on.
      ; We mark the RoutingMap's according Byte as a new Entry Point:
        ...If B$CALLInstruction = &TRUE
            mov B$CALLInstruction &FALSE

            If B$EscapePrefix <> &TRUE
                mov eax D$TestLastLocation
                On B$eax = 0E8, call IsItNoReturnCall ; OpE8
            End_If

            mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SectionsMap
            On eax < D$SectionsMap, jmp L4>>
            On eax > D$EndOfSectionsMap, jmp L4>>
            test B$eax VIRTUALFLAG+IMPORTFLAG+RESOURCESFLAG+EXPORTFLAG+DATAFLAG | jnz L4>>
            jmp L5>

L4:         mov D$DisFailureType 3 | mov B$DisFailure &TRUE | ret

L5:         If B$eax = CODEFLAG
                sub eax D$SectionsMap | add eax D$RoutingMap
                cmp B$eax 0 | je L5>
                test B$eax INSTRUCTION+CHUNKEND | jnz L3>>
                    mov D$DisFailureType 4 | mov B$DisFailure &TRUE | ret

L5:             sub eax D$RoutingMap | add eax D$SectionsMap
; This is not out of logic to re-scan the Code: We are in Try&See actions.
            End_If

            push esi, eax
                sub eax D$SectionsMap | add eax D$UserPeStart
                mov ebx eax | add ebx 100
                push D$DisEndOfChunk
                    call IsItCode eax, ebx, 1
                pop D$DisEndOfChunk
                If eax = &FALSE
                    pop eax, esi
                    mov D$DisFailureType 5 | mov B$DisFailure &TRUE | ret
                End_If
            pop eax, esi

            mov B$eax CODEFLAG
            sub eax D$SectionsMap | add eax D$RoutingMap | test B$eax ACCESSED | jnz L3>
                mov B$NewAccessedLocations &TRUE
L3:         or B$eax NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
        ...End_If

      ; CHUNKEND Flag marks the first Byte *after* a RET or a JMP:
L4:     mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
        If B$DisEndOfChunk = &TRUE
            or B$eax CHUNKEND | dec eax | mov B$DisEndOfChunk &FALSE | ret ; <<<< new
        End_If

        mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
        .If B$ebx = 0
            ; OK
        .Else_If B$ebx <> CODEFLAG
            mov D$DisFailureType 6 | mov B$DisFailure &TRUE | ret
        .End_If

      ; Now Flaged *backward* 'ACCESSED' the Bytes of the new disassembled Instruction,
      ; including the very first Byte of the new coming Instruction, if this one is
      ; not a CHUNKEND (we have 'dec eax' up there in such cases):
L4:     test B$eax ACCESSED | jnz L6>>
            mov ecx D$TestLastLocation | sub ecx D$UserPeStart | add ecx D$RoutingMap

L4:         or B$eax ACCESSED | mov D$NewAccessedLocations &TRUE
            mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
            .If B$ebx = 0
                mov B$ebx CODEFLAG
            .Else_If B$ebx <> CODEFLAG
                mov al B$ebx, B$EncountedFlag al
                mov D$DisFailureType 7 | mov B$DisFailure &TRUE | ret
            .End_If
            sub ebx D$SectionsMap | add ebx D$SizesMap
            If B$ebx <> 0
                mov D$DisFailureType 8 | mov B$DisFailure &TRUE | ret
            End_If

L5:         dec eax

            If eax >= ecx
                test B$eax ACCESSED | jz L4<<
            End_If

L6: If B$DisFlag = DISLINEOVER+DISDONE
        On esi < D$UserPeEnd, jmp L0<<
    Else_If B$DisFlag = DISFAILED
        mov D$DisFailureType 9 | mov B$DisFailure &TRUE | ret
    End_If

  ; Adjust the ProgressBar is wanted:
    If esi > D$NextDisBarPos
        mov eax esi | add eax D$DisBarStep | mov D$NextDisBarPos eax
        call BarProgress
    End_If

  ; Cases of B$DisFlag = DISDONE only (Prefixes, ...):
    On esi < D$UserPeEnd, jmp L1<<

  ; Loop it all until no more new accessed Code Chunks are found:
L9: mov D$NextDisBarPos 0
    If B$NewAccessedLocations = &TRUE
        mov edi D$CodeSource, esi D$UserPeStart
        add esi D$FirstSection
        mov D$NewAccessedLocations &FALSE, D$LastCodeRef 0 | jmp L0<<
    End_If
ret
____________________________________________________________________________________________

[ZeroedEnd: ?    NextDisTITLE: ?   StartOfDataChunks: ?]

DisassembleAndWrite:
    mov B$NonAccessedByteWritten &FALSE, B$LabelWritten &FALSE, D$ZeroedEnd 0
    mov esi D$UserPeStart, D$TestLastLineLocation 0

    NextDisLine

L0: mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    mov B$CALLInstruction &FALSE, B$LeaInstruction &FALSE, B$LabelWritten &FALSE
    mov D$LastCodeRef 0

    mov D$Prefixes 0

    On edi > D$NextDisTITLE, call WriteDisTITLE

  ; Parse only CODEFLAGed Chunks:
L1: mov eax esi | sub eax D$UserPestart | add eax D$SectionsMap

    If B$eax = DATAFLAG
        call WriteOneDataChunksAsFound eax | On esi >= D$UserPeEnd, jmp L9>>
        jmp L1<

    Else_If B$eax = VIRTUALFLAG
        call WriteOneDataChunksAsFound eax | On esi >= D$UserPeEnd, jmp L9>>
        jmp L1<

    Else
        mov D$StartOfDataChunks 0

    End_If

    Test B$eax CODEFLAG | jnz L2>
        inc esi | On esi = D$UserPeEnd, jmp L9>>
            jmp L1<<

L2: mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap

    Test B$ebx EVOCATED | jz L3>
        push ebx
            call WriteDisCodeLabel
        pop ebx
        mov B$edi CR, B$edi+1 LF, D$edi+2 '    ' | add edi 6

      ; Just to force non ACCESSED DB here:
        mov B$NonAccessedByteWritten &FALSE, B$LabelWritten &TRUE

L3: test B$ebx ACCESSED | jnz L3>
        .If B$NonAccessedByteWritten = &FALSE
            If B$LabelWritten = &FALSE
                push ebx
                    call WriteDisCodeLabel | NextDisLine
                pop ebx
            End_If
            push ebx
                call WriteDBandData
            pop ebx
            cmp B$ItWasAlignment &TRUE | je L0<<
            cmp B$NonAccessedByteWritten &FALSE | je L4>
        .End_If
        mov B$edi-2 ';' | jmp L4>

L3: mov B$NonAccessedByteWritten &FALSE

L4: mov D$StartOfDisLine esi, D$AlignedComment edi | add D$AlignedComment 32

    movzx eax B$esi | inc esi
    push esi
        call D$DisOp1+eax*4
            While B$DisFlag = DISDONE
                movzx eax B$esi | inc esi | call D$DisOp1+eax*4
            End_While
        End_If
    pop ebx

  ; Was a real NODE eaten by the Decoding? If yes, go back and re-parse:
  ; Should be no more used since the implementation of upper 'DisAlignedToData':
    While ebx < esi
        mov eax ebx | sub eax D$UserPeStart | add eax D$RoutingMap
        Test B$eax NODE | jz L4>
            call BadDecode | mov esi ebx
            push esi
                call WriteDisCodeLabel | NextDisLine
            pop esi
            jmp L0<<
L4:     inc ebx
    End_While

  ; Adjust the ProgressBar is wanted:
    If esi > D$NextDisBarPos
        mov eax esi | add eax D$DisBarStep | mov D$NextDisBarPos eax
        call BarProgress
    End_If

    If B$DisFlag = DISLINEOVER+DISDONE
        On B$WithCommentedHexa = &TRUE, call CommentHexa
        NextDisLine

    Else_If B$DisFlag = DISDONE
        On esi < D$UserPeEnd, jmp L1<<

    Else_If B$DisFlag = DISFAILED
        push esi, eax
            mov esi D$StartOfDisLine
            call WriteDisCodeLabel
        pop eax, esi
        mov D$edi ' DB ' | add edi 4
        call LoadedOpToHexa | stosw
            NextDisLine
    End_If

    On esi < D$UserPeEnd, jmp L0<<

L9: If D$ZeroedEnd <> 0
        mov edi D$ZeroedEnd, D$edi 0
    End_If
ret
____________________________________________________________________________________________

[RegisterClassStructure: 'Data0403060' 0, 0, 0, 0
 LastRegisterClass: 0   ClassEx: 0]

; Searching downward for "call 'USER32.RegisterClassA'" (or ClassExA):

SearchRegisterClass:
    mov esi D$LastRegisterClass, D$ClassEx 0
  ; 'ClassEx' is for Searching by Structure (Ex >>> one more dWord first for Size).
    While esi < D$SourceEnd
        ...If D$esi = 'Regi'
            ..If D$esi+4 = 'ster'
                .If D$esi-13 = 'call'
                    If D$esi+8 = 'Clas'
                        On W$esi+13 = 'Ex', mov D$ClassEx 4
                        mov D$LastRegisterClass esi | add D$LastRegisterClass 12 | jmp L9>
                    End_If
                .End_If
            ..End_If
        ...End_If
        inc esi
    End_While

    mov D$LastRegisterClass 0

L9: ret
____________________________________________________________________________________________

;;
  The preceeding Instruction may be "push Dataxxx". If found by the ".While", ok. If 
  not found (End of Line encounted), we give a try to previous Instructions in a scope
  of 1000 Bytes (Source Bytes...), that could eventually be some "mov eax Dataxxxx".
;;

[CodeToMainWindowProc: ?]

SearchUpperCodeToMainWindowProc: ret
  ; Upper Instruction was not "push Dataxxx".
  ; Example: push esp >>> try to find some upper 'Code0405060' (very rude...):

    mov esi D$LastRegisterClass

    While B$esi <> CR | dec esi | End_While

    mov ecx 0,  edx 1000 | On B$WithCommentedHexa = &TRUE, mov edx 3000

L0: .While esi > D$CodeSource
        ..If D$esi = 'Code'
          ; Don't confuse with a call, a jump or a Comment:
            On B$esi-2 = ';', jmp L3>
            On D$esi-5 = 'call', jmp L3>
            On D$esi-4 = 'jmp ', jmp L9>

          ; Don't confuse with a Code Label Declaration:
            mov eax esi
            While B$esi <> ':'
L1:             inc esi
                .If B$esi <= ' '
                  ; Is the mainWindowProc candidate first Instruction "push ebp"?
                    call IsThisPushEbp
                    If B$ThisIsEbp = &TRUE
                        mov D$CodeToMainWindowProc eax | jmp L9>>
                    End_If
                .End_If
            End_While

L2:         mov esi eax
        ..End_If

L3:     dec esi | inc ecx | cmp ecx edx | ja L9>
    .End_While

L9: ret


[ThisIsEbp: ?]

IsThisPushEbp:
    mov B$ThisIsEbp &FALSE
    pushad
      ; eax points to the 'Codexxxx' (// esi is on the Byte after).
        add eax 4 | mov edi D$EndOfDisData

        .While edi < D$SourceEnd
            ...If D$edi = 'Code'
                add edi 4 | mov esi eax, ecx 10
                repe cmpsb
                cmp B$edi-1 ':' | jne L2>
                cmp B$esi-1 ' ' | ja L2>
                    While B$edi <> LF | inc edi | End_While
                    While B$edi <= ' ' | inc edi | End_While
                    .If D$edi = 'push'
                        If D$edi+4 = ' ebp'
                            mov B$ThisIsEbp &TRUE | jmp L9>
                        End_If
                    .End_If
                    jmp L9>>
                ..End_If

            ...End_If

L2:         inc edi

        .End_While

L9: popad
ret


SearchMainWindowProc:
    mov esi D$EndOfDisData, ebx 4, D$LastRegisterClass esi, D$CodeToMainWindowProc 0,
            D$MainWindowProcIsThere 0

L0: call SearchRegisterClass

    ...If D$LastRegisterClass <> 0
      ; A "call 'USER32.RegisterClass...'" has been found.
      ; Is MainWindowProc written directely in the RegisterClass Structure?
        call GetMainWindowProcFromStructure

      ; If not, search for any possible 'Code0405060' in the close upper Lines:
        If D$CodeToMainWindowProc = 0
            call SearchUpperCodeToMainWindowProc
        End_If

        If D$CodeToMainWindowProc <> 0
            call WriteMainWindowProc
        Else
            jmp L0<
        End_If

    ...End_If
ret
____________________________________________________________________________________________

;;
  Menus: Usually, 0111 (&WM_COMMAND) in D$ebp+12 // ID in W$ebp+16. Failure Message if not.
;;

[WM_COMMAND_Found: ?]

MenuIdsSubstitutions:
    mov B$WM_COMMAND_Found &FALSE

    On D$MainWindowProcIsThere = 0, ret

  ; 'PrepareDisMenuIDs' Builds the Table of Menu Ids Declaration ready to insert:
    call PrepareDisMenuIDs | call WriteDisMenuIDs

    mov esi D$MainWindowProcIsThere, edx D$SourceEnd

    .While esi < edx
          ; Search for a &WM_COMMAND:
;;
; Example:
  mov eax D$ebp+0C
    cmp eax 01
    
Code040112B: N9: | jne K0>  ; Code04011A4
;;
        ...If D$esi = 'p+0C'

            If D$esi+5 = '0111'
                add esi 5 | jmp L1>
            End_If
L0:         While D$esi <> 'cmp '
                inc esi | On esi >= edx, jmp L9>>
            End_While
            add esi 5
            While B$esi > ' '
                inc esi | On esi >= edx, jmp L9>>
            End_While
            inc esi

            ..If D$esi = '0111'
L1:             .If B$esi-1 <= ' '
                    If B$esi+4 <= ' '
                    ; Example:
                    ; Code0401230: I0:
                    ;     cmp eax 0111 | jne Code04018D1
                        mov ebx esi | sub ebx 2
                        While B$ebx > ' ' | dec ebx | End_While
                        On D$ebx-3 = 'cmp ', jmp L5>>
                    End_If
                .End_If

            ..Else
;;
                  ; Not a '0111' Case >>> Search the 'jne ' >>> Get the next Label:
                    While D$esi <> 'jne '
                        inc esi | On esi >= edx, jmp L9>>
                    End_While
                    While D$esi <> 'Code'
                        inc esi | On esi >= edx, jmp L9>>
                    End_While
                    add esi 7 | lodsd
        
L1:                 While D$esi <> eax
                        inc esi | On esi >= edx, jmp L9>>
                    End_While
                    add esi 4 | On B$esi <> ':', jmp L1<
;;
            ..End_If

            jmp L0<<

       ...End_If

        inc esi
    .End_While

    call 'USER32.MessageBoxA' D$hwnd, {"WM_COMMAND not found:
  
You cannot edit the Resources Main Menu, if any   ", 0},
                                {'Menu IDs substitution failure', 0}, 0
    ret

L5: push esi
        call SaveOriginalMenuIDs
    pop esi

    call DisReplace esi, 4, {'&WM_COMMAND', 0}, 11

    call SubstituteMainMenuIDs

    mov B$WM_COMMAND_Found &TRUE
L9: ret

;;
  Menu ID Equates in 'DataForClipEquates'
  Original Equates in 'OriginalMenuIDs'
  esi yet pointing '0111' (&WM_COMMAND) in the Source
;;
SubstituteMainMenuIDs:
;;
  esi yet pointing the &WM_COMMAND Location:
  
  cmp eax &WM_COMMAND | jne Code04018D1
      mov eax D$ebp+010
      cmp ax 0M00_Calculate_
    
Code0401242: J8: | jne Code040142B
;;
    While B$esi <> '|' | inc esi | End_While
    While W$esi <> ' j' | inc esi | End_While | inc esi

    If D$esi <> 'jne '
        call 'USER32.MessageBoxA' D$hwnd, {'Unexpected MainWindowProc Main Menu Messages Cases organisation', 0},
                                {'Failure of Main Menu IDs substitutions', 0}, 0
        ret
    End_If

  ; Store the end of the Label Name closing the &WM_COMMAND Case in edx ('Text'):
    add esi 9 | While B$esi > ' ' | inc esi | End_While
    mov edx D$esi-4

L0: ..While D$esi <> edx
        inc esi

        ...If D$esi = '$ebp'
            ..If D$esi+4 = '+010'
                If D$esi-5 = 'cmp '
                    add esi 8 | jmp A5>
                End_If
                add esi 8
L1:             While D$esi <> 'cmp '
                    inc esi | On esi = D$SourceEnd, jmp L9>>
                End_While

                add esi 5 | On D$esi = '$ebp', jmp L1<

                While B$esi > ' ' | inc esi | End_While

A5:             .If W$esi = ' 0'
                    add esi 2 | mov ebx 0
                    push esi
L2:                     lodsb | cmp al ' ' | jbe L2>
                        sub al '0' | On al > 9, sub al 7
                        shl ebx 4 | or bl al | jmp L2<
L2:                 pop esi
                    dec esi

                  ; Possible ID in eax. Is it in 'OriginalMenuIDs':
                    If ebx <> 0
                        mov eax ebx, edi OriginalMenuIDs, ecx 100 | repne scasd
                    Else
                        mov ecx 0
                    End_If

                    If ecx > 0
                      ; One Based Indice of Menu ID in ecx (1 is M00_Menu):
                        sub ecx 100 | neg ecx
                        mov edi DataForClipEquates | inc edi

                        While ecx > 0
L3:                         inc edi | cmp B$edi ' ' | ja L3<    ; An Equate Name
L3:                         inc edi | cmp B$edi ' ' | jna L3<   ; Spaces
L3:                         inc edi | cmp B$edi ' ' | ja L3<    ; An Equate Value
L3:                         inc edi | cmp B$edi ' ' | jna L3<   ; Spaces
                            dec ecx
                        End_While

                      ; Length of Equate Name in ebx
                        mov ebx 0
                        While B$edi+ebx > ' ' | inc ebx | End_While
                        On B$edi+ebx-1 = ']', dec ebx
                      ; Length of Original Source Equate Value in ecx
                        mov ecx 0
                        While B$esi+ecx > ' ' | inc ecx | End_While

                        call DisReplace esi, ecx, edi, ebx
                    End_If
                .End_If

                While D$esi <> 'jne '
                    inc esi | On esi = D$SourceEnd, jmp L9>>
                End_While
                While D$esi <> 'Code'
                    inc esi | On esi = D$SourceEnd, jmp L9>>
                End_While

                While B$esi > ' ' | inc esi | End_While
                mov eax D$esi-4
L6:             While D$esi <> eax
                    inc esi | On esi = D$SourceEnd, jmp L9>>
                End_While
                add esi 4 | cmp B$esi ':' | jne L6<

                jmp L1<<
            ..End_If
        ...End_If
    ..End_While

    add esi 4 | cmp B$esi ':' | jne L0<<
L9: ret


; For replacing small words on the fly:

Proc DisReplace:
    Arguments @SourcePos, @DelLength, @Insert, @InsertLength
    Uses esi, edi, ecx

        add D$SourceLen 400 | add D$SourceEnd 400
        mov eax D@InsertLength

        .If eax > D@DelLength
            mov esi D$SourceEnd, edi esi
            add edi D@InsertLength | sub edi D@DelLength
            mov ecx esi | sub ecx D@SourcePos | std | rep movsb | cld

            mov esi D@Insert, edi D@SourcePos, ecx D@InsertLength
            rep movsb

        .Else
            mov esi D@Insert, edi D@SourcePos, ecx D@InsertLength
            rep movsb

            If eax < D@DelLength
                mov esi D@SourcePos | add esi D@DelLength
                mov ecx D$SourceEnd | sub ecx esi
                rep movsb
            End_If

        .End_If

        mov eax D@InsertLength | sub eax D@DelLength
        sub D$SourceLen 400 | add D$SourceLen eax
        sub D$SourceEnd 400 | add D$SourceEnd eax
EndP


[OriginalMenuIDs: ? #100]
; 'uMenu' comments
; We read the original Menu Items IDs yet in the new MenuEx Table:

SaveOriginalMenuIDs: ; Original IDs at +8?   'PrepareDisMenuIDs'  'TurnThisMenuToExType'
    mov D$MenulistPtr MenuList, esi MenuList        ; (ID / Ptr / Size)

    mov eax D$esi, ecx D$esi+8, esi D$esi+4
    inc eax | mov D$FirstMenuId eax

    mov D$EndOfDisMenu esi | add D$EndOfDisMenu ecx

  ; Header:  'uMenu'
    add esi 8

    mov edi OriginalMenuIDs, eax 0, ecx 100 | rep stosd

    mov edi OriginalMenuIDs, edx 0, D$SeparatorsNumber 0, D$PopUpNumber 0

L0: movzx eax W$esi+8
    .If eax = 0             ; No ID >>> Separator or PopUp
        If W$esi+12 = 0     ; Separator (8 zeroed Words)
            add esi 16 | On esi < D$EndOfDisMenu, jmp L0<
                jmp L9>
        Else                ; PopUp (+4 is the Added 'HelpID' for Popups Items only)
            add esi 14
            While W$esi <> 0 | add esi 2 | End_While | add esi 2+4
            Align_On 4, esi
        End_If

    .Else
        stosd
L1:     add esi 14
        If esi < D$EndOfDisMenu
            While W$esi <> 0 | add esi 2 | End_While | add esi 2
            Align_On 4, esi
        Else
L9:         mov D$edi 0 | ret
        End_If

    .End_If

    On esi < D$EndOfDisMenu, jmp L0<

____________________________________________________________________________________________

[MenuCharsSet: B$ '&<>=?@0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.' 0
 MenuCharsLen: D$ len   MenuChar: B$ 0]

; Is a Char valid for Naming The Menu IDs?:

MenuChars:
        push ecx, edi
            mov ecx D$MenuCharsLen, edi MenuCharsSet, B$MenuChar &FALSE
            repne scasb | jnz L9>
                mov B$MenuChar &TRUE
L9:     pop edi, ecx
ret
____________________________________________________________________________________________

[MenuReadPointer: ?   EndOfDisMenu: ?    DisItemFlag: ?]

PrepareDisMenuIDs:  ; 'ClipEquates', 'ForceMenusExType', 'TurnThisMenuToExType'

    mov D$MenulistPtr MenuList, esi MenuList        ; (ID / Ptr / Size)

  ; Temporary consider the first Menu, the Main one.
  ; Add Checking for the Main Window Menu later.
    mov eax D$esi, ecx D$esi+8, esi D$esi+4

  ; Kill the High Bit saying that this is a Pointer to the name of a Named ID, if any:
    and eax 0FFFF

    inc eax | mov D$FirstMenuId eax

    mov D$EndOfDisMenu esi | add D$EndOfDisMenu ecx

    mov D$MenuEquateIndice 'M00_'
    mov eax D$MenulistPtr | sub eax MenuList
    mov ebx 12, edx 0 | div ebx                 ; > indice = 0, 1, 2, ...
    mov ebx 10, edx 0 | div ebx
    add B$MenuEquateIndice+2 dl
    mov edx 0 | div ebx
    add B$MenuEquateIndice+1 dl

  ; Header:  'uMenu'
    add esi 8 | mov D$MenuReadPointer esi

    mov edi DataForClipEquates, edx 0, D$SeparatorsNumber 0, D$PopUpNumber 0
    mov al '[' | stosb | mov D$StartOfItemsLine edi

    mov eax D$MenuEquateIndice | stosd | mov eax 'Menu' | stosd

    dec D$FirstMenuID | call WriteClipIDvalue | inc D$FirstMenuID

L0: push edi
        mov edi OneItemString, esi D$MenuReadPointer
        add esi 14
        movzx eax W$esi-2 | and eax (not &MF_END) | mov D$DisItemFlag eax

        .If esi < D$EndOfDisMenu
            While W$esi <> 0
L1:             lodsw
                    call MenuChars | On B$MenuChar = &FALSE, mov al '_'
                stosb
            End_While | lodsw | stosb
            On D$DisItemFlag <> 0, add esi 4
            Align_On 4, esi | mov D$MenuReadPointer esi
        .Else
            pop edi
            While B$edi-1 <= ' ' | dec edi | End_While
            mov al ']' | stosb | mov al 0 | stosb | ret
        .End_If
    pop edi

    mov esi OneItemString

    While B$esi = tab
        inc esi
    End_While
    If B$esi = 0
        inc D$SeparatorsNumber | inc edx | jmp L0<<
    End_If

    If D$DisItemFlag <> 0
        inc D$PopUpNumber | inc edx | jmp L0<<
    End_If

T0: mov esi OneItemString, eax D$MenuEquateIndice | stosd
    While B$esi <= ' '
        lodsb                                           ; strip leading tabs and spaces
    End_While

L3: lodsb | cmp al 0 | je L4>
        On al = '&', jmp L3<                            ; do not write '&'
        On al = tab  , jmp L4>                          ; do not write 'hot keys'
        On al < '0', mov al '_'
        If al = '_'
            On B$edi-1 = '_', jmp L3<                   ; only one '_' at a time
        End_If
        stosb | jmp L3<                                 ; name

L4: call WriteClipIDvalue | inc edx | jmp L0<<

____________________________________________________________________________________________

[TempoSource: ?]

; Writes the [Menus IDs] into the Source.

WriteDisMenuIDs:
    mov ecx D$SourceLen | add ecx 1_000_000
    VirtualAlloc TempoSource ecx

  ; Make a temporary Copy:
    mov esi D$CodeSource, edi D$TempoSource, ecx D$SourceLen

    Align_On 4 ecx | shr ecx 2 | rep movsd

  ; Now Tempo >>> Original Source (simpler than killing all associated Variables)
  ; (edx = How many added Bytes):

    mov edi D$MainWindowProcIsThere, esi edi
    sub esi D$CodeSource | add esi D$TempoSource

  ; Start the Insert after a CRLF:
    While W$edi-2 <> CRLF | dec edi | dec esi | End_While

  ; Kep track of the original Source Last Chunk length:
    mov ebx D$SourceEnd | sub ebx edi

    mov al '_'
    mov ecx DRAWLINELEN | rep stosb | mov W$edi CRLF | add edi 2
    mov ecx DRAWLINELEN | rep stosb | mov D$edi CRLF2 | add edi 4

    mov edx (DRAWLINELEN+2+DRAWLINELEN+4)

    push esi
        mov esi DataForClipEquates

        While B$esi <> 0 | movsb | inc edx | End_While
    pop esi

    mov D$edi CRLF2 | add edi 4 | add edx 4

    mov ecx ebx | add ecx 400       ; Security CRLFs Tail
    rep movsb

    add D$SourceLen edx | add D$SourceEnd edx

    VirtualFree D$TempoSource
ret

____________________________________________________________________________________________
;;
  See if the Line before "call 'USER32.RegisterClassA'" is some "push Dataxxxx", 
  wich would provide the Structure Pointer:
;;
GetMainWindowProcFromStructure:
    mov esi D$LastRegisterClass
    While B$esi <> CR | dec esi | End_While
    While B$esi <> LF | dec esi | End_While
    While B$esi <= ' ' | inc esi | End_While
    On D$esi <> 'push', jmp L9>>

    add esi 5 | mov edi RegisterClassStructure
    While B$esi > ' ' | movsb | End_While | mov B$edi 0
;;
  At this point we know the Structure is declared in Data or ?Data. It may either be
  filled at write time or at run time. See first if the second (or third) Member of
  the Structure provides directely the 'MainWindowProc' label:
;;
    mov esi RegisterClassStructure, edi D$CodeSource
    call FirstDisSearch | On edi >= D$SourceEnd, jmp L9>>
    inc edi
    If D$ClassEx = 4
        While B$edi <> ':' | inc edi | End_While | inc edi
    End_If
    While B$edi <> ':' | inc edi | End_While
    add edi 2
    If D$edi = 'Code'
        mov D$CodeToMainWindowProc edi | jmp L9>>
    End_If
;;
  The MainWindowProc Label was not written directely in the Structure. It is filled
  at run time. Is the Structure Record accessed from somewhere in the Source? We first
  Add 4 or 8 to the pushed Label found when calling 'USER32.RegisterClass...' 
  (RegisterClassEx Structure has one dword for size in first Pos), and re-run a search
  of this Label evocation in Code:
;;
    .If B$ClassEx = 4
        mov esi RegisterClassStructure | While B$esi+1 <> 0 | inc esi | End_While
L0:     mov al B$esi | sub al '0' | On al > 9, sub al 7 | add al B$ClassEx
        If al > 0F
            mov bl al | sub al 010 | mov bl 1
        Else
            mov bl 0
        End_If
        add al '0' | On al > '9', add al 7 | mov B$esi al
        If bl > 0
            dec esi | jmp L0<
        End_If
    .End_If

    mov esi RegisterClassStructure, edi D$EndOfDisData | call FirstDisSearch

    .While edi < D$SourceEnd
        While B$edi <> ' ' | inc edi | End_While | inc edi

        If D$edi = 'Code'
            mov D$CodeToMainWindowProc edi | ret
        Else
            mov esi RegisterClassStructure | call NextDisSearch
        End_If
    .End_While
L9: ret


[MainWindowProcIsThere: ?]

WriteMainWindowProc:
L1: mov esi D$CodeToMainWindowProc, edi RegisterClassStructure
    While B$esi > ' ' | movsb | End_While
    On B$edi-1 = ']', dec edi
    mov B$edi ':', B$edi+1 0

    mov esi RegisterClassStructure, edi D$EndOfDisData | call FirstDisSearch

    mov D$MainWindowProcIsThere edi

  ; Make room for CRLF 'MainWindowProc: ':
    push edi
        mov esi D$SourceEnd | add esi 400
        mov ecx esi | sub ecx edi | inc ecx
        mov edi esi | add edi 18
        std | rep movsb | cld
        add D$SourceEnd 18 | add D$SourceLen 18
    pop edi

    mov W$edi CRLF | add edi 2
    mov D$edi 'Main', D$edi+4 'Wind', D$edi+8 'owPr', D$edi+12  'oc: '

L9: ret
____________________________________________________________________________________________

[NumberOfDisTitles: ?]

WriteDisTITLE:  ; 'DisTitle'
    push esi
        mov esi DisTitle
        .If B$DisTitle+13 = '9'
            mov B$DisTitle+13 'A'
        .Else
            inc B$DisTitle+13
            If B$DisTitle+13 = '['
                mov B$DisTitle+13 '0' | inc B$DisTitle+12
            End_If
        .End_If
        While B$esi <> 0 | movsb | End_While
    pop esi

    mov eax edi | add eax (TITLE_MAX/2) | mov D$NextDisTITLE eax

    mov D$edi '    ' | add edi 4
ret

____________________________________________________________________________________________
;;
  Problem: When Pointers to either Code or Data are found inside Data, we have to
  choose if they effectively are Pointers or not. It is statistically possible that
  what appears to possiblily be a Pointer, be nothing else but the result of random
  Needless to say, the bigger size the targetted File is, the more chances we have
  for a wrong interpretation.
  
  In such case, wrongly interpreting a random flow of Bytes as a Pointer is not
  catastrophic. The reverse is... : Say we have a dWord like 040301A that we suspect
  to be a Code Pointer. Now, if we do not validate the 'Code040301A' interpretation,
  the Application will hang after re-compilation, because what was located at 040301A
  in the original file has no chance to be at the same Address after re-compilation
  (because of Code size variations across various Compiler, because of PE Sections
  ordering, and so on...). I other words, once re-compiled, 'Code040301A' Value will
  not be 040301A (unless the targetted File had been builded with RosAsm).
  
  So, the better way is to validate those Pointers each time it is possible, when
  interpreting and writing the Data.
  
  For Data Pointers to Data, there is zero problem. In the worst cases, we may, for
  example, induce a no use Data Label, that will, fore example, cut a String presentation,
  into 2 sub-Strings, but this will not change a thing for the re-Compilation. Only
  the Data content (in place of that Pointer will be wrong, and user can restore it
  from the Label Name, if necesary, by hand).
  
  For Code, the problem is about not cutting into the middle of a valid Instruction.
  This is why, before runing 'CheckPointersInData', we have first run
  'DisassembleForIntructions' one shot (for having all Instructions Flaged INSTRUCTION).
  
  Here, we simply Flag the Data Part of RoutingMap, with INDIRECT, for making the 
  interpretations easier. We verify, also that any Pointer to Code does
  not break an Instruction into two. So, the possibilities for wrongly validating a 
  Pointer to Code is divided by 3 or 4...
;;

CheckPointersInData:
    mov esi D$SectionsMap, edx D$EndOfSectionsMap | sub edx 4
    add esi D$FirstSection

    .While esi < edx
    mov eax esi | sub eax D$SectionsMap | add eax D$DisImageBase

        ...If B$esi =  DATAFLAG
          ; We read the dWord Values of each Data in the PE:
L1:         mov eax esi | sub eax D$SectionsMap | add eax D$UserPeStart
            mov eax D$eax | sub eax D$DisImageBase | add eax D$UserPeStart
          ; Is it pointing somewhere inside the PE?
            ..If eax > D$UserPeStart
                .If eax < D$UserPeEnd
                    call IsItTruePointer | cmp eax 0 | je L5>>

                  ; Yes, say it in SizesMap:
                    mov ebx esi | sub ebx D$SectionsMap | add ebx D$SizesMap
                    or B$ebx POINTER

                  ; Yes > read the Pointer Section Flag:
                    sub eax D$UserPeStart | add eax D$SectionsMap
                    mov bl B$eax | and bl CODEFLAG+DATAFLAG+VIRTUALFLAG | jz L5>>
                        sub eax D$SectionsMap | add eax D$RoutingMap
                        or B$eax EVOCATED
                        If bl = CODEFLAG
                            test B$eax INSTRUCTION | jz L5> ; Do not kill a valid instruction.
                              ; Write the Code Routing Map:
                                or B$eax NODE+ACCESSED+LABEL
                              ; Write the Data Routing Map:
L2:                             mov eax esi |  sub eax D$SectionsMap | add eax D$RoutingMap
                                or B$eax INDIRECT
                                add esi 3       ; +1 down here > next dWord.
                        Else_If bl = DATAFLAG
                          ; Do not destroy a valid Data Type: ???
                            mov eax esi |  sub eax D$SectionsMap | add eax D$SizesMap
                          ;  cmp D$eax 0 | jne L5>
                            sub eax D$SizesMap | add eax D$RoutingMap
;;
  Problem: For Erde, it needs 'INDIRECT'. For the big File it needs 'EVOCATED'.
  This should indicate a problem in the Data outputing...
;;
                            or B$eax INDIRECT ;+EVOCATED+LABEL
                            add esi 3
                        End_If
                .End_If
            ..End_If
        ...End_If

L5:     inc esi
    .End_While

  ; Now, force a dummy Label to be outputed at the first and after last INDIRECT
  ; references (first and last of a flow of pointers, in order to ease the Data
  ; interpretations output, and group in one single set, flows of Pointers).
    mov esi D$SectionsMap | add esi D$FirstSection

    .While esi < edx
        If B$esi = DATAFLAG
            mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap

            test B$eax INDIRECT | jz L5>
                mov D$eax LABEL+EVOCATED+INDIRECT  ; 04A
L1:             add eax 4 | add esi 4
                test B$eax INDIRECT | jz L3>
                test B$eax LABEL+EVOCATED | jnz L3>
                    mov D$eax 0 | jmp L1<
L2:
L3:             or B$eax LABEL+EVOCATED

        Else
L5:         inc esi
        End_If

    .End_While
ret
____________________________________________________________________________________________

;;
  In RosAsm Assembler, the Table Sizes (usually zeroed Tables), are limited,
  on purpose (they could as well not be limited...), to push the users to good
  programming practices. So, in the Disassembler we need a Routine for splitting
  these too big Tables, by emitting (forcing) a dummy Label, that has no other
  purpose but forcing the Assembler to eat such demential Tables, and to kill the limitation.
;;

SplitBigData:
    mov esi D$SectionsMap, edx D$EndOfSectionsMap | add esi D$FirstSection
    mov ebx esi | sub ebx D$SectionsMap | add ebx D$RoutingMap

    .While esi < edx
        mov al B$esi

        .If al = DATAFLAG
            mov ecx 4
            While B$esi = al
                inc esi | inc ecx | inc ebx | On esi >= edx, ret
                test B$ebx EVOCATED+LABEL | jz L1>
                    mov ecx 4

L1:             If ecx >= LOOPDATAMAX
                    mov ebx esi | sub ebx D$SectionsMap | add ebx D$RoutingMap
                    or B$ebx LABEL+EVOCATED
                    ;sub ebx D$RoutingMap | add ebx D$DisImageBase | hexprint ebx
                    mov ecx 4
                End_If
            End_While

        .Else_If al = VIRTUALFLAG
            mov ecx 4
            While B$esi = al

                inc esi | inc ecx | inc ebx | On esi >= edx, ret
                test B$ebx LABEL | jz L1>
                    mov ecx 4

L1:             If ecx >= LOOPVDATAMAX
                    mov ebx esi | sub ebx D$SectionsMap | add ebx D$RoutingMap
                    or B$ebx LABEL+EVOCATED
                    ;sub ebx D$RoutingMap | add ebx D$DisImageBase | hexprint ebx
                    mov ecx 4
                End_If
            End_While

        .End_If

        inc esi | inc ebx

    .End_While
ret
____________________________________________________________________________________________

FlagsCoherency:
    mov esi D$SizesMap, edx D$EndOfSizesMap | add esi D$FirstSection
    mov ebx esi | sub ebx D$SizesMap | add ebx D$RoutingMap

    .While esi < edx
        mov eax ebx | sub eax D$RoutingMap | add eax D$SectionsMap
        On B$eax <> DATAFLAG, jmp L2>>

        test B$esi FP4 | jz L1>
            ;call IsLabelInsideFp 4
            and D$ebx 0FF
            mov D$esi 0, B$esi FP4 | jmp L2>>
            ;mov B$esi FP4 | jmp L2>>

L1:     test B$esi FP8 | jz L1>
            ;call IsLabelInsideFp 8
            and D$ebx 0FF | mov D$ebx+4 0
            mov D$esi 0, D$esi+4 0, B$esi FP8 | jmp L2>>

            ;mov B$esi FP8 | jmp L2>>

L1:     test B$esi FP10 | jz L1>
            ;call IsLabelInsideFp 10
            and D$ebx 0FF | mov D$ebx+4 0, W$ebx+8 0
            mov D$esi 0, D$esi+4 0, W$esi+8 0, B$esi FP10 | jmp L2>
           ;mov B$esi FP10 | jmp L2>>

L1:     test B$esi DWORD | jz L1>
            ;call IsDwordString | On eax = &TRUE, jmp L2>
            ;and D$ebx 0FF
            mov B$esi DWORD | jmp L2>

L1:     Test B$esi STRINGS | jz L1>
            mov al POINTER+DWORD | not al | and B$esi al

L1:     Test B$esi POINTER | jz L2>
;;
            push ebx
                sub ebx D$RoutingMap | add ebx D$UserPeStart | mov eax D$ebx
                sub eax D$DisImageBase | add eax D$SectionsMap
                On eax < D$SectionsMap, jmp L4>
                On eax > D$EndOfSectionsMap, jmp L4>
                   ; If B$eax = CODEFLAG
                   ;     sub eax D$SectionsMap | add eax D$RoutingMap
                   ;     test B$eax INSTRUCTION | jnz L3>

L4:                         ;pop ebx | push ebx
                            ;sub ebx D$RoutingMap | add ebx D$DisImageBase
                            ;hexprint ebx
                            ;mov D$MapingBase ebx
                            ;map

                            xor B$esi POINTER
                   ; End_If
L3:         pop ebx
;;
L2:     inc esi | inc ebx
    .End_While
ret


[LabelInsideFP: ?]

;;
  FP Data may be crossed over by Strings Recognitions.
  
  In such cases, the FP is Prevalent, and we push the String Label downward.
;;

Proc IsLabelInsideFp:
    Argument @N

        mov B$LabelInsideFP &FALSE

      ; ebx >>> RoutingMap, first FP Byte.
        mov eax 1, ecx D@N | dec ecx

L0:     test B$ebx+eax LABEL | jz L1>
            mov B$LabelInsideFP &TRUE
L1:     inc eax | loop L0<

        On B$LabelInsideFP = &TRUE, or B$ebx+eax LABEL
EndP
____________________________________________________________________________________________

Proc FlagsCleaner:
    pushad
        mov ecx D$SectionsMap, edx D$EndOfSectionsMap, esi D$RoutingMap, ebx D$SizesMap
        mov eax (not (INSTRUCTION+NODE+EXPORTNODE+PUSH_EBP))

        .While ecx < edx
          ; Data cannot assume any of these Routing Flags:
            If B$ecx = DATAFLAG
                xor al EXPORTNODE
                and B$esi al
                xor al EXPORTNODE

          ; Same for VirtualData:
            Else_If B$ecx = VIRTUALFLAG
                and B$esi al

          ; Code cannot assume any Size Flag:
            Else_If B$ecx = CODEFLAG
                mov B$ebx 0

            End_If

            inc ecx, esi, ebx

        .End_While
    popad
EndP
____________________________________________________________________________________________

;;
  eax points to a supposed Pointer in the PE data section.
  Might not be a true Pointer but, as well, a String, unicode String, or anything else.
;;

IsItTruePointer:
    pushad
      ; esi points to 'SectionsMap'. Read the real PE Value:
        sub esi D$SectionsMap | add esi D$UserPeStart | mov eax D$esi

      ; Do not break any Label:
        mov ebx esi | sub ebx D$UserPeStart | add ebx D$RoutingMap
        test D$ebx ((LABEL shl 24)+(LABEL shl 16)+(LABEL shl 8)) | jz L1>
            and B$ebx (not INDIRECT)
            popad | mov eax 0 | ret

      ; First, do not break any identifed Data Type:
L1:     mov ebx esi | sub ebx D$UserPeStart | add ebx D$SizesMap
        If D$ebx <> 0
            popad | mov eax 0 | ret
        End_If



      ; Do not break an Evocated Ascii String:
        mov edx D$TruthAsciiTable
        mov eax 0 | add esi 3 | add ebx 3 | mov al B$esi

        push esi, ebx
            While B$edx+eax = GOODASCII
                If B$ebx <> 0
                    test B$ebx BYTE | jz L2>
                        or B$ebx STRINGS
                        pop ebx, esi | popad | mov eax 0 | ret
                End_If

                dec esi | dec ebx
                mov al B$esi
            End_While

L2:     pop ebx, esi

      ; Do not break an Evocated Unicode String:
        mov al B$esi
        If al = 0
            dec esi | dec ebx
            mov al B$esi
        End_If

        While B$edx+eax = GOODASCII
            On B$esi+1 <> 0, jmp L2>

            If B$ebx <> 0
                test B$ebx WORD | jz L2>
                    or B$ebx STRINGS
                    popad | mov eax 0 | ret
            End_If

            sub esi 2
            dec ebx | cmp B$ebx 0 | jne L2>
            dec ebx

            mov al B$esi
        End_While

L2:
    popad
ret
____________________________________________________________________________________________


CheckZeroEndedString:
    push edx
        mov ebx eax | sub ebx D$SectionsMap | add ebx D$UserPeStart
        mov edx D$TruthAsciiTable, eax 0

        push ebx
L0:         mov al B$ebx | inc ebx
            cmp B$edx+eax GOODASCII | je L0<
        pop eax

        cmp B$ebx-1 0 | ja L9>
        sub ebx eax | cmp ebx 5 | jb L9>

          ; Valid String found: Flag the String:
            sub eax D$UserPeStart | add eax D$RoutingMap | or B$eax STRINGS+BYTE
          ; FLAG the Pointer:
            mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap | or B$eax POINTER

            add esi 3
L9: pop edx
ret
____________________________________________________________________________________________

WriteLabelFromEsi:
    On esi = D$LastWrittenLabel, ret
    mov ebx esi | sub ebx D$UserPeStart | add ebx D$DisImageBase
            push ebx
            mov ax 0A0D | stosw
            mov D$edi 'Code' | add edi 4
            push 0-1
L0:         mov eax ebx | shr ebx 4 | and eax 0F
            add eax '0' | On eax > '9', add eax 7
            push eax
            cmp ebx 0 | ja L0<
            mov B$edi '0' | inc edi
L0:         pop eax | cmp eax 0-1 | je L9>
            mov B$edi al | inc edi | jmp L0<
L9:         mov W$edi ': ' | add edi 2
        pop eax
        call WriteLocalLabelFromEax
ret


[ItWasAlignment: ?]

; In: ebx (> eax) > RoutingMap // esi > CodeTest // edi > CodeSource
WriteDBandData:
    mov B$ItWasAlignment &FALSE
  ; May be Valid Code, but not ACCESSED:
; (This first part should no more be of any use).
    test B$ebx PUSH_EBP | jz L1>
        push ebx
L0:         inc ebx | cmp ebx D$EndOfRoutingMap | je L0>
            test B$ebx ACCESSED | jnz L0>
                mov eax ebx | sub eax D$RoutingMap | add eax D$UserPestart
                .If B$eax = 05D                 ; pop ebp
                    If B$eax+1 = 0C3            ; ret
                        inc ebx
                        mov ecx ebx | pop ebx | call ReFlagNonaccessed ; | jmp L9>>
                        mov B$NonAccessedByteWritten &FALSE, B$LabelWritten &FALSE | ret
                    Else_If B$eax+1 = 0C2   ; ret n (n dWord aligned)
                        Test B$eax+2 00_11 | jnz L0>
                            add ebx 3
                            mov ecx ebx | pop ebx | call ReFlagNonaccessed ; | jmp L9>>
                            mov B$NonAccessedByteWritten &FALSE, B$LabelWritten &FALSE | ret
                    End_If
                .End_If
            jmp L0<
L0:     pop ebx

  ; May be a 'NOP' or 'Int 3', or... , Alignment:
L1: call AlignRecognition

    If B$ItWasAlignment = &TRUE
        mov D$edi 'Alig', W$edi+4 'n ' | add edi 6
        move D$edi D$Alignement | add edi 4
        mov D$StartOfDisLine esi | add esi D$Alignementlenght
        On B$WithCommentedHexa = &TRUE, call CommentHexa
        NextDisLine | jmp L9>>
    End_If

L2: mov eax ebx
    push esi
       ; If B$LabelWritten = &FALSE
            push eax
                call WriteLabelFromEsi | NextDisLine
            pop eax
       ; End_If
;;
pushad
mov eax esi | sub eax D$UserPeStart | add eax D$DisImageBase
On eax = 0401354, int3
popad
;;
        call WriteDisCodeLabel
      ; Durty: Need a version without the CRLFs:
        While B$edi <> ':' | dec edi | End_While | inc edi
        mov B$edi ' ' | inc edi

        mov edx eax, ecx 0
        mov D$edi 'DB  ' | add edi 3

L0:     test B$edx ACCESSED | jnz L5>  ; 01F
        test B$edx EVOCATED | jz L3>
            pushad
                call WriteDisCodeLabel
            popad
           ; mov B$edi CR, B$edi+1 LF, D$edi+2 '    ' | add edi 6

L3:         movzx eax B$esi | inc esi
            If eax < 010
                mov D$edi '    ' | inc edi
                On eax = 0, inc edi
            End_If
            call WriteEax
            mov B$edi ' ' | inc edi | inc ecx
            If ecx = 20
                mov B$edi-1 ','
                NextDisLine | mov D$edi '    ' | add edi 3
                mov ecx 0
            End_If
            inc edx
            If edx < D$EndOfRoutingMap
                test B$edx EVOCATED | jnz L5>
                jmp L0<
            End_If

L5:     On D$edi-3 = '    ', sub edi 3 ; <--- Probably stupid.
        NextDisLine
        mov ecx esi
    pop esi

    sub ecx esi | jecxz L9>
    mov ebx esi | call WriteCommentedAsciiData | NextDisLine
    .If B$ItWasReallyAscii = &FALSE
        If ecx > 3
            call WritedCommentedWordsData | NextDisLine
        End_If
    .End_If

L9: mov B$NonAccessedByteWritten &TRUE, B$LabelWritten &FALSE
ret

;;
[AlignNop: B$ 090
 AlignInt3: 0CC
 AddB_eaxAl: 0 0
 LeaEspLong: 08D 0A4 024 0 0 0 0    ; lea esp D$esp+00 >>> 8D A4 24 00 00 00 00
 LeaEsiLong: 08D 0B4 026 0 0 0 0    ; lea esi D$esi+00 >>> 8D B4 26 00 00 00 00
 LeaEsp: 08D 064 024 0              ; lea esp D$esp+00 >>> 8D 64 24 00 
 LeaEcx: 08D 049 0                  ; lea ecx D$ecx+00 >>> 8D 49 00 
 LeaEbx: 08D 09B 0 0 0 0            ; lea ebx D$ebx+00 >>> 8D 9B 00 00 00 00
 AddEax0: 05 0 0 0 0                ; add eax 0 >>> 05 00 00 00 00 
 MovEdiEdi: 08B 0FF                 ; mov edi edi >>> 8B FF 
 MovEsiEsi: 089 0F6                 ; mov esi esi >>> 89 F6 
 ]

[AlignTable: AlignNop 1, AlignInt3 1, AddB_eaxAl 2, LeaEspLong 7, LeaEsp 4, LeaEcx 3,
             LeaEbx 6, AddEax0 5, MovEdiEdi 2
             0 0]
;;

[AlignTablePointer: ?    Alignement: ?   Alignementlenght: ?]

AlignRecognition:
    mov B$ItWasAlignment &FALSE
    pushad
      ; esi > UserPe
      ; ebx > RoutingMap . How many Bytes?
        mov ecx 0
L0:     test B$ebx ACCESSED | jnz L1>
            inc ecx | inc ebx | cmp ebx D$EndOfRoutingMap | jb L0<

L1:   ; ecx = How many Bytes unaccessed.
        On ecx = 0, jmp L9>>

        mov D$Alignementlenght ecx

        .If ecx > 0100
            jmp L9>>
        .Else_If ecx > 080
            test ebx 00_1111_1111 | jnz L9>>
                mov D$Alignement '0100'
        .Else_If ecx > 040
            test ebx 00_0111_1111 | jnz L9>>
                mov D$Alignement '080 '
        .Else_If ecx > 020
            test ebx 00_0011_1111 | jnz L9>>
                mov D$Alignement '040 '
        .Else_If ecx > 010
            test ebx 00_0001_1111 | jnz L9>>
                mov D$Alignement '020 '
        .Else_If ecx > 08
            test ebx 00_1111 | jnz L9>>
                mov D$Alignement '010 '
        .Else_If ecx > 04
            test ebx 00_0111 | jnz L9>>
                mov D$Alignement '08  '
        .Else  ; _If ecx > 0
            test ebx 00_0011 | jnz L9>>
                mov D$Alignement '04  '
        .End_If

        mov B$ItWasAlignment &TRUE
L9: popad
ret

;;
  Supposed no more use Instructions checking, because, at that stage, if no pointer
  has been found anywhere in the File to this Location, we can suppose this is neither
  Code nor Data, and that, anyway, even if it was not Alignment, this interpretation
  would not change a thing: The rebuilt would be wron,g in any case. So...
;;
L0:     lodsb

        ..If al = 090                   ; nop
            dec ecx | cmp ecx 0 | ja L0<

        ..Else_If al = 0CC              ; int 3
            dec ecx | cmp ecx 0 | ja L0<

        ..Else_If al = 0
            If B$esi = 0                ; add B$eax al >>> 0 0
                inc esi | sub ecx 2 | jc L9>>
                cmp ecx 0 | ja L0<
            End_If
           ; dec esi
           ; While B$esi = 0
           ;     inc esi | dec ecx | jc L9>>
           ; End_While
           ; cmp ecx 0 | ja L0<

        ..Else_If al = 08D              ; lea esp D$esp+00 >>> 8D A4 24 00 00 00 00
            .If D$esi = 024A4
                If W$esi+4 = 0
                    add esi 6 | sub ecx 7 | jc L9>>
                    cmp ecx 0 | ja L0<
                End_If

            .Else_If D$esi = 0B6       ; lea esi D$esi >>> 8D B6 00 00 00 00
                                       ; LeaEsiLong2 --> Guga
                If B$esi+4 = 0
                    add esi 5 | sub ecx 6 | jc L9>>
                    cmp ecx 0 | ja L0<<
                End_If
            .Else_If D$esi = 026B4      ; lea esi D$esi+00 >>> 8D B4 26 00 00 00 00
                If W$esi+4 = 0
                    add esi 6 | sub ecx 7 | jc L9>>
                    cmp ecx 0 | ja L0<<
                End_If
            .Else_If D$esi-1 = 024648D  ; lea esp D$esp+00 >>> 8D 64 24 00
                add esi 3 | sub ecx 4 | jc L9>>
                cmp ecx 0 | ja L0<<
            .Else_If D$esi = 09B        ; lea ebx D$ebx+00 >>> 8D 9B 00 00 00 00
                If B$esi+4 = 0
                    add esi 5 | sub ecx 6 | jc L9>>
                    cmp ecx 0 | ja L0<<
                End_If
            .Else_If W$esi = 049        ; lea ecx D$ecx+00 >>> 8D 49 00
                add esi 2 | sub ecx 3 | jc L9>>
                cmp ecx 0 | ja L0<<
            .Else_If W$esi = 040        ; lea eax D$eax+00 >>> 8D 40 00
                add esi 2 | sub ecx 3 | jc L9>>
                cmp ecx 0 | ja L0<<
            .End_If

        ..Else_If al = 05               ; add eax 0 >>> 05 00 00 00 00
            If D$esi = 0
                add esi 4 | sub ecx 5 | jc L9>>
                cmp ecx 0 | ja L0<<
            End_If

        ..Else_If al = 089              ; mov esi esi >>> 89 F6
            If B$esi = 0F6
                inc esi | sub ecx 2 | jc L9>>
                cmp ecx 0 | ja L0<<
            End_If

        ..Else_If al = 08B              ; mov edi edi >>> 8B FF
            If B$esi = 0FF
                inc esi | sub ecx 2 | jc L9>>
                cmp ecx 0 | ja L0<<
            Else_If B$esi = 0C0         ; mov eax eax >>> 8B C0
                inc esi | sub ecx 2 | jc L9>>
                cmp ecx 0 | ja L0<<
            End_If
        ..End_If

L5:     On ecx = 0, mov B$ItWasAlignment &TRUE

L9: popad
ret

____________________________________________________________________________________________

;;
  After having analyzed all possible Code, but, before the Negative Recogition,
  if a complete Section holds nothing but Data, we can bet that it is a true
  Data Section, without any Code inside.
  
  Seems to be of little help.
;;

OldFlagTrueDataSection:
    mov ecx D$DisNumberOfSections

    GetPeHeader SectionsHeaders

L0: mov ebx D$eax+SECTION_FLAG
    test ebx &IMAGE_SCN_MEM_EXECUTE | jnz L1>>

    ...If ebx = &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
        push eax, ecx
            mov esi D$eax+SECTION_RVA | add esi D$SectionsMap
            mov edx D$eax+SECTION_RVASIZE
            mov ebx D$eax+SECTION_FILESIZE
            On edx < ebx, xchg edx ebx
            add edx esi | add ebx esi

            push esi, ebx, edx
                .While esi < ebx
                    .If B$esi <> 0
                        If B$esi <> DATAFLAG
                        ;sub esi D$SectionsMap | add esi D$DisImageBase |
                       ; mov D$MapingBase esi | map
                           ; sub esi D$SectionsMap | add esi D$DisImageBase
                           ; hexprint esi

                           ; pop edx, ebx, esi
                           ; sub edx D$SectionsMap | add edx D$DisImageBase
                           ; sub ebx D$SectionsMap | add ebx D$DisImageBase
                           ; sub esi D$SectionsMap | add esi D$DisImageBase
                           ; hexprint esi, ebx, edx
                           ; map
                            movzx eax B$esi | On eax = CODEFLAG, jmp L5>
                            pop eax, eax, eax, ecx, eax | jmp L1>>
                        End_If
                    .End_If

                    inc esi

                .End_While
L5:         pop edx, ebx, esi

          ; If here, this Section holds nothing but data. Flag it all:
            While esi < ebx
                mov B$esi DATAFLAG | inc esi
            End_While
            While esi < edx
                mov B$esi VIRTUALFLAG | inc esi
            End_While

        pop ecx, eax

    ...End_If

L1: add eax SECTIONHEADERSIZE | dec ecx | jnz L0<< ;loop L0<
ret


FlagTrueDataSection:
    mov ecx D$DisNumberOfSections

    GetPeHeader SectionsHeaders

L0: mov ebx D$eax+SECTION_FLAG | test ebx &IMAGE_SCN_MEM_EXECUTE | jnz L8>>

    ...If ebx = &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
        push eax, ecx
            mov esi D$eax+SECTION_RVA | add esi D$SectionsMap
            mov edx D$eax+SECTION_RVASIZE
            mov ebx D$eax+SECTION_FILESIZE
            On edx < ebx, xchg edx ebx
            add edx esi | add ebx esi

            push esi, ebx, edx
                .While esi < ebx
                    .If B$esi <> 0
                        If B$esi = DATAFLAG
                            ; OK.
                        Else_If B$esi = VIRTUALFLAG
                            ; OK.
                        Else
                            pop eax, eax, eax, ecx, eax | jmp L8>>
                        End_If
                    .End_If

                    inc esi

                .End_While

L5:         pop edx, ebx, esi

          ; If here, this Section holds nothing but data. Flag it all:
            While esi < ebx
                mov B$esi DATAFLAG | inc esi
            End_While
            While esi < edx
                mov B$esi VIRTUALFLAG | inc esi
            End_While

        pop ecx, eax

    ...End_If

L8: add eax SECTIONHEADERSIZE | dec ecx | jnz L0<<
ret

____________________________________________________________________________________________

DisAlign:
    mov esi D$SectionsMap | add esi D$FirstSection

    .While esi < D$EndOfSectionsMap
        ...If B$esi = CODEFLAG
            While B$esi = CODEFLAG
                inc esi | On esi >= D$EndOfSectionsMap, ret
            End_While

            ..If B$esi = 0
                mov ebx esi | inc esi
                While B$esi = 0
                    mov eax esi | sub eax D$SectionsMap | add eax D$RoutingMap
                    test B$eax LABEL | jnz L9>
                    inc esi | On esi >= D$EndOfSectionsMap, ret
                End_While

                .If B$esi = CODEFLAG
                    mov edx esi, ecx edx | sub ecx ebx

                    push esi, ebx
                        sub ebx D$SectionsMap | add ebx D$RoutingMap
                        mov esi ebx | sub esi D$RoutingMap | add esi D$UserPeStart
                        call AlignRecognition
                    pop ebx, esi

                    If B$ItWasAlignment = &TRUE
                        mov edi ebx, al CODEFLAG | rep stosb
                    End_If

                .End_If
            ..End_If
        ...End_If

L9:     inc esi
    .End_While
ret
____________________________________________________________________________________________

[NoDirectString: "; No direct Access found to this valid Code Chunk:
    "
 NoDirectStringLength: len]

ReFlagNonaccessed:
    push esi, ecx
        mov esi NoDirectString, ecx D$NoDirectStringLength
        sub edi 2 | rep movsb
    pop ecx, esi

  ; ebx > Start // ecx > End of Chunk that may be forced to ACCESSED, once the
  ; 'NoDirectString' Comment has been written:
    push edi
        sub ecx ebx | inc ecx | mov edi ebx, al ACCESSED | rep stosb
    pop edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[Prefixes: ?]

CommentHexa:
   ; mov al CR | stosb | mov al LF | stosb ;;; for Ret and ret n:
    If W$edi-2 = 0A0D
        sub edi 2
    End_If
    mov al ' ' | While edi < D$AlignedComment | stosb | End_While

    mov D$edi '  ; ' | add edi 4

    mov ebx D$StartOfDisLine ;| sub ebx D$Prefixes |
    mov D$Prefixes 0

    While ebx < esi
        movzx eax B$ebx | inc ebx
        mov ecx eax | shr ecx 4
        and eax 0F | and ecx 0F
        mov al B$HexaTable+eax, cl B$HexaTable+ecx
        shl eax 8 | or eax ecx | or eax 020200000 | stosd | dec edi
    End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; StringsMap jobs

; Example: call GetStringsMap RoutingMap, esi

Proc GetStringsMapSymbol:
    Argument @Map, @Pointer
    Uses esi

        mov eax D@Pointer | sub eax D@Map | shl eax 2 | mov esi D$eax ; Wrong!!!

        If esi <> 0
            mov B$edi '_' | inc edi
            While B$esi > ' ' | movsb | End_While
        End_If
EndP




