TITLE Assembler

;;

  'EncodeLines' 'NewSearchApiName'
____________________________________________________________________________________________
____________________________________________________________________________________________

 This part holds the various Routines envolved in the general Assembly process. No
 overall description can be given for them. The simpler way to understand the Assembler
 organisation is to start at 'AsmMain', and to follow up with Right-Click.

 At a TITLE point of vue, the Assembler is dispatched into 4 TITLEs:
 
 [Parsers] / [Assembler] / [AsmMain] / [Encoder]


 Just a quick description:
 
 Encodage of an instruction is done in 4 steps.

 In the first one, parameters are analyzed by '...ParameterAnalyze's routines. They
 tell us how many parameters, what kind of parameter each and if they are 'complex'
 (for exemple if SIB is needed); all needed variables and flags are set.

 In 'Encode', (second step) we do nothing else than 'routing' the more stupid way
 according with mnemonics and parameters. I did this this way to give the easiest
 access for new mnemonics adding. Killing to write but blazing fast and simple to
 maintain.

 When jumping out 'Encode', a third party performs ending fitting controls and writes
 true code in CodeList. This is this collection of small routines which names discribe
 the critical Intel significants like 'op_op_modReg2Rm_imm8'. I cut encodage itself
 in second and third steps just to make source shorter but these two steps are, in fact,
 one and same operation.

 At the very end of assembly job, a 'FillSymbol' routine fullfills the zeroed part of
 code with whished labels' adresses, either absolute or relative (at coding time these
 values may be unknown, and we work one pass. RosAsm is, as far as i know, the only
 one assembler that do it this way, with respect of user defined sizes).
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

; some basic equates:
                    ; > (As concerned tables have 3 dwords per pointed resource),
                    ; > Allow 100 differents...

[MAXDIALOG 2100]     ; ... Dialogs.
[MAXMENU 2100]       ; ... Menus.
[MAXBITMAP 2100]     ; ... BitMaps
[MAXSTRINGS 2100]
[MAXFONT 2100]
[MAXWAVE 2100]
[MAXAVI 2100]
[MAXRCDATA 2100]
[MAXCURSOR 2100]
[MAXICON 2100]

[MAXRESOURCE 3500]   ; Allow, as a whole, 1000 different resources in one PE


[LINKERDEFAULT 0400000, DRIVERDEFAULT 010000][LinkerDllDefault: 0_1000_0000][RelocsWanted: D$ 0]

[tab 9
 CR 13
 LF 10
 CRLF 0A0D
 CRLF2 0A0D0A0D
 PageSize 01000
 ;PageMask 00_1111_0000_0000_0000   ; to check if a pointer reach next page
 PageMask 0FFFF_F000
 ByteHighbit  080
 WordHighbit  08000
 dWordHighBit 08000_0000]

; For Encoding of reg field:
[regEax  00_0000  regAx   00_0000  regAl   00_0000  regEcx  00_0001  regCx   00_0001
 regCl   00_0001  regEdx  00_0010  regDx   00_0010  regDl   00_0010  regEbx  00_0011
 regBx   00_0011  regBl   00_0011  regEsp  00_0100  regSp   00_0100  regAh   00_0100
 regEbp  00_0101  regBp   00_0101  regCh   00_0101  regEsi  00_0110  regSi   00_0110
 regDh   00_0110  regEdi  00_0111  regDi   00_0111  regBh   00_0111]

; For Encoding of the Segment Register (sreg) Field:
[regEs  0000_0000  regCs  0000_1000  regSs  0001_0000  regDs  0001_1000
 regFs  0010_0000  regGs  0010_1000]

; For Encoding of Special-Purpose Register (eee) Field
; (Control Registers and Debug Registers cannot be used in applications. There need
;  privilege 0 and are for system - not implemented in 'Encode:' MOV to/from Debug/Control
;  registers))
[regCr0  00_0000  regCr2  00_0010  regCr3  00_0011  regCr4  00_0100
 regDr0  00_0000  regDr1  00_0001  regDr2  00_0010
 regDr3  00_0011  regDr6  00_0110  regDr7  00_0111]

; For encoding of FPU regs:
[regST0 0  regST1 1  regST2 2  regST3 3  regST4 4  regST5 5  regST6 6  regST7 7]

; For encoding of MMX registers:
[regMM0 0  regMM1 1  regMM2 2  regMM3 3  regMM4 4  regMM5 5  regMM6 6  regMM7 7]


; For Encoding of Operand Size (w) Bit

[ByteSize    0000        ; true wBit, when encoding, can only be 0 (byte size) or
 wordSize    0011        ; 1 (full size). Word / Double size discrimination
 doubleSize  0001        ; will be done by Operand-size override (066 if bit 2 set)
 QuadSize  000100        ; for FPU
 TenSize   001000        ; for FPU
 HalfSize  001111        ; for Packed BCD
 FPUsize   001010        ; for 108 bytes of FPU image > now X$ !!!!!
 OctoSize 0010000        ; for XMM memories
 Xsize        0FF]       ; for 'Fitting' with specific unregular size Opcodes
;;
 following are to fill 'Operands', which is a global image of what operands in a line.
 in some unregular cases, AL, AX and EAX may have specific encodage we can't' guess
 while analysing parameters. having symbols in order allow tests like:
    cmp Operands, RegToMem | ja >L1    instead of:
    cmp operands, RegToMem | je >L1    which solves this problem in case of no use
 of possible Areg by an instruction encodage.
;;
[RegToReg  1
 MemToReg  2
 RegToMem  3
 ImmToReg  4
 ImmToMem  5
 RegToImm  6  ; yes...: for 'out imm8, accum'
 ImmToImm  7  ; for ENTER and OUT
 ;;;  MemToMem  8  ; for FPU ???

 reg   1    ; for general registers
 sReg  2    ; for segment register
 cReg  3    ; for control registers
 dReg  4    ; for debug registers
 imm   5    ; immediate values
 mem   6    ; for memory symbolic adresses (.$ Labels)
 dis   7
 STreg 8    ; for FPU regs
 MMreg 9    ; for MMX regs
 XMMreg 10] ; for XMM regs

[bMem  'B'  ; byte sized memory
 uMem  'U'  ; Unicode string
 wMem  'W'  ; word size memory
 dMem  'D'  ; double word size memory
 qMem  'Q'  ; height bytes size memory
 rMem  'R'  ; height bytes size for FPU real numbers
 tMem  'T'  ; Ten bytes size  for FPU real numbers
 hMem  'H'  ; Height bytes size for FPU real numbers (to keep a while....)
 fMem  'F'  ; Four bytes size for FPU real numbers
 oMem  'O'  ; OctoWords XMM sizes
 xMem  'X'  ; Weird and XMM sizes

 NotFound 0FF]

; Where to jump:
[DownLong   00_1000    ; ja L9>>
 DownShort  00_0100    ; jmp L7>
 UpLong        0010    ; jz L0<<
 UpShort       0001]   ; loop L5<

; Labels flags:

[DataLabelFlag 00_0010
 CodeLabelFlag 00_0100
 DoneFlag      00_0001]

____________________________________________________________________________________________
____________________________________________________________________________________________

; Main asm memories:

[AsmTablesLength: ?]

GetAsmTables:
    ;VirtualFree D$NewWinEquatesMem
    VirtualFree D$IpTable, D$StatementsTable, D$StatementsTable2
    VirtualFree D$PlainLabelList, D$EquateList, D$MacroData

    call GetResourcesSize

    add eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation
    mov D$AsmTablesLength eax
    VirtualAlloc CodeSourceA eax | add D$CodeSourceA 010

    ;mov eax D$SourceLen | add eax 1_000_000 | add eax D$MemReservation
    VirtualAlloc CodeSourceB D$AsmTablesLength | add D$CodeSourceB 010
ret


GetResourcesSize:
    mov esi CursorList, eax 0 | call AddResourcesSyze
    mov esi GroupCursorList | call AddResourcesSyze
    mov esi IconList | call AddResourcesSyze
    mov esi MenuList | call AddResourcesSyze
    mov esi DialogList | call AddResourcesSyze
    mov esi StringsList | call AddResourcesSyze
    mov esi GroupIconList | call AddResourcesSyze
    mov esi WaveList | call AddResourcesSyze
    mov esi AviList | call AddResourcesSyze
    mov esi RCdataList | call AddResourcesSyze
    mov esi BitMapList | call AddResourcesSyze
ret

AddResourcesSyze:
    While D$esi <> 0
        add eax D$esi+8 | add esi (4*3)
    End_While
ret


; clear memory (CodeList now is a reuse of CodeSourceB):

ReleaseAsmTables:
    VirtualFree D$CodeSourceA, D$CodeSourceB, D$LabelList, D$MacroList,
                D$CodeRef, D$DataRef, D$Relocation, D$ApiListA, D$ApiListB,
                D$DllList
    mov D$CookedErrorMessage 0

    If B$ProfilerFlag = &TRUE
        call ReleaseProfilerTables
        mov B$ProfilerFlag &FALSE
    End_If

ret


ReleaseMainFile:
    VirtualFree D$UserPeStart | mov D$CodeSource 0
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Files access:
____________________________________________________________________________________________
____________________________________________________________________________________________

; 'Open File Name' Box:

[uFileFilter: ? #&MAX_PATH] [SaveFilter: ? #&MAX_PATH] [ChoosenFile: ? #&MAX_PATH]

[FilterPtr: 1  OpenFileTitle: 'Choose a file...' 0]

[UserPeStart: ?   UserPeLen: ?   UserPeEnd: ?]

[SourceHandle: 0 SourceLen: 0
 DestinationHandle: 0 DestinationLen: 0
 NumberOfReadBytes: 0
 SourceFile: B$ 'Src.b', 0]  ;;  DestinationFile: 'Dest2.exe', 0, '                       ']

; All the second 0 in lists declarations are for mem size. We need it for 'VirtualFree'

[CodeSource: ? ? CodeSourceA: ? ? CodeSourceB: ? ?]

;;
  Beware: 'CodeSource' is *not* the Memory Chunk Pointer. 'CodeSource' is a Pointer to
  the Source inside 'UserPeStart'. See, for example 'ReleaseMainFile'.
  
  For a direct Allocation/Re-Allocation, it must be, for example:
  > VirtualAlloc UserPeStart 1_000_000 | move D$CodeSource D$UserPeStart
  
  The only case 'CodeSource' is a real Memory Chunk is while disassembling. The switch
  between the two pointer, with the wished release, is done when the Disassembly is 
  finished.
;;

[InstructionAptr: ?  InstructionBptr: ?]

;;
  'MacroList': |NAME|dWord1 dWord2 Byte|
  
  dWord1: Ptr to MacroData // dWord2: Lenght of these Data // Byte: Done Flag
;;

[EquateList: ? ?  MacroData: ? ?  MacroList: ? ?  Relocation: ? ?  DataList: ? ?
 LabelList: ? ?   CodeList: ? ?   Coderef: ? ?    DataRef: ? ?     DllList: ? ?
 ApiListA: ? ?    ApiListB: ? ?]

; 'CodeRef' is: Dword | ... //... | Name | Dword1 dWord2 | // ....       ; 'CodeRefPtr'
;               ....|LabelName|........|LabelName|........|
; Where dWord is the Table Size.
; In the Records, Dword1 is a Pointer to CodeList and dWord2 a Pointer to Source.

; 'LabelList' is: Dword | ... // ...| Name | Dword1 Byte | // ....
; Where dWord is the Table Size.
; In the Records, Dword1 is the Pointer to CodeList, and Byte can be either/or:
; 'CodeLabelFlag', 'DataLabelFlag' , 'DoneFlag'

[DataRefLimit: ?  EquateListLimit: ? MacroListLimit: ? MacroDataLimit: ?
 LabelListLimit: ?]

[EquateListPtr: ?  MacroDataPtr: ?  MacroListPtr: ?  RelocationPtr: ?  DataListPtr: ?
 DataListPtrAtLastColon: ?
 LabelListPtr: ?   CodeListPtr: ?   CoderefPtr: ?    DataRefPtr: ?     DllListPtr: ?
 ApiListAPtr: ?    ApiListBPtr: ?]

[LenOfCode: ?  SourceReady: ?]

[SourceFilterPtr: 1  OpenSourceFileTitle: B$ 'Choose main asm file...' 0
                     OpenPEFileTitle: B$ 'Choose main RosAsm PE file...' 0
                     ChangeNameTitle:    'Change the PE File Name...', 0
                     SaveDlgNameTitle:   'Give the Dialog Template File Name', 0

 SourceFilesFilters: B$ 'Sources'     0  '*.asm'   0
                        'All'         0  '*.*'   0  0]

 ;[PEFilesFilters: B$ 'RosAsm PE'     0  '*.exe;*.scr;*.dll'   0  0]

[PEFilesFilters: B$ 'Files (*.exe, *.dll, *.wll, *.ocx, *.drv, *.bpl, *.cpl, *.fon, *.mpd, *.vbx, *.vxd, *.sys)', 0

'*.exe;*.dll;*.wll;*.ocx;*.drv;*.bpl;*.cpl;*.fon;*.mpd;*.vbx;*.vxd;*.sys', 0
                 B$ 'Executable Files (*.exe)', 0 '*.exe', 0
                 B$ 'Dll Files (*.dll, *.wll)', 0 '*.dll;*.wll', 0
                 B$ 'Delphi Dll Files (*.bpl)', 0 '*.bpl', 0
                 B$ 'Ocx Files (*.ocx)', 0 '*.ocx', 0
                 B$ 'Driver Files (*.drv)', 0 '*.drv', 0
                 B$ 'Cpl Files (*.cpl)', 0 '*.cpl', 0
                 B$ 'fon Files(*.fon)', 0 '*.fon', 0
                 B$ 'mpd Files(*.mpd)', 0 '*.mpd', 0
                 B$ 'vbx Files(*.ocx)', 0 '*.ocx', 0
                 B$ 'vbx Files(*.vbx)', 0 '*.vbx', 0
                 B$ 'vxd Files(*.vxd)', 0 '*.vxd', 0
                 B$ 'sys Files(*.sys)', 0 '*.sys', 0
                 B$ 'All Files', 0  '*.*', 0 0]

[OpenSourceStruc:  len
 hwndFileOwner: 0  OSSInstance: 0  SourceFilesFilters  uFileFilter  260
 1  SaveFilter  260  ChoosenFile  260  0    ; SaveFilter:  full Path/Name.ext
                                            ; ChoosenFile: only Name.ext
 OpenSourceFileTitle  0281804
 0  0  0  0  0]

[OpenPEStruc: len
 hwndPEFileOwner: 0  OPESInstance: 0  PEFilesFilters  uFileFilter  260
 1 SaveFilter  260  ChoosenFile  260  0
 OpenPEFileTitle  OpenPEStrucFlags: 0281804
 0  0  0  0  0]

; &OFN_NOCHANGEDIR
 _________________________________________________________________________________________


[WindowTitle Trash]

[SetWindowText | call SetRosAsmWindowText | push WindowTitle | push D$hwnd |
 call 'USER32.SetWindowTextA']

SetRosAsmWindowText:
    mov esi AppName, edi WindowTitle, ecx 8 | rep movsb
    While D$esi <> ' -V.' | inc esi | End_While
    mov B$edi ' '| inc edi
    While B$esi <> 0 | movsb | End_While
    mov D$edi '    '| add edi 4
    mov esi SaveFilter
    While B$esi <> 0 | movsb | End_While | mov B$edi 0
ret


;;
  CreateFile is called by 'LoadSrc' 'ReplaceSourceOnly' 'OpenRosAsmPE',
                          'DirectMRUload', 'DirectLoad', 'LastMRULoading',
                          'ReloadForDissassembler'
;;

CreateFile:

    If D$SourceHandle > 0
        push esi
            call 'KERNEL32.CloseHandle' D$SourceHandle | and D$SourceHandle 0
        pop esi
    End_If

    call 'KERNEL32.CreateFileA' esi, &GENERIC_READ, &FILE_SHARE_READ,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL


   .If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFileptr | call MessageBox
        mov eax &INVALID_HANDLE_VALUE | ret

   .Else
        mov D$SourceHandle eax

   .End_If

   call 'KERNEL32.GetFileSize'  eax, 0 | mov D$SourceLen eax
ret

 _______________________________

; Opening file:

ClearSaveFilter:
   mov ecx (&MAX_PATH / 4), edi SaveFilter, eax 0 | rep stosd
ret


ClearChoosenFile:
   mov ecx (&MAX_PATH / 4), edi ChoosenFile, eax 0 | rep stosd
ret


LoadSrc:
    call CreateFile
  ; > eax = D$SourceLen
    On eax = &INVALID_HANDLE_VALUE, ret

    add eax 1_000_000

    push eax
        VirtualAlloc UserPeStart eax | move D$CodeSource D$UserPeStart
    pop eax

    add eax D$UserPeStart | Align_On PAGESIZE eax | sub eax 32
    mov D$EndOfSourceMemory eax

    mov edi D$CodeSource
      mov ax 0A0D, ecx 5
        rep stosw
          mov eax edi       ; security for back test like ESI or EDI - 2 (sys page fault)

    mov D$CodeSource eax, D$NumberOfReadBytes 0

    call 'KERNEL32.ReadFile' D$SourceHandle eax,                ; eax = mem buffer start
                            D$SourceLen NumberOfReadBytes 0

    call 'KERNEL32.CloseHandle' D$SourceHandle | mov D$SourceHandle 0

    mov edi D$CodeSource | add edi D$SourceLen | mov eax 0A0D0A0D, ecx 100 | rep stosd
    mov edi D$CodeSource, ecx 5 | sub edi 10 | rep stosw
    mov eax D$CodeSource | add eax D$SourceLen | mov D$SourceEnd eax
  ;  add eax 1_000_000 | mov D$EndOfSourceMemory eax
ret


; In case user wants to Load a Source Only and Resources are actually available, sends
; a warning Message:

[LosingResources: 'Delete actual Resources?', 0
 WarningTitle: ' Warning:', 0]

LooseResources:
    mov B$KeepResources &FALSE, eax 0
    or eax D$CursorList | or eax D$IconList | or eax D$MenuList
    or eax D$DialogList | or eax D$StringsList | or eax D$WaveList
    or eax D$AviList | or eax D$RCdataList | or eax D$BitMapList

    On eax = 0, ret

    mov B$KeepResources &TRUE

    .If D$NoResourcesPE = &FALSE
        call 'USER32.MessageBoxA' D$hwnd, LosingResources, WarningTitle,
                                 &MB_SYSTEMMODAL__&MB_ICONSTOP__&MB_YESNO
        If eax = &IDYES
            mov B$KeepResources &FALSE
        End_If
    .Else
        mov B$KeepResources &FALSE
    .End_If
ret

[LoadSource | mov esi #1 | call LoadSrc]


[MainName: B$ ? #&MAXPATH] [KeepResources: ?]

OpenSourceOnly:
    call ClearSaveFilter | call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

    On D$SaveFilter = 0, ret

L0: On B$SourceReady = &TRUE, call ReleaseMainFile

    VirtualFree D$UserPeStart

    call ClearBackTable | On B$KeepResources = &FALSE, call ReleaseResourceMemory

    LoadSource SaveFilter | StoreNameOnly SaveFilter

    mov B$SourceReady &TRUE | move D$UpperLine D$CodeSource

    call KillTabs | call KillTrailingSpaces

    SetWindowText
    move D$SavingExtension D$ExeExtension

    call StartEdition
ret


[IncludeLen: ?]

IncludeSource:
    call ClearSaveFilter | call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc

    On D$SaveFilter = 0, ret

    call ClearBackTable

    call 'KERNEL32.CreateFileA' SaveFilter,
                                &GENERIC_READ, &FILE_SHARE_READ,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, &NULL
    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox | ret
    Else
        mov D$SourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize' eax, 0 | mov D$IncludeLen eax

    call ReMapSourceMemoryIfNeeded D$IncludeLen

    If eax = &IDNO
        call 'KERNEL32.CloseHandle' D$SourceHandle | mov D$SourceHandle 0 | ret
    End_If

  ; Ensure the inclusion will not break an existing line in two parts:
    mov edi D$CurrentWritingPos
    While B$edi-1 <> LF | inc edi | inc D$CurrentWritingPos |  End_While

  ; Make room inside actual Source by copying backward:
  ; esi at Source End // edi at Source End + whished room:
    mov esi D$SourceEnd | add esi 400
    mov edi esi | add edi D$IncludeLen | add edi 2      ; this '2' is for added CRLF at end.

  ; How many bytes to move = how many bytes between Source End and actual Pos:
    mov ecx D$SourceEnd | add ecx 400 | sub ecx D$CurrentWritingPos | inc ecx

    std | rep movsb | cld

    move D$BlockStartTextPtr D$CurrentWritingPos

    mov D$NumberOfReadBytes 0
    call 'KERNEL32.ReadFile' D$SourceHandle, D$BlockStartTextPtr, D$IncludeLen, NumberOfReadBytes, 0
    call 'KERNEL32.CloseHandle' D$SourceHandle | mov D$SourceHandle 0

    mov edi D$CurrentWritingPos | add edi D$IncludeLen | mov W$edi CRLF
    mov D$BlockEndTextPtr edi | dec D$BlockEndTextPtr

    mov eax D$IncludeLen | add eax 2 | add D$SourceLen eax | add D$SourceEnd eax
    mov B$BlockInside &TRUE

    mov esi D$BlockStartTextPtr, ecx D$BlockEndTextPtr | sub ecx esi
    call BlockCleaner
ret


[OpeningSourceOnly: ?]

ReplaceSourceOnly:  ; 'ControlS'
    mov B$KeepResources &TRUE

    ..If D$TitleTable = 0
L1:     call OpenSourceOnly | call LoadBookMarks

    ..Else
        mov B$OpeningSourceOnly &TRUE
      ; Tag Dialog 26001
        call 'USER32.DialogBoxParamA' D$hinstance, 26001, &NULL, AllOrPartProc, &NULL
        mov B$OpeningSourceOnly &FALSE

        .If B$AllOrPart = 0-1
            call RestoreRealSource | mov D$TiTleTable 0 | jmp L1<

        .Else_If B$AllOrPart = 1   ; 'OpenSourceOnly'
            call ClearSaveFilter | call 'Comdlg32.GetOpenFileNameA' OpenSourceStruc
            If D$SaveFilter <> 0
                mov esi SaveFilter | call CreateFile | On eax = &INVALID_HANDLE_VALUE, ret
                mov eax D$CodeSource | add eax D$SourceLen | mov D$SourceEnd eax
                mov D$NumberOfReadBytes 0
                call 'KERNEL32.ReadFile' D$SourceHandle, D$CodeSource, D$SourceLen,
                                         NumberOfReadBytes 0

                call 'KERNEL32.CloseHandle' D$SourceHandle | mov D$SourceHandle 0

                call KillTabs | call KillTrailingSpaces
            End_If
        .End_If

    ..End_If
ret


; In case of 'New' Internaly called because user attempted to load a 'no source inside'
; PE, we change the Saving File Name to 'NoName.exe':

[NoNameNewFile: 'NoName.exe' 0]
[NewFileBoxTitle: 'New File Name', 0]

NewFileNameDialog:
    call GetDirectory ActualDir

  ; Tag Dialog 5
    call 'USER32.DialogBoxParamA' D$hInstance, 5, D$hwnd, NewFileType, 0

    mov eax D$FileTypeChoice

    .If eax = 0-1
L8:     call SetDirectory ActualDir | jmp L9>>

    .Else
        call GetBaseName
X1:     mov edi D$BaseNamePointer
        mov ebx ActualDir
        While B$ebx <> 0 | inc ebx | End_While
        mov B$ebx '\' | inc ebx
        While B$esi <> 0
            mov al B$esi, B$edi al, B$ebx al
            inc esi, ebx, edi
        End_While
        mov B$edi 0, B$ebx 0
    .End_If

    call SetDirectory ActualDir

    call 'KERNEL32.CopyFileA', BaseFilesPath, ActualDir, &TRUE

    ..If eax = &FALSE
        call 'KERNEL32.GetLastError'

        .If eax = &ERROR_FILE_EXISTS

            call 'USER32.MessageBoxA' &NULL, {'Overwrite?', 0},
                                    {'File already exist', 0}, &MB_YESNO
            If eax = &IDYES
                call 'KERNEL32.CopyFileA', BaseFilesPath, ActualDir, &FALSE

            Else
                jmp L9>>
            End_If

        .Else
            jmp L8<<

        .End_If
    ..End_If

    mov esi ActualDir, edi SaveFilter
    While B$esi <> 0 | movsb | End_While | movsb

    call DirectLoad
    and D$RelocsWanted 0 ; jE!
    call ChangeName
    cmp D$SavingExtension '.DLL' | setz B$RelocsWanted ; jE! Relocs for Dll
    mov B$SourceReady &TRUE
    call AskForRedraw
    call ReInitUndo
    call SetPartialEditionFromPos | call EnableMenutems
    call LoadBookMarks
L9: ret


ChangeName:
    or D$OpenPEStrucFlags &OFN_OVERWRITEPROMPT
    mov D$OpenPEStruc+(12*4) NewFileBoxTitle
    call 'Comdlg32.GetSaveFileNameA' OpenPEStruc
    mov D$OpenPEStruc+(12*4) &NULL
    On eax = &FALSE, ret

    xor D$OpenPEStrucFlags &OFN_OVERWRITEPROMPT | jmp L1>
AutoNew:
    mov edi SaveFilter, eax 0, ecx 65 | rep stosd


    mov edi SaveFilter

NewEmptyFromMenu:
    mov esi NoNameNewFile
    While B$esi > 0
        movsb
    End_While
    movsb

L1: call SaveNewFileName
    On eax = &FALSE, jmp NewFileNameDialog
    push eax
        SetWindowText
    pop eax
  ; (If here, eax = D$SourceLen)
    xor D$OpenPEStrucFlags &OFN_OVERWRITEPROMPT
    mov eax &TRUE
ret
____________________________________________________________________________________________

;;
  ID in RcData (Files in E:\RosAsm\Start\):
  
  ID 1: Menu
    10: Low/LowStart/Menu      'LowStartMenu.exe'
    11: Low/LowStart           'LowStart.exe'
    20: StandardMacros
    21:     Standard/StandardStart/Menu
    22:     Standard/StandartStart/
    40: HllMacros <<<<<<<<<<<<<<<<<<<< Removed: Now Standard + PREPARSE Equal.
    41:     HLL/HllStart/Menu
    42:     HLL/HllStart
    50: Dll/Standard or HLL
    51: DLL/Low
;;

[PreParseEqual: "PREPARSE Equal


", 0]

FillNewSource:
    mov edi D$CodeSource, bl B$FileTypeChoice+1
    If bl = 20
      ; Standart macros:
        call GetRcData 20 | rep movsb
    Else_If bl = 21
      ; HLL Macros:
        call GetRcData 20 | rep movsb
        mov esi PreParseEqual
        While B$esi <> 0 | movsb | End_While
    End_If

L1: mov al B$FileTypeChoice

      ; exe/Start/Menu:
        .If al = 10
          ; Menu:
            call GetRcData 1
            push esi, edi, ecx, ebx
                VirtualAlloc Trash, ecx
            pop ebx, ecx, edi, esi
            mov D$MenuList 2000, D$MenuList+4 eax, D$MenuList+8 ecx
            lea eax D$MenuList+12 | mov D$MenuListPtr eax

            push edi
                mov edi D$Trash | rep movsb
            pop edi

            If bl = 20
              ; Standard/StandardStart/Menu
                call GetRcData 21 | rep movsb
            Else_If bl = 21
              ; HLL/HllStart/Menu
                call GetRcData 41 | rep movsb
            Else
              ; Low/LowStart/Menu      'LowStartMenu.exe'
                call GetRcData 10 | rep movsb
            End_If

      ; exe/Start:
        .Else_If al = 11
            If bl = 20
              ; Standard/StandardStart
                call GetRcData 22 | rep movsb
            Else_If bl = 21
              ; HLL/HllStart
                call GetRcData 42 | rep movsb
            Else
              ; exe/LowStart
                call GetRcData 11 | rep movsb
            End_If

      ; exe (empty Source):
        .Else_If al = 12
            On bl = 22, jmp L9>>

      ; dll/StandartStart:
        .Else_If al = 13
            If bl = 22
                call GetRcData 51
            Else
                call GetRcData 50
            End_If
            rep movsb

      ; dll (empty Source)
        .Else_If al = 14
            ;
        .End_If

L1: mov D$edi CRLF2 | add edi 4 | mov D$SourceEnd edi
    mov ecx edi | sub ecx D$CodeSource | mov D$SourceLen ecx

    mov eax CRLF2, ecx 100 | rep stosd

L9: call GetEditWindowClientRectangle
    mov D$TitleTable 0, D$PreviousPartialSourceLen 0

    mov B$OnReplaceAll &FALSE, B$BlocKInside &FALSE, B$DownSearch &TRUE, B$ReadyToRun &FALSE
    call StorePosInBackTable

L9: ret


Proc GetRcData:
    Argument @ID
    Local @Handle
    Uses edi

        call 'KERNEL32.FindResourceA' D$hInstance, D@ID, &RT_RCDATA
        mov D@Handle eax
        call 'KERNEL32.SizeofResource' D$hInstance, D@Handle
        push eax
            call 'KERNEL32.LoadResource' D$hInstance, D@Handle
            call 'KERNEL32.LockResource' eax
            mov esi eax
        pop ecx
EndP
____________________________________________________________________________________________

____________________________________________________________________________________________

[FileTypeChoice: ?  NewChoiceListHandle: ?]

; Tag Dialog 5

Proc NewFileType:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        ..If D@wParam = &IDCANCEL
L0:         mov D$FileTypeChoice 0-1
L1:         call 'User32.EndDialog' D@Adressee 0

        ..Else
            .If W@wParam = 10
                If W@wParam+2 = &LBN_DBLCLK
                    call 'USER32.SendMessageA' D$NewChoiceListHandle, &LB_GETCURSEL, 0, 0
                    inc eax | mov D$FileTypeChoice eax | jmp L1<

                End_If

            .End_If

        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

        call 'USER32.GetDlgItem' D@Adressee, 10 | mov D$NewChoiceListHandle eax

        call SetBaseList

    ...Else
        popad | mov eax &FALSE | ExitP

    ...End_If

    popad | mov eax &TRUE
EndP


Proc SetNewChoice:
    Arguments @ListHandle, @Pointer

        pushad
            call 'USER32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, D@Pointer
        popad
EndP


[BaseFilesPath: ? #&MAXPATH]

[BaseNamePointer: 0

 BasesNames:
 Base_01.exe: B$ 'Base_01.exe', 0
 Base_02.exe: 'Base_02.exe', 0
 Base_10.exe: 'Base_10.exe', 0
 Base_11.exe: 'Base_11.exe', 0

 Base_03.dll: 'Base_03.dll', 0
 Base_04.dll: 'Base_04.dll', 0
 Base_05.dll: 'Base_05.dll', 0, 0]

[MissBaseTitle: 'Missing Bases Files', 0

 MissBase: "
 Download the Bases Files from RosAsm.org
 and save them in a folder called 'Bases',
 inside your 'RosAsmFiles' Folder.
 
 If you already have these Files at the
 proper Path, update them with the new
 Version.
 ", 0]

[BasesListHandle: ?  BaseListEnd: ?]

SetBaseList:
    ;call SetNewChoice eax, NewEmpty

    call GetRosAsmFilesPath

    mov esi RosAsmFilesPath, edi BaseFilesPath
    While B$esi <> 0 | movsb | End_While
    mov D$edi 'Base', D$edi+4 's\'
    add edi 6 | mov D$BaseNamePointer edi

    zCopy {'BasesList.txt', 0} | mov B$edi 0

    call 'KERNEL32.CreateFileA' BaseFilesPath, &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0

    .If eax = &INVALID_HANDLE_VALUE
        mov eax 0-1 | ret

    .Else
        mov D$BasesListHandle eax
        call 'KERNEL32.GetFileSize' eax, 0
        mov D$BaseListEnd Trash | add D$BaseListEnd eax
        call 'KERNEL32.ReadFile' D$BasesListHandle, Trash, eax, NumberOfReadBytes, 0
        call 'KERNEL32.CloseHandle' D$BasesListHandle

        mov esi Trash
        .While esi < D$BaseListEnd
            While B$esi <> ' ' | inc esi | End_While
            mov edi TrashString
            While B$esi <> CR | movsb | End_While | mov B$edi 0 | add esi 2

            call SetNewChoice D$NewChoiceListHandle, TrashString
        .End_While

    .End_If
ret


GetBaseName:
  ; eax = Indice (Indice zero assumed as 'NewEmpty' by 'SetBaseList').
    mov esi Trash, ecx 1

  ; Skip lines until indice matches:
    .While ecx < eax
        While B$esi <> CR | inc esi | End_While | add esi 2
        inc ecx
    .End_While

  ; Set a zero at the end of the File Name:
    push esi
        While B$esi <> ' ' | inc esi | End_While
        mov D$esi 0
    pop esi
ret


VerifyBasesFolder:
    call GetRosAsmFilesPath

    mov esi RosAsmFilesPath, edi BaseFilesPath
    While B$esi <> 0 | movsb | End_While
    mov D$edi 'Base', D$edi+4 's\'
    add edi 6 | mov D$BaseNamePointer edi
    mov esi BasesNames

    .While B$esi <> 0
        push edi
            While B$esi <> 0 | movsb | End_While | movsb
            push esi
                call 'KERNEL32.FindFirstFileA' BaseFilesPath, FindFile
                push eax
                    call 'KERNEL32.FindClose' eax
                pop eax
                If eax = &INVALID_HANDLE_VALUE
                    pop eax, eax
                    call 'USER32.MessageBoxA' &NULL, MissBase, MissBaseTitle, &MB_SYSTEMMODAL
                    mov eax &FALSE | ret
                End_If
            pop esi
        pop edi
    .End_While

    mov eax &TRUE
ret

____________________________________________________________________________________________

[BadNewExtension: 'Bad Extension', 0]

SaveNewFileName:
    mov esi SaveFilter, edi MainName

L0: lodsb | stosb | cmp B$esi '.' | je L8>
    cmp B$esi 0 | jne L0<

L8: On B$esi+4 <> 0, jmp L0<

    mov B$edi 0

  ; Check extension:
    lodsd | or eax 020202000
    If eax = '.exe'
        jmp L9>
    Else_If eax = '.dll'
        jmp L9>
    Else_If eax = '.sys'
        jmp L9>
    Else_If eax = '.scr'
        jmp L9>
    End_If

    If al = '.'
        ; OK
    Else_If al = 0
        ; OK
    Else
        mov eax BadNewExtension | call MessageBox | mov eax &FALSE | ret
    End_If

    sub esi 4 | mov edi esi
    mov eax '.exe' | stosd | mov B$edi 0   ; Complete user given name (any use?...)

L9: or eax 020202000 | xor eax 020202000 | mov D$SavingExtension eax | mov eax &TRUE
ret


StartNewFile:
    call ReInitUndo
    call ReInitHeaderFormat
    On B$SourceReady = &TRUE, call ReleaseMainFile

    call ClearBackTable | call ReleaseResourceMemory

    mov D$SourceLen 0

  ;  VirtualAlloc CodeSource 1_000_000
    VirtualAlloc UserPeStart 1_000_000 | move D$CodeSource D$UserPeStart

    add D$CodeSource 10     ; security for back test like ESI or EDI - 2 (no page fault)

    mov edi D$CodeSource
    mov eax 0A0D0A0D, ecx 100 | rep stosd                            ; end security tail
    mov edi D$CodeSource, ecx 5 | sub edi 10 | rep stosw
    mov eax D$CodeSource
    mov D$SourceEnd eax, D$CurrentWritingPos eax, D$CaretLine 0, D$CaretRow 1
    add eax 1_000_000 | mov D$EndOfSourceMemory eax

    mov B$SourceReady &TRUE | move D$UpperLine D$CodeSource

    mov D$TitleTable 0, D$PreviousPartialSourceLen 0

  ; Clear possible previous resources:
    call ClearCustomList

    call AskForRedraw
ret


[UserPEStartOfResources: ?    ResourcesSize: ?]

SearchPEstartOfResource:
    mov B$NoResourcesPE &FALSE
    mov esi D$UserPeStart, D$UserPEStartOfResources 0
    movzx eax W$esi+8
   ; mov eax 0 | add esi 8 | lodsw    ; parag. size of dos header end >PE header adress
    shl eax 4 | sub eax 4
    mov esi D$UserPeStart | add esi eax | lodsd      ; eax = PE header

    mov esi D$UserPeStart | add esi eax
    cmp D$esi 'PE' | jne L9>
 ______________________________________

; read data in PeHeader:

L0: mov ecx 0 | mov cx w$esi+6            ; word record of section number
    add esi 136 | lodsd | mov ebx eax     ; RVA of resources from "Image Data Dir..."
    mov D$ResourcesRVA eax
    move D$ResourcesSize D$esi

                     ; jmp over general purpose headers and reach PE sections headers:
    add esi 120                           ; esi points to RVA of first section header
    If eax = 0
        mov B$NoResourcesPE &TRUE | ret
    End_If

L0: lodsd | cmp eax ebx | je L1>
        add esi 36 | loop L0<
          jmp L9>
 ______________________________________

  ; if here, '.rsrc' section found:

L1:
    add esi 4 | lodsd                            ; > app ptr to resources
    add eax D$UserPeStart | mov D$UserPEStartOfResources eax
L9: ret


[EndOfSectionSearch: ?]

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 This is a little 'unregular' CheckSum stored in an 'unregular' place. It does *not*
 prevent final user from viruses. It is designed to protect RosAsm programmers who
 exchange files from dispatching corrupted files.

 The base idea is that we can hope that nobody will ever write any virus specifically
 designed to target RosAsm produced PEs. Not at all sophisticated, and very easy to
 overcome, but who would?

 As is, if infected, when you [Load] the file in RosAsm, you get a MessageBox to alert
 you. Nothing more, but i think that it is a very good solution to allow Asm communauty
 to exchange files without fearing.
;;
[CheckMessageTitle: 'Bad CheckSum', 0
 CheckInfo: "
 This File has been modified, either
 by an external Tool, or by a Virus.     ", 0]

ReadCheckSum:
    pushad
      ; If a real PE CheckSum has been written, it must be zeroed, before calculation
      ; because it was stored after 'MyCheckSum':
        mov esi D$UserPeStart | add esi CheckSum | sub esi DosHeader
        mov D$esi 0

      ; 'MyCheckSum' must not be considered:
        mov esi D$UserPeStart | add esi MyCheckSum | sub esi DosHeader
        mov edx D$esi, D$esi 0, ebx 0, eax 0

        .If edx > 0
            mov esi D$UserPeStart, ecx D$UserPeLen

L0:         lodsb | add ebx eax | loop L0<

            If edx <> ebx
                call 'USER32.MessageBoxA' D$hwnd, CheckInfo, CheckMessageTitle,
                                           &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION
            End_If
        .End_If
    popad
ret


WriteCheckSum:
    mov eax 0, ebx 0, esi D$CodeList, ecx D$LenOfCode
L0: lodsb | add ebx eax | loop L0<

    If B$ExportsectionWanted = &TRUE
        mov esi D$ExportListBPtr, ecx D$ExportSectionLen
L0:     lodsb | add ebx eax | loop L0<
    End_If

    .If D$SavingExtension = '.SYS'
        jmp L0>
    .Else_If D$RelocsWanted = &TRUE ;SavingExtension = '.DLL' ; jE!
L0:     If D$RelocSectionSize => 8 ;jE!
            mov esi D$Relocation, ecx D$RelocSectionSize
L0:         lodsb | add ebx eax | loop L0<
        End_If
    .End_If

    mov esi D$CodeSource, ecx D$SourceLen
L0: lodsb | add ebx eax | loop L0<

    mov edi D$CodeList | add edi MyCheckSum | sub edi DosHeader
    mov D$edi ebx
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Regular PE CheckSum (original Source for csum.exe, from 'RudeBoy'):
  
  Will be needed for Drivers.


    mov edx 0, esi D$FilePointer, ecx D$FileLen | shr ecx 1

L0: lodsw
    add edx eax
    mov eax edx
    and edx 0ffff 
    shr eax 010
    add edx eax
    loop L0<

    mov eax edx
    shr eax 010
    add ax dx
    add eax dwFileLen
  
____________________________________________________________________________________________

Another one:

    mov ecx, D$Length, esi D$BufferPointer, eax 0
    shr ecx, 1              

    clc                                 
L0: adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
    
    adc eax D$Length
    
    
    
    
    MicrosoftCheckSum2              PROC C uses esi, buf:dword, len:dword
        mov             ecx, [len]           
        mov             edx, [buf]           
        shr             ecx, 1               
        xor             eax, eax             

        clc                                  
@@theLoop:
        adc ax, [edx + (ecx * 2) - 2]
        dec ecx
        jnz @@theLoop
        adc  eax, [len]
        ret
;;

WriteSysFile:
    call SetExeSavingName

    call OpenDestinationFile

    mov D$NumberOfReadBytes 0

    call AraseSourceHeader | call AraseCheckSum

    call WritePeCheckSum

    call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeList, D$LenOfCode,
                              NumberOfReadBytes, 0

    If B$ExportsectionWanted = &TRUE
        call 'KERNEL32.WriteFile' D$DestinationHandle, D$ExportListBPtr,
                                  D$FileAlignedExportSectionLen, NumberOfReadBytes, 0
    End_If

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:
        If D$RelocSectionSize => 8 ;jE!
          call 'KERNEL32.WriteFile' D$DestinationHandle,
                                   D$Relocation, D$FileAlignedRelocationSize,
                                   NumberOfReadBytes, 0
        End_If
    End_If

  ;  call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeSource, D$SourceLen,
  ;                            NumberOfReadBytes, 0

    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
ret


AraseSourceHeader:
    mov edi D$LocOfSourceHeader
    mov ecx 20, eax 0 | rep stosd
ret


AraseCheckSum:
    mov edi D$CodeList | add edi MyCheckSum | sub edi DosHeader
    mov D$edi 0
ret


WritePeCheckSum:
    mov esi D$CodeList, ecx D$LenOfCode, eax 0, ebx ecx
    shr ecx, 1

    clc
L0: adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
    adc ax 0

    If B$ExportsectionWanted = &TRUE
        mov esi D$ExportListBPtr, ecx D$FileAlignedExportSectionLen
        add ebx ecx
        shr ecx 1
        clc
L0:     adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
        adc ax 0
    End_If

    If D$RelocSectionSize => 8 ;jE!
        mov esi D$Relocation, ecx D$FileAlignedRelocationSize
        add ebx ecx
        shr ecx 1
        clc
L0:     adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
       ; adc ax 0
    End_If
;;
    mov esi D$CodeSource, ecx D$SourceLen
        add ebx ecx
        shr ecx 1
        clc
L0:     adc ax W$esi+ecx*2-2 | dec ecx | jnz L0<
;;
    adc eax ebx | call WriteEaxToPeCheckSum
ret


WriteEaxToPeCheckSum:
    mov edi D$CodeList | sub edi DosHeader | add edi CheckSum
    mov D$edi eax
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[BookMarksFileHandle: ?    BookMarksFileLen: ?]

[FindFile:
 FindFile.dwFileAttributes: D$ ?
 FindFile.ftCreationTime.dwLowDateTime: D$ ?
 FindFile.ftCreationTime.dwHighDateTime: D$ ?
 FindFile.ftLastAccessTime.dwLowDateTime: D$ ?
 FindFile.ftLastAccessTime.dwHighDateTime: D$ ?
 FindFile.ftLastWriteTime.dwLowDateTime: D$ ?
 FindFile.ftLastWriteTime.dwHighDateTime: D$ ?
 FindFile.nFileSizeHigh: D$ ?
 FindFile.nFileSizeLow: D$ ?
 FindFile.dwReserved0: D$ ?
 FindFile.dwReserved1: D$ ?]
[FindFile.cFileName: B$ ? #&MAXPATH]
[FindFile.cAlternate: B$ ? #14]

LoadBookMarks:
    mov D$NumberOfBookMarks 0
    mov edi SaveFilter, al 0, ecx 0-1 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While
    push D$edi, edi

        mov D$edi '.BKM'

        call 'KERNEL32.FindFirstFileA' SaveFilter FindFile
        .If eax = &INVALID_HANDLE_VALUE
            VirtualFree D$BookMarks | pop edi, D$edi | ret
        .Else
            call 'KERNEL32.FindClose' eax
        .End_If

        call 'KERNEL32.CreateFileA' SaveFilter &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
   pop edi, D$edi

   .If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox
        pop eax | ret                       ; pop return adress of caller and ret to Callback
   .Else
        mov D$BookMarksFileHandle eax
   .End_If

    call 'KERNEL32.GetFileSize'  eax 0 | mov D$BookMarksFileLen eax

    VirtualFree D$BookMarks | VirtualAlloc BookMarks 01000

    call 'KERNEL32.ReadFile' D$BookMarksFileHandle D$BookMarks D$BookMarksFileLen,
                            NumberOfReadBytes 0
    call CreateTreeViewList | call SetTreeDialogPos

    call 'KERNEL32.CloseHandle' D$BookMarksFileHandle
ret



[EndOfSourceMemory: ?    OldSourceReady: ?]

OpenRosAsmPE:
    move D$OldSourceReady D$SourceReady | mov B$SourceReady &FALSE

    call ClearSaveFilter | call ClearChoosenFile

    push edi
        mov edi OpenPEStruc
        mov eax 04C | stosd
        mov eax D$hwnd | stosd
        mov eax 0 | stosd
        mov eax PEFilesFilters | stosd
        mov eax uFileFilter | stosd
        mov eax 260 | stosd
        mov eax 1 | stosd
        mov eax SaveFilter | stosd
        mov eax 260 | stosd
        mov eax ChoosenFile | stosd
        mov eax 260 | stosd
        mov eax 0 | stosd
        mov eax OpenPEFileTitle | stosd
        mov eax 0281804 | stosd
        mov eax 0 | stosd
        mov eax 0 | stosd
        mov eax 0 | stosd
        mov eax 0 | stosd
        mov eax 0 | stosd
    pop edi

    call 'Comdlg32.GetOpenFileNameA' OpenPEStruc

    If D$SaveFilter = 0
        move D$SourceReady D$OldSourceReady | ret
    End_If

DirectMRUload:
DirectLoad:
    call ReleaseMainFile | StoreNameOnly SaveFilter
    call ClearBackTable | call ReleaseResourceMemory

    VirtualFree D$UserPeStart

LastMRULoading:
ReloadForDissassembler:
    mov esi MainName
    While B$esi <> 0 | inc esi | EndWhile
    While B$esi <> '\' | dec esi | EndWhile
    push D$esi
        mov B$esi+1 0 | call 'Kernel32.SetCurrentDirectoryA' MainName
    pop D$esi

    mov esi SaveFilter | call CreateFile | On eax = &INVALID_HANDLE_VALUE, ret
    Call GetFileNameFromPath
  ; > eax = source len
;;
  The reason for allocating a twice (>>> shl eax 1) bigger Mem:
  
  When having TITLEs inside, the editor make a Copy of the edited TITLE, at the bottom
  of this Mem. Therefore, if the Source is made of two TITLEs, a very short one, say
  for Macros, and a very big one, for everything else, we need twice more Mem. Plus the
  additional Edition security, of course:
;;
    mov D$UserPeEnd eax, D$UserPeLen eax | shl eax 1 | add eax 100_000

    push eax
        VirtualAlloc UserPeStart eax

        add D$UserPeEnd eax
    pop eax

    Align_On PAGESIZE eax | sub eax 32 | add eax D$UserPeStart
    mov D$EndOfSourceMemory eax

    mov D$NumberOfReadBytes 0

    call 'KERNEL32.ReadFile' D$SourceHandle, D$UserPeStart, D$UserPeLen, NumberOfReadBytes, 0

    call 'KERNEL32.CloseHandle' D$SourceHandle | mov D$SourceHandle 0

    mov eax D$UserPeStart | On W$eax <> 'MZ', jmp ExitNotPeExe

    mov eax D$UserPeStart | sub eax DosHeader | add eax PeHeaderPointer
    mov ebx D$eax, eax D$UserPeStart | add eax ebx
    On eax >= D$UserPeEnd, jmp ExitNotPeExe
    On D$eax <> 'PE', jmp ExitNotPeExe
    cmp D$eax+0A0 0 | setne B$RelocsWanted; jE! RelocsWanted
    mov B$ThisSourceIsDisassembled &FALSE

    mov eax D$UserPeStart | add eax 0178 | mov esi eax     ; +0178 > first section header
    add eax 400 | mov D$EndOfSectionSearch eax             ; +(10*40) bytes per section

L0: lodsd | cmp eax '.src' | je L1>
        add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
            jmp TryDisassembly

ExitNotPeExe:
    mov eax D$NotPeExePtr | call MessageBox | call AutoNew | jmp StartNewFile


L1: lodsd | lodsd | mov D$SourceLen eax
    lodsd | lodsd | lodsd
  ; eax = 0 based pointer to source

  ; 'MessageBox' messages could send a WM_PAINT before complete initializations:
    mov D$TiTleTable 0, D$ActualTitle 0

    call ReadCheckSum | call ReadHeaderFormat

    add eax D$UserPeStart | mov D$CodeSource eax

    mov edi D$CodeSource | add edi D$SourceLen | mov eax 0A0D0A0D, ecx 100

    rep stosd

    mov eax D$CodeSource | add eax D$SourceLen | mov D$SourceEnd eax
   ; add eax 1_000_000 | mov D$EndOfSourceMemory eax
    move D$UpperLine D$CodeSource | mov B$SourceReady &TRUE

    call KillTabs | call KillTrailingSpaces

    call SearchPEstartOfResource | On B$NoResourcesPE = &TRUE, jmp L9>>

    ; Upload icon:

; Icon storing more stand-alone because of feature for reading icon in other PEs. So does
; it again job done by 'SearchPEstartOfResource'. So, i make it set 'ResourcesRVA' value
; (very durty, but...).

    move D$iExePtr D$UserPeStart | call ReadRosAsmPeIcon ; load icon with icon editor routine

    If B$PeIconFound = &TRUE
        mov esi eax | mov edi iIcon | rep movsb         ; Copying to icon editor buffer
         call StoreIcon                                  ; and copy it to 'compilable' image
    End_If

   ; call ReadRosAsmPeMenus

   ; call ReadRosAsmPeDialogs

  ;  call ReadRosAsmBitMaps

  ;  call ReadRosAsmStrings

   ; call ReadRosAsmWaves

   ; call ReadRosAsmAvis

   ; call ReadRosAsmRCs

   ; call ReadRosAsmCursors | call ReadRosAsmGroupCursors

   ; call ReadRosAsmIcons | call ReadRosAsmGroupIcons

    call ReadRosAsmResources

L9: mov edi D$CodeSource, ecx 5, eax 0A0D | sub edi 10 | rep stosw ; security for back search
    mov D$DestinationFile 0

    SetWindowText | call StartEdition
ret

; For 'FillExportSection':

GetFileNameFromPath:
    push eax, esi, edi
        mov esi SaveFilter, eax esi
        mov edi ChoosenFile

        While B$esi <> 0 | inc esi | End_while

        While esi > eax
            dec esi
            On B$esi = '\', jmp L1>
        End_While

L1:     inc esi

        While B$esi <> 0
            movsb
        End_While
        mov B$edi 0
    pop edi, esi, eax
ret


[DisChoice: ?]

TryDisassembly:
    On D$AddressToBeForced = &TRUE, jmp DisMain

  ; Tag Dialog 27000
    call 'USER32.DialogBoxParamA' D$hInstance, 27000, D$hwnd, DisassembleProc, 0
    Test D$DisChoice 0_8000_0000 | jz L9>
        and D$DisChoice 0FF | jmp Dismain

L9:     call AutoNew | jmp StartNewFile

____________________________________________________________________________________________

[WithCommentedHexa: &FALSE   WithForcedMapFile: &TRUE  WithMacros: &TRUE]

; Tag Dialog 27000

Proc DisassembleProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND                  ; User action
        ..If D@wParam = &IDCANCEL                   ; User clicks on upper right [X]
            and D$DisChoice 0FF
            call 'User32.EndDialog' D@Adressee 0
          ; call 'User32.DestroyWindow' D@Adressee ; Comments...

        ..Else_If D@wParam = &IDOK
            or D$DisChoice 0_8000_0000
            call 'User32.EndDialog' D@Adressee 0

        ..Else_If D@wParam = &IDHELP
            call Help B_U_AsmName, DisassemblerHelp, ContextHlpMessage

        ..Else
            movzx eax W@wParam
            If eax = 10
                mov B$WithCommentedHexa &FALSE
            Else_If eax = 11
                mov B$WithCommentedHexa &TRUE
            Else_If eax = 12
                mov B$WithCommentedHexa &FALSE
            Else_If eax = 14
                xor B$WithForcedMapFile &TRUE
            Else_If eax = 15
                xor B$WithMacros &TRUE
            Else_If eax = 16
                xor B$AMDassumed &TRUE
            End_If

        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG          ; Win ready to build the Dialog
        If B$WithCommentedHexa = &TRUE
            call 'USER32.CheckDlgButton' D@Adressee, 11, &TRUE
        Else_If B$WithMacros = &TRUE
            call 'USER32.CheckDlgButton' D@Adressee, 15, &TRUE
        Else
            call 'USER32.CheckDlgButton' D@Adressee, 10, &TRUE
        End_If

        call IsForcedMapFile
        .If eax = &TRUE
            call Enable D@Adressee, 14
            If B$WithForcedMapFile = &TRUE
                call 'USER32.CheckDlgButton' D@Adressee, 14, &TRUE
            End_If
        .Else
            call Disable D@Adressee, 14
        .End_If

        If B$AMDassumed = &TRUE
            call 'USER32.CheckDlgButton' D@Adressee, 16, &TRUE
        End_If


  ;  ...Else_If D$Message = &WM_CTLCOLOREDIT        ; Win ready to paint the Dialog
  ;      ; Control of output

    ...Else
        popad | mov eax &FALSE | ExitP               ; Non processed

    ...End_If

    popad | mov eax &TRUE                           ; Processed
EndP
____________________________________________________________________________________________

; >>> peheader <<<

ReadHeaderFormat:
    pushad
        mov eax D$UserPeStart | add eax 0178 | mov esi eax     ; +0178 > first section header
        add eax 400 | mov D$EndOfSectionSearch eax             ; +(10*40) bytes per section

      ; Data Characteristics:
        push esi
L0:         lodsd | cmp eax '.dat' | je L1>
                add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
                jmp L0>
L1:         add esi 32
            lodsd | mov D$DataCharacteristics eax
L0:     pop esi

      ; Code Characteristics:
L0:     lodsd | cmp eax '.tex' | je L1>
            add esi 36 | cmp esi D$EndOfSectionSearch | jb L0<
            mov eax D$NotPEPtr | call MessageBox | ret
L1:     add esi 32
        lodsd | mov D$CodeCharacteristics eax

      ; SubSystem, DLL_characteristcs:
        mov esi PeHeader | sub esi DosHeader
        add esi D$UserPeStart                   ; >>> 'PE' 0 0
        add esi 52                              ; >>>> ImageBase

        mov eax D$esi, D$ImageBase eax, D$LinkerDllDefault eax

        mov edi SubSystem | add esi 40          ; >>> SubSystem // DLL characteristcs
        mov ax W$esi, W$DllAttachDetach ax

        movsd
        movsd   ; AppStackMax
        movsd   ; AppStackMin
        movsd   ; AppHeapMax
        movsd   ; AppHeapMin
    popad
ret


ReInitHeaderFormat:
    push edi
        mov edi SubSystem
        mov eax 2 | stosd                       ; Subsystem GUI  //  DLL characteristics 0
        mov eax 0100000 | stosd                 ; AppStackMax
        mov eax   01000 | stosd                 ; AppStackMin
        mov eax 0100000 | stosd                 ; AppHeapMax
        mov eax       0 | stosd                 ; AppHeapMin
        mov D$CodeCharacteristics 0_60000020    ; readable, runable, code
        mov D$DataCharacteristics 0_C0000040    ; readable, writeble, Data
        mov W$DllCharacteristics 0
    pop edi
ret
 _______________________________

; Writing files:

[DestinationFile: ? #262]

OpenDestinationFile:
    On D$DestinationHandle > 0, call 'KERNEL32.CloseHandle' D$DestinationHandle

    call 'KERNEL32.CreateFileA' DestinationFile, &GENERIC_WRITE,
                                &FILE_SHARE_READ, 0,
                                &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox
        mov B$CompileErrorHappend &TRUE
      ; pop return adress of caller and ret to Callback:
        pop eax | ret

    Else
        mov D$DestinationHandle eax

    End_If
ret

____________________________________________________________________________________________
;;
 This is for developpements test:

 DANGER >>> Writes a 'Test.a' on the disk without warning.
 
 'WriteDestination' is used only by 'AsmMain' when you branch to it. All these commented
 lines are for tests along developpement giving ability, by calling it, to output
 internal tables for exemple, if we want to see what is in 'DllList', we route here with
 a 'jmp L7>' after 'BuildImport'; with "mov eax, 010000 | push eax" and "push D$DllList"
 uncommented.
;;
WriteDestination:
    call OpenDestinationFile

   ; hexprint D$StripLen
   ; showme DestinationFile

    push 0
    mov D$NumberOfReadBytes 0 | push NumberOfReadBytes

      mov eax, 0100

 ;;;; Just uncomment the one you want in 'Test.a':

    ; push D$LenOfCode, D$CodeSource
    ; push D$StripLen, D$CodeSourceB
     push D$StripLen, D$CodeSourceA
    ; push eax, D$EquateList
    ; push eax, D$MacroList
    ; push eax, D$PlainLabelList
    ; push eax, D$MacroData
    ; push eax, D$DataList
    ; push eax, D$LabelList
    ; push D$StripLen, D$Coderef
    ; push eax, D$DataRef
    ; push eax, D$DllList
    ; push eax, D$ApiListA
    ; push eax, D$ApiListB
    ; push eax, D$Relocation
    ; push eax, D$TreeList
    ; push eax, D$CodeList
    ; push eax, D$ExportListAPtr
;;
    mov esi D$StatementsTable | While D$esi > 0 | add esi 4 | End_While
    sub esi D$StatementsTable
    push esi, D$StatementsTable
;;

   ; mov eax D$EndOfSectionsMap | sub eax D$SectionsMap | push eax
;
 ;   push D$SectionsMap

    push D$DestinationHandle
    call 'KERNEL32.WriteFile'
ret


ControlS:
    ..If D$TitleTable = 0
        call SaveSource
        call 'USER32.MessageBoxA' D$hwnd, DestinationFile,
                                {'Saved in actual Directory:', 0}, 0

    ..Else
        call 'USER32.DialogBoxParamA' D$hinstance, 26000, &NULL, AllOrPartProc, &NULL

        .If B$AllOrPart = 0-1
            call RestoreRealSource | call SaveSource
            call SetPartialEditionFromPos

        .Else_If B$AllOrPart = 1
            mov B$WeAreSavingPart &TRUE
            call SaveSource
            mov B$WeAreSavingPart &FALSE

        .Else_If B$AllOrPart = 9
            call 'USER32.MessageBoxA' D$hwnd, {"     Do you wish to save all the TITLEs
of this Application into as many asm Files?      ", 0}, {'Sure?', 0}, &MB_YESNO
            If eax = &IDYES
                mov B$WeAreSavingPart &TRUE
                call SaveActualPos
                call GetTitlesNumber

L0:             push ecx
                    call SelectTitle ecx | call SaveSource
                pop ecx
                loop L0<

                call 'USER32.MessageBoxA' D$hwnd, {'Done', 0}, 0, 0
                call RestoreActualPos
                mov B$WeAreSavingPart &FALSE
            End_If

        .End_If

    ..End_If
ret


[ActualPos: ?]

SaveActualPos:
    move D$ActualPos D$CurrentWritingPos
ret


Proc SelectTitle:
    Argument @Indice

        call RestoreRealSource
        mov eax D@Indice
        dec eax
        mov D$ActualPartIndex eax
        move D$CurrentWritingPos D$TitleTable+eax*4
        call SetPartialEdition

        mov esi D$CodeSource, edi PartName

        If D$esi = 'TITL'
            add esi 5 | While B$esi = ' ' | inc esi | End_While
            While B$esi > ' ' | movsb | End_While | mov B$edi 0
        Else
            mov D$PartName 'Top'
        End_If
EndP


GetTitlesNumber:
    mov esi TitleTable, ecx 0
    While D$esi <> 0
        inc ecx | add esi 4
    End_While
ret


RestoreActualPos:
    move D$CurrentWritingPos D$ActualPos
ret


[BackUpPathName: B$ ? #&MAXPATH][BackUpNewPathName: B$ ? #&MAXPATH]

[WIN32_FIND_DATA:
 dwFileAttributes: D$ 0
 ftCreationTime.dwLowDateTime: D$ 0
 ftCreationTime.dwHighDateTime: D$ 0
 ftLastAccessTime.dwLowDateTime: D$ 0
 ftLastAccessTime.dwHighDateTime: D$ 0
 ftLastWriteTime.dwLowDateTime: D$ 0
 ftLastWriteTime.dwHighDateTime: D$ 0
 nFileSizeHigh: D$ 0
 nFileSizeLow: D$ 0
 dwReserved0: D$ 0
 dwReserved1: D$ 0]
[cFileName: B$ 0 #&MAX_PATH]
[cAlternate: B$ 0 #80]

[BackUpFileHandle: ?    LastBackUpIndice: ?]    [MaxBackUp: B$ '0010' 0]

ControlK:
    On D$MaxBackUp = '0000', ret

    If B$ReadyToRun = &FALSE
        call 'USER32.MessageBoxA' D$hwnd,
                                 {'You have to [Compile] or to [Run] before a PE BackUp', 0},
                                 {'[Ctrl][K] BackUp aborted:', 0}, 0
        ret
    End_If

    mov esi MainName, edi BackUpPathName

    While B$esi <> 0 | movsb | End_While
    While B$esi <> '\' | dec esi | dec edi | End_While

    push esi
        While B$esi <> 0 | movsb | End_While
        mov B$edi '\' | inc esi
        mov D$edi 'Back', D$edi+4 'Up\ ' | add edi 7
    pop esi

    inc esi | While B$esi <> 0 | movsb | End_While
    mov B$edi '*' | inc edi
    mov eax D$SavingExtension | stosd | mov B$edi 0

    push edi
        call 'KERNEL32.FindFirstFileA' BackUpPathName, WIN32_FIND_DATA
        mov D$BackUpFileHandle eax
    pop edi

    ...If eax = &INVALID_HANDLE_VALUE
        While B$edi <> '\' | dec edi | End_While | mov B$edi 0
        push edi
            call 'KERNEL32.CreateDirectoryA' BackUpPathName, 0
        pop edi
        mov B$edi '\'
        While B$edi <> '*' | inc edi | End_While
        mov D$edi '0000' | add edi 4
L0:     mov eax D$SavingExtension | stosd | mov B$edi 0
        mov esi MainName | While B$esi <> 0 | inc esi | End_While
        move D$esi D$SavingExtension | mov B$esi+4 0
        push esi
            call 'KERNEL32.CopyFileA' MainName, BackUpPathName, &FALSE
        pop esi
        mov B$esi 0

    ...Else
L2:     call 'KERNEL32.FindNextFileA' D$BackUpFileHandle, WIN32_FIND_DATA
        cmp eax &TRUE | je L2<

        call 'KERNEL32.FindClose' D$BackUpFileHandle

        mov esi cFileName
        While B$esi <> 0 | inc esi | End_While
        While B$esi <> '.' | dec esi | End_While
        call IncTextDwordBeforeEsi

        mov edi BackUpPathName
        While B$edi <> 0 | inc edi | End_While
        While B$edi <> '*' | dec edi | End_While
        mov eax D$esi-4, D$LastBackUpIndice eax | stosd | jmp L0<<

    ...End_If

    mov eax D$MaxBackUp, ebx D$LastBackUpIndice | bswap eax | bswap ebx

    .If ebx >= eax
        mov esi BackUpPathName
        While B$esi <> 0 | inc esi | End_While
        While B$esi <> '.' | dec esi | End_While
        sub esi 4 | mov D$esi '0000'
        mov ebx esi | sub ebx BackUpPathName
        push ebx
            call 'KERNEL32.DeleteFileA' BackUpPathName
            mov esi BackUpPathName, edi BackUpNewPathName
            While B$esi <> 0 | movsb | End_While | mov B$edi 0
        pop ebx

L0:     lea esi D$BackUpPathName+ebx+4
        call IncTextDwordBeforeEsi
        mov eax D$MaxBackUp, edx D$esi-4 | bswap eax | bswap edx | cmp edx eax | ja L9>
        push ebx
            call 'KERNEL32.MoveFileA' BackUpPathName, BackUpNewPathName
        pop ebx
        If eax = &TRUE
            lea esi D$BackUpNewPathName+ebx+4
            call IncTextDwordBeforeEsi
        End_If
        jmp L0<

L9:     mov esi BackUpNewPathName, edi BackUpPathName
        While B$esi <> 0 | movsb | End_While | mov B$edi 0
        lea esi D$BackUpPathName+ebx+4
        call DecTextDwordBeforeEsi

    .End_If

    call 'USER32.MessageBoxA' D$hwnd, BackUpPathName, {'Backup done to:', 0}, 0
ret

IncTextDwordBeforeEsi:
    inc B$esi-1
    ..If B$esi-1 > '9'
        mov B$esi-1 '0' | inc B$esi-2
        .If B$esi-2 > '9'
            mov B$esi-2 '0' | inc B$esi-3
            If B$esi-3 > '9'
                mov B$esi-3 '0' | inc B$esi-4
            End_If
        .End_If
    ..End_If
ret

decTextDwordBeforeEsi:
    dec B$esi-1
    ..If B$esi-1 < '0'
        mov B$esi-1 '9' | dec B$esi-2
        .If B$esi-2 < '0'
            mov B$esi-2 '9' | dec B$esi-3
            If B$esi-3 < '0'
                mov B$esi-3 '9' | dec B$esi-4
            End_If
        .End_If
    ..End_If
ret

____________________________________________________________________________________________



[PartName: ? #16] [WeAreSavingPart: ?    AllOrPart: ?]
[ReplaceSourceText: 'Load Source', 0]

; Tag Dialog 26000

Proc AllOrPartProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_INITDIALOG
            On B$OpeningSourceOnly = &TRUE,
                call 'USER32.SendMessageA' D@Adressee, &WM_SETTEXT, 0, ReplaceSourceText

            mov esi D$ActualTitle
            .If esi = D$RealCodeSource
                If D$esi <> 'TITL'
                    mov D$PartName 'Top ', B$PartName+3 0
                    call 'USER32.SetDlgItemTextA' D@Adressee, 1, DefaultTopTitle
                    jmp L8>>
                End_If
            .End_If

            While B$esi > ' ' | inc esi | End_While ; jump over 'TITLE'

            While B$esi = ' ' | inc esi | End_While ; >>> 'TitleName
            mov edi PartName
            While B$esi > ' ' | movsb | End_While
            mov al 0 | stosb
            call 'USER32.SetDlgItemTextA' D@Adressee, 1, PartName
            jmp L8>>

    ...Else_If D@Message = &WM_COMMAND
        mov eax D@wParam | and D@wParam 0FFFF | shr eax 16

        If D@wParam = &IDCANCEL
            mov B$AllOrPart 0

        Else_If D@wParam = &IDOK
            mov B$AllOrPart 1

        Else_If D@wParam = 3
            mov B$AllOrPart 0-1

        Else_If D@wParam = 4
            mov B$AllOrPart 9

        End_If

        call 'User32.DestroyWindow' D@Adressee

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE
L9: EndP


[IncSaveTitle: B$ 'Saving .inc or .asm ? ...', 0
 IncSaveBody: "
 
   Yes > Save the inc File in its original Directory      
   
   No  > Save the asm File in current Directory", 0]


SaveSource:
    .If B$WeAreSavingPart = &TRUE
        call RestoreRealSource

        call SearchForIncInclude

        If eax = &TRUE
            mov esi D$PointerToIncFileName, edi DestinationFile
            While B$esi > ' ' | movsb | End_While | mov B$edi 0
        End_If

        push eax
            call SetPartialEditionFromPos
        pop eax

        If eax = &TRUE
            call 'USER32.MessageBoxA' D$hwnd, IncSaveBody, IncSaveTitle,
                                      &MB_SYSTEMMODAL_&MB_YESNO
            On eax = &IDYES, jmp L2>
        End_If
    .End_If

    call SetAsmSavingName

L2: call OpenDestinationFile | mov D$NumberOfReadBytes 0
    call 'KERNEL32.WriteFile'  D$DestinationHandle, D$CodeSource, D$SourceLen,
                               NumberOfReadBytes, 0

    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
ret


SearchForIncInclude:
    mov esi D$CodeSource, eax &FALSE

    .While esi < D$SourceEnd
        call IsItPREPARSE
        .If eax = &TRUE
            add esi 9
L1:         While B$esi = ' ' | inc esi | End_While
            If B$esi = ','
                inc esi | jmp L1<
            End_If

            mov eax &FALSE | call IsItIncIncluder

            If eax = &TRUE
                mov eax &FALSE | call IsTheActualTitleIncluded | On eax = &TRUE, ret
            End_If
        .End_If

        inc esi

    .End_While

    mov eax &FALSE
ret


IsItPREPARSE:
    ..If D$esi = 'PREP'
        .If D$esi+4 = 'ARSE'
            If B$esi-1 < ' '
                On B$esi+8 = ' ', mov eax &TRUE
            End_If
        .End_If
    ..End_If
ret


IsItIncIncluder:
L0:
    mov ebx D$esi | or ebx 020202020
    ..If ebx = 'inci' ; IncIncluder
        mov ebx D$esi+4 | or ebx 020202020
        .If ebx = 'nclu'
            mov bx W$esi+8 | or bx 02020
            If bx = 'de'
                mov bl B$esi+10 | or bl 020 | On bl = 'r', mov eax &TRUE
            End_If
        .End_If
    ..End_If

    .If eax = &FALSE
L1:     While B$esi > ' ' | inc esi | End_While

        While B$esi = ' '  | inc esi | End_While

        If B$esi = ','
            inc esi | jmp L1<
        End_If

        On B$esi > ' ', jmp L0<
    .End_If
ret

IsTheActualTitleIncluded:
    push esi
        mov esi D$CodeSource, eax &FALSE

        While esi < D$SourceEnd
            ...If D$esi = 'INCI'  ; INCI NCLU DE
                ..If D$esi+4 = 'NCLU'
                    .If W$esi+8 = 'DE'
                        If B$esi-1 < ' '
                            On B$esi+10 = ' ', call IsThisTheActualTitle
                            On eax = &TRUE, jmp L9>
                        End_If
                    .End_If
                ..End_If
            ...End_If

            inc esi
        End_While
L9: pop esi
ret


[PointerToIncFileName: ?]

IsThisTheActualTitle:
    add esi 10
    While B$esi = ' ' | inc esi | End_While
    mov D$PointerToIncFileName esi

    mov edx &FALSE
    While B$esi <> CR | inc esi | End_While
    While B$esi <> '\' | dec esi | End_While | inc esi
    mov edi D$ActualTitle  ; TITLE xxxx
    add edi 6 | While B$edi = ' ' | inc edi | End_While

    While B$esi <> '.'
        mov al B$esi, bl B$edi | inc esi | inc edi | cmp al bl | jne L7>
    End_While

    .If B$esi = '.'
        If B$edi <= ' '
            mov edx &TRUE
        End_If
    .End_If

L7: mov eax edx
ret

____________________________________________________________________________________________


[ExportAjust: ?]    ; The value to turn an Export section Adress to RVA

PrepareDllVariables:
    mov eax D$AppStartOfCode
    add eax D$AppTrueCodeSize | Align_On 0200 eax | mov D$AppStartOfExp eax
    add eax D$ExportSectionLen | Align_On 0200 eax

    If D$RelocSectionSize => 8 ;jE!
        mov D$AppStartOfReloc eax | add eax D$RelocSectionSize | Align_On 0200 eax
    Else
        mov D$AppStartOfSrc eax | dec W$NumberOfSections
    End_If
    sub eax D$AppStartOfExp | add D$AppAllDataSize eax

    move eax D$ExportSectionLen , D$AppExpTrueSize eax
    Align_On 0200 eax | mov D$AppExpAlignedSize eax

    mov eax D$AppCodeRVAoffset
    add eax D$AppFileSizeOfCode | Align_On 01000 eax | mov D$AppBaseOfExp eax

    add eax D$AppExpAlignedSize | Align_On 01000 eax

    move D$SectionTable D$AppBaseOfExp
    move D$SectionTable+4 D$AppExpAlignedSize

  ; For ease of 'FillExportSection' job:
    mov eax D$AppBaseOfExp | sub eax D$ExportListBPtr | mov D$ExportAjust eax

    mov eax D$ExportSectionLen | Align_On 0200 eax | mov D$FileAlignedExportSectionLen eax
ret

;;
 Same for Resources (DLL cases, too).

StripResourcesHeader:
    mov edi ResourceSectionHeader | sub edi DosHeader | add edi D$CodeList
    mov esi DataSectionHeader | sub esi DosHeader | add esi D$CodeList
    mov ecx SourceSectionHeader | sub ecx DataSectionHeader

  ; Just because if we already stripped the Import Section in a DLL, all the records
  ; we are attempting to point to are one Section upward now:
    If D$ImportTrueSize = 0
        sub esi 40 | sub edi 40 | sub ecx 40
    End_If

    rep movsb
    mov ebx NumberOfSections | sub ebx DosHeader | add ebx D$CodeList
    dec W$ebx

  ; Clear the Image Base record for Resource RVA and Size:
    mov edi AppBaseOfRsrc | sub edi DosHeader | add edi D$CodeList
    mov eax 0 | stosd | stosd
ret
;;

WritePE:
    If D$SavingExtension = '.SYS'
        call SetSysSourceSavingName
    Else
        call SetExeSavingName
        mov eax 0 | call WriteEaxToPeCheckSum
    End_If

    call OpenDestinationFile

    mov D$NumberOfReadBytes 0

    call WriteCheckSum

    call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeList, D$LenOfCode,
                              NumberOfReadBytes, 0

    If B$ExportsectionWanted = &TRUE
        call 'KERNEL32.WriteFile' D$DestinationHandle, D$ExportListBPtr,
                                  D$FileAlignedExportSectionLen, NumberOfReadBytes, 0
    End_If

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:
        If D$RelocSectionSize => 8 ;jE!
          call 'KERNEL32.WriteFile' D$DestinationHandle,
                                   D$Relocation, D$FileAlignedRelocationSize,
                                   NumberOfReadBytes, 0
        End_If
    End_If

    call 'KERNEL32.WriteFile' D$DestinationHandle, D$CodeSource, D$SourceLen,
                              NumberOfReadBytes, 0

    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0

    On D$SavingExtension = '.SYS', call WriteSysFile
ret

;;
  It has happend that under some condition(s), it is possible that a wrong
  paring of CR/LF come out in a Source. For example "0D, 0D, 0A". As these
  conditions are unknown, a fix is done, here, at Write-Time:

CheckCRLFs:
    mov esi D$CodeSource, edx esi, eax 0 | add edx D$SourceLen
    
    While esi < edx
        If B$esi = CR
            On B$esi+1 <> LF, inc eax
        Else_If B$esi = LF
            On B$esi-1 <> CR, inc eax
        End_If
        
        inc esi
    End_While
 
    ..If eax <> 0
        mov ecx D$SourceLen | add ecx eax | VirtualAlloc Trash ecx
        mov esi D$CodeSource, edi D$Trash, edx esi | add edx D$SourceLen
        
        While esi < edx
            .If B$esi = CR
                If B$esi+1 <> LF
                    movsb | mov B$edi LF | inc edi | inc D$SourceLen
                    hexprint 1
                Else
                    movsb
                End_If
            .Else_If B$esi = LF
                If B$esi-1 <> CR
                    mov B$edi CR | inc edi | movsb | inc D$SourceLen
                    hexprint 2
                Else
                    movsb
                End_If
            .Else
                movsb
            .End_If
            
        End_While
        
        call 'KERNEL32.WriteFile' D$DestinationHandle, D$Trash, D$SourceLen,
                              NumberOfReadBytes, 0
hexprint D$SourceLen
        call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
        
        call 'USER32.MessageBoxA' 0, {"
A bad pairing of CR/LF has been found in the Source,
at Write-Time. The problem is fixed, but RosAsm is
going to shut down, and you will have to Re-Load.

Do not care, then, of the Alert Message for corruption.

Sorry for the inconvenient.

In case you could point out what action(s), in the Editor,
could help reproducing these wrong CR/LFs, please report.
", 0}, 
                                     {'Source failure', 0}, 0
        
        call 'Kernel32.ExitProcess' 0
        
    ..End_If
ret
;;

SaveBookMarks:
    mov edi DestinationFile, ecx 0-1, al 0 | repne scasb
    While B$edi <> '.'
        dec edi
    End_While

    push D$edi, edi
        mov eax '.BKM' | stosd

        call OpenDestinationFile

        mov edi D$BookMarks, al 0, ecx 0-1
L0:     repne scasb | cmp B$edi 0 | ja L0<
        sub edi D$BookMarks | inc edi                    ; edi = lenght.

        call 'KERNEL32.WriteFile'  D$DestinationHandle, D$BookMarks, edi,
                                   NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0
    pop edi, D$edi
ret


StoreChoosenName:
    mov edi MainName, ecx 262, al 0 | rep stosb
    mov edi MainName
L0: lodsb | cmp al 0 | je L9>
           ; cmp al '.' | je L9>
      stosb | jmp L0<

L9: dec edi | cmp B$edi '.' | jne L9< ; Recent add ('.' can be inside the name too!)
    mov B$edi 0
    While B$esi <> '.'
        dec esi
    End_While

L9: lodsd | or eax 020202000
    If eax = '.exe'
        move D$SavingExtension D$ExeExtension
    Else_If eax = '.scr'
        move D$SavingExtension D$ScrExtension
    Else_If eax = '.asm'
        move D$SavingExtension D$ExeExtension
    Else_If eax = '.dll'
        move D$SavingExtension D$DLLExtension
    Else_If eax = '.sys'
        move D$SavingExtension D$SysExtension
    Else
       ; hexxxprint 088888
    End_If
ret

[StoreNameOnly | push #1 | pop esi | call StoreChoosenName]

[SavingExtension: '.EXE'    ExeExtension: '.EXE'    ScrExtension: '.SCR'
 DLLExtension: '.DLL'       SysExtension: '.SYS']

SetExeSavingName:
    mov edi DestinationFile, ecx 262, al 0 | rep stosb
    mov edi DestinationFile, esi MainName
    If B$esi = 0
        mov ax W$DefaultFileName | stosw | jmp L9>
    End_If
L0: lodsb | cmp al 0 | je L9>
        stosb | jmp L0<
L9: mov eax D$SavingExtension | stosd
    mov B$edi 0
ret


SetSysSourceSavingName:
    mov edi DestinationFile, ecx 262, al 0 | rep stosb
    mov edi DestinationFile, esi MainName
    If B$esi = 0
        mov ax W$DefaultFileName | stosw | jmp L9>
    End_If
L0: lodsb | cmp al 0 | je L8>
        stosb | jmp L0<

L8: On W$edi-6 <> 'So', jmp L8>

    If D$edi-4 = 'urce'
        mov B$esi  -7, 0 | jmp L9>
    End_If

L8: mov D$edi 'Sour', W$edi+4 'ce' | add edi 6

L9: mov eax D$SavingExtension | stosd
    mov B$edi 0
ret


[TestName: 'Test.a', 0]

SetTestSavingName:
    mov edi DestinationFile, ecx 262, al 0 | rep stosb
    mov edi DestinationFile, esi TestName
L0: lodsb | stosb | cmp al 0 | ja L0<
    mov B$edi 0
ret


[DefaultFileName: 'PE.', 0]


; Revue: Does not work if the Path contains some '.' Period in a Folder Name (!!!!)....

[FullPartName: B$ ? #&MAXPATH]

SetAsmSavingName:
    mov edi DestinationFile, ecx 262, al 0 | rep stosb

    If B$WeAreSavingPart = &TRUE
        mov esi MainName, edi FullPartName, ecx &MAXPATH | rep movsb | dec edi

        While B$edi = 0 | dec edi | End_While
L0:     cmp B$edi '\' | je L1>
        cmp B$edi ':' | je L1>
            dec edi | jmp L0<

L1:     inc edi
        mov esi PartName
        While B$esi > 0 | movsb | End_While | movsb

        mov esi FullPartName

    Else
        mov esi MainName

    End_If

    mov edi DestinationFile

    If B$esi = 0
        mov ax W$DefaultFileName | stosw | jmp L9>
    End_If

L0: lodsb | cmp al 0 | je L9>
           ; cmp al '.' | je L9>
      stosb | jmp L0<

L9: On B$edi-4 = '.', sub esi 4  ; (RosAsm outputs are always with 3 Chars Extensions).

    mov eax '.asm' | stosd
ret

 ________________________________________________________________________________________
 ________________________________________________________________________________________


____________________________________________________________________________________________
____________________________________________________________________________________________

[ShowWinEquateError: &TRUE]

ReplaceWin32Equates:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen
    mov B$ShowWinEquateError &TRUE, B$ErrorLevel 8  ; 'error8'

L0: lodsb

    ..If al = '&'
        On B$esi < 'A', jmp L2>>
        push ecx, esi, edi
            ;dec esi |
            mov D$imm32 0 | call NewSearchForWinEquate
        pop edi, eax, ecx
      ; mov ebx esi | sub ebx eax | sub ecx ebx ; Same:
        add ecx eax | sub ecx esi | jz L9>
            mov eax D$imm32 | call WriteEax


    ..Else_If al = textSign
        On B$esi-2 > LowSigns, jmp L7>
        dec ecx | mov bl al
L1:     stosb | lodsb
        If al = TextSign
            On B$esi > LowSigns, jmp L8>
            jmp L2>
        End_If
        loop L1<

    ..Else

L2:     stosb

    ..End_If

    loop L0<

L9: sub edi D$CodeSourceB | mov D$StripLen edi
    Exchange D$CodeSourceA D$CodeSourceB

    mov D$imm32 0
ret

L7: mov edx D$MissingSeparator1Ptr | jmp L9>

L8: mov edx D$MissingSeparator2Ptr

L9: mov B$Errorlevel 9, ebx esi
    While B$esi > EOI | dec esi | End_While | inc esi
    While B$ebx > EOI | inc ebx | End_While | mov B$ebx 0
    error edx, esi

 ________________________________________________________________________________________
 ________________________________________________________________________________________


  _____________________
  _____________________

; Same as Above, but with some specific computing of newly added [] inside macros
; unfolded. To be restructured later: make the OpenBracket > OpenVirtual job a called
; routine for both here and uper treatements. Too hot now...

ResetForNewBrackets:
    mov esi D$CodeSourceA, edx esi | add edx D$StripLen
L0: .While esi < edx
        lodsb

        If al = TextSign
            While B$esi <> TextSign | inc esi | End_While | inc esi

        Else_If al = '?'
            On B$esi > LowSigns, jmp L0<
            On B$esi-2 > LowSigns, jmp L0<
            mov ebx esi
            While B$ebx <> OpenBracket
                On B$ebx = OpenVirtual, jmp L0<
                dec ebx
            End_While
            mov B$ebx OpenVirtual, ecx ebx

            While B$esi <> CloseBracket
                inc esi
                On esi >= edx, error D$UnPairedNestedBracketsPtr, ecx
            End_While
            mov B$esi CloseVirtual

        End_If
    .End_While
;;
 'ReorderSource' halas impossible, here.
 
 keeping instructions and brackets' declarations mixed together drives to many
 problems; so we now rewrite it all: at first all bracket declarations, then all
 Virtual Data and at last all EOI instructions text:
;;
    move D$StatementsPtr D$StatementsTable, D$StatementsPtr2  D$StatementsTable2

    mov esi D$CodeSourceA,  edi D$CodesourceB

    mov ecx esi | add ecx D$Striplen | sub ecx 3         ; end of source adress

    On B$esi = EOI, inc esi

  ; Store, first, Brackets / VirtualBrackets:
L0: lodsb | cmp esi ecx | ja L9>>
    If al = TextSign
T0:     lodsb | cmp al TextSign | jne T0<
        jmp L0<
    End_If

    cmp al OpenVirtual | je L1>
    cmp al Openbracket | je L1>
    .If al = EOI
        If B$esi = OpenVirtual
            call IsMixed | On eax = &TRUE, add D$StatementsPtr 4
        Else_If B$esi = OpenBracket
            call IsMixed | On eax = &TRUE, add D$StatementsPtr 4
        Else
            add D$StatementsPtr 4
        End_If
    .End_If

    jmp L0<

  ; Brackets / VirtualBrackets found >>> adjust the Statements Table and copy:
L1: call FillStatementsTable2 | add D$StatementsPtr 4

    stosb | mov B$esi-1 0FF | lodsb | jmp L2> ; 0FF mark for start of [ in next part
L1: stosb | mov B$esi-1 0 | lodsb             ; write [...brackets...]
L2: call IsItText | je L1<                    ; for "text with CR/LF inside"
    cmp al CloseVirtual | je L3>
    cmp al Closebracket | jne L1<
L3: mov B$esi-1 0 | stosb

    jmp L0<<

L9:  _________________________________

  ; Restart the copy for Code Statements:
L3: mov esi D$CodeSourceA

    move D$StatementsPtr D$StatementsTable

L4: lodsb | cmp esi ecx | jae L9>> ;!!!!!!!!!    ; je and not ja because of ending EOIs...
    .If al = 0FF
        add D$StatementsPtr 4
        While B$esi = 0 | lodsb | End_While | jmp L4<

    .Else_If al = EOI
        If B$edi-1 = EOI
            sub D$StatementsPtr 4 | sub D$StatementsPtr2 4 | dec ebx
            call FillStatementsTable2 | add D$StatementsPtr 4 | jmp L4<<
        End_If
        call FillStatementsTable2 | add D$StatementsPtr 4

    .Else_If al = meEOI
        On B$esi <> 0FF, jmp L8>
        On B$esi-2 = 0, jmp L4<<

    .End_If
  ; Write the instructions:
L8: stosb | jmp L4<<
  ; Last EOI:
L9: stosb | mov eax edi | sub eax D$CodeSourceB | mov D$Striplen eax

    mov eax D$StatementsPtr2, D$eax 0
    Exchange D$StatementsTable D$StatementsTable2
    ;call TestStatementsTable
    Exchange D$CodeSourceA D$CodeSourceB
ret


; Does the Statement's unfold result in mixed chunks of normal Code and of Declarations?

IsMixed:
    push esi
        mov al B$esi

        If al = OpenVirtual
            mov bl CloseVirtual
        Else
            mov bl CloseBracket
        End_If

        While B$esi <> EOI
            inc esi

            .If B$esi = bl
                On B$esi+1 = meEOI, inc esi

                If B$esi+1 = EOI
                    mov eax &FALSE | jmp L9>
                Else_If B$esi+1 = OpenVirtual
                    mov bl CloseVirtual
                Else_If B$esi+1 = OpenBracket
                    mov bl CloseBracket
                Else
                    mov eax &TRUE | jmp L9>
                End_If
            .End_If

        End_While

L9: pop esi
ret


FillStatementsTable2:
    push eax, ebx
        mov eax D$StatementsPtr, eax D$eax
        mov ebx D$StatementsPtr2, D$ebx eax
        add D$StatementsPtr2 4
    pop ebx, eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Parser of the Parameters being declared as Macros (or direct Data with Automatic Labels).
  Example:
  
  > [Return | mov eax #1]

  > return {RGB 011,022,033}        ; Unfold as: mov eax 0332211
;;

[ParaMacrosMaxLevel: ?    ParaMacroLevel: ?    MacroModifiedExpression: ?]

ParaMacrosParser:
    mov esi D$InstructionB, edi D$InstructionA

    mov edx D$StripLen | add edx esi
    mov D$ParaMacrosMaxLevel 0, D$ParaMacroLevel 0

    While esi < edx
        On B$esi = CloseParaMacro, jmp L1>
        On B$esi = OpenParaMacro, inc D$ParaMacrosMaxLevel
        movsb
    End_While

    On D$ParaMacrosMaxLevel = 0, ret

L1: mov B$MacroModifiedExpression &TRUE

    mov esi D$InstructionB, edi D$InstructionA, ecx D$ParaMacrosMaxLevel

    .While esi < edx
        ...If B$esi = OpenParaMacro
            inc D$ParaMacroLevel

            ..If D$ParaMacroLevel = ecx
                push edx
                    inc esi     ; Strip 'OpenParaMacro' Char.

                    If B$esi = TextSign
                        call UnfoldDataParameter

                    Else_If B$esi+1 = memMarker
                        call UnfoldDataParameter

                    Else
                        call UnfoldMacroParameter

                    End_If

                    mov B$MacroJobIsOver &FALSE
                pop edx

            ..Else
                movsb

            ..End_If

        ...Else_If B$esi = CloseParaMacro
            dec D$ParaMacroLevel
            movsb

        ...Else
            movsb

        ...End_If
    .End_While

    mov ecx edi | sub ecx D$InstructionA
    mov D$StripLen ecx
    Exchange D$InstructionA D$InstructionB
    Exchange D$InstructionAEnd D$InstructionBEnd
ret


UnfoldDataParameter:
    call CreateNoMeanLabel | call WriteNoMeanLabel

    push esi
        inc esi | While B$esi <> CloseParaMacro | inc esi | End_While | inc esi

        While esi < edx | movsb | End_While
        While B$edi-1 = EOI | dec edi | End_While

        mov B$edi meEOI, B$edi+1 '{' | add edi 2
        call WriteNoMeanLabel | mov B$edi ColonSign | inc edi
    pop esi

    While B$esi <> CloseParaMacro | movsb | End_While
    mov B$edi '}', B$edi+1 EOI, B$edi+2 EOI | add edi 3

    mov esi edx
ret

UnfoldMacroParameter:
    push D$Striplen
        mov D$InstructionAptr esi, ebx esi, ecx 0

      ; clear all possible 'Done Flag's:
        While B$ebx <> CloseParamacro
            On B$ebx = 0, error D$ParaMacroPtr
            and B$ebx 00_0111_1111
            inc ecx | inc ebx
        End_While

      ; End Mark and 'Striplen', for the Macros Parser:
        mov B$ebx EOI | mov D$StripLen ecx

      ; Count the Macro Name Length for 'IsItInMacroList':
        mov ebx esi, D$OneWordLen 0
        While B$ebx > LowSigns
            inc D$OneWordLen | inc ebx
        End_While

        push esi
            push edi
                call GetFromQwordCheckSum esi, D$MacroList, D$MacroListLimit
                On eax = 0, error D$ParaMacroPtr

                While B$eax > LowSigns | inc eax | End_While | inc eax
                mov esi D$eax, ecx D$eax+4

            pop edi

            mov D$InstructionBptr edi
            call ReplaceFromMacroData

        pop esi

        While B$esi <> EOI | inc esi | End_While | inc esi

    pop D$Striplen
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;
; mov eax (((12*3) xor 001010101)+2)

[InsideExpression: ?    StartOfSourceExpression: ?    StartOfDestinationExpression: ?
 RealExpression: ?      RealHexaSize: ?]
;;
 Simply searches for '(', saves Pos and call for the translation after count of nested
 levels in ebx.

 In case of Real, i change the Mem Marker from 'R$/F$' to 'Q$/D$' because i store the
 computed Real in Hexa format (it would be far too complicated to rewrite them in
 Math Decimal notation).
;;

ExpressionParser:
    mov edx esi | add edx D$Striplen
    mov B$InsideExpression &FALSE, B$RealHexaSize 0
    mov ebx 0, ecx 0

L0: .While esi < edx
        lodsb

        ..If al = TextSign
            stosb | While B$esi <> TextSign | movsb | End_While | movsb | jmp L0<
        ..Else_If al = OpenSign
            .If B$InsideExpression = &FALSE
                cmp B$esi-2 memMarker | jne L3>
                mov al B$esi-3 | and al 00_0111_1111 ; Mask Equates and Macros Done Flag.
                cmp al 'F' | jne L1>
                    mov B$edi-2 'D', B$RealHexaSize 8 | jmp L2>
L1:             cmp al 'R' | jne L3>
                    mov B$edi-2 'Q', B$RealHexaSize 16
L2:                 mov B$RealExpression &TRUE | jmp L4>
L3:             mov B$RealExpression &FALSE
L4:             mov ebx 0, ecx 0
                mov D$StartOfSourceExpression esi, D$StartOfDestinationExpression edi
            .End_If
            inc ebx | inc ecx | mov B$InsideExpression &TRUE

        ..Else_If al = CloseSign
            On ecx = 0, error D$ParenthesisPtr
            dec ecx

        ..Else_If al = OpenVirtual
            On B$InsideExpression = &TRUE, error D$ParenthesisPtr
            call CheckBracketExpression

        ..Else_If al = Openbracket
            On B$InsideExpression = &TRUE, error D$ParenthesisPTR
            call CheckBracketExpression

        ..Else_If al = EOI
            On B$InsideExpression = &TRUE, error D$ParenthesisPtr

        ..End_If

        ..If ebx > 0
            .If ecx = 0
                push edx
                    If B$RealExpression = &TRUE
                        call ComputeRealExpression
                    Else
                        call ComputeExpression
                    End_If
                    call WriteExpresionResult
                    mov ebx 0, ecx 0, B$InsideExpression &FALSE
                pop edx
                jmp L0<<
            .End_If
        ..End_If

        stosb
    .End_While
ret

;;
Due to the way the Expression Parser replaces Real by 'D$' / 'Q$' Hexa, all the
Components of a Data set with Real Expressions have to be given with a size marker.
The checking operations are repeatedly splitted for speed purpose.
;;


CheckBracketExpression:
; To be reviewed entirely. There is something out of logic at "L1":
    push ebx
    push eax, esi
      ; Is there any Parenthesis???
;;
        While esi < edx
            On B$esi = OpenSign, jmp L1>
            inc esi
        End_While
        jmp L9>>

      ; OK, OpenSign Inside. Is there any Real Marker???
L1:     pop esi | push esi
        While esi < edx
            If B$esi = MemMarker
                mov al B$esi-1 | and al 00_0111_1111
                On al = 'F', jmp L1>
                On al = 'R', jmp L1>
            End_If
            inc esi
        End_While
        jmp L9>>
;;
        mov ebx 0
        While esi < edx
            lodsb
            .If al = MemMarker
                mov bl B$esi-2 | and bl 00_0111_1111

            .Else_If al = OpenSign
                If bl = 'F'
                    jmp L1>
                Else_If bl = 'R'
                    jmp L1>
                End_If

            .End_If
        End_While
        jmp L9>>


      ; OK, OpenSign plus Real Marker inside >>> Want SizeMarkers everywhere. Not
      ; 'everywhere', in fact, but before and after the Expression... :
L1:     pop esi | push esi
        .While esi < edx
            ...If B$esi = OpenSign
              ; MemMarker wanted before Real Expression:
                If B$esi-1 <> numSign
                    On B$esi-1 <> MemMarker, error D$MarkerBeforePtr
                End_If

                mov eax 1
                While eax > 0
                    inc esi
                    If B$esi = OpenSign
                        inc eax
                    Else_If B$esi = CloseSign
                        dec eax
                    End_If
                    On B$esi = CloseBracket, error D$ParenthesisPtr
                    On B$esi = CloseVirtual, error D$ParenthesisPtr
                End_While

              ; Now, ebx at the end of Expression. CloseBracket >>> OK:
                cmp B$esi+1 CloseBracket | je L9>
                cmp B$esi+1 CloseVirtual | je L9>

                inc esi | On B$esi > Separators, error D$MarkerAfterPtr

                push esi
                    inc esi
                    While B$esi > PartEnds
                        inc esi
                    End_While
                    .If B$esi = ColonSign
                        On B$esi+2 <> MemMarker, error D$MarkerAfterPtr
                    .Else_If B$esi <> MemMarker
                        Error D$MarkerAfterPtr
                    .End_If
                pop esi

            ...End_If
L8:         inc esi
        .End_While

L9: pop esi, eax
    pop ebx
ret


[ExpressionResult: ? ?    Operator: ?]

[ExpressionA: ? ExpressionB: ?]
[ExpressionAPtr: ? ExpressionBPtr: ?]
[ExpressionALimit: ?  ExpressionBLimit: ?]

InitExpressionBuffers:
    VirtualAlloc ExpressionA 01000 | add eax 0800
    mov D$ExpressionALimit eax, D$ExpressionAPtr eax
    VirtualAlloc ExpressionB 01000 | add eax 0800
    mov D$ExpressionBLimit eax, D$ExpressionBPtr eax
ret

; Main parsing of one Expression. Calls for storage (in Binary) and for re-write after
; computation, from the more inside level to first one:

ComputeExpression:
    push esi
        mov ecx esi, esi D$StartOfSourceExpression, edi D$ExpressionA
        dec esi | sub ecx esi | rep movsb            ; copy the Expression to 'ExpressionA'.
        mov al 0 | stosb

L0:     mov D$ExpressionResult 0, D$ExpressionResult+4 0, B$Operator OpenSign
        mov esi D$ExpressionA, edi D$ExpressionB, edx 0

        push ebx
L1:         lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        pop ebx

        If edi > D$ExpressionBLimit
            call ExtendTableMemory ExpressionA, ExpressionAPtr, ExpressionALimit
            call ExtendTableMemory ExpressionB, ExpressionBPtr, ExpressionBLimit
        End_If

L2:     On B$esi = Space, inc esi
        mov eax D$esi | and al 00_0111_1111     ; clear possible 'Done Flag'.

        ...If al >= 'A'
            ..If ax = 'OR'
                mov B$Operator 'O' | add esi 2 | On B$esi = Space, inc esi
            ..Else_If ax = 'XO'
                If B$esi+2 = 'R'
                    mov B$Operator 'X' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else_If al = 'S'
                If W$esi+1 = 'HL'
                    mov B$Operator 'L' | add esi 3 | On B$esi = Space, inc esi
                Else_If W$esi+1 = 'HR'
                    mov B$Operator 'R' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else_If ax = 'AN'
                If B$esi+2 = 'D'
                    mov B$Operator 'A' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else_If ax = 'NO'
                If B$esi+2 = 'T'
                    mov B$Operator 'N' | add esi 3 | On B$esi = Space, inc esi
                Else
                    jmp L9>> ;error D$ExpressionMemberPtr
                End_If
            ..Else
                jmp L9>> ;error D$ExpressionMemberPtr
            ..End_If

        ...Else_If al < '0'
            move D$Operator D$esi | mov B$Operator al | inc esi

        ...Else_If al <= '9'
            push ebx
                If ax = '00'
                    call TranslateBinary | dec esi | call StoreOnExpression
                Else_If al = '0'
                    call TranslateHexa | dec esi | call StoreOnExpression
                Else
                    and B$esi 00_0111_1111
                    call TranslateDecimal | dec esi | call StoreOnExpression

                End_If
            pop ebx

        ...Else
            jmp L9>> ;error D$ExpressionMemberPtr

        ...End_If

        cmp B$esi-1 CloseSign | jne L2<<
            call ReWriteExpression
            dec ebx | jnz L0<<
L9: pop esi
    ret


[ExpressionReal: R$ ?    RealExpressionResult: R$ ?]

ComputeRealExpression:
    On ebx > 1, error D$RealNotationPtr

    push esi
        mov ecx esi, esi D$StartOfSourceExpression, edi D$ExpressionA
        dec esi | sub ecx esi ;| rep movsb          ; copy the Expression to 'ExpressionA'.
      ; Since the Expression parser has moved in between the Equates and Macros jobs,
      ; we have to mask out the Byte HighBit (Done Flag):
L0:     lodsb | and al 00_0111_1111 | stosb | loop L0<
        mov al 0 | stosb


L0:     mov D$RealExpressionResult 0, D$RealExpressionResult+4 0, B$Operator OpenSign
        mov esi D$ExpressionA, edi D$ExpressionB, edx 0

        push ebx
L1:         lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        pop ebx

L2:     On B$esi = Space, inc esi

        ...If B$esi < '0'
            move D$Operator D$esi | inc esi

        ...Else_If B$esi <= '9'
            push ebx
                mov edi ExpressionReal | call atof      ; Result in ST0
                dec esi
                call StoreOnRealExpression
            pop ebx

        ...Else
            error D$RealNotationPtr

        ...End_If

        cmp B$esi CloseSign | jne L2<<
            call ReWriteRealExpression
            dec ebx | jnz L0<<

L9: pop esi
ret


[NegativeMember: ?]

StoreOnExpression:
    ..If B$Operator >= 'A'
        .If B$Operator = 'O'
            or D$ExpressionResult+4 edx | or D$ExpressionResult eax

        .Else_If B$Operator = 'X'
            xor D$ExpressionResult+4 edx | xor D$ExpressionResult eax

        .Else_If B$Operator = 'N'
            On D$ExpressionResult > 0, error D$ExpressionNOTPtr
            On edx > 0, error D$ExpressionNOTPtr
            not eax
            mov D$ExpressionResult eax

        .Else_If B$Operator = 'A'
            and D$ExpressionResult+4 edx | and D$ExpressionResult eax

        .Else_If B$Operator = 'L'
            On edx > 0, error D$ExpressionSHRPtr
            On eax > 0FF, error D$ExpressionSHRPtr
                mov cl al
                shl D$ExpressionResult+4 cl
                shl D$ExpressionResult cl | adc D$ExpressionResult+4 0

        .Else_If B$Operator = 'R'
            On edx > 0, error D$ExpressionSHRPtr
            On eax > 0FF, error D$ExpressionSHRPtr
                mov cl al
                shr D$ExpressionResult cl
                shr D$ExpressionResult+4 cl | adc D$ExpressionResult 0

        .Else
            mov D$ExpressionResult+4 edx, D$ExpressionResult eax

        .End_If

    ..Else_If B$Operator = AddSign
        add D$ExpressionResult+4 edx | add D$ExpressionResult eax

    ..Else_If B$Operator = SubSign
        sub D$ExpressionResult+4 edx | sub D$ExpressionResult eax

    ..Else_If B$Operator = MulSign
        On edx > 0, error D$TooMuchExpressionPtr
        On D$ExpressionResult+4 > 0, error D$TooMuchExpressionPtr
        mov ecx eax, eax D$ExpressionResult
        mul ecx
        mov D$ExpressionResult+4 edx | mov D$ExpressionResult eax

    ..Else_If B$Operator = DivSign
        mov ecx eax, eax D$ExpressionResult, edx D$ExpressionResult+4
        div ecx | mov edx 0
        mov D$ExpressionResult+4 0 | mov D$ExpressionResult eax

    ..Else_If B$Operator = OpenSign
        mov D$ExpressionResult+4 edx, D$ExpressionResult eax

    ..Else
        error D$ExpressionSignPtr

    ..End_If
ret


; The result of new computed Value is in ST0.

[TempoReal: Q$ ?]

StoreOnRealExpression:
    ..If B$RealHexaSize = 8
        fstp F$TempoReal | fld F$RealExpressionResult
        .If B$Operator = AddSign
            fadd F$TempoReal
        .Else_If B$Operator = SubSign
            fsub F$TempoReal
        .Else_If B$Operator = MulSign
            fmul F$TempoReal
        .Else_If B$Operator = DivSign
            fdiv F$TempoReal
        .Else_If B$Operator = OpenSign
            fld F$TempoReal
        .Else
            error D$RealNotationPtr
        .End_If
        fstp F$RealExpressionResult
    ..Else
        fstp R$TempoReal | fld R$RealExpressionResult
        .If B$Operator = AddSign
            fadd R$TempoReal
        .Else_If B$Operator = SubSign
            fsub R$TempoReal
        .Else_If B$Operator = MulSign
            fmul R$TempoReal
        .Else_If B$Operator = DivSign
            fdiv R$TempoReal
        .Else_If B$Operator = OpenSign
            fld R$TempoReal
        .Else
            error D$RealNotationPtr
        .End_If
        fstp R$RealExpressionResult
    ..End_If
ret


[StartOfHexaExpression: ?]

ReWriteExpression:
    push ebx
        On edx > 0, error D$TooMuchExpressionPtr
        mov esi D$ExpressionA, edi D$ExpressionB

L1:     lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        dec edi                                         ; skip first '('.
        mov eax D$ExpressionResult

        mov ebx eax, ecx 8, B$StartOfHexaExpression &TRUE

L0:     mov eax ebx | and eax 0_F000_0000 | shr eax 28
        add al '0' | On al > '9', add al 7
        shl ebx 4
        .If B$StartOfHexaExpression = &TRUE
            If al > '0'
                mov B$StartOfHexaExpression &FALSE
                mov B$edi '0' | inc edi
            End_If
        .End_If
        On B$StartOfHexaExpression = &FALSE, stosb | loop L0<

        If B$StartOfHexaExpression = &TRUE              ; Case of result = 0
            mov B$edi '0' | inc edi
        End_If

        While B$esi-1 <> CloseSign
            inc esi                                     ; jmp over resolved part.
        End_While

        If B$esi > PartEnds
            mov al Space | stosb
        End_If

        While B$esi-1 > 0
            movsb                       ; recopy remainder of expression.
        End_While
        Exchange D$ExpressionA D$ExpressionB
        Exchange D$ExpressionALimit D$ExpressionBLimit
    pop ebx
ret


ReWriteRealExpression:
    push ebx
       ; On edx > 0, error TooMuchExpression ; ??????!!!!!!......
        mov esi D$ExpressionA, edi D$ExpressionB

L1:     lodsb | stosb | cmp al OpenSign | jne L1<
                dec ebx | jnz L1<
        dec edi                                         ; skip first '('.

        mov ecx D$RealHexaSize, B$StartOfHexaExpression &TRUE

        If ecx = 16
            mov ebx D$RealExpressionResult+4
        Else
            mov ebx D$RealExpressionResult
        End_If

L0:     mov eax ebx | and eax 0_F000_0000 | shr eax 28
        add al '0' | On al > '9', add al 7
        shl ebx 4
        .If B$StartOfHexaExpression = &TRUE
            If al > '0'
                mov B$StartOfHexaExpression &FALSE
                mov B$edi '0' | inc edi
            End_If
        .End_If
        On ecx = 9, mov ebx D$RealExpressionResult
        On B$StartOfHexaExpression = &FALSE, stosb | loop L0<

        If B$StartOfHexaExpression = &TRUE              ; Case of result = 0
            mov B$edi '0' | inc edi
        End_If

        While B$esi-1 <> CloseSign
            inc esi                                     ; jmp over resolved part.
        End_While

        If B$esi > PartEnds
            mov al Space | stosb
        End_If

        While B$esi-1 > 0
            movsb                                       ; recopy remainder of expression.
        End_While
        Exchange D$ExpressionA D$ExpressionB
        Exchange D$ExpressionALimit D$ExpressionBLimit
    pop ebx
ret


; True final result rewrite in source:

WriteExpresionResult:
    push esi
        mov edi D$StartOfDestinationExpression, esi D$ExpressionA
      ; (Expression 'A' and not 'B' as source, because they are switched after each pass).

      ; SourceCleaner may leave one more SpAce when ",(" encounted >>> to review.
        If B$edi-1 <> Space
            .If B$edi-1 > NoSpaceAfterThis
               ; mov al Space | stosb
            .End_If
        End_If

        While B$esi > 0
            movsb
        End_While
;;
   ; For Analyzes:
  
        push edi
            mov B$edi 0
            mov edi D$StartOfDestinationExpression
            sub edi 3
            howme edi
            exprint D$edi
        pop edi
;;
    pop esi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Routines for adressing the lists used for data, data labels and code labels
 three main tables are used:
                              DataList, LabelList, CodeRef.
 DataList is a simple list of double words values. LabelList is a index for DataList
 (set of couples of name / adress pointing to DataList). CodeRef is an index
 (same couples structure), pointing to code symbols evocation (offset in code).

 The two index tables are sets of fields like this:
                             ...|MySymbol|000024A0...|...

 Tables will begin with a dWord for the size in bytes. (of little use)


 Filling certain heads words with zeros is usefull in case of error search
;;

[ListsLength: ?]

InitIndex1:
;;
  None of these tables should be greater than stripLen.
  For a big file like RosAsm source, it makes about 3 Mo. for the 11 tables together
  could be much shorter, if any problem, mainly for last 4 tables.
  
  Though, because of Macros possibly creating Data Labels, for example, it is
  possible to overflow easily, particulary on very small files. So one Page is
  added to these Allocations.
  
  Also 'ListsLength', that is used in 'GetFromQwordCheckSum' for controling the
  matching of a scanned Name with its proper List, must be aligned on its final
  - and not so predictable - size (Align_On 01000).
;;
    add D$StripLen 01000

    move D$ListsLength D$StripLen
    Align_On 01000 D$ListsLength

    VirtualAlloc LabelList D$StripLen
    mov edi D$LabelList | mov eax 0 | stosd
    mov al EOI | stosb | mov D$LabelListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | mov D$LabelListLimit edi

    VirtualAlloc EquateList D$StripLen
    mov edi D$EquateList | mov eax 0 | stosd
    mov al EOI | stosb | mov D$EquateListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | mov D$EquateListLimit edi

    VirtualFree D$EquateSubstitutes
    VirtualAlloc EquateSubstitutes D$StripLen
    mov edi D$EquateSubstitutes | mov D$EquateSubstitutesPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY
    mov D$EquateSubstitutesLimit edi
    move D$LastEquateList D$EquateList
    move D$LastEquateListPtr D$EquateListPtr

    VirtualAlloc MacroList D$StripLen
    mov edi D$MacroList | mov eax 0 | stosd
    mov al EOI | stosb | mov D$MacroListPtr edi
    add edi D$StripLen | sub edi TABLES_SECURITY | mov D$MacroListLimit edi

    VirtualAlloc MacroData D$StripLen | move D$MacroDataPtr D$MacroData
    mov eax D$MacroData | add eax D$StripLen | sub eax TABLES_SECURITY
    mov D$MacroDataLimit eax

    sub D$StripLen 01000
ret


InitIndex2:
  ; Don't supress reloc table: Pointers needed for setting of other values:
    VirtualAlloc Relocation D$StripLen | move D$RelocationPtr D$Relocation

    VirtualAlloc ApiListA D$StripLen | move D$ApiListAPtr D$ApiListA

    VirtualAlloc ApiListB D$StripLen | move D$ApiListBPtr D$ApiListB

    VirtualAlloc DllList D$StripLen | move D$DllListPtr D$DllList
ret


InitIndex3:
    VirtualFree D$DllList, D$ApiListB, D$ApiListA

    VirtualAlloc CodeRef D$StripLen

    mov edi D$CodeRef | mov eax 0 | stosd
    mov al EOI | stosb | mov D$CodeRefPtr edi

    VirtualAlloc DataRef D$StripLen
    mov edi D$DataRef | mov eax 0 | stosd
    mov al EOI | stosb | mov D$DataRefPtr edi
    mov eax D$StripLen | add eax D$DataRef | sub eax TABLES_SECURITY
    mov D$DataRefLimit eax
ret

 ________________________________________________________________________________________
 ________________________________________________________________________________________
;;
 analyze of data declarations and macros (+ equates)

 Macros and equates bodies are all stored in MacroData. MacroList and EquateList
 could also be only one table, but i choose to separate them because i wanted to
 alternate the two replacement routines (see ReplaceEquatesAndMacros) for flow
 control (source goes forth and back between CodeSourceA and CodeSourceB) and
 because i wanted equate to be replaced in macro statement 'before' expanding job
 Third reason: A macro is always first word of a line / equates may be anywhere
 in a line; so, the job is different.
 ________________________________________________________________________________________

 MacroList looks  ....|name|dWord1 dWord2 Byte|....
                            dWord1 = adress in MacroData
                                  dWord2 = lenght of statement(s)
                                         byte is set to 0 when storing
 This byte is not for expanding control. It is only for end user can check if unsused
 macro are in his source file (The "Unused Symbols" thingie in the Statistics)

 EquateList looks the same.

 MacroData is a simple text list of what is to replace equates and macros without
 any separators.

 EquateList:
 MacroList:         ; storage of macros and equates ...|name|adr/len/flag|...
 MacroData:         ; raugh storage of macros and equates body(no lenght as first word)
;;
 _______________________________________________________________________________________

[LoadAndClear | lodsb | mov B$esi-1 0]
 _______________________________________________________________________________________

; MacroList begin with a Dword for lenght of table followed by macro items.
 _______________________________________________________________________________________

[LastOctetInSource: ?]

StoreEquates:
    mov edi D$EquateListPtr,  B$esi-1 0FF

    If edi > D$EquateListLimit
        call ExtendTableMemory EquateList, EquateListPtr, EquateListLimit
        mov edi D$EquateListPtr
    End_If

L0: push esi | mov ebx 0
L1:     lodsb | inc ebx | cmp al LowSigns | ja L1<
    pop esi | dec ebx | ;call IsItNewEquate  ; with lenght in ebx

L1: LoadAndClear
    If al = Closebracket
        Error D$MissEquateValPtr
    Else_If al = ColonSign
        While B$edi-1 <> EOI | dec edi | End_While
        Error D$EquateLabelPtr edi
    End_If
    cmp al, Space | je L2>                  ; write name in EquateList up to first space
    On al < LowSigns, error D$MissEquateValPtr
        stosb | jmp L1<
L2: mov al EOI | stosb
    mov eax D$MacroDataPtr | stosd          ; write futur adress of data in Macrolist

    call SetQwordCheckSum D$EquateListPtr

    mov D$EquateListPtr edi                 ; save pointer
    mov edi eax                             ; switch from index to data

L3: loadAndClear | cmp al Separators | jb L9>   ; write equate body
    cmp al Closebracket | je L9>            ; in macroData

    .If al = TextSign
        stosb
L4:     loadAndClear | stosb | cmp al TextSign | jne L4<

    .Else_If al = '<'
        On B$esi = SpAce, LoadAndClear
L4:     loadAndClear | cmp al '>' | je L3<
        On al = Closebracket, error D$TextEquatePtr
        stosb
      ; If Space before '>', strip Space:
        If B$esi = Space
            On B$esi+1 = '>', LoadAndClear
        End_If
        jmp L4<

    .Else
        stosb

    .End_If
    jmp L3<

L9: mov bl al                               ; save lasting sign for ending test
    mov eax edi | sub eax, D$macroDataPtr   ; written data lenght in ax
    mov D$MacroDataPtr edi                  ; save pointer
    If edi > D$MacroDataLimit
        call ExtendTableMemory MacroData, MacroDataPtr, MacroDataLimit
    End_If
    mov edi D$EquateListPtr | stosd         ; store data lenght in MacroList (second word)
    mov al 0 | stosb                        ; room for 'done' flag used when replacing
    mov al EOI | stosb
    mov D$EquateListPtr edi                 ; save pointer
    cmp bl Separators | jb L0<<             ; one more equate > store again
ret

[Equateee "merde agaa"  ]
 _______________________________________________________________________________________

StoreMacros:
    mov edi D$MacroListPtr,  B$esi-1 0

    If edi > D$MacroListLimit
        call ExtendTableMemory MacroList, MacroListPtr, MacroListLimit
        mov edi D$MacroListPtr
    End_If

    push esi
        mov ebx 0
L0:     lodsb | inc ebx | cmp al LowSigns | ja L0<
    pop esi
    dec ebx | ;call IsItNewMacro           ; with symbol lenght in ebx

L1: LoadAndClear | cmp al meEOI | je L2>  ; write name in MacroList up to first separator
        stosb | jmp L1<

L2: mov al EOI | stosb                    ; no meEOI in MacroList (yes in MacroData)
    mov eax D$MacroDataPtr | stosd        ; write futur adress of data in Macrolist

    call SetQwordCheckSum D$MacroListPtr

    mov D$MacroListPtr edi                ; save pointer
    mov edi eax                           ; switch EDI from index to data

L3: LoadAndClear | cmp al CloseBracket | je L9>
        stosb | jmp L3<                   ; write macro body and loop
L9: mov eax edi | sub eax D$macroDataPtr  ; written data lenght in eax
    mov D$MacroDataPtr edi

    If edi > D$MacroDataLimit
        call ExtendTableMemory MacroData, MacroDataPtr, MacroDataLimit
    End_If

    mov edi D$MacroListPtr

    stosd                                 ; store data lenght in MacroList (second word)
    mov al 0 | stosb                      ; room for 'done' flag used when replacing
    mov al EOI | stosb
    mov D$MacroListPtr edi
ret
 ________________________________________________________________________________________

; here we use 'LastOctetInSource' instead of checking '||'

StripZeros:
    mov esi D$CodeSourceA,  edi D$CodeSourceB,  ebx 0
L0: lodsb | cmp esi D$LastOctetInSource | jae L9>>
        cmp al 0FF | je L0<
        cmp al 0 | je L0<

L1:     .If al = EOI                            ; difficulty: double separators '||' may
            If B$edi-1 = meEOI                  ; result after storage. case: '|[....]|'
                mov B$edi-1 EOI | jmp L0<       ; same on second pass after '{}'.
            Else_If B$edi-1 = EOI
                jmp L0<
            End_If

        .Else_If al = meEOI
            If B$edi-1 = meEOI
                jmp L0<
            Else_If B$edi-1 = EOI
                jmp L0<
            End_If

        .End_If

L5: stosb | inc ebx | jmp L0<<
L9: mov al EOI | stosb | inc ebx | mov D$StripLen ebx
    mov eax 02020202 | stosd                    ; security

    Exchange D$CodeSourceA D$CodeSourceB
ret


StripDoneStatementsPointers:
    mov esi D$StatementsTable, edi esi

L0: lodsd
    If eax = DoneFlag
        jmp L0<
    Else_If eax > 0
        stosd | jmp L0<
    End_If

    stosd

; Test of Last recorded Code Statement:

;    mov eax edi | sub eax 8 | mov eax D$eax
;
;    mov ebx eax, dl B$ebx+3, B$OldChar dl, B$ebx+3 0
;    pushad
;        mov esi eax
;        call SetDebuggeeText
;        call AskForRedraw
;    popad
;    exprint eax

ret
 _______________________________________________________________________________________

; after storing equates and macros, stripped results of remaining source will be
; written from CodeSourceA to CodeSourceB.
; At last, StripZero will work back again < > A
 _______________________________________________________________________________________
[FirstPassTest: ?]

StoreAllEquates:
    mov B$FirstPassTest 0

    mov esi D$CodeSourceA,  ecx D$StripLen | add ecx esi | inc ecx  ; ecx = max esi value
    mov D$LastOctetInSource ecx,  B$ErrorLevel 0,  D$bracketCounter 0
    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: lodsb | call IsItText | je L0<
        cmp al EOI | je L9>>                        ; EOI >>> no more brackets.
    cmp al OpenVirtual | jne L1>
        inc D$bracketCounter
        add D$StatementsPtr 4 | jmp L0<
L1: cmp al Openbracket | jne L0<                    ; loop until '['
        inc D$bracketCounter
        add D$StatementsPtr 4
        push esi
        cmp B$esi '<' | jne L2>                     ; Data Alignement?
       ; cmp B$esi+1 '0' | jb L2>
       ; cmp B$esi+1 '9' | ja L2>
        or B$esi 00_1000_0000  ; to prevent ReplaceEquates from replacing '[<' by '[B' in
                               ; case user define an Equate for '<' Char (not reserved Char)
        pop eax | jmp L0<
L2:     lodsb | cmp al, OpenParaMacro | ja L2<
            mov ecx esi                             ; for end reached test
        pop esi
    cmp al ColonSign | jne L3>                      ; data?
        mov esi, ecx | jmp L0<
L3: cmp al Space | jne L3>                          ; equate?
        call StoreEquates
            mov eax D$StatementsPtr, D$eax DoneFlag
        jmp L0<
L3: cmp al meEOI | je L0<                           ; macro?

L3: Error D$UnknownDataPtr

L9: mov edi D$EquateListPtr, ecx 20 | rep stosb     ; '|||||||' as security tail
    mov eax, D$EquateListPtr | sub eax, D$EquateList; write size of equate list
    mov edi, D$Equatelist | inc eax | stosd         ; at first word
ret


StoreAllMacros:
    mov esi D$CodeSourceA,  ecx D$StripLen | add ecx esi | inc ecx  ; ecx = max esi value
    mov D$LastOctetInSource ecx,  B$ErrorLevel 0,  D$bracketCounter 0
    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: lodsb | call IsItText | je L0<
    cmp al EOI | je L9>                             ; EOI >>> no more brackets.
    cmp al OpenVirtual | jne L1>
        inc D$bracketCounter
        add D$StatementsPtr 4 | jmp L0<
L1: cmp al 0FF | je L8>
    cmp al Openbracket | jne L0<                    ; loop until '['
L7:     inc D$bracketCounter
        add D$StatementsPtr 4
    cmp B$esi 0BC | je L0< ; jE! fixed '[<' case
        push esi
L2:         lodsb | cmp al, LowSigns | ja L2<
            mov ecx esi                             ; for end reached test
        pop esi

L3: cmp al meEOI | jne L0<                          ; macro?
        On B$esi = meEOI, error D$UnexpectedCRLFPtr
        call storeMacros
            mov eax D$StatementsPtr, D$eax DoneFlag
        jmp L0<

L8: lodsb | cmp al 0 | je L8<
    inc D$bracketCounter
    add D$StatementsPtr 4
    dec esi | jmp L0<<

L9: mov edi D$MacroListPtr, ecx 20 | rep stosb      ; '|||||||' as security tail
    mov eax, D$MacroListPtr | sub eax D$MacroList   ; write size of macro list
    mov edi, D$Macrolist | inc eax | stosd          ; at first word
ret


StoreEquatesAndMacros:
    call StoreAllEquates
    call ResolveEquates
    call StoreAllMacros
    call StripZeros

    call StripDoneStatementsPointers
  ; Source: A > B
  ; call TestStatementsTable
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[SortTempoMemory: ?]

SortEquatesAndMacrosLists:
    mov eax D$EquateListPtr | sub eax D$EquateList | inc eax
    On eax = 6, jmp L9>>

    push eax
        VirtualAlloc SortTempoMemory eax
    pop ecx
    push ecx
        mov edi D$SortTempoMemory, esi D$EquateList
        shr ecx 2 | inc ecx | rep movsd

        move D$SortSource D$SortTempoMemory | add D$SortSource 5
        move D$SortDestination D$EquateList | add D$SortDestination 5

        mov D$SortStringNumber 0 | mov esi D$SortDestination

L0:     lodsb | cmp al EOI | jne L0<
        add esi 11
        inc D$SortStringNumber
        cmp esi D$EquateListPtr | jb L0<

        call SortStrings

    pop ecx
    mov esi D$EquateList, edi D$SortTempoMemory
    shr ecx 2 | inc ecx | rep movsd

     move D$SortSource D$SortTempoMemory | add D$SortSource 5
     move D$SortDestination D$EquateList | add D$SortDestination 5

    call SortEquatesStringsBySize

    VirtualFree D$SortTempoMemory
L9: ret


;;
 Source and Destination are Pointers to 2 Tables (same lenght). Source holds a set
 of zero ended strings to be sorted and stored in Destination. Source is overwritten
 with 0FF Bytes when finished.
;;
[SortSource: ?    SortDestination: ?    SortStringNumber: ?]

SortStrings:
    mov ecx D$SortStringNumber, edi D$SortDestination

L0: push ecx

        mov esi D$SortSource, ecx D$SortStringNumber, edx 0, bl 0FF

L1:     lodsb
        .If al = 0FF
            ; nop
        .Else_If al < bl
            mov bl al | lea edx D$esi-1
        .Else_If al = bl
            push ebx
                push edx, esi
                    While al = bl
                        lodsb | inc edx | mov bl B$edx
                        cmp al EOI | je L2>
                    End_While
L2:             pop esi, edx
                On al < bl, lea edx D$esi-1
            pop ebx
        .End_If

        While B$esi > EOI
            inc esi
        End_While
        add esi 11 | loop L1<
; add esi 11, because:
; |EquateName| D$ .... .... B$ .|
;            >    >>>> >>>>    >>^

        If edx > 0
            mov esi edx
            While B$esi > EOI
                movsb | mov B$esi-1 0FF
            End_While
            movsb  ; |
            movsd  ; Ptr to MacroData
            movsd  ; MacroData record length
            movsb  ; Flag
            movsb  ; |
        End_If

    pop ecx | dec ecx | cmp ecx 0 | ja L0<<
ret


; Pointers by Sizes: Each dWord will be (possibily -0, if none-) a Pointer to the
; position of the first Equate Equate in the List having the 'given' number of Chars.
; ('given' by the Routine that will finally *search* for the Equate body:
[LabelsPointersBySizes: EquatesPointersBySizes: ? #100]
[MacrosPointersBySizes: ? #100]

[SortBySizeOver: ?]

SortEquatesStringsBySize:
    mov edi EquatesPointersBySizes, eax 0, ecx 100 | rep stosd

    mov edi D$SortDestination, edx 1

L0: mov esi D$SortSource, ecx D$SortStringNumber, B$SortBySizeOver &TRUE

; There is something wrong in this: If i state 'While B$esi <> EOI' instead of
; 'While B$esi > EOI', it doesn't work. It should, and there is no reason for what
; D$esi could be = 0 (this is to say after the end of the Table -i suppose...)...

L1:     lodsb
        .If al = 0FF        ; Done
            While B$esi > EOI | inc esi | End_While
            add esi 11

        .Else
            push ecx
                mov eax esi, ecx 1
                While B$eax > EOI | inc ecx | inc eax | End_While
                If ecx = edx
                    mov al B$esi-1, B$esi-1 0FF
                    On D$EquatesPointersBySizes+edx*4 = 0,
                        mov D$EquatesPointersBySizes+edx*4 edi
                    stosb
                    dec ecx | jecxz L2>
                        rep movsb
L2:                 movsb  ; |
                    movsd  ; Ptr to MacroData
                    movsd  ; MacroData record length
                    movsb  ; Flag
                    movsb  ; |

                Else
                    lea esi D$eax+11
                End_If

            pop ecx
            mov B$SortBySizeOver &FALSE
        .End_If
    loop L1<

    inc edx | cmp B$SortBySizeOver &FALSE | je L0<<
ret




; input: EBX = lenght of researched label set by caller; >ESI > first letter of new label

[ListNumerix: ?]  ; this value is to be add to tables pointers to jmp over numeric data
                  ; when searching for a name

[SearchMain: ?]
SearchForEntryPoint:
    mov B$SearchMain &TRUE
        call GetFromQwordCheckSum EntryPointLabel, D$LabelList, D$LabelListLimit
        If eax = 0
            mov B$ErrorLevel 9
            error D$NoEntryPtr
        End_If

        While B$eax <> EOI | inc eax | End_While | inc eax
        or B$eax+4 DoneFlag
        mov eax D$eax

    mov B$SearchMain &FALSE
ret

 _________________________________________________________________________________________
;;
 replacements of equates and macro evocation of source file
                              source: < > A
 ReplaceMacAndEqu reads one word from source and call IsItInMacroList. If yes, this
 called routine returns, after read in Macrolist, in ECX the lenght of stored text
 data in MacroData and in ESI the offset of these data.
 Then, ReplaceMacAndEqu calls ReplaceFromMacroData to read these data and write them
 in destination file (a new 'source file'). If a '#' symbol is encounted, in turn,
 ParametersUnfolding is called to read the source parameters
 ________________________________________________________________________________________

 input: InstructionBptr point one text word and OneWordLen gets its lenght; al = last
 character of a one word.
 to avoid a same symbol be booth a label and an equate/macro, no unfolding before ':'
;;

[OneWordLen: ?]

 _______________________________________________________________________________________
;;
 Effective replacement of macros:
 It allows loop if '#+n' statement found at the end of declaration, while getting
 control on parameters number when needed....
 Constant First parameter allowed with '#F'. Last one, with '#L'
 Reverse job allowed with '#n' and global replacement with #x>y'

 Very simple for user > very difficult inside...
;;
 _______________________________________________________________________________________

[AfterLastParameter: ?  NumberOfMacroParameters: ?   LastMacPara: ?
 FirstUnfolded: ?  LastUnfolded: ?  UnfoldingRotations: ?]

SearchLastParameter:
    push esi
        mov B$LastMacPara '0', D$NumberOfMacroParameters 0 | mov esi D$InstructionAptr

      ; Number of spaces >>> Number of Parameters:
        lodsb
        While al > EOI
            If al = space
L1:             inc B$LastMacPara
                inc D$NumberOfMacroParameters
           ; Else_If al = ColonSign
           ;     On B$esi <> EOI, jmp L1<
            End_If
            lodsb
        End_While

        dec esi | mov D$AfterLastParameter esi
    pop esi
ret


[NoLabel: ?  NoSizeMarker: ?]

; Unfolds one parameter

ParameterUnfolding:
    mov B$NoLabel &FALSE, B$NoSizeMarker &FALSE
    .If B$esi = '!'
        inc esi
        If B$esi = '!'
            inc esi | mov B$NoSizeMarker &TRUE
        Else
            mov B$NoLabel &TRUE
        End_If
    .End_If
    push ecx, esi
        mov bl al | cmp bl 'F' | jne L0>    ; 'x' of #x in bl
            mov bl '1' | jmp L6>>
L0:     cmp bl, 'L' | jne L1>
            mov bl B$LastMacPara | jmp L6>>
L1:     cmp bl 'N' | jne L2>
            mov bl B$LastMacPara ; | add bl B$MacrosX ; ah have been 'neg'ed
        ;    On bl = '1',  mov B$FirstUnfolded &TRUE | jmp L6>>

L2:     On bl < '1',  error D$MacNumberPtr
        On bl > '9',  error D$MacNumberPtr
        add bl B$MacrosX | On bl = '1',  mov B$FirstUnfolded &TRUE
        On bl = B$LastMacPara,  mov B$LastUnfolded &TRUE
        ...If B$esi >= '0'
            ..If B$esi <= '9'
              ; Extreemely durty hack to inc _ESI_ and dec ECX: (!!!!!!!!!!!!!!!!!)
                pop eax, ecx
                    inc eax | dec ecx
                push ecx, eax
                sub bl '0' | lea ebx D$ebx*4+ebx | shl ebx 1
                lodsb | sub al '0' | add bl al
                add bl '0'
                .If B$esi >= '0'
                    If B$esi <= '9'
                        error D$TooBigXpTR
                    End_If
                .End_If
            ..End_If

        ...End_If

L6:     mov esi D$InstructionAptr                ; switch from MacroData to clean Code Source (A)

L7:     lodsb                                  ; and search parameter
        On al =< EOI, error D$MissingParameterPtr
        cmp al space | jne L7<                 ; spaces count gives
        inc ecx
            dec bl | cmp bl, '0' | jne L7<       ; parameter position
        and B$esi 00_0111_1111                 ; Strip Equates-done-Flag, if any, because
                                                 ; it may corrupt macro-built-symbols.
        On B$NoSizeMarker = &TRUE, add esi 2

L8:     lodsb | cmp al Separators | jb L9>     ; read in sourceA
        stosb | jmp L8<                        ; write parameter in destination (SourceB)

L9: pop esi, ecx
    If B$NoLabel = &TRUE
       dec edi, ecx
       On B$edi = TextSign, error D$TextKillingPtr
    Else_If B$NoSizeMarker = &TRUE
        sub ecx 2
    End_If
ret


MultiParaCheck:
    cmp al 'F' | jne L0>
        mov al '1' | jmp L9>
L0: cmp al 'L' | jne L1>
        mov al B$LastMacPara | jmp L9>
L1: On al = 'N',  error D$NForbidenPtr
L2: On al < '1',  error D$MacNumberPtr
    On al > '9',  error D$MacNumberPtr
L9: ret
 ____________________________________________
;;
 #
 2   >  al  > bl  |               |  >  al
 >                |  >  xchg  >   |
 L          > al  |               |  >  bl
;;
 ____________________________________________


MultiParametersUnfolding:               ; #x>y  has been found  al = x
   ; On ah > 0, error MacNumber
    On B$MacrosX > 0, error D$MacNumberPtr
    call MultiParacheck

    dec ecx | mov bl al | lodsb         ; ecx = lenght from 'IsItInMacroList'
    dec ecx | lodsb                     ; bl = x   al = y
    call MultiParacheck

    push esi, ecx
        mov esi D$InstructionAptr              ; switch from MacroData to clean Code Source
        xchg al bl | cmp al bl | ja L2>

L1:     call UnfoldOneOfMultiPara | inc al | cmp al bl | jna L1<
            jmp L9>

L2:     call UnfoldOneOfMultiPara | dec al | cmp bl al | jna L2<

L9: pop ecx, esi
ret


UnfoldOneOfMultiPara:
    push eax ebx
        mov bl al | mov esi D$InstructionAptr

      ; Search parameter:
L1:     lodsb
        On al <= EOI, error D$MissingParameterPtr
      ; Spaces count gives parameter position:
        cmp al space | jne L1<
            dec bl | cmp bl, '0' | jne L1<

L2:     lodsb | cmp al, Separators | jb L8>
          ; Cases of Text Parameters (???...):
            If al = TextSign
                stosb
                While B$esi <> TextSign | movsb | End_While
                movsb | jmp L2<
          ; Case of {... #2>L} nested Declarations found in Macro Declarations:
            Else_If al = '}'
                error {"| wanted between Multiple Parameters and '}', in Macro Declaration", 0},
                      D$InstructionAptr
            End_If
          ; write parameter in destination
            stosb | jmp L2<
L3:         dec D$AfterLastParameter
      ; Space forced (avoid '|' when reading, at first, the last parameter)
L8:     mov al space | stosb
L9: pop ebx eax
ret
;;
; I do not recall the reason why i did this strange '}' thingie, previously:
L2:     lodsb
        cmp al, Separators | jb L9>
        cmp al '}' | je L3>
          ; write parameter in destination
            stosb | jmp L2<
L3:         dec D$AfterLastParameter
      ; Space forced (avoid '|' when reading at first the last parameter)
L9:     mov al space | stosb                   
    pop ebx eax
ret
;;

;;
  These are the 128 Bytes Tables for storing whatever the user wants to store:
  The internal macros Variables, &0, &1,... , &99:
;;

[MacrosVariablesTable: ? #(32*102)]
[MacroCounters: ? #100]
[MACRO_VARIABLES 100]

ClearMacroVariable:
    mov edi MacrosVariablesTable, ecx ((32*102)+100), eax 0 | rep stosd
ret


[MixedInternalVariables: 'You cannot mix Internal Variables by text and by Number', 0
 CounterSyntax: "Macros Counters syntax example:
&&21=&&21+1 // &&3=&&60-1 // &&5=0", 0]

StoreMacroVariableByNumber: ; StoreMacroVariable
  ; eax = Displacement to the MacroVariable Record, inside 'MacrosVariablesTable'

  ; esi is pointing after '&&23='

  ; Macros Variables Declarations are not true Statements.
  ; So, we have to strip the leading '|' (meEOI) previously written:
    ;dec edi

    push edi
        lea edi D$MacroCounters+eax*4

L0:     cmp B$esi EOI | jbe L9>>

        lodsb

        .If al = '&'
            lodsb | dec ecx
            On al <> '&', error MixedInternalVariables
                dec ecx
                call ParseCounterToCounterAttribution
              ; Unwanted meEOI:
                ;dec ecx

        .Else_If al = TextSign
          ; skip first TextSign
            dec ecx
            call GetAttributionChar | mov D$edi ebx
          ; skip last TextSign
            inc esi | dec ecx

        .Else_If al = NumSign
            dec ecx | lodsb | dec ecx
            If al = 'N'
                move D$edi D$NumberOfMacroParameters
            Else
                error CounterSyntax
            End_If

        .Else_If al < '0'
            error CounterSyntax

        .Else_If al <= '9'
            dec esi
            call GetAttributionNumber | mov D$edi ebx

        .Else_If al = 'P'
            call SaveStatementCounter | sub ecx 3

        .Else
            error CounterSyntax

        .End_If

        ;loop L0<
L9: pop edi
ret


;;
  Example: &&51=Pos (For the #If #ErrorPos).
  
  Saves the actual Source Parsing Pointer into a Macro Counter that the user will
  possibly reuse for having his private error message coming out, with the source
  being pointed to, eventually, at an upward position from the error detection.
;;
SaveStatementCounter:
    If W$esi = 'OS'
        mov eax D$StatementsPtr
        mov D$edi eax
        add esi 2
    Else
        error CounterSyntax
    End_If
ret


ParseCounterToCounterAttribution:
    call GetMacroVariableDis | mov eax D$MacroCounters+eax*4

    ..If ecx = 0
        ; eax ready

    ..Else_If B$esi <= EOI
        ; eax ready

    ..Else_If B$esi = AddSign
        inc esi | dec ecx
        call GetAttributionNumber
        add eax ebx
        ;.If B$esi = '1'
        ;    inc eax | stosd | inc esi | dec ecx
        ;    If ecx <> 0
        ;        On B$esi > EOI, error CounterSyntax
        ;    End_If
        ;.Else
        ;    error CounterSyntax
        ;.End_If

    ..Else_If B$esi = SubSign
        inc esi | dec ecx
        call GetAttributionNumber
        sub eax ebx
       ; .If B$esi = '1'
       ;     dec eax | stosd | inc esi | dec ecx
       ;     If ecx <> 0
       ;         On B$esi > EOI, error CounterSyntax
       ;     End_If
       ; .Else
       ;     error CounterSyntax
       ; .End_If

    ..Else
        error CounterSyntax

    ..End_If

    mov D$edi eax
ret


[CounterAttribution: 'Bad Number Attribution for internal Counter', 0]

GetAttributionNumber:
    If W$esi = '00'
        call GetAttributionBinary
    Else_If B$esi = '0'
        call GetAttributionHexa
    Else_If B$esi < '0'
        error CounterAttribution
    Else_If B$esi <= '9'
        call GetAttributionDecimal
    Else
        error CounterAttribution
    End_If
ret


GetAttributionBinary:
    mov ebx 0 | inc esi | dec ecx

L0: If B$esi = '1'
        shl ebx 1 | or ebx 1
    Else_If B$esi = '0'
        shl ebx 1
    Else
        ret
    End_If
    inc esi | dec ecx | jnz L0<
ret


GetAttributionHexa:
    mov ebx 0
    push eax
L0:     If B$esi < '0'
            pop eax | ret
        Else_If B$esi <= '9'
            shl ebx 4 | mov al B$esi | sub al '0' | or bl al
        Else_If B$esi < 'A'
            pop eax | ret
        Else_If B$esi <= 'F'
            shl ebx 4 | mov al B$esi | sub al '0' | sub al 7 | or bl al
        Else
            pop eax | ret
        End_If
        inc esi | dec ecx | jnz L0<
    pop eax
ret


GetAttributionDecimal:
    push eax
    mov ebx 0
L0: lodsb
    If al < '0'
        ; Out
    Else_If al > '9'
        ; Out
    Else
        sub al '0'
        lea ebx D$ebx+ebx*4
        lea ebx D$eax+ebx*2
        dec ecx | jnz L0<
    End_If

    dec esi | pop eax
ret

[CounterTextAttribution: 'Max Text Attribution is 4 Chars, for a Counter', 0]

GetAttributionChar:
    mov ebx 0

L0: or bl B$esi  | dec ecx | inc esi
    On B$esi = TextSign, ret
    shl ebx 8 | or bl B$esi  | dec ecx | inc esi
    On B$esi = TextSign, ret
    shl ebx 8 | or bl B$esi  | dec ecx | inc esi
    On B$esi = TextSign, ret
    shl ebx 8 | or bl B$esi  | dec ecx | inc esi
    On B$esi <> TextSign, error CounterTextAttribution
ret
____________________________________________________________________________________________

[WritingLowCounter:
'Only "0" to "9" or "A" to "Z" accepted.', "
Bad writing attempt with Counter &&"
 WritingCounter: 0, 0, 0]

WriteMacroVariableByNumber:
    mov ebx eax
    mov eax D$MacroCounters+eax*4 | stosb

    If al < '0'
        ; bad
    Else_If al > 'Z'
        ; bad
    Else_If al <= '9'
        ; good '1' to '9'
        ret
    Else_If al >= 'A'
        ; goo ''A' to 'Z'
        ret
    End_If

    mov edi WritingCounter, eax ebx
    call WriteEaxDecimal | mov B$edi 0
    error WritingLowCounter
ret


StoreMacroVariable:
  ; eax = Displacement to the MacroVariable Record, inside 'MacrosVariablesTable'

  ; Macros Variables Declarations are not true Statements.
  ; So, we have to strip the leading '|' (meEOI) previously written:
    On B$edi-1 = meEOI, dec edi

    push edi
        lea edi D$MacrosVariablesTable+eax

L0:     cmp B$esi EOI | jbe L9>>
        lodsb                           ; read data from MacroData
        .If al = NumSign                ; if parameter, > unfolding
            lodsb | dec ecx
            If al = 'X'
                mov al B$MacrosX | inc al | call AlToDecimal | loop L0<

            Else_If al = 'N'
                mov al B$NumberOfMacroParameters | call AlToDecimal | loop L0<

            Else_If B$esi = '>'
                mov bl B$LastMacPara
                cmp al '9' | ja L2>
                    cmp al bl | ja L3>
L2:             cmp bl '9' | ja L4>
                    cmp B$esi+1 'L' | je L4>
                        cmp B$esi+1 bl | jbe L4>
L3:             dec edi
                cmp B$edi ColonSign | je C1>
                    cmp B$edi EOI | ja L3<
                        jmp C2>
C1:             inc edi
C2:             add esi 2 | sub ecx 2

                jmp L6>
L4:             call MultiParametersUnfolding
                dec edi
L6:             dec ecx

            Else
                call ParameterUnfolding | dec ecx | jnz L0<<

            End_If

        .Else_If al = '&'
            call GetMacroVariableDis | shl eax 7
            On B$esi = '=', error D$NestedMacroVariablePtr
            call WriteMacroVariable
            jmp L8>
L7:         jmp L0<<
L8:         loop L7<

        .Else_If al = '!'
            dec edi
            jmp L8>
L7:         jmp L0<<
L8:         loop L7<

        .Else
            jmp L8>
L7:         jmp L0<<
L8:         stosb | loop L7<                    ; write body in MacroVx

        .End_If

L9:     mov al 0 | stosb
    pop edi
ret


WriteMacroVariable:
    push esi, ebx
        lea esi D$MacrosVariablesTable+eax
        mov ebx edi | add ebx 80

        If B$esi = 0
         ; This is commented out because it outputs NOPEs every now and then when unwished.
         ; I don't find example, when it could be a problem for Conditional Macros...

         ; mov D$edi 'NOPE' | add edi 4
           pop ebx, esi | ret
        End_If
        If B$esi = Space
           inc esi
           ; jE! skip Space, if is 1st byte!
        End_If
        .While B$esi > 0
            If B$esi = '&'
                inc esi
                push ecx
                    call GetMacroVariableDis | shl eax 7
                    On B$esi = '=', error D$NestedMacroVariablePtr

                    push esi
                        call WriteMacroVariable
                    pop esi
                pop ecx
            Else
                movsb | On edi = ebx, error D$MacVarOverFlowPtr
            End_If
        .End_While

    pop ebx, esi
ret

____________________________________________________________________________________________

[NoMeanLabel: 'ZZZZZZZZ' 0    NoMeanEaten: 0]

; B$esi > '0' of '&0' when called:

DataNoMeanLabel:
    lodsb                                                ; strip '0'
    If B$esi = ColonSign                                 ; '&0:' Declaration
        On B$NoMeanEaten = &TRUE, error D$Double0Ptr
        call CreateNoMeanLabel
        inc esi | sub ecx 3                              ; strip ':' and '&0:' count
        call WriteNoMeanLabel
        mov al ColonSign | stosb
        mov B$NoMeanEaten &TRUE
    Else                                                 ; '&0' Evocation
        call WriteNoMeanLabel
        sub ecx 2                                        ; strip '&0' count
  ;  Else
        ; i think no error check usefull here because compilation will fail later...
    End_If
ret


; Prepares a new Automatic Label Name, to be written by 'WriteNoMeanLabel':

CreateNoMeanLabel:
    push edi
        mov edi NoMeanLabel | add edi 7
        dec B$edi
        While B$edi < 'A'
            mov B$edi 'Z' | dec edi | dec B$edi
        End_While
    pop edi
ret


; Write the Automatic Label at edi (without trailing Colon):

WriteNoMeanLabel:
    push esi, ecx
        mov esi NoMeanLabel, ecx 8 | rep movsb
    pop ecx, esi
ret

____________________________________________________________________________________________

; Writes al to edi in Decimal form:

AlToDecimal:
    push ecx, eax
        mov ecx 10
        push 0-1
L1:     mov ah 0 | div cl | add ah '0' | push eax | cmp al 0 | ja L1<

L2:     pop eax
        If eax <> 0-1
            mov B$edi ah | inc edi | jmp L2<
        End_If
    pop eax, ecx
ret

;;
  Gets the Macro Variable Displacement, in the 'MacrosVariablesTable'.
  
  called from: 'ReplaceFromMacroData', 'StoreMacroVariable', plus 'WriteFromMacroVariable'
  in cases of nested MacroVariable.
;;

GetMacroVariableDis:
  ; esi points to the Number first Char after the '&':
    mov al B$esi
    cmp al '9' | ja L0>
    cmp al '0' | jae L1>
L0:     error D$MacroVariableIndicePtr

L1: dec ecx
    push ebx
        mov eax 0, ebx 0

L0:     lodsb | sub al '0' | add ebx eax
        cmp B$esi '0' | jb L9>
        cmp B$esi '9' | ja L9>
          ; ebx = ebx*10:
            lea ebx D$ebx*4+ebx | shl ebx 1 | loop L0<

L9:     mov eax ebx
    pop ebx

  ; eax = &Variable Indice from 1 to 100:
    On eax > MACRO_VARIABLES, error D$MacroVariableIndicePtr

  ; eax = Displacement to the proper 128 Byte Record:
  ;  shl eax 7 ; Done by caller
ret


[MacrosX: ?]

; esi point to MacroData, ecx = length of data (set by IsItInMacroList)

testVariable:
    pushad
        mov D$edi '////'
        mov edx D$InstructionBptr | add edx 128
        call ShowMaping D$InstructionBptr, edx, 0
    popad
ret


ReplaceFromMacroData:
    call SearchLastParameter

    mov edi D$InstructionBptr

    mov B$MacrosX 0, B$FirstUnfolded &FALSE, B$LastUnfolded &FALSE, B$NoMeanEaten &FALSE

X0: push ecx, esi
      ; read data from MacroData:
L0:     lodsb

        ...If al = NumSign
          ; "#" Parameter, > Unfolding:
            call ParseMacroParameter

            If ecx = 0-1
                pop esi, ecx | jmp X0<
            Else_If ecx > 0
               jmp L0<
            Else
                jmp L9>>
            End_If

        ...Else_If al = '&'

            ..If B$esi = '0'
            ; '&0' found:
                call DataNoMeanLabel | cmp ecx 0 | ja L0<
                    jmp L9>>

            ..Else
                .If B$esi = '&'
                ; &&1... &&99 numbered Variable:
                    dec ecx | inc esi
                    cmp B$esi '0' | jb N0>
                    cmp B$esi '9' | ja N0>
                        call ParserMacroVariableByNumber | cmp ecx 0 | jg L0<<
                        jmp L9>

                .Else_If B$esi < '1'
                  ; nop
                .Else_If B$esi > '9'
                  ; nop
                .Else
                  ; &1... &99 Variable:
                    call ParseMacroVariable | cmp ecx 0 | ja L0<<

                    jmp L9>

                .End_If

            ..End_If
        ...End_If

N0:     stosb                          ; write
        On edi >= D$InstructionAEnd, error D$MacrosOverFlowPtr
        dec ecx | jnz L0<<
L9: pop esi, ecx                                 ; true stack restore
ret

____________________
; '#' NumSign found:

ParseMacroParameter:
    lodsb | dec ecx

    ..If al = SubSign
        On B$esi-3 > EOI, error D$BadMacroLoopPtr
      ; +/- in bl // 1/.../9 in al:
        mov bl, al | lodsb | dec ecx

      ; Sign was "-":
        If B$FirstUnfolded = &FALSE
            sub al '0' | sub B$MacrosX al
            On B$MacrosX >= 080, error D$InfiniteLoopPtr
            On B$LastMacPara = '0', error D$MacNumberPtr

          ; Restart unfolding signal:
            mov ecx 0-1

        Else
          ; job is done. strip lasting separator:
            dec edi
          ; if some more text after loop symbol (another one is ready from source)
            On ecx > 0, dec ecx

        End_If
        ret

    ..Else_If al = addSign
        On B$esi-3 > EOI, error D$BadMacroLoopPtr
        mov bl, al | lodsb | dec ecx
      ; sign was "+":
        If B$LastUnfolded = &FALSE
            sub al '0' | add B$MacrosX al
            On B$MacrosX >= 080, error D$InfiniteLoopPtr
            On B$LastMacPara = '0', error D$MacNumberPtr

          ; Restart unfolding signal:
            mov ecx 0-1

        Else
          ; job is done. strip lasting separator:
            dec edi
          ; if some more text after loop symbol (another one is ready from source)
            On ecx > 0, dec ecx

        End_If
        ret

    ..Else_If al = 'X'
        mov al B$MacrosX | On al >= 080, neg al
        inc al | call AlToDecimal

    ..Else_If al = 'N'
        mov al B$NumberOfMacroParameters
        call AlToDecimal

    ..Else_If al = '='
      ; Case of "#=4", for forced Parameters control:
        dec ecx | On ecx = 0, error D$MacParaPtr
        lodsb | sub al '0' | On al > 9, error D$MacNumberPtr
        add al '0' | On B$LastMacPara <> al, error D$MissingParameterPtr
        lodsb | dec ecx | On ecx = 0, error D$MacNumberPtr

    ..Else_If B$esi = '<'
        error D$BadMacroDirectionPtr

    ..Else_If B$esi = '>'
        mov bl B$LastMacPara

        If al <= '9'
          ; 'x' expressed number out of parameter range?
            cmp al bl | ja L3>>
        End_If

        .If bl > '9'
          ; #x>y found (x in AL)
            call MultiParametersUnfolding
            dec edi
        .Else_If B$esi+1 = 'L'
            call MultiParametersUnfolding
            dec edi
        .Else_If B$esi+1 <= bl
            call MultiParametersUnfolding
            dec edi
        .Else
L3:         Do
              ; if no parameter fitting with parameter in...
                dec edi

                If B$edi = ColonSign
                    inc edi | jmp C2>
                End_If
            Loop_Until B$edi <= EOI
          ; previous possible mnemonic written at L0:
C2:         add esi 2 | sub ecx 2
        .End_If

    ..Else
      ; #1/.../#9/#F/#L   found:
        call ParameterUnfolding
    ..End_If

L6: dec ecx | ret

____________________________________________________________________________________________



ParserMacroVariableByNumber:
    dec ecx
    call GetMacroVariableDis
    .If B$esi = '='
      ; strip '=', keep '1' in al
        inc esi | dec ecx
        If B$esi = Space
            inc esi | dec ecx
        End_If

        call StoreMacroVariableByNumber

        If ecx = 0
            mov D$edi 'NOPE' | add edi 4
        Else
          ; Strip next '|' (End >>> None >>> 'jg':
            inc esi | dec ecx
        End_If

    .Else
        call WriteMacroVariableByNumber

    .End_If
ret


ParseMacroVariable:
  ; For the previous '&':
    dec ecx
    call GetMacroVariableDis | shl eax 7
    .If B$esi = '='
        inc esi | dec ecx   ; strip '='
        call StoreMacroVariable

      ;  If ecx = 0
      ;      mov D$edi 'NOPE' | add edi 4
      ;  Else
      ;    ; Strip next '|' (End >>> None >>> 'jg':
      ;      inc esi | dec ecx
      ;  End_If

    .Else
        call WriteMacroVariable

    .End_If
ret

 ________________________________________________________________________________________
;;
; NestingCheck compares source before and after unfolding (sourceA / SourceB)
; if something is different , a macro job has been done. We loop all this job
; the stupid way until it be found of no more use.
; When NestingCheck is called, ESI points either to SourceCodeB or to MacroData,
; depending of the replacement work: as text from ESI could be either shorter or
; longer than new text in EDI, a lenght for shorter > ECX > REPE CMPSB can't be known...
; We can't check if parameters were all used because it is allowed to transmit dummy
; parameters (parameters that would be written in macro evocation only for ease of read
; purpose, for exemple).
;
;
; Infinite loop is not completly impossible. exemple:
;        [ mov | mov #2 #1 ]
;          mov eax, ebx      ; >>>>  mov ebx, eax ... and so on.
;
; The only one solution i choose is to stop unfolding after a certain iterations number:
;;

[MacroJobIsOver: B$ ?]

NestingCheck:
    push esi, ecx, eax, edi
        mov esi D$InstructionAptr, edi D$InstructionBptr, ecx 0

        mov ah B$esi-1        ; exemple: with  [Api | push #L>2 | call #1], an
        mov al B$edi-1        ; api call without parameter will be authorized :
        cmp ax ((EOI shl 8)+meEOI) | jne L0>
        ;cmp ax 0201 | jne L0> ; no error message. Compute > (02h)call(03h)Function(02)
            mov B$edi-1 EOI    ; and not:                    (01h)call(03h)Function(02)

L0:     mov ah B$esi,  al B$edi
        ;and eax 00_01111111__01111111
        inc esi | inc edi
        cmp ax ((EOI shl 8)+meEOI) ;0201 ; this appends when a nested macro evocation does'nt have fitting
            jne L1>             ; transmited parameters and is authorized (L>2 for example)
        mov B$edi 2 | mov al 2
L1:     cmp ah al | jne L2>                   ; AH <> AL          >  Job is not over
            cmp al meEOI | je L9>
            cmp al EOI | je L9>               ; AH = AL = '|' (12)    >  Job is over
            jmp L0<
L2:     On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr
        mov D$MacroJobIsOver &FALSE
L9: pop edi, eax, ecx, esi
ret


ReplaceOneMacro: ; esi = D$InstructionB, edi = D$InstructionA
    movsb | jmp L1>                           ; first separator

L0: stosb

L1: lodsb | IfItIsText L0<
        cmp al EOI | je N0>
            cmp al LowSigns | jb L0<          ; meEOI...
                dec esi | jmp L2>
N0:     cmp B$esi EOI | jne L2>               ; end marker = '||' > write it
            stosb | stosb | ret               ; and exit

L2: mov D$InstructionAptr esi,  D$InstructionBptr edi,  D$OneWordLen 0

L3: lodsb | cmp al LowSigns | jb L4>          ; search now lenght of text word:
        inc D$OneWordLen | jmp L3<            ; lenght of one text word in OneWordLen

; now: one source word is pointed by InstructionBptr and OneWordLen contain the length.

L4: cmp al ColonSign | je L9>>                 ; is it a line label?
    mov esi D$InstructionAptr

    and B$esi 00_0111_1111

    call GetFromQwordCheckSum esi, D$MacroList, D$MacroListLimit
    cmp eax 0 | je L5>

        While B$eax > LowSigns | inc eax | End_While | inc eax
        mov esi D$eax, ecx D$eax+4

        call MacroWithIf

        call ReplaceFromMacroData

        call NestingCheck

        mov esi D$AfterLastParameter | jmp L1<<

  ; direct writing:
L5: mov esi D$InstructionAptr,  edi D$InstructionBptr

L6: lodsb | cmp al EOI | jbe L7>
        stosb | jmp L6<
L7:     dec esi | jmp L1<<

  ; write line labels:
L9: mov esi D$InstructionAptr,  edi D$InstructionBptr
L8: lodsb | cmp al ColonSign | je L9>
        stosb | jmp L8<

L9: dec esi | jmp L1<<
____________________________________________________________________________________________

ReplaceEquates:
    movsb | jmp L1>                             ; strip first separator
L0: stosb
L1: lodsb | cmp al TextSign | jne C5>
        stosb
C0: lodsb | stosb | cmp al TextSign | jne C0<
        jmp L1<

C5: cmp al EOI | ja L2>                         ; all possible separators.
C6:     cmp B$esi EOI | jne L0<                 ; end marker = '||' > write it
            stosb | stosb
            ret

L2: cmp al LowSigns | jb L0<
    dec esi | mov D$InstructionBptr esi,  D$InstructionAptr edi,  D$OneWordLen 0

L3: lodsb | cmp al LowSigns | jb L4>            ; search now lenght of text word
        inc D$OneWordLen | jmp L3<              ; > lenght in 'OneWordLen'

  ; now: one source word is pointed by InstructionBptr and OneWordLen contain the length
L4: mov esi D$InstructionBptr
    test B$esi 00_1000_0000 | jnz L6>>           ; to avoid testing words not in list
    cmp B$esi '0' | jb L4>
    cmp B$esi '9' | ja L4>                     ; No need Parsing Numbers.
        mov ecx 0 | jmp L5>

L4: ;call IsItInEquateList                       ; > MacroData adress in esi, len in ecx
   ; On al = ColonSign, inc D$OneWordLen

    cmp al ColonSign | jne L4>
        mov ecx 0 | jmp L5>

L4: call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

    mov ecx eax | cmp ecx 0 | je L5>
        While B$eax > LowSigns | inc eax | End_While | inc eax
        mov esi D$eax, ecx D$eax+4

L5: cmp ecx 0 | ja L5>                          ; ecx = lenght from 'IsItInMacroList'
        mov esi D$InstructionBptr
        or B$esi 00_1000_0000                   ; word was not in list > flag cancel next time
    jmp L6>

  ; write equate body from MacroData (equ and mac body are all in Macrodata)
L5: mov edi D$InstructionAptr | rep movsb | mov D$InstructionAptr edi
    mov esi D$InstructionBptr | add esi D$OneWordLen | mov D$InstructionBptr esi
    mov D$MacroJobIsOver &FALSE | jmp L1<<

L6: ; direct writing from source:
    mov esi D$InstructionBptr,  edi D$InstructionAptr,  ecx D$OneWordLen | rep movsb
    mov D$InstructionBptr esi,  D$InstructionAptr edi | jmp L1<<


NewReplaceEquates:
    mov D$MacroJobIsOver &TRUE, esi D$InstructionA, edi D$InstructionB

    .While B$esi <> 0
        mov al B$esi

        ..If al = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While

        ..Else_If al < LowSigns
            movsb

        ..Else_If al >= '0'
            .If al <= '9'
              ; Skip numbers:
L1:             While B$esi > LowSigns | movsb | End_While

            .Else
              ; Skip non-Equates:
                test B$esi 00_1000_0000 | jnz L1<

              ; Is it in EquateList:
                call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

                If eax = 0
                  ; Not an Equate:
                    or B$esi 00_1000_0000 | jmp L1<

                Else
                  ; Equate body in 'MacroData' (equ and mac body are all in 'Macrodata'):
                    While B$eax > LowSigns | inc eax | End_While | inc eax
                    push esi
                        mov esi D$eax, ecx D$eax+4
                        rep movsb
                    pop esi

                    mov D$MacroJobIsOver &FALSE
                  ; Skip the parsed Equate:
                    jmp L1<

                End_If
            .End_If

        ..End_If
    .End_While

    mov B$esi 0
ret





    movsb | jmp L1>                             ; strip first separator
L0: stosb
L1: lodsb | cmp al TextSign | jne C5>
        stosb
C0: lodsb | stosb | cmp al TextSign | jne C0<
        jmp L1<

C5: cmp al EOI | ja L2>                         ; all possible separators.
C6:     cmp B$esi EOI | jne L0<                 ; end marker = '||' > write it
            stosb | stosb
            ret

L2: cmp al LowSigns | jb L0<
    dec esi | mov D$InstructionBptr esi,  D$InstructionAptr edi,  D$OneWordLen 0

L3: lodsb | cmp al LowSigns | jb L4>            ; search now lenght of text word
        inc D$OneWordLen | jmp L3<              ; > lenght in 'OneWordLen'

  ; now: one source word is pointed by InstructionBptr and OneWordLen contain the length
L4: mov esi D$InstructionBptr
    test B$esi 00_1000_0000 | jnz L6>>           ; to avoid testing words not in list
    cmp B$esi '0' | jb L4>
    cmp B$esi '9' | ja L4>                     ; No need Parsing Numbers.
        mov ecx 0 | jmp L5>

L4: ;call IsItInEquateList                       ; > MacroData adress in esi, len in ecx
   ; On al = ColonSign, inc D$OneWordLen

    cmp al ColonSign | jne L4>
        mov ecx 0 | jmp L5>

L4: call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

    mov ecx eax | cmp ecx 0 | je L5>
        While B$eax > LowSigns | inc eax | End_While | inc eax
        mov esi D$eax, ecx D$eax+4

L5: cmp ecx 0 | ja L5>                          ; ecx = lenght from 'IsItInMacroList'
        mov esi D$InstructionBptr
        or B$esi 00_1000_0000                   ; word was not in list > flag cancel next time
    jmp L6>

  ; write equate body from MacroData (equ and mac body are all in Macrodata)
L5: mov edi D$InstructionAptr | rep movsb | mov D$InstructionAptr edi
    mov esi D$InstructionBptr | add esi D$OneWordLen | mov D$InstructionBptr esi
    mov D$MacroJobIsOver &FALSE | jmp L1<<

L6: ; direct writing from source:
    mov esi D$InstructionBptr,  edi D$InstructionAptr,  ecx D$OneWordLen | rep movsb
    mov D$InstructionBptr esi,  D$InstructionAptr edi | jmp L1<<

____________________________________________________________________________________________


_________________________________________________________________________________________
;;
 As many full passes as needed: one full for equate, one full for macros, ... and loop
 until job is found to be of no more use; this is to say when source and destination
 are the same. All this process is so made one more time than really needed. RosAsm
 Spoil most of compile time here. I have not found something faster with same flexibility.
 Setting a high bit on 'non equates words' saves about 1/7 of compile time.
;;
_________________________________________________________________________________________

ClearDoneHighBit:
    mov esi D$CodeSourceA, ecx D$StripLen, B$InsideText &FALSE
L0: On B$esi = TextSign, xor B$InsideText &TRUE
    On B$InsideText = &FALSE, and B$esi 00_0111_1111
L9: inc esi | loop L0<
  ret


ReplaceEquOnly:
    mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    mov D$MacroJobIsOver &TRUE
    mov esi D$InstructionA, edi D$InstructionB

    call ReplaceEquates                         ; SourceB  >  SourceA

    mov ecx edi | sub ecx D$InstructionB
    mov D$StripLen ecx

    If B$MacroJobIsOver = &FALSE
        Exchange D$InstructionA D$InstructionB
        Exchange D$InstructionAEnd D$InstructionBEnd | jmp L0<<
    End_If

    mov esi D$InstructionB, edi D$InstructionA
    call ExpressionParser                       ; SourceA  >  SourceB
    mov ecx edi | sub ecx D$InstructionA | mov D$StripLen ecx

 ;  cmp D$MacroJobIsOver &TRUE | jne L0<        ; set by 'NestingCheck'
ret                                             ; when over > SourceB


NewReplaceEquOnly:
    mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    call NewReplaceEquates                         ; SourceB  >  SourceA

    mov ecx edi | sub ecx D$InstructionB
    mov D$StripLen ecx

    If B$MacroJobIsOver = &FALSE
        Exchange D$InstructionA D$InstructionB
        Exchange D$InstructionAEnd D$InstructionBEnd | jmp L0<<
    End_If

    mov esi D$InstructionB, edi D$InstructionA
    call ExpressionParser                       ; SourceA  >  SourceB
    mov ecx edi | sub ecx D$InstructionA | mov D$StripLen ecx

 ;  cmp D$MacroJobIsOver &TRUE | jne L0<        ; set by 'NestingCheck'
ret                                             ; when over > SourceB




ReplaceMacAndEqu:
    mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    mov D$MacroJobIsOver &TRUE
    mov esi D$InstructionA, edi D$InstructionB

        call ReplaceEquates ; zReplaceEquates

        mov ecx edi | sub ecx D$InstructionB | mov D$StripLen ecx
            If B$MacroJobIsOver = &FALSE
                Exchange D$InstructionA D$InstructionB
                Exchange D$InstructionAEnd D$InstructionBEnd | jmp L0<<
            End_If

            .Do
                mov B$MacroModifiedExpression &FALSE

                    call ParaMacrosParser

                        mov esi D$InstructionB, edi D$InstructionA

                        call ExpressionParser

                          ; We have to do this here (and not before the RET of
                          ; 'ExpressionParser') because 'ExpressionParser' is
                          ; also called from 'ReplaceEquOnly':
                            mov ecx edi | sub ecx D$InstructionA | mov D$StripLen ecx
                            Exchange D$InstructionA D$InstructionB
                            Exchange D$InstructionAEnd D$InstructionBEnd
                 inc B$UnfoldingRotations
                 On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

            .Loop_Until B$MacroModifiedExpression = &FALSE

        mov esi D$InstructionB, edi D$InstructionA

        call ReplaceOneMacro

        ;;;;;;;;;;;;;;;;;;; Remove EOI, meEOIs and Spaces right after an EOI:
        mov D$edi 0

        mov esi D$InstructionA, edi esi
        .While B$esi <> 0
            lodsb | stosb
            If al < Separators
                While B$esi = meEOI | inc esi | End_While
            End_If
        .End_While
        ;;;;;;;;;;;;;;;;;;

        mov ecx edi | sub ecx D$InstructionA

      ; If D$InstructionA = EOI, EOI (Cases of empty outputs - impossible actually -):
L2:     If ecx <= 2
            mov edi D$InstructionA, B$edi EOI, D$edi+1 'NOPE', B$edi+5 EOI
            mov D$StripLen 6 | ret
        End_If

        If ecx <> D$StripLen
            mov D$MacroJobIsOver &FALSE
           ; On B$UnfoldingRotations = 0FF, hexprint 6
            On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr
        End_If
        mov D$StripLen ecx

L9: cmp D$MacroJobIsOver &TRUE | jne L0<<                          ; set by 'NestingCheck'
ret                                                                ; when over > SourceB

____________________________________________________________________________________________

zReplaceEquOnly:
    mov B$UnfoldingRotations 0
L0: inc B$UnfoldingRotations | On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr

    call zReplaceEquates

    mov ecx edi | sub ecx D$InstructionB
    mov D$StripLen ecx

    Exchange D$InstructionA D$InstructionB,
             D$InstructionAEnd D$InstructionBEnd

    On B$MacroJobIsOver = &FALSE, jmp L0<<

    call zExpressionParser
ret


zReplaceEquates:
    mov D$MacroJobIsOver &TRUE, esi D$InstructionA, edi D$InstructionB

    .While B$esi <> 0
        mov al B$esi

        ..If al = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While | movsb

        ..Else_If al < LowSigns
            movsb
            On B$esi = '0', jmp L1> ; In case, if the 1st found byte is '0', it obviusly anumbers, and also means
                                    ; it is TextSign, so simply copy the whole data.
        ..Else_If al < '0'
          ; Example: '.If':
            jmp L2>

        ..Else_If al >= '0'
            .If al <= '9'
              ; Simple copy (numbers, non-Equates):
L1:             While B$esi > LowSigns | movsb | End_While

            .Else
              ; Skip non-Equates:
L2:             test B$esi 00_1000_0000 | jnz L1<

              ; Is it in EquateList:
                call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit
                If eax = 0
                    ; Not an Equate:
                    ; the OR will only happens if the Byte is > '9' and <= LowSigns
                    On B$esi <= LowSigns, or B$esi 00_1000_0000
                    jmp L1<

                Else
                  ; Equate body in 'MacroData' (equ and mac body are all in 'Macrodata'):
                    While B$eax > LowSigns | inc eax | End_While | inc eax
                    push esi
                        mov esi D$eax, ecx D$eax+4
                        rep movsb
                    pop esi

                    mov D$MacroJobIsOver &FALSE
                  ; Skip the done Equate:
                    While B$esi > LowSigns | inc esi | End_While

                End_If
            .End_If

        ..End_If
    .End_While

    mov B$edi 0
ret



zExpressionParser:
    mov esi D$InstructionA, edi D$InstructionB
    mov edx esi | add edx D$Striplen | inc edx
    mov B$InsideExpression &FALSE, B$RealHexaSize 0
    mov ebx 0, ecx 0

L0: .While esi < edx
        lodsb

        ..If al = TextSign
            stosb | While B$esi <> TextSign | movsb | End_While | movsb | jmp L0<

        ..Else_If al = OpenSign
            .If B$InsideExpression = &FALSE
                cmp B$esi-2 memMarker | jne L3>
                mov al B$esi-3 | and al 00_0111_1111 ; Mask Equates and Macros Done Flag.
                cmp al 'F' | jne L1>
                    mov B$edi-2 'D', B$RealHexaSize 8 | jmp L2>
L1:             cmp al 'R' | jne L3>
                    mov B$edi-2 'Q', B$RealHexaSize 16
L2:                 mov B$RealExpression &TRUE | jmp L4>
L3:             mov B$RealExpression &FALSE
L4:             mov ebx 0, ecx 0
                mov D$StartOfSourceExpression esi, D$StartOfDestinationExpression edi
            .End_If
            inc ebx | inc ecx | mov B$InsideExpression &TRUE

        ..Else_If al = CloseSign
            On ecx = 0, error D$ParenthesisPtr
            dec ecx

        ..Else_If al = 0
            On B$InsideExpression = &TRUE, error D$ParenthesisPtr
            call CheckBracketExpression

        ..End_If

        ..If ebx > 0
            .If ecx = 0
                push edx
                    If B$RealExpression = &TRUE
                        call ComputeRealExpression
                    Else
                        call ComputeExpression
                    End_If
                    call zWriteExpressionResult
                    mov ebx 0, ecx 0, B$InsideExpression &FALSE
                    mov B$MacroModifiedExpression &TRUE
                pop edx
                jmp L0<<
            .End_If
        ..End_If

        stosb
    .End_While

    mov ecx edi | sub ecx D$InstructionB | mov D$StripLen ecx

    Exchange D$InstructionA D$InstructionB,
             D$InstructionAEnd D$InstructionBEnd
ret


zWriteExpressionResult:
    push esi
        mov esi D$ExpressionA, edi D$StartOfDestinationExpression

        While B$esi > 0
            movsb
        End_While
    pop esi
ret
____________________________________________________________________________________________


;;
        mov ecx edi | sub ecx D$InstructionA

      ; If D$InstructionA = EOI, EOI (Cases of empty outputs - impossible actually -):
        If ecx <= 2
            mov edi D$InstructionA, B$edi EOI, D$edi+1 'NOPE', B$edi+5 EOI
            mov D$StripLen 6 | ret
        End_If

        If ecx <> D$StripLen
            mov D$MacroJobIsOver &FALSE
           ; On B$UnfoldingRotations = 0FF, hexprint 6
            On B$UnfoldingRotations = 0FF, error D$InfiniteLoopPtr
        End_If
        mov D$StripLen ecx

L9: cmp D$MacroJobIsOver &TRUE | jne L0<<                          ; set by 'NestingCheck'
ret                                                                ; when over > SourceB
;;

;;
 In previous versions, 'ReplaceMacAndEqu' was called from AsmMain and all replacement job
 was done at once. This new version cuts the job in as tiny parts as possible in order
 to avoid no use loop analyzes.

 First, as Data brackets are stored at the begining of the cleaned source (and can't
 contain any Macro), we compute them one set by one set, only Equates replacement.

 Second, each instruction is done in turn. All this saves about 15% of replacement job.
 2 wide tables are used to store the parts, and exchanges are done between the pointers
 just because i do not wish to rewrite it all for more accurate namings.
;;

[InstructionA: ?  InstructionB: ?   InstructionAEnd: ?  InstructionBEnd: ?
 CodeSourceBpointer: ?  CodeSourceApointer: ?
 OpenType: ?  CloseType: ?]




;;
  NewReplaceMacAndEqu
  
        zReplaceEquOnly
                zReplaceEquates
                zExpressionParser
        
        ReplaceMacAndEquStatement
                zReplaceEquates
                ParaMacrosParser
                        UnfoldDataParameter
                        UnfoldMacroParameter
                zExpressionParser
                        CheckBracketExpression
                        ComputeRealExpression
                        ComputeExpression
                        WriteExpressionResult
                ReplaceOneMacro
                        MacroWithIf
                        ReplaceFromMacroData
                        NestingCheck
;;

NewReplaceMacAndEqu:
  ; Initializations:

    call ClearMacroVariable

    VirtualAlloc InstructionA D$StripLen
    add eax D$StripLen | Align_On 01000 eax | mov D$InstructionAEnd eax

    VirtualAlloc InstructionB D$StripLen
    add eax D$StripLen | Align_On 01000 eax | mov D$InstructionBEnd eax

    mov B$ErrorLevel 0, D$BracketCounter 0

    move D$StatementsPtr D$StatementsTable,
         D$CodeSourceBpointer D$CodeSourceB,
         D$CodeSourceApointer D$CodeSourceA

    mov edi D$CodeSourceBpointer

;;
  The Source has been re-organized: Brackets first // Code Statements last. The
  Brackets sets do not need anything else but the Equates jobs. So, we parse them,
  first. Once an EOI is encounted, this first job is over:
;;

L0: mov esi D$CodeSourceApointer, edi D$InstructionA
    cmp B$esi EOI | je L0>>

  ; Take a copy one Bracket Statement:
    mov al B$esi ; copy B$esi to al. Forget the lodsb because it will increment esi, bypassing the good pointer
    mov B$OpenType al; <--- I added only this to it points to the proper place on the next instruction, so what is being copied to edi is the starting byte (014)

    Do
        lodsb | stosb
        If al = TextSign
L1:         lodsb | stosb | cmp al TextSign | jne L1<
        End_If
        On B$esi-1 = CloseVirtual, jmp C1>
    Loop_Until B$esi-1 = CloseBracket

C1: mov al B$esi-1, B$CloseType al
  ; Keep track of the real Source:
    mov D$CodeSourceApointer esi
  ; Set an end-mark:
    mov B$edi 0 ; <-------- Here i added too.. We need to set the last byte to 0, and not edi-1.
                ; Othewise we are erasing the good final ending mark (edi was already incremented during the loop,
                ; so the proper place is edi and not edi-1

    inc D$BracketCounter

    call zReplaceEquOnly ; <<<<<<<<<<<<<<<<

    add D$StatementsPtr 4

  ; Copy the Bracket Statement back to CodeSourceB:
    mov esi D$InstructionA, edi D$CodeSourceBpointer

    mov al B$OpenType | stosb
    inc esi ; <---- Ok we want to copy to edi, but on stosb we already copied the 1st byte 014 to edi. So, to avoid duplications, we simply increment esi to the next byte
    While B$esi <> 0 | movsb | End_While ; copy the whole thing to edi
    dec edi ; decrease edi to we put the proper close type here
    mov al B$CloseType | stosb ; put the CoseType at the real end of our data. (After the string)

    mov D$CodeSourceBpointer edi | jmp L0<<
____________________________________________________

L0: mov esi D$CodeSourceApointer
    sub D$StatementsPtr 4
    mov B$ErrorLevel 0, D$StatementsCounter 0

[LenghtOfBracketStatements: ?]

    mov eax D$CodeSourceApointer | sub eax D$CodeSourceA | mov D$LenghtOfBracketStatements eax

  ; Take a copy for the Macros jobs:
L0: mov edi D$InstructionA

    mov al EOI | stosb
    Do
        movsb
    Loop_Until B$esi-1 = EOI

    mov D$CodeSourceApointer esi | mov al EOI | stosb | stosb

    inc D$StatementsCounter | call ReplaceMacAndEqu  ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    add D$StatementsPtr 4

    mov esi D$InstructionA, edi D$CodeSourceBpointer | lodsb

L5: lodsb | stosb | cmp al EOI | jne L5<

    mov D$CodeSourceBpointer edi | mov esi D$CodeSourceApointer | cmp B$esi EOI | jne L0<<
    movsb

    Exchange D$CodeSourceA D$CodeSourceB

    push edi
        inc edi | sub edi D$CodeSourceA | mov D$StripLen edi
    pop edi
    mov eax ((EOI shl 24)+(EOI shl 16)+(EOI shl 8)+EOI)  | stosd   ; security

    VirtualFree D$InstructionA, D$InstructionB
    call ClearDoneHighBit
ret

____________________________________________________________________________________________
;;
  'NewBrackets' makes nested Declarations of '[...]' available for next Equates and
  Macros job pass. We have to replace all substitutes by the true signs ('{' > '[',
  and so on). But we have a difficult problem to solve first: These new Declarations
  will be move to top of 'file' by next treatements, and in case of user error search
  among these new created statements, we have to be able to point out the wrong
  original Statement (the user macro Evocation). So have we to add some source pointers
  in 'StatementsTable', with the same pointer value as the one of user Macro. One
  added difficulty is that, is some case, exemple:

  > [CreateEquatesMacro | {#1 #2} | +2]

  ... the original Macro was stored as a Code Statement into 'StatementsTable'. After
  unfolding, there is no more code at this place. So have we to strip one record for
  this, in such cases:
;;
[MoreBracket: ?    InsideNewBracket: ?    TrueCodeInside: ?]

StatementStep:
    mov ebx D$StatementsPtr, ebx D$ebx
    mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4

   ; mov edx D$StatementsPtr+4
   ; While D$edx = ebx
   ;     mov edx D$StatementsPtr2, D$edx ebx
   ;     add D$StatementsPtr 4 | add D$StatementsPtr2 4
   ;     add D$StatementsPtr 4
   ;     mov edx D$StatementsPtr
   ; End_While
ret

NewBrackets:
    mov esi D$CodeSourceA
    mov ecx esi | add ecx D$Striplen                    ; end of source adress

    move D$StatementsPtr D$StatementsTable, D$StatementsPtr2 D$StatementsTable2

    ..While esi < ecx
L0:    lodsb
;On D$esi = 'ENTS', int3
       ...If al = TextSign
L1:         lodsb | cmp al TextSign | jne L1<

       ...Else_If al = '{'
         ; Possible "Bracket only" statement:
      ; On D$esi+15 = 'BIDO', int3
       ; MainWindowProc@BIDON
           ..If B$esi-2 = EOI
               sub D$StatementsPtr 4 | sub D$StatementsPtr2 4
               mov B$TrueCodeInside &FALSE
               .While al <> EOI
                   If al = '{'
                       mov ebx D$StatementsPtr, ebx D$ebx
                       mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
                       While al <> '}'
                           On al = textSign, call SkipText
                           lodsb
                       End_While
                      ; dec esi ; <<<<<<<< New add while tracking the "Unfolder bug".

                   Else_If al = meEOI
                       On B$esi = EOI, jmp L2>
                       On B$esi <> '{', mov B$TrueCodeInside &TRUE

                   Else_If al = TextSign
                       call Skiptext

                   End_If

L2:                lodsb
               .End_While

               If B$TrueCodeInside = &TRUE
                   mov ebx D$StatementsPtr, ebx D$ebx
                   mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
               End_If
               add D$StatementsPtr 4
               dec esi

           ..Else
             ; (B$esi-2 = meEOI // al = '{') ---> code sure inside...
               sub D$StatementsPtr 4
               .While al <> EOI
                   If al = '{'
                       mov ebx D$StatementsPtr, ebx D$ebx
                       mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
                       While al <> '}'
                            On al = textSign, call SkipText
                            lodsb
                       End_While

                   End_If
L2:                lodsb
               .End_While

               dec esi
               add D$StatementsPtr 4

           ..End_If

       ...Else_If al = OpenBracket
           mov ebx D$StatementsPtr, ebx D$ebx | add D$StatementsPtr 4
           mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4

       ...Else_If al = OpenVirtual
           mov ebx D$StatementsPtr, ebx D$ebx | add D$StatementsPtr 4
           mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4

       ...Else_If al = EOI
           On B$esi = OpenVirtual, jmp L3>
           On B$esi = OpenBracket, jmp L3>
               mov ebx D$StatementsPtr, ebx D$ebx | add D$StatementsPtr 4
               mov edx D$StatementsPtr2, D$edx ebx | add D$StatementsPtr2 4
L3:
       ...End_If

    ..End_While

    mov eax D$StatementsPtr2, D$eax 0
    Exchange D$StatementsTable D$StatementsTable2
________________________

; Now, we simply replace substitutes by true Chars. Use of BL reg as a 'Level Counter'
; in order to allow infinite nestings.

    mov B$MoreBracket &FALSE, B$InsideNewBracket &FALSE
    mov esi D$CodeSourceA,  edi D$CodesourceB, ebx 0

    .While esi < ecx
L0:    lodsb
       .If al = TextSign
         ; Skip Text:
L1:        stosb | lodsb | cmp al TextSign | jne L1<

       .Else_If al = '{'
           inc bl | On bl > 1, jmp L8>
           xor B$InsideNewBracket &TRUE
           On B$esi = Space, lodsb
           mov al OpenBracket, B$MoreBracket &TRUE

       .Else_If al = '}'
           dec bl | jnz L8>
           xor B$InsideNewBracket &TRUE
           On B$esi = Space, lodsb
           ;On B$edi-1 = Space, dec edi
           ;On B$edi-1 = meEOI, dec edi
           On B$edi-1 < Separators, dec edi
           mov al CloseBracket

       .Else_If al = '%'
           If B$InsideNewBracket = &TRUE
               On bl = 1, mov al NumSign
               On bl = 2, mov al NumSign

           End_If

       .End_If

L8:    stosb

    .End_While

    mov eax ((EOI shl 24)+(EOI shl 16)+(EOI shl 8)+EOI) | stosd
    mov eax edi | sub eax D$CodeSourceB | mov D$Striplen eax

   ; call TestStatementsTable
    Exchange D$CodeSourceA D$CodeSourceB
ret


SkipText:
    lodsb | While al <> TextSign | lodsb | End_While
ret
__________________________________________________________________________________________
__________________________________________________________________________________________

; PE Import section construction

; uses 3 tables:   ApiListA to store zero ended api calls
;                  ApiListB to store function name with a number at first for Dll
;                  DllList  to store Dll names
__________________________________________________________________________________________


[ALEOD 0FF]       ; Api Lists End Of Data


[Align_on | add #2 #1-1 | and #2 0-#1]
; Same for Aligning on a Variable-defined-Alignment (eax broken):
[Align_on_Variable | push eax
                         mov eax #1 | dec eax | add #2 eax | xor eax 0-1 | and #2 eax
                     pop eax]


[DllNumber: 0  FunctionNumber: 0  ImportHeaderPtr: 0   ImportTablePtr: 0]


InitApiTables:
   mov ax 0FF00                               ; in memory: 00 FF end mark
   mov edi D$ApiListA | mov W$edi ax
   mov edi D$ApiListB | mov W$edi ax
   mov edi D$DllList  | mov W$edi ax
ret
;;
 Test that api call is in the good form:  CALL 'LIB.Function' (one point, no space, not
 open text and turn dll name upper case to prevent from double storage).
 Return: EBX = lenght / ECX = lenght-1  (- last "'")
;;
TestGoodApiCall:
    push esi
      mov ecx 0,  ebx 0                       ; ebx = '.' counter (must be 0 / 1 / or 2)
      push esi
        While B$esi <> TextSign | On B$esi = '.', inc ebx | inc esi | End_While
      pop esi
      On ebx = 0, jmp L9>

      mov ebx 0
L0:   lodsb | inc ecx | cmp ebx, 0 | ja L1>   ; if ebx = 0  >>>  before '.'  >>>  dll name
        cmp al 'a' | jb L1>
        cmp al 'z' | ja L1>
          sub al 32                     ; turn upper case dll name
          mov B$esi-1 al                ; rewrite CodeSourceB
L1:   On al = '.',  inc ebx
      cmp al Space | je L8>
      cmp ecx 1600 ; 150 ; 125 ; 120 |
      je L8>              ; too long
      cmp al TextSign | jne L0<
      cmp ebx 3 |  jb L9>               ; only one '.' wanted
                                        ; ... or 2 in case of Module given Extension...
L8:       mov B$ErrorLevel 6
          mov edi D$ApiListA            ; just to store zero ended name for error search
          pop esi
              While B$esi > TextSign
                  movsb
              End_While
              mov al 0 | stosb
              mov esi D$ApiListA
              Error D$BadApiPtr

L9: pop esi
ret
 ________________________________________________________________________________________

 ; storage of all encounted api calls in ApilistA:

StoreApi:                                ; 5 =  CALL 'LIBRARY.Function'
  ; add esi 5                            ;       .....^
    call TestGoodApiCall
L0: lodsb | cmp al TextSign | je L1>
      stosb | jmp L0<                    ; write in ApilistA
L1: mov al 0 | stosb
ret


; Simple copy of api calls into 'ApiListA' by 'StoreApi'.
; 'ApiListA' will be: LIBRARY.Function, 0, LIBRARY.Function, 0, ... 0FF

SearchForApis:
    mov esi D$CodeSourceB,  edi D$ApiListA

    ;add esi D$LenghtOfBracketStatements

    While B$esi <> EOI | inc esi | End_While | inc esi

  ; Regular Rosasm Api calls:
L0: ...If D$esi = 'CALL'        ; CALL '
L1:     If W$esi+4 = ((TextSign shl 8)+Space)
            add esi 6 | call StoreApi | jmp L0<
        End_If
;;
  For fancyful things for Disassemblies re-compilation: Store in eax +1 for each TextSign,
  +1 for each '.', +1 if lenght > 6.
  
  Must have: 1 or two '.' // 2 TextSigns // be longer than '..xx' (6 Chars).
;;
    ...Else_If B$esi = Textsign
        inc esi | mov eax 1, ebx esi

        While B$esi > LowSigns
            If B$esi = '.'
                inc ah
           ; Else_If B$esi < '0'  ; Dement Chars in some Api Names !!!!.... :(
           ;      jmp L5>
            Else_If B$esi > 'z'
                jmp L5>
            End_If

            inc esi
        End_While

        On B$esi = TextSign, inc al

        On ah = 2, dec ah       ; 2 '.' allowed in:  mov eax 'MODULE.ext.Funtion'

        .If eax = 01_02
            mov ecx esi | sub ecx ebx
            If ecx > 6
                mov esi ebx | call StoreApi | jmp L0<<
            End_If
        .End_If

    ...End_If

L5: lodsb | cmp al EOI | jne L0<<
    cmp B$esi EOI | jne L0<<                     ; End of file reached?

    .If edi = D$ApiListA
        On D$SavingExtension = '.DLL', jmp L9>

            call 'USER32.MessageBoxA' D$hwnd,
            {"Are you sure you want to continue assemblying this?" 0},
            D$NoApiPtr, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO
            If eax = &IDNO
                mov B$CompileErrorHappend &TRUE, D$NextSearchPos 0
                call CloseProgressBar
                cld | mov esp D$OldStackPointer
                call ReleaseAsmTables
                ret ; for 'no search' errors (esi known) or 'Main:' missing
           End_If
           jmp L9>
    .End_If

    mov al ALEOD |  stosb                       ; end mark for ApiList (strings are zero ended)
L9: ret

;;
'ApiListA' may contain 3 different forms o api calls: 
> MODULE.Function, 0, MODULE.ext.Function, 0, Function, 0...
We turn them all into:
> MODULE.ext.Function, 0, MODULE.ext.Function, 0, MODULE.ext.Function, 0...
;;
FullfillApiList:
    mov D$ModuleHandlesListPointer ModuleHandlesList

  ; First generalise the found MODULE(s) to all naked 'Function'(s)
    mov esi D$ApiListA, edi D$ApiListB

    call FullfillWithExtensions

    mov esi D$ApiListA | While B$esi = 0 | inc esi | End_While

    call FullfillWithoutExtensions

    mov B$edi ALEOD | Exchange D$ApiListA D$ApiListB

    mov ebx D$ModuleHandlesListPointer, D$ebx 0

; Test for viewing the new Api List:
;;
    mov esi D$ApiListA
    .While B$esi <> ALEOD
        howMe esi
        While B$esi <> 0 | inc esi | End_While | inc esi
    .End_While
    Error BadApi
;;

  ; Now, verify that 'ApiListB' is entirely zeroed:

    mov esi D$ApiListB
    .While B$esi <> ALEOD
        If B$esi <> 0
            mov B$ErrorLevel 6 | Error D$BadApiPtr
        End_If
        inc esi
    .End_While
ret


FullfillWithExtensions: ; 'FullfillWithoutExtensions'
    ..While B$esi <> ALEOD
        mov ebx esi, edx 0, ecx 0

        While B$esi <> 0
            inc esi
            .If B$esi = '.'
                If edx = 0
                    lea edx D$esi+1
                Else_If ecx = 0
                    mov ecx edx | lea edx D$esi+1
                Else
                    Error D$BadApiPtr
                End_If
            .End_If
        End_While
      ; Here, edx > Start of Function Name,
      ;       ecx > Start of Module Extension,
      ;       ebx > Start of Module Name
      ;       esi > ending 0
        ..If ecx <> 0
          ; Cases of 'MODULE.ext.Function':
            mov eax edx | sub eax ecx
            If eax > 4
                mov B$ErrorLevel 6, esi ebx
                error D$BadApiPtr
            End_If
            mov esi ebx, D$StartOfDllName edi, D$StartOfFunctionName edx

            While B$esi <> '.' | movsb | End_While | movsb

          ; HAL DLL cannot be loaded in User Mode (consider it like a .sys):
            mov B$WasSysModule &FALSE

            call IsItHAL

          ; Also, .sys Modules cannot be loaded like a DLL:
            mov eax D$esi | or eax 0202020
            On eax = 'sys.', mov B$WasSysModule &TRUE

            While B$esi <> '.' | movsb | End_While | mov D$edi 0

            .If D$ApiCheckFlag <> 2
                If B$WasSysModule = &FALSE
                    call VerifyModuleWithExtensionExist
                    call VerifyFunctionExist
                End_If
            .End_If

            movsb | While B$esi <> 0 | movsb | End_While | movsb

            call AraseSameFunctions

        ..End_If

        While B$esi = 0 | inc esi | End_While
    ..End_While
ret


[WasSysModule: WasHALdll: ?]

IsItHAL:
    mov eax D$StartOfDllName, eax D$eax | or eax 0202020
    If eax = 'hal.'
        mov B$WasSysModule &TRUE
    Else_If eax = 'hal'
        mov B$WasSysModule &TRUE
    End_If
ret


FullfillWithoutExtensions: ; 'FullfillWithExtensions'
L0: ..While B$esi <> ALEOD
        mov ebx esi, edx 0, ecx 0
        While B$esi <> 0
            inc esi
            If B$esi = '.'
                lea edx D$esi+1 | jmp L1>
            End_If
        End_While

      ; Here, edx > Start of Function Name,
      ;       ebx > Start of Module Name
L1:     ..If edx <> 0
          ; Cases of 'MODULE.Function'
            mov D$StartOfDllName edi, D$StartOfFunctionName edx
            mov esi ebx | While B$esi <> '.' | movsb | End_While | mov B$edi 0

            mov B$WasSysModule &FALSE | call IsItHAL

            .If B$WasHALdll = &FALSE
                If D$ApiCheckFlag <> 2
                    call VerifyModuleExist | stosd
                    call VerifyFunctionExist
                Else
                    mov D$edi '.dll' | add edi 4
                End_If
            .Else
                mov D$edi '.dll' | add edi 4
            .End_If

            While B$esi <> 0 | movsb | End_While | mov D$edi 0 | inc edi

            call AraseSameFunctions

        ..End_If

        While B$esi = 0 | inc esi | End_While
    ..End_While
ret


; A 'MODULE.ext.Function' or a 'MODULE.Function' has been found, and recorded.
; We arase all other occurences of 'xxxxFunction':
[CopyOfFunctionName TrashString] ;: ? #32]

AraseSameFunctions:
  ; Now, arase all other evocations of 'MODULE.ext.Function', 'MODULE.Function',
  ; 'Function', in 'ApiListA':
    push esi, edi, edx

        mov esi D$StartOfFunctionName, edi CopyOfFunctionName
        While B$esi <> 0 | movsb | End_While | mov D$edi 0

        mov esi D$ApiListA, edi CopyOfFunctionName, bl B$edi
      ; edx used to test if the first name in ApiListA fits with the searched one:
        mov edx esi | inc edx

L1:     .While B$esi <> ALEOD
            lodsb

            ...If al = bl
                If esi = edx
                    ; Compare (Cases of very first api call in 'ApiListA' given without DLL)
                Else_If B$esi-2 = 0
                    ; Compare
                Else_If B$esi-2 = '.'
                    ; Compare
                Else
                    jmp L1<
                End_If
                push ebx, esi
                    dec esi | mov edi CopyOfFunctionName
L2:                 mov al B$esi, bl B$edi
                    On al <> bl, jmp L7>>
                    On al = 0, jmp L5>
                    On bl = 0, jmp L5>
                    inc esi | inc edi | jmp L2<
L5:                 or al bl
                    ..If al = 0               ; Same > arase:
                        mov edx esi
                        pop esi | push esi
                        mov ebx 0
                        While B$esi <> 0
                            dec esi | On B$esi = '.', inc ebx
                            cmp esi D$ApiListA | je L2>
                        End_While
                        inc esi
L2:                     .If ebx > 0
                            mov ebx D$StartOfDllName
                            mov ebx D$ebx
                            On ebx = D$esi, jmp L2>
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ; New: for Disassemblies:
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                If B$CopyOfFunctionName = '0'
                                    While B$esi <> 0 | inc esi | End_While | jmp L7>
                                End_If
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                mov B$ErrorLevel 6 | error D$DoubleFunctionPtr
                        .End_If
L2:                     While B$esi <> 0 | mov B$esi 0 | inc esi | End_While
                    ..End_If
L7:             pop esi, ebx
            ...End_If

        .End_While
    pop edx, edi, esi
ret


IsItNewDll:
    push edi
      mov edi D$DllList | mov ebx 0 | jmp L1>
L0:   mov al 0,  ecx 0FF
        repne scasb
      cmp B$edi ALEOD | jne L1>
        mov ebx 0 | jmp L9>
L1:     push esi edi
          mov ecx 0FF,  eax 0 | inc ebx
            repe cmpsb
          mov al B$edi-1,  ah B$esi-1
        pop edi esi
        cmp eax 02E00 | jne L0<         ; ('.' in ApiListA, 0 in DllList)
L9: pop edi
ret                                     ; >>> ebx = Dll #n or 0


IsItNewFunction:       ; edi > ApiListB end; esi > ApiListA function name; al > dll #n
    push edi
      mov bl al | mov edi D$ApiListB | jmp L1>
L0:   mov al 0,  ecx 0FF
        repne scasb
      cmp B$edi ALEOD | jne L1>             ; reach end ?
        mov al bl | jmp L9>
L1:   cmp B$edi bl | jne L0<
        inc edi
        push esi edi
          mov eax 0
L2:       mov al B$edi,  ah B$esi
          inc edi | inc esi
          cmp eax 0 | je L3>
          cmp ah al | je L2<
L3:     pop edi esi
        cmp eax 0 | jne L0<
L9: pop edi
    cmp al 0
ret                                         ; >>> ebx = Dll #n or 0


StoreDllName:
    cmp D$DllNumber 0 | je L1>
    call IsItNewDll | cmp ebx 0 | je L1>    ; if 0  >>>  new name to store

L0: lodsb | cmp al '.' | jne L0<
    inc esi
L3: lodsb | cmp al '.' | jne L3<
L4: mov eax ebx                             ; ebx from 'IsItNewDll'
ret

L1: push edi
        mov edi D$DllListPtr
      ; write a new "Dll" name (with Extension):
L2:     lodsb | stosb | cmp al '.' | jne L2<
        While B$esi <> '.' | movsb | End_While
        inc esi
        mov al 0 | stosb | mov B$edi ALEOD                     ; end mark
        mov D$DllListPtr edi | inc D$DllNumber | mov eax D$DllNumber
    pop edi
ret


SortApis:
    mov D$DllNumber 0,  D$FunctionNumber 0
    mov esi D$ApiListA,  edi D$ApiListB
    mov eax 0,  ebx 0
L0: cmp B$esi ALEOD | je L9>>
      call StoreDllName             ; return: ebx = 0 if new, else Dll #
      call IsItNewFunction          ; return: ebx = 0 if new, else Dll #
        ja L2>
L1:  lodsb
       cmp al 0 | jne L1<
         jmp L0<
L2:  stosb                          ; write dll #n
L3:  lodsb
     stosb                          ; write function name until 0
       cmp al 0 | jne L3<
         mov B$edi ALEOD | inc D$FunctionNumber | jmp L0<
L9: ret

;;
 "FreeLibrary" function does NOT really strip off the DLL from memory, if it is of no
 more use. This appends only when we exit our Process. So, After each DDL testing, we
 let it in memory. At each test, we first check if it is already loaded, instead.
;;
[DllHandle: ?    StartOfDllName: ?    StartOfFunctionName: ?    RosAsmDLL: ?]

[ModuleHandlesList: ? #100] [ModuleHandlesListPointer: ?]

VerifyModuleExist:
    pushad
        mov esi D$StartOfDllName

        call TryToLoadModule '.dll'
        If eax <> &NULL
            mov D$DllHandle eax | popad | mov eax '.dll' | ret
        End_If

        call TryToLoadModule '.sys'
        If eax <> &NULL
            mov D$DllHandle eax | popad | mov eax '.sys' | ret
        End_If

        call TryToLoadModule '.drv'
        If eax <> &NULL
            mov D$DllHandle eax | popad | mov eax '.drv' | ret
        End_If

        call TryToLoadModule '.exe'
        If eax <> &NULL
            mov D$DllHandle eax | popad | mov eax '.exe' | ret
        End_If

    mov esi D$StartOfDllName, B$ErrorLevel 4 | error D$BadLibNamePtr


VerifyModuleWithExtensionExist:
    pushad
        mov B$RosAsmDLL &TRUE | call 'KERNEL32.GetModuleHandleA' D$StartOfDllName

        .If eax = &NULL
            mov B$RosAsmDLL &FALSE | call 'KERNEL32.LoadLibraryExA' D$StartOfDllName, 0, D$APICheckFlag
; &DONT_RESOLVE_DLL_REFERENCES &LOAD_LIBRARY_AS_DATAFILE &LOAD_WITH_ALTERED_SEARCH_PATH
            If eax = &NULL
                call TryModuleFromAppDirectory D$StartOfDllName
            End_If

            mov ebx D$ModuleHandlesListPointer, D$ebx eax
            add D$ModuleHandlesListPointer 4
        .End_If

        If eax = &NULL
            mov esi D$StartOfDllName, B$ErrorLevel 4 | error D$BadLibNamePtr
        End_If

        mov D$DllHandle eax
    popad
ret


[ExeSysDrvModuleName: ? #20]

Proc TryToLoadModule:
    Argument @Ext

        mov esi D$StartOfDllName, edi ExeSysDrvModuleName
        While B$esi <> 0 | movsb | End_While
        mov B$edi 0
        move D$edi D@Ext | mov B$edi+4 0

        mov B$RosAsmDLL &TRUE | call 'KERNEL32.GetModuleHandleA' ExeSysDrvModuleName

        .If eax = &NULL
            mov B$RosAsmDLL &FALSE | call 'KERNEL32.LoadLibraryExA' ExeSysDrvModuleName, 0, D$APICheckFlag

            If eax = &NULL
                call TryModuleFromAppDirectory ExeSysDrvModuleName
            End_If

            If eax <> &NULL
                mov ebx D$ModuleHandlesListPointer, D$ebx eax
                add D$ModuleHandlesListPointer 4
            End_If
        .End_If
EndP


;;
  Some users' reports seem to imply that, under some OS Versions, the Directory
  that is considered the default one, for the LoadLibrary Function, could be the
  one where RosAsm lies, instead of the one where the Compiled App lies.
  
  This routines tries to force the Full Path as a last rescue.
;;
[ModuleFullPath: B$ ? #&MAXPATH]

Proc TryModuleFromAppDirectory:
    Argument @Module

        mov edi ModuleFullPath, esi MainName
        While B$esi <> 0 | movsb | End_While
        While B$edi-1 <> '\' | dec edi | End_While

        mov esi D@Module
        While B$esi <> 0 | movsb | End_While | movsb

        call 'KERNEL32.LoadLibraryA' ModuleFullPath
;;
        If eax = 0
            ; call 'KERNEL32.GetLastError'
            ; ---> &ERROR_NOACCESS 
            
            call 'KERNEL32.CreateFileA' ModuleFullPath, &GENERIC_READ,
                                    &FILE_SHARE_READ+&FILE_SHARE_WRITE, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
            
        End_If
;;
EndP


[Aerror: ?]

VerifyFunctionExist:
    pushad
        mov eax D$StartOfFunctionName

        If B$eax = '0'
            mov esi eax | call TranslateHexa
            call 'KERNEL32.GetProcAddress' D$DllHandle, eax
            On eax = &NULL, error D$BadOrdinalPtr
        Else
            call 'KERNEL32.GetProcAddress' D$DllHandle, D$StartOfFunctionName
        End_If

        .If eax = &NULL
            mov edi D$StartOfFunctionName, al 0, ecx 0FFFF | repne scasb
            sub edi 2
            push D$edi, edi
                If B$edi = 'A'
                    mov B$Aerror &FALSE
                Else
                    inc edi | mov al 'A' | stosb | mov B$Aerror &true
                End_If
                mov al 0 | stosb

              ; test with ending 'A'
                call 'KERNEL32.GetProcAddress' D$DllHandle, D$StartOfFunctionName

                mov esi D$StartOfFunctionName, B$ErrorLevel 5  ; error5
            pop edi, D$edi
            inc edi
            push eax
                mov al "'" | stosb | mov al 0 | stosb          ; retore for string search
            pop eax
            If eax = &NULL
                error D$BadFunctionNamePtr
            Else_If B$Aerror = &TRUE
                error D$MissingApiAPtr
            Else
                error D$NoAapiPtr
            End_If
        .End_If
    popad
ret

; Room for headers has been set by 'InitIndex' (CodeListPtr=CodeList+0400)

; We read a Dll Name in DllList (> first = #1)
; We read all #1Functions in ApiListB, and so on.

CreateImportSection:
 ; room for header = (dWord * 5) * (DllNumber + 1)
 ; room for Tables 1, 2 = FunctionNumber + group zero ending = FunctionNumber + DllNumber

    mov D$DllHandle 0
    mov edi D$CodeListPtr                   ; set at 0400 by 'List.a' initialisations
    mov D$ImportHeaderPtr edi

    mov eax D$Dllnumber | inc eax
    shl eax 2 | mov ebx eax | shl ebx 2 | add eax ebx    ; (eax*4)+(eax*16) = eax * 20
    mov ecx eax | mov D$AppImportSize eax   ; write import header size in PE sections header
    push eax
      mov al 0 | rep stosb        ; room for header (eax = header size)
    pop eax
    mov D$ImportTablePtr edi      ; Pointer for Functions names adresses tables writing
    mov eax D$FunctionNumber | add eax D$Dllnumber
    shl eax 2                     ; eax = room for Dwords (shl 2) ptrs
    mov edx eax                   ; EDX = one table size
    add edi eax                   ; room for table 1
    mov ebx edi
      sub ebx D$CodeList
      add ebx 01000                       ; 'uBaseOfImport' not yet filled
        sub ebx 0400                      ; 'uStartOfImport' not yet filled
          mov D$AppSecondImport ebx       ; write adress in PE sections header
          mov D$AppSecondImportSize edx   ; write size in PE sections header
    add edi eax                           ; room for table 2

    mov esi D$DllList | mov eax D$ApiListB,  D$ApiListBPtr eax
    mov cl 1                              ; cl = DLL indice for each function

nextdll:
    On B$esi = ALEOD,  jmp L9>>     ; end mark in DllList
    mov eax edi | sub eax D$CodeList | add eax 01000-0400
    mov ebx D$ImportHeaderPtr
    mov D$ebx+12 eax                ; List adress of dll name written at header fourth dWord
    mov eax D$ImportTablePtr | sub eax D$CodeList | add eax 01000-0400
    mov D$ebx eax | add eax edx | mov D$ebx+16 eax
    mov D$StartOfDllName edi
L0: lodsb                           ; from DllList
      cmp al 0 | je L1>
        stosb                       ; write dll name in Import name list
      jmp L0<
L1: mov B$edi 0                     ; end mark in Import table (will be overwritten)

    mov D$DllListPtr esi
    mov al 0 | stosb
T0: mov esi D$ApiListBPtr

L2: lodsb
     cmp al cl | je L4>             ; actual DLL function?
     cmp al, ALEOD | jne L3>        ; reach end of ApiListB ?
       mov W$edi 0 | add edi 2
       test edi 1 | jz T0>
         mov B$edi 0 | inc edi
T0:    mov ebx D$ImportTablePtr
       mov D$ebx 0                 ; write function name end of chunk in table 1
       mov D$ebx+edx 0             ; write function name end of chunk in table 2
       add D$ImportTablePtr 4      ; ready for next one
       inc cl
       mov  esi D$DllListPtr
       add D$ImportHeaderPtr 20

       jmp nextdll
L3: lodsb
      cmp al 0 | jne L3<
        jmp L2<

L4: mov ebx D$ImportTablePtr
    test edi 1 | jz T0>
      mov B$edi 0 | inc edi
T0: If B$esi = '0'
        pushad
            push ebx, edx
                call TranslateHexa | or eax 08000_0000
            pop edx, ebx
            mov D$ebx eax                  ; write function ordinal in table 1
            mov D$ebx+edx eax              ; write function ordinal in table 2
            add D$ImportTablePtr 4         ; ready for next one
        popad
        mov D$StartOfFunctionName esi
L5:     lodsb | cmp al 0 | jne L5<
    Else
        mov eax edi | sub eax D$CodeList | add eax 01000-0400
        mov D$ebx eax                  ; write function name adress in table 1
        mov D$ebx+edx eax              ; write function name adress in table 2
        add D$ImportTablePtr 4         ; ready for next one
        mov ax 0
        stosw
        mov D$StartOfFunctionName edi

L5:     lodsb | stosb | cmp al 0 | jne L5<   ; writing Functions names list
    End_If
    jmp L2<<                    ; two '0' ??? needed ???... alignement needed ???
                                ; usual import tables names lists seem to be aligned.
L9: ret


BuildImport:
    call InitApiTables

    call SearchForApis      ; copy all api calls in ApiListA (> edi > end of ApiListA).

    If edi = D$ApiListA     ; case of DLL with no api call
        mov D$uBaseOfRsrc 01000, D$uImportSize 0 | ret
    End_If

    call FullfillApiList   ; FullFill each Function in the 'MODULE.ext.Function' Form.

    call SortApis          ; >>> Dll in DllList, Functions in ApiListB + Dll #n at first

    call CreateImportSection

    mov eax edi | Align_on 0200 eax | mov ecx eax | sub ecx edi

    push edi, eax
      mov al 0 | rep stosb  ; fill with 0 because destination have been "reused".
    pop eax edi

    mov D$CodeListPtr eax

    sub eax D$CodeList | sub eax 0400 | add eax 01000 | Align_on 01000 eax
    mov D$uBaseOfRsrc eax
    mov eax edi | sub eax 0400 | sub eax D$CodeList | mov D$uImportSize eax

  ; Release all the loaded Modules not belonging to RosAsm Process:
    mov esi ModuleHandlesList
    While D$esi <> 0
        lodsd
        push esi
            call 'KERNEL32.FreeLibrary' eax
        pop esi
    End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

ExportSectionComments:
;;
; Export Section looks like this:
;
; [.Export: D$ 0, 0, 0, DLLNamePtr, Ord_BASE, NumberOfFunctions, NumberOfNames,
;              ExportAdressesTable, ExportNamesTable, ExportOrdinals
;
;  DLLName: 'MYDLL.DLL', 0  0 0 0 0 0 0 0 0 .....
;
;  ExportAdressesTable: Function1, Function2, Function3 ....
;
;  ExportNamesTable: Function1Name, Function2Name, Function3Name  ....
;
;  ExportOrdinals: W$ (Ord_BASE - Name_Ordinal), ....
;
;
;      Function1Name: 'Function1', 0
;      Function2Name: 'Function2', 0
;      Function3Name: 'Function3', 0 ....]
;;

[EXPnName 12
 EXPnBase 16
 EXPNumberOfFunctions 20
 EXPNumberOfNames 24
 EXPAddressOfFunctions 28
 EXPAddressOfNames 32
 EXPAddressOfNameOrdinals 36]

[ExportAdressesTablePtr: ?    ExportNamesTablePtr: ?    ExportOrdinals: ?
 FunctionNamesPtr: ?          ExportSectionLen: ?       FileAlignedExportSectionLen: ?]

FillExportSection:
    mov edi D$ExportListBPtr
    add edi 40                           ; (nf / nf already written by 'NewBuildExport').

  ; edi points now to 'ExportAdressesTable'. We store the edi in the Header record
  ; 'FunctionsAdressesTable':
    mov eax edi | add eax D$ExportAjust
    mov esi D$ExportListBPtr | mov D$esi+EXPAddressOfFunctions eax, D$ExportAdressesTablePtr eax
    move D$ExpOrdBase D$esi+EXPnBase
  ; We will have as many dWords for Adresses as Exported Functions. So, next header record,
  ; 'ExportNamesTable' will be:
    mov eax D$esi+EXPNumberOfFunctions | shl eax 2 | add eax edi | add eax D$ExportAjust
    mov D$esi+EXPAddressOfNames eax, D$ExportNamesTablePtr eax
    sub eax D$ExportAjust | mov edi eax

  ; We will have as many dWords for Names Pointers as Exported Names.
    mov eax D$esi+EXPNumberOfNames | shl eax 2 | add eax edi | add eax D$ExportAjust
    mov D$esi+EXPAddressOfNameOrdinals eax, D$ExportOrdinals eax
    sub eax D$ExportAjust | mov edi eax

  ; We will have as many Words for Ordinals as Exported Names.
    mov eax D$esi+EXPNumberOfNames | shl eax 1 | add eax edi | add eax D$ExportAjust
    mov D$FunctionNamesPtr eax
;int3
    mov esi D$ExportListAPtr
    and D$Ordinal 0 | mov D$ErrorLevel 9

L1: call StoreExportAdresse | call StoreExportNamePtr
    call StoreExportOrdinal | call StoreFunctionName
    dec D$NumberOfExportedFunctions | jnz L1<

; sort Names & adjaust Ordinals to Base
    mov ebx D$ExportListBPtr | mov ecx D$ebx+EXPNumberOfNames | mov eax D$ebx+EXPnBase
    mov edi D$ebx+EXPAddressOfNameOrdinals | sub edi D$ExportAjust
    call DualBubbleSortExportListPointersAndOrdinals
L0: sub W$edi ax | add edi 2 | loop L0<

    mov ebx D$ExportListBPtr | add ebx EXPnName
    mov edi D$FunctionNamesPtr
    mov D$ebx edi                       ; write DLName pointer.
    sub edi D$ExportAjust
    mov esi ChoosenFile

L0: lodsb | stosb | cmp B$esi '.' | ja L0<

    mov eax D$SavingExtension | or eax 020202000 | xor eax 020202000 | stosd
    mov al 0 | stosb
    sub edi D$ExportListBPtr
    mov D$ExportSectionLen edi
ret


; This is same as 'SearchRegularLabel' with some specifics (no error control -impossible-)
; + end control

[DataExportedLabel: ? OnlyOrdinalExport: ? OrdinalExport: ? ExpOrdBase: ?]

StoreExportAdresse:
    mov B$DataExportedLabel &FALSE | mov B$OnlyOrdinalExport &FALSE
    mov eax D$esi | or eax 0202020 | cmp eax 'ord0' | jne L0>
    lea edi D$esi+4 | mov al EOI | mov ecx 6 | repne scasb
    cmp B$edi TextSign | setne B$OnlyOrdinalExport
    lea edi D$esi+4 | sub edx edx
L1:
    mov al B$edi | inc edi | cmp al '0' | jb L4>
    or al 020 | cmp al '9' | jbe L3> | sub al 027
L3: sub al 030 | shl edx 4 | or dl al | jmp L1<
L4: mov D$Ordinal edx
L0:
    mov edi D$LabelList | mov edx D$edi | add edx edi | add edi 5

L0: push esi
    cmp edx edi | jbe FindLostExportString
L1:     lodsb | mov bl B$edi | inc edi
L2:     If al = '_'
            lodsb | jmp L2<
        Else_If al = EOI
L4:         cmp bl EOI | je L8>
        Else_If al = colonSign ; instead of ':' jE!
            mov B$DataExportedLabel &TRUE | jmp L4< ; jE!
        Else_If al >= 'a'
            and al 00_11011111 | cmp al bl | je L1<
        Else
            cmp al bl | je L1<
        End_If

        cmp bl EOI | jbe L3>            ; case of LabelList name shorter than searched Label
        mov ecx 0FF,  al EOI | repne scasb      ; longer: LabelList edi ptr > after next '|'
L3:     add edi 6                                   ; |LABELNAME|dWord FlagByte|NEXTNAME

L7: pop esi | jmp L0<

L8: pop esi
    mov eax D$edi

    sub eax D$CodeList

    If B$DataExportedLabel = &FALSE
        add eax D$AppBaseOfCode
        sub eax D$AppStartOfCode
    Else
        add eax D$AppBaseOfData
        sub eax D$AppStartOfData
    End_If

    If D$Ordinal <> 0
        mov edi D$ExportListBPtr | add edi 40 | mov edx D$Ordinal | mov ecx edx
        sub edx D$ExpOrdBase ;| cmp D$edi+edx*4 ecx |
        mov D$edi+edx*4 eax
    Else
        mov edi D$ExportAdressesTablePtr | sub edi D$ExportAjust
        While D$edi <> 0 | add edi 4 | End_While
        stosd
        add edi D$ExportAjust | mov D$ExportAdressesTablePtr edi
    End_If
ret


StoreExportNamePtr:
    ON B$OnlyOrdinalExport = &TRUE, ret
    mov edi D$ExportNamesTablePtr, eax D$FunctionNamesPtr
    sub edi D$ExportAjust
    stosd | add D$ExportNamesTablePtr 4
ret


[Ordinal: ?]

StoreExportOrdinal:
    If B$OnlyOrdinalExport = &TRUE
       and D$Ordinal 0 | ret
    End_If

    If D$Ordinal <> 0
        mov edi D$ExportOrdinals, eax D$Ordinal | sub edi D$ExportAjust | stosw
        add D$ExportOrdinals 2 | and D$Ordinal 0
    Else
        mov edi D$ExportOrdinals | sub edi D$ExportAjust ; calc ODR from given pos
        mov eax D$ExportAdressesTablePtr
        sub eax D$ExportAjust | sub eax D$ExportListBPtr | sub eax 40 | shr eax 2
        dec eax | add eax D$ExpOrdBase | stosw | add D$ExportOrdinals 2
    End_If
ret


StoreFunctionName:
    If B$OnlyOrdinalExport = &TRUE
       mov edi esi | mov al EOI | mov ecx 0FF | repne scasb | mov esi edi
       mov edi D$ExportListBPtr | ret
    End_If

    mov edi esi | mov al EOI | mov ecx 0FF | repne scasb | cmp B$edi TextSign | je L3>
    mov edi D$FunctionNamesPtr | sub edi D$ExportAjust
    While B$esi <> EOI
        If B$esi = colonSign ; instead of ':' jE!
            inc esi | jmp L2>
        End_If
        movsb
    End_While
L2: inc esi | jmp L2>
; Copy if FullName
L3: mov esi edi | mov edi D$FunctionNamesPtr | sub edi D$ExportAjust | inc esi
    While B$esi <> TextSign | movsb | End_While | inc esi
L2: mov al 0 | stosb | add edi D$ExportAjust | mov D$FunctionNamesPtr edi
ret

[ExportsectionWanted: ?  NumberOfExportedFunctions: ?  ExportALL: ?
 ExportListAPtr: ?  ExportListBPtr: ?  ExportListPointers: ?  NumberOfJunkBytes: ? ExpOrdArray: ?]

DualBubbleSortExportListPointersAndOrdinals:
    pushad
    mov ebx D$ebx+EXPAddressOfNames | sub ebx D$ExportAjust
    push ecx | sub eax eax

L0: mov esi D$esp | sub ebp ebp | sub ebx 4 | sub edi 2
L1: dec esi | je L5>
    add ebx 4 | add edi 2
    mov ecx D$ebx | mov edx D$ebx+4 | sub ecx D$ExportAjust | sub edx D$ExportAjust
L3: mov al B$ecx | mov ah B$edx | inc ecx | inc edx | test eax eax | je L4>
    cmp ah al | je L3< | ja L1<
    Exchange D$ebx D$ebx+4 | Exchange W$edi W$edi+2 | or ebp 1 | jmp L1<
L4: mov eax D$ebx | sub eax D$ExportAjust | pop ecx | mov D$esp+01C eax | popad
    jmp FindExportFullNameString
L5: test ebp ebp | je L7>

    mov esi D$esp | sub ebp ebp | add ebx 4 | add edi 2
L1: dec esi | je L5>
    sub ebx 4 | sub edi 2
    mov ecx D$ebx | mov edx D$ebx+4 | sub ecx D$ExportAjust | sub edx D$ExportAjust
L3: mov al B$ecx | mov ah B$edx | inc ecx | inc edx | test eax eax | je L4<
    cmp ah al | je L3< | ja L1<
    Exchange D$ebx D$ebx+4 | Exchange W$edi W$edi+2 | or ebp 1 | jmp L1<
L5: test ebp ebp | jne L0<<
L7: pop ecx | popad
ret

FindExportFullNameString:
    mov esi eax | mov edi eax | sub eax eax | or ecx 0-1 | repne scasb | not ecx | dec ecx
    mov ebx ecx | mov edi D$CodeSource | mov edx D$SourceEnd
    While edi < edx
        cmp W$edi '::' | jne L8>
        mov al B$edi+2 | cmp al "'" | je L0> | cmp al '"' | jne L8>
L0:     add edi 2 | pushad | inc edi | mov ecx ebx | repe cmpsb | popad | jne L8>
        cmp B$edi+ebx+1 al | je E0>
L8:
        inc edi
    End_While
    mov eax D$SymbolDupPtr | jmp OutOnError

E0: mov D$ErrorLevel 0 |mov B$esi-1 EOI | mov B$esi+ebx EOI
    mov ecx D$StatementsPtr | mov D$ecx edi
    Error D$SymbolDupPtr, esi

FindLostExportString:
    pop esi
    mov edi esi | mov al EOI | or ecx 0-1 | repne scasb
    If B$edi-2 = 0E
        dec edi | mov B$edi-1 EOI
    End_If
    mov ecx edi | sub ecx esi | mov ebx ecx
L0: mov al B$edi-1 | mov B$edi al | dec edi | loop L0<
    mov B$edi EOI | dec ebx | inc esi | mov edi D$CodeSource | mov edx D$SourceEnd
    While edi < edx
        mov al B$edi | cmp al '"' | je L3> | cmp al "'" | je L3> | cmp al ';' | jne L0>
        If D$edi-1 = MLC
            add edi 2
            Do
            inc edi
            Loop_Until D$edi = MLC
            add edi 3
        Else
            Do
            inc edi
            Loop_Until B$edi < ' '
        End_If
        jmp L8>
L3:     inc edi | or ecx 0-1 | repne scasb | dec edi | jmp L8>
L0:     cmp W$edi '::' | jne L8> | mov ecx edi
L0:     cmp B$edi-1 ' ' | jbe L1>
        cmp B$edi-1 '[' | je L1>
        cmp B$edi-1 ']' | je L1>
        cmp W$edi-2 '[<' | je L1>
        cmp B$edi-1 '|' | je L1>
        dec edi | jmp L0<
L1:     mov eax ecx | sub eax edi | cmp eax ebx | jne L2>
L0:     pushad | mov ecx ebx | repe cmpsb | popad | je E0>
L2:     mov edi ecx
L8:     inc edi
    End_While
E1: mov eax D$UnknownSymbolPtr | jmp OutOnError

E0: mov D$ErrorLevel 0 | mov ecx D$StatementsPtr | mov D$ecx edi
    Error D$UnknownSymbolPtr, esi

 __________________________________________________________________________________________
 __________________________________________________________________________________________
;;
 these 2 following stubs are used to create new PE. only 'Labelled' values are
 modified according with source values. Main filling work is done by the 'Build...'
 routines.

 These two stubs must remain all in one single data set (prevent from RosAms data
 alignement).

00000000: Dos exe file header stub:
;;

[DosHeader:
B$ 'MZ'  ; dos exe signature
D$ 030090; Size of file (I don't understand what it means...)
W$ 00    ; Number of reloc. adresses
W$ 04    ; this dos header size (16*4)
W$ 00    ; min size
W$ 0FFFF ; max size
W$ 00    ; SP reg. value at run time

W$ 0B8  ; checksum for header
W$ 00   ; IP reg. value
W$ 00   ; start of Cseg in file
W$ 00   ; start of reloc. table in file
W$ 040  ; overlay default
W$ 0,0,0

W$ 0,0,0,0, 0,0,0,0     ; reserved words

W$ 0,0,0,0
MyCheckSum: D$ 0  ; 30
PeHeaderPointer:
D$ 080   ; File adress of win header

B$   0E, 01F, 0BA, 0E, 00, 0B4, 09, 0CD, 021, 0B8, 01, 04C, 0CD, 021
; push cs // pop ds // mov dx 0E // mov ah 09 // int 021 // mov ax 4C01 // int 021
; 18
B$ ;'Spindoz 32 spit PEfile made wiz RosAsm Assembler.$'
   'This program cannot be run in DOS mode', CR, LF, '$', 0, 0, 0, 0, 0, 0, 0, 0, 0

; 50+18+30 = 98
; if you modify upper string you must absolutely keep the same lenght.

PeHeader:

B$ 'PE',0,0             ; signature
W$ 014C                 ; 386 and more
NumberOfSections:
W$ 04                   ; 4 sections (code, data, import, resource) not 5...
B$ 0,0,0,0              ; time and date stamp
D$ 0                    ; pointer to symbol table (for debug)
D$ 0                    ; number of symbol
W$ 0E0                  ; size of 'optional header'
PeHeaderCharacteristics:
W$ 00100001111          ; characteristics
   ; bit 0 > 1 > reloc. infos not there
  ; bit 1 > 1 > Runable
 ; bit 2 > 1 > no line number for debug
; bit 3 > 1 > no bebug symbol
; others : unknown
B$ 0B,01                ; referred as 'magic'...
W$ 03                   ; linker version
AppCodeSize: D$ 0       ; size of code (.text section)
AppAllDataSize: D$ 0    ; size of initialized data (.data + .rsrc+... + .reloc)
D$ 0                    ; size of uninitialised data
AppRVAentryPoint: D$ 0  ; RVA entry point adress (414h in RDNrect file)
AppBaseOfCode: D$ 0     ; RVA Base of code (0400 in file)
SHAppBaseOfData: D$ 0   ; RVA Base of data ('SH' because one more 'AppBaseOfData' down there)
ImageBase:
D$ 0400000              ; image base (linker base default)
D$ PageSize             ; sections alignement
D$ 0200                 ; file alignement
W$ 04,00                ; OS version
W$ 01,00                ; image version
W$ 04,00                ; sub system version
B$ 00,00,00,00          ; reserved
AppRVAimageSize: D$ 0   ; RVA image size
D$ 0400                 ; headers size
CheckSum:
D$ 0                    ; checksum (works when zero)
SubSystem:
W$ 02                   ; sub system
DllCharacteristics:     ; 0001h - Per-Process Library Initialization
                        ; 0002h - Per-Process Library Termination
                        ; 0004h - Per-Thread Library Initialization
                        ; 0008h - Per-Thread Library Termination
W$ 0                   ; DLL characteristics
AppStackMax: D$ 0100000     ; stack max
AppStackMin: D$ 01000       ; stack min
AppHeapMax: D$ 0100000      ; heap max
AppHeapMin: D$ 0            ; heap min
D$ 0                   ; loader flags
D$ 0_10                 ; number of possible entries in following section table (16 records)

; Section table (called image data directory):
; first Dwords are the RVA adresses; second ones are the sizes
SectionTable:
D$                 00,                        00   ; export
; In fact, not 'Base_of' but rather a pointer to the Import Directory.
; Some Linkers do not write the Import Directory at first place of .Import,
; but at second place (why do it simple when you can do i complicated???...).
AppBaseOfImport: D$ 0  AppImportSize:       D$ 0   ; import header (Directory only)
AppBaseOfRsrc:   D$ 0    AppRsrcSize:       D$ 0   ; resource
D$                 00,                        00   ; exeption
D$                 00,                        00   ; security
RelocSectionTable:
D$                 00,                        00   ; Relocation
DebugDir:
D$                 00,                        00   ; debug
D$                 00,                        00   ; copyright
D$                 00,                        00   ; machine values (mips gp and global ptr)
D$                 00,                        00   ; thread local storage
D$                 00,                        00   ; load configuration directory
D$                 00,                        00   ;
AppSecondImport: D$ 0  AppSecondImportSize: D$ 0   ; second import (Address Table)
D$                 00,                        00
D$                 00,                        00
D$                 00,                        00

SectionsHeaders:

idataSectionHeader:         ; (import section)

B$   '.idata',0,0
AppImportTrueSize: D$ 0     ; EndOfImport - StartOfImport  true size (Virtual Size)
AppBaseOfImports: D$ 0      ; RVA
AppImportAlignedSize: D$ 0  ; 200h+ImportExt (Physical File Size)
AppStartOfImport: D$ 0      ; idata ptr
D$ 0,0,0
D$ 0_C0000040               ; readable, writable, initialised data


ResourceSectionHeader:

B$ '.rsrc',0,0,0
AppRsrcTrueSize: D$ 0       ; EndOfResource-StartOfResource  true size
AppBaseOfRsrcs: D$ 0        ; RVA
AppRsrcAlignedSize: D$ 0    ; 200h+ResourceExt
AppStartOfRsrc: D$  0
D$ 0,0,0
D$ 0_40000040               ; readable initialised data


DataSectionHeader:

B$ '.data',0,0,0
AppDataTrueSize: D$ 0       ; EndOfData-StartOfData  true size
AppBaseOfData: D$ 0         ; RVA
AppDataAlignedSize: D$ 0    ; 200h+DataExt    aligned size
AppStartOfData: D$ 0        ; data ptr
D$ 0,0,0
DataCharacteristics:
D$ 0_C0000040               ; readable, writable, initialised data


; Code section header: (the four 'dummy' D$ and W$ are of no mean in EXE and DLL files)

B$ '.text',0,0,0
AppTrueCodeSize: D$   0     ; true size of code in file
AppCodeRVAoffset: D$   0    ; RVA offset (aligned on 01000 boundary)
AppFileSizeOfCode: D$   0   ; file aligned size of code (0200 aligned)
AppStartOfCode: D$   00     ; pointer to code (true first code in file - not entry point-)
D$   00                     ; dummy reloc ptr
D$   00                     ; dummy line number ptr
W$   00                     ; dummy reloc number
W$   00                     ; dummy number of line number
CodeCharacteristics:
D$   0_60000020             ; characteristics (readable, runable, code)

ExportSectionHeader:        ;, if any:
D$ 0 0
AppExpTrueSize: 0
AppBaseOfExp: 0
AppExpAlignedSize: 0
AppStartOfExp: 0   0 0 0
D$ 0_40000040               ; readable initialised data

RelocSectionHeader:         ;, if Export:
D$ 0 0
AppRelocTrueSize: 0
AppBaseOfReloc: 0
AppRelocAlignedSize: 0
AppStartOfReloc: 0   0 0 0
D$ 0_40000040               ; readable initialised data

D$ 0 0   0 0 0 0   0 0 0 0  ; just ensure to stop win search of sections.

D$ 0 0   0 0 0 0   0 0 0 0

SourceSectionHeader:
B$ '.src',0,0,0,0           ; Used by RosAsm only (not by loader: 4 sections, not 5)
AppSrcTrueSize: D$ 0        ; D$SourceLen  true size
AppBaseOfSrc: D$ 0          ; RVA
AppSrcAlignedSize: D$ 0     ; 200h+ResourceExt
AppStartOfSrc: D$  0
D$ 0,0,0
D$ 06000840                 ; Not readable initialised data; don't keep; don't cache...


EOPE:
PeHeaderSize: D$  EOPE-PeHeader]  ; 'Len' unusable here
 ________________________________________________________________________________________

; User stub main data:

[uImportSize: D$ 0  uRsrcSize: D$ 0  uDataSize: D$ 0  uCodeSize: D$ 0
 uStartOfImport: D$ 0  uStartOfRsrc: D$ 0  uStartOfData: D$ 0  uStartOfCode: D$ 0
 uEndOfFile: D$ 0  uBaseOfImport: D$ 0  uBaseOfRsrc: D$ 0  uBaseOfData: D$ 0
; uBaseOfExport: D$ 0 uExportSize: D$ 0
 uBaseOfCode: D$ 0  uImageSize: D$ 0  uCodeRVA: D$ 0  uAllDataSize: D$ 0
 uStackMax: D$ 0  uStackMin: D$ 0  uHeapMax: D$ 0  uHeapMin: D$ 0]

; exemple with the fixed values i used in the very first versions:

; [CodeExt 05C00  DataExt 01000  ImportExt 0200  RsrcExt 0200]
; [StartOfImport 0400]
; [StartOfRsrc   0800]   ; 0600+ImportExt (0200)
; [StartOfData   0C00]   ; 0800+ImportExt+RsrcExt (0400)
; [StartOfCode   1E00]   ; 0A00+ImportExt+RsrcExt+DataExt (01400)
; [EndOfFile     7D00]   ; 0C00+ImportExt+RsrcExt+DataExt+CodeExt (07100)

 _________________________________________________________________________________________

; For Saving here the true sizes of each section because they are aligned for computations.
; I need them at the end to fill the so called 'Virtual' sizes records (first record of
; each type header:

[ImportTrueSize: 0    ResourcesTrueSize: 0    DataTrueSize: 0    CodeTrueSize: 0]

[LockInstruction: ?]

[TrueUserDataSize: 0]


; Was no use without the new DLLs stuff, but, when building a DLL after having an EXE PE
; compiled, some of these Data may remain with a 'wrong' value from the previous compilation.

ClearUserStubMainData:
    mov edi uImportSize, eax 0, ecx TrueUserDataSize | add ecx 4
    sub ecx edi | shr ecx 2 | rep stosd
ret

UserMainData:
    move D$ImportTrueSize D$uImportSize,
         D$ResourcesTrueSize D$uRsrcSize,
         D$DataTrueSize D$uDataSize

    mov eax D$uImportSize | Align_on 0200 eax

    mov D$uImportSize eax | mov ecx eax
    mov eax D$uRsrcSize | Align_on 0200 eax
    mov D$uRsrcSize eax | add ecx, eax
    move D$TrueUserDataSize D$uDataSize    ; preserve for the whole (with virtual size) Align
    mov eax D$uDataSize | Align_on 0200 eax
    mov D$uDataSize eax | add ecx, eax
    mov eax D$CodelistPtr | sub eax D$CodeList | sub eax 0400 | sub eax, ecx
    move D$CodeTrueSize eax
    mov D$AppTrueCodeSize eax
    Align_on 0200 eax | mov D$uCodeSize eax

    mov eax 0400 | mov D$uStartOfImport eax
        add eax D$uImportSize | mov D$uStartOfRsrc eax
        add eax, D$URsrcSize | mov D$uStartOfData eax
        add eax D$uDataSize | mov D$uStartOfCode eax
        add eax D$uCodeSize | mov D$uEndOfFile eax

    mov eax 01000 | mov D$uBaseOfImport eax
        add eax D$UImportSize | Align_on 01000 eax
        mov D$uBaseOfRsrc eax
        add eax D$uRsrcSize | Align_on 01000 eax
        mov D$uBaseOfData eax
        add eax D$TrueUserDataSize | add eax D$uVirtualDataSize | Align_on 01000 eax
        mov D$uBaseOfCode eax
        add eax D$uCodeSize | Align_on 01000 eax
        mov D$uImageSize eax

    mov eax LINKERDEFAULT | add eax D$uBaseOfCode
        sub eax D$uStartOfCode | mov D$uCodeRVA eax

    mov eax D$uStartOfCode | sub eax D$uStartOfImport | mov D$uAllDataSize eax
ret
 _______________________________________________________________________________________


PreparePeHeader:
  call UserMainData

  move D$AppCodeSize D$uCodeSize
  move D$AppAllDataSize D$uAllDataSize

  call SearchForEntryPoint
  sub eax D$CodeList | sub eax D$uStartOfCode | add eax D$uBaseOfCode
  mov D$AppRVAentryPoint eax

  move D$AppBaseOfCode D$uBaseOfCode
  move D$SHAppBaseOfData D$uBaseOfData
  move D$AppRVAimageSize D$uImageSize

;  hexprint D$uBaseOfCode,
;           D$uBaseOfImport,
;           D$uBaseOfCode

  move D$AppBaseOfImport D$uBaseOfImport

  move D$AppBaseOfRsrc D$uBaseOfRsrc
  move D$AppRsrcSize D$uRsrcSize       ; should be unaligned size of the section

__________________

  move D$AppCodeRVAoffset D$uBaseOfCode
  move D$AppFileSizeOfCode D$uCodeSize
  move D$AppStartOfCode D$uStartOfCode

  mov eax D$DataTrueSize | add eax D$uVirtualDataSize

  move D$AppDataTrueSize eax
  move D$AppBaseOfData D$uBaseOfData
  move D$AppDataAlignedSize D$uDataSize
  move D$AppStartOfData D$uStartOfData

  move D$AppImportTrueSize D$ImportTrueSize
  move D$AppBaseOfImports D$uBaseOfImport
  move D$AppImportAlignedSize D$uImportSize
  move D$AppStartOfImport D$uStartOfImport

  move D$AppRsrcTrueSize D$ResourcesTrueSize
  move D$AppBaseOfRsrcs D$uBaseOfRsrc
  move D$AppRsrcAlignedSize D$uRsrcSize
  move D$AppStartOfRsrc D$uStartOfRsrc

; Store source values in .Src section:
  move D$AppSrcTrueSize D$SourceLen
  mov eax D$uBaseOfCode | add eax D$LenOfCode | Align_on 01000 eax
  mov D$AppBaseOfSrc eax
  mov eax D$SourceLen | Align_on 0200 eax
  mov D$AppSrcAlignedSize eax
  mov eax D$uEndOfFile | mov D$AppStartOfSrc eax

; Copy:
  mov esi DosHeader | mov edi D$CodeList
  mov ecx 080 | rep movsb                                   ; store Dos header
  mov ecx D$PeHeaderSize | mov esi PeHeader | rep movsb     ; room for PE header
ret


[LocOfSourceHeader: ?]

WritePeHeaders:
    or D$SavingExtension 020202000 | xor D$SavingExtension 020202000

    mov esi DosHeader, edi D$CodeList

    mov ecx PeHeader | sub ecx esi | shr ecx 2 | rep movsd

    mov D$edi 'PE'; W$edi+2 0                   ; signature
    add edi 4 | mov W$edi 014C                  ; 386 and more

  ; Compute Number of Sections (NumberOfSections):
    mov eax 1                                   ; Code Section anyway, i suppose.
    On D$uImportSize > 0, inc eax               ; If Import Section wanted.
    On D$uRsrcList > 0, inc eax                 ; If Resources Section wanted.
    On D$uDataSize > 0, inc eax                 ; If Data Section wanted.
                                                ; Code section assumed.
    On B$ExportsectionWanted = &TRUE, inc eax   ; If Export Section wanted.

    If D$SavingExtension = '.SYS'
        jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:     On D$RelocSectionSize => 8, inc eax      ;jE! If Reloc Section wanted.
    End_If
    add edi 2 | stosw                           ; NumberOfSections

    mov eax 0 | stosd | stosd | stosd           ; time and date stamp
                                                ; pointer to symbol table (for debug)
                                                ; number of symbol
    mov W$edi 0E0 | add edi 2                   ; size of 'optional header'

    mov eax &IMAGE_FILE_32BIT_MACHINE__&IMAGE_FILE_EXECUTABLE_IMAGE
    or eax &IMAGE_FILE_LINE_NUMS_STRIPPED__&IMAGE_FILE_LOCAL_SYMS_STRIPPED
    If D$SavingExtension = '.DLL'
        or eax &IMAGE_FILE_DLL
        On D$RelocSectionSize < 8, or eax &IMAGE_FILE_RELOCS_STRIPPED ;jE!
    Else_If D$SavingExtension = '.SYS'
        On D$RelocSectionSize < 8, or eax &IMAGE_FILE_RELOCS_STRIPPED ;jE!
    Else
        or eax &IMAGE_FILE_RELOCS_STRIPPED
    End_If


    mov W$edi ax | add edi 2                    ; 'PeHeaderCharacteristics'

    mov D$edi 03010B | add edi 4                ; B$ 0B,01  ; referred as 'magic'...
                                                ; W$ 03     ; Dummy linker version (???)
    mov eax D$uCodeSize | stosd                 ; size of code (.text section)

    mov eax D$AppAllDataSize | stosd            ; size of initialized data (.data + .rsrc+... + .reloc)

    mov D$edi 0 | add edi 4                     ; Never any real Uninitialised section.

    call SearchForEntryPoint
    sub eax D$CodeList | sub eax D$uStartOfCode
    add eax D$uBaseOfCode | stosd               ; RVA entry point (Adress of 'Main:')

    mov eax D$uBaseOfCode | stosd               ; RVA Base of code (0400 in file)
    mov eax D$uBaseOfData | stosd               ; RVA Base of data

    If D$SavingExtension = '.DLL'
        mov eax D$LinkerDllDefault
    Else_If D$SavingExtension = '.SYS'
        mov eax DRIVERDEFAULT
    Else
        mov eax LINKERDEFAULT                   ; image base
    End_If

   ; mov eax LINKERDEFAULT ; ??? Should it be allowed or not ???
   ; Does not seem to work... Whereas 2 indentical DLLs, with same Base work well.
    stosd

    mov D$edi PageSize | add edi 4              ; sections alignement
    mov D$edi 0200 | add edi 4                  ; file alignement

    mov D$edi 04 | add edi 4                    ; W$ 04,00       ; OS version
    mov D$edi 01 | add edi 4                    ; W$ 01,00       ; image version
    mov D$edi 04 | add edi 4                    ; W$ 04,00       ; sub system version
    mov eax 0 | stosd                           ; B$ 00,00,00,00 ; reserved

; 'AppRVAimageSize:'
    mov D$FinalImageSize edi
    mov eax 0 | stosd                           ; RVA image size
    mov D$edi 0400 | add edi 4                  ; headers size
    mov D$edi 0 | add edi 4                     ; checksum (works when zero)
    mov ax W$SubSystem | stosw                  ; sub system

    If D$SavingExtension = '.DLL'
        mov ax W$DllCharacteristics
    Else_If D$SavingExtension = '.SYS'
        mov W$edi-2 1 ; SubSystem NATIVE jE! - corrected.
        mov ax 0; &IMAGE_DLLCHARACTERISTICS_WDM_DRIVER. removed WDM, bcoz WDM is not unloadable
    Else
        mov eax 0
    End_If
    stosw                                       ; DllCharacteristics
    mov eax D$AppStackMax | stosd
    mov eax D$AppStackMin | stosd
    mov eax D$AppHeapMax | stosd
    mov eax D$AppHeapMin | stosd

    mov D$edi 0 | add edi 4                     ; loader flags
    mov D$edi 010 | add edi 4                   ; number of possible entries in following
                                                ; section table (16 records)
    _________________________

    push edi
        mov eax 0, ecx 020 | rep stosd  ; Clear all 16 * (2 dWords) Entires ('SectionTable').
    pop edi

 [FinalBaseOfExport: ?    FinalAppBaseOfReloc: ?    FinalImageSize: ?]

    If B$ExportsectionWanted = &TRUE
        mov D$FinalBaseOfExport edi, eax 0 | stosd
        mov eax D$ExportSectionLen | stosd
    Else
        add edi 8
    End_If

    If D$uImportSize > 0
        mov eax D$uBaseOfImport | stosd
        mov eax D$Dllnumber | inc eax
        shl eax 2 | mov ebx eax | shl ebx 2 | add eax ebx    ; (eax*4)+(eax*16) = eax * 20
        stosd
    Else
        add edi 8
    End_If

    If D$uRsrcList > 0
        mov eax D$uBaseOfRsrc | stosd
        mov eax D$uRsrcSize | stosd
    Else
        add edi 8
    End_If

    add edi 16                                  ; Now pointing to 'RelocSectionTable'

    .If D$SavingExtension = '.SYS'
        jmp L1>
    .Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:     If D$RelocSectionSize => 8 ;jE!
            mov eax 0, D$FinalAppBaseOfReloc edi | stosd
            mov eax D$RelocSectionSize | stosd
        Else
            add edi 8
        End_If
    .Else
        add edi 8
    .End_If

    add edi (12*4)                              ; Now pointing to 'AppSecondImport'

    mov eax D$AppSecondImport | stosd
    mov eax D$AppSecondImportSize | stosd

    add edi (6*4)                               ; Now pointing to 'SectionsHeaders'
    _____________

    mov D$NextSectionHeaderRVA 01000, D$NextSectionHeaderFilePointer 0400

    If D$uImportSize > 0
        call WriteOneSectionHeader '.ida', 'ta', D$ImportTrueSize,
            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
    End_If

    If D$uRsrcList > 0
        call WriteOneSectionHeader '.rsr', 'c', D$ResourcesTrueSize,
            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
    End_If

    If D$uDataSize > 0
        push D$NextSectionHeaderRVA
        mov eax D$DataTrueSize
        call WriteOneSectionHeader '.dat', 'a', eax, D$DataCharacteristics
           ; &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE

      ;  mov eax D$uDataSize | add eax D$uVirtualDataSize
        mov eax D$TrueUserDataSize | add eax D$uVirtualDataSize

        pop D$NextSectionHeaderRVA
        mov D$edi-(8*4) eax | Align_On 01000 eax | add D$NextSectionHeaderRVA eax
    End_If

  ; 'AppTrueCodeSize'
    call WriteOneSectionHeader '.tex', 't', D$CodeTrueSize, D$CodeCharacteristics
       ; &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_MEM_EXECUTE___&IMAGE_SCN_MEM_READ

  ; 'ExportSectionHeader'
    If B$ExportsectionWanted = &TRUE
        mov ebx D$FinalBaseOfExport, eax D$NextSectionHeaderRVA, D$ebx eax

        call WriteOneSectionHeader '.eda', 'ta', D$ExportSectionLen,
            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
    End_If

  ; 'RelocSectionHeader'
    .If D$SavingExtension = '.SYS'
        jmp L1>
    .Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
L1:     If D$RelocSectionSize => 8 ;jE!
            mov ebx D$FinalAppBaseOfReloc, eax D$NextSectionHeaderRVA, D$ebx eax

            call WriteOneSectionHeader '.rel', 'oc', D$RelocSectionSize,
                &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
        End_If
    .End_If

    mov ebx D$FinalImageSize, eax D$NextSectionHeaderRVA, D$ebx eax

  ; Security Dummy empty Headeers before Source
    mov eax 0, ecx 10 | rep stosd
  ; SourceSectionHeader
  ; Not readable initialised data; don't keep; don't cache...
    mov D$LocOfSourceHeader edi
    call WriteOneSectionHeader '.src', 0, D$SourceLen, 06000840

    mov ecx D$CodeList | add ecx 0400 | sub ecx edi | shr ecx 2
    mov eax 0 | rep stosd
;Else
;    mov ecx 20, eax 0 | rep stosd
;End_If
ret


; Writes 10 dWords for each Section Header:

[NextSectionHeaderRVA: ?    NextSectionHeaderFilePointer: ?]

Proc WriteOneSectionHeader:
    Arguments @NameLow, @NameHigh, @Size, @Flags

        mov eax D@NameLow | stosd
        mov eax D@NameHigh | stosd
        mov eax D@Size | stosd
        mov eax D$NextSectionHeaderRVA | stosd
        mov eax D@Size | Align_On 0200 eax | stosd
        mov ecx eax
        Align_On 01000 eax | add D$NextSectionHeaderRVA eax
        mov eax D$NextSectionHeaderFilePointer | stosd
        mov eax 0 | stosd | stosd | stosd
        mov eax D@Flags | stosd
        add D$NextSectionHeaderFilePointer ecx
EndP



;&IMAGE_FILE_32BIT_MACHINE 0100h
;IMAGE_FILE_AGGRESIVE_WS_TRIM 010h
;&IMAGE_FILE_BYTES_REVERSED_HI 8000h
;&IMAGE_FILE_BYTES_REVERSED_LO 080h
;IMAGE_FILE_DEBUG_STRIPPED 0200h
;&IMAGE_FILE_DLL 2000h
;&IMAGE_FILE_EXECUTABLE_IMAGE 02h
;IMAGE_FILE_LARGE_ADDRESS_AWARE 020h
;&IMAGE_FILE_LINE_NUMS_STRIPPED 04h
;&IMAGE_FILE_LOCAL_SYMS_STRIPPED 08h
;IMAGE_FILE_NET_RUN_FROM_SWAP 0800h
;IMAGE_FILE_NET_RUN_FROM_SWAP 800
;IMAGE_FILE_RELOCS_STRIPPED 01h
;IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP 0400h
;IMAGE_FILE_SYSTEM 1000h
;IMAGE_FILE_UP_SYSTEM_ONLY 4000h
 _________________________________________________________________________________________

ResourcesStub:
____________________________________________________________________________________________

[NodeFlag  080000000]

    ;RT_NEWRESOURCE      02000
    ;RT_ERROR            07fff

[RT_AVI              0-1
 RT_WAVE             0-2
 RT_CURSOR           00_0001
 RT_BITMAP          00_0010
 RT_ICON             00_0011
 RT_MENU             00_0100
 RT_DIALOG           00_0101
 RT_STRING           00_0110
 RT_FONTDIR          00_0111
 RT_FONT             00_1000
 RT_ACCELERATORS     00_1001
 RT_RCDATA           00_1010
 RT_MESSAGETABLE     00_1011
 RT_GROUP_CURSOR     00_1100
 RT_GROUP_ICON       00_1110
 RT_VERSION     00_0001_0000]

    ;[RT_NEWBITMAP        (RT_BITMAP|RT_NEWRESOURCE)
    ; RT_NEWMENU          (RT_MENU|RT_NEWRESOURCE)
    ; RT_NEWDIALOG        (RT_DIALOG|RT_NEWRESOURCE) ]

;RT_GROUP_CURSOR  equ RT_CURSOR + DIFFERENCE
;RT_GROUP_ICON  equ RT_ICON + DIFFERENCE
;RT_VERSION   equ 16
;RT_DLGINCLUDE  equ 17
;RT_PLUGPLAY  equ 19
;RT_VXD   equ 20
;RT_ANICURSOR  equ 21
;RT_ANIICON   equ 22
;RT_HTML   equ 23
 ________________________________________________________________________________________

; Level 1:

[Resources: W$ 0,0,0,0,0,0,0,2                               ; 2 next resources records
 D$ RT_ICON,  0_80000020         ;Level2Rt_Icon-StartOfRsrc+NodeFlag ;80000028h (0C28h)
    RT_GROUP_ICON, 0_80000038    ;Level2Rt_Group_Icon-StartOfRsrc+NodeFlag;80000040h (0C40h)

; Level 2: (resource TYPEs directory)

Level2Rt_Icon:
W$ 0,0,0,0,0,0,0,1   ; 1 record
D$ 01, 080000050     ;Level3Rt_Icon-StartOfRsrc+NodeFlag;80000070h; ID 1: icon; > 0C70h

Level2Rt_Group_Icon:
W$ 0,0,0,0,0,0,0,1   ; 1 record
D$ 064, 080000068    ;Level3Rt_Group_Icon-StartOfRsrc+NodeFlag; ID 64h: Group icon > 0C88h

; Level 3: (last one to one language resources pointers - lang. dir -)

Level3Rt_Icon:
W$ 0,0,0,0,0,0,0,1
D$ 0409, 080 ;Level4Rt_Icon-StartOfRsrc;B8h, 409 = langage ID: can_english; B8h > CB8h

Level3Rt_Group_Icon:
W$ 0,0,0,0,0,0,0,1
D$ 0409, 090  ;Level4Rt_Group_Icon-StartOfRsrc   ;C8h, CC8h

;Level 4: (records of each resource: PTR, size, CodePage, reserved)

;;RsrcRVA = BaseOfRsrc-StartOfRsrc

Level4Rt_Icon:
;;D$ IconHeader+RsrcRVA, 02E8, 0, 0  ; icon at CF0h;         size=2E8h
D$ 020A8, 02E8, 0, 0

Level4Rt_Group_Icon:
;;D$ Group_Icon+RsrcRVA, 014, 0, 0   ; group icon at FD8h;   size = 14h
D$ 02390, 014, 0, 0

; icon data. This icon image is for compilation only. At start of RosAsm Run, the default
; icon is copyed from Icon Editor to here. The Editor version is in fact used as temporary
; storage.

uIcon:

IconHeader:
B$ 028,0,0,0   ; size
 020,0,0,0     ; width
 040,0,0,0     ; height (maybe 40h because of the two masks)
 01,0          ; planes
 04,0          ; bit count
 0,0,0,0       ; compression 0
 080,02,0,0    ; 0280 > size of icon data
 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


IconPalette:
;Blue,green,red,0    (16 color palette -values seams to be fixed ones-)

   0,  0,  0,  0 ; color 0
   0,  0,080,  0 ;       1
   0,080,  0,  0 ;       2
   0,080,080,  0 ;       3
 080,  0,  0,  0 ;       4
 080,  0,080,  0 ;       5
 080,080,  0,  0 ;       6
 0C0,0C0,0C0,  0 ;       7
 080,080,080,  0 ;       8
   0,  0,0FF,  0 ;       9
   0,0FF,  0,  0 ;       A
   0,0FF,0FF,  0 ;       B
 0FF,  0,  0,  0 ;       C
 0FF,  0,0FF,  0 ;       D
 0FF,0FF,  0,  0 ;       E
 0FF,0FF,0FF,  0 ;       F

IconXorMask:
; XOR color mask: (32*16 octets > 2 pixels / byte > 32*32 pixels)

     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0C,0F0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0C0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0CC,  0
     0,  0, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0C0
    03,03B, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0CF
     0,033, 09,099,090,  0,  0,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 03, 09,099,090,0DB,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DD,033,030,099,099,099,099, 0C,0CC,0CF
     0,  0,  0,  0,  0,0DD,0DD,0FF,0F0,099,099,099,099, 0C,0CC,0CF
     0,  0, 03,03B,0BB,0BD,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 0F,  0,033,0BB,0BB,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0,  0,0C0, 03,03B,0BB,0BD,  0,  0,  0,  0,  0,  0, 0C,0CC,0CF
    0E,0E0,0CC,  0,033,033,033,  0, 0F,033,0BB,0BB,0BB,0BB,0CC,0CF
    0E,0E0,0CC,0C0,  0,  0,  0,  0,0FF,0F3,03B,0BB,0BB,0BB,0BC,0CF
    0E,0E0,0CC,0CC,  0,  0,  0,  0, 0C,0FF,033,0BB,0BB,0BB,0BB,0CF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CF,0F3,033,033,033,033,03F
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0FF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0CF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,  0,  0,  0, 0E,0EE,0EE, 0C,0C0,  0,  0,  0,  0,  0,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0C0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,  0,  0,  0,  0,  0,  0,  0, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,033,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0B0,0AA,0AA,0AA,0AA, 0C,0CC
     0, 03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0B0,  0,  0,  0,  0, 0C,0CC
     0,  0,033,033,033,033,033,033,033,033, 03,0BB,0BB,0BB,0BB,0CC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0F0,03B,0BB,0BB,0BB,0BC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0FF, 03,033,033,033,033

IconAndMask:
; AND monochrome mask: (8*16 octects > 8 pixels / byte > 32*32 pixels)

     0,  0,  0, 07,  0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01
   080,  0,  0,  0,0C0,  0,  0,  0,0E0,  0,  0,  0,0F0,  0,  0,  0
   0F0,  0,  0,  0,0F0,  0,  0,  0,0F0,  0,  0,  0,0F8, 03,080,  0
   0EC, 03,080,  0, 06, 03,080,  0, 03, 03,080,  0, 01,0FF,  0,  0
     0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0
     0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01,  0,  0,  0,  0
     0,  0,  0,  0,  0,  0,  0,  0,080,  0,  0,  0,0C0,  0,  0,  0
   0E0,  0,  0,  0,0F0,  0,  0,  0,0FF,0FF,0C0,  0,0FF,0FF,0C0,  0

uIconEnd:
;;
 Group icon and entry may be considered as fixed, like upper headers. Theorycaly, they are
 not, but in practice, they are. More than that, all these values don't seam to be used
 by win95: changing them all to anything else doesn't make any difference... I suppose
 that Win simply reads the sections headers and jumps directly to the file icon header
 (before palette), which one is NOT unused.
;;
; group icon: (icons directory)

uGroupIcon:
W$  00      ; reserved
    01      ; type 1 (always)
    01      ; 1 entry following
            ; As opposed to what doc says: no padding here.
; icon entry:
B$ 020, 020 ; width, height
B$ 010, 0   ; 16 colors, reserved
W$ 01, 04   ; 1 color plane, 4 bits
D$ 02E8     ; size in bytes (true)
W$ 1        ; Seams to be the order number for both Cursors and icons as they come in the tree.
uGroupIconEnd:
EndOfRsrc: 0]
____________________________________________________________________________________________

[CopyOfuIcon:

;CopyOfIconHeader:
B$ 028,0,0,0   ; size
 020,0,0,0     ; width
 040,0,0,0     ; height (maybe 40h because of the two masks)
 01,0          ; planes
 04,0          ; bit count
 0,0,0,0       ; compression 0
 080,02,0,0    ; 0280 > size of icon data
 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


;CopyOfIconPalette:
;Blue,green,red,0    (16 color palette -values seams to be fixed ones-)

   0,  0,  0,  0 ; color 0
   0,  0,080,  0 ;       1
   0,080,  0,  0 ;       2
   0,080,080,  0 ;       3
 080,  0,  0,  0 ;       4
 080,  0,080,  0 ;       5
 080,080,  0,  0 ;       6
 0C0,0C0,0C0,  0 ;       7
 080,080,080,  0 ;       8
   0,  0,0FF,  0 ;       9
   0,0FF,  0,  0 ;       A
   0,0FF,0FF,  0 ;       B
 0FF,  0,  0,  0 ;       C
 0FF,  0,0FF,  0 ;       D
 0FF,0FF,  0,  0 ;       E
 0FF,0FF,0FF,  0 ;       F

;CopyOfIconXorMask:
; XOR color mask: (32*16 octets > 2 pixels / byte > 32*32 pixels)

     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0C,0F0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0C0,  0
    09,099,099,099,099,099,099,099,099,099,099,099,099, 0C,0CC,  0
     0,  0, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0C0
    03,03B, 09,099,099,099,099,099,099,099,099,099,099, 0C,0CC,0CF
     0,033, 09,099,090,  0,  0,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 03, 09,099,090,0DB,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0BB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DB,0BB,0B0,099,099,099,099, 0C,0CC,0CF
     0,  0, 09,099,090,0DD,0DD,033,030,099,099,099,099, 0C,0CC,0CF
     0,  0,  0,  0,  0,0DD,0DD,0FF,0F0,099,099,099,099, 0C,0CC,0CF
     0,  0, 03,03B,0BB,0BD,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0, 0F,  0,033,0BB,0BB,0DD,  0,  0,099,099,099,099, 0C,0CC,0CF
     0,  0,0C0, 03,03B,0BB,0BD,  0,  0,  0,  0,  0,  0, 0C,0CC,0CF
    0E,0E0,0CC,  0,033,033,033,  0, 0F,033,0BB,0BB,0BB,0BB,0CC,0CF
    0E,0E0,0CC,0C0,  0,  0,  0,  0,0FF,0F3,03B,0BB,0BB,0BB,0BC,0CF
    0E,0E0,0CC,0CC,  0,  0,  0,  0, 0C,0FF,033,0BB,0BB,0BB,0BB,0CF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CF,0F3,033,033,033,033,03F
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0FF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,0CC,0CC,  0, 0E,0EE,0EE, 0C,0CC,0CF,0FF,0FF,0FF,0FF,0FF
    0E,0E0,  0,  0,  0, 0E,0EE,0EE, 0C,0C0,  0,  0,  0,  0,  0,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,  0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0C0
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    0E,0EE,0EE,0EE,0EE,0EE,0EE,0EE, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,  0,  0,  0,  0,  0,  0,  0, 0C,0C0,0AA,0AA,0AA,0AA, 0C,0CC
    03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0C0,0AA,0AA,0AA,0AA, 0C,0CC
     0,033,0BB,0BB,0BB,0BB,0BB,0BB,0BB,0B0,0AA,0AA,0AA,0AA, 0C,0CC
     0, 03,03B,0BB,0BB,0BB,0BB,0BB,0BB,0B0,  0,  0,  0,  0, 0C,0CC
     0,  0,033,033,033,033,033,033,033,033, 03,0BB,0BB,0BB,0BB,0CC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0F0,03B,0BB,0BB,0BB,0BC
     0,  0,  0,  0,  0,  0,  0,  0,  0,0FF,0FF, 03,033,033,033,033

;CopyOfIconAndMask:
; AND monochrome mask: (8*16 octects > 8 pixels / byte > 32*32 pixels)

     0,  0,  0, 07,  0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01
   080,  0,  0,  0,0C0,  0,  0,  0,0E0,  0,  0,  0,0F0,  0,  0,  0
   0F0,  0,  0,  0,0F0,  0,  0,  0,0F0,  0,  0,  0,0F8, 03,080,  0
   0EC, 03,080,  0, 06, 03,080,  0, 03, 03,080,  0, 01,0FF,  0,  0
     0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0,  0,0C0,  0,  0
     0,  0,  0, 07,  0,  0,  0, 03,  0,  0,  0, 01,  0,  0,  0,  0
     0,  0,  0,  0,  0,  0,  0,  0,080,  0,  0,  0,0C0,  0,  0,  0
   0E0,  0,  0,  0,0F0,  0,  0,  0,0FF,0FF,0C0,  0,0FF,0FF,0C0,  0

CopyOfuIconEnd: D$ CopyOfuIconEnd-CopyOfuIcon]


RestoreDefaultIcon:
    mov esi CopyOfuIcon, edi uIcon, ecx D$CopyOfuIconEnd | rep movsb
    mov esi CopyOfuIcon, edi iIcon, ecx D$CopyOfuIconEnd | rep movsb
ret


____________________________________________________________________________________________

;;
 This is what would looks like a hand made menu:

[uMenu:     U$ 01 04 00 00       ; 01 unknown by me; 04 must be &RT_MENU

0 0 0 0          M00_Tree       0   0  '&Tree' 0  0


0 0 0 0          0                0   1  '&File' 0  0 0 0
0 0 0 0          M00_New         0   0  '&New' 0
0 0 0 0          M00_Open        0   0  '&Open' Tab 'F3' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Open_source_only 0   0  'Open source only' 0
0 0 1 0          M00_Open_Icon_only   0   0  'Open Icon only' 0
0 0 1 0          M00_Open_Menu_only   0   0  'Open Menu only' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Side_open   0   0  '&Side open' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Save_As      0   0  'Save &As' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Save_Source_only 0   0  'Save source only' Tab 'F2' 0 0
0 0 1 0          M00_Save_Icon_only   0   0  'Save Icon only' Tab 'F2' 0 0
0 0 1 0          M00_Save_Menu_only   0   0  'Save Menu only' Tab 'F2' 0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Exit        0 080  'E&xit' Tab 'Alt+X' 0 0


0 0 0 0          0               0   1  '&Edit' 0  0 0 0
0 0 1 0          M00_Undo       0   0  '&Undo' Tab 'Ctrl+Z' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_Cut        0   0  'Cu&t' Tab 'Ctrl+X'   0 0
0 0 0 0          M00_Copy       0   0  '&Copy' Tab 'Ctrl+C'    0
0 0 0 0          M00_Paste      0   0  '&Paste' Tab 'Ctrl+V' 0 0
0 0 0 0          M00_Delete     0   0  '&Delete' Tab 'Del'   0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Select_All  0 080  'Select &All'  0 0

0 0 0 0          0               0   1  'Search' 0 0 0
0 0 0 0          M00_Find       0   0  'Find' 0
0 0 0 0          M00_Replace    0 080  'Replace' 0 0

0 0 0 0          M00_Compile      0   0  '&Compile' 0           ; alone

0 0 1 0          M00_Run          0   0  '&Run' 0

0 0 0 0          M00_Calc         0   0  'Calc' 0

0 0 1 0          M00_HexAsc       0   0  'HexAsc' 0

0 0 0 0          0                 0   1  'Resources' 0 0 0 0
0 0 0 0          M00_Icon         0   0  '&Icon' 0 0
0 0 1 0          M00_Dialog_box       0   0  '&Dialog Box' 0 0
0 0 1 0          M00_BitMap       0   0  '&BitMap' 0 0
0 0 1 0          M00_Cursor       0   0  '&Cursor' 0 0
0 0 1 0          M00_String       0   0  '&String' 0 0
0 0 1 0          M00_Font         0   0  '&Font' 0 0
0 0 1 0          M00_Accelerators 0   0  '&Accelerators' 0 0
0 0 1 0          M00_RcData       0   0  'RcData' 0
0 0 1 0          M00_MessageTable 0   0  'MessageTable' 0
0 0 0 0          M00_Menus         0 080  '&Menus' 0

0 0 0 0          M00_Configuration       0   0  'Configuration' 0 0

0 0 0 0          0                 0    1  '&Help' 0 0 0 0
0 0 0 0          M00_Help_Editor    0    0  'Help &Editor' 0
0 0 0 0          M00_Help_Tree_view    0    0  'Help Tree &View' 0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Asm_Mnemonics        0    0  'Asm &Mnemonics' 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 1 0          M00_Win32_Api_List         0    0  'Win32 &Api List' 0 0
0 0 1 0          M00_Win32_Equates_List      0    0  'Win32 E&quates List' 0 0
0 0 1 0          M00_Win32_Structures_List        0    0  'Win32 &Structures List' 0
0 0 1 0          M00_Win32_Data_Types        0    0  'Win32 Data &Types' 0 0
0 0 0 0          0 0 0 0                                           ; separator
0 0 0 0          M00_About        0  080  '&About' Tab 'F1' 0 0

0 0 0 0          M00_<<<<_         0   0  '   <<<<  ' 0 0

0 0 0 0          M00_>>>>_      0   080  '   >>>>  ' 0 0


    ; hot keys (see later):

  ;  01  070  03E8  00    ;03E8 > about ID  (F1)
  ;  01  071  03F4  00    ;03F4 > Save ID   (F2)
  ;  01  072  03EC  00    ;03EC > Open ID   (F3)
  ; 090  078  03ED  00    ;03ED > Exit ID   (Alt-X)  78 > 'x'

  ; D$ 0 0 0 0 0 0 0

uMenuEnd: ]
;;

 __________________________________________________________________________________________
;;
 Resource section begin with a tree structure pointing to data. This tree is like a
 hard disk directory structure. At root, we find the main entries, one for each resource
 type. At last, we find something like a file icon: pointers to true data. This last one
 is a leave (the 'file icon', not the true data). Up others are nodes.
 The levels are:
 - root dir (points to resources types)
 - types dir (points to languages types)
 - pointers dir (points to true data)

 Each dir has an height words header. they are all zero but last one: records number in
 the dir; and as many records of two dWords. Exemple for root dir with two resources:

[Resources: W$ 0,0,0,0,0,0,0,2  ; 2 next resources records
 D$ &RT_ICON,  0_80000020        ; 080 > it points to a node.
 &RT_GROUP_ICON, 0_80000038      ; 020 / 038 are displacememnts Ptr (from start of .rsrc)

 Leaves have no 8 words header and are 4 dWords long (RVA ptr / size / CodePage / reserved)

 The only info i found about CodePage dWord is this:
 "... CodePage ...should be used when decoding code point values within the resource data.
  Typically for new applications the code page would be the unicode code page."
 (??????????????????!!!!!!!!!!!!!!!) >>> set at zero until i understand more...

 For .rsrc section construction, we first search for the total lenght of this tree, save
 pointers to each dir and work downside up: first write the data, then fill dirs backward.
 uRsrcsList is a temporary list containing needed information on each resource in
 memory.
;;
 ___________________________________________________________________________________________

[ID_Icon 1,  Language 0409,  ID_Group_Icon 1]

; resource temporary list: up to 200 resource >>> to change for dynamic memory later.

[DumRL: 0 0 0 0 0]  ; to stop upward search on zeros

[uRsrcList:         ; RosAsm infos storage for .rsrc section building (5 dWords per resource)
   D$ 0             ; Type
      0             ; ID
   0409             ; Language
      0             ; Data pointer
      0             ; Size
      #MAXRESOURCE]
[Dum2RL: 0 0 0 0 0]

[RsrcHeadPtr: 0  RsrcSectionOrigine: 0   RsrcSectionPtr: 0
              RsrcTypePtr: 0  RsrcLangPtr: 0  RsrcPtrPtr: 0
              uRsrcListPtr: 0]

[HeadLine | add edi 2  ; we have just written some dWords with strings instructions
            mov eax #1 | stosw | mov eax 0 | stosw | stosd | stosd | stosd
            sub edi 2] ; we are going to write some more dWords...


; We set here the list of existing resources for the app resources section. We store
; the type, the ID, Dummy language, Pointer to resource data and size. For menu, i let
; some 'RosAsm developpement internal' test that i use to study menus encoding:

[TypeByName: 0]

; Re-ordering the resources Tables Records in ID order:

SortRsrcTable:
    push eax, ebx, esi
L0:     mov bl 0
        While D$esi > 0
            mov eax D$esi
            .If eax > D$esi+12
                If D$esi+12 <> 0
                    Exchange D$esi D$esi+12, D$esi+4 D$esi+16, D$esi+8 D$esi+20
                    mov bl 1
                End_If
            .End_If
            add esi 12
        End_While
        pop esi | push esi | cmp bl 1 | je L0<
    pop esi, ebx, eax
ret
____________________________________________________________________________________________

[NoResources: ?    NoMainIcon: ?]
 ________________________________________________________________________________________
 ________________________________________________________________________________________

;                                   data job
 ________________________________________________________________________________________
 ________________________________________________________________________________________

[LenOfDataSet: 0  AfterLastDataLabel: 0  DataLoopValue: 0  DefSize: 0]

BuildData:
    mov eax D$CodeListPtr | mov D$DataList eax | mov D$DataListPtr eax

        call StoreDatas

    mov eax D$DataListPtr | sub eax D$DataList | mov D$uDataSize eax

    mov eax D$DataListPtr

  ; Clear all possible trailing left Source Text:
    push eax
        While B$eax <> 0
            mov B$eax 0 | inc eax
        End_While
    pop eax

    Align_on 0200 eax | mov D$CodeListPtr eax

        call StoreVirtualData

;;
  Case of ?Data without any Data >>> We force a dummy .Data Section, because usually,
  in RosAsm outputed PEs, the ?Data are a simple RVA extension of Data, for saving the
  addition of one more, and no use .bss Section, with a different header:
;;
    .If D$uDataSize = 0
        If D$uVirtualDataSize <> 0
            add D$CodeListPtr 0200
            mov D$uDataSize 1
        End_If
    .End_If

    call StripDoneStatementsPointers
ret


TranslateText:
    lodsb                        ; strip first text sign

    On B$DefSize = uMem, jmp TranslateUnicode

L0: lodsb | cmp al TextSign | je L9>
      stosb | jmp L0<
L9: lodsb                        ; strip lasting one
    If al = CloseBracket
        dec esi
    Else_If al > Separators
        Error D$MissingSeparatorPtr
    End_If
    mov edx 0
ret


TranslateUnicode:
L0: lodsb | cmp al TextSign | je L9>
        stosb | mov al 0 | stosb | jmp L0<
L9: lodsb                        ; strip lasting one
    mov edx 0
ret


; translations from text expressions to true numbers. Text is previously pointed by esi
; results stored in (ebx >) EDX:EAX

TranslateBinary:
    lodsw                                               ; clear first '00'
NackedBinary:
    mov ebx 0, edx 0, ecx 0
L0: lodsb | cmp al Closebracket | jbe L9>
    sub al '0' | shld edx ebx 1 | shl ebx 1 | or bl al
    cmp edx ecx | jb L8>
        mov ecx edx
            cmp al 2 | jb L0<
L8:             mov ecx D$BinTypePtr | jmp BadNumberFormat
L9: mov eax ebx
ret


TranslateHexa:
    lodsb                                               ; clear first '0'
NackedHexa:
    mov ebx 0,  edx 0, ecx 0
L0: lodsb | cmp al LowSigns | jbe L9>
        sub al '0' | cmp al 9 | jbe L2>
            sub al 7
L2: shld edx ebx 4 | shl ebx 4 | or bl al
    cmp edx ecx | jb L8>
        mov ecx edx
            cmp al 0F | jbe L0<
L8: mov ecx D$HexTypePtr | jmp BadNumberFormat
L9: mov eax ebx
ret


TranslateDecimal:
    mov eax 0, ecx 0

L2: mov cl B$esi | inc esi                        ; (eax used for result > no lodsb)
    cmp cl LowSigns | jbe  L9>

      mov edx 10 | mul edx | jo L3>               ; loaded part * 10
                                                  ; Overflow >>> Qword
        sub  ecx '0' | jc L7>
        cmp  ecx 9   | ja L7>

          add  eax ecx | jnc  L2<
            jmp  L4>                              ; carry >>> Qword

                                                  ; if greater than 0FFFF_FFFF:
L3: sub ecx '0' | jc L7>
    cmp ecx 9   | ja L7>

      add eax ecx

L4:   adc edx 0
      mov cl B$esi | inc  esi
      cmp cl LowSigns | jbe L9>

        mov ebx eax, eax edx, edx 10 | mul edx    ; high part * 10
          jo L6>                                  ; Qword overflow
            xchg eax ebx | mov edx 10 | mul edx   ; low part * 10
            add  edx ebx
            jnc   L3<                             ; carry >>> overflow

L6:           On B$esi < '0', error D$OverFlowPtr
              If B$esi <= '9'
                  inc esi | jmp L6<
              End_If

L7: mov ecx D$DezimalTypePtr | jmp BadNumberFormat
L9: ret                                           ; >>> number in EDX:EAX


TranslateAny:
    If W$esi = '00'
        call TranslateBinary
    Else_If B$esi = '0'
        call TranslateHexa
    Else
        call TranslateDecimal
    End_If
ret

;;
 In error cases, we come here with the error Message in ecx. This is done this way, in
 order to give zero time penality to Clean written Sources. Here, we check for
 alternate expressions of Numbers ( ...B, ... xB, ...D, ...xD, ...H, ...xH, and crazy
 leading zeros in excess).
 
 Note that the whole thingy may be run twice (No care of time cost for alien syntaxes,
 but full care of time for clean written Source -zero added cost for them-).
 
 esi is after the bad Char.
;;

BadNumberFormat:
    dec esi
L0: lodsb | On al = 'X', lodsb

    .If al = 'H'
        cmp B$esi LowSigns | ja L7>
    .Else_If al = 'D'
        cmp B$esi LowSigns | ja L7>
    .Else_If al = 'B'
        cmp B$esi LowSigns | ja L7>
    .Else
      ; Try to read a Type Marker at the end, and re-run if possible:
L7:     While B$esi > LowSigns | inc esi | End_While | dec esi | lodsb
        If al = 'H'
            On ecx = HexType, Error ecx
        Else_If al = 'D'
            On ecx = DezimalType, Error ecx
        Else_If al = 'B'
            On ecx = BinType, Error ecx
        Else
            Error ecx
        End_If
    .End_If

    dec esi
;;
 esi now points to the last Char of the Number. We overwrite it: We kill the Types Markers
 and we fill at the other end (start), with zeros:
;;
    push edi
        mov edi esi | dec esi | On B$esi = 'X', dec esi
        std
            While B$esi > LowSigns | movsb | End_While
        cld
        While B$edi > LowSigns | mov B$edi '0' | dec edi | End_While
    pop edi

    inc esi | While B$esi = '0' | inc esi | End_While

  ; Cases of, for example, "[<0200h" DataAlignment:
    On B$esi = '<', inc esi

    If al = 'H'
        jmp NackedHexa
    Else_If al = 'D'
        jmp TranslateDecimal
    Else  ; al = 'B'
        jmp NackedBinary
    End_If

____________________________________________________________________________________________

AsciiToQword:
L0: call atof
    On al > CloseBracket, error D$BadRealPtr
    fadd D$edi
    fstp Q$edi

L9: cmp B$esi-1 addSign | jne L9>
        mov al addSign | dec esi | jmp L0<<
L9: cmp B$esi-1 subSign | jne L9>
        mov al subSign | dec esi | jmp L0<<

L9: add edi 8
    mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    mov D$DataListPtr edi
ret

FAsciiToQword:
L0: call atof
    On al > CloseBracket, error D$BadRealPtr
    fadd D$edi
    fstp D$edi

L9: cmp B$esi-1 addSign | jne L9>
        mov al addSign | dec esi | jmp L0<<
L9: cmp B$esi-1 subSign | jne L9>
        mov al subSign | dec esi | jmp L0<<

L9: add edi 4
    mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    mov D$DataListPtr edi
ret


TAsciiToQword:
L0: call atof
    On al > CloseBracket, error D$BadRealPtr
    fadd D$edi
    fstp T$edi

L9:   cmp B$esi-1 addSign | jne L9>
        mov al addSign | dec esi | jmp L0<<
L9:   cmp B$esi-1 subSign | jne L9>
        mov al subSign | dec esi | jmp L0<<

L9: add edi 10
    mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    mov D$DataListPtr edi
ret


ParseFpWords:
    push esi

        mov ax 07FFF

        If B$esi = addSign
            inc esi
        Else_If B$esi = subSign
            inc esi | mov ax 0FFFF
        End_If

L1:     ...If B$esi = 'N'
            ..If B$esi+1 = 'A'
                .If B$esi+2 = 'N'
                    If B$esi+3 < LowSigns
                        mov D$edi 0, D$edi+4 0, W$edi+8 07FFF
                        or W$edi+8 ax | add esi 4 | pop eax | jmp L9>
                    End_If
                .End_If
            ..End_If

        ...Else_If B$esi = 'I'
            ..If B$esi+1 = 'N'
                .If B$esi+2 = 'F'
                    If B$esi+3 < LowSigns
                        mov D$edi 0FFFFFFFF, D$edi+4 0FFFFFFFF, W$edi+8 07FFF
                        or W$edi+8 ax | add esi 4 | pop eax | jmp L9>
                    End_If
                .End_If
            ..End_If

        ...Else_If D$esi = 'QNAN'
          ; QNaN  : S=0  E=7FFF  I=1  F=4000_0000_0000_0000..7FFF_FFFF_FFFF_FFFF
            mov D$edi 0FFFFFFFF, D$edi+4 0FFFFFFFF, W$edi+8 07FFF
            or W$edi+8 ax | add esi 4 | pop eax | jmp L9>

        ...End_If

    pop esi | ret

L9: add edi 10
    mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
    mov D$DataListPtr edi
ret


[DataSign: 0]

StoreDataRef:
    On B$DefSize <> 'D' error D$DsizeForLabPtr
    push edi
        mov edi D$DataRefPtr
        ..If B$Dynamic = &TRUE
            If edi > D$DataRefLimit
                call ExtendTableMemory DataRef, DataRefPtr, DataRefLimit
                mov edi D$DataRefPtr
            End_If
        ..End_If

L0:     lodsb | cmp al LowSigns | jb L1>
        stosb | jmp L0<
L1:     mov al EOI | stosb
        mov eax D$DataListPtr | stosd
        mov eax D$bracketCounter | stosd
        mov al B$DataSign | stosb
        mov al EOI | stosb
      mov D$DataRefPtr edi
    pop edi
ret


; j'ai oubli  de commenter   '<<<<' pourquoi faut-il ajouter un dWord apr s la valeur de
; LEN. Sans ce 0 suppl mentaire, la donn e suivante est  crasee (0). Comme  a, tout
; fonctionne mais je ne sais plus pourquoi!!!!!!!!!!...........

[HeadLenPtr: 0    HeadLenFlag: 0    HeadLenSize: dMem]

StoreOneData:
    mov B$DataSign &FALSE
    push edi

        cmp B$esi+1 MemMarker | jne L0>
            lodsb | mov B$DefSize al | lodsb

L0:     mov edi D$DataListPtr | mov D$edi 0 | mov al B$esi
            cmp al Separators | ja L0>
            inc esi | mov al B$esi

L0:     cmp al 'L' | jne L0>>
            cmp B$esi+1 'E' | jne L0>>
            cmp B$esi+2 'N' | jne L0>>
            cmp B$esi+3 LowSigns | ja L0>>

              ; Direct storage for LEN keyword:
                mov eax D$LenOfDataSet
                .If eax = 0
                    mov B$HeadLenFlag &TRUE, D$HeadLenPtr edi

                  ; Store dummy zero. Will be overwritten, after, at the top of 'StoreDatas'
                  ; HeadLen may be W$ for some Win32 Structures. Byte should be no use:
                    If B$DefSize = dMem
                        stosd | mov D$HeadLenSize dMem, D$LenOfDataSet 4
                    Else_If B$DefSize = wMem
                        stosw | mov D$HeadLenSize wMem, D$LenOfDataSet 2
                    Else_If B$DefSize = bMem
                        stosb | mov D$HeadLenSize bMem, D$LenOfDataSet 1
                    Else
                        error D$LenSizePtr
                    End_If

                .Else_If B$HeadLenFlag = &TRUE
                    error D$MixedLenPtr

                .Else
                  ; Normal Len (after Data) is always a dWord, whatever 'DefSize':
                    stosd | mov D$LenOfDataSet 0

                .End_If

                mov D$DataListPtr edi | add esi 4
    pop edi
    ret

L0:     cmp B$DefSize 'R' | jne L0>
        call AsciiToQword
    pop edi
    ret

L0:     cmp B$DefSize 'H' | jne L0>
        error D$NewHmemPtr
      ; call AsciiToQword
    pop edi
    ret

L0:     cmp B$DefSize 'F' | jne L0>
        call FAsciiToQword
    pop edi
    ret

L0:     cmp B$DefSize 'T' | jne L0>
      ;  push esi
      ;      call ParseFpWords
      ;  pop eax
      ;  On esi = eax,
        call TAsciiToQword
    pop edi
    ret

L0:     mov B$DataSign &FALSE | cmp al SubSign | jne L1>
        mov B$DataSign &TRUE | lodsb | mov al B$esi | jmp L1>
L1:     cmp al addsign | jne L2>
        lodsb | mov al B$esi
L2:     cmp al TextSign | jne L2>
        call TranslateText | jmp E9>>   ; (direct storage of text in Translation routine)
L2:     cmp al '0' | ja L4>
        cmp B$esi+1 '0' | jne L2>
            call TranslateBinary | jmp L5>
L2:     call TranslateHexa | jmp L5>
L4:     cmp al '9' | jbe L4>
            call StoreDataRef | jmp L9>>
L4:     call TranslateDecimal

L5:     cmp edx 0 | je L5>
        On B$DefSize <> 'Q', error D$OverFlowPtr
            jmp Q7>>

L5:     cmp B$DefSize 'D' | je L7>
        cmp B$DefSize 'B' | je L6>
        cmp B$DefSize 'W' | je U0>
        cmp B$DefSize 'U' | je U0>
        cmp B$DefSize 'Q' | je Q7>                              ; keep (Q$ with edx = 0)
            error D$UnknownSizePtr

U0:     cmp eax 0FFFF | ja L8>>                                 ; if here, DefSize = 'W'
        On B$DataSign = &TRUE, neg ax | add W$edi ax | jnc L9>  ; include 0 unicode endMark
            jmp L8>>
L6:     cmp eax 0FF | ja L8>>                                   ; 'B'
        On B$DataSign = &TRUE, neg al | add B$edi al | jnc L9>
            jmp L8>>
L7:     On B$DataSign = &TRUE, neg eax | add D$edi eax | jnc L9>; 'D'
        jmp L8>>
Q7:     If B$DataSign = &TRUE
            push 0 | push 0 | sub D$esp eax | sbb D$esp+4 edx ;jE!
            pop eax | pop edx
        End_If

        add D$edi eax | jnc Q8>
        add edx 1 | On edx = 0, error D$OverFlowPtr
Q8:     add edi 4 | add D$edi edx | jnc Q9>
            jmp L8>>
Q9:     sub edi 4

L9:     cmp B$esi-1 addSign | jne L9>
            mov al addSign | dec esi | jmp L0<<
L9:     cmp B$esi-1 subSign | jne L9>
            mov al subSign | dec esi | jmp L0<<

L9:     cmp B$DefSize 'D' | jne L9>
            add edi 4 | jmp E9>
L9:     cmp B$DefSize 'B' | jne L9>
            inc edi   | jmp E9>
L9:     cmp B$DefSize 'Q' | jne L9>
            add edi 8 | jmp E9>
L9:     add edi 2                         ; good for 'W' and 'U'

E9:     mov eax edi | sub eax D$DataListPtr | add D$LenOfDataSet eax
        mov D$DataListPtr edi
    pop edi
ret

L8: error D$OverFlowPtr

 _________________________________________________________________________________________
;;
 storage of labels and datas founded in data declarations square brackets:
 datas are stored in 'DataList'. Labels are stored in the same 'LabelList' as
 code labels, but with adress high bit set.
 indentifications of either label or data is made through Space or Colon ending signs.
 the only one difficulty is that a declaration might end with a label (in case
 user programmer wish to know some data lenght without using 'len', for exemple)
;;
 _________________________________________________________________________________________

AlignDataSet:
    ..If B$esi = '<'
        lodsb                       ; strip '<'
        On B$esi = Space, lodsb
        If W$esi = '00'
            call TranslateBinary | cmp eax 0 | je L9>>
            dec eax | add D$DataListPtr eax
            inc eax | neg eax | and D$DataListPtr eax

        Else_If B$esi = '0'
            call TranslateHexa | cmp eax 0 | je L9>>
            dec eax | add D$DataListPtr eax
            inc eax | neg eax | and D$DataListPtr eax

        Else_If B$esi < '1'
            ; Don't Align

        Else_If B$esi > '9'
            ; Don't Align

        Else
            call TranslateDecimal
            dec eax | add D$DataListPtr eax
            inc eax | neg eax | and D$DataListPtr eax

        End_If
    ..Else
        push esi
            While B$esi > LowSigns | inc esi | End_While | inc esi
            mov eax 4
            .If B$esi+1 = memMarker
                If B$esi = 'B'
                    mov eax 1
                Else_If B$esi = 'D'
                    mov eax 4
                Else_If B$esi = 'W'
                    mov eax 2
                Else_If B$esi = 'U'
                    mov eax 2
                Else_If B$esi = 'F'
                    mov eax 4
                Else_If B$esi = 'R'
                    mov eax 8
                Else_If B$esi = 'Q'
                    mov eax 8
                Else_If B$esi = 'T'
                    mov eax 16
                End_If
            .End_If
        pop esi
        Align_On_Variable eax D$DataListPtr
    ..End_If
L9: ret


[DataLabelsCounter: ?    CodeLabelsCounter: ?] [DataLoopWinEquate: ? #20]
[LOOPDATAMAX 01_000]

[ColonWanted: ?]

StoreDatas:
    mov esi D$CodeSourceB
    mov B$ErrorLevel 0,  D$bracketCounter 0, D$DataLabelsCounter 0
    mov D$HeadLenPtr 0, D$HeadLenFlag 0, D$HeadLenSize dMem

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

L0: .If B$HeadLenFlag = &TRUE
        mov ebx D$HeadLenPtr, eax D$LenOfDataSet, B$HeadLenFlag &FALSE

        If B$HeadLenSize = dMem
            mov D$ebx eax
        Else_If B$HeadLenSize = wMem
            On eax > 0FFFF, error D$OverWordPtr
            mov W$ebx ax
        Else
            On eax > 0FF, error D$OverBytePtr
            mov B$ebx al
        End_If
    .End_If

L1: lodsb
    If al = TextSign
        While B$esi <> TextSign
            lodsb
        End_While
        lodsb | jmp L1<

    Else_If al = OpenVirtual
        inc D$bracketCounter | add D$StatementsPtr 4
        While al <> CloseVirtual
             lodsb
        End_While
        jmp L1<

    Else_If al = Openbracket
        inc D$bracketCounter | add D$StatementsPtr 4 | jmp L1>

    Else_If al = EOI
        ret

    Else
        jmp L1<

    End_If

L1: mov D$LenOfDataSet 0
    mov B$DefSize 'D'   ; default size: dWord

    call AlignDataSet

L2: push esi
L3:     lodsb                                   ; we search ending char (first for 'text')
        cmp al LowSigns | ja L3<
          cmp al TextSign     | je L5>          ; Text (first sign, no need of last one)
          cmp al Space        | je L5>          ; data
          cmp al Closebracket | je L5>          ; lasting data
          cmp al meEOI        | je L5>          ; no error if no comma...
          cmp al ColonSign    | je L6>          ; label (last sign)
          cmp al NumSign      | je L7>>
          cmp al addsign      | je L5>
          cmp al subSign      | je L5>

          cmp al memMarker    | je L3<
            error D$BadSeparatorPtr

L5: pop esi

    call StoreOneData | jmp L9>>                ; if data

; Case "__:__"
L6:     mov B$ColonWanted &FALSE
        On B$esi = meEOI, inc esi               ; no '|' betweem ':' and data
        mov D$AfterLastDataLabel esi
    pop esi

    call StoreDataLabel                         ; if label
    On B$LocalLabel = &TRUE, error D$NoDataLocalLabelPtr
    inc D$DataLabelsCounter
    mov esi D$AfterLastDataLabel | jmp L9>>     ; Called Subs don't change ESI

; Case "__#__"
L7: On B$esi-2 > PartEnds, error D$WhatIsThisPtr
    On B$ColonWanted = &TRUE, error D$NestedLoopPtr
    pop eax

    cmp B$esi-2 memMarker | je E1>
    cmp B$esi-2 ColonSign | jne E2>

E1:     error D$BadLoopPtr
E2:
    On B$esi < '0', error D$BadLoopNumberPtr
    On B$esi > '9',  error D$BadLoopNumberPtr
    cmp D$DataLoopValue 0 | ja L7>
      If B$esi = '0'
        call TranslateHexa
      Else
        call TranslateDecimal
      End_If
      On edx > 0, error D$DataLoopNumberPtr
      On eax > LOOPDATAMAX, error D$DataLoopNumberPtr
      On eax < 2, error D$SmallDataLoopPtr
      mov D$DataLoopValue eax

L7: dec D$DataLoopValue | cmp D$DataLoopValue 0 | ja L8>      ; full set when edx ...
L7: lodsb
      cmp al LowSigns | ja L7<
        mov B$ColonWanted &TRUE | jmp L9>

L8: mov esi D$AfterLastDataLabel | jmp L2<<

L9: If B$esi-1 = Closebracket
       mov eax D$StatementsPtr, D$eax DoneFlag
       jmp L0<<                    ; lasting data
    End_If
    On B$esi  <> Closebracket,  jmp L2<<                      ; lasting label
    mov eax D$StatementsPtr, D$eax DoneFlag
    inc esi | jmp L0<<

_____________________________________________________________________________________

StoreOneVirtualData:
    cmp B$esi+1 MemMarker | jne L0>
        lodsb | mov B$DefSize al | lodsb

L0: lodsb
        cmp al Separators | ja L0>
            lodsb

L0: cmp al '?' | je L0>
        error D$VirtualDataPtr

L0: .If B$DefSize = 'D'
        mov eax 4
    .Else_If B$DefSize = 'B'
        mov eax 1
    .Else_If B$DefSize = 'W'
        mov eax 2
    .Else_If B$DefSize = 'U'
        mov eax 2
    .Else_If B$DefSize = 'Q'
        mov eax 8
    .Else_If B$DefSize = 'R'
        mov eax 4
    .Else_If B$DefSize = 'H'
        mov eax 8
    .Else_If B$DefSize = 'F'
        mov eax 4
    .Else_If B$DefSize = 'T'
        mov eax 10
    .Else
        error D$UnknownSizePtr
    .End_If

    add D$uVirtualDataSize eax | add D$DataListPtr eax

    On B$esi < Separators, inc esi
ret

;;
 Beware, here, we both read from and copy to 'CodeSourceB'. We strip off at once Brackets
 and Virtual Brackets Declarations. We didn't strip Data Brackets previouly to allow
 error checking of user source Brackets.
;;
[uVirtualDataSize: ?]
[NoVirtualLimit: &FALSE]
[LOOPVDATAMAX 010_0000]

StoreVirtualData:
    mov esi D$CodeSourceB,  edi D$CodeSourceB
    mov B$ErrorLevel 0,  D$bracketCounter 0, D$uVirtualDataSize 0

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4

; Next 'if' could be reused (???) if we want to speed up "Looping" '?' Data.
; (All of this could be much faster...):
;
L0: ;If B$HeadLenFlag = &TRUE
;      mov ebx D$HeadLenPtr, eax D$LenOfDataSet, D$ebx eax, B$HeadLenFlag &FALSE
;    End_If

L1: lodsb
      If al = OpenBracket
          While al <> CloseBracket
              lodsb
              cmp al TextSign | jne T9>
          T0: lodsb | cmp al TextSign | jne T0<
          T9:
          End_While
          inc D$bracketCounter | add D$StatementsPtr 4
          jmp L1<
      End_If
      cmp al OpenVirtual | je L1>               ; search for data declaration
      stosb | cmp al EOI | jne L1<
           On B$esi = OpenVirtual, jmp S0>
           On B$esi = Openbracket, jmp S0>
           add D$StatementsPtr 4
S0:  On B$edi-2 = EOI,  dec edi         ; prevents unwished '||', wich occurs for exemple
                                            ; in: ...|[val: 25]|...  after data strip
        cmp B$esi EOI | jne L0<             ; end of source
          stosb
            mov eax edi | sub eax D$CodeSourceB | mov D$StripLen eax
            mov al EOI, ecx 10 | rep stosb      ; ajout recent pour tester un plantage
          ret

L1: inc D$bracketCounter | add D$StatementsPtr 4

    mov B$DefSize 'D'   ; default size: dWord

  ; Re-Write that shit: Need an 'AlignVirtualDataSet' version, instead.
    push D$DataListPtr
        call AlignDataSet
    pop eax
    If D$DataListPtr > eax
        mov ebx D$DataListPtr | sub ebx eax | add D$uVirtualDataSize ebx
    End_If

L2: push esi
L3:   lodsb                                     ; we search ending char (first for 'text')
        cmp al LowSigns | ja L3<
          cmp al TextSign     | je L4>          ; Text (first sign, no need of last one)
          cmp al Space        | je L5>          ; data
          cmp al CloseVirtual | je L5>          ; lasting data
          cmp al meEOI        | je L5>          ; no error if no comma...
          cmp al ColonSign    | je L6>          ; label (last sign)
          cmp al NumSign      | je L7>>
          cmp al addsign      | je L4>
          cmp al subSign      | je L4>
        jmp L3<

L4: error D$VirtualDataPtr

L5: pop esi | call StoreOneVirtualData | jmp L9>>   ; if data

L6:   mov B$ColonWanted &FALSE
      move D$DataListPtrAtLastColon D$DataListPtr
      On B$esi = meEOI, inc esi                 ; no '|' betweem ':' and data
      mov D$AfterLastDataLabel esi
    pop esi
    call StoreDataLabel                         ; if label
    On B$LocalLabel = &TRUE, error D$NoDataLocalLabelPtr
    inc D$DataLabelsCounter
    mov esi D$AfterLastDataLabel | jmp L9>>     ; Called Subs don't change ESI

L7: On B$esi-2 > PartEnds, error D$WhatIsThisPtr
    On B$ColonWanted = &TRUE, error D$NestedLoopPtr
    pop eax
    cmp B$esi-2 memMarker | je E1>
    cmp B$esi-2 ColonSign | jne E2>
E1:     error D$BadLoopPtr

E2: On B$esi < '0', error D$BadLoopNumberPtr
    On B$esi > '9',  error D$BadLoopNumberPtr

    cmp D$DataLoopValue 0 | ja L7>
        If B$esi = '0'
            call TranslateHexa
        Else
            call TranslateDecimal
        End_If
        On edx > 0, error D$VDataLoopNumberPtr
        If B$NoVirtualLimit = &FALSE
            On eax > LOOPVDATAMAX, error D$VDataLoopNumberPtr
        End_If
        On eax < 2, error D$SmallDataLoopPtr
        mov D$DataLoopValue eax

L7: dec D$DataLoopValue | cmp D$DataLoopValue 0 | ja L8>        ; full set when edx ...
L7: lodsb
      cmp al LowSigns | ja L7<
        mov B$ColonWanted &TRUE | jmp L9>

L8:     If D$DataLoopValue > 1
            mov eax D$DataListPtr | sub eax D$DataListPtrAtLastColon
            mov ecx D$DataLoopValue | dec ecx | mul ecx
            add D$DataListPtr eax | add D$uVirtualDataSize eax
            mov D$DataLoopValue 1
        End_If

        mov esi D$AfterLastDataLabel | jmp L2<<

L9: If B$esi-1 = CloseVirtual
        mov eax D$StatementsPtr, D$eax DoneFlag | jmp L0<<      ; lasting data
    End_If
    On B$esi <> CloseVirtual,  jmp L2<<                         ; lasting label
        inc esi | mov eax D$StatementsPtr, D$eax DoneFlag | jmp L0<<

_________________________________________________________________________________________
 _________________________________________________________________________________________

; Labels deal

[StartOfLabelName: ?  LocalLabel: B$ ?]

E0: error D$BadLabelPtr

StoreDataLabel:
    mov edx D$DataListPtr | mov cl DataLabelFlag | jmp L0>

StoreCodeLabel:
    mov edx D$CodeListPtr | mov cl CodeLabelFlag

L0: push edi
        mov edi D$LabelListPtr | mov ebx 0 | mov D$StartOfLabelName esi
        and B$esi 00_0111_1111

        On B$esi = ColonSign, error D$OrphanColonPtr

L1:     lodsb | cmp al ColonSign | je L2>               ; esi set by caller
        inc ebx                                         ; ebx < lenght of label name
        cmp al '.' | jb E0<
        cmp al 'Z' | ja E0<
        stosb | jmp L1<                                 ; write to LabelList

L2:     mov al EOI | stosb | mov B$LocalLabel &FALSE
        cmp ebx 2 | jne L3>                             ; is it a possible local label?
            mov ah B$esi-3,  al B$esi-2
            cmp ah 'A' | jb L3>
                cmp ah 'Z' | ja L3>
                    cmp al '0' | jb L3>
                        cmp al '9' | ja L3>
                            mov B$LocalLabel &TRUE      ; if yes, multiple decs. allowed

L3:   mov eax edx                                       ; either data or code List Pointer
      stosd                                             ; label offset (code or data)
      mov al cl | stosb                                 ; code or data flag byte
      mov al EOI | stosb

    ;  mov esi D$StartOfLabelName                        ; LabelListPtr > first label letter
      On B$LocalLabel = &FALSE, call SetQwordCheckSum D$LabelListPtr
    ;  On B$LocalLabel = &FALSE, call IsItNewDataOrCodeLabel       ; send error message if not new

      mov D$LabelListPtr edi

      If edi > D$LabelListLimit
        call ExtendTableMemory LabelList, LabelListPtr, LabelListLimit
      End_If

    pop edi
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

OpError: Error D$NotAnOpcodePtr
OperandsError: Error D$OperandsTypesPtr
OperandSizeError: Error D$OperandSizePtr
NoFpAssumeError: Error D$FPregNotAssumedPtr

[BadMnemonic | jmp OpError]
[BadOperand | jmp OperandsError]
[BadOperandSize | jmp OperandSizeError]
[NoFpAssume | jmp NoFpAssumeError]

[op1 ah,  op2 al,  op3 bh,  op4 bl,  op5 ch,  op6 cl,  op7 dh,  op8 dl]

[LineStart: D$ 0,  Operands: B$ 0]

[ParametersData: 0  ParametersNumber: 0
 LongRelative: 0  dis32: 0  imm64: imm32: 0 0   ABSimm32: 0  SignedImm8: 0
 Relative: 0                   ; set with >flag when coding calls or jumps

 LocalSize: B$ 0
 Ereg: 0  SIB: 0
 EregInside: 0  immInside: 0  DisInside: 0   SibInside: 0
 DummyDis: 0                   ; true or false, for SIB without dis and without base

 LabelInside: B$ 0  ExpressedLabels: 0  TrueSize: 0
 FirstParaMem: 0  SecondParaMem: 0
 FirstReg: 0  SecondReg: 0  ThirdReg: 0
 FirstGender: 0  SecondGender: 0  ThirdGender: 0
 FirstRegGender: 0  SecondRegGender: 0  ThirdRegGender: 0

 RMbits: B$ 0  wBit: 0            ; for size encoding 1 or 0
 wBitDisagree: 0  sBit: 0            ; 010B or 0
 sBitPossible: 0  sBitWritten: 0
 OneOperandwBit: 0            ; for temporary storage inside One parameter analyze
 FirstOperandwBit: 0            ; These two
 SecondOperandwBit: 0            ; for test of fitting sizes
 ThirdOperandwBit: 0
 ModBits: 0  SIBreg1: 0  SIBreg2: 0
 ScaledIndex: 0  Base: 0  ScaleFound: 0  Reg1isIndex: 0  Reg2isIndex: 0  TwoRegsFound: 0
 PossibleRmEreg: 0  ExpressedSign: 0

 PossibleImmLabel: B$ 0  PossibleFirstImmLabel: 0

 ParametersDataLen: len]

[relativeFlag  00__10000000_00000000_00000000_00000000]

 _________________________________________________________________________________________
 _________________________________________________________________________________________

; expression deal: ModR/M, SIB and immediate in expressions

[IfEregFound | cmp B$Ereg NotFound | jne #1]

[IfEregNotFound | cmp B$Ereg NotFound | je #1]

[IfNotPartEnd | cmp #1 PartEnds | ja #2]

[IfSeparator | cmp #1 Separators | jb #2]

IsitaReg:
     call Store8cars | On op3 nb Separators,  jmp L5>>      ; a 2 letters reg name?

L0: ifnot op2 'X', L2>
        ifnot op1 'A', L1>
            mov al regAx | jmp L3>
L1:     ifnot op1 'B', L1>
            mov al regBx | jmp L3>
L1:     ifnot op1 'C', L1>
            mov al regCx | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            mov al regDx

L3:     mov W$esi 0 | add esi 3 | mov ah reg | mov B$OneOperandwBit WordSize | ret

L2: ifnot op2 'H', L2>
        ifnot op1 'A', L1>
                mov al regAh | jmp L3>
L1:     ifnot op1 'B', L1>
            mov al regBh | jmp L3>
L1:     ifnot op1 'C', L1>
            mov al regCh | jmp L3>
L1:     On ah <> 'D',  jmp L9>>
            mov al regDh | jmp L3>

L2: ifnot op2 'L', L2>
        ifnot op1 'A', L1>
            mov al regAl | jmp L3>
L1:     ifnot op1 'B', L1>
            mov al regBl | jmp L3>
L1:     ifnot op1 'C', L1>
            mov al, regCl | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            mov al, regDl

L3:     mov W$esi 0 | add esi 3 | mov ah reg | mov B$OneOperandwBit ByteSize | ret

L2: ifnot op2 'P', L2>
        ifnot op1 'B', L1>
            mov al regBp | jmp L3>
L1:     On op1 <> 'S',  jmp L9>>
            mov al regSp | jmp L3>

L2: ifnot op2 'I', L2>
        ifnot op1 'S', L1>
            mov al regSi | jmp L3>
L1:     On op1 <> 'D',  jmp L9>>
            mov al regDi

L3:     mov W$esi 0 | add esi 3 | mov ah reg | mov B$OneOperandwBit WordSize | ret

L2: ifnot op2 'S', L5>
        ifnot op1 'E', L1>
           mov al regEs | jmp L3>
L1:     ifnot op1 'C', L1>
           mov al regCs | jmp L3>
L1:     ifnot op1 'S', L1>
           mov al regSs | jmp L3>
L1:     ifnot op1 'D', L1>
           mov al regDs | jmp L3>
L1:     ifnot op1 'F', L1>
           mov al regFs | jmp L3>
L1:     On op1 <> 'G',  jmp L9>>
           mov al regGs

L3:     mov W$esi 0 | add esi 3 | mov ah sReg | mov B$OneOperandwBit WordSize | ret

; Is it a 3 letters MMX register name?

L5: On op4 nb Separators,  jmp L9>>
       IfNot op1, 'M', L5>
         IfNot op2, 'M', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | mov al bh | mov ah MMreg
             and D$esi 0FF000000 | add esi 4 | mov B$OneOperandwBit DoubleSize | ret

; Is it a 3 letters STx register name?

L5: IfNot op1, 'S', L5>
        IfNot op2, 'T', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | mov al bh | mov ah STreg
             and D$esi 0FF000000 | add esi 4 | mov B$OneOperandwBit DoubleSize | ret

; Is it a 3 letters DRx register name?

L5: IfNot op1, 'D', L5>
        IfNot op2, 'R', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '7' | jg L5>
             sub bh, '0' | mov al bh | mov ah dReg
             and D$esi 0FF000000 | add esi 4 | mov B$OneOperandwBit DoubleSize | ret

; Is it a 3 letters CRx register name?

L5: IfNot op1, 'C', L5>
        IfNot op2, 'R', L5>
           cmp op3, '0' | jb L5>
           cmp op3, '4' | jg L5>
             sub bh, '0' | mov al bh | mov ah cReg
             and D$esi 0FF000000 | add esi 4 | mov B$OneOperandwBit DoubleSize | ret


;IsItAreg32:

L5: On op1 <> 'E',  jmp L9>
        ifnot op3 'X', L3>
            ifnot op2 'A', L2>
                mov al regEAX | jmp L4>
L2:         ifnot op2 'C', L2>
                mov al regECX | jmp L4>
L2:         ifnot op2 'D', L2>
                mov al regEDX | jmp L4>
L2:         On op2 <> 'B',  jmp L9>
            mov al regEBX | jmp L4>
L3:     ifnot op3 'P', L3>
            ifnot op2 'S', L2>
                mov al regESP | jmp L4>
L2:         On op2 <> 'B',  jmp L9>
                mov al regEBP | jmp L4>
L3:     On op3 <> 'I',  jmp L9>
            ifnot op2 'S', L2>
                mov al regESI | jmp L4>
L2:         On op2 <> 'D',  jmp L9>
                mov al regEDI

L4:     and D$esi 0FF000000 | add esi 4 | mov ah, Reg | mov B$OneOperandwBit DoubleSize | ret

L9: ; 4 Chars Register? > XMM.?

    ifnot op1 'X', L9>
        ifnot op2 'M', L9>
            ifnot op3 'M', L9>
                cmp op4, '0' | jb L9>
                    cmp op4, '7' | jg L9>
                        sub op4, '0' | mov al op4 | mov ah XMMreg
                        mov D$esi 0 | add esi 5
                        mov B$OneOperandwBit OctoSize | ret


L9: mov eax 0 | ret


; when called 'E' found, esi points to the next letter
; 'IsItAnEreg' doesn't change esi
; 'SearchForEreg' does.

IsItAnEreg:
    mov B$Ereg NotFound | call Store4cars | IfNotPartEnd op3, L9>>

    IfNotPartEnd B$esi-2, L9>>
        ifnot op2 'X', L2>
            ifnot op1 'A', L1>
                mov B$Ereg regEAX | ret
L1:         ifnot op1 'C', L1>
                mov B$Ereg regECX | ret
L1:         ifnot op1 'D', L1>
                mov B$Ereg regEDX | ret
L1:         ifnot op1 'B', L9>
                mov B$Ereg regEBX | ret
L2:     ifnot op2 'P', L2>
            ifnot op1 'B', L1>
                mov B$Ereg regEBP | ret
L1:         ifnot op1 'S', L9>
                mov B$Ereg regESP | ret
L2:     ifnot op2 'I', L9>
            ifnot op1 'S', L1>
                mov B$Ereg regESI | ret
L1:         ifnot op1 'D', L9>
                mov B$Ereg regEDI | ret
L9: ret


SearchForEreg:
    mov B$Ereg NotFound                     ; this double declaration is NOT useless
L0: lodsb | cmp al, 0 | je L0<
        cmp al Separators | jb L9>
            cmp al PartEnds | jb L0<
                On al = 'E',  call IsItAnEreg
                IfEregNotFound L0<

    mov B$EregInside &TRUE | mov al B$Ereg,  B$PossibleRmEReg al
    add esi 2                               ; esi >>> next char. after Ereg.
    mov B$esi-3 0,  B$esi-2 0,  B$esi-1 0   ; clear eReg
    ret

L9: dec esi                                 ; reach end of text word >>> Stay on EOI
ret                                         ; or Space for further possible calls


; SIB byte is [xx xxx xxx] >>> [sf index base]
; in  ECX*4+EDX  , ECX is the index, 4 is the scale factor, EDX is the base.
; when SearchForScale is called , an Ereg has been found. ESI points to possible '*'

SearchForScale:
    mov B$ScaleFound &FALSE | cmp B$esi MulSign | jne L9>
    IfNotPartEnd B$esi+2,  L8>
    mov al B$esi+1
    cmp al '2' | jne L1>
        mov al 00_0100_0000 | jmp L3>
L1: cmp al '4' | jne L2>
        mov al 00_1000_0000 | jmp L3>
L2: cmp al '8' | jne L8>
        mov al 00_1100_0000
L3: On B$Ereg = regESP,  error D$ESPsibPtr              ; ESP can not be an index
    add esi 2 | mov B$ScaleFound &TRUE
    mov B$esi-2 0,  B$esi-1 0                      ; clear scale
    mov bl B$Ereg | shl bl 3 | or bl al            ; store [xx xxx ...]
    mov B$ScaledIndex bl                           ; in ScaledIndex
L9: ret
L8: error D$ScaleValuePtr


; forbidden use of non extended registers in memory adressing statements:

VerifyNoOtherReg:
    push esi
L0:   lodsb
      cmp al 0 | je L0<
        cmp al Separators | jb L9>
            cmp al PartEnds | jb L0<
                dec esi | call IsItAreg | cmp eax 0 | je L1>
                On B$OneOperandwBit <> DoubleSize, error D$WishEregPtr
                On ah <> Reg, error D$WishEregPtr
L1:             inc esi
L2:             cmp B$esi PartEnds | jb L0>         ; strip remaining text
                    inc esi | jmp L2<
L9: pop esi
ret


SearchForSIB:
    mov B$SibInside &FALSE, B$Reg1isIndex &FALSE, B$Reg2isIndex &FALSE, B$TwoRegsFound &FALSE
    push esi
      call SearchForEreg | IfEregFound L0>
        pop esi
          call VerifyNoOtherReg
        ret

L0: mov al B$Ereg,  B$SIBreg1 al

    call SearchForScale | mov al B$ScaleFound, B$Reg1isIndex al   ; true or FALSE
    call SearchForEreg | IfEregNotFound L1>
        On B$esi-4 <> addSign, error D$ExpressionPtr
        mov al B$Ereg,  B$SIBreg2 al | mov B$TwoRegsFound &TRUE

        call SearchForScale | mov al B$ScaleFound,  B$Reg2isIndex al
        call SearchForEreg | On B$Ereg <> NotFound,  error D$expressionPtr    ; 3 regs forbidden

        .If B$ScaleFound = &FALSE
            If B$SIBreg2 = RegEBP
              ; Exchange the two regs, for saving from having to add the zero Dis:
                mov al B$SIBreg1, bl B$SIBreg2
                mov B$SIBreg2 al, B$SIBreg1 bl
            End_If
        .End_If

L1: mov al B$Reg1isIndex,  ah B$Reg2isIndex                    ; 2 index forbidden
    On ax = 0101,  error D$DoubleIndexPtr
    cmp ax 0 | je L4>                                          ; if no index found >L8

    mov B$SIBinside &TRUE | cmp ax 1 | je L2>            ; if here: 1 reg / 1 index
        mov al B$ScaledIndex | or al B$SIBreg1                 ; reg2 is Index and
        mov B$SIB al | jmp L9>>                                 ; reg1 is base

L2: cmp B$TwoRegsFound &TRUE | je L3>
        mov B$SibReg2 00101                              ; if no base > base = 00101
        mov B$DummyDis &TRUE                                    ;

L3: mov al B$ScaledIndex | or al B$SIBreg2                      ; reg1 is Index
      mov B$SIB al | jmp L9>

L4: cmp B$TwoRegsFound &TRUE | jne L5>                           ; no index, but
        mov B$SibInside &TRUE
        If B$SIBreg1 = 00_100
          ; Other way round, because there is no esp Index (only Base)
            mov al B$SIBreg2 | On al = 00_100, error D$ExpressionPtr
            shl al, 3 | or al B$SIBreg1
        Else
            mov al B$SIBreg1 | shl al, 3 | or al B$SIBreg2
        End_If

        mov B$SIB al | jmp L9>

L5: cmp B$SIBreg1 regESP | jne L9>             ; no index found, only one reg found, but
        mov B$SibInside &TRUE | mov B$SIB 024     ; if reg = ESP >SIB
L9: pop esi
    ret


[immSign: ?]

SearchForImm:      ; if yes, return value set in eax by translating routines
                   ; esi incrementations done by translating routine
    mov B$immSign &FALSE
    push esi | jmp L1>

L9: pop esi | ret

L0:     inc esi
L1:     mov dh B$esi-1,  B$ExpressedSign dh | mov ah B$esi,  al B$esi+1
        cmp ah textSign | je L7>>
        ifSeparator ah, L9<
        cmp ah '0' | jb L0<
        cmp ah '9' | ja L0<
        cmp dh PartEnds | ja L0<
        mov B$immInside &TRUE
        push esi
            IfNot ah, '0', L2>
            IfNot al, '0', L3>
            call translateBinary | jmp L4>
L2:         call TranslateDecimal | jmp L4>
L3:         call TranslateHexa
L4:         On edx > 0, error D$OutOfRangePtr
            cmp B$ExpressedSign Space | je S1>

            cmp B$ExpressedSign addSign | jne S2>
S1:             mov ebx D$imm32 | add D$imm32 eax
                On D$imm32 < ebx, mov B$immSign &FALSE
                jmp L5>
S2:         On B$ExpressedSign <> SubSign,  error D$NotYetSignPtr
            On D$imm32 < eax, mov B$immSign &TRUE
            sub  D$imm32 eax

L5:     pop esi
L6:     mov B$esi 0                                         ; clear imm
        inc esi | cmp B$esi PartEnds | ja L6<
        jmp L1<<
;;
  Why the Api calls (that are "call D$Location" Type...) are routed to 'SearchForImm',
  is because the Text Parameter ('Module.Function') looks like a 'Text' Member, for
  the Parsers of the Instructions Parameters.
;;
L7:     inc esi | call NewSearchApiName | dec esi
        On B$ApiFound = &TRUE, jmp L9<<

        push edi
            mov B$immInside &TRUE | mov edi imm32 | lodsb   ; read text sign
L7:         mov B$esi-1 0 | lodsb                           ; clear imm text
            cmp al TextSign | je L8>
            add B$edi al | inc edi
            On edi = imm32+5,  error D$TxtTooMuchPtr             ; max of 4 letters
            jmp L7<
L8:         mov B$esi-1 0                                   ; clear last text marker sign
        pop edi
        jmp L1<<

L9: pop esi
    cmp D$imm32 07F        ; Test for sBit: turn sBit to 0010 if byte sign imm
        ja L9>                   ; max. is 07F for positive values (and  0FF for
        mov B$sBit 0010               ; negative ones).
        mov B$immSign &FALSE  ; Because no need for storing SignExtended Bytes in 'StoreImm'
L9:     mov eax D$imm32
        and eax 0FFFFFF00 | cmp eax 0FFFFFF00 | jne L9>
        mov B$sBit 0010
        mov B$immSign &FALSE  ; Because no need for storing SignExtended Bytes in 'StoreImm'
L9: ret

;;
 I don't use upper chunk (from 'L9: pop esi'), because of the Sign Extensions able
 Opcodes. In case of Byte immediate with a possible immediate Label, we can't know
 here if there will be or not some trailing Label. Example:

 > and D$Label1+020 Label2-4

 Label2 will only be computed and the end of job, *after* other encodages. There is
 a test Routine, 'sim', that does this complicated job at the end of OpCode encodage.
 In cases of negative Sign Extended imm Bytes, the 'B$immSign' is turned &FALSE there
 to solve the problem of overflow tests done in 'StoreImm', for negative Bytes stored
 as dWords for 'non-Sign-Extended' Opcodes.
;;
____________________________________________________________________________________________

; Search for a displacement. If found, return value set by translating routine in eax:

SearchForDis:
    push esi | jmp L1>
L0:     inc esi
L1:     mov dh B$esi-1,  B$ExpressedSign dh
        mov ah B$esi,  al B$esi+1
        cmp ah 0 | je L0<
        ifSeparator ah, L8>>
        cmp ah '0' | jb L0<
        cmp ah '9' | ja L0<
        cmp dh PartEnds | ja L0<
        mov B$DisInside &TRUE
        push esi
            IfNot ah, '0', L2>
            IfNot al, '0', L3>
            call translateBinary | jmp L4>
L2:         call TranslateDecimal | jmp L4>
L3:         call TranslateHexa
L4:         On edx > 0, error D$OutOfRangePtr
            mov bl B$esi-1
            If bl < Separators
            Else_If bl = addSign
            Else_If bl = subSign
            Else_If bl < LowSigns
                Error D$ExpressionPtr
            End_If
            cmp B$ExpressedSign Space | je S1>  ; ... ???? What case ???? ...
            cmp B$ExpressedSign 0 | je S1>
            cmp B$ExpressedSign addSign | jne S2>
                test D$Dis32 0_8000_0000 | jz S1>
                    neg D$Dis32 | sub D$Dis32 eax | neg D$Dis32 | jmp L6>
S1:                 add D$Dis32 eax | jmp L6>
S2:         On B$ExpressedSign <> SubSign,  error D$NotYetSignPtr
            test D$Dis32 0_8000_0000 | jz L5>
                neg D$Dis32 | add D$Dis32 eax | neg D$Dis32 | jmp L6>
L5:                 sub D$Dis32 eax
L6:     pop esi
L7:     mov B$esi 0
        inc esi | cmp B$esi PartEnds | ja L7<
        jmp L1<<

L8: pop esi

    mov eax D$Dis32        ; 080 > -128          //  07F > +127
    If eax > 0FFFF_FF7F    ; 0FFFF_FF80 > -128  //   0FFFF_FF7F > -129
        mov B$LongRelative &FALSE
    Else_If eax < 080
        mov B$LongRelative &FALSE
    Else
        mov B$LongRelative &TRUE
    End_If
ret


SearchForLabel:
    push esi
        mov edi D$CodeRefPtr
L0:     lodsb | cmp al 0 | je L0<
                cmp al Separators | jb L9>>
                cmp al 'A' | jb L0<
                cmp al, 'Z' | ja L0<
        On B$esi-2 = AddSign,  mov B$esi-2 0
        cmp al 'E' | jne L1>
        push eax
            call IsItAnEreg                 ; usefull only in case of mem adressing
        pop eax
        IfEregNotFound L1>
            add esi 2 | jmp L0<             ; (Mod/RM byte done after)

L1:     mov B$esi-1 0                       ; clear label evocation

      ; Local size is used for Local Labels coding
        .If al = '>'
            add B$LocalSize 4     ; > 00100    >> 001000
            If B$LocalSize = 4
              ; OK.
            Else_If B$LocalSize <> 8
E7:             error D$WhatIsThisPtr
            End_If
            If B$esi = '>'
              ; OK
            Else_If B$esi > Separators
                jmp E7<
            End_If
        .Else_If al = '<'
            add B$LocalSize 1     ; < 001      << 0010
            If B$LocalSize = 1
              ; OK.
            Else_If B$LocalSize <> 2
                jmp E7<
            End_If
            If B$esi = '<'
              ; OK
            Else_If B$esi > Separators
                jmp E7<
            End_If

        .End_If

        .If D$esi-9 = 'LOOP'
            If B$LocalSize = 0
                ; OK.
            Else_If B$LocalSize <> 1
                error D$LongLoopPtr
            End_If
        .End_If

        stosb                               ; write label name in CodeRef
        lodsb | IfNotPartEnd al, L1<<
        mov eax D$CodeRefPtr | add eax 2 | cmp eax edi | jne L2>  ; local label without '<'?
          mov ah B$edi-2,  al B$edi-1
          cmp ah 'A' | jb L2>
            cmp ah 'Z' | ja L2>
              cmp al '0' | jb L2>
                cmp al '9' | ja L2>         ; Local label without direction specifier?
                  mov al '<' | stosb        ; default, for exemple for LOOP L0
                    mov B$LocalSize 1       ; UpShort
L2:       mov al EOI | stosb
          inc B$ExpressedLabels
          On B$ExpressedLabels > 2, error D$TooMuchLabelsPtr
          mov B$LabelInside &TRUE,  B$DisInside &TRUE

L9:     mov D$CodeRefPtr edi
    pop esi
ret


SearchForModRMreg:
    mov cl 32 | cmp B$LabelInside &TRUE | je L0>
    mov eax D$Dis32

    On B$LongRelative = &FALSE, mov cl 8
                                      ; for ByteSize tests on Displacement
                                                ; (080 >>> -128)
L0: push esi
        cmp B$SIBinside &TRUE | je L0>                      ; no SIB found? >>> no more regs
            cmp B$EregInside &FALSE | je L1>                ; may be, an Ereg?
                mov al B$PossibleRmEreg | mov B$RMbits al | jmp L3>>

; no RMreg > choice: simple dis32, dis followed by SIB byte, SIB byte only:

L0:     cmp B$DisInside &TRUE | je L2>
            mov B$ModBits 0 | mov B$RMbits 00100 | jmp L9>> ; SIB with no dis

L1:     cmp B$DisInside &TRUE | jne L9>>                    ; nothing at all > exit
            mov B$ModBits 0 | mov B$RMbits 00101 | jmp L9>> ; dis32 with no SIB and no reg

L2:     mov B$RMbits 00100 | cmp cl 32 | je L2>
            cmp B$DummyDis &TRUE | je L2>
            mov B$ModBits 00_01_000_000 | jmp L9>           ; dis8 + SIB
L2:             If B$TwoRegsFound = &TRUE
                    mov B$ModBits 00_10_000_000             ; dis32 + SIB
                Else_If B$Sib = 024
                    mov B$ModBits 00_10_000_000             ; dis32 + esp only SIB
                Else
                    mov B$ModBits 0                         ; dis32 + SIB with no Base
                End_If
                jmp L9>

; RMreg found > choice: no dis, dis8, dis32 (if reg = EBP and no dis > add zero 8bits dis)

L3:     cmp B$DisInside &FALSE | je L6>
            cmp cl 32 | je L5>
L4:             mov B$ModBits 00_01_000_000 | jmp L9>       ; dis8 + reg
L5:             mov B$ModBits 00_10_000_000 | jmp L9>       ; dis32 + reg

L6:     cmp B$RmBits regEBP | jne L7>                       ; reg EBP >>> add zero dis
            mov B$DisInside &TRUE | jmp L4<

L7:     mov B$ModBits 0                                     ; reg without displacement

L9: pop esi

  ; This is to force things like "push D$fs:+0' to be encoded Long, and not short dis form:
    cmp B$DisInside &TRUE | jne L9>
        cmp B$LabelInside &FALSE | jne L9>
            cmp B$EregInside &FALSE | jne L9>
                mov B$LongRelative &TRUE

L9: ret


SearchForFirstFPUimm:
    mov B$FirstGender imm | jmp L0>
SearchForSecondFPUimm:
    mov B$SecondGender imm

L0: push esi
        call atof
        On al > CloseBracket, error D$BadRealPtr
        fstp D$imm32
        mov B$ImmInside &TRUE, B$immSign &FALSE
    pop esi
L0: mov B$esi 0                                     ; clear imm
    inc esi | cmp B$esi PartEnds | ja L0<
  ret


FirstParameterAnalyze:
    mov esi D$LineStart

L0: lodsb                               ; simply increase esi over first space; after this,
    cmp al Space | jne L0<                    ; save esi pointing to parameter (> push/pop)

    cmp B$esi+1 memMarker | jne L4>>               ; if mem marker found, store ascii value
      mov al B$esi | mov B$FirstParaMem al
      mov W$esi 0 | add esi 2                     ; (see equ. for bMem, wMem, dMem)
      If B$esi = MemMarker
        inc esi
      End_If
      mov B$FirstGender mem

        On B$esi = EOI, error D$ParameterPtr

        .If al = dMem
             mov B$FirstOperandwbit DoubleSize   ; D$            4 bytes
        .Else_If al = bMem
             mov B$FirstOperandwbit ByteSize     ; B$            1 byte
        .Else_If al = wMem
             mov B$FirstOperandwbit WordSize     ; W$            2 bytes
        .Else_If al = qMem
             mov B$FirstOperandwbit QuadSize     ; Q$            8 bytes
        .Else_If al = rMem
             mov B$FirstOperandwbit QuadSize     ; R$ = FPU Q$   8 bytes
      ;  .Else_If al = hMem
      ;       error NewHmem
       ;      mov B$FirstOperandwbit QuadSize     ; H$ = FPU Q$   8 bytes >>> 16 bytes (!!!)
        .Else_If al = fMem
             If B$esi < 'A'
                 call SearchForFirstFPUimm | ret     ; exemple: push F$45.2
             Else
                 mov B$FirstOperandwbit DoubleSize   ; F$ = FPU D$
             End_If
        .Else_If al = tMem
             mov B$FirstOperandwbit TenSize      ; T$ = FPU 10 Bytes / 80 bits (m80)
        .Else_If al = xMem
            mov B$FirstOperandwbit XSize         ; Weird and XMM sizes
        .Else_If al = oMem
            mov B$FirstOperandwbit OctoSize         ; Weird and XMM sizes
        .Else
            On al = hMem, error D$NewHmemPtr
            error D$UnknownSizePtr
        .End_If

L3:   call SearchForSIB
      call SearchForDis
      call SearchForLabel
      ...If B$SibInside = &TRUE
        ..If B$DisInside = &FALSE
            .If B$ParametersNumber = 1
                If B$TwoRegsFound = &TRUE
                    mov al B$SIB | and al 00_111
                    On al = 00_101, mov B$DisInside &TRUE, B$LongRelative &true
                End_If
            .End_If
        ..End_If
    ...End_If
      call SearchForModRMreg
        or B$ExpressedLabels 1                           ; we want B$ExpressedLabels < 3
        On B$ExpressedLabels > 2, error D$TooMuchLabelsPtr ; but only one Lab per member
    ret

L4: call IsItaReg | cmp ah 0 | je L5>
      mov B$FirstGender reg,  B$FirstReg al,  B$FirstRegGender ah
      mov al B$OneOperandwbit,  B$FirstOperandwbit al
    ret

L5: call SearchForImm | cmp B$ImmInside &FALSE | je L6>
      mov B$FirstGender imm | mov B$PossibleFirstImmLabel &TRUE
    ret

L6: If B$ApiFound = &TRUE   ; 'NewSearchApiName'
        mov B$DisInside &TRUE, B$ApiFound &FALSE, B$FirstGender mem
        mov B$ModBits 0, B$RmBits 00101
        mov B$FirstOperandwBit doubleSize
        ret
    End_If

L6: call SearchForLabel | On B$LabelInside = &FALSE,  error D$UnknownParameterPtr
      mov B$FirstGender dis
      or B$ExpressedLabels 1                            ; we want B$ExpressedLabels < 3
      On B$ExpressedLabels > 2, error D$TooMuchLabelsPtr     ; but only one Lab per member
    ret
____________________________________________________________________________________________

SecondParameterAnalyze:
    mov esi D$LineStart

L0: lodsb                                        ; simply increase esi over second space
    cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<

    cmp B$esi+1 memMarker | jne L5>>             ; if mem marker found, store ascii value
        mov al B$esi | mov B$secondParaMem al
        mov W$esi 0 | add esi 2                  ; (see equ. for bMem, wMem, dMem, ...)
        mov B$secondGender mem

        On B$esi = EOI, error D$ParameterPtr

        .If al = dMem
             mov B$SecondOperandwBit DoubleSize   ; D$
        .Else_If al = bMem
             mov B$SecondOperandwBit ByteSize     ; B$
        .Else_If al = wMem
             mov B$SecondOperandwBit WordSize     ; W$
        .Else_If al = qMem
             mov B$SecondOperandwBit QuadSize     ; Q$
        .Else_If al = rMem
             mov B$SecondOperandwBit QuadSize     ; R$ = FPU Q$
        .Else_If al = hMem
             error D$NewHmemPtr
       ;      mov B$SecondOperandwBit QuadSize     ; H$ = FPU Q$ (!!! > 16 bytes / 128 bits !!!)
        .Else_If al = fMem
             If B$esi < 'A'
                 call SearchForSecondFPUimm | ret      ; exemple: mov eax F$45.2
             Else
                 mov B$SecondOperandwBit DoubleSize   ; F$ = FPU D$
             End_If

        .Else_If al = tMem
             mov B$SecondOperandwBit TenSize      ; T$ = FPU 10 Bytes
        .Else_If al = xMem
            mov B$SecondOperandwbit XSize         ; Weird and XMM sizes
        .Else_If al = oMem
            mov B$SecondOperandwbit OctoSize      ; XMM sizes
        .Else
            On al = hMem, error D$NewHmemPtr
            error D$UnknownSizePtr
        .End_If

L4:     call SearchForSIB
        call SearchForDis
        call SearchForLabel
        ...If B$SibInside = &TRUE
            ..If B$DisInside = &FALSE
                .If B$FirstGender = reg
                    If B$TwoRegsFound = &TRUE
                        mov al B$SIB | and al 00_111
                        On al = 00_101, mov B$DisInside &TRUE, B$LongRelative &FALSE
                    End_If
                .End_If
            ..End_If
        ...End_If
        call SearchForModRMreg
    ret

L5:   call IsItaReg | cmp ah 0 | je L6>
      mov B$secondGender reg,  B$secondReg al,  B$SecondRegGender ah
      mov al B$OneOperandwbit,  B$secondOperandwbit al

      ...If B$SibInside = &TRUE
        ..If B$DisInside = &FALSE
            .If B$FirstGender = Mem
                If B$TwoRegsFound = &TRUE
                    mov al B$SIB | and al 00_111
                    On al = 00_101, mov B$DisInside &TRUE, B$LongRelative &FALSE
                    call SearchForModRMreg
                End_If
            .End_If
        ..End_If
    ...End_If

    ret

L6:   call SearchForImm
      cmp B$ImmInside &FALSE | je L7>
      mov B$secondGender imm | mov B$PossibleImmLabel &TRUE
    ret

L7: If B$ApiFound = &TRUE   ; 'NewSearchApiName'
        mov B$DisInside &TRUE, B$ApiFound &FALSE, B$SecondGender mem
        mov B$ModBits 0, B$RmBits 00101
        mov B$SecondOperandwBit doubleSize
        ret
    End_If

L7:   cmp D$DisInside &TRUE | jne L8>
      mov B$secondGender imm | mov B$PossibleImmLabel &TRUE
    ret

L8:  If B$ExpressedLabels = 0
        call SearchForLabel
        On B$LabelInside = &FALSE,  error D$UnknownParameterPtr
        mov B$SecondGender dis
      Else                                     ; Case of 'mov D$Lab1 Lab2'
        mov B$ImmInside &TRUE, B$secondGender imm, B$PossibleImmLabel &TRUE
      End_If                                   ; Lab2 checked by PossibleImmLabel
      mov B$sBit 0
    ret

FirstParameterLabel:
    mov esi D$LineStart | jmp L1>

SecondParameterLabel:
    mov esi D$LineStart
L0: lodsb | cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<
    push esi
      mov edi D$CodeRefPtr
L0:   lodsb | cmp al 0 | je L0<
              cmp al Separators | jb L9>>
              cmp al 'A' | jb L0<
              cmp al, 'Z' | ja L0<
      On B$esi-2 = AddSign,  mov B$esi-2 0
      cmp al 'E' | jne L1>
      push eax
        call IsItAnEreg                      ; usefull only in case of mem adressing
      pop eax
          IfEregNotFound L1>
            add esi 2 | jmp L0<              ; (Mod/RM byte done after)
L1:     cmp al  '>' | je A1>
        cmp al  '<' | jne A2>
A1:       error D$NoLocalPtr
A2:     stosb                                ; write label name in CodeRef
        mov B$esi-1 0
        lodsb | IfNotPartEnd al, L1<
        mov eax D$CodeRefPtr | add eax 2 | cmp eax edi | jne L2>  ; no local label here
          mov ah B$edi-2,  al B$edi-1
          cmp ah 'A' | jb L2>
            cmp ah 'Z' | ja L2>
              cmp al '0' | jb L2>
                cmp al '9' | ja L2>
                  jmp A1<                                          ; no local label here
L2:       mov al EOI | stosb
          mov eax D$CodeListPtr
          On B$immInside = &TRUE, sub eax 4
          stosd

          mov eax D$StatementsPtr, eax D$eax | stosd     ; write pointer to source.
          mov al EOI | stosb |  mov D$CodeRefPtr edi

          If B$immInside = &FALSE
             mov edi D$CodeListPtr, eax 0 | stosd | mov D$CodeListPtr edi
          End_If

        ; Very stupid. 'FirstParameterLabel' /'SecondParameterLabel' to be re-structured:
          If B$ParametersNumber > 1
            On B$FirstOperandWbit <> DoubleSize, error D$MissTypePtr
          End_If
L9: pop esi
ret
____________________________________________________________________________________________

; Used only in case of SHLD / SHRD. third parameter must be either imm8 or CL

ThirdParameterAnalyze:
    mov esi D$LineStart
L0: lodsb                            ; Simply increases esi over third space; after this,
    cmp al Space | jne L0<
L1: lodsb | cmp al Space | jne L1<
L2: lodsb | cmp al Space | jne L2<

        call IsItaReg | cmp ah 0 | je L3>
        mov B$ThirdGender reg,  B$ThirdReg al,  B$ThirdRegGender ah
        mov al B$OneOperandwbit,  B$ThirdOperandwbit al
    ret

L3:     call SearchForImm
        cmp B$ImmInside &FALSE | je L4>
        mov B$ThirdGender imm
    ret

L4: error D$ParameterPtr
 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 Main routine for one assembler line encoding (Data lines have been analyzed before)
 esi points to the first letter of an instruction in CodeSourceB
 we may find a label, one or more prefix(es), a mnemonic and parameters.
 Translates StrippedLine in OpCodes.
 after Data Storage, clean source is in  CodeSourceB as input
 results in:
            - LabelList      (data labels done with storage and now code labels)
            - CodeRef        (labels adresses evocations in codeList -will be used by
                              'fillCodeSymbols', in Main.a-)
            - CodeList       (true code for .code section)
            - Relocation     (for .reloc)

 i call a 'Line' one set of statements between between '||'.
;;
 _________________________________________________________________________________________
 _________________________________________________________________________________________

StoreSIB:
   mov edi D$CodeListPtr | mov al B$SIB | stosb | inc D$CodeListPtr
ret
;;
 Label evocations are fulfill by 'Fill'. In case, for exemple, of
 'MOV EDI, My_Data_Label + 8', we store '8'
 as 'LongRelative' default is true, it is used here for any non short relative. this
 is to say even for not relative at all statements (Intel doc is killing on that point)
;;
StoreDis:
    If B$ApiDis = &TRUE
        mov B$ApiDis &FALSE
        mov edi D$CodeRefPtr, eax D$CodeListPtr     ; Store actual code offset in
        stosd                                       ; Coderef for ending 'Fill' routine
        mov al EOI | stosb
        mov D$CodeRefPtr edi
;;
  The CodeRef registration of Api calls does not have the Source Pointer Record.
  (No use as all Errors Checking have already been done when collecting the Api
  calls). So, only // 0FF / PointerToCode / EOI //, here, where "0FF" is written
  by 'NewSearchApiName' (called by 'SearchForImm').
  
  Why the Api calls (that are "call D$Location" Type...) are routed to 'SearchForImm',
  is because the Text Parameter ('Module.Function') looks like a 'Text' Parameter,
  for the Parsers.
;;
        mov edi D$CodeListPtr
        mov eax D$Dis32 | stosd
        mov D$CodeListPtr edi
        ret
    End_If

    cmp B$LabelInside &TRUE | jne L1>
        mov edi D$CodeRefPtr, eax D$CodeListPtr     ; Store actual code offset in
        or eax D$Relative | stosd                   ; Coderef for ending 'Fill' routine
        mov eax D$StatementsPtr, eax D$eax | stosd  ; write pointer to source:
        mov al EOI | stosb
        mov D$CodeRefPtr edi

L1: mov edi D$CodeListPtr
    cmp B$LabelInside &TRUE | jne L2>
      cmp B$LocalSize 4 | je L4>
        cmp B$LocalSize 1 | je L4>
          jmp L3>

L2: cmp B$DummyDis &TRUE | je L3>
    cmp B$LongRelative &TRUE | jne L4>              ; default is true

L3:     mov eax D$Dis32 | stosd                     ; Store displacement in code
        mov D$CodeListPtr edi
    ret

L4:     mov al B$Dis32 | stosb
        mov D$CodeListPtr edi
ret


StoreImm:
    cmp B$immSign &TRUE | jne L0>
        cmp B$TrueSize ByteSize | jne L1>
            On D$imm32 < 0_FFFF_FF00,  error D$overflowPtr
            and D$imm32 0FF | jmp L0>
L1:     cmp B$TrueSize WordSize | jne L0>
            On D$imm32 < 0_FFFF_0000,  error D$overflowPtr
            and D$imm32 0FFFF

L0: mov edi D$CodeListPtr
    cmp B$TrueSize DoubleSize | je L2>
    cmp B$TrueSize ByteSize | je L3>

L1: On D$imm32 > 0FFFF,  error D$overflowPtr     ; word size
    mov ax W$imm32 | stosw | jmp L9>

L2: mov eax D$imm32 | stosd | jmp L9>       ; double size

L3: On D$imm32 > 0FF,  error D$overflowPtr       ; Byte size
    mov al B$imm32 | stosb

L9: mov D$CodeListPtr edi
ret

;;
 wBit is used when encoding instruction as 0 or 1, telling if it is byte or full
 size. full size is either 2 or 4 bytes: 32 bits > double word. With operand size
 override prefix, it is turn to word.
 'TrueSize' is used internally to hold lenght of code to write when fixing adresses
 of symbols after encoding (see Main)
;;
Store2Wbit:
    mov al B$FirstOperandWbit | cmp B$secondOperandWbit al | je L0>
        mov B$wBitDisagree &TRUE
ret


StoreFirstWbit:
    mov al B$FirstOperandWbit

StoreWbit:
L0: cmp al WordSize | jne L1>
        mov edi D$CodeListPtr
        mov B$edi 066 | inc D$CodeListPtr        ; write operand size override prefix
L1:     mov B$TrueSize al | and al 1 | mov B$wBit al
ret


FixInstructionType:
    mov edi D$CodeListPtr
    cmp B$FirstGender reg | jne L3>
      cmp B$SecondGender reg | jne L0>
        mov B$Operands RegToReg | call Store2Wbit | ret
L0:   cmp B$SecondGender mem | jne L1>
        mov B$Operands MemToReg | call Store2Wbit | ret
L1:   cmp B$SecondGender dis | je L2>
      cmp B$SecondGender imm | jne L9>>
L2:     mov B$Operands ImmToReg | call StoreFirstWbit | ret

L3: cmp B$FirstGender Mem | jne L6>
      cmp B$SecondGender reg | jne L4>
        mov B$Operands RegToMem | call Store2Wbit | ret

L4:   cmp B$SecondGender dis | je L5>
      cmp B$SecondGender Imm | jne L9>
L5:
        mov B$Operands ImmToMem | call StoreFirstWbit | ret

L6: cmp B$FirstGender imm | jne L9>
      cmp B$SecondGender reg | jne L7>
        mov B$Operands RegToImm | ret
L7:   cmp B$SecondGender imm | jne L9>
        mov B$Operands ImmToImm
        mov esi D$LineStart | lodsb | cmp al, 'E' | jne L9>
             ret           ; exemple: ENTER 8, 2
                           ; exceptions: Job will be done when coding ENTER
L9: error D$MixTypePtr


SearchLineLabel:
L0: lodsb
    cmp al EOI | jbe L2>                        ; case of simple mnemonics, like |AAA|
        cmp al Space | jne L3>
L2:  mov esi D$LineStart | jmp L9>
L3:     cmp al ColonSign | jne L0<
        push esi
            mov esi D$LineStart | call StoreCodeLabel  ; in 'LabelList'
            inc D$CodeLabelsCounter
        pop esi
        mov D$LineStart esi                     ; new instruction's first letter after label
        If B$esi = ColonSign
            inc esi | inc D$LineStart           ; '::' for .export in DLL.
        End_If
    jmp L0<                                     ; Another Label following?:
L9: ret


ClearParameters:                                ;(clear parameters data area)
    push edi
        mov al 0,  edi ParametersData,  ecx D$ParametersDataLen | rep stosb
    pop edi
ret


; Count of spaces = number of parameters (previous componants are cleared if any)

ParametersCount:
    mov cl 0

    On D$esi = 'ENTE', jmp L9>             ; WordImm / ByteImm Param for Enter...
L0: lodsb | cmp al space | jne L1>
        inc cl                           ; one space >  +1 parameter in cl
        If B$esi-2 = MemMarker
            dec cl | mov B$esi-1 MemMarker
        End_If
L1: cmp al EOI | ja L0<
L9: mov B$ParametersNumber cl
ret

 _______________________________________________________________________________________
;;
 encodage of Apis calls is not done in 'Encode' but here with a memory indirect call:
  1111 1111 : mod 010 r/m
 the 3 routines are linked by jumps when needed (not by calls)
;;

[ApiFound: ?]

; Called with esi pointing to the next Char after 'TextSign'. Called from 'SearchForImm'
; because 'xxxx' may as well be a immediate.


; Previous holding of numbered Api (!!!...):
;
;    push edi
;        mov edx D$ebx               ; Table 1 pointers Ptr
;        mov ebx D$ebx+16            ; table 2 pointers Ptr
;        add edx ecx | add ebx ecx
;
;L0:     mov edi D$edx               ; pointer to function name
;        test edi 0_8000_0000 | jz L1>
;L5:         lodsb | cmp al TextSign | jne L5<
;                pop edi | jmp EncodeApiCall

[ApiByNumber: ? #4] [ApiDis: ?]

NewSearchApiName:
; A Description of .Import is in the Disassembler 'CheckImport'.
    On D$uImportSize = 0, ret

    pushad

    mov B$ApiFound &FALSE

  ; Adjust esi on the FunctionName first Char. We do not take care of DLL Name,
  ; because this verification has already been done by 'BuildImport'. Instead,
  ; we search along all Functions Chunks.
    While B$esi <> TextSign | inc esi | End_While
    While B$esi <> '.'
        dec esi | On B$esi = TextSign, jmp L0>
    End_While
L0: inc esi                             ; Skip '.' or TextSign >>> FunctionName
    mov edx D$CodeList | add edx 0400   ; ebx > Import header dll pointers
    mov ecx edx                         ; to ajust adress, down there.
    sub ecx 01000                       ; Base Of Import.

  ; edx will keep track of the header
  ; ebx will keep track of the Functions Pointers List
L0: mov ebx D$edx+16

  ; Cases of DLL without .Import:
    If ebx = 0
        popad | ret
    End_If

    add ebx ecx      ; Address Table Pointer

  ; ebx > FunctionName Pointers List
L1: mov edi D$ebx
  ; ??????????????
  ; Un-resolved problem: What if several Functions in different DLLs having the same
  ; Function number???!!!...
    test edi 0_8000_0000 | jz L2>
        If B$esi <> '0'
            push esi | jmp L4>
        End_If

        mov eax edi | xor eax 0_8000_0000

        push esi, ebx, eax
            While B$esi = '0' | inc esi | End_While
            mov ebx 0
H0:         lodsb | sub al '0' | On al > 9, sub al 7
            shl ebx 4 | or bl al
            cmp B$esi TextSign | jne H0<
        pop eax

        If eax = ebx
            pop ebx | jmp L5>
        End_If

        pop ebx | jmp L4>

L2: add edi ecx | add edi 2
  ; edi > Function name (+2 is for the leading word).

L3: push esi
L3:     mov al B$esi | mov ah B$edi | inc esi | inc edi | cmp al ah | je L3<
        cmp al TextSign | jne L4>
        cmp ah 0 | je L5>

  ; Not found:
L4: pop esi

    add ebx 4
    .If D$ebx = 0
        add edx 20
        If D$edx = 0
            popad | ret
        Else
            jmp L0<<
        End_If
    .Else
        jmp L1<<
    .End_If

  ; Found:
L5: pop esi
  ; Clear the 'DLL.Function':
    While B$esi <> TextSign | dec esi | End_While
    Do
        mov B$esi 0 | inc esi
    Loop_Until B$esi = TextSign
    mov B$esi 0

    mov eax ebx
    If D$SavingExtension = '.SYS'
        add eax DRIVERDEFAULT | jmp L1>
    Else_If D$RelocsWanted = &TRUE ;D$SavingExtension = '.DLL' ; jE!
        add eax D$LinkerDllDefault
L1:     push ebx
          ; Comments in 'FillCodeSymbols':
            mov ebx D$CodeRefPtr, B$ebx 0FF ;, D$ebx+1 edi, B$ebx+5 EOI
            mov B$ApiDis &TRUE
            add D$CodeRefPtr 1 ;6
          ; The CodeRef thingies are now completed by the 'StoreDis', in order
          ; to assume the Relocations of Api Calls in DLLs.
        pop ebx
    Else
        add eax LINKERDEFAULT
    End_If
    sub eax ecx | mov D$Dis32 eax

    mov B$ApiFound &TRUE

    popad
ret
____________________________________________________________________________________________

;;
 Encodage of an instruction is done. At this time, we control that all parameters
 have been translated (they are supposed to be zeroed)? Reg BL is used to jump
 tests over mnemonic (not zeroed):
;;

PointNextInstruction:
    mov esi D$LineStart, bl 0
L0: lodsb
    cmp al Space | jne L1>
      mov bl 1 | jmp L0<
L1: cmp bl 1 | jne L1>
      cmp al '0' | jbe L1>
     ;  call InternalNextControl  ; (for developpement only)
        error D$UnknownParameterPtr
L1: cmp B$esi EOI | ja L0<
    cmp B$esi 0 | je L0<
  ret


; Private developper control of what exactly is wrong (not zeroed):

InternalNextControl:
    mov esi D$LineStart, ecx 80
L5: lodsb | On al < 32, mov B$esi-1 '.' | loop L5<
    mov B$esi 0
    pushad
        call 'USER32.MessageBoxA' D$hwnd, D$LineStart, ErrorMessageTitle, &MB_SYSTEMMODAL
    popad
ret


Proc IsTrueLocalLabel:
    Argument @Pointer
    Uses ebx

        mov ebx D@Pointer

        ..If B$ebx+2 = ColonSign
            mov al B$ebx
            .If al < 'A'

            .Else_If al > 'Z'

            .Else
                mov al B$ebx+1
                If al < '0'

                Else_If al > '9'

                Else
                    mov eax &TRUE | ExitP
                End_If
            .End_If
        ..End_If

        mov eax &FALSE
EndP


; 'DB 0....' encounted:

StoreFlatData:
    push eax
        If al = 'B'
            ; OK

        Else_If al = 'W'
            ; OK
        Else_If al = 'U'
            ; OK
        Else_If al = 'D'
            ; OK
        Else_If al = 'Q'
            ; OK
        Else_If al = 'F'
            ; OK
        Else_If al = 'R'
            ; OK
        Else_If al = 'T'
            ; OK
        Else_If al = 'X'
            ; OK

        Else
            error D$NotAnOpcodePtr
        End_If

    ;  ; We must have a Label at the beginning of Line:
    ;    mov ebx esi | While B$ebx > EOI | dec ebx | End_While | inc ebx
    ;  ; Cannot be a True Local Label:
    ;    call IsTrueLocalLabel ebx | On eax = &TRUE, error D$LocalDefLabelPtr
    ;  ; But must be a Label:
    ;    While B$ebx > LowSigns | inc ebx | End_While
    ;    On B$ebx <> ColonSign, error D$WhatIsThisPtr
    pop eax

  ; in: al = Size_Type // esi >>> 'DB ' >>> Go to the first Data after 'DB '
    mov edi D$CodeListPtr
; StoreDatas StoreOneData
L0: .If al = 'B'

        call StoreDBcode

    .Else_If al = 'W'
        call StoreDWcode
    .Else_If al = 'U'
        call StoreDUcode
    .Else_If al = 'D'
        call StoreDDcode
    .Else_If al = 'Q'
        call StoreDQcode
    .Else_If al = 'F'
        error D$NotYetMnemoPtr
        ;call StoreDFcode
    .Else_If al = 'R'
        error D$NotYetMnemoPtr
        ;call StoreDRcode
    .Else_If al = 'T'
        error D$NotYetMnemoPtr
        ;call StoreDTcode
    .Else_If al = 'X'
        error D$NotYetMnemoPtr
        ;call StoreDXcode
    .Else
        error D$UnknownSizePtr
    .End_If

    dec esi
    If B$esi > EOI
        inc esi | jmp L0<<
    End_If

    mov D$CodeListPtr edi

  ; Arase the Source Line:
    mov ebx D$LineStart
    While ebx < esi
        mov B$ebx 0 | inc ebx
    End_While
    dec esi
ret


GetFlatInteger:
    If W$esi = '00'
        call TranslateBinary
    Else_If B$esi = '0'
        call TranslateHexa
    Else_If B$esi < '0'
        error D$UnknownDataPtr
    Else
        call TranslateDecimal
    End_If
ret


StoreDBcode:
L0: If B$esi = TextSign
      ; Cases of Text:
        inc esi | While B$esi <> TextSign | movsb | End_While | inc esi
        On B$esi > Space, error D$MissingSeparatorPtr
    Else
      ; Cases of Numbers:
        call GetFlatInteger | On eax > 0FF, error D$OverBytePtr
        stosb
    End_If

    On B$esi+2 = memMarker, ret

    If B$esi+3 = Space
        On B$esi+1 = 'D', ret
    End_If

    On B$esi-1 = Space, jmp L0<
ret


StoreDWcode:
L0: call GetFlatInteger | On eax > 0FFFF, error D$OverWordPtr
    stosw | cmp B$esi-1 Space | je L0<
ret


StoreDUcode:
    .If B$esi = TextSign
        inc esi
        While B$esi <> TextSign
            movsb | mov B$edi 0 | inc edi
        End_While
        inc esi
        On B$esi > Space, error D$MissingSeparator
        On B$esi+2 = memMarker, ret
        If B$esi+3 = Space
            On B$esi+1 = 'D', ret
        End_If
    .End_If

    call GetFlatInteger | On eax > 0FFFF, error D$OverWordPtr

    stosw | cmp B$esi-1 Space | je L0<
ret


StoreDDcode:
L0: call GetFlatInteger | On edx > 0, error D$OverDWordPtr
    stosd | cmp B$esi-1 Space | je L0<
ret


StoreDQcode:
L0: call GetFlatInteger
    stosd | mov D$edi edx | add edi 4 | cmp B$esi-1 Space | je L0<
ret


StoreDFcode:

ret


StoreDRcode:

ret


StoreDTcode:

ret


StoreDXcode:

ret

____________________________________________________________________________________________

[InstructionsCounter: ?]

EncodeLines:
    mov B$ErrorLevel 0, D$CodeLabelsCounter 0
ReCodeLine:
    mov esi D$CodeSourceB
    mov D$StatementsCounter 1, D$InstructionsCounter 0

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4
L0: lodsb | cmp al 0 | je L0<
    On B$esi <= EOI,  ret                           ; search for end mark '||'

    mov D$LineStart esi

    If al = EOI
        inc D$StatementsCounter | add D$StatementsPtr 4
        mov edi D$IpTablePtr, eax D$CodeListPtr
        sub eax D$CodeOrigine                       ; instruction pointers
        stosd | mov D$IpTablePtr edi                ; List for Debug
    End_If

    call ClearParameters | call SearchLineLabel
      cmp B$esi EOI | jbe L0<                       ; case of Label alone
        inc D$InstructionsCounter

   ; call SearchApiCall | cmp B$esi EOI | jbe L0<
   ; Now done by 'NewSearchApiName' (when computing 'Integers')

    call StoreSOP | call Prefix     ; Is There SOP (Segment Prefix Ovewrite) or prefixes?
                                    ; if yes, coding and striping done by called routines
    cmp B$esi EOI | jbe L0<         ; case of prefix alone


    .If B$esi+1 = memMarker
         mov al B$esi | add esi 2 | call StoreFlatData | jmp L0<<
    .Else_If B$esi+2 = Space
        If B$esi = 'D'
            mov al B$esi+1 | add esi 3 | call StoreFlatData | jmp L0<<
        End_If
    .End_If


L3: call ParametersCount            ; > cl = ParametersNumber

    mov B$LongRelative &TRUE        ; will be turn FALSE when needed for short dis.

    cmp cl 0 | je L3>> ; !!!!!!!!!!!!!!! >>
    If cl = 1
      call FirstParameterAnalyze
      mov al B$FirstOperandwbit | call StoreWbit
    Else_If cl = 2
      call FirstParameterAnalyze | call SecondParameterAnalyze
      call FixInstructionType
    Else_If cl = 3                                            ; (SHLD / SHRD > 3 parameters)
      call FirstParameterAnalyze | call SecondParameterAnalyze
      call ThirdParameterAnalyze | call FixInstructionType
    Else
      mov B$errorLevel 0 | error D$TooMuchPtr
    End_If

    On B$LockInstruction = &TRUE, call CheckLockMem

L3: call Encode | mov D$CodeListPtr edi

    On B$SIBinside = &TRUE,  call storeSIB
    cmp B$DisInside &TRUE | jne L4>
        call StoreDis | jmp L5>                    ; cases of label / displacement

L4: cmp B$DummyDis &TRUE | jne L5>

        mov eax 0 | mov edi D$CodeListPtr | stosd
        add D$CodeListPtr 4

L5: On B$immInside = &TRUE,  call storeImm

    On B$PossibleFirstImmLabel = &TRUE, call FirstParameterLabel
        On B$PossibleImmLabel = &TRUE, call SecondParameterLabel

    If B$mm3Dsuffix <> 0
        mov al B$mm3Dsuffix, B$mm3Dsuffix 0
        mov edi D$CodeListPtr | stosb
        inc D$CodeListPtr
    End_If

L6: call PointNextInstruction       ; Includes a developper Message in case of error
                                    ; cases / searches become a problem.
    jmp L0<<

____________________________________________________________________________________________
____________________________________________________________________________________________

; reading and writing chars.
____________________________________________________________________________________________

Store8cars:
    mov op5 B$esi+4, op6 B$esi+5, op7 B$esi+6, op8 B$esi+7
Store4cars:
    mov op1 B$esi, op2 B$esi+1, op3 B$esi+2, op4 B$esi+3
ret


[IfNot | cmp #1, #2 | jne #3]
 ________________________________________________________________________________________

; Called when cc conditions needed (Jcc, MOVcc, ...). esi set by caller:

[tttnBits: B$ ?]

SearchFortttnbits:
     On W$esi = 'NN', add esi 2      ; accept double negation ('jnna' = 'ja')
     call Store4Cars
     ifnot op2 Space, L2>>
       ifnot op1 'E', L1>
         mov B$tttnBits 00100 | ret        ; e
L1:    ifnot op1 'Z',  L1>
         mov B$tttnBits 00100 | ret        ; z
L1:    ifnot op1 'A', L1>
         mov B$tttnBits 00111 | ret        ; a
L1:    ifnot op1 'G', L1>
         mov B$tttnBits 001111 | ret       ; g
L1:    ifnot op1 'B', L1>
         mov B$tttnBits 0010 | ret         ; b
L1:    ifnot op1 'C', L1>
         mov B$tttnBits 0010 | ret         ; c
L1:    ifnot op1 'L', L1>
         mov B$tttnBits 001100 | ret       ; l
L1:    ifnot op1 'O', L1>
         mov B$tttnBits 0000 | ret         ; o
L1:    ifnot op1 'S', L1>
         mov B$tttnBits 001000 | ret       ; s
L1:    ifnot op1 'P', L1>
         mov B$tttnBits 001010 | ret       ; p
L1:    BadMnemonic

L2:  ifnot op3 Space, L3>>
       ifnot op1 'N', L1>
         ifnot op2 'E', L2>
           mov B$tttnBits 00101 | ret    ; ne
L2:      ifnot op2 'Z', L2>
           mov B$tttnBits 00101 | ret    ; nz
L2:      ifnot op2 'O', L2>
           mov B$tttnBits 0001 | ret     ; no
L2:      ifnot op2 'B', L2>
           mov B$tttnBits 0011 | ret     ; nb
L2:      ifnot op2 'C', L2>
           mov B$tttnBits 0011 | ret     ; nc
L2:      ifnot op2 'L', L2>
           mov B$tttnBits 001101 | ret   ; nl
L2:      ifnot op2 'A', L2>
           mov B$tttnBits 00110 | ret    ; na
L2:      ifnot op2 'G', L2>
           mov B$tttnBits 001110 | ret   ; ng
L2:      ifnot op2 'P', L2>
           mov B$tttnBits 001011 | ret   ; np
L2:      ifnot op2 'S', L2>
           mov B$tttnBits 001001 | ret   ; ns
L2:      BadMnemonic
L1:    ifnot op2 'E', L2>
         ifnot op1 'A', L1>
           mov B$tttnBits 0011 | ret     ; ae
L1:      ifnot op1 'B', L1>
           mov B$tttnBits 00110 | ret    ; be
L1:      ifnot op1 'P', L1>
           mov B$tttnBits 001010 | ret   ; pe
L1:      ifnot op1 'G', L1>
           mov B$tttnBits 001101 | ret   ; ge
L1:      ifnot op1 'L', L1>
           mov B$tttnBits 001110 | ret   ; le
L1:       BadMnemonic
L2:      ifnot op1 'P', L1>
           ifnot op2 'O', L2>
             mov B$tttnBits 001011 | ret  ; po
L2:       ; BadMnemonic
L1:      BadMnemonic

L3:   ifnot op1 'N', L9>
        ifnot op3 'E', L9>
          ifnot op4 Space, L9>
            ifnot op2 'G', L2>
              mov B$tttnBits 001100 | ret  ; nge
L2:         ifnot op2 'B', L2>
              mov B$tttnBits 00111 | ret   ; nbe
L2:         ifnot op2 'A', L2>
              mov B$tttnBits 0010 | ret    ; nae
L2:         ifnot op2 'L', L2>
              mov B$tttnBits 001111 | ret  ; nle
L2:
L9: BadMnemonic
 _______________________________________________________________________________________
;;
 search for prefix(es)
 Only one prefix of each group should be allowed > test addition needed
 (to do later if wanted)

 Segment Override Prefix  (SOP)
 When a xS: encounted, the code value is stored and xS: is stripped by ClearSOP
 esi > CodeSourceB  edi > CodeList by CodeListPtr
;;
 _______________________________________________________________________________________

WriteSOP:
    sub esi 3 | lodsb | mov ah al | lodsb | On al <> 'S', Error D$NotSegmentPtr

    ifnot op1 'C',  L1>
        mov al 02E | jmp L9>
L1: ifnot op1 'S', L1>
        mov al 036 | jmp L9>
L1: ifnot op1 'D', L1>
        mov al 03E | jmp L9>
L1: ifnot op1 'E', L1>
        mov al 026 | jmp L9>
L1: ifnot op1 'F', L1>
        mov al 064 | jmp L9>
L1: On op1 <> 'G', error D$NotSegmentPtr
        mov al 065
L9: mov edi D$CodeListPtr | stosb | inc D$CodeListPtr
ret
 __________________________________________________________________________________________
;;
 exemple of ClearSOP job: when called, esi point to  ':'

                  |MOV CS:D$EBX 1|
          new esi>>>>>^  ^<<<<<<< old esi > new edi


 after std, lodsb and stosb esi (new line start) point to the first letter of mnemonic:

                  |MOVMOV D$EBX 1|
                      ^<<<<< edi > last new esi  >>> last new LineStart
;;
 __________________________________________________________________________________________

ClearSOP:                   ; esi > ':' sign  >>>  '.xS:'  (this point is a space)
    push edi
        mov edi esi | sub esi 3
        std
L0:         lodsb | cmp al EOI | jbe L9>
                    cmp al colonSign | je L9> ; To hold Lines with 'head Labels'.
            stosb | jmp L0<
L9:     cld
        mov esi edi | inc esi       ; esi > new start of instruction
        mov D$LineStart esi
    pop edi
ret

StoreSOP:
    push esi
L0:     lodsb | cmp al EOI | jbe L9>
        cmp al ColonSign | jne L0<  ; no possible confusion:
            call WriteSOP           ; labels have been treated before
            call ClearSOP
        pop eax                     ; scratch old esi
        push esi                    ; (re)save new esi
L9: pop esi
ret

 _______________________________________________________________________________________
 _______________________________________________________________________________________

Prefix:
    mov edi D$CodeListPtr,  esi D$LineStart
L0: call Store8cars
    ifnot op1 'R', L6>
      ifnot op2 'E', L9>>
        ifnot op3 'P', L9>>
          cmp op4 Separators | ja L2>                                   ; REP
            mov B$edi 0F3 | add esi 4 | inc edi | jmp L0<
L2:       cmp op5 Separators | ja L4>
            cmp op4 'E' | je L3>
            ifnot op4 'Z', L9>
L3:           mov B$edi 0F3 | add esi 5 | inc edi | jmp L0<             ; REPE/REPZ
L4:       ifnot op4 'N', L9>
            cmp op6 Separators | ja L9>
              cmp op5 'E' | je L5>
              ifnot op5 'Z', L9>
L5:             mov B$edi 0F2 | add esi 6 | inc edi | jmp L0<           ; REPNE/REPNZ
L6: ifnot op1 'U', L6>
      ifnot op2 'T', L9>
        ifnot op3 'J', L9>
            mov B$esi+3 '_' | jmp L9>
L6: ifnot op1 'L', L9>
      ifnot op2 'T', L6>
        ifnot op3 'J', L9>
            mov B$esi+3 '_' | jmp L9>
L6:   ifnot op2 'O', L9>
        ifnot op3 'C', L9>
          ifnot op4 'K' L9>
            cmp op5 Separators | ja L9>
              call CheckLockInstruction
              mov B$LockInstruction &TRUE
              mov B$edi 0F0 | add esi, 5 | inc edi | jmp L0<<            ; LOCK
L9: On esi = D$LineStart,  ret                                 ; nothing done > exit
      On B$esi-1 <= EOI,  dec esi
      mov D$LineStart esi,  D$CodeListPtr edi
ret


LockInstructionError: mov B$LockInstruction &FALSE | error D$LockErrorPtr


CheckLockMem:
    mov B$LockInstruction &FALSE

    If B$FirstGender <> mem
        On B$SecondGender <> mem, error D$LockMemErrorPtr
    End_If
ret

CheckLockInstruction:
;;
  Edi Points to the next Member of the Instruction.
  
  The Instruction can only be ADD, ADC, AND, BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC,
  INC, NEG, NOT, OR, SBB, SUB, XADD, XCHG, or XOR
;;

  push esi
  add esi 5
  ; ADD, ADC, AND:
    ...If B$esi = 'A'
        .If B$esi+1 = 'D'
            If B$esi+2 = 'D'           ; ADD
                ;
            Else_If B$esi+2 = 'C'      ; ADC
                ;
            Else
                jmp LockInstructionError
            End_If

            On B$esi+3 <> Space, jmp LockInstructionError

        .Else_If B$esi+1 = 'N'
            If B$esi+2 = 'D'           ; AND
                On B$esi+3 <> Space, jmp LockInstructionError
            Else
                jmp LockInstructionError
            End_If

        .Else
            jmp LockInstructionError
        .End_If

  ; BTC, BTR, BTS:
    ...Else_If B$esi = 'B'
        .If B$esi+1 = 'T'
            If B$esi+2 = 'C'
                ;
            Else_If B$esi+2 = 'R'
                ;
            Else_If B$esi+2 = 'S'
                ;
            Else
                jmp LockInstructionError
            End_If

            On B$esi+3 <> Space, jmp LockInstructionError

        .Else
            jmp LockInstructionError

        .End_If

  ; DEC
    ...Else_If W$esi = 'DE'
        If B$esi+2 = 'C'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

  ; INC, NEG, NOT, OR, SBB
    ...Else_If W$esi = 'IN'
        If B$esi+2 = 'C'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If W$esi = 'NE'
        If B$esi+2 = 'G'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If W$esi = 'NO'
        If B$esi+2 = 'T'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If W$esi = 'OR'
        On B$esi+2 <> Space, jmp LockInstructionError ; jE!

    ...Else_If W$esi = 'SB'
        If B$esi+2 = 'B'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If
  ; SUB, XADD, XCHG, or XOR
    ...Else_If W$esi = 'SU'
        If B$esi+2 = 'B'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

    ...Else_If D$esi = 'XADD'
        On B$esi+4 <> Space, jmp LockInstructionError

    ...Else_If D$esi = 'XCHG'
        On B$esi+4 <> Space, jmp LockInstructionError

    ...Else_If W$esi = 'XO'
        If B$esi+2 = 'R'
            On B$esi+3 <> Space, jmp LockInstructionError
        Else
            jmp LockInstructionError
        End_If

  ; CMPXCHG, CMPXCHG8B:
    ...Else_If D$esi = 'CMPX'
        ..If W$esi+4 = 'CH'
            .If B$esi+6 = 'G'
                If B$esi+7 = Space
                    ;
                Else_If W$esi+7 = '8B'
                    On B$esi+9 <> Space, jmp LockInstructionError
                Else
                    jmp LockInstructionError
                End_If
            .Else
                jmp LockInstructionError
            .End_If
        ..Else
            jmp LockInstructionError
        ..End_If

    ...Else
        jmp LockInstructionError

    ...End_If

    pop esi
ret

 _________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 When all encoding job is done, we now fill label evocation in code with label adresses
 stored in LabelList. We creat a .reloc section if wanted (DLLs).
;;

[RelocPage: ?    RelocSectionSize: ?    FileAlignedRelocationSize: ?]
[StartOfCodeSection: 01000  StartOfDataSection: 02000] ; redefined any case after encoding

[LastUpSearchLabel: ?]

SearchLocalLabelUp:                     ; EDI >>> LabelList+3 / EDX >>> end of LabelList
L0: push esi, ecx, ebx                  ; this ESI points to CodeRef <<<<<< ESI >>>>>>
L1:   lodsb                             ; jmp over name in Coderef
      cmp al EOI | ja L1<
        lodsb                           ; strip one '|' ('L1<|..' turn previously 'L1||..')
        On B$LongRelative = &TRUE, lodsb ; idem for long > 'L1>>|'  > 'L1|||'
        lodsd                           ; local label evocation offset in EAX
        and eax 07FFFFFFF   ; 0111111111111111B strip relative flag from offset (if any)
        mov ecx eax
        lodsd | mov D$StatementsCounter eax
        mov esi edi         ; switch ESI to LabelList    <<<<<< ESI >>>>>>
        On D$LastUpSearchLabel > 0, mov esi D$LastUpSearchLabel
; ESI > start of first name in LabelList. We now search in LabelList a fitting adress
; by comparison with evocation offset stored in ECX: search for a neighbour label
; located after evocation and set EDI to previous one's end

L2:   mov ebx esi                       ; save start of LabelList name
L3:   lodsb                             ; jmp over name in LabelList
        cmp esi edx | jae L5>           ; reach end of LabelList? (EDX = Max)
        cmp al EOI | ja L3<
        lodsd                           ; read an offset in LabelList
        test B$esi CodeLabelFlag | je L4>
        cmp eax ecx | ja L5>            ; is it about the same offset than the Coderef one?
L4:     lodsb                           ; jump over flag byte
        lodsb                           ; jmp over '|'
      jmp L2<
                                        ;          |PREVIOUSNAME|Dword byte|NAME|
                                        ;                      ^ <<<<<<<<  ^
L5: sub ebx 8 | mov edi ebx             ; set EDI > end of previous LabelList name
    mov D$LastUpSearchLabel ebx         ; This Value saves a lot of Search tim!!!
    pop ebx, ecx, esi                   ; ESI back to CodeRef     <<<<<<< SI >>>>>>
    inc esi                             ; For 'L1' label, ESI was at 'L'. Now, ready for
                                                                  ; backward search
    std                                 ; ready to check backward

    align 32

L6: push esi ecx
      repe cmpsb                 ;  cmp ESI (CodeRef) to EDI (LabelList) until different
      je L9>                            ;  > found
      cmp B$edi+1 EOI | jbe L7>         ; case of LabelList name shorter than CodeRef's one
      mov ecx 0FF,  al EOI | repne scasb  ; longer: LabelList Di ptr > after next '|'
L7:   sub edi 6                           ;          |PREVIOUSNAME|Dword byte|NAME|
                                          ;                      ^ <<<<<<< ^
    pop ecx, esi
    On edi <= D$LabelList, error D$UnknownSymbolPtr esi      ; (we are searching upward)
    jmp L6<

L9: cld                                  ; ESI, EDI > start of identical found label name
    pop ecx esi                          ; ESI > end of name
    On B$LongRelative = &TRUE,  inc esi   ; strip additionnal '|' (old '<')
    add esi 3                            ; ESI > Dword offset of code evocation
    add edi 5                            ; EDI > Dword offset of code or data label
ret


[LastDownSearchLabel: ?]

SearchLocalLabelDown:
L0: push esi, ecx, ebx
L1:   lodsb                             ; jmp over name in Coderef
        cmp al EOI | jne L1<
        lodsb                           ; stip one '|' ('L1>|..' turn previously 'L1||..')
        On B$LongRelative = &TRUE,  lodsb
        lodsd                           ; local label evocation offset in eax
        test eax RelativeFlag | jz L2>
          and eax 00_01111111_11111111_11111111_11111111   ;07FFFFFFF   ; strip flag
L2:   mov ecx eax
      lodsd | mov D$StatementsCounter eax | mov esi edi
      On D$LastDownSearchLabel > 0, mov esi D$LastDownSearchLabel
L3:   mov ebx esi                       ; save start of LabelList name
L4:   lodsb                             ; jmp over name in LabelList
        cmp esi edx | jae L7>           ; reach end of LabelList? (EDX = Max)
        cmp al EOI | ja L4<
        lodsd
        test B$esi CodeLabelFlag | je L5>
          cmp eax ecx | jae L6>         ; is it about the same offset?
L5:     lodsb                           ; jump over flag byte
        lodsb                           ; jmp over '|'
      jmp L3<
L6: mov edi ebx                         ; restore start of LabelList name in DI
    mov D$LastDownSearchLabel ebx       ; This Value saves lot of search time!!!
    pop ebx ecx esi           ; edi > First label name's letter with possible adress
    call SearchRegularLabel
    inc esi                             ; strip '>' for short jump
    On B$LongRelative = &TRUE,  inc esi  ; strip next '>' if long jump
ret

L7: pop ebx ecx esi
    error D$UnknownSymbolPtr esi


; when called, ESI > start of a label name in CodeRef, ECX lenght of this name
; EDI set to >  LabelList + 5  by FillCodeSymbols (start of first label name in LabelList)

SearchRegularLabel:
    mov al EOI
L0: push esi ecx
        repe cmpsb              ;  cmp esi (CodeRef/DataRef) to edi (LabelList) until different
        je L8>                  ;  > found
            cmp B$edi-1 al | jbe L3>      ; case of LabelList name shorter than CodeRef's one
            mov ecx 0FF | repne scasb     ; longer: LabelList edi ptr > after next '|'
L3:         add edi 6                                 ; |LABELNAME|dword FlagByte|NEXTNAME
                                                    ;        EDI ^ >>>>>>>>     ^
            If edi >= edx                       ; edx = LabelList Max.
                pop ecx, esi
                error D$UnknownSymbolPtr esi
            end_if
    pop ecx, esi | jmp L0<

L8: cmp B$Relative &TRUE | je L9>           ; if comming from 'SearchLocalLabeldown', OK
      test D$esi RelativeFlag | jz L9>           ; if not, possible relative:
        mov D$Relative &TRUE,  B$LongRelative &TRUE     ; this is for 'JMP label'
L9: pop eax eax             ; if here, found: dummy stack restore. edi > code adress dWord
    ret


SearchSortedRegularLabel:
;;
    dec ecx
    If D$LabelsPointersBySizes+ecx*4 <> 0
        mov edi D$LabelsPointersBySizes+ecx*4
    Else
        error UnknownSymbol esi
    End_If
    inc ecx | mov al EOI
    

L0: push esi ecx
        repe cmpsb              ;  cmp esi (CodeRef/DataRef) to edi (LabelList) until different
        je L8>                  ;  > found
            cmp B$edi-1 al | jbe L3>      ; case of LabelList name shorter than CodeRef's one
            mov ecx 0FF | repne scasb     ; longer: LabelList edi ptr > after next '|'
L3:         add edi 6                             ; |LABELNAME|dword FlagByte|NEXTNAME
                                                  ;        EDI ^ >>>>>>>>     ^
            If edi >= edx                       ; edx = LabelList Max.
                pop ecx, esi
                error UnknownSymbol esi
            End_if
    pop ecx, esi | jmp L0<
;;

L8: call GetFromQwordCheckSum esi, D$LabelList, D$LabelListLimit

    If eax = 0
        push D$esi
            pushad
                call IsItAreg | On eax <> 0, error D$UnexpectedRegPtr, esi
            popad
        pop D$esi
        error D$UnknownSymbolPtr, esi
    Else
        While B$eax > LowSigns | inc eax | inc esi | End_While | inc eax | inc esi
        mov edi eax, ecx 0
    End_If

L8: cmp B$Relative &TRUE | je L9>           ; if comming from 'SearchLocalLabeldown', OK
        test D$esi RelativeFlag | jz L9>           ; if not, possible relative:
            mov D$Relative &TRUE,  B$LongRelative &TRUE     ; this is for 'JMP label'

L9: ret


; construction of relocation table. called by FillDataSymbols routine when an uncomputed
; adress is found. EDI > code offset of an evocation.

[CodeOrData: ?]

StoreApiReloc: ; NewSearchApiName PreparePeHeader
  ; eat 0FF found at esi, by 'FillCodeSymbols'
    inc esi | mov edi D$esi | add esi 5
;jE!
StoreReloc: ; 'RelocComments'
    push eax, ebx, ecx, edi
L0:
      mov eax edi
      sub eax D$CodeOrData | and eax PageMask ;| add eax PageSize
     ; cmp eax D$RelocPage | je L3>
      cmp eax 01000 | jb L3>>
     ; if in the same page of code, just write (>L3). if not:
     ; writing of reloc page header. Each section begins by a header of two Dwords.
     ; first one is the virtual adress; exemple, 01000 if begins at 00401000.
     ; second one is the total number of octets of the section.
      add D$CodeOrData 01000

If D$RelocSectionSize = 8 ; if unused RelocSection, reuse it for Next
 add D$RelocPage PageSize | mov eax D$RelocPage |
 mov ecx D$RelocationPtr | mov D$ecx-8 eax | jmp L0<
End_IF
      push eax, edi
        mov eax 0 | mov edi D$RelocationPtr
L1:     test D$RelocationPtr 0011 | jz L2>                   ; align on Dword boudary
          stosw                                              ; fill with zeros
          add D$RelocationPtr 2 | add D$RelocSectionSize 2

L2:     mov ebx D$RelocationPtr | sub ebx D$RelocSectionSize | add ebx 4
        mov eax D$RelocSectionSize | mov D$ebx eax     ; set second dWord to section size

        add D$RelocPage PageSize | mov eax D$RelocPage | stosd
        add edi 4 | mov D$RelocationPtr edi | mov D$RelocSectionSize 8
      pop edi, eax
jmp L0<< ; don't continue, becouse can be over NEXT-PAGE

L3:   mov eax edi | sub eax D$CodeOrData | and eax 0FFF| add eax 03000  ; or! eax 03000, RelocTypeFlag
      mov edi D$RelocationPtr | stosw

      add D$RelocSectionSize 2 | mov D$RelocationPtr edi
    pop edi, ecx, ebx, eax
ret
 ________________________________________________________________________________________
;;
 Filling code label evocations (set to zeros at coding time, fill when coding is over)

 > esi points to label name in CodeRef (fix)
 > edi points to label name in LabelList (moveable)
 ecx = lenght of researched name (count from CodeRef)

 True labels adresses writing in generated code. See full comments down there
 about the tables used in this deal
 esi > CodeRef  ebx = CodeRef End     edi > LabelList  edx = LabelList End
 The job is difficult enough: it may be about:
 uncomputed code or data references,
 computed (+/-) jumps or calls either short (one byte) or long (four bytes)
 Values are turned RVA with 'CodeAjust' an 'DataAjust' set by 'InitRelocation'
;;
 ________________________________________________________________________________________

[BadShortDisplacement: 0 #4]

ErrorShortDisUp:
    neg eax | sub eax 127
    If eax > 1
        mov ebx {'Bytes Up', 0}
    Else
        mov ebx {'Byte Up', 0}
    End_If

    jmp L1>

ErrorShortDisDown:
    sub eax 080
    If eax > 1
        mov ebx {'Bytes Down', 0}
    Else
        mov ebx {'Byte Down', 0}
    End_If

L1: ;error D$ShortDisPtr

    CustomError D$ShortDisPtr, '#1', 'Int', eax,
                               '#2', 'Str', ebx

    mov B$ErrorLevel 2 | error TrashString

    ;mov ebx eax | mov B$ErrorLevel 12 | error D$ShortDisPtr

[ApplyShortenJump: ?]

FillCodeSymbols:
    mov eax D$CodeRef | add eax 5
    On D$CodeRefPtr = eax,  ret                 ; if no symbol at all (???!!!)
    mov edx D$LabelList | mov esi edx | lodsd   ; len of LabelList (-1) > EAX > EDX
    add edx eax | dec edx                       ; last octet of LabelList table (edx=edi max)
    mov ebx D$CodeRef | mov esi ebx | lodsd     ; len of CodeRef (-1) > EAX > EBX
    add ebx eax | dec ebx                       ; last octet of CodeRef table (ebx=esi max)
    mov esi D$CodeRef | add esi 5               ; (+5) > jmp over len and '|'

L0: mov edi D$LabelList | add edi 5

    mov B$ApplyShortenJump &FALSE
;;
 For Api calls relocations, the 'EncodeLines' does it directely (> 'NewSearchApiName'): 
 The 'CodeRef' Record for such cases is simply:
 |0FF dWordCodeAddress|. This is done by 'NewSearchApiName' only in case of:
 > If D$SavingExtension = '.DLL'
 Again, The record is irregular: 0FF, 1 dWord, EOI, as there is nothing else to be done
 with this, out of the DLL relocations building:
;;
    If B$esi = 0FF
        call StoreApiReloc | cmp esi ebx | jb L0<
        ret
    End_If

    push esi
      mov ecx 0
L1:   lodsb
        inc ecx                                 ; simple lenght counter of Code symbolics'
        cmp al EOI | ja L1<                     ; lenghts, in CodeRef, including separator
        lodsd | lodsd | mov D$StatementsCounter eax
    pop esi                       ; when encoding 'Relative' is either 0 or high bit flag
    mov D$Relative &FALSE                        ; now on, either true or false
      mov ah B$esi,  al B$esi+1
      cmp ah 'A' | jb S9>>
        cmp ah 'Z' | ja S9>>
          cmp al '0' | jb S9>>
            cmp al '9' | ja S9>>                 ; is it a local label evocation?

            .If B$esi+2 = '>'
                mov D$Relative &TRUE
                If B$esi+3 = '>'
                    mov B$LongRelative &TRUE
                Else_If B$esi+3 = '.'
                    mov B$LongRelative &TRUE | mov B$esi+3 '>'
                    mov B$ApplyShortenJump &TRUE
                Else_If B$esi+3 < Separators
                    mov B$LongRelative &FALSE
                Else
                    Error D$WhatIsThisPtr
                End_If

                mov B$esi+2 EOI | mov ecx 3 | call SearchLocalLabelDown

            .Else_If B$esi+2 = '<'
                mov D$Relative &TRUE
                If B$esi+3 = '<'
                    mov B$LongRelative &TRUE
                Else_If B$esi+3 = '.'
                    mov B$LongRelative &TRUE | mov B$esi+3 '<'
                    mov B$ApplyShortenJump &TRUE
                Else_If B$esi+3 < Separators
                    mov B$LongRelative &FALSE
                Else
                    Error D$WhatIsThisPtr
                End_If

                mov B$esi+2 EOI | mov ecx 3 | call SearchLocalLabelUp

            .Else
S9:             push edx
                    call SearchSortedRegularLabel
                pop edx

            .End_If
;;
  got here after label search up, down or regular (and found)
  edi points to a LabelList'symbol adress (code or data true adress of label)
  esi points to a Coderef evocation adress (code ... adress needing 'fill job')
  'add esi 9', 4 lines down here set this ptr on Coderef next label name (2 dWords + EOI)
;;
    mov eax D$edi                              ; true label adress in eax (EDI > LabelList)

    On B$ApplyShortenJump = &TRUE, mov B$LongRelative &FALSE

  ; write 'done' on LabelList flag byte:
    mov cl B$edi+4 | or B$edi+4 DoneFlag
    mov edi D$esi | add esi 9
  ; ESI > CodeRef offset of evocation > EDI

    ...If D$Relative = &TRUE
      ; no flag > uncomputed label adress: Strip flag (computed signed displacement);
        and edi 07FFFFFFF | sub eax edi
      ; no signed displacement?
        .If B$LongRelative = &FALSE
            test eax dWordHighBit | jz L6>
          ; -127 (0FFFF_FF80 = -128) ; short negative value:
          ; -128 (-1) (limit for signed negative byte)
            On eax < 0FFFF_FF81,  jmp ErrorShortDisUp
                jmp L7>

L6:         On eax > 080, jmp ErrorShortDisDown    ; short positive value > 07F + 1
                                                    ; +127 (limit for signed positive byte)
L7:         sub eax 1 | add B$edi al | cmp esi ebx | jb L0<<     ; store on one byte
            ret

        .Else
            On B$ProfilerFlag = &TRUE, call TimingCalls

L8:         sub eax 4 | add D$edi eax | cmp esi ebx | jb L0<<       ; store on 4 bytes
            ret      ; why ADD and not MOV: exemple: 'ADC ebx MyLabel+2' > '2' previously stored
                ; 'sub ax, 4'  is to jump over storage bytes (L7: idem for one byte storage)
        .End_If
    ...End_If

L9: If D$RelocsWanted = &TRUE ;SavingExtension = '.DLL' ; jE!
        call StoreReloc
    Else_If D$SavingExtension = '.SYS'
        call StoreReloc
    End_If

    If cl < CodeLabelFlag
        add eax D$DataAjust         ; 2 >>> data / 3 >>> data+done
    Else
        add eax D$CodeAjust         ; 4 >>> code / 5 >>> code+done
    End_If

    add D$edi eax                                   ; store on 4 bytes (uncomputed)
    cmp esi ebx | jb L0<<
ret

 ________________________________________________________________________________________
;
; Filling empty rooms left in Data section (same comments as upper ones)
 ________________________________________________________________________________________

FillDataSymbols:
    mov eax D$DataRef | add eax 5
    On D$DataRefPtr = eax,  ret                 ; if no symbol at all
   ; mov edx D$PlainLabelList | mov esi edx | lodsd   ; len of LabelList (-1) > EAX > EDX

   ; add edx eax | dec edx                       ; last octet of LabelList table (edx=edi max)
    mov ebx D$DataRef | mov esi ebx | lodsd     ; len of DataRef (-1) > EAX > EBX
    add ebx eax | dec ebx                       ; last octet of DataRef table (ebx=esi max)
    mov esi D$DataRef | add esi 5               ; (+5) > jmp over len and '|'

L0: ;mov edi D$PlainLabelList | add edi 5
    push esi
        mov ecx 0
L1:     lodsb
            inc ecx                                 ; simple lenght counter of Code symbolics'
            cmp al EOI | ja L1<                     ; lenghts, in DataRef, including separator
            lodsd | mov D$DataFillPtr eax           ; doublon de DataListPtr
            lodsd | mov D$bracketCounter eax
            lodsb | mov B$DataSign al
            lodsb | mov D$DataRefPtr esi            ; strip lasting EOI
    pop esi

    call SearchSortedRegularLabel | mov eax D$edi | mov cl B$edi+4

    cmp cl CodeLabelFlag | jb D1>               ; 'jb' because:
        add eax D$CodeAjust | jmp D2>                 ; 2 > data / 3 > data+done
D1:     add eax D$DataAjust                           ; 4 > code / 5 > code+done
D2:     or B$edi+4 DoneFlag

D2: On B$DataSign = &TRUE, neg eax | mov edi D$DataFillPtr | add D$edi eax

    If D$RelocsWanted = &TRUE ;SavingExtension = '.DLL' ;jE!
        call StoreReloc
    Else_If D$SavingExtension = '.SYS'
        call StoreReloc
    End_If

    mov esi D$DataRefPtr | cmp esi ebx | jb L0<<
  ret


[DataFillPtr: ?  DataAjust: ?  CodeAjust: ?]

Proc SetCodeAdjust:
    Argument @Base

        mov eax D@Base | add eax D$uBaseOfData | sub eax D$Datalist
        mov D$DataAjust eax
        mov eax D@Base | add eax D$uBaseOfCode | sub eax D$Codelist
        sub eax D$uStartOfCode | mov D$CodeAjust eax
EndP


InitRelocationForData:
    push edi eax

        If D$SavingExtension = '.SYS'
            call SetCodeAdjust DRIVERDEFAULT
        Else_If D$SavingExtension = '.DLL'
            call SetCodeAdjust D$LinkerDllDefault
        Else
            call SetCodeAdjust LINKERDEFAULT
        End_If

        mov edi D$RelocationPtr

        mov eax D$uBaseOfImport | add eax D$ImportTrueSize | Align_On 01000 eax

        If B$NoResources = &FALSE
            add eax D$uRsrcSize | Align_On 01000 eax
        End_If

        mov D$RelocPage eax | stosd

        mov eax 0 | stosd
        mov D$RelocationPtr edi | mov D$RelocSectionSize 8

    pop eax edi
ret


InitRelocationForCode:
    push edi eax
        mov edi D$RelocationPtr

        mov eax D$uBaseOfImport | add eax D$ImportTrueSize | Align_On 01000 eax

        If B$NoResources = &FALSE
            add eax D$uRsrcSize | Align_On 01000 eax
        End_If

        ;add eax D$uDataSize | Align_On 01000 eax
        mov eax D$uBaseOfCode
        mov D$RelocPage eax | stosd

        mov eax 0 | stosd
        mov D$RelocationPtr edi | mov D$RelocSectionSize 8
    pop eax edi
ret


CloseRelocation:
    mov eax 0 | mov edi D$RelocationPtr

L1: test D$RelocationPtr 0011 | jz L2>              ; align on Dword boudary
        stosb                                       ; fill with zeros
        inc D$RelocationPtr | inc D$RelocSectionSize
        jmp L1<

L2: mov ebx D$RelocationPtr | sub ebx D$RelocSectionSize | add ebx, 4
    mov eax D$RelocSectionSize | mov D$ebx eax      ; set second dword to section size
ret

RelocComments:
;;
 Relocation Section looks like this, for example:

 D$ 02000  ; RVA (Liker Default is to be added to this by the Loader).
 D$ 034    ; Aligned number of Bytes in the coming Chunk (including upper 8 Bytes+Alignement)
 W$ 03004 03010 03058 ....
 ; the chunk is aligned on dWord with zero padding if needed.
 D$ 03000  ; Next chunk header
 ....

 Even the last Chunk must be aligned and the number of bytes in this last Chunk Header
 holds it too.
;;

[BigFirstTick: ?  BickTickCount: ?]

BuildRelocationAndFillSymbols:                      ; >>> StoreReloc <<<
    call InitRelocationForData
        mov B$ErrorLevel 3
        move D$CodeOrData D$DataList
        call BuildPlainLabelList

      ; Left for viewing the List of Labels, the Debugger, ...
       call SortPlainLabelList

       call FillDataSymbols

        call CloseRelocation
        If D$RelocSectionSize = 8                   ; Case of no relocation
            sub D$RelocationPtr 8                   ; wanted in Data
        End_If

    call InitRelocationForCode
        mov B$ErrorLevel 2
        mov D$LastDownSearchLabel 0, D$LastUpSearchLabel 0
        move D$CodeOrData D$DataList
        mov eax D$uDataSize | Align_On 0200 eax | add D$CodeOrData eax

        call FillCodeSymbols

        call CloseRelocation
        If D$RelocSectionSize = 8                   ; Case of no relocation
            sub D$RelocationPtr 8                   ; wanted in CODE jE!
        mov eax D$RelocationPtr | and D$eax 0 | and D$eax+4 0 ;kill unused
        End_If

        mov eax D$RelocationPtr | sub eax D$Relocation
        ; Case of no relocation at ALL, make MINIMUM-RELOC
        If eax = 0
           mov eax D$Relocation | mov D$eax 0 | mov D$eax+4 08
           add D$RelocationPtr 8
        End_If
  ; Reuse 'RelocSectionSize' to hold what it says, as a whole for headers values:
    mov eax D$RelocationPtr | sub eax D$Relocation | mov D$RelocSectionSize eax
    Align_On 0200 eax | mov D$FileAlignedRelocationSize eax
ret


SortPlainLabelList:
    mov eax D$PlainLabelList, eax D$eax

    push eax
        add eax 4
        VirtualAlloc SortTempoMemory eax
    pop ecx
    push ecx
        mov edi D$SortTempoMemory, esi D$PlainLabelList
        shr ecx 2 | inc ecx | rep movsd

        move D$SortSource D$SortTempoMemory | add D$SortSource 5
        move D$SortDestination D$PlainLabelList | add D$SortDestination 5

        mov D$SortStringNumber 0 | mov esi D$SortDestination

L0:     lodsb | cmp al EOI | jne L0<
        add esi 6
        inc D$SortStringNumber
        cmp esi D$EndOfPlainLabelList | jb L0<

    pop ecx

    call SortLabelStringsBySize

    VirtualFree D$SortTempoMemory
L9: ret


SortLabelStringsBySize:
    mov edi LabelsPointersBySizes, eax 0, ecx 100 | rep stosd

    mov edi D$SortDestination, edx 1

L0: mov esi D$SortSource, ecx D$SortStringNumber, B$SortBySizeOver &TRUE

; There is something wrong in this: If i state 'While B$esi <> EOI' instead of
; 'While B$esi > EOI', it doesn't work. It should, and there is no reason for what
; D$esi could be = 0 (this is to say after the end of the Table -i suppose...)...

L1:     lodsb
        .If al = 0FF        ; Done
            While B$esi > EOI | inc esi | End_While
            add esi 7

        .Else
            push ecx
                mov eax esi, ecx 1
                While B$eax > EOI | inc ecx | inc eax | End_While
                If ecx = edx
                    mov al B$esi-1, B$esi-1 0FF
                    On D$LabelsPointersBySizes+edx*4 = 0,
                        mov D$LabelsPointersBySizes+edx*4 edi
                    stosb
                    dec ecx | jecxz L2>
                        rep movsb
L2:                 movsb  ; |
                    movsd  ; Ptr
                    movsb  ; Flag
                    movsb  ; |

                Else
                    lea esi D$eax+7
                End_If

            pop ecx
            mov B$SortBySizeOver &FALSE
        .End_If
    loop L1<

    inc edx | cmp B$SortBySizeOver &FALSE | je L0<<
ret

;;
 When searching Labels for filling Data Sybols, we do not need Meaningless Local
 Labels. So, we can build a List without them to make FillDataSymbols faster (No
 use sorting the list, as all Label are supposed to be found. If not error later):
 The 'PlainLabelList' Table is: ...|Name|Ptr Flag|Name....
                                        ...... .  ..
                                        1 4   1  1
;;

[PlainLabelList: ?    EndOfPlainLabelList: ?]

BuildPlainLabelList:
    mov eax D$LabelList, eax D$eax | inc eax

    VirtualAlloc PlainLabelList eax

    mov edx D$LabelList | add edx D$edx
    mov edi D$PlainLabelList | add edi 5
    mov esi D$LabelList | add esi 5

    .While esi < edx
        cmp B$esi+2 EOI | jne L1>
            cmp B$esi 'A' | jb L1>
            cmp B$esi 'Z' | ja L1>
                cmp B$esi+1 '0' | jb L1>
                cmp B$esi+1 '9' | ja L1>
                    ; |L0|dWord Byte| >>> 9
                    add esi 9 | jmp L2>

L1:     While B$esi <> EOI
            movsb
        End_While
        movsb   ; |
        movsd   ; Ptr
        movsb   ; Flag
        movsb   ; |
L2: .End_While

    mov eax edi | mov D$EndOfPlainLabelList eax | sub eax D$PlainLabelList
    mov edi D$PlainLabelList | stosd | mov al EOI | stosb
ret

 _______________________________________________________________________________________


FixTableSizes:
    mov eax D$CodeListPtr | Align_on 0200 eax
    mov edi D$CodeList | sub eax edi | mov D$LenOfCode eax

    mov eax D$CodeRefPtr   | mov edi D$CodeRef  | sub eax edi  | stosd

    mov eax D$DataRefPtr   | mov edi D$DataRef  | sub eax edi  | stosd

   ; mov eax D$DataListPtr  | mov edi D$DataList  | sub eax edi | stosd ; no need ???...
    mov eax D$LabelListPtr | mov edi D$LabelList | sub eax edi | stosd

    mov eax D$StartOfCodeSection | add eax D$LenOfCode
    and eax 0FFFFFF00
    add eax 01000 | add eax 0400000
    mov D$StartOfDataSection eax
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  The Symbolic Names are encoded into a qWord CheckSum. Then, this qWord is encoded
  into one another Word CheckSum. Let's call them 'CheckSum64' and 'CheckSum16'.
  __________________
  A two Stages Table is declared to hold 010000h (for storing the distributions
  of the Records) plus 010000h Records (for storing the Linked Records in case
  of identical CheckSum16).
  
  The CheckSum16 is used as a Index to point to the Records of the first stage
  of the Table.
  
  _______________
  Each Record is: [CheckSum64, Pointer, LinkedPointer]

  * 'Pointer' points to a Name, in one of the Assembler internal Lists. It is
    nothing but the Pointer transmitted to the 'SetQwordCheckSum' Procedure,
    when called. We do not need to do do any Name String Copy, as it is already
    there, in the concerned List ('LabelList', 'MacroList', 'EquateList').
    
  * If the second half of the Records Table is empty, 'LinkedPointer' points to
    the top of this Second half Table. Otherwise, in case several 'CheckSum64'
    achieve into the same 'CheckSum16', 'LinkedPointer' points to the next same
    'CheckSum16' Record, in the second half of the Table... and so on...
    
  * The first half of the Table is filled 'randomaly-like' (depending on the
    CheckSum16 value. The second half of the Table is filled in order (Top-Down),
    each time a Record of the First Half Table is found to not be empty.
    
  _________
  Routines:
  
  * 'ClearQwordCheckSum' zeroes the Records Table and sets
    'PointerToCheckSumsLinkedRecords' to the Top of the second half Table.
  
  * 'SetQwordCheckSum' is called with a Parameter pointing to the first Char
    of a Name - in its family List - to be recorded. It also call for:
    
  * 'NoDuplication' to make sure of unique Symbolics Declarations.
  
  * 'GetFromQwordCheckSum' is called with a Parameter pointing to a Name to
     be checked. If found, the Procedure returns the Pointers that was used
     when calling 'SetQwordCheckSum', that are nothing but Pointers to the
     Lists ('MacroList', 'EquateList', 'LabelList'), that hold all infos the
     Assembler needs for doing its job. If not found, it returns zero to the
     caller.
     
  * 'TestRepartition', is just a Dev-test for viewing how all of this goes.
  
  ______________________________
  How the Records Table is used:
  
  When calling 'SetQwordCheckSum', the CheckSum64 is computed from the given
  Name. Then, the CheckSum16 is computed from the given CheckSum64.
  
  CheckSum16 is used as an Indice to the first half of the Records Table.
  For example, if CheckSum16 is 25, we point to the 25th Record, that is
  (CheckSumsRecords + (25*(8+4+4))).
  
  If this record is found empty, it is written: CheckSum64 / Pointer / Link
  
  As long as there is nothing in the second half of the Table, 'Link' points
  to the empty Record at (CheckSumsRecords + (010000h*16) ).
  
  If the first half Table is not found empty, the 'Link' Pointer is read,
  and we jump there,... and so on...
  
  The chances for having two different Symbols achieving into the same CheckSum64
  are, of course, of 1 on 01_0000_0000_0000_0000h, for the second Name, of 1 on
  01_0000_0000_0000_0000h, for the third name, and so on... So, in the very unlikely
  coming out cases when two different Names are computed into the same CheckSum64,
  the new record is linked downward, the same way the Duplications of the CheckSum16
  are Linked.
  
  The reverse case is also theorically possible: For example, you do _not_ implement,
  say, some 'GetPointer' Macro, and you use it in a Statement. It is theorically
  not impossible that some other _declared_ Macro achieves into the same CheckSum64
  as would your non existing 'GetPointer'. In such -very unlikely coming out cases-,
  the 'GetFromQwordCheckSum' would return a pointer, instead of zero. So, when a
  valid CheckSum64 is found, 'GetFromQwordCheckSum' also calls for 'CompareSymbols',
  to make it 100% secure.
;;
____________________________________________________________________________________________

; CheckSum64 / Pointer / Link

[CheckSumsRecords: ? ? ? ? #010000    CheckSumsLinkedRecords: ? ? ? ? #020000
 PointerToCheckSumsLinkedRecords: ?   CheckSumsEnd: ]

ClearQwordCheckSum:
    mov edi CheckSumsRecords, eax 0
    mov ecx CheckSumsEnd | sub ecx CheckSumsRecords | shr ecx 2
    rep stosd

    mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords
ret

[CheckSumImage: ?]

SaveCheckSumTable:
    pushad
        mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords

        push ecx
            VirtualAlloc CheckSumImage, ecx
        pop ecx

        shr ecx 2

        mov esi CheckSumsRecords, edi D$CheckSumImage | rep movsd
    popad
ret

RestoreCheckSumTable:
    pushad
        mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords | shr ecx 2

        mov esi D$CheckSumImage, edi CheckSumsRecords | rep movsd

        VirtualFree D$CheckSumImage
    popad
ret

____________________________________________________________________________________________

CheckSum64:
  ; esi -> Name
    mov eax 0, ebx 0, ecx 0

    While B$esi > ' ' ;LowSigns
        rol eax 1 | lodsb | mul eax | xor ebx edx | inc ecx
    End_While
    add ebx ecx
    If eax = 0
        On ebx = 0, mov eax 1
    End_If
  ; ebx:eax = CheckSum64 // ecx = Length
ret


CheckSum16:
  ; ebx:eax = CheckSum64 (not modified here))
    mov ecx eax | xor ecx ebx | mov edx ecx
    rol edx 16 | xor cx dx
    and ecx 0FFFF | shl ecx 4
  ; ecx = CheckSum16, to be used as a Displacement to the matching Record
  ; (To 'CheckSumsRecords' first half part, 16 Bytes per Record)
ret
____________________________________________________________________________________________

Proc SetQwordCheckSum:
    Argument @Pointer

    pushad

        mov esi D@Pointer

        If B$esi < '0'
            ;
        Else_If B$esi <= '9'
            error D$NumerAsSymbolPtr, D@Pointer
        End_If

        call CheckSum64 | call NoDuplication D@Pointer | call CheckSum16

      ; The List Pointer is used to test empty Records (Lists Pointers can never be zero):
        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            mov D$CheckSumsRecords+ecx eax
            mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 D@Pointer
          ; D$CheckSumsRecords+ecx+12 = 0
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0 | mov edi D$edi+12 | End_While
                move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            mov edi D$PointerToCheckSumsLinkedRecords
            mov D$edi eax
            mov D$edi+4 ebx
            move D$edi+8 D@Pointer
            ;mov eax D$PointerToCheckSumsLinkedRecords | add eax 16
          ; D$edi+12 = 0
            ;mov D$PointerToCheckSumsLinkedRecords eax
            add D$PointerToCheckSumsLinkedRecords 16
        .End_If

    popad
EndP
____________________________________________________________________________________________

Proc GetFromQwordCheckSum:
    Argument @Pointer, @List, @Limit
    Uses esi, edi, ebx, ecx, edx

        mov esi D@Pointer

        call CheckSum64 | call CheckSum16

        lea esi D$CheckSumsRecords+ecx | mov ecx &TRUE

L0:     ..If D$esi = eax
            .If D$esi+4 = ebx
                mov eax D$esi+8
                mov ebx D@List | On eax < ebx, mov ecx &FALSE
                mov ebx D@Limit | On eax > ebx, mov ecx &FALSE

                On ecx = &TRUE, call CompareSymbols D@Pointer, eax

                If ecx = &FALSE
                    push esi
                        mov esi D@Pointer | call CheckSum64
                    pop esi

                    mov ecx &TRUE | jmp L2>
                End_If

            .Else
                jmp L2>

            .End_If

        ..Else
L2:
            ;inc D$EquatesPasses
            mov esi D$esi+12 | cmp esi 0 | je L3>

          ; If no List Pointer, this is the first empty Record in the Linked Record Table:
            mov ecx &TRUE | cmp D$esi+8 0 | ja L0<<

L3:         mov eax 0

        ..End_If
   ; hexprint D$EquatesPasses
EndP
____________________________________________________________________________________________



Proc NoDuplication:
    Argument @Pointer
    Uses eax, ebx, ecx

      ; ebx:eax = CheckSum64
        mov ecx eax | xor ecx ebx | mov edx ecx
        rol edx 16 | xor cx dx
        and ecx 0FFFF | shl ecx 4

        lea esi D$CheckSumsRecords+ecx

L0:     While D$esi+4 <> 0
            If D$esi = eax
                On D$esi+4 = ebx, call CompareSymbols D@Pointer, D$esi+8
                On ecx = &TRUE, error D$SymbolDupPtr
            End_If

            mov esi D$esi+12 | On esi = 0, ExitP

        End_While
EndP
____________________________________________________________________________________________

Proc CompareSymbols:
    Argument @Source, @Destination
    Uses eax, esi, edi

        mov esi D@Source, edi D@Destination

L0:     lodsb | cmp al LowSigns | jb L5>
        inc edi | cmp al B$edi-1 | je L0<

        mov ecx &FALSE | ExitP

L5:     If B$edi < LowSigns
            mov ecx &TRUE
        Else
            mov ecx &FALSE
        End_If
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  This test is for viewing the Records distribution, in the CheckSums Table.
  
  The first Pixels square shows the occupied Records in the first half Part of
  the Table (black Pixels). The occupied Records, in the second half of the
  Table are represented by red Pixels. They, of course, come in the form of
  a red _line_.
  
  The actual distribution seems to be pretty close to a good random one.
;;

[CheckSumsPixelsCount: ?     CheckSumsLinkedRecordsPixels: ?]
[SecondTable: ?]

TestRepartition:
    mov D$CheckSumsPixelsCount 0, D$CheckSumsLinkedRecordsPixels 0

    call 'User32.BeginPaint' D$EditWindowHandle, PAINTSTRUCT | mov D$hdc eax
    call 'User32.GetClientRect' D$EditWindowHandle, RECT

    mov esi CheckSumsRecords

L0: mov eax D$esi | or eax D$esi+4
    If eax <> 0
        mov eax esi | sub eax CheckSumsRecords | shr eax 4
        mov ebx eax | shr eax 8 | and ebx 0FF
        inc D$CheckSumsPixelsCount
        call 'GDI32.SetPixel' D$hdc, eax, ebx, 0
    End_If

    add esi 16 | cmp esi CheckSumsLinkedRecords | jb L0<

L0: mov eax D$esi | or eax D$esi+4
    If eax <> 0
        mov eax esi | sub eax CheckSumsLinkedRecords | shr eax 4
        mov ebx eax | shr eax 8 | and ebx 0FF | add eax 0100
        inc D$CheckSumsPixelsCount
        inc D$CheckSumsLinkedRecordsPixels
        call 'GDI32.SetPixel' D$hdc, ebx, eax, 0FF
    End_If

    add esi 16 | cmp esi PointerToCheckSumsLinkedRecords | jb L0<

    call 'USER32.ReleaseDC' D$EditWindowHandle, D$hdc
    call 'USER32.EndPaint' D$EditWindowHandle, PAINTSTRUCT

    If D$CheckSumsPixelsCount > 0
       call WaitForUserAction

      ; Comment out to have the total number of Pixel and how many Linked Records:
      ; (On RosAsm, for V.1.24e: 64,536 // 313)

      ; Hexprint D$CheckSumsPixelsCount
      ; hexprint D$CheckSumsLinkedRecordsPixels

;;
  __________________________
  Results on RosAsm V.1.25d:
  
  Total = 019D9 = 6617
  Links =  0139 =  313

  Total of available Records in the Table first half = 010000 = 65536

  6617 / 65536 = 10.09 % of the Table is occupied by Records(+Linked Records)

  313 / 6617 = 4.73 % of the Records require a linkage
  
  In short: 5% of the Records are Linked when 10% of the table first half is occupied
;;

    Else
        call 'USER32.MessageBoxA' 0,
      {"The Symoblics'CheckSums'Table can be viewed only after a Compilation.", 0},
      {'Nothing to show', 0}, &MB_OK

    End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Reuse of the CkeckSum64 Method for computing the Win32 Equates:
  
  The CheckSum64 Method is so fast, that it is now useless to save the computed
  Tables, 'Equates.nam' and 'Equates.num', like we did, before V.2.015c.
  
  So, each time RosAsm is started, it now loads the 'Equates.equ' File, and reuse
  the CheckSum64 Tables, to rebuild the search Table.
  
  The Records are used in different menner. Instead of "CheckSum64 / Pointer / Link",
  the 'CheckSumsRecords' for the Win32 Equates, are, "CheckSum64 / Value / Link",
  so that, - as the integrity is verified before usage of the Equates.equ File -,
  we can retrieve the Value immidiately from the CheckSum64 Table.
  
  Another difference is that the normal 'CheckSumsRecords' is Static, for the Symbols
  Jobs for the Assembler. We use it, as is, for building the Win32 Equates Table, but,
  once done, we copy this Table to a Dynamic Memory Chunk.
  
  'NewBuildWin32Equates':
  
      Build the Win32 Equates Table and saves it in 'NewWinEquatesMem'. This Routine
      includes two commented out calls to verify the conformity of the List:
    
      'VerifyEquatesFileConformity' and 'NoDuplicationOfEquates'
    
  'NewSearchForWinEquate' and 'NewGetEquates' are for retrieving the Value from
  an &EQUATE_NAME, from the Assembler ('ReplaceWin32Equates') and/or from 'RightClick'
  
  At Start-Up, you can see the Win32 Equates Table with
  [Tools] / [RosAsm Devs Tests] / [Show Symbols Repartition]
  
  Depending on the Processor, the speed improvement is between 1.5 and 5 %, compared
  to the previous Method, on an Auto-Compilation of RosAsm. For the Table built, it
  is many times faster (now, one second, on my old 95 Box with a generic Pentium, and
  the Click time on my Celeron 1.3).
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


[NewWinEquatesMem: ?   dWordsLenOfEquatesList: ?]

; Called from 'Main', each time RosAsm is started:

NewBuildWin32Equates: ; 'CheckSumsRecords' call OpenEquFiles
    call OpenEquFiles | On B$IncludesOK = &FALSE, ret

    ;call VerifyEquatesFileConformity

    call CountEquates ; >>> 'NumberOfEquates'
    ;hexprint D$NumberOfEquates
    push D$NumberOfEquates

        mov esi D$EquateIncMemory

        mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords

L0:     mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | mov B$ebx 0

        call CheckSum64 | call CheckSum16
    ; ebx:eax = CheckSum64 // ecx = CheckSum16

    ; Get the Hexa Value into edx:
        push eax
            inc esi  ; skip over the space
            mov edx 0
L1:         shl edx 4 | mov al B$esi | sub al '0' | On al > 9, sub al 7
            or dl al
            inc esi | cmp B$esi CR | ja L1<
            add esi 2
        pop eax

        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            mov D$CheckSumsRecords+ecx eax
            mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 edx
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0
                    ;call NoDuplicationOfEquates
                    mov edi D$edi+12
                End_While
                ;call NoDuplicationOfEquates
                move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            mov edi D$PointerToCheckSumsLinkedRecords
            mov D$edi eax
            mov D$edi+4 ebx
            move D$edi+8 edx
            add D$PointerToCheckSumsLinkedRecords 16
        .End_If

        dec D$NumberOfEquates | cmp D$NumberOfEquates 0 | jne L0<<

    pop D$NumberOfEquates

  ; Now, store the Win32 Equates CheckSums Table into Memory:

    mov ecx D$PointerToCheckSumsLinkedRecords | add ecx 010 | sub ecx CheckSumsRecords

    push ecx
        VirtualAlloc NewWinEquatesMem, ecx
    pop ecx

    mov esi CheckSumsRecords, edi D$NewWinEquatesMem | shr ecx 2
    mov D$dWordsLenOfEquatesList ecx | rep movsd
;;
  Adjust all of the Linked Records Pointer (above, they are pointing for a Base
  of 'CheckSumsRecords'. Now, the Base must be inside 'NewWinEquatesMem'
;;
  ; Adjustement Value:
    mov eax CheckSumsRecords | sub eax D$NewWinEquatesMem

    mov ebx CheckSumsRecords | sub ebx eax
  ; edi still points to the End of the fresh copied 'NewWinEquatesMem':
    While ebx < edi
        On D$ebx+12 <> 0, sub D$ebx+12 eax
        add ebx 16
    End_While

    ;call ClearQwordCheckSum
ret
____________________________________________________________________________________________

VerifyEquatesFileConformity:
     mov esi D$EquateIncMemory, edx esi | add edx D$EquatesIncFileSize

     .While esi < edx
L0:   ; Read one Symbol;
        If W$esi = '__'
            jmp BadEquateLine
        Else_If B$esi = '_'
            ; Good
        Else_If B$esi < '0'
            jmp BadEquateLine
        Else_If B$esi > 'Z'
            jmp BadEquateLine
        End_If
        inc esi | cmp B$esi ' ' | ja L0<

      ; No trailing '_':
        On B$esi-1 = '_', jmp BadEquateLine

      ; One Space, followed by one single '0':
        inc esi
        On B$esi <> '0', jmp BadEquateLine
        On B$esi+1 = '0', jmp BadEquateLine

      ; One Hexa Value:
        While B$esi > ' '
            If B$esi < '0'
                jmp BadEquateLine
            Else_If B$esi > 'F'
                jmp BadEquateLine
            End_If

            inc esi
        End_While

      ; CRLF
        On W$esi <> CRLF, jmp BadEquateLine

        add esi 2
    .End_While
ret


[BadEquateLineMessage: "Bad Equate line in Equates.equ: 

"
 BadEquateLineLine: '                                                        ' ]

BadEquateLine:
    While B$esi > CR
        inc esi | On esi = edx, jmp L0>
    End_While

    mov D$esi 0

    dec esi

    While B$esi > LF
        dec esi | On esi = D$EquateIncMemory, jmp L0>
    End_While

L0: mov edi BadEquateLineLine
    While B$esi <> 0 | movsb | End_While | mov B$edi 0

    showme BadEquateLineMessage
ret

____________________________________________________________________________________________

[DuplicatedEquate: 'Duplicated Equates', 0]

NoDuplicationOfEquates:
    If D$edi = eax
        On D$edi+4 = ebx, call ShowDumplicatedEquates
    End_If
ret

[CheckSumEax: ?   CheckSumEbx: ?]

ShowDumplicatedEquates:
    pushad
        mov D$CheckSumEax eax, D$CheckSumEbx ebx
        mov esi D$EquateIncMemory

L0:     mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | mov B$ebx 0

        push esi
            call CheckSum64
        pop edx

        .If eax = D$CheckSumEax
            If ebx = D$CheckSumEbx
                mov edi TrashString
                While B$edx <> 0 | mov al B$edx, B$edi al | inc edx | inc edi | End_While
                mov D$edi ' == ' | add edi 4
                jmp L0>
            End_If
        .End_If
        While B$esi > CR | inc esi | End_While
        add esi 2 | jmp L0<

L0:     mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | mov B$ebx 0

        push esi
            call CheckSum64
        pop edx

        .If eax = D$CheckSumEax
            If ebx = D$CheckSumEbx
                While B$edx <> 0 | mov al B$edx, B$edi al | inc edx | inc edi | End_While
                mov B$edi 0
                jmp L0>
            End_If
        .End_If
        While B$esi > CR | inc esi | End_While
        add esi 2 | jmp L0<

L0: showme TrashString
    popad
ret
____________________________________________________________________________________________

[EquateFound: ?]

NewSearchForWinEquate:
    push esi
        call NewGetEquates

        If B$EquateFound = &FALSE
            mov esi TrashString | error D$BadWinEquPtr ; error8
        End_If

        or D$imm32 eax
    pop esi

    add esi D$NewWinEquateLenght

    .If B$esi = addSign
        On B$esi+1 <> '&', ret
        add esi 2 | jmp NewSearchForWinEquate
    .Else_If B$esi = '&'
        inc esi | jmp NewSearchForWinEquate
    .Else
        While B$esi = '_'
            inc esi
        End_While
        If B$esi = '&'
            inc esi | jmp NewSearchForWinEquate
        End_If
    .End_If
ret


[NewWinEquateLenght: ?]

; Called from the Assembler and from 'RightClick'

NewGetEquates: ;New GetEquates: RightClick
    mov edi TrashString
    While B$esi > ' '
        mov ax W$esi
        If al = '+'
            jmp L1>
        Else_If al = '-'
            jmp L1>
        Else_If ax = '__'
            jmp L1>
        Else_If al = ','
            jmp L1>
        Else_If al = ';'
            jmp L1>
        Else_If ax = '_&'
            jmp L1>
        Else_If al = '&'
            jmp L1>
        Else_If al = ']'
            jmp L1>
        Else_If al = ')'
            jmp L1>
        Else
            On al >= 'a', sub al 32
            mov B$edi al | inc esi | inc edi
        End_If
    End_While

L1: mov B$edi 0 | sub edi TrashString | mov D$NewWinEquateLenght edi

  ; Simplified 'GetFromQwordCheckSum':
    mov esi TrashString

ReadWin32Equate:
    call CheckSum64 | call CheckSum16

    mov esi D$NewWinEquatesMem | add esi ecx

L0: .If D$esi = eax
        If D$esi+4 = ebx
            mov eax D$esi+8, B$EquateFound &TRUE | ret
        End_If
    .End_If

    mov esi D$esi+12 | cmp esi 0 | jne L0<

    mov B$EquateFound &FALSE
ret




