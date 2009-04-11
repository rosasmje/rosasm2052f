TITLE Format
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 >>> 'PeHeader' <<<
 SubSystem: W$ 2 > GUI // 3 > CON

 CodeCharacteristics:
 D$   0_60000020                ; readable, runable, code
      0_40000040                ; readable
      0_C0000040                ; readable, writable, initialised data
      0_C0000000                ; Not readable initialised data; don't keep; don't cache
      0_6000840                 ; Not readable initialised data; don't keep; don't cache
0x00000020 code. Usually set in conjunction with the executable flag (0x80000000
0x00000040 initialized data. Almost all sections except executable and the .bss section have this flag set.
0x00000080 uninitialized data (for example, the .bss section).
0x00000200 comments or some other type of information. A typical use of this section is the .drectve section emitted by the compiler, which contains commands for the linker.
0x00000800 shouldn't be put in the final EXE file. These sections are used by the compiler/assembler to pass information to the linker.
0x02000000 can be discarded, since it's not needed by the process once it's been loaded. The most common discardable section is the base relocations (.reloc).
0x10000000 shareable. When used with a DLL, the data in this section will be shared among all processes using the DLL. The default is for data sections to be nonshared, meaning that each process using a DLL gets its own copy of this section's data. In more technical terms, a shared section tells the memory manager to set the page mappings for this section such that all processes using the DLL refer to the same physical page in memory. To make a section shareable, use the SHARED attribute at link time. For example
LINK /SECTION:MYDATA,RWS ...
tells the linker that the section called MYDATA should be readable, writeable, and shared.
0x20000000 executable. This flag is usually set whenever the "contains code" flag (0x00000020) is set.
0x40000000 readable. This flag is almost always set for sections in EXE files.
0x80000000 writeable. loader should mark the memory mapped pages as read-only or execute-only. Typical sections with this attribute are .data and .bss. Interestingly, the .idata section also has this attribute set.

 >>> code = 0_60000020 (unwriteable) // 0_A0000020 (Writeable)
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

OutputFormat:
    If D$OutputHandle = 0
        call 'USER32.DialogBoxParamA' D$hinstance, 19000,  D$hwnd, OutputFormatProc, 0
    Else
        Beep | ret
    End_If

    .If D$TempoSavingExtension  = '.DLL'
        If D$OutputHandle = 0
            call 'USER32.DialogBoxParamA' D$hinstance, 21000,  D$hwnd, DLLFormatProc, 0
        Else
            Beep | ret
        End_If
    .End_If
;;
    .If D$TempoSavingExtension  = '.SYS'
        If D$OutputHandle = 0
            call 'USER32.DialogBoxParamA' D$hinstance, 21001,  D$hwnd, SYSFormatProc, 0
        Else
            Beep | ret
        End_If
    .End_If
;;
ret


[TempoSubSystem: ?    TempoSavingExtension: ?    TempoCodeCharacteristics: ?
 TempoDataCharacteristics: ?
 TempoLinkerDllDefault: ?   OutputHandle: ?]

; Tag Dialog 19000

Proc OutputFormatProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        .If D@wParam = &IDCANCEL
            jmp L1>

        .Else_If D@wParam = &IDOK
            call SaveOutputFormat
L1:         mov D$OutputHandle 0 | call 'User32.EndDialog' D@Adressee 0

        .Else_If D@wParam = 10
          ; GUI:
            mov D$TempoSubSystem &IMAGE_SUBSYSTEM_WINDOWS_GUI,
                D$TempoSavingExtension '.EXE'
        .Else_If D@wParam = 11
          ; CON:
            mov D$TempoSubSystem &IMAGE_SUBSYSTEM_WINDOWS_CUI,
                D$TempoSavingExtension '.EXE'
        .Else_If D@wParam = 12
          ; ScreenSaver:
            mov D$TempoSubSystem &IMAGE_SUBSYSTEM_WINDOWS_GUI,
                D$TempoSavingExtension '.SCR'
        .Else_If D@wParam = 13
          ; DLL:
            mov D$TempoSubSystem &IMAGE_SUBSYSTEM_WINDOWS_GUI,
                D$TempoSavingExtension '.DLL' | call DlgSetRelocs; jE!
        .Else_If D@wParam = 14
          ; SYS:
            mov D$TempoSubSystem &IMAGE_SUBSYSTEM_NATIVE,
                D$TempoSavingExtension '.SYS' | call DlgSetRelocs; jE!
; 'SubSystem'
        .Else_If D@wParam = 210

    ;  ..Else_If D$wParam = 211                                    ; Num EditControls, no use.
    ;  ..Else_If D$wParam = 212
    ;  ..Else_If D$wParam = 213

        .Else_If D@wParam = 300                                     ; Writeable Code
            xor D$TempoCodeCharacteristics 0_8000_0000

        .Else_If D@wParam = 301
            xor D$TempoDataCharacteristics &IMAGE_SCN_MEM_SHARED

        .Else_If D@wParam = 302 ; jE! Relocs
            xor D$RelocsWanted 1

        .Else_If D@wParam = 303 ; jE! ExportALL
            xor D$ExportALL 1

        .Else
            jmp L8>>

        .End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$OutputHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

        call InitOutputDialog

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP

DlgSetRelocs: mov D$RelocsWanted &TRUE | call 'USER32.CheckDlgButton' D$OutputHandle, 302, &TRUE | ret; jE!

[LinkerDllDefaultString: '           ', 0] [DllAttachDetach: 0]

; Tag Dialog 21000

Proc DLLFormatProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        ..If D@wParam = &IDCANCEL
L1:         mov D$OutputHandle 0
            call 'User32.EndDialog' D@Adressee 0

        ..Else_If D@wParam = &IDOK
            mov ax W$DllAttachDetach, W$DllCharacteristics ax
            call SaveDLLLinkerDefault | cmp eax 0 | jne L1<

 ; DllCharacteristics:     ; 0001h - Per-Process Library Initialization
 ;                         ; 0002h - Per-Process Library Termination
 ;                         ; 0004h - Per-Thread Library Initialization
 ;                         ; 0008h - Per-Thread Library Termination

        ..Else_If D@wParam = 200             ; &DLL_PROCESS_ATTACH = 1
            or D$DllAttachDetach 4 | xor D$DllAttachDetach 4
            or D$DllAttachDetach 1
            call CheckDLLFlags
        ..Else_If D@wParam = 201             ; &DLL_PROCESS_DETACH = 0
            or D$DllAttachDetach 8 | xor D$DllAttachDetach 8
            or D$DllAttachDetach 2
            call CheckDLLFlags
        ..Else_If D@wParam = 202             ; &DLL_THREAD_ATTACH = 2
            or D$DllAttachDetach 1 | xor D$DllAttachDetach 1
            or D$DllAttachDetach 4
            call CheckDLLFlags
        ..Else_If D@wParam = 203             ; &DLL_THREAD_DETACH = 3
            or D$DllAttachDetach 2 | xor D$DllAttachDetach 2
            or D$DllAttachDetach 8
            call CheckDLLFlags

        ..Else_If D@wParam = 100                                     ; LinkerDllDefault

        ..Else
            jmp L8>>

        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$OutputHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        mov edi LinkerDllDefaultString, ecx 10 al ' ' | rep stosb
        mov eax D$LinkerDllDefault, ebx eax | mov edi LinkerDllDefaultString | add edi 10
        std
            mov ecx, 8
L1:         mov al bl | and al 0F | On al >= 0A, add al 7
            add al, '0' | stosb | shr ebx, 4 | loop L1<
        cld
        inc edi
        push edi
            call 'USER32.GetDlgItem' D@Adressee 100
        pop edi
        call 'USER32.SendMessageA' eax &WM_SETTEXT 0 edi

        mov ax W$DllCharacteristics, W$DllAttachDetach ax
        call CheckDLLFlags

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP

____________________________________________________________________________________________

[NO_SOURCE 10, SOURCE_IN_CHECKSUM 11, SOURCE_OUT_CHECKSUM 12]

[SysOutputType: ?]

; Tag Dialog 21001

Proc SYSFormatProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        ..If D@wParam = &IDCANCEL
L1:         mov D$OutputHandle 0
            call 'User32.EndDialog' D@Adressee 0

        ..Else_If D@wParam = &IDOK
            jmp L1<

        ..Else_If D@wParam = 10
            mov D$SysOutputType NO_SOURCE
        ..Else_If D@wParam = 11
            mov D$SysOutputType SOURCE_IN_CHECKSUM
        ..Else_If D@wParam = 12
            mov D$SysOutputType SOURCE_OUT_CHECKSUM
        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$OutputHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

    ...Else
L8:     popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP
____________________________________________________________________________________________


[DllAdressRange: "
 Smaller than: 0_8000_0000
 Bigger  than:    040_0000     ", 0
 DllAdressRangeTitle: 'Dll Load Adress range is:', 0]

SaveDLLLinkerDefault:
    call 'USER32.GetDlgItem' D$OutputHandle 100
    call 'USER32.SendMessageA' eax &WM_GETTEXT 11 LinkerDllDefaultString

    mov esi LinkerDllDefaultString, ebx 0, eax 0
L0: lodsb | cmp al ' ' | je L0<
            cmp al '_' | je L0<
            cmp al 0   | je L8>
    sub al '0' | On al > 9, sub al 7
    If al > 0F
        mov eax 0 | jmp L9>
    End_If
    shl ebx 4 | add ebx eax | jmp L0<

L8: mov eax ebx | Align_On 01000 eax
    If eax < 0_40_0000
        jmp L2>
    Else_If eax >= 0_8000_0000
L2:     call 'USER32.MessageBoxA' D$hwnd, DllAdressRange, DllAdressRangeTitle, &MB_SYSTEMMODAL
        mov eax 0
    Else
        mov D$LinkerDllDefault eax
    End_If

L9: ret


 ; DllCharacteristics:     ; 0001h - Per-Process Library Initialization
 ;                         ; 0002h - Per-Process Library Termination
 ;                         ; 0004h - Per-Thread Library Initialization
 ;                         ; 0008h - Per-Thread Library Termination

CheckDLLFlags:

    On D$DllAttachDetach = 0, mov D$DllAttachDetach 3    ; Default on first run

    Test D$DllAttachDetach 1 | jnz L1>
        or D$DllAttachDetach 4 | jmp L2>    ; If not 1 > 4
L1: and D$DllAttachDetach 0_FFFF_FFFB       ; If 1 > not 4

L2: Test D$DllAttachDetach 2 | jnz L1>
        or D$DllAttachDetach 8 | jmp L2>    ; If not 2 > 8
L1: and D$DllAttachDetach 0_FFFF_FFF7       ; If 2 > not 8

L2: call 'USER32.CheckDlgButton' D$OutputHandle 200 &FALSE
    call 'USER32.CheckDlgButton' D$OutputHandle 201 &FALSE
    call 'USER32.CheckDlgButton' D$OutputHandle 202 &FALSE
    call 'USER32.CheckDlgButton' D$OutputHandle 203 &FALSE

    Test D$DllAttachDetach 1 | jz L1>
        call 'USER32.CheckDlgButton' D$OutputHandle 200 &TRUE
L1: Test D$DllAttachDetach 2 | jz L1>
        call 'USER32.CheckDlgButton' D$OutputHandle 201 &TRUE
L1: Test D$DllAttachDetach 4 | jz L1>
        call 'USER32.CheckDlgButton' D$OutputHandle 202 &TRUE
L1: Test D$DllAttachDetach 8 | jz L1>
        call 'USER32.CheckDlgButton' D$OutputHandle 203 &TRUE
L1: ret

____________________________________________________________________________________________

InitOutputDialog:
    move D$TempoCodeCharacteristics D$CodeCharacteristics
    move D$TempoDataCharacteristics D$DataCharacteristics
    move D$TempoSavingExtension D$SavingExtension
    move D$TempoSubSystem D$SubSystem
    move D$TempoLinkerDllDefault, D$LinkerDllDefault

    call 'USER32.SetDlgItemInt' D$OutputHandle, 210, D$AppStackMin, 0
    call 'USER32.SetDlgItemInt' D$OutputHandle, 211, D$AppStackMax, 0
    call 'USER32.SetDlgItemInt' D$OutputHandle, 212, D$AppHeapMin, 0
    call 'USER32.SetDlgItemInt' D$OutputHandle, 213, D$AppHeapMax, 0

    test D$CodeCharacteristics 0_8000_0000 | jz L2>
        call 'USER32.CheckDlgButton' D$OutputHandle, 300, &TRUE

L2: test D$DataCharacteristics &IMAGE_SCN_MEM_SHARED | jz L2>
        call 'USER32.CheckDlgButton' D$OutputHandle, 301, &TRUE

L2: If D$SavingExtension = '.SCR'
        call 'USER32.CheckDlgButton' D$OutputHandle, 12, &TRUE
    Else_If D$SavingExtension = '.DLL'
        call 'USER32.CheckDlgButton' D$OutputHandle, 13, &TRUE
    Else_If D$SavingExtension = '.SYS'
        call 'USER32.CheckDlgButton' D$OutputHandle, 14, &TRUE
    Else_If D$SubSystem = 2
        call 'USER32.CheckDlgButton' D$OutputHandle, 10, &TRUE
    Else_If D$SubSystem = 3
        call 'USER32.CheckDlgButton' D$OutputHandle, 11, &TRUE
    End_If

    call 'USER32.CheckDlgButton' D$OutputHandle, 302, D$RelocsWanted ; jE!
    call 'USER32.CheckDlgButton' D$OutputHandle, 303, D$ExportALL ; jE!
   call LoadCommandLine
ret


SaveOutputFormat:
    call 'USER32.GetDlgItemInt' D$OutputHandle, 210, &NULL, &FALSE | Align_On 01000 eax
        mov D$AppStackMin eax
    call 'USER32.GetDlgItemInt' D$OutputHandle, 211, &NULL, &FALSE | Align_On 01000 eax
        On eax < D$AppStackMin, mov eax D$AppStackMin
        mov D$AppStackMax eax
    call 'USER32.GetDlgItemInt' D$OutputHandle, 212, &NULL, &FALSE
        On eax > 0,  Align_On 01000 eax
        mov D$AppHeapMin eax
    call 'USER32.GetDlgItemInt' D$OutputHandle, 213, &NULL, &FALSE | Align_On 01000 eax
        On eax < D$AppHeapMin, mov eax D$AppHeapMin
        mov D$AppHeapMax eax

    move D$CodeCharacteristics D$TempoCodeCharacteristics
    move D$DataCharacteristics D$TempoDataCharacteristics
    move D$SavingExtension D$TempoSavingExtension
    move D$SubSystem D$TempoSubSystem

    On D$TempoSavingExtension = '.SYS',
       mov W$DllCharacteristics &IMAGE_DLLCHARACTERISTICS_WDM_DRIVER

    call SaveCommandLine
ret







