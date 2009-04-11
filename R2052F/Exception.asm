TITLE Exception

;;
    General exception handler.
    
    October 2004 - Ludwig Haehne - wkx@gmx.li
    
    Reports a description of the exception that occurred, writes a crash.log file
    and tries to save the source before RosAsm is terminated by the system. 
    
    TODO Create a crash dump (data, stack, context) that can be loaded on a different 
    machine to find the problem.
;;
____________________________________________________________________________________________

Proc FinalExceptionHandler:
    Arguments @ExceptionInfo
    Uses ebx esi edi

  ; Save whole source
    On B$RealSourceRestored = &FALSE, call RestoreRealSource
    mov B$WeAreSavingPart &FALSE | call SaveSource

    mov eax D@ExceptionInfo | call GetExceptionInfo D$eax
    mov eax D@ExceptionInfo | call WriteCrashLog D$eax D$eax+4

    call 'User32.MessageBoxA' 0, ExceptionMessage,
        {'RosAsm crashed' 0}, &MB_OK+&MB_ICONEXCLAMATION

    call 'KERNEL32.SetErrorMode' &SEM_NOGPFAULTERRORBOX

    mov eax &EXCEPTION_CONTINUE_SEARCH
EndP
____________________________________________________________________________________________


[ExceptionMessage:
"An exception occurred inside RosAsm. It must terminate now.

                    YOUR WORK IS NOT LOST!
                
Your source has been saved at the path of your application.
To continue working restart RosAsm, open your application
and replace the source.

Please post a bug report describing how to reproduce this
problem along with the crash.log (in the applications folder)
at RosAsm board.

Thank you and sorry for the inconvenience.

" ExceptionInfo: "Exception occurred at address " ExceptionAddress: "########.
" ExceptionDesc: B$ 0 #256]

[Exception_AV: 'Access Violation! Attempt to ' AV_ReadWrite: '######### address ' AV_Address: '########.' 0]
[Exception_other: 'Unknown exception. Code ' Exception_Code: '########' 0]

Proc GetExceptionInfo:
    Arguments @ExceptionRecord

    mov ebx D@ExceptionRecord

    mov edi ExceptionAddress
    DwordToHex D$ebx+12 ; Address

    mov eax D$ebx ; ExceptionCode

    .If eax = &EXCEPTION_ACCESS_VIOLATION
        mov eax D$ebx+20 ; read/write
        If eax = 0
            mov D$AV_ReadWrite 'read', D$AV_ReadWrite+4 ' fro', B$AV_ReadWrite+8 'm'
        Else
            mov D$AV_ReadWrite 'writ', D$AV_ReadWrite+4 'e at', B$AV_ReadWrite+8 ' '
        EndIf
        mov edi AV_Address
        DwordToHex D$ebx+24 ; inaccessible address
        mov esi Exception_AV
    .Else
        mov edi ExceptionCode
        DwordToHex D$ebx ; exc. code
        mov esi Exception_other
    .EndIf

    mov edi ExceptionDesc
    Do
        movsb
    Loop_until B$esi-1 = 0
EndP
____________________________________________________________________________________________

; Write a log-file which is really helpful.
;   * dump exception information (exc code, inaccessible address, access type)
;   * rosasm version
;   * OS information (NT/9x)
;   * register contents

[NewLineSeq: W$ 0A0D]

Proc EmitNewLine:
    Arguments @File

    call 'Kernel32.WriteFile' D@File, NewLineSeq, 2, BytesTransfered, 0
EndP

[RegContent: B$ 'Exx=12345678' 0D 0A]

Proc WriteCrashLog:
    Arguments @ExceptionRecord @Context
    Local @File

    call 'Kernel32.CreateFileA' {'crash.log' 0}, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
    mov D@File eax

    .If D@File <> &INVALID_HANDLE_VALUE

      ; Output RosAsm version
        mov edi AppName | call StrLen
        mov edx AppName | add edx 2 | sub eax 2
        call 'Kernel32.WriteFile' D@File, edx, eax, BytesTransfered, 0
        call EmitNewLine D@File

      ; Output Windows version
        call GetWindowsVersionString
        mov edi WindowsVersion | call StrLen
        call 'Kernel32.WriteFile' D@File, WindowsVersion, eax, BytesTransfered, 0
        call EmitNewLine D@File
        call EmitNewLine D@File

      ; Output exception info
        mov edi ExceptionInfo | call StrLen
        call 'Kernel32.WriteFile' D@File, ExceptionInfo, eax, BytesTransfered, 0
        call EmitNewLine D@File
        call EmitNewLine D@File

      ; Output reg contents
        mov ebx D@Context

        mov edi RegContent | mov W$edi+1 'AX' | add edi 4
        DwordToHex D$ebx+0B0
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'BX' | add edi 4
        DwordToHex D$ebx+0A4
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'CX' | add edi 4
        DwordToHex D$ebx+0AC
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'DX' | add edi 4
        DwordToHex D$ebx+0A8
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'SI' | add edi 4
        DwordToHex D$ebx+0A0
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'DI' | add edi 4
        DwordToHex D$ebx+09C
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'BP' | add edi 4
        DwordToHex D$ebx+0B4
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'SP' | add edi 4
        DwordToHex D$ebx+0C4
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        call 'Kernel32.CloseHandle' D@File
    .EndIf

EndP
____________________________________________________________________________________________

; Get windows version information. Original C code from MSDN converted to RosAsm.

[WindowsVersion: B$ ? #256]

[OSVersionInfo:
 OSVersionInfo.Size: D$ ?
 OSVersionInfo.MajorVersion: D$ ?
 OSVersionInfo.MinorVersion: D$ ?
 OSVersionInfo.BuildNumber: D$ ?
 OSVersionInfo.PlatformId: D$ ?
 OSVersionInfo.CSDVersion: B$ ? #128
 OSVersionInfo.ServicePackMajor: W$ ?
 OSVersionInfo.ServicePackMinor: W$ ?
 OSVersionInfo.SuiteMask: W$ ?
 OSVersionInfo.ProductType: B$ ?
 OSVersionInfo.Reserved: B$ ?]

[OSVI_SIZE 148 OSVI_EX_SIZE 156]

Win2003ServerProductType:
    mov ax W$OSVersionInfo.SuiteMask
    test ax &VER_SUITE_DATACENTER | jz L0>
        mov esi {'Datacenter Edition' 0} | ret
L0: test ax &VER_SUITE_ENTERPRISE | jz L0>
        mov esi {'Enterprise Edition' 0} | ret
L0: test ax 0400 | jz L0> ;&VER_SUITE_BLADE | jz L0>
        mov esi {'Web Edition' 0} | ret
L0: mov esi {'Standard Edition' 0}
ret

Win2000ServerProductType:
    test ax &VER_SUITE_DATACENTER | jz L0>
        mov esi {'Datacenter Server' 0} | ret
L0: test ax &VER_SUITE_ENTERPRISE | jz L0>
        mov esi {'Advanced Server' 0} | ret
L0: mov esi {'Server' 0}
ret

Proc TestWinNTSP6a:
    Local @Key

    .If D$OSVersionInfo.MajorVersion = 4
        lea eax D@Key
        call 'ADVAPI32.RegOpenKeyExA' &HKEY_LOCAL_MACHINE,
            {'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009' 0},
            0, &KEY_QUERY_VALUE, eax

        If eax = &ERROR_SUCCESS
            mov al 'a' | stosb
        EndIf

        call 'ADVAPI32.RegCloseKey' D@Key
    .EndIf
EndP

[WinNTProductType: B$ ? #80 WinNTPTLen: D$ ?]

Proc GetWindowsProductInfo:
    Local @Key

    mov esi 0

    mov D$OSVersionInfo.Size OSVI_EX_SIZE
    call 'Kernel32.GetVersionExA' OSVersionInfo
    ...If eax = 1

      ; workstation
        ..If W$OsVersionInfo.ProductType = &VER_NT_WORKSTATION
            .If D$OSVersionInfo.MajorVersion = 4
                mov esi {'Workstation 4.0' 0}
            .Else
                mov ax W$OSVersionInfo.SuiteMask
                and ax 0200 ;&VER_SUITE_PERSONAL
                If ax <> 0
                    mov esi {'Home Edition' 0}
                Else
                    mov esi {'Professional' 0}
                EndIf
            .EndIf

       ; server
         ..Else
            .If D$OSVersionInfo.MajorVersion = 5
                If D$OSVersionInfo.MinorVersion = 2
                    call Win2003ServerProductType
                ElseIf D$OSVersionInfo.MinorVersion = 0
                    call Win2000ServerProductType
                EndIf
            .Else
                mov ax W$OSVersionInfo.SuiteMask
                and ax &VER_SUITE_ENTERPRISE
                If ax <> 0
                    mov esi {'Server 4.0 Enterprise' 0}
                Else
                    mov esi {'Server 4.0' 0}
                EndIf
            .EndIf
         ..EndIf

    ...Else

        lea eax D@Key
        call 'ADVAPI32.RegOpenKeyExA' &HKEY_LOCAL_MACHINE,
            {'SYSTEM\CurrentControlSet\Control\ProductOptions' 0},
            0, &KEY_QUERY_VALUE, eax

        On eax <> &ERROR_SUCCESS, ExitP

        mov D$WinNTPTLen 80
        call 'ADVAPI32.RegQueryValueExA' D@Key, {'ProductType' 0},
            0, 0, WinNTProductType, WinNTPTLen

        On eax <> &ERROR_SUCCESS, ExitP
        On D$WinNTPTLen > 80, ExitP

        call 'ADVAPI32.RegCloseKey' D@Key

        If D$WinNTProductType = 'WINN'
            mov esi {'Workstation' 0}
        ElseIf D$WinNTProductType = 'LANM'
            mov esi {'Server' 0}
        ElseIf D$WinNTProductType = 'SERV'
            mov esi {'Advanced Server' 0}
        EndIf

    ...EndIf
EndP

IntToStr:
    mov dl 0FF | push edx                       ; Push stack end mark
    mov ecx 10
L0: mov edx 0
    div ecx | push edx | cmp eax 0 | ja L0<     ; Push remainders
L2: pop eax                                     ; Retrieve Backward
    cmp al 0FF | je L9>                         ; Over?
    add al '0' | stosb | jmp L2<             ; Write
L9: ret

Proc GetWindowsVersionString:

    mov D$OSVersionInfo.Size OSVI_SIZE
    call 'Kernel32.GetVersionExA' OSVersionInfo

    mov edi WindowsVersion

    ..If D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32_NT

      ; Major versions
        .If D$OSVersionInfo.MajorVersion = 5
            If D$OSVersionInfo.MinorVersion = 2
                mov esi {'MS Windows Server 2003' 0}
            ElseIf D$OSVersionInfo.MinorVersion = 1
                mov esi {'MS Windows XP' 0}
            ElseIf D$OSVersionInfo.MinorVersion = 0
                mov esi {'MS Windows 2000' 0}
            EndIf
        .ElseIf D$OSVersionInfo.MajorVersion <= 4
            mov esi {'MS Windows NT' 0}
        .EndIf

        While B$esi <> 0 | movsb | EndWhile
        mov al ' ' | stosb

      ; Service pack number
        mov esi OSVersionInfo.CSDVersion
        While B$esi <> 0 | movsb | EndWhile
        call TestWinNTSP6a

      ; Build number
        mov esi {' Build ' 0}
        While B$esi <> 0 | movsb | EndWhile
        movzx eax W$OSVersionInfo.BuildNumber
        call IntToStr
        mov al ' ' | stosb

      ; Home / Professional / ...
        call GetWindowsProductInfo
        If esi <> 0
            While B$esi <> 0 | movsb | EndWhile
            mov al ' ' | stosb
        EndIf

    ..ElseIf D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32_WINDOWS

      ; Major versions
        .If D$OSVersionInfo.MajorVersion = 4
            mov eax 0
            If D$OSVersionInfo.MinorVersion = 90
                mov esi {'MS Windows ME' 0}
            ElseIf D$OSVersionInfo.MinorVersion = 10
                mov esi {'MS Windows 98' 0}
                On B$OSVersionInfo.CSDVersion+1 = 'A', mov eax ' SE'
            ElseIf D$OSVersionInfo.MinorVersion = 0
                mov esi {'MS Windows 95 ' 0}
                On B$OSVersionInfo.CSDVersion+1 = 'B', mov eax 'OSR2'
                On B$OSVersionInfo.CSDVersion+1 = 'C', mov eax 'OSR2'
            EndIf
            While B$esi <> 0 | movsb | EndWhile
            stosd
        .EndIf

    ..ElseIf D$OSVersionInfo.PlatformId = &VER_PLATFORM_WIN32S

        mov esi {'MS Win32s' 0}
        While B$esi <> 0 | movsb | EndWhile

    ..EndIf

    mov B$edi 0
EndP

























