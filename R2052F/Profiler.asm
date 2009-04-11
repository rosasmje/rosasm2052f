TITLE Profiler

____________________________________________________________________________________________
____________________________________________________________________________________________


ProfileComments:
;;

____________________________________________________________________________________________

  ________
  Routines:
  
  From the Menu 'M00_Profile', call to the Main Routine: 'Profiler'.
  
  From 'AsmMain':
  
  * 'InjectedCopyToCodeSourceA' injects additional Coding in the Timed App,
    instead of the normal 'NewCopyToCodeSourceA'. (Materials at 'InjectedTIME_COUNT')
    
  * In 'HotParsers', after the call to 'StripUnderscore', turn the '.' of
    'InjectedTIME_COUNT' into '_', to build unique Symbols, by calling to
    'InjectDashLines'.
  
  * After execution of 'EncodeLines', call to 'CreateProfilerTables'
  
  * 'FillCodeSymbols' takes in charge the filling of the Relays Table and
    does the substitutions. >>> 'TimingCalls'
  
  ______
  Plan_2:
  
  All calls are turned "Call Time_Count" So, we need a Table for storing the
  real calls Addresses. Same size as the CodeList Buffer. In 'FillCodeSymbols',
  "If B$ProfilerFlag = &TRUE", we do the substitution and save the Original Call
  Address in the Parallel Table.
  
  Inserted Routins, in the compiled Applicatinon, at 'InjectedTIME_COUNT'.
  There, the real call will be in the form of "call D$Address_Relay", which
  Variable will hold the Table Assress.
  
  This Table is created On RosAsm Side: Call for 'CreateMemoryMapFile'. This
  sould be done after the call to EncodeLines, in 'AsmMain', by 'CreateProfilerTables'
  
  The 'Time_Count' inserted Routine must write the Timing in a second Table
  dedicated to these recordings.
  
  How to record the Timings?
  
  We need a qWords Table where what will be stored will be the difference
  between rdtsc, before and after the real call.
  
  Note: I do not see any Function associted to 'CreateFileMapping' that could
  tell the size of the File (?...). For now, simpler is to store this size in
  a Variable of 'InjectedTIME_COUNT'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[TimingMapName: 'TestMem.tst', 0]

[HandleOfTimingMapFile: ?  OrigineOfTimingMap: ?]

Proc CreateMemoryMapFile:
    Argument @Size
        call 'KERNEL32.CreateFileMappingA' 0-1, &NULL, &PAGE_READWRITE,
                                        0, D@Size, TimingMapName
        mov D$HandleOfTimingMapFile eax

        call 'KERNEL32.MapViewOfFile' D$HandleofTimingMapFile, &FILE_MAP_ALL_ACCESS,
                                      0, 0, D@Size
        mov D$OrigineOfTimingMap eax
ret


ReadMemoryMapFile:
    mov esi D$OrigineOfTimingMap, eax D$esi
    ;hexprint eax
ret


DeleteMemoryMapFile:
    call 'KERNEL32.UnmapViewOfFile' D$OrigineOfTimingMap
    call 'KERNEL32.CloseHandle' D$HandleOfTimingMapFile
ret


OpenMemoryMapFile:
    call 'KERNEL32.CreateFileMappingA' 0-1, &NULL, &PAGE_READWRITE,
                                       0, 100, TimingMapName
    mov D$HandleOfTimingMapFile eax

    call 'KERNEL32.MapViewOfFile' D$HandleofTimingMapFile, &FILE_MAP_ALL_ACCESS,
                                  0, 0, 100
    mov D$OrigineOfTimingMap eax
ret


WriteMemoryMapFile:
    mov esi D$OrigineOfTimingMap

    mov D$esi 012345678
ret


CloseMemoryMapFile:
    call 'KERNEL32.UnmapViewOfFile' D$OrigineOfTimingMap
    call 'KERNEL32.CloseHandle' D$HandleOfTimingMapFile
ret

____________________________________________________________________________________________


[ProfileTable: ?   ProfilerFlag: ?]

; ProfileComments

Profiler:
    mov B$ProfilerFlag &TRUE
    mov D$AddressOfTimeCount 0

    mov B$ShowStats &FALSE | call AsmMain | mov B$ShowStats &TRUE
    mov D$OldStackPointer 0
  ; Main Call >>> 'CreateProfilerTables'

  ret


    On B$CompileErrorHappend = &TRUE, ret

  ; Similar to 'ScanShortenJmpsTable'
    mov ebx D$CodeRef | add ebx 5

  ; Case of Api Calls: ...|0FF dWordCodeAddress|:
  ;                   ....|.....|
    While B$ebx = 0FF | add ebx 6 | End_While

    mov D$CodeRefScan ebx

    mov B$ProfilerFlag &FALSE
ret


[ProfilerOriginalCalls: ?  ProfilerNumberOfCalls: ?]

CreateProfilerTables:
;;
 What Table do we need in the MapFile?
 
 - Storing the Real Calls Addresses. Same size as the real Code.
 - Storing the Number of calls. Same size as the real Code.
 - Storing the Timings on the calls Rooms. One qWord each. Double Size.
 
;;
  ; (Size of Code) * 4:
    mov ecx D$CodeListPtr | sub ecx D$CodeOrigine | shl ecx 2

    call CreateMemoryMapFile ecx

  ; The Timed App needs to know of the Displacement...
  ; .. and 'FillCodeSymbols' must fill the proper Displacements...
ret


ReleaseProfilerTables:
    VirtualFree D$ProfilerOriginalCalls, D$ProfilerNumberOfCalls
ret

____________________________________________________________________________________________

[AddressOfTimeCount: ?]
[ModelOfTimeCount: B$ 'TIME_COUNT', 0]

SetAddressOfTimeCount:
    pushad
      ; 'FillCodeSymbols'
        call GetFromQwordCheckSum ModelOfTimeCount, D$LabelList, D$LabelListLimit
        While B$eax > LowSigns | inc eax | inc esi | End_While | inc eax | inc esi
        mov edi eax, ecx 0
    popad
ret

[InjectedTIME_COUNT: B$ "
Time.Count:
  ; The 'Call' Return Address is at D$esp
    pop D$ReturnAddressOfTimeCount

        push eax, ebx, ecx, edx
            cpuid | rdtsc
            sub D$Duration eax | sbb D$Duration+4 edx
        pop edx, ecx, ebx, eax

        call D$CallAddress

        push eax, ebx, ecx, edx
            cpuid | rdtsc
            add D$Duration eax | adc D$Duration+4 edx
        pop edx, ecx, ebx, eax

        hexprint D$Duration+4, D$Duration

    push D$ReturnAddressOfTimeCount
ret

; To be paste here: 'OpenMemoryMapFile', 'WriteMemoryMapFile', 'CloseMemoryMapFile'.

", InjectedTIME_COUNT_Len: Len]


; Called from 'HotParsers': ; CoolParsers

InjectDashLines:
    mov esi D$CodeSourceA, ecx 1
  ; ecx = Hard code number of '.' in 'InjectedTIME_COUNT'.
L0: If B$esi = '.'
        mov B$esi '_' | loop L0<
    Else
        jmp L0<
    End_If
ret


[CallByte: 0E8]

Profile:
    call 'KERNEL32.GetStartupInfoA' STARTUPINFO
    call 'KERNEL32.CreateProcessA' DestinationFile, &NULL, &NULL, &NULL, &FALSE,
                                   &DEBUG_PROCESS__&DEBUG_ONLY_THIS_PROCESS,
                                   &NULL, &NULL, STARTUPINFO, PROCESS_INFORMATION

    .While eax = &TRUE
L0:     call 'KERNEL32.WaitForDebugEvent' DEBUG_EVENT, &INFINITE

        .If D$DE.dwDebugEventCode = &EXIT_PROCESS_DEBUG_EVENT
            jmp L9>>

        .Else_If D$DE.dwDebugEventCode = &EXCEPTION_DEBUG_EVENT
            If D$E.ExceptionCode = &EXCEPTION_BREAKPOINT

                ; Debugger_OnException
                mov D$C.ContextFlags &CONTEXT_FULL
                call 'KERNEL32.GetThreadContext' D$PI.hThread, CONTEXT
                ;mov ebx D$C.regEip

           ;     mov D$C.ContextFlags &CONTEXT_CONTROL
           ;     call 'KERNEL32.GetThreadContext' D$PI.hThread, CONTEXT
           ;     or D$C.regFlag 0100
           ;     call 'KERNEL32.SetThreadContext' D$PI.hThread, CONTEXT
;call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$E.ExceptionAddress, Trash, 1, &NULL
;hexprint D$Trash
                ;call 'KERNEL32.WriteProcessMemory' D$PI.hProcess, D$E.ExceptionAddress,
                ;                                   CallByte, 1, &NULL

                call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$C.regEip, Trash, 5

                mov eax Trash | ;int3
;hexprint D$E.ExceptionAddress
                call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId,
                                                   &DBG_CONTINUE
              jmp L0<<

           ; Elseif D$E.ExceptionCode = &EXCEPTION_SINGLE_STEP
           ;     call 'KERNEL32.GetThreadContext' D$PI.hThread, CONTEXT
           ;     or D$C.regFlag 0100
           ;     call 'KERNEL32.SetThreadContext' D$PI.hThread, CONTEXT
           ;     call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId,
           ;                                        &DBG_CONTINUE
           ;   jmp L0<<

            End_If
        .End_If

        call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId,
                                           &DBG_EXCEPTION_NOT_HANDLED
    .End_While

L9: call 'Kernel32.CloseHandle' D$PI.hProcess
    call 'Kernel32.CloseHandle' D$PI.hThread
ret


ShowProfilerStats:
   ; call 'USER32.MessageBoxA' 0, {'Hi', 0}, {'Hi', 0}, 0
ret


; Called from 'FillCodeSymbols':

TimingCalls:
  ; eax+4 = Original Displacement.

    On B$edi+1 <> 0E8, ret

    On D$AddressOfTimeCount = 0, call SetAddressOfTimeCount
ret



























