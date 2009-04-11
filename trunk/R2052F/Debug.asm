TITLE Debug

[DebuggerVersion: 'RosAsm Debugger V2.2b' 0]

;;
_____________________________________________________________________________________________

                             Debugger [Version 2.2b] - Ludwig Haehne
     
    * [Bugfix] Added security for degenerated call stack cases
    
  RosAsm 2.033b [Debugger V2.2a]
  
    * [Bugfix] FPU flags toolbar was not effective
  
  RosAsm 2.031a [Debugger V2.2]
                 
    * [Feature] Localized user interface
    * [Feature] Recognize stack-frames in Call stack set up by ENTER intruction
    * [Change] Ctrl+F shortcut for FPU info removed (conflict with search feature)
    * [Change] Terminate debuggee immediately when debugger window is closed
    * [Change] Do not inherit startup information to debuggee
    * [Bugfix] Preserve z-order and focus of debuggee's windows across debug events
    * [Bugfix] Minimize/Restore dialog messed up child-window sizes    
    * [Bugfix] Sourceposition was wrong when a breakpoint was set on a single-byte instruction
    * [Bugfix] Error checking for VirtualQuery

  RosAsm 2.027a [Debugger V2.1b]

    * [Bugfix] Menu did not work on Win95
    * [Bugfix] MMX register contents did not show properly on Win9x
    * [Bugfix] Occasional crash when mouse hints showed up behind other windows on Win98

  RosAsm 2.025e [Debugger V2.1a]
  
    * [Bugfix] Thread synch problems fixed when running multiple debugger instances
    * [Bugfix] Logfiles now have application specific names
                             
  RosAsm 2.025a [Debugger V2.1]
  
    * [Feature] Local symbol support for mouse hints and call stack
    * [Feature] New filter capabilities in call stack (context menu)
    * [Feature] Sort data labels by Address or Name (select in context menu)
    * [Feature] Toolbar
    * [Change] Flags moved into toolbar (can be hidden)
    * [Change] General code cleanup
    * [Bugfix] Rare "Invalid path" problem fixed
____________________________________________________________________________________________
          
 Known bugs:
 
    * Check for Invalid parameter counts in callstack computation (MyPNN)
 
    * Two byte breakpoints make the debuggee crash when HoldOnBreakPoints = 0
    * Terminate leaves process and thread handle open    
    * Stepping into int 02e seeds a 'breakpoint' at the return address (TrackPopupMenu)
    * Int 3 in Hook procedures (mouse hooks) hangs the Windows GUI
   
    *   "There is an issue about focus at debug time. "Sometimes" the debuger  set  the 
        focus to main debugee window.
        when the focus was at a second overlaped window (not the main nor a  sysmodal) i want 
        the focus to go back where it was, not to the owner  window. I have a rare efect 
        even without BPs at debug time" (from Marcelo)
        
        >> Find a way to not touch the z-order of the debuggee windows after run/step-over.
        
    * Close debug window when debuggee has message box open  (except.exe)
        -> wait -> "Debug thread does not respond" -> [Ok] 
        -> Unknown exception (C0000008) in CloseHandle called by DebugRsrcManager
        (exception does NOT crash RosAsm also it does not seem to have a SEH attached !?)

 Future ;)
 
    * Window message logger
    * Structured data memory inspector
    * Conditional breakpoints
    * Editable contents (register, memory)
    * Symbolic Profiler
____________________________________________________________________________________________
                          
 Maintainer since November 2003: Ludwig Haehne, wkx@gmx.li
 
 I have almost completely rewritten the existing debugger to introduce new features and 
 have a better UI.
 
 The debugger is now roughly divided into the following sections:
 
 Main Debug Routines
 
  * 'Debugger' is called when you click on [Run] and creates the 'DebugRsrcManagerThread'
  * 'KillDebugger' is called whenever you try to modify your sources
  
 Debugger polling Thread (handles debug events)
 
  * 'DebugThread', 'ShowProcessWindows', 'ScanStackForCodePointer', 'CloseProcess',
    'ReadProcessMem', 'WriteProcessMem', 'SignalDebugEvent', 'EncounterException'
  
 Breakpoint management
 
  * 'CreateBPTable', 'DestroyBPTable', 'AddProcessBreakpoint', 'DeleteProcessBreakpoint', 
    'DisableProcessBreakpoints', 'EnableProcessBreakpoints', 'EncounterBreakpoint', 
    'KillBreakpointGroup'
    
 Breakpoint synchronization with Source editor
  
  * 'CreateBPAnteroom', 'DestroyBPAnteroom', 'InitialFillBPAnteroom', 'AddBPToAnteroom',
    'ClearBPAnteroom'

 Address Space Routines
 
  * 'VirtualQuery', 'IsProcessMemory', 'IsProcessCode', 'FindNextPage', 'FindPrevPage'
  
 Call stack routines
  
  * 'GenerateCallStack', 'DestroyCallStack', 'ReadApplicationStack', 'IsReturnAddress',
    'IsReturnAddressInSource', 'GetCodeLabelFromAddress', 'GetNearestProc', 'CountParametersAndScanStackFrame'
    
 Module management
 
  * 'IsModuleCode'
_____________________________________________________________________________________________

;;

[DEBUG_EVENT:
 DE.dwDebugEventCode: D$ ?
 DE.dwProcessId: D$ ?
 DE.dwThreadId: D$ ?
 DE.u:
    CPDI.hFile:         CTDI.hThread:           E.ExceptionCode:    ODS.DebugString:  LOADDLL.hFile:    UNLOADDLL.Base: D$ ?
    CPDI.hProcess:      CTDI.lpThreadLocalBase: E.ExceptionFlags:   ODS.Unicode:      LOADDLL.Base:     W$ ?
                                                                    ODS.StringLen:    W$ ?
    CPDI.hThread:       CTDI.lpStartAddress:    E.ExceptionRecord:                    LOADDLL.DIOffset: D$ ?
    CPDI.lpBaseOfImage:                         E.ExceptionAddress:                   LOADDLL.DISize:   D$ ?
                                                E.NumParams:                          LOADDLL.Name:     D$ ?
                                                E.ExceptionInfo:                      LOADDLL.Unicode:  D$ ?
                                                                                      D$ ?
    CPDI.lpStartAddress:                                                              D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                                                      D$ ?
                                                E.FirstChance:                        D$ ?]

[DamnedUnionTail: ? #100]


____________________________________________________________________________________________

[CONTEXT:
 C.ContextFlags: D$ ?
 C.iDr0: D$ ?
 C.iDr1: D$ ?
 C.iDr2: D$ ?
 C.iDr3: D$ ?
 C.iDr6: D$ ?
 C.iDr7: D$ ?
 C.FloatSave.ControlWord: D$ ?
 C.FloatSave.StatusWord: D$ ?
 C.FloatSave.TagWord: D$ ?
 C.FloatSave.ErrorOffset: D$ ?
 C.FloatSave.ErrorSelector: D$ ?
 C.FloatSave.DataOffset: D$ ?
 C.FloatSave.DataSelector: D$ ?]
[C.FloatSave.RegisterArea: B$ ? #&SIZE_OF_80387_REGISTERS]
[C.FloatSave.Cr0NpxState: D$ ?
 C.regGs: D$ ?
 C.regFs: D$ ?
 C.regEs: D$ ?
 C.regDs: D$ ?
 C.regEdi: D$ ?
 C.regEsi: D$ ?
 C.regEbx: D$ ?
 C.regEdx: D$ ?
 C.regEcx: D$ ?
 C.regEax: D$ ?
 C.regEbp: D$ ?
 C.regEip: D$ ?
 C.regCs: D$ ?
 C.regFlag: D$ ?
 C.regEsp: D$ ?
 C.regSs: D$ ?]
[C.ExtendedRegisters: B$ ? #32
 C.regMM: B$ ? #128
 C.regXMM: B$ ? #128
 C.UnknownExtendedRegs: B$ ? #224
 EndOfContext: ?]

[ContextSize (EndOfContext-Context)]
____________________________________________________________________________________________

; All user-visible messages are stored here for easier proof-reading / localization.


[ErrorOutside:
"An exception occurred outside the Application. 

This may be due to wrong parameters in an api call or a buggy library.

The debugger highlights the last call of your application before the 
exception has occurred. Verify that the parameters are correctly set.

If the debugger points to the end of the source, either the caller 
couldn't be  identified or the exception occurred after the application 
has terminated. In the latter  case, just test if your application runs 
flawlessly without the debugger.", 0

ErrorOutsideTitle: 'External Error' 0]

[AboutDebugger:
"RosAsm's integrated Win32 debugger.

For bug reports / feature requests use 
RosAsm board or contact me via e-mail. 

Good luck! 

Ludwig Hähne <wkx@gmx.li>" 0]

[DebugThreadHangs:
"The debugger thread does not respond. 
This should not happen! The thread is now terminated!" 0]

[CriticalError: "Critical error" 0]

[MessageKillDebugger:
"You cannot modify the Source in a Debug Session    
    
                     Close the Debugger?" 0]

[DebuggerRunning: "Debugger running ..." 0]
____________________________________________________________________________________________

________________________________________________________________________________________

[Smode: '/S', 0]      ; To run Screen Savers in Saver Mode from the [Run] Button.

[UserHitF9: ?]
____________________________________________________________________________________________

[MY_CONTEXT_FULL &CONTEXT_FULL__&CONTEXT_FLOATING_POINT__&CONTEXT_DEBUG_REGISTERS__&CONTEXT_EXTENDED_REGISTERS]

; Main Routine of the Debugger.

[IsDebugging: &FALSE  Compiling: &FALSE]


[DebugRsrcManagerThreadId: ?]

Proc Debugger:
    On D$IsDebugging = &TRUE, ExitP

    call TestCPUFeatures

    ; Get OS version information - The main difference between Win9x / WinNT
    ; concerning the debugger is the address space layout.
    mov D$OSVersionInfo.Size OSVI_SIZE
    call 'Kernel32.GetVersionExA' OsVersionInfo
    If D$OsVersionInfo.PlatformId = &VER_PLATFORM_WIN32_WINDOWS
        mov D$AddressLowerBound 0
        mov D$AddressUpperBound 0_BFFF_FFFF
    EndIf

    ; Create the debug dialog that displays registers, labels and memory.
    call CreateDebugWindow

    call 'Kernel32.CreateThread' &NULL, 0, DebugRsrcManagerThread, 0,
                                 &THREAD_PRIORITY_NORMAL, DebugRsrcManagerThreadId
    If eax = &NULL
        call ReportWinError {'Debugger: CreateThread (Resource Manager)' 0}
    Else
        ; we don't need the thread handle
        call 'Kernel32.CloseHandle' eax
    EndIf
EndP
____________________________________________________________________________________________

;;
  User attempts to modify the Source in a Debug Session. 
  Impossible > offer him to either close the Debug Session or to abort the Source Edition
;;

KillDebugger:
    call 'USER32.MessageBoxA', D$hwnd, MessageKillDebugger, DebuggerRunning,
        &MB_SYSTEMMODAL__&MB_YESNO

    push eax
        On eax = &IDYES,
            call 'USER32.SendMessageA', D$DebugDialogHandle, &WM_CLOSE, 0, 0
    pop eax
ret
____________________________________________________________________________________________

; Debugger resource manager thread. This thread takes care of initializing and freeing
; all resources (objects, files, memory). It is separated from the polling
; thread to cleanup properly even if the thread hangs (for whatever reason) and must be
; terminated; which (very rarely) caused deadlocks with the past solution.

[DebugThreadHandle: ? DebugThreadId: ?] ; the polling thread
[UserInputEvent: ?] ; synch dialog with polling thread
[ThreadIDHandleTable: ? NumThreads: ?] ; maps thread id's to handles

[PROCESS_INFORMATION: ; the debuggee's process and (main) thread handle & id
 PI.hProcess: ?
 PI.hThread: ?
 PI.dwProcessId: ?
 PI.dwThreadId: ?]

[STARTUPINFO: SI_cb: ?              SI_lpReserved: ?       SI_lpDesktop: ?
              SI_lpTitle: ?         SI_dwX: ?              SI_dwY: ?
              SI_dwXSize: ?         SI_dwYSize: ?          SI_dwXCountChars: ?
              SI_dwYCountChars: ?   SI_dwFillAttribute: ?  SI_dwFlags: ?
              SI_wShowWindow: W$ ?  SI_cbReserved2: W$ ?   SI_lpReserved2: D$ ?
              SI_hStdInput: ?       SI_hStdOutput: ?       SI_hStdError: ?]

DebugRsrcManagerThread:

  ; Create an autoreset event to synchronize the debug dialog with the debugthread.
    call 'Kernel32.CreateEventA' 0, 0, 0, 0
    If eax = &NULL
        call ReportWinError {'Debugger: CreateEvent' 0}
        jmp @Exit
    EndIf
    mov D$UserInputEvent eax

    call CreateBPTable

  ; Create a lookup table to retrieve thread handles. The reason we need this is that
  ; we get a thread handle only with the Create-Thread-Debug-Event. For exception handling
  ; only the thread ID is provided, so the handle and the corresponding ID must be saved.
    VirtualAlloc ThreadIDHandleTable 4096
    mov D$NumThreads 0

    call 'KERNEL32.HeapCreate' 0, 4096, 0 ; Create a growable heap.
    mov D$ModuleNameHeap eax

    VirtualAlloc ModuleList 4096
    mov D$NumModules 0

  ; Create the thread that creates and observes the debuggee.
    call 'Kernel32.CreateThread' &NULL, 0, DebugThread, 0,
                                &THREAD_PRIORITY_NORMAL+&CREATE_SUSPENDED, DebugThreadId
    If eax = &NULL
        call ReportWinError {'Debugger: CreateThread' 0}
        ExitP
    EndIf
    mov D$DebugThreadHandle eax

  ; Create synchronization table (like a printer-spool) for dynamic breakpoints.
    call CreateBPAnteroom
    call InitialFillBPAnteroom

    call InitWatchpointResources

  ; Resume (start) the thread. The thread is created suspended to be sure that the
  ; handle is set correctly.
    call 'Kernel32.ResumeThread' D$DebugThreadHandle

  ; Enter debug event polling
    call 'KERNEL32.WaitForSingleObject' D$DebugThreadHandle, &INFINITE

  ; Now the polling thread is dead- either the debuggee has terminated or the thread
  ; was killed. Clean up.

    call FreeWatchpointResources

    call DestroyBPAnteroom

    call 'Kernel32.CloseHandle' D$DebugThreadHandle
    mov D$DebugThreadHandle 0

  ; Cleanup
    VirtualFree D$ModuleList

    call 'KERNEL32.HeapDestroy' D$ModuleNameHeap
    VirtualFree D$ThreadIDHandleTable

    call 'Kernel32.CloseHandle' D$PI.hThread
    call 'Kernel32.CloseHandle' D$PI.hProcess

    call DestroyBPTable

  ; we post a close signal no matter if the dialog still exists or not
    call 'User32.PostMessageA' D$DebugDialogHandle, &WM_CLOSE, &NULL, &NULL

    call 'Kernel32.CloseHandle' D$UserInputEvent
    mov D$UserInputEvent 0

  ; If the debuggee has called 'Kernel32.OutputDebugString' there is a file in which the
  ; output has been logged. Close the file now.
    If D$DebugLogFile <> 0
        call 'KERNEL32.CloseHandle' D$DebugLogFile
        mov D$DebugLogFile 0
    EndIf

  ; Clear data structures (safety)
    mov ecx EndOfContext | sub ecx Context | shr ecx 2
    mov edi Context, eax 0
    rep stosd

@Exit:
    call 'Kernel32.ExitThread' 0
ret
____________________________________________________________________________________________

[FilterEXE: B$ 'Executables (*.exe)' 0 '*.exe' 0 0]
[HostAppFileName: B$ ? #&MAX_PATH]
[HostAppTitle: B$ 'Host application ...' 0]

[SelectHostAppDialog:
 HostApp.lStructSize: D$ len
 HostApp.hwndOwner: D$ 0
 HostApp.hInstance: D$ 0
 HostApp.lpstrFilter: D$ FilterEXE
 HostApp.lpstrCustomFilter: D$ 0
 HostApp.nMaxCustFilter: D$ 0
 HostApp.nFilterIndex: D$ 0
 HostApp.lpstrFile: D$ DebuggeeExe
 HostApp.nMaxFile: D$ &MAX_PATH
 HostApp.lpstrFileTitle: D$ 0
 HostApp.nMaxFileTitle: D$ 0
 HostApp.lpstrInitialDir: D$ 0
 HostApp.lpstrTitle: D$ HostAppTitle
 HostApp.Flags: D$ &OFN_FILEMUSTEXIST+&OFN_EXPLORER+&OFN_NOCHANGEDIR
 HostApp.nFileOffset: W$ 0
 HostApp.nFileExtension: W$ 0
 HostApp.lpstrDefExt: D$ 0
 HostApp.lCustData: D$ 0
 HostApp.lpfnHook: D$ 0
 HostApp.lpTemplateName: D$ 0]

[DebuggeePath: B$ ? #&MAX_PATH]
[DebuggeeExe: B$ ? #&MAX_PATH]
[DebuggeeExeTitle: D$ ?]
[DebuggeeParams: D$ ?]
[CommandLineString: B$ ? #&MAX_PATH]

Proc CreateDebuggeeProcess:
    Local @Success

    mov D@Success &FALSE

    ; Allocate buffer for the command line, check if a command line file is provided and
    ; create the debuggee with debug rights for RosAsm.

    call 'KERNEL32.GetStartupInfoA' STARTUPINFO
    mov D$SI_dwFlags 0

    ; Setup Path, Executable, Parameters

    mov esi MainName, edi DebuggeePath
    While B$esi <> 0
        movsb
    EndWhile

    On B$edi <> '\', dec edi ; !!!

    While B$edi <> '\'
        mov B$edi 0
        dec edi
    EndWhile

    inc edi
    sub edi DebuggeePath
    mov D$DebuggeeExeTitle edi

    call 'Kernel32.SetCurrentDirectoryA' DebuggeePath

    If D$SavingExtension = '.DLL'
        call 'Comdlg32.GetOpenFileNameA' SelectHostAppDialog
        On eax = &FALSE, ExitP
        movzx eax W$HostApp.nFileOffset
        mov D$DebuggeeExeTitle eax
    Else
        mov edi DebuggeeExe, esi MainName
        While B$esi <> 0
            movsb
        EndWhile
        move D$edi D$SavingExtension
        mov B$edi+4 0
    EndIf

    add D$DebuggeeExeTitle DebuggeeExe

    call SetupCommandLine
    mov D$DebuggeeParams eax

    ; Assemble the commandline
    mov edi CommandLineString, ecx &MAX_PATH
    mov al '"' | stosb | dec ecx
    mov esi DebuggeeExe
    While B$esi <> 0
        movsb | dec ecx
    EndWhile
    mov ax '" ' | stosw | sub ecx 2
    mov esi D$DebuggeeParams
    cmp esi 0 | je L0>
    While B$esi <> 0
        On ecx = 1, jmp L0>
        movsb | dec ecx
    EndWhile
L0: mov B$edi 0

    ; Create the process with debug rights
    call 'KERNEL32.CreateProcessA' 0, CommandLineString, &NULL, &NULL, &FALSE,
                        &DEBUG_PROCESS+&DEBUG_ONLY_THIS_PROCESS+&PROCESS_TERMINATE,
                        &NULL, DebuggeePath, STARTUPINFO, PROCESS_INFORMATION
    mov D@Success eax
    If eax = &FALSE
        call ReportWinError {'Debugger: CreateProcess' 0}
        On eax = &ERROR_DIRECTORY,
            call 'User32.MessageBoxA' 0, DebuggeePath, {'Directory is:' 0}, &MB_OK
    EndIf

    ; Allocated by SetupCommandLine (dirty, but necessary if param-string is static [/S])
    VirtualFree D$CommandLinePtr

    mov eax D@Success
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Debug (polling) Thread
____________________________________________________________________________________________
____________________________________________________________________________________________

; The actual debug work is done by this thread, it creates the debuggee and waits
; for debug-events. If these occur it transfers control to the user who can decide how to
; proceed.

[IsDebugEvent: ? DebugStart: ?]
[BreakpointsEnabled: ?]
[BreakpointOp: B$ 0CC]
[ByteBuf: B$ ?]
[ContinueStatus: D$ ?]

Proc DebugThread:
    Local @Exit, @CloseSent

    call CreateDebuggeeProcess
    If eax = &FALSE
        jmp L9>>
    EndIf

    mov D$BreakpointsEnabled &TRUE
    mov D$IsDebugging &TRUE
    mov D$IsDebugEvent &FALSE
    mov B$DebugStart &FALSE
    mov B$ExceptionFlags 0

    ; Main loop, as long as we don't want to quit, wait for debug events and process them.
    mov D@Exit &FALSE, B$DebugStart &FALSE, D@CloseSent 0

    .While D@Exit = &FALSE

L0:     call 'KERNEL32.WaitForDebugEvent' DEBUG_EVENT, 100

      ; If we receive false, the timeout occurred. Process any user input that makes sense
      ; while the debuggee is running (e.g. killing it or generating breakpoints)
        ..If eax = &FALSE

          ; The user requested the termination of the debuggee while it was running. Send
          ; WM_CLOSE messages to the main windows and then wait a second. Be sure to process
          ; debug messages (do *not* call WaitForSingleObject) inbetween. If the process is
          ; still alive after a second it is terminated.
            .If D$TerminateDebuggee = &TRUE
;;
                mov D$HoldOnBreakpoints 0
                If D@CloseSent = 0
                    call CloseMainWindows
                    mov D@CloseSent 1
                    jmp L0<
                Else
                    inc D@CloseSent
                    On D@CloseSent < 10, jmp L0<
;;
                    call 'KERNEL32.TerminateProcess' D$PI.hProcess, 0
                    jmp L9>>
                ;EndIf
            .End_If

          ; Check if we either need to enable or disable the breakpoints. (HoldOnBreakpoint
          ; has changed).
            mov eax D$HoldOnBreakpoints
            .If eax <> D$BreakpointsEnabled
                If B$HoldOnBreakpoints = &TRUE
                    call EnableProcessBreakpoints
                Else
                    call DisableProcessBreakpoints
                EndIf
            .EndIf

          ; User requested Pause: Generate breakpoint to halt all threads
            If D$PauseThreads = &TRUE
                mov ebx 0, esi D$ThreadIDHandleTable
                While ebx < D$NumThreads
                    call HaltThread D$esi+ebx*8+4
                    inc ebx
                EndWhile
                mov D$PauseThreads &FALSE
            EndIf

          ; Look for BP changes in the source editor
            call ClearBPAnteroom
            jmp L0<<

        ..End_If

        mov eax D$DE.dwProcessId
        If eax <> D$PI.dwProcessId
          ; If this debug event belongs to an other (child) process, continue execution
            call 'Kernel32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId, &DBG_CONTINUE
            jmp L0<<
        End_If

        mov D$ContinueStatus &DBG_CONTINUE

        .If D$DE.dwDebugEventCode = &EXIT_PROCESS_DEBUG_EVENT
            mov D@Exit &TRUE
        .Else_If D$DE.dwDebugEventCode = &CREATE_THREAD_DEBUG_EVENT
            call Debugger_OnCreateThread
        .Else_If D$DE.dwDebugEventCode = &EXIT_THREAD_DEBUG_EVENT
            call Debugger_OnExitThread
        .Else_If D$DE.dwDebugEventCode = &LOAD_DLL_DEBUG_EVENT
            call Debugger_OnLoadDll D$LOADDLL.Base
        .Else_If D$DE.dwDebugEventCode = &UNLOAD_DLL_DEBUG_EVENT
            call Debugger_OnUnloadDll D$UNLOADDLL.Base
        .Else_If D$DE.dwDebugEventCode = &EXCEPTION_DEBUG_EVENT
            call Debugger_OnException
            mov D@Exit eax
        .Else_If D$DE.dwDebugEventCode = &CREATE_PROCESS_DEBUG_EVENT
            call Debugger_OnCreateProcess
        .Else_If D$DE.dwDebugEventCode = &OUTPUT_DEBUG_STRING_EVENT
            call HandleDebugString
        .Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
        .End_If

        call 'KERNEL32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId, D$ContinueStatus

    .End_While

L9: mov B$IsDebugging &FALSE
    call 'Kernel32.ExitThread' 0
EndP
____________________________________________________________________________________________

; When a thread is created in the debuggee. Add Thread ID/Handle pair to table.

Debugger_OnCreateThread:
    mov esi D$ThreadIDHandleTable, ecx D$NumThreads
    move D$esi+ecx*8 D$DE.dwThreadID
    move D$esi+ecx*8+4 D$CTDI.hThread
    inc D$NumThreads

    FormatString DebugLogString, {'Thread ID=%d created' 0}, D$DE.dwThreadId
    call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DebugLogString, ecx
ret
____________________________________________________________________________________________

; Overwrite Thread ID/Handle pair in table with last entry.

Debugger_OnExitThread:
    dec D$NumThreads
    mov ecx D$NumThreads, edx 0
    mov esi D$ThreadIDHandleTable, eax D$DE.dwThreadID
    While D$esi+edx*8 <> eax
        inc edx
    End_While
    move D$esi+edx*8 D$esi+ecx*8
    move D$esi+edx*8+4 D$esi+ecx*8+4

    FormatString DebugLogString, {'Thread ID=%d terminated' 0}, D$DE.dwThreadId
    call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DebugLogString, ecx
ret
____________________________________________________________________________________________

; When process is created. Add main thread ID/handle to table.

[DebugBaseOfCode: ?  DebugCodeSize: ?]

Debugger_OnCreateProcess:
    call 'KERNEL32.CloseHandle' D$CPDI.hFile

    mov D$DebugBaseOfCode 0, D$DebugCodeSize 0

    mov esi D$ThreadIDHandleTable
    move D$esi D$DE.dwThreadID, D$esi+4 D$CPDI.hThread
    inc D$NumThreads

    call ScanPEHeader D$CPDI.lpBaseOfImage

    call GetModuleName D$CPDI.lpBaseOfImage
    FormatString DebugLogString, {'%s ID=%d mapped at 0x%x' 0}, eax, D$DE.dwProcessId, D$CPDI.lpBaseOfImage
    call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DebugLogString, ecx
ret
____________________________________________________________________________________________

; When exception/breakpoint is encountered.

[DebugEventType: ? WatchedAddress: ?]
[DET_BP 1 DET_STEP 2 DET_WP 3 DET_EXCEPTION 4]

; Exception flags
[ExceptionFlags: 0]
[E_HAPPENED 1     E_OUTSIDE 2     E_MUSTEXIT 4]

[CurrentModule: ?]

Proc Debugger_OnException:
    Local @hThread, @Exit

    mov D@Exit &FALSE

    ;mov edi ExceptionTypes, eax D$E.ExceptionCode, ecx 18
    ;repne scasd | jne P9>>
    ;mov eax ExceptionStrings | mov ebx 17 | sub ebx ecx | shl ebx 2 | add eax ebx
    ;mov eax D$eax, D$BreakTitle eax

    ; Retrieve handle of the thread that has thrown the exception
    mov esi D$ThreadIDHandleTable, eax D$DE.dwThreadID
    While D$esi <> eax
        add esi 8
    End_While
    move D@hThread D$esi+4

    ;mov D$C.ContextFlags MY_CONTEXT_FULL
    ;call 'KERNEL32.GetThreadContext' D@hThread, CONTEXT

    ..If B$DebugStart = &TRUE

      ; Take note which window currently has the focus
        call 'USER32.GetForegroundWindow' | mov D$ActiveWindow eax

        mov D$C.ContextFlags MY_CONTEXT_FULL
        call 'KERNEL32.GetThreadContext' D@hThread, CONTEXT

        call IsModuleCode D$C.regEip
        If eax <> 0
            move D$CurrentModule D$eax+ME_Name
        Else
            mov D$CurrentModule 0
        EndIf

        call IsProcessCode D$C.regEip
        If eax = 1
            move D$SourcePosCodeAddress D$C.regEip
        Else
            call ScanStackForCodePointer D$C.regEsp
            dec eax
            mov D$SourcePosCodeAddress eax
        EndIf

        call ResolveSegmentAddresses D@hThread

        ; Write-back dynamic breakpoints. BPPending points at an entry in the
        ; breakpoint table-
        mov ebx D$BPPending
        If ebx <> 0
            call WriteProcessMem D$ebx, BreakpointOp, 1
            mov B$ebx+5 BP_ENABLED
            mov D$BPPending 0
        EndIf

        mov D$ContinueMode CONTINUE_RUN ; default

        .If D$E.ExceptionCode = &EXCEPTION_SINGLE_STEP
            mov D$DebugEventType DET_STEP

            call EncounterWatchPoint

            If D$RunAfterWriteBack = &FALSE
                On D$HoldOnBreakpoints = &TRUE,
                    call SignalDebugEvent        ; <<<<<<<<<<<<<<<<<<<<<<<<<<
            Else
                mov D$ContinueMode CONTINUE_RUN
                mov D$RunAfterWriteBack &FALSE
            EndIf

        .ElseIf D$E.ExceptionCode = &EXCEPTION_BREAKPOINT
            mov D$DebugEventType DET_BP

            call EncounterBreakpoint
            On D$HoldOnBreakpoints = &TRUE,
                call SignalDebugEvent        ; <<<<<<<<<<<<<<<<<<<<<<<<<<

        .Else
            mov D$DebugEventType DET_EXCEPTION

            call EncounterException
            mov D@Exit eax

            ;On D$ContinueStatus <> &DBG_EXCEPTION_NOT_HANDLED,
            ;    call SignalDebugEvent        ; <<<<<<<<<<<<<<<<<<<<<<<<<<

            ;If D$E.ExceptionFlags = 0
            ;    call NextInstructionDecode
            ;    mov eax D$InstructionLength
            ;    add D$C.regEip eax ; TODO this is a dirty hack and no real solution!!
            ;    mov D@Exit 0
            ;EndIf
        .End_If

        .If D@Exit = &FALSE
            ; For step over instructions, we overwrite the process memory after
            ; the next instruction with a breakpoint. The overwritten byte is
            ; preserved with the code address in the breakpoint table.
            If D$ContinueMode = CONTINUE_STEPOVER
                mov ebx D$C.regEip | add ebx D$InstructionLength
                call AddProcessBreakpoint ebx, BP_ONESHOT, BP_ENABLED, 0
            Else_If D$ContinueMode = CONTINUE_RETURNTOCALLER
                call ScanStackForCodePointer D$C.regEsp
                On eax <> 0,
                    call AddProcessBreakpoint eax, BP_ONESHOT, BP_ENABLED, 0
            End_If

            If D$BPPending <> 0
                On D$ContinueMode <> CONTINUE_STEP,
                    mov D$RunAfterWriteBack &TRUE
                mov D$ContinueMode CONTINUE_STEP
            EndIf

            If D$ContinueMode = CONTINUE_STEP
                or D$C.regFlag 0100 ; set trap flag
            Else
                and D$C.regFlag 0_FFFF_FEFF ; in case of Exceptions it's not auto-cleared
            EndIf

            On D$ContinueMode = CONTINUE_RUN,
                call ShowProcessWindows

            On D$TerminateDebuggee = &TRUE,
                mov D@Exit &TRUE

            ; Take care of user-activated/deactivated dynamic breakpoints.
            call ClearBPAnteroom

            call TransferWatchpoints

            mov D$C.ContextFlags MY_CONTEXT_FULL
            call 'Kernel32.SetThreadContext' D@hThread, CONTEXT

        .EndIf

    ..Else

        If D$E.ExceptionCode = &EXCEPTION_BREAKPOINT
            mov B$DebugStart &TRUE
            call ClearBPAnteroom
            call 'User32.PostMessageA' D$DebugDialogHandle, WM_BEGIN_DEBUG, 0, 0
        EndIf

    ..End_If
    mov eax D@Exit

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[LDT_ENTRY: B$ ? #8]

[LinearSegmentAddresses:
 CS.Linear: ? CS.Limit: ? DS.Linear: ? DS.Limit: ? ES.Linear: ? ES.Limit: ?
 FS.Linear: ? FS.Limit: ? GS.Linear: ? GS.Limit: ? SS.Linear: ? SS.Limit: ?]

Proc ResolveSegmentAddresses:
    Arguments @hThread

    mov esi SegRegMap, edi LinearSegmentAddresses, ebx 6

    While ebx > 0
        lodsd
        call 'KERNEL32.GetThreadSelectorEntry' D@hThread, D$eax, LDT_Entry
        If eax = &TRUE
            mov al B$LDT_ENTRY+4, ah B$LDT_ENTRY+7
            shl eax 16
            mov ax W$LDT_ENTRY+2
            stosd
            movzx eax W$LDT_ENTRY
            stosd
        EndIf
        dec ebx
    EndWhile
EndP
____________________________________________________________________________________________

; The 'ModuleList' points at a list of module information entries that are currently
; loaded and mapped into the address space of the debuggee.

; Entry in module list
[ME_Base 0          ; The base VA of the mapped module
 ME_Size 4          ; size in bytes
 ME_Name 8          ; pointer to filename
 ME_CodeBase 12     ; base VA of code section
 ME_CodeSize 16     ; size in bytes of code section
 ME_ExportBase 20   ; base RVA of export section
 ME_ExportSize 24]  ; size in bytes of export section
[SizeOf_ModuleEntry 32]

[ModuleList: ? NumModules: ? ModuleNameHeap: ?]

[new | imul eax D$Num#1s SizeOf_#1Entry | add eax D$#1List | inc D$NumModules | mov #2 eax]
[delete | push esi edi ecx | dec D$Num#1s |
    mov ecx SizeOf_#1Entry | imul esi D$Num#1s SizeOf_#1Entry | add esi D$#1List |
    mov edi #2 | rep movsb |
    pop ecx edi esi]
____________________________________________________________________________________________

; After messing with the toolhelp and psapi and having endless problems enumerating and
; getting module info I finally decided to use none of these evil APIs and read information
; needed directly from the PE image in the debuggee as soon as it is loaded. Most of the
; addresses are stored in the header. Only the module name must be read out of the export
; section or, in case of executables, from the commandline.

Proc ScanPEHeader:
    Arguments @BaseAddress
    Local @Size, @CodeBase, @CodeSize, @ExportBaseRVA, @ExportSize, @Name, @IsExe
    Uses esi, edi, ebx

        ; Read size of image, base-address and size of the code section & export table
        ; from PE image header.
        VirtualAlloc PEBuffer, 0400

            call ReadProcessMem D@BaseAddress, D$PEBuffer, 0400

            call 'KERNEL32.HeapAlloc' D$ModuleNameHeap, 0, 040
            mov D@Name eax

            mov esi D$PEBuffer
            add esi D$esi+03C ; skip DOS header
            test W$esi+016 &IMAGE_FILE_DLL ; characteristics (dll/exe)
            setz B@IsExe
            add esi 018 ; skip file header
            On W$esi <> 010B, jmp @Error ; check optional header ID

            move D@Size D$esi+038

            mov eax D$esi+014 | add eax D@BaseAddress
            mov D@CodeBase eax
            move D@CodeSize D$esi+04
            move D@ExportBaseRVA D$esi+060
            move D@ExportSize D$esi+064

        VirtualFree D$PEBuffer

        ; A PE can be a DLL or an EXE (and some other formats that RosAsm does not deal with yet).
        ; If it is an executable its name is extracted from the commandline. Otherwise (DLL) the
        ; name is expected in the export table.
        .If B@IsExe = &FALSE
            ; A dll without an export table??
            On D@ExportBaseRVA = 0, jmp @Error
            On D@ExportSize = 0, jmp @Error

            ; Load the export table.
            VirtualAlloc PEBuffer, D@ExportSize

                mov eax D@ExportBaseRVA | add eax D@BaseAddress
                call ReadProcessMem eax, D$PEBuffer, D@ExportSize

                mov esi D$PEBuffer, edi D@Name
                ; copy module name
                mov edx D$esi+0C
                sub edx D@ExportBaseRVA
                add edx esi
                Do
                    mov al B$edx
                    stosb
                    inc edx
                Loop_Until al = 0
                mov B$edi 0

            VirtualFree D$PEBuffer

        ; TODO is it possible that exe files are used as library from the actual application exe?
        ; If really possible this branch would be wrong. (ntoskrnl.exe !)
        .Else
            move D@Name D$DebuggeeExeTitle
        .EndIf

        new Module edi

        move D$edi+ME_Base D@BaseAddress
        move D$edi+ME_Size D@Size
        move D$edi+ME_Name D@Name
        move D$edi+ME_CodeBase D@CodeBase
        move D$edi+ME_CodeSize D@CodeSize
        move D$edi+ME_ExportBase D@ExportBaseRVA
        move D$edi+ME_ExportSize D@ExportSize

        mov eax D@BaseAddress, ecx D@CodeSize, edx D@CodeBase
        .If D$SavingExtension = '.DLL'
            If eax = D$LinkerDllDefault
                mov D$DebugBaseOfCode edx
                mov D$DebugCodeSize ecx
            EndIf
        .Else
            If eax = LINKERDEFAULT
                mov D$DebugBaseOfCode edx
                mov D$DebugCodeSize ecx
            EndIf
        .EndIf

        mov eax 1
        ExitP

@Error:
        VirtualFree D$PEBuffer
        mov eax 0
EndP
____________________________________________________________________________________________

Proc Debugger_OnLoadDll:
    Arguments @BaseAddress

        call 'Kernel32.CloseHandle' D$LoadDll.hFile

        call ScanPEHeader D@BaseAddress

        call GetModuleName D@BaseAddress
        FormatString DebugLogString, {'%s mapped at 0x%x' 0}, eax, D@BaseAddress
        call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DebugLogString, ecx

EndP
____________________________________________________________________________________________

Proc Debugger_OnUnloadDll:
    Arguments @BaseAddress
    Uses esi, edi

        call GetModuleName D@BaseAddress
        FormatString DebugLogString, {'%s unmapped' 0}, eax
        call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DebugLogString, ecx

        ; Search module in table
        mov edi D$ModuleList, ecx 0, eax D@BaseAddress
        While ecx < D$NumModules
            On D$edi = eax, jmp L0>
            add edi SizeOf_ModuleEntry
            inc ecx
        EndWhile
        ExitP

L0:
        ; Do cleanup work. Free the memory for the module name.
        call 'KERNEL32.HeapFree' D$ModuleNameHeap, 0, D$edi+ME_Name

        ; Overwrite the entry to delete with last entry in table.
        delete Module edi

EndP
____________________________________________________________________________________________

; Search in the modulelist for the name of the module starting at the base address passed.

GetModuleName:
    mov eax D$esp+4
    mov edx D$ModuleList, ecx D$NumModules
    While ecx > 0 ; bugfix V2.0b
        cmp eax D$edx+ME_Base | je L1>
        add edx SizeOf_ModuleEntry
        dec ecx
    EndWhile
    mov eax 0
ret 4
L1: mov eax D$edx+ME_Name
ret 4
____________________________________________________________________________________________

; Find the module from an address. Return the address of the module entry or NULL if no
; module with such a code address could be found.

Proc IsModuleCode:
    Arguments @CodeAddress
    Uses esi

        mov ecx 0, esi D$ModuleList
        While ecx < D$NumModules
            mov eax D$esi+ME_CodeBase, edx D$esi+ME_CodeSize
            .If D@CodeAddress >= eax
                add edx eax
                If D@CodeAddress < edx
                    mov eax esi
                    ExitP
                EndIf
            .EndIf
            add esi SizeOf_ModuleEntry | inc ecx
        EndWhile
        mov eax 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[DebugLogString: B$ ? #256]

; Log debug output to a file. First read debug string from process memory of the debuggee
; to a local buffer (the stack).

[DebugStringBuffer: ? DebugLogFile: ? DebugLogFileName: B$ ? #&MAXPATH]
[DebugLogName: '_dbg.log' 0]

Proc HandleDebugString:
    Local @Size

  ; Get mem on the stack for the debug string
    movzx eax W$ODS.StringLen
    Align_on 4 eax
    mov D@Size eax
    sub esp D@Size
    mov D$DebugStringBuffer esp

  ; Read the debug string
    call ReadProcessMem D$ODS.DebugString, D$DebugStringBuffer, D@Size

    call 'User32.SendMessageA' D$DebugDialogHandle, WM_LOG, D$DebugStringBuffer, D@Size

    .If D$DebugLogFile = 0

      ; Build logfile-name of the form "[Path]\[AppName]_dbg.log"
        mov esi DebuggeeExe, edi DebugLogFileName, ecx 256

      ; Copy full path+filename+extension
L0:     cmp B$esi 0 | je L1>
        movsb
        loop L0<

L1:     neg ecx | add ecx 256
        mov B$edi 0

      ; Strip extension
L0:     cmp B$edi '.' | je L1>
        dec edi
        loop L0<

L1:     neg ecx | add ecx 256
        mov esi DebugLogName

      ; append "_dbg.log"
L0:     cmp B$esi 0 | je L1>
        movsb
        loop L0<

L1:     mov B$edi 0

        call 'Kernel32.CreateFileA' DebugLogFileName, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
            &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        If eax = &INVALID_HANDLE_VALUE
            call ReportWinError {'HandleDebugString: CreateFile' 0}
            jmp L9>
        Else
            mov D$DebugLogFile eax
        EndIf

    .EndIf

    movzx eax W$ODS.StringLen | dec eax
    call 'KERNEL32.WriteFile' D$DebugLogFile, D$DebugStringBuffer, eax, BytesTransfered, 0

    mov W$esp 0A0D
    call 'KERNEL32.WriteFile' D$DebugLogFile, D$DebugStringBuffer, 2, BytesTransfered, 0

  ; Free mem from the stack
L9: add esp D@Size
EndP
____________________________________________________________________________________________

; Called when an exception in the debuggee occurs. Check if the exception is continueable.
; Returns 1 if the debuggee must be terminated, otherwise 0.

Proc EncounterException:
    Local @Exit, @Dummy

    mov D$ExceptionFlags E_HAPPENED

    call IsProcessCode D$C.regEip
    If eax = 0
        call ScanStackForCodePointer D$C.regEsp
        dec eax
        mov D$SourcePosCodeAddress eax
    EndIf

    ..If D$E.FirstChance <> 0
      ; check if exception handler is inside debuggee code
        lea eax D@Dummy | call ReadProcessMem D$FS.Linear, eax, 4
        mov ecx D@Dummy | add ecx 4 ; get handler address
        lea eax D@Dummy | call ReadProcessMem ecx, eax, 4
        call IsProcessCode D@Dummy ; handler address
        .If eax = 1
            call IsProcessCode D$C.regEip
            On eax = 0, or D$ExceptionFlags E_OUTSIDE

            call SignalDebugEvent
            If D$TerminateDebuggee = &TRUE
                mov eax 1
            Else
                mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
                mov D$ContinueMode CONTINUE_RUN
                mov eax 0
            EndIf
        .Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
            mov D$ContinueMode CONTINUE_RUN
            mov eax 0
        .EndIf
    ..Else
        or D$ExceptionFlags E_MUSTEXIT

        call SignalDebugEvent
        mov eax 1
    ..EndIf

    mov D$ExceptionFlags 0 ; was only needed for UI, clear it so we could proceed (SEH)

;;
  ; Check if Exception happened outside of application code. 
    call IsProcessCode D$C.regEip
    .If eax = 1
        
        On D$E.FirstChance = 0, 
            or D$ExceptionFlags E_MUSTEXIT
            
        call SignalDebugEvent
        
        If D$TerminateDebuggee = &TRUE
            mov eax 1
        Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
            mov D$ContinueMode CONTINUE_RUN
            mov eax 0
        EndIf
        
    .Else
    
        or D$ExceptionFlags E_OUTSIDE
      
      ; Give the module the possibility to handle the exception by its per-thread
      ; exception handler. If it does not catch the exception, show the exception
      ; dialog.
      
        If D$E.FirstChance = 0
            or D$ExceptionFlags E_MUSTEXIT            
            
            call ScanStackForCodePointer D$C.regEsp
            dec eax
            mov D$SourcePosCodeAddress eax
            
            call SignalDebugEvent
          
            mov eax 1
        Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
            mov D$ContinueMode CONTINUE_RUN
            mov eax 0
        EndIf
    
    .EndIf    
    
    mov D$ExceptionFlags 0 ; was only needed for UI, clear it so we could proceed (SEH)
                
    .If D$E.FirstChance = 0
        mov D$ExceptionFlags E_HAPPENED

        call IsProcessCode D$C.regEIP
        If eax = &FALSE
            
        EndIf

        ; Exception is continueable?
        If D$E.ExceptionFlags <> 0
            or D$ExceptionFlags E_MUSTEXIT
            mov D@Exit 1
        EndIf

        ;call SignalDebugEvent

        mov eax D@Exit
        On D$TerminateDebuggee = &TRUE, mov eax 1
    .Else
    
      ; try to pass to apps exception handler
        mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
        mov eax 0
        
    .EndIf
;;
EndP
____________________________________________________________________________________________

; Bring all visible windows of the debuggee to front.

[ActiveWindow: ?]

Proc EnumThreadWindowProc:
    Arguments @Handle, @Param

    call 'USER32.SetWindowPos' D$hwnd, D@Handle, 0, 0, 0, 0, &SWP_NOMOVE+&SWP_NOSIZE

    mov eax D@Handle
    If eax = D$ActiveWindow
        call 'USER32.SetForegroundWindow' D$ActiveWindow
    EndIf

    mov eax 1
EndP

ShowProcessWindows:
    mov ebx 0, esi D$ThreadIDHandleTable
    While ebx < D$NumThreads
        call 'User32.EnumThreadWindows' D$esi+ebx*8, EnumThreadWindowProc, 0
        inc ebx
    EndWhile
ret
____________________________________________________________________________________________

; close all non-child windows. Used when the debuggee is terminated while running.

Proc EnumCloseWindowsProc:
    Arguments @Handle, @Param

    call 'User32.PostMessageA' D@Handle, &WM_CLOSE, 0, 0
    mov eax &TRUE
EndP

CloseMainWindows:
    mov ebx 0, esi D$ThreadIDHandleTable
    While ebx < D$NumThreads
        call 'User32.EnumThreadWindows' D$esi+ebx*8, EnumCloseWindowsProc, 0
        inc ebx
    EndWhile
ret
____________________________________________________________________________________________

; STACK SCANNER
____________________________________________________________________________________________

;;
    The stack has the following structure (for each stackframe)
    
        Parameter n
        [...]
        Parameter 2
    ___ Parameter 1
        Return Address
        Saved EBP (if proc has stackframe)
        Local data
    
  * To differ between procedures we first have to find the return addresses. Then the number
    of parameters must be estimated. Given that information a complete call-stack can
    be generated.
    
  * First, all return addresses on the stack are collected bottom-up. The proc is tried to
    be identified. 
    
  * Note that life would be much easier if every proc was guaranteed to have a stack frame.
    We could just take the saved ebp as a pointer to the callers stack frame. However,
    in Assembly you are free to use ebp for whatever you like, and creating a stack frame is
    optional. Therefore the return addresses are used to identify called procedures.
    
  * Misinterpretation problems arise from "stack pollution", that is procs reserving space on 
    the stack without freeing it (e.g. sub esp 0100). Return addresses from former execution
    pollute the stack in this region. This has been partially solved by validating the stack 
    frames top-down in the third pass (ignore the locals region). The remaining problems
    are due to dynamic stack allocation (e.g. sub esp ecx). 
;;

[BufferOverrun:
"Detected buffer-overrun. 
Contact RosAsm dev team if this happens regularly." 0]

[CallStackDesc: ? CallStackEntries: ? FirstCallStackEntry: ?]
[MAX_CALLSTACK_ENTRIES 512]
[CSE_Address 0      ; points at the ret-address+4 in the LOCAL stack copy
 CSE_ProcAddress 4  ; (estimated) address of the called function
 CSE_ProcName 8     ; points at the name (+estimation info) of the funtion
 CSE_NumParams 12   ; number of paramters (after ret-address)
 CSE_NumLocals 16   ; number of locals (before ret-address)
 CSE_Rating 20      ; probability of correctness
 CSE_Flags 21       ; flags
 CSE_Next 24        ; address of next call-stack entry
 SizeOf_CSE 32]     ; align on cache line

[CSEF_HAS_STACKFRAME 01  ; CSE has a stackframe (push ebp | ...)
 CSEF_FUZZY_ADDRESS  02  ; address of proc is not exact
 CSEF_EXTERNAL       04] ; proc is outside debugged module


[ProcNameHeap: ?]

Proc GenerateCallStack:
    Local @Pointer, @CurrentSize, @ProcAddr, @LastCodeAddr, @Exact, @NumLocals

    call DestroyCallStack

    On D$ProcNameHeap = 0, call 'KERNEL32.HeapCreate' 0, 01000, 0
    mov D$ProcNameHeap eax

    call ReadApplicationStack D$C.regEsp
    On eax = &FALSE, ExitP
    On D$CallStackDesc = 0, VirtualAlloc CallStackDesc, 04000

    call CallStack.Pass1
    If D$CallStackEntries = 0
        mov eax 0
        ExitP
    EndIf
    call CallStack.Pass2
    call CallStack.Pass3
;;
    mov esi D$StackBuffer, edi D$CallStackDesc, D$CallStackEntries 0
    mov D$NextStackFrame 0, D@NumLocals 0
    move D@CurrentSize D$StackSize
    move D@LastCodeAddr D$C.regEip
    .While D@CurrentSize > 0
        lodsd | mov D@Pointer eax

        call IsReturnAddress eax, edi
        .If eax = &TRUE

            mov D$edi+CSE_Address esi

            call 'Kernel32.HeapAlloc' D$ProcNameHeap, 0, 040
            mov D$edi+CSE_ProcName eax

            ; Check if the exact address of the procedure called could be estimated.
            ; If not (call reg / call mem / ...) use the code address which is
            ; somewhere in the proc to find the procedure address and label.
            mov eax D$edi+CSE_ProcAddress
            If eax <> 0
                mov D@ProcAddr eax
                mov D@Exact &TRUE
            Else
                move D@ProcAddr D@LastCodeAddr
                mov D@Exact &FALSE
            EndIf
            call IsProcessCode D@ProcAddr
            If eax = &TRUE
                call ScanLabelListForCodeLabel D@ProcAddr, D@Exact
            Else
                call ScanExportTableForCodeLabel D@ProcAddr, D@Exact
            EndIf

            mov D$edi+CSE_ProcAddress eax

            push esi edi
                mov esi LabelName, edi D$edi+CSE_ProcName
                Do | movsb | Loop_Until B$esi-1 = 0
            pop edi esi

            call CountParametersAndScanStackFrame edi

            ; Local data - Begin to count local data from the last return address.
            ; Therefore NumLocals start with a negative number if parameters were
            ; passed to the last function.
            mov eax D@NumLocals
            On eax l 0, mov eax 0
            mov D$edi+CSE_NumLocals eax

            mov eax D$edi+CSE_NumParams | neg eax
            mov D@NumLocals eax

            move D@LastCodeAddr D@Pointer
            add edi SizeOf_CSE
            inc D$CallStackEntries
        .Else
            inc D@NumLocals
        .EndIf
        sub D@CurrentSize 4
    .EndWhile
;;
    mov eax &TRUE
EndP

____________________________________________________________________________________________

; First pass - search all possible return addresses

Proc Callstack.Pass1:
    Local @CurrentSize, @LastCodeAddr, @Pointer, @ProcAddr, @Exact
    Uses esi, edi

    mov esi D$StackBuffer, edi D$CallStackDesc, D$CallStackEntries 0
    move D@CurrentSize D$StackSize
    move D@LastCodeAddr D$C.regEip
    .While D@CurrentSize > 0
        lodsd | mov D@Pointer eax

        call IsReturnAddress eax, edi
        .If eax = &TRUE
            mov D$edi+CSE_Address esi

            call 'Kernel32.HeapAlloc' D$ProcNameHeap, 0, 040
            mov D$edi+CSE_ProcName eax

          ; Check if the exact address of the procedure called could be estimated.
          ; If not (call reg / ...) use the code address which is
          ; somewhere in the proc to find the procedure address and label.
            mov B$edi+CSE_Flags 0
            mov eax D$edi+CSE_ProcAddress
            If eax <> 0
                mov D@ProcAddr eax
                mov D@Exact &TRUE
            Else
                move D@ProcAddr D@LastCodeAddr
                or B$edi+CSE_Flags CSEF_FUZZY_ADDRESS
                mov D@Exact &FALSE
            EndIf
            call IsProcessCode D@ProcAddr
            If eax = &TRUE
                call ScanLabelListForCodeLabel D@ProcAddr, D@Exact
            Else
                call ScanExportTableForCodeLabel D@ProcAddr, D@Exact
                or B$edi+CSE_Flags CSEF_EXTERNAL
            EndIf

            mov D$edi+CSE_ProcAddress eax

            push esi edi
                mov esi LabelName, edi D$edi+CSE_ProcName
                Do | movsb | Loop_Until B$esi-1 = 0
            pop edi esi

            move D@LastCodeAddr D@Pointer
            add edi SizeOf_CSE
            inc D$CallStackEntries

          ; Prevent buffer overrun
            If D$CallStackEntries >= MAX_CALLSTACK_ENTRIES
                call 'User32.MessageBoxA' D$DebugDialogHandle, BufferOverrun, {'Callstack generation' 0}, &MB_OK+&MB_ICONEXCLAMATION
                mov eax &FALSE
                ExitP
            EndIf
        .EndIf
        sub D@CurrentSize 4
    .EndWhile
    mov eax &TRUE
EndP
____________________________________________________________________________________________

; Second pass - scan stack frames and rate procs

[CSRatingFailed: ?] ; indicate that rating wasn't successful -> don't rely on the ratings

Proc CallStack.Pass2:
    Uses esi, edi, ebx

    mov esi D$StackBuffer, edi D$CallStackDesc, ecx D$CallStackEntries
    mov edx esi | sub edx D$C.regEsp

    mov ebx D$C.regEbp | add ebx edx

L0:     lea eax D$ebx+8
        While eax <> D$edi
            add edi SizeOf_CSE
            dec ecx | jz L9>
        EndWhile
        inc B$edi+CSE_Rating

      ; Check for invalid stackframe ptrs - a stackframe ptr is invalid if it points
      ; to somewhere outside the used stack.
        If ebx < esi
            mov D$CSRatingFailed 1
            ExitP
        EndIf
        mov eax esi | add eax D$StackSize
        If ebx >= eax
            mov D$CSRatingFailed 1
            ExitP
        EndIf

        mov ebx D$ebx | add ebx edx ; next stackframe ptr
    jmp L0<

L9: mov D$CSRatingFailed 0
EndP
____________________________________________________________________________________________

; Third pass - validate, count params, locals, ...

Proc CallStack.Pass3:
    Local @LowerBound, @UpperBound, @LastCSE
    Uses esi, edi, ebx

  ; Traverse the callstack top-down, begin with the last entry
    mov ebx D$CallStackEntries | dec ebx
    imul edi ebx SizeOf_CSE | add edi D$CallStackDesc
    mov esi D$edi+CSE_address ; esi -> params of the root function

    mov eax D$StackBuffer | mov D@LowerBound eax
    add eax D$StackSize   | mov D@UpperBound eax

    mov D@LastCSE 0

    .While ebx ge 0

      ; UpperBound - address of last CSE
      ; LowerBound - limit given through esp

      ; The bounds are stored to detect collisions of CSEs. Collision (or overlaps)
      ; occur if some interpretation mechanism has failed (locals, params) or if the
      ; whole CSE is a ghost entry (old ret addresses in non-overwritten locals)

        mov eax D$edi+CSE_Address
        .If D@UpperBound < eax
            mov edx D@LastCSE | mov al B$edx+CSE_Rating
            If al g B$edi+CSE_Rating
              ; overlapped by local data of a higher rated proc, considered
              ; as stack pollution -> throw away
                jmp L8>
            ElseIf al = B$edi+CSE_Rating
              ; overlapped by local data of equally rated proc, the assumption
              ; about the size of local data of the last CSE might be wrong, correct it (set to zero)
                mov D$edx+CSE_NumLocals 0
                ;mov eax D$edi+CSE_Address | sub eax D@Upperbound | shr eax 2
                ;sub D$edx+CSE_NumLocals eax
                move D@UpperBound D$edi+CSE_Address
            Else
              ; overlapped by local data of lower rated proc (the interpretation failed)
              ; throw away the last(!) CSE
                mov eax D$edx+CSE_Next | mov D@LastCSE eax
            EndIf
        .EndIf

        call CountParametersAndScanStackFrame edi

      ; Check parameters size - the parameter sizes should be ok in most cases.
      ; however, if some ill "ret 086D" statement is somewhere in the source (e.g. MyPNN)
      ; the boundaries are exceeded easily. If this occurs in other apps too, better merge
      ; the validation of param and local sizes and do something more intelligent ...
        mov eax D$edi+CSE_NumParams | shl eax 2
        add eax D$edi+CSE_Address
        If D@UpperBound < eax
          ; MyPNN phenomenon, consider cleaner way to deal with this issue
            mov D$edi+CSE_NumParams 0
        EndIf


        If D$CSRatingFailed = 0
          ; proc with stackframes should be rated if rating was successful
            test B$edi+CSE_Flags CSEF_HAS_STACKFRAME | jz L0>
                cmp B$edi+CSE_Rating 1 | jl L8>
L0:     EndIf

      ; recompute upper bound
        mov eax D$edi+CSE_NumLocals | inc eax | shl eax 2 ; SizeOf(Locals+RetAddress)
        mov edx D$edi+CSE_Address | sub edx eax
        mov D@UpperBound edx

      ; if we just entered the proc the stack might not be fully filled - either we
      ; must handle it or the one who displays it. Otherwise we'd read below the stackptr.
        If edx < D@LowerBound
            sub edx D@Lowerbound ; edx is negative!
            neg edx | shr edx 2
            sub D$edi+CSE_NumLocals edx ; subtract from localnumber
        EndIf

        move D$edi+CSE_Next D@LastCSE
        mov D@LastCSE edi

L8:     sub edi SizeOf_CSE
        dec ebx | js L9>
    .EndWhile

L9: move D$FirstCallStackEntry D@LastCSE

EndP
____________________________________________________________________________________________


____________________________________________________________________________________________

Proc DestroyCallStack:
    mov D$StackSize 0
    VirtualFree D$StackBuffer
    mov D$CallStackEntries 0, D$FirstCallStackEntry 0
    VirtualFree D$CallStackDesc
    If D$ProcNameHeap <> 0
        call 'KERNEL32.HeapDestroy' D$ProcNameHeap
        mov D$ProcNameHeap 0
    EndIf
EndP
____________________________________________________________________________________________

; Copy the complete stack from the address space of the debuggee into our address space.

[StackBuffer: ? StackSize: ?]

Proc ReadApplicationStack:
    Arguments @StackPointer

        ; Get size of current stack
        mov eax D@StackPointer
        On eax = 0, ExitP
        and eax 0_FFFF_FFFC ; dword align
        mov D@StackPointer eax
        call IsProcessMemory D@StackPointer
        On eax = 0, ExitP
        mov D$StackSize eax
        ; Copy stack, starting at the stackpointer
        VirtualAlloc StackBuffer, eax
        call ReadProcessMem D@StackPointer, D$StackBuffer, D$StackSize
EndP
____________________________________________________________________________________________

; Check if the given value is an return address.
; Returns:
;   eax - TRUE / FALSE

; NextStackFrame is the so called ghost-return-address prevention. In external modules
; all procs are assumed to have a stackframe (which is not true). If those procs use the
; stack for local data (sub esp imm) but do not clear/use it, old return addresses might
; still be there (this happens _very_ often).
[NextStackFrame: ?]

Proc IsReturnAddress:
    Arguments @Address, @CSE

    mov eax D@CSE
    mov D$eax+CSE_ProcAddress 0

    call IsProcessCode D@Address
    ..If eax = 1
        call IsReturnAddressInSource D@Address, D@CSE
    ..Else
        mov eax 0
        call IsModuleCode D@Address
        .If eax <> 0
            If edi > D$NextStackFrame
                call IsReturnAddressInModule D@Address, D@CSE, eax
            Else
                mov eax 0
            EndIf
        .EndIf
    ..EndIf
EndP
____________________________________________________________________________________________

; Determine if the given address is a return address which was pushed on the stack by a
; call. We look into process memory if the instruction preceding the address is call.
; There are some different encodings of calls:
;   call Label      >> E8 ## ## ## ##
;   call D$Label    >> FF 15 ## ## ## ##
;   call reg32      >> FF D#
;   call D$reg32    >> FF 1# (eax/ecx/edx/ebx/edi/esi)
;   call D$esp      >> FF 14 24
;   call D$ebp      >> FF 55 00
; And with address arithmetic there are more variants. So we first check for the standard
; call opcode E8. If it is different we scan backward for the opcode FF and try a decode.
; If the decoded instruction is a call and equals the number of bytes searched back
; IsReturnAddress returns 1 otherwise 0 (in eax).

[PrecedingCode: B$ ? #96 PrecedingCodeSize: D$ ?]

Proc IsReturnAddressInModule:
    Arguments @Address, @CSE, @ModuleBase
    Uses ebx, edi

        mov eax D@ModuleBase, ebx D@Address, ecx ebx
        sub ecx D$eax+ME_CodeBase
        If ecx > 8
            mov ecx 8
        ElseIf ecx < 2
            ; the minimum length of a call is two bytes (FF ##)
            jmp L9>>
        Else
            mov D$PrecedingCode 0, D$PrecedingCode+4 0
        EndIf
        mov D$PrecedingCodeSize ecx | sub ebx ecx
        call ReadProcessMem ebx, PrecedingCode, ecx

        mov edi PrecedingCode, ecx D$PrecedingCodeSize
        If B$edi+ecx-5 = 0E8 ; call imm
            mov eax &TRUE
            ; convert the immediate to a virtual address
            mov edx D$edi+ecx-4
            add edx D@Address
            mov ecx D@CSE
            mov D$ecx+CSE_ProcAddress edx
            ExitP
        ElseIf W$edi+ecx-6 = 015FF ; call D$Label
            mov eax D$edi+ecx-4
            call ReadProcessMem eax, PrecedingCode, 4
            mov edx D$PrecedingCode
            mov ecx D@CSE
            mov D$ecx+CSE_ProcAddress edx
            mov eax &TRUE
            ExitP
        EndIf
L1:     mov al 0FF
            repne scasb | jne L9>
            dec edi

            call InstructionDecode edi
            mov eax D$NextInstructionPtr
            .If D$eax = 'call'
                inc ecx
                If ecx = D$InstructionLength
                    mov eax &TRUE
                    ExitP
                EndIf
                dec ecx
            .EndIf
            inc edi
        jmp L1<

L9:     mov eax &FALSE
EndP
____________________________________________________________________________________________

; Simplified version of IsReturnAddress. Here we use the IpTable to find the preceding
; instruction. This is much safer as no data can be mistaken as an opcode.

Proc IsReturnAddressInSource:
    Arguments @Address, @CSE
    Uses ebx, edi, esi

        ; >>> TODO
        ; Most mistaken return addresses are indeed function addresses which are pushed
        ; on the stack (callback functions, window procs). We should take care of the
        ; following case:
        ; Main:
        ; [...]
        ; call 'kernel32.ExitProcess'
        ; Proc MainWindowProc:
        ; [...]
        ; EndP
        ; The address of the window proc is saved on the stack multiple times, therefore
        ; messing up the callstack because it is preceded by call and mistaken as a return
        ; address.
        ; <<< TODO

        mov eax D@Address

      ; Search for return address in the instruction table.
        sub eax D$DebugBaseOfCode
        mov edi D$IpTable, ecx D$IpTablePtr
        sub ecx edi
        shr ecx 2
            jz L9>> ; if IpTable is empty/freed (which should never happen)
        While eax >= D$edi
            dec ecx | jz L9>>
            add edi 4
        EndWhile

      ; Search back for the preceding instruction. We can't just use [edi-8] because
      ; also Labels are recorded in the IpTable.
        sub edi 4
        Do
            On edi < D$IpTable, jmp L9>>
            mov edx D$edi
            sub edi 4
        Loop_Until edx <> eax
        mov ebx eax | sub ebx edx ; statements length
        add edx D$DebugBaseOfCode
        call ReadProcessMem edx, PrecedingCode, ebx

      ; The statement might contain several instruction (call-macro!), so decode
      ; every instruction until we find the last one of that statement.
        mov esi PrecedingCode
        Do
            call InstructionDecode esi
            mov eax D$InstructionLength
            add esi eax
            sub ebx eax
                js L9> ; how is this possible ? it happens... [Bugfix V2.0]
        Loop_Until ebx = 0

        mov eax D$NextInstructionPtr
        .If D$eax = 'call'
            mov eax 1, edx 0
            sub esi D$InstructionLength
            If B$esi = 0E8
                mov edx D$esi+1
                add edx D@Address
                mov ecx D@CSE
                mov D$ecx+CSE_ProcAddress edx
            ElseIf W$esi = 015FF
                mov edx D$esi+2
                call ReadProcessMem edx, PrecedingCode, 4
                mov eax D$PrecedingCode
                mov ecx D@CSE
                mov D$ecx+CSE_ProcAddress eax
                mov eax 1
            EndIf
            ExitP
        .EndIf

L9:     mov eax 0
EndP
____________________________________________________________________________________________

; Lookup the address of the code label after which the given code address follows IOW
; search the procedure which contains the code at the given address.

Proc GetNearestProc:
    Arguments @Address
    Local @NearestProc
    Uses esi, edi, ebx

    mov D@NearestProc 0
    mov edi D$PlainLabelList, ebx D$EndOfPlainLabelList, edx D@Address
    mov ecx D$edi
    add edi 5 | sub ecx 5

    While edi < ebx

        ; address of string
        mov esi edi
        mov al EOI
        repne scasb | jne L9>
        mov eax D$edi | add eax D$CodeAjust
        If eax <= edx
            On eax > D@NearestProc, mov D@NearestProc eax
        EndIf
        add edi 6 | sub ecx 6

    EndWhile

L9: mov eax D@NearestProc
EndP
____________________________________________________________________________________________

[LabelName: B$ ? #128]

CopyStringFromLabelList:
    push esi, edi

        mov edi LabelName
        While B$esi <> EOI
            movsb
        EndWhile
        mov B$edi 0

    pop edi, esi
ret
____________________________________________________________________________________________

; Lookup the label of the procedure starting at the given address.

Proc ScanLabelListForCodeLabel:
    Arguments @Address, @Exact
    Local @NearestProc @NearestProcName
    Uses esi, edi, ebx

    mov edi D$PlainLabelList, ebx D$EndOfPlainLabelList, edx D@Address
    mov ecx D$edi
    add edi 5 | sub ecx 5

    .If D@Exact = &TRUE

        While edi < ebx

            ; address of string
            mov esi edi
            mov al EOI
            repne scasb | jne L9>>
            test B$edi+4 DataLabelFlag | jnz L0>
            mov eax D$edi | add eax D$CodeAjust
            If eax = edx
                call CopyStringFromLabelList
                ExitP
            EndIf
L0:         add edi 6 | sub ecx 6

        EndWhile

    .Else

        mov D@NearestProc 0, D@NearestProcName 0
        While edi < ebx

            ; address of string
            mov esi edi
            mov al EOI
            repne scasb | jne L9>
            mov cl B$edi+4
            test cl DataLabelFlag | jnz L0>
            mov eax D$edi | add eax D$CodeAjust
            If eax <= edx
                On eax > D@NearestProc, mov D@NearestProc eax, D@NearestProcName esi
            EndIf
L0:         add edi 6 | sub ecx 6

        EndWhile

        mov esi D@NearestProcName
        On esi <> 0, call CopyStringFromLabelList
        mov eax D@NearestProc
        On eax <> 0, ExitP

    .EndIf

L9: mov eax D@Address
    call IntToHexString
    move D$LabelName D$HexString, D$LabelName+4, D$HexString+4
    mov B$LabelName+8 0
    mov eax D@Address
EndP
____________________________________________________________________________________________

; Lookup the label of the procedure starting at the given address. The export table of
; the module which is referenced by the code address is scanned.

[PEBuffer: ?]

Proc ScanExportTableForCodeLabel:
    Arguments @CodeAddress, @Exact
    Local @BaseAddress, @ExportBaseRVA, @ExportSize, @NumNames, @ProcAddressRVA, @AddressTableEntry
    Uses ebx, esi, edi

    call IsModuleCode D@CodeAddress
    If eax = 0
        ExitP
    EndIf
    move D@BaseAddress D$eax+ME_Base
    move D@ExportBaseRVA D$eax+ME_ExportBase
    move D@ExportSize D$eax+ME_ExportSize

    On D@ExportBaseRVA = 0, ExitP
    On D@ExportSize = 0, ExitP

    ; Load the export table.
    VirtualAlloc PEBuffer, D@ExportSize

        mov eax D@ExportBaseRVA | add eax D@BaseAddress
        call ReadProcessMem eax, D$PEBuffer, D@ExportSize

        mov esi D$PEBuffer, edi LabelName
        ; address conversion: RVA -> Linear address in copied export-table
        mov eax esi
        sub eax D@ExportBaseRVA
        mov D$ExportTableAdjust eax
        ; copy module name
        mov edx D$esi+0C
        add edx D$ExportTableAdjust
        Do
            mov al B$edx
            If al = 0 ; no library name?
                mov eax '???.'
                stosd
                jmp L0>
            EndIf
            stosb
            inc edx
        Loop_Until al = '.'
        ; number of exported functions
L0:     mov ecx D$esi+014
        move D@NumNames D$esi+018
        ; Convert address table RVA to pointer in local buffer
        mov eax D$esi+01C
        add eax D$ExportTableAdjust
        ; Convert code address to RVA
        mov ebx D@CodeAddress
        sub ebx D@BaseAddress

        ; ebx = Code RVA
        ; eax -> Address table (list of function RVA's)
        ; ecx = Number of functions

        mov D@ProcAddressRVA 0
        .If D@Exact = &TRUE

            mov D@ProcAddressRVA ebx
            While ecx > 0
                If D$eax = ebx
                    call SearchNameInExportTable
                    cmp edx 0 | je L8> ; function has no name (comctl32.dll !)
                    sub edx D@ExportBaseRVA
                    cmp edx D@ExportSize | ja L8> ; ignore invalid function name pointers (does happen!)
                    Do
                        mov al B$esi+edx
                        stosb
                        inc edx
                    Loop_until al = 0
                    jmp L9>>
                EndIf
                add eax 4
                dec ecx
            EndWhile
L8:         DwordToHex D@CodeAddress
            mov B$edi 0

        .Else

            While ecx > 0
                If D$eax <= ebx
                    mov edx D$eax
                    On edx > D@ProcAddressRVA,
                        mov D@ProcAddressRVA edx, D@AddressTableEntry eax
                EndIf
                add eax 4
                dec ecx
            EndWhile
            mov eax D@AddressTableEntry
            call SearchNameInExportTable
            If edx <> 0
                sub edx D@ExportBaseRVA
                cmp edx D@ExportSize | ja L8> ; ignore invalid function name pointers (does happen!)
                Do
                    mov al B$esi+edx
                    stosb
                    inc edx
                Loop_until al = 0
                dec edi
                mov D$edi ' (< ' | add edi 3
                mov edx ebx | sub edx D@ProcAddressRVA
                cmp edx 0_FFFF | ja L8> ; function bigger as 64k ??
                mov ecx 2 | call IntToHex
                mov W$edi ')'
            Else
L8:             mov D@ProcAddressRVA ebx
                mov al '<' | stosb
                DwordToHex D@CodeAddress
                mov B$edi 0
            EndIf
        .EndIf

L9: VirtualFree D$PEBuffer

    mov eax D@ProcAddressRVA
    add eax D@BaseAddress
EndP

[ExportTableAdjust: ?]

; Input
;   esi -> copied Export section
;   eax -> Address table entry
;   ecx = Number of functions
; Output
;   edx = Name RVA  /  zero (no name in export table -ordinal only-)

SearchNameInExportTable:
    mov edx 0
    push eax ecx edi
        sub eax D$ExportTableAdjust ; convert back to RVA
        sub eax D$esi+01C | shr eax 2 ; eax = index of current function
        mov ecx D$esi+018 ; ecx = number of names
        mov edi D$esi+024 | add edi D$ExportTableAdjust ; ordinal table
        repne scasw ; search the ordinal table
            jne L9>
        sub edi D$ExportTableAdjust
        sub edi D$esi+024 | shl edi 1
        add edi D$esi+020
        add edi D$ExportTableAdjust
        mov edx D$edi-4
L9: pop edi ecx eax
ret
____________________________________________________________________________________________

; Try to find out how many parameters are passed to the procedure. If a stackframe is
; found (entry sequence 55 8B EC), the code is scanned for the exit sequence [8B E5]5D | C9.
;
; After that the ret instruction tells how many bytes are removed from the stack, which
; is the number of params multiplied by 4:
;   ret       >> C3 (no params)
;   ret ####  >> C2 ## ##
; If there is a naked ret (C3) then either there are no parameters, or the params get
; removed by the caller (C_Call). Check the code at the return address for:
;   add esp imm >> 83 C4 ##
;
; If no stackframe is found, the assumption is made that no parameters were passed
; on the stack (which may not be true for some asm hardcore code-styles).

[ProcBuffer: ?]

[GUARD_BYTES 4] ; protect from dissassembler reading data below the ip ('op00')

Proc CountParametersAndScanStackFrame:
    Arguments @CSE
    Local @Procedure, @RetAddress, @Result, @Size, @NumLocals
    Uses edi, esi, ebx

    VirtualAlloc ProcBuffer, 01000

        mov D@Result 0

        mov eax D@CSE
        mov D$eax+CSE_NumParams 0
        move D@Procedure D$eax+CSE_ProcAddress
        mov eax D$eax+CSE_Address
        move D@RetAddress D$eax-4

        call IsProcessMemory D@Procedure
        On eax > 01000-GUARD_BYTES, mov eax 01000-GUARD_BYTES
        mov D@Size eax

        mov esi D$ProcBuffer | add esi GUARD_BYTES
        call ReadProcessMem D@Procedure, esi, D@Size
        On eax = &FALSE, jmp @Exit

        mov ecx D@Size
        sub ecx 16 ; guard bytes to prevent from disassembly overflow

      ; Stack frame?
        cmp B$esi 0C8   | je @WithStackFrame ; enter x x
        cmp W$esi 08B55 | jne @WithOutStackFrame
        cmp B$esi+2 0EC | jne @WithOutStackFrame
;;
    the proc-entry has the following structure:
    [in most API calls local and seh section exchanged]
    
    stackframe      push ebp        55
                    mov ebp esp     8B EC
                    
    locals          sub esp imm8/32 83/81 EC im
    
    SEH            (push imm32)     68 imm32 / 6A imm8
                   (push imm32)
                    push imm32
                    mov eax D$fs:0  64 A1 00 00 00 00
                    push eax        50
                    mov D$fs:0 esp  64 89 25 00 00 00 00                 
                   (push ecx)
                   (push ecx)
                   
    save regs       push ebx
                    ...
    
;;

@WithStackFrame:
        mov eax D@CSE | or B$eax+CSE_Flags CSEF_HAS_STACKFRAME

      ; Scan proc entry - size of local data in EBX. Local data is
      ; reserved with "sub esp xx" but also everything that is pushed
      ; _before_ the sub is considered as local data (SEH in Win32 code).

        mov ebx 4, edx 0 ; ebx = sizeof locals, edx = sizeof pushed data
        If B$esi = 0C8 ; enter x x
            add bx W$esi+1
        EndIf

        add esi 3 | sub ecx 3

L0:     call InstructionDecode esi
            If D$eax = 'push'
                add edx 4
            ElseIf D$eax = 'mov '
                ;nop
            ElseIf W$esi = 0EC83 ; sub esp b
                movzx eax B$esi+2
                add ebx eax
                add ebx edx
            ElseIf W$esi = 0EC81 ; sub esp dw
                mov eax D$esi+2
                add ebx eax
                add ebx edx
            Else
                jmp L1>
            EndIf
            mov eax D$InstructionLength
            sub ecx eax | js @Exit
            add esi eax
        jmp L0<

      ; Search exit sequence - might be 'pop ebp' / 'leave'
L1:     dec ecx | jz @Exit ; we need at min 2 bytes left (leave + ret)
            lodsb
            cmp al 05D | je L2> ; pop ebp
            cmp al 0C9 | je L2> ; leave
        jmp L1<

        ; mov esp ebp is not really necessary...
        ;cmp W$esi-3 0E58B | jne L1<

L2:     If B$esi = 0C3 ; ret
            ; C_Call ?
            call ReadProcessMem D@RetAddress, D$ProcBuffer, 4
            mov esi D$ProcBuffer
            cmp W$esi 0C483 | jne @Exit
            movzx eax B$esi+2
            shr eax 2
            mov D@Result eax
            jmp @Exit
        ElseIf B$esi = 0C2 ; ret imm
            movzx eax W$esi+1
            shr eax 2
            mov D@Result eax
            jmp @Exit
        Else ; no ret, maybe data mistaken as exit sequence?
            mov edx 0DEADC0DE ; debug (I-was-here) marker
            jmp L1<
        EndIf
        jmp @Exit

@WithOutStackFrame:
        ; No stack-frame, decode every instruction to find the ret.

L1:     ; Do not scan behind the end of code, may happen if there is a function
        ; without ret (e.g. Main: [...] call 'Kernel32.ExitProcess')

        mov ebx 0
        On D$esi = 0, jmp @Exit
        call InstructionDecode esi
        .If W$eax = 're' ; must be re(t)
            If B$esi = 0C3 ; ret
                ; C_Call ?
                call ReadProcessMem D@RetAddress, D$ProcBuffer, 4
                mov esi D$ProcBuffer
                cmp W$esi 0C483 | jne @Exit
                movzx eax B$esi+2
                shr eax 2
                mov D@Result eax
                jmp @Exit
            ElseIf B$esi = 0C2 ; ret imm
                movzx eax W$esi+1
                shr eax 2
                mov D@Result eax
                jmp @Exit
            Else ; ???
                jmp @Exit
            EndIf
        .EndIf
        add esi D$InstructionLength
        sub ecx D$InstructionLength | jns @WithOutStackFrame
        jmp @Exit

@Exit:
    VirtualFree D$ProcBuffer

    mov eax D@CSE
    move D$eax+CSE_NumParams D@Result
    shr ebx 2
    move D$eax+CSE_NumLocals ebx
EndP
____________________________________________________________________________________________

[StackFragment: B$ ? #32]

Proc ScanStackForCodePointer:
    Arguments @StackPointer
    Local @StackSize, @Pointer, @Offset
    Uses ebx esi edi

        ; Get size of current stack
        mov edi D@StackPointer
        call IsProcessMemory edi
        mov D@StackSize eax
        mov ebx 32
        mov D@Offset 0

        ; Read stack in 32byte fragments
        While D@StackSize >s 0
            On D@StackSize < ebx,
                mov ebx D@StackSize
            call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, edi, StackFragment, ebx, &NULL
            sub D@StackSize ebx
            add edi ebx
            mov ecx ebx
            shr ecx 2 | jecxz L2> ; Bug fix Betov.
            mov esi StackFragment
L1:         lodsd
                mov D@Pointer eax
                ;call IsProcessCode eax
                push ecx ; bugfix V2.0b
                    call CheckReturnAddress eax
                pop ecx
                cmp eax &TRUE | je L2>
                add D@Offset 4
            loop L1<
        End_While
        mov D@Pointer 0

L2:     mov eax D@Pointer
        mov edx D@Offset
EndP

Proc CheckReturnAddress:
    Arguments @Address
    Local @Result
    Structure @Dummy SizeOf_CSE

    call IsProcessCode D@Address
    If eax = 1
        pushad
            call IsReturnAddressInSource D@Address, D@Dummy
            mov D@Result eax
        popad
        mov eax D@Result
    EndIf
EndP
____________________________________________________________________________________________

; Nothing works for fully close the Debuggee. Something remain attached:

CloseProcess:
    call 'KERNEL32.GetExitCodeProcess' D$PI.hProcess, ExitCode
    call 'KERNEL32.TerminateProcess'  D$PI.hProcess, D$ExitCode

    call 'KERNEL32.CloseHandle' D$PI.hThread       ; should be of
    call 'KERNEL32.CloseHandle' D$PI.hProcess      ; no use.
ret
____________________________________________________________________________________________

; TODO
;  * write back real opcodes (into copy!) when reading code sections with dynamic breakpoints inside
;  * reading beyond 2GB in Win9x ? if yes, just return ptr as this section is global

Proc ReadProcessMem:
    Arguments @Source, @Dest, @Size
    Uses ebx esi

    call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D@Source, D@Dest, D@Size, &NULL
    On eax = 0, ExitP

    ; Iterate through breakpoint-table, write back real opcodes
    mov esi D$BPTable, ecx D$NumBreakpoints
L0: While ecx > 0
        mov eax D$esi
        .If eax >= D@Source
            sub eax D@Source ; eax = offset into buffer
            If eax < D@Size
                mov edx D@Dest
                mov bl B$esi+7
                mov B$edx+eax bl ; restore original byte
            EndIf
        .EndIf
        add esi 8
        dec ecx
    EndWhile

    mov eax 1
EndP
____________________________________________________________________________________________

Proc WriteProcessMem:
    Arguments @Dest @Source @Size

    call 'KERNEL32.WriteProcessMemory' D$PI.hProcess, D@Dest, D@Source, D@Size, &NULL
    If eax = &TRUE
      ; should not be needed on x86
        call 'KERNEL32.FlushInstructionCache' D$PI.hProcess, D@Dest, D@Size
    EndIf
EndP
____________________________________________________________________________________________

[ExitCode: ?]

[NumberOfBytesRead: ?]


;;
SetCodeRVA:
    move D$DebugBaseOfCode D$CPDI.lpBaseOfImage

    VirtualAlloc DebugHeaderImage 0400 | mov D$NumberOfBytesRead 0

    call 'KERNEL32.ReadProcessMemory' D$CPDI.hProcess, D$DebugBaseOfCode,
                                     D$DebugHeaderImage, 0400,  NumberOfBytesRead
    mov edi D$DebugHeaderImage, al '.', ecx 0400
L0: repne scasb | jne L9>
       cmp D$edi 'text' | jne L0<
           dec edi
           mov eax D$edi+0C
           add D$DebugBaseOfCode eax
           move D$DebugCodeSize D$edi+8

L9: VirtualFree D$DebugHeaderImage
ret
;;
____________________________________________________________________________________________

SignalDebugEvent:
    mov B$IsDebugEvent &TRUE
    call 'User32.PostMessageA' D$DebugDialogHandle, WM_DEBUGEVENT, 0, 0
    call 'Kernel32.WaitForSingleObject' D$UserInputEvent, &INFINITE
    mov B$IsDebugEvent &FALSE
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

WATCHPOINTS:

; Watchpoints mean everything in RosAsm that uses the debug registers (DR0-DR7).
____________________________________________________________________________________________

; Mutex to control access to shared WP data structures of debug thread and UI thread.
; To prevent deadlocks, do not make API calls while holding the mutex.

[WPSynchMutex: ?]

; Maps to the debug registers DR0, DR1, DR2, DR3, DR7. All r/w access must be protected
; by WPSynchMutex. These values are written by the UI thread and read by the debug thread.
; Exception is WPChanged which is set to 1 by UI and zeroed by the debugger.

[WPSlot0: ?     ; DR0
 WPSlot1: ?     ; DR1
 WPSlot2: ?     ; DR2
 WPSlot3: ?     ; DR3
 WPControl: ?   ; DR7
 WPChanged: ?]  ; Signal changed data
____________________________________________________________________________________________

; Init data and mutex - called by resource manager thread.

InitWatchpointResources:
    mov eax 0
    mov D$WPSlot0 eax
    mov D$WPSlot1 eax
    mov D$WPSlot2 eax
    mov D$WPSlot3 eax
    mov D$WPControl eax
    mov D$WPChanged eax
    call 'Kernel32.CreateMutexA' 0, 0, 0
    mov D$WPSynchMutex eax
    If eax = 0
        call ReportWinError {'CreateWPMutex' 0}
        mov eax 0
    EndIf
ret
____________________________________________________________________________________________

; Destroy mutex and clear data fields - called by resource manager thread.

FreeWatchpointResources:
    call 'KERNEL32.CloseHandle' D$WPSynchMutex
    mov eax 0
    mov D$WPSlot0 eax
    mov D$WPSlot1 eax
    mov D$WPSlot2 eax
    mov D$WPSlot3 eax
    mov D$WPControl eax
    mov D$WPChanged eax
ret
____________________________________________________________________________________________

; Add watchpoint - called by UI.

Proc SetWatchPoint:
    Arguments @Address, @Size, @ReadWrite

    call 'KERNEL32.WaitForSingleObject' D$WPSynchMutex, &INFINITE

        mov eax D@Address
        mov D$WPSlot0 eax
        and D$WPControl 0_FFF0_FFFF
        or  D$WPControl 1 ; activate DR0
        mov eax D@ReadWrite
        and eax 0011
        shl eax 16
        or  D$WPControl eax
        If eax <> 0
            ; 00 1-byte length
            ; 01 2-byte length
            ; 10 Undefined
            ; 11 4-byte length
            mov eax D@Size | dec eax
            shl eax 18
            or  D$WPControl eax
        EndIf

        mov D$WPChanged 1

    call 'KERNEL32.ReleaseMutex' D$WPSynchMutex
EndP
____________________________________________________________________________________________

; Delete watchpoint - called by UI.

Proc DeleteWatchPoint:
    call 'KERNEL32.WaitForSingleObject' D$WPSynchMutex, &INFINITE

        and D$WPControl 0_FFF0_FFFE
        mov D$WPSlot0 0

        mov D$WPChanged 1

    call 'KERNEL32.ReleaseMutex' D$WPSynchMutex
EndP
____________________________________________________________________________________________

; Take over any changed values into thread context structure - called by debugger.

Proc TransferWatchpoints:
    call 'KERNEL32.WaitForSingleObject' D$WPSynchMutex, &INFINITE

        move D$C.iDr0 D$WPSlot0
        move D$C.iDr1 D$WPSlot1
        move D$C.iDr2 D$WPSlot2
        move D$C.iDr3 D$WPSlot3
        move D$C.iDr7 D$WPControl

        mov D$WPChanged 0

    call 'KERNEL32.ReleaseMutex' D$WPSynchMutex
EndP
____________________________________________________________________________________________

; Test if address is watched - called by UI.
; No need to use the mutual exclusion here as the debug thread doesn't write to the fields.

Proc IsWatchPoint:
    Arguments @Address

    mov eax D@Address
    If eax = D$WPSlot0
        mov eax D$WPControl
        shr eax 16
        and eax 0011
    Else
        mov eax 0
    EndIf
EndP
____________________________________________________________________________________________

; Test if a watched data access has taken place - called by debugger.

Proc EncounterWatchPoint:

    test D$C.iDr6 01 | jz L9>

    mov D$DebugEventType DET_WP
    move D$WatchedAddress D$C.iDr0
    dec D$SourcePosCodeAddress

  ; clear status
    mov D$C.iDr6 0
L9:
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; BREAKPOINTS

; Every breakpoint is saved in the breakpoint table. An entry has the following structure:
; DWORD Address
; BYTE  Type (see type flags)
; BYTE  State (see state flags)
; BYTE  Reserved (can be used for bp-group ids)
; BYTE  Overwritten byte

[BPTable: ? NumBreakpoints: ?]

; Type flags
[BP_STATIC 0 BP_STATIC2BYTE 01 BP_DYNAMIC 02 BP_ONESHOT 04]
; State flags
[BP_ENABLED 1 BP_DISABLED 0 BP_ISPENDING 2]

; BPPending has the address of a dynamic breakpoint that is currently deactivated to
; execute the real code that was overwritten by the bp-op.
; Otherwise it is zero- indicating that no BP must be written back to process mem ATM.
[BPPending: ?]

CreateBPTable:
    VirtualFree D$BPTable
    VirtualAlloc BPTable, 01_0000 ; 64k table
    mov D$NumBreakpoints 0
    mov D$BPPending 0
ret

DestroyBPTable:
    mov D$BPPending 0
    mov D$NumBreakpoints 0
    VirtualFree D$BPTable
ret

[RunAfterWriteBack: ?]
____________________________________________________________________________________________

; Add breakpoint to table and, if enabled, to process memory.

Proc AddProcessBreakpoint:
    Arguments @Address, @Type, @State, @GroupID
    Uses edi

        mov edi D$BPTable, ecx D$NumBreakpoints
        lea edi D$edi+ecx*8

        mov eax D@Address   | stosd
        mov al B@Type       | stosb
        mov al B@State      | stosb
        mov al B@GroupID    | stosb

        .If B@Type <> BP_STATIC
            call ReadProcessMem D@Address, edi, 1
            If B@State = BP_ENABLED
                call WriteProcessMem D@Address, BreakpointOp, 1
            EndIf
        .Else
            If B@State = BP_DISABLED
                mov B$ByteBuf 090
                call WriteProcessMem D@Address, ByteBuf, 1
            EndIf
        .EndIf
        inc D$NumBreakpoints

        ;call 'Kernel32.OutputDebugStringA' {'BP manager: BP added' 0}
EndP
____________________________________________________________________________________________

; Delete breakpoint from table and from process memory.

Proc DeleteProcessBreakpoint:
    Arguments @Address
    Uses esi

        On D$NumBreakpoints = 0, jmp L9>>

        ; save last entry on stack, search for breakpoint in table, do the cleanup
        ; work (write back process memory), and overwrite entry with last one from stack
        mov esi D$BPTable, ecx D$NumBreakpoints, eax D@Address
        dec ecx
        push D$esi+ecx*8, D$esi+ecx*8+4
            While D$esi <> eax
                add esi 8
                dec ecx | js L9>
            EndWhile

            .If B$esi+5 = BP_ENABLED
                If B$esi+4 <> BP_STATIC
                    mov al B$esi+7
                Else
                    mov al 090
                EndIf
                mov B$ByteBuf al
                call WriteProcessMem D@Address, ByteBuf, 1

            .ElseIf B$esi+5 = BP_ISPENDING
                ; fix: if breakpoint is currently deactivated and waits for write-back
                ; the write-back command must be killed with the breakpoint
                mov D$BPPending 0
            .EndIf

        pop D$esi+4, D$esi
        dec D$NumBreakpoints
        ;call 'Kernel32.OutputDebugStringA' {'BP manager: BP deleted' 0}
        ExitP

L9:     call 'User32.MessageBoxA' D$hwnd, {'Tried to delete non-existing breakpoint!' 0},
                                  {'Debugger error' 0}, &MB_ICONERROR
EndP
____________________________________________________________________________________________

; When [Hold On Breakpoint] is deactivated we disable all known breakpoints and delete
; all oneshot breakpoints.

Proc DisableProcessBreakpoints:
    Uses esi, edi

        ; Iterate through breakpoint-table, delete oneshot and disable all other bp's.
        mov esi D$BPTable, edi D$NumBreakpoints
L0:     While edi > 0
            If B$esi+4 = BP_ONESHOT
                call DeleteProcessBreakpoint D$esi
                dec edi
                jmp L0<
            ElseIf B$esi+4 = BP_DYNAMIC
                mov B$esi+5 BP_DISABLED
                lea eax D$esi+7
                call WriteProcessMem D$esi, eax, 1
            ElseIf B$esi+4 = BP_STATIC
                mov B$esi+5 BP_DISABLED
                mov B$ByteBuf 090
                call WriteProcessMem D$esi, ByteBuf, 1
            EndIf
            add esi 8
            dec edi
        EndWhile
        mov B$BreakpointsEnabled &FALSE
EndP
____________________________________________________________________________________________

; When [Hold On Breakpoint] is reactivated we enable all known breakpoints

Proc EnableProcessBreakpoints:
    Uses esi, ebx

        ; Iterate through breakpoint-table, write breakpoint ops to process memory.
        mov esi D$BPTable, ebx D$NumBreakpoints ; bugfix V2.0b
L0:     While ebx > 0
            call WriteProcessMem D$esi, BreakpointOp, 1
            mov B$esi+5 BP_ENABLED
            add esi 8
            dec ebx
        EndWhile
        mov B$BreakpointsEnabled &TRUE
EndP
____________________________________________________________________________________________

; Deal with encountered breakpoints. Adjusts EIP according to breakpoint type.

Proc EncounterBreakpoint:
    Uses esi, ebx

        ; Search breakpoint in table
        mov esi D$BPTable, ecx D$NumBreakpoints
        mov ebx D$C.regEip | dec ebx
        On ecx = 0, jmp L1>>
        While D$esi <> ebx
            add esi 8
            dec ecx | jz L1>>
        EndWhile

        ; One shot breakpoints are set by step-over, step-out and pause. The user will
        ; receive them as normal stepping (well...), so they are deleted as soon as they are hit.
        ; They can be grouped (for pause), so that if one is hit all others in the group
        ; are deleted with it.
        .If B$esi+4 = BP_ONESHOT
            lea eax D$esi+7
            call WriteProcessMem ebx, eax, 1
            If B$esi+6 <> 0
                movzx eax B$esi+6
                call KillBreakpointGroup eax
            Else
                call DeleteProcessBreakpoint ebx
            EndIf
            dec D$C.regEIP, D$SourcePosCodeAddress

        ; Dynamic breakpoints are user-defined which appear as bp-marks in the source
        ; editor. When encountered the original operation must be restored, executed
        ; (single-stepped), and written back when the next SINGLE_STEP Debug event occurs.
        .Else_If B$esi+4 = BP_DYNAMIC
            lea eax D$esi+7
            call WriteProcessMem ebx, eax, 1
            mov B$esi+5 BP_ISPENDING
            If B$HoldOnBreakpoints = &TRUE
                mov D$BPPending esi
            EndIf
            ; Weird case when a 'int 3' is marked with a breakpoint. It would loop into infinity
            ; so we just skip over it.
            On B$esi+7 <> 0CC,
                dec D$C.regEIP, D$SourcePosCodeAddress

        ; Static breakpoints need not to be dealt with. Execution can continue
        .EndIf

        ExitP

L1:     ; New static breakpoint encountered.
        call AddProcessBreakpoint ebx, BP_STATIC, D$HoldOnBreakpoints, 0
EndP
____________________________________________________________________________________________

Proc KillBreakpointGroup:
    Arguments @GroupID
    Uses esi, edi, ebx

        ; Search breakpoint with given group-id in table. If one is found, delete it.
        mov esi D$BPTable, edi D$NumBreakpoints, ebx D@GroupID
L0:     While edi > 0
            If B$esi+6 = bl
                call DeleteProcessBreakpoint D$esi
                dec edi
                jmp L0<
            EndIf
            add esi 8
            dec edi
        EndWhile
EndP
____________________________________________________________________________________________

; Interrupts the execution of a thread by setting a breakpoint at the location pointed at by
; the instruction pointer. If it is outside of process code, the stack is scanned for a
; valid code pointer.

Proc HaltThread:
    Arguments @ThreadHandle

        call 'KERNEL32.SuspendThread' D@ThreadHandle
        mov D$C.ContextFlags MY_CONTEXT_FULL
        call 'KERNEL32.GetThreadContext' D@ThreadHandle, Context
        call IsProcessCode D$C.regEip
        .If eax = &TRUE
            call AddProcessBreakpoint D$C.regEip, BP_ONESHOT, BP_ENABLED, 0
        .Else
            call ScanStackForCodePointer D$C.regEsp
            If eax <> 0
                call AddProcessBreakpoint eax, BP_ONESHOT, BP_ENABLED, 0
            EndIf
        .EndIf
        call 'Kernel32.ResumeThread' D@ThreadHandle
EndP
____________________________________________________________________________________________

; SYNCHRONIZE BREAKPOINTS WITH SOURCE EDITOR

[BPAnteroom: ? BPSyncMutex: ?]

CreateBPAnteroom:
    call 'Kernel32.CreateMutexA' 0, 0, 0
    mov D$BPSyncMutex eax
    VirtualAlloc BPAnteroom 01000

    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Created BP synch objects' 0}
ret

DestroyBPAnteroom:
    VirtualFree D$BPAnteroom
    call 'KERNEL32.CloseHandle' D$BPSyncMutex

    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Destroyed BP synch objects' 0}
ret

; Copy initial set of breakpoints from the "OnTable" to the Anteroom. This proc is
; called before the debugger thread starts so we don't need to sync with the mutex.

InitialFillBPAnteroom:
    On D$BPOnTable = 0, ret
    ;call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
    mov esi D$BPOnTable
    mov edi D$BPAnteroom
    While D$esi <> 0
        movsd
        mov eax &TRUE
        stosd
        call 'Kernel32.OutputDebugStringA' {'BP synch: BP set' 0}
    EndWhile
    ;call 'KERNEL32.ReleaseMutex' D$BPSyncMutex
    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Initial fill complete' 0}
ret

; FILL Anteroom: This proc is called by the UI thread (RosAsm's mainthread)

Proc AddBPToAnteroom:
    Arguments @Address @Set

        On edi = 0, ExitP

        call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
        mov edi D$BPAnteroom
        While D$edi <> 0
            add edi 8
        EndWhile
        move D$edi D@Address
        move D$edi+4 D@Set
        call 'KERNEL32.ReleaseMutex' D$BPSyncMutex

        ;call 'Kernel32.OutputDebugStringA' {'BP synch: BP set' 0}
EndP

; CLEAR Anteroom: This proc is called by the debugger thread

Proc ClearBPAnteroom:
    call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
    mov edi D$BPAnteroom
    While D$edi <> 0
        call GetcodeBreakPointPosFromSourcePointer D$edi
        If D$edi+4 = &TRUE ; add breakpoint
            call AddProcessBreakpoint eax, BP_DYNAMIC, D$HoldOnBreakpoints, 0
        Else
            call DeleteProcessBreakpoint eax
        EndIf
        mov D$edi 0, D$edi+4 0
        add edi 8
    EndWhile
    call 'KERNEL32.ReleaseMutex' D$BPSyncMutex

    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Anteroom cleared' 0}
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; ADDRESS SPACE ROUTINES
____________________________________________________________________________________________
____________________________________________________________________________________________

[PageOffsetMask     PageSize-1]
[PageBaseMask       0_FFFF_F000]

[AddressLowerBound:  0_1000]
[AddressUpperBound:  0_7FFF_0000]

[MemoryInformation:
 @BaseAddress: ?
 @AllocationBase: ?
 @AllocationProtect: ?
 @RegionSize: ?
 @State: ?
 @Protect: ?
 @Type: ?]

; Wrap routine for VirtualQueryEx
;   Parameter
;       Virtual Address
;   Output
;       EAX : 1=commited 0=free -1=error
;       ECX : region size

Proc VirtualQuery:
    Arguments @Address

        mov eax D@Address
        If eax >= D$AddressUpperBound
            mov eax &FALSE
            ExitP
        EndIf

        call 'KERNEL32.VirtualQueryEx' D$PI.hProcess, D@Address, MemoryInformation, 28
        If eax <> 28
            mov eax 0-1
            ;call ReportWinError {'VirtualQueryEx reported error:' 0}
            ExitP
        EndIf

        ; There is a bug in the implementation of VirtualQuery under WinNT.
        ; Jeffrey Richter has written a workaround (see VMMap in his book) which I
        ; implemented here.
        test D$MemoryInformation@AllocationBase 0FFF | jz L0>
        inc D$MemoryInformation@AllocationBase

L0:     test D$MemoryInformation@RegionSize 0FFF | jz L0>
        inc D$MemoryInformation@RegionSize

L0:     .If D$MemoryInformation@State <> &MEM_FREE
            If D$MemoryInformation@AllocationProtect = 0
                mov D$MemoryInformation@AllocationProtect &PAGE_READONLY
            EndIf
        .EndIf

        mov ecx D$MemoryInformation@RegionSize

        If D$MemoryInformation@State = &MEM_COMMIT
            mov eax &TRUE
        Else
            mov eax &FALSE
        Endif
EndP
____________________________________________________________________________________________

; Determines if the address points to a commited page in the debuggees address space.
; Returns the size of the block starting at the given address in eax.

Proc IsProcessMemory:
    Arguments @Address
    Local @Offset

        ; Offset from start of Page
        mov eax D@Address
        and eax PageOffsetMask
        mov D@Offset eax
        ; Page address
        mov edx D@Address
        sub edx eax
        ; Query information for corresponding page:
        ;  * if eax is 1 the page was commited
        ;  * ecx contains the number of commited bytes from the page address
        call VirtualQuery edx
        If eax = 1
            mov eax ecx
            sub eax D@Offset
        Else
            mov eax 0
        EndIf
EndP
____________________________________________________________________________________________

Proc FindNextPage:
    Arguments @Address
    Uses ebx

        ; align address on page boundary
        mov ebx D@Address
        and ebx PageBaseMask
        add ebx PageSize
        If ebx >= D$AddressUpperBound
            mov eax 0
            ExitP
        EndIf
        ; advance the regions until a commited page is found or the upper bound is met
        call VirtualQuery ebx
        While eax = 0
            add ebx ecx
            If ebx >= D$AddressUpperBound
                mov eax 0
                ExitP
            EndIf
            call VirtualQuery ebx
            If eax = 0-1
                xor eax eax
                ExitP
            EndIf
        EndWhile
        If eax = 0-1
            xor eax eax
        Else
            mov eax ebx
        EndIf
EndP
____________________________________________________________________________________________

Proc FindPrevPage:
    Arguments @Address
    Uses ebx

        ; align address on page boundary
        mov ebx D@Address
        and ebx PageBaseMask
        sub ebx PageSize
        If ebx < D$AddressLowerBound
            mov eax 0
            ExitP
        EndIf
        ; advance the regions until a commited page is found or the upper bound is met
        call VirtualQuery ebx
        While eax = 0
            sub ebx PageSize
            If ebx < D$AddressLowerBound
                mov eax 0
                ExitP
            EndIf
            call VirtualQuery ebx
        EndWhile
        If eax = 0-1
            xor eax eax
        Else
            mov eax ebx
        EndIf
EndP
____________________________________________________________________________________________

Proc IsProcessCode:
    Arguments @Address

        mov eax &FALSE
        mov edx D$DebugBaseOfCode
        If D@Address >= edx
            add edx D$DebugCodeSize
            On D@Address < edx, mov eax &TRUE
        EndIf
EndP
____________________________________________________________________________________________

Proc IsProcessStack:
    Arguments @Address

        mov eax D@Address
        .If eax >= D$C.regEsp
            call IsProcessMemory D$C.regEsp
            mov edx D@Address
            sub edx D$C.regEsp
            If edx < eax
                mov eax &TRUE
            Else
                mov eax &FALSE
            EndIf
        .Else
            mov eax &FALSE
        .EndIf
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________

; Source Editor integration
____________________________________________________________________________________________
____________________________________________________________________________________________

OldSourceDebugPos:
    mov ebx D$C.regEip | sub ebx D$DebugBaseOfCode

    mov esi D$IpTable, ecx D$IpTablePtr, D$StatementsCounter 0
    sub ecx esi | shr ecx 2

    If D$E.ExceptionCode = &EXCEPTION_BREAKPOINT
       ; mov W$DD_Dims 0C
    Else_If D$E.ExceptionCode = &EXCEPTION_SINGLE_STEP
       ; mov W$DD_Dims 0C
    Else
      ;  mov W$DD_Dims 0B |
      inc D$StatementsCounter              ; hide [Step]
    End_If


L0: lodsd | inc D$StatementsCounter | cmp eax ebx | je L2>
                                                    ja L1>
    loop L0<

    jmp L2> ; If last one, don't dec!!!

  ; ret  ; Abort if faultive statement  not found (????????!!!!!!!!!)
  ; or >>> point end of source ???

L1: dec D$StatementsCounter

L2: move D$StatementsPtr D$StatementsTable
    mov eax D$StatementsCounter | dec eax ;| dec eax |
    shl eax 2
    add D$StatementsPtr eax

    call SetEndOfErrorText | call SetDebuggeeText | call AskForRedraw
ret
____________________________________________________________________________________________

;[ApiWarningDone: ?]

[SourcePos: ?]

;;
  Two parallel Tables:
  
* 'IpTable': Each Record is the Displacement for the Origin of Code (for each encoded
  Instruction. In case of Labels (That _are_ parsed by the Encoder, the Label and the
  relative Instruction are both recorded, with the same Displacement).
  
* 'StatementsTable': Each Record is a Pointer to Source Statement. For example, in cases
  of
  
  > mov eax 1, ebx 2, ecx 3
  
  there will be only one Pointer, in 'StatementsTable', that is a Pointer to this 'm'
  Char inside the user Source.
;;

[SourcePosCodeAddress: ?]

Proc SourceDebugPos:
    Arguments @CodeAddress

    call RestoreRealSource

    mov ebx D@CodeAddress | sub ebx D$DebugBaseOfCode

    mov esi D$IpTable, ecx D$IpTablePtr | sub ecx esi | shr ecx 2

    move D$StatementsPtr D$StatementsTable
;;
  Cases of critical error: C_regEip is not update (eip yet pointing to the faultive
  instruction).
  
  Cases of Int 3 and Stepping: The instruction is executed and C_regEip is updated
  (pointing to the next Instruction).
;;
    call ActualDebugPos

  ; eax = 'StatementsPtr' >>> Pointing to a Source Pos Record in 'StatementsTable'.
    mov eax D$eax

    mov D$SourcePos eax ; <<< for stepping

    .If eax <> 0
        ;mov B$ApiWarningDone &FALSE
L5:     call SetEndOfErrorText | call SetDebuggeeText | call AskForRedraw

    .Else_If D$E.ExceptionCode <> &EXCEPTION_SINGLE_STEP
      ; Error outside > Point to end of Source:
        sub D$StatementsPtr 4 | jmp L5<

    .End_If

    call SetPartialEditionFromPos
EndP


ActualDebugPos:
L0: lodsd

    .If eax = ebx
      ; Cases of Labels in the 'IpTable':
L1:     While D$esi = eax | add esi 4 | add D$StatementsPtr 4 | End_While
        jmp L2>

    .Else_If eax > ebx
        sub D$StatementsPtr 4 | jmp L2>

    .End_If

    add D$StatementsPtr 4 | loop L0<

L2: mov eax D$StatementsPtr
    If eax < D$StatementsTable
        mov eax D$StatementsTable | add D$StatementsPtr 4
    End_If
ret

PreviousDebugPos:
L0: lodsd

    .If eax => ebx
        sub D$StatementsPtr 4 | jmp L2>
    .End_If

    add D$StatementsPtr 4 | loop L0<

L2: mov eax D$StatementsPtr
    If eax < D$StatementsTable
        mov eax D$StatementsTable | add D$StatementsPtr 4
    End_If
ret
____________________________________________________________________________________________

____________________________________________________________________________________________

[CodeBuf: B$ ? #32]

NextInstructionDecode:
    call 'Kernel32.ReadProcessMemory' D$PI.hProcess, D$C.regEip, CodeBuf,
                                      32, NumberOfBytesRead
    call InstructionDecode CodeBuf
ret
____________________________________________________________________________________________

[InstructionLength: ?]
[DecodedInstruction: B$ ? #50]
[NextInstructionPtr: ?]

Proc InstructionDecode:
    Arguments @CodeBuffer

    pushad

    ; Clear buffer, set disassembler flags for simple decode and feed the disassembler
    ; with the code in the codebuffer.

    mov edi DecodedInstruction, ecx 50, eax 0
    rep stosb

    mov B$SimpleScan &TRUE, D$LastCodeRef 0
    mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    mov B$WithCommentedHexa &FALSE

    mov esi D@CodeBuffer, edi DecodedInstruction
    add edi 10 ; disassembler sometimes writes in front of edi, reserve 10 guard bytes
L0: movzx eax B$esi | inc esi | call D$DisOp1+eax*4
    On B$DisFlag = DISDONE, jmp L0<
    mov D$edi 0
    sub esi D@CodeBuffer | mov D$InstructionLength esi
    mov B$SimpleScan &FALSE

  ; Dirty method to find the real beginning of the instruction string:
    .If D$LastCodeRef <> 0
        If B$edi-1 = '$'
            mov eax D$LastCodeRef | call WriteEax
        End_If
    .End_If

    mov edi DecodedInstruction, ecx 50, eax 0
    repe scasb | dec edi
L1: If B$edi = ' '
        inc edi | jmp L1<
    ElseIf B$edi = '|'
        inc edi | jmp L1<
    EndIf
    mov D$NextInstructionPtr edi

    popad

    mov eax D$NextInstructionPtr
EndP
____________________________________________________________________________________________

IsMultiStepInstruction:
    mov esi D$NextInstructionPtr
    lodsd
    If eax = 'call'
        mov eax 1 | ret
    EndIf
    and eax 0FF_FFFF
    If eax = 'rep'
        mov eax 1 | ret
    EndIf
    mov eax 0
ret
____________________________________________________________________________________________

[CPUName: B$ ? #48]
[CPUVendor: B$ ? #16]
[CPUFlags: ? CPUFlagsEx: ?]

[FLAG_FPU 1 FLAG_CMOV 0_8000 FLAG_MMX 080_0000 FLAG_SSE 0200_0000 FLAG_SSE2 0400_0000]
[FLAG_EX_MMX 040_0000 FLAG_EX_3DNOW 0_4000_0000 FLAG_EX_3DNOWEX 0_8000_0000]

Proc TestCpuFeatures:
    Local @MaxFunc @MaxExFunc

    mov D$CPUFlags 0, D$CPUFlagsEx 0, B$CPUName 0

    ; Check if CPUID instruction is available
    pushfd | pop eax
    mov ebx eax
    xor eax 0200000
    push eax | popfd
    pushfd | pop eax

    .If eax <> ebx
        mov eax 0
        cpuid
        mov D@MaxFunc eax
        mov D$CPUVendor ebx, D$CPUVendor+4 edx, D$CPUVendor+8 ecx, D$CPUVendor+12 0
        ; Get general flags
        If D@MaxFunc >= 1
            mov eax 1
            cpuid
            mov D$CPUFlags edx
        End_If

        mov eax 0_8000_0000
        cpuid
        mov D@MaxExFunc eax
        ; Extended flags
        If D@MaxExFunc >= 0_8000_0001
            mov eax 0_8000_0001
            cpuid
            mov D$CPUFlagsEx edx
        End_If
        ; CPU name
        If D@MaxExFunc >= 0_8000_0004
            mov eax 0_8000_0002
            cpuid
            mov D$CPUName eax, D$CPUName+4 ebx, D$CPUName+8 ecx, D$CPUName+12 edx
            mov eax 0_8000_0003
            cpuid
            mov D$CPUName+16 eax, D$CPUName+20 ebx, D$CPUName+24 ecx, D$CPUName+28 edx
            mov eax 0_8000_0004
            cpuid
            mov D$CPUName+32 eax, D$CPUName+36 ebx, D$CPUName+40 ecx, D$CPUName+44 edx
        End_If
    .End_If
EndP

