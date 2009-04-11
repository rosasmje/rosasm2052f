TITLE AsmMain

; 'AsmMain' 'EncodeLines'
 _________________________________________________________________________________________
 _________________________________________________________________________________________

; After Macros and Equates replacement, CodeSourceA is of no more use.
; we reused it for CodeList storage table:

; EvilBro: This next routine is evil, because it uses D$SourceLen. I will have to do something
; about that in the future.

ReuseSourceAForCodeList:
    mov eax D$CodeSourceA | and eax 0_FFFF_FF00  ; kill +010 margin in CodeSources tables
    mov D$CodeList eax, edi eax                ; (needed for alignement)
    mov ecx D$AsmTablesLength
    shr ecx 2
    push eax, edi                              ; recent add > clean it all for ease of
        mov eax 0 | rep stosd                  ; read of Hexa PEs.
    pop edi, eax
    add eax 0400 | mov D$CodeListPtr eax       ; clear header and
ret

;;
 This table is for the Debug routine. Each intruction got a pointer. When searching
 for some instruction in the Debugger, we read EIP reg and compare with this List
 in order to point to the source instruction (after error search with the computed
 number of instruction). This List is not closed after compilation.
;;

[IpTable: ?    IpTablePtr: ?    CodeOrigine: ?]

InitDebugIpTable:
    VirtualFree D$IpTable

    mov eax D$SourceLen | add eax 01000 | VirtualAlloc IpTable eax
    move D$IpTablePtr D$IpTable, D$CodeOrigine D$CodeListPtr
ret


[Time1: ?  Time2: ?  Time3: ?  Time4: ?  Time5: ?  Time6: ?  Time7: ?]

[Alert: 'Something is overwriting the Resources Pointers!!!', 0
 AlertTitle: 'Internal Error!!!...', 0]

AlertResources:
    call 'USER32.MessageBoxA' D$hwnd, Alert, AlertTitle, &MB_OK
ret


AsmMain:
    mov D$EntryPointLabel 'MAIN', B$EntryPointLabel+4 0, D$EntryPointLabelLen 4

    On D$ResourcePointersSecurity <> 0, jmp AlertResources

    mov B$CompileErrorHappend &TRUE | On B$SourceReady = &FALSE, ret
    mov eax D$CodeSource | On D$SourceEnd = eax, ret

    call 'KERNEL32.GetTickCount' | mov D$Time1 eax

    mov eax esp, D$OldStackPointer eax         ; To restore stack on error jump

    mov D$NoMeanLabel 'ZZZZ', D$NoMeanLabel+4 'ZZZZ'
    mov B$CompileErrorHappend &FALSE, B$FirstPass &True

    If B$WeAreUnfolding = &FALSE
        call InitProgressBar | call InitProgressSteps 16, 1
    End_If

  ; Ensure Source is ended with at least one CR/LF for ease of line analyzes:
    mov eax D$SourceEnd | mov bx W$eax-2
    cmp bx CRLF | je L1>
      cmp bh CR | jne L0>
          mov B$eax LF | inc D$SourceEnd | inc D$SourceLen | jmp L1>
L0: mov W$eax CRLF | add D$SourceEnd 2 | add D$SourceLen 2

L1: call GetAsmTables                       ; files ready for read in 'CodeSource'
    call ClearUserStubMainData
    call BarProgress

; -------------------------------  First Parsers Jobs ---------------------------------

    If B$ProfilerFlag = &TRUE
        call InjectedCopyToCodeSourceA, D$CodeSource, D$SourceLen
    Else
        call NewCopyToCodeSourceA D$CodeSource, D$SourceLen
    End_If

    call CoolParsers
;jmp L7>>
    call NewCountStatements | call BarProgress

    call NewPrepareExport

    call HotParsers

    call NoAutomaticLabel

;jmp L7>>
;;
 The Source is no longer a crude Ascii but a cooked translation.
 For example, spaces are no longer ' ' (020), but 'Space' (03), and so on.
 See 'LowSigns' and 'MyAsciiTable'.
;;
    call ReplaceWin32Equates
;jmp L7>>

    call InitIndex1

;jmp L7>>
   ;On B$ParseOOA = &TRUE, call ClassParser     ; <<<<<<<<< Delayed (doesn't work at all).
    On B$ParseEqual = &TRUE, call EqualParser
;jmp L7>>
    On B$ParseNew = &TRUE, call NewParser       ; Volunteers private room
    On B$ParseBinInclude = &TRUE, call BinIncluderParser

; jmp L7>>
    call BarProgress
    call 'User32.SendMessageA'  D$hwndForBar, &WM_SETTEXT, 0, Storing
;jmp L7>>
    call ClearQwordCheckSum
;jmp L7>>

; ------------------------  Start of the Macros and Equates Jobs ------------------------

L1: call StoreEquatesAndMacros

    call BarProgress
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Replacing

    call NewReplaceMacAndEqu

    call NewBrackets

    If B$MoreBracket = &TRUE
        mov B$FirstPass &FALSE
        call ResetForNewBrackets
        On B$WeAreUnfolding = &TRUE, call UnfoldOutput
        jmp L1<
    End_If

    On B$WeAreUnfolding = &TRUE, ret

; ------------------------  End of the Macros and Equates Jobs ------------------------

L2: call ExtendLocalSymbols
;;
  Do not remove this 'Exchange': There must be one, at the end of 'ExtendLocalSymbols',
  because it is also called from inside the 'HotParsers', for the @Locals first Pass.
;;
    Exchange D$CodeSourceA D$CodeSourceB
;;
  Down to here, we alway operated by reading at 'CodeSourceA' and writting at
  'CodeSourceB'. These manipulation are over: From here, we take source in
  'CodeSourceB', and will reuse CodeSourceA for building the Binary
;;

    call InitIndex2

    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingImport
    call BarProgress

    call ReuseSourceAForCodeList ;| jmp L7>>

    call BuildImport

;jmp L7>>
    call InitIndex3
    call BarProgress
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingRsrc

    call BuildRsrc

    call 'KERNEL32.GetTickCount' | mov D$Time2 eax
    call BarProgress
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingData


    call BuildData                         ; result 'CodeSourceB' > 'CodeSourceB'

    call BarProgress
    call InitDebugIpTable
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Encoding

    If B$ShortenJumpsWanted = &TRUE
        call InitShortenJmpsTable
        call EncodeLines
        call JmpsOptimize
        call ReleaseShortenJmpsTable

    Else_If B$ProfilerFlag = &TRUE
        call EncodeLines
        call CreateProfilerTables

    Else
        call EncodeLines                       ; Line > Para > Code > op
    End_If

;jmp L7>>
    call 'KERNEL32.GetTickCount' | mov D$Time3 eax
    call BarProgress
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, BuildingHeader

    call PreparePeHeader | call FixTableSizes

    call BarProgress
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Fixing

    call BuildRelocationAndFillSymbols

    If B$ExportsectionWanted = &TRUE
        call PrepareDllVariables | call FillExportSection
    End_If

    call WritePeHeaders

    call BarProgress
    call 'User32.SendMessageA' D$hwndForBar, &WM_SETTEXT, 0, Writing

    call 'KERNEL32.GetTickCount' | mov D$Time4 eax

    call CloseProgressBar

    If B$ShowStats = &TRUE
        mov D$UnusedSymbolsDialogWanted &FALSE
        call 'USER32.DialogBoxParamA', D$hInstance, 2, D$hWnd, Statistics, 0
    Else
        call WritePE | call RecordMRU | On D$BookMarks > 0, call SaveBookMarks
    End_If

L8:
    On B$CompletionWanted = &TRUE, call BuildCompletionTable

    If D$UnusedSymbolsDialogWanted = &TRUE
        call DisplayUnusedSymbolsDialog D$hInstance
    End_If

    call ReleaseAsmTables

    If D$ShowTreeHandle <> 0
        On B$AutoRebuildTreeView = &TRUE, call TreeUpDate
    End_If
   ;call TestStatementsTable
ret

  ; For developments tests only (comments at "WriteDestination:"):
L7: call SetTestSavingName |  call WriteDestination | call CloseProgressBar | jmp L8<<


QuickOut:    call CloseProgressBar | call ReleaseAsmTables | ret

____________________________________________________________________________________________
____________________________________________________________________________________________

[MultipleCompileFindHandle: ?    OneOfMultiplePathNamePointer: ?]

[MultipleCompilePathTitle: 'For Multiple Compilations Test', 0]

[MultipleCompilePath: B$ ? #&MAXPATH]


MultipleCompileTests:
    call BrowseForFolder D$hwnd, MultipleCompilePathTitle
    On B$BrowseForFolderAborted = &TRUE, ret

    mov esi FolderPath, edi MultipleCompilePath
    While B$esi <> 0 | movsb | End_While | mov B$edi '\' | inc edi
    mov D$OneOfMultiplePathNamePointer edi

    mov edi FolderPath | While B$edi <> 0 | inc edi | End_While
    mov D$edi '\*.e', D$edi+4 'xe'

    call 'KERNEL32.FindFirstFileA' FolderPath, FindFile

    .If eax <> &INVALID_HANDLE_VALUE
        mov D$MultipleCompileFindHandle eax

L1:     mov esi FindFile.cFileName, edi D$OneOfMultiplePathNamePointer
        While B$esi <> 0 | movsb | End_While | movsb

        mov esi MultipleCompilePath, edi SaveFilter
        While D$esi <> 0 | movsb | End_While | movsb

        call DirectLoad

        call 'USER32.MessageBoxA' D$hwnd, SaveFilter,
                                  {'Ready to Compile...', 0}, &MB_SYSTEMMODAL

        call AsmMain | mov D$OldStackPointer 0
        On B$CompileErrorHappend = &TRUE, jmp L9>

        call 'KERNEL32.FindNextFileA' D$MultipleCompileFindHandle, FindFile
        cmp eax &TRUE | je L1<
    .End_If

L9: call 'KERNEL32.FindClose' D$MultipleCompileFindHandle
ret
____________________________________________________________________________________________









