TITLE Includes
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

'BuildEquatesTables' 'GetEquates' 
 
 
Win Equates: How it works.

The Equates.equ Files expected by the Parsers is in the form of:

A0_REG 010
A1_REG 011
A2_REG 012
A3_REG 013
A4_REG 014
A5_REG 015
AADBE_ADD_ENTRY 01
AADBE_DEL_ENTRY 02
... and so on.

That is:

* Zero blank Line.
* One line is: NAME / Space / HEXA / CR-LF
* File CR-LF ended.

Zero flexibility. Simply have 2 spaces instead of one, or a space after the Hexa, will
make it fail.

At launch time, RosAsm attempts to open "Equates.equ". If not found, it sends a 
Message telling the user to provide the path for that file.

After having open "Equates.equ", RosAsm also opens all other '*.equ" Files encounted
aside "Equates.equ", loads all of these Data and compiles them.

Compiling the Win Equates:

The Equates Names and Values are stored into two different parallel Tables, one for
the Names, one for the Values. The names are computed into encoded dWords (a kind
of CheckSum), so that the two Tables are the same size.


The NamesTable begins by a Header of dWords Pointers: In fact, not only the Equates
are encoded, but they also are divided in 'First-Char's Chunks and each Chunk is sorted
in numerical order. When searching for an Equate Value, RosAsm first re-encodes the
Name into the CheckSum, condiders the First Equate Char, read the according Pointer
in the NameTable Header (and also the next Pointer to get the size by substraction),
and begins searching in the proper Chunk only. This final search, for recovering a
Value from a given Name, is done by a fast 2n Search Algo.

____________________________________________________________________________________________
____________________________________________________________________________________________


 Reading the "Include" Files: At least: Equates.equ / Functions.api / Structures.str,
 which are the required ones for Win32. RosAsm does not run without ( > Run Help File
 if not found / Load them if found).

 All other .equ Files found in the same Directory add pasted to the same Memory Tables
 for Equates.

 Functions.api is a single File (Right-Click / Disassembly).

 .str Files will be available trough [Struc] main menu option. If several .str Files
 are found > build a child menu with an Item for each, and branch the sub Menu to
 [Struc].
;;
____________________________________________________________________________________________

[IncludeFileName: B$ ? #&MAX_PATH]

; Called with '.ext' in eax:

Proc SetAllIncludeFilesExtension:
    Argument @Extension

    mov esi EquatesName, edi IncludeFileName

    While B$esi <> 0 | movsb | End_While
    dec edi
    While B$edi <> '.' | dec edi | End_While

L0: dec edi | cmp B$edi '\' | je L1>
              cmp B$edi ':' | je L1>
              cmp edi IncludeFileName | ja L0<
                jmp L2>
L1: inc edi
L2: mov B$edi '*' | inc edi | mov eax D@Extension | stosd | mov B$edi 0
EndP

____________________________________________________________________________________________

[StructuresIncFileSize: ?    StructPopUpHandle: ?    StructuresFileOK: ?]
[StrucPopMenu: 'Struct', 0]

[StructuresItem: 'Structures', 0]

PrepareStructuresFiles:
    call SetAllIncludeFilesExtension '.str'
    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | call SetFullName

    ..If eax = &INVALID_HANDLE_VALUE
        mov B$StructuresFileOK &FALSE
        ret
      ; Better let it run without Structures and Api Files if user wants to...
      ; May be temporary...

        call Help, B_U_AsmName, IncludeFilesHelp, ContextHlpMessage

        mov B$IncludesOK &FALSE

    ..Else  ; 'AddUserMenu'
        mov D$FindIncHandle eax
      ; Copy first the first File Path and Name in case there is only one:
        mov B$StructuresFileOK &TRUE
        mov esi FullName, edi MenuItemString, ecx &MAX_PATH | rep movsb

      ; Is there more than one File.str?
        call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU

;mov eax &FALSE ; <<<<<<<<<<<<<<<< Temporary... (ToDo List...).

        .If eax = &TRUE
            call 'KERNEL32.FindClose' D$FindIncHandle

            mov D$StructureMenuID 4000, B$SeveralStructuresFiles &TRUE

            call 'USER32.CreatePopupMenu' | mov D$StructPopUpHandle eax

            call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU
            mov D$FindIncHandle eax

            Do
                call SetStructuresMenuItem
                call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU
            Loop_Until eax = &FALSE



            call 'USER32.InsertMenuA' D$MenuHandle, M00_Structures,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$StructPopUpHandle, StructuresItem

            call 'USER32.DeleteMenu' D$MenuHandle, M00_Structures, &MF_BYCOMMAND


          ;  call 'USER32.DeleteMenu' D$MenuHandle 8 &MF_BYPOSITION
          ;  call 'USER32.InsertMenuA' D$MenuHandle 8 &MF_BYPOSITION__&MF_STRING__&MF_POPUP,
          ;                    D$StructPopUpHandle  StrucPopMenu
        .Else
          ; Else, there is only one .str File.
            mov B$SeveralStructuresFiles &FALSE

        .End_If

        call 'KERNEL32.FindClose' D$FindIncHandle

    ..End_If
ret

[StructureMenuID: ?]

; Builds the added PopUp Menu under [Struct] main Option:
; (We have a: > mov D$StructureMenuID 4000 in caller ('OpenStructuresFiles').

SetStructuresMenuItem:
    mov esi FIND_EQU_cFileName
    While B$esi <> 0 | inc esi | End_While
    dec esi
    While B$esi <> '.' | dec esi | End_While
    mov B$esi 0
L0: dec esi | cmp B$esi '\' | je L1>
              cmp B$esi ':' | je L1>
              cmp esi FIND_EQU_cFileName | ja L0<

L1: call 'USER32.AppendMenuA' D$StructPopUpHandle &MF_STRING D$StructureMenuID esi
    inc D$StructureMenuID
ret

 ________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 15948 Win Equates are stored in 2 tables of dWords: The first one for the Names
 (stored as one dWord per name after string coding) and the second one for the
 relative Values. The routine for coding the names i used when building these tables
 is the same as the one use here for the Win Equates Search (see down there).

 The storage (and search) algorithm is NOT univoque. It simply revealed efficient
 upon this list (without any double values -same value for 2 names- encounted).

 The Data for OS Equates are now outside RosAsm, and computed at launch time.
;;
____________________________________________________________________________________________





[EquatesCurrentDirectory: B$ ? #&MAX_PATH]

;;
  In case user did not re-define the 'Equates.equ' Path, but set the 'Equates.equ' aside
  RosAsm, if he changes the Current Directory when working, and then run, for example [Struct]
  Menu Item, RosAsm would hang when expecting to find out 'Structures.str' in the new Current
  Path. So, we complete the Path here, if needed.
;;

AppendToCurrentDirectory:
    call 'KERNEL32.GetCurrentDirectoryA' &MAX_PATH, EquatesCurrentDirectory
    If eax <> 0
        mov edi EquatesCurrentDirectory | add edi eax
        mov al '\'
        On B$edi-1 <> al, stosb
        mov esi EquatesName | While B$esi <> 0 | movsb | End_While

        mov esi EquatesCurrentDirectory, edi EquatesName
        While B$esi <> 0 | movsb | End_While
    End_If
ret
____________________________________________________________________________________________

[EquatesInFileTitle: 'Number of Encoded Equates:', 0]

[EquatesIncFileSize: ?    EquateIncMemory: ?    EquateIncMemoryPointer: ?]

[FindIncHandle: ?]

[FIND_EQU:
 FIND_EQU_dwFileAttributes: D$ 0
 FIND_EQU_ftCreationTime.dwLowDateTime: D$ 0
 FIND_EQU_ftCreationTime.dwHighDateTime: D$ 0
 FIND_EQU_ftLastAccessTime.dwLowDateTime: D$ 0
 FIND_EQU_ftLastAccessTime.dwHighDateTime: D$ 0
 FIND_EQU_ftLastWriteTime.dwLowDateTime: D$ 0
 FIND_EQU_ftLastWriteTime.dwHighDateTime: D$ 0
 FIND_EQU_nFileSizeHigh: D$ 0
 FIND_EQU_nFileSizeLow: D$ 0
 FIND_EQU_dwReserved0: D$ 0
 FIND_EQU_dwReserved1: D$ 0]
[FIND_EQU_cFileName: B$ 0 #&MAX_PATH]
[FIND_EQU_cAlternate: B$ 0 #14]

[FullName: ? #&MAX_PATH]

SetFullName:
    pushad
        mov esi IncludeFileName, edi FullName
        While W$esi <> '*.' | movsb | End_While
        mov esi FIND_EQU_cFileName
        While B$esi <> 0 | movsb | End_While | movsb
    popad
ret
____________________________________________________________________________________________

OpenEquFiles:
    call IsEquatesEquThere

    If B$IncludesOK = &TRUE
        call GetEquFilesMemory
        call ReadEquatesEqu
        call ReadOtherEquFiles
        call CleanEquateIncMemory
    End_If
ret


[EquatesEquFileName: 'Equates.equ', 0]

SetEquatesEquFileName:
    mov esi EquatesName, edi IncludeFileName

    While B$esi <> 0 | movsb | End_While
    dec edi
    While B$edi <> '.' | dec edi | End_While

L0: dec edi | cmp B$edi '\' | je L1>
              cmp B$edi ':' | je L1>
              cmp edi IncludeFileName | ja L0<
                jmp L2>
L1: inc edi
L2: mov esi EquatesEquFileName
    While B$esi <> 0 | movsb | End_While | movsb
ret


IsEquatesEquThere:
    call SetEquatesEquFileName

    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU

    .If eax = &INVALID_HANDLE_VALUE
        mov B$IncludesOK &FALSE
        call Help B_U_AsmName, IncludeFilesHelp, RosAsmHlpMessage

    .Else
        mov B$IncludesOK &TRUE
    .End_If
ret


GetEquFilesMemory:
    call SetAllIncludeFilesExtension '.equ'

    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | call SetFullName

    mov D$FindIncHandle eax, D$EquatesIncFileSize 0

L0:     mov eax D$FIND_EQU_nFileSizeLow | add D$EquatesIncFileSize eax

        call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU
        call SetFullName | On eax = &TRUE, jmp L0<

    VirtualAlloc EquateIncMemory D$EquatesIncFileSize

L9: call 'KERNEL32.FindClose' D$FindIncHandle
ret


ReadEquatesEqu:
    call SetEquatesEquFileName
    call 'KERNEL32.CreateFileA' IncludeFileName &GENERIC_READ, &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0

    mov D$NumberOfReadBytes 0
    push eax
        push eax
            call 'KERNEL32.GetFileSize' eax, 0 | mov ecx eax
            If ecx = 0
                pop eax | jmp L9>
            End_If
            add eax D$EquateIncMemory | mov D$EquateIncMemoryPointer eax
        pop eax
        call 'KERNEL32.ReadFile' eax, D$EquateIncMemory, ecx, NumberOfReadBytes, 0
    pop eax

L9: call 'KERNEL32.CloseHandle' eax
ret


IsItEquatesEqu:
    mov esi FIND_EQU_cFileName, edi EquatesEquFileName
L0: mov al B$esi, bl B$edi | inc edi | inc esi
    If al = 0
        cmp bl 0
    Else
        or al 020 | or bl 020 | cmp al bl | je L0<
    End_If
ret


ReadOtherEquFiles:
    call SetAllIncludeFilesExtension '.equ'

    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | call SetFullName

        push 0-1

        mov D$FindIncHandle eax

L0:     call IsItEquatesEqu | je L1>
        mov eax D$FIND_EQU_nFileSizeLow
        push eax
            call 'KERNEL32.CreateFileA' FullName &GENERIC_READ, &FILE_SHARE_READ, 0,
                                        &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
        push eax

L1:     call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU
        call SetFullName | On eax = &TRUE, jmp L0<

        mov D$NumberOfReadBytes 0
        mov edi D$EquateIncMemoryPointer
L0:     pop eax                     ; Handle
        On eax = 0-1, jmp L9>
        pop ecx                     ; Size
        push edi, ecx, eax
            call 'KERNEL32.ReadFile' eax, edi, ecx, NumberOfReadBytes, 0
        pop eax, ecx, edi

        add edi ecx

        On W$edi-2 <> 0A0D, jmp BadEquatesFileEnd
        On W$edi-4 = 0A0D, jmp BadEquatesFileEnd

        push edi | call 'KERNEL32.CloseHandle' eax | pop edi | jmp L0<

L9:     call 'KERNEL32.FindClose' D$FindIncHandle
ret


CleanEquateIncMemory:
    mov esi D$EquateIncMemory, edi esi, edx esi | add edx D$EquatesIncFileSize

    While esi < edx
        On B$esi = Tab, mov B$esi ' '
        inc esi
    End_While

    mov esi edi

    .While esi < edx
        lodsb | stosb
        If al = ' '
            While B$esi = ' '
                inc esi | dec D$EquatesIncFileSize
            End_While
        End_If
    .End_While
ret

____________________________________________________________________________________________

[NumberOfEquates: ?]

; In factn ot only 'Count Equates', but also verify integrity of File syntax:
; One Symbol UpperCase / One space / One Hexa in RosAsm syntax / One CR/LF.

CountEquates:
    mov edx D$EquateIncMemory | add edx D$EquatesIncFileSize

    On W$edx-2 <> 0A0D, jmp BadEquatesFileEnd
    On W$edx-4 = 0A0D, jmp BadEquatesFileEnd

    mov esi D$EquateIncMemory, ecx 0

    .While esi < edx
        mov ebx esi

        While B$esi > ' ' | inc esi | End_While         ; Read Symbol.

        On B$esi <> ' ', jmp BadEquatesFile             ; One single space.

        inc esi | On B$esi <> '0', jmp BadEquatesFile

        While B$esi > ' ' | inc esi | End_While         ; One RosAsm syntax Hexa number.

        On W$esi <> 0A0D, jmp BadEquatesFile  ; CR/LF

        add esi 2 | inc ecx
    .End_While

    mov D$NumberOfEquates ecx
ret

____________________________________________________________________________________________

[BadEquatesFileTitle: 'Bad Equates File', 0]
[BadEquatesFileMessage: ? #10]
[BadEquatesFileEndMessage: 'The Equates File must be ended by *one* CR/FL', 0]

BadEquatesFile:
    mov esi ebx, edi BadEquatesFileMessage
    While B$esi > ' ' | movsb | End_While
    mov D$edi '...', B$edi+4 0


    call 'USER32.MessageBoxA' 0, BadEquatesFileMessage, BadEquatesFileTitle, 0
    call 'KERNEL32.ExitProcess', 0


BadEquatesFileEnd:

    call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
    call 'KERNEL32.ExitProcess', 0


[EquatesNumber: ?     WinEquTableLenght: ?
 NamesTable: ?    ValuesTable: ?
 SortedNamesTable: ?    SortedValuesTable: ?]

[SizeByFirstChar: ? #256] ; 26 Length (Length for Equates beginning by 'A, by 'B',... 'Z'.
; plus '[/]^_', just to have '_' >>> 31 records.
[ApiChunksPointers: ? #256] ; Used to follow up when filling the Api List Chunks.


 ________________________________________________________________________________________









