TITLE D.I.S.


[PesDisPath: B$ ? #&MAX_PATH]
[PeDisFileHandle: ?    PeDisFileLength: ?    PeDisFileMemory: ?    DisFileOk: ?]
[EndOfDisPath: ?]

GetDisPath:
    call GetRosAsmFilesPath
    mov esi RosAsmFilesPath, edi PesDisPath
    While B$esi <> 0 | movsb | End_While

    mov D$edi 'Dis\' | add edi 4 | mov D$EndOfDisPath edi
ret

LoadPeDisFile:
    call GetDisPath | mov edi D$EndOfDisPath, D$edi 'Pe.d', D$edi+4 'is'

    call 'KERNEL32.FindFirstFileA' PesDisPath, FindFile

    ...If eax <> &INVALID_HANDLE_VALUE

        call 'KERNEL32.FindClose' eax

        call 'KERNEL32.CreateFileA' PesDisPath, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        mov D$PeDisFileHandle eax

        call 'KERNEL32.GetFileSize' eax, 0 | mov D$PeDisFileLength eax
        add eax 10

        VirtualAlloc PeDisFileMemory, eax | add D$PeDisFileMemory 10

        call 'KERNEL32.ReadFile' D$PeDisFileHandle, D$PeDisFileMemory,
                                 D$PeDisFileLength, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$PeDisFileHandle

        mov B$DisFileOk &TRUE

    ...Else
        mov B$DisFileOk &FALSE

    ...End_If
ret


[NextLine | While B$esi > CR | inc esi | End_While
            While B$esi < ' ' | inc esi | End_While]

[TestSignatures: ?    TestSignaturesMax: ?    PeDisMatches: ?    PeDisMatchesPointer: ?]

ParsePeDis:
    mov esi D$PeDisFileMemory, edx esi | add edx D$PeDisFileLength

    While D$esi <> '; Da' | inc esi | End_While

L0: cmp B$esi '[' | je L1>
        inc esi | cmp esi edx | jb L0<
            jmp L9>>

L1: .While D$esi <> 'Sign'  ; ature:
        NextLine | On B$esi = ']', jmp L0<
    .End_While

    add esi 11 | inc D$TestSignatures

  ; "Signature:" found:

    mov edi D$DisEntryPoint | sub edi D$DisImageBase | add edi D$UserPeStart

L2: call GetHexaFromSignature

    ...If eax = CRLF
      ; Matching Pattern found.
        push esi
            While B$esi <> '[' | dec esi | End_While
            mov edi D$PeDisMatchesPointer, D$edi esi
            While D$esi <> 'Func' | inc esi | End_While | add esi 15
            call GetDisFunctionSize | mov D$edi+4 eax
        pop esi
        jmp L0<<

    ...Else_If eax = QUESTIONMARKS
        inc edi | jmp L2<

    ...Else
       ; .If D$TestSignatures = 416
       ;     If al <> B$edi
       ;         hexprint eax
       ;         mov B$esi+100 0 | showme esi
       ;         map
       ;     End_If
       ; .End_If
        cmp al B$edi | jne L0<<
            inc edi | jmp L2<<

    ...End_If
L9: ret


GetDisFunctionSize:
    mov ecx 0

L0: lodsb | cmp al '0' | jb L8>
        sub al '0'
        lea ecx D$ecx+ecx*4
        lea ecx D$eax+ecx*2
    jmp L0<

L8: ret


GetBiggerPeDisID:
    mov esi D$PeDisMatches, eax 0, edx 0

L0: If D$esi+4 > eax
        mov eax D$esi+4, edx D$esi
    End_If

    add esi 8 | cmp D$esi 0 | ja L0<

    mov esi edx, B$esi+100 0 | ;showme esi
ret


[QUESTIONMARKS 0FFFF]

GetHexaFromSignature:
    While B$esi = ' ' | inc esi | End_While

    If B$esi = '?'
        add esi 3 | mov eax QUESTIONMARKS
        On B$esi = LF, dec esi
        ret
    Else_If B$esi = CR
        mov eax CRLF | add esi 2 | ret
    End_If

    lodsb | sub al '0' | cmp al 9 | jbe L2>
        sub al 7

L2: mov ah al
    lodsb | sub al '0' | cmp al 9 | jbe L2>
        sub al 7

L2: If ah > 0F
        jmp L8>

    Else_If al > 0F
        jmp L8>

    Else_If B$esi > ' '
L8:
        call 'USER32.MessageBoxA', 0, {'Bad Hexa Data encounted', 0},
                                    FindFile.cFileName, 0 ;{'Reading Pe.dis', 0}, 0
        While B$esi <> '[' | dec esi | End_While
        mov B$esi+200 0 | ;showme esi
        jmp DisFail

    Else
        shl ah 4 | or al ah | and eax 0FF

    End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  For example, a File like vc32rt.dis may be 6,778,426 Bytes long. 
  
  That is: 298,780 Lines... / 12 Lines per Record: 24898 Records to parse (!!!...)
  for one single "ModUsed" File...
  
  So, it's necessary to build a '.bis' File from each '.dis' one.
  
  Structure:
  
  1) A Header Table of 256 dWords pointing to the Signatures Entries.
  
  2) Each 'Entry' is a set of Linked records holding the 'Signatures':
  
  Example, for:
  
  [
  Id: 1
  Name: fFSQRT
  Signature: 0A C9 75 12 D9 FA C3 0A C9 75 0B C3 D9 E4 9B DF E0 9B 9E 75 EB C3 E9
  Entry Point: False 0 <$C>
  Reference List: 0
  Code Label: <$$>0007 _rtforsqrtinf <$$>000C _rtforsqrtzero <$$>0016 sqrtindfnpop
  CRC: 00 0000
  Function Size: 27
  ]
  
  The dWord at (0+(0A*4)) points to the 'fFSQRT' (binary translation) record
  
  Each 'Signature' Binary translation is, for example, with the above 'fFSQRT'
  one:
  
  B$ 23 0A 0C9 075 012 0D9 0FA 0C3 0A 0C9 075 0B 0C3 0D9 0E4 09B 0DF 0E0 09B 09E 075 0EB 0C3 0E9
  
  Where the '23' Byte says that 23 determined Bytes are following.
  
  In cases when undertermined Bytes are included, for example:
  
  [
  Id: 7
  Name: ?do_tolower@?$ctype@D@std@@MBEDD@Z
  Signature: 0F B6 44 24 04 83 C1 08 51 50 E8 ?? ?? ?? ?? 59 59 C2 04 00
  Entry Point: False 0 <$C>
  Reference List: <$$>000B __Tolower
  Code Label: 0
  CRC: 00 0000
  Function Size: 20
  ]
  
  the 'Signature' Binary translation is:
     __                                          ___ _
  B$ 11 0F 0B6 044 024 04 083 0C1 08 051 050 0E8 0-4 5 059 059 0C2 04 0

  ... where 11 says that 11 determined Bytes are following,
           0-4 says that 4 undetermined Bytes are following,
         and 5 says that 5 dtermined Bytes are following.
         
  Now, in the 256 dWords Header, each dWord points to only _one Signature.
  
  As ther may be many different Signatures beginning with the same Byte
  value, we need to Link the Lists of Signatures.
  
  This is to say that, for example, all Signatures beginning by 0A, will
  have to come under the form of a Linked List:
  
  Header dWords: [.... .... .... Ptr0A ... ... ...]
  
  Ptr0A: [Ptr0A_1: D$ Ptr0A_2 B$ 11 0F 0B6 044 024 04....
          Ptr0A_2: D$ Ptr0A_3 B$ ........................
          ....
          0]
          
  Scaning a Disassembled PE, for Pattern Matchings should look like this:
  
  ; ...
    mov eax 0
  ; ....
    mov al B$esi
  
    ...If D$dWordsHeader+eax*4 <> 0
            mov edi D$dWordsHeader+eax*4
          ; Take the Chained Pointer:
            mov edx D$edi | add edi 4
            
          ; Take the Bytes count:
            mov cl B$edi | inc edi
            
            If cl = 0
                ; Found
            Else_If cl <= 32
                While cl <> 0
                    ; Compare
                End_While
            Else
                While cl <> 0
                    inc esi | inc cl
                End_Wile
            End_If

    ...End_If   
    
    inc esi
    
  ; ... 
;;


Proc LoadOneDisFile:
    Argument @Name, @Ext

        mov edi D$EndOfDisPath, esi D@Name
        While B$esi <> 0 | movsb | End_While
        If D@Ext <> &NULL
            move D$edi D@Ext | add edi 4
        End_If
        mov B$edi 0

        call 'KERNEL32.CreateFileA' PesDisPath, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0

        ...If eax <> &INVALID_HANDLE_VALUE

            mov D$PeDisFileHandle eax

            call 'KERNEL32.GetFileSize' eax, 0 | mov D$PeDisFileLength eax
            add eax 10

            VirtualAlloc PeDisFileMemory, eax | add D$PeDisFileMemory 10

            call 'KERNEL32.ReadFile' D$PeDisFileHandle, D$PeDisFileMemory,
                                    D$PeDisFileLength, NumberOfReadBytes, 0

            call 'KERNEL32.CloseHandle' D$PeDisFileHandle

            mov B$DisFileOk &TRUE

        ...Else
            mov B$DisFileOk &FALSE

        ...End_If
EndP


DisToBis:
    mov edi D$EndOfDisPath, D$edi '*.di', D$edi+4 's'

    call 'KERNEL32.FindFirstFileA' PesDisPath, FindFile | mov D$FindHandle eax

    ...If eax = &INVALID_HANDLE_VALUE

        call 'KERNEL32.FindClose' eax

    ...Else
L1:     call LoadOneDisFile FindFile.cFileName, &NULL

        call ComputeDisToBin

        VirtualFree D$PeDisFileMemory

        call 'KERNEL32.FindNextFileA' D$FindHandle FIND_EQU | on eax = &TRUE, jmp L1<

        call 'KERNEL32.FindClose' D$FindHandle
    ...End_If
ret


[BisMem: ?]

ComputeDisToBin:
    VirtualAlloc BisMem D$PeDisFileLength

    mov esi D$PeDisFileMemory, edi D$BisMem, edx esi
    add edx D$PeDisFileLength

    mov D$FirstByte 0

    add edi (0100*4)

  ; ParsePeDis
    While D$esi <> '; Da' | inc esi | End_While

L0: cmp B$esi '[' | je L1>
        inc esi | cmp esi edx | jb L0<
            jmp L9>>

L1: .While D$esi <> 'Sign'  ; ature:
        NextLine | On B$esi = ']', jmp L0<
    .End_While

    add esi 11 | inc D$TestSignatures

  ; "Signature:" found:
   ; call EncodeOneSignature |
   jmp L0<
L9: VirtualFree D$BisMem
    ret


; Signature: 0F B6 44 24 04 83 C1 08 51 50 E8 ?? ?? ?? ?? 59 59 C2 04 00
;    __                                          ___ _
; B$ 11 0F 0B6 044 024 04 083 0C1 08 051 050 0E8 0-4 5 059 059 0C2 04 0


[FirstByte: ?]

EncodeOneSignature:
    mov ecx 0

  ; Read the First Byte of the Signature:
    call GetHexaFromSignature

    If al > B$FirstByte
        mov B$FirstByte al, B$FirstByte+3 0FF
        mov ebx D$BisMem, D$ebx+eax*4 edi

    Else_If al < B$FirstByte
        call 'USER32.MessageBoxA' 0, {'Bad order of Records', 0},
                                     FindFile.cFileName, 0
        While D$esi <> 'Id: ' | dec esi | End_While
        mov B$esi+200 0 | ;showme esi
        jmp DisFail

    End_If

    jmp L1>

L0: call GetHexaFromSignature

L1: .If eax = QUESTIONMARKS
      ; Write the written number of Significative Bytes
        If cl = 0
            call 'USER32.MessageBoxA' 0, {'Record beginning with ??', 0},
                                     FindFile.cFileName, 0
            mov B$esi+200 0 | ;showme esi
            While D$esi <> 'Id: ' | dec esi | End_While
            mov B$esi+200 0 | ;showme esi
            jmp DisFail
        End_If
        mov B$edi cl | add edi ecx | inc edi

        mov ecx 0-1
        While B$esi = '?'
            add esi 3 | dec ecx
        End_While
        mov B$edi cl | inc edi

        mov ecx 0

    .Else_If eax = CRLF
        If cl <s 1
            call 'USER32.MessageBoxA' 0, {'Record ending with ??', 0},
                                     FindFile.cFileName, 0
            While D$esi <> 'Id: ' | dec esi | End_While
            mov B$esi+200 0 | ;showme esi
            jmp DisFail
        End_If

        mov B$edi cl | add edi ecx | inc edi | jmp L9>

    .Else
        inc ecx | mov B$edi+ecx al

    .End_If

    jmp L0<<

L9: ret






