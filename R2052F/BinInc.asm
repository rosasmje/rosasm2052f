TITLE BinInc


;;
____________________________________________________________________________________________
____________________________________________________________________________________________

                    Binary File Includer. Author: Kenny.
                    
____________________________________________________________________________________________

I did several minor modification to the original version:

* Write the Bytes Values in Hexa Format (faster than Decimal).

* Replace the Equate form for the Data Length by a usual 'len' in Data.

This small Pre-Parser seems to me a good idea. Something like RcData, directely into
.Data section, and, so forth more flexible, and without the .rsrc size problem and
usual complications.


Betov.
____________________________________________________________________________________________
____________________________________________________________________________________________

This is a preparser that adds a file to the source at compile time. The syntax is as follows

PREPARSE BinIncluder ; <- this enables the preparser to look for the keyword 'bininclude'
[bininclude myfile.xxx:] ; <- this tells the parser what file to include...

the preparser opens the file, and inputs the file as binary into the data section by creating
something that looks like this:

   [myfile.xxx: B$ 01E 026 085 ... 05 028 08F ... myfile.xxx_Len: len]

You will never actually see the above, but you can use it like it's there:

 > mov al B$myfile.xxx      ; al = 010
 > mov eax myfile.xxx       ; eax = pointer to the file contents
 > mov eax myfile.xxx_len   ; eax = size of file

Enjoy!...
;;

[bininc.filename: B$ ? #&MAX_PATH]
[bininc.filehandle: D$ ?   bininc.filesize: D$ ?   bininc.mem: D$ ?   EcxSave: ?]
[bininc.errornotfound: B$ 'BinIncluder: File not found!', 0
 bininc.errorsize: B$ 'BinIncluder: File size is greater than 1MB!', 0
 bininc.errorSyntax: B$ 'Bad BININCLUDE syntax', 0]

BinIncluderParser:
    call ExtendMemForBinIncluder

    mov esi D$CodeSourceA, edi D$CodeSourceB
    mov ecx esi | add ecx D$StripLen | mov D$EcxSave ecx


    .While esi < D$EcxSave
        cmp D$esi   'BINI' | jne L8>>
        cmp D$esi+4 'NCLU' | jne L8>>
        cmp W$esi+8 'DE'   | jne L8>>
        cmp B$esi-1 Openbracket | jne L8>>

        add esi 10
L3:     inc esi | cmp B$esi ' ' | jbe L3<

        mov edx bininc.filename
L3:         mov al B$esi | mov B$edx al
            inc esi | inc edx
        cmp B$esi ' ' | ja L3<
        mov B$edx 0

        pushad
            call 'KERNEL32.CreateFileA' bininc.filename,
                                        &GENERIC_READ,
                                        &FILE_SHARE_READ,
                                        &NULL,
                                        &OPEN_EXISTING,
                                        &FILE_ATTRIBUTE_NORMAL,
                                        &NULL
            mov D$bininc.filehandle eax

            call 'KERNEL32.GetFileSize' eax, 0 | mov D$bininc.filesize eax

            VirtualAlloc bininc.mem eax

            call 'KERNEL32.ReadFile' D$bininc.filehandle, eax, D$bininc.filesize,
                                     NumberOfReadBytes 0
            call 'KERNEL32.CloseHandle' D$bininc.filehandle
        popad

        push esi
            mov esi bininc.filename
L3:         movsb | cmp D$esi 0 | jne L3<
        pop esi

        movsb ; store the colonsign

        mov al 'B' | stosb | mov al memMarker | stosb

        push esi
            mov esi D$bininc.mem
            mov ecx D$bininc.filesize
L3:         movzx eax B$esi | inc esi | call WriteEax
            mov al Space | stosb | loop L3<

            mov esi bininc.filename
            While B$esi > 0 | movsb | End_While
            mov D$edi 'LEN:' | mov B$edi+3 ColonSign | add edi 4
            mov D$edi 'LEN]' | add edi 3

            push edi
                VirtualFree D$bininc.mem
            pop edi
        pop esi

L8:     movsb
    .End_While

    sub edi D$CodeSourceB | mov D$StripLen edi
    Exchange D$CodeSourceA D$CodeSourceB
ret


[InIncludeSize: ?    CopyOfCodeSourceA: ? CopyOfCodeSourceB: ?]

ExtendMemForBinIncluder:
    mov B$ErrorLevel 9
    mov esi D$CodeSourceA, D$InIncludeSize 0
    mov ecx esi | add ecx D$StripLen | mov D$EcxSave ecx

    .While esi < D$EcxSave
        cmp D$esi   'BINI' | jne L8>>
        cmp D$esi+4 'NCLU' | jne L8>>
        cmp W$esi+8 'DE'   | jne L8>>
        cmp B$esi-1 Openbracket | jne L8>>

        add esi 10
L3:     inc esi | cmp B$esi ' ' | jbe L3<

        mov edx bininc.filename
L3:         mov al B$esi | mov B$edx al
            inc esi | inc edx
        cmp B$esi ' ' | ja L3<
        mov B$edx 0

        If B$esi <> ColonSign
            error bininc.errorSyntax
        Else_If B$esi+1 <> CloseBracket
            error bininc.errorSyntax
        End_If

        pushad
            call 'KERNEL32.CreateFileA' bininc.filename,
                                        &GENERIC_READ,
                                        &FILE_SHARE_READ,
                                        &NULL,
                                        &OPEN_EXISTING,
                                        &FILE_ATTRIBUTE_NORMAL,
                                        &NULL
                mov D$bininc.filehandle eax

            On eax = &INVALID_HANDLE_VALUE, error bininc.errornotfound

            call 'KERNEL32.GetFileSize' eax, 0 | add D$InIncludeSize eax

            call 'KERNEL32.CloseHandle' D$bininc.filehandle
        popad

L8:     inc esi
    .End_While

  ; Fo 1 Byte from File we will have, at most 0FF + Space > 4 Bytes added in Source:
    shl D$InIncludeSize 2

    .If D$InIncludeSize > 500_000
        mov eax D$SourceLen | add eax 1_000_000 | add eax D$InIncludeSize
        push eax
            VirtualAlloc CopyOfCodeSourceA eax | add D$CopyOfCodeSourceA 010
        pop eax
        VirtualAlloc CopyOfCodeSourceB eax | add D$CopyOfCodeSourceB 010

        mov ecx D$SourceLen | Align_On 4 ecx | shr ecx 2
        push ecx
            mov esi D$CodeSourceA, edi D$CopyOfCodeSourceA | rep movsd
        pop ecx
        mov esi D$CodeSourceB, edi D$CopyOfCodeSourceB | rep movsd

        Exchange D$CodeSourceA D$CopyOfCodeSourceA, D$CodeSourceB D$CopyOfCodeSourceB

        VirtualFree D$CopyOfCodeSourceA, D$CopyOfCodeSourceB
    .End_If
ret








