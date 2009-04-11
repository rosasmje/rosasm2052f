TITLE MacroCreator

____________________________________________________________________________________
____________________________________________________________________________________________
;;
MacroCreator

Created By Looki in October/2006

The goal of this routine is to substitute some code to Macros, recover the HLL style from
disassembled code, and clean the code from the various types of aligns

Current Version: 20/November/2006
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[If_MemComp | mov #4 #3 | cmp #1 #4 | jn#2 F1>> | #+4]
[End_If_MemComp | F1: | F9: ]
[Byter dl Wordr dx Dwordr edx]

[While_MultComp | N0: | cmp #1 #3 | jn#2 N9> | #+3]
[End_While_MultComp | jmp N0< | N9: ]

[Buffer: B$ "    
Code02704D20: M8:
    cmp eax ebx | jne Code0403045
    mov eax 0
    cmp eax ecx | jne Code0403040
    mov eax 01
    cmp edx ecx | jne Code0403029
    mov eax 02

Code0403029: H3:
    cmp ecx edx | jne Code040303B
    mov eax 064
jmp Code0403029
    
Code040303B: J1:
    mov eax 0C
    
Code0403040: J6:
    mov eax 0D
    
Code0403045: K1:" 0]
[FirstAdress: ?]
[LastAdress: ?]
[FirstAdressWithMacro: ?]
[LastAdressWithMacro: ?]
[EndOfCodeLine: ?]
[Cache: ? #20]
[CodeSectName: ? #10]
____________________________________________________________________________________________

FindEndAdress:
    mov eax Buffer
    While B$eax <> 0
        inc eax
    End_While
    mov D$LastAdress eax
    ret
____________________________________________________________________________________________

ScanAndRealoc:
    mov ebx 0
    While edx < D$LastAdress
        mov bl B$edx
        mov B$ecx bl
        inc edx
        inc ecx
    End_While
    mov ebx ecx
    While ecx < D$LastAdress
        mov D$ecx 0
        add ecx 4
    End_While
    mov D$LastAdress ebx ; atualiza o ultimo endereço no handle principal
    ret
____________________________________________________________________________________________

InternalSubstitution:
            lea edx D$eax+25 ; endereço da ultima linha onde havia o código da macro
            mov eax D$FirstAdressWithMacro ; pega o primeiro endereço onde reside a macro
                    ; move os dados para o cache
                    mov ecx D$eax
                    mov D$Cache ecx
                    mov ecx D$eax+4
                    mov D$Cache+4 ecx
                    mov ecx D$eax+8
                    mov D$Cache+8 ecx
                    mov ecx D$eax+12
                    mov D$Cache+12 ecx
                    mov cl B$eax+16
                    mov B$Cache+16 cl
                    ; escreve o nome da macro antes do nome da seçao code
                    mov D$eax 'Proc'
                    mov cl ' '
                    mov B$eax+4 cl
                    ; move os do cache para a posiçao apos o nome da macro
                    mov ecx D$Cache
                    mov D$eax+5 ecx
                    mov ecx D$Cache+4
                    mov D$eax+9 ecx
                    mov ecx D$Cache+8
                    mov D$eax+13 ecx
                    mov ecx D$Cache+12
                    mov D$eax+17 ecx
                    mov cl B$Cache+16
                    mov B$eax+21 cl
                    lea ecx D$eax+22 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                    mov eax ecx ; endereço do final da linha code no handle principal
            ret
____________________________________________________________________________________________

MacroIfRecog:
;------------------------------------------------------------------------------------
;macro validada |"cmp eax ebx | jne Code0403045" |Code0403045: K1:|
;------------------------------------------------------------------------------------
                    mov D$eax '..En'
                    mov D$eax+4 'd_if'
                    lea edx D$eax+16 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
                    lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                    mov eax ecx ; endereço do final da linha code no handle principal
                    call ScanAndRealoc

                    mov eax esi
                    mov ecx Cache
;------------------------------------------------------------------------------------
;pega o primeiro parametro da macro
;------------------------------------------------------------------------------------
                    While B$eax <> ' '
                        mov dl B$eax
                        mov B$ecx dl
                        inc ecx
                        inc eax
                    End_While

                    mov D$esi-4 '..If'
                    mov B$esi ' '
                    inc esi
;------------------------------------------------------------------------------------
;prepara para receber o segundo parametro que indica o tipo de operação a se fazer
;------------------------------------------------------------------------------------
                    inc eax
                    mov B$ecx ' '
                    inc ecx

                    mov edi eax ; armazena endereço atual de eax [final do primeiro parâmetro] para procurar o segundo parametro

                    While W$eax <> 'jn'
                        add eax 2
                    End_While
;------------------------------------------------------------------------------------
;reconhece o segundo parametro
;------------------------------------------------------------------------------------
                    ; o lea não modifica a flag do jmp, isso garante que no caso de o código
                    ; já ter passado em uma comparação com resultado positivo ele vai para o L8
            cmp B$eax+2 'e' | jne L1> | mov B$ecx '='| lea ecx D$ecx+1 | je L8>
        L1: cmp B$eax+2 'b' | jne L2> | mov B$ecx '<'| lea ecx D$ecx+1 | je L8>
        L2: cmp B$eax+2 'a' | jne L3> | mov B$ecx '>'| lea ecx D$ecx+1 | je L8>
        L3: cmp B$eax+2 'l' | jne L4> | mov W$ecx '<s'| lea ecx D$ecx+2 | je L8>
        L4: cmp B$eax+2 'g' | jne L5> | mov W$ecx '>s'| lea ecx D$ecx+2 | je L8>
        L5: cmp W$eax+2 'be' | jne L6> | mov W$ecx '<='| lea ecx D$ecx+2 | je L8>
        L6: cmp W$eax+2 'ae' | jne L7> | mov W$ecx '>='| lea ecx D$ecx+2 | je L8>
        L7: cmp W$eax+2 'ne' | jne L8> | mov W$ecx '<>'| lea ecx D$ecx+2 | je L8>
        L8:
;------------------------------------------------------------------------------------
;volta ao fim do primeiro parametro e prepara para receber o terceiro parametro
;------------------------------------------------------------------------------------
                    mov B$ecx ' '
                    mov eax edi ; volta para o endereço do fim do primeiro parâmetro
                    inc ecx
;------------------------------------------------------------------------------------
;pega o terceiro parametro da macro
;------------------------------------------------------------------------------------
                    While B$eax <> ' '
                        mov dl B$eax
                        mov B$ecx dl
                        inc ecx
                        inc eax
                    End_While
;------------------------------------------------------------------------------------
;Monta os 3 parametros na macro
;------------------------------------------------------------------------------------
                    mov edi Cache

                    While edi < ecx
                        mov dl B$edi
                        mov B$esi dl
                        inc edi
                        inc esi
                    End_While

                    lea edx D$eax+18 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
                    lea ecx D$eax+3 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                    mov eax ecx ; endereço do final da linha code no handle principal
                    call ScanAndRealoc

                    add eax 4
                ret
____________________________________________________________________________________________

CreatorMain:

call FindEndAdress
mov D$FirstAdress Buffer
mov eax D$FirstAdress

..While eax < D$LastAdress
;--------------------------------------------------------------------------------------------------
;Proc/EndP Recon Start
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'Code'
        mov D$FirstAdressWithMacro eax ; pega o primeiro endereço onde reside a macro
        add eax 23
        ..If_And D$eax = 'push', D$eax+4 = ' ebp', D$eax+14 = 'mov ', D$eax+18 = 'ebp ', D$eax+21 = ' esp'
                call InternalSubstitution
                call ScanAndRealoc
        ..End_if
    ...End_if
;--------------------------------------------------------------------------------------------------
;Proc/EndP Recon End
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;Loop Recon Start
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'cmp '

            add eax 4
            mov esi eax ; salva o endereço atual do eax, local do primeiro parametro

;------------------------------------------------------------------------------------
;procura e pega o nome do label[seção]
;------------------------------------------------------------------------------------
            .While D$eax <> 'Code'
                inc eax
            .End_While

            mov ebx CodeSectName ; guarda o nome do label para comparar

            .While B$eax <> 0a
                mov dx W$eax
                mov W$ebx dx
                add ebx 2
                add eax 2
            .End_While

            mov D$ebx-1 ':'
            sub ebx 12
;------------------------------------------------------------------------------------
;parte da validação
;------------------------------------------------------------------------------------
            .While eax < D$LastAdress

                If_MemComp D$eax = D$ebx Dwordr, D$eax+4 = D$ebx+4 Dwordr, D$eax+8 = D$ebx+8 Dwordr
                    call MacroIfRecog
                    jmp M1>
                End_If_MemComp

                ;If_And D$eax = 'jmp ', D$eax+4 = 'Code'
                inc eax

            .End_While
M1:
    ...End_if
;--------------------------------------------------------------------------------------------------
;Loop Recon End
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
;Align Recon Start
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'mov '
        If_MemComp D$eax+3 = D$eax+7 Dwordr
            mov D$eax 'Alig'
            mov D$eax+4 'n 04'
            lea edx D$eax+11 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            mov eax ecx ; endereço do final da linha code no handle principal
            call ScanAndRealoc
        End_if_MemComp
    ...End_if
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'xchg'
        If_MemComp D$eax+4 = D$eax+8 Dwordr
            mov D$eax 'Alig'
            mov D$eax+4 'n 04'
            lea edx D$eax+12 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            mov eax ecx ; endereço do final da linha code no handle principal
            call ScanAndRealoc
        End_if_MemComp
    ...End_if
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'lea '
        If_MemComp W$eax+4 = W$eax+10 Wordr;, B$eax+6 = B$eax+12 Byter
            mov D$eax 'Alig'
            mov D$eax+4 'n 04'
            lea edx D$eax+13 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            mov eax ecx ; endereço do final da linha code no handle principal
            call ScanAndRealoc
        End_if_MemComp
    ...End_if
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'add '
        ..If B$eax+8 = '0'
            If_Or D$eax+4 = 'eax ', D$eax+4 = 'ebx ', D$eax+4 = 'ecx ', D$eax+4 = 'edx ',
                  D$eax+4 = 'edi ', D$eax+4 = 'esi ', D$eax+4 = 'ebp ', D$eax+4 = 'esp '
                mov D$eax 'Alig'
                mov D$eax+4 'n 04'
                lea edx D$eax+9 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
                lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
                mov eax ecx ; endereço do final da linha code no handle principal
                call ScanAndRealoc
            End_If
        ..End_If
    ...End_if
;--------------------------------------------------------------------------------------------------
    ...If D$eax = 'nop ';, B$eax+2 = 'p'
            mov D$eax 'Alig'
            mov D$eax+4 'n 04'
;;
            lea edx D$eax+12 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            mov eax ecx ; endereço do final da linha code no handle principal
            call ScanAndRealoc
;;
    ...End_if
;--------------------------------------------------------------------------------------------------
    ...If_And D$eax = 'int ', B$eax+4 = '3'
            mov D$eax 'Alig'
            mov D$eax+4 'n 04'
;;
            lea edx D$eax+12 ; endereço da ultima linha onde havia o código da macro [fik apontado no Byte anterior ao 0D]
            lea ecx D$eax+8 ; endereço da ultima posiçao da linha do code, antes de pular a linha
            mov eax ecx ; endereço do final da linha code no handle principal
            call ScanAndRealoc
;;
    ...End_if
;--------------------------------------------------------------------------------------------------
;Align Recon End
;--------------------------------------------------------------------------------------------------

    inc eax

..End_While

call 'KERNEL32.ExitProcess' &Null
____________________________________________________________________________________________

;;
Alinhamentos

;;falta fazer
nop
int 3

;;feito
mov eax eax | mov ebx ebx | mov ecx ecx | mov edx edx
mov edi edi | mov esi esi | mov ebp ebp | mov esp esp

xchg eax eax

add eax 0 | add ebx 0 | add ecx 0 | add edx 0
add edi 0 | add esi 0 | add ebp 0 | add esp 0

lea eax D$eax | lea ebx D$ebx | lea ecx D$ecx
lea edx D$edx | lea edi D$edi | lea esi D$esi
lea ebp D$ebp | lea esp D$esp
;;
____________________________________________________________________________________________
