TITLE Mem
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
                                   Memory Management.
____________________________________________________________________________________________
____________________________________________________________________________________________
 Managements for VirtualAlloc:

 VirtualAlloc Reserves Memory Chunks with a 01_0000 alignement and Commit Chunks with
 a 0_1000 alignement. These Memory Manager Routines purpose is to completely secure
 and simplify the calls for Mems and to save all these rooms spoiled by the 01_0000
 alignement. To simplify things, let us call 'Mother Chunks' the 01_0000 aligned ones
 (Reservations default) and 'Child Chunks' the 0_1000 aligned ones (Commition default).

 For developements, an Equate 'GUARDPAGE' may be set to 0_1000. This ensure that, in
 case of overflowing writes to a given Memory Chunk, even if 2 chunks are contiguous,
 Win will effectively raise a exception. With this Guard set, the Manager may eat up
 to 8 times less Mem (when dealing with small Chunks). Without Guard (GUARDPAGE set to
 zero), the eaten Mem may be up to 16 times smaller. (This 'GUARDPAGE' has no relation
 with what Win Doc calls so).

 When small Chunks are wanted, the Memory Manager reuse free Chunks that may be found
 inside already Reserved 01_0000 aligned Chunks.

 All Chunks are recorded in a 'MemTable'. Each Record is:

 Pointer / Size / Mother_Pointer

 Where 'Pointer' is the Pointer to the Committed Chunks, Size is the Size of the
 Committed Chunks eventually added of the GUARDPAGE (0_1000 or 0), and 'Mother_Pointer'
 is a Pointer to the 01_0000 aligned Chunk to which may belong the Committed Chunk. Even
 the Mother Chunk get this last Value pointing to itself. This last Value is used to
 control if and when, the Memory manager may effectively Release the whole Chunk. Until
 all of the nested and Mother Chunks are not Decommitted, the Memory Manager do not
 Release the whole Mother Chunk, but proper releasing will be done, whatever order of
 the 'VirtualFree' calls, even if the 'VirtualFree D$MotherChunk' is called first and
 the 'VirtualFree D$ThirdChildChunk' last.

 If more than expected (here 'MEMTABLESIZE') Chunks are required, this is to say,
 if too much Mems are not released (in other words, if i have forgotten to release
 some Allocated Memories), i should also see an error Message ('NoMoreMem') if the
 'MemTable' is overflowed. Added to this i set a Menu Item in [Tools] to see how
 much Memory Chunks are activated at any time ('ViewRosAsmMems'). This does not tell
 how much Mem is used. This only tells how much Chunks are recorded in the 'MemTable',
 no matter what Size.

 When releasing, if the Pointer value is corrupted, we should also see an error
 Message for 'MemInternalAlert'.


 All this is much heavy, but gives full control on any Management problem, and the
 calls remain completly simple:

 > VirtualAlloc Pointer
 > VirtualFree D$Pointer

 When back from 'VirtualAlloc' Macro, the 'Pointer' holds the new Memory Pointer.
 'VirtualFree' reset 'D$Pointer' Value to zero. If called a second time (with a
 zeroed pointer, the Macro aborts without any error Message, because, in some
 complex cases organisation of the source flow, it may become difficult to know if,
 at some given time, the Mem has already be freed or not. This save from having to
 write:

 > On D$Pointer > 0, VirtualFree D$Pointer

 This is the only one case for which the Manager does not send an error Message
 in case of bad call from the Application. If a wrong Address is sent to VirtualFree,
 for example an Application Table Address instead of a Pointer content, the proper
 error Message is sent.


 A 'LastMemRecord' is also managed in order to save from scaning wide 'MenTable' for
 nop, as we cannot rely on zeroed Records to know if we have finished scaning.
;;
____________________________________________________________________________________________

[GUARDPAGE 01000]   ; Comment / unComment these 2 Equates for
;[GUARDPAGE 0]      ; having / not having hand made Guard Pages.

; MemTable is 3 dWords per record: pointer-to-Mem / MemChunkSize / Mother Pointer// ...
[MEMTABLESIZE (01000*12) ] ; room for 01000 Chunks.
[MEMRECORD (4*3)]

[MemTable: D$ 0, MemTableEnd: 0, MemChunSize: 0
 LastMemRecord: 0, TempoMemPointer: 0]    ; used when no named Pointer is used by
                                            ; caller (cases of Tables of Memory Pointers,
                                            ; for example).

[MemAlert: 'Memory attribution failure', 0
 MemInternalAlert: 'Error inside RosAsm Memory Management', 0
 MemAlertText: "   RosAsm is going to close. Shut down some Aplications and re-run      " 0]

[VirtAllocOverFlowString: 'VirtualAlloc Buffer overflowed', 0]
[VirtAllocFailureString:  'VirtualAlloc Function failure' 0]

Proc VirtAlloc:
  Arguments @sz, @ptr
  USES ecx edx ebx esi edi

        .If D$MemTable = 0
            call 'KERNEL32.VirtualAlloc' &NULL, MEMTABLESIZE+02000, &MEM_RESERVE, &PAGE_NOACCESS
            test eax eax | je @bad | add eax 01000
            call 'KERNEL32.VirtualAlloc' eax, MEMTABLESIZE, &MEM_COMMIT, &PAGE_READWRITE
            test eax eax | je @bad
            mov D$MemTable eax | add eax MEMTABLESIZE | mov D$MemTableEnd eax
            call 'KERNEL32.VirtualAlloc' &NULL, MEMTABLEVIEWSIZE+02000, &MEM_RESERVE, &PAGE_NOACCESS
            test eax eax | je @bad | add eax 01000
            call 'KERNEL32.VirtualAlloc' eax, MEMTABLEVIEWSIZE, &MEM_COMMIT, &PAGE_READWRITE
            test eax eax | je @bad
            mov D$MemTableView eax | add eax MEMTABLEVIEWSIZE | mov D$MemTableViewEnd eax
        .End_If
        mov edi D$MemTable                    ; Search an empty Record.
        While D$edi > 0
            add edi MEMRECORD
          ; This Alert can only happend if i completly corrupt a Mem Pointer Value
          ; so that it is no more founded in the 'MemTable':
            If edi = D$MemTableEnd
                call 'USER32.MessageBoxA' D$hwnd, MemAlertText, MemAlert,
                                          &MB_SYSTEMMODAL__&MB_ICONSTOP
                call ViewRosAsmMems
                ShowMe VirtAllocOverFlowString
                ON D$hwnd <> 0, call 'USER32.DestroyWindow' D$hwnd ; jE!
                call 'KERNEL32.ExitProcess' 0
            End_If
        End_While

      ; The 'add eax GUARDPAGE' is to reserve a never Committed Page (allow Win
      ; error if i overflow write to a Buffer):
        mov eax D@sz | Align_On PAGESIZE eax | add eax GUARDPAGE
        mov D$MemChunSize eax, D$edi+4 eax

      ; VirtualAlloc Reserves by 01_0000 Octets Block (16 Pages). So, as any previous
      ; Committed Block can not be smaller than 0_1000 or 0_2000 Octets (1 Page + 1 Page
      ; for 'Guard', the not used remaining space of any reservation cannot be bigger
      ; than 01_0000-(PAGESIZE+GUARDPAGE) (0_E000 or 0F000).
        If D$MemChunSize <= (01_0000-(PAGESIZE+GUARDPAGE))
            push edi | call IsThereSomeRoom | pop edi   ; >>> eax= 0 or Pointer.
        Else                                            ; eax= Pointer >>> ebx = MotherPtr.

            mov eax 0
        End_If

        If eax = 0
            push edi
                mov eax D$MemChunSize | Align_On 010_000 eax
                call 'KERNEL32.VirtualAlloc' &NULL, eax, &MEM_RESERVE, &PAGE_NOACCESS
                cmp eax 0 | jne L0>
                hexprint D$MemChunSize | pop edi | jmp @bad
L0:         pop edi
            mov D$edi eax, D$edi+8 eax      ; D$edi+8 = Pointer
        Else
            mov D$edi eax, D$edi+8 ebx      ; D$edi+8 = MotherPointer ('IsThereSomeRoom' ebx)
        End_If

        On edi > D$LastMemRecord, mov D$LastMemRecord edi
        mov ecx D$MemChunSize | sub ecx GUARDPAGE
      ;  push eax, ecx                      ; For my Tests
        call 'KERNEL32.VirtualAlloc' eax, ecx, &MEM_COMMIT, &PAGE_READWRITE
      ;  pop ecx, ebx                       ; For my Tests
        If eax = 0
@bad:      ;      hexprint ecx | hexprint ebx    ; For my Tests
            call 'USER32.MessageBoxA' D$hwnd, MemInternalAlert, MemAlert,
                                         &MB_SYSTEMMODAL__&MB_ICONSTOP
            call ViewRosAsmMems
            ShowMe VirtAllocFailureString
            ON D$hwnd <> 0, call 'USER32.DestroyWindow' D$hwnd ; jE!
            call 'KERNEL32.ExitProcess' 0
        End_If
    mov ebx D@ptr
    mov D$ebx eax                           ; Writes Value in Pointer & in EAX!
EndP

[VirtualAlloc | push #1 | push #2 | call VirtAlloc | #+2]
; Evocation: VirtualAlloc Pointer, Size
____________________________________________________________________________________________

[VirtFreeOverFlowString: 'VirtualFree Buffer overflow' 0]

VirtFree:       ; eax = D$Pointer
    pushad

  ; I sometimes increase the original Pointer to set a security top margin allowing
  ; computation begining by 'If D$esi-4 = ...", for example >>> Restore origin:
    and eax 0_FFFF_F000                 ; (Chunks are always Page-aligned).

    mov esi D$MemTable
    While D$esi <> eax
        add esi MEMRECORD
        If esi = D$MemTableEnd
            call 'USER32.MessageBoxA' D$hwnd, MemInternalAlert, MemInternalAlert,
                                      &MB_SYSTEMMODAL__&MB_ICONSTOP
            call ViewRosAsmMems
            ShowMe VirtFreeOverFlowString
            ON D$hwnd <> 0, call 'USER32.DestroyWindow' D$hwnd ; jE!
            call 'KERNEL32.ExitProcess' 0
        End_If
    End_While

    push esi
        mov ecx D$esi+4 | sub ecx GUARDPAGE
        call 'KERNEL32.VirtualFree' D$esi, ecx, &MEM_DECOMMIT
    pop esi

  ; Now, we can Release the whole Memory Block only if no other Chunk is Committed.
  ; If so, no other Block whith the same origin (Third dWord of records) can be
  ; found in the 'MemTable':
    mov ebx D$esi+8, ecx 0
    mov D$esi 0, D$esi+4 0, D$esi+8 0       ; Clear the record, anyway.
    push esi
        mov esi D$MemTable
        While esi <= D$LastMemRecord
            If D$esi+8 = ebx
                inc ecx
            End_If
            add esi MEMRECORD
        End_While
        On ecx = 0, call 'KERNEL32.VirtualFree' ebx, &NULL, &MEM_RELEASE
    pop esi
    On esi = D$LastMemRecord, sub D$LastMemRecord MEMRECORD
L9: popad
ret

[VirtualFree | cmp #1 0 | je V9> | mov eax #1
                    call VirtFree
               mov #1 0 | V9: | #+1]

; Evocation: VirtualFree D$Pointer (>>> D$Pointer = 0, when back).
____________________________________________________________________________________________

; Search is some empty room is available inside already Reserved Chunks. 'D$MemChunSize'
; holds the size of the wanted Block + one Page:

IsThereSomeRoom:
    mov esi D$MemTable
    .While esi <= D$LastMemRecord
        ..If D$esi > 0
            mov eax D$esi | add eax D$esi+4
            mov ebx eax | Align_On 01_0000 eax | sub eax ebx

          ; eax = supposed free room in this 01_0000 aligned reserved Chunk or sub-Chunk.
            .If eax >= D$MemChunSize
                mov eax D$esi | add eax D$esi+4
                mov ebx eax | add ebx D$MemChunSize
              ; eax = Start // ebx = End of possible free Block.
                call IsThisRoomFree
                If eax = &TRUE
                    mov eax D$esi, ebx D$esi+8 | add eax D$esi+4 | jmp L9>>
                End_If
            .End_If
        ..End_If
        add esi MEMRECORD
    .End_While
    mov eax 0
L9: ret

;;
 in: eax = candidate Block start // ebx = end. Both must be outside of any other
 Declared Block. This is to say, down here,  (ebx <= ecx) OR (eax >= edx). Example:
 eax > edx >>> good:

 .....................................(eax)XXXXXXXXXXXXXXXXXXXXX(ebx)......
 ....(ecx)XXXXXXXXXXXXXXXXXXXXX(edx).......................................
;;
IsThisRoomFree:
    push esi
        mov esi D$MemTable
        While esi <= D$LastMemRecord
            mov ecx D$esi, edx ecx | add edx D$esi+4
            If ebx <= ecx
                ; OK
            Else_If eax >= edx
                ; OK
            Else
                mov eax &FALSE | jmp L9>
            End_If
            add esi MEMRECORD
        End_While
        mov eax &TRUE
L9: pop esi
ret

____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________



[ExtendMemory | call NewMemory #1, #2, #3 | #+3]

; AddMemory MemoryPointer, ActualSizePointer, SizeToAdd

Proc NewMemory:
    Arguments @Pointer, @SizePointer, @Added
    [@NewPointer: ?]

        pushad
          ; Compute the new Size:
            mov ecx D@SizePointer, ecx D$ecx | add ecx D@Added

            VirtualAlloc @NewPointer, ecx

          ; Copy from Old to new memory:
            push ecx
                mov esi D@Pointer, esi D$esi, edi eax, ecx D@SizePointer, ecx D$ecx
                shr ecx 2 | rep movsd
            pop ecx

          ; Adjust the new Size:
            mov ebx D@SizePointer, D$ebx ecx

          ; Replace the old Memory Pointer by the new one:
            mov ebx D@Pointer | Exchange D$ebx D@NewPointer

            VirtualFree D@NewPointer
        popad
EndP










