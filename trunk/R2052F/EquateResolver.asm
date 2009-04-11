TITLE EquateResolver

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
    Nested Equate Resolver - Ludwig Haehne <wkx@gmx.li>
    
    Substitute nested equates by its values in the equate table to reduce compilation time
    and memory requirements.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[EquateInflation: B$
"Buffer overrun because of equate inflation detected!

Put expressions inside your equates in brackets to
allow the parser to merge them.

e.g. write [B (A+4)] instead of [B A+4]" 0]

[BackwardNesting: B$
"Maximum equate nesting level exceeded! 
Declare equates *before* you reuse them in other equates!" 0]

[CyclicDependancy: B$ "Cyclic dependency detected!" 0]

[CompilationStopped: B$ 'Compilation stopped' 0]
____________________________________________________________________________________________

; This proc resolves nested equates. It scans all equates texts for other nested equates
; and unfolds these into a new buffer. It uses a mixture of iteration and "fake recursion".
; It makes a linear scan of the equate list and replaces nested equates with its value. If
; a not yet scanned equate is found (e.g. [B A+1, A 1]), it saves the current scan position
; on the stack and proceeds with the nested equate.
; To reduce size and compilation time expressions (which contain '(..)') are merged finally.

; Possible Optimizations:
; * merge expressions without brackets e.g. EBP+4+4+...+4 -> EBP+020
; * faster execution path for plain number equates (no nesting, no operators)

; Buffer where the substituted equate contents are stored
[EquateSubstitutes: ? EquateSubstitutesPtr: ? EquateSubstitutesLimit: ?]
; Remember in case of relocation
[LastEquateList: ? LastEquateListPtr: ?]

; The proc works in this way:
;
; For each equate e in E
;   For each token t in e
;     If IsEquate(t)
;       If IsResolved(t)
;         Substitute t
;       Else
;         Resolve(t)
;       End
;     End
;   Next
;   Merge(e)
; Next

ResolveEquates:

    [@ProcessedLimit:   D$ ? ; address of the last resolved equate (in the iteration !)
     @IsNested:         B$ ? ; equate contains nested equates and/or must be restored
     @NumBrackets:      B$ ? ; number of bracket levels in expression
     @UnpairedBrackets: B$ ? ; non-zero if brackets are unpaired
     @UnknownSymbols:   B$ ? ; number of unresolved symbols (regs, macro emitted equates)
     @ErrorText:        D$ ?]; points at the appropriate error text in case of resolving problems

    ;call 'Kernel32.OutputDebugStringA' {'ResolveEquates' 0}

  ; Set exception handler
    push @AbortCompilation
    push @AVHandler
    push D$fs:0
    mov D$fs:0 esp

      ; If table was relocated through memory extension the pointers must be altered
        mov eax D$LastEquateList, edx D$EquateList
        If eax <> edx
            sub D$LastEquateListPtr eax
            add D$LastEquateListPtr edx
            mov D$LastEquateList edx
        EndIf

        mov esi D$LastEquateListPtr, edi D$EquateSubstitutesPtr

      ; Empty list marker
        push 0
____________________________________________________________________________________________

@GetNextEquate: ; Search for next equate to process

      ; Continue with skipped equate (if any)
        On D$esp <> 0, pop esi

      ; Already dealt with? > Skip
        While B$esi-1 = 04
            mov B$esi-1 EOI
L0:         inc esi
            cmp B$esi LowSigns | ja L0<
            add esi 11
        EndWhile

        mov D@ProcessedLimit esi

      ; Check for end of list (when we finish the todo-stack has to be empty)
        If D$esi = 02020202
            add esp 4 ; clear list marker
            pop D$fs:0 | add esp 8; Clear exception handler

            mov D$EquateSubstitutesPtr edi
            move D$LastEquateListPtr D$EquateListPtr
            ret
        EndIf
____________________________________________________________________________________________

@EvaluateEquate: ; We start here when a not-yet-processed equate was found

      ; Skip name.
        mov ebx esi
        SkipName ebx

      ; ebx, esi > Address (D), Size (D), Flag (B)
      ; Scan the equate tokens, copy and unfold but only redirect the pointer
      ; when there were really nested equates.
        push esi

            mov D@IsNested 0

          ; esi > equate content,  ecx = size,  edx > substitute
            mov esi D$ebx
            mov ecx D$ebx+4
            mov edx edi

          ; We MUST copy the expression or GetFromQwordCheckSum might fail
          ; when there's no separator between the equate strings.
          ; (e.g. "...SummerWinter...") The expression buffer is abused for this.
            If B$esi+ecx >= ' '
                push ecx
                    mov edi D$ExpressionA
                    rep movsb
                    mov B$edi 0
                    mov esi D$ExpressionA
                pop ecx
                mov edi edx
            EndIf

          ; Copy contents and replace nested equates
            .While ecx > 0

L7:             While B$esi <= LowSigns
                    movsb
                    dec ecx | jz L5>>
                    cmp B$esi-1 TextSign | je L4>
                EndWhile

                mov al B$esi

                cmp al '0' | jb L3> ; ?
                cmp al '9' | jbe L4>

L3:           ; The equate string contains a text token. Check if it is an equate
                call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit
                .If eax <> 0

                  ; Was equate already processed? Either it is among the equates resolved
                  ; by linear table processing (indicated by ProcessedLimit) or was tagged
                  ; as being (forward-)resolved with 04.
                    ..If eax >= D@ProcessedLimit
                        If B$eax-1 <> 04
                            mov esi eax
                            mov edi edx
                            call @CheckCyclicDependency
                            jmp @EvaluateEquate ; leave esi on stack so we can later continue with this
                        EndIf
                    ..EndIf

                    mov B@IsNested 1

                  ; Resolved equates contents can be copied.
                    SkipName eax
                    push esi ecx
                        mov esi D$eax
                        mov ecx D$eax+4
                        rep movsb
                    pop ecx esi

                  ; Skip name in the source buffer
                    While B$esi > LowSigns
                        inc esi
                        dec ecx | jz L5>
                    EndWhile
                .EndIf

              ; Copy rest of the token
L4:             While B$esi > LowSigns
                    movsb
                    dec ecx | jz L5>
                EndWhile

            .EndWhile


          ; Finished unfold

L5:       ; Rescan equate contents, count brackets and unresolved symbols.
          ; This is done here to keep the unfold loop simple.
            mov eax edx
            push edx
                mov eax 0
                While edx < edi
                    mov al B$edx
                    If al = openSign
                        inc B@NumBrackets
                        inc B@UnpairedBrackets
                    ElseIf al = closeSign
                        dec B@UnpairedBrackets
                    ElseIf al > '9'
                        On ah < LowSigns,
                            inc B@UnknownSymbols
                    EndIf
                    mov ah al
                    inc edx
                EndWhile
            pop edx

          ; Merge expressions if brackets were found and it is a valid statement
            cmp B@NumBrackets      0 | je L6>
            cmp B@UnpairedBrackets 0 | jne L6>
            cmp B@UnknownSymbols   0 | jne L6>

            push ebx
                movzx ebx B@NumBrackets ; nr of bracket pairs
                ;call OutputExp
                    call @MergeEquate
                ;call OutputExp
            pop ebx
            mov B@IsNested 1

L6:     pop esi

      ; Mark list entry as forward-resolved so it can be skipped later in the linear
      ; processing of the equatelist (the temporary 04 tag is removed then).
        If D$esp <> 0
            mov B$esi-1 04
        EndIf

      ; If the equate contents have been altered (nested substituted, expression merged)
      ; redirect the content pointer, otherwise ignore the copied contents.
        If B@IsNested = 1
            mov D$ebx edx ; redirect
            mov ecx edi
            sub ecx edx
            mov D$ebx+4 ecx
        Else
            mov edi edx ; throw away copied substitute
        EndIf

        mov esi ebx
        add esi 10

    jmp @GetNextEquate
____________________________________________________________________________________________

@AbortCompilation: ; Declaration error, reset old SEH & abort
  ; Reset exception handler
    mov eax D$fs:0
    mov eax D$eax
    mov D$fs:0 eax
  ; Abort
    mov eax D@ErrorText
    mov B$ErrorLevel 13 ; TODO assign meaningful error level
    jmp OutOnError
____________________________________________________________________________________________

; The exception handler deals with buffer overruns due to equate inflations which are most
; likely programming errors. Therefore the compilation is stopped.
; Another possible exception is a stack overflow when an outragous amount of nested equates
; must be resolved.

Proc @AVHandler:
    Arguments @ExceptionRecord, @Error, @ThreadContext

        mov ecx D@ExceptionRecord     ; exception record
        test D$ecx+4 1 | jnz L8>      ; non-continueable exception

      ; Check which type of exception occurred:
      ; We catch access violations that tried to read/write beyond the limit of the
      ; equate substitutes buffer. All other AVs are not dealt with.
        If D$ecx = &EXCEPTION_ACCESS_VIOLATION
            mov eax D$ecx+24
            mov edx D$EquateSubstitutesLimit
            sub eax edx
            cmp eax 01000 | ja L8>
            mov D@ErrorText EquateInflation
            ;call 'User32.MessageBoxA' D$hwnd, EquateInflation, CompilationStopped, &MB_ICONEXCLAMATION

      ; Deal with stack overflows (I've never seen this happening here)
        ElseIf D$ecx = &EXCEPTION_STACK_OVERFLOW
            mov D@ErrorText BackwardNesting
            ;call 'User32.MessageBoxA' D$hwnd, BackwardNesting, CompilationStopped, &MB_ICONEXCLAMATION

      ; Unhandled exceptions are forwarded to the OS
        Else
L8:         mov eax 1 | jmp L9>
        EndIf

      ; Set the instruction pointer to continue with the cleanup routine
        mov ecx D@ThreadContext ; get context record in ecx
        mov edx D@Error         ; get pointer to ERR structure
        mov eax D$edx+8         ; get safe place given in ERR structure
        mov D$ecx+0B8 eax       ; replace instruction pointer

        mov eax 0

L9: mov esp ebp
    pop ebp
ret
____________________________________________________________________________________________

; Check if the referenced equate corresponds to an already listed equate in the
; nesting hierarchy.
; Return address is at [esp], therefore we scan from [esp+4] -> [esp+x]=0

@CheckCyclicDependency:
    mov eax 1
    While D$esp+eax*4 <> 0
        cmp esi D$esp+eax*4 | je L0>
        inc eax
    EndWhile
ret
  ; highlight first equate in the cycle
L0: mov edx D$esp+eax*4, edi edx
    mov ecx 0-1, al EOI
    repne scasb
    mov ebx 0-2 | sub ebx ecx
    call InternSearch
    mov D@ErrorText CyclicDependancy

jmp @AbortCompilation
____________________________________________________________________________________________

; Solve expressions inside an equate and replace with the compressed string.
; All registers must be preserved for the calling routine.
;   edx -> Start of expression
;   edi -> End of expression
;   ebx -> Number of bracket pairs '(...)'
;   (1+((2 shl 3)*0010011))
;   ^                      ^
;  edx     ebx=3          edi

@MergeEquate:
    push eax ecx esi
        push edx
            inc edx
            mov D$StartOfSourceExpression edx
            mov D$StartOfDestinationExpression edi
            mov esi edi
            call ComputeExpression
            mov esi D$ExpressionA
            mov ecx edi
            sub ecx esi
            dec ecx
        pop edx

        mov edi edx
        rep movsb

    pop esi ecx eax
ret
____________________________________________________________________________________________

[SkipName
    S0: inc #1
        cmp B$#1 LowSigns | ja S0<
        inc #1]
____________________________________________________________________________________________

[DebugStr: B$ ? #256]

OutputExp:
    pushad
        mov ecx edi
        sub ecx edx
        mov esi edx
        mov edi DebugStr
        While ecx > 0
            lodsb
            If al = openSign
                mov al '('
            ElseIf al = closeSign
                mov al ')'
            ElseIf al = addSign
                mov al '+'
            ElseIf al = subSign
                mov al '-'
            ElseIf al = divSign
                mov al '/'
            ElseIf al = mulSign
                mov al '*'
            ElseIf al = Space
                mov al ' '
            EndIf
            stosb
            dec ecx
        EndWhile
        mov B$edi 0
        call 'Kernel32.OutputDebugStringA' DebugStr
    popad
ret


