TITLE ShowInfos
___________________________________________________________________________________________
___________________________________________________________________________________________

; Win32 Data Types significations (This is the [Win32 Data Types] from [Help] Menu
; -ShowTypesInfo- and the Right-Click upon Mnemonic feature -ShowMnemonicInfo-):

[TypesTitle: 'Win32 Data Types', 0]

; Tag Dialog 1000

ShowTypes:
    If D$ShowTypesDialogHandle = 0
        call 'USER32.DialogBoxParamA' D$hInstance, 1000, &NULL, ShowTypesInfo, &NULL
    Else
        Beep
    End_If
ret


ShowEquates:
    call SetEquatesEquFileName

    call 'SHELL32.ShellExecuteA' D$hwnd, Open, {'WordPad', 0},
                                 IncludeFileName, &NULL, &SW_SHOWNORMAL
ret


[ShowTypesDialogHandle: ?    FirstCTLCOLOREDIT: ?]

Proc ShowTypesInfo:
  Arguments @Adressee, @Message, @wParam, @lParam
  USES EBX ESI EDI


    .If D@Message = &WM_COMMAND
         If D@wParam = &IDCANCEL
            mov D$ShowTypesDialogHandle 0
            call 'User32.EndDialog' D@Adressee 0
         End_If

    .Else_If D@Message = &WM_INITDIALOG
        move D$ShowTypesDialogHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.SetDlgItemTextA' D@Adressee 100 Win32Types
        call 'USER32.SendMessageA' D@Adressee &WM_SETTEXT &NULL TypesTitle
        mov B$FirstCTLCOLOREDIT &TRUE

    .Else_If D@Message = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            call 'USER32.SendMessageA' D@lParam &EM_SETSEL 0 0
            mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    .Else
        mov eax &FALSE | jmp L9>

    .End_If

    mov eax &TRUE

L9: EndP


[Win32Types: " ACHAR         B$ ; ansi character
 ATOM          D$ ; string atom
 BOOL          D$ ; boolean variable
 COLORREF      D$ ; rgb color
 DWORDLONG     Q$ ; 8 bytes
 GLOBALHANDLE  D$ ; global handle
 HACCEL        D$ ; accelerator handle
 HANDLE        D$ ; unspecified handle
 HBITMAP       D$ ; bitmap handle
 HBRUSH        D$ ; brush handle
 HCOLORSPACE   D$ ; color space handle
 HCURSOR       D$ ; cursor handle
 HDC           D$ ; device context handle
 HDWP          D$ ; defer win pos handle
 HENHMETAFILE  D$ ; enh. metafile handle
 HFILE         D$ ; file handle
 HFONT         D$ ; font handle
 HGLOBAL       D$ ; global handle
 HHOOK         D$ ; hook handle
 HICON         D$ ; icon handle
 HINSTANCE     D$ ; instance handle
 HINTERNET     D$ ; internet handle
 HLOCAL        D$ ; local handle
 HMENU         D$ ; menu handle
 HMETAFILE     D$ ; metafile handle
 HPALETTE      D$ ; palette handle
 HPEN          D$ ; pen handle
 HRGN          D$ ; region handle
 HRSRC         D$ ; resource handle
 HSTR          D$ ; string handle
 HTASK         D$ ; task handle
 HTREEITEM     D$ ; tree view item handle
 HWND          D$ ; window handle
 INTEGER       D$ ; standard integer
 LOCALHANDLE   D$ ; local handle
 LONG          D$ ; long integer
 LONGINT       D$ ; long integer
 LPARAM        D$ ; long parameter
 LPBOOL        D$ ; long ptr to boolean
 LPBYTE        D$ ; long ptr to byte
 LPCSTR        D$ ; long ptr to string
 LPCTSTR       D$ ; long ptr to string
 LPCVOID       D$ ; long ptr to buffer
 LPDWORD       D$ ; long ptr to dword
 LPFN          D$ ; long ptr to function
 LPINT         D$ ; long ptr to integer
 LPLONG        D$ ; long ptr to long int
 LPMSG         D$ ; long pointer to message
 LPPAINTSTRUCT D$ ; long ptr to paint struc
 LPRECT        D$ ; long pointer to rectangle
 LPSTR         D$ ; long ptr to string
 LPTSTR        D$ ; long ptr to string
 LPVOID        D$ ; long ptr to buffer
 LPWORD        D$ ; long ptr to word
 LRESULT       D$ ; long result
 POINTER       D$ ; pointer to anything
 PVOID         D$ ; pointer to buffer
 SHORTINT      W$ ; short integer
 UINT          D$ ; unsigned integer
 WCHAR         W$ ; unicode character
 WNDPROC       D$ ; window procedure
 WPARAM        D$ ; word parameter", 0]
____________________________________________________________________________________________
____________________________________________________________________________________________


; > in from Caller (RightClick) ah = first ORed 32 char // edx > second char // ebx = lenght

[MnemonicIndex: ?]

OldSearchMneMonic:
    pushad
        mov D$MnemonicIndex 7
        cmp ah 'z' | ja L2> | cmp ah 'a' | jb L2> | sub ah 020 ; > upper case (all Upper case in the list
L2:     mov esi OpcodesList
        .While B$esi > 0
            lodsb
            .If al = ah
                pushad | mov ecx ebx, edi edx
L0:             lodsb | mov ah B$edi | inc edi | cmp ah 'z' | ja L2> | cmp ah 'a' | jb L2> | sub ah 020 ; Source upper case.
L2:             cmp ah al | jne L1>
                loop L0<
L1:             popad | jne L3>
                If B$esi+ebx = ' '
                    On D$ShowMnemonicHandle > 0, call 'USER32.EndDialog' D$ShowMnemonicHandle 0
                    call 'USER32.DialogBoxParamA' D$hInstance 1000  &NULL ShowMnemonicInfo  &NULL
                    mov B$MnemonicHelpFound &TRUE | jmp L9>
                End_If
L3:
            .End_If
            While B$esi-1 > LF
                inc esi
            End_While
            inc D$MnemonicIndex
        .End_While
L9:  popad
ret


[MnemonicCopy: ? #4]

; > in from Caller (RightClick):
; ah = first ORed 32 char // edx > second char // ebx = lenght

[MnemonicHelpFound: ?]

SearchMneMonic: On ebx > 14, ret
    pushad
        mov D$MnemonicCopy '    ', D$MnemonicCopy+4 '    ',
            D$MnemonicCopy+8 '    ', D$MnemonicCopy+12 '    '

      ; 1) Make an upper Case copy of the possible Mnemonic (spaces ended):
        dec edx | mov esi edx, edi MnemonicCopy, ecx ebx | inc ecx
L0:     lodsb | On al > 'Z', and eax (not 020) | stosb | loop L0<

      ; Search identical record in 'OpCodeList':
        mov edi MnemonicCopy, esi OpCodeList, ebx D$edi
L0:     lodsd | cmp eax 0 | je L9>>
                cmp eax ebx | je L5>

L4:                 Align_On 16 esi | jmp L0<

L5:                     mov edx edi | add edx 4
                        While B$edx <> ' '
                            lodsd | cmp eax D$edx | jne L4<
L6:                         add edx 4
                        End_While
                        On B$esi <> ' ', jmp L4<

                      ; Step back to Main Mnemonic if '<' encounted:
                        Align_On 16 esi | sub esi 16
                        If B$esi-1 = '<'
                            While B$esi-1 = '<'
                                sub esi 16
                            End_While
                        End_If

                      ; Zero end Case sensitive recopy of the MnemonicCopy for ShellExecute:
                        mov edi MnemonicCopy
                        While B$esi <> ' '
                            movsb
                        End_While
                        mov B$edi 0
                        call Help B_U_AsmName, MnemonicCopy, FileNotFound
                        If eax <= 32
                            jmp L9>
                        Else
                            mov B$MnemonicHelpFound &TRUE
                        End_If
    popad
ret

L9: popad
jmp OldSearchMneMonic


[MnemonicInfoString: ? #40] [MnemonicInfoTitle: ? #10]

[ShowMnemonicHandle: 0    MnemonicsTitle: 'Mnemonics list (More Infos with OpHelp.exe...)', 0]

Proc ShowMnemonicInfo:
  Arguments @Adressee, @Message, @wParam, @lParam
  USES EBX ESI EDI


    .If D@Message = &WM_COMMAND
         If D@wParam = &IDCANCEL
            call 'User32.EndDialog' D@Adressee 0
         End_If

    .Else_If D@Message = &WM_INITDIALOG
        move D$ShowMnemonicHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.SetDlgItemTextA' D@Adressee 100 OpcodesListTitle
        call 'USER32.GetDlgItem' D@Adressee 100
        call 'USER32.SendMessageA' eax &EM_LINESCROLL 0  D$MnemonicIndex
        call 'USER32.SendMessageA' D@Adressee &WM_SETTEXT 0 MnemonicsTitle
        mov B$FirstCTLCOLOREDIT &TRUE

    .Else_If D@Message = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            call 'USER32.SendMessageA' D@lParam &EM_SETSEL 0 0
            mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    .Else
        mov eax &FALSE | jmp L9>

    .End_If

    mov eax &TRUE

L9: EndP

____________________________________________________________________________________________
____________________________________________________________________________________________


[ApiTitle: 'Api call infos' 0]

; Tag Dialog 1000

Proc ShowApiInfo:
  Arguments @Adressee, @Message, @wParam, @lParam
  USES EBX ESI EDI


    .If D@Message = &WM_COMMAND
         If D@wParam = &IDCANCEL
            call 'User32.EndDialog' D@Adressee 0
         End_If

    .Else_If D@Message = &WM_INITDIALOG
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon
        call 'USER32.SetDlgItemTextA' D@Adressee 100  OneApiInfo
        call 'USER32.SendMessageA' D@Adressee &WM_SETTEXT 0 ApiTitle
        mov B$FirstCTLCOLOREDIT &TRUE

    .Else_If D@Message = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            call 'USER32.SendMessageA' D@lParam &EM_SETSEL 0 0
            mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        mov eax D$DialogsBackGroundBrushHandle | jmp L9>

    .Else
        mov eax &FALSE | jmp L9>

    .End_If

    mov eax &TRUE

L9: EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[OpcodesListTitle: B$
"            ************************************************
            *  The Complete Pentium Instruction Set Table  *
            *      - Thanks to Sang Cho (SV 1999) -        *
            *==============================================*
            *    Ripped from exe and converted into this   *
            *         text format by Test Department       *
            ************************************************
"


OpcodesList: B$ "
AAA                  37           ASCII adjust AL after addition       
    > mov al 09, bl 02 | add al bl | AAA ; ax = BCD(0101)

AAD                  D5 0A        ASCII adjust AX before division      
    > mov ax 0102 | AAD ; ax = 12

AAM                  D4 0A        ASCII adjust AX after multiplication 
    > mov al 7 | mov cx 8 | mul cx | aam ; ax = 0506

AAS                  3F           ASCII adjust AL after subtraction    
    > sub al bl | aas | jc CarryFlagSet

ADC AL,imm8          14       ib  Add with carry
    > ... To be continued by who wants to...

ADC EAX,imm32        15       id  Add with carry
    > ... Search for 'OpcodesList' Label in RosAsm Source.

ADC r/m8,imm8        80    /2 ib  Add with carry
    > ...

ADC r/m32,imm32      81    /2 id  Add with carry
    > ...

ADC r/m32,imm8       83    /2 ib  Add with carry
    > ...

ADC r/m8,r8          10    /r     Add with carry
    > ...

ADC r/m32,r32        11    /r     Add with carry
ADC r8,r/m8          12    /r     Add with carry 
ADC r32,r/m32        13    /r     Add with carry
ADCX r32,r/m32       66 0F 38 F6 /r    Add with CF, writes only CF.
ADD AL,imm8          04       ib  Add 
ADD EAX,imm32        05       id  Add 
ADD r/m8,imm8        80    /0 ib  Add 
ADD r/m32,imm32      81    /0 id  Add 
ADD r/m32,imm8       83    /0 ib  Add 
ADD r/m8,r8          00    /r     ADD 
ADD r/m32,r32        01    /r     ADD 
ADD r8,r/m8          02    /r     ADD 
ADD r32,r/m32        03    /r     ADD 
ADOX r32,r/m32       F3 0F 38 F6 /r    Add with OF, writes only OF.
AESDEC XMM,XMM/m128     66 0F 38 DE /r
AESDECLAST XMM,XMM/m128 66 0F 38 DF /r
AESENC XMM,XMM/m128     66 0F 38 DC /r
AESENCLAST XMM,XMM/m128 66 0F 38 DD /r
AESIMC XMM,XMM/m128     66 0F 38 DB /r
AESKEYGENASSIST XMM,XMM/m128,imm8   66 0F 3A DF    /r
AND AL,imm8          24       ib  AND 
AND EAX,imm32        25       id  AND
AND r/m8,imm8        80    /4 ib  AND
AND r/m32,imm32      81    /4 id  AND
AND r/m32,imm8       83    /4 ib  AND
AND r/m8,r8          20    /r     AND
AND r/m32,r32        21    /r     AND
AND r8,r/m8          22    /r     AND
AND r32,r/m32        23    /r     AND
ARPL r/m16,r16       63    /r     Adjust Request Privilege Level of Sel.
BLENDPD XMM,XMM/m128,imm8    66 0F 3A 0D /r
BLENDPS XMM,XMM/m128,imm8    66 0F 3A 0C /r
BLENDVPD XMM1,XMM2/m128,<xmm0>  66 0F 38 15 /r
BLENDVPS XMM1,XMM2/m128,<xmm0>  66 0F 38 14 /r
BOUND r32,m32&32     62    /r     Check Array Index Against Bounds
BSF r32,r/m32        0F BC /r     Bit scan forward on r/m32
BSR r32,r/m32        0F BD /r     Bit scan reverse on r/m32
BSWAP r32            0F C8+rd     Reverses the byte order of a r32
BT r/m32,r32         0F A3 /r     Bit Test
BT r/m32,imm8        0F BA /4 ib  Bit Test
BTC r/m32,r32        0F BB /r     Bit Test and Complement
BTC r/m32,imm8       0F BA /7 ib  Bit Test and Complement
BTR r/m32,r32        0F B3 /r     Bit Test and Clear
BTR r/m32,imm8       0F BA /6 ib  Bit Test and Clear
BTS r/m32,r32        0F AB /r     Bit Test and Set
BTS r/m32,imm8       0F BA /5 ib  Bit Test and Set
CALL rel32           E8       cd  Call near, rel to n.inst
CALL r/m32           FF    /2     Call near, abs.ind.add. given in r/m32
CALL ptr16:32        9A       cp  Call far, abs.add. given in operand
CALL m16:32          FF    /3     Call far, abs.ind.add. given in m16:32
CBW                  98           Convert Byte to Word
CWD                  99           Convert Word to Doubleword
CDQ                  99           Convert Doubleword to Quadword 
CLAC                 0F 01 CA     Clear AC flag
CLC                  F8           Clear CF flag
CLD                  FC           Clear DF flag
CLFLUSHOPT m8        66 0F AE /7  Flushes cache at Mem8.
CLI                  FA           Clear interrupt flag
CLTS                 0F 06        Clear Task-Switched Flag in Control Reg. Zero
CLWB m8              66 0F AE /6  Writes back cache line at Mem8,
CMC                  F5           Complement CF flag
CMOVA r32,r/m32      0F 47 /r     Move if above 
CMOVAE r32,r/m32     0F 43 /r     Move if above or equal 
CMOVB r32,r/m32      0F 42 /r     Move if below 
CMOVBE r32,r/m32     0F 46 /r     Move if below or equal 
CMOVC r32,r/m32      0F 42 /r     Move if carry 
CMOVE r32,r/m32      0F 44 /r     Move if equal 
CMOVG r32,r/m32      0F 4F /r     Move if greater 
CMOVGE r32,r/m32     0F 4D /r     Move if greater or equal 
CMOVL r32,r/m32      0F 4C /r     Move if less 
CMOVLE r32,r/m32     0F 4E /r     Move if less or equal 
CMOVNA r32,r/m32     0F 46 /r     Move if not above 
CMOVNAE r32,r/m32    0F 42 /r     Move if not above or equal 
CMOVNB r32,r/m32     0F 43 /r     Move if not below 
CMOVNBE r32,r/m32    0F 47 /r     Move if not below or equal 
CMOVNC r32,r/m32     0F 43 /r     Move if not carry 
CMOVNE r32,r/m32     0F 45 /r     Move if not equal 
CMOVNG r32,r/m32     0F 4E /r     Move if not greater 
CMOVNGE r32,r/m32    0F 4C /r     Move if not greater or equal 
CMOVNL r32,r/m32     0F 4D /r     Move if not less 
CMOVNLE r32,r/m32    0F 4F /r     Move if not less or equal 
CMOVNO r32,r/m32     0F 41 /r     Move if not overflow 
CMOVNP r32,r/m32     0F 4B /r     Move if not parity 
CMOVNS r32,r/m32     0F 49 /r     Move if not sign 
CMOVNZ r32,r/m32     0F 45 /r     Move if not zero 
CMOVO r32,r/m32      0F 40 /r     Move if overflow 
CMOVP r32,r/m32      0F 4A /r     Move if parity 
CMOVPE r32,r/m32     0F 4A /r     Move if parity even 
CMOVPO r32,r/m32     0F 4B /r     Move if parity odd 
CMOVS r32,r/m32      0F 48 /r     Move if sign 
CMOVZ r32,r/m32      0F 44 /r     Move if zero 
CMP AL,imm8          3C       ib  Compare 
CMP EAX,imm32        3D       id  Compare 
CMP r/m8,imm8        80    /7 ib  Compare 
CMP r/m32,imm32      81    /7 id  Compare 
CMP r/m32,imm8       83    /7 ib  Compare 
CMP r/m8,r8          38    /r     Compare 
CMP r/m32,r32        39    /r     Compare 
CMP r8,r/m8          3A    /r     Compare 
CMP r32,r/m32        3B    /r     Compare 
CMPSB                A6           Compare byte at ESI byte at with EDI 
CMPSD                A7           Compare dword  at ESI byte at with EDI
CMPXCHG r/m8,r8      0F B0 /r     Compare and Exchange
CMPXCHG r/m32,r32    0F B1 /r     Compare and Exchange
CMPXCHG8B m64        0F C7 /1 m64 Compare and Exchange
CPUID                0F A2        EAX := Processor id.info.
CRC32 r32,r8/m8   F2 0F 38 F0 /r  Accumulate CRC32 of r8/m8.
CRC32 r32,r16/m16 F2 0F 38 F1 /r  Accumulate CRC32 of r16/m16.
CRC32 r32,r32/m32 F2 0F 38 F1 /r  Accumulate CRC32 of r32/m32.
DAA                  27           Decimal adjust AL after addition
DAS                  2F           Decimal adjust AL after subtraction
DEC r/m8             FE    /1     Decrement r/m8 by 1
DEC r/m32            FF    /1     Decrement r/m32 by 1
DEC r32              48+rd        Decrement r32 by 1
DIV r/m8             F6    /6     Unsigned divide AX by r/m8
DIV r/m16            F7    /6     Unsigned divide DX:AX by r/m16
DIV r/m32            F7    /6     Unsigned divide EDX:EAX by r/m32 
DPPD XMM,XMM/m128,imm8    66 0F 3A 41 /r ib
DPPS XMM,XMM/m128,imm8    66 0F 3A 40 /r ib
EMMS                 0F 77        Set the FP tag word to empty
ENTER imm16,0        C8     iw 00 Create a stack frame for a procedure
ENTER imm16,imm8     C8     iw ib Create a nested stack frame for a proc.
EXTRACTPS r/m32,xmm,imm8   66 0F 3A 17 /r ib    Extract Packed Single-FP Value
F2XM1                D9 F0        Replace ST0 with 2**ST0 - 1
FABS                 D9 E1        Replace ST0 with its absolute value
FADD m32real         D8    /0     Add m32real to ST0 and s.r. in ST0
FADD m64real         DC    /0     Add m64real to ST0 and s.r.in ST0
FADD ST(0),ST(i)     D8 C0+i      Add ST0 to STi and s.r.in ST0
FADD ST(i),ST(0)     DC C0+i      Add STi to ST0 and s.r. in STi
FADDP ST(i),ST(0)    DE C0+i      Add ST0 to STi, s.r.in STi,pop r.stack
FADDP                DE C1        Add ST0 to ST1, s.r.in ST1,pop r.stack
FIADD m32int         DA    /0     Add m32int to ST0 and s.r.in ST0
FIADD m16int         DE    /0     Add m16int to ST0 and s.r.in ST0
FBLD m80bcd          DF    /4     Convert m80BCD to real and push 
FBSTP m80bcd         DF    /6     Store ST0 in m80bcd and pop ST0
FCHS                 D9 E0        Complements sign of ST0
FCLEX                9B DB E2     Clear f.e.f. after checking for ..
FNCLEX               DB E2        Clear f.e.f. without checking for ..
FCMOVB ST(0),ST(i)   DA C0+i      Move if below 
FCMOVE ST(0),ST(i)   DA C8+i      Move if equal 
FCMOVBE ST(0),ST(i)  DA D0+i      Move if below or equal 
FCMOVU ST(0),ST(i)   DA D8+i      Move if unordered 
FCMOVNB ST(0),ST(i)  DB C0+i      Move if not below 
FCMOVNE ST(0),ST(i)  DB C8+i      Move if not equal 
FCMOVNBE ST(0),ST(i) DB D0+i      Move if not below or equal 
FCMOVNU ST(0),ST(i)  DB D8+i      Move if not unordered 
FCOM m32real         D8    /2     Compare ST0 with m32real.
FCOM m64real         DC    /2     Compare ST0 with m64real.
FCOM ST(i)           D8 D0+i      Compare ST0 with STi.
FCOM                 D8 D1        Compare ST0 with ST1.
FCOMP m32real        D8    /3     Compare ST0 with m32real,pop r.stack.
FCOMP m64real        DC    /3     Compare ST0 with m64real,pop r.stack.
FCOMP ST(i)          D8 D8+i      Compare ST0 with STi, pop 
FCOMP                D8 D9        Compare ST0 with ST1, pop 
FCOMPP               DE D9        Compare ST0 with ST1, pop pop
FCOMI ST,ST(i)       DB F0+i      Compare ST0 with STi, set status flags
FCOMIP ST,ST(i)      DF F0+i      Compare ST0 with STi, set s.f. ,pop 
FUCOMI ST,ST(i)      DB E8+i      Compare ST0 with STi, check o.v.set s.f.
FUCOMIP ST,ST(i)     DF E8+i      Compare ST0 with STi, check ovssf pop 
FCOS                 D9 FF        Replace ST0 with its cosine
FDECSTP              D9 F6        Decrement TOP field in FPU status word.
FDIV m32real         D8    /6     Divide ST0 by m32real and s.r.in ST0
FDIV m64real         DC    /6     Divide ST0 by m64real and s.r.in ST0
FDIV ST(0),ST(i)     D8 F0+i      Divide ST0 by STi and s.r.in ST0
FDIV ST(i),ST(0)     DC F8+i      Divide STi by ST0 and s.r.in STi
FDIVP ST(i),ST(0)    DE F8+i      Divide STi by ST0, s.r.in STi pop 
FDIVP                DE F9        Divide ST1 by ST0, s.r.in ST1 pop 
FIDIV m32int         DA    /6     Divide ST0 by m32int and s.r.in ST0
FIDIV m16int         DE    /6     Divide ST0 by m64int and s.r.in ST0
FDIVR m32real        D8    /7     Divide m32real by ST0 and s.r.in ST0
FDIVR m64real        DC    /7     Divide m64real by ST0 and s.r.in ST0
FDIVR ST(0),ST(i)    D8 F8+i      Divide STi by ST0 and s.r.in ST0
FDIVR ST(i),ST(0)    DC F0+i      Divide ST0 by STi and s.r.in STi
FDIVRP ST(i),ST(0)   DE F0+i      Divide ST0 by STi, s.r.in STi pop 
FDIVRP               DE F1        Divide ST0 by ST1, s.r.in ST1 pop 
FIDIVR m32int        DA    /7     Divide m32int by ST0 and s.r.in ST0
FIDIVR m16int        DE    /7     Divide m64int by ST0 and s.r.in ST0
FFREE ST(i)          DD C0+i      Sets tag for STi to empty
FICOM m16int         DE    /2     Compare ST0 with m16int
FICOM m32int         DA    /2     Compare ST0 with m32int
FICOMP m16int        DE    /3     Compare ST0 with m16int and pop 
FICOMP m32int        DA    /3     Compare ST0 with m32int and pop 
FILD m16int          DF    /0     Push m16int 
FILD m32int          DB    /0     Push m32int 
FILD m64int          DF    /5     Push m64int 
FINCSTP              D9 F7        Increment the TOP field FPU status r.
FINIT                9B DB E3     Initialize FPU after ...
FNINIT               DB E3        Initialize FPU without ...
FIST m16int          DF    /2     Store ST0 in m16int
FIST m32int          DB    /2     Store ST0 in m32int
FISTP m16int         DF    /3     Store ST0 in m16int and pop 
FISTP m32int         DB    /3     Store ST0 in m32int and pop 
FISTP m64int         DF    /7     Store ST0 in m64int and pop 
FLD m32real          D9    /0     Push m32real 
FLD m64real          DD    /0     Push m64real 
FLD m80real          DB    /5     Push m80real 
FLD ST(i)            D9 C0+i      Push STi 
FLD1                 D9 E8        Push +1.0 
FLDL2T               D9 E9        Push log2 10 
FLDL2E               D9 EA        Push log2 = 
FLDPI                D9 EB        Push pi 
FLDLG2               D9 EC        Push log10 2 
FLDLN2               D9 ED        Push loge 2 
FLDZ                 D9 EE        Push +0.0 
FLDCW m2byte         D9    /5     Load FPU control word from m2byte
FLDENV m14/28byte    D9    /4     Load FPU environment from m14/m28
FMUL m32real         D8    /1     Multiply ST0 by m32real and s.r.in ST0
FMUL m64real         DC    /1     Multiply ST0 by m64real and s.r.in ST0
FMUL ST(0),ST(i)     D8 C8+i      Multiply ST0 by STi and s.r.in ST0
FMUL ST(i),ST(0)     DC C8+i      Multiply STi by ST0 and s.r.in STi
FMULP ST(i),ST(0)    DE C8+i      Multiply STi by ST0, s.r.in STi pop 
FMULP                DE C9        Multiply ST1 by ST0, s.r.in ST1 pop 
FIMUL m32int         DA    /1     Multiply ST0 by m32int and s.r.in ST0
FIMUL m16int         DE    /1     Multiply ST0 by m16int and s.r.in ST0
FNOP                 D9 D0        No operation is performed
FPATAN               D9 F3        Repalces ST1 with arctan(ST1/ST0) pop 
FPREM                D9 F8        Replaces ST0 with rem (ST0/ST1)
FPREM1               D9 F5        Replaces ST0 with IEEE rem(ST0/ST1)
FPTAN                D9 F2        Replaces ST0 with its tangent push 1.0
FRNDINT              D9 FC        Round ST0 to an integer
FRSTOR m94/108byte   DD    /4     Load FPU status from m94 or m108 byte
FSAVE m94/108byte    9B DD /6     Store FPU status to m94 or m108
FNSAVE m94/108byte   DD    /6     Store FPU environment to m94 or m108
FSCALE               D9 FD        Scale ST0 by ST1
FSIN                 D9 FE        Replace ST0 with its sine
FSINCOS              D9 FB        Compute sine and consine of ST0 s push c
FSQRT                D9 FA        square root of ST0
FST m32real          D9    /2     Copy ST0 to m32real
FST m64real          DD    /2     Copy ST0 to m64real
FST ST(i)            DD D0+i      Copy ST0 to STi
FSTP m32real         D9    /3     Copy ST0 to m32real and pop 
FSTP m64real         DD    /3     Copy ST0 to m64real and pop 
FSTP m80real         DB    /7     Copy ST0 to m80real and pop 
FSTP ST(i)           DD D8+i      Copy ST0 to STi and pop 
FSTCW m2byte         9B D9 /7     Store FPU control word
FNSTCW m2byte        D9    /7     Store FPU control word without
FSTENV m14/28byte    9B D9 /6     Store FPU environment
FNSTENV m14/28byte   D9    /6     Store FPU env without
FSTSW m2byte         9B DD /7     Store FPU status word at m2byte after 
FSTSW AX             9B DF E0     Store FPU status word in AX  after 
FNSTSW m2byte        DD    /7     Store FPU status word at m2byte without 
FNSTSW AX            DF E0        Store FPU status word in AX without 
FSUB m32real         D8    /4     Sub m32real from ST0 and s.r.in ST0
FSUB m64real         DC    /4     Sub m64real from ST0 and s.r.in ST0
FSUB ST(0),ST(i)     D8 E0+i      Sub STi from ST0 and s.r.in ST0
FSUB ST(i),ST(0)     DC E8+i      Sub ST0 from STi and s.r.in STi
FSUBP ST(i),ST(0)    DE E8+i      Sub ST0 from STi, s.r.in STi pop
FSUBP                DE E9        Sub ST0 from ST1, s.r.in ST1 pop 
FISUB m32int         DA    /4     Sub m32int from ST0 and s.r.in ST0
FISUB m16int         DE    /4     Sub m16int from ST0 and s.r.in ST0
FSUBR m32real        D8    /5     Sub ST0 from m32real and s.r.in ST0
FSUBR m64real        DC    /5     Sub ST0 from m64real and s.r.in ST0
FSUBR ST(0),ST(i)    D8 E8+i      Sub ST0 from STi and s.r.in ST0
FSUBR ST(i),ST(0)    DC E0+i      Sub STi from ST0 and s.r.in STi
FSUBRP ST(i),ST(0)   DE E0+i      Sub STi from ST0, s.r. in STi) pop 
FSUBRP               DE E1        Sub ST1 from ST0, s.r.in ST1 pop 
FISUBR m32int        DA    /5     Sub ST0 from m32int and s.r.in ST0
FISUBR m16int        DE    /5     Sub ST0 from m16int and s.r.in ST0
FTST                 D9 E4        Compare ST0 with 0.0
FUCOM ST(i)          DD E0+i      Compare ST0 with STi
FUCOM                DD E1        Compare ST0 with ST1
FUCOMP ST(i)         DD E8+i      Compare ST0 with STi and pop 
FUCOMP               DD E9        Compare ST0 with ST1 and pop 
FUCOMPP              DA E9        Compare ST0 with ST1 and pop pop
FXAM                 D9 E5        Classify value or number in ST0
FXCH ST(i)           D9 C8+i      Exchange ST0 and STi
FXCH                 D9 C9        Exchange ST0 and ST1
FXTRACT              D9 F4        Seperate value in ST(0) exp. and sig.
FYL2X                D9 F1        Replace ST1 with ST1*log2(ST0) and pop
FYL2XP1              D9 F9        Replace ST1 with ST1*log2(ST0+1) pop
GETSEC               0F 37        GETSEC LEAF FUNCTIONS; finction IDs in EAX
HLT                  F4           Halt
IDIV r/m8            F6    /7     Divide   
IDIV r/m32           F7    /7     Divide  
IMUL r/m8            F6    /5     Multiply
IMUL r/m32           F7    /5     Multiply
IMUL r32,r/m32       0F AF /r     Multiply
IMUL r32,r/m32,imm8  6B    /r ib  Multiply
IMUL r32,imm8        6B    /r ib  Multiply
IMUL r32,r/m32,imm32 69    /r id  Multiply
IMUL r32,imm32       69    /r id  Multiply
IN AL,imm8           E4       ib  Input byte from imm8 I/O port address into AL
IN EAX,imm8          E5       ib  Input byte from imm8 I/O port address into EAX
IN AL,DX             EC           Input byte from I/O port in DX into AL
IN EAX,DX            ED           Input doubleword from I/O port in DX into EAX
INC r/m8             FE    /0     Increment 1
INC r/m32            FF    /0     Increment 1
INC r32              40+rd        Increment register by 1
INS m8               6C           Input byte from I/O(DX) into  (E)DI
INS m32              6D           Input dw from I/O(DX) into (E)DI
INSERTPS xmm,xmm/m32,imm8   66 0F 3A 21 /r ib   SSE4_1
INT 3                CC           Interrupt 3--trap to debugger
INT imm8             CD       ib  Interrupt vector number (imm8)
INTO                 CE           Interrupt 4--if overflow flag is 1
INVD                 0F 08        Flush internal caches
INVEPT r32,m128      66 0F 38 80  Invalidates EPT-derived entries in the TLBs; VMX
INVLPG m             0F 01 /7     Invalidate TLB Entry for page (m)
INVVPID r32,m128     66 0F 38 81  Invalidates TLBs and caches based on VPID; VMX
IRETD                CF           Interrupt return(32)
JA rel8              77       cb  Jump short if above 
JAE rel8             73       cb  Jump short if above or equal 
JBE rel8             76       cb  Jump short if below or equal 
JC rel8              72       cb  Jump short if carry 
JECXZ rel8           E3       cb  Jump short if ECX register is 0
JE rel8              74       cb  Jump short if equal 
JG rel8              7F       cb  Jump short if greater 
JGE rel8             7D       cb  Jump short if greater or equal 
JL rel8              7C       cb  Jump short if less 
JLE rel8             7E       cb  Jump short if less or equal 
JNE rel8             75       cb  Jump short if not equal 
JNO rel8             71       cb  Jump short if not overflow 
JNS rel8             79       cb  Jump short if not sign 
JO rel8              70       cb  Jump short if overflow 
JPE rel8             7A       cb  Jump short if parity even 
JPO rel8             7B       cb  Jump short if parity odd 
JS rel8              78       cb  Jump short if sign 
JA rel32             0F 87    cd  Jump near if above 
JAE rel32            0F 83    cd  Jump near if above or equal 
JB rel32             0F 82    cd  Jump near if below 
JBE rel32            0F 86    cd  Jump near if below or equal 
JE rel32             0F 84    cd  Jump near if equal 
JG rel32             0F 8F    cd  Jump near if greater 
JGE rel32            0F 8D    cd  Jump near if greater or equal 
JL rel32             0F 8C    cd  Jump near if less 
JLE rel32            0F 8E    cd  Jump near if less or equal 
JNE rel32            0F 85    cd  Jump near if not equal 
JNO rel32            0F 81    cd  Jump near if not overflow 
JNS rel32            0F 89    cd  Jump near if not sign 
JO rel32             0F 80    cd  Jump near if overflow 
JPE rel32            0F 8A    cd  Jump near if parity even 
JPO rel32            0F 8B    cd  Jump near if parity odd 
JS rel32             0F 88    cd  Jump near if sign 
JMP rel8             EB       cb  Jump short, relative, 
JMP rel32            E9       cd  Jump near, relative, 
JMP r/m32            FF    /4     Jump near, abs.ind.in r/m32
JMP ptr16:32         EA       cp  Jump far, abs.add given in operand
JMP m16:32           FF    /r     Jump far, abs.ind.in m16:32
LAHF                 9F           Load Status Flags into AH 
LAR r32,r/m32        0F 02 /r     Load Access Rights Byte     
LDS ds:r32,m16:32    C5    /r     Load DS:r32 with far ptr
LEA r32,m            8D    /r     Load effective address  
LEAVE                C9           Set ESP to EBP, then pop EBP
LES es:r32,m16:32    C4    /r     Load ES:r32 with far ptr 
LFS fs:r32,m16:32    0F B4 /r     Load FS:r32 with far ptr
LGDT m16&32          0F 01 /2     Load m into GDTR
LGS gs:r32,m16:32    0F B5 /r     Load GS:r32 with far ptr
LIDT m16&32          0F 01 /3     Load m into IDTR
LLDT r/m16           0F 00 /2     Load segment selector r/m16 into LDTR
LMSW r/m16           0F 01 /6     Load r/m16 in machine status word of CR0
LOCK                 F0           Asserts LOCK signal for duration ..
LODSB                AC           Load byte at address ESI into AL
LODSD                AD           Load dword at address ESI into EAX
LODSW                AD           Load word at address ESI into AX
LOOP rel8            E2       cb  Dec count;jump if count # 0
LOOPE rel8           E1       cb  Dec count;jump if count # 0 and ZF=1
LOOPZ rel8           E1       cb  Dec count;jump if count # 0 and ZF=1
LOOPNE rel8          E0       cb  Dec count;jump if count # 0 and ZF=0
LOOPNZ rel8          E0       cb  Dec count;jump if count # 0 and ZF=0
LSL r16,r/m16        0F 03 /r     Load Segment Limit
LSL r32,r/m32        0F 03 /r     Load Segment Limit
LSS ss:r32,m16:32    0F B2 /r     Load SS:r32 with far ptr
LTR r/m16            0F 00 /3     Load Task Register
LZCNT r16,r/m16      F3 0F BD /r  Count Leading Zero Bits
LZCNT r32,r/m32      F3 0F BD /r  Count Leading Zero Bits
MOV r/m8,r8          88    /r     Move 
MOV r/m32,r32        89    /r     Move 
MOV r8,r/m8          8A    /r     Move 
MOV r32,r/m32        8B    /r     Move 
MOV r/m16,Sreg**     8C    /r     Move segment register to r/m16
MOV Sreg,r/m16**     8E    /r     Move r/m16 to segment register
MOV AL, moffs8*      A0           Move byte at ( seg:offset) to AL
MOV AX, moffs16*     A1           Move word at ( seg:offset) to AX
MOV EAX, moffs32*    A1           Move dword at ( seg:offset) to EAX
MOV moffs8*,AL       A2           Move AL to ( seg:offset)
MOV moffs16*,AX      A3           Move AX to ( seg:offset)
MOV moffs32*,EAX     A3           Move EAX to ( seg:offset)
MOV r8,imm8          B0+rb        Move imm8 to r8
MOV r32,imm32        B8+rd        Move imm32 to r32
MOV r/m8,imm8        C6    /0 ib  Move imm8 to r/m8
MOV r/m32,imm32      C7    /0 id  Move imm32 to r/m32
MOV CR0, r32         0F 22 /r     Move r32 to CR0
MOV CR2, r32         0F 22 /r     Move r32 to CR2
MOV CR3, r32         0F 22 /r     Move r32 to CR3
MOV CR4, r32         0F 22 /r     Move r32 to CR4
MOV r32,CR0          0F 20 /r     Move CR0 to r32
MOV r32,CR2          0F 20 /r     Move CR2 to r32
MOV r32,CR3          0F 20 /r     Move CR3 to r32
MOV r32,CR4          0F 20 /r     Move CR4 to r32
MOV r32,DR0-DR7      0F 21 /r     Move debug register to r32
MOV DR0-DR7,r32      0F 23 /r     Move r32 to debug register
MOVBE r/m16,r16      0F 38 F0     Swap bytes in r16 and move to r/m16
MOVBE r/m32,r32      0F 38 F0     Swap bytes in r32 and move to r/m32
MOVBE r16,r/m16      0F 38 F1     Swap bytes in r/m16 and move to r16
MOVBE r32,r/m32      0F 38 F1     Swap bytes in r/m32 and move to r32
MOVD mm,r/m32        0F 6E /r     Move doubleword from r/m32 to mm
MOVD r/m32,mm        0F 7E /r     Move doubleword from mm to r/m32
MOVNTDQA XMM,m128    66 0F 38 2A  SSE4_1
MOVQ mm,mm/m64       0F 6F /r     Move quadword from mm/m64 to mm
MOVQ mm/m64,mm       0F 7F /r     Move quadword from mm to mm/m64
MOVSB                A4           Move byte at ESI to  EDI
MOVSD                A5           Move dword at ESI to  EDI
MOVSW                A5           Move word at ESI to  EDI
MOVSX r32,r/m8       0F BE /r     Move byte to doubleword, sign-extension
MOVSX r32,r/m16      0F BF /r     Move word to doubleword, sign-extension
MOVZX r32,r/m8       0F B6 /r     Move byte to doubleword, zero-extension
MOVZX r32,r/m16      0F B7 /r     Move word to doubleword, zero-extension
MPSADBW xmm,xmm/m128,imm8    66 0F 3A 42 /r ib   SSE4_1
MUL r/m8             F6    /4     Unsigned multiply 
MUL r/m32            F7    /4     Unsigned multiply 
NEG r/m8             F6    /3     Two's complement negate r/m8
NEG r/m32            F7    /3     Two's complement negate r/m32
NOP                  90           No operation
NOT r/m8             F6    /2     Reverse each bit of r/m8
NOT r/m32            F7    /2     Reverse each bit of r/m32
OR AL,imm8           0C       ib  OR
OR EAX,imm32         0D       id  OR 
OR r/m8,imm8         80    /1 ib  OR 
OR r/m32,imm32       81    /1 id  OR 
OR r/m32,imm8        83    /1 ib  OR 
OR r/m8,r8           08    /r     OR 
OR r/m32,r32         09    /r     OR 
OR r8,r/m8           0A    /r     OR 
OR r32,r/m32         0B    /r     OR 
OUT imm8,AL          E6       ib  Output byte in AL to I/O(imm8)
OUT imm8,EAX         E7       ib  Output dword in EAX to I/O(imm8)
OUT DX,AL            EE           Output byte in AL to I/O(DX)
OUT DX,EAX           EF           Output dword in EAX to I/O(DX)
OUTS DX,m8           6E           Output byte from (E)SI to I/O(DX)
OUTS DX,m32          6F           Output dword from (E)SI to I/O (DX)
PABSB mm,mm          0F 38 1C     SSSE3
PABSB xmm,xmm        66 0F 38 1C  SSSE3
PABSD mm,mm          0F 38 1E     SSSE3
PABSD xmm,xmm        66 0F 38 1E  SSSE3
PABSW mm,mm          0F 38 1D     SSSE3
PABSW xmm,xmm        66 0F 38 1D  SSSE3
PACKSSWB mm,mm/m64   0F 63 /r     Pack with Signed Saturation
PACKSSDW mm,mm/m64   0F 6B /r     Pack with Signed Saturation
PACKUSDW xmm,xmm/m128   66 0F 38 2B    SSE4_1
PACKUSWB mm,mm/m64   0F 67 /r     Pack with Unsigned Saturation
PADDB mm,mm/m64      0F FC /r     Add packed bytes 
PADDW mm,mm/m64      0F FD /r     Add packed words 
PADDD mm,mm/m64      0F FE /r     Add packed dwords 
PADDSB mm,mm/m64     0F EC /r     Add signed packed bytes 
PADDSW mm,mm/m64     0F ED /r     Add signed packed words 
PADDUSB mm,mm/m64    0F DC /r     Add unsigned pkd bytes 
PADDUSW mm,mm/m64    0F DD /r     Add unsigned pkd words 
PALIGNR mm,mm/m64,imm8     0F 3A 0F  SSSE3
PALIGNR xmm,xmm/m128,imm8  66 0F 3A 0F  SSSE3
PAND mm,mm/m64       0F DB /r     AND quadword from .. to ..
PANDN mm,mm/m64      0F DF /r     And qword from .. to NOT qw in mm
PBLENDVB xmm,xmm/m128,<XMM0>  66 0F 38 10 /r    SSE4_1
PBLENDW  xmm,xmm/m128,imm8    66 0F 3A 0E /r ib   SSE4_1
PCLMULQDQ xmm,xmm/imm128,imm8    66 0F 3A 44 /r ib    SSE4_1
PCMPEQB mm,mm/m64    0F 74 /r     Packed Compare for Equal
PCMPEQW mm,mm/m64    0F 75 /r     Packed Compare for Equal
PCMPEQD mm,mm/m64    0F 76 /r     Packed Compare for Equal
PCMPEQQ xmm,xmm/m128 66 0F 38 29  SSE4_1
PCMPESTRI xmm,xmm/imm128,imm8 66 0F 3A 61    SSE4_2 store to ECX
PCMPESTRM xmm,xmm/imm128,imm8 66 0F 3A 60    SSE4_2 store to XMM0
PCMPGTB mm,mm/m64    0F 64 /r     Packed Compare for GT
PCMPGTW mm,mm/m64    0F 65 /r     Packed Compare for GT
PCMPGTD mm,mm/m64    0F 66 /r     Packed Compare for GT
PCMPGTQ xmm,xmm/m128    66 0F 38 37    Packed Compare for GT SSE4_2
PCMPISTRI xmm,xmm/imm128,imm8 66 0F 3A 63    SSE4_2 store to ECX
PCMPISTRM xmm,xmm/imm128,imm8 66 0F 3A 62    SSE4_2 store to XMM0
PEXTRB r32/m8,xmm,imm8  66 0F 3A 14    SSE4_1
PEXTRD r32/m32,xmm,imm8 66 0F 3A 16    SSE4_1
PEXTRW r32/m16,xmm,imm8 66 0F 3A 15    SSE4_1
PHADDD mm,mm/m64        0F 38 02 /r    Packed Horizontal Add SSSE3
PHADDD xmm,xmm/m128  66 0F 38 02 /r    Packed Horizontal Add SSSE3
PHADDSW mm,mm/m64    0F 38 03 /r  Packed Horizontal Add and Saturate
PHADDSW xmm,xmm/m128xmm1   66 0F 38 03 /r    Packed Horizontal Add and Saturate
PHADDW mm,mm/m64     0F 38 01 /r  Packed Horizontal Add
PHADDW xmm,xmm/m128  66 0F 38 01 /r  C2+           Packed Horizontal Add
PHMINPOSUW xmm,xmm/m128 66 0F 38 41 /r Packed Horizontal Word Minimum SSE4_1
PHSUBD mm,mm/m64     0F 38 06 /r  Packed Horizontal Subtract
PHSUBD xmm,xmm/m128  66 0F 38 06 /r    Packed Horizontal Subtract
PHSUBSW mm,mm/m64    0F 38 07 /r       Packed Horizontal Subtract and Saturate
PHSUBSW xmm,xmm/m128 66 0F 38 07 /r    Packed Horizontal Subtract and Saturate
PHSUBW mm,mm/m64     0F 38 05 /r       Packed Horizontal Subtract
PHSUBW xmm,xmm/m128  66 0F 38 05 /r    Packed Horizontal Subtract
PINSRB xmm,r/m8,imm8    66 0F 3A 20 /r   Insert Byte SSE4_1
PINSRD xmm,r/m32,imm8   66 0F 3A 22 /r   Insert Dword SSE4_1
PINSRW mm,r/m16,imm8    0F C4 /r  Insert Word
PINSRW xmm,r/m16,imm8   66 0F C4 /r   Insert Word
PMADDUBSW mm,mm/m64     0F 38 04 /r    Multiply and Add Packed Signed and Unsigned Bytes
PMADDUBSW xmm,xmm/m128  66 0F 38 04 /r    Multiply and Add Packed Signed and Unsigned Bytes
PMADDWD mm,mm/m64    0F F5 /r     Packed Multiply and Add
PMADDWD xmm,xmm/m128 66 0F F5 /r     Packed Multiply and Add
PMAXSB xmm,xmm/m128  66 0F 38 3C /r  Maximum of Packed Signed Byte Integers
PMAXSD xmm,xmm/m128  66 0F 38 3D /r  Maximum of Packed Signed Dword Integers
PMAXUD xmm,xmm/m128  66 0F 38 3F /r  Maximum of Packed Unsigned Dword Integers
PMAXUW xmm,xmm/m128  66 0F 38 3E /r  Maximum of Packed Unsigned Word Integers
PMINSB xmm,xmm/m128  66 0F 38 38 /r  Minimum of Packed Signed Byte Integers
PMINSD xmm,xmm/m128  66 0F 38 39 /r  Minimum of Packed Signed Dword Integers
PMINUD xmm,xmm/m128  66 0F 38 3B /r  Minimum of Packed Unsigned Dword Integers
PMINUW xmm,xmm/m128  66 0F 38 3A /r  Minimum of Packed Unsigned Word Integers
PMOVSXBD xmm,xmm/m32 66 0F 38 21 /r  Packed Move w Sign Extend
PMOVSXBQ xmm,xmm/m16 66 0F 38 22 /r  Packed Move w Sign Extend
PMOVSXBW xmm,xmm/m64 66 0F 38 20 /r  Packed Move w Sign Extend
PMOVSXDQ xmm,xmm/m64 66 0F 38 25 /r  Packed Move w Sign Extend
PMOVSXWD xmm,xmm/m64 66 0F 38 23 /r  Packed Move w Sign Extend
PMOVSXWQ xmm,xmm/m32 66 0F 38 24 /r  Packed Move w Sign Extend
PMOVZXBD xmm,xmm/m32 66 0F 38 31 /r  Packed Move w Zero Extend
PMOVZXBQ xmm,xmm/m16 66 0F 38 32 /r  Packed Move w Zero Extend
PMOVZXBW xmm,xmm/m64 66 0F 38 30 /r  Packed Move w Zero Extend
PMOVZXDQ xmm,xmm/m64 66 0F 38 35 /r  Packed Move w Zero Extend
PMOVZXWD xmm,xmm/m64 66 0F 38 33 /r  Packed Move w Zero Extend
PMOVZXWQ xmm,xmm/m32 66 0F 38 34 /r  Packed Move w Zero Extend
PMULDQ xmm,xmm/m128  66 0F 38 28 /r  Multiply Packed Signed Dword Integers
PMULHRSW mm,mm/m64   0F 38 0B /r  Packed Multiply High with Round and Scale
PMULHRSW xmm,xmm/m128 66 0F 38 0B /r   Packed Multiply High with Round and Scale
PMULHW mm,mm/m64     0F E5 /r     Packed Multiply High
PMULLD xmm,xmm2/m128  66 0F 38 40 /r   Multiply Packed Signed Dword Integers and Store Low Result
PMULLW mm,mm/m64     0F D5 /r     Packed Multiply Low
POP m32              8F    /0     Pop m32
POP r32              58+rd        Pop r32
POP DS               1F           Pop DS
POP ES               07           Pop ES
POP SS               17           Pop SS
POP FS               0F A1        Pop FS
POP GS               0F A9        Pop GS
POPAD                61           Pop EDI,... and EAX
POPAW                61           Pop DI,...  and AX
POPCNT r16,r/m16     F3 0F B8 /r  Count nonzero Bits
POPCNT r32,r/m32     F3 0F B8 /r  Count nonzero Bits
POPFD                9D           Pop Stack into EFLAGS Register
POPFW                9D           Pop Stack into FLAGS Register
POR mm,mm/m64        0F EB /r     OR qword from .. to mm
PSHUFB mm,mm/m64     0F 38 00 /r       Packed Shuffle Bytes
PSHUFB xmm,xmm/m128  66 0F 38 00 /r    Packed Shuffle Bytes
PSIGNB mm1,mm2/m64   0F 38 08     Negate/zero/preserve packed bytes
PSIGNB xmm,xmm2/m128 66 0F 38 08     Negate/zero/preserve packed bytes
PSIGND mm,mm/m64     0F 38 0A     Negate/zero/preserve packed bytes
PSIGND xmm,xmm/m128  66 0F 38 0A     Negate/zero/preserve packed bytes
PSIGNW mm,mm/m64     0F 38 09     Negate/zero/preserve packed bytes
PSIGNW xmm,xmm/m128  66 0F 38 09     Negate/zero/preserve packed bytes
PSLLW mm,mm/m64      0F F1 /r     Packed Shift Left Logical
PSLLW mm,imm8        0F 71 /6 ib  Packed Shift Left Logical
PSLLD mm,mm/m64      0F F2 /r     Packed Shift Left Logical
PSLLD mm,imm8        0F 72 /6 ib  Packed Shift Left Logical
PSLLQ mm,mm/m64      0F F3 /r     Packed Shift Left Logical
PSLLQ mm,imm8        0F 73 /6 ib  Packed Shift Left Logical
PSRAW mm,mm/m64      0F E1 /r     Packed Shift Right Arithmetic
PSRAW mm,imm8        0F 71 /4 ib  Packed Shift Right Arithmetic
PSRAD mm,mm/m64      0F E2 /r     Packed Shift Right Arithmetic
PSRAD mm,imm8        0F 72 /4 ib  Packed Shift Right Arithmetic
PSRLW mm,mm/m64      0F D1 /r     Packed Shift Right Logical 
PSRLW mm,imm8        0F 71 /2 ib  Packed Shift Right Logical 
PSRLD mm,mm/m64      0F D2 /r     Packed Shift Right Logical 
PSRLD mm,imm8        0F 72 /2 ib  Packed Shift Right Logical 
PSRLQ mm,mm/m64      0F D3 /r     Packed Shift Right Logical 
PSRLQ mm,imm8        0F 73 /2 ib  Packed Shift Right Logical 
PSUBB mm,mm/m64      0F F8 /r     Packed Subtract
PSUBW mm,mm/m64      0F F9 /r     Packed Subtract
PSUBD mm,mm/m64      0F FA /r     Packed Subtract
PSUBSB mm,mm/m64     0F E8 /r     Packed Subtract with Saturation
PSUBSW mm,mm/m64     0F E9 /r     Packed Subtract with Saturation
PSUBUSB mm,mm/m64    0F D8 /r     Packed Subtract Unsigned with S.
PSUBUSW mm,mm/m64    0F D9 /r     Packed Subtract Unsigned with S.
PTEST xmm,xmm/m128   66 0F 38 17 /r
PUNPCKHBW mm,mm/m64  0F 68 /r     Unpack High Packed Data
PUNPCKHWD mm,mm/m64  0F 69 /r     Unpack High Packed Data
PUNPCKHDQ mm,mm/m64  0F 6A /r     Unpack High Packed Data
PUNPCKLBW mm,mm/m32  0F 60 /r     Unpack Low Packed Data
PUNPCKLWD mm,mm/m32  0F 61 /r     Unpack Low Packed Data
PUNPCKLDQ mm,mm/m32  0F 62 /r     Unpack Low Packed Data
PUSH r/m32           FF    /6     Push r/m32
PUSH r32             50+rd        Push r32
PUSH imm8            6A       ib  Push imm8
PUSH imm32           68       id  Push imm32
PUSH CS              0E           Push CS
PUSH SS              16           Push SS
PUSH DS              1E           Push DS
PUSH ES              06           Push ES
PUSH FS              0F A0        Push FS
PUSH GS              0F A8        Push GS
PUSHAD               60           Push All regs32
PUSHAW               60           Push All regs16
PUSHFD               9C           Push EFLAGS
PUSHFW               9C           Push FLAGS
PXOR mm,mm/m64       0F EF /r     XOR qword
RCL r/m8,1           D0    /2     Rotate 9 bits left once
RCL r/m8,CL          D2    /2     Rotate 9 bits left CL times
RCL r/m8,imm8        C0    /2 ib  Rotate 9 bits left imm8 times
RCL r/m32,1          D1    /2     Rotate 33 bits left once
RCL r/m32,CL         D3    /2     Rotate 33 bits left CL times
RCL r/m32,imm8       C1    /2 ib  Rotate 33 bits left imm8 times
RCR r/m8,1           D0    /3     Rotate 9 bits right once
RCR r/m8,CL          D2    /3     Rotate 9 bits right CL times
RCR r/m8,imm8        C0    /3 ib  Rotate 9 bits right imm8 times
RCR r/m32,1          D1    /3     Rotate 33 bits right once
RCR r/m32,CL         D3    /3     Rotate 33 bits right CL times
RCR r/m32,imm8       C1    /3 ib  Rotate 33 bits right imm8 times
ROL r/m8,1           D0    /0     Rotate 8 bits r/m8 left once
ROL r/m8,CL          D2    /0     Rotate 8 bits r/m8 left CL times
ROL r/m8,imm8        C0    /0 ib  Rotate 8 bits r/m8 left imm8 times
ROL r/m32,1          D1    /0     Rotate 32 bits r/m32 left once
ROL r/m32,CL         D3    /0     Rotate 32 bits r/m32 left CL times
ROL r/m32,imm8       C1    /0 ib  Rotate 32 bits r/m32 left imm8 times
ROR r/m8,1           D0    /1     Rotate 8 bits r/m8 right once
ROR r/m8,CL          D2    /1     Rotate 8 bits r/m8 right CL times
ROR r/m8,imm8        C0    /1 ib  Rotate 8 bits r/m16 right imm8 times
ROR r/m32,1          D1    /1     Rotate 32 bits r/m32 right once
ROR r/m32,CL         D3    /1     Rotate 32 bits r/m32 right CL times
ROR r/m32,imm8       C1    /1 ib  Rotate 32 bits r/m32 right imm8 times
RDMSR                0F 32        Read from Model Specific Register
RDPKRU               0F 01 EE     Read Protection Key Rights for User Pages
RDPMC                0F 33        Read Performance-Monitoring counters
RDRAND r16|r32       0F C7 /6     Read 16 or 32-bit random number to dest Reg.
RDSEED r16|r32       0F C7 /7     Read 16 or 32-bit random SEED to dest Reg.
RDTSC                0F 31        Read Time-Stamp Counter
RDTSCP               0F 01 F9     Read Time-Stamp Counter and Processor ID
REP INS m8,DX        F3 6C        Input ECX bytes from port DX into [(E)DI]
REP INS m32,DX       F3 6D        Input ECX dwords from port DX into [(E)DI]
REP MOVSB            F3 A4        Move ECX bytes from ESI to EDI
REP MOVSD            F3 A5        Move ECX dwords from ESI to EDI
REP MOVSW            F3 A5        Move ECX words from ESI to EDI
REP OUTS DX,m8       F3 6E        Output ECX bytes from ESI to port DX
REP OUTS DX,m32      F3 6F        Output ECX dwords from ESI to port DX
REP LODSB            F3 AC        Load ECX bytes from ESI to AL
REP LODSD            F3 AD        Load ECX dwords from ESI to EAX
REP LODSW            F3 AD        Load ECX words from ESI to AX
REP STOSB            F3 AA        Fill ECX bytes at EDI with AL
REP STOSD            F3 AB        Fill ECX dwords at EDI with EAX
REP STOSD            F3 AB        Fill ECX words at EDI with AX
REPE CMPSB           F3 A6        Find nonmatching bytes in m and m
REPE CMPSD           F3 A7        Find nonmatching dwords in m and m
REPE CMPSW           F3 A7        Find nonmatching words in m and m
REPE SCASB           F3 AE        Find non-AL byte starting at 
REPE SCASD           F3 AF        Find non-EAX dword starting at
REPE SCASW           F3 AF        Find non-AX dword starting at  
REPNE CMPSB          F2 A6        Find matching bytes in m and m
REPNE CMPSD          F2 A7        Find matching dwords in m and m
REPNE CMPSW          F2 A7        Find matching words in m and m
REPNE SCASB          F2 AE        Find AL, starting at EDI
REPNE SCASD          F2 AF        Find EAX, starting at EDI
REPNE SCASW          F2 AF        Find AX, starting at EDI
RET                  C3           Near return 
RETF                 CB           Far return 
RET imm16            C2       iw  Near return, pop imm16 bytes from stack
RETF imm16           CA       iw  Far return, pop imm16 bytes from stack
ROUNDPD xmm,xmm/m128,imm8   66 0F 3A 09    Round Packed Double-FP Values
ROUNDPS xmm,xmm/m128,imm8   66 0F 3A 08    Round Packed Single-FP Values
ROUNDSD xmm,xmm/m128,imm8   66 0F 3A 0B    Round Scalar Double-FP Values
ROUNDSS xmm,xmm/m128,imm8   66 0F 3A 0A    Round Scalar Single-FP Values
RSM                  0F AA        Resume from System Management
SAHF                 9E           Store AH into Flags
SAL r/m8,1           D0    /4     Shift Arithmetic Left
SAL r/m8,CL          D2    /4     Shift Arithmetic Left
SAL r/m8,imm8        C0    /4 ib  Shift Arithmetic Left
SAL r/m32,1          D1    /4     Shift Arithmetic Left
SAL r/m32,CL         D3    /4     Shift Arithmetic Left
SAL r/m32,imm8       C1    /4 ib  Shift Arithmetic Left
SAR r/m8,1           D0    /7     Shift Arithmetic Right
SAR r/m8,CL          D2    /7     Shift Arithmetic Right
SAR r/m8,imm8        C0    /7 ib  Shift Arithmetic Right
SAR r/m32,1          D1    /7     Shift Arithmetic Right
SAR r/m32,CL         D3    /7     Shift Arithmetic Right
SAR r/m32,imm8       C1    /7 ib  Shift Arithmetic Right
SHL r/m8,1           D0    /4     Shift Logical Left
SHL r/m8,CL          D2    /4     Shift Logical Left
SHL r/m8,imm8        C0    /4 ib  Shift Logical Left
SHL r/m32,1          D1    /4     Shift Logical Left
SHL r/m32,CL         D3    /4     Shift Logical Left
SHL r/m32,imm8       C1    /4 ib  Shift Logical Left
SHR r/m8,1           D0    /5     Shift Logical Right
SHR r/m8,CL          D2    /5     Shift Logical Right
SHR r/m8,imm8        C0    /5 ib  Shift Logical Right
SHR r/m32,1          D1    /5     Shift Logical Right
SHR r/m32,CL         D3    /5     Shift Logical Right
SHR r/m32,imm8       C1    /5 ib  Shift Logical Right
SBB AL,imm8          1C       ib  Subtract with borrow 
SBB EAX,imm32        1D       id  Subtract with borrow 
SBB r/m8,imm8        80    /3 ib  Subtract with borrow 
SBB r/m32,imm32      81    /3 id  Subtract with borrow 
SBB r/m32,imm8       83    /3 ib  Subtract with borrow 
SBB r/m8,r8          18    /r     Subtract with borrow 
SBB r/m32,r32        19    /r     Subtract with borrow 
SBB r8,r/m8          1A    /r     Subtract with borrow 
SBB r32,r/m32        1B    /r     Subtract with borrow 
SCASB                AE           Scan String 
SCASD                AF           Scan String
SCASD                AF           Scan String
SETA r/m8            0F 97 /r     Set byte if above 
SETAE r/m8           0F 93 /r     Set byte if above or equal
SETB r/m8            0F 92 /r     Set byte if below 
SETC r/m8            0F 92 /r     Set byte if below 
SETBE r/m8           0F 96 /r     Set byte if below or equal 
SETE r/m8            0F 94 /r     Set byte if equal 
SETZ r/m8            0F 94 /r     Set byte if equal 
SETG r/m8            0F 9F /r     Set byte if greater 
SETGE r/m8           0F 9D /r     Set byte if greater or equal
SETL r/m8            0F 9C /r     Set byte if less 
SETLE r/m8           0F 9E /r     Set byte if less or equal 
SETNE r/m8           0F 95 /r     Set byte if not equal 
SETNO r/m8           0F 91 /r     Set byte if not overflow 
SETNS r/m8           0F 99 /r     Set byte if not sign 
SETO r/m8            0F 90 /r     Set byte if overflow 
SETPE r/m8           0F 9A /r     Set byte if parity even 
SETPO r/m8           0F 9B /r     Set byte if parity odd 
SETS r/m8            0F 98 /r     Set byte if sign 
SGDT m               0F 01 /0     Store GDTR to m
SIDT m               0F 01 /1     Store IDTR to m
SHLD r/m32,r32,imm8  0F A4 /r ib  Double Precision Shift Left
SHLD r/m32,r32,CL    0F A5 /r     Double Precision Shift Left
SHRD r/m32,r32,imm8  0F AC /r ib  Double Precision Shift Right
SHRD r/m32,r32,CL    0F AD /r     Double Precision Shift Right
SLDT r/m32           0F 00 /0     Store Local Descriptor Table Register
SMSW r/m32           0F 01 /4     Store Machine Status Word
STAC                 0F 01 CB     Set the AC flag
STC                  F9           Set Carry Flag
STD                  FD           Set Direction Flag
STI                  FB           Set Interrup Flag
STOSB                AA           Store String (byte)
STOSD                AB           Store String (dWord)
STOSW                AB           Store String (word)
STR r/m16            0F 00 /1     Store Task Register
SUB AL,imm8          2C       ib  Subtract 
SUB EAX,imm32        2D       id  Subtract 
SUB r/m8,imm8        80    /5 ib  Subtract 
SUB r/m32,imm32      81    /5 id  Subtract 
SUB r/m32,imm8       83    /5 ib  Subtract 
SUB r/m8,r8          28    /r     Subtract 
SUB r/m32,r32        29    /r     Subtract 
SUB r8,r/m8          2A    /r     Subtract 
SUB r32,r/m32        2B    /r     Subtract 
TEST AL,imm8         A8       ib  Logical Compare
TEST EAX,imm32       A9       id  Logical Compare
TEST r/m8,imm8       F6    /0 ib  Logical Compare
TEST r/m32,imm32     F7    /0 id  Logical Compare
TEST r/m8,r8         84    /r     Logical Compare
TEST r/m16,r16       85    /r     Logical Compare
TEST r/m32,r32       85    /r     Logical Compare
TZCNT r16,r/m16      F3 0F BC     Count trailing zero bits
TZCNT r32,r/m32      F3 0F BC     Count trailing zero bits
UD2                  0F 0B        Undifined Instruction
VERR r/m16           0F 00 /4     Verify a Segment for Reading
VERW r/m16           0F 00 /5     Verify a Segment for Writing
VMCALL               0F 01 C1     Call to VM Monitor
VMCLEAR m64       66 0F C7 /6     Clear VM Control Structure
VMFUNC               0F 01 D4     Invoke VM function specified in EAX
VMLAUNCH             0F 01 C2     Launch VM
VMPTRLD m64          0F C7 /6     Load VMCS Pointer
VMPTRST m64          0F C7 /7     Store VMCS Pointer
VMREAD r/m32,r32     0F 78        Read Field from VMCS
VMRESUME             0F 01 C3     Resume VM
VMWRITE r32,r/m32    0F 79        Write Field from VMCS
VMXOFF               0F 01 C4     Leave VMX Operation
VMXON m64         F3 0F C7 /6     Enter VMX Operation
WAIT                 9B           Wait
FWAIT                9B           Wait
WBINVD               0F 09        Write Back and Invalidate Cache
WRMSR                0F 30        Write to Model Specific Register
WRPKRU               0F 01 EF     Write Data to User Page Key Register
XADD r/m8,r8         0F C0 /r     Exchange and Add
XADD r/m16,r16       0F C1 /r     Exchange and Add
XADD r/m32,r32       0F C1 /r     Exchange and Add
XCHG EAX,r32         90+rd        Exchange r32 with EAX
XCHG r32,EAX         90+rd        Exchange EAX with r32
XCHG r/m8,r8         86    /r     Exchange byte 
XCHG r8,r/m8         86    /r     Exchange byte 
XCHG r/m32,r32       87    /r     Exchange doubleword 
XCHG r32,r/m32       87    /r     Exchange doubleword 
XGETBV               0F 01 D0     Reads XCR specified by ECX into EDX:EAX
XLAT m8              D7           Table Look-up Translation
XOR AL,imm8          34       ib  Logical Exclusive OR
XOR EAX,imm32        35       id  Logical Exclusive OR
XOR r/m8,imm8        80    /6 ib  Logical Exclusive OR
XOR r/m32,imm32      81    /6 id  Logical Exclusive OR
XOR r/m32,imm8       83    /6 ib  Logical Exclusive OR
XOR r/m8,r8          30    /r     Logical Exclusive OR
XOR r/m32,r32        31    /r     Logical Exclusive OR
XOR r8,r/m8          32    /r     Logical Exclusive OR
XOR r32,r/m32        33    /r     Logical Exclusive OR
XRSTOR mem           0F AE /5     Restore state components specified by EDX:EAX from mem.
XSAVE  mem           0F AE /4     Save state components specified by EDX:EAX to mem.
XSETBV               0F 01 D1     Set Extended Control Register" 0]

;;
  Each Name must be 16 Bytes aligned for the search. Any error breaks all of the
  downward searches down.
  
  A '<' before the Name indicates a substitution. Example, 'AAD' is in the same
  B_U_Asm Description as 'AAA'.
;;
[<16 OpCodeList: B$

'AAA             AAS             AAM             AAD             ADC             '
'ADD             ADDPD           ADDPS           ADDSD           ADDSS           '
'ADDSUBPD        ADDSUBPS        AND             ANDNPD          ANDNPS          '
'ANDPD           ANDPS           ARPL            '

'BOUND           BSF             BSR             BSWAP           BT              '
'BTC             BTR             BTS             '

'CALL           <CALLF           CBW             CWD             CWDE            CDQ             '
'CLC             CLD             CLI             CLTS            CLFLUSH         '
'CMC             CMP             '
'CMPccPD        <CMPEQPD        <CMPLEPD        <CMPLTPD        <CMPNEQPD       <'
'CMPNLEPD       <CMPNLTPD       <CMPORDPD       <CMPPD          <CMPUNORDPD      '
'CMPccPS        <CMPEQPS        <CMPLEPS        <CMPLTPS        <CMPNEQPS       <'
'CMPNLEPS       <CMPNLTPS       <CMPORDPS       <CMPPS          <CMPUNORDPS      '
'CMPccSD        <CMPEQSD        <CMPLESD        <CMPLTSD        <CMPNEQSD       <'
'CMPNLESD       <CMPNLTSD       <CMPORDSD       <CMPUNORDSD      '
'CMPccSS        <CMPEQSS        <CMPLESS        <CMPLTSS        <CMPNEQSS       <'
'CMPNLESS       <CMPNLTSS       <CMPORDSS       <CMPSS          <CMPUNORDSS      '
'CMPSB           CMPSW           CMPSD           CMPXCHG         CMPXCHG8B       '
'CMOVcc         <FCMOVB         <FCMOVBE        <'
'FCMOVE         <FCMOVNB        <FCMOVNBE       <FCMOVNE        <FCMOVNU        <'
'FCMOVU          '
'COMISD          COMISS          CPUID           '

'CVTDQ2PD        CVTDQ2PS        CVTPD2DQ        CVTPD2PI        CVTPD2PS        '
'CVTPI2PD        CVTPI2PS        CVTPS2DQ        CVTPS2PD        CVTPS2PI        '
'CVTSD2SI        CVTSD2SS        CVTSI2SD        CVTSI2SS        CVTSS2SD        '
'CVTSS2SI        CVTTPD2DQ       CVTTPD2PI       CVTTPS2DQ       CVTTPS2PI       '
'CVTTSD2SI       CVTTSS2SI       '

'DAA             DAS             DEC             DIV             DIVPD           '
'DIVPS           DIVSD           DIVSS           '

'EMMS            ENTER           '

'F2XM1           FABS            FADD            FADDP           FBLD            '
'FBSTP           FCHS            FCLEX           '
'FCMOVcc        <FCMOVB         <FCMOVBE        <'
'FCMOVE         <FCMOVNB        <FCMOVNBE       <FCMOVNE        <FCMOVNU        <'
'FCMOVU          '
'FCOM            FCOMP           FCOMPP          FCOMI           FCOMIP          '
'FCOS            FDECSTP         FDIV            FDIVP           FDIVR           '
'FDIVRP          FEMMS           FFREE          <FFREEP          FIADD           FICOM           '
'FICOMP          FIDIV           FIDIVR          FILD            FIST            '
'FISTP           FISTTP          FIMUL           FINCSTP         FINIT           '
'FNINIT          FISUB           FISUBR          FLD             '
'FLDxx          <FLD1           <FLDL2E         <FLDL2T         <FLDLG2         <'
'FLDLN2         <FLDZ           <FLDPI           '
'FLDCW           FLDENV          FMUL            FMULP           FNOP            '
'FNSAVE          FNSTCW          FNSTENV         FPATAN          FPTAN           '
'FNSTSW          '
'FPREM           FPREM1          FRNDINT         FRSTOR          FSAVE           '
'FSCALE          FSETPM          FSIN            FSINCOS         FSQRT           '
'FST             FSTP            FSTCW           FSTENV          FSTSW           '
'FSUB            FSUBP           FSUBR           FSUBRP          FTST            '
'FUCOMxx        <FUCOMP         <FUCOMPP        <FUCOMI         <FUCOMIP         '
'FWAIT           FXAM            FXCH            FXRSTOR         FXSAVE          '
'FXTRACT         FYL2X           FYL2XP1         '

'HADDPD          HADDPS          HSUBPD          HSUBPS          HLT             '

'IDIV            IMUL            INC             IN              INSB            '
'INSD            INSW            INT             INT01           INT1            '
'INT3            ICEBP           INTO            INVD            INVLPG          '
'IRET            IRETW           IRETD           '

'Jcc            <JA             <JAE            <JB             <JBE            <'
'JC             <JE             <JG             <JGE            <JL             <'
'JLE            <JNA            <JNAE           <JNB            <JNBE           <'
'JNC            <JNE            <JNG            <JNGE           <JNL            <'
'JNLE           <JNO            <JNP            <JNS            <JNZ            <'
'JO             <JP             <JPE            <JPO            <JS             <'
'JZ              '
'JCXZ            JECXZ           JMP             '

'LAHF            LAR             LDDQU           LDMXCSR         LDS             '
'LES             LFS             LGS             LSS             LEA             '
'LEAVE           LFENCE          LGDT            LIDT            LLDT            '
'LMSW            LODSB           LODSW           LODSD           LOCK            '
'LOOP            LOOPE           LOOPZ           LOOPNE          LOOPNZ          '
'LSL             LTR             '

'MASKMOVDQU      MASKMOVQ        MAXPD           MAXPS           MAXSD           '
'MAXSS           MFENCE          MINPD           MINPS           MINSD           '
'MINSS           MONITOR         MOV             MOVAPD          MOVAPS          '
'MOVD            MOVDDUP         MOVDQ2Q         MOVDQA          MOVDQU          '
'MOVHLPS         MOVHPD          MOVHPS          MOVLHPS         MOVLPD          '
'MOVLPS          MOVMSKPD        MOVMSKPS        MOVNTDQ         MOVNTI          '
'MOVNTPD         MOVNTPS         MOVNTQ          MOVQ            MOVQ2DQ         '
'MOVSB           MOVSD           MOVSHDUP        MOVSLDUP        MOVSW           '
'MOVSS           MOVSX           MOVUPD          MOVUPS          MOVZX           '
'MUL             MULPD           MULPS           MULSD           MULSS           '
'MWAIT           '

'NEG             NOT             NOP             NOPE            '

'OR              ORPD            ORPS            OUT             OUTSB           '
'OUTSW           OUTSD           '

'PACKSSDW        PACKSSWB        PACKUSWB        PADDB           PADDW           '
'PADDD           PADDQ           PADDSB          PADDSW          PADDUSB         '
'PADDUSW         PAND            PANDN           PAUSE           PAVGB           '
'PAVGW           PAVGUSB         '
'PCMPxxx        <PCMPEQB        <PCMPEQD        <PCMPEQW        <'
'PCMPGTB        <PCMPGTD        <PCMPGTW         '
'PEXTRW          PF2ID           PF2IW           PFACC           PFADD           '
'PFCMPxx        <PFCMPEQ        <PFCMPGE        <PFCMPGT         '
'PFMAX           PFMIN           PFMUL           PFNACC          PFPNACC         '
'PFRCP           PFRCPIT1        PFRCPIT2        PFRSQIT1        PFRSQRT         '
'PFSUB           PFSUBR          PI2FD           PI2FW           PINSRW          '
'PMADDWD         PMAXSW          PMAXUB          PMINSW          PMINUB          '
'PMOVMSKB        PMULHRWA        PMULHUW         PMULHW          PMULLW          '
'PMULUDQ         POP             POPA            POPAD           POPF            '
'POR             PREFETCH       <PREFETCHW       '
'PREFETCHh      <PREFETCHNTA    <PREFETCHT0     <PREFETCHT1     <PREFETCHT2     <PREFETCH0      <PREFETCH1      <PREFETCH2       '
'PSADBW          PSHUFD          PSHUFHW         PSHUFLW         PSHUFW          '
'PSLLDQ          '
'PSLLx          <PSLLW          <PSLLD          <PSLLQ           '
'PSRAx          <PSRAW          <PSRAD          <PSRAQ           '
'PSRLDQ          '
'PSRLx          <PSRLW          <PSRLD          <PSRLQ           '
'PSUBx          <PSUBB          <PSUBW          <PSUBD          <PSUBQ           '
'PSUBxx         <PSUBSB         <PSUBSW         <PSUBUSB        <PSUBUSW         '
'PSWAPD          '
'PUNPCKhxx      <PUNPCKHBW      <PUNPCKHWD      <PUNPCKHDQ      <PUNPCKHQDQ     <'
'PUNPCKLBW      <PUNPCKLWD      <PUNPCKLDQ      <PUNPCKLQDQ      '
'PUSH            PUSHA           PUSHAD          PUSHF           PXOR            '

'RCL             RCR             RCPPS           RCPSS           RDMSR           '
'RDPMC           RDTSC           REP             RET             RETF            '
'RETN            ROL             ROR             '
'RSDC            RSLDT           RSM             RSQRTPS         RSQRTSS         '
'RSTS            '

'SAHF            SAL             SAR             SBB             SCASB           '
'SCASW           SCASD           '
'SETcc           '
'SFENCE          SGDT            SIDT            SLDT            SHL             '
'SHR             SHLD            SHRD            SHUFPD          SHUFPS          '
'SMSW            SQRTPD          SQRTPS          SQRTSD          SQRTSS          '
'STC             STD             STI             STMXCSR         STOSB           '
'STOSW           STOSD           STR             SUB             SUBPD           '
'SUBPS           SUBSD           SUBSS           SYSCALL         SYSENTER        '
'SYSEXIT         SYSRET          '

'TEST            '

'UCOMISD         UCOMISS         UD0             UD1             UD2             '
'UNPCKHPD        UNPCKHPS        UNPCKLPD        UNPCKLPS        '

'VERR            VERW            '

'WAIT            WBINVD          WRMSR           '

'XADD            XCHG            XLATB           XOR             XORPD           '
'XORPS           '

0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0]


____________________________________________________________________________________________
____________________________________________________________________________________________

[DataToStructureHandle: ?]

DataToStructure:
    If D$DataToStructureHandle > 0
        Beep
    Else
        call 'USER32.DialogBoxParamA' D$hInstance, 32500, &NULL, DataToStructureProc, &NULL
    End_If
ret

____________________________________________________________________________________________


[DataTextTable: ?    StructureTextTable: ?    DataToStructureDialogHandle: ?]

Proc DataToStructureProc:
  Arguments @Adressee, @Message, @wParam, @lParam
  USES EBX ESI EDI


    ...If D@Message = &WM_COMMAND
        mov eax D@wParam | and eax 0FFFF
        If eax = &IDCANCEL
L5:         VirtualFree D$DataTextTable, D$StructureTextTable
            call 'User32.EndDialog' D@Adressee 0
        Else_If eax = &IDOK
            call SaveStructureToClipBoard | jmp L5<
        Else_If eax = &IDHELP
            call Help, B_U_AsmName, StructuresHelp, ContextHlpMessage
        Else_If eax = 5     ; [ >>>>>>>> ]
            call FromDataToStructure
      ;  Else_If eax = 6     ; [ <<<<<<<< ]
      ;      call FromStructureToData
        End_If

    ...Else_If D@Message e &WM_INITDIALOG
        VirtualAlloc DataTextTable 01000, StructureTextTable 01000
        move D$DataToStructureDialogHandle D@Adressee

    ...Else_If D@Message e &WM_CTLCOLOREDIT
        ; Control of output

    ...Else
        mov eax &FALSE | ExitP

    ...End_If

    mov eax &TRUE
EndP


[DisScale: ?    EquateValue: ?   FirstDataLabel: ?]

FromStructureToData:
  ; If wanted, later.
ret

;;
[DataTest:
 Val1: 0
 Val2: 0
 Val3: 0]

[DataXTest: ? #8]

[Data2Test:
 Val21: B$ 0
 Val22: 0
 Val23: 0]
;;

SaveStructureToClipBoard:
    mov eax D$StructureTextTable
    While B$eax > 0 | inc eax | end_While
    sub eax D$StructureTextTable | On eax = 0, ret
    mov D$TheClipLenght eax

    call 'USER32.OpenClipboard' D$hwnd | cmp eax 0 | je L9>>
    call 'USER32.EmptyClipboard' | cmp eax 0 | je L8>>

    mov ecx D$TheClipLenght | shl ecx 2               ; *4 > room for Generic Names
    push ecx
        call 'KERNEL32.GlobalAlloc' &GMEM_DDESHARE ecx | cmp eax 0 | jne L1>  ; > eax = handle
        pop eax | jmp L8>>
L1:     mov D$hBlock eax
        call 'KERNEL32.GlobalLock' eax                                       ; > eax = adress
    pop ecx
    shr ecx 2                                                   ; restore true data size
    mov edi eax, esi D$StructureTextTable

    While B$esi > 0 | movsb | End_While | mov eax 0 | stosd

    call 'KERNEL32.GlobalUnlock' D$hBlock
    call 'USER32.SetClipboardData' &CF_TEXT  D$hBlock
L8: call 'USER32.CloseClipboard'
L9: ret

____________________________________________________________________________________________













