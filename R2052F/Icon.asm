TITLE Icon
 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;
                                      icon editor
 
 (To be entirely re-written by who wants to).

 _______________________________________________________________________________________
 _______________________________________________________________________________________
;;

; Dialog Box Template in memory.

[ID_Inew 301  ID_Ipeek 302  ID_Ipoke 303  ID_IfromIco 304  ID_ItoIco 305
 ID_Icancel 306  ID_Ikeep 307  ID_TrackBar 320]

 _______________________________
;;
 The icon editor is set upon a dialog box created at run time (when created
 no dialog resource editor in RosAsm). The only one difficulty i encounted is
 that 'title's must be dWords aligned (doc doesn't tell).

 The predefined classes values are:
 080=button / 081=Edit / 082=Static / 083=ListBox / 084=ScrollBar / 085=ComboBox
;;

[IconDialogData:

; Dialog Box template:

  D$ &DS_SETFONT+&DS_CENTER+&WS_CAPTION+&WS_VISIBLE+&DS_MODALFRAME+&DS_3DLOOK+&WS_SYSMENU  ; style
     0                                                                   ; ext. Style
  U$ 8  0  0  220  200     ; control-number, x, y, width, hight
     0                       ; no menu
     0                       ; class 0 > default
    'Icon Editor' 0 0        ; title
     10 'Helv' 0

; controls (7 button):

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 15  40  8            ; x y w h
     ID_Inew                 ; ID
     0FFFF                   ; Predefined class
     080                     ; 080=button
    '> New >' 0                   ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 30  40  8            ; x y w h
     ID_Ipeek                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '> .exe >' 0 0                  ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 45  40  8            ; x y w h
     ID_Ipoke                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '< .exe <' 0  0                 ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 60  40  8            ; x y w h
     ID_IfromIco             ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '> .ico >' 0  0          ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 75  40  8            ; x y w h
     ID_ItoIco               ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '< .ico <' 0  0          ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 90  40  8           ; x y w h
     &IDCANCEL               ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '= Cancel =' 0  0        ; button title
     0                       ; no creation data

  D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 105 40  8           ; x y w h
     ID_Ikeep                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '* Keep *' 0  0          ; button title
     0]                      ; no creation data

 [AddToIcon: D$ &WS_CHILD&WS_VISIBLE  0     ; style / ext.style
  U$ 10 120 40  8           ; x y w h
     ID_Help                ; ID
     0FFFF                   ; Predefined class
     080                     ; button
    '  Help  ' 0  0          ; button title
     0]                      ; no creation data

 _______________________________

; Icon Edition data

[iIcoFileHeader:

 ; poor little thing for saving a poor little icon alone in a poor little ico file:

 W$    0   ; reserved
       1   ; Type 1
       1   ; entries number
B$   020   ; width
     020   ; hight
     010   ; color count
       0   ; reserved
W$     0   ; planes
       0   ; bits count
D$  02E8   ; size
     016   ; offset from .ico start


; All these data are not the table used to build a PE. Just a temporary table for
; icon edition. In case user compiles a source without having drawn any icon, this
; one is copyied to &TRUE buffer as default. Once user have compiled an application
; reloading it fills this table with previously defined icon.

iIcon:
iIconHeader:
B$ 028,0,0,0     ; size
   020,0,0,0     ; width
   040,0,0,0     ; height (maybe 40h because of the two masks)
   01,0          ; planes
   04,0          ; bit count
   0,0,0,0       ; compression 0
   080,02,0,0    ; 0280 > size of icon data
   0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0   ; (dummy)


iIconPalette:
;Blue,green,red,0    (16 color palette -values seams to be fixed ones-)

B$ 0,  0,  0,  0 ; color 0
   0,  0,080,  0 ;       1
   0,080,  0,  0 ;       2
   0,080,080,  0 ;       3
 080,  0,  0,  0 ;       4
 080,  0,080,  0 ;       5
 080,080,  0,  0 ;       6
 0C0,0C0,0C0,  0 ;       7
 080,080,080,  0 ;       8
   0,  0,0FF,  0 ;       9    rouge
   0,0FF,  0,  0 ;       A
   0,0FF,0FF,  0 ;       B
 0FF,  0,  0,  0 ;       C    bleu
 0FF,  0,0FF,  0 ;       D
 0FF,0FF,  0,  0 ;       E
 0FF,0FF,0FF,  0 ;       F


iIconXorMask:
; XOR color mask: (32*16 octets > 2 pixels / byte > 32*32 pixels)

B$    0    0
B$    0    0    0    0    0    0    0    0    0    0    0   0C    0    0   09  099
B$  099  099  099  099  099  099  099  099  099  099  099   0C  0C0    0   09  099
B$  099  099  099  099  099  099  099  099  099  099  099   0C  0CC    0    0    0
B$   09  099  099  099  099  099  099  099  099  099  099   0C  0CC  0C0   03  03B
B$   09  099  099  099  099  099  099  099  099  099  099   0C  0CC  0C0    0  033
B$   09  099  090    0    0    0    0  099  099  099  099   0C  0CC  0C0    0   03
B$   09  099  090  0B$  0BB  0BB  0B0  099  099  099  099   0C  0CC  0C0    0    0
B$   09  099  090  0DD  0BB  0BB  0B0  099  099  099  099   0C  0CC  0C0    0    0
B$   09  099  090  0DD  0B$  0BB  0B0  099  099  099  099   0C  0CC  0C0    0    0
B$   09  099  090  0DD  0DD  033  030  099  099  099  099   0C  0CC  0C0    0    0
B$    0    0    0  0DD  0DD    0    0  099  099  099  099   0C  0CC  0C0    0    0
B$   03  03B  0BB  0BD  0DD    0    0  099  099  099  099   0C  0CC  0C0    0    0
B$    0  033  0BB  0BB  0DD    0    0  099  099  099  099   0C  0CC  0C0    0    0
B$  0C0   03  03B  0BB  0BD    0    0    0    0    0    0   0C  0CC  0C0   0E  0E0
B$  0CC    0  033  033  033    0    0  033  0BB  0BB  0BB  0BB  0CC  0C0   0E  0E0
B$  0CC  0C0    0    0    0    0    0   03  03B  0BB  0BB  0BB  0BC  0C0   0E  0E0
B$  0CC  0CC    0    0    0    0   0C    0  033  0BB  0BB  0BB  0BB  0C0   0E  0E0
B$  0CC  0CC    0   0E  0EE  0EE   0C  0C0   03  033  033  033  033  030   0E  0E0
B$  0CC  0CC    0   0E  0EE  0EE   0C  0CC    0    0    0    0    0    0   0E  0E0
B$  0CC  0CC    0   0E  0EE  0EE   0C  0CC  0C0    0    0    0    0    0   0E  0E0
B$    0    0    0   0E  0EE  0EE   0C  0C0    0    0    0    0    0    0   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C    0   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C  0C0   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C  0CC   0E  0EE
B$  0EE  0EE  0EE  0EE  0EE  0EE   0C  0C0  0AA  0AA  0AA  0AA   0C  0CC    0    0
B$    0    0    0    0    0    0   0C  0C0  0AA  0AA  0AA  0AA   0C  0CC   03  03B
B$  0BB  0BB  0BB  0BB  0BB  0BB  0BB  0C0  0AA  0AA  0AA  0AA   0C  0CC    0  033
B$  0BB  0BB  0BB  0BB  0BB  0BB  0BB  0B0  0AA  0AA  0AA  0AA   0C  0CC    0   03
B$  03B  0BB  0BB  0BB  0BB  0BB  0BB  0B0    0    0    0    0   0C  0CC    0    0
B$  033  033  033  033  033  033  033  033   03  0BB  0BB  0BB  0BB  0CC    0    0
B$    0    0    0    0    0    0    0    0    0  03B  0BB  0BB  0BB  0BC    0    0
B$    0    0    0    0    0    0    0    0    0   03  033  033  033  033


iIconAndMask:
; AND monochrome mask: (8*16 octects > 8 pixels / byte > 32*32 pixels)

B$    0    0
B$    0   0F    0    0    0   07    0    0    0   03    0    0    0   01  080    0
B$    0   01  0C0    0    0   01  0E0    0    0   01  0F0    0    0   01  0F0    0
B$    0   01  0F0    0    0   01  0F0   03  080   01  0F8   03  080   01  0FC   03
B$  080   01   06   03  080   01   03   03  0C0   01   01  0FF  0E0   01    0  0C0
B$  030   01    0  0C0  018   01    0  0C0   0F  0FF    0  0C0   07  0FF    0    0
B$    0   07    0    0    0   03    0    0    0   01    0    0    0    0    0    0
B$    0    0    0    0    0    0  080    0    0    0  0C0    0    0    0  0E0    0
B$    0    0  0F0    0    0    0  0FF  0FF  0F8    0  0FF  0FF  0FC    0

EndiIconAndMask: ]

 ______________________________

; RGB (eax) <> RGBquad (eax) (red green blue <> blue red green):

reverseRGB:
    push ecx, ebx
      mov ebx eax, ecx eax
      and eax 0FF | and ebx 0FF00 | and ecx 0FF0000
      shl eax 16 | shr ecx 16
      add eax ebx | add eax ecx
    pop ebx, ecx
ret

 _______________________________

; Routines for drawing on Icon Dialog Box.

[iRECT: iRECTx1: 120  iRECTy1: 20  iRECTx2: 130  iRECTy2: 30]

[iPAINTSTRUCT:
 ihdc: ?  ifErase: ?  ircPaint: B$ ?  ifRestore: D$ ?
 ifIncUpdate: ?   irgbReserved: ? ? ? ?  ? ? ? ?]

[iXcount: ?  iYcount: ?]

; drawing one little box (each 'pixel') in edit icon image. Calculations at
; 'IBrush index' are to translate the "handles'Brushes" (linear dWords) in
; a pointer to "Brushes'structures". DL is used to rotary point to AND mask bits.

iOneRect:
    mov dh B$iIconAndMask+ecx | and dh dl
    pushad
      if dh <> 0
; blank area box:
        call 'GDI32.Rectangle' D$ihdc  D$iRECTx1  D$iRECTy1  D$iRECTx2  D$iRECTy2
; blank of true size icon at bottom:
        mov eax D$iRECTx1, ebx D$iRECTy1 ; | sub eax 140 | sub ebx 276
        shr eax 3 | shr ebx 3 | add eax 40 | add ebx 360
        call 'GDI32.SetPixel' D$ihdc eax ebx D$iBackGroundColor
      Else
        push eax
; One color pixel box of icon :
          call 'USER32.FillRect' D$ihdc iRECT D$ihBrush0+eax
        pop ebx
; show true size icon pixel at bottom:
        shr ebx 2                                    ; IBrush index
        mov ecx ebx | shl ebx 1 | add ecx ebx        ; * 3
        inc ecx                                      ; + 1
        shl ecx 2                                    ; * 4
        mov ecx D$IBrush0+ecx
      ;  mov eax D$iRECTx1, ebx D$iRECTy1 | sub eax 140 | sub ebx 276
      ;  shr eax 3 | shr ebx 3 | add eax 40 | add ebx 360

        mov eax D$iRECTx1, ebx D$iRECTy1
        shr eax 3 | shr ebx 3 | add eax 28 | add ebx 320
      ; Toto fix

        call 'GDI32.SetPixel' D$ihdc eax ebx ecx      ; handle, X, Y, color
; NT > problem: only bottom line drawn (ebx always the same value ???) don't find any
; reason for...
      end_if
    popad
    ror dl 1 | On dl = 00_1000_0000, inc ecx
ret


; Drawing of edition image of icon:

iDrawColorBox:
    call 'USER32.BeginPaint' D$IconEditorHandle, iPAINTSTRUCT
    call 'USER32.GetDC' D$IconEditorHandle | mov D$ihdc eax
    mov D$iRECTx1 135,  D$iRECTy1 305, D$iRECTx2 0192,  D$iRECTy2 016C
    call 'USER32.FillRect' D$ihdc iRECT D$IhBackBrush
    mov D$iRECTx1 140,  D$iRECTy1 310, D$iRECTx2 160,  D$iRECTy2 330, ecx 0
L0: push ecx
      dec D$iRECTx1 | dec D$iRECTy1 | inc D$iRECTx2 | inc D$iRECTy2
      call 'GDI32.Rectangle' D$ihdc  D$iRECTx1  D$iRECTy1  D$iRECTx2  D$iRECTy2
      inc D$iRECTx1 | inc D$iRECTy1 | dec D$iRECTx2 | dec D$iRECTy2
    pop ecx
    push ecx
      mov al cl | shr al 2 | cmp B$ActualColor, al | jne L1>
        pushad
          mov eax D$iRECTx1, ebx D$iRECTy1, ecx D$iRECTx2, edx D$iRECTy2
          sub eax 3 | sub ebx 3 | add ecx 3 | add edx 3
          call 'GDI32.Rectangle' D$ihdc  eax ebx ecx edx
        popad
L1:   call 'USER32.FillRect' D$ihdc iRECT D$ihBrush0+ecx
    pop ecx
      add ecx 4 | cmp ecx 64 | jae L9>
        add D$iRECTx1 34 | add D$iRECTx2 34
        If ecx = 32
          mov D$iRECTx1 140,  D$iRECTy1 340, D$iRECTx2 160,  D$iRECTy2 360
        End_If
      jmp L0<<
L9: call 'USER32.ReleaseDC' D$IconEditorHandle, D$ihdc
    call 'USER32.EndPaint' D$IconEditorHandle, iPAINTSTRUCT
 ret


; Main Construction of the editor: edit image + true size icon + color selection boxes:

[iBackGroundColor: ?    DrawIconMessage: ?]

DrawIcon:
    call 'USER32.BeginPaint' D$IconEditorHandle, iPAINTSTRUCT
      call 'USER32.GetDC' D$IconEditorHandle | mov D$ihdc eax
      call 'GDI32.GetPixel' eax 5 5 | mov D$iBackGroundColor eax, D$iBackBrush+4 eax
      call DeleteIconBrushes | call CreateIconBrushes
        mov D$iXcount 0, D$iYcount 0
        mov D$iRECTx1 140,  D$iRECTy1 276,  D$iRECTx2 149,  D$iRECTy2 285
        mov esi iIconXorMask, ecx 0, edx 00_1000_0000

L0:     mov ebx 0, eax 0 | lodsb | mov bl al | and ebx 0F | and eax 0F0 | shr eax 4
          shl eax 2 | shl ebx 2
            call iOneRect
              inc D$iXcount | add D$iRECTx1 8 | add D$iRECTx2 8
                mov eax ebx
                  call iOneRect
                    inc D$iXcount | cmp D$iXcount 32 | je L2>
        add D$iRECTx1 8 | add D$iRECTx2 8 | jmp L0<<

L2:     inc D$iYcount | cmp D$iYcount 32 | je L3>
          mov D$iRECTx1 140,  D$iRECTx2 149, D$iXcount 0
            sub D$iRECTy1 8 | sub D$iRECTy2 8 | jmp L0<<

L3: call 'USER32.ReleaseDC' D$IconEditorHandle,  D$ihdc
    call 'USER32.EndPaint' D$IconEditorHandle, iPAINTSTRUCT
    On D$DrawIconMessage <> &WM_MOUSEMOVE, call iDrawColorBox
ret

 ______________________________________

; Brushes used by icon editor. 'iBrush0', ... are brushes creation data;
; 'ihBrush0, ... are brushes handles.

; [BS_SOLID 0] = first of 3 members, second is the color

[IBrush0: ? ? ?   IBrush1: ? ? ?   IBrush2: ? ? ?   IBrush3: ? ? ?   ; brushes struc
 IBrush4: ? ? ?   IBrush5: ? ? ?   IBrush6: ? ? ?   IBrush7: ? ? ?
 IBrush8: ? ? ?   IBrush9: ? ? ?   IBrushA: ? ? ?   IBrushB: ? ? ?
 IBrushC: ? ? ?   IBrushD: ? ? ?   IBrushE: ? ? ?   IBrushF: ? ? ?  iBrushesEnd: ]

[IhBrush0: ?   IhBrush1: ?   IhBrush2: ?   IhBrush3: ?               ; brushes handles
 IhBrush4: ?   IhBrush5: ?   IhBrush6: ?   IhBrush7: ?
 IhBrush8: ?   IhBrush9: ?   IhBrushA: ?   IhBrushB: ?
 IhBrushC: ?   IhBrushD: ?   IhBrushE: ?   IhBrushF: ?]

[iBackBrush: ? ? ?  IhBackBrush: ?]


CreateIconBrushes:
    mov edi iBrush0, esi iIconPalette | add edi 4        ; store icon palette colors
L0: lodsd | call reverseRGB | stosd | add edi 8          ; in brushes structures
    cmp edi iBrushesEnd | jb L0<

    call 'GDI32.CreateBrushIndirect' iBrush0 | mov D$ihBrush0 eax
    call 'GDI32.CreateBrushIndirect' iBrush1 | mov D$ihBrush1 eax
    call 'GDI32.CreateBrushIndirect' iBrush2 | mov D$ihBrush2 eax
    call 'GDI32.CreateBrushIndirect' iBrush3 | mov D$ihBrush3 eax
    call 'GDI32.CreateBrushIndirect' iBrush4 | mov D$ihBrush4 eax
    call 'GDI32.CreateBrushIndirect' iBrush5 | mov D$ihBrush5 eax
    call 'GDI32.CreateBrushIndirect' iBrush6 | mov D$ihBrush6 eax
    call 'GDI32.CreateBrushIndirect' iBrush7 | mov D$ihBrush7 eax
    call 'GDI32.CreateBrushIndirect' iBrush8 | mov D$ihBrush8 eax
    call 'GDI32.CreateBrushIndirect' iBrush9 | mov D$ihBrush9 eax
    call 'GDI32.CreateBrushIndirect' iBrushA | mov D$ihBrushA eax
    call 'GDI32.CreateBrushIndirect' iBrushB | mov D$ihBrushB eax
    call 'GDI32.CreateBrushIndirect' iBrushC | mov D$ihBrushC eax
    call 'GDI32.CreateBrushIndirect' iBrushD | mov D$ihBrushD eax
    call 'GDI32.CreateBrushIndirect' iBrushE | mov D$ihBrushE eax
    call 'GDI32.CreateBrushIndirect' iBrushF | mov D$ihBrushF eax
    call 'GDI32.CreateBrushIndirect' iBackBrush | mov D$IhBackBrush eax
ret


DeleteIconBrushes:
    call 'GDI32.DeleteObject'  D$ihBrush0
    call 'GDI32.DeleteObject'  D$ihBrush1
    call 'GDI32.DeleteObject'  D$ihBrush2
    call 'GDI32.DeleteObject'  D$ihBrush3
    call 'GDI32.DeleteObject'  D$ihBrush4
    call 'GDI32.DeleteObject'  D$ihBrush5
    call 'GDI32.DeleteObject'  D$ihBrush6
    call 'GDI32.DeleteObject'  D$ihBrush7
    call 'GDI32.DeleteObject'  D$ihBrush8
    call 'GDI32.DeleteObject'  D$ihBrush9
    call 'GDI32.DeleteObject'  D$ihBrushA
    call 'GDI32.DeleteObject'  D$ihBrushB
    call 'GDI32.DeleteObject'  D$ihBrushC
    call 'GDI32.DeleteObject'  D$ihBrushD
    call 'GDI32.DeleteObject'  D$ihBrushE
    call 'GDI32.DeleteObject'  D$ihBrushF
    call 'GDI32.DeleteObject'  D$IhBackBrush
ret
 ________________________________________

; calculation of icon editor position of some user click:

[InsideEditBox edx  InsideColorBox ecx]

iPos:
    mov InsideEditBox &FALSE, InsideColorBox &FALSE
    mov eax D$MousePosX, ebx D$MousePosY
  ;  mov eax 0, ebx 0 | push D$Lparam | pop ax, bx      ; eax > x / ebx > y
    cmp eax 08D  | jbe L9>
    cmp eax 018D | jae L9>
    cmp ebx 01C  | jbe L9>
    cmp ebx 011C | jae L8>
      sub eax 08D | sub ebx 01C                         ; Pos - origin
      mov InsideEditBox &TRUE | ret
L8: cmp ebx 0135 | jb L9>
    cmp ebx 0167  | ja L9>
      sub eax 08D | sub ebx 0135
      mov InsideColorBox &TRUE | ret
L9: ret

; Drawing in icon image.

[ActualColor: ?]  ; accessed as byte and as dword.

; In: eax / ebx = indexes x/y to on icon pixel. we really write at iIconXorMask,
; (iIconAndMask used as "iIconXorMaskEnd"). first line is last one.
; When back from iPos, if user clicked on left upper little box, eax=0 / ebx=0
; ... on right lower one: eax=01F / ebx=01F:

EditIcon:
    shr eax 3 | shr ebx 3                                  ; 8 pixels per little box
    push eax, ebx
      mov esi iIconAndMask | sub esi 16 | shl ebx 4 | sub esi ebx ; + lines
      mov ebx eax                                                 ; keep for odd test
      shr eax 1 | add esi eax
      mov al B$ActualColor | mov ah 00_1111_0000
      Test ebx 1 | jnz L2>
        shl al 4 | mov ah 00_1111
L2:   and B$esi ah | or B$esi al
    pop ebx, eax
    mov esi EndiIconAndMask | sub esi 4 | shl ebx 2 | sub esi ebx
    mov ecx eax
    shr eax 3 | add esi eax
    and ecx 00_111
    mov al 00_1000_0000 | shr al cl
    or B$esi al | xor B$esi al
    call DrawIcon
  ret

; Clearing in icon image.

ClearIconPix:
    shr eax 3 | shr ebx 3                               ; 8 pixels per little box
    push eax, ebx
      mov esi iIconAndMask | sub esi 16 | shl ebx 4 | sub esi ebx  ; + lines
      mov ebx eax                                     ; keep for odd test
      shr eax 1 | add esi eax
      mov al B$ActualColor | mov ah 00_1111_0000
      Test ebx 1 | jnz L2>
        shl al 4 | mov ah 00_1111
L2:   and B$esi ah
    pop ebx, eax
    mov esi EndiIconAndMask | sub esi 4 | shl ebx 2 | sub esi ebx
    mov ecx eax
    shr eax 3 | add esi eax
    and ecx 00_111
    mov al 00_1000_0000 | shr al cl
    or B$esi al
    call DrawIcon
ret


; Search what color choice box user clicked on.

WhatColor:
    shr eax 5 | shr ebx 5 | shl ebx 3
    add eax ebx | mov B$ActualColor al
    call DrawIcon
ret


; user left click on the square rainbow:

[NewColor: ?]

WhatNewColor:
    call 'GDI32.GetPixel' D$RainbowDC eax ebx | call ReverseRGB
    mov edi iIconPalette, ebx D$ActualColor | shl ebx 2 | add edi ebx | stosd
    call DeleteIconBrushes | call CreateIconBrushes
    call iDrawColorBox
ret

; User left Click on selected Color Box while in 'Choose Color':

SetNewColor:
    shr eax 5 | shr ebx 5 | shl ebx 3
    add eax ebx | cmp al B$ActualColor | jne L9>
      call 'USER32.DestroyWindow' D$GreenSliderHandle
      call DeleteRainbowDC
      mov B$OnRainbow &FALSE
      call DrawIcon
L9: ret


iLeft:
    call iPos
    cmp B$OnRainbow &TRUE | jne L5>

    If InsideEditBox = &TRUE
      call WhatNewColor
    Else_If InsideColorBox = &TRUE
      call SetNewColor
    End_If
    ret

L5: If InsideEditBox = &TRUE
      call EditIcon
    Else_If InsideColorBox = &TRUE
      call WhatColor
    End_If
L9: ret

 ________________________________________

; Showing Square Rainbow for color choice:

[RedBit 1  BlueBit  00_00000001_00000000_00000000  GreenBit  00_00000001_00000000
 NO_RED 0FFFFFF00  NO_BLUE 0FFFF  NO_GREEN 0FF00FF]

; size, width, height, planes, bitCount, compression imagesize x/meter y/meter 0  0
[RainBowHeader:  40 255 255 W$ 1 32 D$ 0  65025  xMeter: 0  yMeter: 0   0  0]

[SquareRainbowHandle: ?  SquareRainbowPtr: ?  RainBowHandle: ?  RainbowDC: ?
 RainData: ?  SlideGreen: ?   OldRainbowBitMaP: ?]


; Build a 2 dimensions color table in memory (blue/red):

DeleteRainbowDC:
    call 'GDI32.SelectObject' D$RainBowDC  D$OldRainbowBitMaP
    call 'GDI32.DeleteObject' D$RainbowHandle
    call 'GDI32.DeleteDC' D$RainbowDC
ret


Rainbow:
    call 'USER32.BeginPaint' D$IconEditorHandle iPAINTSTRUCT

    call 'USER32.GetDC' D$IconEditorHandle | mov D$ihdc eax

    call 'GDI32.CreateCompatibleDC' D$ihdc | mov D$RainbowDC eax

    call 'GDI32.CreateDIBSection' D$RainBowDC RainBowHeader,
                                     &DIB_RGB_COLORS RainData 0 0
    mov D$RainbowHandle eax

; filling colors data:

    mov eax D$SlideGreen, ecx 0FF, edi D$RainData
    shl eax 8
L0: push ecx
        mov ecx 0FF
L1:     stosd | add eax RED_BIT | loop L1<
            and eax NO_RED
    pop ecx
    add eax BLUE_BIT | loop L0<

; Painting at screen (we do not release rainbow DC and object here -needed-):

    call 'GDI32.SelectObject' D$RainBowDC  D$RainbowHandle
        mov D$OldRainbowBitMaP eax
    call 'GDI32.BitBlt' D$ihdc 140 30  255 255 D$RainbowDC 0  0  &SRCCOPY

    call 'USER32.ReleaseDC' D$IconEditorHandle D$ihdc

    call 'USER32.EndPaint' D$IconEditorHandle, iPAINTSTRUCT
ret


; Icon edition area is a little bit larger than 255/255 because of little boxes edges.
; we clean that here, at first 'color box right click':

clearIconArea:
    call 'USER32.BeginPaint' D$IconEditorHandle, iPAINTSTRUCT
      call 'USER32.GetDC' D$IconEditorHandle | mov D$ihdc eax
        mov D$iRECTx1 135,  D$iRECTy1 25, D$iRECTx2 400,  D$iRECTy2 290
        call 'USER32.FillRect' D$ihdc iRECT D$IhBackBrush
      call 'USER32.ReleaseDC' D$IconEditorHandle  D$ihdc
    call 'USER32.EndPaint' D$IconEditorHandle, iPAINTSTRUCT
ret


[TrackClassName: 'msctls_trackbar32', 0  TrackTitle: 'Green' 0  GreenSliderHandle: 0]

GreenSlider:
    call 'User32.CreateWindowExA' 0, TrackClassName, TrackTitle,
                                  &WS_CHILD+&WS_VISIBLE+&TBS_LEFT+&TBS_VERT,
                                  410, 25, 20, 265, D$IconEditorHandle, ID_TRACKBAR,
                                  D$hinstance, 0
    mov D$GreenSliderHandle eax
    call 'User32.SendMessageA' D$GreenSliderHandle  &TBM_SETRANGE 1 0FF_0000
    mov eax D$SlideGreen | not al
    call 'User32.SendMessageA' D$GreenSliderHandle  &TBM_SETPOS 1  eax
ret


[OnRainbow: ?  OldSelectedColor: ?]

RestorePreviousColor:
    mov edi iIconPalette, eax D$ActualColor | shl eax 2
    add edi eax | mov eax D$OldSelectedColor | stosd
    call DeleteIconBrushes | call CreateIconBrushes | call DrawIcon
ret


SaveActualColor:
    mov esi iIconPalette, eax D$ActualColor | shl eax 2
    add esi eax | lodsd | mov D$OldSelectedColor eax
ret


iRight:
    If B$OnRainbow = &TRUE
      call RestorePreviousColor
      call 'USER32.DestroyWindow' D$GreenSliderHandle
      call deleteRainbowDC
      mov B$OnRainbow &FALSE | ret
    End_If

    call iPos
    If InsideEditBox = &TRUE
      call ClearIconPix
    Else_If InsideColorBox = &TRUE
      mov B$OnRainbow &TRUE
      call WhatColor
      call SaveActualColor
      mov esi iIconPalette, eax D$ActualColor | shl eax 2
      add esi eax | inc esi | mov eax 0, al B$esi | mov D$SlideGreen eax
      call clearIconArea | call GreenSlider | call RainBow
    End_If
L9: ret


; Retrieve the slider value on user move and redraw rainbow:

iTrackMove:
      call 'User32.SendMessageA' D$GreenSliderHandle &TBM_GETPOS 0 0
        not al | mov D$SlideGreen eax
      call DeleteRainbowDC | call Rainbow
L9: ret

 ___________________________________________________________________________________
 ___________________________________________________________________________________

; reading and writing icons from/to files:

 ___________________________________________________________________________________

; Reading and writing an icon from/to a PE:
 ___________________________________________________________________________________

[iStartOfResources: 0  iExePtr: 0  iExeLen: 0  iResourceRVA: 0  iSourceHandle: 0
 iSourceFilterPtr: 1
 iPEFilesFilters:  B$ 'PE files'     0  '*.exe'   0  0]

[iuFileFilter: ? #262] [iSaveFilter: ? #262] [iChoosenFile: ? #262]

[iOpenPEStruc: len
 ihwndPEFileOwner: 0  iOPESInstance: 0  iPEFilesFilters  iuFileFilter  260
 iSourceFilterPtr  iSaveFilter  260  iChoosenFile  260  0
 OpenPEFileTitle  0281804
 0  0  0  0  0]

[ResourcesRVA: ?    NoResourcesPE: ?    PeIconFound: ?]


OpenPeForReadingIcon:
  ; Opening a file:
    mov B$NoResourcesPE &FALSE

    mov edi iSaveFilter, ecx 260, eax 0 | rep stosd

    call 'Comdlg32.GetOpenFileNameA' iOpenPEStruc
      On D$iSaveFilter = 0, ret
 ______________________________________

  ; Loading the entire file in memory:

    On D$iSourceHandle > 0, call 'KERNEL32.CloseHandle' D$iSourceHandle

    mov esi iSaveFilter

    call 'KERNEL32.CreateFileA' esi &GENERIC_READ, &FILE_SHARE_READ,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox ;;;;| pop eax | ret  ; return to caller of caller
        mov D$iSourceHandle 0
    Else
        mov D$iSourceHandle eax

        call 'KERNEL32.GetFileSize'  eax 0
        mov D$iExeLen eax | VirtualAlloc iExePtr eax

        mov D$NumberOfReadBytes 0
        call 'KERNEL32.ReadFile' D$iSourceHandle D$iExePtr,
                                D$iExeLen NumberOfReadBytes 0      ; load headers
    End_If
ret


ReadRosAsmPeIcon:                     ; reused by general purpose RosAsm PE opening.
    mov B$PeIconFound &FALSE

  ; read dos header:
    mov esi D$iExePtr
    mov eax 0 | add esi 8 | lodsw    ; parag. size of dos header end >PE header adress

    shl eax 4 | sub eax 4
    mov esi D$iExePtr | add esi eax | lodsd      ; eax = PE header

    mov esi D$iExePtr | add esi eax
    If D$esi <> 'PE'
        mov esi D$iExePtr | add esi 03C | lodsd | add eax D$iExePtr | mov esi eax
        cmp D$esi 'PE' | jne PeNotFound
    End_If

 ______________________________________

  ; read data in PE header:

    movzx ecx w$esi+6                     ; word record of section number
    add esi 136 | lodsd | mov ebx eax     ; RVA of resources from "Image Data Dir..."
    mov D$ResourcesRVA eax  ; jmp over general purpose headers and reach PE sections headers:

    add esi 120                           ; esi points to RVA of first section header

L0: lodsd | cmp eax ebx | je L1>
        add esi 36 | loop L0<
        On ebx = 0, jmp AbortIconSearch
          jmp SectNotFound

L1: On D$esi-16 <> '.rsr', jmp AbortIconSearch

 ______________________________________

  ; if here, '.rsrc' section found:

L1: add esi 4 | lodsd                            ; > app ptr to resources

    add eax D$iExePtr | mov D$iStartOfResources eax

DisReadMainIcon:
    mov B$PeIconFound &FALSE

    mov esi eax | add esi 14                     ; > number of ID resources
    mov eax 0 | lodsw | mov ecx eax              ; > in ecx
    add cx W$esi-4                               ; add number of Named IDs
    On eax = 0, jmp AbortIconSearch      ; if no resources at all

  ; search RT_ICON in resource general header:

L0: lodsd | cmp eax RT_ICON | je L1>
    lodsd | loop L0<
      jmp AbortIconSearch                        ; no icon found (possible naked PE)

L1: lodsd                   ; icon found "Level2Rt_Icon-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF        ; strip node flag (0_80000000)
    add eax D$iStartOfResources

    add eax 14 | mov esi eax, edx 0, dx W$esi | sub  esi 2

 ______________________________________

  ; resource TYPEs dir:

NextiRecord:

    add esi 8
    push esi

    lodsd                              ; "Level3Rt_Icon-StartOfRsrc+NodeFlag" in eax
    and eax 0FFFFFFF
    add eax D$iStartOfResources
    add eax 20 | mov esi eax

  ; language. dir:
    lodsd                      ; "Level4Rt_Icon-StartOfRsrc" in eax (no NodeFlag here
                                       ; next one is leave ptr to true resources)
    add eax D$iStartOfResources
    mov esi eax

  ; records of each resource:
    lodsd                              ; ptr to icon data (but RVA - startOfResource)
    mov ecx D$esi

    If ecx <> 02E8                                       ; 2E8h = size of common icons
      pop esi | dec edx | On edx > 0, jmp NextiRecord
        jmp BadIcoSize
    Else
      pop esi
    End_If

    sub eax ebx                              ; - RVA
    add eax D$iStartOfResources              ; eax now points to true icon data
    mov B$PeIconFound &TRUE
  ret


SectNotFound:    mov eax SectionNotFound | jmp L9>
AbortIconSearch:

    If D$SavingExtension = '.DLL'
        jmp L7>
    Else_If D$SavingExtension = '.SYS'
        jmp L7>
    End_If

                    mov eax NoIcon       | jmp L9>
BadIcoSize:      mov eax BadIconSize     | jmp L9>
PeNotFound:      mov eax NoPE

L9: ;If B$Disassembling = &TRUE
    ;    mov D$iExePtr 0 | ret  ; restored by Disassembler call (used as Flag, here).
    ;End_If
    call MessageBox ;;;;| call IconSearchOut ;| pop eax |
    ret     ;;;; return to caller of caller

L7: mov B$NoResourcesPE &TRUE ;;;;| call IconSearchOut |
    ret


;IconSearchOut:
;    mov eax D$UserPeStart | On D$iExePtr <> eax, VirtualFree D$iExePtr
;  ; Don't destroy User Source in any case, if the 'readRosAsmPeIcon' Routine is called
;  ; from some Open PE ,normal Functions.
;ret

[SectionNotFound: 'Section not found', 0
 BadIconSize:     'Icon size not assumed', 0
 NoPE:            'PE signature not found', 0
 NoIcon:          'Icon not found in this file', 0]


; peek from PE:

PeekIcon:
    call OpenPeForReadingIcon

    .If D$iSourceHandle <> 0
        call ReadRosAsmPeIcon                        ; eax > start of icon data

        If B$PeIconFound = &TRUE
            mov esi eax | mov edi iIcon | rep movsb  ; Copying to ower buffer
        End_If

        call 'KERNEL32.CloseHandle' D$iSourceHandle | mov D$iSourceHandle 0

        VirtualFree D$iExePtr
    .End_If
ret


[iDestinationHandle: 0  PokeSure: 'Ready to modify choosen PE?', 0   NullTitle: ' ', 0]

; Poke inside PE:

PokeIcon:
    call OpenPeForReadingIcon                        ; eax > start of icon data

    ..If D$iSourceHandle <> 0
        call ReadRosAsmPeIcon                        ; eax > start of icon data

        .If B$PeIconFound = &TRUE
            mov edi eax | mov esi iIcon | rep movsb  ; Copying from ower buffer
            call 'KERNEL32.CloseHandle' D$iSourceHandle | mov D$iSourceHandle 0
            call 'USER32.MessageBoxA' D$hwnd  PokeSure  NullTitle,
                                    &MB_YESNO+&MB_ICONEXCLAMATION +&MB_SYSTEMMODAL
            On eax = &IDNO, jmp L9>>

            call 'KERNEL32.CreateFileA' iSaveFilter &GENERIC_WRITE,
                                        &FILE_SHARE_READ, 0,
                                        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0
            If eax = &INVALID_HANDLE_VALUE
                mov eax D$BusyFilePtr | call MessageBox | ret
            Else
                mov D$iDestinationHandle eax
            End_If

            mov D$NumberOfReadBytes  0
            call 'KERNEL32.WriteFile'   D$iDestinationHandle D$iExePtr D$iExeLen,
                                        NumberOfReadBytes  0
        .End_If

L9:     VirtualFree D$iExePtr
        call 'KERNEL32.CloseHandle' D$iDestinationHandle
    ..End_If
ret

 _______________________________________________________________________________________

 ; reading and writting .ico files:
 _______________________________________________________________________________________

[FileIconDir: W$ 0 1  FIDcount: 1

FIDentriesTable: FIwidth: B$ 020   FIheight: 020    FIcolorCount: 010  0
                 FIplanes: W$ 0     FIbitCount: 0
             FIBytesInRes: D$ 02E8  FIdwImagePtr: 022    ; * by "FIDcount" number
             FileIconDirLen: len]

[IcoFilePtr: ?  IcoFileLen: ?]

[icoFilesFilters:  B$ 'icon files'     0  '*.ico'   0  0
 OpenIcoFileTitle: 'Choose an icon file', 0]


[icoOpenStruc: len
 icohwndFileOwner: 0  icohInstance: 0  icoFilesFilters  iuFileFilter  260
 iSourceFilterPtr  iSaveFilter  260  iChoosenFile  260  0
 OpenIcoFileTitle  IOSflags: 0281804 ; for read
 0  0  0  0  0]            ; 0280006 : for write  .ico



[BadFIsiz: 'No 36/36 icon in this file (or too much colors)', 0]

BadFIsize: mov eax BadFIsiz | call MessageBox | ret


ReadIcoFile:
  ; Opening a .ico file:

    mov edi iSaveFilter, ecx 260, eax 0 | rep stosd
    mov D$IOSflags 0281804 | call 'Comdlg32.GetOpenFileNameA' icoOpenStruc
      On D$iSaveFilter = 0,  ret

  ; Loading the entire file in memory:

    call 'KERNEL32.CreateFileA' iSaveFilter &GENERIC_READ, &FILE_SHARE_READ,
                                0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
                                              ; hTemplateFile
    If eax = &INVALID_HANDLE_VALUE
      mov eax D$BusyFilePtr | call MessageBox | ret         ; return to caller of caller
    Else
      mov D$iSourceHandle eax
    End_If

    call 'KERNEL32.GetFileSize'  eax 0 | mov D$icoFileLen eax

    VirtualAlloc icoFilePtr eax
    mov D$NumberOfReadBytes 0

    call 'KERNEL32.ReadFile' D$iSourceHandle D$icoFilePtr,
                            D$icoFileLen NumberOfReadBytes 0

    mov esi D$IcoFilePtr | add esi 4
    lodsw | movzx ecx ax                             ; icons number in ecx
L0: cmp B$esi 020 | je L1>                           ; first size
      add esi 16 | loop L0<                          ; next record
        jmp BadFIsize
L1: On D$esi+8 <> 02E8, jmp BadFIsize
    add esi 12                                       ; ptr to ico data
    mov esi D$esi, edi iIcon, ecx 02E8
    add esi D$IcoFilePtr | rep movsb                 ; Copying to ower buffer

    call 'KERNEL32.CloseHandle' D$iSourceHandle

    VirtualFree D$icoFilePtr
ret


[NewOnly: 'This option saves only new files', 0]

WriteIcoFile:
  ; Opening a .ico file:

    mov edi iSaveFilter, ecx 260, eax 0 | rep stosd
    mov D$IOSflags 0288006 | call 'Comdlg32.GetSaveFileNameA' icoOpenStruc
      On D$iSaveFilter = 0,  ret

    call 'KERNEL32.CreateFileA' iSaveFilter &GENERIC_WRITE, 0, 0,
                                &CREATE_NEW, &FILE_ATTRIBUTE_NORMAL, 0

    If eax = &INVALID_HANDLE_VALUE
      mov eax NewOnly | call MessageBox | ret  ; return to caller of caller
    Else
      mov D$iDestinationHandle eax
    End_If

    mov D$NumberOfReadBytes  0
    call 'KERNEL32.WriteFile'   D$iDestinationHandle iIcoFileHeader 02FE,
                               NumberOfReadBytes  0
    call 'KERNEL32.CloseHandle' D$iDestinationHandle
ret

 ____________________________________________________________________________________


; saving icon data in user stub data for effective compilation:

StoreIcon:
    mov esi iIcon, edi uIcon, ecx 02E8 | rep movsb
ret

 ____________________________________________________________________________________
 ____________________________________________________________________________________

; Main of icon edition (dialog box Proc):

IconEdition:
    If D$IconEditorHandle = 0
        call 'User32.DialogBoxIndirectParamA' D$hinstance, IconDialogData, D$hwnd,
                                              IconEditProc, 0
    Else
        Beep
    End_If
ret


[iDraw: B$ ?  iArase: ?] [IconEditorHandle: ?]

Proc IconEditProc:
  Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    move D$DrawIconMessage D@Message

    ...If D@Message = &WM_PAINT
        call DrawIcon

    ...Else_If D@Message = &WM_VSCROLL
        call iTrackMove

    ...Else_If D@Message = &WM_LBUTTONDOWN
        mov B$iDraw &TRUE

    ...Else_If D@Message = &WM_LBUTTONUP
        push D@Lparam | pop W$MousePosX, W$MousePosY
        mov B$idraw &FALSE | call iLeft

    ...Else_If D@Message = &WM_RBUTTONDOWN
        mov B$iArase &TRUE

    ...Else_If D@Message = &WM_RBUTTONUP
        push D@Lparam | pop W$MousePosX, W$MousePosY
        mov B$iArase &FALSE | call iRight

    ...Else_If D@Message = &WM_MOUSEMOVE
        push D@Lparam | pop W$MousePosX, W$MousePosY
        If B$iDraw = &TRUE
            call iLeft
        Else_If B$iArase = &TRUE
            call iRight
        EndIf

    ...Else_If D@Message = &WM_COMMAND
        .If D@wParam = &IDCANCEL
            mov D$IconEditorHandle 0
            call 'User32.EndDialog' D@Adressee 0
        .Else_If D@wParam = ID_Inew
            mov edi iIconAndMask, ecx 128, al 0FF | rep stosb
            mov edi iIconXorMask, ecx 512, al 0   | rep stosb
            call DrawIcon
        .Else_If D@wParam = ID_Ipeek
            call PeekIcon | call DrawIcon
        .Else_If D@wParam = ID_Ipoke
            call PokeIcon
        .Else_If D@wParam = ID_IfromIco
            call ReadIcoFile | call DrawIcon
        .Else_If D@wParam = ID_ItoIco
            call WriteIcoFile
        .Else_If D@wParam = ID_iKeep
            move D$IconEditorHandle 0
            call StoreIcon | call 'User32.EndDialog' D@Adressee 0
        .Else_If D@wParam = ID_Help
            call Help, B_U_AsmName, IconHelp, ContextHlpMessage
        .Else
            popad | mov eax &FALSE | jmp L9>>
        .End_If

    ...Else_If D@Message = &WM_INITDIALOG
        move D$IconEditorHandle D@Adressee
        move D$icohInstance D$hInstance
        move D$iOPESInstance D$hInstance
        move D$icohwndFileOwner D@Adressee
        move D$ihwndPEFileOwner D@Adressee
        call CreateIconBrushes
        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

    ...Else_If D@Message = &WM_Close
        mov D$IconEditorHandle 0
        If B$OnRainbow = &TRUE
            call RestorePreviousColor
            call 'USER32.DestroyWindow' D$GreenSliderHandle
            call deleteRainbowDC
            mov B$OnRainbow &FALSE
        End_If
        popad | call DeleteIconBrushes | mov eax &FALSE | jmp L9>

    ...Else
        popad | mov eax &FALSE | jmp L9>

    ...End_If

    popad | mov eax &TRUE

L9: EndP


