TITLE SourceEdit

 ______________________________________________________________________________________
 ______________________________________________________________________________________
;;
  RosAsm Sources Editor
  
  - The Caret is Screen dependant. Not Text dependant.
  
  - The Editor is _not_ a Line-Oriented one.
  
  - The Main Routine called on WM_PAINT is 'PrintColorText'
  
  - Most minor Routines are called from 'CharMessage' or from 'KeyMessage'
  
  - The coloring is achieve through a parallele Table ('ColorsMap')
  
  __________________
  Overall mechanism: 
  
  This Editor is "File Oriented". This is to say, that, each time we insert
  or delete one single Char, all of the downward Text is moved. Over some size
  (depending on the Processor speed) the Editor looses its responsivity, once
  the Copy Operations are slwoer than the KeyBoard inputs speed.
  
  This was the first reason why a Mechanism of TITLEs was introduced, in order
  to divide the real momolitic user Source into "Sub-Sources": For Sources divided
  into TITLEs, the Editor makes a Copy of the Edited TITLE, at the End of the real
  Source, in Memory, and we always work on this isolated Part. This reduces the
  Copy operation to one smaller TITLE part, and the real Copy, into the real user
  Source is done only for each operation requiring to work with the real Source.
  
  The concerned Routines is 'RestoreRealSource' and, for going back to a TITLE
  Edition, 'SetPartialEditionFromPos'.
  
  ________________
  The user inputs:
  
  As said above ('CharMessage', 'KeyMessage') two imputs are considered:
  
  * &WM_CHAR, of course (Nothing special).
  
  * &WM_KEYDOWN / &WM_KEYUP. For these ones, the Editor manages a 'Keys' Table,
    where each Key State (1/0 -On/Off-) is stored.
    
  So, some Functionalities are driven from the &WM_CHAR, and some are driven
  from the &WM_KEYDOWN Messages.
  
    ____________________
    The output Routines.
    
    'XtoRow' 'RowToX' 'TroncatedXtoRow' 'YtoLine' 'LineToY' 'TroncatedYtoLine'
    
    'InitPrintText' 'BlankRemainders'
    
        'PrintColorText'
        
            'PrintCaretAndUnderline' 'GetLastCharPosOnScreen' 'AjustForRightScroll'
        
            'SetColorsMap' 'TextColorsMap'  'TextOutput'
    
            'InitPrintText'  'ClosePrint'
    
    ______________
    Minor Routines
   
    'MouseTextPos' 'SimpleMouseTextPos'
    
    'DownOneLine' 'UpOneLine' 'FullDown' 'OnlyOnePageDown' 'FullUp' 'OnlyOnePageUp'
    
    'AutoDeleteBlock'
    
    'OverwriteSource' 'InsertSource' 'InsertDoubleByte'
    
    'SetIndent' 'CarriageReturn'
    
    'TryToMove'
    
    'ResetCaretOnStripTab' 'StripBackSpace' 'BackSpace'
    
    'KeyDown' 'KeyUp' 'StartOfLine' 'KeyLeft'
    
    'SetPhysicalCaretRow'
    
    'StartOfWord' 'EndOfLine' 'KeyRight' 'EndOfWord'
    
    'KeyInsert' 'StripOneChar' 'KeyDelete'
    
    'SearchTxtPtr'
    
    'SetCaret' 'LeftButton'
    
    'SetBlock'
    
    'LeftButtonUp'
    
    'ControlC' 'ControlY' 'ControlD' 'ControlX' 'UndoControlV'
    
    'OpenClipBoard' 'ClipBordCleaner' 'KillTabs' 'CloseClipBoard'
    
    'ControlV'
    
    'ReMapSourceMemoryIfNeeded'
    
    'DrawOneLine'
    
    ____________
    Undo feature
    
    'InitUndo' 'KillUndo' 'ReInitUndo' 'DoStoreInsert' 'DoStoreOverWrite'
    'DoStoreCharDelete' 'ReInsertDeletedChar' 'DoStoreBlockPaste'
    
    'ResetUndoFileName' 'IncUndoFileName' 'DecUndoFileName'
    
    'WriteUndoBlockFile'
    
    'DeleteUndoFiles' 'DeleteOldUndoFiles'
    
    'UndoBlockCopy' 'ReadUndoBlockFile'
    
    'ControlZ' 'ReadUndoRecord'
    
    ____________________
    Main Redraw Routines
    
    'AskForRedraw' 'AskForRedrawNow'
    
    _____________________
    Text Pos and searches
    
    'TextPos' 'RedrawScrollBar' 'RePosFromScroll'
    
    'KillTrailingSpaces'
    
    'GetWheelInfo' 'WheelMsg'
    
    'StartEdition' 'ReplaceParagraphByDollar'
    
    'CloseHelp'
    
    'InternSearch'

    'SavePosOnF11' 'SetPosOnF12' 'ClearF11F12' 'ClearF12'
    
    _____
    Fonts (called from the Configuration Tab or from main
    
    'ChooseFontHook' 'SelectFont' 'LoadFont'
;;

; looks: Red , Green , Blue , 0  when bytes,     0/Blue/Green/Red  when dWord

[NormalTextColor: B$    0   0   0 0    ; color for Statements or brackets
 CaretColor:          150 150 150 0    ; (xor 00_01101001 for Insert Mode... avoid modifying)
 PreviousNormalColor:   0   0   0 0]

[PAINTSTRUCT: PShdc: 0  PSfErase: 0  PSrcPaint: 0 0 0 0
              PSfRestore: 0  PSfIncUpdate: 0
              PSrgbReserved: 0 0 0 0  0 0 0 0]

[RECT: RECTleft: ?  RECTtop: ?  RECTright: ?  RECTbottom: ?]
____________________________________________________________________________________________

[EditData:  ; 21 dwords
 hFont: 0
 Col: 0  Line: 0  ; Col2: 0  Line2: 0
 hdc: 0  CurrentWritingPos: 0 ColNumber: 8  LineNumber: 0
 SourceEnd: 0  CaretLine: 0  CaretRow: 1  CaretRowValue: 8  PhysicalCaretRow: 1
 Caret: 0  RightScroll: 0
 CaretEndOfLine: 0  Overwrite: 0  StartOfNormalText: 0  StartOfComment: 0
 StartOfBlockText: 0  UpperLine: 0]

____________________________________________________________________________________________

; SourceEnd: points AFTER the last byte (so that D$SourceEnd-D$CodeSource = D$SourceLen).

[BlankLine: B$ 32 #200, 0] [TRANSPARENT 1]

[BackGroundBrushHandle: ?   DialogsBackGroundBrushHandle: ?]

[Font1Handle: ?]
____________________________________________________________________________________________

Proc XtoRow:
    Argument @X
    Uses edx, ecx

        mov edx 0, eax D@X, ecx D$FontWidth | div ecx
      ; Round up:
        shr ecx 1 | On edx > ecx, inc eax
EndP


[XRemainder: ?    YRemainder: ?]

Proc TroncatedXtoRow:
    Argument @X
    Uses edx

        mov edx 0, eax D@X | div D$FontWidth
        mov D$XRemainder edx
EndP


Proc YtoLine:
    Argument @Y
    Uses edx, ecx

        mov edx 0, eax D@Y, ecx D$FontHeight | div ecx
      ; No 'HalfWay' adjustement to do: The Hot Point of the default Edit-type Cursor
      ; seems to be at the bottom of its shape...
EndP

Proc TroncatedYtoLine:
    Argument @Y
    Uses edx, ecx

        mov edx 0, eax D@Y | div D$FontHeight
        mov D$YRemainder edx
EndP


Proc RowToX:
    Argument @Row
    Uses edx

        mov eax D@Row | mul D$FontWidth
EndP


Proc LineToY:
    Argument @Line
    Uses edx

        mov eax D@Line | mul D$FontHeight
EndP
____________________________________________________________________________________________

; If the division of the writing area by the Font size is not exact (no remainder)
; redraw this background part.

BlankRemainders:
    If D$XRemainder <> 0
        call 'USER32.GetClientRect' D$EditWindowHandle, AraseBackEdit
        move D$AraseBackEdit D$AraseBackEdit+8
        mov eax D$XRemainder | sub D$AraseBackEdit eax
        call 'USER32.FillRect' D$hdc, AraseBackEdit, D$BackGroundBrushHandle
    End_If

    If D$YRemainder <> 0
        call 'USER32.GetClientRect' D$EditWindowHandle, AraseBackEdit
        move D$AraseBackEdit+4 D$AraseBackEdit+12
        mov eax D$YRemainder | sub D$AraseBackEdit+4 eax
        call 'USER32.FillRect' D$hdc, AraseBackEdit, D$BackGroundBrushHandle
    End_If
ret
____________________________________________________________________________________________

[SourceEndReached: ?    EditRightPixel: ?    EditBottomPixel: ?]

InitPrintText:
    call 'User32.BeginPaint' D$EditWindowHandle, PAINTSTRUCT | mov D$hdc eax

    call 'GDI32.SelectObject'  D$hdc D$Font1Handle | mov D$hfont eax
    call 'GDI32.SetBkColor' D$hdc D$NormalBackColor

    call TroncatedXtoRow D$EditWindowW | mov D$ColNumber eax
    call RowToX eax | mov D$EditRightPixel eax

    ;If D$TitleWindowHandle <> 0
    ;    mov eax D$TabWindowH | sub D$EditWindowH eax
    ;End_If

    call TroncatedYtoLine D$EditWindowH | mov D$LineNumber eax
    call LineToY eax | dec eax | mov D$EditBottomPixel eax

    On B$CaretOnlyRedraw = &FALSE, call BlankRemainders

    dec D$LineNumber | dec D$ColNumber

    mov B$TextGoingOn &FALSE | move D$NormalTextColor D$StatementColor
    mov B$SourceEndReached &FALSE
ret


[MarginLine: @X1: ? @Y1: ?   @X2: ? @Y2: ?]

DrawMarginLine:
    mov eax D$FontWidth | dec eax | mov D$MarginLine@X2 eax
    dec eax | mov D$MarginLine@X1 eax
    mov D$MarginLine@Y1 0
    move D$MarginLine@Y2 D$EditWindowH

    call 'USER32.FillRect' D$hdc, MarginLine, D$CaretBrushHandle
ret


RemoveCaretAndUnderline:
    If D$CaretRectangle+8 <> 0
        call 'USER32.FillRect'  D$hdc CaretRectangle D$BackGroundBrushHandle
        mov D$CaretRectangle+8 0
    End_If

    If D$UnderLineRectangle+8 <> 0
        call 'USER32.FillRect' D$hdc, UnderLineRectangle, D$BackGroundBrushHandle
    End_If
ret

ClosePrint:
    call 'GDI32.SelectObject' D$hdc D$hfont
    call 'USER32.EndPaint' D$EditWindowHandle, PAINTSTRUCT

    mov B$TextGoingOn &FALSE
ret
____________________________________________________________________________________________

[TabOldCaretRow: 1    Underline: &FALSE]

[CaretRectangle: ? ? ? ?   CaretBrushHandle: ?  RedBrushHandle: ?
 UnderLineRectangle: ? ? ? ?]

PrintCaretAndUnderline:
    mov eax D$LineNumber | On D$CaretLine > eax, mov D$CaretLine eax

    mov esi D$UpperLine, ecx 0

L0: cmp ecx D$CaretLine | je L2>                 ; search for Caret line (esi)

    On ecx > D$LineNumber, ret
L1: lodsb | cmp al LF | jne L1<
    inc ecx | jmp L0<

L2: mov B$CaretEndOfLine &FALSE, ecx 0
    On D$RightScroll > 0, sub ecx D$RightScroll

L0: cmp ecx D$CaretRow | je L5>>
L1: lodsb | cmp al tab | je L3>
            cmp al CR | ja L2>                      ; search for Caret col (esi too)
      mov B$CaretEndOfLine &TRUE | inc ecx
      mov B$Caret 32, ebx ecx, D$PhysicalCaretRow ebx | jmp L6>>   ; Caret at end of line
L2: inc ecx | jmp L0<

L3: mov ebx D$CaretRow, edx ecx | or ecx 00_111 | inc ecx | On ebx > ecx, Jmp L0<

    inc edx | mov D$CaretRow edx | cmp ebx D$TabOldCaretRow | jbe L5>

    If edx = D$TabOldCaretRow
        inc ecx | mov D$CaretRow ecx | lodsb  ; just for insert Pos on Tab
    End_If

L5: mov B$Caret al, ebx D$CaretRow, D$PhysicalCaretRow ebx
L6: dec esi | mov D$CurrentWritingPos esi | mov D$TabOldCaretRow ebx

    call LineToY D$CaretLine | mov ecx eax
    call RowToX ebx | mov ebx eax

    mov D$CaretRowValue ebx
    On esi >= D$SourceEnd, mov B$Caret ' '

    If ecx > 0
        mov eax D$FontHeight | shr eax 2 | sub ecx eax
    Else
        mov eax 0
    End_If
    mov D$CaretRectangle ebx, D$CaretRectangle+4 ecx
    add ebx 3 | add ecx eax | add ecx D$FontHeight
    mov D$CaretRectangle+8 ebx, D$CaretRectangle+12 ecx

  ; Let Overwrite Dims a special case to let max speed to the normal Caret:
    If B$Overwrite = &TRUE
        mov eax D$FontWidth | add eax D$CaretRectangle | mov D$CaretRectangle+8 eax
        mov eax D$FontHeight | shr eax 2 | add D$CaretRectangle+4 eax
    End_If

    On B$ShowCaret = &TRUE, call 'USER32.InvertRect' D$hdc, CaretRectangle

    .If B$Underline = &TRUE
        move D$UnderLineRectangle D$CaretRectangle,
             D$UnderLineRectangle+4 D$CaretRectangle+4,
             D$UnderLineRectangle+8 D$CaretRectangle+8,
             D$UnderLineRectangle+12 D$CaretRectangle+12

        mov ebx D$CurrentWritingPos | dec ebx
        mov eax D$FontWidth
        While B$ebx > ' ' | dec ebx | sub D$UnderLineRectangle eax | End_While
        mov eax D$FontHeight | inc eax
        add D$UnderLineRectangle+4 eax | sub D$UnderLineRectangle+8 6
        mov eax D$UnderLineRectangle+4 | add eax 2 | mov D$UnderLineRectangle+12 eax

        call 'USER32.FillRect' D$hdc, UnderLineRectangle, D$CaretBrushHandle
    .End_If
ret

____________________________________________________________________________________________

[RosAsmBracket: ?    MultiLinesComment: ?]

[MLC 0D3B3B0A] ; MLC: Multi-Lines Comment (LF ; ; CR). Set first in 'SearchForBrackets'.

____________________________________________________________________________________________
____________________________________________________________________________________________

[LastCharPosOnScreen: ?]

GetLastCharPosOnScreen:
  ; LineNumber is zero based:
    mov eax D$LineNumber | inc eax

    mov esi D$UpperLine

L0: .If B$esi < ' '
        If B$esi = CR
            On B$esi+1 <> LF, mov B$esi ' '
        Else_If B$esi = LF
            On B$esi-1 <> CR, mov B$esi ' '
        Else
            mov B$esi ' '
        End_If
    .End_If

    inc esi | cmp B$esi CR | jne L0<
    dec eax | jnz L0<

    inc esi | mov D$LastCharPosOnScreen esi
ret
____________________________________________________________________________________________

[WantSizeMarkerColor: &TRUE]

Proc SetColorsMap:
    Argument @First, @Last, @Color

        mov esi D@First, edx D@Last | add edx 100
        mov edi esi | sub edi D$CodeSource | add edi D$ColorsMap
        mov bl B@Color | On bl = 0, mov bl 1

L0:     .While esi < edx
            mov al B$esi

            ; Normal = 1 // Brackets = 2 // Strings = 3 // Comments = 4 // Selection 5
            ...If al = "'"
                Do
                    If B$esi >= ' '
                        mov B$edi 3
                    Else
                        mov B$edi 0
                    End_If
                    inc esi | inc edi | cmp esi edx | je L9>>
                Loop_Until B$esi = "'"
                mov B$edi 3 | inc esi | inc edi | jmp L0<

            ...Else_If al = '"'
                mov eax esi
                Do
                    If B$esi >= ' '
                        mov B$edi 3
                    Else
                        mov B$edi 0
                    End_If
                    inc esi | inc edi | cmp esi edx | je L9>>
                Loop_Until B$esi = '"'
                mov B$edi 3 | inc esi | inc edi | jmp L0<<

            ...Else_If al = '['
                mov bl 2, B$edi 2
                inc esi | inc edi | jmp L0<<

            ...Else_If al = ']'
                mov B$edi 2, bl 1
                inc esi | inc edi | jmp L0<<

            ...Else_If B$esi = ';'
                .If D$esi-1 = MLC
                    mov eax esi
                    Do
                        If B$esi < ' '
                            mov B$edi 0
                        Else
                            mov B$edi 4
                        End_If
                        inc esi | inc edi | cmp esi edx | je L9>>
                    Loop_Until D$esi = MLC
                    mov B$edi 0, W$edi+1 0404, B$edi+3 0
                    mov B@Color 0
                    add esi 4 | add edi 4 | jmp L0<<
                .Else
                    Do
                        mov B$edi 4
                        inc esi | inc edi | cmp esi edx | je L9>>
                    Loop_Until B$esi < ' '
                    mov B@Color 0
                    jmp L0<<
                .End_If

            ...Else_If al < ' '
                Do
                    mov B$edi 0
                    inc esi | inc edi | cmp esi edx | je L9>>
                Loop_Until B$esi >= ' '
                jmp L0<<

            ...Else_If al = 36
                mov al B$ParagraphChar, B$esi al
                mov B$edi bl | On B$WantSizeMarkerColor = &TRUE, xor B$edi 0011
                inc esi | inc edi | jmp L0<<

            ...Else_If al = 167
                mov al B$ParagraphChar, B$esi al
                mov B$edi bl | On B$WantSizeMarkerColor = &TRUE, xor B$edi 0011
                inc esi | inc edi | jmp L0<<

            ...Else_If al = '@'
                mov B$edi bl
                On B$WantSizeMarkerColor = &TRUE, xor B$edi 0011
                inc esi | inc edi | jmp L0<<

            ...Else
L1:             mov B$edi bl | inc esi | inc edi | jmp L0<<

            ...End_If

        .End_While

      ; Arase any remaining color flag at the end of the last line
        mov ecx 100, al 0 | rep stosb

L9:     If B$BlockInside = &TRUE
            mov esi D@First, edx D@Last, eax D$BlockStartTextPtr, ebx D$BlockEndTextPtr

            On eax > ebx, xchg eax ebx

            On eax < esi, mov eax esi
            On ebx > edx, mov ebx edx

            sub eax D$CodeSource | add eax D$ColorsMap
            sub ebx D$CodeSource | add ebx D$ColorsMap | inc ebx

          ; Normal = 1 // Brackets = 2 // Strings = 3 // Comments = 4 // Selection 5
            While eax < ebx | On B$eax <> 0, mov B$eax 5 | inc eax | End_While
        End_If
;;
        ..If B$WantSizeMarkerColor = &TRUE
            mov esi D@First, edx D@Last, eax esi, ebx edx

            sub eax D$CodeSource | add eax D$ColorsMap
            sub ebx D$CodeSource | add ebx D$ColorsMap

            .While esi < edx
                .If B$esi = 167     ; Paragraph Char
                    If B$eax = 1
                        mov B$eax 2
                    Else_If B$eax = 2
                        mov B$eax 1
                    End_If
                .Else_If B$esi = 36 ; Dollar Char
                    If B$eax = 1
                        mov B$eax 2
                    Else_If B$eax = 2
                        mov B$eax 1
                    End_If
                .Else_If B$esi = '@'
                    If B$eax = 1
                        mov B$eax 2
                    Else_If B$eax = 2
                        mov B$eax 1
                    End_If
                .End_If
                
                inc eax | inc esi
            .End_While
        ..End_If
;;
EndP
____________________________________________________________________________________________

[ColorsMap: ?    ColorMapSize: ?]

[BlockStartTextPtr: D$ ?  BlockEndTextPtr: ?  ReverseBlock: B$ ?]

TextColorsMap:
    mov eax D$SourceLen | add eax 300 | Align_On 01000 eax

    If D$ColorMapSize < eax
        mov D$ColorMapSize eax
        VirtualFree D$ColorsMap
        VirtualAlloc ColorsMap, D$ColorMapSize
    End_If
ret
____________________________________________________________________________________________

[ParagraphChar: 36] ; 36 or 167 (Dollar or Paragraph)

TextOutput:
    mov esi D$UpperLine, ebx esi | sub ebx D$CodeSource | add ebx D$ColorsMap
    move D$Col D$FontWidth | mov D$Line 0

    push D$LineNumber
        inc D$LineNumber

        On D$RightScroll <> 0, call AjustForRightScroll

        push ebx
            call 'GDI32.TextOutA' D$hdc, 0, D$Line, BlankLine, 1
        pop ebx

      ; Set the Color for a Chunk of Text:
L0:     movzx eax B$ebx
        If al <> 5
            mov eax D$StatementColor+eax*4-4
            push esi, ebx
                push eax
                    call 'GDI32.SetBkColor' D$hdc D$NormalBackColor
                pop eax
                call 'GDI32.SetTextColor' D$hdc, eax
            pop ebx, esi
        Else
            push esi, ebx
                call 'GDI32.SetBkColor' D$hdc D$CaretColor
                call 'GDI32.SetTextColor' D$hdc D$NormalBackColor
            pop ebx, esi
        End_If

      ; How many Chars in that Color Chunk, into ecx:
        mov al B$ebx, ecx 0
        If al <> 0
            While B$ebx = al
                inc ebx | inc ecx
            End_While
        Else
          ; CR/LF:
            add ebx 2 | mov ecx 2 | dec D$LineNumber
        End_If

      ; Output:
        push esi, ebx, ecx
            ..If al <> 0
              ; Normal output:
                call RowToX ecx
              ; > eax = Number of Pixels to output.
                push eax
                  ; Limit to the available number of Chars:
                    add eax D$Col
                    If eax > D$EditRightPixel
                        pop ecx
                            mov ecx D$EditRightPixel | sub ecx D$Col
                        push ecx
                        call XToRow ecx | mov ecx eax
                    End_If
                    .If ecx <> 1
                        call 'GDI32.TextOutA' D$hdc, D$Col, D$Line, esi, ecx
                    .Else
                        If B$esi = 36
                            call 'GDI32.TextOutA' D$hdc, D$Col, D$Line, ParagraphChar, ecx
                        Else_If B$esi = 167
                            call 'GDI32.TextOutA' D$hdc, D$Col, D$Line, ParagraphChar, ecx
                        Else
                            call 'GDI32.TextOutA' D$hdc, D$Col, D$Line, esi, ecx
                        End_If
                    .End_If
                pop eax
                add D$Col eax

            ..Else
              ; CR/LF:
                mov eax D$EditRightPixel
                If eax > D$Col
                  ; Here we "blank" the End of Line:
                    sub eax D$Col | call XToRow eax
                    push ecx
                        call 'GDI32.TextOutA' D$hdc, D$Col, D$Line, BlankLine, eax
                    pop ecx
                End_If

              ; And here, we Blank the next Row Zero (DCBP,...):
                move D$col D$FontWidth | mov eax D$FontHeight | add D$Line eax
                On D$LineNumber > 0, call 'GDI32.TextOutA' D$hdc, 0, D$Line, BlankLine, 1

                If D$RightScroll <> 0
                    pop ecx, ebx, esi
                        call AjustForRightScroll
                    push esi, ebx, ecx
                End_If

            ..End_If
        pop ecx, ebx, esi

        add esi ecx | cmp D$LineNumber 0 | ja L0<<
    pop D$LineNumber
ret
____________________________________________________________________________________________

;;
  Outputs the Char under the Cursor. Usefull in case when the Blinking Cursor is
  set On, in Config, and that the partial redraw is called by 'BlinkProc',
  particulary in OverWrite Mode, that, otherwise, would leave this char blanked.
;;

CharOutput:
    mov esi D$UpperLine, eax 0, ebx 1, edx D$ColorsMap

    While esi < D$CurrentWritingPos
        If B$esi = CR
            add esi 2 | add edx 2
            mov ebx 1 | inc eax
        Else
            inc ebx | inc esi | inc edx
        End_If
    End_While

    call LineToY eax

    push eax | call RowToX ebx | pop ebx

    pushad
        If B$edx <> 5
            mov eax D$StatementColor+eax*4-4
            push eax
                call 'GDI32.SetBkColor' D$hdc D$NormalBackColor
            pop eax
            call 'GDI32.SetTextColor' D$hdc, eax
        Else
            call 'GDI32.SetBkColor' D$hdc D$CaretColor
            call 'GDI32.SetTextColor' D$hdc D$NormalBackColor
        End_If
    popad

    On B$esi <> CR, call 'GDI32.TextOutA' D$hdc, eax, ebx, esi, 1
ret
____________________________________________________________________________________________

AjustForRightScroll:
    mov eax D$RightScroll

L0: cmp B$ebx 0 | je L9>

    inc esi | inc ebx | dec eax | cmp eax 0 | ja L0<

L9: ret

;;
  We do not search directely for _the_ previous Color (far too complicated). We search
  for an upward Position that will give a proper start point for building the ColorMap.
  
  So, we:
  
  * exclude the Text Color (too many possible cases).
  
  * exclude the Comments cases (because of Multi-Lines Comments complications).
  
  We simply search for the next upper Line starting with either Statement or Data
  color. 'GetStartUpColorPos' return this Color (eax) and the according Pos in the
  user's SourceCode (esi). If Top of File is reached, we return the Statement Color.
;;

[PreviousStartUpColour: ?   PreviousStartUpColourPos: ?]

GetStartUpColorPos:
    mov esi D$UpperLine | sub esi D$CodeSource | add esi D$ColorsMap
    mov edx D$ColorsMap

  ; CRLF = 0 // Normal = 1 // Brackets = 2 // Strings = 3 // Comments = 4 // Selection 5
    While esi > edx
        .If B$esi = 0
            If B$esi+1 = 1
                mov eax 1 | jmp L5>
            Else_If B$esi+1 = 2
                mov eax 2 | jmp L5>
            End_If

L0:         dec esi
        .End_If

        dec esi
    End_While

  ; Top of File reached:
    move D$PreviousStartUpColourPos D$CodeSource
    mov D$PreviousStartUpColour 1  | ret

  ; Start point found. Check cases of Local Symbols label at first Row:
L5: inc esi | mov ebx esi
    sub ebx D$ColorsMap | add ebx D$CodeSource | On B$ebx = '@', jmp L0<

    sub esi D$ColorsMap | add esi D$CodeSource
    mov D$PreviousStartUpColourPos esi
    mov D$PreviousStartUpColour eax | ret
____________________________________________________________________________________________

____________________________________________________________________________________________

[PreviousUpperLine: ?]

PrintColorText:
    On B$SourceReady = &FALSE, ret
   ; On D$TitleFontHandle <> 0, ret

    call TextColorsMap | call InitPrintText | On D$LineNumber <s 1, ret

    If B$CaretOnlyRedraw = &TRUE
        call RemoveCaretAndUnderline | call CharOutput | call PrintCaretAndUnderline
        mov B$CaretOnlyRedraw &FALSE | jmp L9>>
    End_If

    call GetLastCharPosOnScreen

    mov eax D$PreviousUpperLine
    If eax <> D$Upperline
        call SetColorsMap D$CodeSource, D$LastCharPosOnScreen, 1
    Else
        call GetStartUpColorPos
        call SetColorsMap D$PreviousStartUpColourPos,
                          D$LastCharPosOnScreen, D$PreviousStartUpColour
    End_If

    call RemoveCaretAndUnderline | call TextOutput | call PrintCaretAndUnderline

    move D$PreviousUpperLine D$UpperLine

    ;If D$BreakPointsTables <> 0
    ;    On D$RightScroll = 0, call DrawTheRedPlots
    ;End_If

L9: ;call DrawMarginLine

    call ClosePrint
ret


PrintBp:
    On B$BpLineDrawn = &FALSE, call DrawBpLine

    If D$BreakPointsTables <> 0
        call DrawTheRedPlots
    Else
        call BlankMargin
    End_If
ret


[BpLineDrawn: ?
 BpLine:
 BpLineX1: ? BpLineY1: ? BpLineX2: ? BpLineY2: ?]

DrawBpLine:
    call 'USER32.BeginPaint' D$BpWindowHandle, PAINTSTRUCT | mov D$hdc eax

    call DrawTheBpLine

    call 'USER32.EndPaint' D$BpWindowHandle, PAINTSTRUCT

    mov B$BpLineDrawn &TRUE
ret

DrawTheBpLine:
    call 'USER32.GetClientRect' D$BpWindowHandle, BpLine

    mov eax D$BpLineX2 | dec eax | mov D$BpLineX1 eax

    call 'USER32.FillRect' D$hdc, BpLine, D$CaretBrushHandle
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 Once user has set the caret, let us say, in the middle of a line, and then moves
 up or down, we do not want the caret horizontal position to be reset definitively
 at left if vertical movement crosses some empty lines (or to left if shorter). So:
 Caret has 2 possible horizontal positions: the one we see at screen (Physical)
 and a virtual one (or physical, if the same), that allow keeping a raw displacement
 while jumping on shorter lines. 'PhysicalCaretRow' is set by writing routine;
 'CaretRow' is set by actions routines. When user hits a key driving an horizontal
 mouvement, we clear this virtual difference if any:
;;

[RealCaretRow | move D$CaretRow D$PhysicalCaretRow]

[MousePosX: ?   MousePosY: ?]

[GetMousePos | push D@Lparam | pop W$MousePosX, W$MousePosY]

[ClickOnMargin: ?  DoubleClickOnMargin: ?]

MouseTextPos:
    call YtoLine D$MousePosY | mov ebx eax
    call XtoRow D$MousePosX

L9: ;If eax < 1
    ;    mov B$ClickOnMargin &TRUE | mov eax 1
    ;Else
    ;    mov B$ClickOnMargin &FALSE
    ;End_If

    On eax = 0, mov eax 1

  ; > eax = Row from start of line // ebx = Line
ret


; Same as above, but with no adjustement (this is for the Mouse Blocks Selections):

SimpleMouseTextPos:
   ; mov eax D$MousePosX, ebx D$MousePosY
   ; shr eax 3 | shr ebx 4
    call YtoLine D$MousePosY | mov ebx eax
    call XtoRow D$MousePosX

    On eax = 0, inc eax
ret                            ; > eax = col from start of line   ebx = line


DownOneLine:
    push eax, ebx, ecx
        mov esi D$UpperLine
L0:     lodsb | cmp al LF | jne L0<                     ; New pos OK:

        mov ebx esi, ecx D$LineNumber
L0:     lodsb | On esi > D$SourceEnd, jmp L9>           ; abort if end of text found
            cmp al LF | jne L0<
        loop L0<
        mov D$UpperLine ebx
L9: pop ecx, ebx, eax
ret


UpOneLine:
    push eax
        mov esi D$UpperLine
        std
            lodsw
L0:         lodsb | cmp al LF | jne L0<
            add esi 2
            If esi >= D$CodeSource
                mov D$UpperLine esi
            Else
                move D$UpperLine D$CodeSource
            End_If
        cld
    pop eax
ret


FullDown:
    move D$UpperLine D$SourceEnd

  ; call OnlyOnePageUp
    mov ecx D$LineNumber | On ecx > 1, dec ecx
L0: call UpOneLine | loop L0<

    call OnlyOnePageDown
ret


OnlyOnePageDown:
L1: mov ebx D$LineNumber

    If D$CaretLine < ebx
        mov esi D$UpperLine, ecx D$LineNumber, ebx 0
L2:     lodsb | On esi > D$SourceEnd, jmp L3>
        cmp al LF | jne L2<
        inc ebx
        loop L2<
L3:     mov D$CaretLine ebx
    Else
        mov ecx ebx | On ecx > 1, dec ecx
L4:     push ecx
            call DownOneLine
        pop ecx | loop L4<
    End_If
ret


FullUp:
    move D$UpperLine D$CodeSource | mov D$CaretLine 0
ret

OnlyOnePageUp:
    mov eax D$LineNumber
    If D$CaretLine > 0
        mov D$CaretLine 0
    Else
        mov ecx eax | On ecx > 1, dec ecx
L0:     call UpOneLine | loop L0<
    End_If
ret


[BlockAutoDelete: &FALSE]

AutoDeleteBlock:
    If B$OldBlockInside = &TRUE
        .If B$SimulatedBlock <> &TRUE
            push eax
                mov B$BlockInside &TRUE | call ControlD | call AskForRedrawNow
            pop eax
        .End_If
    End_If
ret


[RIGHT_FEED 8]

OverwriteSource:
    On B$BlockAutoDelete = &TRUE, call AutoDeleteBlock

    mov edi D$CurrentWritingPos
    On B$edi = CR, jmp InsertSource
  ;  On eax = Tab, jmp TabCarret
    mov cl B$edi

    On al < 32, ret

    stosb

    push eax
        mov eax D$ColNumber
        If D$CaretRow < eax
            inc D$CaretRow
        Else
            add D$RightScroll RIGHT_FEED
            sub D$CaretRow 7
        End_If
    pop eax

    call DoStoreOverWrite
ret


[InsertedChar: ?]

InsertSource:   ; eax = Char.
    On B$BlockAutoDelete = &TRUE, call AutoDeleteBlock
InsertSourceOnBlockIndent:
    RealCaretRow

    Agree  al = 13, al = 10, al = tab
    Reject  al < 32  ;, al > 126

    mov esi D$SourceEnd | add esi 400 | mov edi esi      ; 400 is security 13/10/...
    mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx

    If al = Tab
        mov B$InsertedChar ' ' | jmp L2>
    Else
        mov B$InsertedChar al
    End_If

L1: inc edi
    std
        rep movsb | stosb
    cld
    mov ebx 1 | jmp L3>

L2: mov eax D$CaretRow | add eax D$RightScroll
    add eax D$TabIs | dec eax
    mov ebx D$TabIs | neg ebx | and eax ebx
    mov ebx eax
    sub ebx D$CaretRow | sub ebx D$RightScroll | inc ebx
    add edi ebx
        std
            rep movsb | mov al ' ', ecx ebx | rep stosb
        cld

L3: add D$SourceLen ebx | add D$CurrentWritingPos ebx | add D$SourceEnd ebx

    mov eax D$ColNumber

    cmp D$CaretRow  eax | jae L4>
      add D$CaretRow ebx | jmp L9>
L4:   push ebx
      AlignOn RIGHT_FEED ebx | add D$RightScroll ebx | sub D$CaretRow ebx | inc D$CaretRow
      pop ebx

L9: call DoStoreInsert

    .If B$CompletionWanted = &TRUE
      ; To prevent from runing when doing the Substitution:
        If B$CompletionRuning = &FALSE
            call CodeComplete | mov B$CompletionRuning &FALSE
        End_If
    .End_If

ret
____________________________________________________________________________________________

InsertDoubleByte:
    cmp ah 0 | jne L1>
    call InsertSource
    ret

L1: mov esi D$SourceEnd | add esi 400 | mov edi esi
    mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx

L2: add edi 2
        std
            rep movsb | stosb | mov al ah | stosb
        cld
        mov ebx 2

L3: add D$SourceLen ebx | add D$CurrentWritingPos ebx | add D$SourceEnd ebx

    mov eax D$ColNumber

    cmp D$CaretRow  eax | jae L4>
    add D$CaretRow ebx | jmp L9>
L4: push ebx
        AlignOn RIGHT_FEED ebx | add D$RightScroll ebx | sub D$CaretRow ebx | dec D$CaretRow
    pop ebx
L9: call DoStoreInsert
ret
____________________________________________________________________________________________

;;
 [Carriage Return] includes an indent feature. As many spaces may have to be added
 at once, we do not simply call to "InsertSource" for each but, instead insert all
 spaces at once for speed reasons (we make a move on the entire tail of file...):
;;

[AutoIndent: 0   AutoIndentFlag: &TRUE]

SetIndent:
    push eax
        mov D$AutoIndent 0, esi D$CurrentWritingPos

L0:     dec esi | cmp B$esi LF | ja L0<

        inc esi                             ; Start of Current Line.

        .If B$esi+2 = ':'
            add esi 3 | mov D$AutoIndent 3
            If esi > D$CurrentWritingPos
              ; Cases of CRLF from  a Pos before a Local Label:
                sub esi 3 | sub D$AutoIndent 3
            End_If
        .End_If

        While esi < D$CurrentWritingPos
            lodsb | cmp al ' ' | jne L9>
                inc D$AutoIndent
        End_While
L9: pop eax
ret


CarriageReturn:
    .If B$BlockAutoDelete = &TRUE
        If B$OldBlockInside = &TRUE
            call AutoDeleteBlock | ret
        End_If
    .End_If

    On B$AutoIndentFlag = &TRUE, call SetIndent
    mov eax CR | call InsertSource | mov eax LF | call InsertSource

    If D$AutoIndent > 0                                ; instead of "InsertSource":
        mov esi D$SourceEnd | add esi 400
        mov edi esi | add edi D$AutoIndent | mov ecx esi
        sub ecx D$CurrentWritingPos | inc ecx
        std
            rep movsb | mov ecx D$AutoIndent, al ' ' | rep stosb
        cld
        mov eax D$AutoIndent
        add D$SourceLen eax | add D$CurrentWritingPos eax | add D$SourceEnd eax
    End_If

    mov D$RightScroll 0 | move D$CaretRow D$AutoIndent
    inc D$CaretRow | mov eax D$LineNumber ;;;| mov D$AutoIndent 0   ; in case flag changed

    If D$CaretLine = eax
        call DownOneLine
    Else
        inc D$CaretLine
    End_If

    mov ebx D$AutoIndent     ; If AutoIndent on, we have to memorize this event now
    If ebx > 0               ; (after CR/LF recording. Undo will hold 2 jobs for this:
        mov D$InsertedChar ' '
        call DoStoreInsert   ; one for undo indent and another for undo CR/LF).
    End_If
ret
____________________________________________________________________________________________

;;
 Instead of searching if end of text is on screen, we try to move one line up: if it
 is possible, we try to move one line down. If it is unpossible, end of text is on
 screen and the one-line-up mouvement is validate:
;;

[CanGoUp: B$ ? CanGoDown: B$ ?]

TryToMove:
    On D$LineNumber <s 1, ret

L0: mov B$CanGoUp &FALSE, B$CanGoDown &FALSE,  esi D$UpperLine     ; try Up One Line:
    std
        lodsw
L1:     lodsb | cmp al LF | ja L1<
        add esi 2
    cld
    mov edx esi | On esi > D$CodeSource, mov B$CanGoUp &TRUE

    mov esi D$UpperLine, ecx D$LineNumber       ; try down one line:

L1: lodsb | cmp al LF | ja L1<
    loop L1<
    add esi 2 | On esi < D$SourceEnd, mov B$CanGoDown &TRUE

    On B$CanGoDown = &TRUE, ret
    On B$CanGoUp = &FALSE, ret

    mov D$UpperLine edx | inc D$CaretLine       ; up one line (down false / up true)
    jmp L0<
____________________________________________________________________________________________

ResetCaretOnStripTab:
    pushad
        std
L0:         lodsb | cmp al LF | jne L0<
        cld
        add esi 2 | mov ecx 0
L0:     lodsb | inc ecx | cmp al 255 | je L9>
            cmp al tab | jne L0<
                Align_on 8 ecx | jmp L0<
L9:     inc ecx | mov D$CaretRow ecx, D$PhysicalCaretRow ecx
    popad
ret


StripBackSpace:
    mov esi D$CurrentWritingPos
    If B$esi-1 = tab
        mov B$esi-1 255 | call ResetCaretOnStripTab
    End_If
    mov edi esi | dec edi, D$CurrentWritingPos, D$SourceLen, D$SourceEnd
    mov ecx D$SourceEnd | sub ecx D$CurrentWritingPos | add ecx 0100 | rep movsb
ret


BackSpace:
    ..If B$BlockAutoDelete = &TRUE
        .If B$OldBlockInside = &TRUE
            If al <> Tab
                call AutoDeleteBlock | ret
            End_If
        .End_If
    ..End_If
EraseLastChar:
    RealCaretRow | On D$SourceLen = 0, ret
    mov eax D$CurrentWritingPos | On eax = D$CodeSource, ret
    mov ebx BACK | call DoStoreCharDelete
    call StripBackSpace

    .If D$RightScroll = 0
        dec D$CaretRow
    .Else
        If D$CaretRow = 1
            sub D$RightScroll RIGHT_FEED
            add D$CaretRow (RIGHT_FEED-1)
        Else
            dec D$CaretRow
        End_If
    .End_If

    ...If D$CaretRow = 0
        mov esi D$CurrentWritingPos
        If D$CaretLine = 0
            call UpOneLine
        Else
            dec D$CaretLine
        End_If
        std | lodsb                                        ; strip CR
L0:         lodsb | cmp al LF | jne L0<
        cld
        lodsw                                              ; strip CR/LF
L0:     lodsb | inc D$CaretRow | cmp al tab | jne L1>
        or D$CaretRow 00_111 | inc D$CaretRow | jmp L0<
L1:     cmp al CR | ja L0<
        mov ebx BACK | call DoStoreCharDelete | call StripBackSpace | call TryToMove
    ...Else
;;
  When called from 'SimulateBlockForBackIndent' / 'IsItBlockIndent' / 'RetrieveBlockIndent'
  don't play: [Shift][Tab] is going on, not [Shift][BackSpace]. The 'OldBlockInside' Flag
  let us know of it:
;;
        ..If B$keys+&VK_SHIFT = &TRUE
            .If B$OldBlockInside = &FALSE
                mov eax D$CaretRow | dec eax | mov ebx D$TabIs | dec ebx | and eax ebx
                If eax <> 0
                    call AskForRedrawNow
                    mov eax D$CurrentWritingPos
                    On B$eax-1 = ' ', jmp BackSpace
                End_If
            .End_If
        ..End_If
    ...End_If
ret


KeyDown:
    mov eax D$LineNumber

    If D$CaretLine >= eax
        call DownOneLine
    Else
        mov esi D$CurrentWritingPos
L0:     lodsb | On esi > D$SourceEnd, ret
        cmp al LF | ja L0<
            inc D$CaretLine
    End_If
ret


KeyUp:
    If D$CaretLine = 0
      call UpOneLine
    Else
      dec D$CaretLine
    End_If

ret


StartOfLine:
    mov D$RightScroll 0, eax 1

    mov esi D$CurrentWritingPos | While B$esi-1 <> LF | dec esi | End_While

    .If B$esi+2 = ':'
        add esi 3 | add eax 3
        If B$esi = ':'
            inc esi | inc eax
        End_If
    .End_If
    While B$esi = ' ' | inc esi | inc eax | End_While

    If D$CaretRow = 1
        mov D$CaretRow eax
    Else_If D$CaretRow = eax
        mov D$CaretRow 1
    Else
        mov D$CaretRow eax
    End_If
ret


KeyLeft:
    RealCaretRow
SimpleKeyLeft:
    .If D$RightScroll > 0
        If D$CaretRow = 1
            sub D$RightScroll RIGHT_FEED
            add D$CaretRow (RIGHT_FEED-1)
        Else
            dec D$CaretRow
        End_If
    .Else_If D$CaretRow > 1
        dec D$CaretRow
    .End_If
ret
____________________________________________________________________________________________

; When user selects a Block with the KeyBoard, when he goes Down, and the start point of
; the selection is, say, at row 52, when he stops on a Line with 5 Chars, and hits the
; Left Arrow, he does not mean to wait that the Real Carret Col achieves moving back to
; the end of the 5 Chars Line, but, directely to unselect the 5th Char. For normal use,
; this complicated computation is done both by 'KeyLeft' and by 'PrintCaretAndUnderline',
; but, with Block Selection, there is no call to 'PrintCaretAndUnderline'? So:

SetPhysicalCaretRow:
    mov esi D$UpperLine, ecx 0

L0: cmp ecx D$CaretLine | je L2>                    ; search for Caret line (esi)
L1: lodsb | cmp al LF | jne L1<
    inc ecx | jmp L0<

L2: mov ecx 0
    On D$RightScroll > 0, sub ecx D$RightScroll

L0: cmp ecx D$CaretRow | je L5>>
L1: lodsb | cmp al CR | ja L2>                      ; search for Caret col (esi too)
        inc ecx
         mov D$PhysicalCaretRow ecx | jmp L6>>      ; Caret at end of line
L2: inc ecx | jmp L0<
L6: ret


StartOfWord:  ; KeyLeft
    mov esi D$CurrentWritingPos
    While B$esi-1 = ' ' | call SimpleKeyLeft | dec esi | End_While
    While B$esi-1 > ' ' | call SimpleKeyLeft | dec esi | End_While
    mov D$CurrentWritingPos esi
ret



EndOfLine:
    mov esi D$CurrentWritingPos | mov ecx D$ColNumber

L0: While B$esi <> CR
        If D$CaretRow < ecx
            inc D$CaretRow
        Else
            inc D$RightScroll
        End_If
        inc esi
    End_While

    AlignOn RIGHT_FEED D$RightScroll
ret


KeyRight:
    RealCaretRow

SimpleKeyRight:
    mov eax D$ColNumber

    If D$CaretRow < eax
        inc D$CaretRow
    Else
        add D$RightScroll RIGHT_FEED
        sub D$CaretRow 7
    End_If

    If B$CaretEndOfLine = &TRUE
        .If D$RightScroll > 0
            If D$CaretRow = 1
                sub D$RightScroll RIGHT_FEED
            End_If
        .Else
            dec D$CaretRow
        .End_If
    End_If
ret


EndOfWord:
    mov esi D$CurrentWritingPos
    While B$esi = ' ' | call SimpleKeyRight | inc esi | End_While
    While B$esi > ' ' | call SimpleKeyRight | inc esi | End_While
    mov D$CurrentWritingPos esi
ret


KeyInsert:
    xor B$CaretColor 00_01101001 | xor B$Overwrite &TRUE
ret


StripOneChar:
    inc D$CurrentWritingPos | mov ebx FORTH | call DoStoreCharDelete
    dec D$CurrentWritingPos
    mov esi D$CurrentWritingPos,  edi esi | inc esi | dec D$SourceLen, D$SourceEnd
    mov ecx D$SourceEnd | sub ecx D$CurrentWritingPos
    add ecx 0100 | rep movsb
ret


KeyDelete:
    .If B$BlockAutoDelete = &TRUE
        If B$BlockInside = &TRUE
            call ControlD | ret
        End_If
    .End_If

    RealCaretRow | On D$SourceLen = 0, ret
    mov eax D$CurrentWritingPos
    If eax < D$SourceEnd
        call StripOneChar | mov esi D$CurrentWritingPos
        On B$esi = LF, call StripOneChar
    End_If
ret

; (CaretPos to Source Pos).
; This is to translate screen line / col coordinates in one text pointer:
; input eax = Row; ebx = Line
; output eax = ptr

SearchTxtPtr:
    mov esi D$UpperLine, ecx 0, edx D$SourceEnd
    push eax
L0:     cmp ecx ebx | je L2>
L1:
            lodsb | cmp esi edx | ja L2>
            cmp al LF | jne L1<
                inc ecx | jmp L0<

L2: pop eax | mov ebx eax, ecx 0               ; switch eax (col Pos) > ebx
    add ebx D$RightScroll

L3: cmp esi edx | ja L9>
L3: lodsb | cmp esi edx | ja L9>
    cmp B$esi LF | je L8>
            inc ecx | cmp ecx ebx | jb L3<
                jmp L9>

L8: sub ebx ecx | sub D$CaretRow ebx | inc D$CaretRow

L9: dec esi | mov eax esi

    If D$CaretRow <s 1
        mov D$CaretRow 1 | mov D$RightScroll 0
    End_If
ret

____________________________________________________________________________________________

; In fact, sets the Caret and the Current Writing Position at the next Char after
; the Parameter.

Proc SetCaret:
    Argument @Location

        mov esi D$UpperLine, D$CaretRow 1, D$CaretLine 0

      ; Very rude and durty hak: There is a problem when the Search Box is runing and the
      ; user Click on a TITLE Tab... (temporary security >> To do analyze step by step what
      ; is going on when crossing these two...).
        mov eax D@Location
        If eax < D$CodeSource
            move D@Location D$CodeSource
        Else_If eax > D$SourceEnd
            move D@Location D$SourceEnd
        End_If

        While esi < D@Location
            If B$esi = LF
                mov D$CaretRow 1 | inc D$CaretLine
            Else
                inc D$CaretRow
            End_If
            inc esi
        End_While
        inc D$CaretRow | mov D$CurrentWritingPos esi
EndP


[FullRECT: ? ? ? ?  ConfinedRECT: ? ? ? ?  UserClickAfterEnd: ?  TrueCaretRow: ?]

LeftButtonSimulation:
    mov B$BlockInside &FALSE, B$UserClickAfterEnd &FALSE
    call MouseTextPos |  | jmp L2>>

LeftButton:
    mov B$BlockInside &FALSE, B$UserClickAfterEnd &FALSE, B$ClickOnMargin &FALSE

    If eax = D$EditWindowHandle
        call MouseTextPos | mov D$CaretRow eax, D$CaretLine ebx
    Else_If eax = D$BpWindowHandle
        mov B$ClickOnMargin &TRUE
        call MouseTextPos | mov D$CaretRow 1, eax 1, D$CaretLine ebx
    End_If

    .If B$ClickOnMargin = &TRUE
L1:    ;jmp DoubleClickMarginAction

        On D$BreakPointsTables = 0, call InitBreakPointsTables

        call IsBreakPointHere

        If B$InsideBpTable = &TRUE
            call DeleteBreakPoint | call DoStoreRemoveBP
        Else
            call SetBreakPoint | call DoStoreBP
        End_If

        mov B$UserClickAfterEnd &TRUE | jmp L9>>
    .End_If

L2: mov D$PhysicalCaretRow eax, D$CaretRow eax

    mov D$CaretLine ebx

L1: call SearchTxtPtr | mov D$ShiftDown eax  ; Ready for Block Selection

    If eax < D$SourceEnd
        mov D$CurrentWritingPos eax
    Else
        mov B$UserClickAfterEnd &TRUE
        call SetCaret D$SourceEnd
       ; mov eax D$CaretRow, ebx D$CaretLine | dec ebx | mov D$CaretLine ebx | jmp L1<
    End_If

    call 'USER32.GetClientRect' D$EditWindowHandle, ConfinedRECT
    call 'USER32.ClientToScreen' D$EditWindowHandle, ConfinedRECT
    lea eax D$ConfinedRECT+8
    call 'USER32.ClientToScreen' D$EditWindowHandle, eax
    mov eax D$FontWidth | sub D$ConfinedRECT eax
    call 'USER32.ClipCursor' ConfinedRECT
L9: ret


[StartBlockCol: ?  StartBlockLine: ?  EndBlockCol: ?  EndBlockLine: ?
 BlockInside: ?  BlockRedraw: ?    ShiftBlockInside: ?]

[MovingBlock: ?    BlockMoveDelay: ?    BlockMoveTicks: ?]

[PreviousMouseLine: ?    PreviousMouseCol: ?    FirstBlockDraw: ?]

SetBlock:
   ; Slow down the Block moving up or down (by mouse action). Speed increases while moving:
    .If B$MovingBlock = &TRUE
        If D$BlockMoveTicks = 0
            call 'KERNEL32.GetTickCount'
            add eax D$BlockMoveDelay | mov D$BlockMoveTicks eax
        Else
            Do
                call 'KERNEL32.GetTickCount'
            Loop_Until eax > D$BlockMoveTicks
            add eax D$BlockMoveDelay | mov D$BlockMoveTicks eax
            dec D$BlockMoveDelay | jnz L0>
                mov D$BlockMoveDelay 1
        End_If

    .Else
        mov D$BlockMoveTicks 0, D$BlockMoveDelay 60

    .End_If

  ; Set the new Block:
L0: If B$BlockInside = &TRUE
        move D$PhysicalCaretRow D$CaretRow
        mov B$CaretEndOfLine &FALSE
    Else
      ; D$ShiftDown is now set in 'LeftButton' (from WM_LBUTTONDOWN Case).
        mov eax D$CurrentWritingPos
        mov D$BlockStartTextPtr eax, D$BlockEndTextPtr eax
        call SimpleMouseTextPos
        mov D$CaretLine ebx, D$CaretRow eax
        mov D$PreviousMouseLine ebx, D$PreviousMouseCol eax
        mov D$ShiftBlockLine ebx, D$ShiftBlockCol eax
        mov B$FirstBlockDraw &TRUE, B$BlockRedraw &TRUE, B$BlockInside &TRUE | ret
    End_If

  ; Do not run for nop with an empty Block:
    call SimpleMouseTextPos | call SearchTxtPtr
    If eax = D$ShiftDown
        mov B$BlockInside &FALSE | ret
    End_If

    call SimpleMouseTextPos | mov D$PhysicalCaretRow eax, D$CaretRow eax

  ; Vertical Scrolling, if needed:
    push eax, ebx
        If ebx = 0
            push D$UpperLine
                call UpOneLine
            pop eax
            On D$UpperLine < eax, mov B$MovingBlock &TRUE
        Else_If ebx >= D$LineNumber
            push D$UpperLine
                call DownOneLine
            pop eax
            On D$UpperLine > eax, mov B$MovingBlock &TRUE
        Else
            mov B$MovingBlock &FALSE
        End_If
    pop ebx, eax

  ; Don't redraw for moves after End-of-Line (lazy reuse of SearchTxtPtr to ajust Row):
    push ebx | call SearchTxtPtr | pop ebx | mov eax D$CaretRow

  ; Compute and Redraw only if different:
    If B$FirstBlockDraw = &TRUE
        mov B$FirstBlockDraw &FALSE | jmp L1>
    Else_If B$MovingBlock = &TRUE
        jmp L1>
    Else_If eax <> D$PreviousMouseCol
        jmp L1>
    Else_If ebx <> D$PreviousMouseLine
L1:     mov D$PreviousMouseLine ebx, D$PreviousMouseCol eax
        mov D$CaretRow eax, D$CaretLine ebx

            call SetShiftBlock | mov B$ShiftBlockInside &FALSE

        mov B$BlockRedraw &TRUE
    Else
        mov B$BlockRedraw &FALSE
    End_If
ret
____________________________________________________________________________________________

; "On B$ebx = CR, dec D$BlockEndTextPtr" is because when a block-end-pointer points
; to an empty line, it is right upon the CR of next line. I spent much time to understand
; that, but it is normal. If it was a simple Caret, this would really be the good place.

LeftButtonUp:
    call 'USER32.IsMenu' D$FloatHandle
    On eax = &TRUE, call 'USER32.DestroyMenu' D$FloatHandle

    call 'USER32.ClipCursor' &NULL
    On B$BlockInside = &FALSE, ret
    If B$UserClickAfterEnd = &TRUE
        mov B$BlockInside &FALSE | ret
    End_If

    mov B$MovingBlock &FALSE
ret
; Should be no more use, with new Block Routines:
    mov eax D$BlockStartTextPtr
    cmp eax D$BlockEndTextPtr | jna L5>
        push eax, D$BlockEndTextPtr | pop D$BlockStartTextPtr D$BlockEndTextPtr
L5: mov ebx D$BlockEndTextPtr
    On B$ebx = CR, dec D$BlockEndTextPtr
    mov eax D$RightScroll | add D$BlockStartTextPtr eax | add D$BlockEndTextPtr eax
ret
____________________________________________________________________________________________

; Block action routines:

[hBlock: ?  BlockSize: ?  ClipBoardPTR: ?  ClipBoardLen: ?]

ControlC:
    cmp B$BlockInside &FALSE | je L9>>

    call 'USER32.OpenClipboard' D$hwnd | cmp eax 0 | je L9>>
    call 'USER32.EmptyClipboard' | cmp eax 0 | je L8>>
    mov ecx D$BlockEndTextPtr | sub ecx D$BlockStartTextPtr | inc ecx
    mov D$BlockSize ecx | mov ebx ecx | inc ebx
    call 'KERNEL32.GlobalAlloc' &GMEM_DDESHARE, ebx | cmp eax 0 | je L8>  ; > eax = handle
    mov D$hBlock eax
    call 'KERNEL32.GlobalLock' eax                                       ; > eax = adress
    mov edi eax, esi D$BlockStartTextPtr, ecx D$BlockSize
    rep movsb | mov al 0 | stosb
    call 'KERNEL32.GlobalUnlock' D$hBlock
    call 'USER32.SetClipboardData' &CF_TEXT, D$hBlock

L8: call 'USER32.CloseClipboard'
L9: ret


ControlY:
    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If

    mov esi D$CurrentWritingPos
    While esi > D$CodeSource
        dec esi | cmp B$esi CR | je L1>
    End_While

L1: mov D$BlockStartTextPtr esi
    While esi < D$SourceEnd
        inc esi | cmp B$esi CR | je L1>
    End_While

L1: dec esi | mov D$BlockEndTextPtr esi
    If esi > D$BlockStartTextPtr
        push D$CaretRow
            mov B$BlockInside &TRUE | call ControlD | call KeyDown
        pop D$CaretRow
    End_If
ret


ControlD:
    cmp B$BlockInside &FALSE | je L9>>
    jmp L0>

ControlX:
    cmp B$BlockInside &FALSE | je L9>>
    call ControlC

L0: If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If

    call WriteUndoBlockFileFromBlock | call StoreUserActionOfBlockDeletion

    mov eax D$CaretRow, ebx D$CaretLine | call SearchTxtPtr

    ...If eax <> D$BlockStartTextPtr

        std                                         ; reset cursor at "BlockStartText":
            mov esi D$BlockEndTextPtr
L0:         cmp esi D$BlockStartTextPtr | je L2>
                lodsb
                cmp al LF | jne L0<
                If esi < D$LastCharPosOnScreen         ; do not DEC Caret Line if block bigger
                    sub D$CaretLine 1 | jc L1>      ; than screen "upward drawn" by user
                End_If
                jmp L0<
L1:         mov D$CaretLine 0 | jmp L0<             ; same, but "downward drawn" by user

L2:         mov D$CaretRow 1 | dec esi              ; search for Col:
L3:         lodsb | cmp al LF | je L4>              ; count how many chars between start of block
                ;cmp esi D$CodeSource | jbe L4>      ; and left edge
                If esi <= D$CodeSource
                    add D$CaretRow 2 | jmp L4>
                End_If
                inc D$CaretRow | jmp L3<

L4:         .If esi < D$Upperline                   ; rePos screen if block greater than screen:
                mov eax D$UpperLine
                If eax > D$CodeSource
                    add esi 2
                    mov D$Upperline esi
                    mov D$CaretLine 0
                Else
                    inc D$CaretRow
                End_If
            .End_If

            move D$PhysicalCaretRow D$CaretRow
        cld

        move D$CurrentWritingPos D$BlockStartTextPtr

    ...End_If

UndoControlV:                                   ; strip text:

    mov edi D$BlockStartTextPtr, esi D$BlockEndTextPtr | inc esi

    mov eax esi | sub eax edi                   ; eax = Block-to-strip-out length
    push eax
        mov ecx D$SourceEnd | add ecx 100 | sub ecx esi | rep movsb
    pop eax
    sub D$SourceEnd eax | sub D$SourceLen eax | jnc L0>
        mov D$SourceLen 0

  ; Should be no use. just to be absolutely sure no wrong CR/LF after End Of Source:
L0: mov edi D$SourceEnd | mov eax 0A0D0A0D, ecx 100 | rep stosd
    call TryToMove                              ; ensure screen Pos in all cases.

    mov B$BlockInside &FALSE

;;
  This rebuild should be no use (???), and switches to the Next TITLE, in case of
  Block Deletions, probably, because the 'BlockEndTextPtr', of the deleted Chunk
  is at a virtual Pos in the next Tab (???) before the Redraw of the Caret (???).
  
  In case this Comment-Out, would produce some un-wished side-effect, restore it,
  and try to redefine the Caret Pos (line and Row), at the 'BlockStartTextPtr',
  immidiately after the Block Deletion, above, in 'ControlX'. It is already "done",
  but seems to be ineffective... :( :(( :(((
;;
L9: ;call RestoreRealSource | call SetPartialEditionFromPos
ret


OpenClipBoard:
    mov D$ClipBoardLen 0
    call 'USER32.IsClipboardFormatAvailable' &CF_TEXT | cmp eax 0 | je L9>>
    call 'USER32.OpenClipboard' D$hwnd | cmp eax 0 | je L9>>
    call 'USER32.GetClipboardData' &CF_TEXT  | cmp eax 0 | je L8>>    ; > eax = handle
    mov D$hBlock eax
    call 'KERNEL32.GlobalLock' eax                          ; > eax = pointer

    mov D$ClipBoardPtr eax
    mov edi eax, al 0, ecx 0-1 | repne scasb
    mov ecx edi | sub ecx D$ClipBoardPtr | dec ecx          ; len
    mov D$ClipBoardLen ecx
    call ClipBordCleaner
L9: ret
L8: call 'USER32.CloseClipboard' | ret


ClipBordCleaner:
    mov esi D$ClipBoardPtr
BlockCleaner:
L0: lodsb | If al = CR
                On B$esi <> LF, mov B$esi-1 ' '
                inc esi | dec ecx | jecxz L9>
            Else_If al = LF
                mov B$esi-1 ' '
            Else_If al < ' '
                mov B$esi-1 ' '
            Else_If al = 255      ;> 127
                mov B$esi-1 ' '
            End_If
    loop L0<
L9: ret


KillTabs:
    mov esi D$CodeSource, ecx D$SourceLen
KillTab:
L0: lodsb | cmp al tab | jne L1>
        mov B$esi-1 ' '
L1: loop L0<
ret


CloseClipBoard:
    call 'KERNEL32.GlobalUnlock' D$hBlock
    call 'USER32.CloseClipboard'
ret


[TooLongClipBoard: "
  ClipBord size too big. The limit is 1,000,000 octets.
  Do this in several operations. After each paste, Compile,
  close RosAsm and re-run.", 0

NotEnoughMemLeft: "
  Not enough Memory left for pasting this CliBoard.
  Compile, if you want to save your work, Close
  RosAsm and re-run", 0

CloseToMemoryEnd: "
  After pasting this ClibBoard, Compile, to save your work,
  Close RosAsm and re-run, to enable more Memory", 0]

ControlV:
    call OpenClipBoard | On D$ClipBoardLen = 0, jmp L7>>

    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, jmp L9>>
    End_If

    call ReMapSourceMemoryIfNeeded D$ClipBoardLen | On eax = &IDNO, jmp L7>>

    If B$BlockAutoDelete = &TRUE
        On B$BlockInside = &TRUE, call ControlD
    End_If

    call DoStoreBlockPaste

  ; Make room inside our text:
    mov esi D$SourceEnd | add esi 400
    mov edi esi | add edi D$ClipBoardLen
    mov ecx esi | sub ecx D$CurrentWritingPos | inc ecx
    std | rep movsb | cld | inc esi

  ; Write from clipboard:
    mov edi esi, esi D$ClipBoardPtr, ecx D$ClipBoardLen
    pushad | rep movsb | popad

  ; Search for the new Caret Position:
    mov esi edi, ebx D$CaretLine
L0: lodsb | inc D$CaretRow | cmp al CR | jne L1>
        inc ebx | mov D$CaretRow 1 | lodsb | dec ecx | jz L0>
L1: loop L0<

L0: cmp ebx D$LineNumber | jna L6>
        mov esi D$UpperLine | mov ecx ebx | sub ecx D$CaretLine
L1:     lodsb | cmp al LF | ja L1<
            mov D$UpperLine esi | dec ebx | jmp L0<

L6: mov D$CaretLine ebx

    mov eax D$ClipBoardLen
    add D$SourceLen eax | add D$SourceEnd eax | add D$CurrentWritingPos eax
L7: call 'KERNEL32.GlobalUnlock' D$hBlock
L8: call 'USER32.CloseClipboard'
L9: ret
____________________________________________________________________________________________

[ExtendMemoryString: "This operation requires more Memory than actually reserved:

Extend ?...", 0]

[UserPeStartEqualCodeSource: ?]

Proc ReMapSourceMemoryIfNeeded:
    Argument @Added
    [@TempoPointer: ?]

        shl D@Added 1
      ; (to care about Tabs-partial-editions !!!)

        mov eax D$CodeSource, ebx D$UserPeStart
        and eax 0_FFFF_FFF0 | and ebx 0_FFFF_FFF0

        If eax = ebx
            mov B$UserPeStartEqualCodeSource &TRUE
        Else
            mov B$UserPeStartEqualCodeSource &FALSE
        End_If

        add eax D$SourceLen | add eax D@Added | add eax 400

        ...If eax >= D$EndOfSourceMemory
            call 'USER32.MessageBoxA' D$hwnd, ExtendMemoryString, Argh, &MB_YESNO
            .If eax = &IDYES
                call RestoreRealSource

              ; New User PE Memory size:
                mov ecx D$EndOfSourceMemory | sub ecx D$UserPeStart
                add ecx D@Added | shl ecx 1 | add ecx 0100_000

                push ecx
                  ; Allocate to 'TempoPointer':
                    VirtualAlloc @TempoPointer, ecx

                  ; Copy it all:
                    mov esi D$UserPeStart, edi D@TempoPointer

                    If B$UserPeStartEqualCodeSource = &TRUE
                        mov D$edi CRLF2, D$edi+4 CRLF2 | add edi 4 | add D@TempoPointer 4
                    End_If

                    mov ecx D$EndOfSourceMemory | sub ecx D$UserPeStart
                    shr ecx 2 | rep movsd
                pop ecx
                add ecx D@TempoPointer | mov D$EndOfSourceMemory ecx

              ; New 'CodeSource', 'SourceEnd', 'CurrentWritingPos', 'UpperLine':
                mov eax D@TempoPointer | sub eax D$UserPeStart
                add D$CodeSource eax
                add D$CurrentWritingPos eax
                add D$UpperLine eax
                add D$SourceEnd eax

                push D$CurrentWritingPos, D$UpperLine, D$CaretLine, D$CaretRow

                  ; Release old Memory:
                    Exchange D$UserPeStart D@TempoPointer
                    VirtualFree D@TempoPointer

                  ; SourceLen may be = 0, when pasting after a [File]/[New]:
                    On D$Sourcelen > 0, call StartEdition

                    mov eax &IDOK

                pop D$CaretRow, D$CaretLine, D$UpperLine, D$CurrentWritingPos

                call SetPartialEditionFromPos

                mov eax &IDYES
            .End_If

          ; eax = &IDNO if not &IDYES

        ...Else
            mov eax &IDOK

        ...End_If
EndP
____________________________________________________________________________________________

; [F8] Separator Line

DrawOneLine:
    RealCaretRow

    mov esi D$SourceEnd | add esi 400 | mov edi esi      ; 400 is security 13/10/...
    mov ecx esi, ebx DRAWLINELEN | sub ecx D$CurrentWritingPos | inc ecx

    add edi ebx
        std
          rep movsb
          mov al '_', ecx ebx | rep stosb
        cld

    add D$SourceLen ebx | add D$CurrentWritingPos ebx | add D$SourceEnd ebx

    mov D$InsertedChar '_'

    call DoStoreInsert | call CarriageReturn
ret

____________________________________________________________________________________________

[WithControlA: ?]

ControlA:
    If B$WithControlA = &TRUE
        move D$BlockStartTextPtr D$CodeSource
        move D$BlockEndTextPtr D$SourceEnd
      ; SourceEnd is the Byte _after_ // BlockEndTextPtr is the last Byte:
        dec D$BlockEndTextPtr
        mov B$BlockInside &TRUE
    End_If
ret
____________________________________________________________________________________________

AskForRedraw:
    mov B$CaretOnlyRedraw &FALSE
    call 'USER32.RedrawWindow' D$BpWindowHandle, 0, 0, &RDW_INVALIDATE+&RDW_INTERNALPAINT
    call 'USER32.RedrawWindow' D$EditWindowHandle, 0, 0, &RDW_INVALIDATE+&RDW_INTERNALPAINT
ret


AskForRedrawNow:
    mov B$CaretOnlyRedraw &FALSE
    call 'USER32.RedrawWindow' D$BpWindowHandle, 0, 0,
                               &RDW_INVALIDATE+&RDW_INTERNALPAINT+&RDW_UPDATENOW
    call 'USER32.RedrawWindow' D$EditWindowHandle, 0, 0,
                               &RDW_INVALIDATE+&RDW_INTERNALPAINT+&RDW_UPDATENOW
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

;                               Source Edition Routines.
____________________________________________________________________________________________

; The Source Editor does not use any concept of Line / Row for defining the actual
; editing Position. For Status Bar, we need them, and for ScrollBar, the Line too.

[StatusLine: 0   StatusCol: 0   TotalNumberOfLines: 0]

[VScroll:
 VScroll.cbSize: D$ len
 VScroll.fMask: D$ &SIF_ALL__&SIF_DISABLENOSCROLL ; = &SIF_PAGE+&SIF_POS+&SIF_RANGE // &SIF_DISABLENOSCROLL
 VScroll.nMin: D$ 1
 VScroll.nMax: D$ 0
 VScroll.nPage: D$ 0
 VScroll.nPos: D$ 0
 VScroll.nTrackPos: D$ 0]

[TestCodeSource: ?]

TextPos:
    mov edi D$CodeSource, ecx 0-1, al CR, D$StatusLine 0, D$StatusCol 0
    Align 32

    mov ebx D$UpperLine
    If ebx < D$CodeSource
        move D$UpperLine D$CodeSource, D$CurrentWritingPos D$CodeSource
        mov D$CaretRow 1, D$CaretLine 0, D$PhysicalCaretRow 1

    Else_If ebx > D$SourceEnd
        move D$UpperLine D$CodeSource, D$CurrentWritingPos D$CodeSource
        mov D$CaretRow 1, D$CaretLine 0, D$PhysicalCaretRow 1

    End_If

L0: repne scasb | inc D$StatusLine | cmp edi D$UpperLine | jb L0<

    move D$TotalNumberOfLines D$StatusLine
    mov eax D$CaretLine | add D$StatusLine eax

    mov al CR
L0: repne scasb | inc D$TotalNumberOfLines | cmp edi D$SourceEnd | jb L0<
    mov eax D$TotalNumberOfLines | dec eax

    move D$VScroll.nMax eax
    move D$VScroll.nPage D$LineNumber
    mov eax D$statusLine | sub eax D$CaretLine | inc eax
    mov D$VScroll.nPos eax

RedrawScrollBar:
    On B$ScrollBarWanted = &TRUE,
        call 'USER32.SetScrollInfo' D$ScrollWindowHandle, &SB_VERT, VScroll, &TRUE
ret


RePosFromScroll:    ; called with eax = Line Number Wanted by user Bar Scrolling.
    mov edi D$CodeSource, ecx 0-1, al CR, D$StatusLine 0, D$StatusCol 0
   ; Align 32
    mov ecx D$SourceLen
L0: cmp edx 1 | je L1>
L0: repne scasb | dec edx | cmp edx 1 | ja L0<
   ; jz L9>
    inc edi
L1: mov D$UpperLine edi

L9: ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
 The Editor may let no use Spaces (between the end of text and CR/LF. This happend,
 for example when you enter blanks Lines on Indentations.

 The first time i applied it on RosAsm Source, it retrieved 15046 no use Spaces. This
 is to say less than 1 per cent of the Source. So, i do not implement it in the Menu,
 and as this is fast enough i implement it when opening a file.
 (OpenSourceOnly / OpenRosAsmPE).
;;

KillTrailingSpaces:
    call CorrectCRLFs D$CodeSource, D$SourceEnd

    mov esi D$CodeSource, edi esi, ecx 0, edx D$SourceEnd

    .While esi < edx
        lodsb

        .If al = '"'
L0:         stosb | lodsb | cmp esi edx | jae L9>>
            cmp al '"' | jne L0<    ; Allow blank Lines in Data Text.

        .Else_If al = "'"
L0:         stosb | lodsb | cmp esi edx | jae L9>>
            cmp al "'" | jne L0<

        .Else_If al = ';'
            If D$esi-2 = MLC   ; (LF ; ; CR)
                Do
                    stosb | lodsb | cmp esi edx | jae L9>
                Loop_Until D$esi-2 = MLC
            Else
L0:             stosb | lodsb | cmp al CR | jne L0<
                    jmp L1>
            End_If

        .Else_If al = CR
L1:         While B$edi-1 = ' '
                dec edi | dec D$SourceLen | dec D$SourceEnd
                inc ecx
            End_While

        .End_If

        stosb

    .End_While
    dec edi ; jE! no more src-increment
L9: inc edi
    If B$edi-1 = CR
        mov B$edi LF | inc D$SourceEnd | inc D$SourceLen | inc edi
    End_If

    shr ecx 2 | add ecx 10 | mov eax CRLF2 | rep stosd
ret
____________________________________________________________________________________________

Proc CorrectCRLFs:
    Arguments @Start, @End

        mov esi D@Start, edx D@End

        While esi < edx
            If B$esi = CR
                On B$esi+1 = CR, mov B$esi+1 LF
            Else_If B$esi = LF
                On B$esi-1 = LF, mov B$esi-1 CR
            End_If

            inc esi
        End_While
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

[LineToWheelScroll: D$ 1] ; Default to 1 Line per Message. 'SM_MOUSEWHEELPRESENT' may
; return FALSE even if a Wheel Mouse *is* present.

GetWheelInfo:
    call 'USER32.GetSystemMetrics' &SM_MOUSEWHEELPRESENT
    .If eax = &TRUE
        call 'USER32.SystemParametersInfoA' &SPI_GETWHEELSCROLLLINES, 0,
                                            LineToWheelScroll, 0
        If D$LineToWheelScroll = 0
            mov D$LineToWheelScroll 3
        Else_If D$LineToWheelScroll > 30
            mov D$LineToWheelScroll 3
        End_If
    .End_If
ret


WheelMsg:
    mov ecx D$LineToWheelScroll | shr eax 16 | jecxz L9>
    If ax >s 0
L0:    call UpOneLine | loop L0<
    Else
L0:    call DownOneLine | loop L0<
    End_if
    call AskForRedraw
L9: ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Positioning Edition at 'MainWindowProc' or at 'Main' or at Top:

StartEdition:
    call GetEditWindowClientRectangle

    ;call ScanSourceForBadChars

    mov edx CallBackName, ebx D$CallBackNameLen

    call InternSearch

    On B$BlockInside = &TRUE, jmp L9>
        mov edx EntryPointLabel, ebx 0, eax edx
        While B$eax <> 0 | inc ebx | inc eax | End_While

        call InternSearch

L9: mov B$OnReplaceAll &FALSE, B$BlocKInside &FALSE, B$DownSearch &TRUE, B$ReadyToRun &FALSE
    call StorePosInBackTable

    mov D$TitleTable 0, D$PreviousPartialSourceLen 0

    call ReplaceParagraphByDollar
ret


ScanSourceForBadChars:
    mov esi D$CodeSource, edx D$SourceEnd

    sub edx 100
    showme edx
ret

    While esi < edx
        lodsb

        If al < LF
           ; hexprint 1
        Else_If al > 127
            mov edi TrashString, ecx 100
            dec esi
            rep movsb
            showme TrashString
        End_If
    End_While
ret


GetEditWindowClientRectangle:
    call 'USER32.GetClientRect' D$EditWindowHandle RECT
      mov eax D$RECTright | sub eax D$RECTleft
      shr eax 3 | dec eax | mov D$ColNumber eax
        mov eax D$RECTbottom | sub eax D$RECTtop
          or eax 00111 | xor eax 00111
            shr eax 4 | dec eax | mov D$LineNumber eax
ret


ReplaceParagraphByDollar:
    mov eax D$CodeSource, ecx D$SourceEnd

    While eax < ecx
      ; 167 = Code of Paragraph Char (yet available as keyboard input, but no more
      ; available as screen output).
        If D$eax = MLC
            add eax 4
            While D$eax <> MLC
                On eax = ecx, jmp L9>>
                inc eax
            End_While
            add eax 2

        Else_If B$eax = ';'
            While B$eax > CR
                inc eax | On eax = ecx, jmp L9>>
            End_While

        Else_If B$eax = '"'
            inc eax
            While B$eax <> '"'
                inc eax | On eax = ecx, jmp L9>>
            End_While

        Else_If B$eax = "'"
            inc eax
            While B$eax <> "'"
                inc eax | On eax = ecx, jmp L9>>
            End_While

        Else_If B$eax = 167
            mov B$eax '$'

        End_If

        inc eax
    End_While

L9: ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[HelpHandle: 0    HelpMutexName: 'BaseMutexName', 0
 RosAsmHelpClassName: 'RosAsmHelpEditClass',0]

; We close B_U_Asm if run from RosAsm:

CloseHelp:
    call 'KERNEL32.CreateMutexA' &NULL &TRUE HelpMutexName         ; Someone?
    call 'KERNEL32.GetLastError'
    .If eax = &ERROR_ALREADY_EXISTS
        call 'User32.FindWindowA' RosAsmHelpClassName &NULL | mov D$HelpHandle eax

        call 'User32.GetWindowLongA' D$HelpHandle &GWL_EXSTYLE

        If eax <> &WS_EX_WINDOWEDGE                                 ; WS_EX value if auto-run
            call 'User32.SendMessageA' D$HelpHandle  &WM_CLOSE  0  0 ; If run from RosAsm
        End_If

    .End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; Simplified version of 'RightClick' Search (used by 'StartEdition' to point out 'Main' or
; MainWindowProc:

InternSearch:
    mov B$InsideMLC &FALSE, B$InsideComment &FALSE
    mov ah B$edx | or ah 32                           ; ah = first char
    inc edx                                           ; edx > second char (> edi)
    sub ebx 1

    ; Now, edi (edx) > start+1 of right clicked word; ebx = len-1. Search fitting:
L0: mov esi D$CodeSource, ecx D$SourceLen, B$InsideBracket &FALSE, B$InsideComment &FALSE

    jmp L0>

T0: lodsb | jmp L1>                          ; simplified loop for strip texts and comments
T1: loop T0<
      ret

L0: lodsb | cmp al ah | je L3>>              ; test XORed AH each pass in order to handle
            xor ah 32 | cmp al ah | je L3>>  ; case without modifying AL (following tests)
      jmp L1>
L2: loop L0<
        ret

L1: cmp B$InsideMLC &TRUE | jne L1>
      cmp D$esi-2 MLC | jne T1<          ; (LF ; ; CR)
        mov B$InsideMLC &FALSE | jmp L2<
L1: cmp B$InsideComment &TRUE | jne L1>
      cmp al LF  | jne T1<
        mov B$InsideComment &FALSE | jmp L2<
L1: cmp B$InsideText &FALSE | je L1>
      cmp al B$InsideText | jne T1<
        mov B$InsideText &FALSE | jmp L2<
L1: cmp al "'" | jne L1>
      mov B$InsideText al | jmp T1<
L1: cmp al '"' | jne L1>
      mov B$InsideText al | jmp T1<
L1: cmp al '[' | jne L1>
      mov B$InsideBracket &TRUE, B$DataDeclaration &FALSE    ;;;, B$OddWord 1
S0:   cmp B$esi ' ' | jne L2<
        inc esi | sub ecx 1 | jnc S0<        ; strip double spaces
          ret
L1: cmp al ']' | jne L1>
      mov B$InsideBracket &FALSE, B$DataDeclaration &FALSE | jmp L2<<
L1: cmp al ';' | jne L1>                     ; jmp over comments
        If D$esi-2 = MLC   ; (LF ; ; CR)
            mov B$InsideMLC &TRUE | jmp T1<<
        Else
            mov B$InsideComment &TRUE | jmp T1<<
        End_If
L1: cmp al '|' | jne L1>
      mov B$InsideBracket &FALSE | jmp L2<<
L1: cmp al ':' | jne L2<<
      mov B$DataDeclaration &TRUE

            jmp L2<<                     ; (avoids pointing equates datas).

L3: mov al B$esi-2 | call WordEdge | cmp B$Edge &FALSE | je L2<<     ; left edge?

        mov D$NumberDashLines 0

        pushad | mov ecx ebx, edi edx
C0:       lodsb | mov ah B$edi | inc edi | or ax 02020     ; case insensitive comparison

            While B$esi-1 = '_'
                lodsb | or al 020 | inc D$NumberDashLines
            End_While

            While B$edi-1 = '_'
                mov ah B$edi | or ah 020 | inc edi | dec ecx | jz C1>
            End_While

          cmp ah al | jne C1>
            loop C0<

            mov al B$esi | call WordEdge
            If B$Edge = &FALSE
                popad | jmp L2<<
            End_If
            popad | jmp C2>

C1:     popad | jne L2<<

   ; mov al B$esi+ebx | call WordEdge | cmp B$Edge &FALSE | je L2<<    ; right edge?

; as we have tested for '|' (> InsideBracket = FALSE), "test B$OddWord 1" applies either
; uppon first word of macro def. or odd word of equate def. But data body could still
; be pointed as odd equate dec. So we finally test 'B$DataDeclaration'.

C2: push ebx
        add ebx D$NumberDashLines
        cmp B$esi+ebx ':'
    pop ebx
    je L4>                                 ; Label?
      cmp B$InsideBracket &TRUE | jne L2<<                     ; equ. / macro
          cmp B$DataDeclaration &TRUE | je L2<<  ; avoid pointing data body instead of Equate

L4: dec esi                                                    ; found
    mov D$BlockStartTextPtr esi, D$RCstart esi                 ; RCstart/End used by
    add esi ebx | mov D$BlockEndTextPtr esi, D$RCend esi       ; 'BackClick'
    mov B$BlockInside &TRUE
    inc esi | mov D$CurrentWritingPos esi

    std | mov ecx 0
L5:     lodsb | inc ecx | cmp al LF | jne L5<                  ; search for start of line
    cld | dec ecx

    add esi 2 | mov D$UpperLine esi                            ; and set all needed
    call UpOneLine | call UpOneLine | call UpOneLine           ; variables for Pos, in
    mov D$CaretLine 3, D$CaretRow ecx, D$PhysicalCaretRow ecx  ; case user wish editing
    call TryToMove

L9: ret
____________________________________________________________________________________________

; [F11] / [F12] instant BookMark feature

[F11Upperline: ?    F11CurrentWritingPos: ?    F11CaretLine: ?    F11CaretRow: ?]

SavePosOnF11:
    call ClearF12
    call RestoreRealSource
    move D$F11Upperline D$Upperline, D$F11CurrentWritingPos D$CurrentWritingPos,
         D$F11CaretLine D$CaretLine, D$F11CaretRow D$CaretRow
    call SetPartialEditionFromPos
ret


[F12Upperline: ?    F12CurrentWritingPos: ?    F12CaretLine: ?    F12CaretRow: ?]

SetPosOnF12:
    ..If D$F11Upperline <> 0
        call RestoreRealSource

        mov eax D$Upperline

      ; If we are not already at the saved F11 Pos, we go to it:
        .If eax <> D$F11Upperline
          ; Save first the actual F12 Pos for On-Off effect:
            move D$F12Upperline D$Upperline, D$F12CurrentWritingPos D$CurrentWritingPos,
                 D$F12CaretLine D$CaretLine, D$F12CaretRow D$CaretRow
          ; Go:
            move D$Upperline D$F11Upperline, D$CurrentWritingPos D$F11CurrentWritingPos,
                 D$CaretLine D$F11CaretLine, D$CaretRow D$F11CaretRow

        .Else
          ; Pos is the one save by F11: we switch back to the F12 previous Pos:
            If D$F12Upperline <> 0
                move D$Upperline D$F12Upperline, D$CurrentWritingPos D$F12CurrentWritingPos,
                     D$CaretLine D$F12CaretLine, D$CaretRow D$F12CaretRow
            End_If

        .End_If

        call SetPartialEditionFromPos

        mov esi D$Upperline
        While B$esi-1 <> LF | dec esi | End_While
        mov D$Upperline esi

    ..End_If
ret


ClearF11F12:
    mov edi F11Upperline, eax 0, ecx 4 | rep stosd
ClearF12:
    mov edi F12Upperline, eax 0, ecx 4 | rep stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[EditorLOGFONT:
 @lfHeight: D$ 0_FFFF_FFF3
 @lfWidth: D$ 0
 @lfEscapement: D$ 0
 @lfOrientation: D$ 0
 @lfWeight: D$ 02BC
 @lfItalic: B$ 0
 @lfUnderline: B$ 0
 @lfStrikeOut: B$ 0
 @lfCharSet: B$ 0
 @lfOutPrecision: B$ 03
 @lfClipPrecision: B$ 02
 @lfQuality: B$ 01
 @lfPitchAndFamily: B$ 031
 @lfFaceName: 'Courier New', 0
 @Trailing: B$ 0 #&LF_FACESIZE
 EditorLOGFONT.len: Len]

[EditorCHOOSEFONT:
 @lStructSize: D$ len
 @hwndOwner: D$ 0
 @hDC: D$ &NULL
 @lpLogFont: D$ EditorLOGFONT
 @iPointSize: D$ 0
 @Flags: D$
 &CF_FIXEDPITCHONLY__&CF_SCREENFONTS__&CF_INITTOLOGFONTSTRUCT__&CF_APPLY__&CF_ENABLEHOOK__&CF_NOSIMULATIONS
 @rgbColors: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ ChooseFontHook
 @lpTemplateName: D$ 0
 @hInstance: D$ 0
 @lpszStyle: D$ 0
 @nFontType: W$ &SCREEN_FONTTYPE
 @Alignment: W$ 0
 @nSizeMin: D$ 0
 @nSizeMax: D$ 0]

[EditFontHdc: ?   FontHandle: ?]

[TEXTMETRICA:
 tmHeight: D$ 0
 tmAscent: D$ 0
 tmDescent: D$ 0
 tmInternalLeading: D$ 0
 tmExternalLeading: D$ 0
 tmAveCharWidth: D$ 0
 tmMaxCharWidth: D$ 0
 tmWeight: D$ 0
 tmOverhang: D$ 0
 tmDigitizedAspectX: D$ 0
 tmDigitizedAspectY: D$ 0
 tmFirstChar: B$ 0
 tmLastChar: B$ 0
 tmDefaultChar: B$ 0
 tmBreakChar: B$ 0
 tmItalic: B$ 0
 tmUnderlined: B$ 0
 tmStruckOut: B$ 0
 tmPitchAndFamily: B$ 0
 tmCharSet: B$ 0]

[FontHeight: 16   FontWidth: 8    LineSpacingAdjust: 0]

; React to the [Apply] Button (&CF_APPLY__&CF_ENABLEHOOK, in EditorCHOOSEFONT@Flags):

Proc ChooseFontHook:
    Arguments @Adressee, @Message, @wParam, @lParam

            .If D@Message = &WM_COMMAND
                If D@wParam = 0402
                    call LoadFont | call AskForRedrawNow
                    mov eax &TRUE | ExitP
                End_If
            .End_If

    mov eax &FALSE
EndP


SelectFont:
    move D$EditorCHOOSEFONT@hwndOwner D$ConfigDialogHandle
    call 'Comdlg32.ChooseFontA' EditorCHOOSEFONT

    If eax = &TRUE
        call LoadFont | call AskForRedrawNow | call MainResize
    End_If
ret


LoadFont:
    On D$Font1Handle <> 0, call 'GDI32.DeleteObject' D$Font1Handle
    call 'GDI32.CreateFontIndirectA' EditorLOGFONT | mov D$Font1Handle eax
    call 'USER32.GetDC' D$EditWindowHandle | mov D$EditFontHdc eax
    call 'GDI32.SetMapMode' eax &MM_TEXT
    call 'GDI32.SelectObject' D$EditFontHdc, D$Font1Handle
    call 'GDI32.GetTextMetricsA' D$EditFonthdc, TEXTMETRICA

    mov eax D$tmAveCharWidth | mov D$FontWidth eax
    mov eax D$tmHeight | mov D$FontHeight eax
ret


LoadNationalFont:
    call 'GDI32.CreateFontIndirectA' NATION_LOGFONT | mov D$NationalFontHandle eax
ret


