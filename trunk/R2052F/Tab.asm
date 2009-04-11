TITLE Tab
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
                  Main Editor's Upper Tab Control for TITLEd Chapters.

 When working with enTITLEd Sources, The Editor Copy/Paste the Actually edited Part
 at the very end of the Source Memory. Each time a new Part is activated, the previous
 one is copied back to its own room, and the new selected part is copied down in turn.

 At each event, a 'TitleTable' is build. This Table holds nothing but the Addresses
 of each 'TITLE' inside the Source.

 During Partial Edition, the Partial Source is substituted to the real Source. This
 means that all the KeyBoard features work like if the whole Source was nothing but
 the Partial Source. The modified behaviours are:

 * All KeyBoard actions (example: [Ctrl][PgDwn/PgUp] are limited to the Actual Part).
 * The ScrollBar, if any.
 * The Status Bar, if any.
 * The [Ctrl][S] feature is added a Dialog to choose between Whole/Part Saving.

 All of the Search Features (Right-Click / Tree View / SearchBox / ...) work the
 same as without TITLEs (upon the real whole Source).

 The goals of the feature are:

 * To make Part savings easier (for example, for reuse of wide Chuncks).
 * To accelerate the Source Editor, which, with uge Sources (1 Mega and more) may
   become slow on older Computers.
 * To make easy uge Source reorganisations.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[TITLE_TC_ITEM:
 TITLE_TC_ITEM_imask: D$ &TCIF_TEXT__&TCIF_IMAGE
 TITLE_TC_ITEM_lpReserved1: D$ 0
 TITLE_TC_ITEM_lpReserved2: D$ 0
 TITLE_TC_ITEM_pszText: D$ 0
 TITLE_TC_ITEM_cchTextMax: D$ 0
 TITLE_TC_ITEM_iImage: D$ 0-1
 TITLE_TC_ITEM_lParam: D$ 0]

SetPartialEditionFromPos:
    On D$PreviousPartialSourceLen <> 0, ret

SetPartialEdition:
; Called when opening a new File, or by 'TitleWindowProc' (on user Tab Selection).
    call BuildTitleTable

    If D$TiTleTable <> 0
        call GetActualPartFromPos | call SetActualPartFromPos
    End_If

    mov D$PreviousUpperLine 0-1 ; (To force remaping the Source Editor Colors)

    call AskForRedraw

    mov B$RealSourceRestored &FALSE
ret


SetPartialEditionFromPosNoRedraw:
    On D$PreviousPartialSourceLen <> 0, ret

; Called when opening a new File, or by 'TitleWindowProc' (on user Tab Selection).
    call BuildTitleTable

    If D$TiTleTable <> 0
        call GetActualPartFromPos | call SetActualPartFromPos
    End_If

    mov D$PreviousUpperLine 0-1

    mov B$RealSourceRestored &FALSE
ret


; 'Called by 'ControlZ'.

UndoTitleMove:
    push D$CaretRow D$CaretLine D$CurrentWritingPos D$UpperLine
    push ebx
        call RestoreRealSource | call BuildTitleTable
    pop ebx
    mov eax D$TitleTable+ebx*4, D$CurrentWritingPos eax
    call GetActualPartFromPos | call SetActualPartFromPos

    mov ebx D$UndoPtr | sub bx 32 | mov esi ebx
    If D$esi+(7*4) <> 0
        lodsd | mov D$CaretRow eax
        lodsd | mov D$CaretLine eax
        lodsd | add eax D$CodeSource | mov D$CurrentWritingPos eax
        lodsd | add eax D$CodeSource | mov D$UpperLine eax
        pop eax, eax, eax, eax
    Else
        pop D$UpperLine D$CurrentWritingPos D$CaretLine D$CaretRow
    End_If
ret


[AddedSize: ?]
[RealSourceRestored: ?]

RestoreRealSource:                      ; Called only by 'UndoTitleMove'
    On D$PreviousPartialSourceLen = 0, ret

   ; call DeleteUndoFiles
   ; call KillUndo
   ; call InitUndo

    mov eax D$SourceEnd
    If W$eax-2 <> CRLF
        add D$SourceEnd 2 | add D$SourceLen 2
    End_If

    mov eax D$SourceLen | sub eax D$PreviousPartialSourceLen
    mov D$AddedSize eax

  ; Move down (as many 'AddedSize') from Top of Actual Source Part to
  ; End of Actual Edition, to make room:
    .If eax = 0
        mov edi D$ActualTitle, esi D$CodeSource, ecx D$SourceLen
        rep movsb

    .Else_If eax g 0                ; Positive Number.

        mov esi D$SourceEnd         ; End of Partial Edition
        mov edi esi | add edi D$AddedSize
        mov ecx esi | sub ecx D$NextTitle | inc ecx
      ; ecx = length from Source-Next-Title to End-of-Partial-Edition
        std
            rep movsb
        cld
        mov esi D$CodeSource, edi D$ActualTitle
        add esi D$AddedSize         ; equal to the made up room
        mov ecx D$SourceLen | rep movsb

    .Else                           ; Negative Number.
        mov esi D$CodeSource, edi D$ActualTitle
        mov ecx D$SourceLen | jecxz L0>
            rep movsb
L0:     mov esi D$NextTitle
        mov ecx D$CodeSource | sub ecx D$NextTitle | rep movsb

    .End_If

    mov eax D$CurrentWritingPos
    If eax = D$SourceEnd
L0:     dec eax | cmp B$eax ' ' | jb L0<
    End_If

    sub eax D$CodeSource
    add eax D$ActualTitle | mov D$CurrentWritingPos eax

    mov eax D$ActualTitle | sub eax D$CodeSource    ; eax = adjustement to new Pos.

    add D$UpperLine eax

    If B$BlockInside = &TRUE
        add D$BlockStartTextPtr eax
        add D$BlockEndTextPtr eax
    End_If

    move D$CodeSource D$RealCodeSource
    move D$SourceEnd D$RealSourceEnd
    move D$SourceLen D$RealSourceLen
    mov eax D$AddedSize | add D$SourceEnd eax | add D$SourceLen eax

    mov D$PreviousPartialSourceLen 0

    mov B$RealSourceRestored &TRUE
ret


; Called when Mouse outside User area.

ShowTitles:
    call RestoreRealSource | call BuildTitleTable

    If D$TitleTable > 0
        call CreateTitleTab | call ShowTitleTab | call SetActualTitle

        On B$BlinkingCaretWanted = &TRUE, call KillBlinkCursor
    End_If

    call SetPartialEditionFromPos
ret


[NumberOfTitles: ?   PreviousNumberOfTitles: ?]

BuildTitleTable:
    mov edi D$CodeSource, ecx D$SourceLen, ebx TitleTable
    mov B$edi-1 LF

    move D$PreviousNumberOfTitles D$NumberOfTitles
    mov D$NumberOfTitles 0

L0: mov al 'T'

L1: repne scasb | jne L9>>

        cmp D$edi 'ITLE' | jne L1<
            cmp B$edi+4 ' ' | ja L1<
                cmp B$edi-2 LF | jne L1<

              ; If the first 'TITLE' is not Top-of-File, set one now:
                .If ebx = TitleTable
                    mov eax edi | dec eax
                    If eax > D$CodeSource
                        move D$TitleTable D$CodeSource | add ebx 4
                        inc D$NumberOfTitles
                    End_If
                .End_If

                If ebx > TitleTable
                    call TitleSize | jna L2>
                End_If

                mov eax edi | dec eax | mov D$ebx eax
                add ebx 4
                inc D$NumberOfTitles
                If ebx = TitleTableEnd
                    call 'USER32.MessageBoxA', 0, {'Too many TITLEs', 0},
                                                  {'BuildTitleTable', 0}, 0
                    sub ebx 4
                End_If

L2:             On ecx > 0, jmp L0<<

L9: mov D$ebx 0     ; 'TitleTable' End Mark.

  ; In case when Only one valid TITLE is found, we run no TITLE:
    If D$TitleTable > 0
        On D$TitleTable+4 = 0, mov D$TitleTable 0
    End_If

    mov eax D$NumberOfTitles
    If eax <> D$PreviousNumberOfTitles
        call KillUndo | call InitUndo
    End_If
ret

; If the TITLE too smaller for the screen, skip over:

TitleSize:
    push ecx
        mov esi D$ebx-4 ; previous Title Pos.
        mov ecx 0
        while esi < edi
            On B$esi = CR, inc ecx
            inc esi
        End_While
        cmp ecx D$LineNumber
    pop ecx
ret

____________________________________________________________________________________________
;;
 Partial Edition is done inside the 1_000_000 Octets reserved space that
 RosAsm always sets at the end of User Source. In case User would be
 dividing for the first time a very uge source, and, for example, write
 only one TITLE statement at the Top of Source, and then try to see what
 happends by moving the mouse outside the Client Area, the Partial Edition
 would be unable to copy Megas inside the reserved space. In such cases,
 we abort, and wait until user cut the Source in smaller parts, with
 more TITLES.
;;

[TooLongTitle: 'Part too long:', 0]

[TITLE_MAX 1_000_000]


; Sets the Actual Part Variables, depending on the Writing Position.

[ActualTiTle: ?    NextTitle: ?    PreviousPartialSourceLen: ?
 ActualPartIndex: ?    PreviousPartIndex: ?]

GetActualPartFromPos:
    mov esi TiTleTable, ebx D$CurrentWritingPos, eax D$TiTleTable, edx 0
    mov D$ActualTitle eax | move D$NextTitle D$esi+4

    mov D$ActualPartIndex 0-2
  ; 0-2 because Tab Index are zero based (> -1) and because we are
  ; INCreasing until *next* Title found (> -1)

    While eax <= ebx
        inc D$ActualPartIndex
        mov edx eax | lodsd | On eax = 0, jmp L1>
    End_While

L1: On edx = 0, mov edx D$TiTleTable, eax D$TiTleTable+4
    On eax = 0, mov eax D$SourceEnd

    mov D$ActualTitle edx, D$NextTitle eax
ret
____________________________________________________________________________________________

;;
 The Code Source is always followed by 200 CR/LF (security) + 1_000_000
 octets reserved for Editing. We set the Partial Edition after the 200
 CR/LF.
;;

[RealCodeSource: ?    RealSourceEnd: ?    RealSourceLen: ?]

SetActualPartFromPos:
    On D$TiTleTable = 0, ret

    move D$RealCodeSource D$CodeSource
    move D$RealSourceEnd D$SourceEnd
    move D$RealSourceLen D$SourceLen

    mov edi D$SourceEnd | add edi 400
    mov D$CodeSource edi
    mov esi D$ActualTitle, ecx D$NextTitle | sub ecx esi

    mov D$PreviousPartialSourceLen ecx, D$SourceLen ecx
    mov D$SourceEnd edi | add D$SourceEnd ecx | rep movsb
    mov eax 0A0D0A0D, ecx 100 | rep stosd

    mov eax D$CodeSource | sub eax D$ActualTiTle  ; eax = Displacement.

    add D$CurrentWritingPos eax | add D$UpperLine eax

    If B$BlockInside = &TRUE
        add D$BlockStarttextPtr eax | add D$BlockEndTextPtr eax
        move D$CurrentWritingPos D$BlockEndTextPtr
        call SetCaret D$CurrentWritingPos
    End_If

  ; If user add or suppress a TITLE statement in the Partial Edition;
  ; all these Pointers are wrong. We reset at Top of Part:
    mov eax D$CurrentWritingPos
    If eax >= D$SourceEnd
L1:     move D$CurrentWritingPos D$CodeSource
        move D$UpperLine D$CodeSource
        mov B$BlockInside &FALSE | jmp L9>>
    Else_If eax < D$CodeSource
        jmp L1<
    End_If

    mov eax D$UpperLine
    If eax >= D$SourceEnd
        jmp L1<
    Else_If eax < D$CodeSource
        jmp L1<
    End_If
ret
____________________________________________________________________________________________

KillTitleTab:
    call 'USER32.DestroyWindow' D$TitleWindowHandle | mov D$TitleWindowHandle 0

    On B$BlinkingCaretWanted = &TRUE, call InitBlinkCursor
ret


SetActualTitle:
    call 'User32.SendMessageA' D$TitleWindowHandle, &TCM_SETCURSEL, D$ActualPartIndex, 0
    call 'USER32.ShowWindow' D$TitleWindowHandle, &SW_SHOW
ret


[MAXTITLE 100]
[TitleIndex: ?] [ TiTleTable: ? #MAXTITLE] [TitleTableEnd: ?]
[DefaultTopTitle: 'Top', 0]

[TabRECT: TabRECT.left: D$ 0,  TabRECT.top: D$ 0
          TabRECT.right: D$ 0, TabRECT.bottom: D$ 0]

[ClientToScreenPoint: ? ?]

; One TiTleTable Record is: dWord = Top of Part Pointer
; (>>> "TITLE TitleName")

ShowTitleTab: ; CreateTitleTab
    mov esi TitleTable, D$TitleIndex 0

  ; Add a [Top] Tab Item if user did not.
    mov eax D$esi
    If D$eax <> 'TITL'
        mov D$TITLE_TC_ITEM_pszText DefaultTopTitle
        push esi
            call 'User32.SendMessageA' D$TitleWindowHandle, &TCM_INSERTITEM,
                                       D$TitleIndex, TITLE_TC_ITEM
        pop esi
        inc D$TitleIndex
        move D$TiTleTable D$CodeSource | add esi 4
        On esi = TitleTableEnd, sub esi 4
    End_If

L0: lodsd | cmp eax 0 | je L9>>

    While B$eax = ' ' | inc eax | End_While  ; jump over 'TITLE'.
    While B$eax > ' ' | inc eax | End_While
    mov ebx eax | add ebx 20
    mov D$TITLE_TC_ITEM_pszText eax
    While B$eax = ' '
        inc eax | On eax > ebx, jmp L1>
    End_While

  ; Search for end of 'TitleName', to write a zero, in user source.
    While B$eax > ' '
        inc eax | On eax > ebx, jmp L1>
    End_While
L1: push D$eax, eax, esi
        mov B$eax 0

        call 'USER32.SendMessageA' D$TitleWindowHandle, &TCM_INSERTITEM,
                                   D$TitleIndex, TITLE_TC_ITEM
        inc D$TitleIndex
    pop esi, eax, D$eax
    jmp L0<<

  ; This first 'MoveWindow' is to ensure that the Tab Control has the Width of the
  ; Window, before asking for how many Tab Rows:
L9: move D$TabWindowX D$EditWindowX, D$TabWindowY D$EditWindowY,
         D$TabWindowW D$EditWindowW, D$TabWindowH D$EditWindowH

         move D$TabWindowY D$EditWindowH

    call 'USER32.MoveWindow' D$TitleWindowHandle,
                             D$TabWindowX, D$TabWindowY, D$TabWindowW, D$TabWindowH,
                             &FALSE

  ; Get the hight of _one_ Tab, inside the Tabbed Control:
    call 'USER32.SendMessageA' D$TitleWindowHandle, &TCM_GETITEMRECT, 0, TabRECT
    mov eax D$TabRECT.bottom | sub eax D$TabRECT.top | inc eax

    push eax
        call 'USER32.SendMessageA' D$TitleWindowHandle, &TCM_GETROWCOUNT, 0, 0
    pop ecx

  ; Now: ecx = Hight of a Tab // eax Number of Rows
    mul ecx | mov D$TitleTabHight eax

    call 'USER32.GetSystemMetrics' &SM_CYDLGFRAME | shl eax 1 | add eax 4
    add D$TitleTabHight eax

    mov eax D$TabWindowH | sub eax D$TitleTabHight | mov D$TabWindowY eax

    move D$TabWindowH D$TitleTabHight

    move D$ClientToScreenPoint 0, D$ClientToScreenPoint+4 0

    call 'USER32.ClientToScreen' D$BpWindowHandle, ClientToScreenPoint

    mov eax D$ClientToScreenPoint | add D$TabWindowX eax
    mov eax D$ClientToScreenPoint+4 | add D$TabWindowY eax
    mov eax D$BpMarginWidth | sub D$TabWindowX eax | add D$TabWindowW eax

    call 'USER32.MoveWindow' D$TitleWindowHandle,
                             D$TabWindowX, D$TabWindowY, D$TabWindowW, D$TabWindowH,
                             &FALSE

  ; Set 'TabWindowY' to 'hwnd' coordinates, for firing 'KillTitleTab' on &WM_MOUSEMOVE:
    call 'USER32.ScreenToClient' D$hwnd, TabWindowX

  ; Adjust the "Dead-Point" if the ToolBar is [On]:
    If B$ToolBarWanted = &TRUE
        mov eax D$ToolBarPixelsHight | sub D$TabWindowY eax
    End_If
ret

[TabWindowX: ?  TabWindowY: ?  TabWindowW: ?  TabWindowH: ?]

[ErrorInTab: 'Error in Tab Building:', 0
 MoreThanFive: 'More than 5 Rows encounted', 0
 ZeroTabRow:   'Zero Tab Row...', 0
 BadTabWidth: 'Bad Tab Width', 0]


[TitleWindowHandle: 0    TitleTabHight: 60    TitleFontHandle: 0
 TitleTabWindowTitle: 'SysTabControl32' 0]

;[TITLEWINDOWSTYLE &TCS_MULTILINE__&TCS_BUTTONS__&TCS_HOTTRACK__&TCS_TOOLTIPS__&WS_DLGFRAME__&WS_VISIBLE]
[TITLEWINDOWSTYLE &TCS_MULTILINE__&WS_VISIBLE__&TCS_BUTTONS__&TCS_HOTTRACK]

CreateTitleTab:
    call 'USER32.CreateWindowExA' 0, TitleTabWindowTitle, &NULL,
                                  TITLEWINDOWSTYLE, 0, 0, 0, 0,
                                  D$hWnd, &NULL, D$hInstance, &NULL

    mov D$TitleWindowHandle eax

    call 'USER32.GetWindowLongA' D$TitleWindowHandle, &GWL_STYLE
    and eax (not &WS_CAPTION)
    call 'USER32.SetWindowLongA' D$TitleWindowHandle, &GWL_STYLE, eax

    On D$TitleFontHandle = 0, call CreateFontForTitles
    call 'User32.SendMessageA' D$TitleWindowHandle, &WM_SETFONT, D$TitleFontHandle, &FALSE

    call 'USER32.SetWindowLongA' D$TitleWindowHandle, &GWL_WNDPROC, TitleWindowProc
    mov D$OriginalTitleBarProcedure eax
ret


CreateFontForTitles:
  call 'GDI32.CreateFontA' 8 4 0 0 400 0 0 0  1,   ;  DEFAULT_CHARSET 1  OEM_CHARSET 255
                       0 0 0 0 Helv
  mov D$TitleFontHandle eax
ret


[OriginalTitleBarProcedure: ?]

Proc TitleWindowProc:
    Arguments @Adressee, @Message, @wParam, @lParam

        On B$SourceReady = &FALSE, jmp L8>>

L8:     call 'USER32.CallWindowProcA' D$OriginalTitleBarProcedure,
                                      D$TitleWindowHandle,
                                      D@Message, D@wParam, D@lParam

; Make sure that something would likely work under whatever OS version.
; But makes it as short as possible, because this is re-intrant with...
; here! Risks of Stack overflow, with the Tab Messages.
        ...If D@Message >= &WM_MOUSEFIRST
            ..If D@Message =< &WM_MOUSELAST ; (0209) // &WM_CAPTURECHANGED (0215)
                pushad
                    call 'User32.SendMessageA' D$TitleWindowHandle, &TCM_GETCURSEL, 0, 0
                    If eax <> D$ActualPartIndex
                        mov B$BlockInside &FALSE
                        push eax
                            call RestoreRealSource
                        pop eax
                        mov D$ActualPartIndex eax
                        move D$CurrentWritingPos D$TitleTable+eax*4
                        call SetPartialEdition
                        call AskForRedrawNow
                    End_If
                popad
            ..End_If
        ...End_If
EndP













