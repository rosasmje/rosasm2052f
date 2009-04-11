TITLE ToDo

;;
____________________________________________________________________________________________

What is this strange 'MemSaveFilter'?
____________________________________________________________________________________________

In some cases of RegistryData modifications, it may be corrupted.
____________________________________________________________________________________________

il manque la fonction clic droit sur les API terminées par W.

____________________________________________________________________________________________

NOPE addition on case of "empty" Macro Evocation: File with:

RosAsm/Ludwig/Noname.exe and debug_me.exe

Needs a re-organization of the Macros jobs, with intermediate copy of one Statement.
____________________________________________________________________________________________

Disassembler: Win32Dasm. At 040796E, there is a list of Pointers. Internaly, the
first one should be Flagged Code, and not Data.
____________________________________________________________________________________________

Disassembler: One Export may have severl Names Exported. Example:

        MyAdvapi32.dll: BuildExplicitAccessWithNameW / A

'WriteExportedFunctionLabel' needs another loop somewhere.
____________________________________________________________________________________________

For Guga:

Dllscanner Tool: It fails showing all Imports on WZCAB.DLL (in the [Disassembled] Folder).
____________________________________________________________________________________________

Review all EBX preservations, from 'Dis_rm8_r8'

____________________________________________________________________________________________

Review the Strings analyzes in the Disassembler. Some 9, 10, 13, are replaced by
Space, when isolated (should be the reverse).
____________________________________________________________________________________________

>     mov D$eax+0BC   ; ----------------> Here missing the register. It 
> should be mov D$eax+0BC es
>     mov D$eax+098 ds
>     mov D$eax+094 ss
>     mov D$eax+090 ss
>     mov D$eax+08C cs
>     mov D$eax+0C8 cs

!!!!!!!!!!!!!!!!!!!!!!!!
____________________________________________________________________________________________

Search for (OpD1) PFMUL 3D Now.
____________________________________________________________________________________________

Extend the "Right-Click on Numbers" Functionalities (FPU? Signed Values?...)
____________________________________________________________________________________________


nop  ; <<<<<<<<< Error manager pointing here, because of the Duplication of
     ; 'SIZEOF_materials'

DeclareTable materials 1 1 SAMPLE_Material.size

[sizeof_materials 34]
[DeclareTable| {SIZEOF_#1 #2  SIZEOF_#1_CHUNK #3   SIZEOF_#1_ELEMENT #4}]

Main: call 'Kernel32.ExitProcess'

____________________________________________________________________________________________

I didn't realise that Q$ is only used for integers.
Maybe an entry into B_U_Asm along the lines of Scarmatil's explanation would be
appropriate?
____________________________________________________________________________________________

From 'MAXDIALOG', and friends... Make it all Dynamic as soon as possible.
____________________________________________________________________________________________

When setting a bp, the caret moves to that line.
Should not happen.
____________________________________________________________________________________________

Clip File:

Review the Doc.
____________________________________________________________________________________________

Disassembler: Looki Report, in ...\Eudora\Attach\Looki.

The last Point is a real miss-interpretation.
____________________________________________________________________________________________

'DeleteIcon':

Looki says it is possible to have left over data after removal. to be Reviewed.

____________________________________________________________________________________________


fnstv D$eax sbb D$eax <<< RightClick SBB eax 'OpCodeList' / 'SearchMneMonic'

____________________________________________________________________________________________

Linux-LINE: Chuck reports:

> As it turns out, there is a problem with the debugger. Wine handles the
> KERNEL32.VirtualQueryEx call with its NtQueryVirtualMemory routine, and
> returns "Unsupported on other process", causing the debugger to display
> "VirtualQueryEx reported error".
____________________________________________________________________________________________

[list - tree/import/export] Not assuming anything but CALL '...'.
____________________________________________________________________________________________

Error-Box with Copy&Paste enable.
____________________________________________________________________________________________

Error Message window:
It should be possible to copy text from the edits.
There should be no cursor.
The window should be fixed. Not sizeable. 
____________________________________________________________________________________________

2. There is a cursor in the error message window, and it is possible to read and write inside the EDITs.
What for? It's funny...
____________________________________________________________________________________________

Code Completion:
When RosAsm can't find the equate, a messagebox pops and asks if you want to build a list, and takes the focus from the Editor.
This is a VERY annoying way to tell you that you mistyped an equate.
It has do be done maybe like that:
* If matched completion found, underline (and bold?).
* when no longer matches, remove the line (or if choosed to bold, unbold?).
* If the last chars were deleted and it matches again, put the underline back.
For example:
&CW_USED
&CW_USEDEDAU
&CW_USEDE
and the option to build the list will appear... (sorry, no idea. maybe as text telling you that it's optional in the main menu).
It can't remain like that.
What do you think?
____________________________________________________________________________________________

B_U_Asm Selection should not reload the actual page.
____________________________________________________________________________________________

We have found out one user not understanding the Cnfiguration Dialog Tab:

>>> ToDo: Make the [Companion Files] Tab the first open one.
____________________________________________________________________________________________

Is the Header KILLFALGed?

____________________________________________________________________________________________

 Disassembler: With the Tests DLL, the Exported Names are wrongly two Byte backwarded.

____________________________________________________________________________________________

A user reports having seen a hang at:

>Proc DataView_FillDataLabelCombo:
>    Arguments @ComboHandle @SortByName
>
>    SendMessage D@ComboHandle, &CB_RESETCONTENT, 0, 0
>    move D$DataLabelComboHandle D@ComboHandle
>
>    mov esi D$PlainLabelList
>    lodsd ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

... which is quite "strange"...

____________________________________________________________________________________________

>- dans RosAsm (je sais pas si c'est toujours le cas) me semble
>que quand on sélectionne replace source, il ne travaille plus
>sur le fichier initialement chargé mais sur l'exe correspondant
>au .asm chargé. Moi j'aurai plutôt vu qu'il remplaçait le source
>et c'est tout. (pour open source only, là je suis d'accord qu'il
>le fasse par contre)
____________________________________________________________________________________________

'OpA3': "mov D$FS:0, eax", with 0 in Word form >>> Other cases.

____________________________________________________________________________________________


Review the 'IsItCode' tuning, from the 'IsItNoReturnCall' comments.

____________________________________________________________________________________________

Implement a PUSHW for pushing imm 16 with negative cases assumed.
____________________________________________________________________________________________

Review 'CheckBracketExpression'
____________________________________________________________________________________________

In 'AnalyzeOfCmParameter' study the possibility of branching Imm reals, from the
'memMarker' Case
____________________________________________________________________________________________

Try to improve the Blocks saving, and to separate into some isolated Folders.
Mabe depending on a Time&Date Stamp.
____________________________________________________________________________________________

once RosAsm has tried to install, and failed to,
even if the the "RosAsmFiles" Folder is copied aside, with all required
Files, it does not try to Auto-Install, the way it does, at the very
first try.
____________________________________________________________________________________________

Review the 'CompileErrorHappend' Flag (not always effective, and does not work for
the last Dialog of the Unfolder (wishable to not show an empty bow after error),
at the end of 'UnfoldMacro'
____________________________________________________________________________________________

With some Selected Blocks (ex: Double-Click), the ScrollBar does not work.

____________________________________________________________________________________________

Name001
Name002
Name003
Name004

utiliser :

Remplace Name par EQUATE_Name (par exemple)...

____________________________________________________________________________________________

> In most schemes, you can define a macro called "at-compile-time" which
> will do calculations at compile time.  So, we could do:
> 
> (define sqrt-table (at-compile-time (list->vector (map (lambda (x)
> (sqrt x)) (range 0 500)))))

____________________________________________________________________________________________

In "Proc @ReplaceEquate:" don't forget to tell Ludwig to do:

call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

____________________________________________________________________________________________

Il me semble que dans un source utilisant les TITLES, si une erreur de type :
"Symbol ne correspondant à aucun label" pointe vers une page différente de celle
où tu te trouves quand tu refermes la fenêtre de message d'erreur tu ne te retrouves
pas sur l'erreur comme d'habitude...

; -----------------------------------------------------------

In 'WriteFloatingPointOperationCode'

...Test_If_Not_And bx FloatingPointOperand, dx FloatingPointOperand
;...

..Test_Else_If_And bx ConstantOperand, dx ConstantOperand
       ; mov eax merde
            EPP_Code '{??:R$ #1}|FLD R$?|{??:R$ #2}|FLD R$?|' | 
        mov eax merde

The error Manager seesm to be perverted by this "EPP_Code" Macro

As the same Macro is Evocated upward, can is be something inside
the String, when parsing Conditional Macros (???!!!...), or can
it be something wrong in the ParaMacros Parser (???!!!...).

What relationship with the Statements Counter?

____________________________________________________________________________________________

Tools -> Configuration -> Help Files

Serait-il possible de d finir le r pertoire courant comme r pertoire par d faut et d'attribuer au diff rents chemins d'Help :
RosAsm\RosAsmFiles\xxxxx.xxx si ils sont pr sents.
____________________________________________________________________________________________

Wish list: 

- Remember the width of the debugger window. 

- Make the Edit functions works while using multiple instances of RosAsm, or at least an option to not show the "You are running multiple instances of Rosasm"... box. 

- For new files ask for the name of the exe the first time it compiles and not before. 
- and for the "New File" box allow user-defined templates for new files. Or a menu item called "New from template". 


Also, and most important: 

- Error in B_U_Asm > Mnemonics Ref: In Strings_Op and List_All_Op, MOVSD incorrectly links to the SSE mnemonic. And in SSE_Op, MOVSD is missed. 

- Limitation: Deleting all String resources in a PE is not allowed. 

____________________________________________________________________________________________

A short lesson in Correct code benchmarking:

CLI        ;not yet  
CPUID      ;or any other serialising
RDTSC
...        ;store edx:eax
test:
...        ;code to test (no loops here, execpt if part of test)
RDTSC      
STI        ;if 
...        ;sub edx:eax,[stored]
           ;sub 11 (the time for one RDTSC)
result in edx:eax, and that's the only true figure.


____________________________________________________________________________________________


'OQregRegImm8'

to do List:

Hello Betov, 

Your quite right in doubting the mnemonic for 

1) pextrw 

mov al B$SecondReg | shl al 3 | or al 0011_000_000 | or al B$FirstReg 
should be 
mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg 

2) pinsrw - though there's an error in the doc's we've got this one right. 

3) movm$kps - this one doesn't seem to work the way they've described, 
possibly been corrupted by microsoft and friends... :) :) :) 


Werewolf
____________________________________________________________________________________________

In 'WriteMacroVariable', i comment out the 5th line, without recalling why i wrote
this, previously (it was for the Conditional macros, anyway, but it seems to work
the same without... Wait and see...)

Also to be reviewed, in 'ReplaceFromMacroData', after the 'call StoreMacroVariableByNumber',
the 'NOPE' output should probably not be there, but after the 'call WriteMacroVariableByNumber'
Seems out of logic...
____________________________________________________________________________________________

For Debug Tool-Tips: keep the Equates List alive the same way as the CheckSum Table,
so that the Expression could be parsed?

How to re-Encode? What of the Label?

____________________________________________________________________________________________

Add an error message for Macros stripping the last String Delimiter.

____________________________________________________________________________________________

Encode / Decode Box does not parse the Win32 Equates.

____________________________________________________________________________________________


If you really want me to report the small errors, I will of course start 
doing this. But, as I can fully happily edit all my templates, and my 
apps, to my heart contents with RosAsm, at no problems in 99.9% of cases, 
I do not figure these small details to be important.

As it is to me, a much greater irritant, to use the working windows OS 
menues, than to use a RosAsm that has some rare crash states. Dont know if 
you found the SHIFT+DELETE problem yet. Sometimes, it seems to accumulate 
an error that makes the REPLACE function misbehave.

If I reset RosAsm, by a restart, the REPLACE function works correctly, but 
after some time using it, the replacement function can replace more than 
the exact number of chars. Sometimes it means that a SPACE is added so 
that when I use labels like Button.Close , and I want to replace it with
SkinButton.Close, I end up with SkinButton .Close (which does not compile).

I try to make a list from now on, and post it each week.

But theese small problemes with RosAsm has workarounds, that I use 
instead. For instance, I rarly need the replace functions, so when I do, I 
save, and exit, and restart, because I know it works perfectly then.

____________________________________________________________________________________________

I remember trying to find this in RosAsm's source (long ago) but I couldn't fix it .
Perhaps you could try calling LoadLibrary explicitly with the full path
c:\path\to\program\LibraryToLoad.dll?
It is obvious that there's something funky with setting the current directory.
Btw, I'm also using XP.

____________________________________________________________________________________________


SSE3 Instructions:

FISTTP  DF /1 FISTTP m16int
FISTTP  DB /1 FISTTP m32int
FISTTP  DD /1 FISTTP m64int
LDDQU
MOVSHDUP, MOVSLDUP, MOVDDUP
ADDSUBPS, ADDSUBPD
HADDPS, HSUBPS
HADDPD, HSUBPD
MONITOR
MWAIT

____________________________________________________________________________________________


>@ PI2FW  AMD 3Dnow Packed Integer Word to Floating-Point Conversion-  0Fh 0Fh / 0Ch
>                   - Found in Disassembler ONLY


____________________________________________________________________________________________

What i can do, is, in case of overflow error, go on checking up to the end, so that, 
in case of trailing specifier, it could forward it to the appropriated Routine.

Done for 'TranslateDecimal'.
____________________________________________________________________________________________

Review the BitMap Types. Example, Cursor sizes.
____________________________________________________________________________________________

After the new release of ludwig Debugger (coming after V.2.007d), recall of
the problem of TD_170Graph Demo, that aborts, when trying to Load/Save a File.
____________________________________________________________________________________________

si tu tapes "b tement" &NUL au lieu de &NULL dans un source en contenant plusieurs, 
l'erreur point e est incorrecte :

&NUL

Unknown Win equate name

Certes, mais le saut dans le code se fait, apparemment, sur la premi re  vocation et non sur l'erreur.
____________________________________________________________________________________________

le clic droit sur les appels Api fait appara tre une fen tre "Api call infos" qui
n'est plus redimensionnable. Le probl me est que dans plusieurs cas la largeur 
n'est pas suffisante pour afficher tout le contenu sans retours   la lignes qui 
rendent confus et mal ais  la lecture...

____________________________________________________________________________________________

If the selection is not 3 line after the TITLE, in Search Functions, it is not
shown, because of the pos computation.
____________________________________________________________________________________________

>Apr s un test de d sassemblage d'une petite application utilisant
des TrackBars (r alis e avec RosAsm) tout le code est correctement
>restitu  mais il manque le :
>
>Call 'COMCTL32.InitCommonControls'

____________________________________________________________________________________________


>ici, avec PREPARSE Equal
>
>eax  = 0-32768 produit une erreur
>eax = -32768 produit une erreur
>eax = (-32768) ne produit pas d'erreur
____________________________________________________________________________________________

Hugin/ Nessie/ Nessie.asm: Problem of error not pointed out on Bad Dec. because
of the Dash-Lines considerations, to be implemented, first, into the [Search] Box.
____________________________________________________________________________________________

'LenghtOfBracketStatements' is bad since the modification of the Local Labels
expansions. Used only in 'SearchForApis'. Maybe not worthy the complication...
____________________________________________________________________________________________

For me personally, it would be good if we could configure code completion to match
after a certain, userselectable char. Or maybe match in the whole string ? I write 
"Application.WMSize" or "Application.WMMove", or "SkinSection.GetVisible" or 
"SkinSection.SetText". So code compeltion is a bit useless to me. If I could write 
"WMS" and RosAsm suggested : Application.WMSize, then code completion would be 
_very_ useful. 

And also if matching more than one identifier, the list could be cycled by just 
pressing CTRL+SPACE a second time, or third time. 
____________________________________________________________________________________________

Disassembler: 'NamedIdSubstitution' is wrong with MASM ShowDib2 Demo
____________________________________________________________________________________________

There is a issue in rosasm with local label calls:

Proc DoThis:
 ;code
 call @locall; DoThis@locall
 ;code

Endp
@locall:
 ;code
 ;code
ret

Proc DoThat:
 ;code
 call @locall; DoThat@locall
 ;code

Endp
@locall:
 ;code
 ;code
ret

The code works as espected.
The issue is with right click and tree navigation tools: They go to the  
first label always. Tree shows calls to locals sometimes as childs (as  
must be) but sometimes as orphans.
____________________________________________________________________________________________

[IMAGE_SECTION_HEADER: ]
[Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME]
[MiscPhysicalAddress: MiscVirtualSize: D$ 0
 VirtualAddress: D$ 0
 SizeOfRawData: D$ 0
 PointerToRawData: D$ 0
 PointerToRelocations: D$ 0
 PointerToLinenumbers: D$ 0
 NumberOfRelocations: W$ 0
 NumberOfLinenumbers: W$ 0
 Characteristics: D$ 0]

[Name1Dis 0
 VirtualAddressDis 1
 SizeOfRawDataDis 5
 PointerToRawDataDis 9
 PointerToRelocationsDis 13
 PointerToLinenumbersDis 17
 NumberOfRelocationsDis 21
 NumberOfLinenumbersDis 23
 CharacteristicsDis 25]
 
____________________________________________________________________________________________

Is the 066 Prefix whishable or not for the encoding of ARPL ???
 
____________________________________________________________________________________________

add a [Save all TITLEs as Asm Files] in the [Ctrl] [S] feature Dialog
____________________________________________________________________________________________

Resize the Choose Menu Dialog at 90% of the Screen Width.
____________________________________________________________________________________________

Reuse the Trash1/2 Buffers everywhere possible.
____________________________________________________________________________________________

Add a Warning Edit Control in the Statistics.

When Building .Import, with calls to Comctl32.dll, verify that InitCommonControls
is called. If not, output a warning Message.

See: 'StoreDllName' >>> After the .Import is built, search for COMCTL32.
Found >>> Search for in InitCommonControls in 'ApiListB'.
____________________________________________________________________________________________

The "Peter Ctrl-Z" Bug has been fixed by implementing a security in the 'TextPos'
Routine >>> Rewrite all of the Ctrl-Z Functionalities from scratch when possible.
____________________________________________________________________________________________

Titles after failure of a Disassembling attempt >> Todo List
____________________________________________________________________________________________

Extend the sensitive area of Blank-Right-Click in the four directions.
____________________________________________________________________________________________

Redifine the 'FloatToUString' with Ludwig. ecx is not preserved.
____________________________________________________________________________________________

Is there a limit to the Data Alignment ([<??? ...) ? Is there a validity check for
the Number ?
____________________________________________________________________________________________

In the search/Replace Dialog the Tab-Key will not work properly,
the focus will set only on the selected radio button and can only moved with the arrow keys
____________________________________________________________________________________________

It would be very good to have more files in the MRU list, 
perhaps user- defined in the config setting?
____________________________________________________________________________________________

Implement a Table Switch for Strings Recognitions Table (for foreign languages).
____________________________________________________________________________________________

add a Routine to verify that (in Data) a Pointer does not break a pointer.
Examples in the Disasembly of Guga H2INC.
____________________________________________________________________________________________

Re-Assembling C:\ProgramFiles\... '7zFM' and '7zFM' hangs in 'ResetForNewBrackets'. 
A missing Bracket undetected problem.

To fix: Borbid any use of '?' in Code.
____________________________________________________________________________________________

lea al B$esi ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
____________________________________________________________________________________________

Old: Dialog Editor: The Child Style is broken when re-organising a set of Dialogs 
 (insertion of a new Dialog in a List of Dialogs with ID modified >>> WS_POPUP.
 
 Something is wrong in Dialog Editor. Mouse Pointing to the various Controls inside
 [Other Files] Configuration Tab... This seems to be a problem with Group Boxes.
 
 To be verified first: Where must the Group box be in the Z order, in order to enable
 groups of Radio Buttons to work by themselves. JohnFound said, by placing the Group
 Box *after* the Radio Buttons... (???...).
 
 
 --------------
 
 Answer from Wilhelm Zadrapa:
 
 The Group Box must have the WS_GROUP style.

 The controls after that box, up the the next WS_GROUP style are part of the group.
 
 --------------

Dialog Editor:

* Sort the Dialogs each time we leave the Resources Editor.

* When Loading the Debugger's Dialogs Files, problems:

- Child Style not preserved (!!!???...)

- The is a bug outputing weird char(s) in the Class Record.
____________________________________________________________________________________________

Simplify 'IsItaLabel' and 'StoreBookMark': Store the Label to be BookMarked,
in all cases, to a renamed 'LocalBookMark'.

____________________________________________________________________________________________

Yes, Bookmarking Local Data Labels is not implemented. I never though of it, but this is,
evidently, a must have.

"Number forms"... EditControl, for easy copy paste... Yes.


____________________________________________________________________________________________
____________________________________________________________________________________________



; (8 ) 

How come this is not ok? 

Code:



[Label: ? #50 
 EndOfLabel: ?] 
mov ecx EndOfLabel-Label ; Unable to resolve this parameter 
mov ecx (EndOfLabel-Label) ; Immediate only in expressions

____________________________________________________________________________________________

Add a content Checking in the Function for loading a .dlg (not a RosAsm File > Abort).

____________________________________________________________________________________________

When creating a drop-down combo box in the dialog editor, it will not allow you to select the &CBS_DROPDOWNLIST flag - clicking this will only select the &CBS_DROPDOWN flag. 
____________________________________________________________________________________________

Rewrite 'RightClick', 'InternalRightClick', 'InternSearch'. More modular, more actual
Style.
____________________________________________________________________________________________


 To be verified:
 
 When parsing the Sources, there may be a problem with Comments vs MLC, that is:
 
 When skipping Comment it must stop at CR, not at LF, as this might eat a following
 MLC.
____________________________________________________________________________________________

i try to compile your 06midi sample and
it give me this message.
Orphan colon encoun....
at Proc Midi Stream.
Is it bug or something is change the 
assembler?
____________________________________________________________________________________________


____________________________________________________________________________________________


____________________________________________________________________________________________

 
____________________________________________________________________________________________

review the upper char in Clip operations.
 
____________________________________________________________________________________________

 
____________________________________________________________________________________________

What is this:

; 1 = My MF_POPUP substitution.

Example in 'ResetThisMenuIDs', 'TurnThisMenuToExType'. Why did i substitute 1 to 010 ?
____________________________________________________________________________________________

 
 
 Do not send an error message for Api call by number with same Number in two different
 DLL. Force DLL Name for such calls.
 Disassembler, set the Jumps Table Labels' Names in the form of: "DllName0xxx:"
 
 PMULLW > (done)

____________________________________________________________________________________________

'ResetForNewBrackets' diserves a complete rewrite
";!!!!!!!!!" was the thing that made 'Base3 Uses' (without any parameter) hang. I turn it:
"jae L9>>", but i don't understand what the comment means (!!!...). The hell!
 
____________________________________________________________________________________________


____________________________________________________________________________________________


____________________________________________________________________________________________


____________________________________________________________________________________________

____________________________________________________________________________________________

default icon might be removed from the PE
____________________________________________________________________________________________


____________________________________________________________________________________________

 
____________________________________________________________________________________________

 Occasional Problems with Registry Modifications. Is it possible to directely 'refresh'?
 (Delete // re-Create)?

____________________________________________________________________________________________



____________________________________________________________________________________________

In Iczelion 31ListView.exe:

WIN32_FIND_DATA

[WFD_cAlternate: B$ 0 #14] hangs (in Find File Functions), under 2000, with long Names.
____________________________________________________________________________________________

 
error in the Dx Demo:

I think i found a very serious Errors in (RosAsm414b version). i using this macro

[DxCall | mov eax D$#1 | mov eax D$eax | call D$eax+#3 D$#1 #4>L]

when i write this code ( DxCall lpdd '->' Release) , by unfolding or showing in Debuger
it looks so: 

mov eax D$lpdd 
mov eax D$eax 

where is the rest!!! > ; that's because there is no fourth Parameter !! 

instead i use this one:

[DxCall | mov eax D$#1 | mov eax D$eax | push #L>3 | push D$#1 | call D$eax+#2]

____________________________________________________________________________________________

____________________________________________________________________________________________

    
____________________________________________________________________________________________

 
 Long Jumps optimized to short, when possible by Configuration Flag???...

____________________________________________________________________________________________
 
 
 Namings Check on [Include].

 implement a Memory remaping instead of 1 Mega Limit.
 
 ____________________________________________________________________________________________
 
 
 Scrolling text horizontally sucks
 
 By the way, i don't know how the Errors Manager deals with Run-Time errors
 in DB... I'll have to take a look at this... (To-Do List...)

____________________________________________________________________________________________
____________________________________________________________________________________________

 
 Unify ProgressBar creations, scaling, destructions. 
 
______________________________________________
 
 >[Data: DD 0] , without PREPARSE alternate >>> error pointing to the very first 'D' in
 the Source (!!!) (unknown symbol in 'BuildRelocationAndFillSymbols')... So, revue the
 way the Error Management in that computing search for the faultive Statement.
 
 General clean up of the Sources Editor needed. For example, 'StripBackSpace' is no
 more of any use. 
 
 Follow up with Jonne about Prefetch. 
 (Commenting out "cmp B$EregInside &TRUE | je L1>"  OK ???).
 
 File name when loading .asm!!!! Whishable to change? Without Title... yes...
 
 Bug inside the User Menu definition: Only the first Item was effective. Set *all*
 Paths to &MAX_PATH.
 
 Tree View: suppress the reduce Button in the bar, when runing in Auto-Hide Mode.

 Turn all Api calls into Macros, and store all system calls in one [System] TITLE.

 'DebugActiveProcessStop' >>> Download a more recent Win Help...
 
 Might hangs on upload of a non RosAsm written PE. Maybe the concerned PE did got a '.scr'
 section... See this next time.

 May hang when loading sources with broken resources.

 
 To do???: turn [ ... | ... #1>L | ... | #+1] possible. Actually, only #1, #2,...
 can be rolled.

 Problem with CreateDialog... vs DialogBox... Exit does not behave as Win Doc says.

 We could have a 'ReDo' feature if commenting out the 'ClearNextUndoRecord' call in
 'ControlZ' (keep one for the 'TitleMoveFlag'), and implemeting a 'ControlShiftZ' Routine.

 Add something for Extended Styles in the Dialog Editor. Usefull, for example
 for having a ToolWindow Style Dialog, without modifying in Init.

 In the Dialog Editor: Save to ClipBord >>> turn Styles into Win Equates expressions.

 Reset the overall -general purpose- Comments at Top of each TITLE Parts (partially done).
 
 DLL without anything to Reloc > RelocSize = 8 !!!!!!!!!!!!!!!!! (seems to work OK).
 May be this is even required by the OS (it seems to effectively be required).


__________________
 
 The Api Function calls by Numbers seems wrong in the Disassembly.
 

Disassembler Menus: Menu of Win32Dasm incomplete.
 
____________________________

Add a 0 to 9 UpAndDown Control in the Structure Dialog for multiple Structures.

Re-Write the DkStructures.rtf. Add Examples with Equates forms.

compatible symb.table for external Debuggers

____________________________

The 'EditDialogBoxProc' organisation is now unreadable. An important enhancement
should be done with implementing a Tab Control, under the Main List Box. This Tab 
should say [Style][Dim][ID/Menu][Class][Title][Font/Cdata]. Then, Holding the Tab
Index would much simplify and organise the holding of incoming Messages from the 
various Definitions Controls.

Add the Extended Styles. Search first for what Extended Styles may be coming
with the controls. (Even unsure for the Dialog...). Limit to the ensured ones.

Rewrite, in B_U_Asm [Editors][Dialog_Editor], because the guys do not understand
why some things available in RadAsm are not made available in RosAsm Dialog Editor,
Explain the 'MustHaveBitTable', and the 'ExcludeBitTable' implementation (it
seems nobody noticed this... :().

____________________________________________________________________________________________
____________________________________________________________________________________________

In Asm32Tuts:

Strings: Description / Cases / Endings // Length // Searches //
Copying // Pasting / manipulation.
__________________

 Re-Write C_To_Asm, as HLL_To_Asm. Memory / Pointers / Data / Constructs / Size vs Types
 ...

____________________________________________________________________________________________
____________________________________________________________________________________________

Main Implementations to be entirely done:

* HLL Parser(s). -Non Assembly syntax- . Anybody can do it. A Start point is at 'NewParser'.

* OOA Parsers. -Non Assembly syntax- . Experiment a "Couple of hours" tasks managment.

* Conditional Assembly. Start point at 'MacroWithIf'.

* Version Info Resource. I do not know what this Resource is, physically, but there is
  a description in GorC Resources Compiler Manual.

* Code symbolic Profiler. Only me can do do it.

* Source Ripper. Only me can do it.

* Wizards. (Visual Editors sets, in a DLL, for ToolBars, all Windows Types, from simple
  Buttons to MDI Editors, the various readers and players,... ). Anybody can do it. As
  soon as a volunteer raises his hand up for one of these, open a [User Project] for the
  Wizard Collection, at the Board.
  
* Flirt recognition in the Disassembler. Download and study IdaPro and Dede Disassemblers
  first...
  
* Implement ROS Drivers output. First, find the NT Drivers specifications.
  - They are PEs, but, for example, NT KeyBoard.sys seems a raw Binary.
  - How many Types of Drivers? What .ext?
  - 'PeHeaderCharacteristics' should have something specific.
  - 'ImageBase' ?
  - What Entry Point organisation?
  - What developements rules?
  - What Sections?
  
* A new tool would be great for 'tracking' a Variable. Example, after xxx modifications
  of the Assembler, when fixing a bug inside 'StoreVirtualData, i don't remember what i 
  am doing with 'D$DataListPtr''. The question i wish the answer to is: "Do i make any 
  use of this Variable *after* this given point of the Computation? If yes, where?".
  I think i don't use any more this Variable downward, but, if i turn it zero, nothing
  works... So, i must use it somewhere... Maybe 're-use' for something completely 
  different, that would better require another name...
  
  I imagine an added Double-Click Menu Option saying [Track] and outputing something 
  like a Tree-View of the outines making use of it. It should work the same way for Labels
  (Data and Code), Equates and Macros, and would be great for restructuring, renaming,
  and so on. May be, simply the existing Tree-View, but with the concerned Routines 
  written in Red, or something like this.


