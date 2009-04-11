TITLE OOA
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 OOA (Object Oriented Assembly) Parser.
 
 Author name: Betov

 eMail: < betov@free.fr >

 Home Page: ( http://betov.free.fr/RosAsm.html >
 
____________________________________________________________________________________________
____________________________________________________________________________________________

Plan for RosAsm Object Oriented Programing. (obsolete and delayed...)
____________________________________________________________________________________________
____________________________________________________________________________________________

Introduction.

I use the OOA formulation because we are talking about one Assembly OOP implementation
(not of OOP generally speacking).

The center of RosAsm OOA is the OOA Memory Manager. This is a robust and fast
Memory Manager, that provides your OOA programing with a 'vTable' holding your
Objects representations in Memory. Before any OOA programing, you have to paste
this Manager (from the [Clip] feature), in your Source. This technical choice tends 
to hide as few things as possible, and allows you to add, modify the default OOA 
Memory Manager Routines, if you want to.

____________________________________________________________________________________

A RosAsm OOA Object is a contiguous range of Memory Chunks, organised under a structured
form, holding a Call-Table (Pointers to Procedures) and Objects "private" Data.

"private", here does of course not mean that you can not access these Data by
hand when you want to. We are in Assembly... This simply means that these Data
are supposed to be accessed only by the Call-Table Procedures. It's on you to
apply this rule (or not...). Same for the polymorphism implementation. It's on 
you to not 'overload' your Call-Table' from your 'normal' Code, and to do it from
inside your OOA Procedures, instead.

Objects are Dynamically created, at run time. That is, they live only in Memory
(in vTable), at Run-Time. Their specifications are defined by the CLASS(es)
Declaration(s).
____________________________________________________________________________________________
____________________________________________________________________________________________

CLASSes are abstract (without existance in the dead File) OOA Structures giving 
informations to the RosAsm CLASS Parser. You may consider them a kind of C abstract
Structures.

CLASS Declaration:

>[CLASS Integer         ; 'Integer' will be the Instances Creator.
>  {PreviousClass}      ; {...} is (are) optional Inheritance from another CLASS.
>   SetValue, Int       ; Call-Table, (Procedures pointers with implicit Parameter(s)).
>   AddValue, Int       ; 
>   ShowValue, Int      ; 
>  {Int: D$ 0}]         ; Whatever Data in usual form, with default intitialisations.

* {Inheritance(s)} are optional and can be multiple and/or nested.

* The CLASS Call-Table is Code Procedures Pointers List defining also the default
Parameters to be silently transmitted to the Procedures. These default Parameters
can only be pointers to the same Object elements (Data Pointers, usually).

* {Data: ...} are in the same syntax as ordinary RosAsm Data Declarations, but enclosed
inside {...}, instead of [...], just the same as when Declaring Data by Macros in the
usual RosAsm Convention.


When encounted by the CLASS Parser, the 'Integer' CLASS is stored like a particular
kind of Macro, generating other Macros, and allowing all downward formulations.

You can define the default Initialisations Values of the Data when declaring the CLASS,
and, of course, with one of the Methods of the Object. Example, for overwriting some
Data Value, after the Object Instance creation:

> Integer i        ; D$i+12 = 0  (the '0' declared in the CLASS).
> i.setValue 1     ; D$i+12 = 1
____________________________________________________________________________________________

The Inheritance Declaration is as simple as can be:

[ClASS X {Integer}]

[ClASS Y {Integer}]

[ClASS LeftTop
 {X}
 {Y}]

[ClASS RightBottom
 {X}
 {Y}]

[ClASS RectPos
 {LeftTop}
 {RightBottom}]
 
________________________________________

Notice carefully the difference between:
            
            "[ClASS X {Integer}]"       ; Case 1.

            and:

            "[ClASS X               
              {Integer}]"               ; Case 2
              
            Case 1 is what OOP theory calls an "ISA" relationship, and Case 1 is an "HASA"
            relationship. (Read "Is a" // "Has a").
            
            Macros Names built by case 1 do not require pasting the original name, whereas
            Case 2 does:
            
            * In case 1, for 'SetValue', you say:  > X.SetValue 32
            
            * In case 2, for 'SetValue', you say:  > X.Integer.SetValue 32
            
            To make it short, in Case 1, 'X' is an 'Alias' of Interger. So, in the upper
            example, you have the shorter form of:
            
            > RectPos.RightBottom.X.SetValue 32
            
            ... and not:
            
            > RectPos.RightBottom.X.Integer.SetValue 32
            
            Notice also that Case 1 can only hold one single inheritance, whereas 
            Case 2 may hold as many as wished. You may consider Case 1 as a simple
            Compile time duplication (a substitute) of an existing CLASS, rather 
            than a real new CLASS creation.
              
            
When encounting {PreviousClass}, the CLASS Parser simply replaces this by the real 
"[CLASS PreviousClass...] Declaration. So... :

> RectPos MyRectanglePos
> ...
> MyRectanglePos.LeftTop.X.SetValue 32
____________________________________________________________________________________________
____________________________________________________________________________________________

The default Parameters purpose is not only to save you from having to write the 
default Parameters expected by a Procedure of the Call-Table. It is also the way
for making your Object little closed boxes with an automatic access of the Object
Procedures to the Data belonging to the object:

>Proc SetValue:
>    arguments @NamePtr, @Value
>    
>        mov esi D@NamePtr, eax D@Value, D$esi eax
>EndP

> i.SetValue 1    ; <<<<<<<<<<

; (Instead of "call D$i.SetValue D$i.Int, 1", or something like this).

____________________________________________________________________________________________
____________________________________________________________________________________________

So, first, you declare a Data Table (or Variable(s)) where to store your Objects
Handles:

[i: ?] ; User-side Objects Variable (= Handle = Pointer to vTable).

An Object Handle is nothing but a simple Pointer to the vTable created at run time, 
in Memory, when the Objects creations statements are executed:

> Integer i ; Produces the Creation and the storage in vTable of an image of the
            ; 'Integer' declared CLASS. The Return Parameter (the Pointer to
            ; Object "i", is filled with the real Pointer to the Object image set
            ; in the vTable. Eax, of course, also holds this same Value.

If "Integer i" was not unfolded by the CLASS Parser, but by yourself, 'by Hand',
it would look like this:

> call GetvTable i, 16                      ; Parameters = ObjectHandleRoom, Size.
> push eax, ebx
>   mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+24 0                          ; Store Data initial Value(s).
> pop ebx, eax

"GetvTable" (like "GetMultiplevTable") is a Main Procedure of the OOA Memory Manager. 

Not only the CLASS Parser replaces your "Integer i" statement by the previous Code 
Chunk, but, at the same time, it creates a set of pseudo-Macros, in order to make
your OOP programing friendly. Those Macros could look like this:

> [i.SetValue | mov eax D$i | call D$eax+0 D$eax+4, #1]
> [i.AddValue | mov eax D$i | call D$eax+8 D$eax+12, #1]
> [i.ShowValue | mov eax D$i | call D$eax+16 D$eax+20, #1]

You may consider them usual RosAsm syntax Macros, written for you for free by the
CLASS Parser, and immidiately unfolded by the ClASS Parser *before* all other
downward computations. If an implicit Parameter is to be given in the call to a
"Call-Table-Procedure", you just have to declare it in the CLASS, after the Procedure
Name. The upper Macros examples cover this with "D$eax+4" and friends, what will 
effectively be unfolded under the usual form of a "push D$i+12".

____________________________________________________________________________________

Usage:

> [ClASS Integer
>   SetValue, Int
>   AddValue, Int
>   ShowValue, Int
>  {Int: D$ 0}]
>
> [i: ?]
>
> TestOop:
>    Integer i     ; Object Creation (return: eax = D$i = Pointer to Integer vTable image)
>  ; ...
>    i.SetValue 01 ; D$i+12 = 1
>    i.AddValue 01 ; D$i+12 = 2
>    i.ShowValue   ; "Says 2"
>  ; ...
>    Destroy i     ; Aborts if D$i = 0 // Free the i Object vTable Memory / Set D$i to zero.
> ret


Of course, the example supposes you have 3 Procedures (SetValue, AddValue and ShowValue)
previously written the usual way, the 2 first ones expecting 2 parameters, the last one,
one Parameter.

There is absolutely nothing particular with OOA Procedures. There are nothing but usual
Procedure called in an unusual way, that is, through a Call-Table. Nothing less, 
nothing more:

>Proc SetValue:
>    arguments @NamePtr, @Value
>    
>        mov esi D@NamePtr, eax D@Value, D$esi eax
>EndP

>Proc AddValue:
>    arguments @NamePtr, @Value
>    
>        mov esi D@NamePtr, eax D@Value | add D$esi eax
>EndP

>Proc ShowValue:
>    arguments @NamePtr
>    
>        HexPrint D@NamePtr
>EndP

____________________________________________________________________________________

You can copy an existing Object into a new created one:

> Integer m From i

This formulation allows you to create, for example, a temporary Copy of an Object, 
designed for a temporary Object Data modifications.

Note that this copy does nothing but duplicating the Object: 

If, for example, in "InstanceOne", a Function has been run, in order to allocate some
memory, and to save this memory Pointer into its object Data, the "InstanceTwo" Member
holding this allocated memory Pointer *will* point to the same Memory as the one in
"InstanceOne". If you plan to have different Memories allocated for each Instance,
call the new created one Function for reinitializing the Memory Allocation and its
object Data Pointer. If you plan to have the same dynamic Data (Pointers) in several
Instances, this is the helpfull way. Of course, if your plan is to have two copies
of the Pointed Data, you have to implement a Procedure performing the allocated Data 
Copy from D$PointerOne to D$Pointertwo.

This way for Copying an Object may also be usefull in order to reduce the Code size.
As you may have noticed, each Object Creation adds some real Code in your Application.
This Code Bloat may be reduced by the usage of the "From" Keyword. For example, after
your "Interger i" Statement, instead of:

> Integer j                                 ; what will, again, add to your Code:

> call GetvTable j, 16                      ; Parameters = ObjectHandleRoom, Size.
> push eax, ebx
>   mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+24 0                          ; Store Data initial Value(s).
> pop ebx, eax

You can do:

> Integer j From i                   

; That will be unfolded as one single "call" instead of Bloatware:

> call GetvTableFrom D$i, j, 16

As this formulation may save much of Code Bloat, it is silently applied by the CLASS
Parser, when encounting Multiple Declarations:

> Integer i, j, k  ; Will be unfolded by the CLASS parser as:
>
> ; First shot:
> push eax, ebx
>   mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+24 0                          ; Store Data initial Value(s).
> pop ebx, eax

>
> ; Next shots:
> call GetvTableFrom D$i, j, 16
> call GetvTableFrom D$i, k, 16

____________________________________________________________________________________

        (> [i.SetValue | mov eax D$i | call D$eax+0 D$eax+12, #1])

Now, let us suppose you want a Table of 200 Integers Objects.

> [IntegersHandleTable: ? #200]
>
> Integer IntegersHandleTable, #200    ; <<< "#200" = "Does this 200 times"

When encounting this '#200' Parameter, the CLASS Parser, instead of creating the
usual macros (IntegersHandleTable.SetValue, IntegersHandleTable.AddValue,
IntegersHandleTable.ShowValue) create the Macros in the form of:
'esi.SetValue', 'esi.AddValue', 'esi.ShowValue'. So, limitation:
In order to not multiply no end the created Macros, only esi can be use as a pointer
to OOA handle Tables.

> [IntegersHandleTable: ? #200]
> ...
> Integer IntegersHandleTable, #200    ; <<< "#200" = "Does this 200 times"
> ...
> lea esi D$IntegershandleTable+(3*4)
> esi.SetValue 012345

Will be unfolded as:

> [IntegersHandleTable: ? #200]
> ...
> call GetvTable IntegersHandleTable, 16
> push eax, ebx
>   mov D$eax SetValue                      ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+4 ebx      ; 'Int' Pointer default Parameter
>   mov D$eax+8 AddValue                    ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+12 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+16 ShowValue                  ; store METHOD
>   lea ebx D$eax+24 | mov D$eax+20 ebx     ; 'Int' Pointer default Parameter
>   mov D$eax+24 0                          ; Store Data initial Value(s).
> pop ebx, eax
> 
> call GetMultiplevTableFrom D$IntegersHandleTable, IntegersHandleTable, 16, 199
> ...
> lea esi D$IntegershandleTable+(3*4)
> mov eax D$esi
> call D$eax+0 D$eax+4, 012345

____________________________________________________________________________________________
____________________________________________________________________________________________

Polymorphism formulations look like this:

> i.AddValue = SubValue

... of course to be stated *inside* one of the Call-Table Routines. Example:

>Proc SetValue:
>    arguments @NamePtr, @Value
>    
>        mov esi D@NamePtr, eax D@Value, D$esi eax
>
>        If eax > 0FFF
>            i.addValue = SubValue
>        Else_If eax < 0FF
>            i.addValue = AddValue
>        End_If
>EndP


Stating "i.addValue = SubValue" in your "normal" Code, outside the OOA Procedures,
would breack the OOP style programation, just like directely modifying *by hand*
some "private" Data of an Object. To keep it OOP-like *you* have to follow the OOP
way for doing these things. RosAsm OOA organisation does not at all control if you
are following the OOP programing rules or not. It's on your own choice.

Notice also, that, in the upper example,

> i.SubValue 3

is a wrong statement, as long as "SubValue" appears nowhere inside any CLASS
Declaration. The right statement is yet:

> i.AddValue 3

This *is* one aspect of "Polymorphism": You don't have to know and to care about
how the Object "little independant Robot" does the things once you have defined
this inside the Call-Table Routines. At the time your write your 'normal' Source,
you do not have any more to know if the robot will "add" or "sub" your Value, or
anything else. The robot itself chooses if he wants to add or sub. So, the applyed
Names are to be carefully choosen. In the ridiculous upper example, of course,
you should not say "SubValue" / "AddValue", but, "Modify". A simple way for doing
this is to give *two* Names to your default Procedure:

> Proc AddValue: Modify:             ; <<<<<< 2 Names !!!
>     arguments @NamePtr, @Value
>    
>         mov esi D@NamePtr, eax D@Value | add D$esi eax
> EndP

And to provide the general meaning default Name inside the CLASS Declaration:

> [ClASS Integer
>   SetValue, Int
>   Modify, Int      ; <<<< Default AddValue represented by the General Name.
>   ShowValue, Int
>  {Int: D$ 0}]
> ...
> Integer i
> ...
> i.Modify 1

... and go on, without caring if it is an addition or a substraction.

May be you do not see the point of this, at all. So imagine you have a ball
moving on the screen. You want this ball to grow and decrease repeatitly without
taking care of how the Robot "Ball" does this, in your 'normal' Code. Then, it
would be, simply:

> If D@Message = &WM_PAINT
>     ....
>     Ball.Size.Modify 1
>     ...

Instead of (without Polymorphism):

> If D@Message = &WM_PAINT
>     ....
>     Ball.GetSize
>
>     If B$TheBallIsGrowingUpFlag = &TRUE
>         If eax >= MAX
>             Ball.Size.SubValue 1
>             mov B$TheBallIsGrowingUpFlag &FALSE
>         Else
>             Ball.Size.AddValue 1
>         End_If
>
>     Else
>         If eax =< MIN
>             Ball.Size.AddValue 1
>             mov B$TheBallIsGrowingUpFlag &TRUE
>         Else
>             Ball.Size.SubValue 1
>         End_If
>
>     End_If
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

[OOAinside: ?    PreviousSeparator: ?]

[UnfoldedCLASS: ?    TempoUnfoldedCLASS: ?    ClassUnfoldingOver: ?    ClassListPtr: ?]
[OneCLASSname: ? #80] [OneClassNamePtr: ?    OriginalClassDataPointer: ?]

;;
We Search for [CLASS ...., and store all occurence encounted in the Source into
MacroList and MacroData (the same way as for Macros). At the same time we arase
the Declarations in the Source.
;;

ClassParser:
    mov esi D$CodeSourceA, edi D$CodeSourceB | mov ecx esi | add ecx D$StripLen
    mov B$ErrorLevel 11  ; 'error11'

    move D$StatementsPtr D$StatementsTable | sub D$StatementsPtr 4
;;
[CLASS Integer   30 37 31 0D  >>> 0 7 1 ]
 {NestedClass}
 addvalue Data
 subValue Data
 {Data: 0}]      0a 0d 0a04c >>> ':' ] 
;;
    .While esi < ecx
        If B$esi = TextSign
            movsb | While B$esi <> TextSign | movsb | End_While

        Else_If D$esi = 'CLAS'
            cmp B$esi-1 OpenBracket | jne L1>
            cmp B$esi+4 'S' | jne L1>
            cmp B$esi+5 Space | jne L1>
                mov al B$esi-2, B$PreviousSeparator al
              ; Suppress the last written '[':
                dec edi

                call StoreOneCLASS | mov B$OOAinside &TRUE

        End_If

L1:     movsb
    .End_While
;;
If Some CLASS Declaration has been found, we do all the job, CLASS by CLASS:
;;
    .If B$OOAinside = &TRUE
        sub edi D$CodeSourceB | mov D$StripLen edi
        Exchange D$CodeSourceA D$CodeSourceB

        VirtualAlloc UnfoldedCLASS 10_000, TempoUnfoldedCLASS 10_000

        move D$ClassListPtr D$MacroList | add D$ClassListPtr 5

L0:     mov esi D$ClassListPtr, edi OneCLASSname

        If B$esi > 0
            While B$esi > EOI | movsb | End_While | mov B$edi 0     ; Name
            mov D$OneClassNamePtr edi
            inc esi                                                 ; EOI
            mov D$OriginalClassDataPointer esi
            lodsd                                                   ; Pointer to 'MacroData'
            mov ecx D$esi                                           ; Size
            add esi 5                                               ;( + EOI)
            mov D$ClassListPtr esi                                  ; Ready for Next Record.

            mov esi eax, edi D$UnfoldedCLASS | rep movsb | mov B$edi 0

            call UnfoldIncludedCLASSES | call UnfoldMethods
            call UnfoldPrivateData | call UnfoldOneOOA | jmp L0<    ; Loop each CLASS.
        End_If

        VirtualFree D$UnfoldedCLASS, D$TempoUnfoldedCLASS

      ; Restore the Macros Pointers for Normal Macros:
        move D$MacroListPtr D$MacroList | add D$MacroListPtr 5
        move D$MacroDataPtr D$MacroData
    .End_If

ret


StoreOneCLASS:
    push edi, ecx
        mov edi D$MacroListPtr
      ; esi > 'CLASS Name'
        add esi 6

        On B$esi < '.', error D$MissingCLASSnamePtr
      ; Store the Name into MacroList:
        While B$esi > LowSigns | movsb | End_While
        mov B$edi EOI | inc edi | inc esi

      ; Write Pointer to Body Data:
        mov eax D$MacroDataPtr | stosd | mov ebx edi
        mov edi eax
; [CLASS X = Integer]
      ; Now write the Data Body for the instruction > Name i
;;
> [ClASS Integer
>   SetValue, Int
>   AddValue, Int
>   ShowValue, Int
>  {Int: D$ 0}]
>
> [i: ?]
>
> TestOop:
>    Integer i     ; Object Creation (return: eax = D$i = Pointer to Integer vTable image)
>  ; ...
>    i.SetValue 01 ; D$i+12 = 1
>    i.AddValue 01 ; D$i+12 = 2
>    i.ShowValue   ; "Says 2"
>  ; ...
>    Destroy i     ; Aborts id D$i = 0 // Free the i Object vTable Memory / Set D$i to zero.
> ret

If "Integer i" was not unfolded by the CLASS Parser, but by yourself, 'by Hand',
it would look like this:

> call GetvTable i, 16              ; Parameters = ObjectHandleRoom, Size.
> mov D$eax SetValue | add eax 4    ; store METHOD(s).
> mov D$eax AddValue | add eax 4    ; ...
> mov D$eax ShowValue | add eax 4   ; ...
> mov D$eax 0                       ; Store Data initial Value(s).
;;
        mov edx esi, ecx 0

        While B$esi <> CloseBracket
            lodsb
            If al = EOI
                mov al meEOI
            Else_If al = '{' | inc ecx
                mov al OpenVirtual
            Else_If al = '}' | dec ecx
                mov al CloseVirtual
            End_If
            stosb
        End_While
        On ecx <> 0, error D$UnPairedNestedBracketsPtr

        mov D$MacroDataPtr edi

      ; Write size into MacroList:
        mov ecx esi | sub ecx edx
        mov D$ebx ecx | add ebx 4 | mov B$ebx EOI | inc ebx | mov D$MacroListPtr ebx

      ; Adjust Source Pointer:
        inc esi | mov al B$esi
        On al = B$PreviousSeparator, inc esi

    pop ecx, edi
ret


;;
If a CLASS Declaration body (in MacroData) includes a {Inheritance}, we unfold it now.
This is to say, if this is stored:

[CLASS Integer         ; 'Integer' will the Instances Creator.
  ;{PreviousClass}      ; {...} is (are) Inheritance from another CLASS.
   SetValue, Int       ; Call-Table, (Procedures pointers with implicit Parameter(s).
   AddValue, Int       ; 
   ShowValue, Int      ; 
  {Int: D$ 0}]

[ClASS X
 {Integer}]
 
... We turn the this last {Integer] into:

 X.SetValue, X.Int       ; Call-Table, (Procedures pointers with implicit Parameter(s).
 X.AddValue, X.Int       ; 
 X.ShowValue, X.Int      ; 
{X.Int: D$ 0}]
;;


UnfoldIncludedCLASSES:

L0: mov B$ClassUnfoldingOver &TRUE

    mov esi D$UnfoldedCLASS, edi D$TempoUnfoldedCLASS

  ; Case ot [CLASS X ISA Integer]:
  ;  If D$esi = 03415349
  ;      call ClassSubstitute    ; 'ISA ' >>> 03415349
  ;      jmp L0<
  ;  End_If

L1: .While B$esi > 0
        lodsb

        ..If al = OpenVirtual
            mov ebx esi | While B$ebx > LowSigns | inc ebx | End_While

            .If B$ebx = CloseVirtual
              ; Cases of {InHeritance}
                push ebx
                    call AppendThisClassName esi
                    call CopyThisCLASS | mov B$ClassUnfoldingOver &FALSE
                    If B$NestedClassInside = &TRUE
                        While B$esi-1 > 0 | movsb | End_While
                        Exchange D$UnfoldedCLASS D$TempoUnfoldedCLASS
                        pop ebx | jmp L0<<
                    End_If
                    call StripThisClassName
                pop esi
                inc esi

            .Else
              ; If it is not CloseVirtual, it may be a ColonSign >>> Data (the UnPaired '{}'
              ; checked with Data parsing):
L2:             stosb
            .End_If

        ..Else
          ; If not OpenVirtual >>> Call-Table.
            stosb
        ..End_If
    .End_While

    mov B$edi 0 | Exchange D$UnfoldedCLASS D$TempoUnfoldedCLASS

    If B$ClassUnfoldingOver = &FALSE
      ;  call UpdateMacroData |
      jmp L0<<
    End_If
ret

; In Case ot [CLASS NewClassName = ClassName], we do nothing but substitute the MacroList
; Pointer and Size with the ones of the "Mother" CLASS. This is to say that Two entries
; will exist (2 different Names), for the same CLASS Data. The "= ClassName", in MacroData
; is simply lost:

ClassSubstitute:
  ; D$OriginalClassDataPointer yet points to this Class Pointer to macroList.
L0: add esi 4
    call SearchMacroListClassName
  ; >>> eax = Pointer to MacroData // ecx = Size.
;    If D$eax = 03415349   ; 'ISA ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;        mov esi eax | jmp L0<
;    Else
;        mov ebx D$OriginalClassDataPointer, D$ebx eax, D$ebx+4 ecx
;        mov esi eax, edi D$UnfoldedCLASS | rep movsb | mov B$edi 0
;    End_If
ret


UpdateMacroData:
    mov esi D$UnfoldedCLASS, edi D$MacroDataPtr, ecx 0
  ; New Pointer to MacroData, in MacroList:
    mov ebx D$OriginalClassDataPointer,  D$ebx edi | add ebx 4
  ; Copy the new Body at end of MacroData:
    While B$esi <> 0 | movsb | inc ecx | End_While
  ; New Body Length in MacroList and adjust the Writing Pointer:
    mov D$ebx ecx, D$MacroDataPtr edi
ret


Proc AppendThisClassName:
    Argument @Source
    Uses esi, edi

        mov esi D@Source, edi D$OneClassNamePtr, B$edi '.' | inc edi
        While B$esi > LowSigns | movsb | End_While | mov B$edi 0
        mov D$OneClassNamePtr edi
EndP


StripThisClassName:
  ;  mov eax D$OneClassNamePtr
  ;  While B$eax <> '.' | dec eax | End_While
  ;  mov B$eax 0, D$OneClassNamePtr eax
    mov eax OneCLASSname
    While B$eax <> '.' | inc eax | End_While
    mov B$eax 0, D$OneClassNamePtr eax
ret


SearchMacroListClassName:
    push edi, esi
        mov edi D$MacroList | add edi 5

L0:     mov edx edi | lodsb
        While al = B$edi
            On al < LowSigns, jmp L1>
            lodsb | inc edi
        End_While

        ..If al < LowSigns
L1:         .If B$edi < Separators
              ; Found:
                inc edi
                mov eax D$edi           ; Pointer to MacroData.
                mov ecx D$edi+4   ; Size.
                jmp L9>

            .Else
L2:             pop esi | push esi

                While B$edi > Separators | inc edi | End_While | add edi 10
                If B$edi = 0
                    mov ebx esi | While B$ebx <> CloseVirtual | inc ebx | End_While
                    mov B$ebx 0 | Error D$NoParentClassPtr
                Else
                    jmp L0<
                End_If
            .End_If
        ..Else
            jmp L2<
        ..End_If

L9: pop esi, edi
ret                 ; eax = Pointer to MacroData // ecx = Size.


[NestedClassInside: ?]

CopyThisCLASS:
    call SearchMacroListClassName
    mov B$NestedClassInside &FALSE
  ; Copy Parent CLASS Body, and add the actual CLASS Name before all Components, but
  ; nested Inheritances:
    push esi
        mov esi eax

L0:     ..If B$esi = OpenVirtual

            Do
                .If B$esi > LowSigns
                    mov ebx esi | While B$ebx > LowSigns | inc ebx | End_While
                  ; Cases of Data Declaration: Add the Actual Class Name:
                    If B$ebx = ColonSign
                        Call CopyActualClassName

                        While B$esi > ColonSign
                            movsb | dec ecx | jz L9>
                        End_While

                    Else
                        mov B$NestedClassInside &TRUE

                    End_If
                .End_If

                movsb | dec ecx | jz L9>
            Loop_Until B$esi = CloseVirtual

            movsb | dec ecx | jz L9>
            jmp L0<

        ..Else
          ; In case the first written element start with a Method Char:
            If B$esi < LowSigns
                movsb | dec ecx | jz L9>
                jmp L0<

            Else
                call CopyActualClassName

                While B$esi > Space
                    movsb | dec ecx | jz L9>
                End_While
                movsb | dec ecx | jz L9>
                jmp L0<<

            End_If

        ..End_If

L9: pop esi
ret

CopyActualClassName:
    push esi
        mov esi OneCLASSname
        While B$esi > 0 | movsb | End_While | mov al '.' | stosb
    pop esi
ret


UnfoldMethods:

ret


UnfoldPrivateData:

ret


UnfoldOneOOA:

ret





