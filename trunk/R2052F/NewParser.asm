TITLE NewParser

____________________________________________________________________________________________
____________________________________________________________________________________________
;;

 If you want to write a new RosAsm Pre-Parser, this is the room where to start:

 Author name: ...

 eMail: ...

 Home Page, if any: ...
____________________________________________________________________________________________

 Description (Purpose / Syntaxe(s) / ...).
 
    This has to be a text, ready to be included inside the B_U_Asm File, and you should
    first write this before writing any Instruction. If you are not able to provide the
    user Manual, at least partially and temporary, *before* writing the Implementation,
    you have a real problem...
    
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
 This is the room where volunteers will start the developement of a new PREPARSE Parser.
 Everything is ready to run. If you write a Source with:
 
 > PREPARSE New
 
 at the Top of Source, 'NewParser', down there, will be run.
 
 'New' is the temporary name of your new Pre-Parser (i will update it when ready for
 release).
 
 The job of a Pre-Parser is nothing but the front-end job of any Compiler, wich is
 nothing but replacing a Statement by another one, or by a flow of other ones, and
 ready to be assembled the regular way. So, this all should remain a "simple" (...)
 substitutions work.
 
 ____________________________________________________________________________________________

 Here is now what you have to know, in order to implement a New Pre-Parser:
 
 At this level of the Compile Process, the user Source, that you read from 'CodeSourceA'
 and have to rewrite into 'CodeSourceB', is no longer simple Ascii, but, instead a
 'coocked Ascii' Source.
 
 Before coming here, the Compile Process has already run the 'SourceCleaner', which does
 a lot of clean-up and replacements works. Generaly speacking, all of the particular
 Chars that we have to take care of -when parsing a Source-, have been replaced by lower
 Ascii Chars. To see this list of 'particular' Char, Right-Click upon 'MyAsciiTable' 
 and/or the upper 'LowSigns' Table.
 
 The Source you will have to work with has the following characteristics:
 
 * All is upper case
 * ' ' is turned 'Space'
 * '|' and CR/LF are turned 'EOI'
 * ',' are stripped of, as well as the next CR/LF if any,... and so on.
 
 Try to memorise all of these substitutions, first, as, for example, you will search
 in vain for any ':' inside the Source (as it now is 'ColonSign' -Ascii 14-...).

____________________________________________________________________________________________

 One important thing to know and respect:
 
 In order to keep track of the real user Source Instructions, all along the Compilation
 processes (up to the Debugger run time stage -!!!...-), when adding new instructions 
 with your new Parser, you have to add a new Separator. The normal separator, inside the 
 'coocked' Source is 'EOI' (Ascii 2), which replaces '|' and CR/LF). When you have
 to introduce a new added intruction (an Instruction that is *not* inside the real user
 Source), you have to add a new separator. This new separator is *not* 'EOI' (Ascii 2), 
 but 'meEOI' (Ascii 1). If this rule is broken, the error manager will point to the wrong
 statement, in the user Source, in error case, in all downward computations (!!!...).
 
____________________________________________________________________________________________
 
 If you need to insert some Labels of yours when Parsing, you may :
 
 > call CreateNoMeanLabel
 > call WriteNoMeanLabel
 
 Then, at 'NoMeanLabel', you have a String that you may copy in your output. In case
 of problem, first see how they are used in 'ParaMacrosParser', for example...
 
 The Data Declarations Separators, in that case, are neither 'OpenBracket', 'Closebracket',
 'OpenVirtual', 'CloseVirtual', but simply: '{' and '}'. These Chars are translated into
 the required LowSigns by the donward Computations of the Macros Parser.
 
 Not respecting this point, and the EOI vs meEOI thingies, will foolish the Error
 Manager. To verify that your work respect these constraints, when you have finished
 a working Pre-Parser, just test some Statement(s) making use of it, and, on the next 
 Instruction, produce some on-purpose error. If the error Manager wrongly points to
 a downward Statement, this is because you did an error when insterting a Separator.
 
____________________________________________________________________________________________

  If your Pre-Parser is simple enough, you do not need any Error Management. If something
  is wrong is encounted along your own computation, just abort, and let it go. Downward
  Computations will point out the error and will save you of this complication. If your
  Parser is complex enough and absolutely requires an Errors management, ask me, and i
  will write your Errors Management. (I don't release this, here, by default, in order to
  incitate you to do without, as long as possible).
 
____________________________________________________________________________________________

 Good work. Betov.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


; Your new Pre-Parser Squeleton:

NewParser: RET  ; <<<<<<<<<<<<<<<<<!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    mov esi D$CodeSourceA, edi D$CodeSourceB | mov ecx esi | add ecx D$StripLen

    .While esi < ecx
        ;
        ; Your job...
        ;

        movsb

    .End_While

    sub edi D$CodeSourceB | mov D$StripLen edi
    Exchange D$CodeSourceA D$CodeSourceB
ret




























