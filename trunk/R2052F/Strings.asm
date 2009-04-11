TITLE Strings

;;
  TITLE where to save the RosAsm Messages Strings having a possible counter-part
  in the national languages Files.
  
  In the RosAsmFiles Folder, if a 'RosAsmStrings.xx' is found out, the substitution
  is done by 'OpenStringsFile'.
  
  Organization:
  
  * The default English Strings are declared the usual way.
  
  * The 'StringsTable' contains 2 dWords Records. Typically:
  
      STRING1 ErrorMessageTitlePtr: ErrorMessageTitle
      
      Where: 
      - 'STRING1' is the StringID (Number 1)
      - 'ErrorMessageTitlePtr' is the Substitution Label
      - 'ErrorMessageTitle' is the Pointer to the default English Strings, in the Source
      
   The StringIDs purpose is to introduce some flexibility in the management:
   
   1) The ID as found in the 'RosAsmStrings.xx' File and in the 'StringsTable'
   are simple Ordinal, and must be given, in both cases, in the Ordinal order.
   
   2) Some Strings and IDs may be missing.
   
   * How it works:
   
   When a National Language String is found out, it is loaded for once, in Memory.
   
   CRLF are zeroed and the Strings are parsed:
   
   When some '$$$$25' is found, the 'OpenStringsFile' translates '25' into Binary
   and searches the matching Record ID, inside the 'StringsTable'. Once this
   Record is pointed out, the String Pointer is overwritten, in order to point
   to the char after '$$$$25 '.
   
   In failures cases, an error Message is send.
   
   __________________________
   For the RosAsm volunteers:
   
   When you develop your TITLE, do not use this. Simply write your own errors
   Messages inside your TITLE, the usual way.
   
   Then, only when you think that your developements are more or less in a fixed
   state, considering the errors Messages, Implement the Internationalization.
   
   How to:
   
   1) Move your Strings here, after the existing ones.
   2) If there is not enough 'STRINGXXX' available, add the ones you need.
   3) Extend the 'StringsTable' to hold your own Strings.
   4) In your Source, replace all the concerned 'StringSymbol' by 'D$StringSymbolPtr'.
   2) Take a copy of 'RosAsmStrings.en', and append your Strings.
   
   For the Equates Ordinals, you can go to the next 1000 Boundary, so that each
   Volunteer (or purpose) will have a set of 1000 Strings IDs reserved, in order
   to ease a little bit the future Maintainance. For example, the Equal Pre-Parser
   Strings could begin at 2000, the Debugger Strings at 3000, and so on... I plan
   to use 1000 for the RosAsm Menu, and some other things.
   
   Once done, send me, all at once: Your TITLE, The Strings TITLE and the
   'RosAsmStrings.en' File.
;;



[ErrorMessageTitle: ' error', 0]  ; Error,  OutOnError

; List of error messages:

 [EmptyString: B$   '                                                        ', 0
  MissingFile:      'File not found', 0
  BusyFile:         'Unable to open: System mangling destination file', 0
  CreationFile:     'Error when trying writting file', 0
  OpenFile:         'File To open Not found', 0
  MissFile:         'File not found', 0
  NoReadFile:       'Unable to read file', 0
  MissEquateVal:    'Missing equate value', 0
  EquateLabel:      'An Equate name can not be followed by a Colon Char', 0
  TextEquate:       'Missing ">" Char at the end of Text Equate', 0
  MacPara:          'Bad parameters number in macro evocation', 0
  MissingParameter: 'Bad parameters number in Macro evocation', 0
  MacNumber:        'Bad # construction', 0
  BadMacroDirection: 'Always ">", for Macros Multiple Parameters. Never "<"', 0
  TooBigX:          '#x limit is 9', 0
  TooMuchParam:     'Too much Parameters found when unfolding', 0
  InfiniteLoop:     'An infinite loop has been generated when unfolding this macro', 0
  MacrosOverFlow:   'More than 4000 Bytes outputed at once, when unfolding this Macro!!!', 0
  UnknownData:      'Unknown data type', 0
  UnknownSize:      'Unknown Size marker', 0
  Mem3D:            '64 Bits (Q$...) mem size expected for all 3D Now instructions' 0
  NewHmem:          '"H$" now reserved for futher developement. Use "R$" or "F$" instead', 0
  PrefetchMem:      'Prefetch/w/0/1/2/NTA syntax: > PrefetchXXX B$Address' 0
  DsizeForLab:      'D$ needed for Labels data', 0
  Unknown:          'Unknown text structure. May be internal error', 0
  BadSeparator:     'Unexpected separator', 0
  BinType:          'Invalid binary number', 0
  HexType:          'Invalid Hexa number', 0
  DezimalType:      'Invalid decimal number', 0
  OutOfRange:       'Out of range value', 0
  DefEquate:        'This equate or macro symbol is already used', 0
  DefReEquate:      'This equate symbole is not in use', 0
  TooMuchLabels:    'More than 2 labels is this expression', 0
  BadLabel:         'Bad character in Label name', 0
  LabelDup:         'Duplicate label definition', 0
  SymbolDup:        'Duplicate symbol definition', 0
  OrphanColon:      'Orphan Colon sign encounted', 0
  LocalDefLabel:    'Local label not allowed in data definition', 0
  NoLocalCall:      'You cannot call for a Local Label', 0
  NotAnOpcode:      'Unknown mnemonic', 0
  OverByte:         'Value out of byte range', 0
  OverWord:         'Value out of word range', 0
  OverDword:        'Value out of Dword range', 0
  LenSize:          'Leading Len can only be dWord or word', 0
  BadReal:          'Bad real number declaration' 0
  OrphanPrefix:     'An instruction prefix can not stand alone', 0
  DoubleSOP:        'Double Segment override not allowed', 0
  NotSegment:       'Unresolved segment override', 0
  Parameter:        'Unvalid parameter', 0
  Double0:          'Only one "&0" automatic Label per Macro', 0
  MissingSign:      'Need of equal or colon sign', 0
  MissingSeparator: 'Missing space at end of a Data String declaration', 0
  BadMacroLoop:     'Bad placement of Loop instruction in Macro', 0
  TooMuch:          'Too much operands found', 0
  DBsize:           'Hexa Octets values expected for Code Bytes', 0
  LockError:        'No Lock Prefix with this instruction', 0
  LockMemError:     'Lock: One Member must be Memory', 0
  MissingOperand:   'Missing operand', 0
  NotEnough:        'Missing operand', 0
  MemMem:           'Expressions are allowed for only one memory adressing parameter', 0
  Parenthesis:      'Unpaired parenthesis', 0
  ParaMacro:        'Bad ParaMacro', 0
  MacroVariableIndice: 'Macros Variables are from "&1" to "&100" (Decimal)', 0
  NestedMacroVariable: 'Imbricated declaration of Macro Variable', 0
  MarkerBefore:     '$ize Marker wanted before Real Expression', 0
  MarkerAfter:      '$ize Marker wanted after Real Expression', 0
  ExpressionMember: 'Immediate only in Expressions', 0
  NoDataLocalLabel: 'No Local Labels in Data: They are reserved for Code', 0
  RealNotation:     'Real Expression: only +-*/ Operators, no nested Parenthesis', 0
  ExpressionSign:   'Bad operator in Expression', 0
  DataLoopNumber:   'For Huge Data Declaration, use Virtual (?) Data or call for VirtualAlloc', 0
  SmallDataLoop:    'What do you mean with "repeat less than 2 times???..."', 0
  BadLoop:          'Loop with no previous Data Declaration', 0
  VDataLoopNumber:  'For huge Virtual Tables: VirtualAlloc or [Configuration]/[Bad Habits]', 0
  TooMuchExpression: 'Expression number too big in result or before multiplication', 0
  ExpressionNOT:    'NOT must be at leading Pos of an Expression member', 0
  ExpressionSHR:    'SHR / SHL Parameter over Byte size in Expression', 0
  OpenText:         'Unpaired text delimiter', 0
  Orphanbracket:    'Unpaired square bracket', 0
  UnexpectedCRLF:   'Unexpected CR LF after Bracket', 0
  MissingSeparator1:'Missing Separator before Text Delimiter', 0
  MissingSeparator2:'Missing Separator after Text Delimiter', 0
  UnPairedNestedBrackets: 'Unpaired {Nested} Brackets', 0
  NestedBrackets:   "'{}' chars are reserved for nested Declarations", 0
  PseudoLocal:      "Pseudo local '@Label' before any Plain Label declaration", 0
  DoubleSIB:        'Only one Index and one base allowed in SIB', 0
  EspIndex:         'ESP can t be use as an index in SIB', 0
  Expression:       'Impossible effective address parameter', 0
  ScaleValue:       'Scale possible values are 2, 4, 8 and extensions to 3, 5, 9', 0
  ESPsib:           'ESP can t be used as index register', 0
  DoubleIndex:      'Only one index allowed in expression', 0
  DoubleLabel:      'Double label or unknown error', 0
  UnknownParameter: 'unable to resolve this parameter', 0
  EnterStack:       'Enter Stack size goes up to 0FFFC and must be 4 Bytes aligned', 0
  EnterLevel:       'Enter level goes from 0 to 31', 0
  LeaInstead:       'To retrieve Label+imm Adress, use LEA. Exemple: " > lea ebx D$Label+8" ', 0
  MixType:          'these two parameters types not allowed together', 0
  MissType:         'UnFitting sizes of operands', 0
  OperandsTypes:    'Failure of Analyze in operands types for this mnemonic', 0
  BadAlign:         'The Alignment must be a power of two between 4 and 0100', 0
  OperandSize:      'Bad operand size', 0
  FPregNotAssumed:  "Can't guess Registers for this FP Instruction", 0
  LeaTypes:         '"lea reg, mem" only. For "lea eax imm", use "mov eax imm"', 0
  LeaSize:          'There is no Byte form for lea', 0
  EndingImm:        'Third imm parameter wanted', 0
  Xmarker:          'X$Memory parameter wanted', 0
  OverFlow:         'Over flow of immediate number', 0
  UnknownSymbol:    'This symbolic name does not fit with any label', 0
  WhatIsThis:       'What is this?', 0
  UnAble:           'RosAsm has been unable to assemble this instruction', 0
  NotYetSign:       'Only + - signs allowed here, or see [Expressions] in Help', 0
  NotYetMnemo:      'Sorry, this mnemonic is not yet implemented. Information needed', 0
  ;ShortDis:         'Need of long dis., >> or << (never on LOOP)', 0
        TooLongOf:  '     Byte(s) out of range', 0
  ShortDis:         "Need of long dis., >> or << (never on LOOP)
#1 #2 out of range", 0

  NoPlainLabelForLoop: 'Local Short Label is required for LOOP', 0
  LongLoop:         'Local Short Label is required for LOOP', 0
  NoPlainLabelForJECX: 'Short Local Label (up or down) is required for JCXZ / JECXZ', 0
  LongDis:          'Only Short displacement allowed with this mnemonic', 0
  NForbiden:        'N symbol not allowed here. Use L', 0
  MixedLen:         "You can't mix 'head LEN' and 'Free LEN'", 0
  BadLoopNumber:    'decimal value expected for data declaration loop number', 0
  VirtualData:      'Bad Virtual Data', 0
  NestedLoop:       'Sorry, data loop can not be nested', 0
  NeedByteSize:     'Byte size required', 0
  OnlyAcc:          'Only accumulator with this instruction', 0
  TxtTooMuch:       'Text parameter exceeding 4 bytes', 0
  GPregister:       'A register must be general purpose', 0
  VERRword:         'VERR/VERW / LTR parameter must be 16 bits register or memory', 0
  WishEreg:         'Memory adressing expects full 32 bit general purpose registers', 0
  BadApi:           "Api calls form is: call 'Module.Function'", 0
  NoApi:            'No Api call found in source', 0
  DllNotFound:      'Internal error: user called DLL not found at compile time', 0
  ApiNotFound:      'Internal error: user called API not found at compile time', 0
  NoLocal:          'Local Label not allowed here', 0
  NoEntry:          'Entry Point not found (should be "Main:" if not modified)', 0
  BadFPUcond:       'FPU available conditions: B, E, BE, NA, U, NB, AE, NE, NZ, NBE, A, NU', 0
  ST0wished:        'One of these two reg must be ST0', 0
  STwished:         'Only ST0, ST1, ...., ST7  regs with this mnemonic', 0
  FADDPreg0:        'FADDP second Reg must be ST0', 0
  FSUBPreg0:        'FSUBP second Reg must be ST0', 0
  BadFpChar:        'Unexpected Char inside Float Expression', 0
  NoAdressee:       'Internal: Unknown adressee', 0
  BadMemRelease:    'Internal: Fail of Memory release', 0
  NotPE:            'No source in this PE. Disassemble it?', 0
  NotPeExe:         'This is not a PE File', 0
  BadWinEqu:        'Unknown Win Equates name', 0
  IdTooSmall:       'ID base number must be between 1 and 65535', 0
  IdTooBig:         'ID base number must be between 1 and 65535', 0 ;;;1000 and 32000', 0
  BadLibName:       'Wrong Module name', 0
  DoubleFunction:   'Impossible: Same Function in 2 DLLs       ', 0
  BadFunctionName:  'Wrong api name', 0
  BadOrdinal:       'Wrong api Ordinal', 0
  NoAapi:           'No ending "A" here', 0
  MacVarOverFlow:   'Too long Data (or infinite storage loop) for Macro "&x="', 0
  MacVarNumber:     'Too high Number for a Macro Variable', 0
  MissingApiA:      'Ending "A" (or "W") wanted', 0
  BadPreParse:     'PREPARSE is for: Alternates, Equal', 0
  MissingCLASSname: 'Missing Class Name', 0
  NoParentClass:    'Parent CLASS not found when unfolding Inheritance', 0
  SizeOfResource:   'Too Big Resources. Big Images or Sounds should be left in Files', 0
  NumerAsSymbol:    'A Number cannot be a Symbolic Name', 0
  UnexpectedReg:    'Unexpected Register or too short Symbol', 0
  TextKilling:      'This Macro removes a Text Delimiter', 0
  NestedMacroLoop:  'Split your Macro: No Nested Loop here', 0
  ConditionalLoop:  'A Macro Loop cannot be inside a Conditional "If"', 0
  UnpairedMacroIf:  'Unpaired If in the Macro Declaration', 0
  BadCMIndice:      'Bad Conditional Indice (should be from 1 to 99)', 0
  BadConditionalmacro: 'Bad Conditional macro', 0
  ZZZZreserved: 'Symbols in "ZZZZZZZZ" form are reserved for Automatic Labels', 0
  BadSyntaxBeforeComma: "Bad syntax. Inside your macro and before the comma, you inserted Naked Operator Signs (+ - / ^ *).
  RosAsm will fix this automatically in order to you continue assembling your app.
  Please, consider fixing the syntax manually the next time you assemble your program, to avoid this warning.", 0
  TooManyExports: 'Too many Exports! must less then 65536', 0
  NoSpaceBeforeColon: 'No SPACE before COLON', 0
  NeedSpaceAfterColon: 'Need SPACE or new Line after COLON', 0
  BadExportOrdinal: '"ORD" reserved for Exports. Ordinal must be between 1 and 65535', 0
  SameExportOrdinal: 'Two Exported-Ordinals with same index found', 0
]


;;
  This Table is for holding the national languages, if some 'RosAsmStrings.xx' is
  found out, at launch time. 
;;

[StringsTable:
 1 ErrorMessageTitlePtr: ErrorMessageTitle
 2 EmptyStringPtr: EmptyString
 3 MissingFilePtr: MissingFile
 4 BusyFilePtr: BusyFile
 5 CreationFilePtr: CreationFile
 6 OpenFilePtr: OpenFile
 7 MissFilePtr: MissFile
 8 NoReadFilePtr: NoReadFile
 9 MissEquateValPtr: MissEquateVal
 10 EquateLabelPtr: EquateLabel
 11 TextEquatePtr: TextEquate
 12 MacParaPtr: MacPara
 13 MacNumberPtr: MacNumber
 14 BadMacroDirectionPtr: BadMacroDirection
 15 TooBigXPtr: TooBigX
 16 TooMuchParamPtr: TooMuchParam
 17 InfiniteLoopPtr: InfiniteLoop
 18 MacrosOverFlowPtr: MacrosOverFlow
 19 UnknownDataPtr: UnknownData
 20 UnknownSizePtr: UnknownSize
 21 Mem3DPtr: Mem3D
 22 NewHmemPtr: NewHmem
 23 PrefetchMemPtr: PrefetchMem
 24 DsizeForLabPtr: DsizeForLab
 25 UnknownPtr: Unknown
 26 BadSeparatorPtr: BadSeparator
 27 BinTypePtr: BinType
 28 HexTypePtr: HexType
 29 DezimalTypePtr: DezimalType
 30 OutOfRangePtr: OutOfRange
 31 DefEquatePtr: DefEquate
 32 DefReEquatePtr: DefReEquate
 33 TooMuchLabelsPtr: TooMuchLabels
 34 BadLabelPtr: BadLabel
 35 LabelDupPtr: LabelDup
 36 SymbolDupPtr: SymbolDup
 37 OrphanColonPtr: OrphanColon
 38 LocalDefLabelPtr: LocalDefLabel
 39 NoLocalCallPtr: NoLocalCall
 40 NotAnOpcodePtr: NotAnOpcode
 41 OverBytePtr: OverByte
 42 OverWordPtr: OverWord
 43 OverDwordPtr: OverDword
 44 LenSizePtr: LenSize
 45 BadRealPtr: BadReal
 46 OrphanPrefixPtr: OrphanPrefix
 47 DoubleSOPPtr: DoubleSOP
 48 NotSegmentPtr: NotSegment
 49 ParameterPtr: Parameter
 50 MissingParameterPtr: MissingParameter
 51 Double0Ptr: Double0
 52 MissingSignPtr: MissingSign
 53 MissingSeparatorPtr: MissingSeparator
 54 BadMacroLoopPtr: BadMacroLoop
 55 TooMuchPtr: TooMuch
 56 DBsizePtr: DBsize
 57 LockErrorPtr: LockError
 58 LockMemErrorPtr: LockMemError
 59 MissingOperandPtr: MissingOperand
 60 NotEnoughPtr: NotEnough
 61 MemMemPtr: MemMem
 62 ParenthesisPtr: Parenthesis
 63 ParaMacroPtr: ParaMacro
 64 MacroVariableIndicePtr: MacroVariableIndice
 65 NestedMacroVariablePtr: NestedMacroVariable
 66 MarkerBeforePtr: MarkerBefore
 67 MarkerAfterPtr: MarkerAfter
 68 ExpressionMemberPtr: ExpressionMember
 69 NoDataLocalLabelPtr:  NoDataLocalLabel
 70 RealNotationPtr: RealNotation
 71 ExpressionSignPtr: ExpressionSign
 72 DataLoopNumberPtr: DataLoopNumber
 73 SmallDataLoopPtr: SmallDataLoop
 74 BadLoopPtr: BadLoop
 75 VDataLoopNumberPtr: VDataLoopNumber
 76 TooMuchExpressionPtr: TooMuchExpression
 77 ExpressionNOTPtr: ExpressionNOT
 78 ExpressionSHRPtr: ExpressionSHR
 79 OpenTextPtr: OpenText
 80 OrphanBracketPtr: OrphanBracket
 81 UnexpectedCRLFPtr: UnexpectedCRLF
 82 MissingSeparator1Ptr: MissingSeparator1
 83 MissingSeparator2Ptr: MissingSeparator2
 84 UnPairedNestedBracketsPtr: UnPairedNestedBrackets
 85 NestedBracketsPtr: NestedBrackets
 86 PseudoLocalPtr: PseudoLocal
 87 DoubleSIBPtr: DoubleSIB
 88 EspIndexPtr: EspIndex
 89 ExpressionPtr: Expression
 90 ScaleValuePtr: ScaleValue
 91 ESPsibPtr: ESPsib
 92 DoubleIndexPtr: DoubleIndex
 93 DoubleLabelPtr: DoubleLabel
 94 UnknownParameterPtr: UnknownParameter
 95 EnterStackPtr: EnterStackPtr
 96 EnterLevelPtr: EnterLevel
 97 LeaInsteadPtr: LeaInstead
 98 MixTypePtr: MixType
 99 MissTypePtr: MissType
 100 OperandsTypesPtr: OperandsTypes
 101 BadAlignPtr: BadAlign
 102 OperandSizePtr: OperandSize
 103 FPregNotAssumedPtr: FPregNotAssumed
 104 LeaTypesPtr: LeaTypes
 105 LeaSizePtr: LeaSize
 106 EndingImmPtr: EndingImm
 107 XmarkerPtr: Xmarker
 108 OverFlowPtr: OverFlow
 109 UnknownSymbolPtr: UnknownSymbol
 110 WhatIsThisPtr: WhatIsThis
 111 UnAblePtr: UnAble
 112 NotYetSignPtr: NotYetSign
 113 NotYetMnemoPtr: NotYetMnemo
 114 ShortDisPtr: ShortDis
 115 TooLongOfPtr: TooLongOf
 116 NoPlainLabelForLoopPtr: NoPlainLabelForLoop
 117 LongLoopPtr: LongLoop
 118 NoPlainLabelForJECXPtr: NoPlainLabelForJECX
 119 LongDisPtr: LongDis
 120 NForbidenPtr: NForbiden
 121 MixedLenPtr: MixedLen
 122 BadLoopNumberPtr: BadLoopNumber
 123 VirtualDataPtr: VirtualData
 124 NestedLoopPtr: NestedLoop
 125 NeedByteSizePtr: NeedByteSize
 126 OnlyAccPtr: OnlyAcc
 127 TxtTooMuchPtr: TxtTooMuch
 128 GPregisterPtr: GPregister
 129 VERRwordPtr: VERRword
 130 WishEregPtr: WishEreg
 131 BadApiPtr: BadApi
 132 NoApiPtr: NoApi
 133 DllNotFoundPtr: DllNotFound
 134 ApiNotFoundPtr: ApiNotFound
 135 NoLocalPtr: NoLocal
 136 NoEntryPtr: NoEntry
 137 BadFPUcondPtr: BadFPUcond
 138 ST0wishedPtr: ST0wished
 139 STwishedPtr: STwished
 140 FADDPreg0Ptr: FADDPreg0
 141 FSUBPreg0Ptr: FSUBPreg0
 142 BadFpCharPtr: BadFpChar
 143 NoAdresseePtr: NoAdressee
 144 BadMemReleasePtr: BadMemRelease
 145 NotPEPtr: NotPE
 146 NotPeExePtr: NotPeExe
 147 BadWinEquPtr: BadWinEqu
 148 IdTooSmallPtr: IdTooSmall
 149 IdTooBigPtr: IdTooBig
 150 BadLibNamePtr: BadLibName
 151 DoubleFunctionPtr: DoubleFunction
 152 BadFunctionNamePtr: BadFunctionName
 153 BadOrdinalPtr: BadOrdinal
 154 NoAapiPtr: NoAapi
 155 MacVarOverFlowPtr: MacVarOverFlow
 156 MacVarNumberPtr: MacVarNumber
 157 MissingApiAPtr: MissingApiA
 158 BadPreParsePtr: BadPreParsePtr
 159 MissingCLASSnamePtr: MissingCLASSname
 160 NoParentClassPtr: NoParentClass
 161 SizeOfResourcePtr: SizeOfResource
 162 NumerAsSymbolPtr: NumerAsSymbol
 163 UnexpectedRegPtr: UnexpectedReg
 164 TextKillingPtr: TextKilling
 165 NestedMacroLoopPtr: NestedMacroLoop
 166 UnpairedMacroIfPtr: UnpairedMacroIf
 167 ConditionalLoopPtr: ConditionalLoop
 168 BadCMIndicePtr: BadCMIndice
 169 BadConditionalmacroPtr: BadConditionalmacro
 170 BadSyntaxBeforeCommaPtr: BadSyntaxBeforeComma
 171 TooManyExportsPtr: TooManyExports
 172 NoSpaceBeforeColonPtr: NoSpaceBeforeColon
 173 NeedSpaceAfterColonPtr: NeedSpaceAfterColon
 174 BadExportOrdinalPtr: BadExportOrdinal
 175 SameExportOrdinalPtr: SameExportOrdinal
____________________________________________________________________________________________
; Debugger strings

 3000 StrRunPtr: StrRun
 3001 StrStepIntoPtr: StrStepInto
 3002 StrStepOverPtr: StrStepOver
 3003 StrStepPtr: StrStep
 3004 StrReturnPtr: StrReturn
 3005 StrRetPtr: StrRet
 3006 StrTerminatePtr: StrTerminate
 3007 StrPausePtr: StrPause
 3008 StrHoldOnBpPtr: StrHoldOnBp
 3009 StrInstStepPtr: StrInstStep
 3010 StrSrcStepPtr: StrSrcStep
 3011 StrShowAllPtr: StrShowAll
 3012 StrFontPtr: StrFont
 3013 StrCPUInfoPtr: StrCPUInfo
 3014 StrFPUStatusPtr: StrFPUStatus
 3015 StrShowCodeAtPtr: StrShowCodeAt
 3016 StrAboutPtr: StrAbout
 3017 StrDbgHelpPtr: StrDbgHelp

 3040 StrDataFmtPtr: StrDataFmt

 3050 StrContinuePtr: StrContinue
 3051 StrBreakPtr: StrBreak
 3052 StrSettingsPtr: StrSettings
 3053 StrInformationPtr: StrInformation
 3054 StrHelpPtr: StrHelp

 3060 StrShowInMemInspPtr: StrShowInMemInsp
 3061 StrShowPInMemInspPtr: StrShowPInMemInsp
 3062 StrShowDeclPtr: StrShowDecl
 3063 StrBreakOnWPtr: StrBreakOnW
 3064 StrBreakOnRWPtr: StrBreakOnRW
 3065 StrSortByNamePtr: StrSortByName
 3066 StrSortByAddrPtr: StrSortByAddr

 3080 StrShowInvokePtr: StrShowInvoke
 3081 StrShowAllCallsPtr: StrShowAllCalls
 3082 StrHideModCallsPtr: StrHideModCalls
 3083 StrHideIMCallsPtr: StrHideIMCalls
 3084 StrShowLocalsPtr: StrShowLocals

 3100 FmtHexPtr:      FmtHex
 3101 FmtUDecPtr:     FmtUDec
 3102 FmtSDecPtr:     FmtSDec
 3103 FmtBinaryPtr:   FmtBinary
 3104 FmtFloatPtr:    FmtFloat
 3105 FmtPUBPtr:      FmtPUB
 3106 FmtPSBPtr:      FmtPSB
 3107 FmtPUWPtr:      FmtPUW
 3108 FmtPSWPtr:      FmtPSW
 3109 FmtPUDPtr:      FmtPUD
 3110 FmtPSDPtr:      FmtPSD
 3111 FmtPUQPtr:      FmtPUQ
 3112 FmtPSQPtr:      FmtPSQ
 3113 FmtPFPtr:       FmtPF
 3114 FmtPDPtr:       FmtPD
 3115 FmtHexAnsiPtr:  FmtHexAnsi
 3116 FmtHexDWPtr:    FmtHexDW
 3117 FmtHexWPtr:     FmtHexW
 3118 FmtFloatsPtr:   FmtFloats
 3119 FmtDoublesPtr:  FmtDoubles
 3120 FmtAsciiPtr:    FmtAscii
 3121 FmtHexCookedPtr: FmtHexCooked

 StringsTableEnd: StringsTablePointer: StringsTable]

[StringNamePath: ? #&MAXPATH]
[RosAsmStringsFileHandle: ? RosAsmStringsFilelenght: ? RosAsmStringsMemory: ?]
[StringsLanguage: '.en', 0 RosAsmStringsFiles: 'RosAsmStrings', 0]
[UnicodeStrings: &FALSE]

; Tag Dialog 4800

SetNationalLanguage:
    .If eax = 10
        mov D$StringsLanguage '.en'
    .Else_If eax = 11
        mov D$StringsLanguage '.fr'
    .Else_If eax = 12
        mov D$StringsLanguage '.br'
    .Else_If eax = 13
        mov D$StringsLanguage '.sp'
    .Else_If eax = 14
        mov D$StringsLanguage '.zh'
    .Else_If eax = 15
        mov D$StringsLanguage '.it'
    .Else_If eax = 16
        mov D$StringsLanguage '.de'
    .Else_If eax = 17
        mov D$StringsLanguage '.no'
    .Else_If eax = 18
        mov D$StringsLanguage '.ca'
    .Else_If eax = 201
        call GetNationalFont
    .End_If

    call OpenStringsFile
ret


OpenStringsFile:
    call GetRosAsmFilesPath
    mov esi RosAsmFilesPath, edi StringNamePath
    While B$esi <> 0 | movsb | End_While

    mov D$edi 'Lang', B$edi+4 '\' | add edi 5
    mov esi RosAsmStringsFiles
    While B$esi <> 0 | movsb | End_While | move D$edi D$StringsLanguage

    call 'KERNEL32.FindFirstFileA' StringNamePath, FindFile

    ...If eax <> &INVALID_HANDLE_VALUE

        call 'KERNEL32.FindClose' eax

        call 'KERNEL32.CreateFileA' StringNamePath, &GENERIC_READ,
                                    &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                    &FILE_ATTRIBUTE_NORMAL, 0
        mov D$RosAsmStringsFileHandle eax

        call 'KERNEL32.GetFileSize' eax, 0 | mov D$RosAsmStringsFilelenght eax
        add eax 10

        VirtualAlloc RosAsmStringsMemory eax | add D$RosAsmStringsMemory 10

        call 'KERNEL32.ReadFile' D$RosAsmStringsFileHandle, D$RosAsmStringsMemory,
                                 D$RosAsmStringsFilelenght, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$RosAsmStringsFileHandle

        mov esi D$RosAsmStringsMemory
        mov edx esi | add edx D$RosAsmStringsFilelenght

        If D$StringsLanguage = '.zh'
            mov B$UnicodeStrings &TRUE | call ParseNationalUstrings
        Else
            mov B$UnicodeStrings &FALSE | call ParseNationalStrings
        End_If

    ...Else
        mov D$StringsLanguage '.en'

    ...End_If
ret


ParseNationalUstrings:
    .While esi < edx
        ..If W$esi < '1'
            ;
        ..Else_If W$esi <= '9'
            cmp W$esi-8 '$' | jne L9>>
                cmp W$esi-6 '$' | jne L9>>
                    cmp W$esi-4 '$' | jne L9>>
                        cmp W$esi-2 '$' | jne L9>>

          ; Write the Strings Zero End Marks upon the '$':
            mov W$esi-12 0

          ; Compute the ID Number
L0:         mov ecx 0

L0:         lodsw
            cmp ax ' ' | je L7>
            cmp ax '9' | ja L5>
            cmp ax '0' | jb L5>
                sub ax '0'
                lea ecx D$ecx+ecx*4
                lea ecx D$eax+ecx*2
            jmp L0<

L5:         call 'USER32.MessageBoxA', 0, {'Bad National Strings File. English assumed', 0},
                                          {'National Strings', 0}, 0
            mov D$StringsLanguage '.en'
            ret

          ; esi point to the beginning of a String, and ecx is the ID Number.
          ; Write it into the 'StringsTable'. Allow missing Ordinal IDs:
L7:         mov ebx D$StringsTablePointer

            While D$ebx > ecx
                sub ebx 8 | On ebx < StringsTable, jmp L5<
            End_While
            While D$ebx < ecx
                add ebx 8 | On ebx >= StringsTableEnd, jmp L5<
            End_While
            mov D$ebx+4 esi | mov D$StringsTablePointer ebx
        ..End_If

L9:     add esi 2
    .End_While

  ; Write the Strings Zero End Marks for the last Strings:
    mov W$esi 0
ret


ParseNationalStrings:
    .While esi < edx
        ..If B$esi < '1'
            ;
        ..Else_If B$esi <= '9'
            cmp D$esi-4 '$$$$' | jne L9>>

          ; Write the Strings Zero End Marks:
            mov B$esi-6 0

          ; Compute the ID Number
L0:         mov ecx 0

L0:         lodsb
            cmp al ' ' | je L7>
            cmp al '9' | ja L5>
            cmp al '0' | jb L5>
                sub al '0'
                lea ecx D$ecx+ecx*4
                lea ecx D$eax+ecx*2
            jmp L0<

L5:         call 'USER32.MessageBoxA', 0, {'Bad National Strings File. English assumed', 0},
                                          {'National Strings', 0}, 0
            mov D$StringsLanguage '.en'
            ret

          ; esi point to the beginning of a String, and ecx is the ID Number.
          ; Write it into the 'StringsTable'. Allow missing Ordinal IDs:
L7:         mov ebx D$StringsTablePointer

            While D$ebx > ecx
                sub ebx 8 | On ebx < StringsTable, jmp L5<
            End_While
            While D$ebx < ecx
                add ebx 8 | On ebx >= StringsTableEnd, jmp L5<
            End_While
            mov D$ebx+4 esi | mov D$StringsTablePointer ebx
        ..End_If

L9:     inc esi
    .End_While

  ; Write the Strings Zero End Marks for the last Strings:

    mov B$esi 0
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Custom Strings Routines:
  
  See usage downward, at 'TestCustomString'.
;;
[CustomError
 If D$StringsLanguage = '.zh'
    call CopyUnicodeToTrash1 #1
    BuildTheUnicodeMessage #2>L
 Else
    call CopyToTrash1 #1
    BuildTheAsciiMessage #2>L
 End_If]

[BuildTheUnicodeMessage | call CustomUnicodeStringProc #1, #2, #3 | #+3
    mov esi D$Trash1Ptr, edi TrashString
    While W$esi > 0 | movsw | End_While | movsb]

[BuildTheAsciiMessage | call CustomStringProc #1, #2, #3 | #+3
    mov esi D$Trash1Ptr, edi TrashString
    While B$esi > 0 | movsb | End_While | movsw]

[CustomString | call CopyToTrash1 #1 | CustomString2 #2>L]

[CustomString2 | call CustomStringProc #1, #2, #3 | #+3
 mov esi D$Trash1Ptr, edi TrashString
 While B$esi > 0 | movsb | End_While | movsb]

Proc CopyToTrash1:
    Argument @Source
    Uses esi

        mov esi D@Source, edi Trash1
        While B$esi <> 0 | movsb | End_While | movsb

        mov D$Trash1Ptr Trash1, D$Trash2Ptr Trash2
EndP


Proc CopyUnicodeToTrash1:
    Argument @Source
    Uses esi

        mov esi D@Source, edi Trash1
        While W$esi <> 0 | movsw | End_While | movsw

        mov D$Trash1Ptr Trash1, D$Trash2Ptr Trash2
EndP


Proc CustomStringProc:
    Arguments @Xn, @Type, @Arg
    Uses esi, edi, ebx

        mov esi D$Trash1Ptr, edi D$Trash2Ptr

        mov eax D@Xn
        While W$esi <> ax
            movsb | On B$esi = 0, ExitP
        End_While

        If D@Type = 'Int'
            mov eax D@Arg | call WriteEax

        Else_If D@Type = 'Str'
            push esi
                mov esi D@Arg
                While B$esi <> 0 | movsb | End_While
            pop esi

        End_If

        add esi 2 | While B$esi <> 0 | movsb | End_While | mov B$edi 0

       Exchange D$Trash1Ptr D$Trash2Ptr
EndP


Proc CustomUnicodeStringProc:
    Arguments @Xn, @Type, @Arg
    Uses esi, edi, ebx

        mov esi D$Trash1Ptr, edi D$Trash2Ptr
;;
   '@Xn' stands is the '#1', '#2', '#3' thingies. We make it Unicode, that is,
   for example:
   
   '#1' (031, 023) >>> 0, 031, 0, 023 >>> '#', 0, '1', 0
;;
        mov eax D@Xn | movzx ebx ah
        shl ebx 16 | or eax ebx  | and eax 0FF00FF

        While D$esi <> eax
            movsw | On W$esi = 0, ExitP
        End_While

        If D@Type = 'Int'
            mov eax D@Arg | call WriteEaxUnicode

        Else_If D@Type = 'Str'
            push esi
                mov esi D@Arg
                While B$esi <> 0 | movsb | mov B$edi 0 | inc edi | End_While
            pop esi

        End_If

        add esi 4 | While W$esi <> 0 | movsw | End_While | mov W$edi 0

        Exchange D$Trash1Ptr D$Trash2Ptr
EndP


WriteEaxUnicode:
    mov ebx eax

L3: If ebx = 0
        mov W$edi '0' | add edi 2 | ret
    End_If

    push 0-1

L0: mov eax ebx | shr ebx 4 | and eax 0F

    mov al B$HexaTable+eax
    push eax
    cmp ebx 0 | ja L0<
    mov W$edi '0' | add edi 2
L0: pop eax | cmp eax 0-1 | je L9>
    mov B$edi al | inc edi | mov B$edi 0 | inc edi | jmp L0<
L9: ret


[TestString: 'Access violation at address #1. Tried to read from #2 address', 0]

TestCustomString:
    CustomString TestString, '#1', 'Int', 0112233,
                             '#2', 'Str', {'a very weird', 0}

    Showme TrashString
ret









































