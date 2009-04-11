TITLE Libs

____________________________________________________________________________________________
____________________________________________________________________________________________

;              FIRST PART: TRANSLATING THE WHOLE LIBARY INTO ROSASM SYNTAX

;              Authors: Guga and Betov. August/September 2005?
____________________________________________________________________________________________
____________________________________________________________________________________________

;;

  * Main function:
        LibScanner

  * Main Procedure (Dialog): 'ScanLibFile'
  
  * GUI Functions:

    LibScanCleanUp, LibScanDialog_EnableContinueMenu, SaveLibFileAs, LibScanDialog_ToggleToolbarText,
    LibScanDialog_HoverToolbarText, LibScanDialog_EnableContinuePrevTabMenu, LibScanDialog_EnableContinueNextTabMenu,
    OpenLibFile, LibScanDisplayFileSize, WriteObjectTypeinTitle, CoolControl_LVBeginSort, LibScanDialog_OnNotify,
    CoolControlTB_CreateImageList, CoolControlDlg_CreateTab, Tab1Proc, Tab2Proc, CoolControlWin_CreateToolbar,
    SetupListview, CoolControl_LVBeginSort, ListViewLibSort

  * Library Parsing Routines:
    
    LibCoffMarking, GetLibListing, GetOneCoffListing
    >>> 'OpenLibFile', 'LibSignatureCheck'
  
    >>> either 'GetLibListing' or 'GetOneCoffListing'
    
    'ParseLibObj' calls for 'GetCoffListing', in case of Obj Files in .Lib.
    
    CodeView Checking:
        WriteRawDataDebugS


  * Routines in charge of the IMAGE_FILE_MACHINE Equates:
  
    'WriteIMAGE_FILE_MACHINE', 'IsIMAGE_FILE_MACHINE'
    
    
  * 'WriteAuxiliarySymbolsRecords'

    ToDo:
    
    -   Build the steps for the DIS System. Need to identify all symbols, accordying to the DataTypes. I mean,
        we need to interpret them when they are found inside a CodeView file or inside regular Symbol Structures
        (Auxiliary or not). This is a hell of a work, but it is worthfull, because they contains full info.

    -   Analyze and parse OMF Files (Borland and Microsoft, Libs and Objs). They are not too hard to parse, but there are
        minor differences between M$ and Borland ones.

    -   Analyze and Parse all Delphi DCU files. They are not too hard to parse (Check Dede and DMFtoINT), but
        we must take care of the different delphi versions, because the structures varies sometimes.

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  File Jobs:
;;


[LibsFileFilter: B$ 'Supported Files (*.lib, *.a, *.obj, *.o, *.dcu, *.pdb, *.dbg)', 0  '*.lib;*.a;*.obj;*.o;*.dcu;*.pdb;*.dbg', 0
                 B$ 'Library Files (*.lib)', 0  '*.lib', 0
                 B$ 'Library Files MingWin32/Gcc (*.a)', 0  '*.a', 0
                 B$ 'Object Files (*.obj)', 0  '*.obj', 0
                 B$ 'Object Files MingWin32/Gcc (*.o)', 0  '*.o', 0
                 B$ 'Delphi and Kilyx Control Unit Files  (*.dcu)', 0  '*.dcu', 0
                 B$ 'Program DataBase Files (*.pdb)', 0  '*.pdb', 0
                 B$ 'Debugging Files  (*.dbg)', 0  '*.dbg', 0
                 B$ 'All Files', 0  '*.*', 0 0]

[CustomLibsFileFilter:  ? #&MAX_PATH]
[LibSaveFilter: ? #&MAX_PATH] [ChoosenLibFile: ? #&MAX_PATH]
[OpenLibFileTitle: B$ 'Choose a Lib file...' 0]

; OpenFile Structure for Libs:

[OPENLIB:
 @lStructSize: D$ Len
 @hwndOwner: D$ 0
 @hInstance: D$ 0
 @lpstrFilter: D$ LibsFileFilter
 @lpstrCustomFilter: D$ CustomLibsFileFilter
 @nMaxCustFilter: D$ 260
 @nFilterIndex: D$ 1
 @lpstrFile: D$ LibSaveFilter
 @nMaxFile: D$ 260
 @lpstrFileTitle: D$ ChoosenLibFile
 @nMaxFileTitle: D$ 260
 @lpstrInitialDir: D$ 0
 @lpstrTitle: D$ OpenLibFileTitle
 @Flags: D$ &OFN_EXPLORER__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
 @nFileOffset: W$ 0
 @nFileExtension: W$ 0
 @lpstrDefExt: D$ 0
 @lCustData: D$ 0
 @lpfnHook: D$ 0
 @lpTemplateName: D$ 0]
____________________________________________________________________________________________

[LibFileHandle: ?    LibFileLength: ?    LibFileMemory: ?   LibFileMemoryEnd: ?]

Proc OpenLibFile:
    Arguments @Adressee

    call 'KERNEL32.CreateFileA' LibSaveFilter, &GENERIC_READ,
                                &FILE_SHARE_READ, 0, &OPEN_EXISTING,
                                &FILE_ATTRIBUTE_NORMAL, 0
    mov D$LibFileHandle eax

    call 'KERNEL32.GetFileSize' eax, 0 | mov D$LibFileLength eax

    call LibScanDisplayFileSize D@Adressee

    If eax = 0
        call LibScanDialog_EnableContinueMenu &FALSE
        call 'USER32.MessageBoxA' 0, {B$ "Dumping process aborted !!!", D$ CRLF2, B$ "The file you are trying to load is empty (The size of the file is Zero).", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL
    Else
        add eax 10

        VirtualAlloc LibFileMemory, eax | add D$LibFileMemory 10

        call 'KERNEL32.ReadFile' D$LibFileHandle, D$LibFileMemory,
                                 D$LibFileLength, NumberOfReadBytes, 0

        call 'KERNEL32.CloseHandle' D$LibFileHandle

        mov eax D$LibFileMemory | add eax D$LibFileLength | mov D$LibFileMemoryEnd eax
    End_If
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Called from 'ScanLibFile' Dialog Proc:
  
      Main Procedures for the RosAsm syntax translation of a Library
;;
____________________________________________________________________________________________

; Object and Library Constants
[UNKNOWN_LIB_FILE 0]
[COFF_LIB_FILE 1]
[COFF_OBJ_FILE 2]
[DCU1_KILYX_OBJ_FILE 3]
[DCU2_KILYX_OBJ_FILE 4]
[DCU3_KILYX_OBJ_FILE 5]
[DCU2_OBJ_FILE 6]
[DCU3_OBJ_FILE 7]
[DCU4_OBJ_FILE 8]
[DCU5_OBJ_FILE 9]
[DCU6_OBJ_FILE 10]
[DCU7_OBJ_FILE 11]
[OMF_OBJ_FILE 12]
[PDB_OBJ_FILE 13]
[DBG_OBJ_FILE 14]

; Verification Buffer to hold the above constants
[ValidLib: D$ 0]

Proc LibSignatureCheck:
    pushad

  ; Valid Tag?
    mov esi D$LibFileMemory

    .If D$esi+2 = 'arch'
        ; Valid .lib
        mov D$ValidLib COFF_LIB_FILE

    .Else_If D$esi = 0F21F148C ; Borland Kylix 1.0 unit file (DCU). Ver=100
        mov D$ValidLib DCU1_KILYX_OBJ_FILE

    .Else_If D$esi = 0E1011DD ; Borland Kylix 2.0 unit file (DCU). Ver=101
        mov D$ValidLib DCU2_KILYX_OBJ_FILE

    .Else_If D$esi = 0E0001DD ; Borland Kylix 2.0 unit file (kind $00) (DCU). Ver=101
        mov D$ValidLib DCU2_KILYX_OBJ_FILE

    .Else_If D$esi = 0F1001DD ; Borland Kylix 3.0 unit file (DCU). Ver=102
        mov D$ValidLib DCU3_KILYX_OBJ_FILE

    .Else_If D$esi = 050505348 ; Borland Delphi 2.0 unit file (DCU). Ver=2
        mov D$ValidLib DCU2_OBJ_FILE

    .Else_If D$esi = 044518641 ; Borland Delphi 3.0 unit file (DCU). Ver=3
        mov D$ValidLib DCU3_OBJ_FILE

    .Else_If D$esi = 04768A6D8 ; Borland Delphi 4.0 unit file (DCU). Ver=4
        mov D$ValidLib DCU4_OBJ_FILE

    .Else_If D$esi = 0F21F148B ; Borland Delphi 5.0 unit file (DCU). Ver=5
        mov D$ValidLib DCU5_OBJ_FILE

    .Else_If D$esi = 0E0000DD ; Borland Delphi 6.0 unit file (DCU). Ver=6
        mov D$ValidLib DCU6_OBJ_FILE

    .Else_If D$esi = 0E8000DD ; Borland Delphi 6.0 unit file (kind $80) (DCU). Ver=6
        mov D$ValidLib DCU6_OBJ_FILE

    .Else_If D$esi = 0FF80FFFF ; Borland Delphi 7.0 (Free) (DCU). Ver=7
        mov D$ValidLib DCU7_OBJ_FILE

    .Else_If D$esi = 0FF0000DF ; Borland Delphi 7.0 unit file (DCU). Ver=7
        mov D$ValidLib DCU7_OBJ_FILE

    .Else_If D$esi = 0F0000DF ; Borland Delphi 7.0 unit file (DCU). Ver=7
        mov D$ValidLib DCU7_OBJ_FILE

    .Else_If_Or W$esi = &IMAGE_FILE_MACHINE_UNKNOWN, W$esi = &IMAGE_FILE_MACHINE_ALPHA,
                W$esi = &IMAGE_FILE_MACHINE_ALPHA64, W$esi = &IMAGE_FILE_MACHINE_ARM,
                W$esi = &IMAGE_FILE_MACHINE_AXP64, W$esi = &IMAGE_FILE_MACHINE_CEF,
                W$esi = &IMAGE_FILE_MACHINE_I386, W$esi = &IMAGE_FILE_MACHINE_I486,
                W$esi = &IMAGE_FILE_MACHINE_I586, W$esi = &IMAGE_FILE_MACHINE_IA64,
                W$esi = &IMAGE_FILE_MACHINE_M68K, W$esi = &IMAGE_FILE_MACHINE_MIPS16,
                W$esi = &IMAGE_FILE_MACHINE_MIPSFPU, W$esi = &IMAGE_FILE_MACHINE_MIPSFPU16,
                W$esi = &IMAGE_FILE_MACHINE_POWERPC, W$esi = &IMAGE_FILE_MACHINE_R10000,
                W$esi = &IMAGE_FILE_MACHINE_R3000, W$esi = &IMAGE_FILE_MACHINE_R4000,
                W$esi = &IMAGE_FILE_MACHINE_R6000, W$esi = &IMAGE_FILE_MACHINE_SH3,
                W$esi = &IMAGE_FILE_MACHINE_SH3E, W$esi = &IMAGE_FILE_MACHINE_SH4,
                W$esi = &IMAGE_FILE_MACHINE_SH5, W$esi = &IMAGE_FILE_MACHINE_SH3DSP,
                W$esi = &IMAGE_FILE_MACHINE_THUMB, W$esi = &IMAGE_FILE_MACHINE_WCEMIPSV2,
                W$esi = &IMAGE_FILE_MACHINE_AM33, W$esi = &IMAGE_FILE_MACHINE_AMD64,
                W$esi = &IMAGE_FILE_MACHINE_CEE, W$esi = &IMAGE_FILE_MACHINE_EBC,
                W$esi = &IMAGE_FILE_MACHINE_M32R, W$esi = &IMAGE_FILE_MACHINE_POWERPCFP,
                W$esi = &IMAGE_FILE_MACHINE_TRICORE

        ; Valid .obj, if here:
        mov D$ValidLib COFF_OBJ_FILE

    .Else_If_And B$esi = 080, W$esi+1 < 1024, W$esi+1 > 0 ; the W$esi+1 is the record lenght. The max size of the record len is 1024(The entire record, inclusind the magic signature), and it must not be 0
        ; Valid OMF .obj
        mov D$ValidLib OMF_OBJ_FILE

    .Else_If D$esi+40 = 0474A
        ; Valid Pdb file
        mov D$ValidLib PDB_OBJ_FILE

    .Else_If D$esi = 04944
        ; Possible dbg file found
        mov D$ValidLib DBG_OBJ_FILE

    .Else
;;
        cmp B$esi 0F0; LibHdr
        jnz @TestFileType2
        
            mov eax eax
        @TestFileType2: ; Object File Found

        cmp B$esi 080; THEADDR ; Object OMF Sig
        jz @TestFileType3
        cmp B$esi 082; LHEADDR ; Library OMF Sig (Microsoft)
        jnz @TestFileType4

        @TestFileType3:
            movzx eax B$esi+2
            inc esi
            movzx eax B$esi+1 ; The Record length is a word, but the Low order must be 0, because the lenght must be only 255 bytes long
            movzx ecx al
            
@TestFileType4: ; Not OMF
;;
        mov D$ValidLib UNKNOWN_LIB_FILE
    .End_If
    popad
EndP
____________________________________________________________________________________________

; All Identified Libs and Objects are identified by this function.

Proc ParseIdentifiedLibs:
    Arguments @Adressee
    pushad

    mov esi D$LibFileMemory

    .If D$ValidLib = COFF_LIB_FILE
        call WriteObjectTypeinTitle D@Adressee, {"Library COFF", 0}
        call CoffLibFound
        call GetLibListing D@Adressee, hLibReportEdit, hLibReportEditLength

    .Else_If D$ValidLib = COFF_OBJ_FILE
         call WriteObjectTypeinTitle D@Adressee, {"Object COFF", 0}
         call GetOneCoffListing D@Adressee, hLibReportEdit, hLibReportEditLength


    .Else_If D$ValidLib = DCU1_KILYX_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi Kilyx 1", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi Kilyx 1 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU2_KILYX_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi Kilyx 2", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi Kilyx2 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU3_KILYX_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi Kilyx 3", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi Kilyx3 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU2_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi 2", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi 2 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU3_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi 3", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi 3 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU4_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi 4", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi 4 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU5_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi 5", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi 5 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU6_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi 6", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi 6 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DCU7_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Delphi 7", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Delphi 7 (DCU) Signature was Found..", D$ CRLF2, B$ "Sorry, but delphi objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = OMF_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"OMF Object", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Object Module Format (OMF) Signature was Found..", D$ CRLF2, B$ "Sorry, but OMF objects are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = PDB_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Pdb File", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Program DataBase (Pdb) Signature was Found..", D$ CRLF2, B$ "Sorry, but PDB files are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .Else_If D$ValidLib = DBG_OBJ_FILE
        ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
        call LibScanDialog_EnableContinueMenu &FALSE
        call WriteObjectTypeinTitle D@Adressee, {"Dbg File", 0}
        call 'USER32.MessageBoxA' D@Adressee, {B$ "Degug Format (dbg) Signature was Found..", D$ CRLF2, B$ "Sorry, but dbg files are not handled by this dumper yet.", 0}, {'LibScan', 0},
                                  &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL

    .End_If
    popad
EndP
____________________________________________________________________________________________

Proc CoffLibFound:

  ; Tag:
    add esi 8
   ____________________________________________
  ; First Lib Header:
    If W$esi = '/ '
        push esi
            lea esi D$esi+48 | call GetMemberSize
        pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Second Optional Lib Header:
    If W$esi = '/ '
        push esi
            lea esi D$esi+48 | call GetMemberSize
        pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Third optional Lib Header:
    If W$esi = '//'
        push esi
            lea esi D$esi+48 | call GetMemberSize
        pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Coff Files:
L5: call InitCoffIndice
;;
  We overwrite the "UserID", at +28. We will use this Ordinal to build Synthetic
  Labels (in replacement of the real Names), to save from Duplications, when we
  recompie the output, with RosAsm, for testing the validity of the Rendering
  (Asm Source Form version).
;;
    While B$esi <> 0
        mov eax D$LibObjIndice, D$esi+LIB_USER_ID eax
        mov ax W$LibObjIndice+4, W$esi+LIB_USER_ID+4 ax

        call IncrementLibCoffIndice

        push esi
            lea esi D$esi+LIB_MEMBER_SIZE
            call GetMemberSize
        pop esi

        add esi COFF_HEADER_SIZE | add esi D$MemberSize
        On esi >= D$LibFileMemoryEnd, ExitP
        On B$esi = 0A, inc esi
    End_While

EndP

;;
Proc LibCoffMarking:

  ; Valid Tag?
    mov esi D$LibFileMemory

    .If D$esi+2 = 'arch'
        ; Valid .lib
        mov D$ValidLib COFF_LIB_FILE

    .Else
        call IsIMAGE_FILE_MACHINE D$esi

        If eax = &TRUE
          ; Valid .obj, if here:
            mov D$ValidLib COFF_OBJ_FILE | ExitP

        Else
            call 'USER32.MessageBoxA' 0, {B$ "The 'arch' Magic Signature Tag was not found.", D$ CRLF2, B$ "This is not a COFF Object File", 0}, {'LibScan', 0},
                                      &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL
            mov D$ValidLib UNKNOWN_LIB_FILE | ExitP
        End_If

    .End_If
   ____________________________________________
  ; Tag:
    add esi 8
   ____________________________________________
  ; First Lib Header:
    If W$esi = '/ '
        push esi
            lea esi D$esi+48 | call GetMemberSize
        pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Second Optional Lib Header:
    If W$esi = '/ '
        push esi
            lea esi D$esi+48 | call GetMemberSize
        pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Third optional Lib Header:
    If W$esi = '//'
        push esi
            lea esi D$esi+48 | call GetMemberSize
        pop esi
        add esi 03C | add esi eax | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Coff Files:
L5: call InitCoffIndice

;  We overwrite the "UserID", at +28. We will use this Ordinal to build Synthetic
 ; Labels (in replacement of the real Names), to save from Duplications, when we
  ;recompie the output, with RosAsm, for testing the validity of the Rendering
;  (Asm Source Form version).

    While B$esi <> 0
        mov eax D$LibObjIndice, D$esi+LIB_USER_ID eax
        mov ax W$LibObjIndice+4, W$esi+LIB_USER_ID+4 ax

        call IncrementLibCoffIndice

        push esi
            lea esi D$esi+LIB_MEMBER_SIZE
            call GetMemberSize
        pop esi

        add esi COFF_HEADER_SIZE | add esi D$MemberSize
        On esi >= D$LibFileMemoryEnd, ExitP
        On B$esi = 0A, inc esi
    End_While

EndP

;;

____________________________________________________________________________________________
;;
  To save from Duplications, the Ordinal Coff Indice are stored inside each Heasder
  upon the LIB_USER_ID.
  
  Before writing each Names, we call for 'CopyCoffIndice'.
;;

CopyCoffIndice:
    mov D$edi 'Obj_'
    mov eax D$esi+LIB_USER_ID, D$edi+4 eax,
        ax W$esi+LIB_USER_ID+4, W$edi+8 ax,
        D$edi+10 ' ; '
    add edi 13
ret
____________________________________________________________________________________________

;[@LibHexaMem: ?    @COFFnumber: ?    @Base: ?]
Proc GetLibListing:
    Argument @Handle, @OutPut, @OutputSize

    pushad

        On D$LookUpValidNameCharsTable = 0, call InitLookUpValidNameCharsTable

        ; The Parsed Data will be stored in edi
        mov edi D@OutPut

        mov ecx D$LibFileLength | shl ecx 6 | VirtualAlloc edi ecx

        call GetLongNamesBase

        mov esi D$LibFileMemory, edi D$edi

        ____________________________________________
        ; RosAsm Notice
        mov esi WriteNotice | While B$esi <> 0 | movsb | End_While
        ____________________________________________
        ; Tag:
        call WriteLibTag

        mov D$ARCHIVE_MEMBER_HEADER_Indice '0000', W$ARCHIVE_MEMBER_HEADER_Indice+4 '01'
       ____________________________________________
      ; First Lib Header:
        If W$esi = '/ '
            call Write_IMAGE_ARCHIVE_MEMBER_HEADER_1
          ; First Lib Member:
            push esi
                call WriteHeaderMember1
            pop esi
            add esi D$MemberSize | On B$esi = 0A, inc esi
        End_If
       ____________________________________________
      ; Second optional Lib Header:
        If W$esi = '/ '
            call WriteSecondLibHeaderComment
            call Write_IMAGE_ARCHIVE_MEMBER_HEADER_2
            push esi
                call WriteHeaderMember2
            pop esi
            add esi D$MemberSize | On B$esi = 0A, inc esi
        End_If
       ____________________________________________
      ; Third optional Lib Header:
        If W$esi = '//'

            call WriteThirdLibHeaderComment

            call GetLongNamesPointer
            call Write_IMAGE_ARCHIVE_MEMBER_HEADER_3
            push esi
                call WriteHeaderMember3
            pop esi
            add esi D$MemberSize | On B$esi = 0A, inc esi
        End_If
       ____________________________________________
      ; Coff Files:
        push esi
            mov esi CoffComment
            While B$esi <> 0 | movsb | End_While
        pop esi

        call InitCoffIndice

        sub D$LibFileMemoryEnd 20

        While esi < D$LibFileMemoryEnd
            call WriteCoffTitle
            call Write_Obj_IMAGE_ARCHIVE_MEMBER_HEADER
            call ParseLibObj
            ; To be used in the listview control
            call AddListviewItem, D$hList

            add esi D$MemberSize | On esi >= D$LibFileMemoryEnd, jmp L2>
            On B$esi = 0A, inc esi
            call IncrementLibCoffIndice

        End_While

L2:
        ; The Parsed Data, must be displayed in the edit box
        mov eax D@OutPut    ;   Initial Address of the parsed data
        mov eax D$eax       ;   eax is now at the start of the address

        call 'USER32.SetDlgItemTextA' D$hTabDlg1, IDC_LIB_SOURCE, eax

        ; Computed and store the outputed size of the Parsed Data displayed in the edit control
        mov eax D@OutputSize
        mov ecx D@OutPut
        mov ecx D$ecx
        sub edi ecx
        mov D$eax edi

    popad

EndP

_________________________________________________________________________________________________

[hLibReportEdit: ?];    @COFFnumber: ?    @Base: ?]
[hLibReportEditLength: D$ 0]

Proc GetOneCoffListing:
    Argument @Handle, @OutPut, @OutputSize

    pushad

    ; The Parsed Data will be stored in edi
    mov edi D@OutPut

    call InitCoffIndice

    mov ecx D$LibFileLength | shl ecx 6 | VirtualAlloc edi ecx

    mov esi D$LibFileMemory, edi D$edi

    ____________________________________________
    ; RosAsm Notice
    push esi
    mov esi WriteNotice | While B$esi <> 0 | movsb | End_While
    pop esi

    call ParseLibObj

    call ParseOpenedFilePath LibSaveFilter

    move D$MemberSize D$LibFileLength
    call AddListviewItem, D$hList

    ; The Parsed Data, must be displayed in the edit box
    mov eax D@OutPut    ;   Initial Address of the parsed data
    mov eax D$eax       ;   eax is now at the start of the address
    call 'USER32.SetDlgItemTextA' D$hTabDlg1 , IDC_LIB_SOURCE, eax; SHOW ALL Parsed Data in the 1st Tab dialog...

    ; Computed and store the outpued size of the Parsed Data displayed in the edit control
    mov eax D@OutputSize
    mov ecx D@OutPut
    mov ecx D$ecx
    sub edi ecx
    mov D$eax edi

    popad
EndP

_________________________________________________________________________________________________

; This function is to be used for displaying the object file name in the list view

; Built this way to solve really weird file namings paths. This is similar to ParseLibObjectSymbolName


Proc ParseOpenedFilePath:
    Arguments @StartAddress
    Local @TempEndAddr, @TempStartAddr, @StartExportObjAddr, @EndObjPathAddr, @StartObjExtensionAddr, @UseExportedLibrary

    pushad

    ; Always clear the loaded data before use.
    call ClearBuffer ObjectName, 256
    call ClearBuffer ObjExtensionStr, 16
    call ClearBuffer ObjPathStr, 256

    ; Initialize all the data
    mov D$ObjFileNameType 0
    mov D$UseObjPath 0
    mov D$UseObjExtension 0
    mov D@TempStartAddr 0
    mov D@TempEndAddr 0
    mov D@StartExportObjAddr 0
    mov D@UseExportedLibrary 0

    mov esi D@StartAddress

    While B$esi <> 0
        inc esi
    End_While
    mov D@TempEndAddr esi

    ; 1st Step. Now that we have our full string, we need to see if it have any path inside.
    ; We must start from the end to the beginning of the string to find the path chars.
    mov esi D@StartAddress
    mov ecx D@TempEndAddr

    .While esi <> ecx

        If_Or B$ecx = '/', B$ecx = '\'

            mov D@EndObjPathAddr ecx
            mov D$UseObjPath 1
            jmp L1>
        End_If

        dec ecx
    .End_While

   L1:

    ; 2nd Step. If we have a Path, we will copy it to the proper Buffer (ObjPathStr)

    .If D$UseObjPath = 1
        ; Here esi is already defined as the starting point. We don't need to change it now.
        mov ecx D@EndObjPathAddr
        mov edi ObjPathStr

        .While esi <> ecx
            movsb
            ;inc esi
        .End_While
        inc esi ; Bypass the last "/" or "\" char preceeding the object name.
        mov D@StartAddress esi ; Will points to the beginning of the object name only

    .End_If


    ; 3rd Step. Chekc the file name Type. If the len of the object name is equal or bigger then 16 bytes, it is a LongName.
    ; Otherwise it is short name.

    mov eax D@TempEndAddr
    sub eax D@StartAddress

    If eax <= 16
        mov D$ObjFileNameType 0
    Else
        mov D$ObjFileNameType 1
    End_If



    ; 4th Step. At this point we have only the name of the object and it's extension (if any).
    ; So we must find and copy the object extension.

    mov esi D@StartAddress
    mov ecx D@TempEndAddr
    mov edi ObjectName

    .Do

        .If B$esi = '.'
            mov D$UseObjExtension 1
            mov edi ObjExtensionStr
            inc esi ; Bypass the "." char
            While esi <> ecx
                movsb
            End_While
            jmp L2>
        .End_If

        movsb
    .Loop_Until esi = ecx

   L2:

    popad
EndP

_________________________________________________________________________________________________


[CoffListingTitle: "
; -------------------- Coff Listing: -------------------------

[IMAGE_FILE_HEADER_", 0]

[ObjMachineText: B$ 'Machine: W$ ', 0
 ObjNumberOfSectionsText: 'NumberOfSections: W$ ', 0
 ObjTimeDateStampText: 'TimeDateStamp: D$ ', 0
 ObjPointerToSymbolTableText: 'PointerToSymbolTable: D$ ', 0
 ObjPbjNumberOfSymbolsText: 'NumberOfSymbols: D$ ', 0
 ObjSizeOfOptionalHeaderText: 'SizeOfOptionalHeader: W$ ', 0
 ObjCharacteristicsText: 'Characteristics: W$ ', 0

SymbolTableText: 'ImgSym000001 - IMAGE_FILE_HEADER_', 0]



[SizeOfOptionalHeaderInObj: ?   ObjNumberOfSections: ?   FirstSectionPointer: ?]

GetCoffIMAGE_FILE_HEADER:
    push esi
        mov esi CoffListingTitle
        While B$esi <> 0 | movsb | End_While
        call WriteIndiceOnly
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi

    ; To be used in AddListviewItem
    push esi
        mov D$LvOffsetCOFF 0 ; always initialize at 0 1st, due to the several loopings for each object.
        sub esi D$LibFileMemory
        mov D$LvOffsetCOFF esi
    pop esi

    call WriteLibImportObjItem ObjMachineText, &FALSE | sub edi 2
        ; To be used in AddListviewItem
        mov W$CoffMachineType 0 ; always initialize at 0 1st, due to the several loopings for each object.
        move W$CoffMachineType W$esi
    call Write_IMAGE_FILE_MACHINE D$esi | mov W$edi CRLF | add edi 2

    add esi 2 | movzx eax W$esi | mov D$ObjNumberOfSections eax

    call WriteLibImportObjItem ObjNumberOfSectionsText, &TRUE

    add esi 2 | mov eax D$esi
    call WriteLibImportObjItem ObjTimeDateStampText, &TRUE
    call WriteLinkerMemberTimeDateStamp D$esi

    add esi 4 | mov eax D$esi
    mov D$CoffSymbolsPointer eax ; To be used in WriteImageSymbolTable
    call WriteLibImportObjItem ObjPointerToSymbolTableText, &FALSE | sub edi 2

    If eax <> 0
        call WriteLibImportObjItem SymbolTableText, &FALSE | sub edi 2
        call WriteIndiceOnly
        push esi
            mov esi {' ; Hex Value:  ', 0}
            While B$esi <> 0 | movsb | End_While
        pop esi
    End_If

    call Writeeax | mov W$edi CRLF | add edi 2 ; Write the Value of the SymbolTable Pointer in Hexadecimal string


    add esi 4 | mov eax D$esi
    mov D$CoffSymbolsNumber eax ; To be used in WriteImageSymbolTable
    mov D$ObjSymbolsNumber eax ; To be used in AddListviewItem
    call WriteLibImportObjItem ObjPbjNumberOfSymbolsText, &TRUE

    add esi 4 | movzx eax W$esi | mov D$SizeOfOptionalHeaderInObj eax
    call WriteLibImportObjItem ObjSizeOfOptionalHeaderText, &TRUE

    add esi 2 | movzx eax W$esi
    call WriteLibImportObjItem ObjCharacteristicsText, &TRUE | sub edi 2
    movzx eax W$esi
    If eax <> 0
        call WriteObjCharacteristics
    End_If

    mov W$edi CRLF
    mov B$edi+2 ']'
    mov D$edi+3 CRLF2 | add edi 7

    add esi 2
ret


[CoffListingIMAGE_OPTIONAL_HEADERTitle: B$ "[IMAGE_OPTIONAL_HEADER_", 0

 ImgOptHdr.Magic: W$'ImgOptHdr.Magic: W$ ', 0
 ImgOptHdr.MajorLinkerVersion: 'ImgOptHdr.MajorLinkerVersion: B$ ', 0
 ImgOptHdr.MinorLinkerVersion: 'ImgOptHdr.MinorLinkerVersion: B$ ', 0
 ImgOptHdr.SizeOfCode: 'ImgOptHdr.SizeOfCode: D$ ', 0
 ImgOptHdr.SizeOfInitializedData: 'ImgOptHdr.SizeOfInitializedData: D$ ', 0
 ImgOptHdr.SizeOfUninitializedData: 'ImgOptHdr.SizeOfUninitializedData: D$ ', 0
 ImgOptHdr.AddressOfEntryPoint: 'ImgOptHdr.AddressOfEntryPoint: D$ ', 0
 ImgOptHdr.BaseOfCode: 'ImgOptHdr.BaseOfCode: D$ ', 0
 ImgOptHdr.BaseOfData: 'ImgOptHdr.BaseOfData: D$ ', 0
 ImgOptHdr.ImageBase: 'ImgOptHdr.ImageBase: D$ ', 0
 ImgOptHdr.SectionAlignment: 'ImgOptHdr.SectionAlignment: D$ ', 0
 ImgOptHdr.FileAlignment: 'ImgOptHdr.FileAlignment: D$ ', 0
 ImgOptHdr.MajorOperatingSystemVersion: 'ImgOptHdr.MajorOperatingSystemVersion: W$ ', 0
 ImgOptHdr.MinorOperatingSystemVersion: 'ImgOptHdr.MinorOperatingSystemVersion: W$ ', 0
 ImgOptHdr.MajorImageVersion: 'ImgOptHdr.MajorImageVersion: W$ ', 0
 ImgOptHdr.MinorImageVersion: 'ImgOptHdr.MinorImageVersion: W$ ', 0
 ImgOptHdr.MajorSubsystemVersion: 'ImgOptHdr.MajorSubsystemVersion: W$ ', 0
 ImgOptHdr.MinorSubsystemVersion: 'ImgOptHdr.MinorSubsystemVersion: W$ ', 0
 ImgOptHdr.Win32VersionValue: 'ImgOptHdr.Win32VersionValue: D$ ', 0
 ImgOptHdr.SizeOfImage: 'ImgOptHdr.SizeOfImage: D$ ', 0
 ImgOptHdr.SizeOfHeaders: 'ImgOptHdr.SizeOfHeaders: D$ ', 0
 ImgOptHdr.CheckSum: 'ImgOptHdr.CheckSum: D$ ', 0
 ImgOptHdr.Subsystem: 'ImgOptHdr.Subsystem: W$ ', 0
 ImgOptHdr.DllCharacteristics: 'ImgOptHdr.DllCharacteristics: W$ ', 0
 ImgOptHdr.SizeOfStackReserve: 'ImgOptHdr.SizeOfStackReserve: D$ ', 0
 ImgOptHdr.SizeOfStackCommit: 'ImgOptHdr.SizeOfStackCommit: D$ ', 0
 ImgOptHdr.SizeOfHeapReserve: 'ImgOptHdr.SizeOfHeapReserve: D$ ', 0
 ImgOptHdr.SizeOfHeapCommit: 'ImgOptHdr.SizeOfHeapCommit: D$ ', 0
 ImgOptHdr.LoaderFlags: 'ImgOptHdr.LoaderFlags: D$ ', 0
 ImgOptHdr.NumberOfRvaAndSizes: 'ImgOptHdr.NumberOfRvaAndSizes: D$ ', 0

 ImgDataDir.Export: 'ImgDataDir.Export: D$ ', 0
 ImgDataDir.ExportSize: 'ImgDataDir.ExportSize: D$ ', 0
 ImgDataDir.Import: 'ImgDataDir.Import: D$ ', 0
 ImgDataDir.ImportSize: 'ImgDataDir.ImportSize: D$ ', 0
 ImgDataDir.Resource: 'ImgDataDir.Resource: D$ ', 0
 ImgDataDir.ResourceSize: 'ImgDataDir.ResourceSize: D$ ', 0
 ImgDataDir.Exception: 'ImgDataDir.Exception: D$ ', 0
 ImgDataDir.ExceptionSize: 'ImgDataDir.ExceptionSize: D$ ', 0
 ImgDataDir.Certificate: 'ImgDataDir.Certificate: D$ ', 0
 ImgDataDir.CertificateSize: 'ImgDataDir.CertificateSize: D$ ', 0
 ImgDataDir.Relocation: 'ImgDataDir.Relocation: D$ ', 0
 ImgDataDir.RelocationSize: 'ImgDataDir.RelocationSize: D$ ', 0
 ImgDataDir.Debug: 'ImgDataDir.Debug: DebugDir: D$ ', 0
 ImgDataDir.DebugSize: 'ImgDataDir.DebugSize: D$ ', 0
 ImgDataDir.Architecture: 'ImgDataDir.Architecture: D$ ', 0
 ImgDataDir.ArchitectureSize: 'ImgDataDir.ArchitectureSize: D$ ', 0
 ImgDataDir.GPReg: 'ImgDataDir.GPReg: D$ ', 0
 ImgDataDir.GPRegSize: 'ImgDataDir.GPRegSize: D$ ', 0
 ImgDataDir.Thread: 'ImgDataDir.Thread: D$ ', 0
 ImgDataDir.ThreadSize: 'ImgDataDir.ThreadSize: D$ ', 0
 ImgDataDir.ConfigTable: 'ImgDataDir.ConfigTable: D$ ', 0
 ImgDataDir.ConfigTableSize: 'ImgDataDir.ConfigTableSize: D$ ', 0
 ImgDataDir.BoundIAT: 'ImgDataDir.BoundIAT: D$ ', 0
 ImgDataDir.BoundIATSize: 'ImgDataDir.BoundIATSize: D$ ', 0
 ImgDataDir.IAT: 'ImgDataDir.IAT: D$ ', 0
 ImgDataDir.IATSize: 'ImgDataDir.IATSize: D$ ', 0
 ImgDataDir.DelayID: 'ImgDataDir.DelayID: D$ ', 0
 ImgDataDir.DelayIDSize: 'ImgDataDir.DelayIDSize: D$ ', 0
 ImgDataDir.COM: 'ImgDataDir.COM: D$ ', 0
 ImgDataDir.COMSize: 'ImgDataDir.COMSize: D$ ', 0
 ImgDataDir.Reserved: 'ImgDataDir.Reserved: D$ ', 0
 ImgDataDir.ReservedSize: 'ImgDataDir.ReservedSize: D$ ', 0]

[IMAGE_DATA_DIRECTORY_Title: B$ '; ---- IMAGE_DATA_DIRECTORY ----', 0]


Write_IMAGE_DATA_DIRECTORY_Title:
    mov W$edi CRLF | add edi 2

    push esi
        zCopy IMAGE_DATA_DIRECTORY_Title
    pop esi

    mov D$edi CRLF2 | add edi 4
ret


WriteObjMagic:
    push esi
        If eax = &IMAGE_NT_OPTIONAL_HDR32_MAGIC
            zCopy {' ; &IMAGE_NT_OPTIONAL_HDR32_MAGIC', 0}
        Else_If eax = &IMAGE_NT_OPTIONAL_HDR64_MAGIC
            zCopy {' ; &IMAGE_NT_OPTIONAL_HDR64_MAGIC', 0}
        Else
            zCopy {' ; Likely corrupted unknown Magic Value', 0}
        End_If
    pop esi

    mov W$edi CRLF | add edi 2
ret


WriteObjSubSystem:
    push esi
        .If eax = &IMAGE_SUBSYSTEM_UNKNOWN
            zCopy {' ; &IMAGE_SUBSYSTEM_UNKNOWN', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_NATIVE
            zCopy {' ; &IMAGE_SUBSYSTEM_NATIVE', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_WINDOWS_GUI
            zCopy {' ; &IMAGE_SUBSYSTEM_WINDOWS_GUI', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_WINDOWS_CUI
            zCopy {' ; &IMAGE_SUBSYSTEM_WINDOWS_CUI', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_OS2_CUI
            zCopy {' ; &IMAGE_SUBSYSTEM_OS2_CUI', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_POSIX_CUI
            zCopy {' ; &IMAGE_SUBSYSTEM_POSIX_CUI', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
            zCopy {' ; &IMAGE_SUBSYSTEM_WINDOWS_CE_GUI', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_EFI_APPLICATION
            zCopy {' ; &IMAGE_SUBSYSTEM_EFI_APPLICATION', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
            zCopy {' ; &IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
            zCopy {' ; &IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER', 0}
        .Else_If eax = &IMAGE_SUBSYSTEM_NATIVE_WINDOWS
            zCopy {' ; &IMAGE_SUBSYSTEM_NATIVE_WINDOWS', 0}
        .Else
            zCopy {' ; Likely corrupted unknown SubSystem Record', 0}
        .End_If
    pop esi

    mov W$edi CRLF | add edi 2
ret


WriteDllCharacteristics:
    .If eax <> 0
        push esi
            mov D$edi ' ; ' | add edi 3

            test eax &IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_INIT | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_INIT__', 0}

L1:         test eax &IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_TERM | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_TERM__', 0}

L1:         test eax &IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_INIT | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_INIT__', 0}

L1:         test eax &IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_TERM | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_TERM__', 0}

L1:         test eax &IMAGE_DLLCHARACTERISTICS_NO_BIND | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_NO_BIND__', 0}

L1:         test eax &IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE__', 0}

L1:         test eax &IMAGE_DLLCHARACTERISTICS_WDM_DRIVER | jz L1>
                zCopy {'&IMAGE_DLLCHARACTERISTICS_WDM_DRIVER__', 0}

L1:         While B$edi = '_' | dec edi | End_While
        pop esi
    .End_If

    mov W$edi CRLF | add edi 2
ret


GetCoffIMAGE_OPTIONAL_HEADER:
    push esi
        mov esi CoffListingIMAGE_OPTIONAL_HEADERTitle
        While B$esi <> 0 | movsb | End_While
        call WriteIndiceOnly
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi

    movzx eax W$esi
    push eax
    call WriteLibImportObjItem ImgOptHdr.Magic, &TRUE | sub edi 2
    pop eax
    call WriteObjMagic

    add esi 2 | movzx eax B$esi
    call WriteLibImportObjItem ImgOptHdr.MajorLinkerVersion, &TRUE

    inc esi | movzx eax B$esi
    call WriteLibImportObjItem ImgOptHdr.MinorLinkerVersion, &TRUE

    inc esi
    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfCode, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfInitializedData, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfUninitializedData, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.AddressOfEntryPoint, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.BaseOfCode, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.BaseOfData, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.ImageBase, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SectionAlignment, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.FileAlignment, &TRUE

    lodsw | and eax 0FFFF
    call WriteLibImportObjItem ImgOptHdr.MajorOperatingSystemVersion, &TRUE

    lodsw | and eax 0FFFF
    call WriteLibImportObjItem ImgOptHdr.MinorOperatingSystemVersion, &TRUE

    lodsw | and eax 0FFFF
    call WriteLibImportObjItem ImgOptHdr.MajorImageVersion, &TRUE

    lodsw | and eax 0FFFF
    call WriteLibImportObjItem ImgOptHdr.MinorImageVersion, &TRUE

    lodsw | and eax 0FFFF
    call WriteLibImportObjItem ImgOptHdr.MajorSubsystemVersion, &TRUE

    lodsw | and eax 0FFFF
    call WriteLibImportObjItem ImgOptHdr.MinorSubsystemVersion, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.Win32VersionValue, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfImage, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfHeaders, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.CheckSum, &TRUE

    lodsw | and eax 0FFFF
    push eax
    call WriteLibImportObjItem ImgOptHdr.Subsystem, &TRUE | sub edi 2
    pop eax
    call WriteObjSubSystem

    lodsw | and eax 0FFFF
    push eax
    call WriteLibImportObjItem ImgOptHdr.DllCharacteristics, &TRUE | sub edi 2
    pop eax
    call WriteDllCharacteristics

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfStackReserve, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfStackCommit, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfHeapReserve, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.SizeOfHeapCommit, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.LoaderFlags, &TRUE

    lodsd | call WriteLibImportObjItem ImgOptHdr.NumberOfRvaAndSizes, &TRUE


    call Write_IMAGE_DATA_DIRECTORY_Title


    lodsd | call WriteLibImportObjItem ImgDataDir.Export, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ExportSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Import, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ImportSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Resource, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ResourceSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Exception, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ExceptionSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Certificate, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.CertificateSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Relocation, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.RelocationSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Debug, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.DebugSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Architecture, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ArchitectureSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.GPReg, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.GPRegSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Thread, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ThreadSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ConfigTable, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ConfigTableSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.BoundIAT, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.BoundIATSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.IAT, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.IATSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.DelayID, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.DelayIDSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.COM, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.COMSize, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.Reserved, &TRUE

    lodsd | call WriteLibImportObjItem ImgDataDir.ReservedSize, &TRUE

    sub edi 2 | mov B$edi ']', D$edi+1 CRLF2 | add edi 5

ret
____________________________________________________________________________________________


[CoffSectionHeaderTitle: "
;;
_____________________________________________________

The Section Header
_____________________________________________________
_____________________________________________________

IMAGE_SECTION_HEADER Structure

;;

", 0]

[SectionHeaderNumber: '001', 0]

InitSectionHeaderNumber:
    mov D$SectionHeaderNumber '001'
ret



IncrementSectionHeaderNumber:

    push ebx
        lea ebx D$SectionHeaderNumber+2

        inc B$ebx
        While B$ebx > '9'
            mov B$ebx '0' | dec ebx | inc B$ebx
        End_While
    pop ebx
ret

[CoffSectionHeaderName: 'Name1', 0
 CoffSectionHeaderVirtualSize: 'MiscVirtualSize', 0
 CoffSectionHeaderRVA: 'VirtualAddress', 0
 CoffSectionHeaderSize: 'SizeOfRawData', 0
 CoffSectionHeaderPointerToData: 'PointerToRawData', 0
 CoffSectionHeaderPointerToReloc: 'PointerToRelocations', 0
 CoffSectionHeaderPointerToLinesNumbers: 'PointerToLinenumbers', 0
 CoffSectionHeaderNumberOfRelocations: 'NumberOfRelocations', 0
 CoffSectionHeaderNumberOfLinesNumbers: 'NumberOfLinenumbers', 0
 CoffSectionHeaderCharacteristics: 'Characteristics', 0]

Proc WriteObjSectionHeaderItem:
    Argument @Text1, @Text2

        push esi
            call WriteObjIndice
            zCopy {'ImgSecHdr', 0}
            zCopy SectionHeaderNumber
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text2, al B$eax+2

        ...If al = 'B'
            mov edx esi | add edx 8
            mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:         mov B$edi "'" | inc edi
            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ...Else_If al = 'W'
            lodsw | and eax 0FFFF | call WriteEax

        ...Else

            ..If D$esi <> 0
                mov eax D@Text1

                .If D$eax = 'Size' ; from "SizeOfRawData" string

                    call WriteSectionHeaderRawSizeDiffLabel
                    push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                .Else_If D$eax+8 = 'oRaw'; From "PointerToRawData" string
                    ;mov eax D$esi-4
                    If D$esi-4 = 0
                        mov D$esi 0
                    Else

                        call WriteSectionHeaderPointerToDataDiffLabel
                        push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
                    End_If
                .Else_If D$eax+9 = 'Relo'; From "PointerToRelocations" string
                    call InitSectionRelocNumber
                    ;movzx eax W$esi+4+4
                    ; Fix the value at esi when the Reloc Number is 0, but it is pointing somewhere
                    If W$esi+4+4 = 0
                        mov D$esi 0
                    Else
                        call WriteSectionHeaderPointerToRelocDiffLabel
                        push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
                    End_If
                .Else_If D$eax+9 = 'Line'; From "PointerToLinenumbers" string
                    call InitSectionLineNumber
                    ;movzx eax W$esi+4+2
                    If W$esi+4+2 = 0
                        mov D$esi 0
                    Else
                        call WriteSectionHeaderPointerToLineNumberDiffLabel
                        push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
                    End_If
;;
                Else_If D$eax = 'Misc'; From "MiscVirtualSize" string

                    call WriteSectionHeaderVirtualSizeDiffLabel
                    push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
;;

                .End_If

            ..End_If

            lodsd | call WriteEax

        ...End_If

        mov W$edi CRLF | add edi 2
EndP


WriteCharacteristicsEquates:

    If eax <> 0
        mov D$edi ' ; ' | add edi 3
    End_If

    test eax &IMAGE_SCN_TYPE_REG | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_REG__', 0}
L1: test eax &IMAGE_SCN_TYPE_DSECT | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_DSECT__', 0}
L1: test eax &IMAGE_SCN_TYPE_NOLOAD | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_NOLOAD__', 0}
L1: test eax &IMAGE_SCN_TYPE_GROUP | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_GROUP__', 0}
L1: test eax &IMAGE_SCN_TYPE_NO_PAD | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_NO_PAD__', 0}
L1: test eax &IMAGE_SCN_TYPE_COPY | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_COPY__', 0}
L1: test eax &IMAGE_SCN_CNT_CODE | jz L1>
        zCopy {'&IMAGE_SCN_CNT_CODE__', 0}
L1: test eax &IMAGE_SCN_CNT_INITIALIZED_DATA | jz L1>
        zCopy {'&IMAGE_SCN_CNT_INITIALIZED_DATA__', 0}
L1: test eax &IMAGE_SCN_CNT_UNINITIALIZED_DATA | jz L1>
        zCopy {'&IMAGE_SCN_CNT_UNINITIALIZED_DATA__', 0}
L1: test eax &IMAGE_SCN_LNK_OTHER | jz L1>
        zCopy {'&IMAGE_SCN_LNK_OTHER__', 0}
L1: test eax &IMAGE_SCN_LNK_INFO | jz L1>
        zCopy {'&IMAGE_SCN_LNK_INFO__', 0}
L1: test eax &IMAGE_SCN_TYPE_OVER | jz L1>
        zCopy {'&IMAGE_SCN_TYPE_OVER__', 0}
L1: test eax &IMAGE_SCN_LNK_REMOVE | jz L1>
        zCopy {'&IMAGE_SCN_LNK_REMOVE__', 0}
L1: test eax &IMAGE_SCN_LNK_COMDAT | jz L1>
        zCopy {'&IMAGE_SCN_LNK_COMDAT__', 0}
L1: test eax &IMAGE_SCN_MEM_FARDATA | jz L1>
        zCopy {'&IMAGE_SCN_MEM_FARDATA__', 0}
L1: test eax &IMAGE_SCN_MEM_PURGEABLE | jz L1>
        zCopy {'&IMAGE_SCN_MEM_PURGEABLE__', 0}
L1: test eax &IMAGE_SCN_MEM_16BIT | jz L1>
        zCopy {'&IMAGE_SCN_MEM_16BIT__', 0}
L1: test eax &IMAGE_SCN_MEM_LOCKED | jz L1>
        zCopy {'&IMAGE_SCN_MEM_LOCKED__', 0}
L1: test eax &IMAGE_SCN_MEM_PRELOAD | jz L1>
        zCopy {'&IMAGE_SCN_MEM_PRELOAD__', 0}

L1: mov ebx eax | and ebx 0FF_0000

    .If ebx = &IMAGE_SCN_ALIGN_1BYTES                ; 010_0000
        zCopy {'&IMAGE_SCN_ALIGN_1BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_2BYTES           ; 020_0000
        zCopy {'&IMAGE_SCN_ALIGN_2BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_4BYTES           ; 030_0000
        zCopy {'&IMAGE_SCN_ALIGN_4BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_8BYTES           ; 040_0000
        zCopy {'&IMAGE_SCN_ALIGN_8BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_16BYTES          ; 050_0000
        zCopy {'&IMAGE_SCN_ALIGN_16BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_32BYTES          ; 060_0000
        zCopy {'&IMAGE_SCN_ALIGN_32BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_64BYTES          ; 070_0000
        zCopy {'&IMAGE_SCN_ALIGN_64BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_128BYTES         ; 080_0000
        zCopy {'&IMAGE_SCN_ALIGN_128BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_256BYTES         ; 090_0000
        zCopy {'&IMAGE_SCN_ALIGN_256BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_512BYTES         ; 0A0_0000
        zCopy {'&IMAGE_SCN_ALIGN_512BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_1024BYTES        ; 0B0_0000
        zCopy {'&IMAGE_SCN_ALIGN_1024BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_2048BYTES        ; 0C0_0000
        zCopy {'&IMAGE_SCN_ALIGN_2048BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_4096BYTES        ; 0D0_0000
        zCopy {'&IMAGE_SCN_ALIGN_4096BYTES__', 0}
    .Else_If ebx = &IMAGE_SCN_ALIGN_8192BYTES        ; 0E0_0000
        zCopy {'&IMAGE_SCN_ALIGN_8192BYTES__', 0}
    .End_If

L1: test eax &IMAGE_SCN_LNK_NRELOC_OVFL | jz L1>
        zCopy {'&IMAGE_SCN_LNK_NRELOC_OVFL__', 0}
L1: test eax &IMAGE_SCN_MEM_DISCARDABLE | jz L1>
        zCopy {'&IMAGE_SCN_MEM_DISCARDABLE__', 0}
L1: test eax &IMAGE_SCN_MEM_NOT_CACHED | jz L1>
        zCopy {'&IMAGE_SCN_MEM_NOT_CACHED__', 0}
L1: test eax &IMAGE_SCN_MEM_NOT_PAGED | jz L1>
        zCopy {'&IMAGE_SCN_MEM_NOT_PAGED__', 0}
L1: test eax &IMAGE_SCN_MEM_SHARED | jz L1>
        zCopy {'&IMAGE_SCN_MEM_SHARED__', 0}
L1: test eax &IMAGE_SCN_MEM_EXECUTE | jz L1>
        zCopy {'&IMAGE_SCN_MEM_EXECUTE__', 0}
L1: test eax &IMAGE_SCN_MEM_READ | jz L1>
        zCopy {'&IMAGE_SCN_MEM_READ__', 0}
L1: test eax &IMAGE_SCN_MEM_WRITE | jz L1>
        zCopy {'&IMAGE_SCN_MEM_WRITE__', 0}
L1:

    While B$edi-1 = '_' | dec edi | End_While
ret



____________________________________________________________________________________________

[MemberName1Offset: ?]

Proc GetName1Offset:
    pushad
        ; edi  is used as a Byte Counter
        mov edi 8 ; Size of the Name1 member
        inc esi ; ByPass the 1st '/'
        mov ecx 10, ebx 0, eax 0

        Do
            On B$esi = 0, jmp L0> ; If we reach 0 jmp over
            On B$esi = ' ', jmp L0> ; If we reach ' ' jmp over
            mul ecx
            push eax
                lodsb | sub al '0' | mov bl al
            pop eax
            add eax ebx
            dec edi
        Loop_Until edi = 0

L0:

        mov D$MemberName1Offset eax

    popad
EndP

____________________________________________________________________________________________



Proc WriteSectionNameStringTablePointerComment:
    Uses esi, eax, edx


    ; Initialize the String Record Counter
    call InitStringTableRecord

    ; eax is pointing to the Offset of the String on the String Table
    mov eax D$MemberName1Offset
    mov edx D$PointerToStringTable
    mov esi edx ; esi is pointing to the Beginning of the String Table
    add edx eax ; edx will be using as a counter of the total amount of strings untill we reach the String where we are.
    add esi 4   ; Bypass the String Table Size. and Point to the beginning of the 1st String

    .Do

        While B$esi <> 0 | inc esi | End_While | inc esi | On B$esi = 0, mov edx 0

        call IncrementStringTableRecord

    .Loop_Until esi >= edx


        push esi
            call WriteObjIndice
            zCopy {'StringData', 0}
            zCopy StringTableRecord
        pop esi


    ; Restore the Value of StringTableRecord

    call InitStringTableRecord


EndP

___________________________________________

[SectionNameIndirectMessage: B$ "________________________________________________________________________

; Section Name indirectly addressed in Name1 member.
; The name of the Section is: ", 0]


Proc WriteIndirectSectionName:
    Uses esi

        call GetName1Offset ; convert the Offset to hexadecimal
        call GetStringTablePointer

        ;zCopy SymbolTableTitle
        mov esi D$CoffSectionBase
        add esi D$CoffSymbolsPointer

        push esi
            zCopy SectionNameIndirectMessage
        pop esi

            mov ecx D$PointerToStringTable

        push esi
            add ecx D$MemberName1Offset | mov esi ecx
            While B$esi <> 0 | movsb | End_While
        pop esi

        push esi
            zCopy {W$ CRLF, B$ "; Referenced in String Table data at: ", 0}
            call WriteSectionNameStringTablePointerComment
            zCopy {W$ CRLF, B$ "________________________________________________________________________", D$ CRLF2 0}
        pop esi

EndP

___________________________________________________

;;

Section Name    Content                     Characteristics
.arch           Alpha architecture info     &IMAGE_SCN_MEM_READ__&IMAGE_SCN_CNT_INITIALIZED_DATA (...) &IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_DISCARDABLE
.bss            Uninitialized data          &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.data           Initialized data            &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.edata          Export tables               &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
.idata          Import tables               &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.pdata          Exception information       &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
.rdata          Read-only initialized data  &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ
.reloc          Image relocations           &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_DISCARDABLE
.rsrc           Resource directory          &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.text           Executable code             &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_MEM_EXECUTE (...) &IMAGE_SCN_MEM_READ
.tls            Thread-local storage        &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ (...) &IMAGE_SCN_MEM_WRITE
.xdata          Exception information       &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ


&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE = 0E0000040

&IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE = 0E0000080

Some common Names and their Flag values:

.bss C0300080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0301080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0400080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0401080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.bss C0500080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.CRT C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0400040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0401040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.data C0500040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.debug 42000048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42100048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42101040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42101048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.debug 42400048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_DISCARDABLE__&IMAGE_SCN_MEM_READ
.drectve 100A00 &IMAGE_SCN_LNK_INFO__&IMAGE_SCN_LNK_REMOVE__&IMAGE_SCN_ALIGN_1BYTES
.idata C0000048 &IMAGE_SCN_TYPE_NO_PAD__&IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0100040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0200040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_2BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0201040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_2BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.idata C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.orpc 60501020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.rdata 40300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ
.rdata 40301040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ
.rdata 40400040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ
.rdata 40401040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ
.rdata 40500040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_READ
.text 400020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_ALIGN_8BYTES
.text 60000020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60101020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_1BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60201020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_2BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60300020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60301020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60500020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.text 60501020 &IMAGE_SCN_CNT_CODE__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_16BYTES__&IMAGE_SCN_MEM_EXECUTE__&IMAGE_SCN_MEM_READ
.tls C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_ALIGN_4BYTES__&IMAGE_SCN_MEM_READ__&IMAGE_SCN_MEM_WRITE
.xdata 40401040 &IMAGE_SCN_CNT_INITIALIZED_DATA__&IMAGE_SCN_LNK_COMDAT__&IMAGE_SCN_ALIGN_8BYTES__&IMAGE_SCN_MEM_READ


__________________________________

Detailed sections found on several Libraries and executables files

.text 400020 &IMAGE_SCN_CNT_CODE   &IMAGE_SCN_ALIGN_8BYTES  
.orpc 60501020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60000020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ   
.text 60101020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_1BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60201020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_2BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60300020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES  
.text 60301020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT 
.text 60500020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES  
.text 60501020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES &IMAGE_SCN_LNK_COMDAT 

; Data Flags

.CRT C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES 
.data C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES 
.data C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT
.data C0400040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES 
.data C0401040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT
.data C0500040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_16BYTES 

.edata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.rdata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.rsrc 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.rdata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.INIT 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_DISCARDABLE
.reloc 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_DISCARDABLE
.rdata 050000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_SHARED
.reloc 050000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_SHARED
.rsrc 050000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  &IMAGE_SCN_MEM_SHARED
BSS 0C0000000  &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.tls 0C0000000  &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.idata 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
DATA 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.data 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.shared 0D0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_MEM_SHARED
.idata 040000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ  
.bss 0C0000080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 
.CRT 0C0000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE 

; Really weird sections at C:\WINNT\Driver Cache\i386 
; It is worthfull to analyze to fix them like I was doing for the Dumped files. (RosAsm crashes, for example at ftdisk.sys)

PAGE 060000020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ  
INIT 0E2000020 &IMAGE_SCN_CNT_CODE &IMAGE_SCN_MEM_EXECUTE &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_MEM_DISCARDABLE

.rdata 048000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_NOT_PAGED  
.data 0C8000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_NOT_PAGED &IMAGE_SCN_MEM_WRITE 
.rsrc 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  
.reloc 042000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  
.rsrc 048000040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_NOT_PAGED  


; Debug Data
; Name Types .debug$S; .debug$F; .debug$T; .debug$P

.debug$S 42000048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  
.debug$F 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug$T 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug$P 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug$T 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42101040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug$F 42101048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug$S 42101048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug$F 42400048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_8BYTES

.debug 42100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE   &IMAGE_SCN_ALIGN_1BYTES
.debug 42101040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE  &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug 42000048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  
.debug 42100048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_1BYTES
.debug 42101048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD &IMAGE_SCN_LNK_COMDAT &IMAGE_SCN_ALIGN_1BYTES
.debug 42400048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_DISCARDABLE &IMAGE_SCN_TYPE_NO_PAD  &IMAGE_SCN_ALIGN_8BYTES

.idata C0100040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_1BYTES  
.idata C0200040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_2BYTES  
.idata C0201040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_2BYTES  &IMAGE_SCN_LNK_COMDAT
.idata C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES  
.idata C0301040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES  &IMAGE_SCN_LNK_COMDAT
.idata C0000048 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE  &IMAGE_SCN_TYPE_NO_PAD 

.rdata 040300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES 
.rdata 040301040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT
.rdata 040400040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_8BYTES 
.rdata 040401040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT
.rdata 040500040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_16BYTES 

.tls 0C0300040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES


.xdata 040401040 &IMAGE_SCN_CNT_INITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT

; Virtual Data Flags

.bss 0C0300080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES 
.bss 0C0301080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_4BYTES &IMAGE_SCN_LNK_COMDAT
.bss 0C0400080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES 
.bss 0C0401080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_8BYTES &IMAGE_SCN_LNK_COMDAT
.bss 0C0500080 &IMAGE_SCN_CNT_UNINITIALIZED_DATA &IMAGE_SCN_MEM_READ &IMAGE_SCN_MEM_WRITE &IMAGE_SCN_ALIGN_16BYTES 

; Linker Directive Flags

.drectve 0100A00 &IMAGE_SCN_LNK_INFO &IMAGE_SCN_LNK_REMOVE &IMAGE_SCN_ALIGN_1BYTES




;;

[RawDataType: D$ 0]

; RawData Types Constants

[RDT_DATA 0] ; This is the default. If everything fails, it always set to DATA
[RDT_LNKDIRECTIVE 1]
[RDT_CODE 2]
[RDT_VIRTUALDATA 3]
[RDT_DEBUGS 4]
[RDT_DEBUGF 5]
[RDT_DEBUGT 6]
[RDT_DEBUGP 7]
[RDT_IDATA 8]
[RDT_RDATA 9]
[RDT_XDATA 10]
[RDT_PDATA 11]
[RDT_EDATA 12]
[RDT_TLSDATA 13]
[RDT_RELOCDATA 14]
[RDT_RSRCDATA 15]
[RDT_ERROR 16] ; This is DATA, but we set this Flag to identify some error cases messages
[RDT_STABSTR 17] ; .stabstr ; NetFramework stab string section. This section contains only strings.

Proc IdentifyRawDataType:
    Uses eax, esi, ecx

    mov eax D$esi+36 ; eax points to the Characteristics member

    ; Reinitialize the Raw Data to make sure it always will be set to 0 when the below
    ; checkings fails.

    mov D$RawDataType 0

    ; Check if it is .text or .code or any other section that is related to code only
    .Test_If eax &IMAGE_SCN_CNT_CODE
            jmp L0>
    .Test_Else_If eax &IMAGE_SCN_MEM_EXECUTE
    L0:
            Test_If eax &IMAGE_SCN_CNT_UNINITIALIZED_DATA
                mov D$RawDataType RDT_VIRTUALDATA
            Test_Else
                mov D$RawDataType RDT_CODE
            Test_End

    .Test_Else

        ; All the rest is Data. Let's analyze what kind of data it is.
        ; Find Virtual Data
        ; Guga Note: Look at CTLFWR32.LIB we will need a Virtual data. Like we did for the raw data.

        Test_If eax &IMAGE_SCN_CNT_UNINITIALIZED_DATA
            mov D$RawDataType RDT_VIRTUALDATA
            ExitP
        Test_End

        ; Check for long section names (KSGUID.LIB)
        If B$esi = '/'
            ; Similar to what we found at WriteIndirectSectionName
            call GetName1Offset ; convert the Offset to hexadecimal
            call GetStringTablePointer
            mov esi D$CoffSectionBase
            add esi D$CoffSymbolsPointer
            mov ecx D$PointerToStringTable
            add ecx D$MemberName1Offset | mov esi ecx
        End_If

        ; Find Virtual Data
        ; convert all section names to small caps and compare them.
        ; save the address of esi to be restored later
        push D$esi, D$esi+4
        or D$esi 020202020
        or D$esi+4 020202020

        .If D$esi = 'bss' ; BSS or bss
            jmp L0>
        .Else_If D$esi = '.bss'

        L0:
            mov D$RawDataType RDT_VIRTUALDATA

        ; Find Debug Data. It will use the proper Debug structures to display in the raw data.
        .Else_If D$esi = '.deb'

            If D$esi+4 = 'ug$s' ; .debug$s , .debug$S or Big caps
                mov D$RawDataType RDT_DEBUGS
            Else_If D$esi+4 = 'ug$f' ; .debug$f , .debug$F or Big caps
                mov D$RawDataType RDT_DEBUGF
            Else_If D$esi+4 = 'ug$t' ; .debug$t , .debug$T or Big caps
                mov D$RawDataType RDT_DEBUGT
            Else_If D$esi+4 = 'ug$p' ; .debug$p , .debug$P or Big caps
                mov D$RawDataType RDT_DEBUGP
            Else
                mov D$RawDataType RDT_ERROR
            End_If

        ; Find NetFramework Stab Structure. It will use the proper Debug structures to display in the raw data.
        .Else_If_And D$esi = '.sta', D$esi+4 = 'bstr'
            mov D$RawDataType RDT_STABSTR

        ; Check for Linker Directive data. esi points to the Characteristics member.

        .Else_If_And D$esi = '.dre', D$esi+4 = 'ctve' ; .drectve

            Test_If eax &IMAGE_SCN_LNK_INFO
                ; PointerToRelocations and PointerToLinenumbers must be 0, because
                ; the section must not have relocations or line numbers.
                If_And D$esi+24 = 0, D$esi+28 = 0
                    mov D$RawDataType RDT_LNKDIRECTIVE
                Else
                    mov D$RawDataType RDT_ERROR
                End_If

            Test_Else
                mov D$RawDataType RDT_ERROR
            Test_End

        ; Find Common Data. (.data; .idata; .rdata; .xdata; .pdata; .edata)

        .Else_If_And D$esi = '.dat', B$esi+4 = 'a'
            mov D$RawDataType RDT_DATA

        .Else_If D$esi+2 = 'data'

            If W$esi = '.i' ; Import Data Tables. Need to use proper structures or Data arrays.
                            ; Using IMAGE_IMPORT_DESCRIPTOR structure. Take a look at ACLCLS.LIB to we identify the proper structures and Data Bytes
                mov D$RawDataType RDT_IDATA
            Else_If W$esi = '.r' ; Read Only Data. In general, it is a series of Dwords, but it can be anything, like a serie of structures etc.
                mov D$RawDataType RDT_RDATA
            Else_If W$esi = '.x' ; Exception information Data Tables. Need to use proper structures EXCEPTION_RECORD. _msExcept _msExcInfo
                mov D$RawDataType RDT_XDATA
            Else_If W$esi = '.p' ; Exception information Data Tables. Need to use proper structures (EXCEPTION_RECORD ?)
                mov D$RawDataType RDT_PDATA
            Else_If W$esi = '.e' ; Export Data Tables. Need to use proper structures or Data arrays (IMAGE_EXPORT_DIRECTORY ?)
                mov D$RawDataType RDT_EDATA
            Else
                mov D$RawDataType RDT_ERROR
            End_If

        .Else_If D$esi = '.tls' ; Thread-local storage Data. Some section uses the IMAGE_TLS_DIRECTORY32 to point to
                                ; some data inside this .tls section. See MSVCRT.LIB, MSVCRTD.LIB, LIBCMT.LIB, LIBCMTD.LIB
            mov D$RawDataType RDT_TLSDATA

        .Else_If_And D$esi = '.rel', W$esi+4 = 'oc' ; Image relocations. We must use the (IMAGE_RELOCATION ?) for this
            mov D$RawDataType RDT_RELOCDATA

        .Else_If_And D$esi = '.rsr', B$esi+4 = 'c' ; Resource Section. We must use the IMAGE_RESOURCE_DIRECTORY structure.
            mov D$RawDataType RDT_RSRCDATA

        .Else
            ; When everything else fails or we have some unknown section names, we always set to DATA
            mov D$RawDataType RDT_DATA
        .End_If

        pop D$esi+4, D$esi

    .Test_End

EndP

___________________________________________________

GetCoffSectionHeader:

    push esi
        zCopy CoffSectionHeaderTitle
    pop esi

    ; The name of the section is indirected addressed on the String Table ?
    ; Yes, do next line. No, jmp over. Example: KSGUID.LIB

    If B$esi = '/'
        call WriteIndirectSectionName
    End_If

    push esi
        mov B$edi '[' | inc edi
        call WriteObjIndice
        zCopy {'ImgSecHdr', 0}
        zCopy SectionHeaderNumber
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi

;    call IdentifyRawDataType

    call WriteObjSectionHeaderItem CoffSectionHeaderName, {': B$ ', 0}

    If B$esi-8 = '/'
        call WriteLinkerMemberSizeHex D$MemberName1Offset
    End_If

    call WriteObjSectionHeaderItem CoffSectionHeaderVirtualSize, {': D$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderRVA, {': D$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderSize, {': D$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderPointerToData, {': D$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderPointerToReloc, {': D$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderPointerToLinesNumbers, {': D$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderNumberOfRelocations, {': W$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderNumberOfLinesNumbers, {': W$ ', 0}
    call WriteObjSectionHeaderItem CoffSectionHeaderCharacteristics, {': D$ ', 0}
    push esi
        sub edi 2 | mov eax D$esi-4 | call WriteCharacteristicsEquates
        mov W$edi CRLF, B$edi+2 ']', D$edi+3 CRLF2 | add edi 7
    pop esi
ret
____________________________________________________________________________________________

WriteSectionHeaderVirtualSizeDiffLabel:
    push esi

        call WriteObjIndice
        zCopy {"VirtualDataEnd", 0}
        zCopy SectionHeaderNumber

        mov D$edi ' - ' | add edi 3

        call WriteObjIndice
        zCopy {"VirtualData", 0}
        zCopy SectionHeaderNumber
    pop esi
ret


WriteSectionHeaderRawSizeDiffLabel:

    If D$esi+4 = 0 ; Is PointerToRawData = 0 ? If so, it means we are dealing with Virtual data. Do next line
        push esi | lodsd | call WriteEax | pop esi | ret
    End_If

    push esi

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".RawDataEnd", 0}

        mov D$edi ' - ' | add edi 3

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".RawData", 0}

    pop esi
ret

[IMAGE_FILE_HEADER_Comment1: 'IMAGE_FILE_HEADER_', 0]

WriteSectionHeaderPointerToDataDiffLabel:
    push esi

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".RawData", 0}

        mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        call WriteIndiceOnly

    pop esi
ret


Proc FixRelocDiffLabel:
    uses eax, ebx, ecx, esi, edi

    ; reinitialize the sectino counter
    call InitSectionHeaderNumber

    movzx ecx W$ObjNumberOfSections
    mov ebx D$FirstSectionPointer
    mov edi D$esi ; edi hold the value of our PointerToRelocations to be compared

L0:

    ; Points to PointerToRelocations starting at the 1st section
   .If edi = D$ebx+018 ; is our relocs equal to the relocs on section where we are ?
        If W$ebx+020 <> 0 ; Is NumberOfRelocations = 0 ? Yes, we are analysing our own section
            ExitP
        End_If
   .End_If

   call IncrementSectionHeaderNumber
   add ebx 028 ; go to the next section
   Loop L0<


EndP


WriteSectionHeaderPointerToRelocDiffLabel:

    push D$SectionHeaderNumber

    If W$esi+4+4 = 0 ; Points to NumberOfRelocations. aclcls.lib
        ; Check how many relocs we have. If we have 0, it means that the pointer is being referenced by another section.
         call FixRelocDiffLabel
    End_If

    push esi

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".ImgReloc", 0}
        zCopy RelocNumber

        mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        call WriteIndiceOnly

    pop esi

    pop D$SectionHeaderNumber

ret


WriteSectionHeaderPointerToLineNumberDiffLabel:
    push esi

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".LineNumber", 0}
        zCopy CoffLineNumber

        mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        call WriteIndiceOnly

    pop esi
ret


; It should display for example: ; Stringdata15 - StringTableSize

Proc WriteSectionHeaderPointerToStringTableDiffLabel:
    Uses esi, eax, edx

    ; Initialize the String Record Counter

    call InitStringTableRecord

    ; On this case, the Stringtable must be initialized with 0. Otherwise our counter will already start at 01
    ; So, we must reset the counter to '000000'.

    mov D$StringTableRecord+4 '00'

    lodsd ; eax is pointing to the Offset of the String on the String Table
    mov edx D$PointerToStringTable
    add edx 4 ; Bypass the String Table Size. and Point to the beginning of the 1st String
    mov esi edx ; esi is pointing to the 1st string
    add edx eax ; edx will be using as a counter of the total amount of strings untill we reach the String where we are.

    .Do

        While B$esi <> 0 | inc esi | End_While | inc esi | On B$esi = 0, mov edx 0

        call IncrementStringTableRecord

    .Loop_Until esi >= edx


        push esi
            call WriteObjIndice
            zCopy {'StringData', 0}
            zCopy StringTableRecord
        pop esi

        mov D$edi ' - ' | add edi 3

        call WriteObjIndice
        zCopy {'StringTableSize', 0}
        zCopy {' ; Hex Value:  ', 0}


EndP

____________________________________________________________________________________________


Proc WriteSectionHeaderSymbolConstantIndex:
    uses eax, ecx, ebx, esi

    call InitSymbolIndexRecord
    call WriteObjIndice

    ; Fix the label to show in Caps and replace the '." with an "_"
    ; The 1st char in "Obj000000." is in Caps, so we don't need to overwrite the "O" char

    push edi
    mov B$edi-1 '_', W$edi-9 'BJ'
    pop edi

    push esi
    zCopy {'SYMBOLINDEX', 0}
    pop esi

    mov ecx D$esi

    ; Note to René: Replacing this with a hex to decimal ascii string is better,
    ; but i couldn't make it be on the same style as SymbolTableIndex

    While ecx <> 0
        call IncrementSymbolIndexRecord
        dec ecx
    End_While

    zCopy SymbolTableIndex

    ; restore the SymbolTableIndex
    call InitSymbolIndexRecord

EndP
____________________________________________________________________________________________



[CoffRelocHeaderTitle: "; IMAGE_RELOCATION Structure, pointed at: ", 0]

[CoffRelocHeaderRelocRVA: 'VirtualAddress', 0
 CoffRelocHeaderRelocCount: 'RelocCount', 0
 CoffRelocHeaderSymbolTableIndex: 'SymbolTableIndex', 0
 CoffRelocHeaderType: 'Type1', 0]

Proc WriteObjRelocHeaderItem:
    Argument @Text1, @Text2

        push esi
            call WriteObjIndice

            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber

            zCopy {'.ImgReloc', 0}
            zCopy RelocNumber;SectionHeaderNumber
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text2, al B$eax+2

        ..If al = 'D'

            .If D$esi <> 0

                mov eax D@Text1

                If D$eax = 'Virt' ; from "VirtualAddress" string

                    lodsd | call WriteEax
                    push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                ;Else_If D$eax = 'Relo'; From "RelocCount" string

                    ;call WriteSectionHeaderPointerToDataDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                Else_If D$eax = 'Symb'; From "SymbolTableIndex" string
                    call WriteSectionHeaderSymbolConstantIndex
                    add esi 4 ; we need to increment esi by 4 to we adjust at the end of the End_If macro
                    push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                End_If

            .Else
                add esi 4


            .End_If
            ; We need to subtract esi by 4 (01 Dword) to we get back to the path again. It was increased above.
            sub esi 4 | lodsd | call WriteEax

        ..Else_If al = 'W'
            lodsw | and eax 0FFFF | call WriteEax

        ;..Else

        ..End_If

        mov W$edi CRLF | add edi 2
EndP

_________________________________________________________

;;
a) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_I386, IMAGE_FILE_MACHINE_I486, 
IMAGE_FILE_MACHINE_I586,  IMAGE_FILE_MACHINE_IA64 (So, INtel compatible 
processors), the equates are:

&IMAGE_REL_I386_ABSOLUTE
&IMAGE_REL_I386_DIR16
&IMAGE_REL_I386_REL16
&IMAGE_REL_I386_DIR32
&IMAGE_REL_I386_DIR32NB
&IMAGE_REL_I386_SEG12
&IMAGE_REL_I386_SECTION
&IMAGE_REL_I386_SECREL
&IMAGE_REL_I386_REL32

b) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_MIPS16, IMAGE_FILE_MACHINE_MIPSFPU, 
IMAGE_FILE_MACHINE_MIPSFPU16 , IMAGE_FILE_MACHINE_R10000, 
IMAGE_FILE_MACHINE_R3000,
IMAGE_FILE_MACHINE_R4000, IMAGE_FILE_MACHINE_WCEMIPSV2
(So, MIPS compatible processors), the equates are:

&IMAGE_REL_MIPS_ABSOLUTE
&IMAGE_REL_MIPS_REFHALF
&IMAGE_REL_MIPS_REFWORD
&IMAGE_REL_MIPS_JMPADDR
&IMAGE_REL_MIPS_REFHI
&IMAGE_REL_MIPS_REFLO
&IMAGE_REL_MIPS_GPREL
&IMAGE_REL_MIPS_LITERAL
&IMAGE_REL_MIPS_SECTION
&IMAGE_REL_MIPS_SECREL
&IMAGE_REL_MIPS_SECRELLO
&IMAGE_REL_MIPS_SECRELHI
&IMAGE_REL_MIPS_JMPADDR16
&IMAGE_REL_MIPS_REFWORDNB
&IMAGE_REL_MIPS_PAIR

c) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_ALPHA, IMAGE_FILE_MACHINE_ALPHA64, 
IMAGE_FILE_MACHINE_AXP64 (So, Alpha compatible processors), the equates are:

&IMAGE_REL_ALPHA_ABSOLUTE
&IMAGE_REL_ALPHA_REFLONG
&IMAGE_REL_ALPHA_REFQUAD
&IMAGE_REL_ALPHA_GPREL32
&IMAGE_REL_ALPHA_LITERAL
&IMAGE_REL_ALPHA_LITUSE
&IMAGE_REL_ALPHA_GPDISP
&IMAGE_REL_ALPHA_BRADDR
&IMAGE_REL_ALPHA_HINT
&IMAGE_REL_ALPHA_INLINE_REFLONG
&IMAGE_REL_ALPHA_REFHI
&IMAGE_REL_ALPHA_REFLO
&IMAGE_REL_ALPHA_PAIR
&IMAGE_REL_ALPHA_MATCH
&IMAGE_REL_ALPHA_SECTION
&IMAGE_REL_ALPHA_SECREL
&IMAGE_REL_ALPHA_REFLONGNB
&IMAGE_REL_ALPHA_SECRELLO
&IMAGE_REL_ALPHA_SECRELHI
&IMAGE_REL_ALPHA_REFQ3
&IMAGE_REL_ALPHA_REFQ2
&IMAGE_REL_ALPHA_REFQ1
&IMAGE_REL_ALPHA_GPRELLO
&IMAGE_REL_ALPHA_GPRELHI

d) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_POWERPC, IMAGE_FILE_MACHINE_POWERPCFP, (So, IBM 
POwer PC compatible processors), the equates are:

&IMAGE_REL_PPC_ABSOLUTE
&IMAGE_REL_PPC_ADDR64
&IMAGE_REL_PPC_ADDR32
&IMAGE_REL_PPC_ADDR24
&IMAGE_REL_PPC_ADDR16
&IMAGE_REL_PPC_ADDR14
&IMAGE_REL_PPC_REL24
&IMAGE_REL_PPC_REL14
&IMAGE_REL_PPC_ADDR32NB
&IMAGE_REL_PPC_SECREL
&IMAGE_REL_PPC_SECTION
&IMAGE_REL_PPC_SECREL16
&IMAGE_REL_PPC_REFHI
&IMAGE_REL_PPC_REFLO
&IMAGE_REL_PPC_PAIR
&IMAGE_REL_PPC_SECRELLO
&IMAGE_REL_PPC_SECRELHI
&IMAGE_REL_PPC_GPREL

e) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_SH3, IMAGE_FILE_MACHINE_SH3DSP, 
IMAGE_FILE_MACHINE_SH3E, IMAGE_FILE_MACHINE_SH4, IMAGE_FILE_MACHINE_SH5,  
(So, Hitachi SuperH compatible processors), the equates are:

&IMAGE_REL_SH3_ABSOLUTE
&IMAGE_REL_SH3_DIRECT16
&IMAGE_REL_SH3_DIRECT32
&IMAGE_REL_SH3_DIRECT8
&IMAGE_REL_SH3_DIRECT8_WORD
&IMAGE_REL_SH3_DIRECT8_LONG
&IMAGE_REL_SH3_DIRECT4
&IMAGE_REL_SH3_DIRECT4_WORD
&IMAGE_REL_SH3_DIRECT4_LONG
&IMAGE_REL_SH3_PCREL8_WORD
&IMAGE_REL_SH3_PCREL8_LONG
&IMAGE_REL_SH3_PCREL12_WORD
&IMAGE_REL_SH3_STARTOF_SECTION
&IMAGE_REL_SH3_SIZEOF_SECTION
&IMAGE_REL_SH3_SECTION
&IMAGE_REL_SH3_SECREL
&IMAGE_REL_SH3_DIRECT32_NB

f) When the  Machine member of IMAGE_FILE_HEADER is settlled to one of 
theses: IMAGE_FILE_MACHINE_ARM,  (So, ARM compatible processors), the 
equates are:

&IMAGE_REL_ARM_ABSOLUTE
&IMAGE_REL_ARM_ADDR32
&IMAGE_REL_ARM_ADDR32NB
&IMAGE_REL_ARM_BRANCH24
&IMAGE_REL_ARM_BRANCH11
&IMAGE_REL_ARM_SECTION
&IMAGE_REL_ARM_SECREL
;;

WriteRelocHeaderTypeEquate:
    push esi

    movzx eax W$esi-2 | mov ebx D$CoffSectionBase | movzx ebx W$ebx

    mov D$edi ' ; ' | add edi 3

    ; Intel Compatible: IMAGE_FILE_MACHINE_I386, IMAGE_FILE_MACHINE_I486,
    ; IMAGE_FILE_MACHINE_I586,  IMAGE_FILE_MACHINE_IA64

    ..If ebx = &IMAGE_FILE_MACHINE_I386
L1:     .If eax = &IMAGE_REL_I386_ABSOLUTE
            zCopy {'&IMAGE_REL_I386_ABSOLUTE', 0}
        .Else_If eax = &IMAGE_REL_I386_DIR16
            zCopy {'&IMAGE_REL_I386_DIR16', 0}
        .Else_If eax = &IMAGE_REL_I386_REL16
            zCopy {'&IMAGE_REL_I386_REL16', 0}
        .Else_If eax = &IMAGE_REL_I386_DIR32
            zCopy {'&IMAGE_REL_I386_DIR32', 0}
        .Else_If eax = &IMAGE_REL_I386_DIR32NB
            zCopy {'&IMAGE_REL_I386_DIR32NB', 0}
        .Else_If eax = &IMAGE_REL_I386_SEG12
            zCopy {'&IMAGE_REL_I386_SEG12', 0}
        .Else_If eax = &IMAGE_REL_I386_SECTION
            zCopy {'&IMAGE_REL_I386_SECTION', 0}
        .Else_If eax = &IMAGE_REL_I386_SECREL
            zCopy {'&IMAGE_REL_I386_SECREL', 0}
        .Else_If eax = &IMAGE_REL_I386_REL32
            zCopy {'&IMAGE_REL_I386_REL32', 0}
        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_I486
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_I586
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_IA64
        jmp L1<<


    ; Mips Compatible: IMAGE_FILE_MACHINE_MIPS16, IMAGE_FILE_MACHINE_MIPSFPU,
    ; IMAGE_FILE_MACHINE_MIPSFPU16 , IMAGE_FILE_MACHINE_R10000,
    ; IMAGE_FILE_MACHINE_R3000, IMAGE_FILE_MACHINE_R4000, IMAGE_FILE_MACHINE_WCEMIPSV2

    ..Else_If ebx = &IMAGE_FILE_MACHINE_MIPS16
L1:     .If eax = &IMAGE_REL_MIPS_ABSOLUTE
            zCopy {'&IMAGE_REL_I386_ABSOLUTE', 0}
        .Else_If eax = &IMAGE_REL_MIPS_REFHALF
            zCopy {'&IMAGE_REL_MIPS_REFHALF', 0}
        .Else_If eax = &IMAGE_REL_MIPS_REFWORD
            zCopy {'&IMAGE_REL_MIPS_REFWORD', 0}
        .Else_If eax = &IMAGE_REL_MIPS_JMPADDR
            zCopy {'&IMAGE_REL_MIPS_JMPADDR', 0}
        .Else_If eax = &IMAGE_REL_MIPS_REFHI
            zCopy {'IMAGE_REL_MIPS_REFHI', 0}
        .Else_If eax = &IMAGE_REL_MIPS_REFLO
            zCopy {'&IMAGE_REL_MIPS_REFLO', 0}
        .Else_If eax = &IMAGE_REL_MIPS_GPREL
            zCopy {'&IMAGE_REL_MIPS_GPREL', 0}
        .Else_If eax = &IMAGE_REL_MIPS_LITERAL
            zCopy {'&IMAGE_REL_MIPS_LITERAL', 0}
        .Else_If eax = &IMAGE_REL_MIPS_SECTION
            zCopy {'&IMAGE_REL_MIPS_SECTION', 0}
        .Else_If eax = &IMAGE_REL_MIPS_SECREL
            zCopy {'&IMAGE_REL_MIPS_SECREL', 0}
        .Else_If eax = &IMAGE_REL_MIPS_SECRELLO
            zCopy {'&IMAGE_REL_MIPS_SECRELLO', 0}
        .Else_If eax = &IMAGE_REL_MIPS_SECRELHI
            zCopy {'&IMAGE_REL_MIPS_SECRELHI', 0}
        .Else_If eax = &IMAGE_REL_MIPS_JMPADDR16
            zCopy {'&IMAGE_REL_MIPS_JMPADDR16', 0}
        .Else_If eax = &IMAGE_REL_MIPS_REFWORDNB
            zCopy {'&IMAGE_REL_MIPS_REFWORDNB', 0}
        .Else_If eax = &IMAGE_REL_MIPS_PAIR
            zCopy {'&IMAGE_REL_MIPS_PAIR', 0}
        .End_If


    ..Else_If ebx = &IMAGE_FILE_MACHINE_MIPSFPU
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_MIPSFPU16
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_R10000
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_R3000
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_R4000
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_WCEMIPSV2
        jmp L1<<


    ; Alpha Compatible: IMAGE_FILE_MACHINE_ALPHA, IMAGE_FILE_MACHINE_ALPHA64, IMAGE_FILE_MACHINE_AXP64

    ..Else_If ebx = &IMAGE_FILE_MACHINE_ALPHA
L1:     .If eax = &IMAGE_REL_ALPHA_ABSOLUTE
            zCopy {'&IMAGE_REL_ALPHA_ABSOLUTE', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFLONG
            zCopy {'&IMAGE_REL_ALPHA_REFLONG', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFQUAD
            zCopy {'&IMAGE_REL_ALPHA_REFQUAD', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_GPREL32
            zCopy {'&IMAGE_REL_ALPHA_GPREL32', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_LITERAL
            zCopy {'&IMAGE_REL_ALPHA_LITERAL', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_LITUSE
            zCopy {'&IMAGE_REL_ALPHA_LITUSE', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_GPDISP
            zCopy {'&IMAGE_REL_ALPHA_GPDISP', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_BRADDR
            zCopy {'&IMAGE_REL_ALPHA_BRADDR', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_HINT
            zCopy {'&IMAGE_REL_ALPHA_HINT', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_INLINE_REFLONG
            zCopy {'&IMAGE_REL_ALPHA_INLINE_REFLONG', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFHI
            zCopy {'&IMAGE_REL_ALPHA_REFHI', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFLO
            zCopy {'&IMAGE_REL_ALPHA_REFLO', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_PAIR
            zCopy {'&IMAGE_REL_ALPHA_PAIR', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_MATCH
            zCopy {'&IMAGE_REL_ALPHA_MATCH', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_SECTION
            zCopy {'&IMAGE_REL_ALPHA_SECTION', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_SECREL
            zCopy {'&IMAGE_REL_ALPHA_SECREL', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFLONGNB
            zCopy {'&IMAGE_REL_ALPHA_REFLONGNB', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_SECRELLO
            zCopy {'&IMAGE_REL_ALPHA_SECRELLO', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_SECRELHI
            zCopy {'&IMAGE_REL_ALPHA_SECRELHI', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFQ3
            zCopy {'&IMAGE_REL_ALPHA_REFQ3', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFQ2
            zCopy {'&IMAGE_REL_ALPHA_REFQ2', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_REFQ1
            zCopy {'&IMAGE_REL_ALPHA_REFQ1', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_GPRELLO
            zCopy {'&IMAGE_REL_ALPHA_GPRELLO', 0}
        .Else_If eax = &IMAGE_REL_ALPHA_GPRELHI
            zCopy {'&IMAGE_REL_ALPHA_GPRELHI', 0}
        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_ALPHA64
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_AXP64
        jmp L1<<

    ; IBM Power PC compatible: IMAGE_FILE_MACHINE_POWERPC, IMAGE_FILE_MACHINE_POWERPCFP

    ..Else_If ebx = &IMAGE_FILE_MACHINE_POWERPC
L1:     .If eax = &IMAGE_REL_PPC_ABSOLUTE
            zCopy {'&IMAGE_REL_PPC_ABSOLUTE', 0}
        .Else_If eax = &IMAGE_REL_PPC_ADDR64
            zCopy {'&IMAGE_REL_PPC_ADDR64', 0}
        .Else_If eax = &IMAGE_REL_PPC_ADDR32
            zCopy {'&IMAGE_REL_PPC_ADDR32', 0}
        .Else_If eax = &IMAGE_REL_PPC_ADDR24
            zCopy {'&IMAGE_REL_PPC_ADDR24', 0}
        .Else_If eax = &IMAGE_REL_PPC_ADDR16
            zCopy {'&IMAGE_REL_PPC_ADDR16', 0}
        .Else_If eax = &IMAGE_REL_PPC_ADDR14
            zCopy {'&IMAGE_REL_PPC_ADDR14', 0}
        .Else_If eax = &IMAGE_REL_PPC_REL24
            zCopy {'&IMAGE_REL_PPC_REL24', 0}
        .Else_If eax = &IMAGE_REL_PPC_REL14
            zCopy {'&IMAGE_REL_PPC_REL14', 0}
        .Else_If eax = &IMAGE_REL_PPC_ADDR32NB
            zCopy {'&IMAGE_REL_PPC_ADDR32NB', 0}
        .Else_If eax = &IMAGE_REL_PPC_SECREL
            zCopy {'&IMAGE_REL_PPC_SECREL', 0}
        .Else_If eax = &IMAGE_REL_PPC_SECTION
            zCopy {'&IMAGE_REL_PPC_SECTION', 0}
        .Else_If eax = &IMAGE_REL_PPC_SECREL16
            zCopy {'&IMAGE_REL_PPC_SECREL16', 0}
        .Else_If eax = &IMAGE_REL_PPC_REFHI
            zCopy {'&IMAGE_REL_PPC_REFHI', 0}
        .Else_If eax = &IMAGE_REL_PPC_REFLO
            zCopy {'&IMAGE_REL_PPC_REFLO', 0}
        .Else_If eax = &IMAGE_REL_PPC_PAIR
            zCopy {'&IMAGE_REL_PPC_PAIR', 0}
        .Else_If eax = &IMAGE_REL_PPC_SECRELLO
            zCopy {'&IMAGE_REL_PPC_SECRELLO', 0}
        .Else_If eax = &IMAGE_REL_PPC_SECRELHI
            zCopy {'&IMAGE_REL_PPC_SECRELHI', 0}
        .Else_If eax = &IMAGE_REL_PPC_GPREL
            zCopy {'&IMAGE_REL_PPC_GPREL', 0}
        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_POWERPCFP
        jmp L1<<

    ; Hitachi SuperH compatible: IMAGE_FILE_MACHINE_SH3, IMAGE_FILE_MACHINE_SH3DSP, IMAGE_FILE_MACHINE_SH3E, IMAGE_FILE_MACHINE_SH4, IMAGE_FILE_MACHINE_SH5

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH3
L1:     .If eax = &IMAGE_REL_SH3_ABSOLUTE
            zCopy {'&IMAGE_REL_SH3_ABSOLUTE', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT16
            zCopy {'&IMAGE_REL_SH3_DIRECT16', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT32
            zCopy {'&IMAGE_REL_SH3_DIRECT32', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT8
            zCopy {'&IMAGE_REL_SH3_DIRECT8', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT8_WORD
            zCopy {'&IMAGE_REL_SH3_DIRECT8_WORD', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT8_LONG
            zCopy {'&IMAGE_REL_SH3_DIRECT8_LONG', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT4
            zCopy {'&IMAGE_REL_SH3_DIRECT4', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT4_WORD
            zCopy {'&IMAGE_REL_SH3_DIRECT4_WORD', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT4_LONG
            zCopy {'&IMAGE_REL_SH3_DIRECT4_LONG', 0}
        .Else_If eax = &IMAGE_REL_SH3_PCREL8_WORD
            zCopy {'&IMAGE_REL_SH3_PCREL8_WORD', 0}
        .Else_If eax = &IMAGE_REL_SH3_PCREL8_LONG
            zCopy {'&IMAGE_REL_SH3_PCREL8_LONG', 0}
        .Else_If eax = &IMAGE_REL_SH3_PCREL12_WORD
            zCopy {'&IMAGE_REL_SH3_PCREL12_WORD', 0}
        .Else_If eax = &IMAGE_REL_SH3_STARTOF_SECTION
            zCopy {'&IMAGE_REL_SH3_STARTOF_SECTION', 0}
        .Else_If eax = &IMAGE_REL_SH3_SIZEOF_SECTION
            zCopy {'&IMAGE_REL_SH3_SIZEOF_SECTION', 0}
        .Else_If eax = &IMAGE_REL_SH3_SECTION
            zCopy {'&IMAGE_REL_SH3_SECTION', 0}
        .Else_If eax = &IMAGE_REL_SH3_SECREL
            zCopy {'&IMAGE_REL_SH3_SECREL', 0}
        .Else_If eax = &IMAGE_REL_SH3_DIRECT32_NB
            zCopy {'&IMAGE_REL_SH3_DIRECT32_NB', 0}
        .End_If

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH3DSP
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH3E
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH4
        jmp L1<<

    ..Else_If ebx = &IMAGE_FILE_MACHINE_SH5
        jmp L1<<

    ; ARM compatible: IMAGE_FILE_MACHINE_ARM

    ..Else_If ebx = &IMAGE_FILE_MACHINE_ARM
        .If eax = &IMAGE_REL_ARM_ABSOLUTE
            zCopy {'&IMAGE_REL_ARM_ABSOLUTE', 0}
        .Else_If eax = &IMAGE_REL_ARM_ADDR32
            zCopy {'&IMAGE_REL_ARM_ADDR32', 0}
        .Else_If eax = &IMAGE_REL_ARM_ADDR32NB
            zCopy {'&IMAGE_REL_ARM_ADDR32NB', 0}
        .Else_If eax = &IMAGE_REL_ARM_BRANCH24
            zCopy {'&IMAGE_REL_ARM_BRANCH24', 0}
        .Else_If eax = &IMAGE_REL_ARM_BRANCH11
            zCopy {'&IMAGE_REL_ARM_BRANCH11', 0}
        .Else_If eax = &IMAGE_REL_ARM_SECTION
            zCopy {'&IMAGE_REL_ARM_SECTION', 0}
        .Else_If eax = &IMAGE_REL_ARM_SECREL
            zCopy {'&IMAGE_REL_ARM_SECREL', 0}
        .End_If

    ..End_If

    pop esi


ret

; GugaNote
; Problem of multiple section with different relocs with DX9SDKSampleFramework.lib
; Most often, they may contains relocs offset, but size is 0 (ForceLibrary.lib)

Proc GetCoffRelocHeader:
    uses ecx, ebx

    push esi
        zCopy CoffRelocHeaderTitle
        call WriteObjIndice
        zCopy {"ImgSecHdr", 0}
        zCopy SectionHeaderNumber
        zCopy {".PointerToRelocations", 0}
        zCopy {" Member", CRLF2, "[", 0}

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".ImgReloc", 0}
        zCopy RelocNumber
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi

    call WriteObjRelocHeaderItem CoffRelocHeaderRelocRVA, {': D$ ', 0}
    call WriteObjRelocHeaderItem CoffRelocHeaderSymbolTableIndex, {': D$ ', 0}
    call WriteObjRelocHeaderItem CoffRelocHeaderType, {': W$ ', 0}
    sub edi 2 | call WriteRelocHeaderTypeEquate
    mov W$edi CRLF | add edi 2

    push esi
        mov B$edi ']', D$edi+1 CRLF2 | add edi 5
    pop esi
EndP
____________________________________________________________

[RelocNumber: '000001', 0]


InitSectionRelocNumber:
    mov D$RelocNumber '0000', D$RelocNumber+4 '01'
ret


IncrementSectionRelocNumber:
    lea ebx D$RelocNumber+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret




____________________________________________________________

Proc WriteObjLineHeaderItem:
    Argument @Text1, @Text2
    Uses edx

        push esi
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".LineNumber", 0}
        zCopy CoffLineNumber

        zCopy D@Text1
        zCopy D@Text2
        pop esi


                mov eax D@Text1

                .If D$eax = 'Symb' ; from "SymbolTableIndex" string
                    call WriteSectionHeaderSymbolConstantIndex
                    push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
                    lodsd | call WriteEax

                .Else_If D$eax = 'Virt'; From "VirtualAddress" string
                    lodsd | call WriteEax

                .Else_If D$eax = 'Line'; From "Linenumber" string
                    xor eax eax
                    lodsw | call WriteEax

                .End_If


        mov W$edi CRLF | add edi 2
EndP

;;
[IMAGE_LINENUMBER:
 SymbolTableIndex: VirtualAddress: D$ 0
 Linenumber: W$ 0]
;;

[CoffLineNumberTitle: "
__________________________________________________

; IMAGE_LINENUMBER Structure, pointed at: ", 0]

[CoffLineHeaderSymbolTableIndex: 'SymbolTableIndex', 0
 CoffLineHeaderVirtualAddress: 'VirtualAddress', 0
 CoffLineHeaderLinenumber: 'Linenumber', 0]

Proc GetCoffLineHeader:
    uses ecx, ebx


    push esi
        zCopy CoffLineNumberTitle
        call WriteObjIndice
        zCopy {"ImgSecHdr", 0}
        zCopy SectionHeaderNumber
        zCopy {".PointerToLinenumbers Member", 0}
    pop esi

    mov D$edi CRLF2 | add edi 4

    If W$esi+4 <> 0
        push esi
        zCopy {"; This Structure contains the correspondent line number on the original source specified at LineNumber member.", 0}
        zCopy {W$ CRLF, B$ "; Also it is related only to Code and contains the Virtual Address of some referenced data that can", 0}
        zCopy {W$ CRLF, B$ "; be a Local Variable, Argument, or a Data Pointer", D$ CRLF2, 0}
        pop esi
    End_If


    push esi
        mov B$edi "[" | inc edi
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".LineNumber", 0}
        zCopy CoffLineNumber
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi

    ; problem in ATLDLOAD.LIB

    If W$esi+4 = 0

        call WriteObjLineHeaderItem CoffLineHeaderSymbolTableIndex, {': D$ ', 0}
    Else

        call WriteObjLineHeaderItem CoffLineHeaderVirtualAddress, {': D$ ', 0}

    End_If

    call WriteObjLineHeaderItem CoffLineHeaderLinenumber, {': W$ ', 0}

;    sub edi 2 | call WriteRelocHeaderTypeEquate
 ;   mov W$edi CRLF | add edi 2

    push esi
        mov B$edi ']', D$edi+1 CRLF2 | add edi 5
    pop esi
EndP



[CoffLineNumber: '000001', 0]


InitSectionLineNumber:
    mov D$CoffLineNumber '0000', D$CoffLineNumber+4 '01'
ret


IncrementSectionLineNumber:
    lea ebx D$CoffLineNumber+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


____________________________________________________________

[CoffSectionSize: ?  CoffPointerToData: ?  CoffPointerToReloc: ?  NumberOfRelocations: ?
CoffPointerToLineNumber: ?  NumberOfLineNumbers: ?]

GetCoffSectionsVariables:

    add esi 8   ;  CoffSectionHeaderName, {': B$ ', 0}

    add esi 4   ; CoffSectionHeaderVirtualSize, {': D$ ', 0}

    add esi 4   ; CoffSectionHeaderRVA, {': D$ ', 0} / VirtualAddress

    move D$CoffSectionSize D$esi
    add esi 4   ; CoffSectionHeaderSize, {': D$ ', 0} / SizeOfRawData

    move D$CoffPointerToData D$esi
    add esi 4   ;  CoffSectionHeaderPointerToData, {': D$ ', 0} / PointerToRawData

    move D$CoffPointerToReloc D$esi
    add esi 4   ; CoffSectionHeaderPointerToReloc, {': D$ ', 0} / PointerToRelocations

    move D$CoffPointerToLineNumber D$esi
    add esi 4   ; CoffSectionHeaderPointerToLinesNumbers, {': D$ ', 0} / PointerToLinenumbers

    move W$NumberOfRelocations W$esi
    add esi 2   ; CoffSectionHeaderNumberOfRelocations, {': W$ ', 0}

    move W$NumberOfLineNumbers W$esi
    add esi 2   ; CoffSectionHeaderNumberOfLinesNumbers, {': W$ ', 0}


  ; CoffSectionHeaderNumberOfRelocations, {': W$ ', 0}

  ; CoffSectionHeaderNumberOfLinesNumbers, {': W$ ', 0}
  ; CoffSectionHeaderCharacteristics, {': D$ ', 0}
ret


[CoffSymbolsPointer: ?  CoffSymbolsNumber: ?
 PointerToSymbolsStringTable: ?  SizeOfSymbolsStringsTable: ?
 EndOfSymbolsStringsTable: ?]

GetImageSymbolsVariables:
    push esi
        mov esi D$CoffSectionBase
        mov eax D$esi+8
        If eax <> 0
            add eax esi | mov D$CoffSymbolsPointer eax
        Else
            mov D$CoffSymbolsPointer 0
        End_If

        move D$CoffSymbolsNumber D$esi+12

        If D$CoffSymbolsPointer <> 0
          ; Search the Pointer to the Symbols Strings Table (18 is the Size of one Symbol Record):
            mov ecx D$CoffSymbolsNumber, eax 18
            mul ecx
            add eax D$CoffSymbolsPointer
            mov D$PointerToSymbolsStringTable eax
            move D$SizeOfSymbolsStringsTable D$eax
            add eax D$eax | mov D$EndOfSymbolsStringsTable eax
        End_If
    pop esi
ret
____________________________________________________________________________________________

Proc WriteAuxSymFmt1DiffLabel:
    Uses esi, ebx, ecx, eax, edx, D$SectionHeaderNumber, D$CoffLineNumber


    push esi

    mov ebx D$FirstSectionPointer

    movzx ecx W$esi-14 ; ecx is pointing to the previous section value
    ; we need to recompute the sectionheader number

    call InitSectionHeaderNumber
    call InitSectionLineNumber

    mov edx edi ; for keeping the path to edi, let's save it at edx

    ; Guga note this is the errros on DX9SDKSampleFramework.lib We have an 0 previous section

    On ecx = 0 , ExitP

    If ecx <> 1 ; Are we pointing to thye 1st section ?
        push ecx

            Do
                call IncrementSectionHeaderNumber
                dec ecx
            Loop_Until ecx = 1

        pop ecx

    End_If

    sub esi 18+8 ; esi is pointing to the beginning of previous Symbol

    push edx

    If ecx <> 1     ; If we are pointing to the 1st Section, we don´ need to add it to the offset of the IMAGE_SECTION_HEADER
        mov eax 40  ; eax is the Size of the IMAGE_SECTON_HEADER
        imul ecx    ; multiply by the section where we must go
        add ebx eax
        sub ebx 40  ; need to subtract from the Size of IMAGE_SECTION_HEADER to get back to the proper path.
    End_If

    pop edx


    pop esi ; restore our value at esi

    ; we must point to the proper Section (IMAGE_SECTION_HEADER)
    add ebx 28 ; we are pointing to PointerToLinenumbers
    mov eax D$ebx ; eax points to the value of linenumber in the ImgSec where we are

    mov edi D$esi ; edi is pointing to the LineNumber value to be compare

    .If eax <> edi ; is the pointer where we are is equal to the value found in the linenumber ?
                   ; Yes, jmp over.


        movzx ecx W$ebx+6 ; ecx is our counter it points to the number of line numbers
        ; No. the values are different. We are dealing with several linenumbers in the same section.
        ; Let's check for their values.

        L0:

            On eax = edi, jmp @OutLoop
            add eax 6 ; we need to add the value in edi (IMAGE_SECTION_HEADER where we are) 6 that is the size of the line number structure
            call IncrementSectionLineNumber
        Loop L0<

@OutLoop:


    .End_If


        mov edi edx ; retore the path to edi

        push esi

        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".LineNumber", 0}
        zCopy CoffLineNumber

        mov D$edi ' - ' | add edi 3

        zCopy IMAGE_FILE_HEADER_Comment1
        call WriteIndiceOnly
        zcopy {" ; Hex Value:  ", 0}
        pop esi

EndP



Proc WriteAuxiliarySymbolsRecordsFormat1Item:
    Argument @Text1, @Text2
    Uses edx,  D$SymbolTableIndex , D$SymbolTableIndex+4

        push esi
            call WriteObjIndice
            zCopy {'ImgAuxFmt1.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi


        mov eax D@Text1

        .If D$eax = 'TagI' ; from "TagIndex" string
            ;call WriteAuxSymFmt5DiffLabel
            call WriteSectionHeaderSymbolConstantIndex
            push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
            lodsd | call WriteEax

        .Else_If D$eax = 'Tota'; From "TotalSize" string
            lodsd | call WriteEax

        .Else_If D$eax+9 = 'Line'; From "PointerToLinenumber" string
            If D$esi <> 0
                call WriteAuxSymFmt1DiffLabel
             End_If
            lodsd | call WriteEax

        .Else_If D$eax+9 = 'Next'; From "PointerToNextFunction" string

            If D$esi <> 0
                call WriteSectionHeaderSymbolConstantIndex
                push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
            End_If

            lodsd | call WriteEax

        .Else_If D$eax = 'Rese'; From "Reserved" string
            xor eax eax
            lodsw | call WriteEax

        .End_If


        mov W$edi CRLF | add edi 2


EndP


;[ImgAuxSym: 'ImgAuxSym', 0] ; Guga Note: Remove this data variable. It is not used anymore


[Fmt1TagIndex: 'TagIndex', 0
 Fmt1TotalSize: 'TotalSize', 0
 Fmt1PointerToLinenumber: 'PointerToLinenumber', 0
 Fmt1PointerToNextFunction: 'PointerToNextFunction', 0
 Fmt1Reserved: 'Reserved', 0]

Proc WriteAuxiliarySymbolsRecordsFormat1:

        push esi
            call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt1.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi ':', W$edi+1 CRLF | add edi 3
        pop esi

        call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1TagIndex, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1TotalSize, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1PointerToLinenumber, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1PointerToNextFunction, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat1Item Fmt1Reserved, {': W$ ', 0}
        sub edi 2

EndP

_________________________________________________________

Proc WriteAuxiliarySymbolsRecordsFormat2Item:
    Argument @Text1, @Text2
    Uses edx, ecx, D$SymbolTableIndex, D$SymbolTableIndex+4

        push esi
            call WriteObjIndice
            zCopy {'ImgAuxFmt2.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi


        mov eax D@Text1


        .If D$eax+5 = 'ved1' ; from "Reserved1" string
            lodsd | call WriteEax

        .Else_If D$eax = 'Line'; From "LineNumber" string
            xor eax eax
            lodsw | call WriteEax
            push esi | zcopy {" ; Base Line Number", 0} | pop esi

        .Else_If D$eax+5 = 'ved2'; From "Reserved2" string
            xor eax eax
            mov ecx 6
            L0:
                lodsb | call WriteEax
                mov W$edi ", " | add edi 2
            Loop L0<
            sub edi 2

        .Else_If D$eax = 'Poin'; From "PointerToNextFunction" string

            If D$esi <> 0
                call WriteSectionHeaderSymbolConstantIndex
                push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
            End_If

            lodsd | call WriteEax

        .Else_If D$eax+5 = 'ved3'; From "Reserved3" string
            xor eax eax
            lodsw | call WriteEax

        .End_If

        mov W$edi CRLF | add edi 2

EndP



[Fmt2Reserved1: 'Reserved1', 0
 Fmt2LineNumber: 'LineNumber', 0
 Fmt2Reserved2: 'Reserved2', 0
 Fmt2PointerToNextFunction: 'PointerToNextFunction', 0
 Fmt2Reserved3: 'Reserved3', 0]

Proc WriteAuxiliarySymbolsRecordsFormat2:

        push esi
            call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt2.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi ':', W$edi+1 CRLF | add edi 3
        pop esi

        call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2Reserved1, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2LineNumber, {': W$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2Reserved2, {': B$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2PointerToNextFunction, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat2Item Fmt2Reserved3, {': W$ ', 0}
        sub edi 2

EndP

_________________________________________________________

Proc WriteAuxSymFmt3CharacteristicEquates:
    Uses esi;, ebx

    ;mov ebx esi

    .If D$esi = &IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY
        zCopy {'&IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY ; Hex Value:  ', 0}
    .Else_If D$esi = &IMAGE_WEAK_EXTERN_SEARCH_LIBRARY
        zCopy {'&IMAGE_WEAK_EXTERN_SEARCH_LIBRARY ; Hex Value:  ', 0}
    .Else_If D$esi = &IMAGE_WEAK_EXTERN_SEARCH_ALIAS
        zCopy {'&IMAGE_WEAK_EXTERN_SEARCH_ALIAS ; Hex Value:  ', 0}
    .End_if


EndP

Proc WriteAuxiliarySymbolsRecordsFormat3Item:
    Argument @Text1, @Text2
    Uses edx, ecx, D$SymbolTableIndex, D$SymbolTableIndex+4

        push esi
            call WriteObjIndice
            zCopy {'ImgAuxFmt3.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi


        mov eax D@Text1


        .If D$eax = 'TagI' ; from "TagIndex" string
            If D$esi <> 0
               call WriteSectionHeaderSymbolConstantIndex
               push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
            End_If

            lodsd | call WriteEax

        .Else_If D$eax = 'Char'; From "Characteristics" string
            ;lodsd | call WriteEax
            call WriteAuxSymFmt3CharacteristicEquates
            lodsd | call WriteEax

        .Else_If D$eax = 'Rese'; From "Reserved" string
            xor eax eax
            mov ecx 10
            L0:
                lodsb | call WriteEax
                mov W$edi ", " | add edi 2
            Loop L0<
            sub edi 2

        .End_If

        mov W$edi CRLF | add edi 2

EndP



[Fmt3TagIndex: 'TagIndex', 0
 Fmt3Characteristics: 'Characteristics', 0
 Fmt3Reserved: 'Reserved', 0]

Proc WriteAuxiliarySymbolsRecordsFormat3:

        push esi
            call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt3.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi ':', W$edi+1 CRLF | add edi 3
        pop esi

        call WriteAuxiliarySymbolsRecordsFormat3Item Fmt3TagIndex, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat3Item Fmt3Characteristics, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat3Item Fmt3Reserved, {': B$ ', 0}
        sub edi 2

EndP


_________________________________________________________


Proc WriteAuxiliarySymbolsRecordsFormat4:

        push esi
            call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt4.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi ':', W$edi+1 CRLF | add edi 3
            call WriteObjIndice
            zCopy {'ImgAuxFmt4.Ind', 0}
            zCopy SymbolTableRecord
            zCopy {'.FileName: B$ ', 0}
        pop esi

        mov edx esi | add edx 18

        .If B$esi = 0
            mov B$edi '0' | inc edi | inc esi
        .Else

        mov B$edi "'" | inc edi
L0:     lodsb
            If al = 0
                dec esi | jmp L1>
            End_If
        stosb | On esi < edx, jmp L0<
L1:     mov B$edi "'" | inc edi

        .End_If

        While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ;mov W$edi CRLF | add edi 2


EndP


__________________________________________________________



Proc WriteAuxSymFmt5DiffLabel:
    Uses esi, ebx, ecx, eax, edx

    mov ebx D$FirstSectionPointer

    movzx ecx W$esi-18+12 ; ecx is pointing to the previous section value
    ; we need to recompute the sectionheader number

    call InitSectionHeaderNumber

    mov edx edi ; for keeping the path to edi, let's save it at edx

    ; Guga note this is the errros on DX9SDKSampleFramework.lib We have an 0 previous section

    On ecx = 0 , jmp C2>>

    If ecx <> 1 ; Are we pointing to thye 1st section ?
        push ecx

            Do
                call IncrementSectionHeaderNumber
                dec ecx
            Loop_Until ecx = 1

        pop ecx

    End_If

    sub esi 18 ; esi is pointing to the beginning of previous Symbol

    push edx

    If ecx <> 1     ; If we are pointing to the 1st Section, we don´ need to add it to the offset of the IMAGE_SECTION_HEADER
        mov eax 40  ; eax is the Size of the IMAGE_SECTON_HEADER
        imul ecx    ; multiply by the section where we must go
        add ebx eax
        sub ebx 40  ; need to subtract from the Size of IMAG_SECTION_HEADER to get back to the proper path.
    End_If

    pop edx

    ; we must point to the proper Section (IMAGE_SECTION_HEADER)

    mov eax ebx ; The 1st String where we are (The Section in IMAGE_SECTION_HEADER)
    mov edi esi ; edi is pointing to the beginning of the previous section. (Our string to be compared)

    mov ecx 8   ;how many bytes in the string...char= one byte
    repe cmpsb
    jne C2>

        mov edi edx ; retore the path to edi

        add eax 20 ; eax is pointing to PointerToRawData on IMAGE_SECTION_HEADER from the structure we label
        mov esi D$eax ; now we point it to esi

        .If D$eax <> 0 ; Is PointerToRawData = 0 ? Is it Virtual data ? No, do next line.

            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".RawDataEnd", 0}

            mov D$edi ' - ' | add edi 3

            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".RawData ; Hex Value:  ", 0}

        .End_If

        ExitP

    C2:

    mov edi edx ; retore the path to edi

EndP


Proc WriteAuxSymFmt5TypeEquates:
    Uses esi, ebx

    mov ebx esi


    .If B$ebx = &IMAGE_COMDAT_SELECT_NODUPLICATES
        zCopy {'&IMAGE_COMDAT_SELECT_NODUPLICATES ; Hex Value:  ', 0}
    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_ANY
        zCopy {'&IMAGE_COMDAT_SELECT_ANY ; Hex Value:  ', 0}
    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_SAME_SIZE
        zCopy {'&IMAGE_COMDAT_SELECT_SAME_SIZE ; Hex Value:  ', 0}
    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_EXACT_MATCH
        zCopy {'&IMAGE_COMDAT_SELECT_EXACT_MATCH ; Hex Value:  ', 0}
    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_ASSOCIATIVE
        zCopy {'&IMAGE_COMDAT_SELECT_ASSOCIATIVE ; Hex Value:  ', 0}
    .Else_If B$ebx = &IMAGE_COMDAT_SELECT_LARGEST
        zCopy {'&IMAGE_COMDAT_SELECT_LARGEST ; Hex Value:  ', 0}
    .End_if


EndP


[Fmt5Length: 'Length', 0
 Fmt5NumberOfRelocations: 'NumberOfRelocations', 0
 Fmt5NumberOfLinenumbers: 'NumberOfLinenumbers', 0
 Fmt5CheckSum: 'CheckSum', 0
 Fmt5Number: 'Number', 0
 Fmt5Selection: 'Selection', 0
 Fmt5Reserved: 'Reserved', 0]

Proc WriteAuxiliarySymbolsRecordsFormat5Item:
    Argument @Text1, @Text2
    Uses edx, ecx;, D$SymbolTableIndex, D$SymbolTableIndex+4

        push esi
            call WriteObjIndice
            zCopy {'ImgAuxFmt5.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi


                mov eax D@Text1

                .If D$eax = 'Leng' ; from "Length" string
                    call WriteAuxSymFmt5DiffLabel
                    lodsd | call WriteEax

                .Else_If D$eax+8 = 'Relo'; From "NumberOfRelocations" string
                    xor eax eax
                    lodsw | call WriteEax

                .Else_If D$eax+8 = 'Line'; From "NumberOfLinenumbers" string
                    xor eax eax
                    lodsw | call WriteEax

                .Else_If D$eax = 'Chec'; From "CheckSum" string
                    lodsd | call WriteEax

                .Else_If D$eax = 'Numb'; From "Number" string

                    xor eax eax
                    lodsw | call WriteEax

                .Else_If D$eax = 'Sele'; From "Selection" string
                    call WriteAuxSymFmt5TypeEquates
                    xor eax eax
                    lodsb | call WriteEax

                .Else_If D$eax = 'Rese'; From "Reserved" string
                    xor eax eax
                    mov ecx 3
                    L0:
                        xor eax eax
                        lodsb | call WriteEax
                        mov W$edi ", " | add edi 2
                    Loop L0<
                    sub edi 2

                .End_If


        mov W$edi CRLF | add edi 2


EndP


Proc WriteAuxiliarySymbolsRecordsFormat5:

        push esi
            call WriteObjIndice
            zCopy {B$ 'ImgAuxFmt5.Ind', 0}
            zCopy SymbolTableRecord
            mov B$edi ':', W$edi+1 CRLF | add edi 3
        pop esi

        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Length, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5NumberOfRelocations, {': W$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5NumberOfLinenumbers, {': W$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5CheckSum, {': D$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Number, {': W$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Selection, {': B$ ', 0}
        call WriteAuxiliarySymbolsRecordsFormat5Item Fmt5Reserved, {': B$ ', 0}
        sub edi 2

EndP


_______________________________________________________________


; Examples: frmMainNET.obj, program_PadraoCOM.obj, Hello World_PAdraoNET.obj
; GIven to me (Guga) by Fabricio

Proc WriteAuxiliarySymbolsRecordsFormatUnknown:
    ;Argument @Pointer

    push esi
        call WriteObjIndice
        zCopy {'ImgAuxSym', 0}
        zCopy SymbolTableRecord
        zCopy {'FormatUnknwon: B$ ', 0}
    pop esi

;        mov esi D@Pointer, ecx 18
        mov ecx 18

L0:     lodsb | and eax 0FF | call WriteEax
        mov W$edi ', ' | add edi 2 | loop L0<
        sub edi 2
        ;mov B$edi-2 "'" | dec edi
EndP

_______________________________________________________________

Proc WriteAuxiliarySymbolsRecords:
    Arguments @Number
    Uses ecx, ebx

    .Do

        call WriteSymbolConstantIndex
        call IncrementSymbolIndexRecord

        mov W$edi CRLF | add edi 2
        mov B$edi '[' | inc edi

        call IncrementSymbolTableRecord

            ..If D$SymbolStorageClass = &IMAGE_SYM_CLASS_EXTERNAL
            ; example at libgccguga.a

                .If B$esi-4 = (&IMAGE_SYM_DTYPE_FUNCTION shl 4) + &IMAGE_SYM_TYPE_NULL ; Type1Complex = 020
                    ;If W$esi-6 >= 0  ;
                    ; SectionNumber >= 0. Once again the documentation lied. We can have a 0 section number
                        call WriteAuxiliarySymbolsRecordsFormat1
                    ;End_If
                .End_If

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_FUNCTION
            ; example at shit.obj
                call WriteAuxiliarySymbolsRecordsFormat2

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_WEAK_EXTERNAL
            ; example at stats.lib ; Libdc.lib
                .If W$esi-6 = &IMAGE_SYM_UNDEFINED  ; SectionNumber = &IMAGE_SYM_UNDEFINED
                    If D$esi-10 = 0 ; Value member = 0
                        call WriteAuxiliarySymbolsRecordsFormat3
                    End_If
                .End_If

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_FILE
                call WriteAuxiliarySymbolsRecordsFormat4

            ..Else_If D$SymbolStorageClass = &IMAGE_SYM_CLASS_STATIC
                call WriteAuxiliarySymbolsRecordsFormat5

            ..Else
                call WriteAuxiliarySymbolsRecordsFormatUnknown

            ..End_If

        mov B$edi ']', D$edi+1 CRLF2 | add edi 5
        dec D$CoffSymbolsNumber ; decrement our counter from the total amount symbols
        dec D@Number ; decrement the total amount of Auxiliary Symbols

    .Loop_Until D@Number = 0

EndP

____________________________________________________________________________________________


[SymbolTableTitle: "
_________________________________________________________

; Symbols Table Structure IMAGE_SYMBOL
_________________________________________________________


", 0
SymbolTableShortName: 'NameShort', 0
SymbolTableLongNameZero: 'NameZero', 0
SymbolTableLongNameOffset: 'NameLongOffset', 0
SymbolTableValue: 'Value', 0
SymbolTableSectionNumber: 'SectionNumber', 0
SymbolTableTypeComplex: 'Type1Complex', 0
SymbolTableTypeBase: 'Type1Base', 0
SymbolTableStorageClass: 'StorageClass', 0
SymbolTableNumberOfAuxSymbols: 'NumberOfAuxSymbols', 0]


;[PointerToSymbolsRecords: ?]

[SymbolTableRecord: '000001', 0]


InitSymbolTableRecord:
    mov D$SymbolTableRecord '0000', D$SymbolTableRecord+4 '01'
ret


IncrementSymbolTableRecord:
    lea ebx D$SymbolTableRecord+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


Proc WriteImageSymbolTableFieldName:
    Argument @String1 @String2
    Uses esi, ecx, ebx

   ; If D@String2 < SymbolTableShortName1
   ;      mov eax D@String1, ebx D@String2
   ;      int3
   ; End_If

        call WriteObjIndice
        zCopy D@String1
        zCopy SymbolTableRecord
        zCopy D@String2
EndP


;;
&IMAGE_SYM_UNDEFINED 0 Symbol record is not yet assigned a section.
    If the value is 0 this indicates a references to an external symbol 
    defined elsewhere. If the value is non-zero this is a common symbol 
    with a size specified by the value. 
&IMAGE_SYM_ABSOLUTE -1 The symbol has an absolute (non-relocatable) value 
    and is not an address. 
&IMAGE_SYM_DEBUG -2 The symbol provides general type or debugging information 
    but does not correspond to a section. Microsoft tools use this setting 
    along with .file records (storage class FILE). 
;;

WriteRelocSectionNumberEquate:
    push esi
        ;mov ebx D$PointerToSymbolsRecords

        If W$esi = &IMAGE_SYM_UNDEFINED
            zCopy {'&IMAGE_SYM_UNDEFINED ; Hex Value:  ', 0}
        Else_If W$esi = &IMAGE_SYM_ABSOLUTE
            zCopy {'&IMAGE_SYM_ABSOLUTE ; Hex Value:  ', 0}
        Else_If W$esi = &IMAGE_SYM_DEBUG
            zCopy {'&IMAGE_SYM_DEBUG ; Hex Value:  ', 0}
        End_If
    pop esi
ret

;;
WriteRelocSectionNumberEquate_Old:
    push esi
        mov ebx D$PointerToSymbolsRecords

        If W$ebx = &IMAGE_SYM_UNDEFINED
            zCopy {' ; &IMAGE_SYM_UNDEFINED', 0}
        Else_If W$ebx = &IMAGE_SYM_ABSOLUTE
            zCopy {' ; &IMAGE_SYM_ABSOLUTE', 0}
        Else_If W$ebx = &IMAGE_SYM_DEBUG
            zCopy {' ; &IMAGE_SYM_DEBUG', 0}
        End_If
    pop esi
ret
;;

;;
The most significative byte equates are:

&IMAGE_SYM_DTYPE_NULL
&IMAGE_SYM_DTYPE_POINTER
&IMAGE_SYM_DTYPE_FUNCTION
&IMAGE_SYM_DTYPE_ARRAY


The less signfiicative byte are:

&IMAGE_SYM_TYPE_NULL
&IMAGE_SYM_TYPE_VOID
&IMAGE_SYM_TYPE_CHAR
&IMAGE_SYM_TYPE_SHORT
&IMAGE_SYM_TYPE_INT
&IMAGE_SYM_TYPE_LONG
&IMAGE_SYM_TYPE_FLOAT
&IMAGE_SYM_TYPE_DOUBLE
&IMAGE_SYM_TYPE_STRUCT
&IMAGE_SYM_TYPE_UNION
&IMAGE_SYM_TYPE_ENUM
&IMAGE_SYM_TYPE_MOE
&IMAGE_SYM_TYPE_BYTE
&IMAGE_SYM_TYPE_WORD
&IMAGE_SYM_TYPE_UINT
&IMAGE_SYM_TYPE_DWORD
;;

[RelocTypeD: ?  RelocType: ?]

;;
        .If_And eax >= 32, eax <= 65535 ; Check for Corrupted library. Reserved Bits are not 0

            call LibScan_ErrManager IMP_OBJ_RESBIT_ERR

            If eax = &FALSE ; The user pressed No
                ExitP

            Else_If eax = &TRUE ; The user pressed Yes. We will Zero all reserved bits for him continue the parsing.
                mov eax D@Flags ; Restore the original value of eax to be fixed
                mov ah 0 ; Clear High Bits Flags
                btr eax 5 | btr eax 6 | btr eax 7 ; Clear Bits 5 to 7

            End_If

        .End_If


;;
;WriteRelocTypeEquates:
Proc WriteSymbolTypeEquates:
    Uses esi, ebx

        .If B$esi > 63 ; Check for Corrupted library. The actual maximum value is 63 (03F)

            call LibScan_ErrManager IMP_OBJ_RESBIT_ERR

            If eax = &FALSE ; The user pressed No
                ExitP

            Else_If eax = &TRUE ; The user pressed Yes. We will Zero all reserved bits for him continue the parsing.

                ; Clear all high flags, returning only in the targeting Byte
                xor eax eax | lodsb | btr eax 6 | btr eax 7 ; Clear Bits 6 and , that are what exceed the limit of 63
                mov ebx eax
            End_If

        .Else

            xor eax eax | lodsb | mov ebx eax

        .End_If

        and eax 0F | shr ebx 4

        ;mov D$RelocType eax, D$RelocTypeD ebx

        If ebx = &IMAGE_SYM_DTYPE_NULL
            zCopy {'(&IMAGE_SYM_DTYPE_NULL shl 4)', 0}
        Else_If ebx = &IMAGE_SYM_DTYPE_POINTER
            zCopy {'(&IMAGE_SYM_DTYPE_POINTER shl 4)', 0}
        Else_If ebx = &IMAGE_SYM_DTYPE_FUNCTION
            zCopy {'(&IMAGE_SYM_DTYPE_FUNCTION shl 4)', 0}
        Else_If ebx = &IMAGE_SYM_DTYPE_ARRAY
            zCopy {'(&IMAGE_SYM_DTYPE_ARRAY shl 4)', 0}
        End_If

        .If eax = &IMAGE_SYM_TYPE_NULL
            zCopy {' + &IMAGE_SYM_TYPE_NULL', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_VOID
            zCopy {' + &IMAGE_SYM_TYPE_VOID', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_CHAR
            zCopy {' + &IMAGE_SYM_TYPE_CHAR', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_SHORT
            zCopy {' + &IMAGE_SYM_TYPE_SHORT', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_INT
            zCopy {' + &IMAGE_SYM_TYPE_INT', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_LONG
            zCopy {' + &IMAGE_SYM_TYPE_LONG', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_FLOAT
            zCopy {' + &IMAGE_SYM_TYPE_FLOAT', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_DOUBLE
            zCopy {' + &IMAGE_SYM_TYPE_DOUBLE', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_STRUCT
            zCopy {' + &IMAGE_SYM_TYPE_STRUCT', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_UNION
            zCopy {' + &IMAGE_SYM_TYPE_UNION', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_ENUM
            zCopy {' + &IMAGE_SYM_TYPE_ENUM', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_MOE
            zCopy {' + &IMAGE_SYM_TYPE_MOE', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_BYTE
            zCopy {' + &IMAGE_SYM_TYPE_BYTE', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_WORD
            zCopy {' + &IMAGE_SYM_TYPE_WORD', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_UINT
            zCopy {' + &IMAGE_SYM_TYPE_UINT', 0}
        .Else_If eax = &IMAGE_SYM_TYPE_DWORD
            zCopy {' + &IMAGE_SYM_TYPE_DWORD', 0}
        .End_If

        zCopy {' ; Hex Value:  ', 0}

EndP


;;
&IMAGE_SYM_CLASS_END_OF_FUNCTION -1 Special symbol representing end of function, for debugging purposes. 
&IMAGE_SYM_CLASS_NULL 0 No storage class assigned. 
&IMAGE_SYM_CLASS_AUTOMATIC 1 Automatic (stack) variable. The Value field specifies stack frame offset. 
&IMAGE_SYM_CLASS_EXTERNAL 2 Used by Microsoft tools for external symbols. The Value field indicates the size if the section number is IMAGE_SYM_UNDEFINED (0). If the section number is not 0, then the Value field specifies the offset within the section. 
&IMAGE_SYM_CLASS_STATIC 3 The Value field specifies the offset of the symbol within the section. If the Value is 0, then the symbol represents a section name. 
&IMAGE_SYM_CLASS_REGISTER 4 Register variable. The Value field specifies register number. 
&IMAGE_SYM_CLASS_EXTERNAL_DEF 5 Symbol is defined externally. 
&IMAGE_SYM_CLASS_LABEL 6 Code label defined within the module. The Value field specifies the offset of the symbol within the section. 
&IMAGE_SYM_CLASS_UNDEFINED_LABEL 7 Reference to a code label not defined. 
&IMAGE_SYM_CLASS_MEMBER_OF_STRUCT 8 Structure member. The Value field specifies nth member. 
&IMAGE_SYM_CLASS_ARGUMENT 9 Formal argument (parameter)of a function. The Value field specifies nth argument. 
&IMAGE_SYM_CLASS_STRUCT_TAG 10 Structure tag-name entry. 
&IMAGE_SYM_CLASS_MEMBER_OF_UNION 11 Union member. The Value field specifies nth member. 
&IMAGE_SYM_CLASS_UNION_TAG 12 Union tag-name entry. 
&IMAGE_SYM_CLASS_TYPE_DEFINITION 13 Typedef entry. 
&IMAGE_SYM_CLASS_UNDEFINED_STATIC 14 Static data declaration. 
&IMAGE_SYM_CLASS_ENUM_TAG 15 Enumerated type tagname entry. 
&IMAGE_SYM_CLASS_MEMBER_OF_ENUM 16 Member of enumeration. Value specifies nth member. 
&IMAGE_SYM_CLASS_REGISTER_PARAM 17 Register parameter. 
&IMAGE_SYM_CLASS_BIT_FIELD 18 Bit-field reference. Value specifies nth bit in the bit field. 
&IMAGE_SYM_CLASS_BLOCK 100 A .bb (beginning of block) or .eb (end of block) record. Value is the relocatable address of the code location. 
&IMAGE_SYM_CLASS_FUNCTION 101 Used by Microsoft tools for symbol records that define the extent of a function: begin function (named .bf), end function (.ef), and lines in function (.lf). For .lf records, Value gives the number of source lines in the function. For .ef records, Value gives the size of function code. 
&IMAGE_SYM_CLASS_END_OF_STRUCT 102 End of structure entry. 
&IMAGE_SYM_CLASS_FILE 103 Used by Microsoft tools, as well as traditional COFF format, for the source-file symbol record. The symbol is followed by auxiliary records that name the file. 
&IMAGE_SYM_CLASS_SECTION 104 Definition of a section (Microsoft tools use STATIC storage class instead). 
&IMAGE_SYM_CLASS_WEAK_EXTERNAL 105 Weak external. See Section 5.5.3, “Auxiliary Format 3: Weak Externals,” for more information. 
;;



[SymbolStorageClass: D$ 0]

WriteRelocStorageClassEquate:
    push esi

        movzx eax B$esi | mov D$SymbolStorageClass eax

        .If eax = &IMAGE_SYM_CLASS_ARGUMENT
            zCopy {'&IMAGE_SYM_CLASS_ARGUMENT ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_AUTOMATIC
            zCopy {'&IMAGE_SYM_CLASS_AUTOMATIC ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_BIT_FIELD
            zCopy {'&IMAGE_SYM_CLASS_BIT_FIELD ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_BLOCK
            zCopy {'&IMAGE_SYM_CLASS_BLOCK ; Hex Value:  ', 0}
         .Else_If eax = &IMAGE_SYM_CLASS_END_OF_FUNCTION
            zCopy {'&IMAGE_SYM_CLASS_END_OF_FUNCTION ; Hex Value:  ', 0}
         .Else_If eax = &IMAGE_SYM_CLASS_END_OF_STRUCT
            zCopy {'&IMAGE_SYM_CLASS_END_OF_STRUCT ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_ENUM_TAG
            zCopy {'&IMAGE_SYM_CLASS_ENUM_TAG ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_EXTERNAL
            zCopy {'&IMAGE_SYM_CLASS_EXTERNAL ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_EXTERNAL_DEF
            zCopy {'&IMAGE_SYM_CLASS_EXTERNAL_DEF ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_FAR_EXTERNAL
            zCopy {'&IMAGE_SYM_CLASS_FAR_EXTERNAL ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_FILE
            zCopy {'&IMAGE_SYM_CLASS_FILE ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_FUNCTION
            zCopy {'&IMAGE_SYM_CLASS_FUNCTION ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_LABEL
            zCopy {'&IMAGE_SYM_CLASS_LABEL ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_MEMBER_OF_ENUM
            zCopy {'&IMAGE_SYM_CLASS_MEMBER_OF_ENUM ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_MEMBER_OF_STRUCT
            zCopy {'&IMAGE_SYM_CLASS_MEMBER_OF_STRUCT ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_MEMBER_OF_UNION
            zCopy {'&IMAGE_SYM_CLASS_MEMBER_OF_UNION ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_NULL
            zCopy {'&IMAGE_SYM_CLASS_NULL ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_REGISTER
            zCopy {'&IMAGE_SYM_CLASS_REGISTER ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_REGISTER_PARAM
            zCopy {'&IMAGE_SYM_CLASS_REGISTER_PARAM ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_SECTION
            zCopy {'&IMAGE_SYM_CLASS_SECTION ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_STATIC
            zCopy {'&IMAGE_SYM_CLASS_STATIC ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_STRUCT_TAG
            zCopy {'&IMAGE_SYM_CLASS_STRUCT_TAG ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_TYPE_DEFINITION
            zCopy {'&IMAGE_SYM_CLASS_TYPE_DEFINITION ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_UNDEFINED_LABEL
            zCopy {'&IMAGE_SYM_CLASS_UNDEFINED_LABEL ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_UNDEFINED_STATIC
            zCopy {'&IMAGE_SYM_CLASS_UNDEFINED_STATIC ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_UNION_TAG
            zCopy {'&IMAGE_SYM_CLASS_UNION_TAG ; Hex Value:  ', 0}
        .Else_If eax = &IMAGE_SYM_CLASS_WEAK_EXTERNAL
            zCopy {'&IMAGE_SYM_CLASS_WEAK_EXTERNAL ; Hex Value:  ', 0}
        .End_If

    pop esi
ret



Proc WriteSymbolTableHeaderItem:
    Argument @Text1, @Text2
    Uses edx;, eax

        push esi
            call WriteObjIndice
            zCopy {'ImgSym', 0}
            zCopy SymbolTableRecord
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi


                mov eax D@Text1

                ..If D$eax+4 = 'Shor' ; from "NameShort" string

                    mov edx esi | add edx 8
                    mov B$edi "'" | inc edi
L0:                 lodsb
                    If al = 0
                        dec esi | jmp L1>
                    End_If
                    stosb | On esi < edx, jmp L0<
L1:                 mov B$edi "'" | inc edi
                    While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

                ..Else_If D$eax+4 = 'Zero'; From "NameZero" string
                    lodsd | call WriteEax

                ..Else_If D$eax+8 = 'Offs'; From "NameLongOffset" string

                    call WriteSectionHeaderPointerToStringTableDiffLabel
                    lodsd | call WriteEax

                ..Else_If D$eax = 'Valu'; From "Value" string

                    ;call WriteCharacteristicsEquates
                    lodsd | call WriteEax

                        .If D$esi-4 <> 0 ; If the Value member is not Zero, do next line
                            If B$esi+4 = &IMAGE_SYM_CLASS_SECTION ; If StorageClass member is IMAGE_SYM_CLASS_SECTION, do next line
                                push esi
                                push D$esi-4
                                push eax
                                mov eax D$esi-4
                                call WriteCharacteristicsEquates
                                pop eax
                                pop D$esi-4
                                pop esi

                            End_If
                        .End_If

                ..Else_If D$eax = 'Sect'; From "SectionNumber" string
                    call WriteRelocSectionNumberEquate
                    xor eax eax
                    lodsw | call WriteEax

                ..Else_If D$eax = 'Type'; From "Type1Complex" string
                    call WriteSymbolTypeEquates
                    xor eax eax
                    lodsb | call WriteEax

                ..Else_If D$eax = '1Bas'; From "Type1Base" string
                    call WriteSymbolTypeEquates
                    xor eax eax
                    lodsb | call WriteEax

                ..Else_If D$eax = 'Stor'; From "StorageClass" string

                    call WriteRelocStorageClassEquate
                    xor eax eax
                    lodsb | call WriteEax

                ..Else_If D$eax+8 = 'AuxS'; From "NumberOfAuxSymbols" string
                    xor eax eax
                    lodsb | call WriteEax

;;
                ..Else_If D$eax = 'Misc'; From "MiscVirtualSize" string

                    call WriteSectionHeaderVirtualSizeDiffLabel
                    push esi | zcopy {" ; Hex Value:  ", 0} | pop esi
;;

                ;..Else

                    ;lodsd | call WriteEax

                ..End_If


        mov W$edi CRLF | add edi 2
EndP

____________________________________________________________

[SymbolTableIndex: '000000', 0]


InitSymbolIndexRecord:
    mov D$SymbolTableIndex '0000', D$SymbolTableIndex+4 '00'
ret


IncrementSymbolIndexRecord:
    lea ebx D$SymbolTableIndex+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

Proc WriteSymbolConstantIndex:
    Uses esi, ecx

            mov B$edi '[' | inc edi
            call WriteObjIndice

            ; Fix the label to show in Caps and replace the '." with an "_"
            ; The 1st char in "Obj000000." is in Caps, so we don't need to overwrite the "O" char

            push edi
                mov B$edi-1 '_', W$edi-9 'BJ'
            pop edi

            zCopy {'SYMBOLINDEX', 0}
            zCopy SymbolTableIndex
            mov B$edi ' ' | inc edi

            mov esi SymbolTableIndex

            .If_And D$esi = '0000', D$SymbolTableIndex+4 = '00'

                mov B$edi '0' | inc edi
            .Else

                mov ecx 6 ; Size of the SymbolTable String
            L0:

                While B$esi <> '0'

                    L1:
                        movsb
                    Loop L1<
                    On ecx = 0, jmp @Out

                End_While

                inc esi
            Loop L0<

            .End_If
@Out:
            zCopy {B$ "] ; Symbol Index Constant used in IMAGE_RELOCATION Structure.", W$ CRLF, 0}


EndP

____________________________________________________________


; What do we need here ?
; CoffSymbolsPointer = Pointer to the Coff symbols Table
; CoffSymbolsNumber = Amount of Symbols Table structures

; The values where from GetCoffIMAGE_FILE_HEADER

[AuxSymbolCount: D$ 0]

[StringtableCheckFlag: D$ &FALSE]

[SymbolTableNameLongMessage: B$ "________________________________________________________________________

; Name Label indirectly addressed at NameLongOffset.
; The name of the Symbol is: ", 0]


WriteImageSymbolTable:

    ; Initialise the String Table Checking.

    mov D$StringtableCheckFlag &FALSE

    call InitSymbolTableRecord
    call InitSymbolIndexRecord

    zCopy SymbolTableTitle
    mov esi D$CoffSectionBase
    add esi D$CoffSymbolsPointer

.Do

    If B$esi = 0

        push esi
            zCopy SymbolTableNameLongMessage
        pop esi

            mov ecx D$PointerToStringTable

        push esi
            add ecx D$esi+4 | mov esi ecx
            While B$esi <> 0 | movsb | End_While
            zCopy {W$ CRLF, B$ "________________________________________________________________________", D$ CRLF2 0}
        pop esi

    End_If

        call WriteSymbolConstantIndex
        call IncrementSymbolIndexRecord ; Increment it for the next Normal Symbol or Auxiliary Symbol
        mov W$edi CRLF | add edi 2

    .If_And W$esi+12 = &IMAGE_SYM_UNDEFINED, B$esi+16 = &IMAGE_SYM_CLASS_EXTERNAL
        push esi
        zCopy {B$ "; This symbol is defined externally. It can be a Dll function or a runtime function", D$ CRLF2 0}
        pop esi

    .Else_If B$esi+16 = &IMAGE_SYM_CLASS_SECTION

        If W$esi+12 = &IMAGE_SYM_UNDEFINED
            push esi
            zCopy {B$ "; This symbol represents the Section Name. It's not used because it is not defined in any Section." , W$ CRLF, B$ "; However, the value member is the Characteristics of a Section if existant." , D$ CRLF2 0}
            pop esi
        Else
            push esi
            zCopy {B$ "; This symbol represents the Section Name. The value member is the Characteristics of the Section," , W$ CRLF, B$ "; but it may not be the same as what is found on the Section Structure.", D$ CRLF2 0}
            pop esi
        End_If
    .End_If

        push esi
            mov B$edi '[' | inc edi
            call WriteObjIndice
            zCopy {'ImgSym', 0}
            zCopy SymbolTableRecord
            mov B$edi ':', W$edi+1 CRLF | add edi 3
        pop esi

    .If B$esi = 0

        If D$esi+4 <> 0 ; need this flag to check if we have String Table pointer or not.
            mov D$StringtableCheckFlag &TRUE
        End_If

        call WriteSymbolTableHeaderItem SymbolTableLongNameZero, {': D$ ', 0}
        call WriteSymbolTableHeaderItem SymbolTableLongNameOffset, {': D$ ', 0}

    .Else

        call WriteSymbolTableHeaderItem SymbolTableShortName, {': B$ ', 0}

    .End_If


    call WriteSymbolTableHeaderItem SymbolTableValue, {': D$ ', 0}
    call WriteSymbolTableHeaderItem SymbolTableSectionNumber, {': W$ ', 0}
    call WriteSymbolTableHeaderItem SymbolTableTypeComplex, {': B$ ', 0}
    call WriteSymbolTableHeaderItem SymbolTableTypeBase, {': B$ ', 0}
    call WriteSymbolTableHeaderItem SymbolTableStorageClass, {': B$ ', 0}
    call WriteSymbolTableHeaderItem SymbolTableNumberOfAuxSymbols, {': B$ ', 0}

    sub edi 2
    mov B$edi ']' | inc edi ; Close the bracket

    dec D$CoffSymbolsNumber ; decrement our counter

    If B$esi-1 <> 0 ; Is Auxiliary Value = 0 ? No, do next line.

        movzx eax B$esi-1 | mov D$AuxSymbolCount eax
        mov D$edi CRLF2 | add edi 4
        call WriteAuxiliarySymbolsRecords eax

    Else
        ; To prevent adding extra Paragraphs (8 Bytes) we need this else macro, because the end of the
        ; auxiliary record before, already is adding 4 bytes (CRLF2) on exit.
        mov D$edi CRLF2 | add edi 4

    End_If

        call IncrementSymbolTableRecord

    .Loop_Until D$CoffSymbolsNumber = 0

ret


[PointerToStringTable: D$ 0]

; This function is used to calculate the Pointer to the String table. All we are doing is compute
; the real size of the Symbols Structures (With the Auxiliary Symbols) and adding it to the Coff Base

GetStringTablePointer:
    pushad

    mov esi D$CoffSectionBase
    add esi D$CoffSymbolsPointer
    mov ecx D$CoffSymbolsNumber

    .Do
        add esi 18 ; The size of each IMAGE_SYMBOL Structure
        dec ecx

        If B$esi-1 <> 0 ; Is Auxiliary Value = 0 ? No, do next line.
            movzx eax B$esi-1; | mov D$AuxSymbolCount eax The amount of auxiliary symbols found on this Symbol structure
                ;call WriteAuxiliarySymbolsRecords eax

            Do
                add esi 18 ; Add to the size of each Auxiliary Symbol
                dec ecx ;decrement our counter from the total amount of Symbols
                dec eax ; decrement the total amount of Auxiliary Symbols
            Loop_Until eax = 0 ; Did we finished all Auxiliary symbols for this Symbol ?

        End_If

    .Loop_Until ecx = 0

    mov D$PointerToStringTable esi

    popad

ret



[StringTableRecord: '000001', 0]


InitStringTableRecord:
    mov D$StringTableRecord '0000', D$StringTableRecord+4 '01'
ret


IncrementStringTableRecord:
    lea ebx D$StringTableRecord+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


[StringTableTitle: "
_________________________________________________________

; Strings Table
_________________________________________________________


", 0]


WriteImageSymbolStringsTable:

    ; Initialize the String Record Counter

    call InitStringTableRecord

    push esi
        zCopy StringTableTitle
        mov B$edi '[' | inc edi
        call WriteObjIndice
        zCopy {'StringTableSize: D$ ', 0}

        call WriteObjIndice
        zCopy {'StringDataEnd - ', 0}
        call WriteObjIndice
        zCopy {'StringTableSize] ; Hex Value:  ', 0}

    pop esi

    mov edx esi | add edx D$esi ; We will use edx as a counter delimiter for the strings

    lodsd | call WriteEax
    push esi | zCopy {D$ CRLF2, B$ "; Strings Array", D$ CRLF2 0} | pop esi


    push esi
        mov B$edi '[' | inc edi
        call WriteObjIndice
        zCopy {'StringData', 0}
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi


   .Do

        push esi
            call WriteObjIndice
            zCopy {'StringData', 0}
            zCopy StringTableRecord
            zCopy {": B$ '", 0}
        pop esi


        While B$esi <> 0 | movsb | End_While | inc esi | On B$esi = 0, mov edx 0

        mov D$edi "', 0" | add edi 4
        mov W$edi CRLF | add edi 2

        call IncrementStringTableRecord

    .Loop_Until esi >= edx

    sub edi 2

       push esi
            mov W$edi CRLF | add edi 2
            call WriteObjIndice
            zCopy {B$ "StringDataEnd: ]", W$ CRLF, 0}
        pop esi

ret


____________________________________________________________________________________________

[CoffSectionBase: ?]

GetCoffListing:
    mov D$CoffSectionBase esi

    call GetCoffIMAGE_FILE_HEADER

    If D$SizeOfOptionalHeaderInObj <> 0
        push esi
            call GetCoffIMAGE_OPTIONAL_HEADER
        pop esi
        add esi D$SizeOfOptionalHeaderInObj
    End_If

    call InitSectionHeaderNumber
    mov ecx D$ObjNumberOfSections

    .If ecx <> 0 ; If Section NUmber is not 0, do all checkings

        mov D$FirstSectionPointer esi

        push esi, ecx
        L0:
            push ecx
            call GetCoffSectionHeader
            call IncrementSectionHeaderNumber
            pop ecx
        loop L0<
        pop ecx, esi

        call InitSectionHeaderNumber
        call InitSectionRelocNumber
        mov esi D$FirstSectionPointer

        If D$ObjNumberOfSections <> 0
            L0:
                push ecx, esi
                call IdentifyRawDataType
                call GetCoffSectionsVariables
                call WriteCoffSectionData
                On W$NumberOfRelocations <> 0, call WriteCoffReloc
                On W$NumberOfLineNumbers <> 0, call WriteCoffLineNumber
                call IncrementSectionHeaderNumber
                pop esi, ecx
                add esi 40
            loop L0<
        End_If

        If D$CoffSymbolsPointer <> 0
            call GetStringTablePointer
            call WriteImageSymbolTable
            ; When we don´t have any StringTable pointers, we bypass the next call
            On D$StringtableCheckFlag = &TRUE, call WriteImageSymbolStringsTable
        End_If

    .Else ; Else, if section number is 0, perform only the checkings for the Symbol Pointer. Only this because the Optional
          ; Header check were already performed. This was because a problem like this in corelibc.lib (Pelles file)

        If D$CoffSymbolsPointer <> 0
            call GetStringTablePointer
            call WriteImageSymbolTable
            ; When we don´t have any StringTable pointers, we bypass the next call
            On D$StringtableCheckFlag = &TRUE, call WriteImageSymbolStringsTable
        End_If


    .End_If

ret
____________________________________________________________________________________________

;;
  Values set in 'FirstSectionPointer', 'CoffSectionSize', 'CoffPointerToData'
  by 'GetCoffSectionsVariables'.
;;


__________________________________________________________________

Proc WriteRawDataLinkerDirectiveReport:
    Uses esi, eax, edx

        mov edx esi | add edx ecx

        .If B$esi = 0
            mov B$edi '0' | inc edi | inc esi
        .Else

            mov D$edi '    ' | add edi 4
L0:         lodsb
            If al = 0
                dec esi | jmp L1>

            Else_If al = ' '
                mov W$edi CRLF, D$edi+2 '    ' | add edi 6

            Else
                stosb
            End_If
            On esi < edx, jmp L0<

L1:

        .End_If
;ret
EndP

__________________________________________________________________

;WriteRelocStorageClassEquate

[DebugSIndexValue: D$ 0]

WriteDebugSIndexEquate:
    push esi

        movzx eax W$esi | mov D$DebugSIndexValue eax

        .If eax = &S_COMPILE
            zCopy {'&S_COMPILE ; Hex Value:  ', 0}
        .Else_If eax = &S_REGISTER
            zCopy {'&S_REGISTER ; Hex Value:  ', 0}
        .Else_If eax = &S_CONSTANT
            zCopy {'&S_CONSTANT ; Hex Value:  ', 0}
        .Else_If eax = &S_UDT
            zCopy {'&S_UDT ; Hex Value:  ', 0}
        .Else_If eax = &S_SSEARCH
            zCopy {'&S_SSEARCH ; Hex Value:  ', 0}
        .Else_If eax = &S_END
            zCopy {'&S_END ; Hex Value:  ', 0}
        .Else_If eax = &S_SKIP
            zCopy {'&S_SKIP ; Hex Value:  ', 0}
        .Else_If eax = &S_CVRESERVE
            zCopy {'&S_CVRESERVE ; Hex Value:  ', 0}
        .Else_If eax = &S_OBJNAME
            zCopy {'&S_OBJNAME ; Hex Value:  ', 0}
        .Else_If eax = &S_ENDARG
            zCopy {'&S_ENDARG ; Hex Value:  ', 0}
        .Else_If eax = &S_COBOLUDT
            zCopy {'&S_COBOLUDT ; Hex Value:  ', 0}
        .Else_If eax = &S_MANYREG
            zCopy {'&S_MANYREG ; Hex Value:  ', 0}
        .Else_If eax = &S_RETURN
            zCopy {'&S_RETURN ; Hex Value:  ', 0}
        .Else_If eax = &S_ENTRYTHIS
            zCopy {'&S_ENTRYTHIS ; Hex Value:  ', 0}
        .Else_If eax = &S_BPREL16
            zCopy {'&S_BPREL16 ; Hex Value:  ', 0}
        .Else_If eax = &S_LDATA16
            zCopy {'&S_LDATA16 ; Hex Value:  ', 0}
        .Else_If eax = &S_GDATA16
            zCopy {'&S_GDATA16 ; Hex Value:  ', 0}
        .Else_If eax = &S_PUB16
            zCopy {'&S_PUB16 ; Hex Value:  ', 0}
        .Else_If eax = &S_LPROC16
            zCopy {'&S_LPROC16 ; Hex Value:  ', 0}
        .Else_If eax = &S_GPROC16
            zCopy {'&S_GPROC16 ; Hex Value:  ', 0}
        .Else_If eax = &S_THUNK16
            zCopy {'&S_THUNK16 ; Hex Value:  ', 0}
        .Else_If eax = &S_BLOCK16
            zCopy {'&S_BLOCK16 ; Hex Value:  ', 0}
        .Else_If eax = &S_WITH16
            zCopy {'&S_WITH16 ; Hex Value:  ', 0}
        .Else_If eax = &S_LABEL16
            zCopy {'&S_LABEL16 ; Hex Value:  ', 0}
        .Else_If eax = &S_CEXMODEL16
            zCopy {'&S_CEXMODEL16 ; Hex Value:  ', 0}
        .Else_If eax = &S_VFTPATH16
            zCopy {'&S_VFTPATH16 ; Hex Value:  ', 0}
        .Else_If eax = &S_REGREL16
            zCopy {'&S_REGREL16 ; Hex Value:  ', 0}
        .Else_If eax = &S_BPREL32
            zCopy {'&S_BPREL32 ; Hex Value:  ', 0}
        .Else_If eax = &S_LDATA32
            zCopy {'&S_LDATA32 ; Hex Value:  ', 0}
        .Else_If eax = &S_GDATA32
            zCopy {'&S_GDATA32 ; Hex Value:  ', 0}
        .Else_If eax = &S_PUB32
            zCopy {'&S_PUB32 ; Hex Value:  ', 0}
        .Else_If eax = &S_LPROC32
            zCopy {'&S_LPROC32 ; Hex Value:  ', 0}
        .Else_If eax = &S_GPROC32
            zCopy {'&S_GPROC32 ; Hex Value:  ', 0}
        .Else_If eax = &S_THUNK32
            zCopy {'&S_THUNK32 ; Hex Value:  ', 0}
        .Else_If eax = &S_BLOCK32
            zCopy {'&S_BLOCK32 ; Hex Value:  ', 0}
        .Else_If eax = &S_VFTPATH32
            zCopy {'&S_VFTPATH32 ; Hex Value:  ', 0}
        .Else_If eax = &S_REGREL32
            zCopy {'&S_REGREL32 ; Hex Value:  ', 0}
        .Else_If eax = &S_LTHREAD32
            zCopy {'&S_LTHREAD32 ; Hex Value:  ', 0}
        .Else_If eax = &S_GTHREAD32
            zCopy {'&S_GTHREAD32 ; Hex Value:  ', 0}
        .Else_If eax = &S_LPROCMIPS
            zCopy {'&S_LPROCMIPS ; Hex Value:  ', 0}
        .Else_If eax = &S_GPROCMIPS
            zCopy {'&S_GPROCMIPS ; Hex Value:  ', 0}
        .Else_If eax = &S_PROCREF
            zCopy {'&S_PROCREF ; Hex Value:  ', 0}
        .Else_If eax = &S_DATAREF
            zCopy {'&S_DATAREF ; Hex Value:  ', 0}
        .Else_If eax = &S_ALIGN
            zCopy {'&S_ALIGN ; Hex Value:  ', 0}
        .Else_If eax = &S_BLOCK_CV2
            zCopy {'&S_BLOCK_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_BPREL_CV2
            zCopy {'&S_BPREL_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_BPREL32_CV3
            zCopy {'&S_BPREL32_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_COBOLUDT_CV3
            zCopy {'&S_COBOLUDT_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_COMPILE_CV2
            zCopy {'&S_COMPILE_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_COMPILE_CV3
            zCopy {'&S_COMPILE_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_CONSTANT_CV2
            zCopy {'&S_CONSTANT_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_CONSTANT_CV3
            zCopy {'&S_CONSTANT_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_GDATA_CV2
            zCopy {'&S_GDATA_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_GDATA_CV3
            zCopy {'&S_GDATA_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_GPROC_CV2
            zCopy {'&S_GPROC_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_GPROC32_CV3
            zCopy {'&S_GPROC32_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_GTHREAD_CV3
            zCopy {'&S_GTHREAD_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_LABEL_CV2
            zCopy {'&S_LABEL_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_LDATA_CV2
            zCopy {'&S_LDATA_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_LDATA_CV3
            zCopy {'&S_LDATA_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_LPROC_CV2
            zCopy {'&S_LPROC_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_LPROC_CV3
            zCopy {'&S_LPROC_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_LTHREAD_CV3
            zCopy {'&S_LTHREAD_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_MANYREG_CV3
            zCopy {'&S_MANYREG_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_MSTOOL_CV2
            zCopy {'&S_MSTOOL_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_PUB_CV3
            zCopy {'&S_PUB_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_PUB_DATA_CV2
            zCopy {'&S_PUB_DATA_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_PUB_FUNC1_CV2
            zCopy {'&S_PUB_FUNC1_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_PUB_FUNC2_CV2
            zCopy {'&S_PUB_FUNC2_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_REGISTER_CV3
            zCopy {'&S_REGISTER_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_REGREL_CV3
            zCopy {'&S_REGREL_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_THUNK_CV2
            zCopy {'&S_THUNK_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_UDT_CV2
            zCopy {'&S_UDT_CV2; Hex Value:  ', 0}
        .Else_If eax = &S_UDT_CV3
            zCopy {'&S_UDT_CV3; Hex Value:  ', 0}
        .Else_If eax = &S_VFTTABLE_CV3
            zCopy {'&S_VFTTABLE_CV3; Hex Value:  ', 0}
        .End_If
    pop esi
ret


__________________________________________________________________

Proc WriteRawDataDebugSItem:
    Argument @Text1, @Text2
    uses eax

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice

            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        If D$eax = 'Leng' ; from "Length" string
            xor eax eax
            lodsw | call WriteEax

        Else_If D$eax = 'Inde'; From "Index" string
            call WriteDebugSIndexEquate
            xor eax eax
            lodsw | call WriteEax

        End_If

        mov W$edi CRLF | add edi 2
EndP

__________________________________________________________________


Proc WriteCVdataTitle:
    Uses esi, ecx

    mov D$edi '    ' | add edi 4
    call WriteObjIndice
    zCopy {"Sec", 0}
    zCopy SectionHeaderNumber
    zCopy {".Index", 0}
    zCopy DebugNumber
    zCopy D$CVLabel
    zCopy {'DummyBytes', 0}
    If D$NestedLeafType = &TRUE
        zCopy {'.Arr', 0}
        zCopy LeafTypeArrayObjIndice
    End_If
    mov D$edi ': B$', B$edi+4 ' ' | add edi 5

EndP
__________________________________________________________________

[DebugNumber: '000001', 0]


InitSectionDebugNumber:
    mov D$DebugNumber '0000', D$DebugNumber+4 '01'
ret


IncrementSectionDebugNumber:
    lea ebx D$DebugNumber+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


__________________________________________________________________


__________________________________________________________________

;;
Flag1 Member:

The Flag1 member is a Byte value responsable for several different computatiions accordying to specified bits.
We need to shr the founded values to find the proper equates used.


PCodePresent : 1 Uses Bit 0. When the bit is flagged the file uses PCode. Otherwise it don't use.
                Equates :   CV4_PCODE_ENABLED 01
                            CV4_PCODE_DISABLED 0

FloatPrecision :2   Uses Bits 1 and 2. The FloatPrecision flag is set to 1 if the compiler follows the
                    ANSI C floating-point precision rules. This is specified for Microsoft C compilers
                    by setting the -Op option. So, i builded 4 equates:
    
                    CV4_FP_PRECISION_RESERVED1 0
                    CV4_FP_PRECISION_ANSI 1
                    CV4_FP_PRECISION_RESERVED2 2
                    CV4_FP_PRECISION_RESERVED3 3

FloatPackage :2     Uses bits 3 and 4. The equates used are:

                    CV4_FP8087_PROCESSOR 0 Hardware processor (80x87 for Intel 80x86 processors)
                    CV4_FP8087_EMULATOR 1 Emulator
                    CV4_FP8087_ALTMATH 2 Altmath
                    CV4_FP8087_RESERVED 3 Reserved

AmbientData :3      Uses bits 5, 6, 7 . The equates used are:

                    Ambient code and data memory model enumeration:
                    CV4_AMBIENTDATA_NEAR 0 Near
                    CV4_AMBIENTDATA_FAR 1 Far
                    CV4_AMBIENTDATA_HUGE 2 Huge
                    CV4_AMBIENTDATA_RESERVED3 3 Reserved
                    CV4_AMBIENTDATA_RESERVED4 4 Reserved
                    CV4_AMBIENTDATA_RESERVED5 5 Reserved
                    CV4_AMBIENTDATA_RESERVED6 6 Reserved
                    CV4_AMBIENTDATA_RESERVED7 7 Reserved

                    Note: Values  3 - 7 Reserved
;;


Proc WriteCVCompileFlag1Equates:
    Uses esi, ebx, eax

    xor eax eax | lodsb | mov ebx eax

    push ebx
    push eax

    shr ebx 5 ; This is to calculate AmbientData. Shr by 5 because the Bit 5 is the one that is starting to be flagged.

    .If ebx = &CV4_AMBIENTDATA_NEAR
        zCopy {'(&CV4_AMBIENTDATA_NEAR shl 5)', 0}
    .Else_If ebx = &CV4_AMBIENTDATA_FAR
        zCopy {'(&CV4_AMBIENTDATA_FAR shl 5)', 0}
    .Else_If ebx = &CV4_AMBIENTDATA_HUGE
        zCopy {'(&CV4_AMBIENTDATA_HUGE shl 5)', 0}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED3
        zCopy {'(&CV4_AMBIENTDATA_RESERVED3 shl 5)', 0}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED4
        zCopy {'(&CV4_AMBIENTDATA_RESERVED4 shl 5)', 0}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED5
        zCopy {'(&CV4_AMBIENTDATA_RESERVED5 shl 5)', 0}
    .Else_If ebx = &CV4_AMBIENTDATA_RESERVED6
        zCopy {'(&CV4_AMBIENTDATA_RESERVED6 shl 5)', 0}
    .Else
        zCopy {'(&CV4_AMBIENTDATA_RESERVED7 shl 5)', 0}
    .End_If

        zCopy {' + ', 0}
        ; Now we must Zero bits 5 to 7 to shr again to find the next records for FloatPackage (Bits 3-4)
        btr eax 5 | btr eax 6 | btr eax 7
        ; After clearing the bits, we need to compute only the needed bits to be shred
        shr eax 3 ; This is to calculate FloatPackage. Shr by 3 because the Bit 3 is the one that is starting to be flagged.

    .If eax = &CV4_FP8087_PROCESSOR
        zCopy {'(&CV4_FP8087_PROCESSOR shl 3)', 0}
    .Else_If eax = &CV4_FP8087_EMULATOR
        zCopy {'(&CV4_FP8087_EMULATOR shl 3)', 0}
    .Else_If eax = &CV4_FP8087_ALTMATH
        zCopy {'(&CV4_FP8087_ALTMATH shl 3)', 0}
    .Else
        zCopy {'(&CV4_FP8087_RESERVED shl 3)', 0}
    .End_If

    pop eax
    pop ebx

        zCopy {' + ', 0}

    ; Now we restored the values of ebx and eax. We will going to find the values for
    ; PCodePresent (Bit 0) and FloatPrecision (Bits 1 and 2)
    ; Since we restored the values we must clear bits 3 to 7

    ; zeroes bits 3 to 7
    btr eax 3 | btr eax 4 | btr eax 5 | btr eax 6 | btr eax 7


    shr eax 1 ; This is to calculate FloatPrecision. Shr by 1 because the Bit 1 is the one that is starting to be flagged.

    .If eax = &CV4_FP_PRECISION_RESERVED1
        zCopy {'(&CV4_FP_PRECISION_RESERVED1 shl 1)', 0}
    .Else_If eax = &CV4_FP_PRECISION_ANSI
        zCopy {'(&CV4_FP_PRECISION_ANSI shl 1)', 0}
    .Else_If eax = &CV4_FP_PRECISION_RESERVED2
        zCopy {'(&CV4_FP_PRECISION_RESERVED2 shl 1)', 0}
    .Else
        zCopy {'(&CV4_FP_PRECISION_RESERVED3 shl 1)', 0}
    .End_If

        zCopy {' + ', 0}

    and ebx 01 ; This is to calculate PCodePresent.
               ; The value of 1 is because it is the maximum value of all bits flagged. (Bit 0)

    .If ebx = &CV4_PCODE_ENABLED
        zCopy {'&CV4_PCODE_ENABLED', 0}
    .Else
        zCopy {'&CV4_PCODE_DISABLED', 0}
    .End_If

        zCopy {' ; Hex Value:  ', 0}

EndP

;;

Flag2 Member:

The Flag2 member is a Byte value responsable for several different computatiions accordying to specified bits.
We need to shr the founded values to find the proper equates used.


AmbientCode :3  Uses bits 0, 1 , 2. The equates used are:

                CV4_AMBIENTCODE_NEAR 0 Near
                CV4_AMBIENTCODE_FAR 1 Far
                CV4_AMBIENTCODE_HUGE 2 Huge
                CV4_AMBIENTCODE_RESERVED3 3 Reserved
                CV4_AMBIENTCODE_RESERVED4 4 Reserved
                CV4_AMBIENTCODE_RESERVED5 5 Reserved
                CV4_AMBIENTCODE_RESERVED6 6 Reserved
                CV4_AMBIENTCODE_RESERVED7 7 Reserved

                Note: Values  3 - 7 Reserved

Mode32 :1       Uses Bit 3. When the bit is flagged the file is compiled for 32-bit addresses, otherwise it is not.
                Equates used are:
                
                CV4_MODE32_ENABLED 1 ; Compiled for 32-bit addresses
                CV4_MODE32_DISABLED 0  ; Not compiled for 32-bit addresses

Reserved :4     Used Bits 4,5,6,7. Those bits are reserved. So, they shoulnd't be used, we can set them all to 0.

;;

_______________________________________________

Proc WriteCVCompileFlag2Equates:
    Uses esi, ebx, eax

    If B$esi > 15
        xor eax eax
        lodsb | btr eax 4 | btr eax 5 | btr eax 6 | btr eax 7 ; Clear Bits 4 to 7, that are what exceed the limit of 15
        mov ebx eax
    Else
        xor eax eax | lodsb | mov ebx eax
    End_If

    and eax 07 ; This is to calculate AmbientCode.
               ; The value of 7 is because it is the maximum value of all bits flagged. (0 to 3)

    shr ebx 3 ; This is to calculate Mode32. Shr by 3 because the Bit 3 is the one that is flagged.

    If ebx = &CV4_MODE32_ENABLED
        zCopy {'(&CV4_MODE32_ENABLED shl 3)', 0}
    Else
        zCopy {'(&CV4_MODE32_DISABLED shl 3)', 0}
    End_If

    .If eax = &CV4_AMBIENTCODE_NEAR
        zCopy {' + &CV4_AMBIENTCODE_NEAR', 0}
    .Else_If eax = &CV4_AMBIENTCODE_FAR
        zCopy {' + &CV4_AMBIENTCODE_FAR', 0}
    .Else_If eax = &CV4_AMBIENTCODE_HUGE
        zCopy {' + &CV4_AMBIENTCODE_HUGE', 0}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED3
        zCopy {' + &CV4_AMBIENTCODE_RESERVED3', 0}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED4
        zCopy {' + &CV4_AMBIENTCODE_RESERVED4', 0}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED5
        zCopy {' + &CV4_AMBIENTCODE_RESERVED5', 0}
    .Else_If eax = &CV4_AMBIENTCODE_RESERVED6
        zCopy {' + &CV4_AMBIENTCODE_RESERVED6', 0}
    .Else
        zCopy {' + &CV4_AMBIENTCODE_RESERVED7', 0}
    .End_If
        zCopy {' ; Hex Value:  ', 0}

EndP
_______________

;;
Used Equates. All that is not listed is reserved

&CV4_COMPILE_LANG_C 0 ; C
&CV4_COMPILE_LANG_CPLUS 01 ; C++
&CV4_COMPILE_LANG_FORTRAN 02; Fortran
&CV4_COMPILE_LANG_MASM 03; Masm
&CV4_COMPILE_LANG_PASCAL 04; Pascal
&CV4_COMPILE_LANG_BASIC 05; Basic
&CV4_COMPILE_LANG_COBOL 06; Cobol

;;

Proc WriteCVCompileLanguageEquates:
    Uses esi

    .If B$esi = &CV4_COMPILE_LANG_C
        zCopy {'&CV4_COMPILE_LANG_C ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_LANG_CPLUS
        zCopy {'&CV4_COMPILE_LANG_CPLUS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_LANG_FORTRAN
        zCopy {'&CV4_COMPILE_LANG_FORTRAN ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_LANG_MASM
        zCopy {'&CV4_COMPILE_LANG_MASM ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_LANG_PASCAL
        zCopy {'&CV4_COMPILE_LANG_PASCAL ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_LANG_BASIC
        zCopy {'&CV4_COMPILE_LANG_BASIC ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_LANG_COBOL
        zCopy {'&CV4_COMPILE_LANG_COBOL ; Hex Value:  ', 0}
    .End_if


EndP

_______________

;;

Used Equates. All that is not listed is reserved

&CV4_COMPILE_MACHINE_I80 ; Intel 8080
&CV4_COMPILE_MACHINE_I86 ; Intel 8086
&CV4_COMPILE_MACHINE_I286 ; Intel 80286
&CV4_COMPILE_MACHINE_I386 ; Intel 80386
&CV4_COMPILE_MACHINE_I486 ; Intel 80486
&CV4_COMPILE_MACHINE_I586 ; Intel Pentium
&CV4_COMPILE_MACHINE_R4000 ; MIPS R4000
&CV4_COMPILE_MACHINE_MIPSRESERVED1 ; Reserved for future MIPS processor
&CV4_COMPILE_MACHINE_MIPSRESERVED2 ; Reserved for future MIPS processor
&CV4_COMPILE_MACHINE_MC68000 ; MC68000
&CV4_COMPILE_MACHINE_MC68010 ; MC68010
&CV4_COMPILE_MACHINE_MC68020 ; MC68020
&CV4_COMPILE_MACHINE_MC68030 ; MC68030
&CV4_COMPILE_MACHINE_MC68040 ; MC68040
&CV4_COMPILE_MACHINE_ALPHA ; DEC Alpha
;;


Proc WriteCVCompileMachineEquates:
    Uses esi

    .If B$esi = &CV4_COMPILE_MACHINE_I80
        zCopy {'&CV4_COMPILE_MACHINE_I80 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I86
        zCopy {'&CV4_COMPILE_MACHINE_I86 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I286
        zCopy {'&CV4_COMPILE_MACHINE_I286 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I386
        zCopy {'&CV4_COMPILE_MACHINE_I386 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I486
        zCopy {'&CV4_COMPILE_MACHINE_I486 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_I586
        zCopy {'&CV4_COMPILE_MACHINE_I586 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_R4000
        zCopy {'&CV4_COMPILE_MACHINE_R4000 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MIPSRESERVED1
        zCopy {'&CV4_COMPILE_MACHINE_MIPSRESERVED1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MIPSRESERVED2
        zCopy {'&CV4_COMPILE_MACHINE_MIPSRESERVED2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68000
        zCopy {'&CV4_COMPILE_MACHINE_MC68000 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68010
        zCopy {'&CV4_COMPILE_MACHINE_MC68010 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68020
        zCopy {'&CV4_COMPILE_MACHINE_MC68020 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68030
        zCopy {'&CV4_COMPILE_MACHINE_MC68030 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_MC68040
        zCopy {'&CV4_COMPILE_MACHINE_MC68040 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_COMPILE_MACHINE_ALPHA
        zCopy {'&CV4_COMPILE_MACHINE_ALPHA ; Hex Value:  ', 0}
    .End_if


EndP
________________

Proc WriteRawDataDebugSCompileItem:
    Argument @Text1, @Text2, @DataSize
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Compile.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Mach' ; from "Machine" string
            call WriteCVCompileMachineEquates
            If D@DataSize = 4
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsb | call WriteEax
            End_If
        ..Else_If D$eax = 'Lang'; From "Language" string
            call WriteCVCompileLanguageEquates
            If D@DataSize = 4
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsb | call WriteEax
            End_If

        ..Else_If D$eax+2 = 'ags1'; From "Flags1" string
            call WriteCVCompileFlag1Equates
            If D@DataSize = 4
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsb | call WriteEax
            End_If

        ..Else_If D$eax+2 = 'ags2'; From "Flags2" string
            call WriteCVCompileFlag2Equates
            If D@DataSize = 4
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsb | call WriteEax
            End_If

        ..Else_If D$eax = 'Unkn'; From "Unknown" string
            If D@DataSize = 4
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsw | call WriteEax
            End_If

        ..Else_If D$eax = 'Comp'; From "CompilerID" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            If D@DataSize = 2 ; this happens only in S_COMPILE_CV2. It´' size is not 2, but we use this to identify this type.
                push eax
                    call StrLenProc esi
                    mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                pop eax
            Else
                movzx ecx B$esi-1
            End_If

            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2
EndP

_________________________________________

[CVNonLeafIndexPad03Unknown01: 'Unknown01', 0
 CVNonLeafIndexPad03Unknown02: 'Unknown02', 0
 CVNonLeafIndexPad03Unknown03: 'Unknown03', 0
 CVNonLeafIndexPad03NameLenght: 'NameLenght', 0
 CVNonLeafIndexPad03Name: 'Name', 0]

Proc WriteRawDataDebugSNonLeafIndexPad03:
    Uses ecx, eax, ebx
    ; This is to avoid that the Structure have dummy bytes at the end.
   ; mov ebx esi
  ;  sub ebx 2
 ;   mov ecx D$SizeAdd
;    add ebx ecx

;    push ebx

    call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Unknown01, {': B$ ', 0}
    call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Unknown02, {': B$ ', 0}
    call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Unknown03, {': B$ ', 0}
    call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03NameLenght, {': D$ ', 0}

    mov ebx esi
    mov ecx D$SizeAdd
    add ebx ecx
    add ebx D$esi-4

    If D$esi-4 <> 0 ; is the Name Lenght = 0 ? jmp over.

        push ebx
            call WriteRawDataDebugSNonLeafIndexPad03Item CVNonLeafIndexPad03Name, {': B$ ', 0}
        pop ebx

    End_If
    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________

Proc WriteRawDataDebugSNonLeafIndexPad03Item:
    Argument @Text1, @Text2
    uses eax, ecx, ebx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'NoIndexLeafPad03.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Unkn' ; from "Unknown01", "Unknown02", "Unknown03" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-4
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2
EndP

________________________

[CVCompileMachine: 'Machine', 0
 CVCompileLanguage: 'Language', 0
 CVCompileFlags1: 'Flags1', 0
 CVCompileFlags2: 'Flags2', 0
 CVCompileUnknown: 'Unknown', 0
 CVCompileCompilerID: 'CompilerID', 0
 CVCompileNameLenght: 'NameLenght', 0
 CVCompileName: 'Name', 0]


Proc WriteRawDataDebugSCompile:
    Uses ecx, eax, ebx
    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

    .If W$esi-2 = &S_COMPILE
        call WriteRawDataDebugSCompileItem CVCompileMachine, {': B$ ', 0}, 1
        call WriteRawDataDebugSCompileItem CVCompileLanguage, {': B$ ', 0}, 1
        call WriteRawDataDebugSCompileItem CVCompileFlags1, {': B$ ', 0}, 1
        call WriteRawDataDebugSCompileItem CVCompileFlags2, {': B$ ', 0}, 1
        call WriteRawDataDebugSCompileItem CVCompileNameLenght, {': B$ ', 0}, 1
        call WriteRawDataDebugSCompileItem CVCompileName, {': B$ ', 0}, 1
    .Else_If W$esi-2 = &S_COMPILE_CV3
        call WriteRawDataDebugSCompileItem CVCompileLanguage, {': D$ ', 0}, 4
        call WriteRawDataDebugSCompileItem CVCompileMachine, {': D$ ', 0}, 4
        call WriteRawDataDebugSCompileItem CVCompileFlags2, {': D$ ', 0}, 4
        call WriteRawDataDebugSCompileItem CVCompileFlags1, {': D$ ', 0}, 4
        call WriteRawDataDebugSCompileItem CVCompileCompilerID, {': W$ ', 0}, 2
        call WriteRawDataDebugSCompileItem CVCompileNameLenght, {': B$ ', 0}, 1
        call WriteRawDataDebugSCompileItem CVCompileName, {': B$ ', 0}, 1

    .Else_If W$esi-2 = &S_COMPILE_CV2
        ; The members below seems to be a variation of the S_OBJNAME structured data.
        ; The Unknown seems to be the Signature
        ; The name is the object name, with full path. (And not the name of the object only as in S_OBJECT)
        call WriteRawDataDebugSCompileItem CVCompileUnknown, {': D$ ', 0}, 4
        call WriteRawDataDebugSCompileItem CVCompileName, {': B$ ', 0}, 2
    .End_If

    pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP


________________________________________________________________________________

[CVMsToolMachine: 'Machine', 0
 CVMsToolLanguage: 'Language', 0
 CVMsToolUnknown1: 'Unknown1', 0
 CVMsToolUnknown2: 'Unknown2', 0
 CVMsToolUnknown3: 'Unknown3', 0
 CVMsToolUnknown4: 'Unknown4', 0
 CVMsToolUnknown5: 'Unknown5', 0
 CVMsToolUnknown6: 'Unknown6', 0
 CVMsToolUnknown7: 'Unknown7', 0
 CVMsToolUnknown8: 'Unknown8', 0
 CVMsToolName: 'Name', 0]

Proc WriteRawDataDebugSMsTool:
    Uses ecx, eax, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

    .If W$esi-2 = &S_MSTOOL_CV2
        call WriteRawDataDebugSMsToolItem CVMsToolMachine, {': B$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolLanguage, {': B$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown1, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown2, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown3, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown4, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown5, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown6, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown7, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolUnknown8, {': W$ ', 0}
        call WriteRawDataDebugSMsToolItem CVMsToolName, {': B$ ', 0}
    .End_If

    pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
_________________________________

Proc WriteRawDataDebugSMsToolItem:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'MsTool.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Mach' ; from "Machine" string
            call WriteCVCompileMachineEquates
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Lang'; From "Language" string
            call WriteCVCompileLanguageEquates
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax+4 = 'own1'; From "Unknown1" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own2'; From "Unknown2" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own3'; From "Unknown3" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own4'; From "Unknown4" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own5'; From "Unknown5" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own6'; From "Unknown6" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own7'; From "Unknown7" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax+4 = 'own8'; From "Unknown8" string
                xor eax eax
                lodsw | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name. On this case (S_MSTOOL_CV2) The name is a null terminated string
                push eax
                    call StrLenProc esi
                    mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                pop eax

            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2
EndP

________________________________________________________________________________

[CVRegisterType: 'Type', 0
 CVRegisterOldCVType: 'OldCVType', 0
 CVRegisterRegister: 'Register', 0
 CVRegisterNameLenght: 'NameLenght', 0
 CVRegisterName: 'Name', 0
 CVRegisterTrackingInfo: 'TrackingInfo', 0]

Proc WriteRawDataDebugSRegister:
    Local @EndPos, @OldCodeView
    Uses ecx, eax

    mov D@OldCodeView &FALSE
    If W$esi-2 = &S_REGISTER_CV3
        mov D@OldCodeView &TRUE
    End_If


    ; 1st find start of symbol (Address of index -> S_REGISTER)
    mov ecx esi
    sub ecx 2 ; at ecx we point to the Index value
    movzx eax W$esi-4 ; find the len of the symbol
    add ecx eax
    mov D@EndPos ecx

    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSRegisterItem CVRegisterOldCVType , {': W$ ', 0}
    End_If
    call WriteRawDataDebugSRegisterItem CVRegisterType, {': W$ ', 0}
    call WriteRawDataDebugSRegisterItem CVRegisterRegister, {': W$ ', 0}
    call WriteRawDataDebugSRegisterItem CVRegisterNameLenght, {': B$ ', 0}
    call WriteRawDataDebugSRegisterItem CVRegisterName, {': B$ ', 0}

    ..If esi <> D@EndPos ; is esi ending at EndPos ? No, do next line. Yes, jmp over

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Register.TrackingInfo: B$ ', 0}
        pop esi

        mov ecx D@EndPos
        sub ecx esi

        .Do
            push ecx
            xor eax eax
            lodsb | call WriteEax
            mov W$edi ', ' | add edi 2
            pop ecx
            dec ecx
        .Loop_Until ecx = 0
            sub edi 2
            mov W$edi CRLF | add edi 2
    ..End_If

    ; bypass esi. Need to review this stuff
;    call WriteRawDataDebugSRegisterItem CVRegisterTrackingInfo, {': B$ ', 0}

; type equates here: WriteCVBPRel32TypeEquates
; call Write_IMAGE_FILE_MACHINE D$CoffMachineType
EndP
________________________________________________________________________________

Proc WriteRawDataDebugSRegisterItem:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Register.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Type' ; from "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'OldC' ; from "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Regi'; From "Register" string
            .If_Or D$CoffMachineType = &IMAGE_FILE_MACHINE_I386, D$CoffMachineType = &IMAGE_FILE_MACHINE_I486, D$CoffMachineType = &IMAGE_FILE_MACHINE_I586
                call WriteCVRegisterIntelRegEquates
            .Else_If D$CoffMachineType = &IMAGE_FILE_MACHINE_M68K
                call WriteCVRegisterM68KRegEquates
            .Else_If_Or D$CoffMachineType = &IMAGE_FILE_MACHINE_MIPS16, D$CoffMachineType = &IMAGE_FILE_MACHINE_MIPSFPU, D$CoffMachineType = &IMAGE_FILE_MACHINE_MIPSFPU16
                call WriteCVRegisterMipsRegEquates
            .End_If
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2
EndP

________________________________________________________________________________

Proc WriteCVRegisterIntelRegEquates:
    Uses esi

    .If B$esi = &CV4_REGISTER_INTEL_REG_NONE
        zCopy {'&CV4_REGISTER_INTEL_REG_NONE ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_AL
        zCopy {'&CV4_REGISTER_INTEL_REG_AL ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CL
        zCopy {'&CV4_REGISTER_INTEL_REG_CL ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DL
        zCopy {'&CV4_REGISTER_INTEL_REG_DL ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BL
        zCopy {'&CV4_REGISTER_INTEL_REG_BL ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_AH
        zCopy {'&CV4_REGISTER_INTEL_REG_AH ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CH
        zCopy {'&CV4_REGISTER_INTEL_REG_CH ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DH
        zCopy {'&CV4_REGISTER_INTEL_REG_DH ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BH
        zCopy {'&CV4_REGISTER_INTEL_REG_BH ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_AX
        zCopy {'&CV4_REGISTER_INTEL_REG_AX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CX
        zCopy {'&CV4_REGISTER_INTEL_REG_CX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DX
        zCopy {'&CV4_REGISTER_INTEL_REG_DX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BX
        zCopy {'&CV4_REGISTER_INTEL_REG_BX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_SP
        zCopy {'&CV4_REGISTER_INTEL_REG_SP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_BP
        zCopy {'&CV4_REGISTER_INTEL_REG_BP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_SI
        zCopy {'&CV4_REGISTER_INTEL_REG_SI ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DI
        zCopy {'&CV4_REGISTER_INTEL_REG_DI ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EAX
        zCopy {'&CV4_REGISTER_INTEL_REG_EAX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ECX
        zCopy {'&CV4_REGISTER_INTEL_REG_ECX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EDX
        zCopy {'&CV4_REGISTER_INTEL_REG_EDX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EBX
        zCopy {'&CV4_REGISTER_INTEL_REG_EBX ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ESP
        zCopy {'&CV4_REGISTER_INTEL_REG_ESP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EBP
        zCopy {'&CV4_REGISTER_INTEL_REG_EBP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ESI
        zCopy {'&CV4_REGISTER_INTEL_REG_ESI ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_EDI
        zCopy {'&CV4_REGISTER_INTEL_REG_EDI ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_ES
        zCopy {'&CV4_REGISTER_INTEL_REG_ES ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_CS
        zCopy {'&CV4_REGISTER_INTEL_REG_CS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_SS
        zCopy {'&CV4_REGISTER_INTEL_REG_SS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_DS
        zCopy {'&CV4_REGISTER_INTEL_REG_DS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_FS
        zCopy {'&CV4_REGISTER_INTEL_REG_FS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_REG_GS
        zCopy {'&CV4_REGISTER_INTEL_REG_GS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_IP
        zCopy {'&CV4_REGISTER_INTEL_SPECIALREGCASE_IP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_FLAGS
        zCopy {'&CV4_REGISTER_INTEL_SPECIALREGCASE_FLAGS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_EIP
        zCopy {'&CV4_REGISTER_INTEL_SPECIALREGCASE_EIP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SPECIALREGCASE_EFLAGS
        zCopy {'&CV4_REGISTER_INTEL_SPECIALREGCASE_EFLAGS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_TEMP
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_TEMP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_TEMPH
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_TEMPH ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_QUOTE
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_QUOTE ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED1
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_RESERVED1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED2
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_RESERVED2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED3
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_RESERVED3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED4
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_RESERVED4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_PCODEREG_RESERVED5
        zCopy {'&CV4_REGISTER_INTEL_PCODEREG_RESERVED5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR0
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_CR0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR1
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_CR1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR2
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_CR2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_CR3
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_CR3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR0
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR1
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR2
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR3
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR4
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR5
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR6
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREG_DR7
        zCopy {'&CV4_REGISTER_INTEL_SYSREG_DR7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST0
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST2
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST3
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST4
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST5
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST6
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ST7
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ST7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_CONTROL
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_CONTROL ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_STATUS
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_STATUS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_TAG
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_TAG ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPIP
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPIP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPCS
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPCS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDO
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDO ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDS
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPDS ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_ISEM
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_ISEM ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEIP
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEIP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEDO
        zCopy {'&CV4_REGISTER_INTEL_SYSREGEXTENSION_FPEDO ; Hex Value:  ', 0}
    .End_If

EndP
________________________________________________________________________________

Proc WriteCVRegisterM68KRegEquates:
    Uses esi

    .If B$esi = &CV4_REGISTER_M68K_DATA_REG0
        zCopy {'&CV4_REGISTER_M68K_DATA_REG0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG1
        zCopy {'&CV4_REGISTER_M68K_DATA_REG1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG2
        zCopy {'&CV4_REGISTER_M68K_DATA_REG2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG3
        zCopy {'&CV4_REGISTER_M68K_DATA_REG3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG4
        zCopy {'&CV4_REGISTER_M68K_DATA_REG4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG5
        zCopy {'&CV4_REGISTER_M68K_DATA_REG5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG6
        zCopy {'&CV4_REGISTER_M68K_DATA_REG6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_DATA_REG7
        zCopy {'&CV4_REGISTER_M68K_DATA_REG7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG0
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG1
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG2
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG3
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG4
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG5
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG6
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_ADDRESS_REG7
        zCopy {'&CV4_REGISTER_M68K_ADDRESS_REG7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_CCR
        zCopy {'&CV4_REGISTER_M68K_R68_CCR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_SR
        zCopy {'&CV4_REGISTER_M68K_R68_SR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_USP
        zCopy {'&CV4_REGISTER_M68K_R68_USP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_MSP
        zCopy {'&CV4_REGISTER_M68K_R68_MSP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_SFC
        zCopy {'&CV4_REGISTER_M68K_R68_SFC ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_DFC
        zCopy {'&CV4_REGISTER_M68K_R68_DFC ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_CACR
        zCopy {'&CV4_REGISTER_M68K_R68_CACR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_VBR
        zCopy {'&CV4_REGISTER_M68K_R68_VBR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_CAAR
        zCopy {'&CV4_REGISTER_M68K_R68_CAAR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_ISP
        zCopy {'&CV4_REGISTER_M68K_R68_ISP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_PC
        zCopy {'&CV4_REGISTER_M68K_R68_PC ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED1
        zCopy {'&CV4_REGISTER_M68K_RESERVED1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_FPCR
        zCopy {'&CV4_REGISTER_M68K_R68_FPCR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_FPSR
        zCopy {'&CV4_REGISTER_M68K_R68_FPSR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_R68_FPIAR
        zCopy {'&CV4_REGISTER_M68K_R68_FPIAR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED2
        zCopy {'&CV4_REGISTER_M68K_RESERVED2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT0
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT1
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT2
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT3
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT4
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT5
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT6
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_FLOATING_POINT7
        zCopy {'&CV4_REGISTER_M68K_FLOATING_POINT7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED3
        zCopy {'&CV4_REGISTER_M68K_RESERVED3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED4
        zCopy {'&CV4_REGISTER_M68K_RESERVED4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED5
        zCopy {'&CV4_REGISTER_M68K_RESERVED5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED6
        zCopy {'&CV4_REGISTER_M68K_RESERVED6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED7
        zCopy {'&CV4_REGISTER_M68K_RESERVED7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED8
        zCopy {'&CV4_REGISTER_M68K_RESERVED8 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED9
        zCopy {'&CV4_REGISTER_M68K_RESERVED9 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED10
        zCopy {'&CV4_REGISTER_M68K_RESERVED10 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED11
        zCopy {'&CV4_REGISTER_M68K_RESERVED11 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED12
        zCopy {'&CV4_REGISTER_M68K_RESERVED12 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_RESERVED13
        zCopy {'&CV4_REGISTER_M68K_RESERVED13 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_CV_R68_PSR
        zCopy {'&CV4_REGISTER_M68K_CV_R68_PSR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_M68K_CV_R68_PCSR
        zCopy {'&CV4_REGISTER_M68K_CV_R68_PCSR ; Hex Value:  ', 0}
    .End_If

EndP
________________________________________________________________________________

Proc WriteCVRegisterMipsRegEquates:
    Uses esi

    .If B$esi = &CV4_REGISTER_MIPS_NOREGISTER
        zCopy {'&CV4_REGISTER_MIPS_NOREGISTER ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_ZERO
        zCopy {'&CV4_REGISTER_MIPS_INT_ZERO ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_AT
        zCopy {'&CV4_REGISTER_MIPS_INT_AT ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_V0
        zCopy {'&CV4_REGISTER_MIPS_INT_V0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_V1
        zCopy {'&CV4_REGISTER_MIPS_INT_V1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A0
        zCopy {'&CV4_REGISTER_MIPS_INT_A0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A1
        zCopy {'&CV4_REGISTER_MIPS_INT_A1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A2
        zCopy {'&CV4_REGISTER_MIPS_INT_A2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_A3
        zCopy {'&CV4_REGISTER_MIPS_INT_A3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T0
        zCopy {'&CV4_REGISTER_MIPS_INT_T0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T1
        zCopy {'&CV4_REGISTER_MIPS_INT_T1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T2
        zCopy {'&CV4_REGISTER_MIPS_INT_T2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T3
        zCopy {'&CV4_REGISTER_MIPS_INT_T3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T4
        zCopy {'&CV4_REGISTER_MIPS_INT_T4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T5
        zCopy {'&CV4_REGISTER_MIPS_INT_T5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T6
        zCopy {'&CV4_REGISTER_MIPS_INT_T6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T7
        zCopy {'&CV4_REGISTER_MIPS_INT_T7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S0
        zCopy {'&CV4_REGISTER_MIPS_INT_S0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S1
        zCopy {'&CV4_REGISTER_MIPS_INT_S1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S2
        zCopy {'&CV4_REGISTER_MIPS_INT_S2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S3
        zCopy {'&CV4_REGISTER_MIPS_INT_S3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S4
        zCopy {'&CV4_REGISTER_MIPS_INT_S4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S5
        zCopy {'&CV4_REGISTER_MIPS_INT_S5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S6
        zCopy {'&CV4_REGISTER_MIPS_INT_S6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S7
        zCopy {'&CV4_REGISTER_MIPS_INT_S7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T8
        zCopy {'&CV4_REGISTER_MIPS_INT_T8 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_T9
        zCopy {'&CV4_REGISTER_MIPS_INT_T9 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_KT0
        zCopy {'&CV4_REGISTER_MIPS_INT_KT0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_KT1
        zCopy {'&CV4_REGISTER_MIPS_INT_KT1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_GP
        zCopy {'&CV4_REGISTER_MIPS_INT_GP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_SP
        zCopy {'&CV4_REGISTER_MIPS_INT_SP ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_S8
        zCopy {'&CV4_REGISTER_MIPS_INT_S8 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_RA
        zCopy {'&CV4_REGISTER_MIPS_INT_RA ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_LO
        zCopy {'&CV4_REGISTER_MIPS_INT_LO ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_INT_HI
        zCopy {'&CV4_REGISTER_MIPS_INT_HI ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FIR
        zCopy {'&CV4_REGISTER_MIPS_FIR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_PSR
        zCopy {'&CV4_REGISTER_MIPS_PSR ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT0
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT0 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT1
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT1 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT2
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT2 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT3
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT3 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT4
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT4 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT5
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT5 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT6
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT6 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT7
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT7 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT8
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT8 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT9
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT9 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT10
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT10 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT11
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT11 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT12
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT12 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT13
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT13 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT14
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT14 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT15
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT15 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT16
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT16 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT17
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT17 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT18
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT18 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT19
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT19 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT20
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT20 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT21
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT21 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT22
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT22 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT23
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT23 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT24
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT24 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT25
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT25 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT26
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT26 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT27
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT27 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT28
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT28 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT29
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT29 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT30
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT30 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT31
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT31 ; Hex Value:  ', 0}
    .Else_If B$esi = &CV4_REGISTER_MIPS_FLOATING_POINT_STATUS_REG
        zCopy {'&CV4_REGISTER_MIPS_FLOATING_POINT_STATUS_REG ; Hex Value:  ', 0}
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeStructureItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.VarStr', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Coun'; From "Count" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'Fiel'; From "Field" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax+13 = 'own1'; From "OldCVTypeUnknown1" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax+13 = 'own2'; From "OldCVTypeUnknown2" string
            xor eax eax
            lodsw | call WriteEax
         ..Else_If D$eax = 'Prop'; From "Property" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'dLis'; From "dList" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'vSha'; From "vShape" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'Stru'; From "StructLen" string
            xor eax eax
            lodsw | call WriteEax


        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP
___________________________________________________

[CVLeafTypeStructureCount: 'Count', 0
 CVLeafTypeStructureField: 'Field', 0
 CVLeafTypeStructureOldCVType: 'OldCVType', 0
 CVLeafTypeStructureOldCVTypeUnkn1: 'OldCVTypeUnknown1', 0
 CVLeafTypeStructureOldCVTypeUnkn2: 'OldCVTypeUnknown2', 0
 CVLeafTypeStructureProperty: 'Property', 0
 CVLeafTypeStructuredList: 'dList', 0
 CVLeafTypeStructurevshape: 'vShape', 0
 CVLeafTypeStructuredStructLen: 'StructLen', 0]

Proc WriteRawDataDebugSContantLeafTypeStructure:
    Local @OldCodeView
    Uses eax, ecx

    mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_STRUCTURE_CV3
        mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

        call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureCount, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureField, {': W$ ', 0}
        If D@OldCodeView = &TRUE
            call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeUnionOldCVType, {': W$ ', 0}
            call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureOldCVTypeUnkn1, {': W$ ', 0}
            call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureOldCVTypeUnkn2, {': W$ ', 0}
        End_If
        call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructureProperty, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructuredList, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructurevshape, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeStructureItem CVLeafTypeStructuredStructLen, {': W$ ', 0}

        call WriteRawDataDebugSContantLeafTypeStructureItem CVCompileNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeStructureItem CVCompileName, {': B$ ', 0}
        sub edi 2

    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If


EndP
________________________________________________________________________________

[CVLeafTypeUnionCount: 'Count', 0
 CVLeafTypeUnionField: 'Field', 0
 CVLeafTypeUnionOldCVType: 'OldCVType', 0
 CVLeafTypeUnionProperty: 'Property', 0
 CVLeafTypeUnionUnionLen: 'UnionLength', 0]
;LF_UNION count @field property length
; WriteRawDataDebugSContantLeafTypeMember
Proc WriteRawDataDebugSContantLeafTypeUnion:
    Local @OldCodeView
    Uses eax, ecx


    mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_UNION_CV3
        mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

        call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionCount, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionField, {': W$ ', 0}
        If D@OldCodeView = &TRUE
            call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionOldCVType, {': W$ ', 0}
        End_If
        call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionProperty, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeUnionItem CVLeafTypeUnionUnionLen, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeUnionItem CVCompileNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeUnionItem CVCompileName, {': B$ ', 0}
        sub edi 2

    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If


EndP
________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeUnionItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Union.', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Coun'; From "Count" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'Fiel'; From "Field" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'Prop'; From "Property" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'Unio'; From "UnionLength" string
            xor eax eax
            lodsw | call WriteEax


        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP
________________________________________________________________________________


Proc WriteRawDataDebugSContantLeafTypePointerItem:
   Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Pointer.', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP
_______________________________________________________________________

[CVLeafTypeModifierOldCVType: 'OldCVType', 0
 CVLeafTypeModifierAttribute: 'Attribute', 0
 CVLeafTypeModifierIndex: 'Index', 0]

Proc WriteRawDataDebugSContantLeafTypeModifier:
    Local @OldCodeView
    Uses eax, ecx

    mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_MODIFIER_CV3
        mov D@OldCodeView &TRUE
    End_If


    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        If D@OldCodeView = &TRUE
            call WriteRawDataDebugSContantLeafTypeModifierItem CVLeafTypeModifierOldCVType, {': W$ ', 0}
        End_If
        call WriteRawDataDebugSContantLeafTypeModifierItem CVLeafTypeModifierAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeModifierItem CVLeafTypeModifierIndex, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If


EndP
_________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeModifierItem:
   Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Modifier.', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            ; same as in WriteCVCompileFlag2Equates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Inde'; From "Type" string
            xor eax eax
            lodsw | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

_________________________________________________________________________________


[CVLeafTypePointerOldCVType: 'OldCVType', 0
 CVLeafTypePointerAttribute: 'Attribute', 0
 CVLeafTypePointerType: 'Type', 0]

Proc WriteRawDataDebugSContantLeafTypePointer:
    Local @OldCodeView
    Uses eax, ecx

    mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_POINTER_CV3
        mov D@OldCodeView &TRUE
    End_If


    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        If D@OldCodeView = &TRUE
            call WriteRawDataDebugSContantLeafTypePointerItem CVLeafTypePointerOldCVType, {': W$ ', 0}
        End_If
        call WriteRawDataDebugSContantLeafTypePointerItem CVLeafTypePointerAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypePointerItem CVLeafTypePointerType, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If


EndP
________________________________________________________________________________

            ;WriteRawDataDebugSBPRel32
[CVLeafTypeArrayElemType: 'ElemType' , 0
 CVLeafTypeArrayIdxType: 'IdxType' , 0
 CVLeafTypeArrayLength: 'Length' , 0
 CVLeafTypeArrayNamelenght: 'NameLenght' , 0
 CVLeafTypeArrayName: 'Name' , 0]

Proc WriteRawDataDebugSContantLeafTypeArray:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayElemType , {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayIdxType , {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayLength, {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayNamelenght, {': B$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
________________________________________________________________________________
Proc WriteRawDataDebugSContantLeafTypeArrayItem:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.LeafType.Array.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Elem' ; from "ElemType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'IdxT'; From "IdxType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Leng'; From "Lenght" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

________________________________________________________________________________
Proc WriteRawDataDebugSContantLeafTypeProcedureItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Procedure.', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

         ..If D$eax = 'rvTy'; From "rvType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Call'; From "Call" string
            call WriteCVLF_ProcCallConvTypeEquates
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Rese'; From "Reserved" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Parm'; From "Parms" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'ArgL'; From "ArgList" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

_________________________________________

[CVLeafTypeProcrvType: 'rvType', 0
 CVLeafTypeProcCall: 'Call', 0
 CVLeafTypeProcrvReserved: 'Reserved', 0
 CVLeafTypeProcrvParms: 'Parms', 0
 CVLeafTypeProcrvArgList: 'ArgList', 0]

Proc WriteRawDataDebugSContantLeafTypeProcedure:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcCall, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvReserved, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvParms, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeProcedureItem CVLeafTypeProcrvArgList, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

[CVLeafTypeMFunctionRvType: 'RvType', 0
 CVLeafTypeMFunctionClass: 'Class', 0
 CVLeafTypeMFunctionThis: 'This', 0
 CVLeafTypeMFunctionCall: 'Call', 0
 CVLeafTypeMFunctionRes: 'Res', 0
 CVLeafTypeMFunctionParms: 'Parms', 0
 CVLeafTypeMFunctionOldCVType: 'OldCVType', 0
 CVLeafTypeMFunctionArglist: 'Arglist', 0
 CVLeafTypeMFunctionThisAdjust: 'ThisAdjust', 0]

Proc WriteRawDataDebugSContantLeafTypeMFunction:
    Local @OldCodeView
    Uses eax, ecx


    mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_MFUNCTION_CV3
        mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

        If D@OldCodeView = &TRUE
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionRvType, {': D$ ', 0}, &TRUE
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionClass, {': D$ ', 0}, &TRUE
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionThis, {': D$ ', 0}, &TRUE
        Else
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionRvType, {': W$ ', 0}, &FALSE
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionClass, {': W$ ', 0}, &FALSE
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionThis, {': W$ ', 0}, &FALSE

        End_If
        call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionCall, {': B$ ', 0}, &FALSE
        call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionRes, {': B$ ', 0}, &FALSE
        call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionParms, {': W$ ', 0}, &FALSE
        If D@OldCodeView = &TRUE
            call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionOldCVType, {': W$ ', 0}, &TRUE
        End_If
        call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionArglist, {': W$ ', 0}, &FALSE
        call WriteRawDataDebugSContantLeafTypeMFunctionItem CVLeafTypeMFunctionThisAdjust, {': D$ ', 0}, &FALSE
        sub edi 2

    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeMFunctionItem:
    Argument @Text1, @Text2, @UseOldCVType
    uses eax, ecx, ebx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.MFunction.', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'RvTy'; From "RvType" string
            call WriteCVBPRel32TypeEquates
            If D@UseOldCVType = &TRUE
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsw | call WriteEax
            End_If
        ..Else_If D$eax = 'Clas'; From "Class" string
            call WriteCVBPRel32TypeEquates
            If D@UseOldCVType = &TRUE
                lodsd | call WriteEax
            Else
                xor eax eax
                lodsw | call WriteEax
            End_If
        ..Else_If D$eax = 'This'; From "This" string
            .If D$eax+4 = 'Adju'; From "ThisAdjust" string
                lodsd | call WriteEax
            .Else   ; From "This" string
                call WriteCVBPRel32TypeEquates
                If D@UseOldCVType = &TRUE
                    lodsd | call WriteEax
                Else
                    xor eax eax
                    lodsw | call WriteEax
                End_If
            .End_If
        ..Else_If D$eax = 'Call'; From "Call" string
            call WriteCVLF_ProcCallConvTypeEquates
            xor eax eax
            lodsb | call WriteEax
        ..Else_If W$eax = 'Re'; From "Res" string
            xor eax eax
            lodsb | call WriteEax
        ..Else_If D$eax = 'Parm'; From "Parms" string
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax
        ..Else_If D$eax = 'Argl'; From "Arglist" string
            xor eax eax
            lodsw | call WriteEax
        ..End_If

        mov W$edi CRLF | add edi 2

EndP

________________________________________________________________________________

Proc WriteCVLF_ProcCallConvTypeEquates:
    Uses esi

    ..If W$esi = &CV4_CALL_NEAR_C
        zCopy {'&CV4_CALL_NEAR_C; Near C (arguments pushed right to left, caller pops arguments) - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_FAR_C
        zCopy {'&CV4_CALL_FAR_C; Far C - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_NEAR_PASCAL
        zCopy {'&CV4_CALL_NEAR_PASCAL; Near Pascal (arguments pushed left to right, callee pops arguments) - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_FAR_PASCAL
        zCopy {'&CV4_CALL_FAR_PASCAL; Far Pascal - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_NEAR_FASTCALL
        zCopy {'&CV4_CALL_NEAR_FASTCALL; Near fastcall - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_FAR_FASTCALL
        zCopy {'&CV4_CALL_FAR_FASTCALL; Far fastcall - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED6
        zCopy {'&CV4_CALL_RESERVED6; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_NEAR_STDCALL
        zCopy {'&CV4_CALL_NEAR_STDCALL; Near stdcall - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_FAR_STDCALL
        zCopy {'&CV4_CALL_FAR_STDCALL; Far stdcall - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_NEAR_SYSCALL
        zCopy {'&CV4_CALL_NEAR_SYSCALL; Near syscall - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_FAR_SYSCALL
        zCopy {'&CV4_CALL_FAR_SYSCALL; Far syscall - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_THIS_CALL
        zCopy {'&CV4_CALL_THIS_CALL; This call - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_MIPS_CALL
        zCopy {'&CV4_CALL_MIPS_CALL; MIPS call - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_GENERIC
        zCopy {'&CV4_CALL_GENERIC; Generic - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED14
        zCopy {'&CV4_CALL_RESERVED14; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED15
        zCopy {'&CV4_CALL_RESERVED15; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED16
        zCopy {'&CV4_CALL_RESERVED16; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED17
        zCopy {'&CV4_CALL_RESERVED17; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED18
        zCopy {'&CV4_CALL_RESERVED18; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED19
        zCopy {'&CV4_CALL_RESERVED19; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED20
        zCopy {'&CV4_CALL_RESERVED20; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED21
        zCopy {'&CV4_CALL_RESERVED21; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED22
        zCopy {'&CV4_CALL_RESERVED22; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED23
        zCopy {'&CV4_CALL_RESERVED23; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED24
        zCopy {'&CV4_CALL_RESERVED24; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED25
        zCopy {'&CV4_CALL_RESERVED25; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED26
        zCopy {'&CV4_CALL_RESERVED26; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED27
        zCopy {'&CV4_CALL_RESERVED27; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED28
        zCopy {'&CV4_CALL_RESERVED28; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED29
        zCopy {'&CV4_CALL_RESERVED29; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED30
        zCopy {'&CV4_CALL_RESERVED30; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED31
        zCopy {'&CV4_CALL_RESERVED31; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED32
        zCopy {'&CV4_CALL_RESERVED32; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED33
        zCopy {'&CV4_CALL_RESERVED33; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED34
        zCopy {'&CV4_CALL_RESERVED34; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED35
        zCopy {'&CV4_CALL_RESERVED35; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED36
        zCopy {'&CV4_CALL_RESERVED36; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED37
        zCopy {'&CV4_CALL_RESERVED37; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED38
        zCopy {'&CV4_CALL_RESERVED38; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED39
        zCopy {'&CV4_CALL_RESERVED39; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED40
        zCopy {'&CV4_CALL_RESERVED40; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED41
        zCopy {'&CV4_CALL_RESERVED41; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED42
        zCopy {'&CV4_CALL_RESERVED42; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED43
        zCopy {'&CV4_CALL_RESERVED43; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED44
        zCopy {'&CV4_CALL_RESERVED44; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED45
        zCopy {'&CV4_CALL_RESERVED45; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED46
        zCopy {'&CV4_CALL_RESERVED46; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED47
        zCopy {'&CV4_CALL_RESERVED47; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED48
        zCopy {'&CV4_CALL_RESERVED48; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED49
        zCopy {'&CV4_CALL_RESERVED49; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED50
        zCopy {'&CV4_CALL_RESERVED50; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED51
        zCopy {'&CV4_CALL_RESERVED51; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED52
        zCopy {'&CV4_CALL_RESERVED52; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED53
        zCopy {'&CV4_CALL_RESERVED53; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED54
        zCopy {'&CV4_CALL_RESERVED54; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED55
        zCopy {'&CV4_CALL_RESERVED55; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED56
        zCopy {'&CV4_CALL_RESERVED56; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED57
        zCopy {'&CV4_CALL_RESERVED57; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED58
        zCopy {'&CV4_CALL_RESERVED58; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED59
        zCopy {'&CV4_CALL_RESERVED59; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED60
        zCopy {'&CV4_CALL_RESERVED60; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED61
        zCopy {'&CV4_CALL_RESERVED61; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED62
        zCopy {'&CV4_CALL_RESERVED62; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED63
        zCopy {'&CV4_CALL_RESERVED63; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED64
        zCopy {'&CV4_CALL_RESERVED64; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED65
        zCopy {'&CV4_CALL_RESERVED65; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED66
        zCopy {'&CV4_CALL_RESERVED66; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED67
        zCopy {'&CV4_CALL_RESERVED67; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED68
        zCopy {'&CV4_CALL_RESERVED68; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED69
        zCopy {'&CV4_CALL_RESERVED69; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED70
        zCopy {'&CV4_CALL_RESERVED70; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED71
        zCopy {'&CV4_CALL_RESERVED71; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED72
        zCopy {'&CV4_CALL_RESERVED72; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED73
        zCopy {'&CV4_CALL_RESERVED73; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED74
        zCopy {'&CV4_CALL_RESERVED74; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED75
        zCopy {'&CV4_CALL_RESERVED75; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED76
        zCopy {'&CV4_CALL_RESERVED76; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED77
        zCopy {'&CV4_CALL_RESERVED77; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED78
        zCopy {'&CV4_CALL_RESERVED78; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED79
        zCopy {'&CV4_CALL_RESERVED79; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED80
        zCopy {'&CV4_CALL_RESERVED80; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED81
        zCopy {'&CV4_CALL_RESERVED81; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED82
        zCopy {'&CV4_CALL_RESERVED82; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED83
        zCopy {'&CV4_CALL_RESERVED83; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED84
        zCopy {'&CV4_CALL_RESERVED84; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED85
        zCopy {'&CV4_CALL_RESERVED85; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED86
        zCopy {'&CV4_CALL_RESERVED86; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED87
        zCopy {'&CV4_CALL_RESERVED87; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED88
        zCopy {'&CV4_CALL_RESERVED88; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED89
        zCopy {'&CV4_CALL_RESERVED89; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED90
        zCopy {'&CV4_CALL_RESERVED90; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED91
        zCopy {'&CV4_CALL_RESERVED91; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED92
        zCopy {'&CV4_CALL_RESERVED92; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED93
        zCopy {'&CV4_CALL_RESERVED93; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED94
        zCopy {'&CV4_CALL_RESERVED94; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED95
        zCopy {'&CV4_CALL_RESERVED95; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED96
        zCopy {'&CV4_CALL_RESERVED96; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED97
        zCopy {'&CV4_CALL_RESERVED97; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED98
        zCopy {'&CV4_CALL_RESERVED98; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED99
        zCopy {'&CV4_CALL_RESERVED99; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED100
        zCopy {'&CV4_CALL_RESERVED100; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED101
        zCopy {'&CV4_CALL_RESERVED101; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED102
        zCopy {'&CV4_CALL_RESERVED102; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED103
        zCopy {'&CV4_CALL_RESERVED103; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED104
        zCopy {'&CV4_CALL_RESERVED104; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED105
        zCopy {'&CV4_CALL_RESERVED105; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED106
        zCopy {'&CV4_CALL_RESERVED106; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED107
        zCopy {'&CV4_CALL_RESERVED107; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED108
        zCopy {'&CV4_CALL_RESERVED108; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED109
        zCopy {'&CV4_CALL_RESERVED109; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED110
        zCopy {'&CV4_CALL_RESERVED110; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED111
        zCopy {'&CV4_CALL_RESERVED111; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED112
        zCopy {'&CV4_CALL_RESERVED112; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED113
        zCopy {'&CV4_CALL_RESERVED113; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED114
        zCopy {'&CV4_CALL_RESERVED114; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED115
        zCopy {'&CV4_CALL_RESERVED115; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED116
        zCopy {'&CV4_CALL_RESERVED116; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED117
        zCopy {'&CV4_CALL_RESERVED117; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED118
        zCopy {'&CV4_CALL_RESERVED118; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED119
        zCopy {'&CV4_CALL_RESERVED119; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED120
        zCopy {'&CV4_CALL_RESERVED120; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED121
        zCopy {'&CV4_CALL_RESERVED121; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED122
        zCopy {'&CV4_CALL_RESERVED122; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED123
        zCopy {'&CV4_CALL_RESERVED123; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED124
        zCopy {'&CV4_CALL_RESERVED124; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED125
        zCopy {'&CV4_CALL_RESERVED125; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED126
        zCopy {'&CV4_CALL_RESERVED126; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED127
        zCopy {'&CV4_CALL_RESERVED127; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED128
        zCopy {'&CV4_CALL_RESERVED128; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED129
        zCopy {'&CV4_CALL_RESERVED129; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED130
        zCopy {'&CV4_CALL_RESERVED130; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED131
        zCopy {'&CV4_CALL_RESERVED131; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED132
        zCopy {'&CV4_CALL_RESERVED132; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED133
        zCopy {'&CV4_CALL_RESERVED133; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED134
        zCopy {'&CV4_CALL_RESERVED134; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED135
        zCopy {'&CV4_CALL_RESERVED135; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED136
        zCopy {'&CV4_CALL_RESERVED136; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED137
        zCopy {'&CV4_CALL_RESERVED137; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED138
        zCopy {'&CV4_CALL_RESERVED138; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED139
        zCopy {'&CV4_CALL_RESERVED139; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED140
        zCopy {'&CV4_CALL_RESERVED140; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED141
        zCopy {'&CV4_CALL_RESERVED141; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED142
        zCopy {'&CV4_CALL_RESERVED142; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED143
        zCopy {'&CV4_CALL_RESERVED143; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED144
        zCopy {'&CV4_CALL_RESERVED144; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED145
        zCopy {'&CV4_CALL_RESERVED145; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED146
        zCopy {'&CV4_CALL_RESERVED146; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED147
        zCopy {'&CV4_CALL_RESERVED147; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED148
        zCopy {'&CV4_CALL_RESERVED148; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED149
        zCopy {'&CV4_CALL_RESERVED149; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED150
        zCopy {'&CV4_CALL_RESERVED150; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED151
        zCopy {'&CV4_CALL_RESERVED151; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED152
        zCopy {'&CV4_CALL_RESERVED152; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED153
        zCopy {'&CV4_CALL_RESERVED153; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED154
        zCopy {'&CV4_CALL_RESERVED154; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED155
        zCopy {'&CV4_CALL_RESERVED155; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED156
        zCopy {'&CV4_CALL_RESERVED156; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED157
        zCopy {'&CV4_CALL_RESERVED157; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED158
        zCopy {'&CV4_CALL_RESERVED158; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED159
        zCopy {'&CV4_CALL_RESERVED159; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED160
        zCopy {'&CV4_CALL_RESERVED160; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED161
        zCopy {'&CV4_CALL_RESERVED161; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED162
        zCopy {'&CV4_CALL_RESERVED162; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED163
        zCopy {'&CV4_CALL_RESERVED163; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED164
        zCopy {'&CV4_CALL_RESERVED164; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED165
        zCopy {'&CV4_CALL_RESERVED165; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED166
        zCopy {'&CV4_CALL_RESERVED166; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED167
        zCopy {'&CV4_CALL_RESERVED167; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED168
        zCopy {'&CV4_CALL_RESERVED168; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED169
        zCopy {'&CV4_CALL_RESERVED169; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED170
        zCopy {'&CV4_CALL_RESERVED170; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED171
        zCopy {'&CV4_CALL_RESERVED171; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED172
        zCopy {'&CV4_CALL_RESERVED172; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED173
        zCopy {'&CV4_CALL_RESERVED173; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED174
        zCopy {'&CV4_CALL_RESERVED174; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED175
        zCopy {'&CV4_CALL_RESERVED175; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED176
        zCopy {'&CV4_CALL_RESERVED176; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED177
        zCopy {'&CV4_CALL_RESERVED177; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED178
        zCopy {'&CV4_CALL_RESERVED178; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED179
        zCopy {'&CV4_CALL_RESERVED179; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED180
        zCopy {'&CV4_CALL_RESERVED180; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED181
        zCopy {'&CV4_CALL_RESERVED181; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED182
        zCopy {'&CV4_CALL_RESERVED182; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED183
        zCopy {'&CV4_CALL_RESERVED183; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED184
        zCopy {'&CV4_CALL_RESERVED184; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED185
        zCopy {'&CV4_CALL_RESERVED185; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED186
        zCopy {'&CV4_CALL_RESERVED186; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED187
        zCopy {'&CV4_CALL_RESERVED187; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED188
        zCopy {'&CV4_CALL_RESERVED188; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED189
        zCopy {'&CV4_CALL_RESERVED189; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED190
        zCopy {'&CV4_CALL_RESERVED190; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED191
        zCopy {'&CV4_CALL_RESERVED191; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED192
        zCopy {'&CV4_CALL_RESERVED192; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED193
        zCopy {'&CV4_CALL_RESERVED193; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED194
        zCopy {'&CV4_CALL_RESERVED194; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED195
        zCopy {'&CV4_CALL_RESERVED195; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED196
        zCopy {'&CV4_CALL_RESERVED196; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED197
        zCopy {'&CV4_CALL_RESERVED197; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED198
        zCopy {'&CV4_CALL_RESERVED198; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED199
        zCopy {'&CV4_CALL_RESERVED199; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED200
        zCopy {'&CV4_CALL_RESERVED200; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED201
        zCopy {'&CV4_CALL_RESERVED201; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED202
        zCopy {'&CV4_CALL_RESERVED202; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED203
        zCopy {'&CV4_CALL_RESERVED203; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED204
        zCopy {'&CV4_CALL_RESERVED204; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED205
        zCopy {'&CV4_CALL_RESERVED205; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED206
        zCopy {'&CV4_CALL_RESERVED206; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED207
        zCopy {'&CV4_CALL_RESERVED207; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED208
        zCopy {'&CV4_CALL_RESERVED208; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED209
        zCopy {'&CV4_CALL_RESERVED209; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED210
        zCopy {'&CV4_CALL_RESERVED210; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED211
        zCopy {'&CV4_CALL_RESERVED211; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED212
        zCopy {'&CV4_CALL_RESERVED212; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED213
        zCopy {'&CV4_CALL_RESERVED213; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED214
        zCopy {'&CV4_CALL_RESERVED214; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED215
        zCopy {'&CV4_CALL_RESERVED215; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED216
        zCopy {'&CV4_CALL_RESERVED216; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED217
        zCopy {'&CV4_CALL_RESERVED217; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED218
        zCopy {'&CV4_CALL_RESERVED218; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED219
        zCopy {'&CV4_CALL_RESERVED219; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED220
        zCopy {'&CV4_CALL_RESERVED220; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED221
        zCopy {'&CV4_CALL_RESERVED221; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED222
        zCopy {'&CV4_CALL_RESERVED222; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED223
        zCopy {'&CV4_CALL_RESERVED223; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED224
        zCopy {'&CV4_CALL_RESERVED224; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED225
        zCopy {'&CV4_CALL_RESERVED225; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED226
        zCopy {'&CV4_CALL_RESERVED226; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED227
        zCopy {'&CV4_CALL_RESERVED227; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED228
        zCopy {'&CV4_CALL_RESERVED228; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED229
        zCopy {'&CV4_CALL_RESERVED229; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED230
        zCopy {'&CV4_CALL_RESERVED230; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED231
        zCopy {'&CV4_CALL_RESERVED231; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED232
        zCopy {'&CV4_CALL_RESERVED232; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED233
        zCopy {'&CV4_CALL_RESERVED233; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED234
        zCopy {'&CV4_CALL_RESERVED234; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED235
        zCopy {'&CV4_CALL_RESERVED235; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED236
        zCopy {'&CV4_CALL_RESERVED236; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED237
        zCopy {'&CV4_CALL_RESERVED237; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED238
        zCopy {'&CV4_CALL_RESERVED238; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED239
        zCopy {'&CV4_CALL_RESERVED239; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED240
        zCopy {'&CV4_CALL_RESERVED240; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED241
        zCopy {'&CV4_CALL_RESERVED241; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED242
        zCopy {'&CV4_CALL_RESERVED242; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED243
        zCopy {'&CV4_CALL_RESERVED243; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED244
        zCopy {'&CV4_CALL_RESERVED244; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED245
        zCopy {'&CV4_CALL_RESERVED245; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED246
        zCopy {'&CV4_CALL_RESERVED246; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED247
        zCopy {'&CV4_CALL_RESERVED247; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED248
        zCopy {'&CV4_CALL_RESERVED248; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED249
        zCopy {'&CV4_CALL_RESERVED249; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED250
        zCopy {'&CV4_CALL_RESERVED250; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED251
        zCopy {'&CV4_CALL_RESERVED251; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED252
        zCopy {'&CV4_CALL_RESERVED252; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED253
        zCopy {'&CV4_CALL_RESERVED253; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED254
        zCopy {'&CV4_CALL_RESERVED254; Reserved - Hex Value:  ', 0}
    ..Else_If W$esi = &CV4_CALL_RESERVED255
        zCopy {'&CV4_CALL_RESERVED255; Reserved - Hex Value:  ', 0}
    ..End_if

EndP
________________________________________________________________________________


Proc WriteRawDataDebugSContantLeafTypeArgListItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.ArgList', 0}
        zCopy D@Text1
        If D$ArgListIndexCount <> 0
            ZCopy LeafTypeArgListIndice
        End_If
        zCopy D@Text2
    pop esi

    call WriteCVBPRel32TypeEquates
    xor eax eax
    lodsw | call WriteEax
    mov W$edi CRLF | add edi 2

EndP

_________________________________________________

[CVLeafTypeArgListIndice: 'Indice', 0
CVLeafTypeArgListOldCVType: 'OldCVType', 0]


;    mov D$LeafTypeArgListIndice '0000', D$LeafTypeArgListIndice+4 '01'
;            call IncrementLeafTypeArgListIndex

[LeafTypeArgListIndice: '000001', 0]

IncrementLeafTypeArgListIndex:
    lea ebx D$LeafTypeArgListIndice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

[ArgListIndexCount: D$ 0]

Proc WriteRawDataDebugSContantLeafTypeArgList:
    Local @OldCodeView
    Uses eax, ecx, ebx

    mov D@OldCodeView &FALSE
    If W$esi-2 = &LF_ARGLIST_CV3
        mov D@OldCodeView &TRUE
    End_If

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        push ecx
        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.LeafType.ArgList.', 0}
            zCopy {'ArgCount: W$ ', 0}
        pop esi
        xor eax eax
        lodsw | call WriteEax
        mov W$edi CRLF | add edi 2
        pop ecx

        movzx ecx W$esi-2

    mov D$ArgListIndexCount ecx
    mov D$LeafTypeArgListIndice '0000', D$LeafTypeArgListIndice+4 '01'

    .If ecx <> 0 ; if the amount of bytes is 0 jmp over

        L0:
            call WriteRawDataDebugSContantLeafTypeArgListItem CVLeafTypeArgListIndice, {': W$ ', 0}
            If D@OldCodeView = &TRUE
                call WriteRawDataDebugSContantLeafTypeArgListItem CVLeafTypeArgListOldCVType, {': W$ ', 0}
            End_If

            call IncrementLeafTypeArgListIndex
        Loop L0<

    .End_If

    pop ebx

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP


________________________________________________________________________________

[NestedLeafType: D$ 0]

Proc WriteRawDataDebugSContantLeafTypeFieldList:
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx
    ;mov D@EndAddr ebx


    mov D$NestedLeafType &TRUE
    mov D$LeafTypeArrayObjIndice '0000', D$LeafTypeArrayObjIndice+4 '01'

    .Do

        push esi
            mov W$edi CRLF | add edi 2
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.LeafType.Descriptor', 0}
        pop esi

        ; from here we must insert a Buffer flag saying it is nested to we use the array indexs
;            push ebx

        ..If W$esi = &LF_BCLASS
            push esi | zCopy {'.BClass.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_BCLASS', 0}
            call WriteRawDataDebugSContantLeafTypeBClass &FALSE

        ..Else_If W$esi = &LF_VBCLASS
            push esi | zCopy {'.VBClass.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VBCLASS', 0}
            call WriteRawDataDebugSContantLeafTypeVBClass &FALSE

        ..Else_If W$esi = &LF_IVBCLASS
            push esi | zCopy {'.IVBClass.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_IVBCLASS', 0}
            call WriteRawDataDebugSContantLeafTypeIVBClass &FALSE

        ..Else_If W$esi = &LF_ENUMERATE
            push esi | zCopy {'.Enumerate.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ENUMERATE', 0}
            call WriteRawDataDebugSContantLeafTypeEnumerate &FALSE

        ..Else_If W$esi = &LF_FRIENDFCN
            push esi | zCopy {'.FriendFcn.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_FRIENDFCN', 0}
            call WriteRawDataDebugSContantLeafTypeFriendFcn &FALSE

        ..Else_If W$esi = &LF_INDEX
            push esi | zCopy {'.Index.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_INDEX', 0}
            call WriteRawDataDebugSContantLeafTypeIndex &FALSE

        ..Else_If W$esi = &LF_MEMBER;, W$esi = 01405
            ;WriteRawDataDebugSRegister
            ;push ebx
            push esi | zCopy {'.Member.Arr', 0} | pop esi
                If W$esi = &LF_MEMBER
                    call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MEMBER', 0}
                Else
                    ;call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MEMBER_CV3', 0}
                End_If
                call WriteRawDataDebugSContantLeafTypeMember &FALSE
            ;pop ebx
        ..Else_If W$esi = &LF_STMEMBER
            push esi | zCopy {'.STMember.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_STMEMBER', 0}
            call WriteRawDataDebugSContantLeafTypeSTMember &FALSE

        ..Else_If W$esi = &LF_METHOD
            push esi | zCopy {'.Method.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_METHOD', 0}
            call WriteRawDataDebugSContantLeafTypeMethod &FALSE

        ..Else_If W$esi = &LF_NESTTYPE
            push esi | zCopy {'.NestType.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_NESTTYPE', 0}
            call WriteRawDataDebugSContantLeafTypeNestType &FALSE

        ..Else_If W$esi = &LF_VFUNCTAB
            push esi | zCopy {'.VFuncTab.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VFUNCTAB', 0}
            call WriteRawDataDebugSContantLeafTypeVFuncTab &FALSE

        ..Else_If W$esi = &LF_FRIENDCLS
            push esi | zCopy {'.FriendCls.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_FRIENDCLS', 0}
            call WriteRawDataDebugSContantLeafTypeFriendCls &FALSE

        ..Else_If W$esi = &LF_ONEMETHOD
            push esi | zCopy {'.OneMethod.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ONEMETHOD', 0}
            call WriteRawDataDebugSContantLeafTypeOneMethod &FALSE

        ..Else_If W$esi = &LF_VFUNCOFF
            push esi | zCopy {'.VFuncOffset.Arr', 0} | pop esi
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VFUNCOFF', 0}
            call WriteRawDataDebugSContantLeafTypeVFuncOffset &FALSE

        ..End_If
;            push ebx
            .If_And B$esi > &LF_PAD0, B$esi =< &LF_PAD15

                movzx ecx B$esi
                sub ecx 0F0
                mov W$edi CRLF | add edi 2
                call WriteCVdataTitle

                    mov edx 0
                    xor eax eax

                L0:

                    movzx eax B$esi
                    push ebx | call WriteEax | pop ebx

                    mov W$edi ', ' | add edi 2 | inc esi
                    inc edx
                    If edx > 15
                        mov W$edi CRLF | add edi 2 | mov edx 0
                        mov D$edi '    ', D$edi+4 '    ' | add edi 8
                    End_If
                Loop L0<
                sub edi 2
                dec edi
                mov W$edi+1 CRLF | add edi 2
                inc edi

            .End_If

            call IncrementLeafTypeArrayIndex
;            pop ebx
            ;pop ebx
        .Loop_Until esi = ebx
;L2:
    mov D$NestedLeafType &FALSE
    mov W$edi CRLF | add edi 2
;    sub edi 2
 ;   dec edi
  ;  mov W$edi+1 CRLF | add edi 2
   ; inc edi

EndP


________________________________________________________________________________

[LeafTypeArrayObjIndice: '000001', 0]

Proc IncrementLeafTypeArrayIndex:

    pushad

    lea ebx D$LeafTypeArrayObjIndice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While

    popad

EndP
____________________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeBClassItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.BClass.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            xor eax eax
            lodsw | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeBClassType: 'Type', 0
 CVLeafTypeVFuncTypeBClassAttribute: 'Attribute', 0
 CVLeafTypeVFuncTypeBClassOffset: 'Offset', 0]

Proc WriteRawDataDebugSContantLeafTypeBClass:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeBClassItem CVLeafTypeVFuncTypeBClassType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeBClassItem CVLeafTypeVFuncTypeBClassAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeBClassItem CVLeafTypeVFuncTypeBClassOffset, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVBClassItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.VBClass.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Inde'; From "Index" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'bTyp'; From "bType" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'VbTy'; From "VbType" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Vbpo'; From "Vbpoff" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Vbof'; From "Vboff" string
            xor eax eax
            lodsw | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeVBClassType: 'Index', 0
 CVLeafTypeVFuncTypeVBClassbType: 'bType', 0
 CVLeafTypeVFuncTypeVBClassvbType: 'VbType', 0
 CVLeafTypeVFuncTypeVBClassAttribute: 'Attribute', 0
 CVLeafTypeVFuncTypeVBClassvbpoff: 'Vbpoff', 0
 CVLeafTypeVFuncTypeVBClassvboff: 'Vboff', 0]

Proc WriteRawDataDebugSContantLeafTypeVBClass:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassbType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassvbType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassvbpoff, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeVBClassItem CVLeafTypeVFuncTypeVBClassvboff, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeIVBClassItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.IVBClass.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Inde'; From "Index" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'bTyp'; From "bType" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'VbTy'; From "VbType" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Vbpo'; From "Vbpoff" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Vbof'; From "Vboff" string
            xor eax eax
            lodsw | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeIVBClassType: 'Index', 0
 CVLeafTypeVFuncTypeIVBClassbType: 'bType', 0
 CVLeafTypeVFuncTypeIVBClassvbType: 'VbType', 0
 CVLeafTypeVFuncTypeIVBClassAttribute: 'Attribute', 0
 CVLeafTypeVFuncTypeIVBClassvbpoff: 'Vbpoff', 0
 CVLeafTypeVFuncTypeIVBClassvboff: 'Vboff', 0]


Proc WriteRawDataDebugSContantLeafTypeIVBClass:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassbType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassvbType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassvbpoff, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeIVBClassItem CVLeafTypeVFuncTypeIVBClassvboff, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeEnumerateItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Enumerate.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1


        ..If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Valu'; From "Value" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeEnumerateAttribute: 'Attribute', 0
 CVLeafTypeVFuncTypeEnumerateValue: 'Value', 0
 CVLeafTypeVFuncTypeEnumerateNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeEnumerateName: 'Name', 0]

Proc WriteRawDataDebugSContantLeafTypeEnumerate:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateValue, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeEnumerateItem CVLeafTypeVFuncTypeEnumerateName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeFriendFcnItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.FriendFcn.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeFriendFcnType: 'Type', 0
 CVLeafTypeVFuncTypeFriendFcnNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeFriendFcnName: 'Name', 0]

Proc WriteRawDataDebugSContantLeafTypeFriendFcn:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeFriendFcnItem CVLeafTypeVFuncTypeFriendFcnType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeFriendFcnItem CVLeafTypeVFuncTypeFriendFcnNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeFriendFcnItem CVLeafTypeVFuncTypeFriendFcnName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeIndexItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Type.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        xor eax eax
        lodsw | call WriteEax

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeTypeIndex: 'Index', 0]

Proc WriteRawDataDebugSContantLeafTypeIndex:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeIndexItem CVLeafTypeVFuncTypeTypeIndex, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeMemberItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Member.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'OldC' ; from "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeMemberOldCVType: 'OldCVType', 0
 CVLeafTypeVFuncTypeMemberType: 'Type', 0
 CVLeafTypeVFuncTypeMemberAttribute: 'Attribute', 0
 CVLeafTypeVFuncTypeMemberOffset: 'Offset', 0
 CVLeafTypeVFuncTypeMemberNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeMemberName: 'Name', 0]
;WriteRawDataDebugSRegister
Proc WriteRawDataDebugSContantLeafTypeMember:
    Arguments @Nested
    Local @OldCodeView
    Uses eax, ecx, ebx

    mov D@OldCodeView &FALSE
    If W$esi-2 = 01405
        mov D@OldCodeView &TRUE
    End_If
    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        If D@OldCodeView = &TRUE
            Call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberOldCVType , {': W$ ', 0}
        End_If
        call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberOffset, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMemberItem CVLeafTypeVFuncTypeMemberName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeSTMemberItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.StMember.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeStMemberType: 'Type', 0
 CVLeafTypeVFuncTypeStMemberAttribute: 'Attribute', 0
 CVLeafTypeVFuncTypeStMemberNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeStMemberName: 'Name', 0]

Proc WriteRawDataDebugSContantLeafTypeSTMember:
    Arguments @Nested
    Uses eax, ecx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberAttribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeSTMemberItem CVLeafTypeVFuncTypeStMemberName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeMethodItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.Method.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Coun'; From "Count" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'mLis'; From "mList" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeMethodCount: 'Count', 0
 CVLeafTypeVFuncTypeMethodmList: 'mList', 0
 CVLeafTypeVFuncTypeMethodNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeMethodTypeName: 'Name', 0]

Proc WriteRawDataDebugSContantLeafTypeMethod:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodCount, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodmList, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeMethodItem CVLeafTypeVFuncTypeMethodTypeName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
_________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeNestTypeItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.NestedType.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Inde'; From "Index" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeNestTypeIndex: 'Index', 0
 CVLeafTypeVFuncTypeNestTypeNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeNestTypeName: 'Name', 0]

Proc WriteRawDataDebugSContantLeafTypeNestType:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeNestTypeItem CVLeafTypeVFuncTypeNestTypeIndex, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeNestTypeItem CVLeafTypeVFuncTypeNestTypeNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeNestTypeItem CVLeafTypeVFuncTypeNestTypeName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
_________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVFuncTabItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.VFuncTab.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        xor eax eax
        lodsw | call WriteEax

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeVFuncTabType: 'Type', 0]

Proc WriteRawDataDebugSContantLeafTypeVFuncTab:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeVFuncTabItem CVLeafTypeVFuncTypeVFuncTabType, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeFriendClsItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.FriendClass.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        xor eax eax
        lodsw | call WriteEax

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeFriendClsType: 'Type', 0]

Proc WriteRawDataDebugSContantLeafTypeFriendCls:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeFriendClsItem CVLeafTypeVFuncTypeFriendClsType, {': W$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

__________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeOneMethodItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.OneMethod.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax = 'Attr'; From "Attribute" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'vBas'; From "vBaseOffset" string
            lodsd | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncTypeOneMethodattribute: 'Attribute', 0
 CVLeafTypeVFuncTypeOneMethodType: 'Type', 0
 CVLeafTypeVFuncTypeOneMethodvBaseOffset: 'vBaseOffset', 0
 CVLeafTypeVFuncTypeOneMethodNameLenght: 'NameLenght', 0
 CVLeafTypeVFuncTypeOneMethodName: 'Name', 0]

Proc WriteRawDataDebugSContantLeafTypeOneMethod:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodattribute, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodvBaseOffset, {': D$ ', 0}
        call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodNameLenght, {': B$ ', 0}
        call WriteRawDataDebugSContantLeafTypeOneMethodItem CVLeafTypeVFuncTypeOneMethodName, {': B$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVFuncOffsetItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.VFunc.', 0}
        zCopy D@Text1
        If D$NestedLeafType = &TRUE
            zCopy {'.Arr', 0}
            zCopy LeafTypeArrayObjIndice
        End_If
        zCopy D@Text2
    pop esi

        mov eax D@Text1

         ..If D$eax = 'Type'; From "Type" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | call WriteEax

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________________

[CVLeafTypeVFuncType: 'Type', 0
 CVLeafTypeVFuncOffset: 'Offset', 0]

Proc WriteRawDataDebugSContantLeafTypeVFuncOffset:
    Arguments @Nested
    Uses eax, ecx, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx
        call WriteRawDataDebugSContantLeafTypeVFuncOffsetItem CVLeafTypeVFuncType, {': W$ ', 0}
        call WriteRawDataDebugSContantLeafTypeVFuncOffsetItem CVLeafTypeVFuncOffset, {': D$ ', 0}
        sub edi 2
    pop ebx

    ; Is this structure inside another one ? Yes, exit. FALSE = Nested. TRUE = not nested
    ; This is because the leaf types:
    ; &LF_BCLASS; &LF_VBCLASS; &LF_IVBCLASS; &LF_ENUMERATE; &LF_FRIENDFCN; &LF_INDEX
    ; &LF_MEMBER; &LF_STMEMBER; &LF_METHOD; &LF_NESTTYPE; &LF_VFUNCTAB; &LF_ONEMETHOD ; &LF_VFUNCOFF
    ; May exists inside the &LF_FIELDLIST Structure like an array of structures.
    ; So we need to jmp over the lenght check if we have nested structures like this.

    On D@Nested = &FALSE, ExitP

    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeComplexNumber:
    Argument @Text, @Size
    Uses eax, ecx

    mov ebx esi
    mov ecx D@Size
    add ebx ecx

    .If esi <> ebx
        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.LeafType.', 0}
            zCopy D@Text
            zCopy {': B$ ', 0}
        pop esi

        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

        sub edi 2
        dec edi
        mov W$edi+1 CRLF | add edi 2
        pop ecx
        inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
________________________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeVarStrItem:
    Argument @Text1, @Text2
    uses eax, ecx

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'Constant.LeafType.VarStr', 0}
        zCopy D@Text1
        zCopy D@Text2
    pop esi

        mov eax D@Text1

        ..If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx W$esi-2
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

______________________________________________

Proc WriteRawDataDebugSContantLeafTypeVarStr:
    Uses eax, ecx

    call WriteRawDataDebugSContantLeafTypeVarStrItem CVCompileNameLenght, {': W$ ', 0}
    call WriteRawDataDebugSContantLeafTypeVarStrItem CVCompileName, {': B$ ', 0}
    sub edi 2

EndP
______________________________________________

Proc WriteRawDataDebugSContantLeafTypeEquate:
    Argument @Text
    uses eax, ecx, ebx


    If D$NestedLeafType = &TRUE
        push esi | zCopy LeafTypeArrayObjIndice | pop esi
    End_If
    push esi
        zCopy {': W$ ', 0}
        zCopy D@Text
        zCopy {' ; Hex Value:  ', 0}
    pop esi
    xor eax eax
    lodsw | call WriteEax
    mov W$edi CRLF | add edi 2

EndP

___________________________________________

[LeafTypeError: D$ &TRUE]

; The PDebug section is nothing but a Type.
; Note: Check the file SDKUTIL from lcc library

Proc WriteRawDataDebugSContantLeafTypeItem:
    Uses eax, ecx

    mov D$LeafTypeError &TRUE ; In Any case, the default is True, meaning that we have no errors.

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.LeafType', 0}
        pop esi

    ; 1st we need to identify the Undefined Leaf Types. Accordying to the documentation, we have:

    ; a) No LF_... index can have a value of 0x0000.
    ; b) Because of the method used to maintain natural alignment in complex lists, no leaf index can
    ;    have a value greater than or equal to 0xf000.
    ; c) Also, no leaf index can have a value such that the least significant 8 bits of the value
    ;    is greater than or equal to 0xf0.

    ...If W$esi = 0
        jmp L0>
    ...Else_If W$esi >= 0F000
L0:     push esi | zCopy {': W$ ', 0} | pop esi
        xor eax eax
        lodsw | call WriteEax
        zCopy {' ; No Defined Type', 0}

        mov D$LeafTypeError &FALSE ; Set the error case

    ...Else_If B$esi >= 0F0

        push esi | zCopy {'.Padding: B$ ', 0} | pop esi

        push esi

        .If B$esi = &LF_PAD0
            zCopy {'&LF_PAD0 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD1
            zCopy {'&LF_PAD1 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD2
            zCopy {'&LF_PAD2 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD3
            zCopy {'&LF_PAD3 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD4
            zCopy {'&LF_PAD4 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD5
            zCopy {'&LF_PAD5 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD6
            zCopy {'&LF_PAD6 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD7
            zCopy {'&LF_PAD7 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD8
            zCopy {'&LF_PAD8 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD9
            zCopy {'&LF_PAD9 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD10
            zCopy {'&LF_PAD10 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD11
            zCopy {'&LF_PAD11 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD12
            zCopy {'&LF_PAD12 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD13
            zCopy {'&LF_PAD13 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD14
            zCopy {'&LF_PAD14 ; Hex Value:  ', 0}
        .Else_If B$esi = &LF_PAD15
            zCopy {'&LF_PAD15 ; Hex Value:  ', 0}
        .End_If

        pop esi

        xor eax eax
        lodsb | call WriteEax

        push esi
            zCopy {' - No Defined Type', 0}
            mov W$edi CRLF | add edi 2
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'.Undefined: B$ ', 0}
        pop esi

        xor eax eax
        lodsb | call WriteEax
        zCopy {' ; No Defined Type', 0}

        mov D$LeafTypeError &FALSE ; Set the error case

    ; 2nd We must now identify the known defined Types.

    ...Else

        ..If_Or W$esi = &LF_MODIFIER, W$esi = &LF_MODIFIER_CV3
            If W$esi = &LF_MODIFIER
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MODIFIER', 0}
            Else
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MODIFIER_CV3', 0}
            End_If
            call WriteRawDataDebugSContantLeafTypeModifier

        ..Else_If_Or W$esi = &LF_POINTER, W$esi = &LF_POINTER_CV3 ; see sdkutil.lib to we handle the equates for the different Bits (A true hell)
            If W$esi = &LF_POINTER
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_POINTER', 0}
            Else
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_POINTER_CV3', 0}
            End_If
            call WriteRawDataDebugSContantLeafTypePointer

        ..Else_If W$esi = &LF_ARRAY
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ARRAY', 0}
            call WriteRawDataDebugSContantLeafTypeArray

        ..Else_If W$esi = &LF_CLASS
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_CLASS', 0}
            call WriteRawDataDebugSContantLeafTypeStructure

        ..Else_If_Or W$esi = &LF_STRUCTURE, W$esi = &LF_STRUCTURE_CV3
            If W$esi = &LF_STRUCTURE
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_STRUCTURE', 0}
            Else
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_STRUCTURE_CV3', 0}
            End_If
            call WriteRawDataDebugSContantLeafTypeStructure

        ..Else_If_Or W$esi = &LF_UNION, W$esi = &LF_UNION_CV3
            If W$esi = &LF_UNION
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_UNION', 0}
            Else
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_UNION_CV3', 0}
            End_If
            call WriteRawDataDebugSContantLeafTypeUnion

        ..Else_If W$esi = &LF_ENUM
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ENUM', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_PROCEDURE
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_PROCEDURE', 0}
            call WriteRawDataDebugSContantLeafTypeProcedure

        ..Else_If_Or W$esi = &LF_MFUNCTION, W$esi = &LF_MFUNCTION_CV3
            If W$esi = &LF_MFUNCTION
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MFUNCTION', 0}
            Else
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MFUNCTION_CV3', 0}
            End_If
                call WriteRawDataDebugSContantLeafTypeMFunction

        ..Else_If W$esi = &LF_VTSHAPE
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VTSHAPE', 0}
            sub esi 2
            jmp L0>>
            ; Note to Guga. The error in sdkutil is here. Missing this parse

        ..Else_If W$esi = &LF_COBOL0
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_COBOL0', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_COBOL1
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_COBOL1', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_BARRAY
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_BARRAY', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_LABEL
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_LABEL', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_NULL
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_NULL', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_NOTTRAN
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_NOTTRAN', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMARRAY
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DIMARRAY', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_VFTPATH
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VFTPATH', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_PRECOMP
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_PRECOMP', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_ENDPRECOMP
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ENDPRECOMP', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_OEM
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_OEM', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_RESERVED
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_RESERVED', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_SKIP
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_SKIP', 0}
            jmp L0>>

        ..Else_If_Or W$esi = &LF_ARGLIST, W$esi = &LF_ARGLIST_CV3
            If W$esi = &LF_ARGLIST
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ARGLIST', 0}
            Else
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ARGLIST_CV3', 0}
            End_If
            call WriteRawDataDebugSContantLeafTypeArgList

        ..Else_If W$esi = &LF_DEFARG
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DEFARG', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_LIST
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_LIST', 0}
            jmp L0>>

        ..Else_If_Or W$esi = &LF_FIELDLIST;, W$esi = 01203
        ;WriteRawDataDebugSRegister
            If W$esi = &LF_FIELDLIST
                call WriteRawDataDebugSContantLeafTypeEquate {'&LF_FIELDLIST', 0}
            Else
                ;call WriteRawDataDebugSContantLeafTypeEquate {'&LF_FIELDLIST_CV3', 0}
            End_If
            call WriteRawDataDebugSContantLeafTypeFieldList

        ..Else_If W$esi = &LF_DERIVED
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DERIVED', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_BITFIELD
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_BITFIELD', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_METHODLIST
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_METHODLIST', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMCONU
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DIMCONU', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMCONLU
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DIMCONLU', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMVARU
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DIMVARU', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_DIMVARLU
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_DIMVARLU', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_REFSYM
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_REFSYM', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_BCLASS
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_BCLASS', 0}
            call WriteRawDataDebugSContantLeafTypeBClass &FALSE

        ..Else_If W$esi = &LF_VBCLASS
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VBCLASS', 0}
            call WriteRawDataDebugSContantLeafTypeVBClass &FALSE

        ..Else_If W$esi = &LF_IVBCLASS
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_IVBCLASS', 0}
            call WriteRawDataDebugSContantLeafTypeIVBClass &FALSE

        ..Else_If W$esi = &LF_ENUMERATE
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ENUMERATE', 0}
            call WriteRawDataDebugSContantLeafTypeEnumerate &FALSE

        ..Else_If W$esi = &LF_FRIENDFCN
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_FRIENDFCN', 0}
            call WriteRawDataDebugSContantLeafTypeFriendFcn &FALSE

        ..Else_If W$esi = &LF_INDEX
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_INDEX', 0}
            call WriteRawDataDebugSContantLeafTypeIndex &FALSE

        ..Else_If W$esi = &LF_MEMBER
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_MEMBER', 0}
            call WriteRawDataDebugSContantLeafTypeMember &FALSE

        ..Else_If W$esi = &LF_STMEMBER
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_STMEMBER', 0}
            call WriteRawDataDebugSContantLeafTypeSTMember &FALSE

        ..Else_If W$esi = &LF_METHOD
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_METHOD', 0}
            call WriteRawDataDebugSContantLeafTypeMethod &FALSE

        ..Else_If W$esi = &LF_NESTTYPE
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_NESTTYPE', 0}
            call WriteRawDataDebugSContantLeafTypeNestType &FALSE

        ..Else_If W$esi = &LF_VFUNCTAB
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VFUNCTAB', 0}
            call WriteRawDataDebugSContantLeafTypeVFuncTab &FALSE

        ..Else_If W$esi = &LF_FRIENDCLS
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_FRIENDCLS', 0}
            call WriteRawDataDebugSContantLeafTypeFriendCls &FALSE

        ..Else_If W$esi = &LF_ONEMETHOD
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ONEMETHOD', 0}
            call WriteRawDataDebugSContantLeafTypeOneMethod &FALSE

        ..Else_If W$esi = &LF_VFUNCOFF
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VFUNCOFF', 0}
            call WriteRawDataDebugSContantLeafTypeVFuncOffset &FALSE

        ..Else_If W$esi = &LF_NUMERIC
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_NUMERIC', 0}
            jmp L0>>

        ..Else_If W$esi = &LF_CHAR
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_CHAR', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Char', 0}, 1

        ..Else_If W$esi = &LF_SHORT
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_SHORT', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Short', 0}, 2

        ..Else_If W$esi = &LF_USHORT
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_USHORT', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'UShort', 0}, 2

        ..Else_If W$esi = &LF_LONG
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_LONG', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Long', 0}, 4

        ..Else_If W$esi = &LF_ULONG
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_ULONG', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'ULong', 0}, 4

        ..Else_If W$esi = &LF_REAL32
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_REAL32', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Float32Bit', 0}, 4

        ..Else_If W$esi = &LF_REAL64
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_REAL64', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Float64Bit', 0}, 8

        ..Else_If W$esi = &LF_REAL80
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_REAL80', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Float80Bit', 0}, 10

        ..Else_If W$esi = &LF_REAL128
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_REAL128', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Float128Bit', 0}, 16

        ..Else_If W$esi = &LF_QUADWORD
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_QUADWORD', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Quadword', 0}, 8

        ..Else_If W$esi = &LF_UQUADWORD
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_UQUADWORD', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'UQuadword', 0}, 8

        ..Else_If W$esi = &LF_REAL48
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_REAL48', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Real48Bit', 0}, 6

        ..Else_If W$esi = &LF_COMPLEX32
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_COMPLEX32', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex32BitReal', 0}, 4
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex32BitImaginary', 0}, 4

        ..Else_If W$esi = &LF_COMPLEX64
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_COMPLEX64', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex64BitReal', 0}, 8
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex64BitImaginary', 0}, 8

        ..Else_If W$esi = &LF_COMPLEX80
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_COMPLEX80', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex80BitReal', 0}, 10
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex80BitImaginary', 0}, 10

        ..Else_If W$esi = &LF_COMPLEX128
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_COMPLEX128', 0}
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex128BitReal', 0}, 16
            call WriteRawDataDebugSContantLeafTypeComplexNumber {'Complex128BitImaginary', 0}, 16

        ..Else_If W$esi = &LF_VARSTRING
            call WriteRawDataDebugSContantLeafTypeEquate {'&LF_VARSTRING', 0}
            call WriteRawDataDebugSContantLeafTypeVarStr

        ..Else
L0:

            push ecx
            push eax
            push esi
                If D$NestedLeafType = &TRUE
                    zCopy LeafTypeArrayObjIndice
                End_If
                zCopy {'.TypeUnknown: W$ ', 0}
            pop esi
            xor eax eax
            lodsw | call WriteEax
            mov W$edi CRLF | add edi 2
            pop eax
            pop ecx
            sub D$SizeAdd 2


        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.LeafType', 0}
        pop esi


            call WriteRawDataDebugSContantLeafTypeUnkown
            mov D$LeafTypeError &FALSE ; Set the error case

        ..End_If

    ...End_If

    mov W$edi CRLF | add edi 2

EndP
__________________________________________________________________

Proc WriteRawDataDebugSContantLeafTypeUnkown:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
;    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx
;;
    push ebx

                    push esi
                        mov D$edi '    ' | add edi 4
                        call WriteObjIndice
                        zCopy {"Sec", 0}
                        zCopy SectionHeaderNumber
                        zCopy {".Index", 0}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {'Constant.LeafType.Unknown: B$ ', 0}
                    pop esi
    pop ebx
;;
;;

    push ebx
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayElemType , {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayIdxType , {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayLength, {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayNamelenght, {': B$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayName, {': B$ ', 0}
        sub edi 2
    pop ebx
;;
    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
;;
        mov W$edi CRLF | add edi 2
        ;call WriteCVdataTitle
                    push esi
                        mov D$edi '    ' | add edi 4
                        call WriteObjIndice
                        zCopy {"Sec", 0}
                        zCopy SectionHeaderNumber
                        zCopy {".Index", 0}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {'Constant.LeafType.Unknown: B$ ', 0}
                    pop esi

;;
        push esi | ZCopy {'.Unknown: B$ ', 0} | pop esi

        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP
__________________________________________________________________

Proc WriteRawDataDebugSContantItem:
    Argument @Text1, @Text2, @UseLen
    uses eax, ecx, edx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Type' ; from "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'OldC' ; from "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            If D@UseLen = &TRUE ; this happens only in S_CONSTANT_CV2.
                push eax
                    call StrLenProc esi
                    mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                pop eax
            Else
                movzx ecx B$esi-1
            End_If

            mov edx esi | add edx ecx


            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP
_______________________________________


;;
Structures used:

. Note, in all equates used to dirrefernciate the structures, when Type member is 0 (&T_NOTYPE), the structure
is related to a enumeration, and the leaf member types (achieved from WriteRawDataDebugSContantLeafTypeItem) are not
used. Instead, the Constant.LeafType is replaced by an EnumerationValue member

 - S_CONSTANT_CV3 uses:
 
     CV3.Length: W$ 015
     CV3.Index: W$ &S_CONSTANT_CV3; Hex Value:  01002
     CV3.Constant.OldCVType: W$ 01058
     CV3.Constant.Type: W$ &T_NOTYPE ; Uncharacterized type (no type) - Hex Value:  0
     CV3.Constant.EnumerationValue: W$ 02 ; This symbol is an enumeration constant type.
     CV3.Constant.NameLenght: B$ 0C
     CV3.Constant.Name: B$ 'STUB_MARSHAL'

 - S_CONSTANT_CV2 uses:
 
     CV3.Length: W$ 015
     CV3.Index: W$ &S_CONSTANT_CV2; Hex Value:  01107
     CV3.Constant.OldCVType: W$ 01058
     CV3.Constant.Type: W$ &T_NOTYPE ; Uncharacterized type (no type) - Hex Value:  0
     CV3.Constant.EnumerationValue: W$ 02 ; This symbol is an enumeration constant type.
     CV3.Constant.NameLenght: B$ 0C
     CV3.Constant.Name: B$ 'STUB_MARSHAL'

;;

[CVConstantOldCVType: 'OldCVType', 0
 CVConstantType: 'Type', 0
 CVConstantNameLenght: 'NameLenght', 0
 CVConstantName: 'Name', 0]

Proc WriteRawDataDebugSContant:
    Local @OldCodeView
    Uses ecx, eax


    mov D@OldCodeView &FALSE
    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

    If W$esi-2 = &S_CONSTANT_CV3
        call WriteRawDataDebugSContantItem CVConstantOldCVType, {': W$ ', 0}, &FALSE
    Else_If W$esi-2 = &S_CONSTANT_CV2
        call WriteRawDataDebugSContantItem CVConstantOldCVType, {': W$ ', 0}, &FALSE
        mov D@OldCodeView &TRUE
    End_If

    call WriteRawDataDebugSContantItem CVConstantType, {': W$ ', 0}, &FALSE

    .If W$esi-2 = &T_NOTYPE
        push ecx
        push eax
        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'Constant.EnumerationValue: W$ ', 0}
        pop esi
        xor eax eax
        lodsw | call WriteEax

        push esi | zCopy {' ; This symbol is an enumeration constant type.', 0} | pop esi
        mov W$edi CRLF | add edi 2
        pop eax
        pop ecx
    .Else
        call WriteRawDataDebugSContantLeafTypeItem
    .End_If

    .If D$LeafTypeError <> &FALSE ; No Errors. Do next Line. Jmp otherwise
        If D@OldCodeView = &FALSE
            call WriteRawDataDebugSContantItem CVCompileNameLenght, {': B$ ', 0}, &FALSE
            call WriteRawDataDebugSContantItem CVCompileName, {': B$ ', 0}, &FALSE
        Else
            call WriteRawDataDebugSContantItem CVCompileName, {': B$ ', 0}, &TRUE
        End_If
    .End_If


    pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

__________________________________________________________________

[CVUDTOldCVType: 'OldCVType', 0
 CVUDTType: 'Type' , 0
 CVUDTNamelenght: 'NameLenght' , 0
 CVUDTName: 'Name' , 0]

Proc WriteRawDataDebugSUDT:
    Local @OldCodeView
    Uses ecx

    mov D@OldCodeView 0
    If W$esi-2 = &S_UDT_CV3
        mov D@OldCodeView 1
    Else_If W$esi-2 = &S_UDT_CV2
        mov D@OldCodeView 2
    End_If

    If D@OldCodeView <> 0
        Call WriteRawDataDebugSUDTItem CVUDTOldCVType , {': W$ ', 0}, &FALSE
    End_If

    Call WriteRawDataDebugSUDTItem CVUDTType , {': W$ ', 0}, &FALSE

    If D@OldCodeView = 2
        Call WriteRawDataDebugSUDTItem CVUDTName , {': B$ ', 0}, &TRUE
    Else
        Call WriteRawDataDebugSUDTItem CVUDTNamelenght , {': B$ ', 0}, &FALSE
        Call WriteRawDataDebugSUDTItem CVUDTName , {': B$ ', 0}, &FALSE
    End_If
EndP
__________________________________________________________________

Proc WriteRawDataDebugSUDTItem:
    Argument @Text1, @Text2, @UseLen
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'UDT.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            If D@UseLen = &TRUE ; this happens only in S_CONSTANT_CV2.
                push eax
                    call StrLenProc esi
                    mov ecx eax
                    inc ecx ; The size is increased to we alow including the zero byte
                pop eax
            Else
                movzx ecx B$esi-1
            End_If

            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP
__________________________________________________________________



;WriteCVCompileFlag1Equates
;WriteCVGlobalProcFlagEquates

;;
Flag Member:

The Flag member is a Byte value responsable for several different computations accordying to specified bits.
We need to shr the founded values to find the proper equates used. This member is the Flags for the specified
procedure.

Fpo :1  Uses Bit 0. When this bit is flagged the function has frame pointer omitted. Otherwise it don't use.
        Equates:    CV4_FUNCTION_FPO_ENABLED 01
                    CV4_FUNCTION_FPO_DISABLED 0

Interrupt :1  Uses Bit 1. When this bit is flagged the function is interrupt routine. Otherwise it is not.
              Equates:    CV4_FUNCTION_INT_ENABLED 01
                          CV4_FUNCTION_INT_DISABLED 0

Return :1  Uses Bit 2. When this bit is flagged the function performs Far return. Otherwise it doesn't.
            Equates:    CV4_FUNCTION_RET_ENABLED 01
                        CV4_FUNCTION_RET_DISABLED 0

Never :1  Uses Bit 3. When this bit is flagged the function never returns. Otherwise it does.
            Equates:    CV4_FUNCTION_NORET_ENABLED 01
                        CV4_FUNCTION_NORET_DISABLED 0

Bits 4 to 7 are not used. They must be settled to 0.

;;

Proc WriteCVGlobalProcFlagEquates:
    Uses esi, ebx, eax

    xor eax eax | lodsb
   ; We must clear Bits 4 to 7 because they are not used
    btr eax 4 | btr eax 5 | btr eax 6 | btr eax 7
    mov ebx eax

    push ebx
    push eax

    shr ebx 3 ; This is to calculate Never member. Shr by 3 because the Bit 3 is the one that is starting to be flagged.

    .If ebx = &CV4_FUNCTION_NORET_ENABLED
        zCopy {'(&CV4_FUNCTION_NORET_ENABLED shl 3)', 0}
    .Else;_If ebx = CV4_FUNCTION_NORET_DISABLED
        zCopy {'(&CV4_FUNCTION_NORET_DISABLED shl 3)', 0}
    .End_If

        zCopy {' + ', 0}
        ; Now we must Zero bit 3 to shr again to find the next records for Return Member (Bit 2)
        btr eax 3
        ; After clearing the bits, we need to compute only the needed bits to be shred
        shr eax 2 ; This is to calculate Return member. Shr by 2 because the Bit 2 is the one that is starting to be flagged.

    .If eax = &CV4_FUNCTION_RET_ENABLED
        zCopy {'(&CV4_FUNCTION_RET_ENABLED shl 2)', 0}
    .Else;_If eax = CV4_FUNCTION_RET_DISABLED
        zCopy {'(&CV4_FUNCTION_RET_DISABLED shl 2)', 0}
    .End_If

    pop eax
    pop ebx

        zCopy {' + ', 0}

    ; Now we restored the values of ebx and eax. We will going to find the values for
    ; Interrupt (Bit 1) and Fpo (Bit 0)
    ; Since we restored the values we must clear bits 2 to 3 (Rememeber that bits 4 to 7 are already cleared)

    ; zeroes bits 2 to 3
    btr eax 2 | btr eax 3

    shr eax 1 ; This is to calculate Interrupt. Shr by 1 because the Bit 1 is the one that is starting to be flagged.

    .If eax = &CV4_FUNCTION_INT_ENABLED
        zCopy {'(&CV4_FUNCTION_INT_ENABLED shl 1)', 0}
    .Else;_If eax = CV4_FUNCTION_INT_DISABLED
        zCopy {'(&CV4_FUNCTION_INT_DISABLED shl 1)', 0}
    .End_If

        zCopy {' + ', 0}

    and ebx 01 ; This is to calculate Fpo.
               ; The value of 1 is because it is the maximum value of all bits flagged. (Bit 0)

    .If ebx = &CV4_FUNCTION_FPO_ENABLED
        zCopy {'&CV4_FUNCTION_FPO_ENABLED', 0}
    .Else
        zCopy {'&CV4_FUNCTION_FPO_DISABLED', 0}
    .End_If

        zCopy {' ; Hex Value:  ', 0}

EndP

______________________________________

Proc WriteRawDataDebugSGlobalProcItem:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy D$CVGlobalProcLabel;{'GlobalProcedure.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Ppar' ; from "Pparent" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Pend'; From "Pend" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Pnex'; From "Pnext" string
            lodsd | call WriteEax

        ..Else_If D$eax+2 = 'ocLe'; From "ProcLength" string
            lodsd | call WriteEax

        ..Else_If D$eax+4 = 'gSta'; From "DebugStart" string
            lodsd | call WriteEax

        ..Else_If D$eax+4 = 'gEnd'; From "DebugEnd" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Segm'; From "Segment" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+2 = 'octy'; From "Proctype" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax
            .If W$esi-2 >= &CV_FIRST_NONPRIM
                push esi | ZCopy {"; This is a Non-Primitive Type. The value is: ", 0} | pop esi
                movzx eax W$esi-2
                sub eax &CV_FIRST_NONPRIM
                call WriteEax
            .End_If

        ..Else_If D$eax = 'Flag'; From "Flags" string
            call WriteCVGlobalProcFlagEquates
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                ;mov B$edi "'" | inc edi
                mov B$edi '"' | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             ;mov B$edi "'" | inc edi
                mov B$edi '"' | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

________________________

[CVGlobalProcPparent: 'Pparent' , 0
 CVGlobalProcPend: 'Pend' , 0
 CVGlobalProcPnext: 'Pnext' , 0
 CVGlobalProcLength: 'ProcLength' , 0
 CVGlobalProcDebugStart: 'DebugStart' , 0
 CVGlobalProcDebugEnd: 'DebugEnd' , 0
 CVGlobalProcOldCVType: 'OldCVType' , 0 ; for obsolete versinos of CodeView (V 3 and earlier)
 CVGlobalProcOffset: 'Offset' , 0
 CVGlobalProcSegment: 'Segment' , 0
 CVGlobalProcProctype: 'Proctype' , 0
 CVGlobalProcFlags: 'Flags' , 0
 CVGlobalProcNamelenght: 'NameLenght' , 0
 CVGlobalProcName: 'Name' , 0]

[CVGlobalProcLabel: B$ 0 #20]

Proc WriteRawDataDebugSGlobalProc:
    Local @OldCodeView
    Uses ecx

    mov D@OldCodeView &FALSE

    If W$esi-2 = &S_LPROC32
        move D$CVGlobalProcLabel {'LocalProcedure.', 0}
    Else
        move D$CVGlobalProcLabel {'GlobalProcedure.', 0}
    End_If

    If W$esi-2 = &S_GPROC32_CV3
        mov D@OldCodeView &TRUE
    End_If

    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcPparent , {': D$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcPend , {': D$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcPnext , {': D$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcLength , {': D$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcDebugStart , {': D$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcDebugEnd , {': D$ ', 0}

    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSGlobalProcItem CVGlobalProcOldCVType , {': W$ ', 0}
    End_If

    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcOffset , {': D$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcSegment , {': W$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcProctype , {': W$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcFlags , {': B$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcNamelenght , {': B$ ', 0}
    Call WriteRawDataDebugSGlobalProcItem CVGlobalProcName , {': B$ ', 0}

EndP

__________________________________________________________________

Proc WriteRawDataDebugSObjectName:
;    Uses ecx
    Uses ecx, eax, ebx
    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx



    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'ObjName.Signature: D$ ', 0}
    pop esi

        lodsd | call WriteEax
        mov W$edi CRLF | add edi 2

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'ObjName.NameLenght: B$ ', 0}
    pop esi

        xor eax eax
        lodsb | call WriteEax

        mov W$edi CRLF | add edi 2

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {".Index", 0}
        zCopy DebugNumber
        zCopy D$CVLabel
        zCopy {'ObjName.Name: B$ ', 0}
    pop esi

        ; ecx points to the size of the Name
        movzx ecx B$esi-1
        mov edx esi | add edx ecx

        .If B$esi = 0
            mov B$edi '0' | inc edi | inc esi
        .Else

        mov B$edi "'" | inc edi
L0:     lodsb
            If al = 0
                dec esi | jmp L1>
            End_If
        stosb | On esi < edx, jmp L0<
L1:     mov B$edi "'" | inc edi

        .End_If

        While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        mov W$edi CRLF | add edi 2

    pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If


EndP

__________________________________________________________________

[CVThunkStart1632Pparent: 'Pparent' , 0
 CVThunkStart1632Pend: 'Pend' , 0
 CVThunkStart1632Pnext: 'Pnext' , 0
 CVThunkStart1632Offset: 'Offset' , 0
 CVThunkStart1632Segment: 'Segment' , 0
 CVThunkStart1632ThunkLength: 'ThunkLength' , 0
 CVThunkStart1632Ordinal: 'Ordinal' , 0
 CVThunkStart1632Namelenght: 'NameLenght' , 0
 CVThunkStart1632Name: 'Name' , 0
 CVThunkStart1632VariantAdjustorDelta: 'VariantAdjustor.Delta', 0
 CVThunkStart1632VariantAdjustorTargetFunctionNameLen: 'VariantAdjustor.TargetFunctionNameLenght', 0
 CVThunkStart1632VariantAdjustorTargetFunctionName: 'VariantAdjustor.TargetFunctionName', 0
 CVThunkStart1632VariantVCallVTableDisplacement: 'VariantVCall.VTableDisplacement', 0
 CVThunkStart1632VariantPCodeSegment: 'VariantPCode.Segment', 0
 CVThunkStart1632VariantPCodeOffset: 'VariantPCode.Offset', 0]

Proc WriteRawDataDebugSThunkStart1632:
    Local @OrdinalValue
    Uses ecx, eax

    mov D@OrdinalValue 0

    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Pparent , {': D$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Pend , {': D$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Pnext , {': D$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Offset , {': D$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Segment , {': W$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632ThunkLength , {': W$ ', 0}
    movzx eax B$esi
    mov D@OrdinalValue eax
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Ordinal , {': B$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Namelenght , {': B$ ', 0}
    Call WriteRawDataDebugSThunkStart1632Item CVThunkStart1632Name , {': B$ ', 0}

    ; Below are some members that i have no files to check, but accordying to the documentation, it seems correct.
    .If D@OrdinalValue = &CV4_THUNK32_ADJUSTOR
        push edi | push esi | call 'USER32.MessageBoxA' 0, {"CV4 - S_THUNK32 / Variant. Please, send this file to us to we check if the CV4 structure is correct", 0}, {"Attention !!!!", 0}, &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL | pop esi | pop edi
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantAdjustorDelta, {': W$ ', 0}
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantAdjustorTargetFunctionNameLen, {': B$ ', 0}
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantAdjustorTargetFunctionName, {': B$ ', 0}
    .Else_If D@OrdinalValue = &CV4_THUNK32_VCALL
        push edi | push esi | call 'USER32.MessageBoxA' 0, {"CV4 - S_THUNK32 / Variant. Please, send this file to us to we check if the CV4 structure is correct", 0}, {"Attention !!!!", 0}, &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL | pop esi | pop edi
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantVCallVTableDisplacement, {': W$ ', 0}
    .Else_If D@OrdinalValue = &CV4_THUNK32_PCODE
        push edi | push esi | call 'USER32.MessageBoxA' 0, {"CV4 - S_THUNK32 / Variant. Please, send this file to us to we check if the CV4 structure is correct", 0}, {"Attention !!!!", 0}, &MB_OK__&MB_ICONWARNING__&MB_SYSTEMMODAL | pop esi | pop edi
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantPCodeSegment, {': W$ ', 0}
        Call WriteRawDataDebugSThunkStart1632VariantItem CVThunkStart1632VariantPCodeOffset, {': D$ ', 0}
    .End_If

EndP
__________________________________________________________________

Proc WriteRawDataDebugSThunkStart1632Item:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'ThunkStart1632.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Ppar' ; from "Pparent" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Pend'; From "Pend" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Pnex'; From "Pnext" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Segm'; From "Segment" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Thun'; From "ThunkLength" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Ordi'; From "Ordinal" string
            call WriteCVThunkStart1632OrdinalEquates
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

__________________________________________________________________

Proc WriteRawDataDebugSThunkStart1632VariantItem:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'ThunkStart1632.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1


        ..If D$eax+16 = 'Delt' ; from "VariantAdjustor.Delta" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+34 = 'Leng'; From "VariantAdjustor.TargetFunctionNameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax+30 = 'Name'; From "VariantAdjustor.TargetFunctionName" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..Else_If D$eax+13 = 'VTab'; From "VariantVCall.VTableDisplacement" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+13 = 'Segm'; From "VariantPCode.Segment" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+13 = 'Offs'; From "VariantPCode.Offset" string
            lodsd | call WriteEax

         ..End_If

        mov W$edi CRLF | add edi 2

EndP

__________________________________________________________________

Proc WriteCVThunkStart1632OrdinalEquates:
    Uses esi;, ebx, eax

    .If B$esi = &CV4_THUNK32_NOTYPE
        zCopy {'&CV4_THUNK32_NOTYPE', 0}
    .Else_If B$esi = &CV4_THUNK32_ADJUSTOR
        zCopy {'&CV4_THUNK32_ADJUSTOR', 0}
    .Else_If B$esi = &CV4_THUNK32_VCALL
        zCopy {'&CV4_THUNK32_VCALL', 0}
    .Else_If B$esi = &CV4_THUNK32_PCODE
        zCopy {'&CV4_THUNK32_PCODE', 0}

    .End_If

        zCopy {' ; Hex Value:  ', 0}

EndP
__________________________________________________________________


;;
These are the primitives Types Listings. All values not used below are unknown or not specified.

Equates used:

Special Types

T_NOTYPE 0x0000 Uncharacterized type (no type)
T_ABS 0x0001 Absolute symbol
T_SEGMENT 0x0002 Segment type
T_VOID 0x0003 Void
T_PVOID 0x0103 Near pointer to void
T_PFVOID 0x0203 Far pointer to void
T_PHVOID 0x0303 Huge pointer to void
T_32PVOID 0x0403 32-bit near pointer to void
T_32PFVOID 0x0503 32-bit far pointer to void
T_CURRENCY 0x0004 Basic 8-byte currency value
T_NBASICSTR 0x0005 Near Basic string
T_FBASICSTR 0x0006 Far Basic string
T_NOTTRANS 0x0007 Untranslated type record from Microsoft symbol format
T_BIT 0x0060 Bit
T_PASCHAR 0x0061 Pascal CHAR

Character Types

T_CHAR 0x0010 8-bit signed
T_UCHAR 0x0020 8-bit unsigned
T_PCHAR 0x0110 Near pointer to 8-bit signed
T_PUCHAR 0x0120 Near pointer to 8-bit unsigned
T_PFCHAR 0x0210 Far pointer to 8-bit signed
T_PFUCHAR 0x0220 Far pointer to 8-bit unsigned
T_PHCHAR 0x0310 Huge pointer to 8-bit signed
T_PHUCHAR 0x0320 Huge pointer to 8-bit unsigned
T_32PCHAR 0x0410 16:32 near pointer to 8-bit signed
T_32PUCHAR 0x0420 16:32 near pointer to 8-bit unsigned
T_32PFCHAR 0x0510 16:32 far pointer to 8-bit signed
T_32PFUCHAR 0x0520 16:32 far pointer to 8-bit unsigned

Real Character Types

T_RCHAR 0x0070 Real char
T_PRCHAR 0x0170 Near pointer to a real char
T_PFRCHAR 0x0270 Far pointer to a real char
T_PHRCHAR 0x0370 Huge pointer to a real char
T_32PRCHAR 0x0470 16:32 near pointer to a real char
T_32PFRCHAR 0x0570 16:32 far pointer to a real char

Wide Character Types

T_WCHAR 0x0071 Wide char
T_PWCHAR 0x0171 Near pointer to a wide char
T_PFWCHAR 0x0271 Far pointer to a wide char
T_PHWCHAR 0x0371 Huge pointer to a wide char
T_32PWCHAR 0x0471 16:32 near pointer to a wide char
T_32PFWCHAR 0x0571 16:32 far pointer to a wide char

Real 16-bit Integer Types

T_INT2 0x0072 Real 16-bit signed int
T_UINT2 0x0073 Real 16-bit unsigned int
T_PINT2 0x0172 Near pointer to 16-bit signed int
T_PUINT2 0x0173 Near pointer to 16-bit unsigned int
T_PFINT2 0x0272 Far pointer to 16-bit signed int
T_PFUINT2 0x0273 Far pointer to 16-bit unsigned int
T_PHINT2 0x0372 Huge pointer to 16-bit signed int
T_PHUINT2 0x0373 Huge pointer to 16-bit unsigned int
T_32PINT2 0x0472 16:32 near pointer to 16-bit signed int
T_32PUINT2 0x0473 16:32 near pointer to 16-bit unsigned int
T_32PFINT2 0x0572 16:32 far pointer to 16-bit signed int
T_32PFUINT2 0x0573 16:32 far pointer to 16-bit unsigned int

16-bit Short Types

T_SHORT 0x0011 16-bit signed
T_USHORT 0x0021 16-bit unsigned
T_PSHORT 0x0111 Near pointer to 16-bit signed
T_PUSHORT 0x0121 Near pointer to 16-bit unsigned
T_PFSHORT 0x0211 Far pointer to 16-bit signed
T_PFUSHORT 0x0221 Far pointer to 16-bit unsigned
T_PHSHORT 0x0311 Huge pointer to 16-bit signed
T_PHUSHORT 0x0321 Huge pointer to 16-bit unsigned
T_32PSHORT 0x0411 16:32 near pointer to 16-bit signed
T_32PUSHORT 0x0421 16:32 near pointer to 16-bit unsigned
T_32PFSHORT 0x0511 16:32 far pointer to 16-bit signed
T_32PFUSHORT 0x0521 16:32 far pointer to 16-bit unsigned

Real 32-bit Integer Types

T_INT4 0x0074 Real 32-bit signed int
T_UINT4 0x0075 Real 32-bit unsigned int
T_PINT4 0x0174 Near pointer to 32-bit signed int
T_PUINT4 0x0175 Near pointer to 32-bit unsigned int
T_PFINT4 0x0274 Far pointer to 32-bit signed int
T_PFUINT4 0x0275 Far pointer to 32-bit unsigned int
T_PHINT4 0x0374 Huge pointer to 32-bit signed int
T_PHUINT4 0x0375 Huge pointer to 32-bit unsigned int
T_32PINT4 0x0474 16:32 near pointer to 32-bit signed int
T_32PUINT4 0x0475 16:32 near pointer to 32-bit unsigned int
T_32PFINT4 0x0574 16:32 far pointer to 32-bit signed int
T_32PFUINT4 0x0575 16:32 far pointer to 32-bit unsigned int

32-bit Long Types

T_LONG 0x0012 32-bit signed
T_ULONG 0x0022 32-bit unsigned
T_PLONG 0x0112 Near pointer to 32-bit signed
T_PULONG 0x0122 Near pointer to 32-bit unsigned
T_PFLONG 0x0212 Far pointer to 32-bit signed
T_PFULONG 0x0222 Far pointer to 32-bit unsigned
T_PHLONG 0x0312 Huge pointer to 32-bit signed
T_PHULONG 0x0322 Huge pointer to 32-bit unsigned
T_32PLONG 0x0412 16:32 near pointer to 32-bit signed
T_32PULONG 0x0422 16:32 near pointer to 32-bit unsigned
T_32PFLONG 0x0512 16:32 far pointer to 32-bit signed
T_32PFULONG 0x0522 16:32 far pointer to 32-bit unsigned

Real 64-bit int Types

T_INT8 0x0076 64-bit signed int
T_UINT8 0x0077 64-bit unsigned int
T_PINT8 0x0176 Near pointer to 64-bit signed int
T_PUINT8 0x0177 Near pointer to 64-bit unsigned int
T_PFINT8 0x0276 Far pointer to 64-bit signed int
T_PFUINT8 0x0277 Far pointer to 64-bit unsigned int
T_PHINT8 0x0376 Huge pointer to 64-bit signed int
T_PHUINT8 0x0377 Huge pointer to 64-bit unsigned int
T_32PINT8 0x0476 16:32 near pointer to 64-bit signed int
T_32PUINT8 0x0477 16:32 near pointer to 64-bit unsigned int
T_32PFINT8 0x0576 16:32 far pointer to 64-bit signed int
T_32PFUINT8 0x0577 16:32 far pointer to 64-bit unsigned int

64-bit Integral Types

T_QUAD 0x0013 64-bit signed
T_UQUAD 0x0023 64-bit unsigned
T_PQUAD 0x0113 Near pointer to 64-bit signed
T_PUQUAD 0x0123 Near pointer to 64-bit unsigned
T_PFQUAD 0x0213 Far pointer to 64-bit signed
T_PFUQUAD 0x0223 Far pointer to 64-bit unsigned
T_PHQUAD 0x0313 Huge pointer to 64-bit signed
T_PHUQUAD 0x0323 Huge pointer to 64-bit unsigned
T_32PQUAD 0x0413 16:32 near pointer to 64-bit signed
T_32PUQUAD 0x0423 16:32 near pointer to 64-bit unsigned
T_32PFQUAD 0x0513 16:32 far pointer to 64-bit signed
T_32PFUQUAD 0x0523 16:32 far pointer to 64-bit unsigned

32-bit Real Types

T_REAL32 0x0040 32-bit real
T_PREAL32 0x0140 Near pointer to 32-bit real
T_PFREAL32 0x0240 Far pointer to 32-bit real
T_PHREAL32 0x0340 Huge pointer to 32-bit real
T_32PREAL32 0x0440 16:32 near pointer to 32-bit real
T_32PFREAL32 0x0540 16:32 far pointer to 32-bit real

48-bit Real Types

T_REAL48 0x0044 48-bit real
T_PREAL48 0x0144 Near pointer to 48-bit real
T_PFREAL48 0x0244 Far pointer to 48-bit real
T_PHREAL48 0x0344 Huge pointer to 48-bit real
T_32PREAL48 0x0444 16:32 near pointer to 48-bit real
T_32PFREAL48 0x0544 16:32 far pointer to 48-bit real

64-bit Real Types

T_REAL64 0x0041 64-bit real
T_PREAL64 0x0141 Near pointer to 64-bit real
T_PFREAL64 0x0241 Far pointer to 64-bit real
T_PHREAL64 0x0341 Huge pointer to 64-bit real
T_32PREAL64 0x0441 16:32 near pointer to 64-bit real
T_32PFREAL64 0x0541 16:32 far pointer to 64-bit real

80-bit Real Types

T_REAL80 0x0042 80-bit real
T_PREAL80 0x0142 Near pointer to 80-bit real
T_PFREAL80 0x0242 Far pointer to 80-bit real
T_PHREAL80 0x0342 Huge pointer to 80-bit real
T_32PREAL80 0x0442 16:32 near pointer to 80-bit real
T_32PFREAL80 0x0542 16:32 far pointer to 80-bit real

128-bit Real Types
T_REAL128 0x0043 128-bit real
T_PREAL128 0x0143 Near pointer to 128-bit real
T_PFREAL128 0x0243 Far pointer to 128-bit real
T_PHREAL128 0x0343 Huge pointer to 128-bit real
T_32PREAL128 0x0443 16:32 near pointer to 128-bit real
T_32PFREAL128 0x0543 16:32 far pointer to 128-bit real

32-bit Complex Types

T_CPLX32 0x0050 32-bit complex
T_PCPLX32 0x0150 Near pointer to 32-bit complex
T_PFCPLX32 0x0250 Far pointer to 32-bit complex
T_PHCPLX32 0x0350 Huge pointer to 32-bit complex
T_32PCPLX32 0x0450 16:32 near pointer to 32-bit complex
T_32PFCPLX32 0x0550 16:32 far pointer to 32-bit complex

64-bit Complex Types

T_CPLX64 0x0051 64-bit complex
T_PCPLX64 0x0151 Near pointer to 64-bit complex
T_PFCPLX64 0x0251 Far pointer to 64-bit complex
T_PHCPLX64 0x0351 Huge pointer to 64-bit complex
T_32PCPLX64 0x0451 16:32 near pointer to 64-bit complex
T_32PFCPLX64 0x0551 16:32 far pointer to 64-bit complex

80-bit Complex Types

T_CPLX80 0x0052 80-bit complex
T_PCPLX80 0x0152 Near pointer to 80-bit complex
T_PFCPLX80 0x0252 Far pointer to 80-bit complex
T_PHCPLX80 0x0352 Huge pointer to 80-bit complex
T_32PCPLX80 0x0452 16:32 near pointer to 80-bit complex
T_32PFCPLX80 0x0552 16:32 far pointer to 80-bit complex

128-bit Complex Types

T_CPLX128 0x0053 128-bit complex
T_PCPLX128 0x0153 Near pointer to 128-bit complex
T_PFCPLX128 0x0253 Far pointer to 128-bit complex
T_PHCPLX128 0x0353 Huge pointer to 128-bit real
T_32PCPLX128 0x0453 16:32 near pointer to 128-bit complex
T_32PFCPLX128 0x0553 16:32 far pointer to 128-bit complex

Boolean Types

T_BOOL08 0x0030 8-bit Boolean
T_BOOL16 0x0031 16-bit Boolean
T_BOOL32 0x0032 32-bit Boolean
T_BOOL64 0x0033 64-bit Boolean
T_PBOOL08 0x0130 Near pointer to 8-bit Boolean
T_PBOOL16 0x0131 Near pointer to 16-bit Boolean
T_PBOOL32 0x0132 Near pointer to 32-bit Boolean
T_PBOOL64 0x0133 Near pointer to 64-bit Boolean
T_PFBOOL08 0x0230 Far pointer to 8-bit Boolean
T_PFBOOL16 0x0231 Far pointer to 16-bit Boolean
T_PFBOOL32 0x0232 Far pointer to 32-bit Boolean
T_PFBOOL32 0x0233 Far pointer to 64-bit Boolean
T_PHBOOL08 0x0330 Huge pointer to 8-bit Boolean
T_PHBOOL16 0x0331 Huge pointer to 16-bit Boolean
T_PHBOOL32 0x0332 Huge pointer to 32-bit Boolean
T_PHBOOL64 0x0333 Huge pointer to 64-bit Boolean
T_32PBOOL08 0x0430 16:32 near pointer to 8-bit Boolean
T_32PBOOL16 0x0431 16:32 near pointer to 16-bit Boolean
T_32PBOOL32 0x0432 16:32 near pointer to 32-bit Boolean
T_32PBOOL64 0x0433 16:32 near pointer to 64-bit Boolean
T_32PFBOOL08 0x0530 16:32 far pointer to 8-bit Boolean
T_32PFBOOL16 0x0531 16:32 far pointer to 16-bit Boolean
T_32PFBOOL32 0x0532 16:32 far pointer to 32-bit Boolean
T_32PFBOOL64 0x0533 16:32 far pointer to 64-bit Boolean
;;

Proc WriteCVBPRel32TypeEquates:
    Uses esi

    ..If W$esi = &T_NOTYPE
        zCopy {'&T_NOTYPE ; Uncharacterized type (no type) - Hex Value:  ', 0}
    ..Else_If W$esi = &T_ABS
        zCopy {'&T_ABS ; Absolute symbol - Hex Value:  ', 0}
    ..Else_If W$esi = &T_SEGMENT
        zCopy {'&T_SEGMENT ; Segment type - Hex Value:  ', 0}
    ..Else_If W$esi = &T_VOID
        zCopy {'&T_VOID ; Void - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PVOID
        zCopy {'&T_PVOID ; Near pointer to void - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFVOID
        zCopy {'&T_PFVOID ; Far pointer to void - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHVOID
        zCopy {'&T_PHVOID ; Huge pointer to void - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PVOID
        zCopy {'&T_32PVOID ; 32-bit near pointer to void - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFVOID
        zCopy {'&T_32PFVOID ; 32-bit far pointer to void - Hex Value:  ', 0}
    ..Else_If W$esi = &T_CURRENCY
        zCopy {'&T_CURRENCY ; Basic 8-byte currency value - Hex Value:  ', 0}
    ..Else_If W$esi = &T_NBASICSTR
        zCopy {'&T_NBASICSTR ; Near Basic string - Hex Value:  ', 0}
    ..Else_If W$esi = &T_FBASICSTR
        zCopy {'&T_FBASICSTR ; Far Basic string - Hex Value:  ', 0}
    ..Else_If W$esi = &T_NOTTRANS
        zCopy {'&T_NOTTRANS ; Untranslated type record from Microsoft symbol format - Hex Value:  ', 0}
    ..Else_If W$esi = &T_BIT
        zCopy {'&T_BIT ; Bit - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PASCHAR
        zCopy {'&T_PASCHAR ; Pascal CHAR - Hex Value:  ', 0}
    ..Else_If W$esi = &T_CHAR
        zCopy {'&T_CHAR ; 8-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_UCHAR
        zCopy {'&T_UCHAR ; 8-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PCHAR
        zCopy {'&T_PCHAR ; Near pointer to 8-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PUCHAR
        zCopy {'&T_PUCHAR ; Near pointer to 8-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFCHAR
        zCopy {'&T_PFCHAR ; Far pointer to 8-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFUCHAR
        zCopy {'&T_PFUCHAR ; Far pointer to 8-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHCHAR
        zCopy {'&T_PHCHAR ; Huge pointer to 8-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHUCHAR
        zCopy {'&T_PHUCHAR ; Huge pointer to 8-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PCHAR
        zCopy {'&T_32PCHAR ; 16:32 near pointer to 8-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PUCHAR
        zCopy {'&T_32PUCHAR ; 16:32 near pointer to 8-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFCHAR
        zCopy {'&T_32PFCHAR ; 16:32 far pointer to 8-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFUCHAR
        zCopy {'&T_32PFUCHAR ; 16:32 far pointer to 8-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_RCHAR
        zCopy {'&T_RCHAR ; Real char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PRCHAR
        zCopy {'&T_PRCHAR ; Near pointer to a real char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFRCHAR
        zCopy {'&T_PFRCHAR ; Far pointer to a real char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHRCHAR
        zCopy {'&T_PHRCHAR ; Huge pointer to a real char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PRCHAR
        zCopy {'&T_32PRCHAR ; 16:32 near pointer to a real char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFRCHAR
        zCopy {'&T_32PFRCHAR ; 16:32 far pointer to a real char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_WCHAR
        zCopy {'&T_WCHAR ; Wide char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PWCHAR
        zCopy {'&T_PWCHAR ; Near pointer to a wide char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFWCHAR
        zCopy {'&T_PFWCHAR ; Far pointer to a wide char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHWCHAR
        zCopy {'&T_PHWCHAR ; Huge pointer to a wide char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PWCHAR
        zCopy {'&T_32PWCHAR ; 16:32 near pointer to a wide char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFWCHAR
        zCopy {'&T_32PFWCHAR ; 16:32 far pointer to a wide char - Hex Value:  ', 0}
    ..Else_If W$esi = &T_INT2
        zCopy {'&T_INT2 ; Real 16-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_UINT2
        zCopy {'&T_UINT2 ; Real 16-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PINT2
        zCopy {'&T_PINT2 ; Near pointer to 16-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PUINT2
        zCopy {'&T_PUINT2 ; Near pointer to 16-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFINT2
        zCopy {'&T_PFINT2 ; Far pointer to 16-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFUINT2
        zCopy {'&T_PFUINT2 ; Far pointer to 16-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHINT2
        zCopy {'&T_PHINT2 ; Huge pointer to 16-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHUINT2
        zCopy {'&T_PHUINT2 ; Huge pointer to 16-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PINT2
        zCopy {'&T_32PINT2 ; 16:32 near pointer to 16-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PUINT2
        zCopy {'&T_32PUINT2 ; 16:32 near pointer to 16-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFINT2
        zCopy {'&T_32PFINT2 ; 16:32 far pointer to 16-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFUINT2
        zCopy {'&T_32PFUINT2 ; 16:32 far pointer to 16-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_SHORT
        zCopy {'&T_SHORT ; 16-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_USHORT
        zCopy {'&T_USHORT ; 16-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PSHORT
        zCopy {'&T_PSHORT ; Near pointer to 16-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PUSHORT
        zCopy {'&T_PUSHORT ; Near pointer to 16-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFSHORT
        zCopy {'&T_PFSHORT ; Far pointer to 16-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFUSHORT
        zCopy {'&T_PFUSHORT ; Far pointer to 16-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHSHORT
        zCopy {'&T_PHSHORT ; Huge pointer to 16-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHUSHORT
        zCopy {'&T_PHUSHORT ; Huge pointer to 16-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PSHORT
        zCopy {'&T_32PSHORT ; 16:32 near pointer to 16-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PUSHORT
        zCopy {'&T_32PUSHORT ; 16:32 near pointer to 16-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFSHORT
        zCopy {'&T_32PFSHORT ; 16:32 far pointer to 16-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFUSHORT
        zCopy {'&T_32PFUSHORT ; 16:32 far pointer to 16-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_INT4
        zCopy {'&T_INT4 ; Real 32-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_UINT4
        zCopy {'&T_UINT4 ; Real 32-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PINT4
        zCopy {'&T_PINT4 ; Near pointer to 32-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PUINT4
        zCopy {'&T_PUINT4 ; Near pointer to 32-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFINT4
        zCopy {'&T_PFINT4 ; Far pointer to 32-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFUINT4
        zCopy {'&T_PFUINT4 ; Far pointer to 32-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHINT4
        zCopy {'&T_PHINT4 ; Huge pointer to 32-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHUINT4
        zCopy {'&T_PHUINT4 ; Huge pointer to 32-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PINT4
        zCopy {'&T_32PINT4 ; 16:32 near pointer to 32-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PUINT4
        zCopy {'&T_32PUINT4 ; 16:32 near pointer to 32-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFINT4
        zCopy {'&T_32PFINT4 ; 16:32 far pointer to 32-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFUINT4
        zCopy {'&T_32PFUINT4 ; 16:32 far pointer to 32-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_LONG
        zCopy {'&T_LONG ; 32-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_ULONG
        zCopy {'&T_ULONG ; 32-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PLONG
        zCopy {'&T_PLONG ; Near pointer to 32-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PULONG
        zCopy {'&T_PULONG ; Near pointer to 32-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFLONG
        zCopy {'&T_PFLONG ; Far pointer to 32-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFULONG
        zCopy {'&T_PFULONG ; Far pointer to 32-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHLONG
        zCopy {'&T_PHLONG ; Huge pointer to 32-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHULONG
        zCopy {'&T_PHULONG ; Huge pointer to 32-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PLONG
        zCopy {'&T_32PLONG ; 16:32 near pointer to 32-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PULONG
        zCopy {'&T_32PULONG ; 16:32 near pointer to 32-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFLONG
        zCopy {'&T_32PFLONG ; 16:32 far pointer to 32-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFULONG
        zCopy {'&T_32PFULONG ; 16:32 far pointer to 32-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_INT8
        zCopy {'&T_INT8 ; 64-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_UINT8
        zCopy {'&T_UINT8 ; 64-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PINT8
        zCopy {'&T_PINT8 ; Near pointer to 64-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PUINT8
        zCopy {'&T_PUINT8 ; Near pointer to 64-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFINT8
        zCopy {'&T_PFINT8 ; Far pointer to 64-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFUINT8
        zCopy {'&T_PFUINT8 ; Far pointer to 64-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHINT8
        zCopy {'&T_PHINT8 ; Huge pointer to 64-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHUINT8
        zCopy {'&T_PHUINT8 ; Huge pointer to 64-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PINT8
        zCopy {'&T_32PINT8 ; 16:32 near pointer to 64-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PUINT8
        zCopy {'&T_32PUINT8 ; 16:32 near pointer to 64-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFINT8
        zCopy {'&T_32PFINT8 ; 16:32 far pointer to 64-bit signed int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFUINT8
        zCopy {'&T_32PFUINT8 ; 16:32 far pointer to 64-bit unsigned int - Hex Value:  ', 0}
    ..Else_If W$esi = &T_QUAD
        zCopy {'&T_QUAD ; 64-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_UQUAD
        zCopy {'&T_UQUAD ; 64-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PQUAD
        zCopy {'&T_PQUAD ; Near pointer to 64-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PUQUAD
        zCopy {'&T_PUQUAD ; Near pointer to 64-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFQUAD
        zCopy {'&T_PFQUAD ; Far pointer to 64-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFUQUAD
        zCopy {'&T_PFUQUAD ; Far pointer to 64-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHQUAD
        zCopy {'&T_PHQUAD ; Huge pointer to 64-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHUQUAD
        zCopy {'&T_PHUQUAD ; Huge pointer to 64-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PQUAD
        zCopy {'&T_32PQUAD ; 16:32 near pointer to 64-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PUQUAD
        zCopy {'&T_32PUQUAD ; 16:32 near pointer to 64-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFQUAD
        zCopy {'&T_32PFQUAD ; 16:32 far pointer to 64-bit signed - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFUQUAD
        zCopy {'&T_32PFUQUAD ; 16:32 far pointer to 64-bit unsigned - Hex Value:  ', 0}
    ..Else_If W$esi = &T_REAL32
        zCopy {'&T_REAL32 ; 32-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PREAL32
        zCopy {'&T_PREAL32 ; Near pointer to 32-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFREAL32
        zCopy {'&T_PFREAL32 ; Far pointer to 32-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHREAL32
        zCopy {'&T_PHREAL32 ; Huge pointer to 32-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PREAL32
        zCopy {'&T_32PREAL32 ; 16:32 near pointer to 32-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFREAL32
        zCopy {'&T_32PFREAL32 ; 16:32 far pointer to 32-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_REAL48
        zCopy {'&T_REAL48 ; 48-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PREAL48
        zCopy {'&T_PREAL48 ; Near pointer to 48-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFREAL48
        zCopy {'&T_PFREAL48 ; Far pointer to 48-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHREAL48
        zCopy {'&T_PHREAL48 ; Huge pointer to 48-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PREAL48
        zCopy {'&T_32PREAL48 ; 16:32 near pointer to 48-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFREAL48
        zCopy {'&T_32PFREAL48 ; 16:32 far pointer to 48-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_REAL64
        zCopy {'&T_REAL64 ; 64-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PREAL64
        zCopy {'&T_PREAL64 ; Near pointer to 64-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFREAL64
        zCopy {'&T_PFREAL64 ; Far pointer to 64-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHREAL64
        zCopy {'&T_PHREAL64 ; Huge pointer to 64-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PREAL64
        zCopy {'&T_32PREAL64 ; 16:32 near pointer to 64-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFREAL64
        zCopy {'&T_32PFREAL64 ; 16:32 far pointer to 64-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_REAL80
        zCopy {'&T_REAL80 ; 80-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PREAL80
        zCopy {'&T_PREAL80 ; Near pointer to 80-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFREAL80
        zCopy {'&T_PFREAL80 ; Far pointer to 80-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHREAL80
        zCopy {'&T_PHREAL80 ; Huge pointer to 80-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PREAL80
        zCopy {'&T_32PREAL80 ; 16:32 near pointer to 80-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFREAL80
        zCopy {'&T_32PFREAL80 ; 16:32 far pointer to 80-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_REAL128
        zCopy {'&T_REAL128 ; 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PREAL128
        zCopy {'&T_PREAL128 ; Near pointer to 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFREAL128
        zCopy {'&T_PFREAL128 ; Far pointer to 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHREAL128
        zCopy {'&T_PHREAL128 ; Huge pointer to 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PREAL128
        zCopy {'&T_32PREAL128 ; 16:32 near pointer to 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFREAL128
        zCopy {'&T_32PFREAL128 ; 16:32 far pointer to 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_CPLX32
        zCopy {'&T_CPLX32 ; 32-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PCPLX32
        zCopy {'&T_PCPLX32 ; Near pointer to 32-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFCPLX32
        zCopy {'&T_PFCPLX32 ; Far pointer to 32-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHCPLX32
        zCopy {'&T_PHCPLX32 ; Huge pointer to 32-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PCPLX32
        zCopy {'&T_32PCPLX32 ; 16:32 near pointer to 32-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFCPLX32
        zCopy {'&T_32PFCPLX32 ; 16:32 far pointer to 32-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_CPLX64
        zCopy {'&T_CPLX64 ; 64-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PCPLX64
        zCopy {'&T_PCPLX64 ; Near pointer to 64-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFCPLX64
        zCopy {'&T_PFCPLX64 ; Far pointer to 64-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHCPLX64
        zCopy {'&T_PHCPLX64 ; Huge pointer to 64-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PCPLX64
        zCopy {'&T_32PCPLX64 ; 16:32 near pointer to 64-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFCPLX64
        zCopy {'&T_32PFCPLX64 ; 16:32 far pointer to 64-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_CPLX80
        zCopy {'&T_CPLX80 ; 80-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PCPLX80
        zCopy {'&T_PCPLX80 ; Near pointer to 80-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFCPLX80
        zCopy {'&T_PFCPLX80 ; Far pointer to 80-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHCPLX80
        zCopy {'&T_PHCPLX80 ; Huge pointer to 80-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PCPLX80
        zCopy {'&T_32PCPLX80 ; 16:32 near pointer to 80-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFCPLX80
        zCopy {'&T_32PFCPLX80 ; 16:32 far pointer to 80-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_CPLX128
        zCopy {'&T_CPLX128 ; 128-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PCPLX128
        zCopy {'&T_PCPLX128 ; Near pointer to 128-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFCPLX128
        zCopy {'&T_PFCPLX128 ; Far pointer to 128-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHCPLX128
        zCopy {'&T_PHCPLX128 ; Huge pointer to 128-bit real - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PCPLX128
        zCopy {'&T_32PCPLX128 ; 16:32 near pointer to 128-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFCPLX128
        zCopy {'&T_32PFCPLX128 ; 16:32 far pointer to 128-bit complex - Hex Value:  ', 0}
    ..Else_If W$esi = &T_BOOL08
        zCopy {'&T_BOOL08 ; 8-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_BOOL16
        zCopy {'&T_BOOL16 ; 16-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_BOOL32
        zCopy {'&T_BOOL32 ; 32-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_BOOL64
        zCopy {'&T_BOOL64 ; 64-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PBOOL08
        zCopy {'&T_PBOOL08 ; Near pointer to 8-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PBOOL16
        zCopy {'&T_PBOOL16 ; Near pointer to 16-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PBOOL32
        zCopy {'&T_PBOOL32 ; Near pointer to 32-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PBOOL64
        zCopy {'&T_PBOOL64 ; Near pointer to 64-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFBOOL08
        zCopy {'&T_PFBOOL08 ; Far pointer to 8-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFBOOL16
        zCopy {'&T_PFBOOL16 ; Far pointer to 16-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFBOOL32
        zCopy {'&T_PFBOOL32 ; Far pointer to 32-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PFBOOL32
        zCopy {'&T_PFBOOL32 ; Far pointer to 64-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHBOOL08
        zCopy {'&T_PHBOOL08 ; Huge pointer to 8-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHBOOL16
        zCopy {'&T_PHBOOL16 ; Huge pointer to 16-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHBOOL32
        zCopy {'&T_PHBOOL32 ; Huge pointer to 32-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_PHBOOL64
        zCopy {'&T_PHBOOL64 ; Huge pointer to 64-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PBOOL08
        zCopy {'&T_32PBOOL08 ; 16:32 near pointer to 8-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PBOOL16
        zCopy {'&T_32PBOOL16 ; 16:32 near pointer to 16-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PBOOL32
        zCopy {'&T_32PBOOL32 ; 16:32 near pointer to 32-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PBOOL64
        zCopy {'&T_32PBOOL64 ; 16:32 near pointer to 64-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFBOOL08
        zCopy {'&T_32PFBOOL08 ; 16:32 far pointer to 8-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFBOOL16
        zCopy {'&T_32PFBOOL16 ; 16:32 far pointer to 16-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFBOOL32
        zCopy {'&T_32PFBOOL32 ; 16:32 far pointer to 32-bit Boolean - Hex Value:  ', 0}
    ..Else_If W$esi = &T_32PFBOOL64
        zCopy {'&T_32PFBOOL64 ; 16:32 far pointer to 64-bit Boolean - Hex Value:  ', 0}
    ..End_if

EndP
_____________________________________
; Non Primitive Types, can be interpreted as the leaf types, for values bigger then 01000
Proc WriteCVNonPrimitiveTypeEquates:
    Uses esi

        ; For values bigger or equal to 01000, it is probably that they are related to leaf type indices, because
        ; they are related to Non-Primitive types

        .If W$esi = &LF_ENUMERATE_CV2
            zCopy {'&LF_ENUMERATE_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ARRAY_CV2
            zCopy {'&LF_ARRAY_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_CLASS_CV2
            zCopy {'&LF_CLASS_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_STRUCTURE_CV2
            zCopy {'&LF_STRUCTURE_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_UNION_CV2
            zCopy {'&LF_UNION_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ENUM_CV2
            zCopy {'&LF_ENUM_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_MEMBER_CV2
            zCopy {'&LF_MEMBER_CV2; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_MODIFIER_CV3
            zCopy {'&LF_MODIFIER_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_POINTER_CV3
            zCopy {'&LF_POINTER_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ARRAY_CV3
            zCopy {'&LF_ARRAY_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_CLASS_CV3
            zCopy {'&LF_CLASS_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_STRUCTURE_CV3
            zCopy {'&LF_STRUCTURE_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_UNION_CV3
            zCopy {'&LF_UNION_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ENUM_CV3
            zCopy {'&LF_ENUM_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_PROCEDURE_CV3
            zCopy {'&LF_PROCEDURE_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_MFUNCTION_CV3
            zCopy {'&LF_MFUNCTION_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_COBOL0_CV3
            zCopy {'&LF_COBOL0_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_BARRAY_CV3
            zCopy {'&LF_BARRAY_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DIMARRAY_CV3
            zCopy {'&LF_DIMARRAY_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_VFTPATH_CV3
            zCopy {'&LF_VFTPATH_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_PRECOMP_CV3
            zCopy {'&LF_PRECOMP_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_OEM_CV3
            zCopy {'&LF_OEM_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_SKIP_CV3
            zCopy {'&LF_SKIP_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ARGLIST_CV3
            zCopy {'&LF_ARGLIST_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DEFARG_CV3
            zCopy {'&LF_DEFARG_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_FIELDLIST_CV3
            zCopy {'&LF_FIELDLIST_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DERIVED_CV3
            zCopy {'&LF_DERIVED_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_BITFIELD_CV3
            zCopy {'&LF_BITFIELD_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_METHODLIST_CV3
            zCopy {'&LF_METHODLIST_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DIMCONU_CV3
            zCopy {'&LF_DIMCONU_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DIMCONLU_CV3
            zCopy {'&LF_DIMCONLU_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DIMVARU_CV3
            zCopy {'&LF_DIMVARU_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_DIMVARLU_CV3
            zCopy {'&LF_DIMVARLU_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_BCLASS_CV3
            zCopy {'&LF_BCLASS_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_VBCLASS_CV3
            zCopy {'&LF_VBCLASS_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_IVBCLASS_CV3
            zCopy {'&LF_IVBCLASS_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_FRIENDFCN_CV3
            zCopy {'&LF_FRIENDFCN_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_INDEX_CV3
            zCopy {'&LF_INDEX_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_MEMBER_CV3
            zCopy {'&LF_MEMBER_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_STMEMBER_CV3
            zCopy {'&LF_STMEMBER_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_METHOD_CV3
            zCopy {'&LF_METHOD_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_NESTTYPE_CV3
            zCopy {'&LF_NESTTYPE_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_VFUNCTAB_CV3
            zCopy {'&LF_VFUNCTAB_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_FRIENDCLS_CV3
            zCopy {'&LF_FRIENDCLS_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ONEMETHOD_CV3
            zCopy {'&LF_ONEMETHOD_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_VFUNCOFF_CV3
            zCopy {'&LF_VFUNCOFF_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_NESTTYPEEX_CV3
            zCopy {'&LF_NESTTYPEEX_CV3; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_CHAR
            zCopy {'&LF_CHAR; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_NUMERIC
            zCopy {'&LF_NUMERIC; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_SHORT
            zCopy {'&LF_SHORT; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_USHORT
            zCopy {'&LF_USHORT; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_LONG
            zCopy {'&LF_LONG; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_ULONG
            zCopy {'&LF_ULONG; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_REAL32
            zCopy {'&LF_REAL32; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_REAL64
            zCopy {'&LF_REAL64; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_REAL80
            zCopy {'&LF_REAL80; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_REAL128
            zCopy {'&LF_REAL128; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_QUADWORD
            zCopy {'&LF_QUADWORD; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_UQUADWORD
            zCopy {'&LF_UQUADWORD; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_REAL48
            zCopy {'&LF_REAL48; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_COMPLEX32
            zCopy {'&LF_COMPLEX32; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_COMPLEX64
            zCopy {'&LF_COMPLEX64; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_COMPLEX80
            zCopy {'&LF_COMPLEX80; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_COMPLEX128
            zCopy {'&LF_COMPLEX128; - Hex Value:  ', 0}
        .Else_If W$esi = &LF_VARSTRING
            zCopy {'&LF_VARSTRING; - Hex Value:  ', 0}
        .Else_If W$esi = &CV_FIRST_NONPRIM
            zCopy {'&CV_FIRST_NONPRIM; - Hex Value:  ', 0}
        .End_If

EndP

[MemberSize2: ?]

Proc WriteCVBPRel32OffsetComment:
    Arguments @dwValue
    Uses esi, eax, ebx, edx, ecx

    mov eax D@dwValue

    If eax = 0
        zCopy {" ; This symbol cannot be evaluated because its location is unknown.", 0}
    End_If

    jns @sign
        push eax
            zCopy {" ; This is a Local variable: ebp-", 0}
            neg eax
            call WriteEax
        pop eax
            zCopy {" , ebp+", 0}
            call WriteEax
            ExitP
@sign:

        ..If eax < 8
            zCopy {" ; Incorrect Symbol Allocation. This can be a Data Pointer, and not a Local Var or a Argument for this function.", 0}
        ..Else
            mov ecx eax
            sub eax 8

            ..If eax = 0
                zCopy {" ; This is a Parameter: ebp+08. Argument = 1. RosAsm Style: Arg1", 0}
            ..Else

                ; divide eax by 4 to calculate how many parameters we have, and if it is a multiple of 4 or not.
                mov edx 0, ebx 4
                div ebx

                If edx = 0

                    inc eax ; Increment eax to display the proper amount of Arguments.

                    push eax
                        zCopy {" ; This is a Parameter: ebp+", 0}
                        mov eax ecx | call WriteEax
                    pop eax

                    zCopy {". Argument = ", 0}

                    push esi
                    push edi
                    push eax
                        ; Calculate and Display Size (Convert Hexa do Decimal String)
                        mov D$membersize2 eax
                        mov esi membersize2, ecx 4
                        call toUDword
                        mov esi edi, edi DecimalBuffer
                        Do | movsb | LoopUntil B$esi-1 = 0
                    pop eax
                    pop edi
                    pop esi

                    zCopy DecimalBuffer
                    zCopy {". RosAsm Style: Arg", 0}
                    zCopy DecimalBuffer
                Else
                    zCopy {" ; Incorrect Symbol Allocation. This can be a Data Pointer, and not a Local Var or a Argument for this function.", 0}
                End_If

            ..End_If

        ..End_If

EndP
_____________________________________

Proc WriteRawDataDebugSBPRel32Item:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy {'BPRel32.', 0}
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1

        ..If D$eax = 'Offs' ; from "Offset" string
            lodsd | call WriteEax
            call WriteCVBPRel32OffsetComment D$esi-4

        ..Else_If D$eax = 'OldC'; From "OldCVType" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP

________________

[CVBPRel32Offset: 'Offset' , 0
 CVBPRel32OldCVType: 'OldCVType', 0
 CVBPRel32Type: 'Type' , 0
 CVBPRel32Namelenght: 'NameLenght' , 0
 CVBPRel32Name: 'Name' , 0]


Proc WriteRawDataDebugSBPRel32:
    Local @OldCodeView
    Uses ecx

    mov D@OldCodeView &FALSE
    If W$esi-2 = &S_BPREL32_CV3
        mov D@OldCodeView &TRUE
    End_If


    Call WriteRawDataDebugSBPRel32Item CVBPRel32Offset , {': D$ ', 0}
    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSBPRel32Item CVBPRel32OldCVType , {': W$ ', 0}
    End_If
    Call WriteRawDataDebugSBPRel32Item CVBPRel32Type , {': W$ ', 0}
    Call WriteRawDataDebugSBPRel32Item CVBPRel32Namelenght , {': B$ ', 0}
    Call WriteRawDataDebugSBPRel32Item CVBPRel32Name , {': B$ ', 0}

EndP

__________________________________________________________________

[CVLocalData32OldCVType: 'OldCVType', 0
 CVLocalData32Offset: 'Offset' , 0
 CVLocalData32Segment: 'Segment' , 0
 CVLocalData32Type: 'Type' , 0
 CVLocalData32Namelenght: 'NameLenght' , 0
 CVLocalData32Name: 'Name' , 0]

[CVLocalData32Label: B$ 0 #20]

Proc WriteRawDataDebugSLData32:
    Local @OldCodeView
;    Uses ecx
    Uses ecx, eax, ebx

    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx

    push ebx

    If W$esi-2 = &S_LDATA32
        move D$CVLocalData32Label {'LocalData32.', 0}
    Else_If_Or W$esi-2 = &S_GDATA32, W$esi-2 = &S_GDATA_CV3
        move D$CVLocalData32Label {'GlobalData32.', 0}
    Else
        move D$CVLocalData32Label {'PublicData32.', 0}
    End_If

    If W$esi-2 = &S_GDATA_CV3
        mov D@OldCodeView &TRUE
    End_If

    If D@OldCodeView = &TRUE
        Call WriteRawDataDebugSLData32Item CVLocalData32OldCVType , {': D$ ', 0}
    End_If

    Call WriteRawDataDebugSLData32Item CVLocalData32Offset , {': D$ ', 0}
    Call WriteRawDataDebugSLData32Item CVLocalData32Segment , {': W$ ', 0}
    Call WriteRawDataDebugSLData32Item CVLocalData32Type , {': W$ ', 0}
    Call WriteRawDataDebugSLData32Item CVLocalData32Namelenght , {': B$ ', 0}
    Call WriteRawDataDebugSLData32Item CVLocalData32Name , {': B$ ', 0}

    pop ebx

    ; Is the end of this structure ends on the proper place ?

    .If esi <> ebx
        mov W$edi CRLF | add edi 2
        call WriteCVdataTitle
        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If


EndP

______________________________________

Proc WriteRawDataDebugSLData32Item:
    Argument @Text1, @Text2
    uses eax, ecx

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {".Index", 0}
            zCopy DebugNumber
            zCopy D$CVLabel
            zCopy D$CVLocalData32Label
            zCopy D@Text1
            zCopy D@Text2
        pop esi

        mov eax D@Text1


        ..If D$eax = 'OldC'; From "OldCVType" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Offs'; From "Offset" string
            lodsd | call WriteEax

        ..Else_If D$eax = 'Segm'; From "Segment" string
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax = 'Type'; From "Type" string
            call WriteCVBPRel32TypeEquates
            xor eax eax
            lodsw | call WriteEax

        ..Else_If D$eax+4 = 'Leng'; From "NameLenght" string
            xor eax eax
            lodsb | call WriteEax

        ..Else_If D$eax = 'Name'; From "Name" string

            ; ecx points to the size of the Name
            movzx ecx B$esi-1
            mov edx esi | add edx ecx

            .If B$esi = 0
                mov B$edi '0' | inc edi | inc esi
            .Else

                mov B$edi "'" | inc edi
L0:             lodsb
                If al = 0
                    dec esi | jmp L1>
                End_If
                stosb | On esi < edx, jmp L0<
L1:             mov B$edi "'" | inc edi

            .End_If

            While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

        ..End_If

        mov W$edi CRLF | add edi 2

EndP


__________________________________________________________________


[CVDataLenght: 'Length', 0
 CVDataIndex: 'Index', 0]

[SizeAdd: D$ 0]
[CVLabel: B$ 0 #6]
[CV5Label: B$ '.CV5.' , 0] ; found in some .NET Object Files
[CV4Label: B$ '.CV4.' , 0]
[CV3Label: B$ '.CV3.' , 0]
[CV2Label: B$ '.CV2.' , 0] ; Unknown CV Signature

Proc WriteRawDataDebugS:
    Local @CVSize, @RawDataEnd
    Uses ecx, esi, eax

    mov ecx esi
    add ecx D$CoffSectionSize
    mov D@RawDataEnd ecx

    ; If Signature is not 1. Do a byte chain. Signature = 1 is Code or CodeView 4 ?. Other values are data or CodeView older versions ?

    .If D$esi = 1
        move D$CVLabel CV4Label
        jmp L0>>
    .Else_If D$esi = 2
        move D$CVLabel CV3Label
        jmp L0>>
    .Else_If D$esi = 4
        sub edi 4
        push esi | ZCopy {D$ CRLF2, B$ '; This Codeview format is unespecified. It is common for Dot NET files. ', D$ CRLF2, B$ '    B$: ', 0} | pop esi
        mov ecx D$CoffSectionSize
        call WriteRawDataDataSection
        ExitP
    .Else    ;   Sometimes they do not contain signatures like in: clusapi2.obj
        move D$CVLabel CV2Label
        mov D$SizeAdd 0
        call InitSectionDebugNumber
        move D@CVSize D$CoffSectionSize
        sub edi 4
        add D@CVSize esi ; CVSize points to the beginning of the Debug Raw Data
        mov W$edi CRLF | add edi 2
        mov D$edi '    ' | add edi 4
        push esi | zCopy {'; This Debug Section have no signature.', 0} | pop esi
        mov W$edi CRLF | add edi 2
        jmp L1>>

    .End_If

L0:

    mov D$SizeAdd 0

    call InitSectionDebugNumber

    move D@CVSize D$CoffSectionSize
    sub edi 4

    mov D$edi CRLF2 | add edi 4

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy D$CVLabel
        zCopy {'Signature: D$ ', 0}
    pop esi


        add D@CVSize esi ; CVSize points to the beginning of the Debug Raw Data

        lodsd | call WriteEax

        If D$esi-4 = 1
            push esi | ZCopy {" ; The compiler that made this Object File, emitted OMF (Object Module Format) types on it's symbols and Types below", 0} | pop esi
        End_If

        mov W$edi CRLF | add edi 2

   L1:

    .While esi <> D@RawDataEnd

        movzx eax W$esi
        mov D$SizeAdd eax

        ...If_Or W$esi = 0, W$esi+2 = 0 ; If lenght = 0 , or index = 0 do next line
            add esi 2 ; only increment esi by 2
        ...Else

            .If_Or W$esi+2 = &S_GPROC32, W$esi+2 = &S_LPROC32, W$esi+2 = &S_THUNK32, W$esi+2 = &S_GPROC32_CV3
                push esi | ZCopy {D$ CRLF2, B$ '; ---------------------- Procedure Start  ---------------------- ', W$ CRLF, 0} | pop esi
            .End_If

            call WriteRawDataDebugSItem CVDataLenght, {': W$ ', 0}
            call WriteRawDataDebugSItem CVDataIndex, {': W$ ', 0}

            ..If_Or W$esi-2 = &S_COMPILE, W$esi-2 = &S_COMPILE_CV3, W$esi-2 = &S_COMPILE_CV2
                call WriteRawDataDebugSCompile

            ..Else_If W$esi-2 = &S_MSTOOL_CV2 ; found in acos.obj This is a variation of S_COMPILE found in older versions of codeview
                call WriteRawDataDebugSMsTool

            ..Else_If_Or W$esi-2 = &S_REGISTER, W$esi-2 = &S_REGISTER_CV3; Register variable
                call WriteRawDataDebugSRegister

            ..Else_If_Or W$esi-2 = &S_CONSTANT, W$esi-2 = &S_CONSTANT_CV3, W$esi-2 = &S_CONSTANT_CV2 ; Constant symbol (frmMainNET.obj)
                call WriteRawDataDebugSContant

            ..Else_If_Or W$esi-2 = &S_UDT, W$esi-2 = &S_UDT_CV3, W$esi-2 = &S_UDT_CV2; User-defined Type (frmMainNET.obj)
                call WriteRawDataDebugSUDT

            ..Else_If W$esi-2 = &S_SSEARCH ; Start search
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_END ; End block, procedure, with, or thunk
                push esi | ZCopy {W$ CRLF, B$ '; ---------------------- Procedure End  ---------------------- ', D$ CRLF2, 0} | pop esi

            ..Else_If W$esi-2 = &S_SKIP ; Skip - Reserve symbol space
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_CVRESERVE ; Reserved for internal use by the Microsoft debugger
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_OBJNAME ; Specify name of object file
                call WriteRawDataDebugSObjectName

            ..Else_If W$esi-2 = &S_ENDARG ; Specify end of arguments in function symbols
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_COBOLUDT ; Microfocus COBOL user-defined type
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_MANYREG ; Many register symbol
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_RETURN ; Function return description
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_ENTRYTHIS ; Description of this pointer at entry
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_BPREL16 ; BP relative 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LDATA16 ; Local data 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GDATA16 ; Global data 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_PUB16 ; Public symbol 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LPROC16 ; Local procedure start 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GPROC16 ; Global procedure start 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_THUNK16 ; Thunk start 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_BLOCK16 ; Block start 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_WITH16 ; With start 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LABEL16 ; Code label 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_CEXMODEL16 ; Change execution model 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_VFTPATH16 ; Virtual function table path descriptor 16:16
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_REGREL16 ; Specify 16:16 offset relative to arbitrary register
                mov eax eax
                jmp L0>>

            ..Else_If_Or W$esi-2 = &S_BPREL32, W$esi-2 = &S_BPREL32_CV3 ; BP relative 16:32
                call WriteRawDataDebugSBPRel32
            ..Else_If W$esi-2 = &S_LDATA32 ; Local data 16:32
                call WriteRawDataDebugSLData32

            ..Else_If_Or W$esi-2 = &S_GDATA32, W$esi-2 = &S_GDATA_CV3 ; Global data 16:32
                call WriteRawDataDebugSLData32

            ..Else_If W$esi-2 = &S_PUB32 ; Public symbol 16:32
                call WriteRawDataDebugSLData32

            ..Else_If W$esi-2 = &S_LPROC32 ; Local (Static) procedure start 16:32
                call WriteRawDataDebugSGlobalProc
            ..Else_If_Or W$esi-2 = &S_GPROC32, W$esi-2 = &S_GPROC32_CV3 ; Global procedure start 16:32
                call WriteRawDataDebugSGlobalProc
            ..Else_If W$esi-2 = &S_THUNK32 ; Thunk start 16:32
                call WriteRawDataDebugSThunkStart1632

            ..Else_If W$esi-2 = &S_BLOCK32 ; Block start 16:32
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_VFTPATH32 ; Virtual function table path descriptor 16:32
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_REGREL32 ; 16:32 offset relative to arbitrary register
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LTHREAD32 ; Local Thread Storage data
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GTHREAD32 ; Global Thread Storage data
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_LPROCMIPS ; Local procedure start MIPS
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_GPROCMIPS ; Global procedure start MIPS
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_PROCREF ; Reference to a procedure
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_DATAREF ; Reference to data
                mov eax eax
                jmp L0>>

            ..Else_If W$esi-2 = &S_ALIGN ; Page align symbols
                mov eax eax
                jmp L0>>

            ..Else_If_Or W$esi-2 >= 01000;, W$esi-2 >= 0
L0:
                push esi
                    mov D$edi '    ' | add edi 4
                    call WriteObjIndice
                    zCopy {"Sec", 0}
                    zCopy SectionHeaderNumber
                    zCopy {".Index", 0}
                    zCopy DebugNumber
                    zCopy D$CVLabel
                    zCopy {'Unknown', 0}
                pop esi
                call WriteRawDataDebugSUnknown

        ..End_If

    ...End_If

        call IncrementSectionDebugNumber
        mov eax D@CVSize
        mov W$edi CRLF | add edi 2

;    .Loop_Until esi >= eax
    .End_While
EndP
__________________________________________________________________

Proc WriteRawDataDebugSUnknown:
    Uses eax, ecx


    ; This is to avoid that the Structure have dummy bytes at the end.
    mov ebx esi
    sub ebx 2
    mov ecx D$SizeAdd
    add ebx ecx
;;
    push ebx

                    push esi
                        mov D$edi '    ' | add edi 4
                        call WriteObjIndice
                        zCopy {"Sec", 0}
                        zCopy SectionHeaderNumber
                        zCopy {".Index", 0}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {'Constant.LeafType.Unknown: B$ ', 0}
                    pop esi
    pop ebx
;;
;;

    push ebx
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayElemType , {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayIdxType , {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayLength, {': W$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayNamelenght, {': B$ ', 0}
        Call WriteRawDataDebugSContantLeafTypeArrayItem CVLeafTypeArrayName, {': B$ ', 0}
        sub edi 2
    pop ebx
;;
    ; Is the end of this structure ends on the proper place ?
    .If esi <> ebx
;;
        mov W$edi CRLF | add edi 2
        ;call WriteCVdataTitle
                    push esi
                        mov D$edi '    ' | add edi 4
                        call WriteObjIndice
                        zCopy {"Sec", 0}
                        zCopy SectionHeaderNumber
                        zCopy {".Index", 0}
                        zCopy DebugNumber
                        zCopy D$CVLabel
                        zCopy {'Constant.LeafType.Unknown: B$ ', 0}
                    pop esi

;;
        push esi | ZCopy {'.Unknown: B$ ', 0} | pop esi

        push ecx

        mov edx 0
        xor eax eax

        Do

            movzx eax B$esi
            push ebx | call WriteEax | pop ebx

            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If

        Loop_Until esi = ebx

            sub edi 2
            dec edi
            mov W$edi+1 CRLF | add edi 2
            pop ecx
            inc edi
    .Else
        mov W$edi CRLF | add edi 2
    .End_If

EndP

__________________________________________________________________



Proc WriteRawDataDebugT:
    Local @CVSize, @RawDataEnd
    Uses ecx, esi, eax

    mov ecx esi
    add ecx D$CoffSectionSize
    mov D@RawDataEnd ecx

    ; If Signature is not 1. Do a byte chain. Signature = 1 is Code or CodeView 4 ?. Other values are data or CodeView older versions ?
    If D$esi = 1
        move D$CVLabel CV4Label
        jmp L0>
    Else_If D$esi = 2
        move D$CVLabel CV3Label
        jmp L0>
    Else_If D$esi = 4
        move D$CVLabel CV5Label ; found in NET files. This is really really painfull.
        jmp L0>
    Else
        call WriteRawDataDataSection
        ExitP
    End_If

L0:

    mov D$SizeAdd 0

    call InitSectionDebugNumber

    move D@CVSize D$CoffSectionSize
    sub edi 4

    mov D$edi CRLF2 | add edi 4

    push esi
        mov D$edi '    ' | add edi 4
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy D$CVLabel
        zCopy {'Signature: D$ ', 0}
    pop esi

        add D@CVSize esi ; CVSize points to the beginning of the Debug Raw Data

        lodsd | call WriteEax
        If D$esi-4 = 1
            push esi | ZCopy {" ; The compiler that made this Object File, emitted OMF (Object Module Format) types on it's symbols and Types below", 0} | pop esi
        End_If

        mov W$edi CRLF | add edi 2

        ; sometimes we may have a file with only the Signature Byte. (Size of raw data is 4 in VBAEXE6.LIB)
        On D@RawDataEnd = esi, ExitP

    .Do
        movzx eax W$esi
        mov D$SizeAdd eax

        ...If W$esi = 0 ; If lenght = 0 do next line
            add esi 2 ; only increment esi by 2

        ...Else

            push eax

            push esi
                mov D$edi '    ' | add edi 4
                call WriteObjIndice

                zCopy {"Sec", 0}
                zCopy SectionHeaderNumber
                zCopy {".Index", 0}
                zCopy DebugNumber
                zCopy D$CVLabel
                zCopy {"Length: W$ ", 0}
            pop esi

                xor eax eax
                lodsw | call WriteEax

            mov W$edi CRLF | add edi 2
            pop eax

            call WriteRawDataDebugSContantLeafTypeItem

    ...End_If

        call IncrementSectionDebugNumber
        mov eax D@CVSize
        mov W$edi CRLF | add edi 2

    .Loop_Until esi >= eax

EndP

__________________________________________________________________

Proc WriteRawDataDebugFItem:
    Argument @Text1, @Text2
    uses eax

        push esi
            mov D$edi '    ' | add edi 4
            call WriteObjIndice

            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber

            zCopy {'.FPOData', 0}
            mov B$edi '.' | inc edi
            zCopy D@Text1
            zCopy D@Text2
        pop esi

                mov eax D@Text1

                If D$eax = 'ulOf' ; from "ulOffStart" string
                    lodsd | call WriteEax
                    ;call WriteSectionHeaderRawSizeDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                Else_If D$eax+2 = 'Proc'; From "cbProcSize" string
                    lodsd | call WriteEax
                    ;call WriteSectionHeaderPointerToDataDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                Else_If D$eax = 'cdwL'; From "cdwLocals" string
                    lodsd | call WriteEax
                    ;call InitSectionRelocNumber
                    ;call WriteSectionHeaderPointerToRelocDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                Else_If D$eax = 'cdwP'; From "cdwParams" string
                    xor eax eax
                    lodsw | call WriteEax
                    ;call InitSectionLineNumber
                    ;call WriteSectionHeaderPointerToLineNumberDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                Else_If D$eax+2 = 'Prol'; From "cbProlog" string
                    xor eax eax
                    lodsb | call WriteEax
                    ;call InitSectionLineNumber
                    ;call WriteSectionHeaderPointerToLineNumberDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                Else_If D$eax = 'Func'; From "FunctionSpec" string
                    xor eax eax
                    lodsb | call WriteEax
                    ;call InitSectionLineNumber
                    ;call WriteSectionHeaderPointerToLineNumberDiffLabel
                    ;push esi | zcopy {" ; Hex Value:  ", 0} | pop esi

                End_If

        mov W$edi CRLF | add edi 2
EndP


;;
 similar to WriteImageSymbolTable and GetCoffSectionHeader
 Uses FPO_DATA Structure. The FPO_DATA in the structures.str is wrong, the correct is:

[FPOData:
 FPOData.ulOffStart: D$ 0
 FPOData.cbProcSize: D$ 0
 FPOData.cdwLocals: D$ 0
 FPOData.cdwParams: W$ 0
 FPOData.cbProlog: B$ 0
 FPOData.FunctionSpec: B$ 0]
;;

[RawDataulOffStart: 'ulOffStart', 0
 RawDatacbProcSize: 'cbProcSize', 0
 RawDatacdwLocals: 'cdwLocals', 0
 RawDatacdwParams: 'cdwParams', 0
 RawDatacbProlog: 'cbProlog', 0
 RawDataFunctionSpec: 'FunctionSpec', 0]


WriteRawDataDebugF:

    sub edi 4

    mov D$edi CRLF2 | add edi 4
    call WriteRawDataDebugFItem RawDataulOffStart, {': D$ ', 0}
    call WriteRawDataDebugFItem RawDatacbProcSize, {': D$ ', 0}
    call WriteRawDataDebugFItem RawDatacdwLocals, {': D$ ', 0}
    call WriteRawDataDebugFItem RawDatacdwParams, {': W$ ', 0}
    call WriteRawDataDebugFItem RawDatacbProlog, {': B$ ', 0}
    call WriteRawDataDebugFItem RawDataFunctionSpec, {': B$ ', 0}

ret
__________________________________________________________________

WriteRawDataOnlyStringsSection:

    ; Initialize the String Record Counter

    call InitStringTableRecord
    push esi
        call WriteObjIndice
        zCopy {"Sec", 0}
        zCopy SectionHeaderNumber
        zCopy {'.StringData.Signature', 0}
        mov D$edi ': B$', B$edi+4 ' ' | add edi 5
    pop esi
        While B$esi <> 0 | movsb | End_While
    xor eax eax
    lodsb | call WriteEax

    mov W$edi CRLF | add edi 2

    mov edx D$CoffSectionSize
    dec edx
    add edx, esi ; We will use edx as a counter delimiter for the strings

   .Do

        push esi
            call WriteObjIndice
            zCopy {"Sec", 0}
            zCopy SectionHeaderNumber
            zCopy {'.StringData.Arr', 0}
            zCopy StringTableRecord
            zCopy {": B$ '", 0}
        pop esi


        While B$esi <> 0 | movsb | End_While | inc esi | On B$esi = 0, mov edx 0

        mov D$edi "', 0" | add edi 4
        mov W$edi CRLF | add edi 2

        call IncrementStringTableRecord

    .Loop_Until esi >= edx

    sub edi 2
    mov W$edi CRLF | add edi 2
ret

__________________________________________________________________

WriteRawDataDataSection:

L0:         movzx eax B$esi | call WriteEax
            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If
            loop L0<

            sub edi 2
ret
__________________________________________________________________

WriteRawDataCodeSection:

L0:         movzx eax B$esi | call WriteEax
            mov W$edi ', ' | add edi 2 | inc esi
            inc edx
            If edx > 15
                mov W$edi CRLF | add edi 2 | mov edx 0
                mov D$edi '    ', D$edi+4 '    ' | add edi 8
            End_If
            loop L0<

            sub edi 2

ret
__________________________________________________________________

WriteRawDataLinkerDirective:

    mov edx esi | add edx ecx

    .If B$esi = 0
        mov B$edi '0' | inc edi | inc esi
    .Else

        mov B$edi "'" | inc edi
L0:     lodsb
        If al = 0
            dec esi | jmp L1>
        End_If
        stosb | On esi < edx, jmp L0<
L1:     mov B$edi "'" | inc edi

    .End_If

    While esi < edx | lodsb | mov D$edi ', 0' | add edi 3 | End_While

ret
__________________________________________________________________

WriteRawdataEnd:

    mov W$edi CRLF | add edi 2
    call WriteObjIndice
    zCopy {"Sec", 0}
    zCopy SectionHeaderNumber
    zCopy {".RawDataEnd", 0}
    zCopy {": ]", CRLF2}
ret
__________________________________________________________________

Proc WriteRawdataTitle:
    Uses esi, ecx

    zCopy CoffRawDataSectionTitle
    call WriteObjIndice
    zCopy {"ImgSecHdr", 0}
    zCopy SectionHeaderNumber
    zCopy {B$ ".PointerToRawData", W$ CRLF, 0}

    mov W$edi CRLF, B$edi+2 '[' | add edi 3
    call WriteObjIndice
    zCopy {"Sec", 0}
    zCopy SectionHeaderNumber
    zCopy {'.RawData', 0}
    mov D$edi ': B$', B$edi+4 ' ' | add edi 5

EndP
__________________________________________________________________

[CoffRawDataInfoTitle: B$ "_________________________________________________________

; Raw Data Info
_________________________________________________________
", 0]

[CoffRawDataSectionTitle: "
; These are the Raw Data Values referenced at: ", 0]

Proc WriteCoffSectionData:
    Uses esi, eax, ecx, edx


    ...If D$CoffSectionSize = 0

    ...Else_If D$CoffPointerToData = 0 ; Is the Pointer to Raw Data = 0 ? Yes, exit

    ...Else

        push esi | zCopy CoffRawDataInfoTitle | pop esi
        ; esi points to the Begin of the Raw data section, ecx points to the size of the raw data
        mov esi D$CoffSectionBase | add esi D$CoffPointerToData
        mov ecx D$CoffSectionSize, edx 0

        ..If D$RawDataType = RDT_LNKDIRECTIVE
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    Linker Directive Reports:", D$ CRLF2, 0} | pop esi
                call WriteRawDataLinkerDirectiveReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataLinkerDirective

        ..Else_If D$RawDataType = RDT_CODE
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to CODE", D$ CRLF2, 0} | pop esi
                ; call WriteRawDataCodeSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataCodeSection

        ..Else_If D$RawDataType = RDT_VIRTUALDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Virtual Data", D$ CRLF2, 0} | pop esi
                ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_DEBUGS
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Debug$S", D$ CRLF2, 0} | pop esi
                ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            ;call WriteRawDataDebugS
            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_DEBUGF
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Debug$F - Frame Point Omission", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDebugFReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDebugF

        ..Else_If D$RawDataType = RDT_DEBUGT
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Debug$T - Debug Type", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection
            ;call WriteRawDataDebugT

        ..Else_If D$RawDataType = RDT_DEBUGP
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Debug$P", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle
            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_STABSTR
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to NetFramework strings section - stabstr", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle
            sub edi 4
            mov W$edi CRLF | add edi 2
            call WriteRawDataOnlyStringsSection

        ..Else_If D$RawDataType = RDT_IDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Import Table Data - idata", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_RDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Read-Only Data - rdata", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_XDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Exception information - xdata", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_PDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Exception information - pdata", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_EDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Export tables - edata", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_TLSDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Thread-local storage - tls", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_RELOCDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to Image relocations - reloc", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_RSRCDATA
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to resources - rsrc", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else_If D$RawDataType = RDT_ERROR
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    There was an error on the debug or the data section. These raw values will be assumed as DATA", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..Else
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            push esi | zCopy {B$ "    This Raw Data is related only to DATA", D$ CRLF2, 0} | pop esi
            ; call WriteRawDataDataSectionReport
            mov W$edi CRLF, W$edi+2 ';;', W$edi+4 CRLF | add edi 6
            call WriteRawdataTitle

            call WriteRawDataDataSection

        ..End_If

        call WriteRawdataEnd

    ...End_If

EndP



;;
  Values set in 'FirstSectionPointer', 'NumberOfRelocations', 'CoffPointerToReloc'
  by 'GetCoffSectionsVariables'.
;;

WriteCoffReloc:

    call InitSectionRelocNumber

    push esi
        mov esi D$CoffSectionBase | add esi D$CoffPointerToReloc
        mov ecx D$NumberOfRelocations

L0:     push ecx
            call GetCoffRelocHeader
            call IncrementSectionRelocNumber
        pop ecx

        loop L0<
    pop esi
ret



WriteCoffLineNumber:
    call InitSectionLineNumber
    push esi
        mov esi D$CoffSectionBase | add esi D$CoffPointerToLineNumber
        mov ecx D$NumberOfLineNumbers

L0:     push ecx
            call GetCoffLineHeader
            call IncrementSectionLineNumber
        pop ecx

        loop L0<
    pop esi
ret


____________________________________________________________________________________________




[ImageFileEquate: B$ '&IMAGE_FILE_', 0
 IMAGE_FILE_16BIT_MACHINE: '16BIT_MACHINE', 0
 IMAGE_FILE_32BIT_MACHINE: '32BIT_MACHINE__', 0
 IMAGE_FILE_AGGRESSIVE_WS_TRIM: 'AGGRESSIVE_WS_TRIM', 0
 IMAGE_FILE_BYTES_REVERSED_HI: 'BYTES_REVERSED_HI__', 0
 IMAGE_FILE_BYTES_REVERSED_LO: 'BYTES_REVERSED_LO__', 0
 IMAGE_FILE_DEBUG_STRIPPED: 'DEBUG_STRIPPED__', 0
 IMAGE_FILE_DLL: 'DLL__', 0
 IMAGE_FILE_EXECUTABLE_IMAGE: 'EXECUTABLE_IMAGE__', 0
 IMAGE_FILE_LARGE_ADDRESS_AWARE: 'LARGE_ADDRESS_AWARE__', 0
 IMAGE_FILE_LINE_NUMS_STRIPPED: 'LINE_NUMS_STRIPPED__', 0
 IMAGE_FILE_LOCAL_SYMS_STRIPPED: 'LOCAL_SYMS_STRIPPED__', 0
 IMAGE_FILE_NET_RUN_FROM_SWAP: 'NET_RUN_FROM_SWAP__', 0
 IMAGE_FILE_RELOCS_STRIPPED: 'RELOCS_STRIPPED__', 0
 IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP: 'REMOVABLE_RUN_FROM_SWAP__', 0
 IMAGE_FILE_SYSTEM: 'SYSTEM__', 0
 IMAGE_FILE_UP_SYSTEM_ONLY: 'UP_SYSTEM_ONLY__', 0]


WriteObjCharacteristics:
    push esi

    mov D$edi ' ; ' | add edi 3

    test eax &IMAGE_FILE_32BIT_MACHINE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_32BIT_MACHINE

L1: test eax &IMAGE_FILE_16BIT_MACHINE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_16BIT_MACHINE

L1: test eax &IMAGE_FILE_BYTES_REVERSED_HI | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_BYTES_REVERSED_HI

L1: test eax &IMAGE_FILE_AGGRESSIVE_WS_TRIM | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_AGGRESSIVE_WS_TRIM

L1: test eax &IMAGE_FILE_BYTES_REVERSED_LO | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_BYTES_REVERSED_LO

L1: test eax &IMAGE_FILE_DEBUG_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_DEBUG_STRIPPED

L1: test eax &IMAGE_FILE_DLL | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_DLL

L1: test eax &IMAGE_FILE_EXECUTABLE_IMAGE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_EXECUTABLE_IMAGE

L1: test eax &IMAGE_FILE_LARGE_ADDRESS_AWARE | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_LARGE_ADDRESS_AWARE

L1: test eax &IMAGE_FILE_LINE_NUMS_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_LINE_NUMS_STRIPPED

L1: test eax &IMAGE_FILE_LOCAL_SYMS_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_LOCAL_SYMS_STRIPPED

L1: test eax &IMAGE_FILE_NET_RUN_FROM_SWAP | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_NET_RUN_FROM_SWAP

L1: test eax &IMAGE_FILE_RELOCS_STRIPPED | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_RELOCS_STRIPPED

L1: test eax &IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP

L1: test eax &IMAGE_FILE_SYSTEM | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_SYSTEM

L1: test eax &IMAGE_FILE_UP_SYSTEM_ONLY | jz L1>
        zCopy ImageFileEquate, IMAGE_FILE_UP_SYSTEM_ONLY

L1: pop esi

    While B$edi-1 = '_' | dec edi | End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Data, Equates used more than once
;;

[COFF_HEADER_SIZE 03C    ARCH_TAG 8
 LIB_USER_ID 01C         LIB_MEMBER_SIZE 030]

[IMAGE_ARCHIVE_MEMBER_HEADERstring: ' Structure: IMAGE_ARCHIVE_MEMBER_HEADER', 0]
____________________________________________________________________________________________

;;
  This is called multiple times
;;

[MemberSize: ?]

GetMemberSize:
    push esi
        mov ecx 10, ebx 0, eax 0
        While B$esi > ' '
            mul ecx
            push eax
                lodsb | sub al '0' | mov bl al
            pop eax
            add eax ebx
        End_While
        mov D$MemberSize eax
    pop esi
ret

______________________________

[MemberDate: ?]

GetMemberDate:
    push esi
        mov ecx 10, ebx 0, eax 0
        While B$esi > ' '
            mul ecx
            push eax
                lodsb | sub al '0' | mov bl al
            pop eax
            add eax ebx
        End_While
        mov D$MemberDate eax
    pop esi
ret

______________________________

Proc WriteNamesLinkerMemberX:
    Argument @Name, @n, @LinkerMember
    Local @Item

        push esi
            mov esi D@LinkerMember
            While B$esi <> 0 | movsb | End_While

            mov B$edi '.' | inc edi
            mov esi D@Name

            move D@Item D$esi

            If D$esi = 'Size'
                pop esi | push esi
                call GetMemberSize
                mov esi D@Name

            Else_If D$esi = 'Date'
                pop esi | push esi
                call GetMemberDate
                mov esi D@Name
            End_If

            While B$esi <> 0 | movsb | End_While
        pop esi

        mov D$edi ': B$' | add edi 4

        .If D@n <> 0-2

            mov W$edi " '" | add edi 2
            mov ecx D@n

            If D@Item = 'User'
                mov D$edi '****', W$edi+4 '**' | add edi 6 | sub ecx 6 | add esi 6
            End_If

            rep movsb

            While B$edi-1 = 0 | dec edi | End_While

            mov B$edi "'" | inc edi
            mov W$edi CRLF | add edi 2

        .Else
            mov B$edi ' ' | inc edi
            mov ecx D@n | neg ecx
L0:         lodsb | and eax 0FF | call WriteEax | mov W$edi ', ' | add edi 2 | loop L0<
            sub edi 2

        .End_If
EndP

____________________________________________________________________________________________

[LibObjIndice: '000001', 0]

InitCoffIndice:
    mov D$LibObjIndice '0000', D$LibObjIndice+4 '01'
ret

IncrementLibCoffIndice:
    lea ebx D$LibObjIndice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

WriteCoffIndice:
    push esi
        mov D$edi 'Obj_' | add edi 4
        mov eax D$LibObjIndice, D$edi eax
        mov ax W$LibObjIndice+4, W$edi+4 ax
        mov D$edi+6 ': ; ' | add edi 10
    pop esi
ret


_________________________________________________________

[szDateString: B$ ? #64]
[szTimeString: B$ ? #64]

Proc WriteLinkerMemberTimeDateStamp:
    Arguments @TimeDate
    Uses esi

        ; Write the Value of the Date in Hexadecimal string
        mov esi {' ; Hex Value:  ', 0}
        sub edi 2 ; We need to subtract 02 Bytes, to bypass the CRLF
        While B$esi <> 0 | movsb | End_While
        mov eax D@TimeDate
        call Writeeax

        mov esi {' - TimeDate Stamp: ', 0}
        While B$esi <> 0 | movsb | End_While

      ; Get the DateTime Stamp

      ; Time date stamp to string Function
        call TimeDateStampToString D@TimeDate, {"yyyy/MM/dd ddd ", 0},
                                   {"HH:mm:ss UTC", 0}, szDateString, szTimeString

      ; Write the TimeDate Stamp strings
        mov esi szDateString
        While B$esi <> 0 | movsb | End_While

        mov esi szTimeString
        While B$esi <> 0 | movsb | End_While

      ; We now Add the paragraphs marks, and add 02 Bytes at edi:
        mov W$edi CRLF | add edi 2


EndP

_________________________________________________________

Proc WriteLinkerMemberSizeHex:
    Arguments @SizeValue
    Uses esi
    ; Write the Value of the Size in Hexadecimal string

        mov esi {' ; Hex Value:  ', 0}
        sub edi 2 ; We need to subtract 02 Bytes, to bypass the CRLF
        While B$esi <> 0 | movsb | End_While
        mov eax D@SizeValue
        call Writeeax
        mov W$edi CRLF | add edi 2 ; We now Add the paragraphs marks, and add 02 Bytes at edi

EndP
_________________________________________________________


[NamesLinkerMember: 'NamesLinkerMember'
 ARCHIVE_MEMBER_HEADER_Indice: '000001', 0]


[NamesLinkerMember1: 'NamesLinkerMember_1', 0]

Write_IMAGE_ARCHIVE_MEMBER_HEADER_1:
    mov B$edi '[' | inc edi

    call WriteNamesLinkerMemberX {'Name1', 0}, 16, NamesLinkerMember1
    call WriteNamesLinkerMemberX {'Date', 0}, 12, NamesLinkerMember1
    call WriteLinkerMemberTimeDateStamp D$MemberDate
    call WriteNamesLinkerMemberX {'UserID', 0}, 6, NamesLinkerMember1
    call WriteNamesLinkerMemberX {'GroupID', 0}, 6, NamesLinkerMember1
    call WriteNamesLinkerMemberX {'Mode', 0}, 8, NamesLinkerMember1
    call WriteNamesLinkerMemberX {'Size1', 0}, 10, NamesLinkerMember1
    call WriteLinkerMemberSizeHex D$MemberSize
    call WriteNamesLinkerMemberX {'EndHeader', 0}, 0-2, NamesLinkerMember1

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


[NamesLinkerMember2: 'NamesLinkerMember_2', 0]

Write_IMAGE_ARCHIVE_MEMBER_HEADER_2:
    mov B$edi '[' | inc edi

    call WriteNamesLinkerMemberX {'Name1', 0}, 16, NamesLinkerMember2
    call WriteNamesLinkerMemberX {'Date', 0}, 12, NamesLinkerMember2
    call WriteLinkerMemberTimeDateStamp D$MemberDate
    call WriteNamesLinkerMemberX {'UserID', 0}, 6, NamesLinkerMember2
    call WriteNamesLinkerMemberX {'GroupID', 0}, 6, NamesLinkerMember2
    call WriteNamesLinkerMemberX {'Mode', 0}, 8, NamesLinkerMember2
    call WriteNamesLinkerMemberX {'Size1', 0}, 10, NamesLinkerMember2
    call WriteLinkerMemberSizeHex D$MemberSize
    call WriteNamesLinkerMemberX {'EndHeader', 0}, 0-2, NamesLinkerMember2

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+3 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret


[NamesLinkerMember3: 'NamesLinkerMember_3', 0]

Write_IMAGE_ARCHIVE_MEMBER_HEADER_3:
    mov B$edi '[' | inc edi

    call WriteNamesLinkerMemberX {'Name1', 0}, 16, NamesLinkerMember3
    call WriteNamesLinkerMemberX {'Date', 0}, 12, NamesLinkerMember3
    call WriteLinkerMemberTimeDateStamp D$MemberDate
    call WriteNamesLinkerMemberX {'UserID', 0}, 6, NamesLinkerMember3
    call WriteNamesLinkerMemberX {'GroupID', 0}, 6, NamesLinkerMember3
    call WriteNamesLinkerMemberX {'Mode', 0}, 8, NamesLinkerMember3
    call WriteNamesLinkerMemberX {'Size1', 0}, 10, NamesLinkerMember3
    call WriteLinkerMemberSizeHex D$MemberSize
    call WriteNamesLinkerMemberX {'EndHeader', 0}, 0-2, NamesLinkerMember3

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

[CoffTitle: "

_____________________________________________________________________________________________
_____________________________________________________________________________________________

; The Coff File Format Starts here. Indice: ", 0]

WriteCoffTitle:
    push esi
        mov esi CoffTitle | While B$esi <> 0 | movsb | End_While
        mov eax D$LibObjIndice, D$edi eax,
            ax W$LibObjIndice+4, W$edi+4 ax,
            D$edi+6 CRLF2 | add edi 10
    pop esi
ret


[Obj_LIB_USER_ID: 'Obj_'
 CopyOf_LIB_USER_ID: '000001', 0]

Proc Write_Obj_IMAGE_ARCHIVE_MEMBER_HEADER:
    Local @StartAddress

    mov B$edi '[' | inc edi

    ...If B$esi = '/'
      ; First or Second Header: nop
        ..If B$esi+1 <> ' '
          ; Case of Obj with LongNames Table in the Third Header:
            call WriteCoffIndice
            push esi
                inc esi
                If D$LongNamesBase <> 0
                    call CheckLongNameDisplacement | On eax = &FALSE, jmp L1>
                    ; to be used in AddListviewItem
                    mov D$StartObjNameAddress edi
                    call WriteLongName esi
                Else

   L1:
                    ; to be used in AddListviewItem
                    mov D$StartObjNameAddress edi
                    call CopyObjSymbolName

                End_If
                mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring
                While B$esi <> 0 | movsb | End_While
                mov W$edi CRLF | add edi 2
            pop esi
        ..Else
            ; There seem to exist case of Functions called by Number, here
            mov D$StartObjNameAddress edi

            call WriteCoffIndice
            While B$esi <> 0 | movsb | End_While
            push esi
                mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring
                While B$esi <> 0 | movsb | End_While
                mov W$edi CRLF | add edi 2
            pop esi

        ..End_If

    ...Else
      ; Case of Obj without LongNames Table (No Third Header) Ex.: UUID.LIB ; WLDAP32.LIB:
        call WriteCoffIndice

        mov D$StartObjNameAddress edi
        push esi

            call CopyObjSymbolName
            mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring
            While B$esi <> 0 | movsb | End_While
            mov W$edi CRLF | add edi 2
        pop esi

    ...End_If

    call ParseLibObjectSymbolName D$StartObjNameAddress

    mov eax D$esi+LIB_USER_ID, D$CopyOf_LIB_USER_ID eax,
        ax W$esi+LIB_USER_ID+4, W$CopyOf_LIB_USER_ID+4 ax

    call WriteNamesLinkerMemberX {'Name1', 0}, 16, Obj_LIB_USER_ID
    call WriteNamesLinkerMemberX {'Date', 0}, 12, Obj_LIB_USER_ID
    call WriteLinkerMemberTimeDateStamp D$MemberDate
    call WriteNamesLinkerMemberX {'UserID', 0}, 6, Obj_LIB_USER_ID
    call WriteNamesLinkerMemberX {'GroupID', 0}, 6, Obj_LIB_USER_ID
    call WriteNamesLinkerMemberX {'Mode', 0}, 8, Obj_LIB_USER_ID
    call WriteNamesLinkerMemberX {'Size1', 0}, 10, Obj_LIB_USER_ID
    call WriteLinkerMemberSizeHex D$MemberSize
    call WriteNamesLinkerMemberX {'EndHeader', 0}, 0-2, Obj_LIB_USER_ID

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5

    lea ebx D$ARCHIVE_MEMBER_HEADER_Indice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While

EndP
____________________________________________________________________________________________


; This function is to be used for displaying the object file name in the list view

; Built this way to solve really weird file nameings paths like:

;;
[Obj_000009: ; Long Name: wizard\wizard.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000001: ; Short Name: poide.exe Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000004: ; Short Name: poide.exe Function Name: _WizAddProjectFileA@4 Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000050: ; Long Name: unwind-dw2-fde.o Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000235 ; Short Name: $UWD\except.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000235 ; Long Name: $UWD\validadd.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000135 ; Long Name: ../XXX\nanonano.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
[Obj_000134 ; Long Name: ./.XXX/nanonano.obj Structure: IMAGE_ARCHIVE_MEMBER_HEADER
;;

[ObjectName: B$ 0 #256]
[StartObjNameAddress: D$ 0]
[EndObjNameAddress: D$ 0]
[ObjExtensionStr: B$ 0 #16]
[UseObjExtension: D$ 0]
[ExportedLibrary: D$ 0]
[ExportedLibraryStr: B$ 0 #256]
[UseObjPath: D$ 0]
[ObjPathStr: B$ 0 #256]

Proc ParseLibObjectSymbolName:
    Arguments @StartAddress
    Local @TempEndAddr, @TempStartAddr, @StartExportObjAddr, @EndObjPathAddr, @StartObjExtensionAddr, @UseExportedLibrary

    pushad

    ; Always clear the loaded data before use.
    call ClearBuffer ObjectName, 256
    call ClearBuffer ObjExtensionStr, 16
    call ClearBuffer ObjPathStr, 256

    ; Initialize all the data
    mov D$ObjFileNameType 0
    mov D$UseObjPath 0
    mov D$UseObjExtension 0
    mov D@TempStartAddr 0
    mov D@TempEndAddr 0
    mov D@StartExportObjAddr 0
    mov D@UseExportedLibrary 0

    mov esi D@StartAddress

    ; 1St Step. Locate the start and the end of the String, and also get the FileNameType and the check Exported Library
    mov ecx 0 ; we will use ecx as a counter.
              ; The maximum size allowed for this is 784 bytes (Max path + Max Object Name + Max exported Lib string + Max extension)
    .While ecx <> (256*3+16)

        .If_And D$esi = 'Long', D$esi+4 = ' Nam' ; is it a Long Name string type ?

            ; to be used in AddListviewItem
            mov D$ObjFileNameType 1
            add D@StartAddress 11 ; (String len) "Long Name: "

        .Else_If_And D$esi = 'Shor', D$esi+4 = 't Na' ; is it a Short Name string type ?

            ; to be used in AddListviewItem
            mov D$ObjFileNameType 0
            add D@StartAddress 12 ; (String len) "Short Name: "

        .Else_If_And D$esi = ' Fun', D$esi+4 = 'ctio', D$esi+8 = 'n Na', D$esi+12 = 'me: '

            ; to be used in AddListviewItem
            mov D@StartExportObjAddr esi
            mov D@UseExportedLibrary 1

        .Else_If_And D$esi = ' Str', D$esi+4 = 'uctu', D$esi+8 = 're: ' ; Did we reached the end of the string ?
                                                                        ; Our string always ends with " Structure: "
            mov D@TempEndAddr esi
            jmp L1>

        .End_If
        inc ecx
        inc esi
    .End_While

   L1:


    ; 2nd Step. Now that we have the start and ending address, we must check if we are dealing with a non-common format.
    ; These formats always have the String " Function Name: " on it. Copy it to the proper buffer (ExportedLibraryStr)

    .If D@UseExportedLibrary = 1
        mov esi D@StartExportObjAddr
        move D@TempEndAddr D@StartExportObjAddr
    .End_If


    ; 3rd Step. Now that we have our full string, we need to see if it have any path inside.
    ; We must start from the end to the beginning of the string to find the path chars.
    mov esi D@StartAddress
    mov ecx D@TempEndAddr

    .While esi <> ecx

        If_Or B$ecx = '/', B$ecx = '\'

            mov D@EndObjPathAddr ecx
            mov D$UseObjPath 1
            jmp L1>
        End_If

        dec ecx
    .End_While

   L1:

    ; 4th Step. If we have a Path, we will copy it to the proper Buffer (ObjPathStr)

    .If D$UseObjPath = 1
        ; Here esi is already defined as the starting point. We don't need to change it now.
        mov ecx D@EndObjPathAddr
        mov edi ObjPathStr

        .While esi <> ecx
            movsb
            ;inc esi
        .End_While
        inc esi ; Bypass the last "/" or "\" char preceeding the object name.
        mov D@StartAddress esi ; Will points to the beginning of the object name only

    .End_If

    ; 5th Step. At this point we have only the name of the object and it's extension (if any).
    ; So we must find and copy the object extension.

    mov esi D@StartAddress
    mov ecx D@TempEndAddr
    mov edi ObjectName

    On esi = ecx, jmp L2> ; If the end and starting address is teh same ones, it is an empty string

    .Do

        .If B$esi = '.'
            mov D$UseObjExtension 1
            mov edi ObjExtensionStr
            inc esi ; Bypass the "." char
            While esi <> ecx
                movsb
            End_While
            jmp L2>
        .End_If

        movsb
    .Loop_Until esi = ecx

   L2:

    popad
EndP

____________________________________________________________________________________________


ParseLibObj:
    push esi
        ...If W$esi = &IMAGE_FILE_MACHINE_UNKNOWN
            ..If W$esi+2 = &IMPORT_OBJECT_HDR_SIG2
              ; +12 is the 'NameType'. 0 >>> Ordinal for the Function,
              ; that should be at +10...
                .If W$esi+012 <> 0
                    call WriteLibImportObj
                    add esi 014
                    call WriteImportObjStrings

                .Else
                    ;call SendMeThisLib
                    call WriteLibImportObj

                    movzx eax W$esi+010
                    mov edx D$esi+0C
                    add esi 014
                    add edx esi
                    mov ebx esi | While B$ebx <> 0 | inc ebx | End_While | inc ebx
                    If ebx < edx
                        call WriteImportObjStrings
                    Else
                        call WriteImportObjStringAndOrdinal eax
                    End_If
                .End_If

            ..Else
                ; to be added in AddListviewItem
                ; Always clear the loaded data before use.
                call ClearBuffer ExportedLibraryStr, 256
                mov D$ExportedLibrary 0
                call GetCoffListing

            ..End_If

        ...Else
            ; to be added in AddListviewItem
            ; Always clear the loaded data before use.
            call ClearBuffer ExportedLibraryStr, 256
            mov D$ExportedLibrary 0
            call GetCoffListing

        ...End_If
    pop esi
ret
____________________________________________________________________________________________

[IMAGE_FILE_MACHINE_Text:
 &IMAGE_FILE_MACHINE_UNKNOWN, '&IMAGE_FILE_MACHINE_UNKNOWN', 0
 &IMAGE_FILE_MACHINE_ALPHA, '&IMAGE_FILE_MACHINE_ALPHA', 0
 &IMAGE_FILE_MACHINE_ALPHA64, '&IMAGE_FILE_MACHINE_ALPHA64', 0
 &IMAGE_FILE_MACHINE_ARM, '&IMAGE_FILE_MACHINE_ARM', 0
 &IMAGE_FILE_MACHINE_AXP64, '&IMAGE_FILE_MACHINE_AXP64', 0
 &IMAGE_FILE_MACHINE_CEF, '&IMAGE_FILE_MACHINE_CEF', 0
 &IMAGE_FILE_MACHINE_I386, '&IMAGE_FILE_MACHINE_I386', 0
 &IMAGE_FILE_MACHINE_I486, '&IMAGE_FILE_MACHINE_I486', 0
 &IMAGE_FILE_MACHINE_I586, '&IMAGE_FILE_MACHINE_I586', 0
 &IMAGE_FILE_MACHINE_IA64, '&IMAGE_FILE_MACHINE_IA64', 0

 &IMAGE_FILE_MACHINE_M68K, '&IMAGE_FILE_MACHINE_M68K', 0
 &IMAGE_FILE_MACHINE_MIPS16, '&IMAGE_FILE_MACHINE_MIPS16', 0
 &IMAGE_FILE_MACHINE_MIPSFPU, '&IMAGE_FILE_MACHINE_MIPSFPU', 0
 &IMAGE_FILE_MACHINE_MIPSFPU16, '&IMAGE_FILE_MACHINE_MIPSFPU16', 0
 &IMAGE_FILE_MACHINE_POWERPC, '&IMAGE_FILE_MACHINE_POWERPC', 0
 &IMAGE_FILE_MACHINE_R10000, '&IMAGE_FILE_MACHINE_R10000', 0
 &IMAGE_FILE_MACHINE_R3000, '&IMAGE_FILE_MACHINE_R3000', 0
 &IMAGE_FILE_MACHINE_R4000, '&IMAGE_FILE_MACHINE_R4000', 0
 &IMAGE_FILE_MACHINE_R6000, '&IMAGE_FILE_MACHINE_R6000', 0
 &IMAGE_FILE_MACHINE_SH3, '&IMAGE_FILE_MACHINE_SH3', 0

 &IMAGE_FILE_MACHINE_SH3E, '&IMAGE_FILE_MACHINE_SH3E', 0
 &IMAGE_FILE_MACHINE_SH4, '&IMAGE_FILE_MACHINE_SH4', 0
 &IMAGE_FILE_MACHINE_SH5, '&IMAGE_FILE_MACHINE_SH5', 0
 &IMAGE_FILE_MACHINE_SH3DSP, '&IMAGE_FILE_MACHINE_SH3DSP', 0
 &IMAGE_FILE_MACHINE_THUMB, '&IMAGE_FILE_MACHINE_THUMB', 0
 &IMAGE_FILE_MACHINE_WCEMIPSV2, '&IMAGE_FILE_MACHINE_WCEMIPSV2', 0
 &IMAGE_FILE_MACHINE_AM33, '&IMAGE_FILE_MACHINE_AM33', 0
 &IMAGE_FILE_MACHINE_AMD64, '&IMAGE_FILE_MACHINE_AMD64', 0
 &IMAGE_FILE_MACHINE_CEE, '&IMAGE_FILE_MACHINE_CEE', 0
 &IMAGE_FILE_MACHINE_EBC, '&IMAGE_FILE_MACHINE_EBC', 0

 &IMAGE_FILE_MACHINE_M32R, '&IMAGE_FILE_MACHINE_M32R', 0
 &IMAGE_FILE_MACHINE_POWERPCFP, '&IMAGE_FILE_MACHINE_POWERPCFP', 0
 &IMAGE_FILE_MACHINE_TRICORE, '&IMAGE_FILE_MACHINE_TRICORE', 0, 0-1, 0-1]

[UnKown_IMAGE_FILE_MACHINE: ' ; Unknown or corrupted IMAGE_FILE_MACHINE', 0]

Proc WriteIMAGE_FILE_MACHINE:
    Argument @Type
    Uses esi

        mov esi IMAGE_FILE_MACHINE_Text, eax D@Type | and eax 0FFFF

        ..While D$esi <> 0-1
            .If D$esi = eax
                add esi 4 | While B$esi <> 0 | movsb | End_While | ExitP
            .Else
                add esi 5
                .While B$esi <> '&'
                    inc esi
                    If D$esi = 0-1
                        call WriteEax
                        mov esi UnKown_IMAGE_FILE_MACHINE
                        While B$esi <> 0 | movsb | End_While | ExitP
                    End_If
                .End_While
                sub esi 4
            .End_If
        ..End_While
EndP


Proc IsIMAGE_FILE_MACHINE:
    Argument @Type
    Uses esi

        mov esi IMAGE_FILE_MACHINE_Text, eax D@Type | and eax 0FFFF

        .While D$esi <> 0-1
            .If D$esi = eax
                mov eax &TRUE | ExitP
            .Else
                add esi 5
                While B$esi <> '&'
                    inc esi
                    If D$esi = 0-1
                        mov eax &FALSE | ExitP
                    End_If
                End_While
                sub esi 4
            .End_If
        .End_While
EndP

____________________________________________________________________________________________



;;
 
 
 LibScan_ErrManager
 _____________________
 
    This function manages the error cases of the LIbScanner system
 
 Parameters: 
 
    ErrFlag: Handle to the error message. The equates used on this function are:

    IMP_OBJ_RESBIT_ERR = 01 ; Reserved Bits are not Zero
    IMP_OBJ_UNKTYPE_ERR = 02 ; Unknown Type
    

        PE_DATADIRSMALL 01 = The Directory Entry is located before the 1st section, on the DosHeader or in the PEHeader.
 
 Return values:
 
    Return &TRUE if  the user wants to contine analysing the file.
    Return &FALSE if he wants to stop the analysis.
 
 
;;

;[PE_DATADIRSMALL 01]
;[PE_DIRDATADOS 02]
;[PE_DIRDATASIZE 03]
;[PE_DIRDATAUNKNOWN 04]

[IMP_OBJ_RESBIT_ERR 01]
[IMP_OBJ_UNKTYPE_ERR 02]


Proc LibScan_ErrManager:
    Arguments @ErrFlag
    Uses edx ; messagebox function changes the value of edx...So we need to preserve it, because we will use it.

    mov eax D@ErrFlag

    .If eax = IMP_OBJ_RESBIT_ERR


        call 'USER32.MessageBoxA' 0, {"CORRUPTED LIBRARY !!!

The reserved Bits for the ImpObjHdr.Type member of the structure IMPORT_OBJECT_HEADER
on this Library are not Zero.

RosAsm can continue loading this file, but we will set the reserved Bits to ZERO to you be able to load this safelly.

* Press the YES button to set the necessary bits to Zero.
* Press the NO button to end analysing this file.
  
If after the automatically fix, this file still have errors, please, report this error to the author at:
  Betov@free.fr

Or report this error to the Development Team at the main RosAsm Forum.

Many thanks in advance. Betov.
  
", 0}, {'Corrupted Library', 0}, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO

            If eax = &IDNO
                mov eax &FALSE
            Else_If eax = &IDYES
                mov eax &TRUE
            End_If

    .Else_If eax = IMP_OBJ_UNKTYPE_ERR

          call 'USER32.MessageBoxA' D$hwnd {"UNKNOWN TYPE !!!.

The reserved Bits for the ImpObjHdr.Type member of the structure IMPORT_OBJECT_HEADER
on this Library are of a Unknown Type.

The regular Type values of this member can not be 3, 7, 11, 15 to 31.

Your file have one of those unknown types.

RosAsm can't continue loading this file.

Please, report this error to the author, and send this file to the author at:
  Betov@free.fr

Or report this error to the Development Team at the main RosAsm Forum.

Many thanks in advance. Betov." 0} {"Unknown Type" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

            mov eax &FALSE

    .End_If

EndP


____________________________________________________________________________________________

[ImpObjHdrSig1Text: B$ 'ImpObjHdr.Sig1: W$ &IMAGE_FILE_MACHINE_UNKNOWN', 0
 ImpObjHdrSig2Text: 'ImpObjHdr.Sig2: W$ &IMPORT_OBJECT_HDR_SIG2', 0
 ImpObjHdrVersionText: 'ImpObjHdr.Version: W$ ', 0
 ImpObjHdrMachineText: 'ImpObjHdr.Machine: W$ ', 0
 ImpObjHdrTimeDateStampText: 'ImpObjHdr.TimeDateStamp: D$ ', 0
 ImpObjHdrSizeOfDataText: 'ImpObjHdr.SizeOfData: D$ ', 0
 ImpObjHdrOrdinalText: 'ImpObjHdr.Ordinal.Hint: W$ ', 0
 ImpObjHdrType: 'ImpObjHdr.Type: W$ ', 0]

WriteObjIndice:
    mov D$edi 'Obj',
        ecx D$LibObjIndice, D$edi+3 ecx,
        cx W$LibObjIndice+4, W$edi+7 cx

    add edi 9

    mov B$edi '.' | inc edi
ret


WriteIndiceOnly:
    mov ecx D$LibObjIndice, D$edi ecx, cx W$LibObjIndice+4, W$edi+4 cx
    add edi 6
ret


Proc WriteLibImportObjItem:
    Arguments @Text, @Flag
    Uses esi

        call WriteObjIndice

        mov esi D@Text | While B$esi <> 0 | movsb | End_While

        If D@Text = ImpObjHdrMachineText
            call WriteIMAGE_FILE_MACHINE eax

        Else_If D@Text = ImpObjHdrType
            ; Need to increase edi with CRLF at the end, because when  the function returns, it
            ; decreases edi by 2. And we need to close the bracket after the paragraph, due to the commented
            ; Hexadecimal Value below.
            push eax
            call WriteLibImportObjType eax
            pop eax
            zCopy {' ; Hex Value:  ', 0}
            call WriteEax
            mov W$edi CRLF | add edi 2

        Else_If D@Text = ImpObjHdrSizeOfDataText

            push esi
                call WriteObjIndice
                zCopy {'ImportStringsEnd', 0}
            pop esi

            mov D$edi ' - ' | add edi 3
            push esi
                call WriteObjIndice
                zCopy {'ImportStrings ; Hex Value:  ', 0}
            pop esi
            call WriteEax

        Else_If B@Flag = &TRUE
            call WriteEax

        End_If

        mov W$edi CRLF | add edi 2
EndP

____________________________________________________________________________________________


[LibImportObjType:
; Bits 0 and 1:

 IMPORT_OBJECT_CODE: '&IMPORT_OBJECT_CODE', 0
 IMPORT_OBJECT_DATA: '&IMPORT_OBJECT_DATA', 0
 IMPORT_OBJECT_CONST: '&IMPORT_OBJECT_CONST', 0

; Bits 2 to 3 (SHRed) >>> 0 to 1
 IMPORT_OBJECT_NAME: '&IMPORT_OBJECT_NAME', 0
 IMPORT_OBJECT_NAME_NO_PREFIX: '&IMPORT_OBJECT_NAME_NO_PREFIX', 0
 IMPORT_OBJECT_NAME_UNDECORATE: '&IMPORT_OBJECT_NAME_UNDECORATE', 0
 IMPORT_OBJECT_ORDINAL: '&IMPORT_OBJECT_ORDINAL', 0
]


;;
  Example, if a Type Word would be:
  
  '&IMPORT_OBJECT_DATA' and '&IMPORT_OBJECT_NAME_NO_PREFIX'
  
  The Assembly representation would be:
  
  &IMPORT_OBJECT_DATA + (&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)
;;

;;

    We have at eax the ImpObjHdr.Type member that is a WORD data. Accordying to the documentation, we only
    are interested in Bits 0 to 3 to use for Type and NameType members interpretations.
    Bit4 is for Unknown Type and should not be considered to analysis at the present moment.
    Bits 5 to 15 are reserved and should be setlled to 0 accordying to the docs, because they are not
    interpreteded and are not used whatsoever.
    
    A Word data have a maximum value of 0FFFF (65535), meaning that bits 0 to 15 are all settled to 1.
    
    Since bits 5 to 15 are not used, we can set them all to 0, that will result on a maximum value for this member of
    31 (Bits 0 to 4 are all settled to 1, then).
    
    Now, our WORD have a maximum range of 31.
    
    Accordying to the docs, we can have a possible set of Equates under the following form:
 
    EQUATES Forms                                               Value   Bits Sets                       Union
    
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_ORDINAL shl 2)          = 0     No Bits are Flaged              Ordinal
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_NAME shl 2)             = 4     Only Bit2 is flaged             Hint
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)   = 8     Only Bit3 is flaged             Hint
    &IMPORT_OBJECT_CODE+(&IMPORT_OBJECT_NAME_UNDECORATE shl 2)  = 12    Only Bit2 and Bit3 are flaged   Hint
     
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_ORDINAL shl 2)          = 1     Only Bit0 is flaged             Ordinal
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_NAME shl 2)             = 5     Only Bit0 and Bit2 are flaged   Hint
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)   = 9     Only Bit0 and Bit3 are flaged   Hint
    &IMPORT_OBJECT_DATA+(&IMPORT_OBJECT_NAME_UNDECORATE shl 2)  = 13    Only Bit0,Bit2,Bit3 are flaged  Hint
 
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_ORDINAL shl 2)         = 2     Only Bit1 is flaged             Ordinal
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_NAME shl 2)            = 6     Only Bit1 and Bit2 are flaged   Hint
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_NAME_NO_PREFIX shl 2)  = 10    Only Bit1 and Bit3 are flaged   Hint
    &IMPORT_OBJECT_CONST+(&IMPORT_OBJECT_NAME_UNDECORATE shl 2) = 14    Only Bit1,Bit2,Bit3 are flaged  Hint

    
    So, All values equal or above 14 are not allowed and also we can not have the following values and interpretations:
    So, from values between 31 and 14 we have an Unknown Type. All Values bigger or equal then 32 we have a corrupted
    Library because the reserved Fields are not 0.
    
    The list of Uknown Types are:
    03; 07; 11, 15 to 31

   Ex.: We have a WORD with the value of 4073. So we clean the high bits resulting, like this:
                mov eax 4073 | mov ah 0 ; Bits 8 to 15 settled to 0)
        
        Now we must clear the bits 5 to 7, because they are unused. Like this:
         btr eax 5 | btr eax 6 | btr eax 7 ; I´m using this, because it is probably faster on big files.
         
        Or we can also do this:
        shl eax 3 | mov ah 0 | shr eax 3
        
   
   
;;

[SymNameType: B$ 0] ; Holds the NameType value to be used to Skip the leading chars on NameType member.

Proc WriteLibImportObjType:
    Argument @Flags
    Uses esi, ebx

        mov eax D@Flags

        .If_And eax >= 32, eax <= 65535 ; Check for Corrupted library. Reserved Bits are not 0

            call LibScan_ErrManager IMP_OBJ_RESBIT_ERR

            If eax = &FALSE ; The user pressed No
                ExitP

            Else_If eax = &TRUE ; The user pressed Yes. We will Zero all reserved bits for him continue the parsing.
                mov eax D@Flags ; Restore the original value of eax to be fixed
                mov ah 0 ; Clear High Bits Flags
                btr eax 5 | btr eax 6 | btr eax 7 ; Clear Bits 5 to 7

            End_If

        .End_If

        ; Check for Unknown Types


        test eax 010 | jz A1> ; Is Bit4 Flagged ? So, is eax = 16 to 31 ? No, jmp over
            call LibScan_ErrManager IMP_OBJ_UNKTYPE_ERR | ExitP
A1:
        test eax 1 | jz A2> ; Is bit0 flagged ? No...Ok, we don´ have any unknown types (15, 11, 7, 3)
            test eax 2 | jz A2> ; is bit1 also Flagged ? No...Ok, we don´ have any unknown types (15, 11, 7, 3) jmp over.

            call LibScan_ErrManager IMP_OBJ_UNKTYPE_ERR | ExitP ; Yes bit0 and bit1 are flagged, we have unknown types (15, 11, 7, 3)

A2:


    ; We are Ok. We have only the values 0,1,2,4,5,6,8,9,10,12,13,14 on eax

          ; First lower two Bits (Code / Data / Constant). We can only have one of the 02 bits set, or none of them.

            test eax &IMPORT_OBJECT_DATA | jz L1> ; Is Bit0 Flaged ? Yes, do next line. Values = 1,5,9,13
                mov esi IMPORT_OBJECT_DATA
                jmp L2>

L1:         test eax &IMPORT_OBJECT_CONST | jz L1> ; Is Bit1 Flaged ? Yes, do next line. Values = 2,6,10,14
                mov esi IMPORT_OBJECT_CONST
                jmp L2>

L1:         ; we don´ have bit0 and bit1 flagged. Values = 0,4,8,12
                mov esi IMPORT_OBJECT_CODE

L2:
                While B$esi <> 0 | movsb | End_While
                mov W$edi '+(' | add edi 2


          ; Second Part of the Record of Bits 2,3 (Ordinal, Name, NoPrefix, Undecorated)
          ; We can have all of the 02 bits set, or none of them.

                shr eax 2


            cmp eax &IMPORT_OBJECT_NAME_UNDECORATE | jnz L1> ; Is Bit0 and 1 Flaged ? Yes, do next line
                                                            ; Values = 12, 13, 14
                mov esi IMPORT_OBJECT_NAME_UNDECORATE
                mov B$SymNameType &IMPORT_OBJECT_NAME_UNDECORATE ; Save for later skip leading chars
                jmp L2>


L1:         test eax &IMPORT_OBJECT_NAME | jz L1> ; Is Bit0 Flaged ? Yes, do next line. Values = 4,5,6
                mov esi IMPORT_OBJECT_NAME
                jmp L2>

L1:         test eax &IMPORT_OBJECT_NAME_NO_PREFIX | jz L1> ; Is Bit1 Flaged ? Yes, do next line. Values = 8,9,10
                mov esi IMPORT_OBJECT_NAME_NO_PREFIX
                mov B$SymNameType &IMPORT_OBJECT_NAME_NO_PREFIX ; Save for later skip leading chars
                jmp L2>

L1:         ; we don't have bit0 and bit1 flagged. Values = 0,1,2
                mov esi IMPORT_OBJECT_ORDINAL

L2:
                While B$esi <> 0 | movsb | End_While
                mov D$edi ' shl', D$edi+4 ' 2)' | add edi 7

EndP


____________________________________________________________________________________________

Proc WriteImportObjHdrTimeDateStamp:
    Arguments @TimeDate
    Uses esi

        sub edi 2 ; We need to subtract 02 Bytes, to bypass the previous CRLF
        mov esi {' ; - TimeDate Stamp: ', 0}
        While B$esi <> 0 | movsb | End_While

        ; Get the DateTime Stamp

        ; Time date stamp to string Function
        call TimeDateStampToString D@TimeDate {"yyyy/MM/dd ddd ", 0} {"HH:mm:ss UTC", 0} szDateString szTimeString

        ; Write the TimeDate Stamp strings
        mov esi szDateString
        While B$esi <> 0 | movsb | End_While

        mov esi szTimeString
        While B$esi <> 0 | movsb | End_While

        mov W$edi CRLF | add edi 2 ; We now Add the paragraphs marks, and add 02 Bytes at edi

EndP
____________________________________________________________________________________________

[ImpObjDateStamp: ?]

WriteLibImportObj:

    push esi
        mov esi CoffListingTitle
        While B$esi <> 0 | movsb | End_While
        call WriteIndiceOnly
        mov B$edi ':', W$edi+1 CRLF | add edi 3
    pop esi

    ; To be used in AddListviewItem
    push esi
        mov D$LvOffsetCOFF 0 ; always initialize at 0 1st, due to the several loopings for each object.
        sub esi D$LibFileMemory
        mov D$LvOffsetCOFF esi
    pop esi

    push esi
;        mov B$edi '[' | inc edi

        call WriteLibImportObjItem ImpObjHdrSig1Text, &FALSE | add esi 2

        call WriteLibImportObjItem ImpObjHdrSig2Text, &FALSE | add esi 2

        movzx eax W$esi | add esi 2
        call WriteLibImportObjItem ImpObjHdrVersionText, &TRUE

        ; To be used in AddListviewItem
        mov W$CoffMachineType 0 ; always initialize at 0 1st, due to the several loopings for each object.
        move W$CoffMachineType W$esi

        movzx eax W$esi | add esi 2
        call WriteLibImportObjItem ImpObjHdrMachineText, &TRUE

        lodsd
        mov D$ImpObjDateStamp eax
        call WriteLibImportObjItem ImpObjHdrTimeDateStampText, &TRUE
        call WriteImportObjHdrTimeDateStamp D$ImpObjDateStamp

        lodsd
        If D$esi-4 <> 0
            call WriteLibImportObjItem ImpObjHdrSizeOfDataText, &TRUE
        Else

            push esi
                call WriteObjIndice
                zCopy ImpObjHdrSizeOfDataText
            pop esi
            call WriteEax
            mov W$edi CRLF | add edi 2
        End_If

        movzx eax W$esi | add esi 2
        call WriteLibImportObjItem ImpObjHdrOrdinalText, &TRUE

        movzx eax W$esi | add esi 2
        call WriteLibImportObjItem ImpObjHdrType, &TRUE

        mov B$edi-2 ']' | dec edi | mov D$edi CRLF2 | add edi 4
    pop esi
ret


[ImportStringsText: B$ "ImportStrings: B$ '", 0
 RosAsmInterpretation: "; RosAsm Interpretation: '", 0
 RosAsmUndecoratedName: "; Full Undecorated Name: ", 0]

;[UndecNameBuffer: D$ 0 #4000]
;[Size_Of_UndecNameBuffer 4000]

Proc WriteImportObjStrings:
    Uses ecx, esi

    mov D$edi '[' | inc edi
    mov D$edi 'Obj',
        eax D$LibObjIndice, D$edi+3 eax,
        ax W$LibObjIndice+4, W$edi+7 ax,
        B$edi+9 '.' | add edi 10
    push esi
        mov esi ImportStringsText | While B$esi <> 0 | movsb | End_While
    pop esi

    push esi
        ; to be added in AddListviewItem
        push esi
        push edi
            ; Always clear the loaded data before use.
            call ClearBuffer ExportedLibraryStr, 256
            mov D$ExportedLibrary 1
            mov edi ExportedLibraryStr
            While B$esi <> 0 | movsb | End_While
        pop edi
        pop esi


        ; Copy the Function Name:
        While B$esi <> 0 | movsb | End_While
        mov D$edi "', 0", D$edi+4 ", '" | add edi 7
        While B$esi = 0 | inc esi | End_While

        ; Copy the Module Name:
        push esi
            While B$esi <> 0 | movsb | End_While
            mov D$edi "', 0" | add edi 4
            mov W$edi CRLF | add edi 2
            push esi
                call WriteObjIndice
                zCopy {'ImportStringsEnd: ]', 0}
            pop esi
            mov D$edi CRLF2 | add edi 4

            mov esi RosAsmInterpretation | While B$esi <> 0 | movsb | End_While
        pop esi

        ; Copy the Commented Module Name:
        While B$esi <> 0 | movsb | End_While
        mov eax D$edi-4 | or eax 020202000
        If eax = '.dll'
            sub edi 4
        End_If

        mov B$edi '.' | inc edi
    pop esi

    mov ecx esi ; Copy esi String to be used for name undecoration

  ; Copy the Commented Function Name:

    ; Remove 1st '_'; '@'; '?'
    .If B$SymNameType = &IMPORT_OBJECT_NAME_NO_PREFIX

        On B$esi = '_', inc esi
        On B$esi = '@', inc esi
        On B$esi = '?', inc esi

       While B$esi <> 0 | movsb | End_While

    ; Remove 1st '_'; '@'; '?' and truncate after next '@'.
    ; Ex:   _AbortPath@4 to AbortPath
    ;       ?WSCWriteProviderOrder@@YGHPAKK@Z to WSCWriteProviderOrder WS2_32.lib
    ;       ___CxxLongjmpUnwind@4 to __CxxLongjmpUnwind (MSVCRT.LIB)

    .Else_If B$SymNameType = &IMPORT_OBJECT_NAME_UNDECORATE

        On B$esi = '_', inc esi
        On B$esi = '@', inc esi
        On B$esi = '?', inc esi
        ; Ok, removed the above chars. No we need to truncate after findng the @
        While B$esi <> '@'

            On B$esi = 0, jmp L0> ; Sometimes we have No @ char here (Pelles\advapi32.lib)
                                  ; [Obj000397.ImportStrings: B$ '_SetServiceBits', 0, 'ADVAPI32.dll', 0]
            movsb

        End_While

        L0:

    .Else
        ; For all other cases, keep the lib name intact
       While B$esi <> 0 | movsb | End_While

    .End_if

    mov B$edi "'" | inc edi

    mov D$edi CRLF2 | add edi 4

    ; Write the full Undecorated name. Ecx ecx points to {'?wndTopMost@CWnd@@2V1@B' , 0}

    push esi
        mov esi RosAsmUndecoratedName | While B$esi <> 0 | movsb | End_While
    pop esi

    push ecx
    call 'IMAGEHLP.UnDecorateSymbolName' ecx, Trash, 4000, &UNDNAME_COMPLETE
    pop ecx

    call StrCmp ecx Trash

    ; If the strings are the same it means that the above function failed, because the undecoretated name is not the same
    ; as the original loaded name.
    If eax = 0
        call Simple_UndecorateSymbolName ecx, Trash
    End_If

    push esi
        mov esi Trash | While B$esi <> 0 | movsb | End_While
    pop esi


    mov D$edi CRLF2 | add edi 4
EndP
___________________________________________________________________________
;;

Bad Values

[Obj000004.ImportStrings: B$ '?ClearFontManager@@YAXXZ', 0, 'AYGSHELL.dll', 0
Obj000004.ImportStringsEnd: ]

; RosAsm Interpretation: 'AYGSHELL.?ClearFontManager@@YAXXZ'

; Full Undecorated Name: void __cdecl ClearFontManager(void)


Good values


; RosAsm Interpretation: 'ACLUI.EditSecurity'

; Full Undecorated Name: _EditSecurity@8
;;

Proc Simple_UndecorateSymbolName:
    Arguments @Input, @Output
    Local @ParamCount

    pushad

    mov D@ParamCount 0
    mov edi D@Input

    While B$edi <> 0
        On B$edi = '?', jmp L4>> ; On this simplified versin we cannot have this char
        On B$edi = '$', jmp L4>> ; On this simplified versin we cannot have this char
        On B$edi = '@', jmp L1> ; This is possible to have, but a good symbols can only have this once.
        inc edi
    End_While

    jmp L2>
L1:
   inc edi

    While B$edi <> 0
        On B$edi = '@', jmp L4>> ; If we found this value again, we have a bad symbol
        If_And B$edi < '0', B$edi > '9' ; the values after the '@' char must be numbers from 0 to 9. Otherwise it is a bad symbol
            jmp L4>
        End_If
        inc edi
    End_While

    ; If we reach here we have a Good Symbol.
L2:

    mov esi D@Input
    mov edi D@Output

    If B$esi = '_'
        inc esi
    End_If

    If B$esi = 0
        push esi | ZCopy {"Invalid ! Null Symbol Name.", 0} | pop esi
        jmp L4>
    End_If

    .While B$esi <> 0

        .If B$esi = '@' ; We found our delimiter for the Parameters amount.
            inc esi ; Bypass the '@' char.
            call DecimalStringToDword esi
            shr eax 2 ; divide the result by 4
            push esi | ZCopy {" - Amount of Parameters: ", 0} | pop esi

            ; Convert Dword to Decimal String
            push edi
                mov D@ParamCount eax
                lea esi D@ParamCount
                mov ecx 4
                call toUDword
                mov esi edi
            pop edi

            Do | movsb | LoopUntil B$esi-1 = 0
            dec edi
            jmp L3>
        .End_If

        movsb
    .End_While

L3:
    mov B$edi 0

   L4:

    popad

EndP

___________________________________________________________________________
___________________________________________________________________________

; based on GetMemberSize

Proc DecimalStringToDword:
    Arguments @String
    Uses, esi, ebx, ecx, edx

    mov esi D@String
    mov ecx 10, ebx 0, eax 0

    .While B$esi <> 0
        on B$esi = ' ', ExitP
        mul ecx
        push eax
            lodsb | sub al '0' | mov bl al
        pop eax
        add eax ebx
    .End_While

EndP
___________________________________________________________________________
___________________________________________________________________________

Proc WriteImportObjStringAndOrdinal:
    Argument @Ordinal

        mov D$edi '[' | inc edi
        mov D$edi 'Obj',
            eax D$LibObjIndice, D$edi+3 eax,
            ax W$LibObjIndice+4, W$edi+7 ax,
            B$edi+9 '.' | add edi 10
        push esi
            mov esi ImportStringsText | While B$esi <> 0 | movsb | End_While
        pop esi


        ; to be added in AddListviewItem
        push esi
        push edi

            ; Always clear the loaded data before use.
            call ClearBuffer ExportedLibraryStr, 256
            mov D$ExportedLibrary 1
            mov edi ExportedLibraryStr
            While B$esi <> 0 | movsb | End_While
        pop edi
        pop esi


        ; Copy the Module Name:
        push esi
            While B$esi <> 0 | movsb | End_While
            mov D$edi "', 0" | add edi 4
            mov W$edi CRLF | add edi 2
            push esi
                call WriteObjIndice
                zCopy {'ImportStringsEnd: ]', 0}
            pop esi
            mov D$edi CRLF2 | add edi 4

            mov esi RosAsmInterpretation | While B$esi <> 0 | movsb | End_While

        pop esi

      ; Copy the Commented Module Name:
        While B$esi <> 0 | movsb | End_While
        mov eax D$edi-4 | or eax 020202000
        If eax = '.dll'
            sub edi 4
        End_If

        mov B$edi '.' | inc edi
    pop esi
  ; Copy the Commented Function Ordinal:
    mov eax D@Ordinal | call WriteEax

    mov D$edi CRLF2 | add edi 4
EndP



[LookUpValidNameCharsTable: B$ ? #0100]

InitLookUpValidNameCharsTable:
    mov edi LookUpValidNameCharsTable, eax 0, ecx 0100

  ; Build a normal Asicii Table:
L0: mov B$edi+eax al | inc eax | loop L0<
ret

  ; Replace everything above 127 by '_'
    mov ecx 080, ebx 080, al '_'
L0: mov B$edi+ebx al | inc ebx | loop L0<

  ; Replace Ascii 127 by '_'
    mov B$edi+07F '_'

    mov eax '\' | mov B$edi+eax '.'

    mov eax '?' | mov B$edi+eax '.'

    mov eax '_' | mov B$edi+eax '.'

    mov eax '$' | mov B$edi+eax '.'

    mov eax '|' | mov B$edi+eax '.'
ret
;;
  ; Other possibility:
  
    mov al '_', ecx 0100, edi LookUpValidNameCharsTable | rep stosb
    
    mov edi LookUpValidNameCharsTable, eax '0'
    While eax <= '9'
        mov B$edi+eax al | inc al
    End_While
    
    mov eax 'A'
    While eax <= 'Z'
        mov B$edi+eax al | inc al
    End_While
    
    mov eax 'a'
    While eax <= 'z'
        mov B$edi+eax al | inc al
    End_While
ret
;;

[LookUpValidNameChars | lodsb | and eax 0FF | mov al B$LookUpValidNameCharsTable+eax | stosb]

[IMAGE_ARCHIVE_MEMBER_SIZE 030]

GetLongNamesPointer:  ; GetMemberSize
    ...If D$LongNamesBase = 0
        pushad
          ;  mov esi D$LibFileMemory | add esi 8

L0:         If W$esi = '//'
                add esi 03C
                mov D$LongNamesBase esi

            Else_If B$esi = '/'
                push D$MemberSize
                    add esi IMAGE_ARCHIVE_MEMBER_SIZE
                    call GetMemberSize
                    sub esi IMAGE_ARCHIVE_MEMBER_SIZE
                    add esi 03C | add esi D$MemberSize
                pop D$MemberSize | jmp L0<

            End_If
        popad
    ...End_If
ret


CheckLongNameDisplacement:
    push esi
        While B$esi <> ' '
            lodsb
            If al < '0'
                mov eax &FALSE | jmp L9>
            Else_If al > '9'
                mov eax &FALSE | jmp L9>
            End_If
        End_While
        mov eax &TRUE
L9: pop esi
ret


Proc WriteLongName:  ; GetMemberSize
    Argument @Dis
    Uses ebx, ecx, edx

    push esi
        mov esi {'Long Name: ', 0};IMAGE_ARCHIVE_MEMBER_HEADERstring
        While B$esi <> 0 | movsb | End_While
    pop esi

        mov esi D@Dis, ecx 10, ebx 0, eax 0

        While B$esi > ' '
            mul ecx
            push eax
                lodsb | sub al '0' | mov bl al
            pop eax
            add eax ebx
        End_While

        mov esi D$LongNamesBase | add esi eax

        While B$esi >= ' ' | movsb | End_While
        On B$edi-1 = '/', dec edi
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[LongNamesBase: ?]

GetLongNamesBase:
    mov D$LongNamesBase 0
   ____________________________________________
  ; Lib Tag:
    mov esi D$LibFileMemory | add esi 8

    If W$esi = '/ '
        push esi
            add esi LIB_MEMBER_SIZE | call GetMemberSize
        pop esi
        add esi COFF_HEADER_SIZE
        add esi D$MemberSize | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Second optional Lib Header:
    If W$esi = '/ '
        push esi
            add esi LIB_MEMBER_SIZE | call GetMemberSize
        pop esi
        add esi COFF_HEADER_SIZE
        add esi D$MemberSize | On B$esi = 0A, inc esi
    End_If
   ____________________________________________
  ; Third optional Lib Header:
    If W$esi = '//'
        add esi COFF_HEADER_SIZE
        mov D$LongNamesBase esi
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
                      Write the Tag and first Library Header
;;
____________________________________________________________________________________________

[WriteNotice: ";;

RosAsm Development Team presents:
________________________________________________

______________ Lib Scanner v 1.0  ______________
__________________  Set/05  ____________________
________________________________________________

This file was generated by RosAsm Library File Dumper,
aka known as Lib Scanner v 1.0.

http://www.rosasm.org
http://rosasm.org
http://betov.free.fr

If you have any questions about this file, read RosAsm's
Help file (B_U_Asm.exe), or feel free to contact us at:

http://www.quanta-it.com/easbell/RosAsmForum/index.php

_______________________

Author(s):

René Tournois (Betov)
Gustavo Trigueiros (Beyond2000!)

;;
________________________________________________


", 0]


[LibTag: ";;

________________________________________________

    Tag Data. The Magic String
________________________________________________
________________________________________________
;;

[Magic: B$   '!<arch>', 0A]

;;
________________________________________________

    First Linker Header - Names Linker Member
________________________________________________
________________________________________________
;;

; This is the IMAGE_ARCHIVE_MEMBER_HEADER Structure.

", 0]
____________________________________________________________________________________________

WriteLibTag:
  ; (Includes the First Headers comments at once):
    mov esi LibTag | While B$esi <> 0 | movsb | End_While

    mov esi D$LibFileMemory | add esi 8
ret
____________________________________________________________________________________________

[AmountOfMembers: "; Amount of Members on the file.

[PublicSymbols1: D$ ", 0]

[NumberOfSymbols: ?]

ShowAmountOfSymbols1:
;;
  Show the Amount of Symbols (a dWord stored the other way round):
;;
    push esi
        mov esi AmountOfMembers | While B$esi <> 0 | movsb | End_While
    pop esi
    lodsd | bswap eax | mov D$NumberOfSymbols eax | call WriteEax

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[AmountOfOffsets1: "; Amount of Offsets of the Public Symbols.

[SymOffset:
 ", 0]

ShowOffsetTable1: ; 'ShowOffsetTable2'
;;
  Show the Offset Table (where each dWord is a Displacement from Top of File,
  to the IMAGE_ARCHIVE_MEMBER_HEADER of the .Obj File concerned with the
  matching Name found in the next "Symbols Table" String):
;;
    push esi
        mov esi AmountOfOffsets1 | While B$esi <> 0 | movsb | End_While
    pop esi

    mov ecx D$NumberOfSymbols

L0: lodsd
    push esi
    ; Eax: Displacement to the SymOffset array Data. If it is 0, jmp over.
        ..If eax = 0
            mov esi {'&NULL ; Null Pointer. Does not point to any Object in this file.', 0}

        ..Else
            mov esi D$LibFileMemory | bswap eax | add esi eax

            .If B$esi = '/'
L1:             push ecx
                    call GetLongNamesPointer
                    call CopyCoffIndice | inc esi
                    If D$LongNamesBase <> 0
                       ;                     mov esi D$LongNamesBase
                        call CheckLongNameDisplacement | On eax = &FALSE, jmp L1>
                        call WriteLongName esi
                    Else
L1:                     call CopySymbolName
                    End_If
                pop ecx

            .Else
                call CopyCoffIndice | call CopySymbolName

            .End_If

            mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring

        ..End_If

        While B$esi <> 0 | movsb | End_While
    pop esi
    mov W$edi CRLF, B$edi+2 ' ' | add edi 3 | dec ecx | jnz L0<<
ret


CopySymbolName:
  ; Copy the ARCHIVE_MEMBER_HEADER Name:
  push ecx
  mov ecx 1 ; It is used as a counter. Starting with 1 (The 1st byte)


    push esi
        mov esi {'Short Name: ', 0}
        While B$esi <> 0 | movsb | End_While
    pop esi

    push esi

    While B$esi > ' '
        movsb
            ; Short Names always ends with an "/ ". Or sometimes with an "/"
            If W$esi+1 = '/ '
                movsW
                jmp L2>
            Else_If ecx = 16; Did we reached the 16th byte ? ON that case we may still have "/" as the last char
                jmp L2>
            End_If
        inc ecx
    End_While


L2:     On B$edi-1 = '/', dec edi ; Clean Up the last "/" char

    pop esi

    ; We need to retrieve the Function Name in case for the Import Header (The non common Coff format)
    ; To avoid inserting the ".directive" strings etc. We need only the real function name for this type
    ; of structure.

    ..If W$esi+58+2 = &IMAGE_FILE_MACHINE_UNKNOWN
        .If W$esi+58+2+2 = &IMPORT_OBJECT_HDR_SIG2

            push esi
                mov esi {' Function Name: ', 0}
                While B$esi <> 0 | movsb | End_While
            pop esi

            lea esi D$esi+60+20
            On B$esi = '/', inc esi
            While B$esi > '/' | LookUpValidNameChars | End_While

        .End_If
    ..End_If

   ; If this is a common Coff Format, we compute the Function name it follows it here
    pop ecx
ret


CopyObjSymbolName:
  ; Copy the ARCHIVE_MEMBER_HEADER Name:
  push ecx
  mov ecx 1 ; It is used as a counter. Starting with 1 (The 1st byte)

    push esi
        mov esi {'Short Name: ', 0}
        While B$esi <> 0 | movsb | End_While
    pop esi

    push esi

    While B$esi > ' '
        movsb
            ; Short Names always ends with an "/ ". Or sometimes with an "/"
            If W$esi+1 = '/ '
                movsW
                jmp L2>
            Else_If ecx = 16; Did we reached the 16th byte ? ON that case we may still have "/" as the last char
                jmp L2>
            End_If
        inc ecx
    End_While


L2:     On B$edi-1 = '/', dec edi ; Clean Up the last "/" char

    pop esi
    mov D$ExportedLibrary 0
    ; We need to retrieve the Function Name in case for the Import Header (The non common Coff format)
    ; To avoid inserting the ".directive" strings etc. We need only the real function name for this type
    ; of structure.

    ..If W$esi+58+2 = &IMAGE_FILE_MACHINE_UNKNOWN
        .If W$esi+58+2+2 = &IMPORT_OBJECT_HDR_SIG2

        mov D$ExportedLibrary edi

        push esi
            mov esi {' Function Name: ', 0}
            While B$esi <> 0 | movsb | End_While
        pop esi

        lea esi D$esi+60+20
        While B$esi >= ' ' | LookUpValidNameChars | End_While

        .End_If

    ..End_If

   ; If this is a common Coff Format, we compute the Function Name it follows it here
    pop ecx

ret
____________________________________________________________________________________________

[StringsArrayComments1: "]

; Array of null terminated strings that are the name of
;the Symbols List (The value of the array is defined by PublicSymbols member).

[Symname1:
 ", 0]

[Symname1.Data: B$   "Symname1.Data"
 SymName1Counter: "000001: B$   '", 0]

ShowSymbolsTable1:
;;
  Show the Symbols Table, that is a simple zero-ended string array. Each String
  is a Symbol used, in order, by the .Obj File, which the IMAGE_ARCHIVE_MEMBER_HEADER
  is pointed to, by the above Offset Table:
;;
    push esi
        mov esi StringsArrayComments1
        While B$esi <> 0 | movsb | End_While
    pop esi

    mov D$SymName1Counter '0000', W$SymName1Counter+4 '01', ecx D$NumberOfSymbols

L0: push esi
        mov esi Symname1.Data
        While B$esi <> 0 | movsb | End_While
    pop esi
    While B$esi <> 0 | LookUpValidNameChars | End_While | inc esi
    mov D$edi "', 0", W$edi+4 CRLF, B$edi+6 ' ' | add edi 7
    lea ebx D$SymName1Counter+5
    inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx
        inc B$ebx
    End_While
    loop L0<
    sub edi 3
    mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

WriteHeaderMember1:
    call ShowAmountOfSymbols1
    If D$NumberOfSymbols > 0
        call ShowOffsetTable1
        call ShowSymbolsTable1
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
                              Write the Second Library Header
;;
____________________________________________________________________________________________

[SecondLibHeaderComment: ";;
_____________________________________________________

    Secondary Linker Header - Names Linker Member
_____________________________________________________
_____________________________________________________
;;

; This is the IMAGE_ARCHIVE_MEMBER_HEADER Structure.

", 0]
____________________________________________________________________________________________

WriteSecondLibHeaderComment:
    push esi
        mov esi SecondLibHeaderComment
        While B$esi <> 0 | movsb | End_While
    pop esi
ret
____________________________________________________________________________________________

[AmountOfSymbols2: "; Total amount of Objects files.

[PublicSymbols2: D$ ", 0]

ShowAmountOfSymbols2:
;;
  Show the Amount of Symbols (a dWord stored the other way round):
;;
    push esi
        mov esi AmountOfSymbols2 | While B$esi <> 0 | movsb | End_While
    pop esi
    lodsd | mov D$NumberOfSymbols eax | call WriteEax

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[AmountOfOffsets2: "; Amount of Offsets of the Object Files Found.

[SymOffset2:
 ", 0]

ShowOffsetTable2:  ; 'ShowOffsetTable1'
;;
  Show the Offset Table (where each dWord is a Displacement from Top of File,
  to the IMAGE_ARCHIVE_MEMBER_HEADER of the .Obj File concerned with the
  matching Name found in the next "Symbols Table" String):
;;
    push esi
        mov esi AmountOfOffsets2 | While B$esi <> 0 | movsb | End_While
    pop esi

    mov ecx D$NumberOfSymbols

L0: lodsd
    push esi
  ; Eax: Displacement to the SymOffset array Data. If it is 0, jmp over.

        ..If eax = 0
            mov esi {'&NULL ; Null Pointer. Does not point to any Object in this file.', 0}

        ..Else
            mov esi D$LibFileMemory | add esi eax
            .If B$esi = '/'
                call GetLongNamesPointer
                call CopyCoffIndice | inc esi
                If D$LongNamesBase <> 0
                    call CheckLongNameDisplacement | On eax = &FALSE, jmp L1>
                    call WriteLongName esi
                Else
L1:                 call CopySymbolName
                End_If

            .Else
                call CopyCoffIndice | call CopySymbolName

            .End_If

            mov esi IMAGE_ARCHIVE_MEMBER_HEADERstring

        ..End_If

        While B$esi <> 0 | movsb | End_While

    pop esi
    mov W$edi CRLF, B$edi+2 ' ' | add edi 3 | dec ecx | jnz L0<< ;loop L0<
    mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[AmountOfSymbols3: "; Amount of Public Symbols on the file.

[PublicSymbols3: D$ ", 0]

ShowAmountOfSymbols2Bis:
;;
  Show the Amount of Members (a dWord stored the other way round), for Second Header:
;;
    push esi
        mov esi AmountOfSymbols3 | While B$esi <> 0 | movsb | End_While
    pop esi
    lodsd | mov D$NumberOfSymbols eax | call WriteEax

    mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

[IndexString2: "; Array of the Word data, representing the Index of the exported functions of the Symbols List.
; It points to the object files inside the lib.
; (The amount of the elements in the array is defined by the PublicSymbols2 member)

[SymIndex:
 ", 0]

[SymIndex2.Data: "SymIndex.Data"
 SymIndex2Counter: "000001:  W$    ", 0]


ShowIndexTable2:
;;
  Flow of reversed Words. They are indexes to point out what String goes with what Obj File:
;;
    push esi
        mov esi IndexString2 | While B$esi <> 0 | movsb | End_While
    pop esi

    mov ecx D$NumberOfSymbols, D$SymIndex2Counter '0000', W$SymIndex2Counter+4 '01'

L0: push esi
        mov esi SymIndex2.Data
        While B$esi <> 0 | movsb | End_While
    pop esi
  ; This one is not reversed:
    lodsw | and eax 0FFFF | call WriteEax



    lea ebx D$SymIndex2Counter+5
    inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx
        inc B$ebx
    End_While

    mov W$edi CRLF, B$edi+2 ' ' | add edi 3 | loop L0<

    sub edi 3
ret
____________________________________________________________________________________________

[StringsArrayComments2: "]

; Array of null terminated strings that are the name of
;the Symbols List (The value of the array is defined by PublicSymbols member).

[Symname2:
 ", 0]

[Symname2.Data: B$   "Symname2.Data"
 SymName2Counter: "000001: B$   '", 0]

ShowSymbolsTable2:
;;
  Show the Symbols Table, that is a simple zero-ended string array. Each String
  is a Symbol used, in order, by the .Obj File, which the IMAGE_ARCHIVE_MEMBER_HEADER
  is pointed to, by the above Offset Table:
;;
    push esi
        mov esi StringsArrayComments2
        While B$esi <> 0 | movsb | End_While
    pop esi

    mov D$SymName2Counter '0000', W$SymName2Counter+4 '01', ecx D$NumberOfSymbols

L0: push esi
        mov esi Symname2.Data
        While B$esi <> 0 | movsb | End_While
    pop esi
    While B$esi <> 0 | LookUpValidNameChars | End_While | inc esi
    mov D$edi "', 0", W$edi+4 CRLF, B$edi+6 ' ' | add edi 7
    lea ebx D$SymName2Counter+5
    inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx
        inc B$ebx
    End_While
    loop L0<
    sub edi 3
    mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret
____________________________________________________________________________________________

WriteHeaderMember2:
    call ShowAmountOfSymbols2
    On D$NumberOfSymbols > 0, call ShowOffsetTable2
    call ShowAmountOfSymbols2Bis
    If D$NumberOfSymbols > 0
        call ShowIndexTable2
        call ShowSymbolsTable2
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
   Write the third Library Header
;;
____________________________________________________________________________________________


[ThirdLibHeaderComment: ";;
_____________________________________________________

    Tertiary Linker Header - Names Linker Member
_____________________________________________________
_____________________________________________________
;;

; This is the IMAGE_ARCHIVE_MEMBER_HEADER Structure.

", 0]
____________________________________________________________________________________________

WriteThirdLibHeaderComment:
    push esi
        mov esi ThirdLibHeaderComment
        While B$esi <> 0 | movsb | End_While
    pop esi
ret
____________________________________________________________________________________________

[LongNamesTableComment3:
"; Amount of Null Terminated String related to the Objects with LongNames.

[", 0]

[LongObjectNames: "LongObjectNames.data"

LongObjectNamesCounter: "000001: B$ '", 0]
____________________________________________________________________________________________

WriteLongNameTableComment3:
    mov D$LongObjectNamesCounter '0000', W$LongObjectNamesCounter+4 '01'

    push esi
        mov esi LongNamesTableComment3
        While B$esi <> 0 | movsb | End_While
    pop esi
ret

ShowLongNamesTable3:
    mov edx esi | add edx D$MemberSize

    ..While esi < edx
        push esi
            mov esi LongObjectNames
            While B$esi <> 0 | movsb | End_While

            lea ebx D$LongObjectNamesCounter+5
            inc B$ebx
            While B$ebx > '9'
                mov B$ebx '0' | dec ebx | inc B$ebx
            End_While
        pop esi

        While B$esi >= ' ' | LookUpValidNameChars | End_While | inc esi
        While B$esi < ' ' | inc esi | End_While
        mov D$edi "', 0", W$edi+4 CRLF | add edi 6
    ..End_While

    sub edi 2 | mov B$edi ']', D$edi+1 CRLF2 | add edi 5
ret


WriteHeaderMember3:
    If D$MemberSize > 0
        call WriteLongNameTableComment3
        call ShowLongNamesTable3
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

; Coff

[CoffComment: ";;
_____________________________________________________

Coff File Headers:
_____________________________________________________
_____________________________________________________
;;

", 0]

____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________
____________________________________________________________________________________________


;;
                                      Lib jobs
;;

[LibSymbolsMap: ?]

[SymbolsNumber: ?    CoffIndice: ?]

GetLibCode:
    mov D$SymbolsNumber 0, D$LibNumberOfBytes 0
    call GetLibSymbolsNumber | On D$SymbolsNumber = 0, jmp L9>>

    call CreateLibBuffers
    move D$LibDisassemblyPtr D$LibDisassembly, D$LibBytesCopyPtr D$LibBytesCopy
    mov D$CoffIndice 0

L0: call GetCoffBase D$CoffIndice
    .If W$esi <> 0-1
        If W$esi+2 <> 0-1
            call ScanCoff
        End_If
    .End_If
    inc D$CoffIndice | mov eax D$CoffIndice | cmp eax D$SymbolsNumber | jb L0<<

    call DecodeLib
L9: ret


GetLibSymbolsNumber:
    mov esi D$LibFileMemory

  ; Number of SYMBOLs in ecx:
    mov eax D$esi+COFF_HEADER_SIZE+ARCH_TAG | bswap eax
    mov D$SymbolsNumber eax
ret

; esi point to Coff Base (014C):

[CoffHeaderBase: ?]

[LibNumberOfBytes: ?]

ScanCoff:
    ..If W$esi <> 0-1 ; 'PeHeader'
        mov W$esi 0-1
        mov D$CoffHeaderBase esi
      ; Number of Coff Sections in ecx:
        movzx ecx W$esi+2
        add esi 014 ; COFF_HEADER_SIZE ; example: '.text'

L0:     and D$esi+SECTION_FLAG &IMAGE_SCN_CNT_CODE

        .If D$esi+SECTION_FLAG = &IMAGE_SCN_CNT_CODE
            push ecx, esi
                mov ecx D$esi+SECTION_FILESIZE, esi D$esi+SECTION_FILEPOINTER

                If ecx <> 0
                    add esi D$CoffHeaderBase
                    mov edi D$LibBytesCopyPtr
                    mov D$edi ecx | add edi 4
                    add D$LibNumberOfBytes ecx
                    rep movsb
                    mov D$LibBytesCopyPtr edi
                End_If
            pop esi, ecx
        .End_If

        add esi SECTIONHEADERSIZE | loop L0<
    ..End_If
ret

;;
B$ '.text',0,0,0
AppTrueCodeSize: D$   0     ; true size of code in file
AppCodeRVAoffset: D$   0    ; RVA offset (aligned on 01000 boundary)
AppFileSizeOfCode: D$   0   ; file aligned size of code (0200 aligned)
AppStartOfCode: D$   00     ; pointer to code (true first code in file - not entry point-)
D$   00                     ; dummy reloc ptr
D$   00                     ; dummy line number ptr
W$   00                     ; dummy reloc number
W$   00                     ; dummy number of line number
CodeCharacteristics:
D$   0_60000020             ; characteristics (readable, runable, code)
;;

[LibBytesCopy: ?    LibBytesCopyPtr: ?    LibDisassembly: ?    LibDisassemblyPtr: ?]

CreateLibBuffers:
    VirtualAlloc LibBytesCopy, D$LibFileLength
  ; VirtualAlloc LibSymbolsMap, D$LibFileLength
    mov ecx D$LibFileLength | shl ecx 5 | VirtualAlloc LibDisassembly ecx
    move D$LibDisassemblyPtr D$LibDisassembly
    add D$LibDisassembly 4
ret


Proc GetCoffBase:
    Argument @Indice

        mov esi D$LibFileMemory, eax D@Indice
        mov esi D$esi+eax*4+COFF_HEADER_SIZE+ARCH_TAG+4
        bswap esi | add esi D$LibFileMemory

      ; esi now points to a COFF Object:
        add esi COFF_HEADER_SIZE

      ; esi now point, typically, to 014C
EndP



Proc SetStringPointer:
    Arguments @Base, @Offset, @String
    Uses edi
      ; Write the Pointer to String directly on the Code "0, 0, 0, 0":
        mov edi D@Base
        add edi D$esi+SECTION_FILEPOINTER
        add edi D@Offset
        move D$edi D@String
EndP

____________________________________________________________________________________________



__________________________________________________________________________________________________________________
__________________________________________________________________________________________________________________

;;

----------------------------------------------------------------------
        Data used to build the Main dialog and Tab controls.
----------------------------------------------------------------------
----------------------------------------------------------------------

;;

__________________________________________________________________________________________________________________

;;
Proc WriteSectionHeaderSymbolConstantIndex:
    uses eax, ecx, ebx, esi

    call InitSymbolIndexRecord
    call WriteObjIndice

    ; Fix the label to show in Caps and replace the '." with an "_"
    ; The 1st char in "Obj000000." is in Caps, so we don't need to overwrite the "O" char

    push edi
    mov B$edi-1 '_', W$edi-9 'BJ'
    pop edi

    push esi
    zCopy {'SYMBOLINDEX', 0}
    pop esi

    mov ecx D$esi

    ; Note to René: Replacing this with a hex to decimal ascii string is better,
    ; but i couldn't make it be on the same style as SymbolTableIndex

    While ecx <> 0
        call IncrementSymbolIndexRecord
        dec ecx
    End_While

    zCopy SymbolTableIndex

    ; restore the SymbolTableIndex
    call InitSymbolIndexRecord

EndP
;;


; LV_ITEM Structure

[buffer2: B$ 0 #&MAXPATH]

[lvi:
 lvi.imask: D$ &LVIF_TEXT
 lvi.iItem: D$ 0
 lvi.iSubItem: D$ 0
 lvi.state: D$ &LVIS_FOCUSED
 lvi.stateMask: D$ 0
 lvi.pszText: D$ buffer2
 lvi.cchTextMax: D$ &MAXPATH
 lvi.iImage: D$ 0
 lvi.lParam: D$ 0
 lvi.iIndent: D$ 0]

;&LVCF_ORDER

[LvOffsetCOFF: D$ 0]
;GetCoffIMAGE_FILE_HEADER

[DecimalBuffer: B$ ? #16];B$ 020 #10 0]
[HexaDecimalBuffer: B$ ? #16]

[ObjSymbolsNumber: D$ 0]

[CoffMachineType: D$ 0]

[ObjFileNameType: D$ 0]


; The decimal and hexadecimal convertion routines comes from MouseHintDrawWindow

Proc AddListviewItem:
    Arguments @h2List
    pushad


    ; Calculate and Display Index Value
    mov edi buffer2
    mov esi LibObjIndice
    While B$esi <> 0 | movsb | End_While

    sub edi 6
    mov D$lvi.pszText edi
    mov D$lvi.iSubItem, 0
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTITEM, 0, lvi

    ; Calculate and Display FileName

    move D$lvi.pszText ObjectName
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Calculate and Display Extension

    If D$UseObjExtension = 0
        mov D$lvi.pszText {"No extension", 0}
    Else
        move D$lvi.pszText ObjExtensionStr
    End_If
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ; Display File Name Type
    ; Definition of values in Write_Obj_IMAGE_ARCHIVE_MEMBER_HEADER

    If D$ObjFileNameType = 1
        mov D$lvi.pszText {"Long Name", 0}
    Else
        mov D$lvi.pszText {"Short Name", 0}
    End_If
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ; Display the Path of the Object file
    If D$UseObjPath = 0
        mov D$lvi.pszText {"Not used", 0}
    Else
        mov D$lvi.pszText ObjPathStr
    End_If

    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

   ; Calculate and Display Offset (Convert hexa dword to string)

    mov edi HexaDecimalBuffer
    DwordToHex D$LvOffsetCOFF
    mov B$edi 0

    move D$lvi.pszText HexaDecimalBuffer
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Calculate and Display Size (Convert Hexa do Decimal String)

    mov esi MemberSize, ecx 4
    call toUDword
    mov esi edi, edi DecimalBuffer
    Do | movsb | LoopUntil B$esi-1 = 0

    move D$lvi.pszText DecimalBuffer
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Display Object Type
    If D$ExportedLibrary = 0
        mov D$lvi.pszText {"Runtime Object", 0}
    Else
        mov D$lvi.pszText {"Exported Object", 0}
    End_If

    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Display Function Name used in Exported Library
    If D$ExportedLibrary = 0
        mov D$lvi.pszText {"Not used", 0}
    Else
        move D$lvi.pszText ExportedLibraryStr
    End_If

    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ; Display the Machine Type

    mov D$buffer2 edi ; eax holds the address of the initial Equate String

    call Write_IMAGE_FILE_MACHINE D$CoffMachineType
    mov B$edi 0 ; Fix the end of edi in all cases. Force Null Terminated String
    mov eax D$buffer2

    If B$eax = '&'
        inc D$buffer2
        move D$lvi.pszText D$buffer2
    Else
        mov D$lvi.pszText {"Unknown or Corrupted Machine Type", 0}
    End_If
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ;  Calculate and Display Number of Sections (Convert Hexa do Decimal String)

    .If D$ExportedLibrary = 0
        mov esi ObjNumberOfSections, ecx 4
        call toUDword
        mov esi edi, edi DecimalBuffer
        Do | movsb | LoopUntil B$esi-1 = 0

        mov D$lvi.pszText DecimalBuffer
    .Else
        mov D$lvi.pszText {"No Sections", 0}
    .End_If

    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Display Time and Date Stamp
    ; This came from WriteImportObjHdrTimeDateStamp
    ;TimeDateStringtoDword
    call Concatenation szDateString, szTimeString, buffer2
    move D$lvi.pszText buffer2
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi


    ;  Calculate and Display Number of Symbols (Convert Hexa do Decimal String) CoffSymbolsNumber
    .If D$ExportedLibrary = 0
        mov esi ObjSymbolsNumber, ecx 4
        call toUDword
        mov esi edi, edi DecimalBuffer
        Do | movsb | Loop_Until B$esi-1 = 0

        mov D$lvi.pszText DecimalBuffer
    .Else
        mov D$lvi.pszText {"No Symbols", 0}
    .End_If

    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Show Optional Header Options (False if it don't use Optional Header, True otherwise)

    If D$SizeOfOptionalHeaderInObj = 0
        mov D$lvi.pszText {"False", 0}
    Else
        mov D$lvi.pszText {"True", 0}
    End_If
    inc D$lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, lvi

    ; Clear the Buffer before exit

    mov ecx &MAXPATH
    mov edi 0
    L0:
        mov B$buffer2+edi 0
        inc edi
    Loop L0<

    popad

EndP


___________________________________________

; Used Macros


; Amount of Columns of the ListView
[Lib_LVTotalCol 14]

; String Data

[Header1: B$ 'Index',0]
[Header2: B$ 'File Name',0]
[Header3: B$ 'Extension',0]
[Header4: B$ 'FileName Type',0]
[Header5: B$ 'Path',0]
[Header6: B$ 'Offset',0]
[Header7: B$ 'Size',0]
[Header8: B$ 'Object Type',0]
[Header9: B$ 'Exported Function',0]
[Header10: B$ 'Machine Type',0]
[Header11: B$ 'Sections',0]
[Header12: B$ 'Time and Date',0]
[Header13: B$ 'Symbols',0]
[Header14: B$ 'Optional Header',0]

; Our used Constants to identify what field is what
[LVIEW_INDEX 0]
[LVIEW_FILENAME 01]
[LVIEW_EXTENSION 02]
[LVIEW_NAMETYPE 03]
[LVIEW_PATH 04]
[LVIEW_OFFSET 05]
[LVIEW_SIZE 06]
[LVIEW_OBJTYPE 07]
[LVIEW_EXPORTEDFUNCTION 08]
[LVIEW_MACHINE 09]
[LVIEW_SECTION 10]
[LVIEW_TIME 11]
[LVIEW_SYMBOL 12]
[LVIEW_OPTIONAL 13]

[hHeader: D$ ?]
[hList: D$ ?]

; LV_COLUMN Structure

[lvc:
 lvc.imask: D$ 0
 lvc.fmt: D$ &LVCFMT_LEFT
 lvc.lx: D$ 80  ; columnwidth
 lvc.pszText: D$ 0
 lvc.cchTextMax: D$ 0
 lvc.iSubItem: D$ 0
 lvc.iImage: D$ 0
 lvc.iOrder: D$ 0]

Proc SetupListview:
    Arguments @h2List

    ; /*Listview setup */
    mov D$lvc.imask, &LVCF_TEXT+&LVCF_WIDTH
    mov D$lvc.pszText, Header1
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_INDEX, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header2
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_FILENAME, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header3
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_EXTENSION, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header4
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_NAMETYPE, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header5
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_PATH, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header6
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_OFFSET, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header7
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_SIZE, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header8
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_OBJTYPE, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header9
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_EXPORTEDFUNCTION, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header10
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_MACHINE, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header11
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_SECTION, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header12
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_TIME, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header13
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_SYMBOL, lvc
    or D$lvc.imask, &LVCF_FMT
    mov D$lvc.pszText, Header14
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTCOLUMN, LVIEW_OPTIONAL, lvc

    ;/* these 5 lines create a FLAT columnheader */
    call 'user32.SendMessageA', D@h2List, &LVM_GETHEADER__&LVM_ENSUREVISIBLE__&LVM_SETCOLUMNORDERARRAY, 0, 0 ;// get handle to header;&LVM_GETHEADER, 0, 0 ;// get handle to header
    mov D$hHeader, eax ;// preserve header handle
    call 'user32.GetWindowLongA', D$hHeader, &GWL_STYLE ;// get current window styles
    xor eax, &HDS_BUTTONS
    call 'user32.SetWindowLongA', D$hHeader, &GWL_STYLE, eax ;// set the new header styles

    ;/* Setup extended styles like gridlines, back-foregroundcolors */
    call 'user32.SendMessageA', D@h2List, &LVM_SETEXTENDEDLISTVIEWSTYLE, 0, &LVS_EX_FULLROWSELECT__&LVS_EX_HEADERDRAGDROP__&LVS_EX_SUBITEMIMAGES__&LVS_EX_GRIDLINES__&LVS_EX_FLATSB
    call 'user32.SendMessageA', D@h2List, &LVM_SETTEXTCOLOR, 0, {RGB 186 48 38};{RGB 0 0 0} ;0
    call 'user32.SendMessageA', D@h2List, &LVM_SETBKCOLOR, 0, {RGB 255 255 255} ; 0FFFFFF
    call 'user32.SendMessageA', D@h2List, &LVM_SETTEXTBKCOLOR, 0, {RGB 240, 247, 166} ; 0A6F7F0

EndP

___________________________________________________________________________________________________________

; ----------------------------------------------
; 2nd Dialog Tab Frame Procedure
; This is to show the Object Listing
; ----------------------------------------------


Proc Tab2Proc:
    Arguments @hWin, @uMsg, @wParam, @lParam

    pushad

    If D@uMsg = &WM_INITDIALOG
        call 'USER32.SendMessageW' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP
    Else_If D@uMsg = &WM_NOTIFY
        call LibScanDialog_OnNotify D@hWin, D@lParam
    Else_If D@uMsg = &WM_CTLCOLOREDIT
        call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP
    Else
        popad | mov eax &FALSE | ExitP
    End_If

    popad | mov eax &TRUE
EndP


; ----------------------------------------------
; 1st Dialog Tab Frame Procedure
; This is where the MZ Header dialog is used
; ----------------------------------------------


Proc Tab1Proc:
    Arguments @hWin, @uMsg, @wParam, @lParam

    pushad

    .If D@uMsg = &WM_INITDIALOG
        call 'USER32.SendMessageW' D@lParam, &EM_SETSEL, 0-1, 0
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP
    .Else_If D@uMsg = &WM_NOTIFY
        call LibScanDialog_OnNotify D@hWin, D@lParam
    .Else_If D@uMsg = &WM_CTLCOLOREDIT
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP
    .Else
        popad | mov eax &FALSE | ExitP
    .End_If

    popad | mov eax &TRUE
EndP




______________________________________________________________________________________________

; constants for the library dialogs

; Dialog Controls

[IDD_MAINLIB 20] ; Main Dialog where it will hold the tabs.

; Internal controls for this dialog

[IDC_LIBFILESIZE 4] ; The Edit Control where the path name is displayed
[IDC_OPENLIBFILE 5] ; The Edit Control where the path name is displayed
[IDC_TABCTRL 38]  ; Main Tab Control Resource.
[IDC_LOADLIB 3]     ; The "Open" Button.

[IDD_TAB1 21] ; Id of the 1st Dialog "Library Structure"

; Internal controls for this dialog

[IDC_LIB_TARGET 11] ; 1 59 199 188 . ES_MULTI ES_AUTOVS ES_AUTOH WS_VS WS_HSC WS_TAB WS_BO
[IDC_LIB_SOURCE 10] ; 202 57 178 188. ES_MULTI ES_AUTOVS WS_VS WS_BO


[IDD_TAB2 22]  ; Id of the 2nd Dialog "Object Listing"
; Internal controls for this dialog
[IDC_LVIEW 30] ; Id for the listview control

[IDD_TAB3 23] ; Id of the 3rd Dialog other Header
[IDD_TAB4 24] ; Id of the 4th Dialog Section Images


; Menu Equates

[M03_Menu  4000                  M03_Open  4001                  M03_Close  4002
 M03_Save_Report  4003           M03_Exit  4004                  M03_Single_Object_File  4005
 M03_Runtime_Objects_Only  4006  M03_Exported_Objects_Only  4007 M03_All_Object_Files  4008
 M03_Build_Dis_File  4009        M03_Next_Tab  4010              M03_Previous_Tab  4011
 M03_First_Tab  4012             M03_Last_Tab  4013              M03_Show_ToolBarText  4014
 M03_Hover_ToolBar  4015         M03_About  4016]

; Structures used

[NMHDR.hwndFromDis 0
 NMHDR.idFromDis 4
 NMHDR.codeDis 8]


[hInst: D$ 0]

[SortDecimal: D$ 0] ; Sort Buffer

[flag_TB_00: ?]

; Tab Selection
[LibScanSelTabStruc 0]
[LibScanSelTabObj 1]
[LibScanSelTabDisasm 2]
[LibScanSelTabDisBuild 3]

[LibScanTab_ImageList: ?]
; Tag Dialog 20

Proc ScanLibFile:
    Arguments @Adressee, @Message, @wParam, @lParam

     pushad

    ...If D@Message = &WM_COMMAND                  ; User action

        ..If_Or D@wParam = &IDCANCEL, D@wParam = M03_Exit  ; User clicks on upper right [X] or Exited through the menu
            call LibScanCleanUp D@Adressee
            call 'COMCTL32.ImageList_Destroy' D$LibScanDialog_ImageList
            call 'COMCTL32.ImageList_Destroy' D$LibScanTab_ImageList
            call 'USER32.EndDialog' D@Adressee 0
            mov D$LibScanToolbarHandle 0
            mov B$LibScanIsFileOpen &FALSE

        ..Else_If D@wParam = M03_Close
            call LibScanCleanUp D@Adressee
            mov B$LibScanIsFileOpen &FALSE
            call LibScanDialog_EnableContinueMenu &FALSE
            jmp L5>>

        ..Else_If D@wParam = M03_Save_Report
            call SaveLibFileAs D@Adressee, D$hLibReportEdit, D$hLibReportEditLength

        ..Else_If D@wParam = M03_Single_Object_File
            call SaveOneObjectFile D@Adressee

        ..Else_If D@wParam = M03_Runtime_Objects_Only
            call SaveAllObjectFile D@Adressee, EXPORT_RUNTIME_OBJECTS

        ..Else_If D@wParam = M03_Exported_Objects_Only
            call SaveAllObjectFile D@Adressee, EXPORT_EXPORTED_OBJECTS

        ..Else_If D@wParam = M03_All_Object_Files
            call SaveAllObjectFile D@Adressee, EXPORT_ALL_OBJECTS

        ..Else_If D@wParam = M03_Show_ToolBarText
            call LibScanDialog_ToggleToolbarText D@Adressee

        ..Else_If D@wParam = M03_Hover_ToolBar
            call LibScanDialog_HoverToolbarText D@Adressee

        ..Else_If D@wParam = M03_Next_Tab
            call LibScanDialog_EnableContinuePrevTabMenu &TRUE
            call 'USER32.SendMessageA' D$hTab &TCM_GETCURSEL 0 0
            .If eax <> LibScanSelTabDisBuild
                inc eax
                call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL eax eax
                inc eax

                If eax <> D$SelTab
                    push eax
                    mov eax D$SelTab
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_HIDE
                    pop eax
                    mov D$SelTab eax
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_SHOWDEFAULT
                End_If
            .End_If

            If D$SelTab = LibScanSelTabDisBuild
                call LibScanDialog_EnableContinueNextTabMenu &FALSE
            End_If

        ..Else_If D@wParam = M03_Previous_Tab

            call 'user32.SendMessageA' D$hTab &TCM_GETCURSEL 0 0
            .If eax <> LibScanSelTabStruc
                dec eax
                call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL eax eax
                dec eax

                If eax <> D$SelTab
                    push eax
                    mov eax D$SelTab
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_HIDE
                    pop eax
                    mov D$SelTab eax
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_SHOWDEFAULT
                End_If
                call LibScanDialog_EnableContinueNextTabMenu &TRUE
            .End_If

            If D$SelTab = LibScanSelTabStruc
                call LibScanDialog_EnableContinuePrevTabMenu &FALSE
            End_If

        ..Else_If D@wParam = M03_First_Tab
L5:
                call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL LibScanSelTabStruc LibScanSelTabStruc
                mov eax LibScanSelTabStruc
                mov D$SelTab LibScanSelTabObj

                If eax <> D$SelTab
                    push eax
                    mov eax D$SelTab
                    call 'user32.ShowWindow' D$eax*4+hTabDlg1 &SW_HIDE
                    pop eax
                    mov D$SelTab eax
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_SHOWDEFAULT
                End_If
                call LibScanDialog_EnableContinueNextTabMenu &TRUE
                call LibScanDialog_EnableContinuePrevTabMenu &FALSE

        ..Else_If D@wParam = M03_Last_Tab

                call 'USER32.SendMessageA' D$hTab &TCM_SETCURSEL LibScanSelTabDisBuild LibScanSelTabDisBuild
                mov ecx LibScanSelTabDisBuild
                mov eax D$SelTab
                If eax = D$SelTab
                    push eax
                    mov eax D$SelTab
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_HIDE
                    pop eax
                    mov eax LibScanSelTabDisBuild
                    mov D$SelTab eax
                    call 'USER32.ShowWindow' D$eax*4+hTabDlg1 &SW_SHOWDEFAULT
                End_If
                call LibScanDialog_EnableContinueNextTabMenu &FALSE
                call LibScanDialog_EnableContinuePrevTabMenu &TRUE

        ..Else_If D@wParam = M03_Open

            mov D$ChoosenLibFile 0
            move D$OPENLIB@hwndOwner D@Adressee
            move D$OPENLIB@hInstance D$hInstance
            move D$OPENLIB@lpstrFilter LibsFileFilter
            call 'COMDLG32.GetOpenFileNameA' OPENLIB

            .If D$ChoosenLibFile <> 0
                call LibScanCleanUp D@Adressee
                call LibScanDialog_EnableContinueMenu &TRUE

                call 'USER32.SendDlgItemMessageA' D@Adressee IDC_OPENLIBFILE &WM_SETTEXT 0 LibSaveFilter
                call 'USER32.SendDlgItemMessageA' D@Adressee IDC_OPENLIBFILE &WM_GETTEXT 0 LibSaveFilter ; GET THE FILENAME

                ; The user is opening the file, Clean the list view.
                call 'USER32.SendMessageA', D$hlist, &LVM_DELETEALLITEMS, 0, lvi

                call OpenLibFile D@Adressee
                On D$LibFileLength = 0, jmp L9>> ; Exit when the file size is 0.

                call LibSignatureCheck

                If D$ValidLib = UNKNOWN_LIB_FILE
                    ; Disables the Menus itens to prevent the user tries to save a report, or do something wrong with it
                    call LibScanDialog_EnableContinueMenu &FALSE
                    call 'USER32.DialogBoxParamA' D$hinstance, IDD_LIBSCANWARNINGMSG, D@Adressee, LibScanWarning, &NULL
                    jmp L9>>
                End_If

                call ParseIdentifiedLibs D@Adressee

                ; Below we can only remove after we build the parser for each one of these libraries.
                ; But we can leave this lines here, in the meanwhile
                If_Or D$ValidLib = DCU1_KILYX_OBJ_FILE, D$ValidLib = DCU2_KILYX_OBJ_FILE,
                      D$ValidLib = DCU3_KILYX_OBJ_FILE, D$ValidLib = DCU2_OBJ_FILE,
                      D$ValidLib = DCU3_OBJ_FILE, D$ValidLib = DCU4_OBJ_FILE,
                      D$ValidLib = DCU5_OBJ_FILE, D$ValidLib = DCU6_OBJ_FILE,
                      D$ValidLib = DCU7_OBJ_FILE, D$ValidLib = OMF_OBJ_FILE,
                      D$ValidLib = PDB_OBJ_FILE, D$ValidLib = DBG_OBJ_FILE
                    jmp L9>>
                End_If
            .End_If

            call CoolControl_LVBeginSort ListViewLibSort, SortDecimal, D$hlist, 1
      ;  ..Else_If D@wParam = &IDHELP
      ;      call Help, B_U_AsmName, DisassemblerHelp, ContextHlpMessage

        ..End_If


    ...Else_If D@Message = &WM_NOTIFY
        call LibScanDialog_OnNotify D@Adressee, D@lParam

    ...Else_If D@Message = &WM_NCMOUSEMOVE
        ..If B$LibScanHoverTBText <> &MF_UNCHECKED
            .If D$flag_TB_00 = 0                    ;check flag toolbar
                call 'USER32.ShowWindow' D$LibScanToolbarHandle &SW_SHOW
                mov D$flag_TB_00 1                  ;set flag toolbar
            .End_If
        ..End_If
    ...Else_If D@Message = &WM_MOUSEMOVE
        ..If B$LibScanHoverTBText <> &MF_UNCHECKED
            .If D$flag_TB_00 = 1                    ;check flag toolbar
                call 'USER32.ShowWindow' D$LibScanToolbarHandle &SW_HIDE
                mov D$flag_TB_00 0                  ;set flag toolbar
            .End_If
        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG

        call 'USER32.GetMenu' D@Adressee | mov D$LibScanMenuHandle eax

        ; Create the image list
        call CoolControlTB_CreateImageList LibScanDialog_ImageList, IDB_LibScanEnableTB, IDB_LibScanDisableTB, 20, 20, &ILC_COLOR32+&ILC_MASK, LibScanToolButtonsNumber, LibScanToolButtonsNumber

        call CoolControlTB_CreateImageList LibScanTab_ImageList, IDB_LibScanTABEnable, IDB_LibScanTABDisable, 20, 20, &ILC_COLOR32+&ILC_MASK, LibScanTabControlsNumber, LibScanTabControlsNumber

        ;Create the tabs
        call CoolControlDlg_CreateTab D@Adressee, IDC_TABCTRL, hTab, LibScanTabControl

        ;Create the tab dialogs
        call 'user32.CreateDialogParamA' D$hInstance IDD_TAB1 D$hTab Tab1Proc 0
        mov D$hTabDlg1 eax

        call 'user32.CreateDialogParamA' D$hInstance IDD_TAB2 D$hTab Tab2Proc 0
        mov D$hTabDlg2 eax


        call 'user32.GetDlgItem' D$hTabDlg2 IDC_LVIEW ; Get the ListView Control in the 2nd TAB resources, and
                                                    ; use it on the handle of the main window
        mov D$hList eax                              ; Now we return the result (found in eax), copying it to
                                                    ; the Handle of the Tab Control in the resource.
                                                    ; So, all we did was get the TAB Control and save it to the Tab handle.

        ; Create the toolbar
        call CoolControlWin_CreateToolbar D@Adressee, LibScanToolbarHandle, LibScanToolbarButtons,
        LibScanToolButtonsNumber, LibScanToolTipsStrings, LSTBWin_Cmd

        ; Initialize the Menu state
        call LibScanDialog_EnableContinueMenu &FALSE
        call LibScanDialog_EnableContinuePrevTabMenu &FALSE

        ;/* Setup listview */
        call SetupListview, D$hList
        call CoolControl_LVBeginSort ListViewLibSort, SortDecimal, D$hlist, 1
;;
        call 'user32.CreateDialogParamA' D$hInstance IDD_TAB3 D$hTab Tab3Proc 0
        mov D$hTabDlg3 eax
        call 'user32.CreateDialogParamA' D$hInstance IDD_TAB4 D$hTab Tab4Proc 0
        mov D$hTabDlg4 eax
;;


    ...Else_If D@Message = &WM_CLOSE
        call LibScanCleanUp D@Adressee
        call 'COMCTL32.ImageList_Destroy' D$LibScanDialog_ImageList
        call 'COMCTL32.ImageList_Destroy' D$LibScanTab_ImageList
        call 'USER32.EndDialog' D@Adressee &NULL
        mov D$LibScanToolbarHandle 0
        mov B$LibScanIsFileOpen &FALSE

    ...Else_If D@Message = &WM_CTLCOLOREDIT
        call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP

    ...Else
        popad | mov eax &FALSE | ExitP

    ...End_If

L9: popad | mov eax &TRUE
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; This function handles all Notifications messages from all controls in the different Tabs
; It is mandatory to they be called inside each Tab Procedure (Tab1Proc, Tab2Proc ....)
; because we need they behave as a callback to handle the proper messages from each control Tab

Proc LibScanDialog_OnNotify:
    Arguments @Adressee, @Notification

    mov ebx D@Notification
    mov edx D$ebx+NMHDR.idFromDis
    mov eax D$ebx+NMHDR.codeDis

    .If edx = IDC_TABCTRL
        If eax = &TCN_SELCHANGE
            call CoolControlTabChange_OnNotify D@Notification, SelTab, D$hTab
        End_If

    .Else_If edx = IDC_LVIEW
        If eax = &LVN_COLUMNCLICK
            call CoolControl_ListViewAlternateSort ListViewLibSort, D@Notification, SortDecimal, D$hList, Lib_LVTotalCol
        Else_If eax = &NM_DBLCLK
            call SaveOneObjectFile D@Adressee
        End_If

    .Else_If eax = &TTN_NEEDTEXT

        If_And edx >= D$LSTB01.idCommand, edx <= D$LSTB10.idCommand
            call CoolControlTB_OnNotify D@Adressee, D@Notification, LibScanToolbarButtons, LibScanToolButtonsNumber, LibScanToolTipsStrings
        Else_If_And edx >= LibScanSelTabStruc, edx <= LibScanSelTabDisBuild
            call CoolControlTabToolTip_OnNotify D@Notification, LibScanTabNumber, LibScanTabToolTipStrings
        End_If

    .End_If

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc SaveOneObjectFile:
    Arguments @Adressee

    pushad

    call 'user32.SendMessageA' D$hList, &LVM_GETNEXTITEM, 0-1, &LVNI_SELECTED
    If eax <> 0-1
        call SaveSingleObjectFileAs D@Adressee, eax
    Else
        call 'user32.MessageBoxA' D@Adressee, {B$ "No field selected or empty fields.", D$ CRLF2, B$ "You must select one field in the ListControl to allow exporting the Selected Object file.", 0}, {"Attention !", 0}, &MB_ICONEXCLAMATION
    End_If

    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________


[ObjSaveFileFilter: B$ 'Object File (*.obj)', 0  '*.obj', 0 0]
[ObjSaveFilter: ? #&MAX_PATH]

Proc SaveSingleObjectFileAs:
    Arguments @Adressee, @SelectedItem
    pushad

    ; 1st we get the FileName

    move D$lvi.iItem D@SelectedItem
    mov D$lvi.iSubItem 1 ; [Header2: B$ 'File Name',0] ; string
    call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
    ; the result is in szBuff0

    mov edi ObjSaveFilter
    push esi | ZCopy szBuff0 | pop esi
    mov B$edi 0

    mov D$ChoosenLibFile 0
    move D$SAVELIB.lpstrFile ObjSaveFilter
    move D$SAVELIB.hwndOwner D@Adressee
    move D$SAVELIB.hInstance D$hInstance
    move D$SAVELIB.lpstrFilter ObjSaveFileFilter
    call 'Comdlg32.GetSaveFileNameA' SAVELIB

    ..If eax <> 0
        .If D$ChoosenLibFile <> 0
            call ForceExtension ObjSaveFilter, '.obj'

            ; Get the Offset
            move D$lvi.iItem D@SelectedItem
            mov D$lvi.iSubItem 5 ; [Header6: B$ 'Offset',0] ; hexadecimal value string
            call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
            call AsciiBase szBuff0 BASE_HEX
            mov edi eax

            ; Get the size
            move D$lvi.iItem D@SelectedItem
            mov D$lvi.iSubItem 6 ; [Header7: B$ 'Size',0] ; decimal value
            call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
            call String2Dword szBuff0

            mov esi D$LibFileMemory
            add esi edi
            call SaveLibFile, esi, eax, ObjSaveFilter
        .End_If
    ..End_If

    popad
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________

[ObjPathTitle: B$ 'For the Exported Object Files', 0]
[MultipleObjectPath: B$ ? #&MAXPATH]

[EXPORT_ALL_OBJECTS 0]
[EXPORT_RUNTIME_OBJECTS 1]
[EXPORT_EXPORTED_OBJECTS 2]
; Flag = 0 (Save all Objects)
; Flag = 1 (Save all Runtime Objects)
; Flag = 2 (Save all Exported Objects)
Proc SaveAllObjectFile:
    Arguments @Adressee, @Flag
    Local @SelectedItem, @CountItens, @FolderPathStart, @RepeatedName

; More examples here MultipleCompileTests SaveSingleObjectFileAs
    pushad

    call BrowseForFolder D@Adressee, ObjPathTitle
    ...If B$BrowseForFolderAborted <> &TRUE

        ; Initialize all revelant Data
        mov D@RepeatedName 0
        mov D@CountItens 0
        mov D$RepeatedObjIndice '0000', D$RepeatedObjIndice+4 '00'
        ; 1st we calculate the amount of objectsinside the lib
        call String2Dword LibObjIndice
        mov D@CountItens eax

        mov D@SelectedItem 0 ; always starts at the 1st item (that have the value of 0)
        mov esi FolderPath, edi MultipleObjectPath
        While B$esi <> 0 | movsb | End_While | mov B$edi '\' | inc edi
        mov D@FolderPathStart edi

        .Do

            .If D@Flag = EXPORT_RUNTIME_OBJECTS
                move D$lvi.iItem D@SelectedItem
                mov D$lvi.iSubItem 7 ; [Header8: B$ 'Object Type',0] ; string
                call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
                On D$szBuff0 <> 'Runt', jmp L1>> ; Not a runtime Object, jmp over it
            .Else_If D@Flag = EXPORT_EXPORTED_OBJECTS
                move D$lvi.iItem D@SelectedItem
                mov D$lvi.iSubItem 7 ; [Header8: B$ 'Object Type',0] ; string
                call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
                On D$szBuff0 <> 'Expo', jmp L1>> ; Not a Export Object, jmp over it
            .End_If
            ; 1st we get the FileName

            move D$lvi.iItem D@SelectedItem
            mov D$lvi.iSubItem 1 ; [Header2: B$ 'File Name',0] ; string
            call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi

            ; Copy the filename to the path and append the '.obj' extension
            ; the result is in szBuff0
            mov esi szBuff0
            While B$esi <> 0 | movsb | End_While
            mov D$edi '.obj' | add edi 4 | mov B$edi 0

            call 'KERNEL32.FindFirstFileA' MultipleObjectPath, FindFile

            If eax <> &INVALID_HANDLE_VALUE ; do we have an existing file with this name ?
                ; Yes...append a new name for it
                mov D$edi-4 '_New'
                call IncrementObjExportNameIndex
                mov esi RepeatedObjIndice
                While B$esi <> 0 | movsb | End_While
                mov D$edi '.obj' | add edi 4 | mov B$edi 0

            End_If

            ; Get the Offset
            move D$lvi.iItem D@SelectedItem
            mov D$lvi.iSubItem 5 ; [Header6: B$ 'Offset',0] ; hexadecimal value string
            call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
            call AsciiBase szBuff0 BASE_HEX
            mov edi eax

            ; Get the size
            move D$lvi.iItem D@SelectedItem
            mov D$lvi.iSubItem 6 ; [Header7: B$ 'Size',0] ; decimal value
            call 'USER32.SendMessageA' D$hlist, &LVM_GETITEM, D@SelectedItem, lvi
            call String2Dword szBuff0

            mov esi D$LibFileMemory
            add esi edi
            call SaveLibFile, esi, eax, MultipleObjectPath
L1:
            mov edi D@FolderPathStart
            inc D@SelectedItem
            dec D@CountItens
        .Loop_Until D@CountItens = 0

    ...End_If
    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[RepeatedObjIndice: '000000', 0]

IncrementObjExportNameIndex:
    lea ebx D$RepeatedObjIndice+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While
ret

____________________________________________________________________________________________
____________________________________________________________________________________________



[LibSaveFileFilter: B$ 'Text File (*.txt)', 0  '*.txt', 0
                    B$ 'Assembly File (*.asm)', 0  '*.asm', 0
                    B$ 'All Files', 0  '*.*', 0 0]

[CustomLibFileFilter:  0 #&MAX_PATH]

[SaveLibFileTitle: B$ 'Save Library Structure report as...', 0]

; OPENFILENAMEA Structure

[SAVELIB:
 SAVELIB.lStructSize: D$ Len
 SAVELIB.hwndOwner: D$ 0
 SAVELIB.hInstance: D$ 0
 SAVELIB.lpstrFilter: D$ LibSaveFileFilter
 SAVELIB.lpstrCustomFilter: D$ CustomLibFileFilter
 SAVELIB.nMaxCustFilter: D$ 260
 SAVELIB.nFilterIndex: D$ 1
 SAVELIB.lpstrFile: D$ LibSaveFilter
 SAVELIB.nMaxFile: D$ 260
 SAVELIB.lpstrFileTitle: D$ ChoosenLibFile
 SAVELIB.nMaxFileTitle: D$ 260
 SAVELIB.lpstrInitialDir: D$ 0
 SAVELIB.lpstrTitle: D$ SaveLibFileTitle
 SAVELIB.Flags: D$ &OFN_EXPLORER__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
 SAVELIB.nFileOffset: W$ 0
 SAVELIB.nFileExtension: W$ 0
 SAVELIB.lpstrDefExt: D$ 0
 SAVELIB.lCustData: D$ 0
 SAVELIB.lpfnHook: D$ 0
 SAVELIB.lpTemplateName: D$ 0]

[LibScanIsFileOpen: B$ &FALSE]

Proc SaveLibFileAs:
    Arguments @Adressee, @OutPut, @OutPutSize
    uses eax

    ; To Insert the Appended String. We inset it, when the user opens a new file only. On case the user presses Clear Button
    ; the Buffer is emptyed.
    If_And B$LibScanIsFileOpen = &FALSE, D$hLibReportEdit <> 0
        call LibScanAppendReportName LibSaveFilter
    End_If

    mov D$ChoosenLibFile 0
    move D$SAVELIB.lpstrFile LibSaveFilter
    move D$SAVELIB.hwndOwner D@Adressee
    move D$SAVELIB.hInstance D$hInstance
    move D$SAVELIB.lpstrFilter LibSaveFileFilter
    call 'Comdlg32.GetSaveFileNameA' SAVELIB

    On eax = 0, ExitP
    .If D$ChoosenLibFile <> 0
        If D$SAVELIB.nFilterIndex = 1 ; extension .txt
            call ForceExtension LibSaveFilter, '.txt'
        Else_If D$SAVELIB.nFilterIndex = 2 ; extension .asm
            call ForceExtension LibSaveFilter, '.asm'
        Else ; Force txt extensino if the user is trying to save as "all"
            call ForceExtension LibSaveFilter, '.txt'
        End_If
        call SaveLibFile, D@OutPut, D@OutPutSize, LibSaveFilter
    .End_If

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanAppendReportName:
    Arguments @String
    Uses eax, ecx, edi

    mov edi D@String
    ;sub edi 2     ; edi points to the end of the string 2

    .While edi >= D@String

        .If_Or B$edi = ':', B$edi = '/', B$edi = '\'

            Do
                On B$edi = '.', jmp L1>
                inc edi
            Loop_Until B$edi = 0
            jmp L1>
        .End_If

        ;dec edi
        inc edi
    .End_While
    L1:
        push esi | ZCopy {"Report.txt", 0} | pop esi
        mov D$edi 0
    mov B$LibScanIsFileOpen &TRUE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc SaveLibFile:
    Arguments @OutPut, @OutPutSize, @FileFilter

    pushad

    call 'KERNEL32.CreateFileA' D@FileFilter, &GENERIC_WRITE,
                                &FILE_SHARE_READ, &NULL, &CREATE_ALWAYS,
                                &FILE_ATTRIBUTE_NORMAL, &NULL

    mov D$LibFileHandle eax

    If eax = &INVALID_HANDLE_VALUE
        mov eax D$BusyFilePtr | call MessageBox | popad | ret
    End_If

    mov D$DestinationHandle eax, D$NumberOfReadBytes 0

    call 'KERNEL32.WriteFile' D$DestinationHandle, D@OutPut, D@OutPutSize, NumberOfReadBytes  0
    call 'KERNEL32.CloseHandle' D$DestinationHandle | mov D$DestinationHandle 0

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; This function will cleanup all previously loaded Data in case the user reopens the file without
; closing the CharMap Dialog

Proc LibScanCleanUp:
    Arguments @Adressee
    ; Free LibFileMemory
    VirtualFree D$LibFileMemory
    ; Free hLibReportEdit Report
    VirtualFree D$hLibReportEdit

    call 'USER32.SendDlgItemMessageA' D@Adressee, IDC_OPENLIBFILE, &WM_SETTEXT, 0, &NULL
    call 'USER32.SendDlgItemMessageA' D@Adressee, IDC_LIBFILESIZE, &WM_SETTEXT, 0, &NULL
    call 'USER32.SendDlgItemMessageA' D$hTabDlg1, IDC_LIB_SOURCE, &WM_SETTEXT, 0, &NULL
    call 'user32.SendMessageA', D$hList, &LVM_DELETEALLITEMS, 0, lvi
    call 'USER32.SendMessageA' D@Adressee, &WM_SETTEXT, 0, {"ScanLib v 1.0", 0}

    call ClearBuffer szBuff0, 256
    call ClearBuffer szBuff1, 256

    ; Must clean these listview itens to avoid that we have errors on the resequence
    mov D$lvi.iItem 0
    mov D$lvi.iSubItem 0
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CoolControlDlg_CreateTab:
    Arguments @Adressee, @nIDDlgItem, @OutPutHandle, @CCTabStructure
    Local @TabsAmount, @UseImage


    mov D@TabsAmount 0
    mov D@UseImage 0

    call 'user32.GetDlgItem' D@Adressee, D@nIDDlgItem ; Get the Tab Control in the resources, and
                                                    ; use it on the handle of the main window
                                                    ; of this procedure.
    lea edi D@OutPutHandle
    mov edi D$edi
    mov D$edi eax                                   ; Now we return the result (found in eax), copying it to
                                                    ; the Handle of the Tab Control in the resource.
                                                    ; So, all we did was get the TAB Control and save it to the Tab handle.

    mov ecx D@CCTabStructure
    mov eax D$ecx+CCTabOrganize.ImageListDis

    If eax = 0
        mov D@UseImage &FALSE
    Else
        mov D@UseImage &TRUE
        push ecx | SendMessage D$edi, &TCM_SETIMAGELIST, 0, D$eax | pop ecx
    End_If

    mov edx D$ecx+CCTabOrganize.TCITEMDis
    move D$edx+TC_ITEM.imaskDis D$ecx+CCTabOrganize.iMaskFlagDis

    mov eax D$ecx+CCTabOrganize.TabsAmountDis
    move D@TabsAmount D$eax

    mov ebx 0

    .Do

        ; TabTitle copied to ts.pszText
        mov esi D$ecx+CCTabOrganize.StringArrayDis
        mov esi D$esi+ebx*4
        mov D$edx+TC_ITEM.pszTextDis esi

        ; TabTitleLen copied to ts.cchTextMax
        mov esi D$ecx+CCTabOrganize.StringLenArrayDis
        mov esi D$esi+ebx*4
        move D$edx+TC_ITEM.cchTextMaxDis D$esi

        If D@UseImage = &TRUE
            ; Image ID copied to ts.iImage
            mov D$edx+TC_ITEM.iImageDis ebx
        End_If

        push edx
        push ecx
        call 'user32.SendMessageA' D$edi &TCM_INSERTITEM ebx edx
        pop ecx
        pop edx

        inc ebx
    .Loop_Until ebx = D@TabsAmount
;;
[TC_ITEM.imaskDis 0
 TC_ITEM.lpReserved1Dis 4
 TC_ITEM.lpReserved2Dis 8
 TC_ITEM.pszTextDis 12
 TC_ITEM.cchTextMaxDis 16
 TC_ITEM.iImageDis 20
 TC_ITEM.lParamDis 24]
     
;;
;;

        mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        mov D$ts.pszText TabTitle1
        mov D$ts.cchTextMax TabTitle1Len
        mov D$ts.iImage 0
        call 'user32.SendMessageA' D$edi &TCM_INSERTITEM 0 ts

        mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        mov D$ts.pszText TabTitle2
        mov D$ts.cchTextMax TabTitle2Len
        mov D$ts.iImage 1
        call 'user32.SendMessageA' D$edi &TCM_INSERTITEM 1 ts

        mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        mov D$ts.pszText TabTitle3
        mov D$ts.cchTextMax TabTitle3Len
        mov D$ts.iImage 2
        call 'user32.SendMessageA' D$edi &TCM_INSERTITEM 2 ts


        mov D$ts.imask &TCIF_TEXT__&TCIF_IMAGE ; Mask text & image
        mov D$ts.pszText TabTitle4
        mov D$ts.cchTextMax TabTitle4Len
        mov D$ts.iImage 3
        call 'user32.SendMessageA' D$edi &TCM_INSERTITEM 3 ts
;;

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________


[LibScannerTitle: B$ 0 #100]

Proc WriteObjectTypeinTitle:
    Arguments @Adressee, @TextTitle

    pushad

    mov eax D@TextTitle
    mov edi LibScannerTitle
    push esi | Zcopy {"ScanLib v 1.0 - ", 0} | pop esi

    push esi | ZCopy eax | pop eax
    push esi | Zcopy {" Format", 0} | pop esi

    mov B$edi 0
    call 'USER32.SendMessageA' D@Adressee, &WM_SETTEXT, 0, LibScannerTitle

    popad

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Enable / Gray-out execution control commands.

Proc LibScanDialog_EnableContinueMenu:
    Arguments @Enable

    If D@Enable = 1
        mov ebx &MF_ENABLED
    Else
        mov ebx &MF_GRAYED
    EndIf

    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Close, ebx
    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Save_Report, ebx
    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Single_Object_File, ebx
    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_All_Object_Files, ebx
    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Runtime_Objects_Only, ebx
    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Exported_Objects_Only, ebx
    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Build_Dis_File, ebx

    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Close, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Save_Report, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Single_Object_File, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_All_Object_Files, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Runtime_Objects_Only, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Exported_Objects_Only, D@Enable
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Build_Dis_File, D@Enable

  ; Invert
    If D@Enable = 1
        mov ebx &MF_GRAYED
        mov D@Enable 0
    Else
        mov ebx &MF_ENABLED
        mov D@Enable 1
    EndIf

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDialog_EnableContinueNextTabMenu:
    Arguments @Enable

    If D@Enable = 1
        mov ebx &MF_ENABLED
    Else
        mov ebx &MF_GRAYED
    EndIf

    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Next_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Next_Tab, D@Enable

    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Last_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Last_Tab, D@Enable
  ; Invert
    If D@Enable = 1
        mov ebx &MF_GRAYED
        mov D@Enable 0
    Else
        mov ebx &MF_ENABLED
        mov D@Enable 1
    EndIf

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDialog_EnableContinuePrevTabMenu:
    Arguments @Enable
    uses eax, ecx, edx

    If D@Enable = 1
        mov ebx &MF_ENABLED
    Else
        mov ebx &MF_GRAYED
    EndIf

    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_Previous_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_Previous_Tab, D@Enable

    call 'USER32.EnableMenuItem' D$LibScanMenuHandle, M03_First_Tab, ebx
    SendMessage D$LibScanToolbarHandle, &TB_ENABLEBUTTON, M03_First_Tab, D@Enable


  ; Invert
    If D@Enable = 1
        mov ebx &MF_GRAYED
        mov D@Enable 0
    Else
        mov ebx &MF_ENABLED
        mov D@Enable 1
    EndIf

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDisplayFileSize:
    Arguments @Adressee

    pushad
    ; Calculate and Display File Size
    mov edi buffer2
    lea esi D$LibFileLength

    mov ecx 4
    call toUDword
    mov esi edi, edi DecimalBuffer
    Do | movsb | LoopUntil B$esi = 0
    push esi | ZCopy {' Bytes', 0} | pop esi
    mov B$edi 0

    call 'USER32.SendDlgItemMessageA' D@Adressee IDC_LIBFILESIZE &WM_SETTEXT 0 DecimalBuffer
    call 'USER32.SendDlgItemMessageA' D@Adressee IDC_LIBFILESIZE &WM_GETTEXT 0 DecimalBuffer ; GET THE FILENAME
    popad
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc LibScanDialog_ToggleToolbarText:
    Arguments @Addresse
    Structure @TBButtonInfo 32, @Size 0, @Mask 4, @Text 24

    mov D@Size 32, D@Mask &TBIF_TEXT

  ; get check state of menu item, invert, toggle band and set inverted check state
    call 'User32.GetMenuState' D$LibScanMenuHandle, M03_Show_ToolBarText, &MF_BYCOMMAND
    push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        mov B$LibScanShowTBText dl
        call CoolControlWin_CreateCommandTB D@Addresse, LibScanToolbarHandle, LibScanToolbarButtons, LibScanToolButtonsNumber, LibScanToolTipsStrings, LSTBWin_Cmd
    pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    call 'USER32.CheckMenuItem' D$LibScanMenuHandle, M03_Show_ToolBarText, eax
    SendMessage D$LibScanMenuHandle, &TB_AUTOSIZE, 0, 0

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

[LibScanHoverTBText: ?]

Proc LibScanDialog_HoverToolbarText:
    Arguments @Addresse
    Structure @TBButtonInfo 32, @Size 0, @Mask 4, @Text 24

    mov D@Size 32, D@Mask &TBIF_TEXT

  ; get check state of menu item, invert, toggle band and set inverted check state
    call 'User32.GetMenuState' D$LibScanMenuHandle, M03_Hover_ToolBar, &MF_BYCOMMAND
    push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        mov B$LibScanHoverTBText dl
        ;call CoolControlWin_CreateCommandTB D@Addresse, LibScanToolbarHandle, LibScanToolbarButtons, LibScanToolButtonsNumber, LibScanToolTipsStrings, LSTBWin_Cmd
    pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    call 'USER32.CheckMenuItem' D$LibScanMenuHandle, M03_Hover_ToolBar, eax
    SendMessage D$LibScanMenuHandle, &TB_AUTOSIZE, 0, 0

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

DecodeLib:   ; EncodeDecode
    mov B$WeAreInTheCodeBox &TRUE

        mov esi D$LibBytesCopy, edi D$LibDisassemblyPtr

      ; Disassemble: (DisMain)
        .While D$esi <> 0
            lodsd | mov edx esi | add edx eax

            While esi < edx
                push edx
                    mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
                    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
                    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE

L1:                 movzx eax B$esi | inc esi | call D$DisOp1+eax*4
                    On B$DisFlag = DISDONE, jmp L1<

                    mov W$edi CRLF | add edi 2
                pop edx
            End_While

            mov D$edi CRLF2 | add edi 4 | mov esi edx
        .End_While

        mov D$LibDisassemblyPtr edi

    mov B$WeAreInTheCodeBox &FALSE
ret




__________________________________________________________________________________________________________________



; Tag Dialog 20

LibScanner:
    call 'USER32.DialogBoxParamA' D$hinstance, IDD_MAINLIB, &NULL, ScanLibFile, &NULL
ret

____________________________________________________________________________________________

[IDD_LIBSCANWARNINGMSG 23]
[IDC_DISPLAYWARNINGMSG 10]
[IDC_DISPLAYWARNINGHEXA 20]

[IDB_LIBWARNING_BITMAP 200]
[IDC_LIBWARNING_SHOWICON 6]

[LibScanWarnMsg: B$ "The Magic Signature Tag was not found.

This is not a Object File supported by this dumper
__________________________________________________", 0]

[LibScanHexaMsg: 0 #256]
[LibWarningStaticImage: D$ 0]
[LibScanhIcon: D$ 0]

Proc LibScanWarning:
    Arguments @Adressee, @Message, @wParam, @lParam

     pushad


    ..If D@Message = &WM_INITDIALOG

        call 'user32.GetDlgItem' D@Adressee IDC_LIBWARNING_SHOWICON ; Get the handle of the Static Image
        mov D$LibWarningStaticImage eax
        call 'USER32.LoadBitmapA' D$hInstance, IDB_LIBWARNING_BITMAP
        mov D$LibScanhIcon eax

        call 'user32.SendMessageA' D$LibWarningStaticImage, &STM_SETIMAGE, &IMAGE_BITMAP, D$LibScanhIcon
        call 'USER32.SendDlgItemMessageA' D@Adressee IDC_DISPLAYWARNINGMSG &WM_SETTEXT 0 LibScanWarnMsg
        call 'USER32.SendDlgItemMessageA' D@Adressee IDC_DISPLAYWARNINGMSG &WM_GETTEXT 0 LibScanWarnMsg

        ; Calculate and Display File Size
        mov edi LibScanHexaMsg
        mov ecx D$LibFileLength
        mov esi D$LibFileMemory

        If D$LibFileLength < 80  ; Maximum amount of bytes
            mov ecx D$LibFileLength
        Else_If ecx = 0 ; This shouldn´t happen, because we already have a Zero Size check, but...security is never too much ;)
            mov ecx 1
        Else
            mov ecx 80
        End_If

        ; Convert the 1st 80 Bytes to Decimal String notation.
        xor eax eax
        mov edx 0
        Do
            lodsb
            call writeeax
            xor eax eax
            mov B$edi ' ' | inc edi
            dec ecx
        Loop_Until ecx = 0
        mov B$edi 0

        call 'USER32.SetDlgItemTextA' D@Adressee, IDC_DISPLAYWARNINGHEXA, LibScanHexaMsg

    ..Else_If D@Message = &WM_CLOSE
        call ClearBuffer LibScanHexaMsg, 256
        call 'User32.EndDialog' D@Adressee &NULL

    ..Else_If D@Message = &WM_COMMAND
        .If D@wParam = &IDOK
            call ClearBuffer LibScanHexaMsg, 256
            call 'User32.EndDialog' D@Adressee &NULL
         .End_If

    ..Else

       popad | mov eax &FALSE | ExitP

    ..End_If

    popad
    mov eax &TRUE
EndP

____________________________________________________________

[szBuff1: B$ 0 #256]

Proc ListViewLibSort:
    Arguments @lParam1, @lParam2, @lParamSort
    Local @Dir1, @Dir2
    Uses edi, ebx, esi, ecx, edx

;    call ClearCharMapData szBuff0, 256
;    call ClearCharMapData szBuff1, 256

    and D@Dir1 0
    and D@Dir2 0
    mov D$lvi.imask &LVIF_TEXT
    lea eax D$szBuff0
    mov D$lvi.pszText eax
    mov D$lvi.cchTextMax 256

    mov D$lvi.iSubItem 01
    call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
    xor eax eax
    mov al B$szBuff0
    mov D@Dir1 eax
    call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
    xor eax eax
    mov al B$szBuff0
    mov D@Dir2 eax

    ; Decimal Value [Header1: B$ 'Index',0]
    ..If_Or D@lParamSort = 01, D@lParamSort = 02

        mov D$lvi.iSubItem 0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call String2Dword szBuff0
        mov edi eax
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
        call String2Dword szBuff0

        If D@lParamSort = 1
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; Hexadecimal Value [Header2: B$ 'File Name',0] ; string
    ..Else_If_Or D@lParamSort = 3, D@lParamSort = 4

        mov D$lvi.iSubItem 1
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 3
            call lstrcmpi szBuff1, szBuff0
        Else
            call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header3: B$ 'Extension',0] ; string
    ..Else_If_Or D@lParamSort = 5, D@lParamSort = 6

        mov D$lvi.iSubItem 2
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 5
            call stricmp szBuff1, szBuff0
        Else
            call stricmp szBuff0, szBuff1
        End_If

    ; [Header4: B$ 'FileName Type',0] ; string
    ..Else_If_Or D@lParamSort = 7, D@lParamSort = 8

        mov D$lvi.iSubItem 3
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 7
            call stricmp szBuff1, szBuff0
        Else
            call stricmp szBuff0, szBuff1
        End_If

    ; [Header5: B$ 'Path',0] ; string
    ..Else_If_Or D@lParamSort = 9, D@lParamSort = 10

        mov D$lvi.iSubItem 4
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 9
            call lstrcmpi szBuff1, szBuff0
        Else
            call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header6: B$ 'Offset',0] ; hexadecimal value string
    ..Else_If_Or D@lParamSort = 11, D@lParamSort = 12

        mov D$lvi.iSubItem 5
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call AsciiBase szBuff0 BASE_HEX
        mov edi eax
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
        call AsciiBase szBuff0 BASE_HEX

        If D@lParamSort = 11
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If


    ; [Header7: B$ 'Size',0] ; decimal value
    ..Else_If_Or D@lParamSort = 13, D@lParamSort = 14

        mov D$lvi.iSubItem 6
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call String2Dword szBuff0
        mov edi eax
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
        call String2Dword szBuff0

        If D@lParamSort = 13
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header8: B$ 'Object Type',0] ; string
    ..Else_If_Or D@lParamSort = 15, D@lParamSort = 16

        mov D$lvi.iSubItem 7
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 15
            call stricmp szBuff1, szBuff0
        Else
            call stricmp szBuff0, szBuff1
        End_If

    ; [Header9: B$ 'Exported Function',0] ; string
    ..Else_If_Or D@lParamSort = 17, D@lParamSort = 18

        mov D$lvi.iSubItem 8
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 17
            call lstrcmpi szBuff1, szBuff0
        Else
            call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header10: B$ 'Machine Type',0] ; string
    ..Else_If_Or D@lParamSort = 19, D@lParamSort = 20

        mov D$lvi.iSubItem 9
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        call strcpy szBuff1, szBuff0
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi

        If D@lParamSort = 19
            call lstrcmpi szBuff1, szBuff0
        Else
            call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header11: B$ 'Sections',0] ; decimal value
    ..Else_If_Or D@lParamSort = 21, D@lParamSort = 22

        mov D$lvi.iSubItem 10
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ecti' ; "No Sections" string = 0
                mov eax 0
            Else
                call String2Dword szBuff0
            End_If
        mov edi eax
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ecti' ; "No Sections" string = 0
                mov eax 0
            Else
                call String2Dword szBuff0
            End_If
        If D@lParamSort = 21
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header12: B$ 'Time and Date',0] ; time and date string - convert it to dword
    ..Else_If_Or D@lParamSort = 23, D@lParamSort = 24

        mov D$lvi.iSubItem 11
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
        ;TimeDateStringtoDword
        call strcpy szBuff1, szBuff0
        mov edi eax
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
        call String2Dword szBuff0

        If D@lParamSort = 23
            call lstrcmpi szBuff1, szBuff0
        Else
            call lstrcmpi szBuff0, szBuff1
        End_If

    ; [Header13: B$ 'Symbols',0] ; decimal value
    ..Else_If_Or D@lParamSort = 25, D@lParamSort = 26
        mov D$lvi.iSubItem 12
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ymbo' ; "No Symbols" String = 0
                mov eax 0
            Else
                call String2Dword szBuff0
            End_If
        mov edi eax
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
            If_And D$szBuff0 = 'No S', D$szBuff0+4 = 'ymbo' ; "No Symbols" String = 0
                mov eax 0
            Else
                call String2Dword szBuff0
            End_If
        If D@lParamSort = 25
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header14: B$ 'Optional Header',0] ; string
    ..Else_If_Or D@lParamSort = 27, D@lParamSort = 28
        ; Optional Header can be only True or False.
        mov D$lvi.iSubItem 13
        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam1, lvi
            If D$szBuff0 = 'True'
                mov eax 1
            Else
                mov eax 0
            End_If
        mov edi eax

        call 'USER32.SendMessageA' D$hlist, &LVM_GETITEMTEXT, D@lParam2, lvi
            If D$szBuff0 = 'True'
                mov eax 1
            Else
                mov eax 0
            End_If

        If D@lParamSort = 27
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If



    ..End_If
;---------- [Keep the folders on top] ----------

    .If_And B@Dir1 = '<', B@Dir2 <> '<'
        xor eax eax
        dec eax
        ExitP
    .Else_If_And B@Dir2 = '<', B@Dir1 <> '<'
        xor eax eax
        inc eax
        ExitP
    .Else_If_And B@Dir1 = '<', B@Dir2 = '<'
        xor eax eax
    .End_If

EndP


________________________________________________________________________________________


; ToolBar Data and Functions

[LibScanMenuHandle: ?]
[LibScanDialog_ImageList: ?]
[IDB_LibScanEnableTB 30]
[IDB_LibScanDisableTB 31]
[LibScanToolbarHandle: D$ 0]

[LIBSCANDLG_TOOLBAR 400]

; Flow control buttons ; TBBUTTON Structure

[LibScanToolbarButtons:
    LSTB01.iBitmap: D$ 0
    LSTB01.idCommand: D$ M03_Open
    LSTB01.fsState: B$ &TBSTATE_ENABLED
    LSTB01.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB01._wPad1: W$ 0
    LSTB01.dwData: D$ 0
    LSTB01.iString: D$ 0

    LSTB02.iBitmap: D$ 1
    LSTB02.idCommand: D$ M03_Close
    LSTB02.fsState: B$ &TBSTATE_ENABLED
    LSTB02.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB02._wPad1: W$ 0
    LSTB02.dwData: D$ 0
    LSTB02.iString: D$ 0

    LSTB03.iBitmap: D$ 2
    LSTB03.idCommand: D$ M03_Save_Report
    LSTB03.fsState: B$ &TBSTATE_ENABLED
    LSTB03.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB03._wPad1: W$ 0
    LSTB03.dwData: D$ 0
    LSTB03.iString: D$ 0

    LSTB04.iBitmap: D$ 3
    LSTB04.idCommand: D$ M03_Single_Object_File
    LSTB04.fsState: B$ &TBSTATE_ENABLED
    LSTB04.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB04._wPad1: W$ 0
    LSTB04.dwData: D$ 0
    LSTB04.iString: D$ 0

    LSTB05.iBitmap: D$ 4
    LSTB05.idCommand: D$ M03_All_Object_Files
    LSTB05.fsState: B$ &TBSTATE_ENABLED
    LSTB05.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB05._wPad1: W$ 0
    LSTB05.dwData: D$ 0
    LSTB05.iString: D$ 0

    LSTB06.iBitmap: D$ 5
    LSTB06.idCommand: D$ M03_Build_Dis_File
    LSTB06.fsState: B$ &TBSTATE_ENABLED
    LSTB06.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB06._wPad1: W$ 0
    LSTB06.dwData: D$ 0
    LSTB06.iString: D$ 0

    LSTB07.iBitmap: D$ 6
    LSTB07.idCommand: D$ M03_Next_Tab
    LSTB07.fsState: B$ &TBSTATE_ENABLED
    LSTB07.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB07._wPad1: W$ 0
    LSTB07.dwData: D$ 0
    LSTB07.iString: D$ 0

    LSTB08.iBitmap: D$ 7
    LSTB08.idCommand: D$ M03_Previous_Tab
    LSTB08.fsState: B$ &TBSTATE_ENABLED
    LSTB08.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB08._wPad1: W$ 0
    LSTB08.dwData: D$ 0
    LSTB08.iString: D$ 0

    LSTB09.iBitmap: D$ 8
    LSTB09.idCommand: D$ M03_First_Tab
    LSTB09.fsState: B$ &TBSTATE_ENABLED
    LSTB09.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB09._wPad1: W$ 0
    LSTB09.dwData: D$ 0
    LSTB09.iString: D$ 0

    LSTB10.iBitmap: D$ 9
    LSTB10.idCommand: D$ M03_Last_Tab
    LSTB10.fsState: B$ &TBSTATE_ENABLED
    LSTB10.fsStyle: B$ &BTNS_AUTOSIZE
    LSTB10._wPad1: W$ 0
    LSTB10.dwData: D$ 0
    LSTB10.iString: D$ 0]


[LIBSCAN_TOOLBAR_STYLE &WS_CHILD__&WS_VISIBLE__&TBSTYLE_FLAT__&TBSTYLE_LIST__&TBSTYLE_AUTOSIZE__&TBSTYLE_TRANSPARENT__&TBSTYLE_TOOLTIPS__&CCS_TOP]

[LibScanShowTBText: ?]

[StrLibScanOpen:      'Open' 0
 StrLibScanClose:     'Close' 0
 StrLibScanSave:      'Save Report' 0
 StrLibScanSingleObj: 'Extract Single Object' 0
 StrLibScanAllObj:    'Extract All Objects' 0
 StrLibScanDIS:       'Build DIS File' 0
 StrLibScanNextTab:   'Goto Next tab' 0
 StrLibScanPrevTab:   'Goto Previous tab' 0
 StrLibScanFirstTab:  'Goto First tab' 0
 StrLibScanLastTab:   'Goto Last tab' 0]

[LibScanToolButtonsNumber 10]

[LibScanToolTipsStrings: StrLibScanOpen StrLibScanClose StrLibScanSave StrLibScanSingleObj
                         StrLibScanAllObj StrLibScanDIS StrLibScanNextTab StrLibScanPrevTab
                         StrLibScanFirstTab StrLibScanLastTab]


[LSTBWin_Cmd:
 LSTBWin_Cmd.bWidth: D$ 20
 LSTBWin_Cmd.dwStyle: D$ LIBSCAN_TOOLBAR_STYLE
 LSTBWin_Cmd.hMenu: D$ LIBSCANDLG_TOOLBAR
 LSTBWin_Cmd.hIml: D$ LibScanDialog_ImageList
 LSTBWin_Cmd.ShowTxtFlag: D$ LibScanShowTBText]

__________________________________________________________________________________________

; Tab Control Data and Functions

[IDB_LibScanTABEnable 40]
[IDB_LibScanTABDisable 41]

[LibScanTabControlsNumber 4]

; This structure is of the type TC_ITEM
[ts:
 ts.imask: D$ &TCIF_TEXT
 ts.lpReserved1: D$ 0
 ts.lpReserved2: D$ 0
 ts.pszText: D$ 0
 ts.cchTextMax: len
 ts.iImage: D$ 0-01
 ts.lParam: D$ 0]

[TC_ITEM.imaskDis 0
 TC_ITEM.lpReserved1Dis 4
 TC_ITEM.lpReserved2Dis 8
 TC_ITEM.pszTextDis 12
 TC_ITEM.cchTextMaxDis 16
 TC_ITEM.iImageDis 20
 TC_ITEM.lParamDis 24]

; Tab Dialog Messages and Data

[TabTitle1: B$ "Library Structure" 0
TabTitle1Len: D$ len]

[TabTitle2: B$ "Object Files" 0
TabTitle2Len: D$ len]

[TabTitle3: B$ "Disassembled Display" 0
TabTitle3Len: D$ len]

[TabTitle4: B$ "DIS Builder" 0
TabTitle4Len: D$ len]

[LibScanTabToolTipStrings: TabTitle1 TabTitle2 TabTitle3 TabTitle4]
[LibScanTabToolTipStringsLen: TabTitle1Len TabTitle2Len TabTitle3Len TabTitle4Len]

; CCTabOrganize Structure

[LibScanTabControl:
 LibScanTabControl.TCITEM: D$ ts
 LibScanTabControl.iMaskFlag: D$ &TCIF_TEXT__&TCIF_IMAGE
 LibScanTabControl.ImageList: D$ LibScanTab_ImageList
 LibScanTabControl.TabsAmount: D$ LibScanTabNumber
 LibScanTabControl.StringArray: D$ LibScanTabToolTipStrings
 LibScanTabControl.StringLenArray: D$ LibScanTabToolTipStringsLen]

[CCTabOrganize.TCITEMDis 0
 CCTabOrganize.iMaskFlagDis 4
 CCTabOrganize.ImageListDis 8
 CCTabOrganize.TabsAmountDis 12
 CCTabOrganize.StringArrayDis 16
 CCTabOrganize.StringLenArrayDis 20]

[Size_Of_CCTabOrganize 24]


[LibScanTabNumber: 4]

[hTab: D$ 0] ; Handle of the Tab Control in the resource, that is inside the main Dialog

[SelTab: D$ 0] ; Selection Tab Flag.

[hTabDlg1: D$ 0 ; Handle of the 1st Dialog MZ Header
 hTabDlg2: D$ 0 ; Handle of the 2nd Dialog PE Header
 hTabDlg3: D$ 0 ; Handle of the 3rd Dialog PE DataDirectory Header
 hTabDlg4: D$ 0] ; Handle of the 4th Dialog Image Section Headers

__________________________________________________________________________________________


; RunTime Functions


;;

    Concatenation
    
    This function concatenates (join) 02 strings, and save the result on a buffer.

 Parameters:
 
    Source1: The 1st string to be joined
  
    Source2: The 2nd string to be joined
    
    Destination: The outputed buffer where the new string is stored
 

 Usage Example:
 
 
    call Concatenation {"HY ! My name is",0} {"Guga", 0}  edi

_______________________________________________________________

  Author:
    
    RobotBob (Eric) - Original Author

;;


Proc Concatenation:
    Arguments @Source1, @Source2, @Destination
    Uses esi, edi

        mov esi D@Source1, edi D@Destination
        While B$esi <> 0 | movsb | End_While

        mov esi D@Source2
        While B$esi <> 0 | movsb | End_While

        movsb
EndP
____________________________________________________________________________________________________________

; Guga CodeView Equates



;;
#define MAKESIG(a,b,c,d)        ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))
#define CODEVIEW_NB09_SIG       MAKESIG('N','B','0','9')
#define CODEVIEW_NB10_SIG       MAKESIG('N','B','1','0')
#define CODEVIEW_NB11_SIG       MAKESIG('N','B','1','1')
#define CODEVIEW_RSDS_SIG       MAKESIG('R','S','D','S')

;;

[MAKESIG | (#1 or (#2 shl 8) or (#3 shl 16) or (#4 shl 24))]

[CODEVIEW_NB09_SIG 03930424E] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','9'] ; The result is reversed NB09
[CODEVIEW_NB10_SIG 03031424E] ; [CODEVIEW_NB10_SIG  MAKESIG 'N','B','1','0'] ; The result is reversed NB10
[CODEVIEW_NB11_SIG 03131424E] ; [CODEVIEW_NB11_SIG  MAKESIG 'N','B','1','1'] ; The result is reversed NB11
[CODEVIEW_RSDS_SIG 053445352] ; [CODEVIEW_RSDS_SIG  MAKESIG 'R','S','D','S'] ; The result is reversed RSDS


; weird. The real signature (pdb files) in hexadecimal is not reversed. It is the regular order, Like:
;;
[CODEVIEW_NB00_SIG 04E423030] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','0'] ; The result is NB00
[CODEVIEW_NB01_SIG 04E423031] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','1'] ; The result is NB01
[CODEVIEW_NB02_SIG 04E423032] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','2'] ; The result is NB02
[CODEVIEW_NB03_SIG 04E423033] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','3'] ; The result is NB03
[CODEVIEW_NB04_SIG 04E423034] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','4'] ; The result is NB04
[CODEVIEW_NB05_SIG 04E423035] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','5'] ; The result is NB05
[CODEVIEW_NB06_SIG 04E423036] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','6'] ; The result is NB06
[CODEVIEW_NB07_SIG 04E423037] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','7'] ; The result is NB07
[CODEVIEW_NB08_SIG 04E423038] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','8'] ; The result is NB08
[CODEVIEW_NB09_SIG 04E423039] ; [CODEVIEW_NB09_SIG  MAKESIG 'N','B','0','9'] ; The result is NB09
[CODEVIEW_NB10_SIG 04E423130] ; [CODEVIEW_NB10_SIG  MAKESIG 'N','B','1','0'] ; The result is NB10
[CODEVIEW_NB11_SIG 04E423131] ; [CODEVIEW_NB11_SIG  MAKESIG 'N','B','1','1'] ; The result is NB11
[CODEVIEW_RSDS_SIG 052534453] ; [CODEVIEW_RSDS_SIG  MAKESIG 'R','S','D','S'] ; The result is RSDS
;;


;;

Note to Guga:

It seems that when a Lib file is used by the linker, it inserts the whole object file that
uses a specific function that was called by the source.

For example, if we have a source file with a call to a external lib containing "Function01", and this lib file
have 45 different Object files. The only Object file that will be used and "linked" is the one containing the 
"Functino01" function. (Example, if this function exists only in Object 32.

What happens is that the whole Object32.obj (Inside that lib) is used and linked/injected inside to produce the new
application. This explains why some applications have too many nested data inside code. Because the Object file that
is injected is fixed. And it contains theit own Code and Data sectinos, but the final application interprets it as 
Code only to inject the function on the final code section.

Sometimes it don´ happens, because when you build your file, the 1st to be done is to use the linked object/libs,
and after it is used, the compiler translates the Source code to produce the new object file. So this new object
file is surrounding the fixed object files. Like:


1st step:

Source Code
 --- Fixed Object File (Code Section)
 ----Fixed Object File (Data Section)
 
 2nd step , Linking
 
 Resultant Object File from source file. (Code Section from the new Object file) --|
 --- Fixed Object File (Code Section)                                              |
 ----Fixed Object File (Data Section)                                              |
 Resultant Object File from source file. (Data Section from the new Object file) --|
 
 Or something Like this.
 
 So, what we can do for the DIS ?
 
 Maybe the better is we once we identify a specific function, we check which Object File (Inside or not a lib)
 it belongs to, and then we just need to disassemble that whole object file that is evidently inside the exe.

Bingo !!!
This is exactly this :):):)
Check on ml.exe

We will see this function:
_flsall at 0043E359

It belongs to a specific object file 394 inside that consists in only this function.

Immediatelly after this function we have another one:

__mbsdec at 0043E3C6

This one belongs to other object file. (On the same lib)

Also, check the function _memcpy it contains nested code and data, and no debug info, but the relocs, and symbols seems
to be enough to find what are the data and code part of it.

;;



;;
 Note to René

a) Assembler Errors

        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0

        mov edi RegContent | mov W$edi+1 'DX' | add edi 4
"       DwordToHex D$ebx+0A8
        call 'Kernel32.WriteFile' D@File, RegContent, 14, BytesTransfered, 0


When i try to assemble a copy of RoAsm2028b with RosAsm2028b, it keeps inserting a " char like above.
It causes rosasm to now assemble the file. The reason is because after assemblying it and RosAsm do
his job, it changes the lines of the source code and adds this extra ".


LibScannner

a) Errors: 

In several places of the whole RosAsm source i found a mismatch of size in CRLF displacement.
It was displaying this:

mov D$edi CRLF | add edi 2

When the correct is:

mov W$edi CRLF | add edi 2

Like in the Error Tab, here:

mov esi D@Text1, edi Trash1, ecx 0 | mov D$edi CRLF | add edi 2
(...)
mov esi D@Text2, edi Trash2, ecx 0 | mov D$edi CRLF | add edi 2


b) On file libgccguga.a, and on libgcc.a it was crashing due to an memory error.


I Fixed it increasing allocated memory here:

  ; Allocate mem for Strings ('CompletionTable')
  ; and for list of pointers to Strings ('CompletionPointers'):

    push ecx
        ;inc ecx | shl ecx 2 | or ecx 1
        inc ecx | shl ecx 5 | or ecx 1 ; -----------> Added this
        VirtualAlloc CompletionPointers ecx
    pop ecx

    ;shl ecx 5 | or ecx 1
    shl ecx 8 | or ecx 1 ; -------------------------> Added this
    VirtualAlloc CompletionTable ecx | inc D$CompletionTable

  ; Fill the Tables:
    mov edi D$CompletionTable, ebx D$CompletionPointers


Note: Although it is fixed, the same memory problem hapens on UAFXCWD.LIB
The file does not load completelly on the object listing and also don't load on the structure editor.
So, maybe we need to calculate and extend memory like in ReMapSourceMemoryIfNeeded
The reason is because the generated text file is something around 0C0D3407 bytes (202,20 Mb)

c) Even fixing the above memory problem. It still crashes on LIBCMTD.LIB
THis one i couldn't fix 

Proc SetQwordCheckSum:
    Argument @Pointer

    pushad

        mov esi D@Pointer

        If B$esi < '0'
            ;
        Else_If B$esi <= '9'
            error D$NumerAsSymbolPtr
        End_If

        call CheckSum64 | call NoDuplication D@Pointer | call CheckSum16

      ; The List Pointer is used to test empty Records (Lists Pointers can never be zero):
        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            mov D$CheckSumsRecords+ecx eax
            mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 D@Pointer
          ; D$CheckSumsRecords+ecx+12 = 0
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0 | mov edi D$edi+12 | End_While
                move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            mov edi D$PointerToCheckSumsLinkedRecords
            mov D$edi eax ; -----------------------------> Crashes here

d) Fixed Loading BookMarks when Loading the MRU files.

When i loaded a file through the MRU List, the bookmarks where not being loaded.
I fixed here (MainProc TAB):



            .Else_If eax > 3000             ; Most Recently Used Files.
                If eax < 3005
                    push eax
                        call Security       ; 'Security' also uses eax as return Value.
                    pop ebx
                    On eax = &IDCANCEL, jmp L9>>
                        mov eax ebx         ; ... and 'LoadRMUfile' as input.
                        call LoadRMUfile | call EnableMenutems | call LoadBookMarks ; added the call to LoadBookmarks


e) Small codecomplete error.

I added some new equates for CodeView 4, but if i start typing:

"&CV4_" and press control space an error messagebox shows up saying that it can´ find the equate and ask me to build a list.

But, if i type only: "&CV" and press control+space, the list displays properly

I added some equates like: &CV4_COMPILE_MACHINE_I80


f) RosAsm is crashing on Preparse Equal

If i write at the top of this lib "PREPARSE Equal" (After the Title) RosAsm crashes badly

g) Disassembler error on Floating Point Data.

Here:

[Data04D17F4: T$ 1.414214]
[<Data04D17FE: B$ 0, 0, 080, 07F  B$ 0, 0]
[Data04D1804: B$ 0, 048, 0C0, 0FF, F$ 0.5, F$ -1.443411e+035, F$ -4.952773e-007
                 F$ 2.295747e-041, F$ 4.573838e-041]
                 
Should be:

[Data04D17F4: T$ 1.414214]
[Data04D17FE: F$ 3.4028237e+038]
[Data04D1804: F$ -5.1117124e+038, F$ 0.5, F$ -1.443411e+035, F$ -4.952773e-007
                 F$ 2.295747e-041, F$ 4.573838e-041]

This is because all of them points to Fld instructions, like:

here:
    fld F$Data04D17FE
jmp E4>  ; Code04AAF8C
    
Code04AAF1B: J1:


and here:
    
Code04AAFD2: L4:
    fstp ST0
    fld F$Data04D1804
    
Code04AAFDA: M2:

h) Disassembler error on MMX mnemonic.

On testing.exe from C:\Guga\EvilHomerNewSite\site\stig.servehttp.com\homer\DirectX\Example1, ehit is being disasembled:

    cvtpq2ps XMM0 XMM2                ; 0F 5B C2 
    cvtpq2ps XMM2 XMM7                ; 0F 5B D7 
    mulps XMM0 XMM1                   ; 0F 59 C1 
    mulps XMM3 XMM2                   ; 0F 59 DA 
    pop ecx                           ; 59 
    ret                               ; C3 
    
Code040A4CC: B2:

But the correct is cvtdq2ps, like:

    cvtdq2ps XMM0 XMM2                ; 0F 5B C2 
    cvtdq2ps XMM2 XMM7                ; 0F 5B D7 
    mulps XMM0 XMM1                   ; 0F 59 C1 
    mulps XMM3 XMM2                   ; 0F 59 DA 
    pop ecx                           ; 59 
    ret                               ; C3 
    
Code040A4CC: B2:

I changed here:


cvtdq2ps

Op5B:
    .If B$EscapePrefix = &FALSE
        mov D$edi 'pop ' | add edi 4
        If B$OperandSizeOverride = &FALSE
            inc D$LikelyCode
            mov D$edi 'ebx ' | add edi 3
        Else
           ; inc D$UnLikelyCode
            mov W$edi 'bx' | add edi 2
        End_If

    .Else

        ;inc D$UnLikelyCode
;        mov D$edi 'cvtp'
        If B$OperandSizeOverride = &TRUE ; CVTPS2DQ xmm1, xmm2/m128
            mov D$edi 'cvtp'
            mov D$edi+4 's2dq'
        Else                             ; CVTDQ2PS xmm1, xmm2/m128
            mov D$edi 'cvtd'            ; Guga fix. It must be cvtd
            mov D$edi+4 'q2ps'          ; Guga Error Here. It is d2ps and not q2ps
        End_If
        mov B$edi+8 ' ' | add edi 9 | jmp Dis_xmm1__xmm2_m128

Also on this same file we have several problems of bad interpretations of code as data. It comes for example from here:

All of the Pointers are to Code and Not data, like "mov D$ecx  Code04124BB" ; "mov D$ecx+04 Code0411707" etc etc

Code0406E53: O7:
    call Code0406D6F
    test al 020
    mov ecx D$esp+04 | je Code040708C
    test al al | jns Code040708C
    test ah 01
    mov D$ecx  Data04124BB ; error here it is a pointer to code and not to data
    mov D$ecx+04 Data0411707 ; error here it is a pointer to code and not to data
    mov D$ecx+08 Data041277C ; error here it is a pointer to code and not to data
    mov D$ecx+0C Data040D957 ; error here it is a pointer to code and not to data
    mov D$ecx+014 Data0412510 ; error here it is a pointer to code and not to data
    mov D$ecx+018 Data0411778 ; error here it is a pointer to code and not to data
    mov D$ecx+01C Code04117DF
    mov D$ecx+020 Data04127FE ; error here it is a pointer to code and not to data
    mov D$ecx+024 Data0412549 ; error here it is a pointer to code and not to data
    mov D$ecx+028 Code0411CBF
    mov D$ecx+02C Data040DB89 ; error here it is a pointer to code and not to data
    mov D$ecx+030 Data0412BA6 ; error here it is a pointer to code and not to data
    mov D$ecx+034 Data0412BE9 ; error here it is a pointer to code and not to data
    mov D$ecx+038 Code0411CC4
    mov D$ecx+03C Code0411E4C
    mov D$ecx+040 Data041285D ; error here it is a pointer to code and not to data
    mov D$ecx+044 Data04125AD ; error here it is a pointer to code and not to data
    mov D$ecx+048 Data04116EB ; error here it is a pointer to code and not to data
    mov D$ecx+04C Data040C47C ; error here it is a pointer to code and not to data
    mov D$ecx+050 Data040C515 ; error here it is a pointer to code and not to data
    mov D$ecx+054 Code0412D8B
    mov D$ecx+058 Data0412CCD ; error here it is a pointer to code and not to data
    mov D$ecx+05C Data040C572 ; error here it is a pointer to code and not to data
    mov D$ecx+060 Data040C5B6 ; error here it is a pointer to code and not to data
    mov D$ecx+064 Data040C9E7 ; error here it is a pointer to code and not to data
    mov D$ecx+068 Data040DC58 ; error here it is a pointer to code and not to data
    mov D$ecx+06C Data040DCA8 ; error here it is a pointer to code and not to data
    mov D$ecx+070 Data040DD93 ; error here it is a pointer to code and not to data
    mov D$ecx+074 Data040CAAC ; error here it is a pointer to code and not to data
    mov D$ecx+078 Data040DE6F ; error here it is a pointer to code and not to data
    mov D$ecx+07C Data040DED2 ; error here it is a pointer to code and not to data
    mov D$ecx+080 Data040DF30 ; error here it is a pointer to code and not to data
    mov D$ecx+084 Data040DF89 ; error here it is a pointer to code and not to data
    mov D$ecx+088 Data0412DEF ; error here it is a pointer to code and not to data
    mov D$ecx+08C Code040CB18
    mov D$ecx+090 Data040CC4F ; error here it is a pointer to code and not to data
    mov D$ecx+094 Data040DFD0 ; error here it is a pointer to code and not to data
    mov D$ecx+098 Data040E078 ; error here it is a pointer to code and not to data
    mov D$ecx+09C Code040E1FF
    mov D$ecx+0A0 Data040F397 ; error here it is a pointer to code and not to data
    mov D$ecx+0A4 Data040F53C ; error here it is a pointer to code and not to data
    mov D$ecx+0A8 Code040FA70
    mov D$ecx+0AC Data040CCDC ; error here it is a pointer to code and not to data
    mov D$ecx+0B0 Data040CD6E ; error here it is a pointer to code and not to data
    mov D$ecx+0D8 Data041298F ; error here it is a pointer to code and not to data
    mov D$ecx+0CC Data041183E ; error here it is a pointer to code and not to data
    mov D$ecx+0C0 Data0412645 ; error here it is a pointer to code and not to data
    mov D$ecx+0B8 Data041268E ; error here it is a pointer to code and not to data
    mov D$ecx+0D0 Data0412A00 ; error here it is a pointer to code and not to data
    mov D$ecx+0C4 Data04118AF ; error here it is a pointer to code and not to data
    mov D$ecx+0DC Data040CDD2 ; error here it is a pointer to code and not to data
    mov D$ecx+0E0 Data040CE79 ; error here it is a pointer to code and not to data
    mov D$ecx+0D4 Data0412AC8 ; error here it is a pointer to code and not to data
    mov D$ecx+0BC Data04126D1 ; error here it is a pointer to code and not to data
    mov D$ecx+0C8 Data0411983 ; error here it is a pointer to code and not to data
    mov D$ecx+010 Data0410FBF ; error here it is a pointer to code and not to data
    mov D$ecx+0B4 Code040CF1E
    mov D$ecx+0E8 Code0412200
    mov D$ecx+0E4 Code0412340 | je H6>  ; Code040708C
    test ah 02 | je H6>  ; Code040708C
    mov D$ecx+03C Code0412072
    mov D$ecx+048 Code0410CDD
    mov D$ecx+098 Data040E14A ; error here it is a pointer to code and not to data
    mov D$ecx+09C Code040FB89
    mov D$ecx+060 Data040C7D2 ; error here it is a pointer to code and not to data
    
Code040708C: H6:
    test al 040 | je A0>  ; Code04070E0
    mov D$ecx+0F8 Code040BE40
    mov D$ecx+0FC Code040BEE0
    mov D$ecx+0100 Data040C200 ; error here it is a pointer to code and not to data
    mov D$ecx+0EC Data040B7C0 ; error here it is a pointer to code and not to data
    mov D$ecx+0F0 Data040BA20 ; error here it is a pointer to code and not to data
    mov D$ecx+0F4 Data040BC80 ; error here it is a pointer to code and not to data
    mov D$ecx+0104 Code040B700
    mov D$ecx+0110 Code040B640
    
Code04070E0: A0:
    ret


You will also note that even fixing this we have other errors like it may result in this (After fixing the Data to Code problem):

Code040C47C: M4:
    mov edx D$esp+0C
    mov eax D$esp+08
    movq MM2 Q$edx 
    movq MM3 Q$edx+08
    movq MM4 MM2
    movq MM5 MM3
    movq MM0 Q$eax 
    punpckhdq MM4 MM2
    punpckhdq MM5 MM3
    movq MM1 Q$eax+08
    punpckldq MM4 MM2
    punpckldq MM5 MM3
    mov eax D$esp+04
    movq MM6 MM4
    movq MM7 MM5
    psrlw MM6 Q$edi+ecx+089E02DEF ; Error Here
    inc ecx
    add B$edi  cl
    psubusb MM6 Q$edi+ecx+089E015EF
    inc ecx
    add B$edi  cl
    psubsb MM6 Q$edi+ecx+0FB4E10F
    psrlq MM3 Q$esi+0F1A6F0F
    pmulhw MM3 Q$edx+086A6F0F
    psubw MM6 Q$edi+ecx+089E035EF
    inc ecx
    add B$edi  cl
    psrad MM5 Q$esi+0B4F00F0F
    pxor MM5 Q$Data04189E0
    psubusb MM6 Q$edi+ecx+0F0F207F
jmp 0F74FD4B7

But the correct is:
Code040C47C: M4:

.text:0040C47C                 mov     edx, [esp+0Ch]
.text:0040C480                 mov     eax, [esp+8]
.text:0040C484                 movq    mm2, qword ptr [edx]
.text:0040C487                 movq    mm3, qword ptr [edx+8]
.text:0040C48B                 movq    mm4, mm2
.text:0040C48E                 movq    mm5, mm3
.text:0040C491                 movq    mm0, qword ptr [eax]
.text:0040C494                 punpckhdq mm4, mm2
.text:0040C497                 punpckhdq mm5, mm3
.text:0040C49A                 movq    mm1, qword ptr [eax+8]
.text:0040C49E                 punpckldq mm4, mm2
.text:0040C4A1                 punpckldq mm5, mm3
.text:0040C4A4                 mov     eax, [esp+4]
.text:0040C4A8                 movq    mm6, mm4
.text:0040C4AB                 movq    mm7, mm5
.text:0040C4AE                 pfmul   mm2, mm1
.text:0040C4B2                 pxor    mm5, ds:qword_4189E0
.text:0040C4B9                 pfmul   mm3, mm0
.text:0040C4BD                 pxor    mm2, ds:qword_4189E0
.text:0040C4C4                 pfmul   mm5, mm0
.text:0040C4C8                 pfmul   mm4, mm1
.text:0040C4CC                 pfadd   mm2, mm3
.text:0040C4D0                 movq    mm3, qword ptr [edx]
.text:0040C4D3                 pfsub   mm4, mm5
.text:0040C4D7                 movq    mm5, qword ptr [edx+8]
.text:0040C4DB                 pfmul   mm7, mm1
.text:0040C4DF                 pxor    mm6, ds:qword_4189E0
.text:0040C4E6                 pfacc   mm4, mm2
.text:0040C4EA                 pfmul   mm6, mm0
.text:0040C4EE                 pxor    mm5, ds:qword_4189E0
.text:0040C4F5                 pfmul   mm3, mm0
.text:0040C4F9                 movq    qword ptr [eax], mm4
.text:0040C4FC                 pfmul   mm5, mm1
.text:0040C500                 pfadd   mm6, mm7
.text:0040C504                 pfsub   mm5, mm3
.text:0040C508                 pfacc   mm6, mm5
.text:0040C50C                 movq    qword ptr [eax+8], mm6
.text:0040C510                 femms
.text:0040C512                 retn    0Ch
.text:0040C515 ; ---------------------------------------------------------------------------


;;




;;

Guga changes
a)

                    Else_If B$esi-1 = 0E9
                        test B$eax EVOCATED | jz L1>
                      ; 2 or more JMP Instructions to the same location:
                        ;or B$eax PUSH_EBP+NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        ; GugaNote This is a jmp call
                        or B$eax NODE+INSTRUCTION+ACCESSED+EVOCATED+LABEL
                        mov ebx eax | sub ebx D$RoutingMap | add ebx D$SectionsMap
                        mov B$ebx CODEFLAG
                        jmp L5>
                        
b)

This is for the JCC
    While edi < edx
        If B$esi > 3
            On B$edi = 0, mov B$edi CODEFLAG
            On B$edi = CODEFLAG, mov B$ebx INSTRUCTION+EVOCATED+LABEL+ACCESSED;+PUSH_EBP
        End_If
        inc esi, edi, ebx
    End_While
;;


;;

More errors.

On ntdll.dll (D:\RosAsm\dlls\NewDlls)

Here:

    mov D$eax+0BC es                  ; 8C 88 BC 00 00 00 
    mov D$eax+098 es                  ; 8C 98 98 00 00 00 
    mov D$eax+094 es                  ; 8C 80 94 00 00 00 
    mov D$eax+090 es                  ; 8C A0 90 00 00 00 
    mov D$eax+08C es                  ; 8C A8 8C 00 00 00 
    mov D$eax+0C8 es                  ; 8C 90 C8 00 00 00 
    mov D$eax  010007                 ; C7 00 07 00 01 00 
    push 01                           ; 6A 01 
    push eax                          ; 50 
    push D$ebp+08                     ; FF 75 08 
    call Code078468A5C                ; E8 29 C5 FD FF 
(...)
    call Code07848C4A0                ; E8 48 FF FF FF 
RtlRaiseStatus:: Code07848C558: P2:

The "es" is repeated. It dhould be:

    mov D$eax+0BC cs                  ; 8C 88 BC 00 00 00 
    mov D$eax+098 ds                  ; 8C 98 98 00 00 00 
    mov D$eax+094 es                  ; 8C 80 94 00 00 00 
    mov D$eax+090 fs                  ; 8C A0 90 00 00 00 
    mov D$eax+08C gs                  ; 8C A8 8C 00 00 00 
    mov D$eax+0C8 ss                  ; 8C 90 C8 00 00 00 
    mov D$eax  010007                 ; C7 00 07 00 01 00 
    push 01                           ; 6A 01 
    push eax                          ; 50 
    push D$ebp+08                     ; FF 75 08 
    call Code078468A5C                ; E8 29 C5 FD FF 
(...)
    call Code07848C4A0                ; E8 48 FF FF FF 
RtlRaiseStatus:: Code07848C558: P2:


The error was here:

WriteEffectiveAddressFromModRm:  ; 044 00_100_100
(...)

    .Else_If al = 2
        call StartEffectiveAddress | RmMask bl To al

        If al = 0      | mov D$edi 'eax+' | add edi 4
        Else_If al = 1 | mov D$edi 'ecx+' | add edi 4
        Else_If al = 2 | mov D$edi 'edx+' | add edi 4
        Else_If al = 3 | mov D$edi 'ebx+' | add edi 4
        Else_If al = 4 | call WriteFromSib
            ..If B$edi-1 <> '+'
                mov B$edi '+' | inc edi
            ..End_If
        Else_If al = 5 | mov D$edi 'ebp+' | add edi 4
        Else_If al = 6 | mov D$edi 'esi+' | add edi 4
        Else           | mov D$edi 'edi+' | add edi 4
        End_If

        ; call Writedis32 ; ----> Here was the error. The values of eax and ebx was changing
        push ebx | push eax | call Writedis32 | pop eax | pop ebx ; --> This was my preliminary fix.

    .Else ; bl = 3

The 1st fix i made was only a palleative way to retrieve back the values of eax and ebx in order to don´t change
the values of bl and al, but this is not the proper way to go, because we have several other functions that call to
this Writedis32 and also in some functions below or after it.

All of those surrounding functions changes the values, because they are not preserved. The functions are top-down, like:

WriteBase5dis32: ; starting address
    If B$edi-1 <> '+'
        mov B$edi '+' | inc edi
    End_If

WriteDis32:
    If B$AddressSizeOverride = &FALSE
        lodsd
    Else
        lodsw | and eax 0FFFF | call WriteEax | ret
    End_If

WriteDisRelative:
    mov D$LastCodeRef eax | On eax = 0, jmp L8>>

L0: If B$SimpleScan = &TRUE
        mov B$DisFlag DISDONE+DISLINEOVER | ret
    End_If

So i fix it all only adding a Proc macro and a use macro in all functinos and adding a call to the next function in sequence,
like this:


Proc WriteBase5dis32:
    Uses eax, ebx, ecx, edx
    
    If B$edi-1 <> '+'
        mov B$edi '+' | inc edi
    End_If
    call WriteDis32
EndP

Proc WriteDis32:
    Uses eax, ebx, ecx, edx

    If B$AddressSizeOverride = &FALSE
        lodsd
    Else
        lodsw | and eax 0FFFF | call WriteEax | ExitP;ret
    End_If
    call WriteDisRelative
EndP

Proc WriteDisRelative:
    ;Uses eax, ebx, ecx, edx ; Maybe not use eax here bccause of  | jnz WriteDisRelative
    Uses ebx, ecx, edx ; Maybe not use eax here

    mov D$LastCodeRef eax | On eax = 0, jmp L8>>

L0: If B$SimpleScan = &TRUE
        mov B$DisFlag DISDONE+DISLINEOVER | ExitP;ret
    End_If

L0: On B$WeAreInTheCodeBox = &TRUE, jmp L8>>

    sub eax D$DisImageBase | add eax D$SectionsMap

    On eax >= D$EndOfSectionsMap, jmp L8>>
    On eax <= D$SectionsMap, jmp L8>>

    mov B$ToJumpsTable &FALSE
    mov al B$eax | and al DATAFLAG+VIRTUALFLAG+IMPORTFLAG+CODEFLAG+CONSTANTFLAG

    ..If al = 0
        mov eax D$LastCodeRef

        mov ebx eax | sub ebx D$DisImageBase | add ebx D$SizesMap

            .If B$LeaInstruction = &TRUE
               ; or B$ebx POINTER | mov B$LeaInstruction &FALSE
            .Else_If W$edi-2 = 'B$'
                or B$ebx BYTE
                sub ebx D$SizesMap | add ebx D$SectionsMap | mov B$ebx DATAFLAG
            .Else_If W$edi-2 = 'W$'
                or B$ebx WORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov B$ebx DATAFLAG, B$ebx+1 DATAFLAG
            .Else_If W$edi-2 = 'D$'
                or B$ebx DWORD
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'F$'
                or B$ebx FP4
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS
            .Else_If W$edi-2 = 'R$'
                or B$ebx FP8
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS
            .Else_If W$edi-2 = 'T$'
                or B$ebx FP10
                sub ebx D$SizesMap | add ebx D$SectionsMap
                mov D$ebx FOURDATAFLAGS, D$ebx+4 FOURDATAFLAGS, D$ebx+6 FOURDATAFLAGS
            .Else
                jmp L8>>
               ; or B$ebx POINTER
            .End_If

    ..Else_If al = DATAFLAG
        mov eax D$LastCodeRef | call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        mov D$edi 'Data' | add edi 4 | jmp L8>>

    ..Else_If al = CODEFLAG
      ; Is it a call to a Jumps Table?
        mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$UserPeStart

        If W$eax = 025FF ; Code of jmp relative long
            mov ebx D$eax+2 | sub ebx D$DisImageBase | add ebx D$SectionsMap
            On ebx > D$EndOfSectionsMap, jmp L1>
            On ebx < D$SectionsMap, jmp L1>
                On B$ebx <> IMPORTFLAG, jmp L1>

                    mov B$ApiCommentWanted &TRUE, ebx D$eax+2, D$PtrToApiName ebx

                  ; On B$WithSymbolicsAnalyzes = &FALSE, mov B$ToJumpsTable &TRUE
;                    push eax
;                        mov al B$WithSymbolicsAnalyzes | xor al &TRUE
;                        mov B$ToJumpsTable al
;                        On B$WithoutJumpsTableCalls = &TRUE, mov B$ToJumpsTable &FALSE
;                    pop eax
;                    mov ebx D$eax+2 | jmp L5>>
        End_If

L1:     mov eax D$LastCodeRef
        sub eax D$DisImageBase | add eax D$RoutingMap
        test B$eax INSTRUCTION | jz L8>>
        or B$eax NODE+LABEL | mov D$edi 'Code' | add edi 4 | jmp L8>>

    ..Else_If al = VIRTUALFLAG
        mov eax D$LastCodeRef | call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        mov D$edi 'Virt', D$edi+4 'ual ' | add edi 7 | jmp L8>>

    ..Else_If al = CONSTANTFLAG
L1:     mov eax D$LastCodeRef | call StoreDisSize
        sub eax D$DisImageBase | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        mov D$edi 'Cons', D$edi+4 'tant', B$edi+8 ' ' | add edi 8 | jmp L8>>

    ..Else_If al = IMPORTFLAG
        mov ebx D$LastCodeRef
L5:     sub ebx D$DisImageBase | add ebx D$UserPeStart | mov ebx D$ebx
      ; May be a wrong pointing inside the .Import!
      ; Add a Pointer test!
        On ebx < D$ApiBuffer, ExitP;ret
        On ebx >= D$EndOfApiBuffer, ExitP;ret

        push esi

            On W$edi-2 = 'D$', sub edi 2

            mov esi ebx

            .If D$edi-4 = 'jmp '    ; Jumps Table?
                call WriteApiJmpTableLabel

                While B$esi <> 0 | movsb | End_While

            .Else_If D$edi-4 = 'all '  ; call api?
                call FlagNoReturnApi
                call FlagApiProcedures
                While B$esi <> 0 | movsb | End_While

          ; Other case: Either "mov eax D$ApiCall" or "mov D$eax ApiCall"
            .Else
                push edi
                    mov al "'"
                    While B$edi > LF
                        dec edi
                        On B$edi = '$', mov al 0
                    End_While
                pop edi
                mov esi ebx
                If al = 0
                    While B$esi <> '.' | inc esi | End_While | inc esi
                End_If
                While B$esi <> 0 | movsb | End_While
                On al = 0, dec edi

            .End_If
        pop esi

     ..End_If

    ExitP;ret
______________________________________________
; Note to René: All of this is for your Tests ?

L7:         pop ebx, eax, esi
            mov B$edi ':' | inc edi | NextDisLine

            mov D$edi 'jmp ' | add edi 4
        .End_If
ExitP;ret

       ; If B$ToJumpsTable = &FALSE
       ;     mov B$edi "'" | inc edi
       ; End_If
        push esi
            mov esi ebx | While B$esi <> 0 | movsb | End_While
        pop esi
      ;  mov eax D$edi-4 | or eax 0202020 | On eax = '.dll' | sub edi 4
      ;  mov B$edi '.' | inc edi
;      ; Switch to the real Import for reading the Pointer to Function Name:
;        sub ebx D$RoutingMap | add ebx D$UserPeStart
;        Test D$ebx 0_8000_0000 | jnz L6>
;            push esi
;                mov esi D$ebx | add esi 2 | add esi D$UserPeStart
;                If esi < D$UserPeStart
;D0:                 mov B$DisFlag DISFAILED | pop esi | ret
;                Else_If esi > D$UserPeEnd
;                    jmp D0<
;                End_If
;                While B$esi <> 0 | movsb | End_While
;            pop esi
;            If B$ToJumpsTable = &FALSE
;                mov B$edi "'" | inc edi
;            End_If
            ExitP;ret

;L6:     mov eax D$ebx | xor eax 0_8000_0000 | call WriteEax
;        If B$ToJumpsTable = &FALSE
;            mov B$edi "'" | inc edi
;        End_If
        ExitP;ret

    ..Else_If al = 0
        mov ebx D$LastCodeRef
        sub ebx D$DisImageBase | add ebx D$RoutingMap
        mov eax D$ebx
        If eax = 0
            sub ebx D$RoutingMap | add ebx D$SectionsMap
            mov B$ebx CONSTANTFLAG | jmp L1<<
        End_If
    ..End_If
_______________________________________

L8: On B$WeAreInTheCodeBox = &FALSE, jmp L8>
    On D$LibFileMemory = 0, jmp L8>
        push esi
            mov esi D$LastCodeRef
            .If esi > D$LibFileMemory
                mov eax D$LibFileMemory | add eax D$LibFileLength
                If esi < eax
                    While B$esi <> 0 | movsb | End_While
                   ; mov D$edi ' ; <', D$edi+4 '<<<<' | add edi 8
                Else
                    pop esi | jmp L8>
                End_If
            .Else
                pop esi | jmp L8>
            .End_If
        pop esi
        ExitP;ret

L8: ;On D$LastCodeRef = 0438E28, int3

    .If B$edi-1 = '+'
        If W$edi-3 = '*2'
            call TryWithIndice 2
        Else_If W$edi-3 = '*4'
            call TryWithIndice 4
        Else_If W$edi-3 = '*8'
            call TryWithIndice 8
        End_If
    .End_If

    If W$DisplacementFromLabel = 0
        mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SizesMap
        mov ebx D$SizesMap | add ebx 4
        On eax < ebx, jmp L8>
        On eax > D$EndOfSizesMap, jmp L8>
        test B$eax-4 FP8 | jz L8>
            sub D$LastCodeRef 4 | mov W$DisplacementFromLabel '+4'
    End_If

L8: push 0-1

    mov ebx D$LastCodeRef

L0: mov eax ebx | shr ebx 4 | and eax 0F
    add eax '0' | On eax > '9', add eax 7
    push eax
    cmp ebx 0 | ja L0<

    mov B$edi '0' | inc edi
L0: pop eax | cmp eax 0-1 | je L9>
    mov B$edi al | inc edi | jmp L0<

L9: If W$DisplacementFromLabel <> 0
        mov ax W$DisplacementFromLabel | stosw
        mov W$DisplacementFromLabel 0
    End_If

    mov eax D$LastCodeRef | sub eax D$DisImageBase | add eax D$SectionsMap

    .If eax < D$SectionsMap
        ;
    .Else_If eax < D$EndOfSectionsMap
        If B$eax = DATAFLAG
            sub eax D$SectionsMap | add eax D$RoutingMap | or B$eax LABEL+EVOCATED
        End_If
    .End_If

    On B$ApiCommentWanted = &TRUE, call WriteApiLabelComment
;ret
EndP



------------------------

i Also changed here:


BuildTruthAsciiTable:
    VirtualAlloc TruthAsciiTable 256
    mov edi D$TruthAsciiTable, al BADASCII, ecx 256 | rep stosb

    mov edi D$TruthAsciiTable, B$edi 0
    mov B$edi+LF GOODASCII, B$edi+CR GOODASCII,
        B$edi+0A7 GOODASCII,    ; $
        B$edi+025 GOODASCII,    ; %
        B$edi+0A9 GOODASCII,    ; ®
        B$edi+02F GOODASCII     ; /

    mov ebx 32, ecx 127;8
    While ebx =< ecx
        mov B$edi+ebx GOODASCII | inc ebx
    End_While

  ; 128 - 32 + 6 > 102
ret

Chars 127 and 128 are not Good Ascii
;;







;;
    lstrcmpi

    This function compare two Null Terminated Ascii strings (With the same size or not). It looks for the
    Language version of your system to order the Strings.
    The functionality is exactly the same as on lstrcmpiA inside Kernel32.dll.
    
    Arguments:
    
    String1:    First ascii null terminated to be inputed
    String2:    Second ascii null terminated to be inputed
    
    Return Values:
        If the Strings are the same, eax returns 0
        If the 1st String alphabetically preceeds the 2nd String, eax returns 0-1.
        If the 1st String is alphabetically located after the 2nd String, eax returns 1.
        
    Example of usage and their return values:

    call lstrcmpi {B$ "AB", 0} {B$ "AB", 0} ; Eax = 0
    call lstrcmpi {B$ "AB", 0} {B$ "CD", 0} ; Eax = 0-1
    call lstrcmpi {B$ "CD", 0} {B$ "AB", 0} ; Eax = 1
    call lstrcmpi {B$ "RosAsm", 0} {B$ "Assembly", 0} ; Eax = 1
    call lstrcmpi {B$ "Assembly", 0} {B$ "RosAsm", 0} ; Eax = 0-1
    
    Note: If you are using this function to organize (list) strings. The starting order in crescent sttrings.
    Like the following list of strings:
        
        Home, House, Indian, RosAsm, Slovak, Story

    If you are trying to build a list of strings without having to use the Languiage definitions of your system, use
    the functions:
    
    strcmp - The same functionality as this lstrcmpi, but built on a simplified way to not use the Lang definitions.
             It will displays the order of strings in crescent order.

    stricmp - The same functionality as this lstrcmpi, but built on a simplified way to not use the Lang definitions.
             It will displays the order of strings in decrescent order.
;;

Proc lstrcmpi:
    Arguments @String1, @String2
    uses ebx, esi, ecx, edx

    call 'kernel32.GetThreadLocale'
    call 'kernel32.CompareStringA' eax, &LOCALE_ILANGUAGE__&LOCALE_USE_CP_ACP, D@String1, 0-01, D@String2, 0-1

    ...If eax = 0

        call 'kernel32.GetSystemDefaultLCID'
        call 'kernel32.CompareStringA' eax, &LOCALE_ILANGUAGE__&LOCALE_USE_CP_ACP, D@String1, 0-01, D@String2, 0-1

        ..If eax = 0

            .If D@String1 <> 0

                If D@String2 <> 0
                    call stricmp D@String1, D@String2
                Else_If D@String1 <> 0 ; There is an error on the original dll. This may be eax and not String1
                    mov eax 1
                Else
                    mov eax D@String2
                    neg eax
                    sbb eax eax
                End_If
                ExitP

            .End_If

            mov eax D@String2
            neg eax
            sbb eax eax
            ExitP

        ..End_If

    ...End_If

    add eax 0-02

EndP





;;
    Stricmp

    This function compare two Null Terminated Ascii strings (With the same size or not). It is a simplified version
    of lstrcmpi from kernel32.dll, with the diference that in this function it does not look for the
    Language version of your system, and also it reverses the order if a user is building a List of Strings.
    For example, on lstrcmpi the initial order of a String Listings is crescent. On this function the starting order
    is decrescent.
    The functionality is exactly the same as on _stricmp and _strcmpi inside ntdll.dll.
    It handle only latin alphabet without accents. So, only vowels and consonants from A to Z.
    
    To display a list of files in crescent alphabetical order, use Strcmp function.
    
    Arguments:
    
    String1:    First ascii null terminated to be inputed
    String2:    Second ascii null terminated to be inputed
    
    Return Values:
        If the Strings are the same, eax returns 0
        If the 1st String alphabetically preceeds the 2nd String, eax returns 1.
        If the 1st String is alphabetically located after the 2nd String, eax returns 0-1.
        
    Example of usage and their return values:
        
    call stricmp {B$ "AB", 0} {B$ "AB", 0} ; Eax = 0
    call stricmp {B$ "AB", 0} {B$ "CD", 0} ; Eax = 1
    call stricmp {B$ "CD", 0} {B$ "AB", 0} ; Eax = 0-1
    call stricmp {B$ "RosAsm", 0} {B$ "Assembly", 0} ; Eax = 0-1
    call stricmp {B$ "Assembly", 0} {B$ "RosAsm", 0} ; Eax = 1
    
    Note: Used Macros. Default RosAsm macro system, plus:
    
    [If_And    | If #1 #2 #3    | #+3]
    [.If_And   | .If #1 #2 #3   | #+3]
    [..If_And  | ..If #1 #2 #3  | #+3]
    [...If_And | ...If #1 #2 #3 | #+3]

    [Else_If_And    | Else    | If_And    #F>L]
    [.Else_If_And   | .Else   | .If_And   #F>L]
    [..Else_If_And  | ..Else  | ..If_And  #F>L]
    [...Else_If_And | ...Else | ...If_And #F>L]

    If you are using this function to organize (list) strings. The starting order in decrescent strings.
    Like the following list of strings:
        
        Story, Slovak, RosAsm, Indian, House, Home

;;

Proc stricmp:
    Arguments @String1, @String2
    Uses edi, esi, ebx

    mov esi D@String2
    mov edi D@String1
    mov al 0FF


    .Do
        Do
            On al = 0, jmp L2>
            mov al B$esi
            inc esi
            mov ah B$edi
            inc edi
        Loop_Until ah <> al

        ; Lower case convertion
        If_And al >= 'A', al <= 'Z'
            xor al 32
        End_If

        If_And ah >= 'A', ah <= 'Z'
            xor ah 32
        End_If

    .Loop_Until al <> ah
    sbb al al
    sbb al 0-1
L2:
    movsx eax al

EndP

____________________________________________________________________________________________

;;
    String2Dword

    This function converts a Null Terminated Decimal Ascii String to a 32 Bits Dword Value.
    
    Arguments:
    
    String:    The inputed String to be converted
    
    Return Values:
        If the function suceeds eax returns the value of the dword string.
        If the function fails eax returns 0.
;;

Proc String2Dword:
    Arguments @String
    Local @Result
    Uses ecx, edi, edx, esi

    mov D@Result 0
    mov edi D@String
    mov ecx D@String
    On ecx = 0, ExitP

    call StrLenProc edi

    .While B$ecx <> 0

        xor edx edx
        mov dl B$edi
        sub dl '0'  ; subtrack each digit with "0" to convert it to hex value
        mov esi eax
        dec esi
        push eax
            mov eax edx
            push ebx
                mov ebx 10;0A
                While esi > 0
                    mul ebx
                    dec esi
                End_While
            pop ebx
            add D@Result eax
        pop eax
        inc edi
        dec eax
        inc ecx
    .End_While
    mov eax D@Result

EndP
____________________________________________________________________________________________


;;
    Strcmp

    This function compare two Null Terminated Ascii strings (With the same size or not). It is a simplified version
    of lstrcmpi from kernel32.dll, with the diference that in this function it does not look for the
    Language version of your system. It also displays the same order if a user is building a List of Strings.
    For example, on lstrcmpi the initial order of a String Listings is crescent. On this function the starting order
    is also crescent.
    It handle only latin alphabet without accents. So, only vowels and consonants from A to Z.
    
    To display a list of files in decrescent alphabetical order, use Stricmp function.
        
    Arguments:
    
    String1:    First ascii null terminated to be inputed
    String2:    Second ascii null terminated to be inputed
    
    Return Values:
        If the Strings are the same, eax returns 0
        If the 1st String alphabetically preceeds the 2nd String, eax returns 0-1.
        If the 1st String is alphabetically located after the 2nd String, eax returns 1.
        
    Example of usage and their return values:
        
    call strcmp {B$ "AB", 0} {B$ "AB", 0} ; Eax = 0
    call strcmp {B$ "AB", 0} {B$ "CD", 0} ; Eax = 0-1
    call strcmp {B$ "CD", 0} {B$ "AB", 0} ; Eax = 1
    call strcmp {B$ "RosAsm", 0} {B$ "Assembly", 0} ; Eax = 1
    call strcmp {B$ "Assembly", 0} {B$ "RosAsm", 0} ; Eax = 0-1
    
    Note: Used Macros. Default RosAsm macro system, plus:
    
    [If_And    | If #1 #2 #3    | #+3]
    [.If_And   | .If #1 #2 #3   | #+3]
    [..If_And  | ..If #1 #2 #3  | #+3]
    [...If_And | ...If #1 #2 #3 | #+3]

    [Else_If_And    | Else    | If_And    #F>L]
    [.Else_If_And   | .Else   | .If_And   #F>L]
    [..Else_If_And  | ..Else  | ..If_And  #F>L]
    [...Else_If_And | ...Else | ...If_And #F>L]

    Note: If you are using this function to organize (list) strings. The starting order in crescent sttrings.
    Like the following list of strings:
        
        Home, House, Indian, RosAsm, Slovak, Story

;;

Proc strcmp:
    Arguments @String1, @String2
    Uses edi, esi, ebx

    mov esi D@String1
    mov edi D@String2
    mov al 0FF


    .Do
        Do
            On al = 0, jmp L2>
            mov al B$esi
            inc esi
            mov ah B$edi
            inc edi
        Loop_Until ah <> al

        ; Lower case convertion
        If_And al >= 'A', al <= 'Z'
            xor al 32
        End_If

        If_And ah >= 'A', ah <= 'Z'
            xor ah 32
        End_If

    .Loop_Until al <> ah
    sbb al al
    sbb al 0-1
L2:
    movsx eax al

EndP


[szBuff0: B$ ? #256]

[BASE_HEX 16, BASE_DEC 10, BASE_OCT 8, BASE_FOUR 4, BASE_BIN 2]

Proc AsciiBase:
    Arguments @String, @Base
    Uses esi, ebx, edx

        mov esi D@String, eax 0, ebx 0

        While B$esi <> 0
            mov eax ebx | mul D@Base | mov ebx eax, eax 0
            mov al B$esi | sub al '0'
          ; Cases of Hexa Notation:
            On al > 9, sub al 7
            add ebx eax | inc esi
        End_While

        mov eax ebx
EndP

;;
    Strcpy

    This function copies a Null Terminated String to a Buffer. The size of the Buffer must be bigger or equal to the
    size of the string.
    It have the same functionality as lstrcpyA in kernel32.dll
    
    Arguments:
    
    InBound:    The inputed String
    
    OutBound:   The Buffer which will hold the String.
    
;;

Proc strcpy:
    Arguments @OutBound, @InBound
    Uses ecx, esi, edi

    xor ecx ecx
    mov esi D@InBound

    Do
        inc ecx
        inc esi
    Loop_Until B$esi = 0

    mov esi D@InBound | mov edi D@OutBound | rep movsb
    mov B$edi 0

EndP
____________________________________________________________________________________________













































