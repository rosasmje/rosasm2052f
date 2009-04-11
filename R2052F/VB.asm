TITLE VB

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Special case of VB compiled PEs. The first Instructions of a VB PE are:
  
  > push Vb6Header
  > call 'MSVBVM50.064'
  
  The VB Header is a Structure that is saved in Code, and that discribes the
  organisation of the File:
  
  [VB6HEADER:|Vbsignature B 4|CompilerVersion W|SzLangDLL B 14|SzSecLangDLL B 14|
  RuntimeDLLVersion W|LanguageID D|BackupLanguageID D|
  >>> SubMain <<< D| >>> ProjectInfo <<< D|
  Flag2 W|Flag3 W|Flag4 D|ThreadSpace B|Const1 B|Flag5 W|Flag6 W|Flag7 W|FormCount W|
  ExternalComponentCount W|Flag8 B|Flag9 B|Flag10 W|DialogStructureGUI D|
  ExternalComponentTable D|Project D|ProjectExename D|ProjectTitle D|HelpFile D|
  ProjectName D|Flag11 D|Flag12 D|Flag13 D|Flag14 W
  
  
  Main Routine: 'MarkVbPe'
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; VB6HEADER Structure (Displacements Equates):

[VBdis.VbsignatureDis 0
 VBdis.CompilerVersionDis 4
 VBdis.SzLangDLLDis 6
 VBdis.SzSecLangDLLDis 20
 VBdis.RuntimeDLLVersionDis 34
 VBdis.LanguageIDDis 36
 VBdis.BackupLanguageIDDis 40
 VBdis.SubMainDis 44
 VBdis.ProjectInfoDis 48
 VBdis.Flag2Dis 52
 VBdis.Flag3Dis 54
 VBdis.Flag4Dis 56
 VBdis.ThreadSpaceDis 60
 VBdis.Const1Dis 61
 VBdis.Flag5Dis 62
 VBdis.Flag6Dis 64
 VBdis.Flag7Dis 66
 VBdis.FormCountDis 68
 VBdis.ExternalComponentCountDis 70
 VBdis.Flag8Dis 72
 VBdis.Flag9Dis 73
 VBdis.Flag10Dis 74
 VBdis.DialogStructureGUIDis 76
 VBdis.ExternalComponentTableDis 80
 VBdis.ProjectDis 84
 VBdis.ProjectExenameDis 88
 VBdis.ProjectTitleDis 92
 VBdis.HelpFileDis 96
 VBdis.ProjectNameDis 100
 VBdis.Flag11Dis 104
 VBdis.Flag12Dis 108
 VBdis.Flag13Dis 112
 VBdis.Flag14Dis 116
 VB_HEADER_LEN 118]


; For flaging the 'SizesMap':

 [VBsizes:
 VBsizes.Vbsignature: B$ BYTE+STRINGS #4
 VBsizes.CompilerVersion: W$ WORD
 VBsizes.SzLangDLL: B$ BYTE #14
 VBsizes.SzSecLangDLL: B$ BYTE #14
 VBsizes.RuntimeDLLVersion: W$ WORD
 VBsizes.LanguageID: D$ DWORD
 VBsizes.BackupLanguageID: D$ DWORD
 VBsizes.SubMain: D$ DWORD
 VBsizes.ProjectInfo: D$ DWORD
 VBsizes.Flag2: W$ WORD
 VBsizes.Flag3: W$ WORD
 VBsizes.Flag4: D$ DWORD
 VBsizes.ThreadSpace: B$ BYTE
 VBsizes.Const1: B$ BYTE
 VBsizes.Flag5: W$ WORD
 VBsizes.Flag6: W$ WORD
 VBsizes.Flag7: W$ WORD
 VBsizes.FormCount: W$ WORD
 VBsizes.ExternalComponentCount: W$ WORD
 VBsizes.Flag8: B$ BYTE
 VBsizes.Flag9: B$ BYTE
 VBsizes.Flag10: W$ WORD
 VBsizes.DialogStructureGUI: D$ DWORD
 VBsizes.ExternalComponentTable: D$ DWORD
 VBsizes.Project: D$ DWORD
 VBsizes.ProjectExename: D$ DWORD
 VBsizes.ProjectTitle: D$ DWORD
 VBsizes.HelpFile: D$ DWORD
 VBsizes.ProjectName: D$ DWORD
 VBsizes.Flag11: D$ DWORD
 VBsizes.Flag12: D$ DWORD
 VBsizes.Flag13: D$ DWORD
 VBsizes.Flag14: W$ WORD]


; For Flaging the 'RoutingMap':

 [VBrouting:
 VBrouting.Vbsignature: D$ EVOCATED+LABEL
 VBrouting.CompilerVersion: W$ EVOCATED+LABEL
 VBrouting.SzLangDLL: B$ 0 #14
 VBrouting.SzSecLangDLL: B$ 0 #14
 VBrouting.RuntimeDLLVersion: W$ EVOCATED+LABEL
 VBrouting.LanguageID: D$ EVOCATED+LABEL
 VBrouting.BackupLanguageID: D$ EVOCATED+LABEL
 VBrouting.SubMain: D$ EVOCATED+LABEL+POINTER ; >>> Flag the pointed to Code later
 VBrouting.ProjectInfo: D$ EVOCATED+LABEL
 VBrouting.Flag2: W$ EVOCATED+LABEL
 VBrouting.Flag3: W$ EVOCATED+LABEL
 VBrouting.Flag4: D$ EVOCATED+LABEL
 VBrouting.ThreadSpace: B$ EVOCATED+LABEL
 VBrouting.Const1: B$ EVOCATED+LABEL
 VBrouting.Flag5: W$ EVOCATED+LABEL
 VBrouting.Flag6: W$ EVOCATED+LABEL
 VBrouting.Flag7: W$ EVOCATED+LABEL
 VBrouting.FormCount: W$ EVOCATED+LABEL
 VBrouting.ExternalComponentCount: W$ EVOCATED+LABEL
 VBrouting.Flag8: B$ EVOCATED+LABEL
 VBrouting.Flag9: B$ EVOCATED+LABEL
 VBrouting.Flag10: W$ EVOCATED+LABEL
 VBrouting.DialogStructureGUI: D$ EVOCATED+LABEL+POINTER
 VBrouting.ExternalComponentTable: D$ EVOCATED+LABEL
 VBrouting.Project: D$ EVOCATED+LABEL
 VBrouting.ProjectExename: D$ EVOCATED+LABEL
 VBrouting.ProjectTitle: D$ EVOCATED+LABEL
 VBrouting.HelpFile: D$ EVOCATED+LABEL
 VBrouting.ProjectName: D$ EVOCATED+LABEL
 VBrouting.Flag11: D$ EVOCATED+LABEL
 VBrouting.Flag12: D$ EVOCATED+LABEL
 VBrouting.Flag13: D$ EVOCATED+LABEL
 VBrouting.Flag14: W$ EVOCATED+LABEL]


; ToDo: Implied SectionsMap Flags. Example, 'SubMain': At the DWORD >>> CODE

____________________________________________________________________________________________
____________________________________________________________________________________________

MarkVbPe:
    mov eax D$DisEntryPoint | sub eax D$DisImageBase | add eax D$UserPeStart
    mov esi D$eax+1 | sub esi D$DisImageBase | add esi D$UserPeStart
    On D$esi <> 'VB5!', ret

  ; Mark the VB Header Flags in the Tables. First, the 'SectionsMap' to Data:
    mov edi esi | sub edi D$UserPeStart | add edi D$SectionsMap
    mov ecx VB_HEADER_LEN, al DATAFLAG | rep stosb

  ; Routing Map according 'VB6HeaderRouting':
    mov eax esi | sub eax D$UserPeStart | add eax D$RoutingMap
    mov B$eax ACCESSED+LABEL
    push esi
        mov edi eax,  esi VBrouting, ecx VB_HEADER_LEN | rep movsb
    pop esi

  ; Sizes Map according 'VB6HeaderSizes':
    sub eax D$RoutingMap | add eax D$SizesMap
    push esi
        mov edi eax,  esi VBSizes, ecx VB_HEADER_LEN | rep movsb
    pop esi

    add esi VBdis.SubMainDis

    mov eax D$esi | sub eax D$DisImageBase | add eax D$SectionsMap
    If eax < D$SectionsMap
        ; nop (There are VB PEs without any direct Code inside).
    Else_If eax < D$EndOfSectionsMap
        mov B$eax CODEFLAG
        sub eax D$SectionsMap | add eax D$RoutingMap
        mov B$eax INSTRUCTION+EVOCATED+LABEL+ACCESSED+PUSH_EBP
    End_If

    add esi VBdis.ProjectInfoDis ; >>> ProjectInfo Data

    mov eax D$esi | sub eax D$DisImageBase | add eax D$SectionsMap
    If eax < D$SectionsMap
        ; nop (There are VB PEs without any direct Code inside).
    Else_If eax < D$EndOfSectionsMap
        mov B$eax DATAFLAG
        sub eax D$SectionsMap | add eax D$RoutingMap
        mov B$eax LABEL+ACCESSED
    End_If
ret










