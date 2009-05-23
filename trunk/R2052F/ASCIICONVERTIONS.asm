TITLE ASCIICONVERTIONS
_________________________________________________________________________________________________________
_________________________________________________________________________________________________________



;;

    ASCII CONVERTION FUNCTIONS AND OTHERS RELATED v 2.1 (September 2.008).
    
    
    Convert decimal Ascii to Hexadecimal values:
    
        Asciito8, Asciito16, Asciito32, Asciito64, Asciito80, Asciito96, Asciito112, Asciito128
        Asciito224, Asciito752, Asciito864, Asciito4096
        AsciitoAnyBit

    Convert Hexadecimal to string:
    
        HextoBinaryString, qWordToAscii, QwordtoST0
        
        Dword32toAscii, Dword64toAscii, Dword80toAscii, Dword96toAscii

    General Usage:
    
        DwordMultiplier, DwordDivider, BitDigitConverter, Ones32, AllNegativeConverter


Reference:

The decimal ascii functions were created upon _atoi64 from ReactOS project.

More info about the C sources can be found here:

    http://www.koders.com/c/fid983E079B992010C00CAACE5FC41E8448118A6630.aspx
    http://www.reactos.org/generated/doxygen/d4/d42/string_2atoi64_8c.html
    http://www.koders.com/noncode/fid2CADAC57E6FBE0330B1E26CFA55D4AD2795CF150.aspx
    http://www.koders.com/noncode/fidCB2CAFDA546551CB6C992848ECEA50D7B2B90CC4.aspx
    http://www.koders.com/noncode/fid3AD9D6D41D3B67C98711305E17F5C38AABE91001.aspx
    

Older versions:

- v 2.0 (April 2.007)

Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org
;;


_________________________________________________________________________________________________________


_________________________________________________________________________________________________________


_________________________________________________________________________________________________________

;;
    DwordMultiplier v 2.0
    
    This function multiplies a array of Dwords by ten.
    
    
    Arguments:
    
    Array:  (Input/Output) Pointer to a array of dwords where each element will be multiplied by 10.
            This procedure can be used as a part of Ascii String convertions.
            
            On output, each member of the array contains the multiplied value.


    ArrCounter:     The total amount of elements of the array
            
  
    Returned Values: None
    
    Remarks:    This function can be used to convert ascii decimal values to any bit size hexadecimal values.
                Each element of the array is a 32Bit dword value. So you can compute multiples of 32 Bits, like
                decimal ascii to 32 bit, ascii to 64, ascii to 96, ascii to 128, ascii to 512 and so on.
                You only need to remember the following rule:

                ArrCounter = 1 => 32 Bits (1*32)
                ArrCounter = 2 => 64 Bits (2*32)
                ArrCounter = 3 => 96 Bits (3*32)
                ArrCounter = 4 => 128 Bits (4*32)
                ArrCounter = n => XXX Bits (n*32)
                

                For further informations, see functions: AsciitoAnyBit, Atoi32, Atoi64, Atoi96, Atoi128
                
                The DwordMultiplier function does similar operations as allmul function with the difference that
                while allmul function only multiplies 2 unsigned dwords (1 Qword) integer by 10, the DwordMultiplier
                function multiplies XXX dwords by ten.


    Example of usage:
    
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0
         Value.Conv196Bit: D$ 0
         Value.Conv224Bit: D$ 0
         Value.Conv256Bit: D$ 0]

        [NUMBER_OF_ELEMENTS 8]
        
         call DwordMultiplier Value, NUMBER_OF_ELEMENTS

Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org
;;


Proc DwordMultiplier:
    Arguments @Array, @ArrCounter
    Local @PreviousProduct


    mov esi D@Array ; pointer to an array of dwords

    xor eax eax ; always initialize eax 0

    mov ecx 10  ; Multiplicand value. Ecx can be used as an Base for ascii convertions.
                ; So, if you are converting an decimal ascii string, use ecx = 10
                ; If you are converting an octal, use ecx = 8

    mov edi D@ArrCounter   ; edi is used as a counter for the next loop.
                           ; It counts from the last element of the array to zero.

    lea ebx D$edi*4-4      ; Ebx contains the size of the last element of the array.
                           ; This is because the loop routine always starts pointing from
                           ; the last element to the 1st one. (Bottom to Top of the array)

    L1:
        mov D@PreviousProduct eax   ; Copy to the product of the last multiplication to the variable PreviousProduct
                                    ; that will be added to the remainder (overflown stored in edx) of the multiplication.

        mov eax D$esi+ebx   ; ebx contains the value of the member of the array Value where we are currently
                            ; pointing to, to it be multiplied by 10.
                            ; On every loop we multiply from the last element to the previous one. So, if our
                            ; array have 4 elements. On the 1st loop eax will have the value of the 4th array,
                            ; on the next loop ebx wil have the value of the 3rd array and so on.

        mul ecx             ; multiply the value by 10

        mov D$esi+ebx eax   ; Copy the product of the multiplicaiton to the element of the array we are currently
                            ; pointing to.
        add edx D@PreviousProduct   ; add the product of the previous multplication to the remainder (overflown, if existant)

        ; We are going to copy the remainder to the next element of the array, but we need to make sure that
        ; we are not copying it outside the limits of the array. So, avoid copyinf it to an element that does not exist.
        cmp edi D@ArrCounter | je L0> ; Are we at the last element of the array ? If yes, jump over (It happens whenever we are on the 1st loop)
            mov D$esi+ebx+4 edx        ; copy the remainder to the next element of the array, otherwise.
        L0:

        ; ebx now must contains the size of the previous element of the array.
        ; We are doing a bottom to top operation, remember ?
        ; since the array is formed by dwords, we subtract ebx by 4.

        sub ebx 4
    dec edi | jne L1<   ; decrease our counter and see if we ended it. So, we see if our counter reached 0.
                        ; We are entering on a loop whenever the counter is not zeroed.

EndP


Proc DwordDivider:
    Arguments @Array, @ArrCounter
    Local @PreviousProduct


    mov esi D@Array ; pointer to an array of dwords

    xor eax eax ; always initialize eax 0
    xor edx edx

    mov ecx 10  ; Multiplicand value. Ecx can be used as an Base for ascii convertions.
                ; So, if you are converting an decimal ascii string, use ecx = 10
                ; If you are converting an octal, use ecx = 8

    mov edi D@ArrCounter   ; edi is used as a counter for the next loop.
                           ; It counts from the last element of the array to zero.

    lea ebx D$edi*4-4      ; Ebx contains the size of the last element of the array.
                           ; This is because the loop routine always starts pointing from
                           ; the last element to the 1st one. (Bottom to Top of the array)

    L1:
        mov D@PreviousProduct eax   ; Copy to the product of the last multiplication to the variable PreviousProduct
                                    ; that will be added to the remainder (overflown stored in edx) of the multiplication.

        mov eax D$esi+ebx   ; ebx contains the value of the member of the array Value where we are currently
                            ; pointing to, to it be multiplied by 10.
                            ; On every loop we multiply from the last element to the previous one. So, if our
                            ; array have 4 elements. On the 1st loop eax will have the value of the 4th array,
                            ; on the next loop ebx wil have the value of the 3rd array and so on.

        div ecx             ; divide the value by 10

        mov D$esi+ebx eax   ; Copy the product of the multiplicaiton to the element of the array we are currently
                            ; pointing to.
        ;add edx D@PreviousProduct   ; add the product of the previous multplication to the remainder (overflown, if existant)
        xor edx edx

        ; We are going to copy the remainder to the next element of the array, but we need to make sure that
        ; we are not copying it outside the limits of the array. So, avoid copyinf it to an element that does not exist.
        ;cmp edi D@ArrCounter | je L0> ; Are we at the last element of the array ? If yes, jump over (It happens whenever we are on the 1st loop)
         ;   mov D$esi+ebx+4 edx        ; copy the remainder to the next element of the array, otherwise.
        ;L0:

        ; ebx now must contains the size of the previous element of the array.
        ; We are doing a bottom to top operation, remember ?
        ; since the array is formed by dwords, we subtract ebx by 4.

        sub ebx 4
    dec edi | jne L1<   ; decrease our counter and see if we ended it. So, we see if our counter reached 0.
                        ; We are entering on a loop whenever the counter is not zeroed.

EndP

_________________________________________________________________________________________________________


;;

    BitDigitConverter
    
    This function retrieves the size (in bytes) of the amount of chars (decimal digits) that fits inside
    a XXX bit Binary value (in bits).
    
    It is used to compute the maximum buffer used for an ascii decimal convertion of a given Bit Set.

    Arguments:
    
        InputedBits: A bit set value to be converted.
    

    Returned values: Eax will return the necessary size.
    
    Remarks:
            When you are converting a decimals ascii to hexadecimal value, you need always that there is enough
            size of the buffer that holds the string. So, the len of the string must be big enough for that convertion.
            
            
            To calculate the necessary len of the string, you need to provide the Amount of Bits you want to be converted
            and use the following formula:
            
            Decimal Digits = log(2) * BitNumber
            
            Example: If you want to know the minimum len of a string that will convert an 128 Bit value, you do the
                     following operation:
                     
                     log(2) = 0,30102999566398119521373889472449
                     BitNumber = 128
                     Decimal Digits (Buffer) = 0,3010299956... * 128 =~ 38,53 ==> rounding = 39 digits (bytes)
                     
                     
                     In case, the maximum size of the string is 39. If your string is null-terminated, then you need 
                     only to add an extra byte to hold the zero. So the maximum size of a null terminated ascii string
                     that will convert a 128 Bit value is 40 Bytes (39 + 1 for zero trailing)
            

        Example of usage:
                    
            call BitDigitConverter 320
            ; eax will return 98 bytes

Author: Gustavo Trigueiros (aka Beyond2000!)
        Wolfgang Kern (Kesys)

www.rosasm.org


;;


Proc BitDigitConverter:
    Arguments @InputedBits
    Local @Result

    fild D@InputedBits
    FLDLG2
    fmulp ST1, ST0
    fistp D@Result
    mov eax D@Result

EndP

_________________________________________________________________________________________________________


;;
    Ones32 - Population Count (Countage of 1 Bit)


    The population count of a binary integer value x is the number of one bits in the value.
    Although many machines have single instructions for this, the single instructions are usually microcoded
    loops that test a bit per cycle; a log-time algorithm coded in assembly is often faster.
    
    This function uses a variable-precision SWAR algorithm to perform a tree reduction adding the bits
    in a 32-bit value.
    
    It is worthwhile noting that the SWAR population count algorithm given above can be improved upon
    for the case of counting the population of multi-word bit sets.
    
    How ?
    
    The last few steps in the reduction are using only a portion of the SWAR width to produce their results;
    thus, it would be possible to combine these steps across multiple words being reduced. 


    This function can be also used to calculate the log2 of an integer.
    

    Arguments:
    
    Input:  The 32-Bit value to be calculated
            
    Returned Values: Eax will contains the total amount of 1 Bits on the inputed dword.


    Remarks: For more information about SWAR algorithm, see here:
             (double right click on the links below to open the site)

             http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=1769323
             http://www.swarmintelligence.org/SIBook/TOC.php
             http://web.syr.edu/~gkamathh/topics/pso.html
             http://en.wikipedia.org/wiki/Swarm_Intelligence
             http://aggregate.org/MAGIC/index.html
             

Author: Gustavo Trigueiros (aka Beyond2000!)
        Many thanks to Prof. Henry G.(Hank) Dietz from University of Kentucky for his C version of this algoritm.
        
www.rosasm.org    

;;

Proc Ones32:
    Arguments @Input

    mov eax D@Input

    mov ecx eax

    ; x - = ((x >> 1) & 0x55555555);
    shr eax 1
    and eax 00_01010101_01010101_01010101_01010101
    sub ecx eax

    ; x = (((x >> 2) & 0x33333333) + (x & 0x33333333));

    mov esi ecx
    and esi 00_00110011_00110011_00110011_00110011
    shr ecx 2
    and ecx 00_00110011_00110011_00110011_00110011
    add ecx esi
    mov esi ecx ; esi = ecx

    ; x = (((x >> 4) + x) & 0x0f0f0f0f);

    shr ecx 4
    add ecx esi
    and ecx 00_00001111_00001111_00001111_00001111

    ; x += (x >> 8);
    mov esi ecx ; esi = ecx
    shr ecx 8
    add esi ecx

    ; x += (x >> 16);
    mov eax esi
    shr eax 16
    add eax esi

    and eax 00_00000000_00000000_00000000_00111111


EndP


_________________________________________________________________________________________________________
; convert a data chunck of any size to it´s negative form
Proc AllNegativeConverter:

EndP
_________________________________________________________________________________________________________


_________________________________________________________________________________________________________


_________________________________________________________________________________________________________



; Ascii Convertion Equates Table

; Equates representing the size of each element of an array of 161 Dwords. Each Dword represents 32 Bit set.

[Size_Of_Conv32BitArray 0]      [Size_Of_Conv64BitArray 4]      [Size_Of_Conv96BitArray 8]
[Size_Of_Conv128BitArray 12]    [Size_Of_Conv160BitArray 16]    [Size_Of_Conv192BitArray 20]
[Size_Of_Conv224BitArray 24]    [Size_Of_Conv256BitArray 28]    [Size_Of_Conv288BitArray 32]
[Size_Of_Conv320BitArray 36]    [Size_Of_Conv352BitArray 40]    [Size_Of_Conv384BitArray 44]
[Size_Of_Conv416BitArray 48]    [Size_Of_Conv448BitArray 52]    [Size_Of_Conv480BitArray 56]
[Size_Of_Conv512BitArray 60]    [Size_Of_Conv544BitArray 64]    [Size_Of_Conv576BitArray 68]
[Size_Of_Conv608BitArray 72]    [Size_Of_Conv640BitArray 76]    [Size_Of_Conv672BitArray 80]
[Size_Of_Conv704BitArray 84]    [Size_Of_Conv736BitArray 88]    [Size_Of_Conv768BitArray 92]
[Size_Of_Conv800BitArray 96]    [Size_Of_Conv832BitArray 100]   [Size_Of_Conv864BitArray 104]
[Size_Of_Conv896BitArray 108]   [Size_Of_Conv928BitArray 112]   [Size_Of_Conv960BitArray 116]
[Size_Of_Conv992BitArray 120]   [Size_Of_Conv1024BitArray 124]  [Size_Of_Conv1056BitArray 128]
[Size_Of_Conv1088BitArray 132]  [Size_Of_Conv1120BitArray 136]  [Size_Of_Conv1152BitArray 140]
[Size_Of_Conv1184BitArray 144]  [Size_Of_Conv1216BitArray 148]  [Size_Of_Conv1248BitArray 152]
[Size_Of_Conv1280BitArray 156]  [Size_Of_Conv1312BitArray 160]  [Size_Of_Conv1344BitArray 164]
[Size_Of_Conv1376BitArray 168]  [Size_Of_Conv1408BitArray 172]  [Size_Of_Conv1440BitArray 176]
[Size_Of_Conv1472BitArray 180]  [Size_Of_Conv1504BitArray 184]  [Size_Of_Conv1536BitArray 188]
[Size_Of_Conv1568BitArray 192]  [Size_Of_Conv1600BitArray 196]  [Size_Of_Conv1632BitArray 200]
[Size_Of_Conv1664BitArray 204]  [Size_Of_Conv1696BitArray 208]  [Size_Of_Conv1728BitArray 212]
[Size_Of_Conv1760BitArray 216]  [Size_Of_Conv1792BitArray 220]  [Size_Of_Conv1824BitArray 224]
[Size_Of_Conv1856BitArray 228]  [Size_Of_Conv1888BitArray 232]  [Size_Of_Conv1920BitArray 236]
[Size_Of_Conv1952BitArray 240]  [Size_Of_Conv1984BitArray 244]  [Size_Of_Conv2016BitArray 248]
[Size_Of_Conv2048BitArray 252]  [Size_Of_Conv2080BitArray 256]  [Size_Of_Conv2112BitArray 260]
[Size_Of_Conv2144BitArray 264]  [Size_Of_Conv2176BitArray 268]  [Size_Of_Conv2208BitArray 272]
[Size_Of_Conv2240BitArray 276]  [Size_Of_Conv2272BitArray 280]  [Size_Of_Conv2304BitArray 284]
[Size_Of_Conv2336BitArray 288]  [Size_Of_Conv2368BitArray 292]  [Size_Of_Conv2400BitArray 296]
[Size_Of_Conv2432BitArray 300]  [Size_Of_Conv2464BitArray 304]  [Size_Of_Conv2496BitArray 308]
[Size_Of_Conv2528BitArray 312]  [Size_Of_Conv2560BitArray 316]  [Size_Of_Conv2592BitArray 320]
[Size_Of_Conv2624BitArray 324]  [Size_Of_Conv2656BitArray 328]  [Size_Of_Conv2688BitArray 332]
[Size_Of_Conv2720BitArray 336]  [Size_Of_Conv2752BitArray 340]  [Size_Of_Conv2784BitArray 344]
[Size_Of_Conv2816BitArray 348]  [Size_Of_Conv2848BitArray 352]  [Size_Of_Conv2880BitArray 356]
[Size_Of_Conv2912BitArray 360]  [Size_Of_Conv2944BitArray 364]  [Size_Of_Conv2976BitArray 368]
[Size_Of_Conv3008BitArray 372]  [Size_Of_Conv3040BitArray 376]  [Size_Of_Conv3072BitArray 380]
[Size_Of_Conv3104BitArray 384]  [Size_Of_Conv3136BitArray 388]  [Size_Of_Conv3168BitArray 392]
[Size_Of_Conv3200BitArray 396]  [Size_Of_Conv3232BitArray 400]  [Size_Of_Conv3264BitArray 404]
[Size_Of_Conv3296BitArray 408]  [Size_Of_Conv3328BitArray 412]  [Size_Of_Conv3360BitArray 416]
[Size_Of_Conv3392BitArray 420]  [Size_Of_Conv3424BitArray 424]  [Size_Of_Conv3456BitArray 428]
[Size_Of_Conv3488BitArray 432]  [Size_Of_Conv3520BitArray 436]  [Size_Of_Conv3552BitArray 440]
[Size_Of_Conv3584BitArray 444]  [Size_Of_Conv3616BitArray 448]  [Size_Of_Conv3648BitArray 452]
[Size_Of_Conv3680BitArray 456]  [Size_Of_Conv3712BitArray 460]  [Size_Of_Conv3744BitArray 464]
[Size_Of_Conv3776BitArray 468]  [Size_Of_Conv3808BitArray 472]  [Size_Of_Conv3840BitArray 476]
[Size_Of_Conv3872BitArray 480]  [Size_Of_Conv3904BitArray 484]  [Size_Of_Conv3936BitArray 488]
[Size_Of_Conv3968BitArray 492]  [Size_Of_Conv4000BitArray 496]  [Size_Of_Conv4032BitArray 500]
[Size_Of_Conv4064BitArray 504]  [Size_Of_Conv4096BitArray 508]  [Size_Of_Conv4128BitArray 512]
[Size_Of_Conv4160BitArray 516]  [Size_Of_Conv4192BitArray 520]  [Size_Of_Conv4224BitArray 524]
[Size_Of_Conv4256BitArray 528]  [Size_Of_Conv4288BitArray 532]  [Size_Of_Conv4320BitArray 536]
[Size_Of_Conv4352BitArray 540]  [Size_Of_Conv4384BitArray 544]  [Size_Of_Conv4416BitArray 548]
[Size_Of_Conv4448BitArray 552]  [Size_Of_Conv4480BitArray 556]  [Size_Of_Conv4512BitArray 560]
[Size_Of_Conv4544BitArray 564]  [Size_Of_Conv4576BitArray 568]  [Size_Of_Conv4608BitArray 572]
[Size_Of_Conv4640BitArray 576]  [Size_Of_Conv4672BitArray 580]  [Size_Of_Conv4704BitArray 584]
[Size_Of_Conv4736BitArray 588]  [Size_Of_Conv4768BitArray 592]  [Size_Of_Conv4800BitArray 596]
[Size_Of_Conv4832BitArray 600]  [Size_Of_Conv4864BitArray 604]  [Size_Of_Conv4896BitArray 608]
[Size_Of_Conv4928BitArray 612]  [Size_Of_Conv4960BitArray 616]  [Size_Of_Conv4992BitArray 620]
[Size_Of_Conv5024BitArray 624]  [Size_Of_Conv5056BitArray 628]  [Size_Of_Conv5088BitArray 632]
[Size_Of_Conv5120BitArray 636]  [Size_Of_Conv5152BitArray 640]


; Constants representing the displacement of each element inside the array. It counts how many dwords
; we are currently handling.

[Conv32BitArrayID 1]        [Conv64BitArrayID 2]        [Conv96BitArrayID 3]        [Conv128BitArrayID 4]
[Conv160BitArrayID 5]       [Conv192BitArrayID 6]       [Conv224BitArrayID 7]       [Conv256BitArrayID 8]
[Conv288BitArrayID 9]       [Conv320BitArrayID 10]      [Conv352BitArrayID 11]      [Conv384BitArrayID 12]
[Conv416BitArrayID 13]      [Conv448BitArrayID 14]      [Conv480BitArrayID 15]      [Conv512BitArrayID 16]
[Conv544BitArrayID 17]      [Conv576BitArrayID 18]      [Conv608BitArrayID 19]      [Conv640BitArrayID 20]
[Conv672BitArrayID 21]      [Conv704BitArrayID 22]      [Conv736BitArrayID 23]      [Conv768BitArrayID 24]
[Conv800BitArrayID 25]      [Conv832BitArrayID 26]      [Conv864BitArrayID 27]      [Conv896BitArrayID 28]
[Conv928BitArrayID 29]      [Conv960BitArrayID 30]      [Conv992BitArrayID 31]      [Conv1024BitArrayID 32]
[Conv1056BitArrayID 33]     [Conv1088BitArrayID 34]     [Conv1120BitArrayID 35]     [Conv1152BitArrayID 36]
[Conv1184BitArrayID 37]     [Conv1216BitArrayID 38]     [Conv1248BitArrayID 39]     [Conv1280BitArrayID 40]
[Conv1312BitArrayID 41]     [Conv1344BitArrayID 42]     [Conv1376BitArrayID 43]     [Conv1408BitArrayID 44]
[Conv1440BitArrayID 45]     [Conv1472BitArrayID 46]     [Conv1504BitArrayID 47]     [Conv1536BitArrayID 48]
[Conv1568BitArrayID 49]     [Conv1600BitArrayID 50]     [Conv1632BitArrayID 51]     [Conv1664BitArrayID 52]
[Conv1696BitArrayID 53]     [Conv1728BitArrayID 54]     [Conv1760BitArrayID 55]     [Conv1792BitArrayID 56]
[Conv1824BitArrayID 57]     [Conv1856BitArrayID 58]     [Conv1888BitArrayID 59]     [Conv1920BitArrayID 60]
[Conv1952BitArrayID 61]     [Conv1984BitArrayID 62]     [Conv2016BitArrayID 63]     [Conv2048BitArrayID 64]
[Conv2080BitArrayID 65]     [Conv2112BitArrayID 66]     [Conv2144BitArrayID 67]     [Conv2176BitArrayID 68]
[Conv2208BitArrayID 69]     [Conv2240BitArrayID 70]     [Conv2272BitArrayID 71]     [Conv2304BitArrayID 72]
[Conv2336BitArrayID 73]     [Conv2368BitArrayID 74]     [Conv2400BitArrayID 75]     [Conv2432BitArrayID 76]
[Conv2464BitArrayID 77]     [Conv2496BitArrayID 78]     [Conv2528BitArrayID 79]     [Conv2560BitArrayID 80]
[Conv2592BitArrayID 81]     [Conv2624BitArrayID 82]     [Conv2656BitArrayID 83]     [Conv2688BitArrayID 84]
[Conv2720BitArrayID 85]     [Conv2752BitArrayID 86]     [Conv2784BitArrayID 87]     [Conv2816BitArrayID 88]
[Conv2848BitArrayID 89]     [Conv2880BitArrayID 90]     [Conv2912BitArrayID 91]     [Conv2944BitArrayID 92]
[Conv2976BitArrayID 93]     [Conv3008BitArrayID 94]     [Conv3040BitArrayID 95]     [Conv3072BitArrayID 96]
[Conv3104BitArrayID 97]     [Conv3136BitArrayID 98]     [Conv3168BitArrayID 99]     [Conv3200BitArrayID 100]
[Conv3232BitArrayID 101]    [Conv3264BitArrayID 102]    [Conv3296BitArrayID 103]    [Conv3328BitArrayID 104]
[Conv3360BitArrayID 105]    [Conv3392BitArrayID 106]    [Conv3424BitArrayID 107]    [Conv3456BitArrayID 108]
[Conv3488BitArrayID 109]    [Conv3520BitArrayID 110]    [Conv3552BitArrayID 111]    [Conv3584BitArrayID 112]
[Conv3616BitArrayID 113]    [Conv3648BitArrayID 114]    [Conv3680BitArrayID 115]    [Conv3712BitArrayID 116]
[Conv3744BitArrayID 117]    [Conv3776BitArrayID 118]    [Conv3808BitArrayID 119]    [Conv3840BitArrayID 120]
[Conv3872BitArrayID 121]    [Conv3904BitArrayID 122]    [Conv3936BitArrayID 123]    [Conv3968BitArrayID 124]
[Conv4000BitArrayID 125]    [Conv4032BitArrayID 126]    [Conv4064BitArrayID 127]    [Conv4096BitArrayID 128]
[Conv4128BitArrayID 129]    [Conv4160BitArrayID 130]    [Conv4192BitArrayID 131]    [Conv4224BitArrayID 132]
[Conv4256BitArrayID 133]    [Conv4288BitArrayID 134]    [Conv4320BitArrayID 135]    [Conv4352BitArrayID 136]
[Conv4384BitArrayID 137]    [Conv4416BitArrayID 138]    [Conv4448BitArrayID 139]    [Conv4480BitArrayID 140]
[Conv4512BitArrayID 141]    [Conv4544BitArrayID 142]    [Conv4576BitArrayID 143]    [Conv4608BitArrayID 144]
[Conv4640BitArrayID 145]    [Conv4672BitArrayID 146]    [Conv4704BitArrayID 147]    [Conv4736BitArrayID 148]
[Conv4768BitArrayID 149]    [Conv4800BitArrayID 150]    [Conv4832BitArrayID 151]    [Conv4864BitArrayID 152]
[Conv4896BitArrayID 153]    [Conv4928BitArrayID 154]    [Conv4960BitArrayID 155]    [Conv4992BitArrayID 156]
[Conv5024BitArrayID 157]    [Conv5056BitArrayID 158]    [Conv5088BitArrayID 159]    [Conv5120BitArrayID 160]
[Conv5152BitArrayID 161]


_____________________________________________________________________________________________________________

;;

    Ascii Decimal to 8 Bit convertion.
    
    
    This function converts a signed or unsigned ascii decimal string to a correspondant 8 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a dword (32 bits = 4 bytes), representing the bit set to be converted.
                The last 3 bytes are used as a verification for overflown.
                 
                 On output, the dword will contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE
        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 8 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        
        [Size_Of_Conv32BitArray 0]

        [String: B$ "4294967294", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0]
        
        call Asciito8 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;

Proc Asciito8:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        xor eax eax
        mov ecx 10
        mov eax D$esi+Size_Of_Conv32BitArray
        mul ecx
        mov D$esi+Size_Of_Conv32BitArray eax

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char. the product of the multiplication of the char by 10 is stored in esi

        If W$esi+Size_Of_Conv32BitArray+1 <> 0 ; 32-((4-(4-1))*8) = 16
            xor eax eax
            ExitP
        End_If

        inc edi

    End_While

    mov eax &TRUE

EndP
_____________________________________________________________________________________________________________


;;

    Ascii Decimal to 16 Bit convertion.
    
    
    This function converts a signed or unsigned ascii decimal string to a correspondant 16 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a dword (32 bits = 4 bytes), representing the bit set to be converted.
                The last word is used as a verification for overflown.
                 
                 On output, the dword will contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE
        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 16 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        
        [Size_Of_Conv32BitArray 0]

        [String: B$ "4294967294", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0]
        
        call Asciito16 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito16:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        xor eax eax
        mov ecx 10
        mov eax D$esi+Size_Of_Conv32BitArray
        mul ecx
        mov D$esi+Size_Of_Conv32BitArray eax

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char. the product of the multiplication of the char by 10 is stored in esi

        If W$esi+Size_Of_Conv32BitArray+2 <> 0 ; 32-(2*8) = 16
            xor eax eax
            ExitP
        End_If

        inc edi

    End_While

    mov eax &TRUE

EndP

_____________________________________________________________________________________________________________

;;

    Ascii Decimal to 32 Bit convertion.
    
    
    This function converts a signed or unsigned ascii decimal string to a correspondant 32 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 2 dwords (64 bits = 8 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 64 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]

        [String: B$ "4294967294", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito32 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito32:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct


    pushad

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv64BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv64BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv64BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv64BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv64BitArray is filled with some value, the ZF is zeroed
        je L1> | popad | xor eax eax | ExitP | L1:
;;
        If D$esi+Size_Of_Conv64BitArray <> 0
            xor eax eax
            ExitP
        End_If
;;
        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
    .End_If

    popad
    mov eax &TRUE

EndP



_________________________________________________________________________________________________________


;;

    Ascii Decimal to 64 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 64 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 3 dwords (96 bits = 12 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE
        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 96 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]

        [String: B$ "18446744073709551615", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito64 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito64:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv96BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv96BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv96BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv96BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv96BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:

;        If D$esi+Size_Of_Conv96BitArray <> 0
 ;           xor eax eax
  ;          ExitP
   ;     End_If
        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray; | sbb D$esi+Size_Of_Conv64BitArray 0
    .End_If

    mov eax &TRUE

EndP




_________________________________________________________________________________________________________


;;

    Ascii Decimal to 80 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 80 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 3 dwords (96 bits = 12 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE
        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            the last word of Bit 96 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]

        [String: B$ "18446744073709551615", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito80 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito80:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv96BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv96BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv96BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        If W$esi+Size_Of_Conv96BitArray+2 <> 0 ; 96-(2*8) = 80
            xor eax eax
            ExitP
        End_If

        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray; | jc L1> | sbb D$esi+Size_Of_Conv32BitArray 0 | L1:
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg W$esi+Size_Of_Conv96BitArray | sbb W$esi+Size_Of_Conv96BitArray 0
    .End_If

    mov eax &TRUE

EndP



_________________________________________________________________________________________________________


;;

    Ascii Decimal to 96 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 96 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 4 dwords (128 bits = 16 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.
        
        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 128 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]

        [String: B$ "745878414651169950410482426", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito96 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito96:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv128BitArrayID ; number of elements on the array
                                      ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv128BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv128BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv128BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv128BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:
        ;If D$esi+Size_Of_Conv128BitArray <> 0
         ;   xor eax eax
          ;  ExitP
        ;End_If
        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
    .End_If

    mov eax &TRUE

EndP


_________________________________________________________________________________________________________


;;

    Ascii Decimal to 112 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 112 bit hexadecimal value.
    It can be used to convert the values found using FLDENV mnemonic.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 4 dwords (128 bits = 16 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 128 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]

        [String: B$ "745878414651169950410482425", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito112 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito112:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv128BitArrayID ; number of elements on the array
                                      ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv128BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv128BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        If W$esi+Size_Of_Conv128BitArray+2 <> 0 ; 128-(2*8)=112
            xor eax eax
            ExitP
        End_If

        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
        neg W$esi+Size_Of_Conv128BitArray | sbb W$esi+Size_Of_Conv128BitArray 0
    .End_If

    mov eax &TRUE

EndP


_________________________________________________________________________________________________________


;;

    Ascii Decimal to 128 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 128 bit hexadecimal value.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 5 dwords (160 bits = 20 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0
                 Value.Conv160Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.
        
        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 160 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        [Conv160BitArrayID 5]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]
        [Size_Of_Conv160BitArray 16]

        [String: B$ "7458784146511699504104824251512520706", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito128 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito128:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv160BitArrayID   ; number of elements on the array
                                        ; ebx = size of the bit160 element of the array
            mov ebx Size_Of_Conv160BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv160BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv160BitArray 0   ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv160BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv160BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:
;;
        If D$esi+Size_Of_Conv160BitArray <> 0
            xor eax eax
            ExitP
        End_If
;;
        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
        neg D$esi+Size_Of_Conv128BitArray | sbb D$esi+Size_Of_Conv128BitArray 0
    .End_If

    mov eax &TRUE

EndP


__________________________________________________________________________________________________________________


;;

    Ascii Decimal to 224 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 224 bit hexadecimal value.
    It can be used to convert the values found from a operatino envolving the FLDENV mnemonic.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 8 dwords (256 bits = 32 bytes), where each element of the array
                represents the bit set
                to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0
                 Value.Conv160Bit: D$ 0
                 Value.Conv196Bit: D$ 0
                 Value.Conv224Bit: D$ 0
                 Value.Conv256Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 256 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        [Conv160BitArrayID 5]
        [Conv192BitArrayID 6]
        [Conv224BitArrayID 7]
        [Conv256BitArrayID 8]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]
        [Size_Of_Conv160BitArray 16]
        [Size_Of_Conv192BitArray 20]
        [Size_Of_Conv224BitArray 24]
        [Size_Of_Conv256BitArray 28]

        [String: B$ "7458784146511699504104824251512520706", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0
         Value.Conv196Bit: D$ 0
         Value.Conv224Bit: D$ 0
         Value.Conv256Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito224 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;

Proc Asciito224:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv256BitArrayID   ; number of elements on the array
                                        ; ebx = size of the bit160 element of the array
            mov ebx Size_Of_Conv256BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv256BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv160BitArray 0   ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv192BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv224BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv256BitArray 0   ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv256BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv256BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:
        ;If D$esi+Size_Of_Conv256BitArray <> 0
         ;   xor eax eax
          ;  ExitP
        ;End_If

        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
        neg D$esi+Size_Of_Conv128BitArray | sbb D$esi+Size_Of_Conv128BitArray 0
        neg D$esi+Size_Of_Conv160BitArray | sbb D$esi+Size_Of_Conv160BitArray 0
        neg D$esi+Size_Of_Conv192BitArray | sbb D$esi+Size_Of_Conv192BitArray 0
        neg D$esi+Size_Of_Conv224BitArray | sbb D$esi+Size_Of_Conv224BitArray 0
    .End_If

    mov eax &TRUE

EndP



__________________________________________________________________________________________________________________

;;

    Ascii Decimal to 752 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 752 bit hexadecimal value.
    It can be used to compute the decimal string from an operation envolving the FSAVE mnemonic.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 24 dwords (768 bits = 96 bytes), where each element of the array
                represents the bit set to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0
                 Value.Conv160Bit: D$ 0
                 Value.Conv196Bit: D$ 0
                 Value.Conv224Bit: D$ 0
                 Value.Conv256Bit: D$ 0
                 (...)
                 Value.Conv752Bit: D$ 0
                 Value.Conv768Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE
        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 768 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        [Conv160BitArrayID 5]
        [Conv192BitArrayID 6]
        [Conv224BitArrayID 7]
        [Conv256BitArrayID 8]
        (...)
        [Conv704BitArrayID 22]
        [Conv736BitArrayID 23]
        [Conv768BitArrayID 24]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]
        [Size_Of_Conv160BitArray 16]
        [Size_Of_Conv192BitArray 20]
        [Size_Of_Conv224BitArray 24]
        [Size_Of_Conv256BitArray 28]
        (...)
        [Size_Of_Conv704BitArray 84]
        [Size_Of_Conv736BitArray 88]
        [Size_Of_Conv768BitArray 92]

        [String: B$ "7458784146511699504104824251512520706", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0
         Value.Conv196Bit: D$ 0
         Value.Conv224Bit: D$ 0
         Value.Conv256Bit: D$ 0
         (...)
         Value.Conv752Bit: D$ 0
         Value.Conv768Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito752 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito752:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    .While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv768BitArrayID   ; number of elements on the array
                                        ; ebx = size of the bit160 element of the array
            mov ebx Size_Of_Conv768BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv768BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv160BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv192BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv224BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv256BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv288BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv320BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv352BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv384BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv416BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv448BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv480BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv512BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv544BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv576BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv608BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv640BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv672BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv704BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv736BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv768BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        If W$esi+Size_Of_Conv768BitArray+2 <> 0  ; 768-(2*8)= 752
            xor eax eax
            ExitP
        End_If

        inc edi

    .End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
        neg D$esi+Size_Of_Conv128BitArray | sbb D$esi+Size_Of_Conv128BitArray 0
        neg D$esi+Size_Of_Conv160BitArray | sbb D$esi+Size_Of_Conv160BitArray 0
        neg D$esi+Size_Of_Conv192BitArray | sbb D$esi+Size_Of_Conv192BitArray 0
        neg D$esi+Size_Of_Conv224BitArray | sbb D$esi+Size_Of_Conv224BitArray 0
        neg D$esi+Size_Of_Conv256BitArray | sbb D$esi+Size_Of_Conv256BitArray 0
        neg D$esi+Size_Of_Conv288BitArray | sbb D$esi+Size_Of_Conv288BitArray 0
        neg D$esi+Size_Of_Conv320BitArray | sbb D$esi+Size_Of_Conv320BitArray 0
        neg D$esi+Size_Of_Conv352BitArray | sbb D$esi+Size_Of_Conv352BitArray 0
        neg D$esi+Size_Of_Conv384BitArray | sbb D$esi+Size_Of_Conv384BitArray 0
        neg D$esi+Size_Of_Conv416BitArray | sbb D$esi+Size_Of_Conv416BitArray 0
        neg D$esi+Size_Of_Conv448BitArray | sbb D$esi+Size_Of_Conv448BitArray 0
        neg D$esi+Size_Of_Conv480BitArray | sbb D$esi+Size_Of_Conv480BitArray 0
        neg D$esi+Size_Of_Conv512BitArray | sbb D$esi+Size_Of_Conv512BitArray 0
        neg D$esi+Size_Of_Conv544BitArray | sbb D$esi+Size_Of_Conv544BitArray 0
        neg D$esi+Size_Of_Conv576BitArray | sbb D$esi+Size_Of_Conv576BitArray 0
        neg D$esi+Size_Of_Conv608BitArray | sbb D$esi+Size_Of_Conv608BitArray 0
        neg D$esi+Size_Of_Conv640BitArray | sbb D$esi+Size_Of_Conv640BitArray 0
        neg D$esi+Size_Of_Conv672BitArray | sbb D$esi+Size_Of_Conv672BitArray 0
        neg D$esi+Size_Of_Conv704BitArray | sbb D$esi+Size_Of_Conv704BitArray 0
        neg D$esi+Size_Of_Conv736BitArray | sbb D$esi+Size_Of_Conv736BitArray 0
        neg W$esi+Size_Of_Conv768BitArray | sbb W$esi+Size_Of_Conv768BitArray 0
    .End_If

    mov eax &TRUE

EndP



__________________________________________________________________________________________________________________

;;

    Ascii Decimal to 864 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 864 bit hexadecimal value.
    It can be used to compute the decimal string from an operation envolving the FSAVE mnemonic.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 28 dwords (896 bits = 112 bytes), where each element of the array
                represents the bit set to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0
                 Value.Conv160Bit: D$ 0
                 Value.Conv196Bit: D$ 0
                 Value.Conv224Bit: D$ 0
                 Value.Conv256Bit: D$ 0
                 (...)
                 Value.Conv864Bit: D$ 0
                 Value.Conv896Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 896 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        [Conv160BitArrayID 5]
        [Conv192BitArrayID 6]
        [Conv224BitArrayID 7]
        [Conv256BitArrayID 8]
        (...)
        [Conv864BitArrayID 27]
        [Conv896BitArrayID 28]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]
        [Size_Of_Conv160BitArray 16]
        [Size_Of_Conv192BitArray 20]
        [Size_Of_Conv224BitArray 24]
        [Size_Of_Conv256BitArray 28]
        (...)
        [Size_Of_Conv864BitArray 104]
        [Size_Of_Conv896BitArray 108]

        [String: B$ "7458784146511699504104824251512520745641855606", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0
         Value.Conv196Bit: D$ 0
         Value.Conv224Bit: D$ 0
         Value.Conv256Bit: D$ 0
         (...)
         Value.Conv864Bit: D$ 0
         Value.Conv896Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito864 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc Asciito864:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    .While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv896BitArrayID   ; number of elements on the array
                                        ; ebx = size of the bit160 element of the array
            mov ebx Size_Of_Conv896BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv896BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv160BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv192BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv224BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv256BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv288BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv320BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv352BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv384BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv416BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv448BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv480BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv512BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv544BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv576BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv608BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv640BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv672BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv704BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv736BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv768BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv800BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv832BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv864BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv896BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv896BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv896BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:
        ;If D$esi+Size_Of_Conv896BitArray <> 0
         ;   xor eax eax
          ;  ExitP
        ;End_If

        inc edi

    .End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
        neg D$esi+Size_Of_Conv128BitArray | sbb D$esi+Size_Of_Conv128BitArray 0
        neg D$esi+Size_Of_Conv160BitArray | sbb D$esi+Size_Of_Conv160BitArray 0
        neg D$esi+Size_Of_Conv192BitArray | sbb D$esi+Size_Of_Conv192BitArray 0
        neg D$esi+Size_Of_Conv224BitArray | sbb D$esi+Size_Of_Conv224BitArray 0
        neg D$esi+Size_Of_Conv256BitArray | sbb D$esi+Size_Of_Conv256BitArray 0
        neg D$esi+Size_Of_Conv288BitArray | sbb D$esi+Size_Of_Conv288BitArray 0
        neg D$esi+Size_Of_Conv320BitArray | sbb D$esi+Size_Of_Conv320BitArray 0
        neg D$esi+Size_Of_Conv352BitArray | sbb D$esi+Size_Of_Conv352BitArray 0
        neg D$esi+Size_Of_Conv384BitArray | sbb D$esi+Size_Of_Conv384BitArray 0
        neg D$esi+Size_Of_Conv416BitArray | sbb D$esi+Size_Of_Conv416BitArray 0
        neg D$esi+Size_Of_Conv448BitArray | sbb D$esi+Size_Of_Conv448BitArray 0
        neg D$esi+Size_Of_Conv480BitArray | sbb D$esi+Size_Of_Conv480BitArray 0
        neg D$esi+Size_Of_Conv512BitArray | sbb D$esi+Size_Of_Conv512BitArray 0
        neg D$esi+Size_Of_Conv544BitArray | sbb D$esi+Size_Of_Conv544BitArray 0
        neg D$esi+Size_Of_Conv576BitArray | sbb D$esi+Size_Of_Conv576BitArray 0
        neg D$esi+Size_Of_Conv608BitArray | sbb D$esi+Size_Of_Conv608BitArray 0
        neg D$esi+Size_Of_Conv640BitArray | sbb D$esi+Size_Of_Conv640BitArray 0
        neg D$esi+Size_Of_Conv672BitArray | sbb D$esi+Size_Of_Conv672BitArray 0
        neg D$esi+Size_Of_Conv704BitArray | sbb D$esi+Size_Of_Conv704BitArray 0
        neg D$esi+Size_Of_Conv736BitArray | sbb D$esi+Size_Of_Conv736BitArray 0
        neg D$esi+Size_Of_Conv768BitArray | sbb D$esi+Size_Of_Conv768BitArray 0
        neg D$esi+Size_Of_Conv800BitArray | sbb D$esi+Size_Of_Conv800BitArray 0
        neg D$esi+Size_Of_Conv832BitArray | sbb D$esi+Size_Of_Conv832BitArray 0
        neg D$esi+Size_Of_Conv864BitArray | sbb D$esi+Size_Of_Conv864BitArray 0
    .End_If

    mov eax &TRUE

EndP



__________________________________________________________________________________________________________________


;;

    Ascii Decimal to 4096 Bit convertion.
    
    
    This function converts a unsigned ascii decimal string to a correspondant 4096 bit hexadecimal value.
    It can be used to compute the decimal string from an operation envolving the FSAVE mnemonic.

    Parameters:
        
        String: The unsigned ascii decimal string to be converted.
        
        Array:  (Input/Output) Pointer to a array of 129 dwords (4128 bits = 516 bytes), where each element of the array
                represents the bit set to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0
                 Value.Conv128Bit: D$ 0
                 Value.Conv160Bit: D$ 0
                 Value.Conv196Bit: D$ 0
                 Value.Conv224Bit: D$ 0
                 Value.Conv256Bit: D$ 0
                 (...)
                 Value.Conv4096Bit: D$ 0
                 Value.Conv4128Bit: D$ 0] ; <--- overflown check
                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-8, just do:
                          
                                  [String: B$ "8", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0]
                                   
                                   call Asciito8 String, Value, &TRUE

        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            Bit 4128 starts to be filled.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        [Conv128BitArrayID 4]
        [Conv160BitArrayID 5]
        [Conv192BitArrayID 6]
        [Conv224BitArrayID 7]
        [Conv256BitArrayID 8]
        (...)
        [Conv4096BitArrayID 128]
        [Conv4128BitArrayID 129]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]
        [Size_Of_Conv128BitArray 12]
        [Size_Of_Conv160BitArray 16]
        [Size_Of_Conv192BitArray 20]
        [Size_Of_Conv224BitArray 24]
        [Size_Of_Conv256BitArray 28]
        (...)
        [Size_Of_Conv4096BitArray 508]
        [Size_Of_Conv4128BitArray 512]

        [String: B$ "7458784146511699504104824251512520745641855606", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0
         Value.Conv196Bit: D$ 0
         Value.Conv224Bit: D$ 0
         Value.Conv256Bit: D$ 0
         (...)
         Value.Conv4096Bit: D$ 0
         Value.Conv4128Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call Asciito4096 String, Value, &FALSE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;

Proc Asciito4096:
    Arguments @String, @Array, @IsNegative
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    .While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv4128BitArrayID   ; number of elements on the array
                                        ; ebx = size of the bit160 element of the array
            mov ebx Size_Of_Conv4128BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv4128BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv160BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv192BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv224BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv256BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv288BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv320BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv352BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv384BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv416BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv448BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv480BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv512BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv544BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv576BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv608BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv640BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv672BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv704BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv736BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv768BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv800BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv832BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv864BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv896BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv928BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv960BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv992BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1024BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1056BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1088BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1120BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1152BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1184BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1216BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1248BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1280BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1312BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1344BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1376BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1408BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1440BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1472BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1504BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1536BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1568BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1600BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1632BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1664BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1696BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1728BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1760BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1792BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1824BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1856BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1888BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1920BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1952BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv1984BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2016BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2048BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2080BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2112BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2144BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2176BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2208BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2240BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2272BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2304BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2336BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2368BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2400BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2432BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2464BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2496BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2528BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2560BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2592BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2624BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2656BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2688BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2720BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2752BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2784BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2816BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2848BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2880BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2912BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2944BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv2976BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3008BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3040BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3072BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3104BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3136BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3168BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3200BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3232BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3264BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3296BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3328BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3360BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3392BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3424BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3456BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3488BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3520BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3552BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3584BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3616BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3648BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3680BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3712BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3744BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3776BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3808BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3840BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3872BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3904BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3936BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv3968BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv4000BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv4032BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv4064BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv4096BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv4128BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv4128BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv4128BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:
        ;If D$esi+Size_Of_Conv4128BitArray <> 0
         ;   xor eax eax
          ;  ExitP
        ;End_If

        inc edi

    .End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg D$esi+Size_Of_Conv96BitArray | sbb D$esi+Size_Of_Conv96BitArray 0
        neg D$esi+Size_Of_Conv128BitArray | sbb D$esi+Size_Of_Conv128BitArray 0
        neg D$esi+Size_Of_Conv160BitArray | sbb D$esi+Size_Of_Conv160BitArray 0
        neg D$esi+Size_Of_Conv192BitArray | sbb D$esi+Size_Of_Conv192BitArray 0
        neg D$esi+Size_Of_Conv224BitArray | sbb D$esi+Size_Of_Conv224BitArray 0
        neg D$esi+Size_Of_Conv256BitArray | sbb D$esi+Size_Of_Conv256BitArray 0
        neg D$esi+Size_Of_Conv288BitArray | sbb D$esi+Size_Of_Conv288BitArray 0
        neg D$esi+Size_Of_Conv320BitArray | sbb D$esi+Size_Of_Conv320BitArray 0
        neg D$esi+Size_Of_Conv352BitArray | sbb D$esi+Size_Of_Conv352BitArray 0
        neg D$esi+Size_Of_Conv384BitArray | sbb D$esi+Size_Of_Conv384BitArray 0
        neg D$esi+Size_Of_Conv416BitArray | sbb D$esi+Size_Of_Conv416BitArray 0
        neg D$esi+Size_Of_Conv448BitArray | sbb D$esi+Size_Of_Conv448BitArray 0
        neg D$esi+Size_Of_Conv480BitArray | sbb D$esi+Size_Of_Conv480BitArray 0
        neg D$esi+Size_Of_Conv512BitArray | sbb D$esi+Size_Of_Conv512BitArray 0
        neg D$esi+Size_Of_Conv544BitArray | sbb D$esi+Size_Of_Conv544BitArray 0
        neg D$esi+Size_Of_Conv576BitArray | sbb D$esi+Size_Of_Conv576BitArray 0
        neg D$esi+Size_Of_Conv608BitArray | sbb D$esi+Size_Of_Conv608BitArray 0
        neg D$esi+Size_Of_Conv640BitArray | sbb D$esi+Size_Of_Conv640BitArray 0
        neg D$esi+Size_Of_Conv672BitArray | sbb D$esi+Size_Of_Conv672BitArray 0
        neg D$esi+Size_Of_Conv704BitArray | sbb D$esi+Size_Of_Conv704BitArray 0
        neg D$esi+Size_Of_Conv736BitArray | sbb D$esi+Size_Of_Conv736BitArray 0
        neg D$esi+Size_Of_Conv768BitArray | sbb D$esi+Size_Of_Conv768BitArray 0
        neg D$esi+Size_Of_Conv800BitArray | sbb D$esi+Size_Of_Conv800BitArray 0
        neg D$esi+Size_Of_Conv832BitArray | sbb D$esi+Size_Of_Conv832BitArray 0
        neg D$esi+Size_Of_Conv864BitArray | sbb D$esi+Size_Of_Conv864BitArray 0
        neg D$esi+Size_Of_Conv896BitArray | sbb D$esi+Size_Of_Conv896BitArray 0
        neg D$esi+Size_Of_Conv928BitArray | sbb D$esi+Size_Of_Conv928BitArray 0
        neg D$esi+Size_Of_Conv960BitArray | sbb D$esi+Size_Of_Conv960BitArray 0
        neg D$esi+Size_Of_Conv992BitArray | sbb D$esi+Size_Of_Conv992BitArray 0
        neg D$esi+Size_Of_Conv1024BitArray | sbb D$esi+Size_Of_Conv1024BitArray 0
        neg D$esi+Size_Of_Conv1056BitArray | sbb D$esi+Size_Of_Conv1056BitArray 0
        neg D$esi+Size_Of_Conv1088BitArray | sbb D$esi+Size_Of_Conv1088BitArray 0
        neg D$esi+Size_Of_Conv1120BitArray | sbb D$esi+Size_Of_Conv1120BitArray 0
        neg D$esi+Size_Of_Conv1152BitArray | sbb D$esi+Size_Of_Conv1152BitArray 0
        neg D$esi+Size_Of_Conv1184BitArray | sbb D$esi+Size_Of_Conv1184BitArray 0
        neg D$esi+Size_Of_Conv1216BitArray | sbb D$esi+Size_Of_Conv1216BitArray 0
        neg D$esi+Size_Of_Conv1248BitArray | sbb D$esi+Size_Of_Conv1248BitArray 0
        neg D$esi+Size_Of_Conv1280BitArray | sbb D$esi+Size_Of_Conv1280BitArray 0
        neg D$esi+Size_Of_Conv1312BitArray | sbb D$esi+Size_Of_Conv1312BitArray 0
        neg D$esi+Size_Of_Conv1344BitArray | sbb D$esi+Size_Of_Conv1344BitArray 0
        neg D$esi+Size_Of_Conv1376BitArray | sbb D$esi+Size_Of_Conv1376BitArray 0
        neg D$esi+Size_Of_Conv1408BitArray | sbb D$esi+Size_Of_Conv1408BitArray 0
        neg D$esi+Size_Of_Conv1440BitArray | sbb D$esi+Size_Of_Conv1440BitArray 0
        neg D$esi+Size_Of_Conv1472BitArray | sbb D$esi+Size_Of_Conv1472BitArray 0
        neg D$esi+Size_Of_Conv1504BitArray | sbb D$esi+Size_Of_Conv1504BitArray 0
        neg D$esi+Size_Of_Conv1536BitArray | sbb D$esi+Size_Of_Conv1536BitArray 0
        neg D$esi+Size_Of_Conv1568BitArray | sbb D$esi+Size_Of_Conv1568BitArray 0
        neg D$esi+Size_Of_Conv1600BitArray | sbb D$esi+Size_Of_Conv1600BitArray 0
        neg D$esi+Size_Of_Conv1632BitArray | sbb D$esi+Size_Of_Conv1632BitArray 0
        neg D$esi+Size_Of_Conv1664BitArray | sbb D$esi+Size_Of_Conv1664BitArray 0
        neg D$esi+Size_Of_Conv1696BitArray | sbb D$esi+Size_Of_Conv1696BitArray 0
        neg D$esi+Size_Of_Conv1728BitArray | sbb D$esi+Size_Of_Conv1728BitArray 0
        neg D$esi+Size_Of_Conv1760BitArray | sbb D$esi+Size_Of_Conv1760BitArray 0
        neg D$esi+Size_Of_Conv1792BitArray | sbb D$esi+Size_Of_Conv1792BitArray 0
        neg D$esi+Size_Of_Conv1824BitArray | sbb D$esi+Size_Of_Conv1824BitArray 0
        neg D$esi+Size_Of_Conv1856BitArray | sbb D$esi+Size_Of_Conv1856BitArray 0
        neg D$esi+Size_Of_Conv1888BitArray | sbb D$esi+Size_Of_Conv1888BitArray 0
        neg D$esi+Size_Of_Conv1920BitArray | sbb D$esi+Size_Of_Conv1920BitArray 0
        neg D$esi+Size_Of_Conv1952BitArray | sbb D$esi+Size_Of_Conv1952BitArray 0
        neg D$esi+Size_Of_Conv1984BitArray | sbb D$esi+Size_Of_Conv1984BitArray 0
        neg D$esi+Size_Of_Conv2016BitArray | sbb D$esi+Size_Of_Conv2016BitArray 0
        neg D$esi+Size_Of_Conv2048BitArray | sbb D$esi+Size_Of_Conv2048BitArray 0
        neg D$esi+Size_Of_Conv2080BitArray | sbb D$esi+Size_Of_Conv2080BitArray 0
        neg D$esi+Size_Of_Conv2112BitArray | sbb D$esi+Size_Of_Conv2112BitArray 0
        neg D$esi+Size_Of_Conv2144BitArray | sbb D$esi+Size_Of_Conv2144BitArray 0
        neg D$esi+Size_Of_Conv2176BitArray | sbb D$esi+Size_Of_Conv2176BitArray 0
        neg D$esi+Size_Of_Conv2208BitArray | sbb D$esi+Size_Of_Conv2208BitArray 0
        neg D$esi+Size_Of_Conv2240BitArray | sbb D$esi+Size_Of_Conv2240BitArray 0
        neg D$esi+Size_Of_Conv2272BitArray | sbb D$esi+Size_Of_Conv2272BitArray 0
        neg D$esi+Size_Of_Conv2304BitArray | sbb D$esi+Size_Of_Conv2304BitArray 0
        neg D$esi+Size_Of_Conv2336BitArray | sbb D$esi+Size_Of_Conv2336BitArray 0
        neg D$esi+Size_Of_Conv2368BitArray | sbb D$esi+Size_Of_Conv2368BitArray 0
        neg D$esi+Size_Of_Conv2400BitArray | sbb D$esi+Size_Of_Conv2400BitArray 0
        neg D$esi+Size_Of_Conv2432BitArray | sbb D$esi+Size_Of_Conv2432BitArray 0
        neg D$esi+Size_Of_Conv2464BitArray | sbb D$esi+Size_Of_Conv2464BitArray 0
        neg D$esi+Size_Of_Conv2496BitArray | sbb D$esi+Size_Of_Conv2496BitArray 0
        neg D$esi+Size_Of_Conv2528BitArray | sbb D$esi+Size_Of_Conv2528BitArray 0
        neg D$esi+Size_Of_Conv2560BitArray | sbb D$esi+Size_Of_Conv2560BitArray 0
        neg D$esi+Size_Of_Conv2592BitArray | sbb D$esi+Size_Of_Conv2592BitArray 0
        neg D$esi+Size_Of_Conv2624BitArray | sbb D$esi+Size_Of_Conv2624BitArray 0
        neg D$esi+Size_Of_Conv2656BitArray | sbb D$esi+Size_Of_Conv2656BitArray 0
        neg D$esi+Size_Of_Conv2688BitArray | sbb D$esi+Size_Of_Conv2688BitArray 0
        neg D$esi+Size_Of_Conv2720BitArray | sbb D$esi+Size_Of_Conv2720BitArray 0
        neg D$esi+Size_Of_Conv2752BitArray | sbb D$esi+Size_Of_Conv2752BitArray 0
        neg D$esi+Size_Of_Conv2784BitArray | sbb D$esi+Size_Of_Conv2784BitArray 0
        neg D$esi+Size_Of_Conv2816BitArray | sbb D$esi+Size_Of_Conv2816BitArray 0
        neg D$esi+Size_Of_Conv2848BitArray | sbb D$esi+Size_Of_Conv2848BitArray 0
        neg D$esi+Size_Of_Conv2880BitArray | sbb D$esi+Size_Of_Conv2880BitArray 0
        neg D$esi+Size_Of_Conv2912BitArray | sbb D$esi+Size_Of_Conv2912BitArray 0
        neg D$esi+Size_Of_Conv2944BitArray | sbb D$esi+Size_Of_Conv2944BitArray 0
        neg D$esi+Size_Of_Conv2976BitArray | sbb D$esi+Size_Of_Conv2976BitArray 0
        neg D$esi+Size_Of_Conv3008BitArray | sbb D$esi+Size_Of_Conv3008BitArray 0
        neg D$esi+Size_Of_Conv3040BitArray | sbb D$esi+Size_Of_Conv3040BitArray 0
        neg D$esi+Size_Of_Conv3072BitArray | sbb D$esi+Size_Of_Conv3072BitArray 0
        neg D$esi+Size_Of_Conv3104BitArray | sbb D$esi+Size_Of_Conv3104BitArray 0
        neg D$esi+Size_Of_Conv3136BitArray | sbb D$esi+Size_Of_Conv3136BitArray 0
        neg D$esi+Size_Of_Conv3168BitArray | sbb D$esi+Size_Of_Conv3168BitArray 0
        neg D$esi+Size_Of_Conv3200BitArray | sbb D$esi+Size_Of_Conv3200BitArray 0
        neg D$esi+Size_Of_Conv3232BitArray | sbb D$esi+Size_Of_Conv3232BitArray 0
        neg D$esi+Size_Of_Conv3264BitArray | sbb D$esi+Size_Of_Conv3264BitArray 0
        neg D$esi+Size_Of_Conv3296BitArray | sbb D$esi+Size_Of_Conv3296BitArray 0
        neg D$esi+Size_Of_Conv3328BitArray | sbb D$esi+Size_Of_Conv3328BitArray 0
        neg D$esi+Size_Of_Conv3360BitArray | sbb D$esi+Size_Of_Conv3360BitArray 0
        neg D$esi+Size_Of_Conv3392BitArray | sbb D$esi+Size_Of_Conv3392BitArray 0
        neg D$esi+Size_Of_Conv3424BitArray | sbb D$esi+Size_Of_Conv3424BitArray 0
        neg D$esi+Size_Of_Conv3456BitArray | sbb D$esi+Size_Of_Conv3456BitArray 0
        neg D$esi+Size_Of_Conv3488BitArray | sbb D$esi+Size_Of_Conv3488BitArray 0
        neg D$esi+Size_Of_Conv3520BitArray | sbb D$esi+Size_Of_Conv3520BitArray 0
        neg D$esi+Size_Of_Conv3552BitArray | sbb D$esi+Size_Of_Conv3552BitArray 0
        neg D$esi+Size_Of_Conv3584BitArray | sbb D$esi+Size_Of_Conv3584BitArray 0
        neg D$esi+Size_Of_Conv3616BitArray | sbb D$esi+Size_Of_Conv3616BitArray 0
        neg D$esi+Size_Of_Conv3648BitArray | sbb D$esi+Size_Of_Conv3648BitArray 0
        neg D$esi+Size_Of_Conv3680BitArray | sbb D$esi+Size_Of_Conv3680BitArray 0
        neg D$esi+Size_Of_Conv3712BitArray | sbb D$esi+Size_Of_Conv3712BitArray 0
        neg D$esi+Size_Of_Conv3744BitArray | sbb D$esi+Size_Of_Conv3744BitArray 0
        neg D$esi+Size_Of_Conv3776BitArray | sbb D$esi+Size_Of_Conv3776BitArray 0
        neg D$esi+Size_Of_Conv3808BitArray | sbb D$esi+Size_Of_Conv3808BitArray 0
        neg D$esi+Size_Of_Conv3840BitArray | sbb D$esi+Size_Of_Conv3840BitArray 0
        neg D$esi+Size_Of_Conv3872BitArray | sbb D$esi+Size_Of_Conv3872BitArray 0
        neg D$esi+Size_Of_Conv3904BitArray | sbb D$esi+Size_Of_Conv3904BitArray 0
        neg D$esi+Size_Of_Conv3936BitArray | sbb D$esi+Size_Of_Conv3936BitArray 0
        neg D$esi+Size_Of_Conv3968BitArray | sbb D$esi+Size_Of_Conv3968BitArray 0
        neg D$esi+Size_Of_Conv4000BitArray | sbb D$esi+Size_Of_Conv4000BitArray 0
        neg D$esi+Size_Of_Conv4032BitArray | sbb D$esi+Size_Of_Conv4032BitArray 0
        neg D$esi+Size_Of_Conv4064BitArray | sbb D$esi+Size_Of_Conv4064BitArray 0
        neg D$esi+Size_Of_Conv4096BitArray | sbb D$esi+Size_Of_Conv4096BitArray 0
    .End_If

    mov eax &TRUE

EndP

__________________________________________________________________________________________________________________

;;

    Ascii Decimal to Any BitSet convertion.
    
    
    This function converts a signed or unsigned ascii decimal string to a any bitset hexadecimal value.
    It can be used to compute the decimal string from an operation envolving the FSAVE mnemonic.

    Parameters:
        
        String: The ascii decimal string to be converted. It is important that you keep the string unsigned.
                No "0-...", "-...." are allowed. To convert the string to the negative form, set the 
                IsNegative flag to &TRUE.
        
        Array:  (Input/Output) Pointer to a array of dwords, where each element of the array represents the bit
                set to be converted. The last dword is used as a verification for overflown.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                a) If you are converting 80 bit string, do:
                    [Value:
                     Value.Conv32Bit: D$ 0
                     Value.Conv64Bit: D$ 0
                     Value.Conv96Bit: D$ 0] ; Extra dword <--- overflown check

                b) If you are converting 96 bit string, do:
                    [Value:
                     Value.Conv32Bit: D$ 0
                     Value.Conv64Bit: D$ 0
                     Value.Conv96Bit: D$ 0
                     Value.Conv128Bit: D$ 0] ; Extra dword <--- overflown check


                c) If you are converting 32 bit string, do:
                    [Value:
                     Value.Conv32Bit: D$ 0
                     Value.Conv64Bit: D$ 0] ; Extra dword <--- overflown check

                d) If you are converting 16 bit string, do:
                    ; Since the string is smaller then 32 bits, at least 1 dword we must have on the array
                    [Value:
                     Value.Conv32Bit: D$ 0] ; Extra dword <--- overflown check

                 
                 On output, each member of the array contains the converted value.

        IsNegative: A Flag containing the type of the inputed string. If the input is a decimal positive number,
                    set this flag to &FALSE, otherwise, set it to &TRUE.
                    Note: If the inputed to be converted is a negative number, don´t use "0-..." or "-...." as a prefix.
                          Example to calculate the negative form of 0-1208925819614629174706175, just do:
                          
                                  [String: B$ "1208925819614629174706175", 0]
                                  
                                  [Value:
                                   Value.Conv32Bit: D$ 0
                                   Value.Conv64Bit: D$ 0
                                   Value.Conv96Bit: D$ 0]
                                   
                                   call AsciitoAnyBit String5, Value, 80, &TRUE

        
        Returned values:    If the function suceeds it return &TRUE.
                            If the function fails it will return &FALSE. The function will return &FALSE whenever
                            the data exceeds the inputed Bitset.

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        ; 4096bit convertion (positive number)
        [String: B$ "7458784146511699504104824251512520745641855606", 0]
        
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0
         Value.Conv96Bit: D$ 0
         Value.Conv128Bit: D$ 0
         Value.Conv160Bit: D$ 0
         Value.Conv196Bit: D$ 0
         Value.Conv224Bit: D$ 0
         Value.Conv256Bit: D$ 0
         (...)
         Value.Conv4096Bit: D$ 0
         Value.Conv4128Bit: D$ 0] <--- This last member is used only as a verification check for overflown
        
        call AsciitoAnyBit String, Value, 4096, &FALSE
        

        ; 80bit convertion to negative form
        
        [String: B$ "1208925819614629174706175", 0]

        [Value:
            Value.Conv32Bit: D$ 0
            Value.Conv64Bit: D$ 0
            Value.Conv96Bit: D$ 0]
                                   
        call AsciitoAnyBit String, Value, 80, &TRUE
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;

Proc AsciitoAnyBit:
    Arguments @String, @Array, @BitSet, @IsNegative
    Local @PreviousProduct, @ConvBitArrayID, @Size_Of_ConvBitArray, @IsRemainder

    ; Check if this bitset may contains any remainders. That is, we divide the bitset by 32 and see the result.
    ; If the result of the division contains remainder, it means that the remainder is the amount of bits that
    ; does not fits the 32 bit chain. Example: 80/32 = 2,5 = 2 dwords + 16 bits (1 word). The remainder in edx = 16
    ; Why doing this ? Because we need to check if the data stored in the last Dword exceeds the bit limits. Example:
    ; When we are computing 80 bit set, the last dword may contains only 16 used bits. So, if the data fills any bits
    ; beyond the 16th, the function returns error, because the inputed strings does not fits the 80 bit set.

    mov eax D@BitSet
    mov D@IsRemainder 0
    xor edx edx
    mov ecx 32
    div ecx
    mov D@IsRemainder edx

    ; Now we get the total amount of Arrays and their sizes.
    mov eax D@BitSet
    shr eax 5 ; divide it by 32
    inc eax ; number of elements on the array
    mov D@ConvBitArrayID eax
    dec eax
    shl eax 2 ; mul by 4
    mov D@Size_Of_ConvBitArray eax

    mov edi D@String
    mov esi D@Array

    .While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi D@ConvBitArrayID;Conv96BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx D@Size_Of_ConvBitArray;Size_Of_Conv96BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi D@ConvBitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        mov eax D@ConvBitArrayID
        dec eax
        mov ecx eax
        lea ecx D$ecx*2 ; mul by 2. Why lea ? Because we don´t want to touch the carry flag.
                        ; (we could do shl ecx 1, but CF is changed then)

        L6: jz L5>
            adc D$esi+ecx 0    ; the product of the multiplication of the char by 10 is stored in esi
                               ; we need to perform a loop untill we reach the end of the array.
                               ; Why i made such a mess with the code, insetad a simple While/End_While is
                               ; because the carry flag MUST not be affected whatsoever
            dec eax | jz L5> ; if zeroed we exit the loop
            mov ecx eax
            inc ecx
            lea ecx D$ecx*4 ; mul by 4. Why lea ? Because we don´t want to touch the carry flag.
                            ; (we could do shl ecx 2, but CF is changed then)
        jmp L6<
        L5:
        ; If D$esi+Size_Of_Conv96BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv96BitArray is filled with some value, the ZF is zeroed
        je L1> | xor eax eax | ExitP | L1:


        ; Here we check if the filled data exceeded the remainder bits.
        ; Ex: If our data is a 80 bit set, we see if bit81 to 96 are filled
        ..If D@IsRemainder <> 0
            push ecx
            push ebx
                mov ebx D@Size_Of_ConvBitArray
                mov eax D$esi+ebx
                .If eax <> 0
                    mov ecx D@IsRemainder
                    shr eax cl
                    If eax <> 0
                        xor eax eax
                        pop ebx
                        pop ecx
                        ExitP
                    End_If

                .End_If
            pop ebx
            pop ecx

        ..End_If

        inc edi

    .End_While


    .If D@IsNegative = &TRUE

        neg D$esi+Size_Of_Conv32BitArray
        mov ecx 0
        mov eax D@ConvBitArrayID
        dec eax | jz L7>
        mov edx ecx

        L6: inc edx
            inc ecx
            lea ecx D$ecx*4 ; mul by 4. Why lea ? Because we don´t want to touch the carry flag.
            neg D$esi+ecx   ; Point to the next array
            sbb D$esi+ecx 0
            mov ecx edx
            dec eax | jnz L6<

            lea ecx D$ecx*4 ; mul by 4. Why lea ? Because we don´t want to touch the carry flag.
            sbb D$esi+ecx 0

        L7:
            If D@IsRemainder <> 0
                mov eax 32
                sub eax D@IsRemainder
                mov edx D$esi+ecx

                push ecx
                    mov ecx eax
                    shl edx cl
                    shr edx cl
                pop ecx

                mov D$esi+ecx edx
            End_If
    .End_If

    mov eax &TRUE

EndP
__________________________________________________________________________________________________________________

;;

    HextoBinaryString
    
    
    This function converts a hexadecimal value (Byte, word, dword, qword, tenbyte and so on) of any size, to
    a correspondant ascii binary string.


    Parameters:
    
        InputedValue:   (Input) Pointer to an address that contains the value to be converted. It can be a array
                        of dwords, a structure, a byte, a word, a dword, a sequence of bytes and so on.
        
        
        BinBuffType:    The Binary type of the data to be converted. It is the Bit Set to be converted. Ex: 128 bits
                        will display the 128 bits of the inputed value.
        
        
        TextBuffer:     (Output) Pointer to a Buffer that will hold the binary strings of the converted data.
        
        BufferLen:      Size (in bytes) of TextBuffer.
        
        

    Returned values:    If sucessiful, the function returns TRUE.
                        On failure it will return FALSE. It will fails whenever there is not enough size for the 
                        TextBuffer.


    Remarks:    The format of the decimal binary is represented in a chain of octets (8 bits), separated by a
                "__" underline mark. On each nibble (4 bits) the separation is with a single "_". Example:
    
                16 in decimal, is represented in binary string as 0001_0000. 
                The low nibble is 0000, the higher nibble is 0001
                
                336 in decimal, is represented in binary string as 0000_0001__0101_0000.
                The 1st octet is formed by the low nibble is 0000, the higher nibble is 0101
                The 2nd octet is formed by the low nibble is 0001, the higher nibble is 0000
                Between the 1st and 2nd octet we insert the separator "__"
                
    
    Example of usage:
    
        [Size_Of_TextBuffer 256]
        [BinFormTextBuffer: B$ 0 #256]
        
        [Value:
         Value.Conv32Bit: D$ 091FFFCA8
         Value.Conv64Bit: D$ 0CF064DD5
         Value.Conv80Bit: W$ 014D8]
        
        call HextoBinaryString Value, 80, BinFormTextBuffer, Size_Of_TextBuffer
        
        The return is:
        00__0001_0100__1101_1000__1100_1111__0000_0110__0100_1101__1101_0101__1001_0001__1111_1111__1111_1100__1010_1000
        
        In decimal value, this string is: 98446744073709551615144



Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;


Proc HextoBinaryString:
    Arguments @InputedValue, @BinBuffType, @TextBuffer, @BufferLen
    Uses edx

    mov edi D@TextBuffer
    mov ebx edi ; to check for delimiters
    If D@BufferLen < 12 ; the minimum size is 11 because of 8 bits number and the zero trailing: "00_00000000, 0"
        xor eax eax | ExitP
    End_If

    mov esi D@InputedValue

    mov ecx D@BinBuffType ; Get Bit number
    shr ecx 3 ; divide by 8 (Convert it to byte)
    jnc L1> ; If carry flag is checked, we have a fractional number. Ex: 117/8 = 14,625 bytes
            ; On this situation we need to add 1 byte on the product (14) for the checkings..
            ; For values smaller then 8, ZF, Cf and PF are checked. Ex: 6/8 = 0,75. so we add 1 byte to the procudt (0)
        inc ecx
    L1:

    ; get the proper amount of usable bytes. If last bytes are zeroed, we decrease them untill we reach the next used byte
    While B$esi+ecx = 0
        dec ecx
    End_While
    ; increment eax to show the correct 8 digits
    inc ecx

    add edi D@BufferLen
    dec edi ; since we need the null terminated string. we decrease it by 1 byte. (the last byte is the zero terminated)
    mov B$edi 0 ; clean the last byte.. it must be a null terminated string


L0:     mov dl 1
        lodsb

L1:     mov ah '0'
        test al dl | jz L2>
        inc ah
L2:

        If edi <= ebx   ; if the size of the buffer is not enough we return false and exit
            xor eax eax | ExitP
        End_If


        dec edi | mov B$edi ah
        cmp dl 8 | jne L3> | dec edi | mov B$edi '_' | L3: ; did we reached 4 bytes at edi ? Ex.: 0000 0000
                                                           ; it is 8 to avoid we change the carry flag
        shl dl 1
        jnc L1<

        dec edi | dec edi | mov W$edi '__'
        dec ecx | js L3> | jne L0< | L3:

        dec edi | dec edi
        mov W$edi '00'

        mov eax &TRUE

EndP

_______________________________________________________

;;

    QwordtoAscii v1.0
    
    Qword to Ascii String
    
    This function converts a qword data on Ascii Decimal String. (Only positive numbers - Unsigned)
   
    Parameters:

        dwValue: A Buffer that holds the address of the value to be converted.
        
        lpBuffer: A Buffer that will store the String on output. The lenght of the buffer must be at least 21 bytes

    Note: We use 1 Data to temporarilly store the whole Qword value. It is:
            Remainder: T$ 0
            
          This is where we store the Remainder Data of a given Qword to be computed. Example:
          ; Value to be computed is: 7766279631452241919 (Decimal) 06BC75E2D630FFFFF (Hexadecimal)
          The above value is stored on the Qword as:
            LowPart: T$ 0630FFFFF
            HiPart: T$ 06BC75E2D

          It is better to not use it as Local Variable. Insetad we are using it as Terabyte, because we need precision,
          and the Remainder is used to store huge values on the order of a power of 2^62. So we need maximum
          precision to get a perfect result.
          
        Usage Example:
        
        ;The value to be computed is: 06BC75E2D630FFFFF (Hexadecimal) = 7766279631452241919 (Decimal)

        [Test: D$ 0630FFFFF
               D$ 06BC75E2D]

        call qwordtoascii Test, Buffer

Author:
    Beyond2000! (Guga)
;;

[Remainder: T$ 0]

Proc qWordToAscii:
    Arguments @dwValue, @lpBuffer
    Local @ControlWord, @MyControlWord, @Divisor, @MaxDigits, @FinalDigit
    Uses ebx, esi, edi, eax, ecx, edx

    mov edi D@lpBuffer
    mov esi edi
    mov D@Divisor 10


    ; The maximum value allowed for a Qword is 0FFFFFFFFFFFFFFFF = 18446744073709551615, that have 20 digits
    mov D@MaxDigits 20

    ; Put the Qword on ST0. We don't need to initialize the FPU (finit) because this function already does it.
    call QwordtoST0 D@dwValue

    ; We need to reset the ControlWord to handle exceptions. What we are doing is cleaning all previous exceptions
    ; from QwordtoST0.
    fclex | fstcw W@ControlWord
    mov W@MyControlWord &FPU_EXCEPTION_INVALIDOPERATION__&FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_ZERODIV__&FPU_EXCEPTION_OVERFLOW__&FPU_EXCEPTION_UNDERFLOW__&FPU_EXCEPTION_PRECISION__&FPU_PRECISION_64BITS__&FPU_ROUNDINGMODE_TRUNCATE
    fldcw W@MyControlWord

    Do
        fst ST1
        fstp T$Remainder
        fild D@Divisor
        fxch

        fprem | FRNDINT | fistp D@FinalDigit
        mov eax D@FinalDigit
        add al '0'
        mov B$edi al
        inc edi

        fld T$Remainder
        fidiv D@Divisor

        dec D@MaxDigits
    Loop_Until D@MaxDigits = 0

    finit ; reset all the FPU Flags before exiting

    mov B$edi 0 ; terminate the string

    ; We now have all the digits, but in reverse order.

    ; Check if the last Strings starts with '0'. They are in reversed order, so we need to clean it to avoid
    ; we end up with 0001235456879....

    While B$edi-1 = '0' | dec edi | mov B$edi 0 | End_While

    ; Now we have all digits cleaned and in reversed order.

    While esi < edi

        dec edi
        mov al B$esi
        mov ah B$edi
        mov B$edi al
        mov B$esi ah
        inc esi

    End_While

EndP
_______________________________________________________

;;

    QwordtoST0 v1.0
    
    Qword to FPU Register in ST0
    
    This function puts a qword data on the ST0 Register of the FPU. (Only positive numbers - Unsigned)
   
    Parameters:

        dwValue: A Buffer that holds the address of the value to be converted.
        

    Note: We use 2 Data to temporarilly store the whole Qword value. They are:
            LowPart: D$ 0
            HiPart: D$ 0

          On each one of them we store the Data of a given Qword to be computed. Example:
          ; Value to be computed is: 7766279631452241919 (Decimal) 06BC75E2D630FFFFF (Hexadecimal)
          The above value is stored on the Qword as:
            LowPart: T$ 0630FFFFF
            HiPart: T$ 06BC75E2D

          It is better to not use them as Local Variables. Insetad we are using them as Terabytes, because we need precision,
          and the HiPart is used to store huge values on the order of a power of 2^62. So we need maximum
          precision to get a perfect result.
          
          The computations are done in 2 parts each one of them calculates and places on ST0 the Hi Part and Low Part
          of the Qword.
          
          What we are doing is computing the Power of 2 for each bit set on each Part and then adding these values together.
          This is more preise then simply Loading them onto the FPU unit directly.
          
          Keep in mind, that Hexadecimal values are calculated with a sum of a power of 2 on each Bit set.
          So, if Bit0 is checked, the value is 2^0 = 1. If Bit1 is checked is 2^1 = 2 and so on.
          For example, a value of 15 in binary is: 0000_0111. The value is computed like: 2^0 + 2^1 + 2^2 + 2^3 = 15
          
          The computations are only performing a sum of the power of 2 for each bit settled from bit 0 to 63 (That forms the qword)
    
        Usage Example:
        
        ;The value to be computed is: 06BC75E2D630FFFFF (Hexadecimal) = 7766279631452241919 (Decimal)

        [Test: D$ 0630FFFFF
               D$ 06BC75E2D]

        call QwordtoST0 Test

Author:
    Beyond2000! (Guga)
;;

[LowPart: T$ 0]
[HiPart: T$ 0]

Proc QwordtoST0:
    Arguments @Value
    Local @BitSettled, @ControlWord, @MyControlWord, @TempLowPart, @Divisor
    Uses eax, ecx, ebx, edx

    mov ecx 0 ; Bit Value
    mov ebx D@Value
    mov ebx D$ebx

    finit
    fclex | fstcw W@ControlWord
    mov W@MyControlWord &FPU_EXCEPTION_INVALIDOPERATION__&FPU_EXCEPTION_DENORMALIZED__&FPU_EXCEPTION_ZERODIV__&FPU_EXCEPTION_OVERFLOW__&FPU_EXCEPTION_UNDERFLOW__&FPU_EXCEPTION_PRECISION__&FPU_PRECISION_64BITS__&FPU_ROUNDINGMODE_TRUNCATE
    fldcw W@MyControlWord

    bt ebx 0 | jnc L1>
        fldz | jmp L2>
    L1:
        fldz | shr ebx 1 | mov ecx 1
    L2:

    ; Get 1st Part of the dword (Bits 0 to 31). The values are in form of 2^bit. Ex.: 2^0+2^1+...+2^31
Do
    ;If eax = 1 ; If Bit 1 ?
    bt ebx 0 | jnc L1>
      ;D$Result = 2^ecx
        push ebx
        push ecx
            mov D@BitSettled ecx
            xor ecx ecx
            mov eax 1
            mov ebx 2
            While ecx <> D@BitSettled
                mul ebx
                inc ecx
            End_While
        pop ecx
        pop ebx

      mov D@TempLowPart eax
      fild D@TempLowPart
      faddp ST1 ST0
    L1:
    inc ecx
    shr ebx 1
Loop_Until ecx = 32


    fstp T$LowPart ; Save the lower part of the qword

    ; Start computing from Bit32
    mov ecx 32
    mov ebx D@Value;2

    mov ebx D$ebx+4

    ; Get 2nd Part of the dword (Bits 32 to 63).The values are in form of 2^bit. Ex.: 2^0+2^1+...+2^31

    mov D@TempLowPart 0
    mov D@Divisor 2
Do
    ; Is the Bit set ? No jmp over.
    bt ebx 0 | jnc L1>

        push ebx
        push ecx
            mov D@BitSettled ecx
            fild D@BitSettled
            fild D@Divisor
            FSCALE
            fidiv D@Divisor
            fld T$HiPart
            faddp ST1 ST0
            fstp T$HiPart
            finit
        pop ecx
        pop ebx

    L1:
    inc ecx
    shr ebx 1
Loop_Until ecx = 64

    fld T$HiPart
    fld T$LowPart
    faddp ST1 ST0

    ; clear The contents of the Low and Hi Parts before exit.
    mov D$HiPart 0, D$HiPart+4 0, W$HiPart+8 0
    mov D$LowPart 0, D$LowPart+4 0, W$LowPart+8 0

EndP
__________________________________________________________________________________________________________________
;;

    Dword32toAscii
    32 Bit Data to Unsigned or Signed Ascii Decimal convertion.
    
    
    This function converts a 32 bit hexadecimal value to a unsigned or signed ascii decimal string.

    Parameters:
        
        Array:  (Input/Output) Pointer to a array of 1 dword (32 bits = 4 bytes), where each element of the array
                represents the bit set to be converted.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0]
                 
                 On output, each member of the array contains the converted value.
        
        String: (Output) The converted unsigned ascii decimal string.
        
        BufferLen: The len of the string to be converted. Make sure that the size of the string buffer is big enough
                   to hold the converted data. If you want to calculate the minimum size of the string buffer, please
                   see BitDigitConverter function.

        SignFlag: A flag to check if the output decimal string will be signed or not. The flag can be &TRUE or &FALSE
        
        Returned values:    If the function suceeds it return the starting address of the converted string.
                            This function never return false, because it will convert all the inputed
                            1 dword (32 bit).

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        
        [Size_Of_Conv32BitArray 0]

        [String: D$ 0 #125]
        [Size_Of_TextBuffer 256]
        
        [Value:
         Value.Conv32Bit: D$ 0-1]
        
        call Dword32toAscii Value, String, Size_Of_TextBuffer, &FALSE
        eax will return the string: "4294967295"
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org
;;

Proc Dword32toAscii:
    Arguments @Array, @String, @BufferLen, @SignFlag
    Local @PreviousProduct, @IsSigned
    Structure @BitHolder (32*Conv32BitArrayID), @BitHolderStartDis 0
    uses esi, edi, ecx, ebx


    mov D@IsSigned &FALSE
    mov edi D@String ; outputed
    add edi D@BufferLen
    dec edi
    dec edi ; The last byte must be 0 (null terminated string). We point the buffer to the last byte before the ending zero.

    ; The inputed data will be changed before the function exits. So we must do the computations on a copy of the Array data.
    mov eax D@Array
    lea esi D@BitHolderStartDis
    move D$esi+Size_Of_Conv32BitArray D$eax+Size_Of_Conv32BitArray

    ..If D@SignFlag = &TRUE
        push eax
            .If eax <> 0
                mov D@IsSigned &TRUE
                mov eax 0
                clc
                sub eax D$esi+Size_Of_Conv32BitArray
                mov D$esi+Size_Of_Conv32BitArray eax
            .End_If
        pop eax
    ..End_If

    xor edx edx
L2:
        ; DwordDivider routine. The same functionality as DwordDivider function, but this is faster
        ; For informations and comments about this routine, please see DwordDivider function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv32BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv32BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            div ecx
            mov D$esi+ebx eax
            sub ebx 4
            dec edi | jne L1<
        pop edi

        mov D$esi+Size_Of_Conv32BitArray 0

        clc ; always clear the carry flag before we start
        add dl '0' ; subtract eax with '0'. Convert decimal string to hex
        clc ; always clear the carry flag before we start
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char

        ; If D$esi+Size_Of_Conv96BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv96BitArray is filled with some value, the ZF is zeroed
        mov B$edi dl
        If eax = 0
            On D@PreviousProduct = 0, jmp L3> ; cases of 0FFFFFFFFF; 0FFFFFFFFFFFFFFFF
        End_If
        xor edx edx

        dec edi

        jmp L2<<

L3:
    mov eax edi ; return the buffer where the string is located

    .If W$edi <> 030 ; is this only 0 ?
        If D@IsSigned = &TRUE
            mov W$eax-2 '0-'
            sub eax 2
        End_If
    .End_If
EndP

;;

    Dword64toAscii
    64 Bit Data to Unsigned Ascii Decimal convertion.
    
    
    This function converts a 64 bit hexadecimal value to a unsigned ascii decimal string.

    Parameters:
        
        Array:  (Input/Output) Pointer to a array of 2 dwords (64 bits = 8 bytes), where each element of the array
                represents the bit set to be converted.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0]
                 
                 On output, each member of the array contains the converted value.
        
        String: (Output) The converted unsigned ascii decimal string.
        
        BufferLen: The len of the string to be converted. Make sure that the size of the string buffer is big enough
                   to hold the converted data. If you want to calculate the minimum size of the string buffer, please
                   see BitDigitConverter function.

        Returned values:    If the function suceeds it return the starting address of the converted string.
                            This function never retunrs false, because it will convert all the inputed
                            2 dwords (64 bit).

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]

        [String: D$ 0 #125]
        [Size_Of_TextBuffer 256]
        
        [Value:
         Value.Conv32Bit: D$ 0-1
         Value.Conv64Bit: D$ 0-1]
        
        call Dword64toAscii Value, String, Size_Of_TextBuffer
        eax will return the string: "18446744073709551615"
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org
;;

Proc Dword64toAscii:
    Arguments @Array, @String, @BufferLen, @SignFlag
    Local @PreviousProduct, @IsSigned
    Structure @BitHolder (32*Conv64BitArrayID), @BitHolderStartDis 0
    uses esi, edi, ecx, ebx

    mov D@IsSigned &FALSE
    mov edi D@String ; outputed
    add edi D@BufferLen
    dec edi
    dec edi ; The last byte must be 0 (null terminated string). We point the buffer to the last byte before the ending zero.

    ; The inputed data will be changed before the function exits. So we must do the computations on a copy of the Array data.
    mov eax D@Array
    lea esi D@BitHolderStartDis
    move D$esi+Size_Of_Conv32BitArray D$eax+Size_Of_Conv32BitArray
    move D$esi+Size_Of_Conv64BitArray D$eax+Size_Of_Conv64BitArray

    ..If D@SignFlag = &TRUE
        push eax
            .If eax <> 0
                mov D@IsSigned &TRUE
                clc ; always clear the carry flag before we start
                mov eax 0 | sub eax D$esi+Size_Of_Conv32BitArray | mov D$esi+Size_Of_Conv32BitArray eax
                mov eax 0
                ; if carry flag is set
                jc L1>
                    mov ecx 1 ; carry flag is not settled
                    jmp L2>
                L1:
                    xor ecx ecx ; carry flag is settled
                L2:
                clc
                sub ecx 1 ; If ecx = 0, carry flag is settled, else, cary flag is not settled
                sbb eax D$esi+Size_Of_Conv64BitArray
                mov D$esi+Size_Of_Conv64BitArray eax
            .End_If
        pop eax
    ..End_If
    xor edx edx
L2:
        ; DwordDivider routine. The same functionality as DwordDivider function, but this is faster
        ; For informations and comments about this routine, please see DwordDivider function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv64BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv64BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            div ecx
            mov D$esi+ebx eax
            ;On ebx <> 0, add edx D@PreviousProduct
            sub ebx 4
            dec edi | jne L1<
        pop edi

        mov D$esi+Size_Of_Conv32BitArray 0

        clc ; always clear the carry flag before we start
        add dl '0' ; subtract eax with '0'. Convert decimal string to hex
        clc ; always clear the carry flag before we start
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv96BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv96BitArray is filled with some value, the ZF is zeroed
        mov B$edi dl

        If eax = 0
            On D@PreviousProduct = 0, jmp L3> ; cases of 0FFFFFFFFF; 0FFFFFFFFFFFFFFFF
        End_If

        xor edx edx

        dec edi

        jmp L2<<

L3:
    mov eax edi ; return the buffer where the string is located

    .If W$edi <> 030 ; is this only 0 ?
        If D@IsSigned = &TRUE
            mov W$eax-2 '0-'
            sub eax 2
        End_If
    .End_If

EndP

;;

    Dword80toAscii
    80 Bit Data to Unsigned Ascii Decimal convertion.
    
    
    This function converts a 80 bit hexadecimal value to a unsigned ascii decimal string.

    Parameters:
        
        Array:  (Input/Output) Pointer to a array of 3 dwords (80 bits = 10 bytes), where each element of the array
                represents the bit set to be converted.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0] ; The last dword must be used only the word (Low Part) to fit the 80 bit set
                                        ; It must be smaller or equal to 65535.
                 
                 On output, each member of the array contains the converted value.
        
        String: (Output) The converted unsigned ascii decimal string.
        
        BufferLen: The len of the string to be converted. Make sure that the size of the string buffer is big enough
                   to hold the converted data. If you want to calculate the minimum size of the string buffer, please
                   see BitDigitConverter function.

        Returned values:    If the function suceeds it return the starting address of the converted string.
                            If the function fails, it will return false. It will return false whenever the inputed
                            data is bigger then 80 bits

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]

        [String: D$ 0 #125]
        [Size_Of_TextBuffer 256]
        
        [Value:
         Value.Conv32Bit: D$ 0-1
         Value.Conv64Bit: D$ 0-1
         Value.Conv96Bit: D$ 0FFFF] ; the last dword must be smaller or equal to 65535
        
        call Dword80toAscii Value, String, Size_Of_TextBuffer
        eax will return the string: "1208925819614629174706175"
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org    
;;

Proc Dword80toAscii:
    Arguments @Array, @String, @BufferLen, @SignFlag
    Local @PreviousProduct, @IsSigned
    Structure @BitHolder (32*Conv96BitArrayID), @BitHolderStartDis 0
    uses esi, edi, ecx, ebx

    mov D@IsSigned &FALSE
    mov edi D@String ; outputed
    add edi D@BufferLen
    dec edi
    dec edi ; The last byte must be 0 (null terminated string). We point the buffer to the last byte before the ending zero.

    mov eax D@Array
        ; If bits 81 to 96 are filled, return false.
        If W$eax+Size_Of_Conv96BitArray+2 <> 0 ; 96-(2*8) = 80
            xor eax eax
            ExitP
        End_If

    ; The inputed data will be changed before the function exits. So we must do the computations on a copy of the Array data.
    lea esi D@BitHolderStartDis
    move D$esi+Size_Of_Conv32BitArray D$eax+Size_Of_Conv32BitArray
    move D$esi+Size_Of_Conv64BitArray D$eax+Size_Of_Conv64BitArray
    move D$esi+Size_Of_Conv96BitArray D$eax+Size_Of_Conv96BitArray
    ..If D@SignFlag = &TRUE
        push eax
            .If eax <> 0
                mov D@IsSigned &TRUE
                clc ; always clear the carry flag before we start
                mov eax 0 | sub eax D$esi+Size_Of_Conv32BitArray | mov D$esi+Size_Of_Conv32BitArray eax
                mov eax 0 | sbb eax D$esi+Size_Of_Conv64BitArray | mov D$esi+Size_Of_Conv64BitArray eax
                mov eax 0
                ; if carry flag is set
                jc L1>
                    mov ecx 1 ; carry flag is not settled
                    jmp L2>
                L1:
                    xor ecx ecx ; carry flag is settled
                L2:
                clc
                sub ecx 1 ; If ecx = 0, carry flag is settled, else, cary flag is not settled
                sbb eax D$esi+Size_Of_Conv96BitArray
                mov D$esi+Size_Of_Conv96BitArray eax
                mov W$esi+Size_Of_Conv96BitArray+2 0 ; detele the highword of the data. It is only 80 bits, right)
            .End_If
        pop eax
    ..End_If
    xor edx edx
L2:
        ; DwordDivider routine. The same functionality as DwordDivider function, but this is faster
        ; For informations and comments about this routine, please see DwordDivider function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv96BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv96BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            div ecx
            mov D$esi+ebx eax
            ;On ebx <> 0, add edx D@PreviousProduct
            sub ebx 4
            dec edi | jne L1<
        pop edi

        mov D$esi+Size_Of_Conv32BitArray 0

        clc ; always clear the carry flag before we start
        add dl '0' ; subtract eax with '0'. Convert decimal string to hex
        clc ; always clear the carry flag before we start
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi


        If W$esi+Size_Of_Conv96BitArray+2 <> 0 ; 96-(2*8) = 80 ; if the last word is filled, we exceeded the amount of 80bit limit
            xor eax eax
            ExitP
        End_If
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv96BitArray is filled with some value, the ZF is zeroed
        mov B$edi dl
        If eax = 0
            On D@PreviousProduct = 0, jmp L3> ; cases of 0FFFFFFFFF; 0FFFFFFFFFFFFFFFF
        End_If
        xor edx edx

        dec edi

        jmp L2<<

L3:
    mov eax edi ; return the buffer where the string is located

    .If W$edi <> 030 ; is this only 0 ?
        If D@IsSigned = &TRUE
            mov W$eax-2 '0-'
            sub eax 2
        End_If
    .End_If

EndP


;;

    Dword96toAscii
    96 Bit Data to Unsigned Ascii Decimal convertion.
    
    
    This function converts a 96 bit hexadecimal value to a unsigned ascii decimal string.

    Parameters:
        
        Array:  (Input/Output) Pointer to a array of 3 dwords (96 bits = 12 bytes), where each element of the array
                represents the bit set to be converted.

                This array you can build in a form of a structure, in order to make the interpretation of each bit set
                more readable. Such as:
                
                [Value:
                 Value.Conv32Bit: D$ 0
                 Value.Conv64Bit: D$ 0
                 Value.Conv96Bit: D$ 0]
                 
                 On output, each member of the array contains the converted value.
        
        String: (Output) The converted unsigned ascii decimal string.
        
        BufferLen: The len of the string to be converted. Make sure that the size of the string buffer is big enough
                   to hold the converted data. If you want to calculate the minimum size of the string buffer, please
                   see BitDigitConverter function.

        Returned values:    If the function suceeds it return the starting address of the converted string.
                            This function never retunrs false, because it will convert all the inputed
                            3 dwords (96 bit).

    Remarks:    This function uses internal equates to place the generated data on their correct array elements. (dwords)
                The equates starting with  "Size_Of_" are only the size of each member of the array, and the ones starting
                with "Conv" are enumerations of the elements of the array (The 1st element starts with 1).
                
                If you want to analyse a set of predefined equates from Bit32 to Bit5152, please see "Ascii Convertion
                Equates Table"
    
    Example of usage:
    
        [Conv32BitArrayID 1]
        [Conv64BitArrayID 2]
        [Conv96BitArrayID 3]
        
        [Size_Of_Conv32BitArray 0]
        [Size_Of_Conv64BitArray 4]
        [Size_Of_Conv96BitArray 8]

        [String: D$ 0 #125]
        [Size_Of_TextBuffer 256]
        
        [Value:
         Value.Conv32Bit: D$ 0-1
         Value.Conv64Bit: D$ 0-1
         Value.Conv96Bit: D$ 0-1]
        
        call Dword96toAscii Value, String, Size_Of_TextBuffer
        eax will return the string: "79228162514264337593543950335"
        
Author: Gustavo Trigueiros (aka Beyond2000!)
www.rosasm.org
;;

Proc Dword96toAscii:
    Arguments @Array, @String, @BufferLen
    Local @PreviousProduct

    mov edi D@String ; outputed
    add edi D@BufferLen
    dec edi ; The last byte must be 0 (null terminated string). We point the buffer to the last byte before the ending zero.
    mov esi D@Array

    xor edx edx
L2:
        ; DwordDivider routine. The same functionality as DwordDivider function, but this is faster
        ; For informations and comments about this routine, please see DwordDivider function.
        push edi
            xor eax eax
            mov ecx 10
            mov edi Conv96BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv96BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            div ecx
            mov D$esi+ebx eax
            ;On ebx <> 0, add edx D@PreviousProduct
            sub ebx 4
            dec edi | jne L1<
        pop edi

        mov D$esi+Size_Of_Conv32BitArray 0

        add dl '0' ; subtract eax with '0'. Convert decimal string to hex
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv96BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv96BitArray is filled with some value, the ZF is zeroed
        mov B$edi dl
        On eax = 0, jmp L3>
        xor edx edx

        dec edi

        jmp L2<<

L3:
    mov eax edi ; return the buffer where the string is located

EndP





;;
 convert a decimal string to qword value
 number in edx:eax
 eax = lowpart
 edx = hipart
;;
Proc atoi64:
    Arguments @String

    mov ebx D@String

    xor esi esi ; HiQword
    xor ecx ecx ; LowQWord

    While B$ebx <> 0
        call alldecmul ecx, esi
        mov ecx eax
        mov esi edx

        movsx eax B$ebx
        sub eax '0' | jc L7>
        cmp eax 9 | ja L7>
        cdq
        add ecx eax
        adc esi edx

        inc ebx
        cmp B$ebx LowSigns | jbe L6>

    End_While

L6:
    mov eax ecx
    mov edx esi

    mov esi ebx ; to avoid error anywhere else

    jmp L8>
L7:
   mov ecx D$DezimalTypePtr
   mov esi ebx
   jmp BadNumberFormat

L8:

EndP

;;
Multiply a Qword unsigned integer by 10

;;

Proc alldecmul:
    Arguments @ValA_Low, @ValA_Hi
    uses esi, ebx

    mov eax D@ValA_Hi
    mov ecx 10
    mov ebx D@ValA_Low

    cmp eax 0 | jne @Notzero
    mov eax ebx
    mul ecx
    ExitP

@Notzero:

    mul ecx

    mov esi eax
    mov eax ebx ; ValA_Low

    mul ecx
    add edx esi

EndP




Proc HextoBinary2:
    Arguments @InputedValue, @BinBuffType, @BufferLen, @TextBuffer
    Uses edx

    mov edi D@TextBuffer
    mov esi D@InputedValue

    mov ecx D@BinBuffType
    add edi D@BufferLen


L0:     mov dl 1
        lodsb

L1:     mov ah '0'
        test al dl | jz L2>
        inc ah
L2:     dec edi | mov B$edi ah
        cmp dl 8 | jne L3> | dec edi | mov B$edi '_' | L3: ; did we reached 4 bytes at edi ? Ex.: 0000 0000
                                                           ; it is 8 to avoid we change the carry flag
        shl dl 1
        jnc L1<

        dec edi | dec edi | mov W$edi '__'
        dec ecx | js L3> | jne L0< | L3:
        ;loop L0<
        mov W$edi 0
        inc edi
        inc edi

EndP

;;

    AsciiBase
    
    This function converts an decimal, Hexadecimal, Octal, Base Four, and Binary string to 32 Bits dword value.
    
    Parameters:
    
        String: The inputed string to be converted. It can be a null terminated string or not.
                The input can be a string representation of a Hexadecimal, Binary, Octal, Base Four or Decimal Numbers.
        
        Base:   An constant representing the Base of the value be converted.
                The following constantes are used:
             
                    Constant name      Value
                    
                    Base_Hex            16             
                    Base_Dec            10
                    Base_Oct             8
                    Base_Four            4
                    Base_Bin             2

    Return values:
    
            The 32 bits dword value is returned in eax.
            
Usage Examples:

    a)
    
        [HexaString: '19E', 0]
        (...)
        call AsciiBase HexaString Base_Hex

    b)
    
        [DecimalString: B$ '414']
        [ResultData: B$ '271       ' ]
        [Buffer: D$ 0]
        
        (...)
        call AsciiBase {'19E', 0} Base_Hex
        call AsciiBase DecimalString Base_Dec
        
        call RemoveChar ResultData Buffer ' '
        call AsciiBase Buffer Base_Oct

        hexprint eax
        

Author:
    René Tournois - Betov (Original author)
;;

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


Proc AsciiBaseto32:
    Arguments @String, @Array, @IsNegative, @Base
    Local @PreviousProduct


    pushad

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx D@Base
            mov edi Conv64BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv64BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv64BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        On eax > 9, sub eax 7 ; cases of hexa notation
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        ; If D$esi+Size_Of_Conv64BitArray <> 0, return false and exit.
        ; We don´t need a "cmp", because the Zero Flag is settled or not from the "adc" mnemonic above.
        ; So, whenever the address at D$esi+Size_Of_Conv64BitArray is filled with some value, the ZF is zeroed
        je L1> | popad | xor eax eax | ExitP | L1:
;;
        If D$esi+Size_Of_Conv64BitArray <> 0
            xor eax eax
            ExitP
        End_If
;;
        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
    .End_If

    popad
    mov eax &TRUE

EndP


;;

    AsciiBase64
    
    This function converts an decimal, Hexadecimal, Octal, Base Four, and Binary string to 64 Bits dword value.
    
    Parameters:
    
        String: The inputed string to be converted. It can be a null terminated string or not.
                The input can be a string representation of a Hexadecimal, Binary, Octal, Base Four or Decimal Numbers.
        
        Base:   An constant representing the Base of the value be converted.
                The following constantes are used:
             
                    Constant name      Value
                    
                    Base_Hex            16             
                    Base_Dec            10
                    Base_Oct             8
                    Base_Four            4
                    Base_Bin             2

        Output: An array of 2 dwords that holds the converted value

    Return values:
    
            This function does not returns any values
    Remarks: This function does not check for the syntax of the inputed string
    
Usage Examples:

    a)
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0]
        [HexaString: '19E', 0]
        (...)
        call AsciiBase64 HexaString, Base_Hex, Value

    b)
        [Value:
         Value.Conv32Bit: D$ 0
         Value.Conv64Bit: D$ 0]
        [HexaString: '0FFFFFEBABAA00179', 0]
        (...)
        call AsciiBase64 HexaString, Base_Hex, Value    
       

Author:
    Gustavo Trigueiros - Guga (Aka Beyond2000!) May/2.009
;;
; 2^bit63 = 08000000000000000 = 9223372036854775808
Proc AsciiBase64:
    Arguments @String, @Base, @Output
    Uses esi, ebx, edx

    mov esi D@String, eax 0, ebx 0, edi D@Output
    While B$esi <> 0
        mov eax ebx, D$edi+Size_Of_Conv32BitArray ebx | mul D@Base
        jo L2> | jmp L3>
        L2:
        If D$edi+Size_Of_Conv64BitArray = 0 ; is the next dword empty ?
            mov D$edi+Size_Of_Conv64BitArray ebx
        End_If
        L3:
        mov ebx eax, eax 0
        mov al B$esi | sub al '0'
        ; Cases of Hexa Notation:
        On al > 9, sub al 7
        add ebx eax | inc esi
        mov D$edi+Size_Of_Conv32BitArray ebx
    End_While

EndP


Proc AsciiBase80:
    Arguments @String, @Array, @IsNegative, @BASE
    Local @PreviousProduct

    mov edi D@String
    mov esi D@Array

    While B$edi <> 0

        ; DwordMultiplier routine. The same functionality as DwordMultiplier function, but this is faster
        ; For informations and comments about this routine, please see DwordMultiplier function.
        push edi
            xor eax eax
            mov ecx D@Base
            mov edi Conv96BitArrayID ; number of elements on the array
                                           ; ebx = size of the last element of the array
            mov ebx Size_Of_Conv96BitArray ; On this function it is better a mov here then a lea. So we avoid AGI stalls.

        L1: mov D@PreviousProduct eax
            mov eax D$esi+ebx
            mul ecx
            mov D$esi+ebx eax
            add edx D@PreviousProduct
            ; compare edi with the amount of elements we have
            cmp edi Conv96BitArrayID | je L0> | mov D$esi+ebx+4 edx | L0:
            sub ebx 4
            dec edi | jne L1<
        pop edi

        movsx eax B$edi

        sub eax '0' ; subtract eax with '0'. Convert decimal string to hex
        On eax > 9, sub eax 7 ; cases of hexa notation
        add D$esi+Size_Of_Conv32BitArray eax  ; 1st char
        adc D$esi+Size_Of_Conv64BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi
        adc D$esi+Size_Of_Conv96BitArray 0    ; the product of the multiplication of the char by 10 is stored in esi

        If W$esi+Size_Of_Conv96BitArray+2 <> 0 ; 96-(2*8) = 80
            xor eax eax
            ExitP
        End_If

        inc edi

    End_While

    .If D@IsNegative = &TRUE
        neg D$esi+Size_Of_Conv32BitArray
        neg D$esi+Size_Of_Conv64BitArray | sbb D$esi+Size_Of_Conv64BitArray 0
        neg W$esi+Size_Of_Conv96BitArray | sbb W$esi+Size_Of_Conv96BitArray 0
    .End_If

    mov eax &TRUE

EndP


































