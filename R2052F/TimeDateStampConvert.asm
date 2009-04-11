TITLE TimeDateStampConvert
______________________________________________________________________________________
______________________________________________________________________________________


; Functions related to Time convertion:
; TimeDateStampToString , CurrentTimeDateStampToString , TimeDateStringtoDword, GetDayofWeekinCurrentMonth
; GetMonthStringandValue, CurrentDateStampToString, CurrentTimeStampToString, SBMBinSearch



;;

        __________________________________________________________________________________________________

                                        Time Date Stamp to String function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

TimeDateStampToString



    The TimeDateStampToString function was builded to help the user to convert a time date stamp (found inside the IMAGE_NT_HEADERS
structure) into a readable string.

    Nowadays, there are five time formats, as described in the table below:
    
    
    
    Format      Type Definition         Description
    _______________________________________________________________________________________________________________
    
    System      SYSTEMTIME              Year, month, day, hour, second, millisecond,
                                        achieved internally from the hardware clock.
                                        
    File        FILETIME                100-nanosecond intervals since January 1, 1601.
    
    Local       SYSTEMTIME or FILETIME  A system time or file time converted to the system's local time zone. 
    
    MS-DOS      WORD                    A packed word for the date, another for the time.
    
    Windows     DWORD                   The number of milliseconds since the system booted; a quantity that cycles
                                        every 49.7 days.


    Our function basically compares 02 diferents time data. The diference resultant is our data time.
For start we have to calculate the time stamp of IMAGE_NT_HEADERS structure.

    The member that we use is TimeDateStamp, that represents the date and time the file header was created by the linker.
The value is represented in the number of seconds elapsed since midnight (00:00:00), January 1, 1970,
Universal Coordinated Time (UTC), according to the system clock.


    Next, for calculating the time that the file was created, we must use the FILETIME structure. A file time is a
64-bit value (Qword) that represents the number of 100 nanosecond intervals that have elapsed since
12:00 A.M. January 1, 1601 (UTC). The system records file times whenever applications create, access, and write to files.
   
   So, for files, there should not be any with a date before 1st of january, 1601. This is our comparison time start.
Since, the TimeDateStamp member calculates values after 1970, we need to calculate the data between 1601 and 1970,
which is nothing more, nothing less then the amount of seconds between them.

    When we deal with those periods of time, we are naming them as Epochs. Both epochs are Gregorian.
TimeDateStamp Member (And also Unix files) is 1970 and win32 is 1601 so 370 is the number of years (1970-1601 = 369)

    Assuming a leap year every four years gives 92 leap years (369 / 4 = 92).
However, 1700, 1800, and 1900 were NOT leap years, which leaves us with 89 leap years and 280 non-leap years.

    To Calculate the number of days:

    (Leap Years)*(Number of days on 01 Leap year) + (Non Leap Years)*(Number of days on 01 Non Leap year)
    
    89 * 366 + 280 * 365 = 134744 days between epochs.
    
    To Calculate the number of seconds:
    
    01 Day = 24 Hours * 60 minutes * 60 seconds = 86400 seconds per day,
    

    The amount of seconds between the epochs are calculated by multiplying both results.
    So, 134744 * 86400 = 11644473600 seconds between epochs.
    
    
    This result is also confirmed in the MSDN documentation on how to convert a time_t value to a win32 FILETIME. Example
found there is:

        Microsoft:
        
            void TimetToFileTime( time_t t, LPFILETIME pft )
            {
                LONGLONG ll = Int32x32To64(t, 10000000) + 116444736000000000;
                pft->dwLowDateTime = (DWORD) ll;
                pft->dwHighDateTime = ll >>32;
            }

        Unix:
        
            static void TimetToFileTime( time_t t, LPFILETIME pft )
            {
                UINT64 u = ((UINT64)t + &EPOCH_DIFF_SECS) * &SECS_TO_100NS;
                pft->dwLowDateTime = (DWORD)u;
                pft->dwHighDateTime = (DWORD)(u>>32);
            }



    Ok, now that we calculated the amount of seconds between the Epochs, we need to calculate the amount of 100 nanoseconds
intervals to be used with the FILETIME structure. To do that we just perform this calculation:

    01 sec = 1000000000 nanoseconds

    The interval is from each 100 nanoseconds...So:
    
    Interval = 1000000000 / 100 = 10000000


    To calculate the amount of intervals of 100 nanoseconds between 1601 and 1970 we just multiply the results:
    
    11644473600 * 10000000 = 116444736000000000 intervals (019DB1DED53E8000 in hexadecimal value)


    All of this is necessary because our function will just perform a check for the time stamp strings, after calculating
the amount of time based on a fixed value. This value is exactely the amount of 100 nanoseconds intervals between 1601 and 1970.
So, the time stamp will begin with any value after that period of time.

    We get the actual time stamp found in the IMAGE_FILE_HEADER structure, and then multily the value with 10000000 to 
know exactly the amount of 100 nanoseconds intervals existant. Then we add to the result the amount of the intervals between
1601 and 1970.

    This express better the situation:
    
    
    Initial Basic Time Period               End of Basic Time Period (IMAGE_NT_HEADERS)                      PE FILE

   12:00:00 AM January 1, 1601                      (00:00:00), January 1, 1970                               XXXXX
   
            |<________________________________________________>|<______________________________________________>|
            
                                     |                                                     |
                 Fixed Basic Time interval of 100 nanoseconds       Time interval of 100 nanoseconds after 01/01/1970
                          (116444736000000000)                 +                 (XXXXXXXXXXXXXXXXXXXXXXX)
                          
                          
            <------------------------|-----------------------------------------------------|-------------------->
                                        The resultant value is the sum of the 02 intervals
   

    The final data will then be passed onto the FILEIME structure and used values on SYSTEMTIME structure to we get the value
of month, day, year, hours etc...related to that structure.    


    Constants related to this function and other eventually used to find or convert the time stamp are:

&MAX_UTC_TIME_LEN    =   80 ; Maximum bytes to be displayed on the string to be shownd

&SECS_PER_MIN        =   60  ; Amount of total seconds in 01 Minute

&SECS_PER_HOUR       =   (60*SECS_PER_MIN)   = 3600 ; Amount of total seconds in 01 Hour

&SECS_PER_DAY        =   (24*SECS_PER_HOUR)  = 86400 ; Amount of total seconds in 01 Day

&SECS_PER_YEAR       =   (365*SECS_PER_DAY)  = 31536000 ; Amount of total seconds in 01 Year

&EPOCH_DIFF_DAYS     =   134744  ; Amount of days between epochs (1601/1970).

&EPOCH_DIFF_SECS     =   (EPOCH_DIFF_DAYS*SECS_PER_DAY)  =  11644473600 ; Amount of seconds between epochs (1601/1970).
                                    This data is represented as Qword.
                                    
&SECS_TO_100NS       =   10000000 ; Amount of 100 nanoseconds intervals per second (10^7)

&EPOCH_DIFF_SECS_INTERVAL    =   (EPOCH_DIFF_SECS*SECS_TO_100NS) = 116444736000000000 = 019DB1DED53E8000 (In hexadecimal) ;
                                Amount of 100 nanoseconds intervals between epochs (1601/1970), represented as Qword.

&EPOCH_DIFF_SECS_INTERVAL_LOW    =   0D53E8000 (In hexadecimal);  Low-order Part of EPOCH_DIFF_SECS_INTERVAL.

&EPOCH_DIFF_SECS_INTERVAL_HIGH   =   019DB1DE  (In hexadecimal);  High-order Part of EPOCH_DIFF_SECS_INTERVAL.

&EPOCH_DIFF_SECS_INTERVAL ; &EPOCH_DIFF_SECS ; &MAX_TIME_VALUE (Also a qword), but is not being shownd completelly on
Win Equate Value and on Bases Forms....
And on compilation, only the low order part is being compiled, and not the full value
                

Syntax:

TimeDateStampToString ; (Function name)

    TimeStamp, DateFmt, TimeFmt, DateOut, TimeOut ; (Parameters names)
        


Parameters:


    TimeStamp:  Pointer to the Data Value of the time stamp found in the Target File. If you loaded the file and achieved
              the value of the TimeDateStamp on the IMAGE_NT_HEADERS, this parameters is a pointer to the value. For example:
              D$TimeData, where TimeData = 03836401D (1999/11/20 sab 06:30:53 UTC).
              Also, if you don't want to use this parameter as a pointer, you can directly use the value of the timedate stamp,
              assuming you know what it is. This parameter is used as Input.
              
    
    
    DateFmt:  Pointer to a string used to form the date string. The string must be zero terminated. This parameter can't
              be NULL.
              
              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "MM" not "mm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
                            
                            d           Day of month as digits with no leading zero for single-digit days.
                            
                            dd          Day of month as digits with leading zero for single-digit days.
                            
                            ddd         Day of week as a three-letter abbreviation. The function uses the
                                        LOCALE_SABBREVDAYNAME value associated with the specified locale.
                                        
                            dddd        Day of week as its full name. The function uses the LOCALE_SDAYNAME value
                                        associated with the specified locale.
                                        
                            M           Month as digits with no leading zero for single-digit months.
                            
                            MM          Month as digits with leading zero for single-digit months.
                            
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.
                            
                            y           Year as last two digits, but with no leading zero for years less than 10.
                            
                            yy          Year as last two digits, but with leading zero for years less than 10.
                            
                            yyyy        Year represented by full four digits.
                            
                            gg          Period/era string. The function uses the CAL_SERASTRING value associated with the
                                        specified locale. This element is ignored if the date to be formatted does not have
                                        an associated era or period string.
                                        

              For example, to get the date string
                
                "Sun, Jun 06 04"
              
              use the following picture string:
                
                "ddd',' MMM dd yy"

              
              This parameter is used as input.

    
    TimeFmt:  Pointer to a string used to form the time string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "ss" not "SS").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            h           Hours with no leading zero for single-digit hours; 12-hour clock.
                            
                            hh          Hours with leading zero for single-digit hours; 12-hour clock.
                            
                            H           Hours with no leading zero for single-digit hours; 24-hour clock.
                            
                            HH          Hours with leading zero for single-digit hours; 24-hour clock.
                            
                            m           Minutes with no leading zero for single-digit minutes.
                            
                            mm          Minutes with leading zero for single-digit minutes.
                            
                            s           Seconds with no leading zero for single-digit seconds.
                            
                            ss          Seconds with leading zero for single-digit seconds.
                            
                            t           One character time-marker string, such as A or P.
                            
                            tt          Multicharacter time-marker string, such as AM or PM.

              
              For example, to get the time string 

                "11:29:40 PM"

              use the following picture string: 

                "hh':'mm':'ss tt"


              This parameter is used as input.
    
    
    DateOut:    Pointer to a buffer that receives the formatted date string, and output the result in the format specified
              in DateFmt. The buffer can be limited to the lenght of the formated string.
    
    
    TimeOut:    Pointer to a buffer that receives the formatted time string, and output the result in the format specified
              in TimeFmt. The buffer can be limited to the lenght of the formated string.

        


Usage Examples:


a)

[DataPointer: D$ ?] ; Points to the data that stored the value of the timedate stamp.
[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)


        call TimeDateStampToString D$DataPointer {"yyyy/MM/dd ddd ", 0},
                                                 {"HH:mm:ss UTC", 0},
                                                  szDateString szTimeString

b)

[DataPointer: D$ ?] ; Points to the data that stored the value of the timedate stamp.

[Sz_Year: B$ "yyyy/MM/dd ddd ",0]
[Sz_Hour: B$ "HH:mm:ss UTC",0]

[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)


        call TimeDateStampToString D$DataPointer Sz_Year Sz_Hour szDateString szTimeString

c)


[DataPointer: D$ ?] ; Points to the data that stored the value of the timedate stamp.

[Sz_Year: B$ "yyyy/MM/dd ddd ",0
 Year_Len: D$ len]
[Sz_Hour: B$ "HH:mm:ss UTC",0
 Hour_Len: D$ len]


        call TimeDateStampToString D$DataPointer Sz_Year Sz_Hour Year_Len Hour_Len
        

Notes:

I - Concerning the Equates

    The Equates used on this function are mostly Dwords. So, you can use them like:
    
        push &MAX_UTC_TIME_LEN
        
        or
        
        mov eax &MAX_UTC_TIME_LEN
        
        or
        
        call XXXX &MAX_UTC_TIME_LEN YYYYY
        
        
        etc etc
        
        
      But, there are exceptions. Two of the equates are Qwords. So, to fully get their high and low parts, you must
    use them as the following example:
    
    

    [EPOCH_DIFF_SECS_INTERVAL 019DB1DED53E8000]
                                    
    [Data01: Q$ EPOCH_DIFF_SECS_INTERVAL]
        
        push Q$Data01
        
        or
        
        mov eax Q$Data01
        
        etc etc
        
        
    These exceptions are inside the equates.equ, but to use them fully you must define them as an constant, and not
  use them as an regular Equate. So, you don't need to use the "&" char. At least untill the tme this function was written (06/06/04).
  Since RosAsm is under constant development, in the future, you may be able to use the Qworded equates fully.
      
        
    List of Equates and their data types
    
    Equates                         Data Type
    
    &MAX_UTC_TIME_LEN               Dword
    
    &SECS_PER_MIN                   Dword

    &SECS_PER_HOUR                  Dword

    &SECS_PER_DAY                   Dword

    &SECS_PER_YEAR                  Dword

    &EPOCH_DIFF_DAYS                Dword

    &EPOCH_DIFF_SECS_INTERVAL_LOW   Dword

    &EPOCH_DIFF_SECS_INTERVAL_HIGH  Dword

    &SECS_TO_100NS                  Dword

    &EPOCH_DIFF_SECS_INTERVAL       Qword

    &EPOCH_DIFF_SECS                Qword


    
    
      
        mov eax D$edi+FileHeader.TimeDateStampDis
        mov edx 10000000
        mul edx

                   
        add eax 0D53E8000 ; 019DB1DE D53E8000
        adc edx 019DB1DE ; 019DB1DE

 
        mov D$St_DateTimeStamp.dwLowDateTime eax
        mov D$St_DateTimeStamp.dwHighDateTime edx
        
        call 'kernel32.FileTimeToSystemTime' St_DateTimeStamp St_UTC;stLocal

        call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT &NULL St_UTC Sz_Year2 szDateString 64
        call 'KERNEL32.GetTimeFormatA' &LOCALE_SYSTEM_DEFAULT &NULL St_UTC Sz_Hour2 szTimeString 64


II - Structures used

    This function uses two fixed structures. FILETIME and SYSTEMTIME.
    
    The FILETIME structure is used to store the data found by TimeStamp. When the time date is found he add the result in
higher and lower part to  &EPOCH_DIFF_SECS_INTERVAL_LOW &EPOCH_DIFF_SECS_INTERVAL_HIGH equates.
                   
    The result is a new data time stamp stored at FILETIME structure.
    
    
    Then, the result will be passed to the SYSTEMTIME structure in order to convert the data to minutes, days, hours etc. It
will be converted through the FileTimeToSystemTime function.

    To achieve the results for date and time, all you need is use the new data in the SYSTEMTIME structure and pass it through
GetDateFormat and GetTimeFormat functions.


    This function is limited, since we use fixed values for the parameters of the GetDateFormat and GetTimeFormat functions, but
you can also define another parameter in the TimeDateStampToString that can be a pointer to a structure that can handle those values.
This structure, can contain data to be stored to be used in the timing functinos, to the user define wheter he will use a &LOCALE_SYSTEM_DEFAULT,
or a &MAX_UTC_TIME_LEN etc etc....You can define in this structure a mix of the parameters used in the GetDateFormat and GetTimeFormat functions.

    On that way you can get more control on the functinos. This structure can be used either as local or a regular one.
    

Bibliographic Reference:

    http://dotnet.di.unipi.it/Content/sscli/docs/doxygen/pal/filetime_8c-source.html
    http://xml-x.org/time.html#time
    http://bg.php.net/manual/de/function.date.php
    http://de3.php.net/manual/it/function.mktime.php
    http://www.tex.ac.uk/CTAN/systems/texlive/Source/source-win32-4.diff
    http://lists.helixcommunity.org/pipermail/common-cvs/2003-February/000516.html    
    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/converting_a_time_t_value_to_a_file_time.asp

;;




; FILETIME structure
[St_DateTimeStamp:
 St_DateTimeStamp.dwLowDateTime: D$ 0
 St_DateTimeStamp.dwHighDateTime: D$ 0]


; SYSTEMTIME structure

[St_UTC:
 St_UTC.wYear: W$ 0
 St_UTC.wMonth: W$ 0
 St_UTC.wDayOfWeek: W$ 0
 St_UTC.wDay: W$ 0
 St_UTC.wHour: W$ 0
 St_UTC.wMinute: W$ 0
 St_UTC.wSecond: W$ 0
 St_UTC.wMilliseconds: W$ 0]

Proc TimeDateStampToString:
    Arguments @TimeStamp, @DateFmt, @TimeFmt, @DateOut, @TimeOut

        mov eax D@TimeStamp
        mov edx &SECS_TO_100NS
        mul edx

        add eax &EPOCH_DIFF_SECS_INTERVAL_LOW
        adc edx &EPOCH_DIFF_SECS_INTERVAL_HIGH

        mov D$St_DateTimeStamp.dwLowDateTime eax
        mov D$St_DateTimeStamp.dwHighDateTime edx

        call 'kernel32.FileTimeToSystemTime' St_DateTimeStamp, St_UTC

        call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT,
                                       &NULL,
                                       St_UTC,
                                       D@DateFmt,
                                       D@DateOut,
                                       &MAX_UTC_TIME_LEN

        call 'KERNEL32.GetTimeFormatA' &LOCALE_SYSTEM_DEFAULT,
                                       &NULL,
                                       St_UTC,
                                       D@TimeFmt,
                                       D@TimeOut,
                                       &MAX_UTC_TIME_LEN
EndP




_____________________________________________________________________________________________________________________

;;

        __________________________________________________________________________________________________

                                        Current Time and Date Stamp to String function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

CurrentTimeDateStampToString


    The CurrentTimeDateStampToString function was builded to help the user to convert the current time date stamp
    of his system into a readable string.


Parameters:

 @DateFmt, @TimeFmt, @DateOut, @TimeOut, @SysTimeStr, @FileTimeStr
    
    DateFmt:  Pointer to a string used to form the date string. The string must be zero terminated. This parameter can't
              be NULL.
              
              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "MM" not "mm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
                            
                            d           Day of month as digits with no leading zero for single-digit days.
                            
                            dd          Day of month as digits with leading zero for single-digit days.
                            
                            ddd         Day of week as a three-letter abbreviation. The function uses the
                                        LOCALE_SABBREVDAYNAME value associated with the specified locale.
                                        
                            dddd        Day of week as its full name. The function uses the LOCALE_SDAYNAME value
                                        associated with the specified locale.
                                        
                            M           Month as digits with no leading zero for single-digit months.
                            
                            MM          Month as digits with leading zero for single-digit months.
                            
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.
                            
                            y           Year as last two digits, but with no leading zero for years less than 10.
                            
                            yy          Year as last two digits, but with leading zero for years less than 10.
                            
                            yyyy        Year represented by full four digits.
                            
                            gg          Period/era string. The function uses the CAL_SERASTRING value associated with the
                                        specified locale. This element is ignored if the date to be formatted does not have
                                        an associated era or period string.
                                        

              For example, to get the date string
                
                "Sun, Jun 06 04"
              
              use the following picture string:
                
                "ddd',' MMM dd yy"

              
              This parameter is used as input.

    
    TimeFmt:  Pointer to a string used to form the time string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "ss" not "SS").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            h           Hours with no leading zero for single-digit hours; 12-hour clock.
                            
                            hh          Hours with leading zero for single-digit hours; 12-hour clock.
                            
                            H           Hours with no leading zero for single-digit hours; 24-hour clock.
                            
                            HH          Hours with leading zero for single-digit hours; 24-hour clock.
                            
                            m           Minutes with no leading zero for single-digit minutes.
                            
                            mm          Minutes with leading zero for single-digit minutes.
                            
                            s           Seconds with no leading zero for single-digit seconds.
                            
                            ss          Seconds with leading zero for single-digit seconds.
                            
                            t           One character time-marker string, such as A or P.
                            
                            tt          Multicharacter time-marker string, such as AM or PM.

              
              For example, to get the time string 

                "11:29:40 PM"

              use the following picture string: 

                "hh':'mm':'ss tt"


              This parameter is used as input.
    
    
    DateOut:  Pointer to a buffer that receives the formatted date string, and output the result in the format specified
              in DateFmt. The buffer can be limited to the lenght of the formated string.
    
    
    TimeOut:  Pointer to a buffer that receives the formatted time string, and output the result in the format specified
              in TimeFmt. The buffer can be limited to the lenght of the formated string.


    SysTimeStr: Pointer to a SYSTEMTIME structure buffer where it will output the result data of it's members (Day, Day of Week,
                time, hour, seconds, etc).
                If the value of this parameter is &NULL, it won´ output the data.


    FileTimeStr:    Pointer to a FILETIME structure buffer where it will output the result data of it's members (Low and High Values ot the timedate stamp).
                If the value of this parameter is &NULL, it won´ output the data.


Usage Examples:


a)

[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)


        call CurrentTimeDateStampToString {"yyyy/MM/dd ddd ", 0}, {"HH:mm:ss UTC", 0}, szDateString, szTimeString, &NULL, &NULL

b)

[Sz_Year: B$ "yyyy/MM/dd ddd ",0]
[Sz_Hour: B$ "HH:mm:ss UTC",0]

[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)


        call CurrentTimeDateStampToString Sz_Year, Sz_Hour, szDateString, szTimeString, &NULL, &NULL

c)

; system_time SYSTEMTIME Structure
[system_time:
 system_time.wYear: W$ 0
 system_time.wMonth: W$ 0
 system_time.wDayOfWeek: W$ 0
 system_time.wDay: W$ 0
 system_time.wHour: W$ 0
 system_time.wMinute: W$ 0
 system_time.wSecond: W$ 0
 system_time.wMilliseconds: W$ 0]

; FILETIME structure
[St_DateTimeStamp:
 St_DateTimeStamp.dwLowDateTime: D$ 0
 St_DateTimeStamp.dwHighDateTime: D$ 0]


[Sz_Year: B$ "yyyy/MM/dd ddd ",0
 Year_Len: D$ len]
[Sz_Hour: B$ "HH:mm:ss UTC",0
 Hour_Len: D$ len]


        call CurrentTimeDateStampToString Sz_Year, Sz_Hour, Year_Len, Hour_Len, system_time, St_DateTimeStamp
        
;;

Proc CurrentTimeDateStampToString:
    Arguments @DateFmt, @TimeFmt, @DateOut, @TimeOut, @SysTimeStr, @FileTimeStr
    Structure @St_UTC 16, @St_UTC.wYearDis 0,  @St_UTC.wMonthDis 2,  @St_UTC.wDayOfWeekDis 4,  @St_UTC.wDayDis 6,  @St_UTC.wHourDis 8,  @St_UTC.wMinuteDis 10,  @St_UTC.wSecondDis 12,  @St_UTC.wMillisecondsDis 14
    Local @St_DateTimeStamp.dwLowDateTime, @St_DateTimeStamp.dwHighDateTime

    call 'KERNEL32.GetLocalTime' D@St_UTC
    If D@SysTimeStr <> 0
        mov edi D@SysTimeStr
        mov esi D@St_UTC
        mov ecx 0
        While ecx <> Size_Of_SYSTEMTIME
            movsb
            inc ecx
        End_While
    End_If
    lea eax D@St_DateTimeStamp.dwLowDateTime
    call 'KERNEL32.SystemTimeToFileTime' D@St_UTC, D$eax

    lea eax D@St_DateTimeStamp.dwLowDateTime
    call 'kernel32.FileTimeToSystemTime' D$eax, D@St_UTC
    If D@FileTimeStr <> 0
        mov edi D@FileTimeStr
        mov esi D@St_DateTimeStamp.dwLowDateTime
        mov ecx 0
        While ecx <> Size_Of_FILETIME
            movsb
            inc ecx
        End_While
    End_If
    call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@DateFmt D@DateOut &MAX_UTC_TIME_LEN
    call 'KERNEL32.GetTimeFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@TimeFmt D@TimeOut &MAX_UTC_TIME_LEN

EndP

_____________________________________________________________________________________________________________________



;;

        __________________________________________________________________________________________________

                                   Time and Date String to TimeDate Stamp (Dword) function. v 1.0
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

TimeDateStringtoDword


    The TimeDateStringtoDword function converts a Time and Date Strings to the dword value of the TimeDateStamp.
    It is the reverse of the function TimeDateStampToString.
    
    When this function was built (20/03/2006), we did not inserted any error checkings. So, you must obey the
    formats for the strings to be parsed to avoid errors.


Parameters:
 @TimeStamp, @DateFmt, @TimeFmt, @DateString, @TimeString
     
    TimeStamp: Pointer to a Buffer that will hold the outputed value of the converted timestamp.
    
    DateFmt:  Pointer to a string used to form the date string. The string must be zero terminated. This parameter can't
              be NULL.
              
              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "MM" not "mm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
                            
                            d           Day of month as digits with no leading zero for single-digit days.
                            
                            dd          Day of month as digits with leading zero for single-digit days.
                            
                            ddd         Day of week as a three-letter abbreviation. The function uses the
                                        LOCALE_SABBREVDAYNAME value associated with the specified locale.
                                        
                            dddd        Day of week as its full name. The function uses the LOCALE_SDAYNAME value
                                        associated with the specified locale.
                                        
                            M           Month as digits with no leading zero for single-digit months.
                            
                            MM          Month as digits with leading zero for single-digit months.
                            
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.
                            
                            y           Year as last two digits, but with no leading zero for years less than 10.
                            
                            yy          Year as last two digits, but with leading zero for years less than 10.
                            
                            yyyy        Year represented by full four digits.
                            
                            gg          Period/era string. The function uses the CAL_SERASTRING value associated with the
                                        specified locale. This element is ignored if the date to be formatted does not have
                                        an associated era or period string.
                                        

              For example, to get the date string
                
                "Sun, Jun 06 04"
              
              use the following picture string:
                
                "ddd',' MMM dd yy"

              
              This parameter is used as input.

    
    TimeFmt:  Pointer to a string used to form the time string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "ss" not "SS").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            h           Hours with no leading zero for single-digit hours; 12-hour clock.
                            
                            hh          Hours with leading zero for single-digit hours; 12-hour clock.
                            
                            H           Hours with no leading zero for single-digit hours; 24-hour clock.
                            
                            HH          Hours with leading zero for single-digit hours; 24-hour clock.
                            
                            m           Minutes with no leading zero for single-digit minutes.
                            
                            mm          Minutes with leading zero for single-digit minutes.
                            
                            s           Seconds with no leading zero for single-digit seconds.
                            
                            ss          Seconds with leading zero for single-digit seconds.
                            
                            t           One character time-marker string, such as A or P.
                            
                            tt          Multicharacter time-marker string, such as AM or PM.

              
              For example, to get the time string 

                "11:29:40 PM"

              use the following picture string: 

                "hh':'mm':'ss tt"


              This parameter is used as input.
    

    DateString: Pointer to a buffer containing the Date string to be parsed. The string must fits exactly with the DateFmt.
                It means that you must use the same locations for day, month, year, day of the week, respecting the chars
                used as a separator on the DateFmt.
                
                For example, if the DateFmt is "yyyy/MM/dd ddd ", the separatos are the "/" and the " " char. So the 
                string to be parsed must use them as well, like "2006/10/02 wed ". Below are some examples of good and bad strings
                
                a) Good Strings
                    "yyyy/MM/dd ddd " 
                    "2006/10/02 wed "
                b) Good Strings
                    "dddd MMMM yy" 
                    "tuesday december 02"

                c) Bad Strings
                    "dddd \ MMMM yy"
                    "tuesday december 02"
                    The correct one should be:
                    "tuesday \ december 02"

                d) Bad Strings
                    "yyyy/MM/dd ddd "
                    "2006/10/02 wed"
                    The correct one should be:
                    "2006/10/02 wed " ; with the last space at the end.

    
    TimeString: Pointer to a buffer containing the Time string to be parsed. The string must fits exactly with the TimeFmt.
                It means that you must use the same locations for hour, minutes, seconds, respecting the chars
                used as a separator on the TimeFmt.
                
                For example, if the TimeFmt is "hh:mm:ss tt", the separators are the ":" and the " " char. So the 
                string to be parsed must use them as well, like "11:29:40 PM". Below are some examples of good and bad strings
                
                a) Good Strings
                    "hh:mm:ss tt" 
                    "11:29:40 PM"
                b) Good Strings
                    "HH:mm:ss UTC" 
                    "01:54:10 UTC"

                c) Bad Strings
                    "hh:mm:ss tt" 
                    "18:29:40 PM"
                    hh time mark is from 1 to 12. So, The correct one should be:
                    "06:29:40 PM"

                d) Bad Strings
                    "hh:mm:ss tt "
                    "18:29:40 PM"
                    The correct one should be:
                    "18:29:40 PM " ; with the last space at the end.
    
    

Usage Examples:


a)

[szDateString: B$ "2005/08/11 thu ", 0]
[szTimeString: B$ "01:54:10 UTC", 0]
[TimeStamp: D$ 0]

        call TimeDateStringtoDword TimeStamp, {"yyyy/MM/dd ddd ", 0}, {"HH:mm:ss UTC", 0}, szDateString, szTimeString
        
        The output will be:
        [TimeStamp: D$ 042FAAFC2]

;;


[TimeBuffer: B$ 0 #80]
[TimeMap: B$ 0 #80]
[NewDateBuffer: B$ 0 #80]
[FullWeekFmt: B$ 0 #5]

Proc TimeDateStringtoDword:
    Arguments @TimeStamp, @DateFmt, @TimeFmt, @DateString, @TimeString
    Local @TimeMark, @TmpHour, @TmpMinute, @TmpSecond, @TmpYear, @TmpDayofWeek, @UseFullWeek_Month, @TmpMonth, @TmpDay, @FoundSourceLen
    Uses ebx, ecx, edx, esi, edi
; _________________________________________________________________
;   1St Step. Get the Time Stamp
; _________________________________________________________________

    mov edi TimeBuffer
    mov eax D@TimeFmt
    mov esi D@TimeString

    ; Initialize all Local variables.
    mov D@TimeMark 0
    mov D@TmpHour 0
    mov D@TmpMinute 0
    mov D@TmpSecond 0
    mov D@TmpYear 0
    mov D@TmpDayofWeek 0
    mov D@UseFullWeek_Month 0
    mov D@TmpMonth 0
    mov D@TmpDay 0
    mov D@FoundSourceLen 0

    ; 1St Locate the Hour chars. It can be "HH", "H", "hh", "h"

    mov edi TimeBuffer
    mov esi D@TimeFmt
    mov ecx 0

    .While B$esi <> 0

        ..If B$esi = 'H'
            .If B$esi+1 = 'H' ; Is it of the Format 'HH' ?
                mov esi D@TimeString
                add esi ecx
                movsw       ; copy the Decimal String (Hour) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 'H' time format. Check the byte immediate after it to get the end of the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@TimeString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi

                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Hour. Now we need to put it on the St_UTC structure
            mov D@TmpHour eax
            mov D@TimeMark 0 ; No Time Marks (AM/PM, A/P)
            jmp @HourFound


        ..Else_If B$esi = 'h'
            .If B$esi+1 = 'h' ; Is it of the Format 'hh' ?
                mov esi D@TimeString
                add esi ecx
                movsw       ; copy the Decimal String (Hour) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 'h' time format. Check the byte immediate after it to get the end of the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@TimeString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi

                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Hour. Now we need to put it on the St_UTC structure
            mov D@TmpHour eax

            ; Now we check for the time markers (AM/PM, A/P)

            mov esi D@TimeFmt
            mov ecx 0

            While B$esi <> 0
                .If B$esi = 't'
                    mov esi D@TimeString
                    add esi ecx
                    If B$esi = 'A'
                        mov D@TimeMark 1 ; AM
                    Else_If B$esi = 'P'
                        mov D@TimeMark 2 ;PM
                    Else ; something is wrong. Set it to 0
                        mov D@TimeMark 0
                    End_If
                    jmp @HourFound
                .End_If
                inc ecx
                inc esi
            End_While

            mov D@TimeMark 0
            jmp @HourFound

        ..End_If
        inc ecx
        inc esi
    .End_While

@HourNotFound:
    mov W$St_UTC.wHour 0  ; Could not find any Hours chars. It is possible that it is something wrong with the Hour Format
                            ; so we settle the Hour to 0.

@HourFound:

    ; 2nd Locate the Minutes chars. It can be 'mm', 'm'

    mov edi TimeBuffer
    mov esi D@TimeFmt
    mov ecx 0

    .While B$esi <> 0

        ..If B$esi = 'm'
            .If B$esi+1 = 'm' ; Is it of the Format 'mm' ?
                mov esi D@TimeString
                add esi ecx
                movsw       ; copy the Decimal String (Minute) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 'm' time format. Check the byte immediate after it to get the end of the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@TimeString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi

                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Hour. Now we need to put it on the St_UTC structure
            mov D@TmpMinute eax
            jmp @MinuteFound

        ..End_If
        inc ecx
        inc esi
    .End_While

@MinuteNotFound:
    mov W$St_UTC.wMinute 0  ; Could not find any Hours chars. It is possible that it is something wrong with the Hour Format
                            ; so we settle the Hour to 0.

@MinuteFound:

    ; 3rd Locate the Seconds chars. It can be 'ss', 's'
    mov edi TimeBuffer
    mov esi D@TimeFmt
    mov ecx 0

    .While B$esi <> 0

        ..If B$esi = 's'
            .If B$esi+1 = 's' ; Is it of the Format 'mm' ?
                mov esi D@TimeString
                add esi ecx
                movsw       ; copy the Decimal String (Minute) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 's' time format. Check the byte immediate after it to get the endof the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@TimeString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi
                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Hour. Now we need to put it on the St_UTC structure
            mov D@TmpSecond eax
            jmp @SecondFound

        ..End_If
        inc ecx
        inc esi
    .End_While

@SecondNotFound:
    mov W$St_UTC.wSecond 0  ; Could not find any Hours chars. It is possible that it is something wrong with the Hour Format
                            ; so we settle the Hour to 0.

@SecondFound:

    ; 4th Now that we computed all time data, we check for erros.
    ; If we have no errors, we copy them to the proper structure members

    ; a) See if all Dat fits their proper values

;;
    ; Guga Note 14/02/06. Skip this error checking for now.
    ..If D@TimeMark <> 0 ; If TimeMark is not zero, it means that we have the type 'hh' or 'h' for hour data.
        
        ; So, let's check if the values are smaller then 12
        If D@TmpHour > 12
            mov eax 1
            ExitP
        End_If
        
         ; PM. Need to multiply the hour by 2 to we get the proper hour in 24 hour type
        .If_And D@TimeMark = 2, D@TmpHour = 12
            If_Or D@TmpMinute <> 0, D@TmpSecond <> 0 
            
            End_If
        .End_If
    ..End_If

;;
; St_UTC.wHour: W$ 0
 ;St_UTC.wMinute: W$ 0
 ;St_UTC.wSecond: W$ 0

    ; 5th Now that we computed all time data, we just copy them to the proper structure members.

    move W$St_UTC.wHour W@TmpHour
    move W$St_UTC.wMinute W@TmpMinute
    move W$St_UTC.wSecond W@TmpSecond



; _________________________________________________________________
;   2nd Step. Get the Date Stamp
; _________________________________________________________________


    ; 1st Locate the Year chars. It can be 'yyyy', 'yy', 'y'

    mov edi TimeBuffer
    mov esi D@DateFmt
    mov ecx 0

    ; 1st Since we can have string with unknown len (from dddd or MMMM)
L1:

    .While B$esi <> 0
        ..If W$esi = 'dd' ; Full week name

            If W$esi+2 = 'dd' ; is it 'dddd' ?
                mov D$FullWeekFmt 'dddd'
                mov B$FullWeekFmt+4 0 ; ensure it will end with 0
            Else_If B$esi+2 = 'd' ; is it 'ddd' ?
                mov W$FullWeekFmt 'dd'
                mov B$FullWeekFmt+2 'd'
                mov B$FullWeekFmt+3 0 ; ensure it will end with 0
                mov B$FullWeekFmt+4 0 ; ensure it will end with 0
            Else
                ; Found only 'dd' or 'd'
                jmp Z5>>
            End_If

            ; Compare the full week name and mark it as a dword already.
            mov edx ecx ; edx = starting address of the found string.
            mov ecx 1

            .Do

                call GetDayofWeekinCurrentMonth ecx, FullWeekFmt, TimeBuffer; 0 = monday
                mov D@TmpDayofWeek eax
                call StrLenProc D@DateString
                mov esi eax
                call StrLenProc TimeBuffer
                mov D@FoundSourceLen eax
                call SBMBinSearch 0, D@DateString, esi, TimeBuffer, eax
                .If eax <> 0-1
                    mov ebx eax
                    add ebx D@FoundSourceLen
                    ; The String was found
                    ; At eax we have the pos (in bytes) where the string starts.
                    ; At ebx we have ending pos of the found string
                    mov edi TimeMap
                    If D@UseFullWeek_Month = &FALSE
                        mov esi D@DateFmt
                    Else
                        mov esi NewDateBuffer
                    End_If
                    ; Copy the initial part before the changes
                    mov ecx edx
                    rep movsb
                    mov B$edi 0

                   ; Copy the changed part
                    mov ecx ebx
                    sub ecx eax
                    mov al 'z'
                    rep stosb

                    ; Copy the final part to the new changed one.

                    If D@UseFullWeek_Month = &FALSE
                        mov esi D@DateFmt
                    Else
                        mov esi NewDateBuffer
                    End_If

                    add esi edx

                    If D$FullWeekFmt = 'dddd'
                        add esi 4
                    Else
                        add esi 3
                    End_If

                    While B$esi <> 0
                        movsb
                    End_While

                    mov B$edi 0
                    mov esi TimeMap
                    mov edi NewDateBuffer
                    push esi | ZCopy TimeMap | pop esi
                    mov D@UseFullWeek_Month &TRUE

                    jmp L1<< ; do again to we see if we don´ have the 'MMMM' format type

                .End_If
                inc ecx
            .Loop_Until ecx = 8 ; 1 week have 7 days. So we limit it to 8 (from day 1 to day 8)

        ..Else_If W$esi = 'MM'

            If W$esi+2 = 'MM' ; is it 'MMMM' ?
                mov D$FullWeekFmt 'MMMM'
                mov B$FullWeekFmt+4 0 ; ensure it will end with 0
            Else_If B$esi+2 = 'M' ; is it 'MMM' ?
                mov W$FullWeekFmt 'MM'
                mov B$FullWeekFmt+2 'M'
                mov B$FullWeekFmt+3 0 ; ensure it will end with 0
                mov B$FullWeekFmt+4 0 ; ensure it will end with 0
            Else
                jmp Z5>> ; Found only 'MM' or 'M'
            End_If
            ; Compare the full week name and mark it as a dword already.
            mov edx ecx ; edx = starting address of the found string.
            mov ecx 1

            .Do

                call GetMonthStringandValue ecx, FullWeekFmt, TimeBuffer; 0 = monday
                mov D@TmpMonth eax
                call StrLenProc D@DateString
                mov esi eax
                call StrLenProc TimeBuffer
                mov D@FoundSourceLen eax
                call SBMBinSearch 0, D@DateString, esi, TimeBuffer, eax
                .If eax <> 0-1
                    mov ebx eax
                    add ebx D@FoundSourceLen
                    ; The String was found
                    ; At eax we have the pos (in bytes) where the string starts.
                    ; At ebx we have ending pos of the found string
                    mov edi TimeMap

                    ; Copy the initial part before the changes
                    If D@UseFullWeek_Month = &FALSE
                        mov esi D@DateFmt
                    Else
                        mov esi NewDateBuffer
                    End_If
                    mov ecx edx
                    rep movsb
                    mov B$edi 0

                   ; Copy the changed part
                    mov ecx ebx
                    sub ecx eax
                    mov al 'x'
                    rep stosb

                    ; Copy the final part to the new changed one.
                    If D@UseFullWeek_Month = &FALSE
                        mov esi D@DateFmt
                    Else
                        mov esi NewDateBuffer
                    End_If
                    add esi edx

                    If D$FullWeekFmt = 'MMMM'
                        add esi 4
                    Else
                        add esi 3
                    End_If

                    While B$esi <> 0
                        movsb
                    End_While

                    mov B$edi 0
                    mov esi TimeMap
                    mov edi NewDateBuffer
                    push esi | ZCopy TimeMap | pop esi
                    mov D@UseFullWeek_Month &TRUE
                    jmp L1<< ; do again to we see if we don´ have the 'MMMM' format type

                .End_If
                inc ecx
            .Loop_Until ecx = 13 ; maximum months of 1 year (From 1 to 12)

        ..End_If
Z5:

        inc ecx
        inc esi
    .End_While

    ; 2nd Find now the year

    xor ecx ecx ; clear ecx

    If D@UseFullWeek_Month = &TRUE
        mov esi NewDateBuffer
    Else
        mov esi D@DateFmt
    End_If

    .While B$esi <> 0

        ..If D$esi = 'yyyy'
            mov esi D@DateString
            add esi ecx
            movsd       ; copy the Decimal String (Minute) to the buffer
            mov B$edi 0 ; ensure it ends with 0
            sub edi 4   ; decrease it by 2 to ensure it will point to the start of the string.
            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Year. Now we need to put it on the St_UTC structure
            mov D@TmpYear eax
            jmp @YearFound

        ..Else_If B$esi = 'y' ; In any case (yy or y), we must check with the actual system date, because we are dealing with decade.
                              ; It means that the year is smaller then 10
            .If B$esi+1 = 'y' ; Is it of the Format 'mm' ?
                mov esi D@DateString
                add esi ecx
                movsw       ; copy the Decimal String (Minute) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 'y' time format. Check the byte immediate after it to get the end of the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@DateString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi

                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            mov ecx eax ; copy the Date Value to ecx
            ; Now, since we are dealing with 'yy' or 'y' format, we have to find which full year we are.
            ; This is because the formats we are parsing refers to the current decade (values from 0 to 9).
            ; For example. now it is year 2006. If we found a value of 02, it means that the year we parsed is 2002

            call CurrentDateStampToString {"yyyy", 0}, edi, &NULL, &NULL
            mov B$edi+3 '0' ; Zero the last String Byte. Ex: 2006 turns onto 2000

            call String2Dword edi ; convert the Decimal String to Dword
            add eax ecx ; eax = eax+ecx (2000+6)
            ; At eax we have the dword value of the Year. Now we need to put it on the St_UTC structure
            mov D@TmpYear eax
            jmp @YearFound

        ..End_If
        inc ecx
        inc esi
    .End_While

@YearNotFound:
    mov W$St_UTC.wYear 0  ; Could not find any Hours chars. It is possible that it is something wrong with the Hour Format
                            ; so we settle the Hour to 0.

@YearFound:


    ; 3rd Locate the Day of Month chars. It can be 'dd', 'd'
    mov edi TimeBuffer

    xor ecx ecx ; clear ecx

    If D@UseFullWeek_Month = &TRUE
        mov esi NewDateBuffer
    Else
        mov esi D@DateFmt
    End_If


    .While B$esi <> 0

        ..If B$esi = 'd'
            .If B$esi+1 = 'd' ; Is it of the Format 'dd' ?
                mov esi D@DateString
                add esi ecx
                movsw       ; copy the Decimal String (Minute) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 's' time format. Check the byte immediate after it to get the endof the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@DateString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi
                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Hour. Now we need to put it on the St_UTC structure
            mov D@TmpDay eax
            jmp @DayofMonth

        ..End_If
        inc ecx
        inc esi
    .End_While

@DayofMonthNotFound:
    mov W$St_UTC.wDay 0  ; Could not find any Hours chars. It is possible that it is something wrong with the Hour Format
                            ; so we settle the Hour to 0.

@DayofMonth:



    ; 3rd Locate the Month chars. It can be 'MM', 'M'
    mov edi TimeBuffer

    xor ecx ecx ; clear ecx

    If D@UseFullWeek_Month = &TRUE
        mov esi NewDateBuffer
    Else
        mov esi D@DateFmt
    End_If


    .While B$esi <> 0

        ..If B$esi = 'M'
            .If B$esi+1 = 'M' ; Is it of the Format 'dd' ?
                mov esi D@DateString;D@TimeString
                add esi ecx
                movsw       ; copy the Decimal String (Minute) to the buffer
                mov B$edi 0 ; ensure it ends with 0
                sub edi 2   ; decrease it by 2 to ensure it will point to the start of the string.
            .Else
                ; Found only the 's' time format. Check the byte immediate after it to get the endof the string
                ; ecx = starting point of the address
                movzx edx B$esi+1 ; edx is the ending char we need to find.

                mov esi D@DateString
                add esi ecx ; esi is now the starting address
                push esi
                    mov eax 0 ; Our byte counter
                    While B$esi <> dl ; is it equal to the lower part of edx ? (The lower part is where it is stored our char)
                        inc eax
                        inc esi
                    End_While
                pop esi
                If eax = 1
                    movsb
                    mov B$edi 0 ; ensure it ends with 0
                Else
                    movsw
                    mov B$edi 0 ; ensure it ends with 0
                End_If
                sub edi eax
            .End_If

            call String2Dword edi ; convert the Decimal String to Dword
            ; At eax we have the dword value of the Hour. Now we need to put it on the St_UTC structure
            mov D@TmpMonth eax
            jmp @Month

        ..End_If
        inc ecx
        inc esi
    .End_While

@MonthNotFound:
    mov W$St_UTC.wMonth 0  ; Could not find any Hours chars. It is possible that it is something wrong with the Hour Format
                            ; so we settle the Hour to 0.
@Month:


; _________________________________________________________________
;   3rd Step. Get FILETIME Structure
; _________________________________________________________________


    ; Now that we have the full SYSTEMTIME Structure, we must get the proper FILETIME

    move W$St_UTC.wYear W@TmpYear
    move W$St_UTC.wMonth W@TmpMonth
    move W$St_UTC.wDayOfWeek W@TmpDayofWeek
    move W$St_UTC.wDay W@TmpDay
    move W$St_UTC.wHour W@TmpHour
    move W$St_UTC.wMinute W@TmpMinute
    move W$St_UTC.wSecond W@TmpSecond
    mov W$St_UTC.wMilliseconds 0

    call 'KERNEL32.SystemTimeToFileTime' St_UTC, St_DateTimeStamp


; _________________________________________________________________
;   4th Step. Convert the FILETIME to the dword
; _________________________________________________________________


        mov eax D$St_DateTimeStamp.dwLowDateTime
        mov edx D$St_DateTimeStamp.dwHighDateTime


        sub eax &EPOCH_DIFF_SECS_INTERVAL_LOW
        sbb edx &EPOCH_DIFF_SECS_INTERVAL_HIGH

        ; The following routines are used to divide a Qword with a Dword
        mov D$St_DateTimeStamp.dwLowDateTime eax
        mov D$St_DateTimeStamp.dwHighDateTime edx

        xor edx edx
        xor eax eax

        mov eax D$St_DateTimeStamp.dwHighDateTime
        mov ecx &SECS_TO_100NS
        div ecx

        mov eax D$St_DateTimeStamp.dwLowDateTime
        mov ecx &SECS_TO_100NS
        div ecx

        mov edi D@TimeStamp
        mov D$edi eax

;;
Error Types:
1 = The Hour value is of the type AM/PM, so it can´t be bigger then 12
;;

EndP


_____________________________________________________________________________________________________________________



;;

        __________________________________________________________________________________________________

                                          Get Day of Week in current month
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

GetDayofWeekinCurrentMonth


    The GetDayofWeekinCurrentMonth function retrieves the day of week (string) of a given day in the current month.


Parameters:

 @InputMonth, @DateFmt, @DateOut
    
    InputMonth:  Value of the month we want to be converted to string. The values are from 1 to 12 represented as:
                 1 = January
                 2 = February
                 3 = March
                 ....
                 12 = December
    
    DateFmt:  Pointer to a string used to form the month string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              The letters must be in uppercase as shown in the table (for example, "MMM" not "mmm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.

              
              For example, to get the time string 

                "December"

              use the following picture string: 

                'MMMM"


              This parameter is used as input.
    
    
    DateOut:  Pointer to a buffer that receives the formatted date string, and output the result in the format specified
              in DateFmt. The buffer can be limited to the lenght of the formated string.
    
    
Usage Examples:


a)

[TimeBuffer: B$ 0 #80]
        call GetMonthStringandValue 2, {"MMMM", 0}, TimeBuffer

    It will output on the timeBuffer the string "february" (Of course, depending of the language on your system).
    At eax it will output the value of the month. In case it is 2 (The same as the input).
b)

[TimeBuffer: B$ 0 #80]
        call GetMonthStringandValue 2, {"MMM", 0}, TimeBuffer

    It will output on the timeBuffer the string "feb" (Of course, depending of the language on your system).
    At eax it will output the value of the month. In case it is 2 (The same as the input).
        
;;

Proc GetDayofWeekinCurrentMonth:
    Arguments @InputDay, @DateFmt, @DateOut
    Structure @St_UTC 16, @St_UTC.wYearDis 0,  @St_UTC.wMonthDis 2,  @St_UTC.wDayOfWeekDis 4,  @St_UTC.wDayDis 6,  @St_UTC.wHourDis 8,  @St_UTC.wMinuteDis 10,  @St_UTC.wSecondDis 12,  @St_UTC.wMillisecondsDis 14
    Local @CurrentDay, @Weekday

    pushad
        call 'KERNEL32.GetLocalTime' D@St_UTC
        movzx eax W@St_UTC.wDayDis
        push eax
            mov ecx D@InputDay
            mov W@St_UTC.wDayDis cx
            call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@DateFmt D@DateOut &MAX_UTC_TIME_LEN
        pop eax
        ; current Day. 5 = Friday ; 6 Saturday, 0 Sunday, 1 Monday ...
        movzx ecx W@InputDay
        mov W@St_UTC.wDayDis cx ; inputed day
        move W@Weekday W@St_UTC.wDayOfWeekDis
        ; Calculate the dword value related to the day-of-the-week to be found
        .If eax >= ecx ; If current day is bigger or equal to the inputed day

            .While eax <> ecx

                dec eax ; decrease current day
                dec W@St_UTC.wDayOfWeekDis
                If W@St_UTC.wDayOfWeekDis = 0-1 ; when we reach here. we are on Saturday. so we must set it to the proper value
                    mov W@St_UTC.wDayOfWeekDis 6 ; Saturday
                End_If

            .End_While


        .Else ; If current day is smaller then the inputed day

            .While eax <> ecx

                inc eax ; increase current day
                inc W@St_UTC.wDayOfWeekDis
                If W@St_UTC.wDayOfWeekDis = 7 ; when we reach here. we are on Sunday. so we must set it to the proper value
                    mov W@St_UTC.wDayOfWeekDis 0 ; Sunday
                End_If

            .End_While


        .End_If

    popad
        movzx eax W@St_UTC.wDayOfWeekDis

EndP

_____________________________________________________________________________________________________________________

;;

        __________________________________________________________________________________________________

                                        Month Dword to String function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

GetMonthStringandValue


    The GetMonthStringandValue function converts a month value into a readable string.


Parameters:

 @InputMonth, @DateFmt, @DateOut
    
    InputMonth:  Value of the month we want to be converted to string. The values are from 1 to 12 represented as:
                 1 = January
                 2 = February
                 3 = March
                 ....
                 12 = December
    
    DateFmt:  Pointer to a string used to form the month string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              The letters must be in uppercase as shown in the table (for example, "MMM" not "mmm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.

              
              For example, to get the time string 

                "December"

              use the following picture string: 

                'MMMM"


              This parameter is used as input.
    
    
    DateOut:  Pointer to a buffer that receives the formatted date string, and output the result in the format specified
              in DateFmt. The buffer can be limited to the lenght of the formated string.
    
    
Usage Examples:


a)

[TimeBuffer: B$ 0 #80]
        call GetMonthStringandValue 2, {"MMMM", 0}, TimeBuffer

    It will output on the timeBuffer the string "february" (Of course, depending of the language on your system).
    At eax it will output the value of the month. In case it is 2 (The same as the input).
b)

[TimeBuffer: B$ 0 #80]
        call GetMonthStringandValue 2, {"MMM", 0}, TimeBuffer

    It will output on the timeBuffer the string "feb" (Of course, depending of the language on your system).
    At eax it will output the value of the month. In case it is 2 (The same as the input).
        
;;

Proc GetMonthStringandValue:
    Arguments @InputMonth, @DateFmt, @DateOut
    Structure @St_UTC 16, @St_UTC.wYearDis 0,  @St_UTC.wMonthDis 2,  @St_UTC.wDayOfWeekDis 4,  @St_UTC.wDayDis 6,  @St_UTC.wHourDis 8,  @St_UTC.wMinuteDis 10,  @St_UTC.wSecondDis 12,  @St_UTC.wMillisecondsDis 14
    Local @CurrentDay, @Weekday

    pushad
        call 'KERNEL32.GetLocalTime' D@St_UTC
        movzx eax W@St_UTC.wMonthDis ; 1 = January ; 2 = February ; 3 = March....; 12 = December
        push eax
            mov ecx D@InputMonth
            mov W@St_UTC.wMonthDis cx
            call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@DateFmt D@DateOut &MAX_UTC_TIME_LEN
        pop eax
    popad
        mov eax D@InputMonth

EndP

_____________________________________________________________________________________________________________________


;;

        __________________________________________________________________________________________________

                                        Current Date Stamp to String function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

CurrentDateStampToString

    The CurrentDateStampToString function was builded to help the user to convert the current date stamp
    of his system into a readable string.


Parameters:

 @DateFmt, @DateOut, @SysTimeStr, @FileTimeStr
    
    DateFmt:  Pointer to a string used to form the date string. The string must be zero terminated. This parameter can't
              be NULL.
              
              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "MM" not "mm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
                            
                            d           Day of month as digits with no leading zero for single-digit days.
                            
                            dd          Day of month as digits with leading zero for single-digit days.
                            
                            ddd         Day of week as a three-letter abbreviation. The function uses the
                                        LOCALE_SABBREVDAYNAME value associated with the specified locale.
                                        
                            dddd        Day of week as its full name. The function uses the LOCALE_SDAYNAME value
                                        associated with the specified locale.
                                        
                            M           Month as digits with no leading zero for single-digit months.
                            
                            MM          Month as digits with leading zero for single-digit months.
                            
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.
                            
                            y           Year as last two digits, but with no leading zero for years less than 10.
                            
                            yy          Year as last two digits, but with leading zero for years less than 10.
                            
                            yyyy        Year represented by full four digits.
                            
                            gg          Period/era string. The function uses the CAL_SERASTRING value associated with the
                                        specified locale. This element is ignored if the date to be formatted does not have
                                        an associated era or period string.
                                        

              For example, to get the date string
                
                "Sun, Jun 06 04"
              
              use the following picture string:
                
                "ddd',' MMM dd yy"

              
              This parameter is used as input.

    
    
    DateOut:  Pointer to a buffer that receives the formatted date string, and output the result in the format specified
              in DateFmt. The buffer can be limited to the lenght of the formated string.
    
    
    SysTimeStr: Pointer to a SYSTEMTIME structure buffer where it will output the result data of it's members (Day, Day of Week,
                time, hour, seconds, etc).
                If the value of this parameter is &NULL, it won´t output the data.


    FileTimeStr:    Pointer to a FILETIME structure buffer where it will output the result data of it's members (Low and High Values ot the timedate stamp).
                If the value of this parameter is &NULL, it won´t output the data.


Usage Examples:


a)

[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)

        call CurrentDateStampToString {"yyyy", 0}, szDateString, &NULL, &NULL

b)

[Sz_Year: B$ "yyyy/MM/dd ddd ",0]

[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)

        call CurrentDateStampToString Sz_Year, szDateString, &NULL, &NULL

c)

; system_time SYSTEMTIME Structure
[system_time:
 system_time.wYear: W$ 0
 system_time.wMonth: W$ 0
 system_time.wDayOfWeek: W$ 0
 system_time.wDay: W$ 0
 system_time.wHour: W$ 0
 system_time.wMinute: W$ 0
 system_time.wSecond: W$ 0
 system_time.wMilliseconds: W$ 0]

; FILETIME structure
[St_DateTimeStamp:
 St_DateTimeStamp.dwLowDateTime: D$ 0
 St_DateTimeStamp.dwHighDateTime: D$ 0]


[Sz_Year: B$ "yyyy/MM/dd ddd ",0]
[szDateString: B$ ? #64] ; Lenght of the date string. (Maximum is &MAX_UTC_TIME_LEN)

        call CurrentDateStampToString Sz_Year, szDateString, system_time, St_DateTimeStamp
        
;;

[Size_Of_SYSTEMTIME 16]
[Size_Of_FILETIME 8]

Proc CurrentDateStampToString:
    Arguments @DateFmt, @DateOut, @SysTimeStr, @FileTimeStr
    Structure @St_UTC 16, @St_UTC.wYearDis 0,  @St_UTC.wMonthDis 2,  @St_UTC.wDayOfWeekDis 4,  @St_UTC.wDayDis 6,  @St_UTC.wHourDis 8,  @St_UTC.wMinuteDis 10,  @St_UTC.wSecondDis 12,  @St_UTC.wMillisecondsDis 14
    Local @St_DateTimeStamp.dwLowDateTime, @St_DateTimeStamp.dwHighDateTime
    Uses ecx, edx

    call 'KERNEL32.GetLocalTime' D@St_UTC

    If D@SysTimeStr <> 0
        mov edi D@SysTimeStr
        mov esi D@St_UTC
        mov ecx 0
        While ecx <> Size_Of_SYSTEMTIME
            movsb
            inc ecx
        End_While
    End_If

    lea eax D@St_DateTimeStamp.dwLowDateTime
    call 'KERNEL32.SystemTimeToFileTime' D@St_UTC, D$eax

    If D@FileTimeStr <> 0
        mov edi D@FileTimeStr
        mov esi D@St_DateTimeStamp.dwLowDateTime
        mov ecx 0
        While ecx <> Size_Of_FILETIME
            movsb
            inc ecx
        End_While
    End_If

    lea eax D@St_DateTimeStamp.dwLowDateTime
    call 'kernel32.FileTimeToSystemTime' D$eax, D@St_UTC
    call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@DateFmt D@DateOut &MAX_UTC_TIME_LEN

EndP

_____________________________________________________________________________________________________________________

;;

        __________________________________________________________________________________________________

                                        Current Time Stamp to String function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

CurrentTimeStampToString


    The CurrentTimeStampToString function was builded to help the user to convert the current time stamp
    of his system into a readable string.


Parameters:

 @TimeFmt, @TimeOut, @SysTimeStr, @FileTimeStr    

    
    TimeFmt:  Pointer to a string used to form the time string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "ss" not "SS").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            h           Hours with no leading zero for single-digit hours; 12-hour clock.
                            
                            hh          Hours with leading zero for single-digit hours; 12-hour clock.
                            
                            H           Hours with no leading zero for single-digit hours; 24-hour clock.
                            
                            HH          Hours with leading zero for single-digit hours; 24-hour clock.
                            
                            m           Minutes with no leading zero for single-digit minutes.
                            
                            mm          Minutes with leading zero for single-digit minutes.
                            
                            s           Seconds with no leading zero for single-digit seconds.
                            
                            ss          Seconds with leading zero for single-digit seconds.
                            
                            t           One character time-marker string, such as A or P.
                            
                            tt          Multicharacter time-marker string, such as AM or PM.

              
              For example, to get the time string 

                "11:29:40 PM"

              use the following picture string: 

                "hh':'mm':'ss tt"


              This parameter is used as input.
    
    
    TimeOut:  Pointer to a buffer that receives the formatted time string, and output the result in the format specified
              in TimeFmt. The buffer can be limited to the lenght of the formated string.


    SysTimeStr: Pointer to a SYSTEMTIME structure buffer where it will output the result data of it's members (Day, Day of Week,
                time, hour, seconds, etc).
                If the value of this parameter is &NULL, it won´ output the data.


    FileTimeStr:    Pointer to a FILETIME structure buffer where it will output the result data of it's members (Low and High Values ot the timedate stamp).
                If the value of this parameter is &NULL, it won´ output the data.


Usage Examples:


a)

[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)

        call CurrentTimeStampToString {"HH:mm:ss UTC", 0}, szTimeString, &NULL, &NULL

b)

[Sz_Hour: B$ "HH:mm:ss UTC",0]
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)


        call CurrentTimeStampToString Sz_Hour, szTimeString, &NULL, &NULL

c)

; system_time SYSTEMTIME Structure
[system_time:
 system_time.wYear: W$ 0
 system_time.wMonth: W$ 0
 system_time.wDayOfWeek: W$ 0
 system_time.wDay: W$ 0
 system_time.wHour: W$ 0
 system_time.wMinute: W$ 0
 system_time.wSecond: W$ 0
 system_time.wMilliseconds: W$ 0]

; FILETIME structure
[St_DateTimeStamp:
 St_DateTimeStamp.dwLowDateTime: D$ 0
 St_DateTimeStamp.dwHighDateTime: D$ 0]


[Sz_Year: B$ "yyyy/MM/dd ddd ",0
 Year_Len: D$ len]
[Sz_Hour: B$ "HH:mm:ss UTC",0
 Hour_Len: D$ len]

[szTimeString: B$ ? #64]

        call CurrentTimeStampToString {"HH:mm:ss UTC", 0}, szTimeString, system_time, St_DateTimeStamp
        
;;

Proc CurrentTimeStampToString:
    Arguments @TimeFmt, @TimeOut, @SysTimeStr, @FileTimeStr
    Structure @St_UTC 16, @St_UTC.wYearDis 0,  @St_UTC.wMonthDis 2,  @St_UTC.wDayOfWeekDis 4,  @St_UTC.wDayDis 6,  @St_UTC.wHourDis 8,  @St_UTC.wMinuteDis 10,  @St_UTC.wSecondDis 12,  @St_UTC.wMillisecondsDis 14
    Local @St_DateTimeStamp.dwLowDateTime, @St_DateTimeStamp.dwHighDateTime

    pushad

    call 'KERNEL32.GetLocalTime' D@St_UTC
    If D@SysTimeStr <> 0
        mov edi D@SysTimeStr
        mov esi D@St_UTC
        mov ecx 0
        While ecx <> Size_Of_SYSTEMTIME
            movsb
            inc ecx
        End_While
    End_If

    lea eax D@St_DateTimeStamp.dwLowDateTime
    call 'KERNEL32.SystemTimeToFileTime' D@St_UTC, D$eax

    If D@FileTimeStr <> 0
        mov edi D@FileTimeStr
        mov esi D@St_DateTimeStamp.dwLowDateTime
        mov ecx 0
        While ecx <> Size_Of_FILETIME
            movsb
            inc ecx
        End_While
    End_If

    lea eax D@St_DateTimeStamp.dwLowDateTime
    call 'kernel32.FileTimeToSystemTime' D$eax D@St_UTC
    call 'KERNEL32.GetTimeFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@TimeFmt D@TimeOut &MAX_UTC_TIME_LEN

    popad
EndP
_____________________________________________________________________________________________________________________

;;

        __________________________________________________________________________________________________

                                        DOS Time and Date Stamp to String function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________

DOSTimeDateStampToString

    The DOSTimeDateStampToString function was builded to help the user to convert a inputed DOS TimeDate stamp
    into a readable string. This function can be used to retrieve the time and date from files builded in
    delphi, like some .dcu files (kylix), or com files, or 16 Bits windows or dos files and so on.


Parameters:

 @TimeStamp, @DateFmt, @TimeFmt, @DateOut, @TimeOut

    TimeStamp:  Pointer to the Data Value of the DOS time and date stamp found in the Target File. The DOS Time and date
                Stamp consists in 2 encoded values (in Word data type) inside a dword value, relative to date and time.
                These values are called DosFatDate and DosFatTime respectivelly. This parameter is used as Input.
                On this function we will compute both (Date and Time through one single dword value), so, if the input is:
                024CF774A
                It means, that the low word of this value is the DosFatTime,while the Hi Word is the DosFatDate, that will result in:
                [DosFatDate: W$ 024CF]
                [DosFatTime: W$ 0774A]
                
                The DOS Date and Time values are encoded to represent the real time and date of the value we want to convert.
                This functino is similar to TimeDateStampToString, with the difference that we don't compute the interval 
                values. (&EPOCH_DIFF_SECS_INTERVAL_LOW  &EPOCH_DIFF_SECS_INTERVAL_HIGH).
                
                Insetad calculating the interval values, we directly convert the DosTimeDate Stamp through the DosDateTimeToFileTime
                located in kernel32.dll.
                
    DateFmt:  Pointer to a string used to form the date string. The string must be zero terminated. This parameter can't
              be NULL.
              
              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "MM" not "mm").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
                            
                            d           Day of month as digits with no leading zero for single-digit days.
                            
                            dd          Day of month as digits with leading zero for single-digit days.
                            
                            ddd         Day of week as a three-letter abbreviation. The function uses the
                                        LOCALE_SABBREVDAYNAME value associated with the specified locale.
                                        
                            dddd        Day of week as its full name. The function uses the LOCALE_SDAYNAME value
                                        associated with the specified locale.
                                        
                            M           Month as digits with no leading zero for single-digit months.
                            
                            MM          Month as digits with leading zero for single-digit months.
                            
                            MMM         Month as a three-letter abbreviation. The function uses the LOCALE_SABBREVMONTHNAME
                                        value associated with the specified locale.
                            
                            MMMM        Month as its full name. The function uses the LOCALE_SMONTHNAME value associated
                                        with the specified locale.
                            
                            y           Year as last two digits, but with no leading zero for years less than 10.
                            
                            yy          Year as last two digits, but with leading zero for years less than 10.
                            
                            yyyy        Year represented by full four digits.
                            
                            gg          Period/era string. The function uses the CAL_SERASTRING value associated with the
                                        specified locale. This element is ignored if the date to be formatted does not have
                                        an associated era or period string.
                                        

              For example, to get the date string
                
                "Sun, Jun 06 04"
              
              use the following picture string:
                
                "ddd',' MMM dd yy"

              
              This parameter is used as input.

    TimeFmt:  Pointer to a string used to form the time string. The string must be zero terminated. This parameter can't
              be NULL.

              Use the following letters to build the formated string.
              
              If you use spaces to separate the letters in the formated string, these spaces will appear in the same
              location in the output string.
              
              The letters must be in uppercase or lowercase as shown in the table (for example, "ss" not "SS").
              
              Characters in the format string that are enclosed in single quotation marks will appear in the same
              location and unchanged in the output string.

                            Letters     Description
    
                            h           Hours with no leading zero for single-digit hours; 12-hour clock.
                            
                            hh          Hours with leading zero for single-digit hours; 12-hour clock.
                            
                            H           Hours with no leading zero for single-digit hours; 24-hour clock.
                            
                            HH          Hours with leading zero for single-digit hours; 24-hour clock.
                            
                            m           Minutes with no leading zero for single-digit minutes.
                            
                            mm          Minutes with leading zero for single-digit minutes.
                            
                            s           Seconds with no leading zero for single-digit seconds.
                            
                            ss          Seconds with leading zero for single-digit seconds.
                            
                            t           One character time-marker string, such as A or P.
                            
                            tt          Multicharacter time-marker string, such as AM or PM.

              
              For example, to get the time string 

                "11:29:40 PM"

              use the following picture string: 

                "hh':'mm':'ss tt"


              This parameter is used as input.
    
    DateOut:  Pointer to a buffer that receives the formatted date string, and output the result in the format specified
              in DateFmt. The buffer can be limited to the lenght of the formated string.
    
    TimeOut:  Pointer to a buffer that receives the formatted time string, and output the result in the format specified
              in TimeFmt. The buffer can be limited to the lenght of the formated string.



Usage Examples:


a)

[szDateString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)

        call DOSTimeDateStampToString 024CF774A {"yyyy/MM/dd ddd ", 0} {"HH:mm:ss UTC", 0} szDateString szTimeString
b)

[szDateString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)
[szTimeString: B$ ? #64] ; Lenght of the time string. (Maximum is &MAX_UTC_TIME_LEN)
[DosDateStamp: D$ 024CF774A]

        call DOSTimeDateStampToString D$DosDateStamp {"yyyy/MM/dd ddd ", 0} {"HH:mm:ss UTC", 0} szDateString szTimeString
;;


Proc DOSTimeDateStampToString:
    Arguments @TimeStamp, @DateFmt, @TimeFmt, @DateOut, @TimeOut
    Structure @St_UTC 16, @St_UTC.wYearDis 0,  @St_UTC.wMonthDis 2,  @St_UTC.wDayOfWeekDis 4,  @St_UTC.wDayDis 6,  @St_UTC.wHourDis 8,  @St_UTC.wMinuteDis 10,  @St_UTC.wSecondDis 12,  @St_UTC.wMillisecondsDis 14
    Local @St_DateTimeStamp.dwLowDateTime, @St_DateTimeStamp.dwHighDateTime

    pushad
        movzx edx W@TimeStamp   ; DosFatDate
        movzx ecx W@TimeStamp+2 ; DosFatTime

        lea eax D@St_DateTimeStamp.dwLowDateTime
        call 'kernel32.DosDateTimeToFileTime' ecx, edx, D$eax

        lea eax D@St_DateTimeStamp.dwLowDateTime
        call 'kernel32.FileTimeToSystemTime' D$eax, D@St_UTC

        call 'KERNEL32.GetDateFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@DateFmt D@DateOut &MAX_UTC_TIME_LEN
        call 'KERNEL32.GetTimeFormatA' &LOCALE_SYSTEM_DEFAULT &NULL D@St_UTC D@TimeFmt D@TimeOut &MAX_UTC_TIME_LEN
    popad
EndP
_____________________________________________________________________________________________________________________










































;;

        __________________________________________________________________________________________________

                                        Simple Boyer Moore Binary Search function.
                                                by Beyond2000! (Guga)
        __________________________________________________________________________________________________


SBMBinSearch

    This function is a variation of a Boyer Moore exact pattern matching algorithm uses the GOOD SUFFIX shift
    with the extra heuristic to handle repeated sequences of characters.
    
    It can be used for a search engine system for Bytes or Strings (Null terminated or not)


Parameters:

 @startpos, @lpSource, @srcLngth, @lpSubStr, @subLngth
 
    startpos:
                Value (in bytes) of the start position of the source string/binary data.
                When the value is 0, the search process will start at position 0 (1st byte included) on the source data.
                If the value is 1, it will start from the 2nd byte.
                Value = 3 start from the 4th byte (including it).....
                Below is a simple scheme of some Bytes to be searched and their related position.
                
                        1st Byte    2nd Byte    3rd Byte    4th Byte    5th Byte ............
                Values    055          044         0FA        088          067   ............
                Position   0            1           2          3            4    ............

    lpSource:
                Pointer to the address of the inputed (Source) data/string to be searched.
 
    srcLngth:
                Lenght of the inputed data/string
 
    lpSubStr:
                Pointer to the address of the data/string to search inside the targeted data/string.

    subLngth:
                Lenght of the data/string to search inside the source data/string.


Return Values:

    If the function suceeds, it returns the position (in bytes) where the string starts in the source data.
    If the function fail, it returns 0-1

Usage Examples:

a)
    call SBMBinSearch 0, {"Hi, my name is Guga. Now i´m typing this", 0}, 40, {"Guga", 0}, 4
    
b)

[Data01: B$ 055 025 014 0 058 0 0 0 025 099 0FA 0DE 045 0DD]
[Data02: B$ 025 099 0FA 0DE 045]
    
    call SBMBinSearch 0, Data01, 14, Data02, 5


Authors:
    Original author (unknown)
    Gustavo (Guga/Beyond2000!) - modified the function a bit, and adapted it to RosAsm

;;

[shift_table: D$ 0 #256]
Proc SBMBinSearch:
    Arguments @startpos, @lpSource, @srcLngth, @lpSubStr, @subLngth
    Uses ebx, esi, edi, ecx, edx

    mov edx D@subLngth

    cmp edx 1 | jg @StrSizeOk
        mov eax 0-2     ; string too short, must be > 1
jmp @Cleanup

@StrSizeOk: F4:
    mov esi D@lpSource
    add esi D@srcLngth
    sub esi edx
    mov ebx esi      ; set Exit Length

  ; ----------------------------------------
  ; load shift table with value in subLngth
  ; ----------------------------------------
    mov ecx 256
    mov eax edx
    lea edi D$shift_table
    rep stosd

  ; ----------------------------------------------
  ; load decending count values into shift table
  ; ----------------------------------------------
    mov ecx edx         ; SubString length in ECX
    dec ecx             ; correct for zero based index
    mov esi D@lpSubStr   ; address of SubString in ESI
    lea edi D$shift_table
    xor eax eax

@Write_Shift_Chars:
    mov al B$esi        ; get the character
    inc esi
    mov D$edi+eax*4 ecx ; write shift for each character
    dec ecx | jne @Write_Shift_Chars ; to ascii location in table

  ; -----------------------------
  ; set up for main compare loop
  ; -----------------------------

    mov esi D@lpSource
    mov edi D@lpSubStr
    dec edx
    xor eax eax         ; zero EAX
    add esi D@startpos    ; add starting position
jmp @Cmp_Loop

@Calc_Suffix_Shift: L6:
    add ecx D$eax*4+shift_table   ; add shift value to loop counter
    sub ecx edx | jns @Pre_Compare        ; sub pattern length
    mov ecx 01              ; minimum shift is 1

@Pre_Compare: N2:
    add esi ecx     ; add suffix shift
    mov ecx edx     ; reset counter for compare loop

@Exit_Text: N6:
    cmp ebx esi | jl @No_Match ; test exit condition

    xor eax eax         ; clear EAX for following partial writes
    mov al B$ecx+esi
    cmp al B$ecx+edi | je @KeepScan    ; cmp characters in ESI / EDI
    add esi D$eax*4+shift_table
jmp @Exit_Text

@KeepScan: P9:
    dec ecx
    xor eax eax         ; clear EAX for following partial writes

@Cmp_Loop: A2:
    mov al B$ecx+esi
    cmp al B$ecx+edi | jne @Calc_Suffix_Shift ; cmp characters in ESI / EDI
    dec ecx | jns @Cmp_Loop  ; if not equal, get next shift

@Match: ; match on fall through
    sub esi D@lpSource  ; sub source from ESI
    mov eax esi         ; put length in eax
jmp @Cleanup

@No_Match: C2:
    mov eax 0-1

@Cleanup:

EndP


____________________________________________________________________________________________________








