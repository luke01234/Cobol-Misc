       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT_TIME_TO_SEC.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LO-TIME-IN-MILI-SEC      PIC 9(13).

       LINKAGE SECTION.
       01  LS-DATE-DATA.
           05 LS-YEAR      PIC 9(4).
           05 LS-MONTH     PIC 9(2).
           05 LS-DAY       PIC 9(2).
           05 LS-HOUR      PIC 9(2).
           05 LS-MIN       PIC 9(2).
           05 LS-SEC       PIC 9(2).
           05 LS-MILISEC   PIC 9(2).
       
       PROCEDURE DIVISION USING LS-DATE-DATA.
       COMPUTE LO-TIME-IN-MILI-SEC = LS-YEAR * 31536000.
       COMPUTE LO-TIME-IN-MILI-SEC = 
       LO-TIME-IN-MILI-SEC + LS-MONTH * 2629743.
       COMPUTE LO-TIME-IN-MILI-SEC = 
       LO-TIME-IN-MILI-SEC + LS-DAY * 86400.
       COMPUTE LO-TIME-IN-MILI-SEC = 
       LO-TIME-IN-MILI-SEC + LS-HOUR * 3600.
       COMPUTE LO-TIME-IN-MILI-SEC = 
       LO-TIME-IN-MILI-SEC + LS-MIN * 60.
       COMPUTE LO-TIME-IN-MILI-SEC = 
       LO-TIME-IN-MILI-SEC + LS-SEC.
       COMPUTE LO-TIME-IN-MILI-SEC = 
       LO-TIME-IN-MILI-SEC * 100 + LS-MILISEC.
       MOVE LO-TIME-IN-MILI-SEC TO LS-DATE-DATA.

      * DISPLAY LO-TIME-IN-MILI-SEC AT 001001.

       END PROGRAM CONVERT_TIME_TO_SEC.

