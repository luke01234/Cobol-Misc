       IDENTIFICATION DIVISION.
       PROGRAM-ID. timout.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INP         PIC 9(3).

       01  WS-PRE-TIME    PIC 9(16).
       
       01  WS-POST-TIME   PIC 9(16).

       01  WS-TIME-DIF    pic 9(3).

       PROCEDURE DIVISION.
       MAIN-PARA.
       SET ENVIRONMENT 'COB_TIMEOUT_SCALE' TO '3'.
       MOVE FUNCTION CURRENT-DATE TO WS-PRE-TIME.
       ACCEPT WS-INP TIME-OUT AFTER 1. 
       MOVE FUNCTION CURRENT-DATE TO WS-POST-TIME.
       COMPUTE WS-TIME-DIF = WS-POST-TIME - WS-PRE-TIME.
       COMPUTE WS-INP = WS-INP + 7.
       DISPLAY "answer is " AT 0101 WS-INP AT 0111.
       DISPLAY WS-TIME-DIF AT 0201.
       STOP RUN.