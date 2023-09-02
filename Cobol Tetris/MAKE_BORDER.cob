      *=================================================================
      *    MAKE BORDER PROGGRAM
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAKE_BORDER.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LO-TOP-PRINT-POS     PIC 9(4).
       01  LO-BOTTOM-PRINT-POS  PIC 9(4).
       01  LO-PRINT-INDEX       PIC 9(2).
       01  LO-TOP-CHAR          PIC X(1) VALUE "_".
       01  LO-BOTTOM-CHAR       PIC X(1) VALUE "T".
       01  LO-SIDE-PRINT-POS    PIC 9(4).
       
       01  LO-HEIGHT         PIC 9(2).
       01  LO-WIDTH          PIC 9(2).
       01  LO-TEMP              PIC 9(2).

       LINKAGE SECTION.
       01  LI-STARTING-POS   PIC 9(4).
       01  LI-HEIGHT         PIC 9(2).
       01  LI-WIDTH          PIC 9(2).

       PROCEDURE DIVISION USING  
       LI-STARTING-POS LI-HEIGHT LI-WIDTH.
       COMPUTE LO-HEIGHT = LI-HEIGHT * 2.
       COMPUTE LO-WIDTH = LI-WIDTH * 2.

       COMPUTE LO-TOP-PRINT-POS = LI-STARTING-POS - 100.
       COMPUTE LO-BOTTOM-PRINT-POS = LI-STARTING-POS + (100*LO-HEIGHT).
       PERFORM VARYING LO-TEMP FROM 0 BY 1 UNTIL LO-TEMP = LO-WIDTH
       COMPUTE LO-TOP-PRINT-POS = LO-TOP-PRINT-POS + 1  
       DISPLAY LO-TOP-CHAR AT LO-TOP-PRINT-POS 
       WITH FOREGROUND-COLOR 4 
       COMPUTE LO-BOTTOM-PRINT-POS  = LO-BOTTOM-PRINT-POS + 1 
       DISPLAY LO-BOTTOM-CHAR AT LO-BOTTOM-PRINT-POS
       WITH FOREGROUND-COLOR 4
       END-PERFORM.
       
       COMPUTE LO-SIDE-PRINT-POS = LI-STARTING-POS - 100.
       PERFORM VARYING LO-TEMP FROM 0 BY 1 UNTIL LO-TEMP = LO-HEIGHT
       COMPUTE LO-SIDE-PRINT-POS = LO-SIDE-PRINT-POS + 100
       DISPLAY "|" AT LO-SIDE-PRINT-POS WITH FOREGROUND-COLOR 4
       COMPUTE LO-SIDE-PRINT-POS = LO-SIDE-PRINT-POS + LO-WIDTH + 1
       DISPLAY "|" AT LO-SIDE-PRINT-POS WITH FOREGROUND-COLOR 4
       COMPUTE LO-SIDE-PRINT-POS = LO-SIDE-PRINT-POS - LO-WIDTH - 1
       END-PERFORM.

       END PROGRAM MAKE_BORDER.

      *=================================================================
      *    MAKE BORDER END
      *=================================================================