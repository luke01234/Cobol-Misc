      *=================================================================
      *    DRAW BOARD 
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRAW_BOARD.

       DATA DIVISION.
       LOCAL-STORAGE SECTION. 
       01  LO-PRINT-INDEX    PIC 9(2).
       01  LO-CUR-PRINT-POS  PIC 9(4).
       01  LO-TEMP-PRINT-POS PIC 9(4).

       01  LO-CHAR           PIC X(1).
       01  LO-INDEX-ROW      PIC 9(2).
       01  LO-INDEX-COL      PIC 9(2).
       01  LO-PRINT-AT       PIC 9(4).


       LINKAGE SECTION. 
       01  LI-BOARD.
           05  LI-BOARD-ROW OCCURS 20 TIMES.
               06  LI-BOARD-COLS OCCURS 10 TIMES.
                   07  LI-BOARD-VAL    PIC X(1).
       01  LI-STARTING-POS   PIC 9(4).
       01  LI-HEIGHT         PIC 9(2).
       01  LS-WIDTH          PIC 9(2).
       01  LI-SCORE          PIC 9(10).

       PROCEDURE DIVISION USING BY REFERENCE 
       LI-BOARD LI-STARTING-POS LI-HEIGHT LS-WIDTH LI-SCORE.
       DISPLAY LI-SCORE AT 0101.
       
       PERFORM VARYING LO-INDEX-ROW FROM 1 BY 1 UNTIL
       LO-INDEX-ROW = 21
       PERFORM VARYING LO-INDEX-COL FROM 1 BY 1 UNTIL
       LO-INDEX-COL = 11
       MOVE LI-BOARD-COLS(LO-INDEX-ROW,LO-INDEX-COL) TO LO-CHAR
       COMPUTE LO-PRINT-AT = LO-INDEX-ROW * 200 + LO-INDEX-COL * 2 + 
       LI-STARTING-POS - 100
      * COMPUTE LO-PRINT-AT = (LO-PRINT-AT - 1) * 2 
       IF LO-CHAR = "A" THEN 
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 7 
       FOREGROUND-COLOR 1 END-DISPLAY
       COMPUTE LO-PRINT-AT = LO-PRINT-AT - 1
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 7 
       FOREGROUND-COLOR 1 END-DISPLAY
       COMPUTE LO-PRINT-AT = LO-PRINT-AT - 100
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 7 
       FOREGROUND-COLOR 1 END-DISPLAY
       COMPUTE LO-PRINT-AT = LO-PRINT-AT + 1
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 7 
       FOREGROUND-COLOR 1 END-DISPLAY
       ELSE
       DISPLAY LO-CHAR AT LO-PRINT-AT 
       COMPUTE LO-PRINT-AT = LO-PRINT-AT - 1
       DISPLAY LO-CHAR AT LO-PRINT-AT
       COMPUTE LO-PRINT-AT = LO-PRINT-AT - 100
       DISPLAY LO-CHAR AT LO-PRINT-AT
       COMPUTE LO-PRINT-AT = LO-PRINT-AT + 1
       DISPLAY LO-CHAR AT LO-PRINT-AT
       END-IF
       END-PERFORM
       END-PERFORM. 

       END PROGRAM DRAW_BOARD.

      *=================================================================
      *    DRAW BOARD END
      *=================================================================