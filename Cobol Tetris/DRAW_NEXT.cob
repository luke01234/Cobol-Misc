      *=================================================================
      *    DRAW NEXT
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRAW_NEXT.

       DATA DIVISION.
       LOCAL-STORAGE SECTION. 
       01  LO-PRINT-INDEX    PIC 9(2).
       01  LO-CUR-PRINT-POS  PIC 9(4).
       01  LO-TEMP-PRINT-POS PIC 9(4).

       01  LO-CHAR           PIC X(1).
       01  LO-INDEX-ROW      PIC 9(2).
       01  LO-INDEX-COL      PIC 9(2).
       01  LO-PRINT-AT       PIC 9(4).
       
       01  LO-BLUE           PIC 9(1) VALUE 1.
       01  LO-GREEN          PIC 9(1) VALUE 2.
       01  LO-CYAN           PIC 9(1) VALUE 3.
       01  LO-RED            PIC 9(1) VALUE 4.
       01  LO-PURPLE         PIC 9(1) VALUE 5.
       01  LO-GOLD           PIC 9(1) VALUE 6.
       01  LO-WHITE          PIC 9(1) VALUE 7.
       01  LO-BLACK          PIC 9(1) VALUE 8.

       01  LO-FORE-COLOR     PIC 9(1) VALUE 7.
       01  LO-BACK-COLOR     PIC 9(1) VALUE 8.

       01  LO-TEMP-NUMS.
           05 LO-TEMP-NUM PIC 9(4) OCCURS 10 TIMES. 

       LINKAGE SECTION. 
       01  LI-BOARD.
           05  LI-BOARD-ROW OCCURS 5 TIMES.
               06  LI-BOARD-COLS OCCURS 5 TIMES.
                   07  LI-BOARD-VAL    PIC X(1).
                   
       01  LI-STARTING-POS   PIC 9(4).
       01  LI-HEIGHT         PIC 9(2).
       01  LI-WIDTH          PIC 9(2).


       PROCEDURE DIVISION USING 
       LI-BOARD LI-STARTING-POS LI-HEIGHT LI-WIDTH.
      * DISPLAY LI-SCORE AT 0101.
       COMPUTE LO-TEMP-NUM(1) = LI-HEIGHT + 1.
       COMPUTE LO-TEMP-NUM(2) = LI-WIDTH + 1.
       PERFORM VARYING LO-INDEX-ROW FROM 1 BY 1 UNTIL
       LO-INDEX-ROW = LO-TEMP-NUM(1)
       PERFORM VARYING LO-INDEX-COL FROM 1 BY 1 UNTIL
       LO-INDEX-COL = LO-TEMP-NUM(2)
       MOVE LI-BOARD-COLS(LO-INDEX-ROW,LO-INDEX-COL) TO LO-CHAR
       COMPUTE LO-PRINT-AT = LO-INDEX-ROW * 200 + LO-INDEX-COL * 2 + 
       LI-STARTING-POS - 100
      * COMPUTE LO-PRINT-AT = (LO-PRINT-AT - 1) * 2 
      *> 1=blue 2=green 3=cyan 4=red 5=purple 6=gold 7=white
       EVALUATE LO-CHAR
       WHEN "1" 
       MOVE LO-GOLD TO LO-BACK-COLOR
       MOVE LO-GOLD TO LO-FORE-COLOR
       WHEN "2" 
       MOVE LO-RED TO LO-BACK-COLOR
       MOVE LO-RED TO LO-FORE-COLOR
       WHEN "3" 
       MOVE LO-GREEN TO LO-BACK-COLOR
       MOVE LO-GREEN TO LO-FORE-COLOR
       WHEN "4" 
       MOVE LO-BLUE TO LO-BACK-COLOR
       MOVE LO-BLUE TO LO-FORE-COLOR
       WHEN "5" 
       MOVE LO-WHITE TO LO-BACK-COLOR
       MOVE LO-WHITE TO LO-FORE-COLOR
       WHEN "6" 
       MOVE LO-PURPLE TO LO-BACK-COLOR
       MOVE LO-PURPLE TO LO-FORE-COLOR
       WHEN "7" 
       MOVE LO-CYAN TO LO-BACK-COLOR
       MOVE LO-CYAN TO LO-FORE-COLOR
       WHEN "/"
       MOVE LO-BLACK TO LO-BACK-COLOR
       MOVE LO-BLUE TO LO-FORE-COLOR
       WHEN OTHER
       MOVE LO-BLACK TO LO-BACK-COLOR
       MOVE LO-WHITE TO LO-FORE-COLOR
       END-EVALUATE
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 
       LO-BACK-COLOR FOREGROUND-COLOR LO-FORE-COLOR 
       COMPUTE LO-PRINT-AT = LO-PRINT-AT - 1
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 
       LO-BACK-COLOR FOREGROUND-COLOR LO-FORE-COLOR
       COMPUTE LO-PRINT-AT = LO-PRINT-AT - 100
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 
       LO-BACK-COLOR FOREGROUND-COLOR LO-FORE-COLOR
       COMPUTE LO-PRINT-AT = LO-PRINT-AT + 1
       DISPLAY LO-CHAR AT LO-PRINT-AT WITH BACKGROUND-COLOR 
       LO-BACK-COLOR FOREGROUND-COLOR LO-FORE-COLOR
       END-PERFORM
       END-PERFORM. 

       END PROGRAM DRAW_NEXT.

      *=================================================================
      *    DRAW NEXT END
      *=================================================================

