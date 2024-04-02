       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK_AND_CLEAR_ROWS.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       
       01  LO-BOARD.
           05  LO-BOARD-ROW OCCURS 20 TIMES.
               06  LO-BOARD-COLS OCCURS 10 TIMES.
                   07  LO-BOARD-VAL    PIC X(1).

       01  LO-COL-INDEX          PIC 9(2).

       01  LO-ROW-INDEX          PIC 9(2).

       01  LO-ROW-MAX        PIC 9(2) VALUE 21.

       01  LO-COL-MAX        PIC 9(2) VALUE 11.
       
       01  LO-CUR-CHAR       PIC X(1).
       
       01  LO-CLEAR          PIC 9(1) VALUE 1.

       01  LO-TEMP           PIC 9(2) VALUE 1.

       01  LO-TEMP2          PIC 9(2).

       LINKAGE SECTION. 
       01  LI-BOARD.
           05  LI-BOARD-ROW OCCURS 20 TIMES.
               06  LI-BOARD-COLS OCCURS 10 TIMES.
                   07  LI-BOARD-VAL    PIC X(1).
       
       01  LI-EMPTY-SPACE    PIC X(1).

       01  LI-CHECKED-ROW    PIC 9(2).

       01  LI-CLEARED-ROWS   PIC 9(1).
       
       PROCEDURE DIVISION USING LI-BOARD LI-EMPTY-SPACE LI-CHECKED-ROW
       LI-CLEARED-ROWS.
      *=================================================================
      *    Check rows before clearing by looping through cols
      *=================================================================
       CHECK-PARA.
       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 
       UNTIL LO-COL-INDEX = LO-COL-MAX
       MOVE LI-BOARD-COLS(LI-CHECKED-ROW,LO-COL-INDEX) TO LO-CUR-CHAR
       IF LO-CUR-CHAR = LI-EMPTY-SPACE THEN
       MOVE 0 TO LO-CLEAR 
       MOVE 10 TO LO-COL-INDEX
       END-IF
       END-PERFORM.
       IF LO-CLEAR = 1 THEN *>if a row needs to be cleared, clear it
           PERFORM CLEAR-PARA
       END-IF.

       EXIT PROGRAM. *>end program here, do not run other paras
      
      *=================================================================
      *    Finish Checking Rows
      *=================================================================
       
      *=================================================================
      *    Clear rows that are full
      *=================================================================
       CLEAR-PARA.  
       COMPUTE LI-CLEARED-ROWS = LI-CLEARED-ROWS + 1. *> iterate cleared rows
       *> for keeping score
      * DISPLAY LI-CLEARED-ROWS AT 3030.
       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 *>clear the row by iterating
       UNTIL LO-COL-INDEX = LO-COL-MAX
       MOVE LI-EMPTY-SPACE TO LI-BOARD-COLS(LI-CHECKED-ROW,LO-COL-INDEX)
       END-PERFORM.
       PERFORM DROP-ROWS-PARA. *>drop all rows above the cleared one

      *=================================================================
      *    Finish Clear Rows
      *=================================================================

      *=================================================================
      *    Drop rows above cleared row
      *=================================================================
       DROP-ROWS-PARA.
       MOVE LI-BOARD TO LO-BOARD. *>copy board into local storage
       
       COMPUTE LO-TEMP2 = LI-CHECKED-ROW - 1.
       PERFORM VARYING LO-ROW-INDEX FROM LO-TEMP2 BY -1 
       UNTIL LO-ROW-INDEX = 0 *>loop rows till you reach the top

       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 
       UNTIL LO-COL-INDEX = LO-COL-MAX *>loop each column
       
       COMPUTE LO-TEMP = LO-ROW-INDEX + 1 
       MOVE LO-BOARD-COLS(LO-ROW-INDEX,LO-COL-INDEX) TO
       LI-BOARD-COLS(LO-TEMP,LO-COL-INDEX) *> copy everything one row down
       
       END-PERFORM
       
       END-PERFORM.
       
       
       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 *>clear top row
       UNTIL LO-COL-INDEX = LO-COL-MAX
       MOVE LI-EMPTY-SPACE TO LI-BOARD-COLS(1,LO-COL-INDEX)
       END-PERFORM.

       
       END PROGRAM CHECK_AND_CLEAR_ROWS.

