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

       01  LO-TEMP           PIC 9(4) VALUE 1.

       LINKAGE SECTION. 
       01  LI-BOARD.
           05  LI-BOARD-ROW OCCURS 20 TIMES.
               06  LI-BOARD-COLS OCCURS 10 TIMES.
                   07  LI-BOARD-VAL    PIC X(1).
       
       01  LI-EMPTY-SPACE    PIC X(1).

       01  LI-CHECKED-ROW    PIC 9(2).
       
       PROCEDURE DIVISION USING LI-BOARD LI-EMPTY-SPACE LI-CHECKED-ROW.
       CHECK-PARA.
       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 
       UNTIL LO-COL-INDEX = LO-COL-MAX
       MOVE LI-BOARD-COLS(LI-CHECKED-ROW,LO-COL-INDEX) TO LO-CUR-CHAR
       IF LO-CUR-CHAR = LI-EMPTY-SPACE THEN
       MOVE 0 TO LO-CLEAR 
       MOVE 10 TO LO-COL-INDEX
       END-IF
       END-PERFORM.
       IF LO-CLEAR = 1 THEN
           PERFORM CLEAR-PARA
       END-IF.

       EXIT PROGRAM.
       
       
       CLEAR-PARA.
       DISPLAY "CLEARING" AT 3030.
       DISPLAY LO-CLEAR AT 3130.
       Display LI-CHECKED-ROW AT 3230.

       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 
       UNTIL LO-COL-INDEX = LO-COL-MAX
       MOVE "." TO LI-BOARD-COLS(LI-CHECKED-ROW,LO-COL-INDEX)
       MOVE "." TO LI-BOARD-COLS(20,LO-COL-INDEX)
       END-PERFORM.
       PERFORM DROP-ROWS-PARA.

       DROP-ROWS-PARA.
       MOVE LI-BOARD TO LO-BOARD.
       PERFORM VARYING LO-ROW-INDEX FROM LI-CHECKED-ROW BY 1 
       UNTIL LO-ROW-INDEX = LO-COL-MAX
       PERFORM VARYING LO-COL-INDEX FROM 1 BY 1 
       UNTIL LO-COL-INDEX = LO-COL-MAX
       MOVE LO-BOARD-COLS(LI-CHECKED-ROW,LO-COL-INDEX)
       END-PERFORM.

       
       END PROGRAM CHECK_AND_CLEAR_ROWS.


