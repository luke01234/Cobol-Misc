       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITIALIZE_PIECES.

       DATA DIVISION.
       
       LOCAL-STORAGE SECTION.

       01  LO-TEMP                 PIC 9(4).

       01  LO-INDEX                PIC 9(1).
      
       01  LO-TO-DISPLAY           PIC 9(1) VALUE 7.
       
       LINKAGE SECTION.

       01  LS-BLOCKS.
           05 LS-PIECES OCCURS 7 TIMES.
              06 LS-ROTATIONS OCCURS 4 TIMES.
                 07 LS-PIECE-ROW   PIC 9(1) OCCURS 4 TIMES.
                 07 LS-PIECE-COL   PIC 9(1) OCCURS 4 TIMES.
       
       PROCEDURE DIVISION USING LS-BLOCKS.
      
      *=================================================================
      *    INITIALIZE SQUARE PIECE
      *=================================================================
       MOVE 1 TO LS-PIECE-ROW(1,1,1).
       MOVE 1 TO LS-PIECE-COL(1,1,1).
       MOVE 1 TO LS-PIECE-ROW(1,1,2).
       MOVE 2 TO LS-PIECE-COL(1,1,2).
       MOVE 2 TO LS-PIECE-ROW(1,1,3).
       MOVE 1 TO LS-PIECE-COL(1,1,3).
       MOVE 2 TO LS-PIECE-ROW(1,1,4).
       MOVE 2 TO LS-PIECE-COL(1,1,4).
       
       MOVE LS-ROTATIONS(1,1) TO LS-ROTATIONS(1,2).
       MOVE LS-ROTATIONS(1,1) TO LS-ROTATIONS(1,3).
       MOVE LS-ROTATIONS(1,1) TO LS-ROTATIONS(1,4).
      
      *=================================================================
      *    INITIALIZE Z PIECE
      *=================================================================

       MOVE 1 TO LS-PIECE-ROW(2,1,1).
       MOVE 1 TO LS-PIECE-COL(2,1,1).
       MOVE 1 TO LS-PIECE-ROW(2,1,2).
       MOVE 2 TO LS-PIECE-COL(2,1,2).
       MOVE 2 TO LS-PIECE-ROW(2,1,3).
       MOVE 2 TO LS-PIECE-COL(2,1,3).
       MOVE 2 TO LS-PIECE-ROW(2,1,4).
       MOVE 3 TO LS-PIECE-COL(2,1,4).

       MOVE LS-ROTATIONS(2,1) TO LS-ROTATIONS(2,3).

       MOVE 0 TO LS-PIECE-ROW(2,2,1).
       MOVE 2 TO LS-PIECE-COL(2,2,1).
       MOVE 1 TO LS-PIECE-ROW(2,2,2).
       MOVE 2 TO LS-PIECE-COL(2,2,2).
       MOVE 1 TO LS-PIECE-ROW(2,2,3).
       MOVE 1 TO LS-PIECE-COL(2,2,3).
       MOVE 2 TO LS-PIECE-ROW(2,2,4).
       MOVE 1 TO LS-PIECE-COL(2,2,4).

       MOVE 0 TO LS-PIECE-ROW(2,4,1).
       MOVE 1 TO LS-PIECE-COL(2,4,1).
       MOVE 1 TO LS-PIECE-ROW(2,4,2).
       MOVE 1 TO LS-PIECE-COL(2,4,2).
       MOVE 1 TO LS-PIECE-ROW(2,4,3).
       MOVE 0 TO LS-PIECE-COL(2,4,3).
       MOVE 2 TO LS-PIECE-ROW(2,4,4).
       MOVE 0 TO LS-PIECE-COL(2,4,4). 
      
      *=================================================================
      *    INITIALIZE S PIECE
      *=================================================================
       
       MOVE 1 TO LS-PIECE-ROW(3,1,1).
       MOVE 1 TO LS-PIECE-COL(3,1,1).
       MOVE 1 TO LS-PIECE-ROW(3,1,2).
       MOVE 2 TO LS-PIECE-COL(3,1,2).
       MOVE 2 TO LS-PIECE-ROW(3,1,3).
       MOVE 1 TO LS-PIECE-COL(3,1,3).
       MOVE 2 TO LS-PIECE-ROW(3,1,4).
       MOVE 0 TO LS-PIECE-COL(3,1,4). 

       MOVE LS-ROTATIONS(3,1) TO LS-ROTATIONS(3,3)

       MOVE 0 TO LS-PIECE-ROW(3,2,1).
       MOVE 1 TO LS-PIECE-COL(3,2,1).
       MOVE 1 TO LS-PIECE-ROW(3,2,2).
       MOVE 1 TO LS-PIECE-COL(3,2,2).
       MOVE 1 TO LS-PIECE-ROW(3,2,3).
       MOVE 2 TO LS-PIECE-COL(3,2,3).
       MOVE 2 TO LS-PIECE-ROW(3,2,4).
       MOVE 2 TO LS-PIECE-COL(3,2,4). 
       
       MOVE 0 TO LS-PIECE-ROW(3,4,1).
       MOVE 0 TO LS-PIECE-COL(3,4,1).
       MOVE 1 TO LS-PIECE-ROW(3,4,2).
       MOVE 0 TO LS-PIECE-COL(3,4,2).
       MOVE 1 TO LS-PIECE-ROW(3,4,3).
       MOVE 1 TO LS-PIECE-COL(3,4,3).
       MOVE 2 TO LS-PIECE-ROW(3,4,4).
       MOVE 1 TO LS-PIECE-COL(3,4,4). 
       
       
      *=================================================================
      *    INITIALIZE J PIECE
      *=================================================================

       MOVE 1 TO LS-PIECE-ROW(4,1,1).
       MOVE 0 TO LS-PIECE-COL(4,1,1).
       MOVE 1 TO LS-PIECE-ROW(4,1,2).
       MOVE 1 TO LS-PIECE-COL(4,1,2).
       MOVE 1 TO LS-PIECE-ROW(4,1,3).
       MOVE 2 TO LS-PIECE-COL(4,1,3).
       MOVE 2 TO LS-PIECE-ROW(4,1,4).
       MOVE 2 TO LS-PIECE-COL(4,1,4).

       MOVE 0 TO LS-PIECE-ROW(4,2,1).
       MOVE 1 TO LS-PIECE-COL(4,2,1).
       MOVE 1 TO LS-PIECE-ROW(4,2,2).
       MOVE 1 TO LS-PIECE-COL(4,2,2).
       MOVE 2 TO LS-PIECE-ROW(4,2,3).
       MOVE 1 TO LS-PIECE-COL(4,2,3).
       MOVE 2 TO LS-PIECE-ROW(4,2,4).
       MOVE 0 TO LS-PIECE-COL(4,2,4).

       MOVE 1 TO LS-PIECE-ROW(4,3,1).
       MOVE 0 TO LS-PIECE-COL(4,3,1).
       MOVE 2 TO LS-PIECE-ROW(4,3,2).
       MOVE 0 TO LS-PIECE-COL(4,3,2).
       MOVE 2 TO LS-PIECE-ROW(4,3,3).
       MOVE 1 TO LS-PIECE-COL(4,3,3).
       MOVE 2 TO LS-PIECE-ROW(4,3,4).
       MOVE 2 TO LS-PIECE-COL(4,3,4).

       MOVE 0 TO LS-PIECE-ROW(4,4,1).
       MOVE 1 TO LS-PIECE-COL(4,4,1).
       MOVE 0 TO LS-PIECE-ROW(4,4,2).
       MOVE 2 TO LS-PIECE-COL(4,4,2).
       MOVE 1 TO LS-PIECE-ROW(4,4,3).
       MOVE 1 TO LS-PIECE-COL(4,4,3).
       MOVE 2 TO LS-PIECE-ROW(4,4,4).
       MOVE 1 TO LS-PIECE-COL(4,4,4). 
       
      *=================================================================
      *    INITIALIZE L PIECE
      *=================================================================
       
       MOVE 1 TO LS-PIECE-ROW(5,1,1).
       MOVE 0 TO LS-PIECE-COL(5,1,1).
       MOVE 1 TO LS-PIECE-ROW(5,1,2).
       MOVE 1 TO LS-PIECE-COL(5,1,2).
       MOVE 1 TO LS-PIECE-ROW(5,1,3).
       MOVE 2 TO LS-PIECE-COL(5,1,3).
       MOVE 2 TO LS-PIECE-ROW(5,1,4).
       MOVE 0 TO LS-PIECE-COL(5,1,4).

       MOVE 0 TO LS-PIECE-ROW(5,2,1).
       MOVE 0 TO LS-PIECE-COL(5,2,1).
       MOVE 0 TO LS-PIECE-ROW(5,2,2).
       MOVE 1 TO LS-PIECE-COL(5,2,2).
       MOVE 1 TO LS-PIECE-ROW(5,2,3).
       MOVE 1 TO LS-PIECE-COL(5,2,3).
       MOVE 2 TO LS-PIECE-ROW(5,2,4).
       MOVE 1 TO LS-PIECE-COL(5,2,4).

       MOVE 1 TO LS-PIECE-ROW(5,3,1).
       MOVE 2 TO LS-PIECE-COL(5,3,1).
       MOVE 2 TO LS-PIECE-ROW(5,3,2).
       MOVE 0 TO LS-PIECE-COL(5,3,2).
       MOVE 2 TO LS-PIECE-ROW(5,3,3).
       MOVE 1 TO LS-PIECE-COL(5,3,3).
       MOVE 2 TO LS-PIECE-ROW(5,3,4).
       MOVE 2 TO LS-PIECE-COL(5,3,4).

       MOVE 0 TO LS-PIECE-ROW(5,4,1).
       MOVE 1 TO LS-PIECE-COL(5,4,1).
       MOVE 1 TO LS-PIECE-ROW(5,4,2).
       MOVE 1 TO LS-PIECE-COL(5,4,2).
       MOVE 2 TO LS-PIECE-ROW(5,4,3).
       MOVE 1 TO LS-PIECE-COL(5,4,3).
       MOVE 2 TO LS-PIECE-ROW(5,4,4).
       MOVE 2 TO LS-PIECE-COL(5,4,4).
       
      *=================================================================
      *    INITIALIZE T PIECE
      *=================================================================

       MOVE 1 TO LS-PIECE-ROW(6,1,1).
       MOVE 0 TO LS-PIECE-COL(6,1,1).
       MOVE 1 TO LS-PIECE-ROW(6,1,2).
       MOVE 1 TO LS-PIECE-COL(6,1,2).
       MOVE 1 TO LS-PIECE-ROW(6,1,3).
       MOVE 2 TO LS-PIECE-COL(6,1,3).
       MOVE 2 TO LS-PIECE-ROW(6,1,4).
       MOVE 1 TO LS-PIECE-COL(6,1,4).

       MOVE 0 TO LS-PIECE-ROW(6,2,1).
       MOVE 1 TO LS-PIECE-COL(6,2,1).
       MOVE 1 TO LS-PIECE-ROW(6,2,2).
       MOVE 0 TO LS-PIECE-COL(6,2,2).
       MOVE 1 TO LS-PIECE-ROW(6,2,3).
       MOVE 1 TO LS-PIECE-COL(6,2,3).
       MOVE 2 TO LS-PIECE-ROW(6,2,4).
       MOVE 1 TO LS-PIECE-COL(6,2,4).
      
       MOVE 1 TO LS-PIECE-ROW(6,3,1).
       MOVE 1 TO LS-PIECE-COL(6,3,1).
       MOVE 2 TO LS-PIECE-ROW(6,3,2).
       MOVE 0 TO LS-PIECE-COL(6,3,2).
       MOVE 2 TO LS-PIECE-ROW(6,3,3).
       MOVE 1 TO LS-PIECE-COL(6,3,3).
       MOVE 2 TO LS-PIECE-ROW(6,3,4).
       MOVE 2 TO LS-PIECE-COL(6,3,4).
       
       MOVE 0 TO LS-PIECE-ROW(6,4,1).
       MOVE 1 TO LS-PIECE-COL(6,4,1).
       MOVE 1 TO LS-PIECE-ROW(6,4,2).
       MOVE 1 TO LS-PIECE-COL(6,4,2).
       MOVE 1 TO LS-PIECE-ROW(6,4,3).
       MOVE 2 TO LS-PIECE-COL(6,4,3).
       MOVE 2 TO LS-PIECE-ROW(6,4,4).
       MOVE 1 TO LS-PIECE-COL(6,4,4).

      *=================================================================
      *    INITIALIZE LINE PIECE
      *=================================================================
       
       MOVE 2 TO LS-PIECE-ROW(7,1,1).
       MOVE 0 TO LS-PIECE-COL(7,1,1).
       MOVE 2 TO LS-PIECE-ROW(7,1,2).
       MOVE 1 TO LS-PIECE-COL(7,1,2).
       MOVE 2 TO LS-PIECE-ROW(7,1,3).
       MOVE 2 TO LS-PIECE-COL(7,1,3).
       MOVE 2 TO LS-PIECE-ROW(7,1,4).
       MOVE 3 TO LS-PIECE-COL(7,1,4).

       MOVE 0 TO LS-PIECE-ROW(7,2,1).
       MOVE 2 TO LS-PIECE-COL(7,2,1).
       MOVE 1 TO LS-PIECE-ROW(7,2,2).
       MOVE 2 TO LS-PIECE-COL(7,2,2).
       MOVE 2 TO LS-PIECE-ROW(7,2,3).
       MOVE 2 TO LS-PIECE-COL(7,2,3).
       MOVE 3 TO LS-PIECE-ROW(7,2,4).
       MOVE 2 TO LS-PIECE-COL(7,2,4).

       MOVE LS-ROTATIONS(7,1) TO LS-ROTATIONS(7,3)

       MOVE 0 TO LS-PIECE-ROW(7,4,1).
       MOVE 1 TO LS-PIECE-COL(7,4,1).
       MOVE 1 TO LS-PIECE-ROW(7,4,2).
       MOVE 1 TO LS-PIECE-COL(7,4,2).
       MOVE 2 TO LS-PIECE-ROW(7,4,3).
       MOVE 1 TO LS-PIECE-COL(7,4,3).
       MOVE 3 TO LS-PIECE-ROW(7,4,4).
       MOVE 1 TO LS-PIECE-COL(7,4,4).



       END PROGRAM INITIALIZE_PIECES.
