      *=================================================================
      *    MAIN PROGRAM 
      *=================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobol_tetris.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *=================================================================
      *   BOARD VARS
      *=================================================================

       01  WS-BOARD-POS-LS.
           05 WS-BOARD-POS-ROW   PIC 9(2) VALUE 4.
           05 WS-BOARD-POS-COL   PIC 9(2) VALUE 4.

       01  WS-BOARD-HEIGHT   PIC 9(2)    VALUE 20.
       01  WS-BOARD-WIDTH    PIC 9(2)    VALUE 10.
      
       01  WS-EMPTY-SPACE          PIC X(1) VALUE ".".

       01  WS-BOARD.       
           05  WS-BOARD-ROW OCCURS 20 TIMES.
               06  WS-BOARD-COLS OCCURS 10 TIMES.
                   07  WS-BOARD-VAL    PIC X(1) VALUE ".".
       
      *=================================================================
      *   GAME STATE AND SCORE
      *=================================================================

       01  WS-GAME-ON        PIC X(1)    VALUE "\".
       
       01  WS-SCORE          PIC 9(10)   VALUE ZEROS.

       01  WS-CLEARED-ROWS   PIC 9(1).

       01  WS-CHECK-FOR-CLEAR    PIC 9(2).

      *=================================================================
      *   PLAYER MOVEMENT VARS
      *=================================================================

       01  WS-PIECE-COLLISION    PIC 9(1).

       01  WS-CAN-MOVE       PIC 9(1).

       01  WS-REL-PLAYERPOS-LS.
           05 WS-REL-ROW  PIC S9(2) VALUE +01.
           05 WS-REL-COL  PIC S9(2) VALUE +01.
           
       01  WS-CHECK-PLAYERPOS-LS.
           05 WS-CHECK-ROW  PIC S9(2) VALUE +01.
           05 WS-CHECK-COL  PIC S9(2) VALUE +01.
       
       01  WS-PREV-PLAYERPOS-LS.
           05 WS-PREV-ROW PIC S9(2) VALUE +01.
           05 WS-PREV-COL PIC S9(2) VALUE +01.

       01  WS-PIECE-START-POS-LS.
           05 WS-START-ROW PIC S9(2) VALUE +01.
           05 WS-START-COL PIC S9(2) VALUE +04.        
       
       01  WS-INP            PIC X(1).

       01  WS-DIR            PIC X(1).

      *=================================================================
      *   PIECES AND ROTATIONS
      *=================================================================
      
       01  WS-BLOCKS.
           05 WS-PIECES OCCURS 7 TIMES.
              06 WS-ROTATIONS OCCURS 4 TIMES.
                 07 WS-PIECE-ROW PIC 9(1) OCCURS 4 TIMES.
                 07 WS-PIECE-COL PIC 9(1) OCCURS 4 TIMES.

       01  WS-CURRENT-PIECE     PIC 9(1).

       01  WS-NEXT-PIECE        PIC 9(1).
       
       01  WS-SEED              PIC 9(8).

       01  WS-ROTATION          PIC 9(1) VALUE 1.

      *=================================================================
      *   TIME VARS
      *=================================================================

       01  WS-TIME-BEFORE-DROP         PIC 9(4) VALUE 100.
       01  WS-LAST-DROP-TIME           PIC 9(13).

       01  WS-TIME-SINCE-DROP          PIC 9(13).

       01  WS-START-TIME-DATA.
           05 WS-START-DATE            PIC 9(16).
       
       01  WS-CURRENT-TIME-DATA.
           05 WS-CURRENT-DATE          PIC 9(16).

       01  WS-TIME-SINCE-START-DATA.
           05 WS-TIME-SINCE-START              PIC 9(13).

       LOCAL-STORAGE SECTION.
      *=================================================================
      *   TEMP VARS
      *=================================================================

       01  LO-TEMP-NUM        PIC 9(4).

       01  LO-ROW-TEMP        PIC 9(4).

       01  LO-COL-TEMP        PIC 9(4).

       01  LO-TEMP-CHAR       PIC X(4).
       
      *=================================================================
      *   BOUNDARY VARS
      *=================================================================

       01  LO-TOP-BOUND       PIC 9(2).
       01  LO-BOTTOM-BOUND    PIC 9(2).
       01  LO-RIGHT-BOUND     PIC 9(2).
       01  LO-LEFT-BOUND      PIC 9(2).

       01  LO-FLOOR           PIC 9(2).



       PROCEDURE DIVISION.
       MAIN-PARA.
       
      *=================================================================
      *   Initialization
      *=================================================================
       
       SET ENVIRONMENT "COB_TIMEOUT_SCALE" TO "3"
       PERFORM START-CLOCK-PARA.
       PERFORM CALCULATE-BOUNDS-PARA.
       CALL "INITIALIZE_PIECES" USING WS-BLOCKS.
       COMPUTE WS-SEED = WS-START-DATE/1000.
       COMPUTE WS-CURRENT-DATE = FUNCTION RANDOM(WS-SEED).
       CALL "GET_NEXT_PIECE" USING WS-CURRENT-PIECE.
       CALL "GET_NEXT_PIECE" USING WS-NEXT-PIECE.
       
       CALL "MAKE_BORDER" USING 
       WS-BOARD-POS-LS WS-BOARD-HEIGHT WS-BOARD-WIDTH.
       PERFORM RESET-POSITION-PARA.

      *=================================================================
      *    GAME LOOP START
      *=================================================================

       PERFORM UNTIL WS-GAME-ON = WS-INP

       PERFORM CLOCK-PARA
       ACCEPT WS-INP AT 5001 WITH AUTO-SKIP TIME-OUT AFTER 1 

         
       PERFORM NEW-PLAYER-MOVE-PARA
       
       DISPLAY WS-INP AT 5003 WITH BACKGROUND-COLOR 4 

       PERFORM CHANGE-PLAYER-POS-PARA 

       ADD 1 TO WS-SCORE

       DISPLAY WS-REL-PLAYERPOS-LS AT 0301
      * MOVE WS-BOARD-ROW(1) TO WS-BOARD-ROW(20)
       END-PERFORM.
      *=================================================================
      *    GAME LOOP END
      *=================================================================
       STOP RUN.
       
      *=================================================================
      *   CALCULATE BOUNDS
      *=================================================================
       
       CALCULATE-BOUNDS-PARA.
       COMPUTE LO-LEFT-BOUND = 1.
       COMPUTE LO-RIGHT-BOUND = WS-BOARD-WIDTH.
       COMPUTE LO-TOP-BOUND = 1.
       COMPUTE LO-BOTTOM-BOUND = WS-BOARD-HEIGHT.
       
      *=================================================================
      *    HANDLE PLAYER MOVEMENT
      *=================================================================

       NEW-PLAYER-MOVE-PARA.
      * MOVE 0 TO WS-PIECE-COLLISION.
       MOVE FUNCTION UPPER-CASE (WS-INP) TO WS-INP.
       MOVE WS-INP TO WS-DIR
       MOVE WS-REL-PLAYERPOS-LS TO WS-CHECK-PLAYERPOS-LS.
       EVALUATE WS-DIR
       
       WHEN "F"
       PERFORM FLOOR-PIECE-PARA
       EXIT PARAGRAPH
       
       WHEN "R"
       DIVIDE WS-ROTATION BY 4 GIVING LO-TEMP-NUM REMAINDER WS-ROTATION
       ADD 1 TO WS-ROTATION
       PERFORM COLLISION-CHECK-PARA
       IF WS-PIECE-COLLISION = 1 THEN
       ADD 2 TO WS-ROTATION
       DIVIDE WS-ROTATION BY 4 GIVING LO-TEMP-NUM REMAINDER WS-ROTATION
       ADD 1 TO WS-ROTATION
       END-IF
       EXIT PARAGRAPH
       
       WHEN "Q"
       EXIT PARAGRAPH
       
       WHEN "S"
       ADD 1 TO WS-CHECK-ROW
       PERFORM COLLISION-CHECK-PARA
       IF WS-PIECE-COLLISION = 0 THEN
       MOVE WS-CHECK-PLAYERPOS-LS TO WS-REL-PLAYERPOS-LS
       ELSE 
       PERFORM PLACE-CURRENT-PIECE-PARA
       END-IF
       PERFORM RESET-DROP-PARA
       EXIT PARAGRAPH

       WHEN "A"
       SUBTRACT 1 FROM WS-CHECK-COL
       
       WHEN "D"
       ADD 1 TO WS-CHECK-COL
       END-EVALUATE.
       PERFORM COLLISION-CHECK-PARA.
       IF WS-PIECE-COLLISION = 0 THEN
       MOVE WS-CHECK-PLAYERPOS-LS TO WS-REL-PLAYERPOS-LS
       END-IF.
       
       
       
      *=================================================================
      *   CHECK COLLISION
      *================================================================= 

       COLLISION-CHECK-PARA.
      * DISPLAY WS-CHECK-PLAYERPOS-LS AT 4030.
      * COMPUTE LO-COL-TEMP = WS-CHECK-COL.
      * DISPLAY LO-COL-TEMP AT 4230.
      * COMPUTE LO-COL-TEMP = WS-CHECK-COL + 
      * WS-PIECE-ROW(WS-CURRENT-PIECE, WS-ROTATION, 4).
      * DISPLAY LO-COL-TEMP AT 4330.
       MOVE 0 TO WS-PIECE-COLLISION
       PERFORM VARYING LO-TEMP-NUM FROM 1 BY 1 UNTIL LO-TEMP-NUM = 5
       COMPUTE LO-ROW-TEMP = WS-CHECK-ROW + 
       WS-PIECE-ROW(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       COMPUTE LO-COL-TEMP = WS-CHECK-COL + 
       WS-PIECE-COL(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       MOVE WS-BOARD-COLS(LO-ROW-TEMP,LO-COL-TEMP) TO LO-TEMP-CHAR
       IF LO-TEMP-CHAR <> WS-EMPTY-SPACE OR LO-COL-TEMP < 1 OR 
       LO-COL-TEMP > 10 OR LO-ROW-TEMP > 20 THEN
       MOVE 1 TO WS-PIECE-COLLISION
       DISPLAY "COLLISION" AT 4130 
       EXIT PARAGRAPH
       END-PERFORM.
       

      *=================================================================
      *   SET THE POSITION OF THE PLAYER INSIDE THE BOARD ARRAY
      *   DELETE THE LAST POSITION SO YOU DO NOT LEAVE A TRAIL
      *=================================================================

       CHANGE-PLAYER-POS-PARA.
       PERFORM FIND-FLOOR-PARA.
       PERFORM DRAW-SHADOW-PIECE-PARA.
       PERFORM DRAW-CURRENT-PIECE-PARA.
       CALL "DRAW_BOARD" USING BY REFERENCE WS-BOARD WS-BOARD-POS-LS 
       WS-BOARD-HEIGHT WS-BOARD-WIDTH WS-SCORE.
       PERFORM CLEAR-SHADOW-PIECE-PARA.
       PERFORM CLEAR-CURRENT-PIECE-PARA.
       MOVE WS-REL-PLAYERPOS-LS TO WS-PREV-PLAYERPOS-LS.

       
      *=================================================================
      *   HANDLE PIECE MOVING DOWN
      *================================================================= 

       DROP-PIECE-PARA.
       MOVE "S" TO WS-INP.
       PERFORM NEW-PLAYER-MOVE-PARA.

       DRAW-SHADOW-PIECE-PARA.
      * PERFORM FIND-FLOOR-PARA.
       PERFORM VARYING LO-TEMP-NUM FROM 1 BY 1 UNTIL LO-TEMP-NUM = 5
       COMPUTE LO-ROW-TEMP = LO-FLOOR + 
       WS-PIECE-ROW(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       COMPUTE LO-COL-TEMP = WS-REL-COL + 
       WS-PIECE-COL(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       MOVE "/" TO WS-BOARD-COLS(LO-ROW-TEMP,LO-COL-TEMP)
       END-PERFORM.

       CLEAR-SHADOW-PIECE-PARA.
      * PERFORM FIND-FLOOR-PARA.
       PERFORM VARYING LO-TEMP-NUM FROM 1 BY 1 UNTIL LO-TEMP-NUM = 5
       COMPUTE LO-ROW-TEMP = LO-FLOOR + 
       WS-PIECE-ROW(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       COMPUTE LO-COL-TEMP = WS-REL-COL + 
       WS-PIECE-COL(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       MOVE WS-EMPTY-SPACE TO WS-BOARD-COLS(LO-ROW-TEMP,LO-COL-TEMP)
       END-PERFORM.

       DRAW-CURRENT-PIECE-PARA.
       PERFORM VARYING LO-TEMP-NUM FROM 1 BY 1 UNTIL LO-TEMP-NUM = 5
       COMPUTE LO-ROW-TEMP = WS-REL-ROW + 
       WS-PIECE-ROW(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       COMPUTE LO-COL-TEMP = WS-REL-COL + 
       WS-PIECE-COL(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       MOVE WS-CURRENT-PIECE TO WS-BOARD-COLS(LO-ROW-TEMP,LO-COL-TEMP)
       END-PERFORM.
       
       PLACE-CURRENT-PIECE-PARA.
       PERFORM DRAW-CURRENT-PIECE-PARA.
       MOVE WS-NEXT-PIECE TO WS-CURRENT-PIECE.
       MOVE 0 TO WS-CLEARED-ROWS.
       MOVE WS-REL-ROW TO WS-CHECK-FOR-CLEAR.
       PERFORM VARYING LO-TEMP-NUM FROM 1 BY 1 UNTIL LO-TEMP-NUM = 5
       IF WS-CHECK-FOR-CLEAR < 21 THEN
       DISPLAY WS-CHECK-FOR-CLEAR AT 3530
       CALL "CHECK_AND_CLEAR_ROWS" USING BY REFERENCE 
       WS-BOARD WS-EMPTY-SPACE WS-CHECK-FOR-CLEAR WS-CLEARED-ROWS
       COMPUTE WS-CHECK-FOR-CLEAR = WS-CHECK-FOR-CLEAR  + 1 
       END-IF
       END-PERFORM.
       CALL "GET_NEXT_PIECE" USING WS-NEXT-PIECE.
       PERFORM RESET-POSITION-PARA.

       CLEAR-CURRENT-PIECE-PARA.
       PERFORM VARYING LO-TEMP-NUM FROM 1 BY 1 UNTIL LO-TEMP-NUM = 5
       COMPUTE LO-ROW-TEMP = WS-REL-ROW + 
       WS-PIECE-ROW(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       COMPUTE LO-COL-TEMP = WS-REL-COL + 
       WS-PIECE-COL(WS-CURRENT-PIECE, WS-ROTATION, LO-TEMP-NUM)
       MOVE WS-EMPTY-SPACE TO WS-BOARD-COLS(LO-ROW-TEMP,LO-COL-TEMP)
       END-PERFORM.
       
       FIND-FLOOR-PARA.
       MOVE 0 TO WS-PIECE-COLLISION.
       DISPLAY WS-REL-PLAYERPOS-LS AT 4030.
       MOVE WS-REL-PLAYERPOS-LS TO WS-CHECK-PLAYERPOS-LS.
       PERFORM UNTIL WS-PIECE-COLLISION <> 0
       PERFORM COLLISION-CHECK-PARA
       COMPUTE WS-CHECK-ROW = WS-CHECK-ROW + 1 
       END-PERFORM.
       COMPUTE LO-FLOOR = WS-CHECK-ROW - 2.
       DISPLAY LO-FLOOR AT 3330.

       FLOOR-PIECE-PARA.
       PERFORM FIND-FLOOR-PARA.
       MOVE LO-FLOOR TO WS-REL-ROW.
       PERFORM DROP-PIECE-PARA.
      * PERFORM CHANGE-PLAYER-POS-PARA.

       RESET-POSITION-PARA.
       MOVE WS-PIECE-START-POS-LS TO WS-REL-PLAYERPOS-LS.
       MOVE WS-PIECE-START-POS-LS TO WS-PREV-PLAYERPOS-LS.
       PERFORM CHANGE-PLAYER-POS-PARA.
       
      *=================================================================
      *    HANDLE PLAYER MOVEMENT END
      *=================================================================
       
      *=================================================================
      *   HANDLE TIME SENSITIVE FUNCTIONS
      *=================================================================
      
       START-CLOCK-PARA.
      *begin the clock for the rest of the program relies on
      *initialize last drop time for dropping pieces
       MOVE FUNCTION CURRENT-DATE TO WS-START-TIME-DATA.
       CALL "CONVERT_TIME_TO_SEC" USING WS-START-TIME-DATA.
       MOVE WS-START-TIME-DATA TO WS-LAST-DROP-TIME.

       CLOCK-PARA.
      *clock function that runs each game cycle (determined by player input inta
      *gets the current date, calculates the time since start, and time since la
      *check if its time to drop the piece
      *display time stats at bottom of screen
       MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME-DATA.
       CALL "CONVERT_TIME_TO_SEC" USING WS-CURRENT-TIME-DATA.
       COMPUTE WS-TIME-SINCE-START =
       (WS-CURRENT-DATE - WS-START-DATE)/1000.

       COMPUTE WS-TIME-SINCE-DROP = (WS-CURRENT-DATE/1000) - 
       WS-LAST-DROP-TIME.
       
       PERFORM CHECK-TIME-PARA.

       DISPLAY WS-START-DATE AT 4701.
       DISPLAY  WS-CURRENT-DATE AT 4801.
       DISPLAY WS-TIME-SINCE-START AT 4901.
       DISPLAY WS-TIME-SINCE-DROP AT 5101.

       CHECK-TIME-PARA.
      *check time since last drop vs time before drop to see if the piece should
       IF WS-TIME-SINCE-DROP > WS-TIME-BEFORE-DROP THEN
       PERFORM DROP-PIECE-PARA
       COMPUTE WS-LAST-DROP-TIME = (WS-CURRENT-DATE/1000)
       END-IF.

       RESET-DROP-PARA.
      *function called when player moves piece down manually, just resets the la
       MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME-DATA.
       CALL "CONVERT_TIME_TO_SEC" USING WS-CURRENT-TIME-DATA.
       COMPUTE WS-LAST-DROP-TIME = WS-CURRENT-DATE/1000.

       END PROGRAM cobol_tetris.

      *=================================================================
      *    MAIN PROGRAM END
      *=================================================================


