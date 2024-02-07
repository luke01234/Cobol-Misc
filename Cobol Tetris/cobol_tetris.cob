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
       
      *=================================================================
      *   PLAYER MOVEMENT VARS
      *=================================================================

       01  WS-PIECE-COLLISION    PIC 9(1).

       01  WS-CAN-MOVE       PIC 9(1).

       01  WS-REL-PLAYERPOS-LS.
           05 WS-REL-ROW  PIC 9(2) VALUE 01.
           05 WS-REL-COL  PIC 9(2) VALUE 01.
       
       01  WS-PREV-PLAYERPOS-LS.
           05 WS-PREV-ROW PIC 9(2) VALUE 01.
           05 WS-PREV-COL PIC 9(2) VALUE 01.

       01  WS-PIECE-START-POS PIC 9(4) VALUE 0105.       
       
       01  WS-INP            PIC X(1).

       01  WS-DIR            PIC X(1).

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

       01  LO-TEMP-CHAR       PIC X(4).
       
      *=================================================================
      *   BOUNDARY VARS
      *=================================================================

       01  LO-TOP-BOUND       PIC 9(2).
       01  LO-BOTTOM-BOUND    PIC 9(2).
       01  LO-RIGHT-BOUND     PIC 9(2).
       01  LO-LEFT-BOUND      PIC 9(2).



       PROCEDURE DIVISION.
       MAIN-PARA.
       
      *=================================================================
      *   Initialization
      *=================================================================

       SET ENVIRONMENT "COB_TIMEOUT_SCALE" TO "3"
       PERFORM START-CLOCK-PARA.
       PERFORM CALCULATE-BOUNDS-PARA.
       CALL "MAKE_BORDER" USING 
       WS-BOARD-POS-LS WS-BOARD-HEIGHT WS-BOARD-WIDTH.
       PERFORM RESET-POSITION-PARA.

      *=================================================================
      *    GAME LOOP START
      *=================================================================

       PERFORM UNTIL WS-GAME-ON = WS-INP

       PERFORM CLOCK-PARA
       ACCEPT WS-INP AT 5001 WITH AUTO-SKIP TIME-OUT AFTER 1000
       PERFORM PLAYER-MOVE-PARA
       
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

       PLAYER-MOVE-PARA.
       MOVE FUNCTION UPPER-CASE (WS-INP) TO WS-INP.
       MOVE WS-INP TO WS-DIR
       EVALUATE WS-INP
       WHEN "A"
       PERFORM HORIZONTAL-MOVE-PARA
       WHEN "D"
       PERFORM HORIZONTAL-MOVE-PARA
       WHEN "W"
       IF WS-REL-ROW <> LO-TOP-BOUND THEN
       COMPUTE WS-REL-ROW = WS-REL-ROW
       END-IF
       WHEN "S"
       PERFORM RESET-DROP-PARA
       PERFORM DROP-PIECE-PARA
       WHEN "G"
       DISPLAY WS-REL-PLAYERPOS-LS AT 0210
       END-EVALUATE.
       
      *=================================================================
      *   CHECK BELOW PIECE TO SEE IF IT SHOULD BE PLACED
      *================================================================= 
       
       CHECK-BEFORE-MOVE-PARA.
       EVALUATE WS-DIR
       WHEN "S"
       COMPUTE LO-TEMP-NUM = WS-REL-ROW + 1
       MOVE WS-BOARD-COLS(LO-TEMP-NUM,WS-REL-COL) TO LO-TEMP-CHAR
       IF LO-TEMP-CHAR <> WS-EMPTY-SPACE THEN
       MOVE 1 TO WS-PIECE-COLLISION
       ELSE IF LO-TEMP-NUM > WS-BOARD-HEIGHT
       MOVE 1 TO WS-PIECE-COLLISION
       ELSE 
       MOVE 0 TO WS-PIECE-COLLISION
       END-IF
       WHEN "A"
       COMPUTE LO-TEMP-NUM = WS-REL-COL - 1
       MOVE WS-BOARD-COLS(WS-REL-ROW,LO-TEMP-NUM) TO LO-TEMP-CHAR
       IF LO-TEMP-CHAR <>    WS-EMPTY-SPACE THEN
       MOVE 1 TO WS-PIECE-COLLISION
       ELSE IF LO-TEMP-NUM < 1
       MOVE 1 TO WS-PIECE-COLLISION
       ELSE 
       MOVE 0 TO WS-PIECE-COLLISION
       END-IF
       WHEN "D"
       COMPUTE LO-TEMP-NUM = WS-REL-COL + 1
       MOVE WS-BOARD-COLS(WS-REL-ROW,LO-TEMP-NUM) TO LO-TEMP-CHAR
       IF LO-TEMP-CHAR <> WS-EMPTY-SPACE THEN
       MOVE 1 TO WS-PIECE-COLLISION
       ELSE IF LO-TEMP-NUM > 10
       MOVE 1 TO WS-PIECE-COLLISION
       ELSE 
       MOVE 0 TO WS-PIECE-COLLISION
       END-IF
       END-EVALUATE.

       HORIZONTAL-MOVE-PARA.
       MOVE WS-INP TO WS-DIR.
       PERFORM CHECK-BEFORE-MOVE-PARA.
       IF WS-PIECE-COLLISION = 0 THEN
       EVALUATE WS-DIR
       WHEN "A"
       SUBTRACT 1 FROM WS-REL-COL
       WHEN "D"
       ADD 1 TO WS-REL-COL
       END-EVALUATE.

       

      *=================================================================
      *   SET THE POSITION OF THE PLAYER INSIDE THE BOARD ARRAY
      *   DELETE THE LAST POSITION SO YOU DO NOT LEAVE A TRAIL
      *=================================================================

       CHANGE-PLAYER-POS-PARA.
       MOVE WS-EMPTY-SPACE TO WS-BOARD-COLS(WS-PREV-ROW,WS-PREV-COL)
       MOVE "A" TO WS-BOARD-COLS(WS-REL-ROW,WS-REL-COL)
       CALL "DRAW_BOARD" USING BY REFERENCE WS-BOARD WS-BOARD-POS-LS 
       WS-BOARD-HEIGHT WS-BOARD-WIDTH WS-SCORE.
       MOVE WS-REL-PLAYERPOS-LS TO WS-PREV-PLAYERPOS-LS.

       
      *=================================================================
      *   HANDLE PIECE MOVING DOWN
      *================================================================= 

       DROP-PIECE-PARA.
       MOVE "S" TO WS-DIR.
       PERFORM CHECK-BEFORE-MOVE-PARA.
       IF WS-PIECE-COLLISION = 0 THEN
       ADD 1 TO WS-REL-ROW
       ELSE 
       MOVE "A" TO WS-BOARD-COLS(WS-REL-ROW,WS-REL-COL)
       CALL "CHECK_AND_CLEAR_ROWS" USING 
       WS-BOARD WS-EMPTY-SPACE WS-REL-ROW
       PERFORM RESET-POSITION-PARA
       END-IF.
       PERFORM CHANGE-PLAYER-POS-PARA.
       
       RESET-POSITION-PARA.
       MOVE WS-PIECE-START-POS TO WS-REL-PLAYERPOS-LS.
       MOVE WS-PIECE-START-POS TO WS-PREV-PLAYERPOS-LS.
       
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


