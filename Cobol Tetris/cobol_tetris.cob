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
      
       01  WS-BOARD.       
           05  WS-BOARD-ROW OCCURS 20 TIMES.
               06  WS-BOARD-COLS OCCURS 10 TIMES.
                   07  WS-BOARD-VAL    PIC X(1) VALUE "/".

      *=================================================================
      *   GAME STATE AND SCORE
      *=================================================================

       01  WS-GAME-ON        PIC X(1)    VALUE "\".
       01  WS-SCORE          PIC 9(10)   VALUE ZEROS.
       
      *=================================================================
      *   PLAYER MOVEMENT VARS
      *=================================================================

       01  WS-PLACE-PIECE    PIC 9(1).

       01  WS-REL-PLAYERPOS-LS.
           05 WS-REL-ROW  PIC 9(2) VALUE 01.
           05 WS-REL-COL  PIC 9(2) VALUE 01.
       
       01  WS-PREV-PLAYERPOS-LS.
           05 WS-PREV-ROW PIC 9(2) VALUE 01.
           05 WS-PREV-COL PIC 9(2) VALUE 01.

       01  WS-PIECE-START-POS PIC 9(4) VALUE 0105.       
       
       01  WS-INP            PIC X(1).

      *=================================================================
      *   TIME VARS
      *=================================================================

       01  WS-TIME-BEFORE-DROP         PIC 9(4) VALUE 200.
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

       SET ENVIRONMENT "COB_TIMEOUT_SCALE" TO "3"
       PERFORM START-CLOCK-PARA.
       PERFORM CALCULATE-BOUNDS-PARA.
       CALL "MAKE_BORDER" USING 
       WS-BOARD-POS-LS WS-BOARD-HEIGHT WS-BOARD-WIDTH.
       MOVE WS-PIECE-START-POS TO WS-REL-PLAYERPOS-LS.
       MOVE WS-PIECE-START-POS TO WS-PREV-PLAYERPOS-LS.

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
       EVALUATE WS-INP
       WHEN "A"
       IF WS-REL-COL <> LO-LEFT-BOUND THEN
       ADD -1 TO WS-REL-COL
       WHEN "D"
       IF WS-REL-COL <> LO-RIGHT-BOUND THEN
       ADD 1 TO WS-REL-COL
       END-IF
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

       CHECK-BELOW-PIECE.
       COMPUTE LO-TEMP-NUM = WS-REL-ROW + 1.
       MOVE WS-BOARD-COLS(LO-TEMP-NUM,WS-REL-COL) TO LO-TEMP-CHAR.
       IF LO-TEMP-CHAR = "A" THEN
       MOVE 1 TO WS-PLACE-PIECE
       ELSE IF LO-TEMP-NUM > WS-BOARD-HEIGHT
       MOVE 1 TO WS-PLACE-PIECE
       ELSE 
       MOVE 0 TO WS-PLACE-PIECE
       END-IF.
       
      *=================================================================
      *   SET THE POSITION OF THE PLAYER INSIDE THE BOARD ARRAY
      *   DELETE THE LAST POSITION SO YOU DO NOT LEAVE A TRAIL
      *=================================================================

       CHANGE-PLAYER-POS-PARA.
       MOVE "/" TO WS-BOARD-COLS(WS-PREV-ROW,WS-PREV-COL)
       MOVE "A" TO WS-BOARD-COLS(WS-REL-ROW,WS-REL-COL)
       CALL "DRAW_BOARD" USING BY REFERENCE WS-BOARD WS-BOARD-POS-LS 
       WS-BOARD-HEIGHT WS-BOARD-WIDTH WS-SCORE.
       MOVE WS-REL-PLAYERPOS-LS TO WS-PREV-PLAYERPOS-LS.

       
      *=================================================================
      *   HANDLE PIECE MOVING DOWN
      *================================================================= 

       DROP-PIECE-PARA.
       PERFORM CHECK-BELOW-PIECE.
       IF WS-PLACE-PIECE = 0 THEN
       ADD 1 TO WS-REL-ROW
       ELSE 
       MOVE "A" TO WS-BOARD-COLS(WS-REL-ROW,WS-REL-COL)
       MOVE WS-PIECE-START-POS TO WS-REL-PLAYERPOS-LS
       MOVE WS-PIECE-START-POS TO WS-PREV-PLAYERPOS-LS
       END-IF.
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
      *clock function that runs each game cycle (determined by player input intake)
      *gets the current date, calculates the time since start, and time since last drop
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
      *check time since last drop vs time before drop to see if the piece should drop, then reset time last drop time to the current time
       IF WS-TIME-SINCE-DROP > WS-TIME-BEFORE-DROP THEN
       PERFORM DROP-PIECE-PARA
       COMPUTE WS-LAST-DROP-TIME = (WS-CURRENT-DATE/1000)
       END-IF.

       RESET-DROP-PARA.
      *function called when player moves piece down manually, just resets the last drop time to current time
       MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME-DATA.
       CALL "CONVERT_TIME_TO_SEC" USING WS-CURRENT-TIME-DATA.
       COMPUTE WS-LAST-DROP-TIME = WS-CURRENT-DATE/1000.

       END PROGRAM cobol_tetris.

      *=================================================================
      *    MAIN PROGRAM END
      *=================================================================