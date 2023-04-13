       IDENTIFICATION DIVISION.
       PROGRAM-ID. ifstatements.
       
       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 VAR01 PIC 9(3).
           01 VAR02 PIC 9(5) VALUE 200.
           01 VAR03 PIC 9(3).
           01 VAR04 PIC 9(4) VALUE 100.
       
       PROCEDURE DIVISION.
           A000-FIRST-PARA.
           MOVE 100 TO VAR01.
           MOVE 900 TO VAR03.
           display 
           "VARS are: "
           VAR01" "
           VAR02" "
           VAR03" "
           VAR04" "
           end-display

           IF VAR03 > VAR01 THEN
              DISPLAY "First if"
              
           
           ELSE
              DISPLAY "First else"
           END-IF.

           IF VAR01 <> VAR04 THEN
              DISPLAY "Second if"
           END-IF
           
           IF VAR03 > VAR04 THEN
              ADD 800 TO VAR04
              DISPLAY "Var4 is " VAR04

       STOP RUN.