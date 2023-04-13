       IDENTIFICATION DIVISION.
       PROGRAM-ID. ifstatements.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 VAR01 PIC 9(3).
           01 VAR02 PIC X(5) VALUE "False".
           01 VAR03 PIC 9(3).
           01 VAR04 PIC A(4).
       
       PROCEDURE DIVISION.
           A000-FIRST-PARA.
           MOVE 100 TO VAR01.
           MOVE 900 TO VAR03.
           MOVE "True" TO VAR04.

           IF VAR03 > VAR01 THEN
              DISPLAY VAR04
              
           
           ELSE
              DISPLAY VAR02
           END-IF.

       STOP RUN.