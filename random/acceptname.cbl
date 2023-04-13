       IDENTIFICATION DIVISION.
       PROGRAM-ID. testing.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 inputVar PIC X(20).
           
       PROCEDURE DIVISION.
           DISPLAY "Please enter your name: "
           ACCEPT inputVar.
           DISPLAY "Welcome to Cobol " inputVar 
       STOP RUN.