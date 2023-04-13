       IDENTIFICATION DIVISION.
       PROGRAM-ID. hellofromfile.
       
       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
             FILE-CONTROL.
             SELECT inpFile ASSIGN TO 'hello.txt'
             ORGANIZATION IS LINE SEQUENTIAL.            
       
       DATA DIVISION.
          FILE SECTION.
          FD inpFile.
          01 inpFile-FILE.
             05 chars  PIC X(40).
       
          WORKING-STORAGE SECTION.
          01 WS-STRING.
             05 WS-TEXT PIC X(40).
          01 WS-EOF PIC A(3). 
       
       PROCEDURE DIVISION.
       OPEN INPUT inpFile.
          PERFORM UNTIL WS-EOF='END'
             READ inpFile INTO WS-STRING
                AT END MOVE 'END' TO WS-EOF
                NOT AT END DISPLAY WS-STRING 
             END-READ
          END-PERFORM.
       CLOSE inpFile.
       
       STOP RUN.