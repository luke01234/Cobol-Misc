       identification division.
       program-id. isOddorEven.
       
       data division.
       WORKING-STORAGE SECTION.
       01 inputVar  PIC 9(10).
       01 result    PIC 9(1).
       01 remain    PIC 9(1).
       procedure division.
       main-PARA.
       display "Please enter an integer (10 Digits MAX)".
       accept inputVar.
       perform function-PARA.
       
       STOP RUN.
       
       function-PARA.
       DIVIDE inputVar BY 2 GIVING result REMAINDER remain.
       IF remain = 0 THEN
       display "integer is Even."
       ELSE
       display "integer is Odd."
       END-IF.
