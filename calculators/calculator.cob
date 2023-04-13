       identification division.
       program-id. calculator.

       data division.
           WORKING-STORAGE SECTION.
           01 inputVar1 PIC A(12).
           01 inputVar2 Pic 9(4).
           01 inputVar3 Pic 9(4).
           01 loopOn    PIC 9(1) VALUE 0.
           01 solution Pic 9(10).
       
       procedure division.
       main-PARA.
       perform loop-PARA until loopOn=1.
       STOP RUN.

       loop-PARA.
       MOVE 0 TO solution.
       display "Hello, what would you like to do today?".
       display "(Choose: ""ADD"",""SUBTRACT"",""DIVIDE"","
       """MULTIPLY"",""""EXPONENTIATE"") or ""EXIT"" to quit."          
       end-display.
       accept inputVar1.
       
       IF inputVar1 = "ADD" THEN
          display "What Numbers would you like to add? (4 digits)"
          accept inputVar2
          display "Choose a second number (4 digits)"
          accept inputVar3
          ADD inputVar2 TO solution
          ADD inputVar3 TO solution
          display inputVar2 " + " inputVar3 " = " solution
       ELSE IF inputVar1 = "SUBTRACT" THEN
           display "What is the number you would like to subtract "
           "from? (4 digits)"
           end-display 
           accept inputVar2
           display "Choose a number you would like to subtract (4 "
           "digits)"
           end-display 
           accept inputVar3
           ADD inputVar2 TO solution
           SUBTRACT inputVar3 FROM solution
           display inputVar2 " - " inputVar3 " = " solution
       ELSE IF inputVar1 = "MULTIPLY" THEN
           display "What Numbers would you like to multiply? (4 "
           "digits)"
           end-display 
           accept inputVar2
           display "Choose a number to multiply by (4 digits)"  
           accept inputVar3
           MULTIPLY inputVar2 BY inputVar3 GIVING solution
           display inputVar2 " * " inputVar3 " = " solution
       ELSE IF inputVar1 = "DIVIDE" THEN
           display "What Numbers would you like to divide"
           " from? (4 digits)"
           accept inputVar2
           display "Choose a number to divide by (4 digits)"
           accept inputVar3
           DIVIDE inputVar2 BY inputVar3 GIVING solution
           display inputVar2 " / " inputVar3 " = " solution
       ELSE IF inputVar1 = "EXPONENTIATE" THEN
           display "What Numbers would you like as a base? (4 "
           "digits)"
           end-display 
           accept inputVar2
           display "Choose a power (4 digits)"
           accept inputVar3
           COMPUTE solution = inputVar2 ** inputVar3 
           display inputVar2 " ^ " inputVar3 " = " solution
       ELSE IF inputVar1 = "EXIT" THEN
           ADD 1 TO loopOn
       ELSE
           display "Unrecognized Input."
       end-if.
