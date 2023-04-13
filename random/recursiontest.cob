       identification division.
       program-id. recurstiontest.
       
       data division.
       WORKING-STORAGE SECTION.
       01 cycleNum     PIC 9(2) VALUE 0.
       01 totalCycles  PIC 9(2).
       
       
       procedure division.
       main-PARA.
       display "Enter number of cycles:"
       accept totalCycles.
       CALL "subfunc" USING cycleNum totalCycles.
       STOP RUN.

       END PROGRAM recurstiontest.

       identification division.
       program-id. subfunc RECURSIVE.

       data division.
       LINKAGE SECTION.
       01  cycleNum    PIC 9(2).
       01  totalCycles PIC 9(2).
       procedure division USING cycleNum totalCycles.
       display cycleNum.
       IF cycleNum < totalCycles THEN
       ADD 1 to cycleNum
       CALL "subfunc" USING cycleNum totalCycles
       END-IF.

       END PROGRAM subfunc.