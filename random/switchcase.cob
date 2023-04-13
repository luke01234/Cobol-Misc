       identification division.
       program-id. switchcase.

       data division.
       WORKING-STORAGE SECTION.
       01 num    PIC 9(3).

       procedure division.
       main-PARA.
       PERFORM varying num from 0 by 1 until num > 6
       EVALUATE num
       when 0
       display "num is zero."
       when 1
       display "num is one."
       when 2 
       display "num is two."
       when 3 
       display "num is three."
       when 4
       display "num is four."
       when other
       display "num is over four."
       END-EVALUATE
       END-PERFORM.
       STOP RUN.