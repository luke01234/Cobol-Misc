       identification division.
       program-id. returns.

       data division.
       working-storage section.
       01  VAR01   PIC 9(10).



       procedure division.
       main-PARA.
       call "recc" using VAR01 returning VAR01.
       display VAR01.
       STOP RUN.

       end program returns.
       
      *==============================================

       identification division.
       program-id. recc.

       data division.
       linkage section.
       01  var2  PIC 9(10).

       procedure division using var2 returning var2.
       MOVE 10000 TO var2.
       end program recc.