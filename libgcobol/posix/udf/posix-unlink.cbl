       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This function is in the public domain.
      *  Contributed by 
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   
        Identification Division.
        Function-ID. posix-unlink.
        Data Division.
        Working-Storage Section.
          77 bufsize Usage Binary-Long.
          77 Ws-pathname PIC X(8192).
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-pathname PIC X ANY LENGTH.
          
        Procedure Division using
             By Reference Lk-pathname,
             Returning Return-Value.

          Move Lk-pathname To Ws-pathname.
          Inspect Ws-pathname 
                  Replacing Trailing Space By Low-Value

          Inspect Backward Ws-pathname Replacing Leading Space, 
      -      By Low-Value.
          Call "unlink" using
             By Reference Ws-pathname,
             Returning Return-Value.
          Goback.
        End Function posix-unlink.
        >> POP SOURCE FORMAT
