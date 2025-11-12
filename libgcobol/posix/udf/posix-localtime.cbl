      *  int  stat(const char *  pathname,  struct stat *  statbuf)
        Identification Division.
        Function-ID. posix-localtime.
        Data Division.
        Working-Storage Section.
          77 bufsize Usage Binary-Long.
          77 Tm-pointer Usage Pointer.
          01 Lk-tm-posix Based.
          COPY tm.
        Linkage Section.
          77 Return-Value Usage Binary-Long.
          01 Lk-timep Usage Binary-Long.
          01 Lk-tm.
          COPY tm.
          
        Procedure Division using
             By Reference Lk-timep,
             By Reference Lk-tm, 
             Returning Return-Value.

          Move Function Length(Lk-tm-posix) to bufsize.
          Call "posix_localtime" using
             By Reference Lk-timep,
             By Value     bufsize, 
             Returning tm-pointer.

          If tm-pointer = NULL
             move -1 to Return-Value
          Else
             move 0 to Return-Value
             set address of lk-tm-posix to tm-pointer
             move lk-tm-posix to lk-tm.
             
          Goback.
        End Function posix-localtime.
