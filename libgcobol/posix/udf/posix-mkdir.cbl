        Identification Division.
        Function-ID. posix-mkdir.
        Data Division.
        Working-Storage Section.
          77 bufsize Usage Binary-Long.
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-pathname PIC X ANY LENGTH.
          01 Lk-Mode Binary-Long.
          
        Procedure Division using
             By Reference Lk-pathname,
             By Value Lk-Mode, 
             Returning Return-Value.
          Inspect Backward Lk-pathname Replacing Leading Space By Low-Value
          Call "mkdir" using
             By Reference Lk-pathname,
             By Value Lk-Mode, 
             Returning Return-Value.
          Goback.
        End Function posix-mkdir.
