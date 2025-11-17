      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This function is in the public domain.
      *  Contributed by James K. Lowden of COBOLworx November 2025.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   
      *  long  write( int  fd, const void *  buf,  unsigned long  count)
        Identification Division.
        Function-ID. posix-write.
        Data Division.
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-fd PIC 9(8) Usage COMP.
          01 Lk-buf PIC X ANY LENGTH.
          01 Lk-count PIC 9(8) Usage COMP.
        Procedure Division using
             By Value Lk-fd,
             By Reference Lk-buf,
             By Value Lk-count
             Returning Return-Value.

          Call "write" using
             By Value Lk-fd,
             By Reference Lk-buf,
             By Value Lk-count
             Returning Return-Value.
          Goback.
        End Function posix-write.
