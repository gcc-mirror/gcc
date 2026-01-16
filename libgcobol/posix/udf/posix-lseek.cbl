      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This function is in the public domain.
      *  Contributed by James K. Lowden of COBOLworx November 2025.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   
      *  unsigned long  lseek( int  fd,  unsigned long  offset,  int  whence)
        Identification Division.
        Function-ID. posix-lseek.
        Data Division.
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-fd PIC 9(8) Usage COMP.
          01 Lk-offset Binary-Long.
          01 Lk-whence Binary-Long.
             88 SEEK-SET VALUE 2.
             88 SEEK-CUR VALUE 4.
             88 SEEK-END VALUE 8.
        Procedure Division using
             By Value Lk-fd,
             By Value Lk-offset,
             By Value Lk-whence
             Returning Return-Value.
          Call "posix_lseek" using
             By Value Lk-fd,
             By Value Lk-offset,
             By Value Lk-whence
             Returning Return-Value.
          Goback.
        End Function posix-lseek.
