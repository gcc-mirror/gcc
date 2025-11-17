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
        Procedure Division using
             By Value Lk-fd,
             By Value Lk-offset,
             By Value Lk-whence
             Returning Return-Value.
          Call "lseek" using
             By Value Lk-fd,
             By Value Lk-offset,
             By Value Lk-whence
             Returning Return-Value.
          Goback.
        End Function posix-lseek.
