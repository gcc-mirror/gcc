      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This function is in the public domain.
      *  Contributed by smckinney of COBOLworx Feb 2026.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  int ftruncate(int fd, off_t length);
        Identification Division.
        Function-ID. posix-ftruncate.
        Data Division.
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-fd PIC 9(8) Usage COMP.
          01 Lk-offset Binary-Long.
        Procedure Division using
             By Value Lk-fd,
             By Value Lk-offset,
             Returning Return-Value.
          Display 'posix-ftruncate fd: ' Lk-fd ', Lk-offset: ' Lk-offset.
          Call "ftruncate" using
             By Value Lk-fd,
             By Value Lk-offset,
             Returning Return-Value.
          Goback.
        End Function posix-ftruncate.
