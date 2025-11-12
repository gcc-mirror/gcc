        >> PUSH source format
        >>SOURCE format is fixed

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This function is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in October 2025.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       Identification Division.
       Function-ID. posix-errno.

       Data Division.
       Linkage Section.
       77 Return-Value Binary-Long.
       01 Error-Msg PIC X ANY LENGTH.

       Procedure Division
           using Error-Msg
           Returning Return-Value.
       CALL "posix_errno"
           returning Return-Value.
       CALL "strerror"
           using by value Return-Value
           returning error-msg.
       Goback.
       END FUNCTION posix-errno.
        >> POP source format
