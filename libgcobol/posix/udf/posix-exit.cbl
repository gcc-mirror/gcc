       Identification Division.
       Function-ID. posix-exit.

       Data Division.
       Linkage Section.
       77 Return-Value Binary-Long.
       77 Exit-Status Binary-Long.

       Procedure Division using Exit-Status Returning Return-Value.
       CALL "_exit" using by value Exit-Status.
       Goback.
       END FUNCTION posix-exit.