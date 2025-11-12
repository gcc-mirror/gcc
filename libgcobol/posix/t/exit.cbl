      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This program is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in October 2025
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

        COPY posix-exit.

        Identification Division.
        Program-ID. test-exit.
        Data Division.
        Working-Storage Section.
        77 Return-Value Binary-Long.
        77 Exit-Status Binary-Long Value 1.

        Procedure Division.
        Display 'calling posix-exit ...'
        Move Function posix-exit(Exit-Status) to Return-Value.
      * Does not return, Does not print
        Display 'How did we get here?'
        Goback.
