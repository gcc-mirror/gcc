      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This program is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in October 2025
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

        COPY posix-mkdir.
        COPY posix-errno.

        Identification Division.
        Program-ID. test-errno.
        Data Division.
        Working-Storage Section.
        77 Return-Value Binary-Long.
        77 Exit-Status Binary-Long Value 1.
        77 error-msg PIC X(100).
        77 errnum Binary-Long.
        77 Filename PIC X(100) Value '/'.

        Procedure Division.
        Display 'calling posix-mkdir with a foolish name ...'
        Move Function posix-mkdir(Filename, 0) to Return-Value.
        If Return-Value <> 0
            Display 'calling posix-errno ...'
            Move Function posix-errno(error-msg) to errnum
            Display 'error: "' Filename '": ' error-msg ' (' errnum ')'
            Goback with Error Status errnum
        Else
            Display 'Return-Value is ' Return-Value
        End-If.

        Goback.
