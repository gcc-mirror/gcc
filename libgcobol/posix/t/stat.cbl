      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This program is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in October 2025
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      * Include the posix-stat function
        COPY posix-stat.
        COPY posix-errno.

        Identification Division.
        Program-ID. test-stat.
        Data Division.
        Working-Storage Section.
        77 Return-Value Usage Binary-Long.
        77 Stat-Status  Usage Binary-Long Value 1.
        77 Filename     Pic x(80) Value 'Makefile'.
        77 Msg Pic x(100).
        01 Lk-statbuf.
        COPY statbuf.
        
        Procedure Division.
        Display 'calling posix-stat ...'
        Move Function posix-stat(Filename, lk-statbuf) to Return-Value.
        Display 'posix-stat return value:' Return-Value.
        If Return-Value < 0 then
          Display Function Trim(Filename) ': '
                  'errno ', Function posix-errno(Msg), ': ' Msg.
          
        Goback.
