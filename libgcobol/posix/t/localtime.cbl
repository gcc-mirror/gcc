      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This program is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in October 2025
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      * Include the posix-stat and posix-localtime functions.
        COPY posix-stat.
        COPY posix-localtime.
        COPY posix-errno.

        Identification Division.
        Program-ID. test-localtime.
        Data Division.
        Working-Storage Section.
        77 Return-Value Usage Binary-Long.
        77 Stat-Status  Usage Binary-Long Value 1.
        77 Filename     Pic x(80) Value 'Makefile'.
        77 Msg Pic x(100).
        01 Lk-statbuf.
        COPY statbuf.
        01 Lk-tm.
        COPY tm.
        01 Today.
           02 tm_year PIC 9999.
           02 tm_mon  PIC 99.
           02 tm_wday PIC 99.

        Procedure Division.
        Display 'calling posix-stat for ' Function Trim(Filename) ' ...'
        Move Function posix-stat(Filename, lk-statbuf) to Return-Value.
        Display 'posix-stat returned: ' Return-Value.
        If Return-Value < 0 then
          Display Function Trim(Filename) ': '
                  'errno ', Function posix-errno(Msg), ': ' Msg
          Goback.

        Display 'calling posix-localtime ...'
        Move Function posix-localtime(st_mtime, lk-tm) to Return-Value.
        Display 'posix-localtime returned: ' Return-Value.
        If Return-Value < 0 then
          Display 'posix-localtime: ', Function Trim(Filename) ': '
                  'errno ', Function posix-errno(Msg), ': ' Msg
                ' (st_mtime ' st_mtime ')'
          Goback.
        Move Corresponding Lk-tm to Today.
        Add 1900 to tm_year of Today.
        Display "'" Function trim(Filename) "'"
                ' (st_mtime ' st_mtime ') modified '
                tm_year of Today '-'
                tm_mon  of Today '-'
                tm_wday of Today.
        Goback.
