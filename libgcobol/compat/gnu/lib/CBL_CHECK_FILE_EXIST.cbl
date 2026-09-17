       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
      * Copyright (c) 2021-2026 Symas Corporation
      *
      * Redistribution and use in source and binary forms, with or without
      * modification, are permitted provided that the following conditions are
      * met:
      *
      * * Redistributions of source code must retain the above copyright
      *   notice, this list of conditions and the following disclaimer.
      * * Redistributions in binary form must reproduce the above
      *   copyright notice, this list of conditions and the following disclaimer
      *   in the documentation and/or other materials provided with the
      *   distribution.
      * * Neither the name of the Symas Corporation nor the names of its
      *   contributors may be used to endorse or promote products derived from
      *   this software without specific prior written permission.
      *
      * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
      * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
      * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
      * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
      * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
      * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
      * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
      * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
      * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
      * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
      * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

        COPY "cblproto.cpy".
      * Include the posix-stat function
        COPY posix-stat.
      * Include the posix-localtime function
        COPY posix-localtime.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_CHECK_FILE_EXIST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FUNC-RETURN-VALUE		PIC 9(8) COMP-5.
       01  STAT-BUFFER.
           COPY statbuf.
       01  TM-BUFFER.
           COPY tm.
       01  ERRNO-VAL BINARY-LONG.
       LINKAGE SECTION.
       77  RETCODE			PIC X(2) COMP-5.
       01  FILE-PATH 			PIC X ANY LENGTH.
      * see libgcobol/compat/gnu/cpy/cbltypes.cpy
      * and libgcobol/posix/udf/posix-localtime.cbl
       COPY cbltypes.
       01 FI-FILE-INFO TYPE CBLT-FILEEXIST-BUF.

       PROCEDURE DIVISION USING FILE-PATH, FI-FILE-INFO,
                          RETURNING RETCODE.
       MAIN SECTION.
           MOVE FUNCTION posix-stat(FILE-PATH, STAT-BUFFER)
             TO FUNC-RETURN-VALUE.

           IF FUNC-RETURN-VALUE <> ZERO
            PERFORM RETURN-ERROR
            GOBACK
           END-IF.

           MOVE st_size TO cblte-fe-filesize.

           MOVE FUNCTION posix-localtime(address of st_mtime, TM-BUFFER)
            TO FUNC-RETURN-VALUE.

           IF FUNC-RETURN-VALUE <> ZERO
            PERFORM RETURN-ERROR
            GOBACK
           END-IF.

           ADD 1900 TO tm_year.
           MOVE tm_year TO cblte-fe-year.
           MOVE tm_mon TO cblte-fe-month.
           ADD 1 TO cblte-fe-month.
           MOVE tm_mday TO cblte-fe-day.

           MOVE tm_hour TO cblte-fe-hours.
           MOVE tm_min TO cblte-fe-minutes.
           MOVE tm_sec TO cblte-fe-seconds.
      *> localtime(3) operates on time_t, so no sub-second precision.
           MOVE 0 TO cblte-fe-hundreths.
           MOVE 0 TO RETCODE.
           GOBACK.

       RETURN-ERROR SECTION.
           Move Function COBRT-FILE-STATUS() to RETCODE.
           EXIT PARAGRAPH.

       END PROGRAM CBL_CHECK_FILE_EXIST.
        >> POP SOURCE FORMAT
`
