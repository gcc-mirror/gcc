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

        COPY posix-read.
        COPY posix-lseek.
        COPY posix-fstat.
        COPY psx-lseek.
        COPY "cblproto.cpy".

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_READ_FILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. Posix
       >>IF DEBUGGING-MODE IS Defined
          With Debugging Mode
       >>END-IF
       .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FUNC-RETURN-VALUE		PIC S9(8) COMP-5.
       77  remaining-bytes      Binary-Long.
       77  bytes-read           Binary-Long.
       77  Lk-whence            PIC S9(9) USAGE COMP-5 VALUE 0.
       77  errno-val            Binary-Long.
       01  statbuf.
        COPY statbuf.

       LINKAGE SECTION.
       01  file-handle    	PIC X(4) COMP-5.
       01  file-offset    	PIC X(8) COMP-5.
       01  byte-count     	pic x(4) comp-x.
       01  flags            PIC X COMP-X.
       01  buffer           PIC X ANY LENGTH.

       PROCEDURE DIVISION USING
                   By Reference file-handle,
                   By Reference file-offset,
                   By Reference byte-count,
                   By Reference flags,
                   By Reference buffer.
        MAIN SECTION.

           IF flags = 0
           THEN
              Move SEEK_SET to Lk-whence
              MOVE FUNCTION posix-lseek(file-handle,
                                        file-offset,
                                        Lk-whence)
                TO FUNC-RETURN-VALUE

              If FUNC-RETURN-VALUE >= 0
              Then
                Perform ATTEMPT-READ
              Else
                PERFORM RETURN-ERROR
                GOBACK
              End-If

           ELSE IF flags = 128
           THEN
            MOVE FUNCTION posix-fstat(file-handle, statbuf)
                              TO FUNC-RETURN-VALUE

            IF FUNC-RETURN-VALUE = 0
            THEN
              MOVE st_size OF statbuf TO file-offset
              MOVE 0 TO RETURN-CODE
            ELSE
              PERFORM RETURN-ERROR
              GOBACK
            END-IF
           ELSE
              Display 'Error Invalid value for flags!'
           END-IF.

      D     Display 'CBL_READ_FILE flags: ' flags ', fd: ' file-handle ', byte-count: ' byte-count ', file-offset: ' file-offset ', rc: ' RETCODE.
           GOBACK.

       ATTEMPT-READ SECTION.
           MOVE byte-count TO remaining-bytes.
           MOVE 0 TO bytes-read.

           PERFORM UNTIL bytes-read >= byte-count
             MOVE FUNCTION posix-read(file-handle,
               buffer (bytes-read + 1 : remaining-bytes),
               remaining-bytes) TO FUNC-RETURN-VALUE

             IF FUNC-RETURN-VALUE < 0
               PERFORM RETURN-ERROR
               GOBACK
             ELSE
               SUBTRACT FUNC-RETURN-VALUE FROM remaining-bytes
               ADD FUNC-RETURN-VALUE TO bytes-read
             END-IF
           END-PERFORM.

           MOVE 0 TO RETURN-CODE.
           EXIT.

       RETURN-ERROR SECTION.
           Move Function COBRT-FILE-STATUS() to RETURN-CODE.
           EXIT PARAGRAPH.

           END PROGRAM CBL_READ_FILE.

       >> POP SOURCE FORMAT
