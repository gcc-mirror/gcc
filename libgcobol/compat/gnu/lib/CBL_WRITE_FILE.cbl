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

       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
        COPY "cblproto.cpy".
        COPY posix-write.
        COPY posix-lseek.
        COPY psx-lseek.
        COPY posix-ftruncate.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_WRITE_FILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. Posix
       >>IF DEBUGGING-MODE IS Defined
          With Debugging Mode
       >>END-IF
       .

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  null-byte            PIC X(1) VALUE X'00'.
       77  file-size        		Binary-Long.
       77 Lk-whence             PIC S9(9) USAGE COMP-5 VALUE 0.
       77 func-return           Binary-Long.
       77 errno-val             Binary-Long.
       77 remaining-bytes       Binary-Long.
       77 bytes-written         Binary-Long.

       LINKAGE SECTION.
       01  file-handle    	PIC X(4) COMP-5.
       01  file-offset    	PIC X(8) COMP-x.
       01  byte-count     	pic x(4) comp-x.
       01  flags            pic x comp-x.
       01  buffer           PIC X ANY LENGTH.

       PROCEDURE DIVISION USING
                   By Reference file-handle,
                   By Reference file-offset,
                   By Reference byte-count,
                   By Reference flags,
                   By Reference buffer.
        MAIN SECTION.
           *> special processing to truncate or extend the file
           If byte-count = 0
            PERFORM ATTEMPT-TRUNCATE-EXTEND
           Else
            PERFORM ATTEMPT-WRITE
           End-If.

           GOBACK.

        ATTEMPT-TRUNCATE-EXTEND SECTION.
          MOVE SEEK_END to Lk-whence.
          MOVE FUNCTION posix-lseek(file-handle,
                                    0,
                                    Lk-whence)
                                    TO file-size.

          If file-size < 0
            Perform RETURN-ERROR
            Goback
          End-If.

          If file-size > file-offset  *> truncate the file
            MOVE FUNCTION posix-ftruncate(file-handle,
                                          file-offset)
                                          TO func-return

            If func-return < 0
              Perform RETURN-ERROR
              Goback
            End-If
          Else If file-size < file-offset *> extend the file
            Move SEEK_SET to Lk-whence
            MOVE FUNCTION posix-lseek(file-handle,
                                      file-offset,
                                      Lk-whence)
                                      TO func-return

            If func-return < 0
              Perform RETURN-ERROR
              Goback
            End-If

            MOVE 1 to byte-count
            Set Address Of buffer To Address Of null-byte
            Perform ATTEMPT-WRITE
          End-If

          Exit Paragraph.

        ATTEMPT-WRITE SECTION.
          *> posix-write might return byte-count or smaller.
          *> Since CBL_WRITE_FILE must not return on partial writes,
          *> it must call posix-write multiple times if a partial
          *> write occurs.
          MOVE byte-count TO remaining-bytes.
          MOVE 0 TO bytes-written.

          PERFORM UNTIL bytes-written >= byte-count
            MOVE FUNCTION posix-write(file-handle,
              buffer (bytes-written + 1 : remaining-bytes),
              remaining-bytes) TO RETURN-CODE

            IF RETURN-CODE < 0
              PERFORM RETURN-ERROR
              GOBACK
            ELSE
              SUBTRACT RETURN-CODE FROM remaining-bytes
              ADD RETURN-CODE TO bytes-written
            END-IF
          END-PERFORM.

          MOVE 0 TO RETURN-CODE.
          EXIT PARAGRAPH.

       RETURN-ERROR SECTION.
          Move Function COBRT-FILE-STATUS() to RETURN-CODE.
          EXIT PARAGRAPH.

       >> POP SOURCE FORMAT
