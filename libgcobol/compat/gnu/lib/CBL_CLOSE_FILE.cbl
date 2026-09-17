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
        COPY posix-close.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_CLOSE_FILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. Posix
       >>IF DEBUGGING-MODE IS Defined
          With Debugging Mode
       >>END-IF
       .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FUNC-RETURN-VALUE		Binary-Long.
       77  errno-val            Binary-Long.

       LINKAGE SECTION.
       01  file-handle    	PIC X(4) COMP-5.

       PROCEDURE DIVISION USING
                   By Reference file-handle.

           MOVE FUNCTION posix-close(file-handle)
             TO FUNC-RETURN-VALUE.

           IF FUNC-RETURN-VALUE < 0
             Move Function COBRT-FILE-STATUS() to RETURN-CODE
           ELSE
             MOVE 0 TO RETURN-CODE
           END-IF.

      D     Display 'CBL_CLOSE_FILE fd: ' file-handle ', rc: ' RETCODE.
           END PROGRAM CBL_CLOSE_FILE.

       >> POP SOURCE FORMAT