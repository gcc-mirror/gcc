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
      * Include the posix_open function
        COPY posix-open.
        COPY psx-open.
        COPY cblproto.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_OPEN_FILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. Posix
       >>IF DEBUGGING-MODE IS Defined
          With Debugging Mode
       >>END-IF
       .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  errno-val            Binary-Long.
       01  ws-access-mode PIC 9(8) comp-5.
       LINKAGE SECTION.
       01  RETCODE     PIC X(2) COMP-5 VALUE 0.
       01  REDEFINES RETCODE.
        03 MSB PIC X.
        03 LSB BINARY-CHAR.
       01  filename 	 PIC X ANY LENGTH.
       01  access-mode PIC X COMP-X.
       01  deny-mode   PIC X COMP-X.  *>  Not supported (must be 0).
       01  device      PIC X COMP-X.  *>  Not supported (must be 0).
       01  file-handle PIC X(4) COMP-5.

       PROCEDURE DIVISION USING filename,
                       By Reference access-mode,
                       By Reference deny-mode,
                       By Reference device,
                       By Reference file-handle
                RETURNING RETCODE.

           MOVE access-mode TO ws-access-mode.

           IF ws-access-mode >= 64
               SUBTRACT 64 FROM ws-access-mode *> Remove large file bit if set
           END-IF.

      D     Display 'CBL_OPEN_FILE: access-mode: ' access-mode ', '
      D     Display                  'deny-mode: ' deny-mode.
           EVALUATE ws-access-mode
             WHEN 1  *> Read only
                 Move O_RDONLY to ws-access-mode
             WHEN 2  *> Write only (deny-mode must be 0)
                 Move O_WRONLY to ws-access-mode
             WHEN 3  *> Read/write
                 Move O_RDWR to ws-access-mode
             WHEN OTHER
                 MOVE "9" TO MSB
                 *> COBRT022 Illegal or impossible access mode for OPEN
                 MOVE 22 TO LSB
                 GOBACK
            END-EVALUATE.

           MOVE FUNCTION posix-open(filename, ws-access-mode, deny-mode)
               TO errno-val.
      D     Display 'CBL_OPEN_FILE: RETCODE: ' RETCODE.
           If errno-val is < 0
           then
               Move Function COBRT-FILE-STATUS() to RETCODE
      D        Display 'COBRT-FILE-STATUS returned: ' RETCODE
           else
               Move errno-val to file-handle
               Move 0 to RETCODE
           end-if.

           END PROGRAM CBL_OPEN_FILE.

        >> POP SOURCE FORMAT
