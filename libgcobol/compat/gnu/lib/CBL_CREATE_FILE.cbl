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
        COPY posix-open.
        COPY psx-open.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_CREATE_FILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. Posix
       >>IF DEBUGGING-MODE IS Defined
          With Debugging Mode
       >>END-IF
       .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  func-ret             Binary-Long.
       77  errno-val            Binary-Long.
       77  lk-mode              PIC 9(8) COMP-5.
       77  filename-len         PIC 9(4) BINARY VALUE ZERO.
       01  ws-access-mode       PIC 9(8) COMP-5.

       LINKAGE SECTION.
       77  RETCODE              PIC X(2) COMP-5.
       01  filename  	          PIC X ANY LENGTH.
       01  access-mode          PIC x COMP-x.
       01  deny-mode            PIC x comp-x.  *>  Not supported (must be 0).
       01  device               PIC x comp-x.  *>  Not supported (must be 0).
       01  file-handle          PIC X(4) COMP-5.

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

           COMPUTE filename-len =
                FUNCTION LENGTH(FUNCTION TRIM(filename)).
           MOVE X"00" TO filename(filename-len + 1:1).
      D     Display 'CBL_CREATE_FILE: filename: [' filename ']'
      D     Display               'ws-access-mode: ' ws-access-mode ', '
      D     Display                 'deny-mode: ' deny-mode.
           EVALUATE ws-access-mode
             WHEN 1  *> Read only
                 Move O_RDONLY to ws-access-mode
             WHEN 2  *> Write only (deny-mode must be 0)
                 Move O_WRONLY to ws-access-mode
             WHEN 3  *> Read/write
                 Move O_RDWR to ws-access-mode
             WHEN OTHER
                 Display 'CBL_CREATE_FILE invalid mode: ' ws-access-mode
                 Move -1 to RETCODE
                 GOBACK
            END-EVALUATE.

      *    TODO: Validate these settings:
           Compute ws-access-mode = ws-access-mode + O_CREAT + O_TRUNC.
           Compute Lk-mode = S_IRUSR + S_IWUSR + S_IRGRP + S_IWGRP.

           MOVE FUNCTION posix-open(filename, ws-access-mode, lk-mode)
             TO func-ret.

           If func-ret is < 0
           Then
               Move Function COBRT-FILE-STATUS() to RETCODE
      D        Display 'COBRT-FILE-STATUS returned: ' RETCODE
           else
               Move func-ret to file-handle
               Move 0 to RETCODE
           end-if.

           END PROGRAM CBL_CREATE_FILE.

        >> POP SOURCE FORMAT
