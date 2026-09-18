       >>PUSH source format
       >>SOURCE format is fixed
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

        IDENTIFICATION DIVISION.
        PROGRAM-ID. CBL_GET_PROGRAM_INFO.
        DATA DIVISION.
        LINKAGE SECTION.
        COPY "cbltypes.cpy".
        01 fn pic x(4) comp-5.
        01 param-block type cblt-prog-info-params.
        01 return-buf pic x any length.
        01 return-buf-len pic x(4) comp-5.
        77 retcode pic x(2) comp-5.
        procedure division using by value fn
                                 by reference param-block
                                 by reference return-buf
                                 by reference return-buf-len
                                 returning retcode.

        call "cbl_gpi" using by value fn
                             by reference param-block
                                          return-buf
                                          return-buf-len
                             returning retcode.

        END PROGRAM CBL_GET_PROGRAM_INFO.

        >>POP source format