        >> PUSH source format
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

        COPY posix-clock-gettime.
        COPY posix-errno.

        Identification Division.
        Program-ID. test-gettime.
        Data Division.
        Working-Storage Section.
        77 ClockType Binary-Long Value 0.
        01 Lk-timespec.
            05 tv_sec     Usage is Binary-Double  Unsigned.
            05 tv_nsec    Usage is Binary-Double  Unsigned.
        77 Exit-Status Binary-Long Value 1.
        77 error-msg PIC X(100).

        Procedure Division.
        Display 'calling posix-clock-gettime ...'

        MOVE FUNCTION posix-clock-gettime(ClockType, Lk-timespec)
            TO Exit-Status.
        If Exit-Status < 0 then
          Display 'errno ', Function posix-errno(error-msg),
          ': ' error-msg
          Goback
        End-If.
        Display tv_sec IN Lk-timespec.
        Display tv_nsec IN Lk-timespec.
        Display 'finished successfully ...'.
        End Program test-gettime.
        >> POP SOURCE FORMAT
