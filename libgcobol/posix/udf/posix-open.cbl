       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
      * int open(const char *pathname, int flags);
        Identification Division.
        Function-ID. posix-open.

        Environment Division.
        Configuration Section.
          Source-Computer. Sesame-Cookie
        >>IF DEBUGGING-MODE is Defined
          With Debugging Mode
        >>END-IF
          .
        
        Data Division.
        Working-Storage Section.
          77 Ws-pathname PIC X(8192).
          77 Ws-mode-ptr Pointer.
          77 Ws-mode     PIC 9(8) Value 0.
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-pathname PIC X ANY LENGTH.
          01 Lk-flags    PIC 9(8).
          01 Lk-mode     PIC 9(8).

        Procedure Division using
             By Reference Lk-pathname,
             By Reference Lk-flags,
             By Reference Optional Lk-mode 
             Returning Return-Value.

          Move Lk-pathname To Ws-pathname.
          Inspect Ws-pathname 
                  Replacing Trailing Space By Low-Value

      D   Display 'posix-open: Ws-pathname ', Ws-pathname.
      D   Perform Show-Flags.

          Set ws-mode-ptr to Address Of Lk-mode.

          If ws-mode-ptr > 0 Then *> O_CREAT requires mode
            Move Lk-mode to Ws-mode.
            
          Call "posix_open" using Ws-pathname, Lk-flags, Ws-mode, 
                              Returning Return-Value.
          Goback.

          Show-Flags Section.

            Display 'Flags: ', Function Hex-Of(Lk-flags).

        End Function posix-open.
        >> POP SOURCE FORMAT
