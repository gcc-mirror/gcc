       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
      * int  stat(const char *  pathname,  struct stat *  statbuf)
        Identification Division.
        Function-ID. posix-stat.

        Environment Division.
        Configuration Section.
          Source-Computer. Alpha-Romeo
        >>IF DEBUGGING-MODE is Defined
          With Debugging Mode
        >>END-IF
          .
        
        Data Division.
        Working-Storage Section.
          77 bufsize Usage Binary-Long.
          77 Ws-pathname PIC X(8192).
        Linkage Section.
          77 Return-Value Binary-Long.
          01 Lk-pathname PIC X ANY LENGTH.
          01 Lk-statbuf.
          COPY statbuf.
          
        Procedure Division using
             By Reference Lk-pathname,
             By Reference Lk-statbuf, 
             Returning Return-Value.

          Move Lk-pathname To Ws-pathname.
          Inspect Ws-pathname 
                  Replacing Trailing Space By Low-Value

          Move Function Byte-Length(Lk-statbuf) to bufsize.

      D   Display 'posix-stat: Ws-pathname ', Ws-pathname.
      D   Display 'posix-stat: Lk-statbuf has ', bufsize ' bytes'.

          Call "posix_stat" using Ws-pathname, Lk-statbuf
                     By Value     bufsize
                        Returning Return-Value.
      D      Perform Show-Statbuf.
          Goback.

          Show-Statbuf Section.

            Display 'st_dev:     '  st_dev.
            Display 'st_ino:     '  st_ino.
            Display 'st_mode:    '  st_mode.
            Display 'st_nlink:   '  st_nlink.
            Display 'st_uid:     '  st_uid.
            Display 'st_gid:     '  st_gid.
            Display 'st_rdev:    '  st_rdev.
            Display 'st_size:    '  st_size.
            Display 'st_blksize: '  st_blksize.
            Display 'st_blocks:  '  st_blocks.
            Display 'st_atime:   '  st_atime.
            Display 'st_mtime:   '  st_mtime.
            Display 'st_ctime:   '  st_ctime.

        End Function posix-stat.
        >> POP SOURCE FORMAT
