      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  This function is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in October 2025
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   
        COPY posix-errno.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. gcobol-smoke-test.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           GNU-Linux.
       OBJECT-COMPUTER.
           GNU-Linux.

       >>Define FILENAME as "/tmp/smoke.empty"

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXPENDABLE
                  ACCESS MODE IS SEQUENTIAL
                  SEQUENTIAL
                  ASSIGN TO FILENAME.

       DATA DIVISION.
       FILE SECTION.
      * FD not required per ISO but fails under gcobol. 
        FD EXPENDABLE.
          01 Extraneous PIC X.
        
       WORKING-STORAGE SECTION.
        77 File-Name   PIC X(100) VALUE FILENAME.
        77 status-code BINARY-LONG SIGNED.

      * CBL_ALLOC_MEM
        01 mem-pointer	usage pointer.
        77 mem-size	pic x(8) comp-5 VALUE 64.
        77 flags        pic x(8) comp-5 VALUE 0.

      * CBL_CHECK_FILE_EXIST
        01 file-info.
          03 file-modification-day.
            05 File-Size-In-Bytes  PIC 9(18)  COMP.
            05 Mod-DD              PIC 9(2)   COMP. *> Modification Date
            05 Mod-MO              PIC 9(2)   COMP.
            05 Mod-YYYY            PIC 9(4)   COMP.
          03 file-modification-time.
            05 Mod-HH              PIC 9(2)   COMP. *> Modification Time
            05 Mod-MM              PIC 9(2)   COMP.
            05 Mod-SS              PIC 9(2)   COMP.
            05 FILLER              PIC 9(2)   COMP. *> Always 00

       PROCEDURE DIVISION.

        Display 'Allocating ' mem-size ' bytes ... ' with No Advancing.
        
        Call "CBL_ALLOC_MEM" using
                               mem-pointer
                     by value  mem-size
                     by value  flags
                     returning status-code.

        Display 'CBL_ALLOC_MEM        status: ' status-code.
        
        Display 'Checking on  ' Function Trim(File-Name) ' ...                 '
                with No Advancing.

        Call "CBL_CHECK_FILE_EXIST"  using    File-Name
                                     file-info
                           returning status-code.

        Display 'CBL_CHECK_FILE_EXIST status: ' status-code.

        Display 'Deleting     ' Function Trim(File-Name) ' ...                 '
                with No Advancing.

        Call "CBL_DELETE_FILE" using File-Name
                     returning status-code.

        Display 'CBL_DELETE_FILE      status: ' status-code.

        Display 'Freeing ' mem-size ' bytes ...    ' with No Advancing.

        Call "CBL_FREE_MEM" using by value mem-pointer
                    returning      status-code.

        Display 'CBL_FREE_MEM         status: ' status-code.

        >>IF CBL_READ_FILE is defined
        Call "CBL_READ_FILE" 
         using handle, offset, count, flags, buf
         returning status-code.
        >>END-IF

