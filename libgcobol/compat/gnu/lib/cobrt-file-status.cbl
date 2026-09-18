        >> PUSH source format
        >>SOURCE format is fixed
        IDENTIFICATION DIVISION.
        FUNCTION-ID. COBRT-FILE-STATUS.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 FsErrno CONSTANT 1000000.
        LINKAGE SECTION.
        01 ERRNO BINARY-LONG.
        01 FILE-STATUS PIC X(2) COMP-5.
        01 REDEFINES FILE-STATUS.
          03 MSB PIC X.
          03 LSB BINARY-CHAR.

        PROCEDURE DIVISION
            RETURNING FILE-STATUS.
          CALL "__compat_file_status_word" USING
              by Value FsErrno, FILE-STATUS
              Returning FILE-STATUS.
          END FUNCTION COBRT-FILE-STATUS.
        >> POP source format
