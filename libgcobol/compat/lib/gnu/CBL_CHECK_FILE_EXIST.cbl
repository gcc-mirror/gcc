       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
      * Include the posix-stat function
        COPY posix-stat.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * MODIFY AFTER SUCCESSFUL TESTING / IMPLEMENTATION (VPH)
      *  This function is in the public domain.
      *  Contributed by James K. Lowden of Cobolworx in August 2024
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_CHECK_FILE_EXIST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FUNC-RETURN-VALUE		PIC 9(8) COMP-5.
       01  STAT-BUFFER.
           COPY statbuf.
       LINKAGE SECTION.
       77  RETURN-CODE			PIC 9(8) COMP-5.
       01  FILE-PATH 			PIC X ANY LENGTH.
       01  FI-FILE-INFO.
           05  FI-FILE-SIZE-IN-BYTES	PIC 9(8) COMP-4.
           05  FI-FILE-MOD-DATE-TIME.
               10  FI-FILE-DATE		PIC 9(8) COMP-4.
               10  FI-FILE-TIME		PIC 9(8) COMP-4.

       PROCEDURE DIVISION USING FILE-PATH, FI-FILE-INFO,
                          RETURNING RETURN-CODE.
           MOVE FUNCTION posix-stat(FILE-PATH, STAT-BUFFER)
             TO FUNC-RETURN-VALUE.
             
           IF FUNC-RETURN-VALUE = ZERO
           THEN
               MOVE ZERO TO RETURN-CODE
               MOVE st_size  TO FI-FILE-SIZE-IN-BYTES
               MOVE st_mtime TO FI-FILE-MOD-DATE-TIME
           ELSE
               MOVE 1 TO RETURN-CODE
               MOVE ZERO TO FI-FILE-SIZE-IN-BYTES
               MOVE ZERO TO FI-FILE-DATE
               MOVE ZERO TO FI-FILE-TIME.

           END PROGRAM CBL_CHECK_FILE_EXIST.

        >> POP SOURCE FORMAT
`