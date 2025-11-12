       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
      * Include the posix-unlink function
        COPY posix-unlink.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * MODIFY AFTER SUCCESSFUL TESTING / IMPLEMENTATION (VPH)
      *  This function is in the public domain.
      *  Contributed by 
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_DELETE_FILE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
          77 BUFSIZE USAGE BINARY-LONG.
       LINKAGE SECTION.
       77  RETURN-CODE			PIC 9(8) COMP-5.
       01  FILE-PATH			PIC X ANY LENGTH.

       PROCEDURE DIVISION USING FILE-PATH, RETURNING RETURN-CODE.

           INSPECT FILE-PATH 
                   REPLACING TRAILING SPACE BY LOW-VALUE

           MOVE FUNCTION posix-unlink(FILE-PATH) TO RETURN-CODE.

           END PROGRAM CBL_DELETE_FILE.

        >> POP SOURCE FORMAT