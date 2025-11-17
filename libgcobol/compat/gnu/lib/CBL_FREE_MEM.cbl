       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * MODIFY AFTER SUCCESSFUL TESTING / IMPLEMENTATION (VPH)
      *  This function is in the public domain.
      *  Contributed by 
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_FREE_MEM.

       DATA DIVISION.
       LINKAGE SECTION.
       77  RETURN-CODE			PIC 9(8) COMP.
       01  MEMORY-ADDRESS		USAGE IS POINTER.

       PROCEDURE DIVISION USING MEMORY-ADDRESS, 
                      RETURNING RETURN-CODE.

           FREE MEMORY-ADDRESS.
           MOVE ZERO TO RETURN-CODE.

           END PROGRAM CBL_FREE_MEM.

        >> POP SOURCE FORMAT