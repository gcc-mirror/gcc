       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * MODIFY AFTER SUCCESSFUL TESTING / IMPLEMENTATION (VPH)
      *  This function is in the public domain.
      *  Contributed by James K. Lowden
      * 
      * CALL "CBL_ALLOC_MEM" using     mem-pointer
      *                      by value  mem-size
      *                      by value  flags
      *                      returning status-code
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_ALLOC_MEM. 

       DATA DIVISION.

       LINKAGE SECTION.
       01  MEMORY-REQUESTED		PIC X(8) COMP-5.
       01  MEMORY-ALLOCATED		USAGE IS POINTER.
       01  FLAGS                	PIC X(8) COMP-5.
       77  STATUS-CODE                  BINARY-LONG SIGNED VALUE 0.

       PROCEDURE DIVISION USING     MEMORY-ALLOCATED,
                          BY VALUE  MEMORY-REQUESTED, 
                          BY VALUE  FLAGS
                          RETURNING STATUS-CODE.

      D     Display 'MEMORY-REQUESTED: ' MEMORY-REQUESTED
      D            ' CHARACTERS INITIALIZED'

           ALLOCATE MEMORY-REQUESTED CHARACTERS INITIALIZED,
                    RETURNING MEMORY-ALLOCATED.

      D    IF MEMORY-ALLOCATED = NULLS THEN MOVE 1 TO STATUS-CODE.

           END PROGRAM CBL_ALLOC_MEM.

        >> POP SOURCE FORMAT