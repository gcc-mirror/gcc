!     ACC_PRESENT_OR_CREATE, ACC_PRESENT_OR_COPYIN, etc.
!     Variant of "../libgomp.oacc-c-c++-common/lib-32.c".
!     Variant using the "openacc" module.

!     { dg-do run }

      PROGRAM MAIN
      USE OPENACC
      IMPLICIT NONE

      INTEGER, PARAMETER :: N = 10000
      INTEGER, ALLOCATABLE :: H(:)
      INTEGER :: I
      LOGICAL :: SHARED_MEM

      ALLOCATE (H(N))
      DO I = 1, N
         H(I) = I + 0
      END DO

      SHARED_MEM = ACC_IS_PRESENT (H)

      CALL ACC_PRESENT_OR_CREATE (H, INT (SIZEOF (H), 4))
      IF (.NOT. ACC_IS_PRESENT (H, INT (SIZEOF (H), 8))) STOP 1

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         H(I) = I + 1
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (1, 0, SHARED_MEM)) STOP 2
         H(I) = I + 2
      END DO

      CALL ACC_PRESENT_OR_CREATE (H)

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (2, 1, SHARED_MEM)) STOP 3
         H(I) = I + 3
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (3, 2, SHARED_MEM)) STOP 4
         H(I) = I + 4
      END DO

      CALL ACC_PCREATE (H, INT (SIZEOF (H), 4))

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (4, 3, SHARED_MEM)) STOP 5
         H(I) = I + 5
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (5, 4, SHARED_MEM)) STOP 6
         H(I) = I + 6
      END DO

      CALL ACC_PRESENT_OR_COPYIN (H, INT (SIZEOF (H), 8))

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (6, 5, SHARED_MEM)) STOP 7
         H(I) = I + 7
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (7, 6, SHARED_MEM)) STOP 8
         H(I) = I + 8
      END DO

      CALL ACC_PCOPYIN (H, INT (SIZEOF (H), 4))

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (8, 7, SHARED_MEM)) STOP 9
         H(I) = I + 9
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (9, 8, SHARED_MEM)) STOP 10
         H(I) = I + 10
      END DO

      CALL ACC_COPYOUT_FINALIZE (H, INT (SIZEOF (H), 4))
      IF (.NOT. SHARED_MEM) THEN
         IF (ACC_IS_PRESENT (H, INT (SIZEOF (H), 8))) STOP 11
      ENDIF

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (10, 9, SHARED_MEM)) STOP 12
      END DO

      CALL ACC_PCOPYIN (H)
      IF (.NOT. ACC_IS_PRESENT (H, INT (SIZEOF (H), 4))) STOP 13

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (10, 9, SHARED_MEM)) STOP 14
         H(I) = I + 11
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (11, 9, SHARED_MEM)) STOP 15
         H(I) = I + 12
      END DO

      CALL ACC_PCOPYIN (H, INT (SIZEOF (H), 8))

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (12, 11, SHARED_MEM)) STOP 16
         H(I) = I + 13
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (13, 12, SHARED_MEM)) STOP 17
         H(I) = I + 14
      END DO

      CALL ACC_PCREATE (H)

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (14, 13, SHARED_MEM)) STOP 18
         H(I) = I + 15
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (15, 14, SHARED_MEM)) STOP 19
         H(I) = I + 16
      END DO

      CALL ACC_PCREATE (H, INT (SIZEOF (H), 8))

!$ACC PARALLEL LOOP DEFAULT (PRESENT)
      DO I = 1, N
         IF (H(I) .NE. I + MERGE (16, 15, SHARED_MEM)) STOP 20
         H(I) = I + 17
      END DO
!$ACC END PARALLEL LOOP

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (17, 16, SHARED_MEM)) STOP 21
         H(I) = I + 18
      END DO

      CALL ACC_UPDATE_SELF (H, INT (SIZEOF (H), 4))
      IF (.NOT. ACC_IS_PRESENT (H, INT (SIZEOF (H), 8))) STOP 22

      DO I = 1, N
         IF (H(I) .NE. I + MERGE (18, 17, SHARED_MEM)) STOP 23
      END DO

      CALL ACC_DELETE_FINALIZE (H)
      IF (.NOT. SHARED_MEM) THEN
         IF (ACC_IS_PRESENT (H, INT (SIZEOF (H), 4))) STOP 24
      ENDIF

      DEALLOCATE (H)

      END PROGRAM MAIN
