!     Test of reduction on parallel directive (with async).
!     Variant of "../libgomp.oacc-c-c++-common/par-reduction-2.c".
!     Variant using "openacc_lib.h".

!     { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER RES, RES1, RES2

      RES1 = 0
      RES2 = 0

!$ACC PARALLEL NUM_GANGS(256) NUM_WORKERS(32) VECTOR_LENGTH(32)
!$ACC& REDUCTION(+:RES1) COPY(RES1, RES2) ASYNC(1)
      res1 = res1 + 5

!$ACC ATOMIC
      res2 = res2 + 5
!$ACC END PARALLEL

      IF (ACC_GET_DEVICE_TYPE () .EQ. ACC_DEVICE_HOST) THEN
         RES = 1 * 5
      ELSE
         RES = 256 * 5
      END IF

      CALL ACC_ASYNC_WAIT (1)

      IF (RES .NE. RES1) STOP 1
      IF (RES .NE. RES2) STOP 2

      RES1 = 1
      RES2 = 1

!$ACC PARALLEL NUM_GANGS(8) NUM_WORKERS(32) VECTOR_LENGTH(32)
!$ACC& REDUCTION(*:RES1) COPY(RES1, RES2) ASYNC(1)
      res1 = res1 * 5

!$ACC ATOMIC
      res2 = res2 * 5
!$ACC END PARALLEL

      IF (ACC_GET_DEVICE_TYPE () .EQ. ACC_DEVICE_HOST) THEN
         RES = 5 ** 1
      ELSE
         RES = 5 ** 8
      END IF

      CALL ACC_ASYNC_WAIT_ALL

      IF (RES .NE. RES1) STOP 3
      IF (RES .NE. RES2) STOP 4

      END PROGRAM
