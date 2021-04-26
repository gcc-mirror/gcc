!     Test of reduction on parallel directive (with async).
!     Variant of "../libgomp.oacc-c-c++-common/par-reduction-2.c".
!     Variant using the "openacc" module.

!     { dg-do run }

!     { dg-additional-options "-Wopenacc-parallelism" } for
!     testing/documenting aspects of that functionality.

      PROGRAM MAIN
      USE OPENACC
      IMPLICIT NONE

      INTEGER RES, RES1, RES2

      RES1 = 0
      RES2 = 0

!$ACC PARALLEL NUM_GANGS(256) NUM_WORKERS(32) VECTOR_LENGTH(32)
!$ACC& REDUCTION(+:RES1) COPY(RES1, RES2) ASYNC(1)
!     { dg-bogus "\[Ww\]arning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction', 'atomic'" { xfail *-*-* } .-1 }
!     { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-2 }
!     { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-3 }
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
!     { dg-bogus "\[Ww\]arning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction', 'atomic'" { xfail *-*-* } .-1 }
!     { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-2 }
!     { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-3 }
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
