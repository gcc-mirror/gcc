! { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE

! Initialize before the checkpoint, in case this produces any output.
!$ACC PARALLEL
!$ACC END PARALLEL

      PRINT *, "CheCKpOInT"
!$ACC PARALLEL
      STOP 35
!$ACC END PARALLEL
      PRINT *, "WrONg WAy"

      END PROGRAM MAIN

! { dg-output "CheCKpOInT(\n|\r\n|\r)+" }
! { dg-output "STOP 35(\n|\r\n|\r)+" }
! PR85463.  The "minimal" libgfortran implementation used with nvptx
! offloading is a little bit different.
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }
! { dg-output "$" }
! { dg-shouldfail "" }
