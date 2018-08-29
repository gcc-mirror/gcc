! { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE

      PRINT *, "CheCKpOInT"
!$ACC PARALLEL
      ERROR STOP
!$ACC END PARALLEL
      PRINT *, "WrONg WAy"

      END PROGRAM MAIN

! { dg-output "CheCKpOInT(\n|\r\n|\r)+" }
! { dg-output "ERROR STOP (\n|\r\n|\r)+" }
! PR85463.  The "minimal" libgfortran implementation used with nvptx
! offloading is a little bit different.
! { dg-output "Error termination.*" { target { ! openacc_nvidia_accel_selected } } }
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }
! { dg-shouldfail "" }
