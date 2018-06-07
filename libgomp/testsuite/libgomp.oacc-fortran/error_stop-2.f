! { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE

      PRINT *, "CheCKpOInT"
!$ACC PARALLEL
      ERROR STOP 35
!$ACC END PARALLEL
      PRINT *, "WrONg WAy"

      END PROGRAM MAIN

! { dg-output "CheCKpOInT(\n|\r\n|\r)+" }
! { dg-output "ERROR STOP 35(\n|\r\n|\r)+" }
! PR85463.  The "minimal" libgfortran implementation used with nvptx
! offloading is a little bit different.
! { dg-output "Error termination.*" { target { ! openacc_nvidia_accel_selected } } }
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }
! { dg-shouldfail "" }
