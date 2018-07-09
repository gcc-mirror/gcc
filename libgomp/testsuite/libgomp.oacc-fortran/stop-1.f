! { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE

      PRINT *, "CheCKpOInT"
!$ACC PARALLEL
      STOP
!$ACC END PARALLEL
      PRINT *, "WrONg WAy"

      END PROGRAM MAIN

! { dg-output "CheCKpOInT(\n|\r\n|\r)+" }
! PR85463.  The "minimal" libgfortran implementation used with nvptx
! offloading is a little bit different.
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }
! { dg-output "$" }
! PR85463.  STOP with code zero (as implied here) should actually
! terminate the process normally, but doesn't in the "minimal"
! libgfortran implementation used with nvptx offloading.
! { dg-shouldfail "" { openacc_nvidia_accel_selected } }
