! { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE

! Initialize before the checkpoint, in case this produces any output.
!$ACC PARALLEL
!$ACC END PARALLEL

      PRINT *, "CheCKpOInT"
!$ACC PARALLEL
      STOP "SiGN"
!$ACC END PARALLEL
      PRINT *, "WrONg WAy"

      END PROGRAM MAIN

! { dg-output "CheCKpOInT(\n|\r\n|\r)+" }

! { dg-output "STOP SiGN(\n|\r\n|\r)+" }
!
! PR85463.  The 'exit' implementation used with nvptx
! offloading is a little bit different.
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }

! { dg-output "$" }

! PR85463.  STOP with code zero (as implied here) should actually
! terminate the process normally, but doesn't with the 'exit'
! implementation used with nvptx offloading.
! { dg-shouldfail PR85463 { openacc_nvidia_accel_selected } }
