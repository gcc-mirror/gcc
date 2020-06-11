! { dg-do run }

      PROGRAM MAIN
      IMPLICIT NONE

! Initialize before the checkpoint, in case this produces any output.
!$ACC PARALLEL
!$ACC END PARALLEL

      PRINT *, "CheCKpOInT"
!$ACC PARALLEL
      ERROR STOP "SiGN"
!$ACC END PARALLEL
      PRINT *, "WrONg WAy"

      END PROGRAM MAIN

! { dg-output "CheCKpOInT(\n|\r\n|\r)+" }
! { dg-output "ERROR STOP SiGN(\n|\r\n|\r)+" }
!
! In gfortran's main program, libfortran's set_options is called - which sets
! compiler_options.backtrace = 1 by default.  For an offload libgfortran, this
! is never called and, hence, "Error termination." is never printed.  Thus:
! { dg-output "Error termination.*" { target { ! { openacc_nvidia_accel_selected || openacc_radeon_accel_selected } } } }
!
! PR85463:
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }
!
! { dg-shouldfail "" }
