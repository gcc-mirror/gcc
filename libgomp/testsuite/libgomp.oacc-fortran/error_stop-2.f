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
!
! In gfortran's main program, libfortran's set_options is called - which sets
! compiler_options.backtrace = 1 by default.  For an offload libgfortran, this
! is never called and, hence, "Error termination." is never printed.  Thus:
! { dg-output "Error termination.*" { target { ! { openacc_nvidia_accel_selected || openacc_amdgcn_accel_selected } } } }
!
! PR85463:
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }
!
! { dg-shouldfail "" }
