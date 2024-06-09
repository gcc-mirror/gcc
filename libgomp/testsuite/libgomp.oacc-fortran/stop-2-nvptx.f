! 'stop-2.f' nvptx offloading variant

! { dg-do run { target openacc_nvidia_accel_selected } }
!
! The PTX JIT doesn't understand the 'STOP' call graph, and therefore:
!     warning : Stack size for entry function 'main$_omp_fn$0' cannot be statically determined
! Running with default 1024-bytes GPU thread stack size overflows the stack,
! so raise it to an arbitrarily higher value:
! { dg-set-target-env-var GOMP_NVPTX_NATIVE_GPU_THREAD_STACK_SIZE 3333 }

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
!
! PR85463.  The 'exit' implementation used with nvptx
! offloading is a little bit different.
! { dg-output "libgomp: cuStreamSynchronize error.*" { target openacc_nvidia_accel_selected } }

! { dg-output "$" }

! { dg-shouldfail "" }
