! Ensure that write on the offload device works, nvptx offloading variant.

! { dg-do run { target openacc_nvidia_accel_selected } }
! { dg-output "The answer is 42(\n|\r\n|\r)+" }
!
! The PTX JIT doesn't understand the 'write' call graph, and therefore:
!     warning : Stack size for entry function 'main$_omp_fn$0' cannot be statically determined
! Running with default 1024-bytes GPU thread stack size overflows the stack,
! so raise it to an arbitrarily higher value:
! { dg-set-target-env-var GOMP_NVPTX_NATIVE_GPU_THREAD_STACK_SIZE 3333 }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0] }
! { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

program main
  implicit none
  integer :: var = 42

!$acc parallel ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'dt_parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: artificial} {} { target *-*-* } l_compute$c_compute }
  write (0, '("The answer is ", I2)') var
!$acc end parallel

end program main
