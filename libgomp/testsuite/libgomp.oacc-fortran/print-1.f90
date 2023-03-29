! Ensure that write on the offload device works.

! { dg-do run }
! { dg-output "The answer is 42(\n|\r\n|\r)+" }

! Separate file 'print-1-nvptx.f90' for nvptx offloading.
! { dg-skip-if "separate file" { offload_target_nvptx } }

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
