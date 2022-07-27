! Ensure that write on the offload device works.

! { dg-do run }
! { dg-output "The answer is 42(\n|\r\n|\r)+" }

! Separate file 'print-1-nvptx.f90' for nvptx offloading.
! { dg-skip-if "separate file" { offload_target_nvptx } }

! For GCN offloading compilation, when gang-privatizing 'dt_parm.N'
! (see below), we run into an 'gang-private data-share memory exhausted'
! error: the default '-mgang-private-size' is too small.  Per
! 'gcc/fortran/trans-io.cc'/'libgfortran/io/io.h', that one is
! 'struct st_parameter_dt', which indeed is rather big.  Instead of
! working out its exact size (which may vary per GCC configuration),
! raise '-mgang-private-size' to an arbitrary high value.
! { dg-additional-options "-foffload-options=amdgcn-amdhsa=-mgang-private-size=13579" { target openacc_radeon_accel_selected } }

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
  ! { dg-note {variable 'dt_parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'dt_parm\.[0-9]+' ought to be adjusted for OpenACC privatization level: 'gang'} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'dt_parm\.[0-9]+' adjusted for OpenACC privatization level: 'gang'} {} { target { ! openacc_host_selected } } l_compute$c_compute }
  write (0, '("The answer is ", I2)') var
!$acc end parallel

end program main
