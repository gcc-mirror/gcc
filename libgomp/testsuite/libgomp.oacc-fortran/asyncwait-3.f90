! { dg-do run }

! See also '../libgomp.oacc-c-c++-common/f-asyncwait-3.c'.

! { dg-additional-options "--param=openacc-kernels=decompose" } */

! { dg-additional-options "-fopt-info-all-omp" }
! { dg-additional-options "-foffload=-fopt-info-all-omp" } */

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0 c_loop_i 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".  */

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:), c(:)
  integer i

  allocate (a(N))
  allocate (b(N))
  allocate (c(N))

  !$acc parallel async (0)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
    a(i) = 1
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
    b(i) = 1
  end do
  !$acc end parallel

  !$acc wait (0, 1)

  !$acc parallel
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
    c(i) = a(i) + b(i)
  end do
  !$acc end parallel

  do i = 1, N
    if (c(i) .ne. 2.0) STOP 1
  end do

  !$acc kernels async (0)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
    a(i) = 1
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
    b(i) = 1
  end do
  !$acc end kernels

  !$acc wait (0, 1)

  !$acc kernels
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
    c(i) = a(i) + b(i)
  end do
  !$acc end kernels

  do i = 1, N
    if (c(i) .ne. 2.0) STOP 2
  end do
  
  deallocate (a)
  deallocate (b)
  deallocate (c)
end program asyncwait
