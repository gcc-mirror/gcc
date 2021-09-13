! { dg-do run }
! { dg-additional-options "-fopt-info-omp-all" }
! { dg-additional-options "--param=openacc-kernels=decompose" }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_loop_i 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

subroutine kernel(lo, hi, a, b, c)
  implicit none
  integer :: lo, hi, i
  real, dimension(lo:hi) :: a, b, c

  !$acc kernels copyin(lo, hi)
  !$acc loop independent ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-message "note: parallelized loop nest in OpenACC 'kernels' region" "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = lo, hi
     b(i) = a(i)
  end do
  !$acc loop independent ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-message "note: parallelized loop nest in OpenACC 'kernels' region" "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = lo, hi
     c(i) = b(i)
  end do
  !$acc end kernels
end subroutine kernel

program main
  integer :: n = 20
  real, dimension(1:20) :: a, b, c

  a(:) = 1
  b(:) = 2
  c(:) = 3

  call kernel(1, n, a, b, c)

  do i = 1, n
     if (c(i) .ne. 1) call abort
  end do
end program main
