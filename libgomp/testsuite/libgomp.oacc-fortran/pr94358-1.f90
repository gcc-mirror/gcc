! { dg-do run }
! { dg-additional-options "-fopt-info-omp-all" }

subroutine kernel(lo, hi, a, b, c)
  implicit none
  integer :: lo, hi, i
  real, dimension(lo:hi) :: a, b, c

  !$acc kernels copyin(lo, hi) ! { dg-optimized "assigned OpenACC seq loop parallelism" }
  !$acc loop independent
  do i = lo, hi
     b(i) = a(i)
  end do
  !$acc loop independent
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
