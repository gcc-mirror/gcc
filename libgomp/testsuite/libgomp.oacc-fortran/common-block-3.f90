! { dg-do run }
!
! Test data located inside common blocks.  This test does not exercise
! ACC DECLARE.  Most of the data clauses are implicit.

module consts
  integer, parameter :: n = 100
end module consts

subroutine validate
  use consts

  implicit none
  integer i, j
  real*4 x(n), y(n), z
  common /BLOCK/ x, y, z, j

  do i = 1, n
     if (abs(x(i) - i - z) .ge. 0.0001) stop 1
  end do
end subroutine validate

subroutine incr_parallel
  use consts

  implicit none
  integer i, j
  real*4 x(n), y(n), z
  common /BLOCK/ x, y, z, j

  !$acc parallel loop
  do i = 1, n
     x(i) = x(i) + z
  end do
  !$acc end parallel loop
end subroutine incr_parallel

subroutine incr_kernels
  use consts

  implicit none
  integer i, j
  real*4 x(n), y(n), z
  common /BLOCK/ x, y, z, j

  !$acc kernels
  do i = 1, n
     x(i) = x(i) + z
  end do
  !$acc end kernels
end subroutine incr_kernels

program main
  use consts

  implicit none
  integer i, j
  real*4 a(n), b(n), c
  common /BLOCK/ a, b, c, j

  !$acc data copyout(a, c)

  c = 1.0

  !$acc update device(c)

  !$acc parallel loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel loop

  call incr_parallel
  call incr_parallel
  call incr_parallel
  !$acc end data

  c = 3.0
  call validate

  ! Test pcopy without copyout

  c = 2.0
  call incr_kernels
  c = 5.0
  call validate

  !$acc kernels
  do i = 1, n
     b(i) = i
  end do
  !$acc end kernels

  !$acc parallel loop
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc end parallel loop

  call validate

  a(:) = b(:)
  c = 0.0
  call validate

  ! Test copy

  c = 1.0
  !$acc parallel loop
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc end parallel loop

  call validate

  c = 2.0
  !$acc data copyin(b, c) copyout(a)

  !$acc kernels
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc end kernels

  !$acc end data

  call validate

  j = 0

  !$acc parallel loop reduction(+:j)
  do i = 1, n
     j = j + 1
  end do
  !$acc end parallel loop

  if (j .ne. n) stop 2
end program main
