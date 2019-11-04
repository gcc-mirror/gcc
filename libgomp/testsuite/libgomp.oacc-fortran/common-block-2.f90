! { dg-do run }
!
! Test data located inside common blocks.  This test does not exercise
! ACC DECLARE.  All data clauses are explicit.

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

subroutine incr
  use consts

  implicit none
  integer i, j
  real*4 x(n), y(n), z
  common /BLOCK/ x, y, z, j

  !$acc parallel loop pcopy(/BLOCK/)
  do i = 1, n
     x(i) = x(i) + z
  end do
  !$acc end parallel loop
end subroutine incr

program main
  use consts

  implicit none
  integer i, j
  real*4 a(n), b(n), c
  common /BLOCK/ a, b, c, j

  ! Test copyout, pcopy, device

  !$acc data copyout(a, c)

  c = 1.0

  !$acc update device(c)

  !$acc parallel loop pcopy(a)
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel loop

  call incr
  call incr
  call incr
  !$acc end data

  c = 3.0
  call validate

  ! Test pcopy without copyout

  c = 2.0
  call incr
  c = 5.0
  call validate

  ! Test create, delete, host, copyout, copyin

  !$acc enter data create(b)

  !$acc parallel loop pcopy(b)
  do i = 1, n
     b(i) = i
  end do
  !$acc end parallel loop

  !$acc update host (b)

  !$acc parallel loop pcopy(b) copyout(a) copyin(c)
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc end parallel loop

  !$acc exit data delete(b)

  call validate

  a(:) = b(:)
  c = 0.0
  call validate

  ! Test copy

  c = 1.0
  !$acc parallel loop copy(/BLOCK/)
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc end parallel loop

  call validate

  ! Test pcopyin, pcopyout FIXME

  c = 2.0
  !$acc data copyin(b, c) copyout(a)

  !$acc parallel loop pcopyin(b, c) pcopyout(a)
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc end parallel loop

  !$acc end data

  call validate

  ! Test reduction, private

  j = 0

  !$acc parallel private(i) copy(j)
  !$acc loop reduction(+:j)
  do i = 1, n
     j = j + 1
  end do
  !$acc end parallel

  if (j .ne. n) stop 2

  ! Test firstprivate, copy

  a(:) = 0
  c = j

  !$acc parallel loop firstprivate(c) copyout(a)
  do i = 1, n
     a(i) = i + c
  end do
  !$acc end parallel loop

  call validate
end program main
