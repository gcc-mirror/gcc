! { dg-do run }
!
! Test data located inside common blocks.  This test does not exercise
! ACC DECLARE.

module const
  integer, parameter :: n = 100
end module const

subroutine check
  use const

  implicit none
  integer i, x(n), y
  common /BLOCK/ x, y

  do i = 1, n
     if (x(i) .ne. y) stop 1
  end do
end subroutine check

module m
  use const
  integer a(n), b
  common /BLOCK/ a, b

contains
  subroutine mod_implicit_incr
    implicit none
    integer i

    !$acc parallel loop
    do i = 1, n
       a(i) = b
    end do
    !$acc end parallel loop

    call check
  end subroutine mod_implicit_incr

  subroutine mod_explicit_incr
    implicit none
    integer i

    !$acc parallel loop copy(a(1:n)) copyin(b)
    do i = 1, n
       a(i) = b
    end do
    !$acc end parallel loop

    call check
  end subroutine mod_explicit_incr
end module m

subroutine sub_implicit_incr
  use const

  implicit none
  integer i, x(n), y
  common /BLOCK/ x, y

  !$acc parallel loop
  do i = 1, n
     x(i) = y
  end do
  !$acc end parallel loop

  call check
end subroutine sub_implicit_incr

subroutine sub_explicit_incr
  use const

  implicit none
  integer i, x(n), y
  common /BLOCK/ x, y

  !$acc parallel loop copy(x(1:n)) copyin(y)
  do i = 1, n
     x(i) = y
  end do
  !$acc end parallel loop

  call check
end subroutine sub_explicit_incr

program main
  use m

  implicit none

  a(:) = -1
  b = 5
  call mod_implicit_incr

  a(:) = -2
  b = 6
  call mod_explicit_incr

  a(:) = -3
  b = 7
  call sub_implicit_incr

  a(:) = -4
  b = 8
  call sub_explicit_incr
end program main
