! { dg-do run }

! Test the deviceptr clause with various directives
! and in combination with other directives where
! the deviceptr variable is implied.

subroutine subr1 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  integer :: b(N)
  integer :: i = 0

  !$acc data deviceptr (a)

  !$acc parallel copy (b)
    do i = 1, N
      a(i) = i * 2
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc end data

end subroutine

subroutine subr2 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  !$acc declare deviceptr (a)
  integer :: b(N)
  integer :: i = 0

  !$acc parallel copy (b)
    do i = 1, N
      a(i) = i * 4
      b(i) = a(i)
    end do
  !$acc end parallel

end subroutine

subroutine subr3 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  !$acc declare deviceptr (a)
  integer :: b(N)
  integer :: i = 0

  !$acc kernels copy (b)
    do i = 1, N
      a(i) = i * 8
      b(i) = a(i)
    end do
  !$acc end kernels

end subroutine

subroutine subr4 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  integer :: b(N)
  integer :: i = 0

  !$acc parallel deviceptr (a) copy (b)
    do i = 1, N
      a(i) = i * 16
      b(i) = a(i)
    end do
  !$acc end parallel

end subroutine

subroutine subr5 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  integer :: b(N)
  integer :: i = 0

  !$acc kernels deviceptr (a) copy (b)
    do i = 1, N
      a(i) = i * 32
      b(i) = a(i)
    end do
  !$acc end kernels

end subroutine

subroutine subr6 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  integer :: b(N)
  integer :: i = 0

  !$acc parallel deviceptr (a) copy (b)
    do i = 1, N
      b(i) = i
    end do
  !$acc end parallel

end subroutine

subroutine subr7 (a, b)
  implicit none
  integer, parameter :: N = 8
  integer :: a(N)
  integer :: b(N)
  integer :: i = 0

  !$acc data deviceptr (a)

  !$acc parallel copy (b)
    do i = 1, N
      a(i) = i * 2
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc parallel copy (b)
    do i = 1, N
      a(i) = b(i) * 2
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc end data

end subroutine

program main
  use iso_c_binding, only: c_ptr, c_f_pointer
  implicit none
  type (c_ptr) :: cp
  integer, parameter :: N = 8
  integer, pointer :: fp(:)
  integer :: i = 0
  integer :: b(N)

  interface
    function acc_malloc (s) bind (C)
      use iso_c_binding, only: c_ptr, c_size_t
      integer (c_size_t), value :: s
      type (c_ptr) :: acc_malloc
    end function
  end interface

  cp = acc_malloc (N * sizeof (fp(N)))
  call c_f_pointer (cp, fp, [N])

  call subr1 (fp, b)

  do i = 1, N
    if (b(i) .ne. i * 2) call abort
  end do

  call subr2 (fp, b)

  do i = 1, N
    if (b(i) .ne. i * 4) call abort
  end do

  call subr3 (fp, b)

  do i = 1, N
    if (b(i) .ne. i * 8) call abort
  end do

  call subr4 (fp, b)

  do i = 1, N
    if (b(i) .ne. i * 16) call abort
  end do

  call subr5 (fp, b)

  do i = 1, N
    if (b(i) .ne. i * 32) call abort
  end do

  call subr6 (fp, b)

  do i = 1, N
    if (b(i) .ne. i) call abort
  end do

  call subr7 (fp, b)

  do i = 1, N
    if (b(i) .ne. i * 4) call abort
  end do

end program main
