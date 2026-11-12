! { dg-do run }
! PR 128678
!
! Decls local to a SIMD body that own allocated memory, and CLASS
! temporaries there, must not be freed by the OpenMP destructor hook: a
! CLASS temporary for a non-polymorphic actual points to that actual.

module m
  implicit none
  type t
    real, allocatable :: v(:)
  end type
  type acc
    integer :: base = 10
  end type
  type, extends(acc) :: acc2
    integer :: extra = 1
  end type
contains
  pure integer function bump (this, k)
    class(acc), intent(in) :: this
    integer, intent(in) :: k
    bump = this%base + k
  end function
end module

program p
  use m
  implicit none
  type(acc) :: o
  type(acc2) :: o2
  integer :: r(50), s(50), i
  real :: x(50)

  !$omp parallel do simd
  do i = 1, 50
    r(i) = bump (o, i)
    s(i) = bump (o2, -i)
  end do

  !$omp parallel do simd
  do i = 1, 50
    block
      real, allocatable :: w(:)
      type(t) :: y
      class(acc), allocatable :: c
      allocate (w(i))
      w = 1.0
      y%v = w
      allocate (acc2 :: c)
      c%base = i
      x(i) = sum (y%v) + c%base
    end block
  end do

  do i = 1, 50
    if (r(i) /= 10 + i .or. s(i) /= 10 - i) stop 1
    if (x(i) /= 2 * i) stop 2
  end do
end program
