! { dg-do run }

! Test transfer of dynamically-allocated arrays to target using map
! iterators, with multiple iterators, function calls and non-constant
! bounds in the iterator expression.

program test
  implicit none

  integer, parameter :: DIM1 = 16
  integer, parameter :: DIM2 = 4

  type :: array_ptr
    integer, pointer :: arr(:)
  end type

  type (array_ptr) :: x(DIM1), y(DIM1)
  integer :: expected, sum, i, j, k
  integer :: i_ubound
  integer :: k_ubound

  expected = mkarrays (k_ubound)
  i_ubound = k_ubound / 4 - 1

  !$omp target map(iterator(i=0:i_ubound, j=0:3), to: x(f (i, j))%arr(:)) &
  !$omp        map(iterator(k=1:k_ubound), to: y(k)%arr(:)) &
  !$omp        map(from: sum)
    sum = 0
    do i = 1, DIM1
      do j = 1, DIM2
	sum = sum + x(i)%arr(j) * y(i)%arr(j)
      end do
    end do
  !$omp end target

  if (sum .ne. expected) stop 1
contains
  integer function mkarrays (ubound)
    integer, intent(out) :: ubound
    integer :: exp = 0

    do i = 1, DIM1
      allocate (x(i)%arr(DIM2))
      allocate (y(i)%arr(DIM2))
      do j = 1, DIM2
	x(i)%arr(j) = i * j
	y(i)%arr(j) = i + j
	exp = exp + x(i)%arr(j) * y(i)%arr(j)
      end do
    end do

    ubound = DIM1
    mkarrays = exp
  end function

  integer function f (i, j)
    integer, intent(in) :: i, j

    f = i * 4 + j + 1
  end function
end program
