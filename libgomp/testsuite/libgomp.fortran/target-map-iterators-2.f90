! { dg-do run }

! Test transfer of dynamically-allocated arrays from target using map
! iterators.

program test
  implicit none

  integer, parameter :: DIM1 = 8
  integer, parameter :: DIM2 = 15

  type :: array_ptr
    integer, pointer :: arr(:)
  end type

  type (array_ptr) :: x(DIM1)
  integer :: expected, sum, i, j

  call mkarray

  !$omp target map(iterator(i=1:DIM1), from: x(i)%arr(:)) map(from: expected)
    expected = 0
    do i = 1, DIM1
      do j = 1, DIM2
	x(i)%arr(j) = (i+1) * (j+1)
	expected = expected + x(i)%arr(j)
      end do
    end do
  !$omp end target

  sum = 0
  do i = 1, DIM1
    do j = 1, DIM2
      sum = sum + x(i)%arr(j)
    end do
  end do

  if (sum .ne. expected) stop 1
contains
  subroutine mkarray
    do i = 1, DIM1
      allocate (x(i)%arr(DIM2))
    end do
  end subroutine
end program
