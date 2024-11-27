! { dg-do run }

! Test transfer of dynamically-allocated arrays to target using map
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

  expected = mkarray ()

  !$omp target map(iterator(i=1:DIM1), to: x(i)%arr(:)) map(from: sum)
    sum = 0
    do i = 1, DIM1
      do j = 1, DIM2
	sum = sum + x(i)%arr(j)
      end do
    end do
  !$omp end target

  if (sum .ne. expected) stop 1
contains
  integer function mkarray ()
    integer :: exp = 0

    do i = 1, DIM1
      allocate (x(i)%arr(DIM2))
      do j = 1, DIM2
        x(i)%arr(j) = i * j
	exp = exp + x(i)%arr(j)
      end do
    end do

    mkarray = exp
  end function
end program
