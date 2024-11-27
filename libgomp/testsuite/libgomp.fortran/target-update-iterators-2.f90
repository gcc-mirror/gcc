! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

! Test target enter data and target update from the target using map
! iterators.

program test
  integer, parameter :: DIM1 = 8
  integer, parameter :: DIM2 = 15

  type :: array_ptr
    integer, pointer :: arr(:)
  end type

  type (array_ptr) :: x(DIM1)
  integer :: sum, expected

  call mkarray (x)

  !$omp target enter data map(to: x(:DIM1))
  !$omp target enter data map(iterator(i=1:DIM1), to: x(i)%arr(:))
  !$omp target map(from: expected)
    expected = 0
    do i = 1, DIM1
      do j = 1, DIM2
	x(i)%arr(j) = (i + 1) * (j + 2)
	expected = expected + x(i)%arr(j)
      end do
    end do
  !$omp end target

  ! Host copy of x should remain unchanged.
  sum = 0
  do i = 1, DIM1
    do j = 1, DIM2
      sum = sum + x(i)%arr(j)
    end do
  end do
  if (sum .ne. 0) stop 1

  !$omp target update from(iterator(i=1:DIM1): x(i)%arr(:))

  ! Host copy should now be updated.
  sum = 0
  do i = 1, DIM1
    do j = 1, DIM2
      sum = sum + x(i)%arr(j)
    end do
  end do

  if (sum .ne. expected) stop 2
contains
  subroutine mkarray (x)
    type (array_ptr), intent(inout) :: x(DIM1)

    do i = 1, DIM1
      allocate (x(i)%arr(DIM2))
      do j = 1, DIM2
	x(i)%arr(j) = 0
      end do
    end do
  end subroutine
end program
