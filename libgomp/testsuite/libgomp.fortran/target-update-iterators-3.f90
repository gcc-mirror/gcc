! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

! Test target enter data and target update to the target using map
! iterators with a function.

program test
  implicit none

  integer, parameter :: DIM1 = 8
  integer, parameter :: DIM2 = 15

  type :: array_ptr
    integer, pointer :: arr(:)
  end type

  type (array_ptr) :: x(DIM1)
  integer :: x_new(DIM1, DIM2)
  integer :: expected, sum, i, j

  call mkarray (x)

  !$omp target enter data map(to: x(:DIM1))
  !$omp target enter data map(iterator(i=1:DIM1), to: x(i)%arr(:))

  ! Update x on host.
  do i = 1, DIM1
    do j = 1, DIM2
      x_new(i, j) = x(i)%arr(j)
      x(i)%arr(j) = (i + 1) * (j + 2);
    end do
  end do

  ! Update a subset of x on target.
  !$omp target update to(iterator(i=1:DIM1/2): x(f (i))%arr(:))

  !$omp target map(from: sum)
    sum = 0
    do i = 1, DIM1
      do j = 1, DIM2
	sum = sum + x(i)%arr(j)
      end do
    end do
  !$omp end target

  ! Calculate expected value on host.
  do i = 1, DIM1/2
    do j = 1, DIM2
      x_new(f (i), j) = x(f (i))%arr(j)
    end do
  end do

  expected = 0
  do i = 1, DIM1
    do j = 1, DIM2
      expected = expected + x_new(i, j)
    end do
  end do

  if (sum .ne. expected) stop 1
contains
  subroutine mkarray (x)
    type (array_ptr), intent(inout) :: x(DIM1)

    do i = 1, DIM1
      allocate (x(i)%arr(DIM2))
      do j = 1, DIM2
	x(i)%arr(j) = i * j
      end do
    end do
  end subroutine

  integer function f (i)
    integer, intent(in) :: i

    f = i * 2
  end function
end program
