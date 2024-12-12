! { dg-do run }

! Test target enter data and target update to the target using map
! iterators with non-constant bounds.

program test
  integer, parameter :: DIM1 = 8
  integer, parameter :: DIM2 = 15

  type :: array_ptr
    integer, pointer :: arr(:)
  end type

  type (array_ptr) :: x(DIM1)
  integer :: expected, sum, i, j, ubound

  expected = mkarray (x, ubound)

  !$omp target enter data map(to: x)
  !$omp target enter data map(iterator(i=1:ubound), to: x(i)%arr(:))
  !$omp target map(from: sum)
    sum = 0
    do i = 1, ubound
      do j = 1, DIM2
	sum = sum + x(i)%arr(j)
      end do
    end do
  !$omp end target

  print *, sum, expected
  if (sum .ne. expected) stop 1

  expected = 0
  do i = 1, ubound
    do j = 1, DIM2
      x(i)%arr(j) = x(i)%arr(j) * i * j
      expected = expected + x(i)%arr(j)
    end do
  end do

  !$omp target update to(iterator(i=1:ubound): x(i)%arr(:))

  !$omp target map(from: sum)
    sum = 0
    do i = 1, ubound
      do j = 1, DIM2
	sum = sum + x(i)%arr(j)
      end do
    end do
  !$omp end target

  if (sum .ne. expected) stop 2
contains
  integer function mkarray (x, bound)
    type (array_ptr), intent(inout) :: x(DIM1)
    integer, intent(out) :: bound
    integer :: exp = 0

    do i = 1, DIM1
      allocate (x(i)%arr(DIM2))
      do j = 1, DIM2
	x(i)%arr(j) = i * j
	exp = exp + x(i)%arr(j)
      end do
    end do

    bound = DIM1
    mkarray = exp
  end function
end program
