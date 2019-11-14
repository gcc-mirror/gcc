! { dg-do run }

program test
  implicit none
  real, allocatable :: a(:), b(:)
  integer :: i

  a = [(i, i = 1, 100)]
  allocate(b, mold=a)
  b = 0

  !$omp target simd map(to:a) map(from:b)
  do i = 1, size(a)
    b(i) = 5.0 * a(i)
  end do

  if (any (b - 5.0 *a > 10.0*epsilon(a))) stop 1

  !$omp target simd map(to:a) map(from:b)
  do i = 1, size(a)
    b(i) = 2.0 * a(i)
  end do
  !$omp end target simd

  if (any (b - 2.0 *a > 10.0*epsilon(a))) stop 2
end program test
