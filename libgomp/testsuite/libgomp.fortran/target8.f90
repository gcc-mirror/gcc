! { dg-do run }

  integer, parameter :: n = 1000
  integer, parameter :: c = 100
  integer :: i, j
  real :: a(n)
  do i = 1, n
    a(i) = i
  end do
  !$omp parallel
  !$omp single
  do i = 1, n, c
    !$omp task shared(a)
      !$omp target map(a(i:i+c-1))
        !$omp parallel do
          do j = i, i + c - 1
            a(j) = foo (a(j))
          end do
      !$omp end target
    !$omp end task
  end do
  !$omp end single
  !$omp end parallel
  do i = 1, n
    if (a(i) /= i + 1) call abort
  end do
contains
  real function foo (x)
    !$omp declare target
    real, intent(in) :: x
    foo = x + 1
  end function foo
end
