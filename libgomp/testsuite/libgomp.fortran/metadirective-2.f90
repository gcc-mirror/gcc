! { dg-do run }

program test
  implicit none
  integer, parameter :: N = 100
  real, parameter :: PI_CONST = 2.0*acos(0.0)
  real, parameter :: E_CONST = exp(1.0)
  real, parameter :: EPSILON = 0.001
  integer :: i
  real :: a(N)

  !$omp target map(from: a)
    call f (a, PI_CONST)
  !$omp end target

  do i = 1, N
    if (abs (a(i) - (PI_CONST * i)) .gt. EPSILON) stop 1
  end do

  call f (a, E_CONST)

  do i = 1, N
    if (abs (a(i) - (E_CONST * i)) .gt. EPSILON) stop 2
  end do
contains
  subroutine f (a, x)
    integer :: i
    real :: a(N), x
    !$omp declare target

    !$omp metadirective &
    !$omp&  when (construct={target}: distribute parallel do ) &
    !$omp&  default(parallel do simd)
      do i = 1, N
	a(i) = x * i
      end do
  end subroutine
end program
