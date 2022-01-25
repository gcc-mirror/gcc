! { dg-do run }

program test
  implicit none

  integer, parameter :: N = 100
  integer :: a(N)
  integer :: res

  if (f (a, .false.)) stop 1
  if (.not. f (a, .true.)) stop 2
contains
  logical function f (a, flag)
    integer :: a(N)
    logical :: flag
    logical :: res = .false.
    integer :: i
    f = .false.
    !$omp metadirective &
    !$omp&  when (user={condition(.not. flag)}: &
    !$omp&	 target teams distribute parallel do &
    !$omp&		map(from: a(1:N)) private(res)) &
    !$omp&  default(parallel do)
      do i = 1, N
	a(i) = i
	f = .true.
     end do
  end function
end program
