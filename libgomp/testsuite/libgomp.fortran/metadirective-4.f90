! { dg-do run }

program test
  use omp_lib

  implicit none
  integer, parameter :: N = 100
  integer :: a(N)
  logical :: is_parallel, is_static

  ! is_static is always set if run_parallel is false.
  call f (a, .false., .false., is_parallel, is_static)
  if (is_parallel .or. .not. is_static) stop 1

  call f (a, .false., .true., is_parallel, is_static)
  if (is_parallel .or. .not. is_static) stop 2

  call f (a, .true., .false., is_parallel, is_static)
  if (.not. is_parallel .or. is_static) stop 3

  call f (a, .true., .true., is_parallel, is_static)
  if (.not. is_parallel .or. .not. is_static) stop 4
contains
  subroutine f (a, run_parallel, run_static, is_parallel, is_static)
    integer :: a(N)
    logical, intent(in) :: run_parallel, run_static
    logical, intent(out) :: is_parallel, is_static
    integer :: i

    is_parallel = .false.
    is_static = .false.

    !$omp begin metadirective when(user={condition(run_parallel)}: parallel)
      if (omp_in_parallel ()) is_parallel = .true.

      !$omp metadirective &
      !$omp&  when(construct={parallel}, user={condition(.not. run_static)}: &
      !$omp&       do schedule(guided) private(is_static)) &
      !$omp&  when(construct={parallel}: do schedule(static))
	do i = 1, N
	  a(i) = i
	  is_static = .true.
	end do
    !$omp end metadirective
  end subroutine
end program
