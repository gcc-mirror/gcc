! { dg-do run }

module mod1
  contains

    subroutine foo (x, y)
      integer :: x, y

      !$omp taskgroup task_reduction (+: x, y)

      !$omp target in_reduction (+: x, y)
      x = x + 8
      y = y + 16
      !$omp end target

      !$omp task in_reduction (+: x, y)
      x = x + 2
      y = y + 4
      !$omp end task

      !$omp end taskgroup
    end subroutine foo

    integer function bar (x)
      integer, value :: x

      !$omp taskgroup task_reduction (+: x)

      !$omp target in_reduction (+: x)
      x = x + 16
      !$omp end target

      !$omp task in_reduction (+: x)
      x = x + 32
      !$omp end task

      !$omp end taskgroup

      bar = x
    end function bar
  end module mod1

program main
  use mod1
  integer :: x, y
  real :: f;

  x = 1
  y = 1

  call foo (x, y)

  if (x .ne. 11) stop 1
  if (y .ne. 21) stop 2

  y = bar (8)
  if (y .ne. 56) stop 3

  x = 0
  f = 0.0

  !$omp taskgroup task_reduction (+: x, f)
  !$omp target in_reduction (+: x, f)
  x = x + 1
  f = f + 2.0
  !$omp end target

  !$omp task in_reduction (+: x, f)
  x = x + 2
  f = f + 3.0
  !$omp end task

  !$omp end taskgroup

  if (x .ne. 3) stop 4
  if (f .ne. 5.0) stop 5

end program main
