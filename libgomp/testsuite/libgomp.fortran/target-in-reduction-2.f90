! { dg-do run }

program main
  integer :: x

  x = 0
  !$omp taskgroup task_reduction (+: x)
  call foo (x)
  call bar (x)
  !$omp end taskgroup

  if (x .ne. 3) stop 1

contains

  subroutine foo (x)
    integer :: x
    !$omp task in_reduction (+: x)
    x = x + 1
    !$omp end task
  end subroutine foo

  subroutine bar (x)
    integer :: x
    !$omp target in_reduction (+: x)
    x = x + 2
    !$omp end target
  end subroutine bar

end program main
