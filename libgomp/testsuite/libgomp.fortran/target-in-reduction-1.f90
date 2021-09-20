! { dg-do run }

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

program main
  integer :: x, y

  x = 1
  y = 1

  call foo (x, y)

  if (x .ne. 11) stop 1
  if (y .ne. 21) stop 2

end program main
