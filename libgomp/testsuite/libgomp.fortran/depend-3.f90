! { dg-do run }

  integer :: x(2, 3)
  integer, allocatable :: z(:, :)
  allocate (z(-2:3, 2:4))
  call foo (x, z)
contains
  subroutine foo (x, z)
    integer :: x(:, :), y
    integer, allocatable :: z(:, :)
    y = 1
    !$omp parallel shared (x, y, z)
      !$omp single
        !$omp taskgroup
          !$omp task depend(in: x)
  	  if (y.ne.1) call abort
          !$omp end task
          !$omp task depend(out: x(1:2, 1:3))
  	  y = 2
          !$omp end task
        !$omp end taskgroup
        !$omp taskgroup
          !$omp task depend(in: z)
  	  if (y.ne.2) call abort
          !$omp end task
          !$omp task depend(out: z(-2:3, 2:4))
  	  y = 3
          !$omp end task
        !$omp end taskgroup
        !$omp taskgroup
          !$omp task depend(in: x)
  	  if (y.ne.3) call abort
          !$omp end task
          !$omp task depend(out: x(1:, 1:))
  	  y = 4
          !$omp end task
        !$omp end taskgroup
      !$omp end single
    !$omp end parallel
    if (y.ne.4) call abort
  end subroutine
end
