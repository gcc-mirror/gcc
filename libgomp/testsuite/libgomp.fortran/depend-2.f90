! { dg-do run }

  integer :: x(3:6, 7:12), y
  y = 1
  !$omp parallel shared (x, y)
    !$omp single
      !$omp taskgroup
        !$omp task depend(in: x(:, :))
	  if (y.ne.1) stop 1
        !$omp end task
        !$omp task depend(out: x(:, :))
	  y = 2
        !$omp end task
      !$omp end taskgroup
      !$omp taskgroup
        !$omp task depend(in: x(4, 7))
	  if (y.ne.2) stop 2
        !$omp end task
        !$omp task depend(out: x(4:4, 7:7))
	  y = 3
        !$omp end task
      !$omp end taskgroup
      !$omp taskgroup
        !$omp task depend(in: x(4:, 8:))
	  if (y.ne.3) stop 3
        !$omp end task
        !$omp task depend(out: x(4:6, 8:12))
	  y = 4
        !$omp end task
      !$omp end taskgroup
    !$omp end single
  !$omp end parallel
  if (y.ne.4) stop 4
end
