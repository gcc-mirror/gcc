! { dg-do run }

program example
   integer :: x
   x = 0
   !$omp parallel
   !$omp single
      !$omp task shared(x) depend(out: x)
         x = 1
      !$omp end task
      !$omp task shared(x) depend(out: x)
         x = 2
      !$omp end task
      !$omp taskwait
      if ((x .ne. 1) .and. (x .ne. 2)) STOP 1
   !$omp end single
   !$omp end parallel
end program
