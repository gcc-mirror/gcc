! { dg-do run }

module m
  integer :: a = 0
end module m

program main
  !$omp task
    !$omp taskgroup task_reduction (+: a)
      !$omp task in_reduction (+: a)
        a = a + 1
      !$omp end task
    !$omp end taskgroup
  !$omp end task
end
