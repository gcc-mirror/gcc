! { dg-do run }

module m
  integer :: a
end module m

program main
  use omp_lib
  use m
  implicit none
  !$omp task
    if (.not. omp_in_explicit_task ()) &
      error stop
    !$omp task
      if (.not. omp_in_explicit_task ()) &
        error stop
      !$omp taskgroup task_reduction (+: a)
        if (.not. omp_in_explicit_task ()) &
          error stop
	!$omp task in_reduction (+: a)
          a = a + 1
          if (.not. omp_in_explicit_task ()) &
            error stop
	!$omp end task
      !$omp end taskgroup
      if (.not. omp_in_explicit_task ()) &
        error stop
      !$omp taskwait
    !$omp end task
  !$omp end task
end
