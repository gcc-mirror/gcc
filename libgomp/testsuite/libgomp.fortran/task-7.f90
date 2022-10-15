! { dg-do run }

program main
  use omp_lib
  implicit none

  !$omp task final (.true.)
    if (.not. omp_in_final ()) &
      error stop
    !$omp task
      if (.not. omp_in_final ()) &
        error stop
      !$omp target nowait
      if (omp_in_final ()) &
        error stop
      !$omp end target
      if (.not. omp_in_final ()) &
        error stop
      !$omp taskwait
    !$omp end task
  !$omp end task
end
