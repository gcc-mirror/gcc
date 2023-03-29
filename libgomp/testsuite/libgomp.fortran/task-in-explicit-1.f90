! { dg-do run }

program main
  use omp_lib
  implicit none
  integer :: i

  if (omp_in_explicit_task ()) &
    error stop
  !$omp task
  if (.not. omp_in_explicit_task ()) &
    error stop
  !$omp end task

  !$omp task final (.true.)
    if (.not. omp_in_explicit_task ()) &
      error stop
    !$omp task
    if (.not. omp_in_explicit_task ()) &
      error stop
    !$omp end task
  !$omp end task

  !$omp parallel
    if (omp_in_explicit_task ()) &
      error stop
    !$omp task if (.false.)
        if (.not. omp_in_explicit_task ()) &
          error stop
        !$omp task if (.false.)
          if (.not. omp_in_explicit_task ()) &
            error stop
        !$omp end task
    !$omp end task
    !$omp task final (.true.)
      if (.not. omp_in_explicit_task ()) &
        error stop
    !$omp end task
    !$omp barrier
    if (omp_in_explicit_task ()) &
      error stop
    !$omp taskloop num_tasks (24)
    do i = 1, 32
      if (.not. omp_in_explicit_task ()) &
        error stop
    end do
    !$omp masked
    !$omp task
    if (.not. omp_in_explicit_task ()) &
      error stop
    !$omp end task
    !$omp end masked
    !$omp barrier
    if (omp_in_explicit_task ()) &
      error stop
  !$omp end parallel

  !$omp target
    if (omp_in_explicit_task ()) &
      error stop
    !$omp task if (.false.)
    if (.not. omp_in_explicit_task ()) &
      error stop
    !$omp end task
    !$omp task
    if (.not. omp_in_explicit_task ()) &
      error stop
    !$omp end task
  !$omp end target

  !$omp target teams
    !$omp distribute
    do i = 1, 4
      if (omp_in_explicit_task ()) then
        error stop
      else
          !$omp parallel
            if (omp_in_explicit_task ()) &
              error stop
            !$omp task
            if (.not. omp_in_explicit_task ()) &
              error stop
            !$omp end task
            !$omp barrier
            if (omp_in_explicit_task ()) &
              error stop
          !$omp end parallel
      end if
    end do
  !$omp end target teams

  !$omp teams
    !$omp distribute
    do i = 1, 4
      if (omp_in_explicit_task ()) then
        error stop
      else
          !$omp parallel
            if (omp_in_explicit_task ()) &
              error stop
            !$omp task
            if (.not. omp_in_explicit_task ()) &
              error stop
            !$omp end task
            !$omp barrier
            if (omp_in_explicit_task ()) &
              error stop
          !$omp end parallel
      end if
    end do
    !$omp end distribute
  !$omp end teams
end
