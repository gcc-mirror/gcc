! { dg-do run }

! Test the detach clause when the task is undeferred.

program task_detach_11
  use omp_lib

  integer (kind=omp_event_handle_kind) :: detach_event

  !$omp task detach (detach_event)
    call omp_fulfill_event (detach_event)
  !$omp end task
end program
