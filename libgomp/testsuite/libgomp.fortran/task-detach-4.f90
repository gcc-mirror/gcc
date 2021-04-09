! { dg-do run }

! Test detach clause, where a task fulfills its own completion event.

program task_detach_4

  use omp_lib

  integer (kind=omp_event_handle_kind) :: detach_event
  integer :: x = 0

  !$omp parallel
    !$omp single
      !$omp task detach (detach_event)
        x = x + 1
	call omp_fulfill_event (detach_event)
      !$omp end task
    !$omp end single
  !$omp end parallel

  if (x /= 1) stop 1
end program
