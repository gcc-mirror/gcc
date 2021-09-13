! { dg-do run }

! Test handling of detach clause with only a single thread.  The runtime
! should not block when a task with an unfulfilled event finishes
! running.

program task_detach_2
  use omp_lib

  integer (kind=omp_event_handle_kind) :: detach_event1, detach_event2
  integer :: x = 0, y = 0, z = 0

  !$omp parallel num_threads (1)
    !$omp single
      !$omp task detach (detach_event1)
        x = x + 1
      !$omp end task

      !$omp task detach (detach_event2)
        y = y + 1
	call omp_fulfill_event (detach_event1)
      !$omp end task

      !$omp task
        z = z + 1
	call omp_fulfill_event (detach_event2)
      !$omp end task
    !$omp end single
  !$omp end parallel

  if (x /= 1) stop 1
  if (y /= 1) stop 2
  if (z /= 1) stop 3
end program
