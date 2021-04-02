! { dg-do run }

! Test chaining of detached tasks, with each task fulfilling the
! completion event of the previous one.

program task_detach_1
  use omp_lib

  integer (kind=omp_event_handle_kind) :: detach_event1, detach_event2
  integer :: x = 0, y = 0, z = 0

  !$omp parallel
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
