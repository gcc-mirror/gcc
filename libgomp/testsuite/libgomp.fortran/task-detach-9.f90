! { dg-do run }

! Test tasks with detach clause.  Each thread spawns off a chain of tasks
! in a taskgroup, that can then be executed by any available thread.

program task_detach_9
  use omp_lib

  integer (kind=omp_event_handle_kind) :: detach_event1, detach_event2
  integer :: x = 0, y = 0, z = 0
  integer :: thread_count

  !$omp parallel private (detach_event1, detach_event2)
    !$omp taskgroup
      !$omp single
	thread_count = omp_get_num_threads ()
      !$omp end single

      !$omp task detach (detach_event1) untied
	!$omp atomic update
	  x = x + 1
      !$omp end task

      !$omp task detach (detach_event2) untied
	!$omp atomic update
	  y = y + 1
	call omp_fulfill_event (detach_event1);
      !$omp end task

      !$omp task untied
	!$omp atomic update
	  z = z + 1
	call omp_fulfill_event (detach_event2);
      !$omp end task
    !$omp end taskgroup
  !$omp end parallel

  if (x /= thread_count) stop 1
  if (y /= thread_count) stop 2
  if (z /= thread_count) stop 3
end program
