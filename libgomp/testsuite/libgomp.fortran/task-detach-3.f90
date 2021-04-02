! { dg-do run }

! Test the task detach clause used together with dependencies.

program task_detach_3

  use omp_lib

  integer (kind=omp_event_handle_kind) :: detach_event
  integer :: x = 0, y = 0, z = 0
  integer :: dep

  !$omp parallel
    !$omp single
      !$omp task depend (out:dep) detach (detach_event)
        x = x + 1
      !$omp end task

      !$omp task
        y = y + 1
	call omp_fulfill_event (detach_event)
      !$omp end task

      !$omp task depend (in:dep)
        z = z + 1
      !$omp end task
    !$omp end single
  !$omp end parallel

  if (x /= 1) stop 1
  if (y /= 1) stop 2
  if (z /= 1) stop 3
end program
