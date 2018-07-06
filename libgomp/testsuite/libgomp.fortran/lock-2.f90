! { dg-do run }

  use omp_lib

  integer (kind = omp_nest_lock_kind) :: lock
  logical :: l

  l = .false.
  call omp_init_nest_lock (lock)
!$omp parallel num_threads (1) reduction (.or.:l)
  if (omp_test_nest_lock (lock) .ne. 1) STOP 1
  if (omp_test_nest_lock (lock) .ne. 2) STOP 2
!$omp task if (.false.) shared (lock, l)
  if (omp_test_nest_lock (lock) .ne. 0) l = .true.
!$omp end task
!$omp taskwait
  if (omp_test_nest_lock (lock) .ne. 3) l = .true.
  call omp_unset_nest_lock (lock)
  call omp_unset_nest_lock (lock)
  call omp_unset_nest_lock (lock)
!$omp end parallel
  if (l) STOP 3
  call omp_destroy_nest_lock (lock)
end
