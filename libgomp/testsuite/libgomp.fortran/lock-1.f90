! { dg-do run }

  use omp_lib

  integer (kind = omp_nest_lock_kind) :: lock
  logical :: l

  l = .false.
  call omp_init_nest_lock (lock)
  if (omp_test_nest_lock (lock) .ne. 1) stop 1
  if (omp_test_nest_lock (lock) .ne. 2) stop 2
!$omp parallel if (.false.) reduction (.or.:l)
  ! In OpenMP 2.5 this was supposed to return 3,
  ! but in OpenMP 3.0 the parallel region has a different
  ! task and omp_*_lock_t are owned by tasks, not by threads.
  if (omp_test_nest_lock (lock) .ne. 0) l = .true.
!$omp end parallel
  if (l) stop 3
  if (omp_test_nest_lock (lock) .ne. 3) stop 4
  call omp_unset_nest_lock (lock)
  call omp_unset_nest_lock (lock)
  call omp_unset_nest_lock (lock)
  call omp_destroy_nest_lock (lock)
end
