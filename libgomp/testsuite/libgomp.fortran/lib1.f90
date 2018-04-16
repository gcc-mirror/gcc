! { dg-do run }

  use omp_lib

  double precision :: d, e
  logical :: l
  integer (kind = omp_lock_kind) :: lck
  integer (kind = omp_nest_lock_kind) :: nlck

  d = omp_get_wtime ()

  call omp_init_lock (lck)
  call omp_set_lock (lck)
  if (omp_test_lock (lck)) STOP 1
  call omp_unset_lock (lck)
  if (.not. omp_test_lock (lck)) STOP 2
  if (omp_test_lock (lck)) STOP 3
  call omp_unset_lock (lck)
  call omp_destroy_lock (lck)

  call omp_init_nest_lock (nlck)
  if (omp_test_nest_lock (nlck) .ne. 1) STOP 4
  call omp_set_nest_lock (nlck)
  if (omp_test_nest_lock (nlck) .ne. 3) STOP 5
  call omp_unset_nest_lock (nlck)
  call omp_unset_nest_lock (nlck)
  if (omp_test_nest_lock (nlck) .ne. 2) STOP 6
  call omp_unset_nest_lock (nlck)
  call omp_unset_nest_lock (nlck)
  call omp_destroy_nest_lock (nlck)

  call omp_set_dynamic (.true.)
  if (.not. omp_get_dynamic ()) STOP 7
  call omp_set_dynamic (.false.)
  if (omp_get_dynamic ()) STOP 8

  call omp_set_nested (.true.)
  if (.not. omp_get_nested ()) STOP 9
  call omp_set_nested (.false.)
  if (omp_get_nested ()) STOP 10

  call omp_set_num_threads (5)
  if (omp_get_num_threads () .ne. 1) STOP 11
  if (omp_get_max_threads () .ne. 5) STOP 12
  if (omp_get_thread_num () .ne. 0) STOP 13
  call omp_set_num_threads (3)
  if (omp_get_num_threads () .ne. 1) STOP 14
  if (omp_get_max_threads () .ne. 3) STOP 15
  if (omp_get_thread_num () .ne. 0) STOP 16
  l = .false.
!$omp parallel reduction (.or.:l)
  l = omp_get_num_threads () .ne. 3
  l = l .or. (omp_get_thread_num () .lt. 0)
  l = l .or. (omp_get_thread_num () .ge. 3)
!$omp master
  l = l .or. (omp_get_thread_num () .ne. 0)
!$omp end master
!$omp end parallel
  if (l) STOP 17

  if (omp_get_num_procs () .le. 0) STOP 18
  if (omp_in_parallel ()) STOP 19
!$omp parallel reduction (.or.:l)
  l = .not. omp_in_parallel ()
!$omp end parallel
!$omp parallel reduction (.or.:l) if (.true.)
  l = .not. omp_in_parallel ()
!$omp end parallel
  if (l) STOP 20

  e = omp_get_wtime ()
  if (d .gt. e) STOP 21
  d = omp_get_wtick ()
  ! Negative precision is definitely wrong,
  ! bigger than 1s clock resolution is also strange
  if (d .le. 0 .or. d .gt. 1.) STOP 22
end
