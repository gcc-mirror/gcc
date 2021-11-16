! { dg-additional-options "-Wno-deprecated-declarations" }

program teams1
  use omp_lib
  integer :: i
!$omp teams thread_limit (2)
  !$omp distribute dist_schedule(static,1)
  do i = 1, 1
    if (omp_in_parallel ()) stop 1
    if (omp_get_level () .ne. 0) stop 2
    if (omp_get_ancestor_thread_num (0) .ne. 0) stop 3
    if (omp_get_ancestor_thread_num (1) .ne. -1) stop 4
    call omp_set_dynamic (.false.)
    call omp_set_nested (.true.)
  end do
!$omp parallel num_threads (2)
  if (.not. omp_in_parallel ()) stop 5
  if (omp_get_level () .ne. 1) stop 6
  if (omp_get_ancestor_thread_num (0) .ne. 0) stop 7
  if (omp_get_ancestor_thread_num (1) &
&     .ne. omp_get_thread_num ()) stop 8
  if (omp_get_ancestor_thread_num (2) .ne. -1) stop 9
!$omp end parallel
!$omp end teams
end program
