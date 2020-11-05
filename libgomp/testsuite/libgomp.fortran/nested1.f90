! { dg-do run }
! { dg-additional-options "-Wno-deprecated-declarations" }

program nested1
  use omp_lib
  integer :: e1, e2, e3, e
  integer :: tn1, tn2, tn3
  e1 = 0
  e2 = 0
  e3 = 0
  call omp_set_nested (.true.)
  call omp_set_dynamic (.false.)
  if (omp_in_parallel ()) stop 1
  if (omp_get_num_threads ().ne.1) stop 2
  if (omp_get_level ().ne.0) stop 3
  if (omp_get_ancestor_thread_num (0).ne.0) stop 4
  if (omp_get_ancestor_thread_num (-1).ne.-1) stop 5
  if (omp_get_ancestor_thread_num (1).ne.-1) stop 6
  if (omp_get_team_size (0).ne.1) stop 7
  if (omp_get_team_size (-1).ne.-1) stop 8
  if (omp_get_team_size (1).ne.-1) stop 9
  if (omp_get_active_level ().ne.0) stop 10
!$omp parallel num_threads (4) private (e, tn1)
  e = 0
  tn1 = omp_get_thread_num ()
  if (.not.omp_in_parallel ()) e = e + 1
  if (omp_get_num_threads ().ne.4) e = e + 1
  if (tn1.lt.0.or.tn1.ge.4) e = e + 1
  if (omp_get_level ().ne.1) e = e + 1
  if (omp_get_ancestor_thread_num (0).ne.0) e = e + 1
  if (omp_get_ancestor_thread_num (1).ne.tn1) e = e + 1
  if (omp_get_ancestor_thread_num (-1).ne.-1) e = e + 1
  if (omp_get_ancestor_thread_num (2).ne.-1) e = e + 1
  if (omp_get_team_size (0).ne.1) e = e + 1
  if (omp_get_team_size (1).ne.4) e = e + 1
  if (omp_get_team_size (-1).ne.-1) e = e + 1
  if (omp_get_team_size (2).ne.-1) e = e + 1
  if (omp_get_active_level ().ne.1) e = e + 1
  !$omp atomic
    e1 = e1 + e
!$omp parallel num_threads (5) if (.false.) firstprivate (tn1) &
!$omp& private (e, tn2)
  e = 0
  tn2 = omp_get_thread_num ()
  if (.not.omp_in_parallel ()) e = e + 1
  if (omp_get_num_threads ().ne.1) e = e + 1
  if (tn2.ne.0) e = e + 1
  if (omp_get_level ().ne.2) e = e + 1
  if (omp_get_ancestor_thread_num (0).ne.0) e = e + 1
  if (omp_get_ancestor_thread_num (1).ne.tn1) e = e + 1
  if (omp_get_ancestor_thread_num (2).ne.tn2) e = e + 1
  if (omp_get_ancestor_thread_num (-1).ne.-1) e = e + 1
  if (omp_get_ancestor_thread_num (3).ne.-1) e = e + 1
  if (omp_get_team_size (0).ne.1) e = e + 1
  if (omp_get_team_size (1).ne.4) e = e + 1
  if (omp_get_team_size (2).ne.1) e = e + 1
  if (omp_get_team_size (-1).ne.-1) e = e + 1
  if (omp_get_team_size (3).ne.-1) e = e + 1
  if (omp_get_active_level ().ne.1) e = e + 1
  !$omp atomic
    e2 = e2 + e
!$omp parallel num_threads (2) firstprivate (tn1, tn2) &
!$omp& private (e, tn3)
  e = 0
  tn3 = omp_get_thread_num ()
  if (.not.omp_in_parallel ()) e = e + 1
  if (omp_get_num_threads ().ne.2) e = e + 1
  if (tn3.lt.0.or.tn3.ge.2) e = e + 1
  if (omp_get_level ().ne.3) e = e + 1
  if (omp_get_ancestor_thread_num (0).ne.0) e = e + 1
  if (omp_get_ancestor_thread_num (1).ne.tn1) e = e + 1
  if (omp_get_ancestor_thread_num (2).ne.tn2) e = e + 1
  if (omp_get_ancestor_thread_num (3).ne.tn3) e = e + 1
  if (omp_get_ancestor_thread_num (-1).ne.-1) e = e + 1
  if (omp_get_ancestor_thread_num (4).ne.-1) e = e + 1
  if (omp_get_team_size (0).ne.1) e = e + 1
  if (omp_get_team_size (1).ne.4) e = e + 1
  if (omp_get_team_size (2).ne.1) e = e + 1
  if (omp_get_team_size (3).ne.2) e = e + 1
  if (omp_get_team_size (-1).ne.-1) e = e + 1
  if (omp_get_team_size (4).ne.-1) e = e + 1
  if (omp_get_active_level ().ne.2) e = e + 1
  !$omp atomic
    e3 = e3 + e
!$omp end parallel
!$omp end parallel
!$omp end parallel
  if (e1.ne.0.or.e2.ne.0.or.e3.ne.0) stop 11
end program nested1
