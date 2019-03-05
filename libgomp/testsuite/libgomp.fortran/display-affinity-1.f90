! { dg-set-target-env-var OMP_PROC_BIND "spread,close" }
! { dg-set-target-env-var OMP_PLACES "cores" }
! { dg-set-target-env-var OMP_NUM_THREADS "4" }
! { dg-set-target-env-var OMP_AFFINITY_FORMAT "hello" }

  use omp_lib
  character(len=68) :: buf, buf2
  character(len=8) :: buf3
  character(len=1) :: buf4
  integer :: l1, l2

  buf = 'L:%0.5L%%%n>%32H<!%.33{host}!%.6P_%i_%0.18i_%0.7{ancestor_tnum} %18A'
  call omp_set_affinity_format (format = buf)
  if (omp_get_affinity_format (buf4) /= 68) stop 1
  if (buf4 /= 'L') stop 2
  if (omp_get_affinity_format (buf2) /= 68) stop 3
  if (buf2 /= buf) stop 4
  if (omp_get_affinity_format (buf3) /= 68) stop 5
  if (buf3 /= 'L:%0.5L%') stop 6
  call omp_display_affinity ('')
  call omp_display_affinity ('%%%0.9N')
  l1 = omp_capture_affinity (buf4, '%0.5{nesting_level}%%|||%0.7a%3N!%N!')
  buf = '%.5L%%|||%0.7{ancestor_tnum}%3{num_threads}!%{num_threads}!'
  call omp_set_affinity_format (trim (buf))
  l2 = omp_capture_affinity (buf2, '')
  if (l1 /= l2) stop 7
  if (l1 /= 22) stop 8
  if (buf2 /= '    0%|||-0000011  !1!') stop 9
  if (buf4 /= '0') stop 10
!$omp parallel num_threads (4) proc_bind(spread)
  call omp_display_affinity ('%0.2a!%n!%.4L!%N;%.2t;%0.2T;&
                             &%{team_num};%{num_teams};%A')
!$omp end parallel
end
