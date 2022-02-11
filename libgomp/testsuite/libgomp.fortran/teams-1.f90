program main
  use omp_lib
  implicit none (type, external)
  integer :: i

  !$omp teams num_teams (5)
    if (omp_get_num_teams () /= 5) stop 1
    !$omp distribute dist_schedule(static,1)
    do i = 0, 4
      if (omp_get_team_num () /= i) stop 2
    end do
  !$omp end teams

  !$omp teams num_teams (7 : 9)
    if (omp_get_num_teams () < 7 .or. omp_get_num_teams () > 9) &
      stop 3
    !$omp distribute dist_schedule(static,1)
    do i = 0, omp_get_num_teams () - 1
      if (omp_get_team_num () /= i) stop 4
    end do
  !$omp end teams
end program main
