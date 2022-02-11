use omp_lib
implicit none (type, external)
  if (.not. env_exists ("OMP_NUM_TEAMS") &
      .and. omp_get_max_teams () /= 0) &
    error stop 1
  call omp_set_num_teams (7)
  if (omp_get_max_teams () /= 7) &
    error stop 2
  if (.not. env_exists ("OMP_TEAMS_THREAD_LIMIT") &
      .and. omp_get_teams_thread_limit () /= 0) &
    error stop 3
  call omp_set_teams_thread_limit (15)
  if (omp_get_teams_thread_limit () /= 15) &
    error stop 4
  !$omp teams
   !$omp parallel if(.false.)
    if (omp_get_max_teams () /= 7 &
        .or. omp_get_teams_thread_limit () /= 15 &
        .or. omp_get_num_teams () < 1 &
        .or. omp_get_num_teams () > 7 &
        .or. omp_get_team_num () < 0 &
        .or. omp_get_team_num () >= omp_get_num_teams () &
        .or. omp_get_thread_limit () < 1 &
        .or. omp_get_thread_limit () > 15) &
      error stop 5
   !$omp end parallel
  !$omp end teams
  !$omp teams num_teams(5) thread_limit (13)
   !$omp parallel if(.false.)
    if (omp_get_max_teams () /= 7 &
        .or. omp_get_teams_thread_limit () /= 15 &
        .or. omp_get_num_teams () /= 5 &
        .or. omp_get_team_num () < 0 &
        .or. omp_get_team_num () >= omp_get_num_teams () &
        .or. omp_get_thread_limit () < 1 &
        .or. omp_get_thread_limit () > 13) &
      error stop 6
   !$omp end parallel
  !$omp end teams
  !$omp teams num_teams(8) thread_limit (16)
   !$omp parallel if(.false.)
    if (omp_get_max_teams () /= 7 &
        .or. omp_get_teams_thread_limit () /= 15 &
        .or. omp_get_num_teams () /= 8 &
        .or. omp_get_team_num () < 0 &
        .or. omp_get_team_num () >= omp_get_num_teams () &
        .or. omp_get_thread_limit () < 1 &
        .or. omp_get_thread_limit () > 16) &
      error stop 7
   !$omp end parallel
  !$omp end teams
contains
  logical function env_exists (name)
    character(len=*) :: name
    character(len=40) :: val
    integer :: stat
    call get_environment_variable (name, val, status=stat)
    if (stat == 0) then
      env_exists = .true.
    else if (stat == 1) then
      env_exists = .false.
    else
      error stop 10
    endif
  end
end
