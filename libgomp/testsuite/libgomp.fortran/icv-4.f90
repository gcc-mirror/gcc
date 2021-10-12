! { dg-set-target-env-var OMP_NUM_TEAMS "6" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT "12" }

use omp_lib
implicit none (type, external)
  if (env_is_set ("OMP_NUM_TEAMS", "6")) then
    if (omp_get_max_teams () /= 6) &
      error stop 1
  else
    call omp_set_num_teams (6)
  end if
  if (env_is_set ("OMP_TEAMS_THREAD_LIMIT", "12")) then
    if (omp_get_teams_thread_limit () /= 12) &
      error stop 2
  else
    call omp_set_teams_thread_limit (12)
  end if
  !$omp teams
    if (omp_get_max_teams () /= 6 &
        .or. omp_get_teams_thread_limit () /= 12 &
        .or. omp_get_num_teams () < 1 &
        .or. omp_get_num_teams () > 6 &
        .or. omp_get_team_num () < 0 &
        .or. omp_get_team_num () >= omp_get_num_teams () &
        .or. omp_get_thread_limit () < 1 &
        .or. omp_get_thread_limit () > 12) &
      error stop 3
  !$omp end teams
contains
  logical function env_is_set (name, val)
    character(len=*) :: name, val
    character(len=40) :: val2
    integer :: stat
    call get_environment_variable (name, val2, status=stat)
    if (stat == 0) then
      if (val == val2) then
        env_is_set = .true.
        return
      end if
    else if (stat /= 1) then
      error stop 10
    endif
    env_is_set = .false.
  end
end
