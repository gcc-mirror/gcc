! { dg-set-target-env-var OMP_NUM_TEAMS_ALL "3" }
! { dg-set-target-env-var OMP_NUM_TEAMS_DEV "4" }
! { dg-set-target-env-var OMP_NUM_TEAMS "5" }
! { dg-set-target-env-var OMP_NUM_TEAMS_DEV_0 "6" }
! { dg-set-target-env-var OMP_NUM_TEAMS_DEV_1 "7" }
! { dg-set-target-env-var OMP_NUM_TEAMS_DEV_2 "8" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "2" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV "3" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT "4" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_0 "5" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_1 "6" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_2 "7" }

use omp_lib
implicit none (type, external)
  integer :: num_devices, i, large_num_teams, large_threads_limit
  logical :: err

  if (omp_get_num_devices () > 3) then
    num_devices = 3
  else
    num_devices = omp_get_num_devices ()
  end if

  do i=0,num_devices-1

    ! Testing NUM_TEAMS.
    if (env_is_set_dev ("OMP_NUM_TEAMS_DEV_", i, 6 + i)) then
      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_max_teams () /= 6 + i) err = .true.
      !$omp end target
      if (err) stop 1

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      if (omp_get_num_teams () > 6 + i .or. omp_get_team_num () >= 6 + i) &
        err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 2

      err = .false.
      !$omp target device(i) map(tofrom: err)
      call omp_set_num_teams (5 + i)
      if (omp_get_max_teams () /= 5 + i) err = .true.
      !$omp end target
      if (err) stop 3

      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_max_teams () /= 5 + i) err = .true.
      !$omp end target
      if (err) stop 4

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      if (omp_get_num_teams () > 5 + i .or. omp_get_team_num () >= 5 + i) &
        err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 5

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams num_teams(6 + i)
      if (omp_get_num_teams () > 6 + i .or. omp_get_team_num () >= 6 + i) &
        err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 6

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams num_teams(4 + i)
      if (omp_get_num_teams () > 4 + i .or. omp_get_team_num () >= 4 + i) &
        err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 7

      large_num_teams = 66000
      err = .false.
      !$omp target device(i) map(tofrom: err)
      call omp_set_num_teams (large_num_teams + i)
      if (omp_get_max_teams () /= large_num_teams + i) err = .true.
      !$omp end target
      if (err) stop 8

      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_max_teams () /= large_num_teams + i) err = .true.
      !$omp end target
      if (err) stop 9

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      if (omp_get_num_teams () > large_num_teams + i &
          .or. omp_get_team_num () >= large_num_teams + i) err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 10
    end if

    ! Testing TEAMS-THREAD-LIMIT
    if (env_is_set_dev ("OMP_TEAMS_THREAD_LIMIT_DEV_", i, 5 + i)) then
      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_teams_thread_limit () /= 5 + i) err = .true.
      !$omp end target
      if (err) stop 11

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      !$omp parallel
      if (omp_get_thread_limit () > 5 + i .or. omp_get_thread_num () >= 5 + i) &
        err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 12

      err = .false.
      !$omp target device(i) map(tofrom: err)
      call omp_set_teams_thread_limit (4 + i)
      if (omp_get_teams_thread_limit () /= 4 + i) err = .true.
      !$omp end target
      if (err) stop 13

      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_teams_thread_limit () /= 4 + i) err = .true.
      !$omp end target
      if (err) stop 14

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      !$omp parallel
      if (omp_get_thread_limit () > 4 + i .or. omp_get_thread_num () >= 4 + i) &
        err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 15

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams thread_limit(5 + i)
      !$omp parallel
      if (omp_get_thread_limit () > 5 + i .or. omp_get_thread_num () >= 5 + i) &
        err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 16

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams thread_limit(3 + i)
      !$omp parallel
      if (omp_get_thread_limit () > 3 + i .or. omp_get_thread_num () >= 3 + i) &
        err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 17

      large_threads_limit = 67000
      err = .false.
      !$omp target device(i) map(tofrom: err)
      call omp_set_teams_thread_limit (large_threads_limit + i)
      if (omp_get_teams_thread_limit () /= large_threads_limit + i) err = .true.
      !$omp end target
      if (err) stop 18

      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_teams_thread_limit () /= large_threads_limit + i) err = .true.
      !$omp end target
      if (err) stop 19

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      !$omp parallel
      if (omp_get_thread_limit () > large_threads_limit + i &
          .or. omp_get_thread_num () >= large_threads_limit + i) err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 20
    end if

  end do

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
      error stop 30
    endif
    env_is_set = .false.
  end
  logical function env_is_set_dev (name, dev_num, val)
    character(len=*) :: name
    integer :: dev_num, val
    character(len=64) :: dev_num_str, env_var, val_str
    dev_num_str = ADJUSTL(dev_num_str)
    env_var = name // dev_num_str
    val_str = ADJUSTL(val_str)
    env_is_set_dev = env_is_set (TRIM(env_var), TRIM(val_str))
  end
end
