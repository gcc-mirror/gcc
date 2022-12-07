! { dg-set-target-env-var OMP_NUM_TEAMS_ALL "3" }
! { dg-set-target-env-var OMP_NUM_TEAMS_DEV "4" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "2" }
! { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV "3" }

! This test considers the hierarchical usage of ICVs on the device, i.e. if
! e.g. OMP_NUM_TEAMS_DEV_<device_num> is not configured, then the value of
! OMP_NUM_TEAMS_DEV should be used for the targets.

use omp_lib
implicit none (type, external)
  integer :: num_devices, i, stat, tmp
  logical :: err
  character(len=40) :: val

  ! The following environment variables should not be set.
  call get_environment_variable ("OMP_NUM_TEAMS_DEV_0", val, status=stat)
  if (stat /= 1) return
  call get_environment_variable ("OMP_NUM_TEAMS_DEV_1", val, status=stat)
  if (stat /= 1) return
  call get_environment_variable ("OMP_NUM_TEAMS_DEV_2", val, status=stat)
  if (stat /= 1) return
  call get_environment_variable ("OMP_TEAMS_THREAD_LIMIT_DEV_0", val, status=stat)
  if (stat /= 1) return
  call get_environment_variable ("OMP_TEAMS_THREAD_LIMIT_DEV_1", val, status=stat)
  if (stat /= 1) return
  call get_environment_variable ("OMP_TEAMS_THREAD_LIMIT_DEV_2", val, status=stat)
  if (stat /= 1) return

  if (omp_get_num_devices () > 3) then
    num_devices = 3
  else
    num_devices = omp_get_num_devices ()
  end if

  do i=0,num_devices-1

    ! Testing NUM_TEAMS.
    if (env_is_set ("OMP_NUM_TEAMS_DEV", "4")) then
      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_max_teams () /= 4) err = .true.
      !$omp end target
      if (err) stop 1

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      if (omp_get_num_teams () > 4 .or. omp_get_team_num () >= 4) &
        err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 2

      err = .false.
      !$omp target device(i) map(tofrom: err)
      call omp_set_num_teams (3 + i)
      if (omp_get_max_teams () /= 3 + i) err = .true.
      !$omp end target
      if (err) stop 3

      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_max_teams () /= 3 + i) err = .true.
      !$omp end target
      if (err) stop 4

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      if (omp_get_num_teams () > 3 + i .or. omp_get_team_num () >= 3 + i) &
        err = .true.
      !$omp end teams
      !$omp end target
      if (err) stop 5
    end if

    ! Testing TEAMS-THREAD-LIMIT
    if (env_is_set ("OMP_TEAMS_THREAD_LIMIT_DEV", "3")) then
      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_teams_thread_limit () /= 3) err = .true.
      !$omp end target
      if (err) stop 6

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      !$omp parallel
      if (omp_get_thread_limit () > 3 .or. omp_get_thread_num () >= 3) &
        err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 7

      err = .false.
      !$omp target device(i) map(tofrom: err)
      call omp_set_teams_thread_limit (2 + i)
      if (omp_get_teams_thread_limit () /= 2 + i) err = .true.
      !$omp end target
      if (err) stop 8

      err = .false.
      !$omp target device(i) map(tofrom: err)
      if (omp_get_teams_thread_limit () /= 2 + i) err = .true.
      !$omp end target
      if (err) stop 9

      err = .false.
      !$omp target device(i) map(tofrom: err)
      !$omp teams
      !$omp parallel
      if (omp_get_thread_limit () > 2 + i .or. omp_get_thread_num () >= 2 + i) &
        err = .true.
      !$omp end parallel
      !$omp end teams
      !$omp end target
      if (err) stop 10
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
      error stop 10
    endif
    env_is_set = .false.
  end
end
