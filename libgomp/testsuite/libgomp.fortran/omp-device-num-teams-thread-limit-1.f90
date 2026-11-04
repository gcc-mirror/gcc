! This test assumes that no environment variable changes the default!
! i.e. neither NUM_TEAMS (> 0) is set nor OMP_TEAMS_THREAD_LIMIT.

program main
  use omp_lib
  implicit none (type, external)

  integer :: nteams, tlimit
  integer :: ndevs, dev, i

  ndevs = omp_get_num_devices ()

  if (omp_default_device < -10 .or. omp_invalid_device < -10) &
    stop 1

  do dev = -10, 10
    block
      logical :: is_host, is_invalid

      is_host = (dev == omp_initial_device  &
                 .or. dev == ndevs  &
                 .or. (dev == omp_default_device  &
                       .and. (omp_get_default_device() == omp_initial_device  &
                              .or. omp_get_default_device() == ndevs)))
      is_invalid = (dev /= omp_default_device  &
                    .and. dev /= omp_initial_device  &
                    .and. (dev < 0 .or. dev > ndevs))

      nteams = omp_get_device_num_teams (dev)
      tlimit = omp_get_device_teams_thread_limit (dev)

      if (is_host  &
          .and. (tlimit /= omp_get_teams_thread_limit ()  &
                 .or. tlimit /= omp_get_device_teams_thread_limit (dev))) &
        stop 2

      if (is_invalid) then
        if (nteams /= 0 .or. tlimit /= 0) &
          stop 3
      else if (is_host .and. nteams /= 1) then  ! outside 'teams'
        stop 4
      else if (.not. is_host .and. nteams /= 0) then  ! default init = 0 (according to the spec)
        stop 5
      end if

      if (.not. is_invalid) then
        !$omp target firstprivate(nteams, tlimit) device(dev)
          if (tlimit /= omp_get_teams_thread_limit ()) &
            stop 6
        !$omp end target
      end if
    end block
  end do

  call omp_set_default_device (omp_initial_device)

  do i = 0, 3
  block
    integer :: val1, val2

    val1 = 4 + i
    val2 = 3 + i

    select case (i)
      case(0); call omp_set_num_teams(val1)
      case(1); call omp_set_device_num_teams (val1, omp_initial_device)
      case(2); call omp_set_device_num_teams (val1, omp_default_device)
      case(3); call omp_set_device_num_teams (val1, ndevs)
    end select

    !$omp teams
      !$omp parallel if(.false.)
        if (val1 /= omp_get_num_teams ()) &
            stop 7
        if (val1 /= omp_get_device_num_teams (omp_initial_device)) &
            stop 8
        if (val1 /= omp_get_device_num_teams (omp_default_device)) &
            stop 9
        if (val1 /= omp_get_device_num_teams (ndevs)) &
            stop 10
      !$omp end parallel
    !$omp end teams

    select case(i)
      case(0); call omp_set_teams_thread_limit (val2)
      case(1); call omp_set_device_teams_thread_limit (val2, omp_initial_device)
      case(2); call omp_set_device_teams_thread_limit (val2, omp_default_device)
      case(3); call omp_set_device_teams_thread_limit (val2, ndevs)
    end select

      if (val2 /= omp_get_teams_thread_limit ()) &
        stop 11
      if (val2 /= omp_get_device_teams_thread_limit (omp_initial_device)) &
        stop 12
      if (val2 /= omp_get_device_teams_thread_limit (omp_default_device)) &
        stop 13
      if (val2 /= omp_get_device_teams_thread_limit (ndevs)) &
        stop 14
  end block
  end do

  do dev = 0, ndevs - 1
    call omp_set_device_num_teams (5, dev)
    if (5 /= omp_get_device_num_teams (dev)) &
      stop 15

    !$omp target teams device(dev)
      if (5 /= omp_get_num_teams ()) &
        stop 16
    !$omp end target teams

    call omp_set_device_teams_thread_limit (7, dev)

    if (7 /= omp_get_device_teams_thread_limit (dev)) &
      stop 17

    !$omp target teams device(dev) num_teams(1)
      !$omp parallel if(.false.)
        if (7 /= omp_get_teams_thread_limit ()) &
          stop 18
      !$omp end parallel
    !$omp end target teams
  end do

  do dev = omp_initial_device, ndevs
    block
      logical :: is_host

      is_host = (dev == omp_initial_device .or. dev == ndevs)

      call omp_set_device_teams_thread_limit (8, dev)

      if (omp_get_device_teams_thread_limit (dev) /= 8) &
        stop 19
      if (is_host .and. omp_get_teams_thread_limit () /= 8) &
        stop 19

      ! Setting to value -1 is not permitted, hence, expect old value:
      call omp_set_device_teams_thread_limit (-1, dev)

      if (omp_get_device_teams_thread_limit (dev) /= 8) &
        stop 20
      if (is_host .and. omp_get_teams_thread_limit () /= 8) &
        stop 21

      ! Likewise.
      if (is_host) then
        call omp_set_teams_thread_limit (-1)
        if (omp_get_device_teams_thread_limit (dev) /= 8) &
          stop 22
        if (is_host .and. omp_get_teams_thread_limit () /= 8) &
          stop 23
      end if

      ! Let's try value = 0. This value is well defined (to mean implementation
      ! defined) but not permitted. Hence, permit both results:
      call omp_set_device_teams_thread_limit (0, dev)
      if (omp_get_device_teams_thread_limit (dev) /= 8 &
          .and. omp_get_device_teams_thread_limit (dev) /= 0) &
        stop 24
      if (is_host &
          .and. omp_get_teams_thread_limit () /= 8 &
          .and. omp_get_teams_thread_limit () /= 0) &
        stop 25

      ! Likewise.
      if (is_host) then
        call omp_set_teams_thread_limit (0)
        if (omp_get_device_teams_thread_limit (dev) /= 8 &
            .and.  omp_get_device_teams_thread_limit (dev) /= 0) &
          stop 26
        if (is_host &
            .and. omp_get_teams_thread_limit () /= 8 &
            .and. omp_get_teams_thread_limit () /= 0) &
          stop 27
      end if

      ! However, the nteams-ICV can be set to zero,
      ! but we cannot check its value on the host as
      ! omp_get_num_teams gives the effective team size not the ICV value.
      if (is_host) &
        cycle
      
      call omp_set_device_num_teams (8, dev)
      if (omp_get_device_num_teams (dev) /= 8) &
        stop 28

      ! Setting to value 0 is permitted, hence, expect that value:
      call omp_set_device_num_teams (0, dev)
      if (omp_get_device_num_teams (dev) /= 0) &
        stop 29
    end block
  end do
end program
