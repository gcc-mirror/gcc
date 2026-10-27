module m
  use omp_lib
  implicit none (type, external)
contains
subroutine test_it (dev)
  integer, value :: dev

  ! Invalid access argument - expect 0
  if (omp_get_gprivate_limit (dev, -1_omp_access_kind) /= 0) &
    stop 1

  ! Invalid access argument - expect 0
  if (omp_get_gprivate_limit (dev, 99_omp_access_kind) /= 0) &
    stop 2

  ! FIXME: cgroup - should be valid in principle:
  ! - offload: groupprivate permitted for offloading,
  !            even if dyn_groupprivate is not implemented.
  ! - host: dyn_groupprivate implementable, but not implemented.
  !
  if (dev == omp_invalid_device &
      .and. omp_get_gprivate_limit (dev, omp_access_cgroup) /= 0) then
    stop 3
  else if (dev /= omp_invalid_device  &  ! For now, expect 0
           .and. omp_get_gprivate_limit (dev, omp_access_cgroup) /= 0) then
    stop 4
  end if

  ! pteam - enum value defined but otherwise not specified at all!
  ! but in principle the limit question could be answered.
  !
  ! Still, for now assume == 0.
  if (omp_get_gprivate_limit (dev, omp_access_pteam) /= 0) &
    stop 5

  ! access is optional and defaults to 'omp_access_cgroup'.
  if (omp_get_gprivate_limit (dev, omp_access_cgroup) &
      /= omp_get_gprivate_limit (dev)) &
    stop 6
end
end

program main
  use m
  implicit none (type, external)
  integer :: dev
  ! avoid - will exit after printing: libgomp: omp_invalid_device encountered
  ! call test_it (omp_invalid_device)
  call test_it (omp_default_device)

  do dev = omp_initial_device, omp_get_num_devices ()
    call test_it (dev)
  end do
end
