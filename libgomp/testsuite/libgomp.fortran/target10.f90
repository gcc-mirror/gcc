! { dg-do run }
! { dg-xfail-run-if TODO { offload_device_intel_mic } }

program main
  use omp_lib
  implicit none
  integer :: device_num, host_device_num
  logical :: initial_device

  host_device_num = omp_get_device_num ()
  if (host_device_num .ne. omp_get_initial_device ()) stop 1

  !$omp target map(from: device_num, initial_device)
  initial_device = omp_is_initial_device ()
  device_num = omp_get_device_num ()
  !$omp end target

  if (initial_device .and. (host_device_num .ne. device_num)) stop 2
  if ((.not. initial_device) .and. (host_device_num .eq. device_num)) stop 3

end program main
