! { dg-do run }

program main
  use omp_lib
  implicit none
  integer :: device_num, host_device_num, i
  logical :: initial_device

  host_device_num = omp_get_device_num ()
  if (host_device_num .ne. omp_get_initial_device ()) stop 1

  do i = 0, omp_get_num_devices ()
    !$omp target map(from: device_num, initial_device) device(i)
      initial_device = omp_is_initial_device ()
      device_num = omp_get_device_num ()
    !$omp end target
    if (i /= device_num) stop 2
    if (initial_device .and. (host_device_num .ne. device_num)) stop 3
    if ((.not. initial_device) .and. (host_device_num .eq. device_num)) stop 4
  end do

end program main
