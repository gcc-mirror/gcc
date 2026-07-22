! { dg-do run }
!
! Port of lib-4.f90 replacing applicable runtime calls with directives.

program main
  use openacc
  implicit none

  integer n

  if (acc_get_num_devices (acc_device_host) .ne. 1) STOP 1

  if (acc_get_num_devices (acc_device_none) .ne. 0) STOP 2

  !$acc init device_type(host)

  if (acc_get_device_type () .ne. acc_device_host) STOP 3

  !$acc set device_type(host)

  if (acc_get_device_type () .ne. acc_device_host) STOP 4

  n = 0

  !$acc set device_type(host) device_num(n)

  if (acc_get_device_num (acc_device_host) .ne. 0) STOP 5

  if (.NOT. acc_async_test (n) ) STOP 6

  call acc_wait (n)

  call acc_wait_all ()

  !$acc shutdown device_type(host)

end program
