! { dg-do run }

program main
  use openacc
  implicit none

  integer n

  if (acc_get_num_devices (acc_device_host) .ne. 1) STOP 1

  if (acc_get_num_devices (acc_device_none) .ne. 0) STOP 2

  call acc_init (acc_device_host)

  if (acc_get_device_type () .ne. acc_device_host) STOP 3

  call acc_set_device_type (acc_device_host)

  if (acc_get_device_type () .ne. acc_device_host) STOP 4

  n = 0

  call acc_set_device_num (n, acc_device_host)

  if (acc_get_device_num (acc_device_host) .ne. 0) STOP 5

  if (.NOT. acc_async_test (n) ) STOP 6

  call acc_wait (n)

  call acc_wait_all ()

  call acc_shutdown (acc_device_host)

end program
