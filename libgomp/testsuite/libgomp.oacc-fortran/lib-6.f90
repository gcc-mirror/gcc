! { dg-do run }

program main
  implicit none
  include "openacc_lib.h"

  integer n

  if (acc_get_num_devices (acc_device_host) .ne. 1) call abort

  if (acc_get_num_devices (acc_device_none) .ne. 0) call abort

  call acc_init (acc_device_host)

  if (acc_get_device_type () .ne. acc_device_host) call abort

  call acc_set_device_type (acc_device_host)

  if (acc_get_device_type () .ne. acc_device_host) call abort

  n = 0

  call acc_set_device_num (n, acc_device_host)

  if (acc_get_device_num (acc_device_host) .ne. 0) call abort

  if (.NOT. acc_async_test (n) ) call abort

  call acc_wait (n)

  call acc_wait_all ()

  call acc_shutdown (acc_device_host)

end program
