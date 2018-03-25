! { dg-do run }

program main
  implicit none
  include "openacc_lib.h"

  integer n

  if (acc_get_num_devices (acc_device_nvidia) .eq. 0) call exit

  call acc_init (acc_device_nvidia)

  n = 0

  call acc_set_device_num (n, acc_device_nvidia)

  if (acc_get_device_num (acc_device_nvidia) .ne. 0) STOP 1

  if (acc_get_num_devices (acc_device_nvidia) .gt. 1) then

    n = 1

    call acc_set_device_num (n, acc_device_nvidia)

    if (acc_get_device_num (acc_device_nvidia) .ne. 1) STOP 2

  end if

  call acc_shutdown (acc_device_nvidia)

end program
