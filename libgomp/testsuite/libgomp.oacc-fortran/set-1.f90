! { dg-do run }
!
! Test the OpenACC set directive at run time.

use openacc

implicit none

logical :: l
integer :: n

if (acc_get_num_devices (acc_device_host) .ne. 1) stop 1

call acc_init (acc_device_host)

!$acc set device_type(host)

if (acc_get_device_type () .ne. acc_device_host) stop 2


! This set device_num(0) call init all the devices,
! so we can skip the init call later

!$acc set device_num(0)

if (acc_get_device_num (acc_device_host) .ne. 0) stop 3

!$acc set device_type(host) device_num(0)

if (acc_get_device_type () .ne. acc_device_host) stop 4
if (acc_get_device_num (acc_device_host) .ne. 0) stop 5

l = .true.
!$acc set if (l) device_type(host)

if (acc_get_device_type () .ne. acc_device_host) stop 6

l = .false.
n = 1
!$acc set if (l) device_num(n)

if (acc_get_device_num (acc_device_host) .ne. 0) stop 7

call acc_shutdown (acc_device_host)

if (acc_get_num_devices (acc_device_nvidia) .ne. 0) then

  !$acc set device_type(nvidia)

  if (acc_get_device_type () .ne. acc_device_nvidia) stop 11

  n = 0
  !$acc set device_type(nvidia) device_num(n)

  if (acc_get_device_num (acc_device_nvidia) .ne. 0) stop 12

  l = .false.
  !$acc set if (l) device_type(host)
  if (acc_get_device_type () .ne. acc_device_nvidia) stop 13

  call acc_shutdown (acc_device_nvidia)
end if

if (acc_get_num_devices (acc_device_radeon) .ne. 0) then

  !$acc set device_type(radeon)

  if (acc_get_device_type () .ne. acc_device_radeon) stop 21

  n = 0
  !$acc set device_type(radeon) device_num(n)

  if (acc_get_device_num (acc_device_radeon) .ne. 0) stop 22

  l = .false.
  !$acc set if (l) device_type(host)
  if (acc_get_device_type () .ne. acc_device_radeon) stop 23

  call acc_shutdown (acc_device_radeon)
end if

end
