! { dg-do run }
!
! Test the OpenACC init directive at run time.

use openacc

implicit none

logical :: l
integer :: n

if (acc_get_num_devices (acc_device_host) .ne. 1) stop 1

!$acc init
!$acc shutdown

!$acc init device_type(host)

if (acc_get_device_type () .ne. acc_device_host) stop 2
if (acc_get_device_num (acc_device_host) .ne. 0) stop 3

!$acc shutdown device_type(host)

!$acc init device_type(host) device_num(0)

if (acc_get_device_type () .ne. acc_device_host) stop 4
if (acc_get_device_num (acc_device_host) .ne. 0) stop 5

!$acc shutdown device_type(host)

l = .true.
!$acc init if (l) device_type(host)

if (acc_get_device_type () .ne. acc_device_host) stop 6

!$acc shutdown device_type(host)

n = acc_get_num_devices (acc_device_default)
!$acc init if (.false.) device_num(n)

if (acc_get_num_devices (acc_device_nvidia) .ne. 0) then
  l = .true.
  !$acc init if (l) device_type(nvidia)
  l = .false.
  !$acc init if (l) device_type(host) 
  if (acc_get_device_type () .ne. acc_device_nvidia) stop 7
  !$acc shutdown device_type(nvidia)
end if

if (acc_get_num_devices (acc_device_radeon) .ne. 0) then
  l = .true.
  !$acc init if (l) device_type(radeon)
  l = .false.
  !$acc init if (l) device_type(host) 
  if (acc_get_device_type () .ne. acc_device_radeon) stop 8
  !$acc shutdown device_type(radeon)
end if

end
