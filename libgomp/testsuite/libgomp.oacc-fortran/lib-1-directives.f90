! { dg-do run }
!
! Port of lib-1.f90 replacing applicable runtime calls with directives.

use openacc

if (acc_get_num_devices (acc_device_host) .ne. 1) STOP 1
!$acc set device_type(host)
if (acc_get_device_type () .ne. acc_device_host) STOP 2
!$acc set device_type(host) device_num(0)
if (acc_get_device_num (acc_device_host) .ne. 0) STOP 3
!$acc shutdown device_type(host)

!$acc init device_type(host)
!$acc shutdown device_type(host)

end
