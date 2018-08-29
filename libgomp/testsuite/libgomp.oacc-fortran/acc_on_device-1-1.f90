! { dg-additional-options "-cpp" }
! TODO: Have to disable the acc_on_device builtin for we want to test the
! libgomp library function?  The command line option
! '-fno-builtin-acc_on_device' is valid for C/C++/ObjC/ObjC++ but not for
! Fortran.

use openacc
implicit none

! Host.

if (.not. acc_on_device (acc_device_none)) STOP 1
if (.not. acc_on_device (acc_device_host)) STOP 2
if (acc_on_device (acc_device_not_host)) STOP 3
if (acc_on_device (acc_device_nvidia)) STOP 4


! Host via offloading fallback mode.

!$acc parallel if(.false.)
if (.not. acc_on_device (acc_device_none)) STOP 5
if (.not. acc_on_device (acc_device_host)) STOP 6
if (acc_on_device (acc_device_not_host)) STOP 7
if (acc_on_device (acc_device_nvidia)) STOP 8
!$acc end parallel


#if !ACC_DEVICE_TYPE_host

! Offloaded.

!$acc parallel
if (acc_on_device (acc_device_none)) STOP 9
if (acc_on_device (acc_device_host)) STOP 10
if (.not. acc_on_device (acc_device_not_host)) STOP 11
#if ACC_DEVICE_TYPE_nvidia
if (.not. acc_on_device (acc_device_nvidia)) STOP 12
#else
if (acc_on_device (acc_device_nvidia)) STOP 13
#endif
!$acc end parallel

#endif

end
