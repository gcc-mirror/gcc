! { dg-do run }
! { dg-additional-options "-cpp" }
!
! TODO: Have to disable the acc_on_device builtin for we want to test
! the libgomp library function?  The command line option
! '-fno-builtin-acc_on_device' is valid for C/C++/ObjC/ObjC++ but not
! for Fortran.

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

!Host.

      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NONE)) STOP 1
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_HOST)) STOP 2
      IF (ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) STOP 3
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 4


!Host via offloading fallback mode.

!$ACC PARALLEL IF(.FALSE.)
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NONE)) STOP 5
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_HOST)) STOP 6
      IF (ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) STOP 7
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 8
!$ACC END PARALLEL


#if !ACC_DEVICE_TYPE_host

! Offloaded.

!$ACC PARALLEL
      IF (ACC_ON_DEVICE (ACC_DEVICE_NONE)) STOP 9
      IF (ACC_ON_DEVICE (ACC_DEVICE_HOST)) STOP 10
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) STOP 11
#if ACC_DEVICE_TYPE_nvidia
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 12
#else
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 13
#endif
!$ACC END PARALLEL

#endif

      END
