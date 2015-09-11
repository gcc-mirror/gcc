! { dg-additional-options "-cpp" }
! TODO: Have to disable the acc_on_device builtin for we want to test
! the libgomp library function?  The command line option
! '-fno-builtin-acc_on_device' is valid for C/C++/ObjC/ObjC++ but not
! for Fortran.

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

!Host.

      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NONE)) CALL ABORT
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_HOST)) CALL ABORT
      IF (ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) CALL ABORT
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) CALL ABORT


!Host via offloading fallback mode.

!$ACC PARALLEL IF(.FALSE.)
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NONE)) CALL ABORT
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_HOST)) CALL ABORT
      IF (ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) CALL ABORT
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) CALL ABORT
!$ACC END PARALLEL


#if !ACC_DEVICE_TYPE_host

! Offloaded.

!$ACC PARALLEL
      IF (ACC_ON_DEVICE (ACC_DEVICE_NONE)) CALL ABORT
      IF (ACC_ON_DEVICE (ACC_DEVICE_HOST)) CALL ABORT
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) CALL ABORT
#if ACC_DEVICE_TYPE_nvidia
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) CALL ABORT
#else
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) CALL ABORT
#endif
!$ACC END PARALLEL

#endif

      END
