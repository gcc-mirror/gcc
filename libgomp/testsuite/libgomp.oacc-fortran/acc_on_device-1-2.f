! { dg-do run }
! { dg-additional-options "-cpp" }

! Disable the acc_on_device builtin; we want to test the libgomp library function.
! { dg-additional-options -fno-builtin-acc_on_device }

! { dg-additional-options "-fopt-info-all-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-all-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

      USE OPENACC
      IMPLICIT NONE

!Host.

      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NONE)) STOP 1
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_HOST)) STOP 2
      IF (ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) STOP 3
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 4
      IF (ACC_ON_DEVICE (ACC_DEVICE_RADEON)) STOP 4


!Host via offloading fallback mode.

!$ACC PARALLEL IF(.FALSE.)
! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } .-1 }
!TODO Unhandled 'CONST_DECL' instances for constant arguments in 'acc_on_device' calls.
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NONE)) STOP 5
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_HOST)) STOP 6
      IF (ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) STOP 7
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 8
      IF (ACC_ON_DEVICE (ACC_DEVICE_RADEON)) STOP 8
!$ACC END PARALLEL


#if !ACC_DEVICE_TYPE_host

! Offloaded.

!$ACC PARALLEL
! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target { ! openacc_host_selected } } .-1 }
      IF (ACC_ON_DEVICE (ACC_DEVICE_NONE)) STOP 9
      IF (ACC_ON_DEVICE (ACC_DEVICE_HOST)) STOP 10
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NOT_HOST)) STOP 11
#if ACC_DEVICE_TYPE_nvidia
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 12
#else
      IF (ACC_ON_DEVICE (ACC_DEVICE_NVIDIA)) STOP 13
#endif
#if ACC_DEVICE_TYPE_radeon
      IF (.NOT. ACC_ON_DEVICE (ACC_DEVICE_RADEON)) STOP 14
#else
      IF (ACC_ON_DEVICE (ACC_DEVICE_RADEON)) STOP 15
#endif
!$ACC END PARALLEL

#endif

      END
