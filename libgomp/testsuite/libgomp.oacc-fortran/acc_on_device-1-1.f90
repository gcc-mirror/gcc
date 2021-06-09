! { dg-do run }
! { dg-additional-options "-cpp" }

! { dg-additional-options "-fopt-info-all-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-all-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

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
if (acc_on_device (acc_device_radeon)) STOP 4


! Host via offloading fallback mode.

!$acc parallel if(.false.)
! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } .-1 }
!TODO Unhandled 'CONST_DECL' instances for constant arguments in 'acc_on_device' calls.
if (.not. acc_on_device (acc_device_none)) STOP 5
if (.not. acc_on_device (acc_device_host)) STOP 6
if (acc_on_device (acc_device_not_host)) STOP 7
if (acc_on_device (acc_device_nvidia)) STOP 8
if (acc_on_device (acc_device_radeon)) STOP 8
!$acc end parallel


#if !ACC_DEVICE_TYPE_host

! Offloaded.

!$acc parallel
! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target { ! openacc_host_selected } } .-1 }
if (acc_on_device (acc_device_none)) STOP 9
if (acc_on_device (acc_device_host)) STOP 10
if (.not. acc_on_device (acc_device_not_host)) STOP 11
#if ACC_DEVICE_TYPE_nvidia
if (.not. acc_on_device (acc_device_nvidia)) STOP 12
#else
if (acc_on_device (acc_device_nvidia)) STOP 13
#endif
#if ACC_DEVICE_TYPE_radeon
if (.not. acc_on_device (acc_device_radeon)) STOP 14
#else
if (acc_on_device (acc_device_radeon)) STOP 15
#endif
!$acc end parallel

#endif

end
