!
! Ensure that a mapping with no argument works
!

! { dg-additional-options -foffload-options=nvptx-none=-misa=sm_35 { target offload_target_nvptx } }

module m
  implicit none (type, external)
  integer :: x = 32
  integer :: dev_num2 = -1
contains
subroutine  foo()
  use omp_lib, only: omp_get_device_num
  x = x + 10
  dev_num2 = omp_get_device_num()
end
end module m

use m
use omp_lib
!$omp requires reverse_offload
implicit none (type, external)
integer :: dev_num = -1
!$omp target map(from:dev_num)
  dev_num = omp_get_device_num()
  ! This calls GOMP_target_ext with number of maps = 0
  !$omp target device(ancestor:1)
    call foo
  !$omp end target
!$omp end target

if (omp_get_num_devices() > 0 .and.  dev_num2 == dev_num) stop 1
if (x /= 42) stop 2
end
