! Check whether 'omp_is_initial_device()' is properly compile-time optimized. */

! { dg-additional-options "-fdump-tree-gimple -fdump-tree-optimized" }
! { dg-additional-options -foffload-options=-fdump-tree-optimized { target { offload_target_nvptx || offload_target_amdgcn } } }

! { dg-final { scan-tree-dump-times "omp_is_initial_device" 1 "gimple" } }

! { dg-final { scan-tree-dump-not "omp_is_initial_device" "optimized" } }

! { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump-not "omp_is_initial_device" "optimized" { target offload_target_amdgcn } } }
! { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-not "omp_is_initial_device" "optimized" { target offload_target_nvptx } } }


program main
  use omp_lib
  implicit none (type, external)
  integer :: dev_num, initial, dev
  logical :: is_initial

  initial = omp_get_initial_device()
  do dev = omp_initial_device, omp_get_num_devices()
      dev_num = 99
      !$omp target map(from: is_initial, dev_num) device(dev)
        is_initial = omp_is_initial_device ()
        dev_num = omp_get_device_num ()
      !$omp end target
      if (dev == omp_initial_device .or. dev == initial) then
        if (dev_num /= initial .or. .not. is_initial) &
          stop 1
      else
        if (dev_num /= dev .or. is_initial) &
          stop 2
      end if
  end do
end
