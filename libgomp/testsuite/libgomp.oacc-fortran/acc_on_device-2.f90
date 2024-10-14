! { dg-do link }

! Check whether 'acc_on_device()' is properly compile-time optimized. */

! { dg-additional-options "-fdump-tree-gimple -fdump-tree-optimized" }
! { dg-additional-options -foffload-options=-fdump-tree-optimized { target { offload_target_nvptx || offload_target_amdgcn } } }

! { dg-final { scan-tree-dump-times "acc_on_device" 1 "gimple" } }

! { dg-final { scan-tree-dump-not "acc_on_device" "optimized" } }

! { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump-not "acc_on_device" "optimized" { target offload_target_amdgcn } } }
! { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-not "acc_on_device" "optimized" { target offload_target_nvptx } } }


module m
   integer :: xxxx
   !$acc declare device_resident(xxxx)
contains
  subroutine set_var
    !$acc routine
    use openacc
    implicit none (type, external)
    if (acc_on_device(acc_device_host)) then
      xxxx = 1234
    else
      xxxx = 4242
    end if
  end
end module m


program main
  use m
  call set_var
  !$acc serial
    ! { dg-warning "using 'vector_length \\(32\\)', ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
    call set_var
  !$acc end serial
end
