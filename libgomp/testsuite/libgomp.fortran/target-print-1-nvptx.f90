! Ensure that write on the offload device works, nvptx offloading variant.

! This doesn't compile: for nvptx offloading we're using a minimal libgfortran
! configuration.
! { dg-do link } ! ..., but still apply 'dg-do run' options.
! { dg-xfail-if "minimal libgfortran" { offload_target_nvptx } }

! Skip duplicated testing.
! { dg-skip-if "separate file" { ! offload_target_nvptx } }

include 'target-print-1.f90'
