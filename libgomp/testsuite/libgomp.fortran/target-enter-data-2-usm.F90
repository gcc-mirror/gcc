! { dg-additional-options "-DMEM_SHARED" }
! { dg-do run }

! { dg-require-effective-target omp_usm }
!$omp requires unified_shared_memory self_maps

#include "target-enter-data-2.F90"
