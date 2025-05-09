! { dg-do run }

! { dg-require-effective-target omp_usm }
!$omp requires unified_shared_memory self_maps

include 'map-subarray-6.f90'

! No 'dg-shouldfail'.
