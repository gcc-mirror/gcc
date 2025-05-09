! { dg-do run }
! { dg-additional-options "-cpp" }
! { dg-require-effective-target omp_usm }
! { dg-additional-options "-DOMP_USM" }
! { dg-additional-options "-DMEM_SHARED" }

#include "target-allocatable-1-1.f90"
