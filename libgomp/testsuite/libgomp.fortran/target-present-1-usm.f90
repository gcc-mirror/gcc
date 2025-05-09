! { dg-additional-options -cpp }
! { dg-require-effective-target omp_usm }
! { dg-additional-options -DOMP_USM }

#include "target-present-1.f90"

! No 'dg-shouldfail'.
