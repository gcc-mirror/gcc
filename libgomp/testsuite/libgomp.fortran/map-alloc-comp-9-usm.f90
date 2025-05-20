! { dg-additional-options "-cpp -DUSE_USM_REQUIREMENT=1 -Wno-openmp" }
!
! We silence the warning:
!  Mapping of polymorphic list item '...' is unspecified behavior [-Wopenmp]
!
! Ensure that polymorphic mapping is diagnosed as undefined behavior
! Ensure that static access to polymorphic variables works

! Run map-alloc-comp-9.f90 in unified-shared-memory mode

#include "map-alloc-comp-9.f90"
