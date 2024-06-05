! { dg-do run }
! { dg-additional-options "-g -cpp" }

! Check an unroll factor that does not divide the number of iterations
! of the loops in the test implementation.
#define UNROLL_FACTOR 3
#include "unroll-7.f90"
