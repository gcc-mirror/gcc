! { dg-do run }
! { dg-additional-options "-g -cpp" }

! Check an unroll factor that is larger than the number of iterations
! of the loops in the test implementation.
#define UNROLL_FACTOR 113
#include "unroll-7.f90"
