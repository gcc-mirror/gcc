! { dg-additional-options "-O0 -g -cpp" }
! { dg-do run }

! Check an unroll factor that is larger than the number of iterations
! of the loops in the test implementation.
#define UNROLL_FACTOR 113
#include "unroll-7.f90"
