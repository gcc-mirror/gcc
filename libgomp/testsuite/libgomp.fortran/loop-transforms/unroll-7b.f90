! { dg-additional-options "-O0 -g -cpp" }
! { dg-do run }

! Check an unroll factor that does not divide the number of iterations
! of the loops in the test implementation.
#define UNROLL_FACTOR 3
#include "unroll-7.f90"
