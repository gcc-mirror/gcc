! { dg-do run }
! { dg-additional-options "-g -cpp" }

! Check an unroll factor that divides the number of iterations
! of the loops in the test implementation.
#define UNROLL_FACTOR 5
#include "unroll-7.f90"
