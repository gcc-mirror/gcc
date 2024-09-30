// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run }
// { dg-additional-options "-std=c++17 -frange-for-ext-temps" }
// { dg-require-effective-target tls_runtime }

#define RANGE_FOR_EXT_TEMPS 1
#include "range-for-1.C"
