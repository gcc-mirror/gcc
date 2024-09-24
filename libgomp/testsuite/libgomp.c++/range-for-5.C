// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run }
// { dg-additional-options "-std=gnu++17 -fno-range-for-ext-temps" }
// { dg-require-effective-target tls_runtime }

#define RANGE_FOR_EXT_TEMPS 0
#include "range-for-1.C"
