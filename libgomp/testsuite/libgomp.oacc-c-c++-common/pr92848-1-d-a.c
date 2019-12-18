/* Verify device memory allocation/deallocation
   { dg-additional-options "-DOPENACC_DIRECTIVES" } using OpenACC directives,
   { dg-additional-options "-DARRAYS" } using arrays.  */

/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include "pr92848-1-r-p.c"
