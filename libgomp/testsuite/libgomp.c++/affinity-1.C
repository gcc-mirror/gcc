// { dg-do run }
// { dg-set-target-env-var OMP_PROC_BIND "true" }
// { dg-additional-options "-Wno-deprecated-declarations" }

#include "../libgomp.c/affinity-1.c"
