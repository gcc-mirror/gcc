/* { dg-additional-options "--param=openacc-kernels=decompose" } */
/* Hopefully, this is the same issue as '../../../gcc/testsuite/c-c++-common/goacc/kernels-decompose-ice-1.c'.
   { dg-ice "TODO" }
   TODO { dg-prune-output "during GIMPLE pass: omplower" }
   TODO { dg-do link } */

#undef KERNELS_DECOMPOSE_ICE_HACK
#include "declare-vla.c"
