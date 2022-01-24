/* { dg-additional-options "--param=openacc-kernels=decompose" } */
/* ICE similar to PR100280, but not the same.
   { dg-ice "TODO" }
   TODO { dg-prune-output "during GIMPLE pass: omplower" }
   TODO { dg-do link } */

/* { dg-additional-options "-fopt-info-omp-all" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

#undef KERNELS_DECOMPOSE_ICE_HACK
#include "declare-vla.c"

/* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } 27 } */

/* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } 61 } */

/* { dg-bogus {note: variable [^\n\r]+ candidate for adjusting OpenACC privatization level} {TODO 'data'} { xfail *-*-* } 42 } */
