/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* See also 'declare-vla-kernels-decompose-ice-1.c'.  */

/* { dg-additional-options "-fopt-info-omp-all" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

#define KERNELS_DECOMPOSE_ICE_HACK
#include "declare-vla.c"

/* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } 27 } */

/* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } 61 } */

/* { dg-bogus {note: variable [^\n\r]+ candidate for adjusting OpenACC privatization level} {TODO 'data'} { xfail *-*-* } 42 } */

/* { dg-note {variable 'i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } 58 }
   { dg-note {variable 'N\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } 58 } */

/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { ! __OPTIMIZE__ } } 24 }
   { dg-optimized {assigned OpenACC gang loop parallelism} {} { target { __OPTIMIZE__ } } 24 } */

/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { ! __OPTIMIZE__ } } 58 }
   { dg-optimized {assigned OpenACC gang loop parallelism} {} { target { __OPTIMIZE__ } } 58 } */
