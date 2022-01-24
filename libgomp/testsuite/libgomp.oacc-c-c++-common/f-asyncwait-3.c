/* { dg-do run } */

/* Based on '../libgomp.oacc-fortran/asyncwait-3.f90'.  */

/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable '[Di]\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop_i 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#include <stdlib.h>

#define N 64

int
main (void)
{
  int *a, *b, *c;

  a = (int *)malloc (N * sizeof (*a));
  b = (int *)malloc (N * sizeof (*b));
  c = (int *)malloc (N * sizeof (*c));

#pragma acc parallel copy (a[0:N]) async (0) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
  for (int i = 0; i < N; ++i)
    a[i] = 1;

#pragma acc parallel copy (b[0:N]) async (1) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
  for (int i = 0; i < N; ++i)
    b[i] = 1;

#pragma acc wait (0, 1)

#pragma acc parallel copy (a[0:N], b[0:N], c[0:N]) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
  for (int i = 0; i < N; ++i)
    c[i] = a[i] + b[i];

  for (int i = 0; i < N; ++i)
    if (c[i] != 2)
      abort ();

#pragma acc kernels copy (a[0:N]) async (0)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
  /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
  for (int i = 0; i < N; ++i)
    a[i] = 1;

#pragma acc kernels copy (b[0:N]) async (1)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
  /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
  for (int i = 0; i < N; ++i)
    b[i] = 1;

#pragma acc wait (0, 1)

#pragma acc kernels copy (a[0:N], b[0:N], c[0:N])
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
  /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
  /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
  for (int i = 0; i < N; ++i)
    c[i] = a[i] + b[i];

  for (int i = 0; i < N; ++i)
    if (c[i] != 2)
      abort ();

  free (a);
  free (b);
  free (c);
}
