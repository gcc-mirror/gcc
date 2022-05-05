/* { dg-do run } */

/* Based on '../libgomp.oacc-fortran/asyncwait-1.f90'.  */

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
  int *a, *b, *c, *d, *e;

  a = (int*)malloc (N * sizeof (*a));
  b = (int*)malloc (N * sizeof (*b));
  c = (int*)malloc (N * sizeof (*c));
  d = (int*)malloc (N * sizeof (*d));
  e = (int*)malloc (N * sizeof (*e));

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {

#pragma acc parallel async /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 3)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {
#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2) abort ();
      if (b[i] != 2) abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N])
  {

#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];


#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 9)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
      e[i] = 0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N])
  {

#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];

#pragma acc parallel async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];


#pragma acc parallel wait (1) async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      e[i] = a[i] + b[i] + c[i] + d[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (b[i] != 4)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
      if (e[i] != 11)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {

#pragma acc kernels async
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 3)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N])
  {
#pragma acc kernels async (1)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      b[i] = a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (b[i] != 2)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 3;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
    }

#pragma acc data copy (a[0:N]) copy (b[0:N]) copy (c[0:N]) copy (d[0:N])
  {
#pragma acc kernels async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' declared in block requested to be made addressable} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target { ! __OPTIMIZE__ } } l_compute$c_compute }
       { dg-optimized "assigned OpenACC gang loop parallelism" "" { target { __OPTIMIZE__ } } l_compute$c_compute } */
    /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc kernels async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' declared in block requested to be made addressable} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target { ! __OPTIMIZE__ } } l_compute$c_compute }
       { dg-optimized "assigned OpenACC gang loop parallelism" "" { target { __OPTIMIZE__ } } l_compute$c_compute } */
    /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];

#pragma acc kernels async (1)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 3)
	abort ();
      if (b[i] != 9)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
    }

  for (int i = 0; i < N; ++i)
    {
      a[i] = 2;
      b[i] = 0;
      c[i] = 0;
      d[i] = 0;
      e[i] = 0;
    }

#pragma acc data copy (a[0:N], b[0:N], c[0:N], d[0:N], e[0:N])
  {
#pragma acc kernels async (1) /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' declared in block requested to be made addressable} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target { ! __OPTIMIZE__ } } l_compute$c_compute }
       { dg-optimized "assigned OpenACC gang loop parallelism" "" { target { __OPTIMIZE__ } } l_compute$c_compute } */
    /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    for (int i = 0; i < N; ++i)
      b[i] = (a[i] * a[i] * a[i]) / a[i];

#pragma acc kernels async (1)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      c[i] = (a[i] * 4) / a[i];

#pragma acc kernels async (1)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      d[i] = ((a[i] * a[i] + a[i]) / a[i]) - a[i];

#pragma acc kernels wait (1) async (1)
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      e[i] = a[i] + b[i] + c[i] + d[i];

#pragma acc wait (1)
  }

  for (int i = 0; i < N; ++i)
    {
      if (a[i] != 2)
	abort ();
      if (b[i] != 4)
	abort ();
      if (c[i] != 4)
	abort ();
      if (d[i] != 1)
	abort ();
      if (e[i] != 11)
	abort ();
    }

  free (a);
  free (b);
  free (c);
  free (d);
  free (e);

  return 0;
}
