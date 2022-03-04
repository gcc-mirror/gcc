/* Test OpenACC 'kernels' construct decomposition.  */

/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop_c 0 c_loop_i 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#undef NDEBUG
#include <assert.h>

static int g1;
static int g2;

int main()
{
  int a = 0;
  /*TODO Without making 'a' addressable, for GCN offloading we will not see the expected value copied out.  (But it does work for nvptx offloading, strange...)  */
  (volatile int *) &a;
#define N 123
  int b[N] = { 0 };
  unsigned long long f1;
  /*TODO See above.  */
  (volatile void *) &f1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'g2\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'f1\.1' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'f1\.2' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  {
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    int c = 234;
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'c' declared in block requested to be made addressable} "" { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'c' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {variable 'c' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute } */

#pragma acc loop independent gang /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      b[i] = c;

    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    a = c;

    /* PR104132, PR104133 */
    {
      /* Use the 'kernels'-top-level 'int c' as loop variable.  */

      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {variable 'c' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 0; c < N / 2; c++)
	b[c] -= 10;

      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {variable 'c' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 0; c < N / 2; c++)
	g1 = c;

      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {variable 'c' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 0; c <= N; c++)
	g2 += c;

    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
      f1 = 1;
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {variable 'c' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 20; c > 0; --c)
	f1 *= c;

      /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
      if (c != 234)
	__builtin_abort ();
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute$c_compute } */
    }
  }

  assert (a == 234);
  for (int i = 0; i < N; ++i)
    if (i < N / 2)
      assert (b[i] == 234 - 10);
    else
      assert (b[i] == 234);
  assert (g1 == N / 2 - 1);
  assert (g2 == N * (N + 1) / 2);
  assert (f1 == 2432902008176640000ULL);

  return 0;
}
