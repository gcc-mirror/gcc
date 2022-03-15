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

/* PR100280, etc. */

static void f1 ()
{
  int a = 0;
#define N 123
  int b[N] = { 0 };
  unsigned long long f1;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'f1' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'f1' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'a' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'a' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'g2' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'g2' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'g1' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'g1' made addressable} {} { target *-*-* } l_compute$c_compute } */
  {
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    int c = 234;
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'c' declared in block requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'c' made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'c' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute$c_compute } */

#pragma acc loop independent gang /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i } */
    for (int i = 0; i < N; ++i)
      b[i] = c;

    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    a = c;

    /* PR104132, PR104133, PR104774 */
    {
      /* Use the 'kernels'-top-level 'int c' as loop variable.  */

#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-note {variable 'c' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 0; c < N / 2; c++)
	b[c] -= 10;

#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-note {variable 'c' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 0; c < N / 2; c++)
	g1 = c;

#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-note {variable 'c' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 0; c <= N; c++)
	g2 += c;
	/* { dg-note {variable 'g2\.0' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */

      /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
      f1 = 1;
#pragma acc loop /* { dg-line l_loop_c[incr c_loop_c] } */
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-note {variable 'c' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c$c_loop_c } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
      for (c = 20; c > 0; --c)
	f1 *= c;

      {
	/* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
	unsigned long long f2 = 1;
	/* { dg-note {OpenACC 'kernels' decomposition: variable 'f2' declared in block requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
	   { dg-note {variable 'f2' made addressable} {} { target *-*-* } l_compute$c_compute }
	   { dg-note {variable 'f2' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute$c_compute } */
#pragma acc loop independent reduction(*: f2) /* { dg-line l_loop_c[incr c_loop_c] } */
	/* { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_c$c_loop_c } */
	/* { dg-note {variable 'c' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c$c_loop_c } */
	/* { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
	for (c = 20; c > 0; --c)
	  f2 *= c;

	{
	  /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
	  if (f2 != f1)
	    __builtin_abort ();

	  /* As this is still in the preceding 'parloops' part:
	     { dg-bogus {note: beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
	  unsigned long long f3 = f2;
	  /* { dg-note {OpenACC 'kernels' decomposition: variable 'f3' declared in block requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
	     { dg-note {variable 'f3' made addressable} {} { target *-*-* } l_compute$c_compute }
	     { dg-note {variable 'f3' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute$c_compute } */
#pragma acc loop seq /* { dg-line l_loop_c[incr c_loop_c] } */
	  /* { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_c$c_loop_c } */
	  /* { dg-note {variable 'c' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_c$c_loop_c } */
	  /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_c$c_loop_c } */
	  for (c = 20; c > 0; --c)
	    f3 /= c;

	  /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
	  if (f3 != 1)
	    __builtin_abort ();
	}

	/* As this is still in the preceding 'parloops' part:
	   { dg-bogus {note: beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
	if (f2 != f1)
	  __builtin_abort ();
      }

      /* As this is still in the preceding 'parloops' part:
	 { dg-bogus {note: beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
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

#undef N
}


/* PR104086 */

static void f2 ()
{
#pragma acc data
  /* { dg-bogus {note: variable [^\n\r]+ candidate for adjusting OpenACC privatization level} {TODO 'data'} { xfail *-*-* } .-1 } */
  {
    int i;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    i = 1;

    assert (i == 1);

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'i' already made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    i = -1;

    assert (i == -1);
  }


  int ia[1];

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'ia' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'ia' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
  ia[0] = -2;

  assert (ia[0] == -2);

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'ia' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'ia' already made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' declared in block requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'i' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
  for (int i = 0; i < 100; ++i)
    ++ia[0];

  assert (ia[0] == -2 + 100);
}


int main()
{
  f1 ();

  f2 ();

  return 0;
}
