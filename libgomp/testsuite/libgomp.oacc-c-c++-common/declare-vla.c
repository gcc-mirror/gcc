/* Verify OpenACC 'declare' with VLAs.  */

/* { dg-additional-options "-fopt-info-omp-all" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

#include <assert.h>


void
f (void)
{
  int N = 1000;
  int i, A[N];
#pragma acc declare copy(A)

  for (i = 0; i < N; i++)
    A[i] = -i;

#pragma acc kernels
  /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { ! __OPTIMIZE__ } } .-1 }
     { dg-optimized {assigned OpenACC gang loop parallelism} {} { target { __OPTIMIZE__ } } .-2 } */
  for (i = 0; i < N; i++)
    A[i] = i;

#pragma acc update host(A)

  for (i = 0; i < N; i++)
    assert (A[i] == i);
}


/* The same as 'f' but everything contained in an OpenACC 'data' construct.  */

void
f_data (void)
{
#pragma acc data
  /* { dg-bogus {note: variable [^\n\r]+ candidate for adjusting OpenACC privatization level} {TODO 'data'} { xfail *-*-* } .-1 } */
  {
    int N = 1000;
    int i, A[N];
# pragma acc declare copy(A)

    for (i = 0; i < N; i++)
      A[i] = -i;

    /* See 'declare-vla-kernels-decompose.c'.  */
#ifdef KERNELS_DECOMPOSE_ICE_HACK
    (volatile int *) &i;
    (volatile int *) &N;
#endif

# pragma acc kernels
  /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { ! __OPTIMIZE__ } } .-1 }
     { dg-optimized {assigned OpenACC gang loop parallelism} {} { target { __OPTIMIZE__ } } .-2 } */
    for (i = 0; i < N; i++)
      A[i] = i;

# pragma acc update host(A)

    for (i = 0; i < N; i++)
      assert (A[i] == i);
  }
}


int
main ()
{
  f ();

  f_data ();

  return 0;
}


/* { dg-note dummy "" { target n-on-e } } to disable 'prune_notes'.  */
