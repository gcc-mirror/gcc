/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <stdio.h>
#include <openacc.h>
#include <string.h>
#include <gomp-constants.h>
#include <stdlib.h>

#if 0
#define DEBUG(DIM, IDX, VAL) \
  fprintf (stderr, "%sdist[%d] = %d\n", (DIM), (IDX), (VAL))
#else
#define DEBUG(DIM, IDX, VAL)
#endif

#define N (32*32*32)

int
check (const char *dim, int *dist, int dimsize)
{
  int ix;
  int exit = 0;

  for (ix = 0; ix < dimsize; ix++)
    {
      DEBUG(dim, ix, dist[ix]);
      if (dist[ix] < (N) / (dimsize + 0.5)
	  || dist[ix] > (N) / (dimsize - 0.5))
	{
	  fprintf (stderr, "did not distribute to %ss (%d not between %d "
		   "and %d)\n", dim, dist[ix], (int) ((N) / (dimsize + 0.5)),
		   (int) ((N) / (dimsize - 0.5)));
	  exit |= 1;
	}
    }

  return exit;
}

int main ()
{
  int ary[N];
  int ix;
  int exit = 0;
  int gangsize, workersize, vectorsize;
  int *gangdist, *workerdist, *vectordist;

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;

#define NG 32
#define NW 32
#define VL 32
#pragma acc parallel num_gangs(NG) num_workers(NW) vector_length(VL) \
	    copy(ary)
  /* { dg-note {variable 'ix' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
  {
#pragma acc loop gang worker vector
    /* { dg-note {variable 'ix' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'g' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
    /* { dg-note {variable 'w' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-3 } */
    /* { dg-note {variable 'v' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-4 } */
    for (unsigned ix = 0; ix < N; ix++)
      {
	int g, w, v;

	g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
	w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
	v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

	ary[ix] = (g << 16) | (w << 8) | v;
      }
  }
  gangsize = NG;
  workersize = NW;
  vectorsize = VL;
#if defined ACC_DEVICE_TYPE_host
  gangsize = 1;
  workersize = 1;
  vectorsize = 1;
#elif defined ACC_DEVICE_TYPE_radeon
  /* AMD GCN has an upper limit of 'num_workers(16)'.  */
  if (workersize > 16)
    workersize = 16;
  /* AMD GCN uses the autovectorizer for the vector dimension: the use
     of a function call in vector-partitioned code in this test is not
     currently supported.  */
  vectorsize = 1;
#endif

  gangdist = (int *) __builtin_alloca (gangsize * sizeof (int));
  workerdist = (int *) __builtin_alloca (workersize * sizeof (int));
  vectordist = (int *) __builtin_alloca (vectorsize * sizeof (int));
  memset (gangdist, 0, gangsize * sizeof (int));
  memset (workerdist, 0, workersize * sizeof (int));
  memset (vectordist, 0, vectorsize * sizeof (int));

  /* Test that work is shared approximately equally amongst each active
     gang/worker/vector.  */
  for (ix = 0; ix < N; ix++)
    {
      int g = (ary[ix] >> 16) & 255;
      int w = (ary[ix] >> 8) & 255;
      int v = ary[ix] & 255;

      if (g >= gangsize
	  || w >= workersize
	  || v >= vectorsize)
	__builtin_abort ();

      gangdist[g]++;
      workerdist[w]++;
      vectordist[v]++;
    }

  exit = check ("gang", gangdist, gangsize);
  exit |= check ("worker", workerdist, workersize);
  exit |= check ("vector", vectordist, vectorsize);

  return exit;
}
