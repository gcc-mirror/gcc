/* { dg-do run } */
/* { dg-additional-options "-fmodulo-sched -fmodulo-sched-allow-regmoves" } */

#include <stdlib.h>

int
main(int argc, char **argv)
{
  int   iexp, igot, imax, imin;
  long long lexp, lgot;
  int   N = 32;
  int	i;
  int   idata[N];
  long long ldata[N];
  float fexp, fgot;
  float fdata[N];

  igot = 1234;
  iexp = 31;

  for (i = 0; i < N; i++)
    idata[i] = i;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { idata[i] = igot; igot = i; }
  }

  imax = 0;
  imin = N;

  for (i = 0; i < N; i++)
    {
      imax = idata[i] > imax ? idata[i] : imax;
      imin = idata[i] < imin ? idata[i] : imin;
    }

  if (imax != 1234 || imin != 0)
    abort ();

  return 0;

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { idata[i] = igot; igot++; }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { idata[i] = igot; ++igot; }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { ++igot; idata[i] = igot; }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { igot++; idata[i] = igot; }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { idata[i] = igot; igot--; }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { idata[i] = igot; --igot; }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { --igot; idata[i] = igot; }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
#pragma acc atomic capture
      { igot--; idata[i] = igot; }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = + */
  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { idata[i] = igot; igot += expr; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { igot += expr; idata[i] = igot; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { idata[i] = igot; igot = igot + expr; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { idata[i] = igot; igot = expr + igot; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { igot = igot + expr; idata[i] = igot; }
      }
  }

  if (iexp != igot)
    abort ();


  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { igot = expr + igot; idata[i] = igot; }
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = * */
  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = 2LL;

#pragma acc atomic capture
      { ldata[i] = lgot; lgot *= expr; }
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2LL;

#pragma acc atomic capture
        { lgot *= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2LL;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = lgot * expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = 2LL;

#pragma acc atomic capture
      { ldata[i] = lgot; lgot = expr * lgot; }
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2LL;

#pragma acc atomic capture
        { lgot = lgot * expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = 2;

#pragma acc atomic capture
      { lgot = expr * lgot; ldata[i] = lgot; }
    }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = - */
  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      int expr = 1;

#pragma acc atomic capture
      { idata[i] = igot; igot -= expr; }
    }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { igot -= expr; idata[i] = igot; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { idata[i] = igot; igot = igot - expr; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 1;
  iexp = 1;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      int expr = 1;

#pragma acc atomic capture
      { idata[i] = igot; igot = expr - igot; }
    }
  }

  for (i = 0; i < N; i++)
    if (i % 2 == 0)
      {
	if (idata[i] != 1)
	  abort ();
      }
    else
      {
	if (idata[i] != 0)
	  abort ();
      }

  if (iexp != igot)
    abort ();

  igot = 1;
  iexp = -31;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { igot = igot - expr; idata[i] = igot; }
      }
  }

  if (iexp != igot)
    abort ();

  igot = 1;
  iexp = 1;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        { igot = expr - igot; idata[i] = igot; }
      }
  }

  for (i = 0; i < N; i++)
    if (i % 2 == 0)
      {
	if (idata[i] != 0)
	  abort ();
      }
    else
      {
	if (idata[i] != 1)
	  abort ();
      }

  if (iexp != igot)
    abort ();

  /* BINOP = / */
  lgot = 1LL << 32;
  lexp = 1LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2LL;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot /= expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << 32;
  lexp = 1LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2LL;

#pragma acc atomic capture
        { lgot /= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << 32;
  lexp = 1LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = 2LL;

#pragma acc atomic capture
      { ldata[i] = lgot; lgot = lgot / expr; }
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = 2LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = expr / lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 2LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic capture
        { lgot = lgot / expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 2LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic capture
        { lgot = expr / lgot; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = & */
  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { ldata[i] = lgot; lgot &= expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  iexp = 0LL; 

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot &= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = lgot & expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = expr & lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  iexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot = lgot & expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = ~(1 << i);

#pragma acc atomic capture
      { lgot = expr & lgot; ldata[i] = lgot; }
    }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = ^ */
  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = 1 << i;

#pragma acc atomic capture
      { ldata[i] = lgot; lgot ^= expr; }
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  iexp = 0LL; 

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot ^= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = lgot ^ expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = ~(1 << i);

#pragma acc atomic capture
      { ldata[i] = lgot; lgot = expr ^ lgot; }
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  iexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot = lgot ^ expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = ~0LL;
  lexp = 0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot = expr ^ lgot; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = | */
  lgot = 0LL;
  lexp = ~0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1 << i;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot |= expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 0LL;
  iexp = ~0LL; 

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot |= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 0LL;
  lexp = ~0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = lgot | expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 0LL;
  lexp = ~0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = expr | lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 0LL;
  iexp = ~0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot = lgot | expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 0LL;
  lexp = ~0LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = ~(1 << i);

#pragma acc atomic capture
        { lgot = expr | lgot; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = << */
  lgot = 1LL;
  lexp = 1LL << N;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot <<= expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  iexp = 1LL << N; 

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { lgot <<= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << N;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = lgot << expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = expr << lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { lgot = lgot << expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { lgot = expr << lgot; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = >> */
  lgot = 1LL << N;
  lexp = 1LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;
  
#pragma acc atomic capture
        { ldata[i] = lgot; lgot >>= expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << N;
  iexp = 1LL; 

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { lgot >>= expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << N;
  lexp = 1LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = lgot >> expr; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << (N - 1);

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic capture
        { ldata[i] = lgot; lgot = expr >> lgot; }
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << N;
  lexp = 1LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic capture
        { lgot = lgot >> expr; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << (N - 1);

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic capture
        { lgot = expr >> lgot; ldata[i] = lgot; }
      }
  }

  if (lexp != lgot)
    abort ();

  // FLOAT FLOAT FLOAT

  /* BINOP = + */
  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      float expr = 1.0;

#pragma acc atomic capture
      { fdata[i] = fgot; fgot += expr; }
    }
  }

  if (fexp != fgot)
    abort ();

  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fgot += expr; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { idata[i] = fgot; fgot = fgot + expr; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      float expr = 1.0;

#pragma acc atomic capture
      { fdata[i] = fgot; fgot = expr + fgot; }
    }
  }

  if (fexp != fgot)
    abort ();

  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fgot = fgot + expr; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fgot = expr + fgot; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = * */
  fgot = 1.0;
  fexp = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      float expr = 2.0;

#pragma acc atomic capture
      { fdata[i] = fgot; fgot *= expr; }
    }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fgot *= expr; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot = fgot * expr; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot = expr * fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      long long expr = 2LL;

#pragma acc atomic capture
      { lgot = lgot * expr; ldata[i] = lgot; }
    }
  }

  if (lexp != lgot)
    abort ();

  fgot = 1.0;
  fexp = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2;

#pragma acc atomic capture
        { fgot = expr * fgot; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = - */
  fgot = 32.0;
  fexp = 0.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
  
#pragma acc atomic capture
        { fdata[i] = fgot; fgot -= expr; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 32.0;
  fexp = 0.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
      float expr = 1.0;

#pragma acc atomic capture
      { fgot -= expr; fdata[i] = fgot; }
    }
  }

  if (fexp != fgot)
    abort ();

  fgot = 32.0;
  fexp = 0.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot = fgot - expr; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = 1.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot = expr - fgot; }
      }
  }

  for (i = 0; i < N; i++)
    if (i % 2 == 0)
      {
	if (fdata[i] != 1.0)
	  abort ();
      }
    else
      {
	if (fdata[i] != 0.0)
	  abort ();
      }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = -31.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fgot = fgot - expr; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = 1.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fgot = expr - fgot; fdata[i] = fgot; }
      }
  }

  for (i = 0; i < N; i++)
    if (i % 2 == 0)
      {
	if (fdata[i] != 0.0)
	  abort ();
      }
    else
      {
	if (fdata[i] != 1.0)
	  abort ();
      }

  if (fexp != fgot)
    abort ();

  /* BINOP = / */
  fgot = 8192.0*8192.0*64.0;
  fexp = 1.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot /= expr; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 8192.0*8192.0*64.0;
  fexp = 1.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fgot /= expr; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 8192.0*8192.0*64.0;
  fexp = 1.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot = fgot / expr; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 8192.0*8192.0*64.0;
  fexp = 1.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;

#pragma acc atomic capture
        { fdata[i] = fgot; fgot = expr / fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 4.0;
  fexp = 4.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic capture
        { fgot = fgot / expr; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 4.0;
  fexp = 4.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        { fgot = expr / fgot; fdata[i] = fgot; }
      }
  }

  if (fexp != fgot)
    abort ();

  return 0;
}
