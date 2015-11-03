/* { dg-do run } */

#include <stdlib.h>

int
main(int argc, char **argv)
{
  int   iexp, igot;
  long long lexp, lgot;
  int   N = 32;
  int   idata[N];
  long long   ldata[N];
  float fexp, fgot;
  float fdata[N];
  int i;

  igot = 0;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
#pragma acc atomic capture
        idata[i] = igot++;
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
#pragma acc atomic capture
        idata[i] = igot--;
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
#pragma acc atomic capture
        idata[i] = ++igot;
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
#pragma acc atomic capture
        idata[i] = --igot;
      }
  }

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
        idata[i] = igot += expr;
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
        idata[i] = igot = igot + expr;
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
        idata[i] = igot = expr + igot;
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = * */
  lgot = 1LL;
  lexp = 1LL << N;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 2LL;

#pragma acc atomic capture
        ldata[i] = lgot *= expr;
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
        long long expr = 2LL;

#pragma acc atomic capture
        ldata[i] = lgot = lgot * expr;
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
        long long expr = 2LL;

#pragma acc atomic capture
        ldata[i] = lgot = expr * lgot;
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
        idata[i] = igot -= expr;
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
        idata[i] = igot = igot - expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = 32;
  iexp = 32;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        idata[i] = igot = expr - igot;
      }
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
        ldata[i] = lgot /= expr;
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
        ldata[i] = lgot = lgot / expr;
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
        ldata[i] = lgot = expr / lgot;
      }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = & */
  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot &= expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot = igot & expr;
    }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot = expr & igot;
     }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = ^ */
  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot ^= expr;
     }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot = igot ^ expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot = expr ^ igot;
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = | */
  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot |= expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot = igot | expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot, idata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1 << i;

#pragma acc atomic capture
        idata[i] = igot = expr | igot;
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = << */
  lgot = 1LL;
  lexp = 1LL << N;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = 1;

#pragma acc atomic capture
        ldata[i] = lgot <<= expr;
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
        int expr = 1;

#pragma acc atomic capture
        idata[i] = lgot = lgot << expr;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 2LL;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel
    {
      long long expr = 1LL;

#pragma acc atomic capture
      ldata[0] = lgot = expr << lgot;
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
        ldata[i] = lgot >>= expr;
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
        int expr = 1;

#pragma acc atomic capture
        ldata[i] = lgot = lgot >> expr;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << 63;
  lexp = 1LL << 32;

#pragma acc data copy (lgot, ldata[0:N])
  {
#pragma acc parallel
    {
      long long expr = 1LL << 32;

#pragma acc atomic capture
      ldata[0] = lgot = expr >> lgot;
    }
  }

  if (lexp != lgot)
    abort ();

  fgot = 0.0;
  fexp = 32.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
#pragma acc atomic capture
        fdata[i] = fgot++;
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
#pragma acc atomic capture
        fdata[i] = fgot--;
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
#pragma acc atomic capture
        fdata[i] = ++fgot;
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
#pragma acc atomic capture
        fdata[i] = --fgot;
      }
  }

  if (fexp != fgot)
    abort ();

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
        fdata[i] = fgot += expr;
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
        fdata[i] = fgot = fgot + expr;
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
        fdata[i] = fgot = expr + fgot;
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
        fdata[i] = fgot *= expr;
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
        long long expr = 2LL;

#pragma acc atomic capture
        fdata[i] = fgot = fgot * expr;
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
        fdata[i] = fgot = expr * fgot;
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
        fdata[i] = fgot -= expr;
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
        fdata[i] = fgot = fgot - expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1.0;
  fexp = 0.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 32.0;

#pragma acc atomic capture
        fdata[i] = fgot = expr - fgot;
      }
  }

  for (i = 0; i < N; i++)
    if (i % 2 == 0)
      {
	if (fdata[i] != 31.0)
	  abort ();
      }
    else
      {
	if (fdata[i] != 1.0)
	  abort ();
      }


  /* BINOP = / */
  fexp = 1.0;
  fgot = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        fdata[i] = fgot /= expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fexp = 1.0;
  fgot = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic capture
        fdata[i] = fgot = fgot / expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fexp = 1.0;
  fgot = 8192.0*8192.0*64.0;

#pragma acc data copy (fgot, fdata[0:N])
  {
#pragma acc parallel
    {
      float expr = 8192.0*8192.0*64.0;

#pragma acc atomic capture
      fdata[0] = fgot = expr / fgot;
    }
  }

  if (fexp != fgot)
    abort ();
  
  return 0;
}
