/* { dg-do run } */

#include <stdlib.h>

int
main(int argc, char **argv)
{
  float fexp, fgot;
  int   iexp, igot;
  long long lexp, lgot;
  int   N = 32;
  int	i;

  fgot = 1234.0;
  fexp = 1235.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
#pragma acc atomic update
      fgot++;
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = fgot - N;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
#pragma acc atomic update
        fgot--;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = fgot + N;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
#pragma acc atomic update
        ++fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = fgot - N;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
#pragma acc atomic update
        --fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = + */

  fgot = 1234.0;
  fexp = fgot + N;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
#pragma acc atomic update
        fgot += expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = fgot + N;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
#pragma acc atomic update
        fgot = fgot + expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = fgot + N;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
#pragma acc atomic update
        fgot = expr + fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 0.5;
#pragma acc atomic update
        fgot = (expr + expr) + fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = * */

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp *= 2.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
#pragma acc atomic update
        fgot *= expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = fexp * 2.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
#pragma acc atomic update
        fgot = fgot * expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = 2.0 * fexp;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
#pragma acc atomic update
        fgot = expr * fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
#pragma acc atomic update
        fgot = (expr + expr) * fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = - */

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp -= 2.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
#pragma acc atomic update
        fgot -= expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = fexp - 2.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
#pragma acc atomic update
        fgot = fgot - expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = 2.0 - fexp;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic update
        fgot = expr - fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
#pragma acc atomic update
        fgot = (expr + expr) - fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = / */

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp /= 2.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
#pragma acc atomic update
        fgot /= expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = fexp / 2.0;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;
	
#pragma acc atomic update
        fgot = fgot / expr;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = 2.0 / fexp;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 2.0;

#pragma acc atomic update
        fgot = expr / fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  fgot = 1234.0;
  fexp = 1234.0;

  for (i = 0; i < N; i++)
    fexp = 2.0 / fexp;

#pragma acc data copy (fgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        float expr = 1.0;
#pragma acc atomic update
        fgot = (expr + expr) / fgot;
      }
  }

  if (fexp != fgot)
    abort ();

  /* BINOP = & */

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = ~(1 << i);

#pragma acc atomic update
        igot &= expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = ~(1 << i);
#pragma acc atomic update
        igot = igot / expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = ~(1 << i);
#pragma acc atomic update
        igot = expr & igot;
     }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = ~(1 << i);
        int zero = 0;

#pragma acc atomic update
        igot = (expr + zero) & igot;
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = ^ */

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);

#pragma acc atomic update
        igot ^= expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);

#pragma acc atomic update
        igot = igot ^ expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);

#pragma acc atomic update
        igot = expr ^ igot;
      }
  }

  if (iexp != igot)
    abort ();

  igot = ~0;
  iexp = 0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);
        int zero = 0;

#pragma acc atomic update
        igot = (expr + zero) ^ igot;
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = | */

  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);

#pragma acc atomic update
        igot |= expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);

#pragma acc atomic update
        igot = igot | expr;
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);

#pragma acc atomic update
        igot = expr | igot;
      }
  }

  if (iexp != igot)
    abort ();

  igot = 0;
  iexp = ~0;

#pragma acc data copy (igot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        int expr = (1 << i);
        int zero = 0;

#pragma acc atomic update
        igot = (expr + zero) | igot;
      }
  }

  if (iexp != igot)
    abort ();

  /* BINOP = << */

  lgot = 1LL;
  lexp = 1LL << N;

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic update
        lgot <<= expr;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << N;

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic update
        lgot = lgot << expr;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 2LL;

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL;

#pragma acc atomic update
        lgot = expr << lgot;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 2LL;

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL;
        long long zero = 0LL;

#pragma acc atomic update
        lgot = (expr + zero) << lgot;
      }
  }

  if (lexp != lgot)
    abort ();

  /* BINOP = >> */

  lgot = 1LL << N;
  lexp = 1LL;

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic update
        lgot >>= expr;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL << N;
  lexp = 1LL;

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < N; i++)
      {
        long long expr = 1LL;

#pragma acc atomic update
        lgot = lgot >> expr;
      }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << (N - 1);

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL << N;

#pragma acc atomic update
        lgot = expr >> lgot;
    }
  }

  if (lexp != lgot)
    abort ();

  lgot = 1LL;
  lexp = 1LL << (N - 1);

#pragma acc data copy (lgot)
  {
#pragma acc parallel loop
    for (i = 0; i < 1; i++)
      {
        long long expr = 1LL << N;
        long long zero = 0LL;

#pragma acc atomic update
        lgot = (expr + zero) >> lgot;
    }
  }

  if (lexp != lgot)
    abort ();

  return 0;
}
