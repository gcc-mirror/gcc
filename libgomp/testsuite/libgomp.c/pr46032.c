/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -std=c99 -fipa-pta" } */


extern void abort (void);

#define nEvents 1000

static void __attribute__((noinline, noclone, optimize("-fno-tree-vectorize")))
init (unsigned *results, unsigned *pData)
{
  unsigned int i;
  for (i = 0; i < nEvents; ++i)
    pData[i] = i % 3;
}

static void __attribute__((noinline, noclone, optimize("-fno-tree-vectorize")))
check (unsigned *results)
{
  unsigned sum = 0;
  for (int idx = 0; idx < (int)nEvents; idx++)
    sum += results[idx];

  if (sum != 1998)
    abort ();
}

int
main (void)
{
  unsigned results[nEvents];
  unsigned pData[nEvents];
  unsigned coeff = 2;

  init (&results[0], &pData[0]);

#pragma omp parallel for
  for (int idx = 0; idx < (int)nEvents; idx++)
    results[idx] = coeff * pData[idx];

  check (&results[0]);

  return 0;
}
