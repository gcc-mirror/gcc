/* { dg-do run } */
/* { dg-options "-ftree-parallelize-loops=2 -O1 -fno-tree-loop-im" } */

#define N 1000

unsigned int i = 0;

static void __attribute__((noinline, noclone))
foo (void)
{
  unsigned int z;
  for (z = 0; z < N; ++z)
    ++i;
}

extern void abort (void);

int
main (void)
{
  foo ();
  if (i != N)
    abort ();

  return 0;
}
