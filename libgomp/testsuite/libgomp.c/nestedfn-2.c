/* { dg-do run } */
// { dg-additional-options "-Wno-deprecated-openmp" }

extern void abort (void);

int
main (void)
{
  int i;
  void
  foo (void)
  {
#pragma omp master
    i += 8;
  }
  i = 4;
  foo ();
  if (i != 12)
    abort ();
  return 0;
}
