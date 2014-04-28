/* { dg-do run } */

extern void abort (void);

volatile int count;
static int test (void)
{
  return ++count > 0;
}

int i;

int
main ()
{
  #pragma omp for lastprivate (i)
  for (i = 0; i < 10; ++i)
    {
      int *p = &i;
      if (test ())
	continue;
      abort ();
    }
  if (i != count)
    abort ();
  return 0;
}
