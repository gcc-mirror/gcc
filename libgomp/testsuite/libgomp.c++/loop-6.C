// { dg-do run }

extern "C" void abort (void);

volatile int count;
static int test(void)
{
  count = count + 1;
  return count > 0;
}

int i;

int main()
{
  #pragma omp for lastprivate (i)
  for (i = 0; i < 10; ++i)
    {
      if (test())
	continue;
      abort ();
    }
  if (i != count)
    abort ();
  return 0;
}
