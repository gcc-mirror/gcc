#if defined(__hppa__) && !defined(__LP64__)
#define NUM_THREADS 50
#else
#define NUM_THREADS 64
#endif

extern "C" void abort (void);
int a;

void
foo ()
{
  int i;
  a = 30;
#pragma omp barrier
#pragma omp for lastprivate (a)
  for (i = 0; i < 1024; i++)
    {
      a = i;
    }
  if (a != 1023)
    abort ();
}

int
main (void)
{
#pragma omp parallel num_threads (NUM_THREADS)
  foo ();

  return 0;
}
