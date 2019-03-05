#ifdef __cplusplus
extern "C"
#endif
void abort (void);

int a;
long int b = 1;

void
foo (void)
{
  int i;
  for (i = 0; i < 2; i++)
    #pragma omp task in_reduction (+: a) in_reduction (*: b)
    {
      a += 7;
      b *= 2;
    }
}

int
main ()
{
  int c = 0;
  #pragma omp parallel
  #pragma omp single
  {
    long int d = 1;
    #pragma omp taskgroup task_reduction (+: a, c) task_reduction (*: b, d)
    {
      int i;
      for (i = 0; i < 4; i++)
	#pragma omp task in_reduction (+: a, c) in_reduction (*: b, d)
	{
	  int j;
	  a += 7;
	  b *= 2;
	  for (j = 0; j < 2; j++)
	    #pragma omp task in_reduction (+: a, c) in_reduction (*: b, d)
	    {
	      a += 7;
	      b *= 2;
	      c += 9;
	      d *= 3;
	      foo ();
	    }
	  c += 9;
	  d *= 3;
	}
    }
#define THREEP4 (3L * 3L * 3L * 3L)
    if (d != (THREEP4 * THREEP4 * THREEP4))
      abort ();
  }
  if (a != 28 * 7 || b != (1L << 28) || c != 12 * 9)
    abort ();
  return 0;
}
