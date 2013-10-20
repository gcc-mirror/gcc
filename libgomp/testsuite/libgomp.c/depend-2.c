#include <stdlib.h>
#include <unistd.h>

void
foo (int do_sleep)
{
  int a[64], i, *p = a + 4, x = 0;
  asm volatile ("" : "+r" (p));
  for (i = 0; i < 64; i++)
    a[i] = i + 8;
  #pragma omp parallel private (i)
  {
    #pragma omp single nowait
    {
      for (i = 0; i < 8; i++)
	{
	  #pragma omp task depend(out: a[i * 8 : 4])
	    a[i * 8] += (i + 2) * 9;
	  #pragma omp task depend(out: p[i * 8 : 2])
	    p[i * 8] += (i + 3) * 10;
	  #pragma omp task depend(out: x)
	    x = 1;
	}
      for (i = 0; i < 8; i++)
	#pragma omp task depend(in: a[i * 8 : 4]) \
			 depend(inout: a[i * 8 + 4 : 2]) \
			 depend(in: a[0 : 4]) depend(in: x)
	{
	  if (a[0] != 8 + 2 * 9 || x != 1)
	    abort ();
	  if (a[i * 8] != i * 8 + 8 + (i + 2) * 9)
	    abort ();
	  if (a[4 + i * 8] != 4 + i * 8 + 8 + (i + 3) * 10)
	    abort ();
	  p[i * 8] += a[i * 8];
	}
      for (i = 0; i < 8; i++)
	#pragma omp task depend(inout: a[i * 8 : 4]) \
			 depend(in: p[i * 8 : 2]) \
			 depend(in: p[0 : 2], x)
	{
	  if (p[0] != 4 + 8 + 3 * 10 + 0 + 8 + 2 * 9 || x != 1)
	    abort ();
	  if (a[i * 8] != i * 8 + 8 + (i + 2) * 9)
	    abort ();
	  if (a[4 + i * 8] != (4 + i * 8 + 8 + (i + 3) * 10
			       + i * 8 + 8 + (i + 2) * 9))
	    abort ();
	  a[i * 8] += 2;
	}
      for (i = 0; i < 4; i++)
	#pragma omp task depend(in: a[i * 16 : 4], a[i * 16 + 8 : 4], x)
	{
	  if (a[i * 16] != i * 16 + 8 + (2 * i + 2) * 9 + 2 || x != 1)
	    abort ();
	  if (p[i * 16 + 4] != i * 16 + 8 + 8 + (2 * i + 1 + 2) * 9 + 2)
	    abort ();
	}
    }
    if (do_sleep)
      sleep (1);
  }
}

int
main ()
{
  foo (1);
  foo (0);
  return 0;
}
