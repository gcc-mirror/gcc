#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int a[8], b[8], i;
  for (i = 0; i < 8; i++)
    {
      a[i] = i;
      b[i] = 2 * i;
    }
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared(a) depend(in: a[0])
    {
      usleep (5000);
      a[0] = 42;
    }
    #pragma omp task shared(a) depend(out: a[1])
    {
      usleep (5000);
      a[1] = 43;
    }
    #pragma omp task shared(a) depend(inout: a[2])
    {
      usleep (5000);
      a[2] = 44;
    }
    #pragma omp task shared(a) depend(mutexinoutset: a[3])
    {
      usleep (5000);
      a[3] = 45;
    }
    #pragma omp task shared(a)
    {
      usleep (15000);
      a[4] = 46;
    }
    #pragma omp task shared(b) depend(in: b[0])
    {
      usleep (5000);
      b[0] = 47;
    }
    #pragma omp task shared(b) depend(in: b[4])
    {
      usleep (5000);
      b[4] = 48;
    }
    #pragma omp task shared(b) depend(inoutset: b[5])
    {
      usleep (5000);
      b[5] = 49;
    }
    /* None of the above tasks depend on each other.
       The following task depends on all but the a[4] = 46; one.  */
    #pragma omp task shared(a, b) depend(iterator (j=0:7), inout: omp_all_memory) private(i)
    {
      if (a[0] != 42 || a[1] != 43 || a[2] != 44 || a[3] != 45
	  || a[5] != 5 || a[6] != 6 || a[7] != 7
	  || b[0] != 47 || b[1] != 2 || b[2] != 4 || b[3] != 6
	  || b[4] != 48 || b[5] != 49 || b[6] != 12 || b[7] != 14)
	abort ();
      for (i = 0; i < 8; ++i)
	if (i != 4)
	  a[i] = 3 * i + 7;
      for (i = 0; i < 8; ++i)
	b[i] = 4 * i - 7;
    }
    /* The following task depends on both b[0] = 47; and
       above omp_all_memory tasks, but as the latter depends on
       the former, effectively it is dependent just on the omp_all_memory
       task.  */
    #pragma omp task shared(b) depend(inout: b[0])
    {
      usleep (5000);
      b[0] = 49;
    }
    /* The following task depends on all the above except a[4] = 46; one,
       but it can be reduced to dependency on the above omp_all_memory
       one and b[0] = 49; one.  */
    #pragma omp task shared(a, b) depend(inout: b[7]) depend(iterator(j=4:5), out: omp_all_memory) \
		     depend(inout: b[6]) private(i)
    {
      for (i = 0; i < 8; ++i)
	if (i != 4)
	  {
	    if (a[i] != 3 * i + 7)
	      abort ();
	    a[i] = 5 * i + 50;
	  }
      if (b[0] != 49)
	abort ();
      b[0] = 6 * i + 57;
      for (i = 1; i < 8; ++i)
	{
	  if (b[i] != 4 * i - 7) 
	    abort ();
	  b[i] = 6 * i + 57;
	}
    }
    #pragma omp taskwait
    if (a[4] != 46)
      abort ();
  }
  return 0;
}
