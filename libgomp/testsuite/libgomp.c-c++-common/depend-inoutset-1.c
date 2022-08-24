#include <omp.h>
#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int a[8] = {};
  omp_depend_t d1, d2;
  #pragma omp depobj (d1) depend(inoutset: a)
  #pragma omp depobj (d2) depend(inout: a)
  #pragma omp depobj (d2) update(inoutset)
  #pragma omp parallel
  {
    #pragma omp barrier
    #pragma omp master
    {
    #pragma omp task shared(a) depend(out: a)
    {
      usleep (5000);
      a[0] = 1; a[1] = 2; a[2] = 3; a[3] = 4;
    }
    /* The above task needs to finish first.  */
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 1 || a[1] != 2 || a[2] != 3 || a[3] != 4)
	abort ();
      usleep (5000);
      a[4] = 42;
    }
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 1 || a[1] != 2 || a[2] != 3 || a[3] != 4)
	abort ();
      usleep (5000);
      a[5] = 43;
    }
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 1 || a[1] != 2 || a[2] != 3 || a[3] != 4)
	abort ();
      usleep (5000);
      a[6] = 44;
    }
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 1 || a[1] != 2 || a[2] != 3 || a[3] != 4)
	abort ();
      usleep (5000);
      a[7] = 45;
    }
    /* The above 4 tasks can be scheduled in any order but need to wait
       for the depend(out: a) task.  */
    #pragma omp task shared(a) depend(inoutset: a)
    {
      if (a[4] != 42 || a[5] != 43 || a[6] != 44 || a[7] != 45)
	abort ();
      usleep (5000);
      a[0] = 42;
    }
    #pragma omp task shared(a) depend(iterator(i=1:3:2), inoutset: a)
    {
      if (a[4] != 42 || a[5] != 43 || a[6] != 44 || a[7] != 45)
	abort ();
      usleep (5000);
      a[1] = 43;
    }
    #pragma omp task shared(a) depend(depobj: d1)
    {
      if (a[4] != 42 || a[5] != 43 || a[6] != 44 || a[7] != 45)
	abort ();
      usleep (5000);
      a[2] = 44;
    }
    #pragma omp task shared(a) depend(depobj: d2)
    {
      if (a[4] != 42 || a[5] != 43 || a[6] != 44 || a[7] != 45)
	abort ();
      usleep (5000);
      a[3] = 45;
    }
    /* The above 4 tasks can be scheduled in any order but need to wait
       for all the above depend(in: a) tasks.  */
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 42 || a[1] != 43 || a[2] != 44 || a[3] != 45)
	abort ();
      usleep (5000);
      a[4] = 46;
    }
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 42 || a[1] != 43 || a[2] != 44 || a[3] != 45)
	abort ();
      usleep (5000);
      a[5] = 47;
    }
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 42 || a[1] != 43 || a[2] != 44 || a[3] != 45)
	abort ();
      usleep (5000);
      a[6] = 48;
    }
    #pragma omp task shared(a) depend(in: a)
    {
      if (a[0] != 42 || a[1] != 43 || a[2] != 44 || a[3] != 45)
	abort ();
      usleep (5000);
      a[7] = 49;
    }
    /* The above 4 tasks can be scheduled in any order but need to wait
       for all the above depend(inoutset: a),
       depend(iterator(i=1:3:2), inoutset: a), depend(depobj: d1) and
       depend(depobj: d2) tasks.  */
    #pragma omp task shared(a) depend(inoutset: a)
    {
      if (a[4] != 46|| a[5] != 47 || a[6] != 48 || a[7] != 49)
	abort ();
      usleep (5000);
      a[0] = 50;
    }
    /* The above task needs to wait for all the above 4 depend(in: a)
       tasks.  */
    #pragma omp task shared(a) depend(out: a)
    {
      if (a[0] != 50 || a[4] != 46|| a[5] != 47 || a[6] != 48 || a[7] != 49)
	abort ();
      usleep (5000);
      a[0] = 51;
    }
    /* The above task needs to wait for the above depend(inoutset: a) task.  */
    #pragma omp task shared(a) depend(inoutset: a)
    {
      if (a[0] != 51 || a[4] != 46|| a[5] != 47 || a[6] != 48 || a[7] != 49)
	abort ();
      usleep (5000);
      a[0] = 52;
    }
    /* The above task needs to wait for the above depend(out: a) task.  */
    #pragma omp task shared(a) depend(mutexinoutset: a)
    {
      if (a[0] != 52 || a[4] != 46|| a[5] != 47 || a[6] != 48 || a[7] != 49)
	abort ();
      usleep (5000);
      a[0] = 53;
    }
    /* The above task needs to wait for the above depend(inoutset: a) task.  */
    #pragma omp task shared(a) depend(inoutset: a)
    {
      if (a[0] != 53 || a[4] != 46|| a[5] != 47 || a[6] != 48 || a[7] != 49)
	abort ();
      usleep (5000);
      a[0] = 54;
    }
    /* The above task needs to wait for the above
       depend(mutexinoutset: a) task.  */
    }
  }
  if (a[0] != 54 || a[1] != 43 || a[2] != 44 || a[3] != 45
      || a[4] != 46|| a[5] != 47 || a[6] != 48 || a[7] != 49)
    abort ();
  return 0;
}
