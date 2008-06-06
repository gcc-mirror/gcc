/* { dg-do run } */

#include <stdlib.h>
#include <omp.h>

int
main (void)
{
  int i, j, k, l = 0, f = 0;
  int m1 = 4, m2 = -5, m3 = 17;

  #pragma omp parallel for num_threads (8) collapse(3) \
		       schedule(static, 9) reduction(+:l) \
		       firstprivate(f)
    for (i = -2; i < m1; i++)
      for (j = m2; j < -2; j++)
	{
	  for (k = 13; k < m3; k++)
	    {
	      if (omp_get_num_threads () == 8
		  && ((i + 2) * 12 + (j + 5) * 4 + (k - 13)
		      != (omp_get_thread_num () * 9
			  + f++)))
		l++;
	    }
	}
  if (l)
    abort ();
  return 0;
}
