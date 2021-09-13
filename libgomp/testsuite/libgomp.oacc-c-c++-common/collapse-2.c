/* { dg-do run } */

#include <stdlib.h>

int
main (void)
{
  int i, j, k, l = 0, f = 0, x = 0, l2 = 0;
  int m1 = 4, m2 = -5, m3 = 17;

#pragma acc parallel
  #pragma acc loop seq collapse(3) reduction(+:l)
    for (i = -2; i < m1; i++)
      for (j = m2; j < -2; j++)
	{
	  for (k = 13; k < m3; k++)
	    {
              if ((i + 2) * 12 + (j + 5) * 4 + (k - 13) !=  9 + f++)
		l++;
	    }
	}

  /*  Test loop with > condition.  */
#pragma acc parallel
  #pragma acc loop seq collapse(3) reduction(+:l2)
    for (i = -2; i < m1; i++)
      for (j = -3; j > (m2 - 1); j--)
	{
	  for (k = 13; k < m3; k++)
	    {
	      if ((i + 2) * 12 + (j + 5) * 4 + (k - 13) !=  9 + f++)
		l2++;
	    }
	}

    for (i = -2; i < m1; i++)
      for (j = m2; j < -2; j++)
	{
	  for (k = 13; k < m3; k++)
	    {
              if ((i + 2) * 12 + (j + 5) * 4 + (k - 13) !=  9 + f++)
		x++;
	    }
	}

  if (l != x || l2 != x)
    abort ();

  return 0;
}
