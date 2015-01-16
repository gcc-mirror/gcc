/* { dg-do run } */

#include <string.h>
#include <stdlib.h>

int
main (void)
{
  int i, j, k, l = 0;
  int a[3][3][3];

  memset (a, '\0', sizeof (a));
  #pragma acc parallel
  #pragma acc loop collapse(4 - 1)
    for (i = 0; i < 2; i++)
      for (j = 0; j < 2; j++)
	for (k = 0; k < 2; k++)
	  a[i][j][k] = i + j * 4 + k * 16;
  #pragma acc parallel
    {
      #pragma acc loop collapse(2) reduction(|:l)
	for (i = 0; i < 2; i++)
	  for (j = 0; j < 2; j++)
	    for (k = 0; k < 2; k++)
	      if (a[i][j][k] != i + j * 4 + k * 16)
		l = 1;
    }
  if (l)
    abort ();
  return 0;
}
