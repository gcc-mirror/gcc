/* { dg-do run } */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int
main (void)
{
  int i2, l = 0, r = 0;
  int a[3][3][3];

  memset (a, '\0', sizeof (a));
  #pragma acc parallel
  #pragma acc loop collapse(4 - 1)
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++)
	for (int k = 0; k < 2; k++)
	  a[i][j][k] = i + j * 4 + k * 16;
#pragma acc parallel
    {
      #pragma acc loop collapse(2) reduction(|:l)
	for (i2 = 0; i2 < 2; i2++)
	  for (int j = 0; j < 2; j++)
	    for (int k = 0; k < 2; k++)
	      if (a[i2][j][k] != i2 + j * 4 + k * 16)
		l += 1;
    }

    for (i2 = 0; i2 < 2; i2++)
      for (int j = 0; j < 2; j++)
	for (int k = 0; k < 2; k++)
	  if (a[i2][j][k] != i2 + j * 4 + k * 16)
	    r += 1;

  if (l != r)
    abort ();
  return 0;
}
