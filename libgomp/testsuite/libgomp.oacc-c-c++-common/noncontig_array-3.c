/* { dg-do run } */

#include <assert.h>
#include "noncontig_array-utils.h"

int main (void)
{
  int n = 20, x = 5, y = 12;
  int *****a = (int *****) create_ncarray (sizeof (int), n, 5);

  int sum1 = 0, sum2 = 0, sum3 = 0;

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++)
	for (int l = 0; l < n; l++)
	  for (int m = 0; m < n; m++)
	    {
	      a[i][j][k][l][m] = 1;
	      sum1++;
	    }

  #pragma acc parallel copy (a[x:y][x:y][x:y][x:y][x:y]) copy(sum2)
  {
    for (int i = x; i < x + y; i++)
      for (int j = x; j < x + y; j++)
	for (int k = x; k < x + y; k++)
	  for (int l = x; l < x + y; l++)
	    for (int m = x; m < x + y; m++)
	      {
		a[i][j][k][l][m] = 0;
		sum2++;
	      }
  }

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++)
	for (int l = 0; l < n; l++)
	  for (int m = 0; m < n; m++)
	    sum3 += a[i][j][k][l][m];

  assert (sum1 == sum2 + sum3);
  return 0;
}
