/* { dg-do run } */

#include <assert.h>
#include "noncontig_array-utils.h"

int
main (void)
{
  int n = 10;
  int ***a = (int ***) create_ncarray (sizeof (int), n, 3);
  int ***b = (int ***) create_ncarray (sizeof (int), n, 3);
  int ***c = (int ***) create_ncarray (sizeof (int), n, 3);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++)
	{
	  a[i][j][k] = i + j * k + k;
	  b[i][j][k] = j + k * i + i * j;
	  c[i][j][k] = a[i][j][k];
	}

  #pragma acc parallel copy (a[0:n][0:n][0:n]) copyin (b[0:n][0:n][0:n])
  {
    for (int i = 0; i < n; i++)
      for (int j = 0; j < n; j++)
	for (int k = 0; k < n; k++)
	  a[i][j][k] += b[k][j][i] + i + j + k;
  }

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++)
	assert (a[i][j][k] == c[i][j][k] + b[k][j][i] + i + j + k);

  return 0;
}
