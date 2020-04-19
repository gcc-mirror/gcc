/* { dg-do run } */

#include <assert.h>
#include "noncontig_array-utils.h"

int main (void)
{
  int n = 128;
  double ***a = (double ***) create_ncarray (sizeof (double), n, 3);
  double ***b = (double ***) create_ncarray (sizeof (double), n, 3);

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++)
	a[i][j][k] = i + j + k + i * j * k;

  /* This test exercises async copyout of non-contiguous array rows.  */
  #pragma acc parallel copyin(a[0:n][0:n][0:n]) copyout(b[0:n][0:n][0:n]) async(5)
  {
    #pragma acc loop gang
    for (int i = 0; i < n; i++)
      #pragma acc loop vector
      for (int j = 0; j < n; j++)
	for (int k = 0; k < n; k++)
	  b[i][j][k] = a[i][j][k] * 2.0;
  }

  #pragma acc wait (5)

  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++)
	assert (b[i][j][k] == a[i][j][k] * 2.0);

  return 0;
}
