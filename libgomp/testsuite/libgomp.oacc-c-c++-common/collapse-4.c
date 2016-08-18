/* { dg-do run } */

#include <string.h>

int
main (void)
{
  int l = 0;
  int b[3][3];
  int i, j;

  memset (b, '\0', sizeof (b));

#pragma acc parallel copy(b[0:3][0:3])
    {
#pragma acc loop collapse(2) reduction(+:l)
	for (i = 0; i < 2; i++)
	  for (j = 0; j < 2; j++)
	    if (b[i][j] != 16)
		  l += 1;
    }

  if (l != 2 * 2)
    __builtin_abort();

  return 0;
}
