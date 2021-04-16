/* { dg-do run } */

/* Test reductions on explicitly private variables.  */

#include <assert.h>

int
main ()
{
  int i, j, red[10];
  int v;

  for (i = 0; i < 10; i++)
    red[i] = -1;

#pragma acc parallel copyout(red)
  {
#pragma acc loop gang private(v)
    for (j = 0; j < 10; j++)
      {
	v = j;

#pragma acc loop vector reduction (+:v)
	for (i = 0; i < 100; i++)
	  v++;

	red[j] = v;
      }
  }

  for (i = 0; i < 10; i++)
    assert (red[i] == i + 100);

  return 0;
}
