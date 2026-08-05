/* { dg-do run { target { ! openacc_host_selected } } } */

#include <stdlib.h>
#include <assert.h>
#include <openacc.h>

struct dc
{
  int a;
  int **b;
};

int
main ()
{
  int n = 100, i, j, k;
  struct dc v = { .a = 3 };

  v.b = (int **) malloc (sizeof (int *) * n);
  for (i = 0; i < n; i++)
    v.b[i] = (int *) malloc (sizeof (int) * n);

  for (k = 0; k < 16; k++)
    {
#pragma acc data copy(v)
      {
#pragma acc data copy(v.b[:n])
	{
	  for (i = 0; i < n; i++)
	    {
	      acc_copyin (v.b[i], sizeof (int) * n);
	      acc_attach ((void **) &v.b[i]);
	    }

#pragma acc parallel loop
	  for (i = 0; i < n; i++)
	    for (j = 0; j < n; j++)
	      v.b[i][j] = v.a + i + j;

	  for (i = 0; i < n; i++)
	    {
	      acc_detach ((void **) &v.b[i]);
	      acc_copyout (v.b[i], sizeof (int) * n);
	    }
	}
      }

      for (i = 0; i < n; i++)
	for (j = 0; j < n; j++)
	  assert (v.b[i][j] == v.a + i + j);

      assert (!acc_is_present (&v, sizeof (v)));
      assert (!acc_is_present (v.b, sizeof (int *) * n));
      for (i = 0; i < n; i++)
	assert (!acc_is_present (v.b[i], sizeof (int) * n));
    }

  return 0;
}
