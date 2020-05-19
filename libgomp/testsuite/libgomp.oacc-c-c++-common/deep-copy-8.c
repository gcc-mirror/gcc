/* { dg-do run { target { ! openacc_host_selected } } } */

#include <stdlib.h>
#include <assert.h>
#include <openacc.h>

struct dc
{
  int a;
  int *b;
  int *c;
  int *d;
};

int
main ()
{
  int n = 100, i, j, k;
  struct dc v = { .a = 3 };

  v.b = (int *) malloc (sizeof (int) * n);
  v.c = (int *) malloc (sizeof (int) * n);
  v.d = (int *) malloc (sizeof (int) * n);

#pragma acc enter data copyin(v)

  for (k = 0; k < 16; k++)
    {
#pragma acc enter data copyin(v.a, v.b[:n], v.c[:n], v.d[:n])

#pragma acc parallel loop
      for (i = 0; i < n; i++)
	v.b[i] = v.a + i;

#pragma acc exit data copyout(v.b[:n])
#pragma acc exit data copyout(v.c[:n])
#pragma acc exit data copyout(v.d[:n])
#pragma acc exit data copyout(v.a)

      for (i = 0; i < n; i++)
	assert (v.b[i] == v.a + i);

      assert (acc_is_present (&v, sizeof (v)));
      assert (!acc_is_present (v.b, sizeof (int) * n));
      assert (!acc_is_present (v.c, sizeof (int) * n));
      assert (!acc_is_present (v.d, sizeof (int) * n));
    }

#pragma acc exit data copyout(v)

  assert (!acc_is_present (&v, sizeof (v)));

  return 0;
}
