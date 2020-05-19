/* { dg-do run { target { ! openacc_host_selected } } } */

#include <stdlib.h>
#include <assert.h>
#include <openacc.h>

struct dc
{
  int a;
  int *b;
};

int
main ()
{
  int n = 100, i, j, k;
  struct dc v = { .a = 3 };

  v.b = (int *) malloc (sizeof (int) * n);

  for (k = 0; k < 16; k++)
    {
      /* Here, we do not explicitly copy the enclosing structure, but work
	 with fields directly.  Make sure attachment counters and reference
	 counters work properly in that case.  */
#pragma acc enter data copyin(v.a, v.b[0:n])
#pragma acc enter data pcopyin(v.b[0:n])
#pragma acc enter data pcopyin(v.b[0:n])

#pragma acc parallel loop present(v.a, v.b)
      for (i = 0; i < n; i++)
	v.b[i] = v.a + i;

#pragma acc exit data copyout(v.b[:n]) finalize
#pragma acc exit data delete(v.a)

      for (i = 0; i < n; i++)
	assert (v.b[i] == v.a + i);

      assert (!acc_is_present (&v, sizeof (v)));
      assert (!acc_is_present (v.b, sizeof (int) * n));
    }

  return 0;
}
