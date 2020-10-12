/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

/* Variant of 'deep-copy-7.c'.  */

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
#pragma acc enter data copyin(v.a, v.b[0:n]) // 1
      assert (acc_is_present (&v.b, sizeof v.b));
      assert (acc_is_present (v.b, sizeof (int) * n));
#pragma acc enter data pcopyin(v.b[0:n]) // 2
#pragma acc enter data pcopyin(v.b[0:n]) // 3

#pragma acc parallel loop present(v.a, v.b)
      for (i = 0; i < n; i++)
	v.b[i] = k + v.a + i;

      switch (k % 5)
	{ // All optional.
	case 0:
	  break;
	case 1:
	  ; //TODO PR95901
#pragma acc exit data detach(v.b) finalize
	  break;
	case 2:
	  ; //TODO PR95901
#pragma acc exit data detach(v.b)
	  break;
	case 3:
	  acc_detach_finalize ((void **) &v.b);
	  break;
	case 4:
	  acc_detach ((void **) &v.b);
	  break;
	}
      assert (acc_is_present (&v.b, sizeof v.b));
      assert (acc_is_present (v.b, sizeof (int) * n));
      { // 3
	acc_delete (&v.b, sizeof v.b);
	assert (acc_is_present (&v.b, sizeof v.b));
	acc_copyout (v.b, sizeof (int) * n);
	assert (acc_is_present (v.b, sizeof (int) * n));
      }
      { // 2
	acc_delete (&v.b, sizeof v.b);
	assert (acc_is_present (&v.b, sizeof v.b));
	acc_copyout (v.b, sizeof (int) * n);
	assert (acc_is_present (v.b, sizeof (int) * n));
      }
      { // 1
	acc_delete (&v.b, sizeof v.b);
	assert (!acc_is_present (&v.b, sizeof v.b));
	acc_copyout (v.b, sizeof (int) * n);
	assert (!acc_is_present (v.b, sizeof (int) * n));
      }
#pragma acc exit data delete(v.a)

      for (i = 0; i < n; i++)
	assert (v.b[i] == k + v.a + i);

      assert (!acc_is_present (&v, sizeof (v)));
    }

  return 0;
}
