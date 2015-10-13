/* { dg-require-effective-target offload_device_nonshared_as } */

#include <stdlib.h>
#include <assert.h>

#define N 32

void test_array_section (int *p)
{
  #pragma omp target data map(alloc: p[0:N])
    {
      int ok = 1;
      for (int i = 10; i < 10 + 4; i++)
	p[i] = 997 * i;

      #pragma omp target map(always to:p[10:4]) map(tofrom: ok)
	for (int i = 10; i < 10 + 4; i++)
	  if (p[i] != 997 * i)
	    ok = 0;

      assert (ok);

      #pragma omp target map(always from:p[7:9])
	for (int i = 0; i < N; i++)
	  p[i] = i;
    }
}

int main ()
{
  int aa = 0, bb = 0, cc = 0, dd = 0;

  #pragma omp target data map(tofrom: aa) map(to: bb) map(from: cc, dd)
    {
      int ok;
      aa = bb = cc = 1;

      /* Set dd on target to 0 for the further check.  */
      #pragma omp target map(always to: dd)
	;

      dd = 1;
      #pragma omp target map(tofrom: aa) map(always to: bb) \
	map(always from: cc) map(to: dd) map(from: ok)
	{
	  /* bb is always to, aa and dd are not.  */
	  ok = (aa == 0) && (bb == 1) && (dd == 0);
	  aa = bb = cc = dd = 2;
	}

      assert (ok);
      assert (aa == 1);
      assert (bb == 1);
      assert (cc == 2); /* cc is always from.  */
      assert (dd == 1);

      dd = 3;
      #pragma omp target map(from: cc) map(always to: dd) map(from: ok)
	{
	  ok = (dd == 3); /* dd is always to.  */
	  cc = dd = 4;
	}

      assert (ok);
      assert (cc == 2);
      assert (dd == 3);
    }

  assert (aa == 2);
  assert (bb == 1);
  assert (cc == 4);
  assert (dd == 4);

  int *array = calloc (N, sizeof (int));
  test_array_section (array);

  for (int i = 0; i < 7; i++)
    assert (array[i] == 0);
  for (int i = 7; i < 7 + 9; i++)
    assert (array[i] == i);
  for (int i = 7 + 9; i < N; i++)
    assert (array[i] == 0);

  free (array);
  return 0;
}
