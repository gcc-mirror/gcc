/* { dg-require-effective-target offload_device_nonshared_as } */

#include <stdlib.h>
#include <assert.h>

#define N 40

int sum;
int var1 = 1;
int var2 = 2;

#pragma omp declare target
int D[N];
#pragma omp end declare target

void enter_data (int *X)
{
  #pragma omp target enter data map(to: var1, var2, X[:N]) map(alloc: sum)
}

void exit_data_0 (int *D)
{
  #pragma omp target exit data map(delete: D[:N])
}

void exit_data_1 ()
{
  #pragma omp target exit data map(from: var1)
}

void exit_data_2 (int *X)
{
  #pragma omp target exit data map(from: var2) map(release: X[:N], sum)
}

void exit_data_3 (int *p)
{
  #pragma omp target exit data map(from: p[:0])
}

void test_nested ()
{
  int X = 0, Y = 0, Z = 0;

  #pragma omp target data map(from: X, Y, Z)
    {
      #pragma omp target data map(from: X, Y, Z)
	{
	  #pragma omp target map(from: X, Y, Z)
	    X = Y = Z = 1337;
	  assert (X == 0);
	  assert (Y == 0);
	  assert (Z == 0);

	  #pragma omp target exit data map(from: X) map(release: Y)
	  assert (X == 0);
	  assert (Y == 0);

	  #pragma omp target exit data map(release: Y) map(delete: Z)
	  assert (Y == 0);
	  assert (Z == 0);
	}
      assert (X == 1337);
      assert (Y == 0);
      assert (Z == 0);

      #pragma omp target map(from: X)
	X = 2448;
      assert (X == 2448);
      assert (Y == 0);
      assert (Z == 0);

      X = 4896;
    }
  assert (X == 4896);
  assert (Y == 0);
  assert (Z == 0);
}

int main ()
{
  int *X = malloc (N * sizeof (int));
  int *Y = malloc (N * sizeof (int));
  X[10] = 10;
  Y[20] = 20;
  enter_data (X);

  exit_data_0 (D); /* This should have no effect on D.  */

  #pragma omp target map(alloc: var1, var2, X[:N]) map(to: Y[:N]) map(always from: sum)
    {
      var1 += X[10];
      var2 += Y[20];
      sum = var1 + var2;
      D[sum]++;
    }

  assert (var1 == 1);
  assert (var2 == 2);
  assert (sum == 33);

  exit_data_1 ();
  assert (var1 == 11);
  assert (var2 == 2);

  /* Increase refcount of already mapped X[0:N].  */
  #pragma omp target enter data map(alloc: X[16:1])

  exit_data_2 (X);
  assert (var2 == 22);

  exit_data_3 (X + 5); /* Unmap X[0:N].  */

  free (X);
  free (Y);

  test_nested ();

  return 0;
}
