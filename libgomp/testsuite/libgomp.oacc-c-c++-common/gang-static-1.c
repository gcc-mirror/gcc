#include <assert.h>

#define N 100

void
test (int *a, int *b, int sarg)
{
  int i;

  for (i = 0; i < N; i++)
    assert (a[i] == b[i] + sarg);
}

int
main ()
{
  int a[N], b[N];
  int i;

  for (i = 0; i < N; i++)
    b[i] = i+1;

#pragma acc parallel loop gang (static:*) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = b[i] + 0;

  test (a, b, 0);

#pragma acc parallel loop gang (static:1) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = b[i] + 1;

  test (a, b, 1);

#pragma acc parallel loop gang (static:5) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = b[i] + 5;

  test (a, b, 5);

#pragma acc parallel loop gang (static:20) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = b[i] + 20;

  test (a, b, 20);

  return 0;
}
