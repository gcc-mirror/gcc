#include <assert.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N 100

#define GANG_ID(I)						\
  (acc_on_device (acc_device_not_host)				\
   ? __builtin_goacc_parlevel_id (GOMP_DIM_GANG)					\
   : (I))

void
test_static(int *a, int num_gangs, int sarg)
{
  int i, j;

  if (acc_on_device (acc_device_host))
    return;

   if (sarg == 0)
    sarg = 1;

  for (i = 0; i < N / sarg; i++)
    for (j = 0; j < sarg; j++)
      assert (a[i*sarg+j] == i % num_gangs);
}

void
test_nonstatic(int *a, int gangs)
{
  int i, j;

  if (acc_on_device (acc_device_host))
    return;

  for (i = 0; i < N; i+=gangs)
    for (j = 0; j < gangs; j++)
      assert (a[i+j] == i/gangs);
}

int
main ()
{
  int a[N];
  int i, x;

#pragma acc parallel loop gang (static:*) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_nonstatic (a, 10);

#pragma acc parallel loop gang (static:1) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_static (a, 10, 1);

#pragma acc parallel loop gang (static:2) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_static (a, 10, 2);

#pragma acc parallel loop gang (static:5) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_static (a, 10, 5);

#pragma acc parallel loop gang (static:20) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_static (a, 10, 20);

  /* Non-static gang.  */
#pragma acc parallel loop gang num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_nonstatic (a, 10);

  /* Static arguments with a variable expression.  */

  x = 20;
#pragma acc parallel loop gang (static:0+x) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_static (a, 10, 20);

  x = 20;
#pragma acc parallel loop gang (static:x) num_gangs (10)
  for (i = 0; i < 100; i++)
    a[i] = GANG_ID (i);

  test_static (a, 10, 20);

  return 0;
}
