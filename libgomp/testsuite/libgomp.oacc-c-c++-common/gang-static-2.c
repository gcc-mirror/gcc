/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* This code uses nvptx inline assembly guarded with acc_on_device, which is
   not optimized away at -O0, and then confuses the target assembler.
   { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

#include <assert.h>
#include <openacc.h>

#define N 100

#define GANG_ID(I)						\
  (acc_on_device (acc_device_nvidia)				\
   ? ({unsigned __r;						\
       __asm__ volatile ("mov.u32 %0,%%ctaid.x;" : "=r" (__r));	\
       __r; }) : (I))

int
test_static(int *a, int num_gangs, int sarg)
{
  int i, j;

  if (sarg == 0)
    sarg = 1;

  for (i = 0; i < N / sarg; i++)
    for (j = 0; j < sarg; j++)
      assert (a[i*sarg+j] == i % num_gangs);
}

int
test_nonstatic(int *a, int gangs)
{
  int i, j;

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
