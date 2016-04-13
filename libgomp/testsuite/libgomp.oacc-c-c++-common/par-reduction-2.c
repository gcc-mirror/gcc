/* { dg-additional-options "-w" } */

#include <assert.h>
#include <openacc.h>

/* Test of reduction on parallel directive (with async).  */

int
main (int argc, char *argv[])
{
  int res, res1 = 0, res2 = 0;

#if defined(ACC_DEVICE_TYPE_host)
# define GANGS 1
#else
# define GANGS 256
#endif
  #pragma acc parallel num_gangs(GANGS) num_workers(32) vector_length(32) \
    reduction(+:res1) copy(res1, res2) async(1)
  {
    res1 += 5;

    #pragma acc atomic
    res2 += 5;
  }
  res = GANGS * 5;

  acc_wait (1);

  assert (res == res1);
  assert (res == res2);
#undef GANGS

  res = res1 = res2 = 1;

#if defined(ACC_DEVICE_TYPE_host)
# define GANGS 1
#else
# define GANGS 8
#endif
  #pragma acc parallel num_gangs(GANGS) num_workers(32) vector_length(32) \
    reduction(*:res1) copy(res1, res2) async(1)
  {
    res1 *= 5;

    #pragma acc atomic
    res2 *= 5;
  }
  for (int i = 0; i < GANGS; ++i)
    res *= 5;

  acc_wait (1);

  assert (res == res1);
  assert (res == res2);

  return 0;
}
