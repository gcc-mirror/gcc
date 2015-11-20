#include <assert.h>
#include <openacc.h>

int
main (int argc, char *argv[])
{
  int res, res2 = 0;

#if defined(ACC_DEVICE_TYPE_host)
# define GANGS 1
#else
# define GANGS 256
#endif
  #pragma acc parallel num_gangs(GANGS) copy(res2) async(1)
  {
    #pragma acc atomic
    res2 += 5;
  }
  res = GANGS * 5;

  acc_wait (1);

  assert (res == res2);
#undef GANGS

  res = res2 = 1;

#if defined(ACC_DEVICE_TYPE_host)
# define GANGS 1
#else
# define GANGS 8
#endif
  #pragma acc parallel num_gangs(GANGS) copy(res2) async(1)
  {
    #pragma acc atomic
    res2 *= 5;
  }
  for (int i = 0; i < GANGS; ++i)
    res *= 5;

  acc_wait (1);

  assert (res == res2);

  return 0;
}
