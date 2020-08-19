/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <openacc.h>
#include <stdint.h>

#define N 128

int
main ()
{
  int *ptrarr[N];
  int otherarr[N];
  int sum = 0, hostsum = 0;

  for (int i = 0; i < N; i++)
    {
      otherarr[i] = i * 2 + 1;
      ptrarr[i] = &otherarr[N - 1 - i];
      hostsum += otherarr[i];
    }

  acc_copyin (otherarr, sizeof otherarr);
  acc_copyin (ptrarr, sizeof ptrarr);

  for (int i = 0; i < N; i++)
    {
      #pragma acc enter data attach(ptrarr[i])
    }

  #pragma acc parallel loop copyin(ptrarr[0:N], otherarr[0:N]) \
		       reduction(+:sum)
  for (int i = 0; i < N; i++)
    sum += *ptrarr[i];

  for (int i = 0; i < N; i++)
    {
      #pragma acc exit data detach(ptrarr[i])
    }

  assert (sum == hostsum);

  acc_delete (ptrarr, sizeof ptrarr);
  acc_delete (otherarr, sizeof otherarr);

  return 0;
}

