#include <assert.h>

/* Test of reduction on both parallel and loop directives (worker and
   vector-partitioned loops individually in gang-partitioned mode, int
   type).  */

int
main (int argc, char *argv[])
{
  int i, j, arr[32768], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
    reduction(+:res) copy(res)
  {
    #pragma acc loop gang /* { dg-warning "nested loop in reduction needs reduction clause for 'res'" "TODO" } */
    for (j = 0; j < 32; j++)
      {
	#pragma acc loop worker reduction(+:res)
	for (i = 0; i < 1024; i++)
	  res += arr[j * 1024 + i];

	#pragma acc loop vector reduction(+:res)
	for (i = 1023; i >= 0; i--)
	  res += arr[j * 1024 + i];
      }
  }

  for (j = 0; j < 32; j++)
    for (i = 0; i < 1024; i++)
      hres += arr[j * 1024 + i] * 2;

  assert (res == hres);

  return 0;
}
