/* Tests of reduction on loop directive.  */

#include <assert.h>


/* Test of reduction on loop directive (gangs, non-private reduction
   variable).  */

void g_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copy(res)
  {
    #pragma acc loop gang reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);

  res = hres = 1;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copy(res)
  {
    #pragma acc loop gang reduction(*:res)
    for (i = 0; i < 12; i++)
      res *= arr[i];
  }

  for (i = 0; i < 12; i++)
    hres *= arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs and vectors, non-private
   reduction variable).  */

void gv_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copy(res)
  {
    #pragma acc loop gang vector reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs and workers, non-private
   reduction variable).  */

void gw_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copy(res)
  {
    #pragma acc loop gang worker reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs, workers and vectors, non-private
   reduction variable).  */

void gwv_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copy(res)
  {
    #pragma acc loop gang worker vector reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);
}


int main()
{
  g_np_1();
  gv_np_1();
  gw_np_1();
  gwv_np_1();

  return 0;
}
