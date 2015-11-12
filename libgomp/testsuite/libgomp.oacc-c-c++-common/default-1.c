/* { dg-do run } */

#include  <openacc.h>

int test_parallel ()
{
  int ok = 1;
  int val = 2;
  int ary[32];
  int ondev = 0;

  for (int i = 0; i < 32; i++)
    ary[i] = ~0;

  /* val defaults to firstprivate, ary defaults to copy.  */
#pragma acc parallel num_gangs (32) copy (ok) copy(ondev)
  {
    ondev = acc_on_device (acc_device_not_host);
#pragma acc loop gang(static:1)
    for (unsigned i = 0; i < 32; i++)
      {
	if (val != 2)
	  ok = 0;
	val += i;
	ary[i] = val;
      }
  }

  if (ondev)
    {
      if (!ok)
	return 1;
      if (val != 2)
	return 1;

      for (int i = 0; i < 32; i++)
	if (ary[i] != 2 + i)
	  return 1;
    }
  
  return 0;
}

int test_kernels ()
{
  int val = 2;
  int ary[32];
  int ondev = 0;

  for (int i = 0; i < 32; i++)
    ary[i] = ~0;

  /* val defaults to copy, ary defaults to copy.  */
#pragma acc kernels copy(ondev)
  {
    ondev = acc_on_device (acc_device_not_host);
#pragma acc loop 
    for (unsigned i = 0; i < 32; i++)
      {
	ary[i] = val;
	val++;
      }
  }

  if (ondev)
    {
      if (val != 2 + 32)
	return 1;

      for (int i = 0; i < 32; i++)
	if (ary[i] != 2 + i)
	  return 1;
    }
  
  return 0;
}

int main ()
{
  if (test_parallel ())
    return 1;

  if (test_kernels ())
    return 1;

  return 0;
}
