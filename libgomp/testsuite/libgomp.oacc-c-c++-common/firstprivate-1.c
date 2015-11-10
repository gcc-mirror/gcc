/* { dg-do run } */

#include  <openacc.h>

int main ()
{
  int ok = 1;
  int val = 2;
  int ary[32];
  int ondev = 0;

  for (int i = 0; i < 32; i++)
    ary[i] = ~0;
  
#pragma acc parallel num_gangs (32) copy (ok) firstprivate (val) copy(ary, ondev)
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
