/* { dg-do run } */

#include  <openacc.h>

int main ()
{
  int ok = 1;
  int val = 2;

#pragma acc data copy(val)
  {
#pragma acc parallel present (val)
    {
      val = 7;
    }

#pragma acc parallel firstprivate (val) copy(ok)
    {
      ok  = val == 7;
      val = 9;
    }

  }

  if (!ok)
    return 1;
  if(val != 7)
    return 1;

  return 0;
}
