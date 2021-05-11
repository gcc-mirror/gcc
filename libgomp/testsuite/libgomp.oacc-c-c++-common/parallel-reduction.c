/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

#include <stdlib.h>
#include <openacc.h>

#define N 10

int
main ()
{
  int s1 = 0, s2 = 0;
  int i;
  int dummy = 0;

#pragma acc data copy (dummy)
  {
#pragma acc parallel num_gangs (N) reduction (+:s1) copy(s1)
    /* { dg-bogus "warning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-1 } */
    {
      s1++;
    }
  }

  if (acc_get_device_type () == acc_device_host)
    {
      if (s1 != 1)
	abort ();
    }
  else
    {
      if (s1 != N)
	abort ();
    }

  s1 = 0;
  s2 = 0;

#pragma acc parallel num_gangs (10) reduction (+:s1, s2) copy(s1, s2)
  /* { dg-bogus "warning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-1 } */
  {
    s1++;
    s2 += N;
  }

  if (acc_get_device_type () == acc_device_host)
    {
      if (s1 != 1)
	abort ();
      if (s2 != N)
	abort ();
    }
  else
    {
      if (s1 != N)
	abort ();
      if (s2 != N*N)
	abort ();
    }

  s1 = 0;

#pragma acc parallel num_gangs (10) reduction (+:s1) copy(s1)
  {
#pragma acc loop gang reduction (+:s1)
    for (i = 0; i < 10; i++)
      s1++;
  }

  if (s1 != N)
    abort ();

  return 0;
}
