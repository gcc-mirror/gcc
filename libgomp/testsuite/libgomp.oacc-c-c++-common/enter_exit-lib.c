/* Verify enter/exit data interoperability between pragmas and
   acc library calls.  */

/* { dg-do run } */

#include <stdlib.h>
#include <assert.h>
#include <openacc.h>

int
main ()
{
  int *p = (int *)malloc (sizeof (int));

  /* Test 1: pragma input, library output.  */
  
#pragma acc enter data copyin (p[0:1])

#pragma acc parallel present (p[0:1]) num_gangs (1)
  {
    p[0] = 1;
  }

  acc_copyout (p, sizeof (int));

  assert (p[0] == 1);
  
  /* Test 2: library input, pragma output.  */

  acc_copyin (p, sizeof (int));

#pragma acc parallel present (p[0:1]) num_gangs (1)
  {
    p[0] = 2;
  }

#pragma acc exit data copyout (p[0:1])
  
  assert (p[0] == 2);

  /* Test 3: library input, library output.  */

  acc_copyin (p, sizeof (int));

#pragma acc parallel present (p[0:1]) num_gangs (1)
  {
    p[0] = 3;
  }

  acc_copyout (p, sizeof (int));
  
  assert (p[0] == 3);

  /* Test 4: pragma input, pragma output.  */

#pragma acc enter data copyin (p[0:1])
  
#pragma acc parallel present (p[0:1]) num_gangs (1)
  {
    p[0] = 3;
  }

#pragma acc exit data copyout (p[0:1])
  
  assert (p[0] == 3);
  
  free (p);

  return 0;
}
