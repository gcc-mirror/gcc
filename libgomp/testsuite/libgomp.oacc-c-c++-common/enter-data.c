/* This test verifies that the present data clauses to acc enter data
   don't cause duplicate mapping failures at runtime.  */

/* { dg-do run } */

#include <stdlib.h>

int
main (void)
{
  int a;

#pragma acc enter data copyin (a)
#pragma acc enter data pcopyin (a)
#pragma acc enter data pcreate (a)
#pragma acc exit data delete (a)

#pragma acc enter data create (a)
#pragma acc enter data pcreate (a)
#pragma acc exit data delete (a)

  return 0;
}
