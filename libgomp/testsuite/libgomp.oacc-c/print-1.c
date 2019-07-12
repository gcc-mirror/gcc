/* Ensure that printf on the offload device works.  */

/* { dg-do run } */
/* { dg-output "The answer is 42(\n|\r\n|\r)+" } */

#include <stdio.h>

int var = 42;

int
main ()
{
#pragma acc parallel
    {
      printf ("The answer is %d\n", var);
    }
}
