/* { dg-do run } */

#include <stdlib.h>

int
main (void)
{
  __builtin_printf ("CheCKpOInT\n");
#pragma acc kernels
  {
    abort ();
  }

  return 0;
}

/* { dg-output "CheCKpOInT" } */
/* { dg-shouldfail ""  } */
