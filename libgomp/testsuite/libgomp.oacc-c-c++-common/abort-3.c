/* { dg-do run } */

#include <stdio.h>
#include <stdlib.h>

int
main (void)
{
  fprintf (stderr, "CheCKpOInT\n");
#pragma acc kernels
  {
    abort ();
  }

  return 0;
}

/* { dg-output "CheCKpOInT" } */
/* { dg-shouldfail ""  } */
