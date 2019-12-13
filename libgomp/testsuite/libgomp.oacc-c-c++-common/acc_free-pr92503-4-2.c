/* Verify that we refuse 'acc_free', inside 'host_data', inside 'data'.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 108;

  char *h = (char *) malloc (N);
#pragma acc data create (h[0:N - 44])
  {
#pragma acc host_data use_device (h)
    {
      fprintf (stderr, "CheCKpOInT\n");
      acc_free (h);
    }
  }

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
   { dg-output "refusing to free device memory space at \[0-9a-fA-FxX\]+ that is still mapped at \\\[\[0-9a-fA-FxX\]+,\\\+64\\\]" }
   { dg-shouldfail "" } */
