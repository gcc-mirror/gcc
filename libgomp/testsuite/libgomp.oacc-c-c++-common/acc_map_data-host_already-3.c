/* Verify that we refuse 'acc_map_data' when the "host address [...] is already
   mapped".  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdio.h>
#include <openacc.h>

float global_var;
#pragma acc declare create (global_var)

int
main ()
{
  void *d = acc_malloc (sizeof global_var);
  assert (d);
  fprintf (stderr, "CheCKpOInT\n");
  acc_map_data (&global_var, d, sizeof global_var);

  return 0;
}


/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "host address \\\[\[0-9a-fA-FxX\]+, \\\+4\\\] is already mapped" } */
/* { dg-shouldfail "" } */
