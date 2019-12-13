/* Verify that we refuse 'acc_map_data' when the "device address [...] is
   already mapped".  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdio.h>
#include <openacc.h>

double global_var;
#pragma acc declare create (global_var)

int
main ()
{
  double var;
  void *d = acc_deviceptr (&global_var);
  assert (d);
  /* Try to arrange a setting such that a later 'acc_unmap_data' would find the
     device memory object still referenced elsewhere.  This is not possible,
     given the semantics of 'acc_map_data'.  */
  fprintf (stderr, "CheCKpOInT\n");
  acc_map_data (&var, d, sizeof var);

  return 0;
}


/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "device address \\\[\[0-9a-fA-FxX\]+, \\\+8\\\] is already mapped" { xfail *-*-* } } TODO PR92888 */
/* { dg-shouldfail "TODO PR92888" { this-really-should-fail } } */
