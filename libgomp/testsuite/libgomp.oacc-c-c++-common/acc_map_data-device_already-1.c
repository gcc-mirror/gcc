/* Verify that we refuse 'acc_map_data' when the "device address [...] is
   already mapped".  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 131;

  char *h1 = (char *) malloc (N);
  assert (h1);
  void *d = acc_malloc (N);
  assert (d);
  acc_map_data (h1, d, N);

  char *h2 = (char *) malloc (N);
  assert (h2);
  /* Try to arrange a setting such that a later 'acc_unmap_data' would find the
     device memory object still referenced elsewhere.  This is not possible,
     given the semantics of 'acc_map_data'.  */
  fprintf (stderr, "CheCKpOInT\n");
  acc_map_data (h2, d, N);

  return 0;
}


/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "device address \\\[\[0-9a-fA-FxX\]+, \\\+131\\\] is already mapped" } */
/* { dg-shouldfail "" } */
