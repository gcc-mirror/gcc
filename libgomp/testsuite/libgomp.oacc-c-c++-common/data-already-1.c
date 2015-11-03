/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char *argv[])
{
  int i;

  acc_copyin (&i, sizeof i);

  fprintf (stderr, "CheCKpOInT\n");
#pragma acc data copy (i)
  ++i;

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "Trying to map into device \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) object when \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) is already mapped" } */
/* { dg-shouldfail "" } */
