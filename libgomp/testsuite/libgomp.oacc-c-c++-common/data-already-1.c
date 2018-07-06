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
