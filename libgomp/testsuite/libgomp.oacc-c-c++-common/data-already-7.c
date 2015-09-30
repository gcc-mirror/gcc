/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char *argv[])
{
  int i;

#pragma acc enter data create (i)
  fprintf (stderr, "CheCKpOInT\n");
  acc_create (&i, sizeof i);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "already mapped to" } */
/* { dg-shouldfail "" } */
