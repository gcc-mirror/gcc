/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <openacc.h>

int
main (int argc, char *argv[])
{
  int i;

#pragma acc enter data create (i)
  acc_create (&i, sizeof i);

  return 0;
}

/* { dg-shouldfail "" }
   { dg-output "already mapped to" } */
