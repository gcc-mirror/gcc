/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <openacc.h>

int
main (int argc, char *argv[])
{
  int i;

  acc_copyin (&i, sizeof i);

#pragma acc data copy (i)
  ++i;

  return 0;
}

/* { dg-shouldfail "" }
   { dg-output "Trying to map into device .* object when .* is already mapped" } */
