/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>

int
main (int argc, char *argv[])
{
  int i;

#pragma acc data create (i)
  {
    fprintf (stderr, "CheCKpOInT\n");
#pragma acc parallel copyin (i)
    ++i;
  }

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
