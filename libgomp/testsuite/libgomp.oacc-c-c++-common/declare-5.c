/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <stdio.h>

int
main (int argc, char **argv)
{
  int a[8] __attribute__((unused));

  fprintf (stderr, "CheCKpOInT\n");
#pragma acc declare present (a)
}

/* { dg-output "CheCKpOInT" } */
/* { dg-shouldfail "" } */
