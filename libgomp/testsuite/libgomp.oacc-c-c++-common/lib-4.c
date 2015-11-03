/* { dg-do run } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  fprintf (stderr, "CheCKpOInT\n");
  acc_init ((acc_device_t) 99);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "unknown device type \[0-9\]+" } */
/* { dg-shouldfail "" } */
