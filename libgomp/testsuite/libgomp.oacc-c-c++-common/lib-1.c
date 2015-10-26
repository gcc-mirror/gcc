/* { dg-do run } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_init (acc_device_default);
  fprintf (stderr, "CheCKpOInT\n");
  acc_init (acc_device_default);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "device already active" } */
/* { dg-shouldfail "" } */
