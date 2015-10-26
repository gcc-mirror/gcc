/* Expect error message when shutting down a device that has never been
   initialized.  */
/* { dg-do run } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  fprintf (stderr, "CheCKpOInT\n");
  acc_shutdown (acc_device_default);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "no device initialized" } */
/* { dg-shouldfail "" } */
