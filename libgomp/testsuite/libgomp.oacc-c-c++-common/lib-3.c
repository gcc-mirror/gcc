/* Expect an error message when shutting down a device different from the one
   that has been initialized.  */
/* { dg-do run { target { ! openacc_host_selected } } } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_init (acc_device_host);

  fprintf (stderr, "CheCKpOInT\n");
  acc_shutdown (acc_device_not_host);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "no device initialized" } */
/* { dg-shouldfail "" } */
