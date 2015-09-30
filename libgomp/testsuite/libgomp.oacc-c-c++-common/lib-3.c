/* { dg-do run } */

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
/* TODO: currently prints: "libgomp: no device found".  */
/* { dg-output "device \[0-9\]+\\\(\[0-9\]+\\\) is initialized" { xfail *-*-* } } */
/* { dg-shouldfail "" } */
