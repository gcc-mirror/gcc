/* { dg-do run } */

#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_init (acc_device_host);

  acc_shutdown (acc_device_not_host);

  return 0;
}

/* { dg-output "device \[0-9\]+\\\(\[0-9\]+\\\) is initialized" } */
/* { dg-shouldfail "" } */
