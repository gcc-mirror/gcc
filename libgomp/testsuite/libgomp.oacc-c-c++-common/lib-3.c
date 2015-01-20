/* { dg-do run } */

#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_init (acc_device_host);

  acc_shutdown (acc_device_not_host);

  return 0;
}

/* { dg-shouldfail "libgomp: device 4(4) is initialized" } */
