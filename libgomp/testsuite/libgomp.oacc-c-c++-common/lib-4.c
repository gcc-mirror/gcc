/* { dg-do run } */

#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_init ((acc_device_t) 99);

  return 0;
}

/* { dg-shouldfail "libgomp: device 99 is out of range" } */
