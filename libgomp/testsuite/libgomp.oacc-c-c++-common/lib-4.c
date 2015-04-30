/* { dg-do run } */

#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_init ((acc_device_t) 99);

  return 0;
}

/* { dg-output "device \[0-9\]+ out of range" } */
/* { dg-shouldfail "" } */
