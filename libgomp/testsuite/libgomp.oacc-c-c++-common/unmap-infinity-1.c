/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <openacc.h>

int foo[16];
#pragma acc declare device_resident(foo)

int
main (int argc, char *argv[])
{
  acc_init (acc_device_default);
  acc_unmap_data ((void *) foo);
/* { dg-output "libgomp: refusing to unmap block \\\[\[0-9a-fA-FxX\]+,\\\+64\\\] that has not been mapped by 'acc_map_data'" } */
  return 0;
}

/* { dg-shouldfail "" } */
