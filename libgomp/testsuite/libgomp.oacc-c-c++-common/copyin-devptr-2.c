/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>

int main (int argc, char* argv[])
{
  char *block1 = (char *) malloc (1024);
  char *block2 = (char *) malloc (1024);
  char *block3 = (char *) malloc (1024);
  int i;
  void *dst;
  for (i = 0; i < 1024; i++)
    block1[i] = block2[i] = block3[i] = i;
  #pragma acc data copyin(block1[0:1024]) copyin(block2[0:1024]) \
		   copyin(block3[0:1024])
  {
    dst = acc_deviceptr (block2);
    for (i = 0; i < 1024; i += 256)
      {
	void *partdst = acc_pcopyin (&block2[i], 256);
	assert ((uintptr_t) partdst == (uintptr_t) dst + i);
      }
  }
  assert (acc_is_present (block2, 1024));
  for (i = 0; i < 1024; i += 256)
    acc_delete (&block2[i], 256);
  assert (!acc_is_present (block2, 1024));
  free (block1);
  free (block2);
  free (block3);
  return 0;
}
