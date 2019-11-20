/* { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } } */

#include <openacc.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>

int main (int argc, char* argv[])
{
  char *myblock = (char *) malloc (1024);
  int i;
  void *dst;
  for (i = 0; i < 1024; i++)
    myblock[i] = i;
  dst = acc_copyin (myblock, 1024);
  for (i = 0; i < 1024; i += 256)
    {
      void *partdst = acc_pcopyin (&myblock[i], 256);
      assert ((uintptr_t) partdst == (uintptr_t) dst + i);
    }
  for (i = 0; i < 1024; i += 256)
    acc_delete (&myblock[i], 256);
  assert (acc_is_present (myblock, 1024));
  acc_delete (myblock, 1024);
  assert (!acc_is_present (myblock, 1024));
  free (myblock);
  return 0;
}
