#include <unistd.h>

int
nvptx_usleep (useconds_t d)
{
  /* This function serves as a replacement for usleep in
     this test case.  It does not even attempt to be functionally
     equivalent  - we just want some sort of delay. */
  int i;
  int N = d * 2000;
  for (i = 0; i < N; i++)
    asm volatile ("" : : : "memory");
  return 0;
}

#pragma omp declare variant (nvptx_usleep) match(construct={target},device={arch(nvptx)})
#pragma omp declare variant (usleep) match(user={condition(1)})
int
tgt_usleep (useconds_t d)
{
  return 0;
}

#pragma omp declare target to (nvptx_usleep, tgt_usleep)
