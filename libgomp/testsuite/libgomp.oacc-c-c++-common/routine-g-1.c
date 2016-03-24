/* This code uses nvptx inline assembly guarded with acc_on_device, which is
   not optimized away at -O0, and then confuses the target assembler.
   { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

#include <stdio.h>

#define N (32*32*32+17)

#pragma acc routine gang
void __attribute__ ((noinline)) gang (int ary[N])
{
#pragma acc loop gang
    for (unsigned ix = 0; ix < N; ix++)
      {
	if (__builtin_acc_on_device (5))
	  {
	    int g = 0, w = 0, v = 0;

	    __asm__ volatile ("mov.u32 %0,%%ctaid.x;" : "=r" (g));
	    __asm__ volatile ("mov.u32 %0,%%tid.y;" : "=r" (w));
	    __asm__ volatile ("mov.u32 %0,%%tid.x;" : "=r" (v));
	    ary[ix] = (g << 16) | (w << 8) | v;
	  }
	else
	  ary[ix] = ix;
      }
}

int main ()
{
  int ary[N];
  int ix;
  int exit = 0;
  int ondev = 0;

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;
  
#pragma acc parallel num_gangs(32) vector_length(32) copy(ary) copy(ondev)
  {
    ondev = __builtin_acc_on_device (5);
    gang (ary);
  }

  for (ix = 0; ix < N; ix++)
    {
      int expected = ix;
      if(ondev)
	{
	  int g = ix / ((N + 31) / 32);
	  int w = 0;
	  int v = 0;

	  expected = (g << 16) | (w << 8) | v;
	}
      
      if (ary[ix] != expected)
	{
	  exit = 1;
	  printf ("ary[%d]=%x expected %x\n", ix, ary[ix], expected);
	}
    }
  
  return exit;
}
