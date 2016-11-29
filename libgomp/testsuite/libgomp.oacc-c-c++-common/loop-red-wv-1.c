/* { dg-do run } */
/* { dg-additional-options "-O2" } */

#include <stdio.h>

#define N (32*32*32+17)
int main ()
{
  int ix;
  int ondev = 0;
  int t = 0, h = 0;
  
#pragma acc parallel num_workers(32) vector_length(32) copy(ondev)
  {
#pragma acc loop worker vector reduction (+:t)
    for (unsigned ix = 0; ix < N; ix++)
      {
	int val = ix;
	
	if (__builtin_acc_on_device (5))
	  {
	    int g = 0, w = 0, v = 0;

	    __asm__ volatile ("mov.u32 %0,%%ctaid.x;" : "=r" (g));
	    __asm__ volatile ("mov.u32 %0,%%tid.y;" : "=r" (w));
	    __asm__ volatile ("mov.u32 %0,%%tid.x;" : "=r" (v));
	    val = (g << 16) | (w << 8) | v;
	    ondev = 1;
	  }
	t += val;
      }
  }

  for (ix = 0; ix < N; ix++)
    {
      int val = ix;
      if(ondev)
	{
	  int g = 0;
	  int w = (ix / 32) % 32;
	  int v = ix % 32;

	  val = (g << 16) | (w << 8) | v;
	}
      h += val;
    }
  if (t != h)
    {
      printf ("t=%x expected %x\n", t, h);
      return 1;
    }
  
  return 0;
}
