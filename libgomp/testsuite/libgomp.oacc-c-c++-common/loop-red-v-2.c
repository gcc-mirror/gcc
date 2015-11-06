/* { dg-do run } */
/* { dg-additional-options "-O2" */
/* <http://news.gmane.org/find-root.php?message_id=%3C563B78B5.5090506%40acm.org%3E>
   { dg-xfail-if "TODO" { *-*-* } } */

#include <stdio.h>

#define N (32*32*32+17)

int main ()
{
  int ix;
  int ondev = 0;
  int q = 0,  h = 0;

#pragma acc parallel vector_length(32) copy(q) copy(ondev)
  {
    int t = q;
    
#pragma acc loop vector reduction (+:t)
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
    q = t;
  }

  for (ix = 0; ix < N; ix++)
    {
      int val = ix;
      if (ondev)
	{
	  int g = 0;
	  int w = 0;
	  int v = ix % 32;

	  val = (g << 16) | (w << 8) | v;
	}
      h += val;
    }

  if (q != h)
    {
      printf ("t=%x expected %x\n", q, h);
      return 1;
    }
  
  return 0;
}
