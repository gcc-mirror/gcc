/* { dg-do run } */

#include <stdio.h>

#pragma acc routine vector
void __attribute__((noinline, noclone))
Vector (int *ptr, int n, const int &inc)
{
#pragma acc loop vector
  for (unsigned ix = 0; ix < n; ix++)
    ptr[ix] += inc;
}

#pragma acc routine worker
void __attribute__((noinline, noclone))
Worker (int *ptr, int m, int n, const int &inc)
{
#pragma acc loop worker
  for (unsigned ix = 0; ix < m; ix++)
    Vector(ptr + ix * n, n, inc);
}

int
main (void)
{
  const int n = 32, m = 32;

  int ary[m][n];
  unsigned ix,  iy;

  for (ix = m; ix--;)
    for (iy = n; iy--;)
      ary[ix][iy] = (ix << 8) + iy;

#pragma acc parallel copy(ary)
  {
    Worker (&ary[0][0], m, n, 1 << 16);
  }

  int err = 0;

  for (ix = m; ix--;)
    for (iy = n; iy--;)
      if (ary[ix][iy] != ((1 << 16) + (ix << 8) + iy))
	{
	  printf ("ary[%u][%u] = %x expected %x\n",
		  ix, iy, ary[ix][iy], ((1 << 16) + (ix << 8) + iy));
	  err++;
	}

  if (err)
    {
      printf ("%d failed\n", err);
      return 1;
    }

#pragma acc parallel copy(ary)
  {
    Vector (&ary[0][0], m * n, (1 << 24) - (1 << 16));
  }

  for (ix = m; ix--;)
    for (iy = n; iy--;)
      if (ary[ix][iy] != ((1 << 24) + (ix << 8) + iy))
	{
	  printf ("ary[%u][%u] = %x expected %x\n",
		  ix, iy, ary[ix][iy], ((1 << 24) + (ix << 8) + iy));
	  err++;
	}

  if (err)
    {
      printf ("%d failed\n", err);
      return 1;
    }

  return 0;
}
