#include <omp.h>
#include <stdlib.h>

__attribute__((noinline))
void
foo (int x, int y, int z, int *a, int *b)
{
  if (x == 0)
    {
      int i, j;
      for (i = 0; i < 64; i++)
	#pragma omp parallel for shared (a, b)
	for (j = 0; j < 32; j++)
	  foo (3, i, j, a, b);
    }
  else if (x == 1)
    {
      int i, j;
      #pragma omp distribute dist_schedule (static, 1)
      for (i = 0; i < 64; i++)
	#pragma omp parallel for shared (a, b)
	for (j = 0; j < 32; j++)
	  foo (3, i, j, a, b);
    }
  else if (x == 2)
    {
      int j;
      #pragma omp parallel for shared (a, b)
      for (j = 0; j < 32; j++)
	foo (3, y, j, a, b);
    }
  else
    {
      #pragma omp atomic
      b[y] += z;
      #pragma omp atomic
      *a += 1;
    }
}

__attribute__((noinline))
int
bar (int x, int y, int z)
{
  int a, b[64], i;
  a = 8;
  for (i = 0; i < 64; i++)
    b[i] = i;
  foo (x, y, z, &a, b);
  if (x == 0)
    {
      if (a != 8 + 64 * 32)
	return 1;
      for (i = 0; i < 64; i++)
	if (b[i] != i + 31 * 32 / 2)
	  return 1;
    }
  else if (x == 1)
    {
      int c = omp_get_num_teams ();
      int d = omp_get_team_num ();
      int e = d;
      int f = 0;
      for (i = 0; i < 64; i++)
	if (i == e)
	  {
	    if (b[i] != i + 31 * 32 / 2)
	      return 1;
	    f++;
	    e = e + c;
	  }
	else if (b[i] != i)
	  return 1;
      if (a < 8 || a > 8 + f * 32)
	return 1;
    }
  else if (x == 2)
    {
      if (a != 8 + 32)
	return 1;
      for (i = 0; i < 64; i++)
	if (b[i] != i + (i == y ? 31 * 32 / 2 : 0))
	  return 1;
    }
  else if (x == 3)
    {
      if (a != 8 + 1)
	return 1;
      for (i = 0; i < 64; i++)
	if (b[i] != i + (i == y ? z : 0))
	  return 1;
    }
  return 0;
}

int
main ()
{
  int i, j, err = 0;
  #pragma omp teams reduction(+:err)
  err += bar (0, 0, 0);
  if (err)
    abort ();
  #pragma omp teams reduction(+:err)
  err += bar (1, 0, 0);
  if (err)
    abort ();
  #pragma omp teams reduction(+:err)
  #pragma omp distribute
  for (i = 0; i < 64; i++)
    err += bar (2, i, 0);
  if (err)
    abort ();
  #pragma omp teams reduction(+:err)
  #pragma omp distribute
  for (i = 0; i < 64; i++)
  #pragma omp parallel for reduction(+:err)
    for (j = 0; j < 32; j++)
      err += bar (3, i, j);
  if (err)
    abort ();
  return 0;
}
