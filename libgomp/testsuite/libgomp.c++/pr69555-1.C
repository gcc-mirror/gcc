// PR libgomp/69555
// { dg-do run }

#include <omp.h>

__attribute__((noinline, noclone)) void
f1 (int y)
{
  int a[y - 2];
  int (&c)[y - 2] = a;
  c[0] = 111;
  int e = 0;

  #pragma omp parallel private (c) num_threads (4) reduction (+:e)
  {
    int v = omp_get_thread_num ();
    for (int i = 0; i < y - 2; i++)
      c[i] = i + v;
    #pragma omp barrier
    for (int i = 0; i < y - 2; i++)
      if (c[i] != i + v)
	e++;
  }
  if (c[0] != 111 || e)
    __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f2 (int y)
{
  int a[y - 2];
  int (&c)[y - 2] = a;
  c[0] = 111;

  #pragma omp task private (c)
  {
    int v = omp_get_thread_num ();
    for (int i = 0; i < y - 2; i++)
      c[i] = i + v;
    asm volatile ("" : : "r" (&c[0]) : "memory");
    for (int i = 0; i < y - 2; i++)
      if (c[i] != i + v)
	__builtin_abort ();
  }
  if (c[0] != 111)
    __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f3 (int y)
{
  int a[y - 2];
  int (&c)[y - 2] = a;
  for (int i = 0; i < y - 2; i++)
    c[i] = i + 4;

  #pragma omp parallel firstprivate (c) num_threads (4)
  {
    int v = omp_get_thread_num ();
    for (int i = 0; i < y - 2; i++)
      {
	if (c[i] != i + 4)
	  __builtin_abort ();
	c[i] = i + v;
      }
    #pragma omp barrier
    for (int i = 0; i < y - 2; i++)
      if (c[i] != i + v)
	__builtin_abort ();
  }
  for (int i = 0; i < y - 2; i++)
    if (c[i] != i + 4)
      __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f4 (int y)
{
  int a[y - 2];
  int (&c)[y - 2] = a;
  for (int i = 0; i < y - 2; i++)
    c[i] = i + 4;

  #pragma omp task firstprivate (c)
  {
    int v = omp_get_thread_num ();
    for (int i = 0; i < y - 2; i++)
      {
	if (c[i] != i + 4)
	  __builtin_abort ();
	c[i] = i + v;
      }
    asm volatile ("" : : "r" (&c[0]) : "memory");
    for (int i = 0; i < y - 2; i++)
      if (c[i] != i + v)
	__builtin_abort ();
  }
  for (int i = 0; i < y - 2; i++)
    if (c[i] != i + 4)
      __builtin_abort ();
}

int
main ()
{
  f1 (6);
  f3 (6);
  #pragma omp parallel num_threads (4)
  {
    f2 (6);
    f4 (6);
  }
  return 0;
}
