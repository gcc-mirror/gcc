// PR libgomp/69555
// { dg-do run }

__attribute__((noinline, noclone)) void
f1 (int y)
{
  int a[y - 2];
  int (&c)[y - 2] = a;
  for (int i = 0; i < y - 2; i++)
    c[i] = i + 4;

  #pragma omp target firstprivate (c)
  {
    for (int i = 0; i < y - 2; i++)
      {
	if (c[i] != i + 4)
	  __builtin_abort ();
	c[i] = i + 9;
      }
    asm volatile ("" : : "r" (&c[0]) : "memory");
    for (int i = 0; i < y - 2; i++)
      if (c[i] != i + 9)
	__builtin_abort ();
  }
  for (int i = 0; i < y - 2; i++)
    if (c[i] != i + 4)
      __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f2 (int y)
{
  int a[y - 2];
  int (&c)[y - 2] = a;
  for (int i = 0; i < y - 2; i++)
    c[i] = i + 4;

  #pragma omp target private (c)
  {
    for (int i = 0; i < y - 2; i++)
      c[i] = i + 9;
    asm volatile ("" : : "r" (&c[0]) : "memory");
    for (int i = 0; i < y - 2; i++)
      if (c[i] != i + 9)
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
  f2 (6);
  return 0;
}
