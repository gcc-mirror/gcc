// PR middle-end/86539

int a[384];

__attribute__((noipa)) void
foo (int &b, int &c)
{
  #pragma omp taskloop shared (a) collapse(3)
  for (int i = 0; i < 1; i++)
    for (int *p = &b; p < &c; p++)
      for (int j = 0; j < 1; j++)
	if (p < &a[128] || p >= &a[256])
	  __builtin_abort ();
	else
	  p[0]++;
}

int
main ()
{
  #pragma omp parallel
  #pragma omp single
    foo (a[128], a[256]);
  for (int i = 0; i < 384; i++)
    if (a[i] != (i >= 128 && i < 256))
      __builtin_abort ();
  return 0;
}
