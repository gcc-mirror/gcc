void __attribute__((noinline, noclone))
foo (int j, int n, int *a)
{
  int i;
#pragma omp target
#pragma omp teams
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i) firstprivate(j)
    for (i = j + 1; i < n; i++)
      a[i] = i;
}

int main (int argc, char **argv)
{
  int n = 32;
  int *a = __builtin_malloc (sizeof (int) * n);
  int i, j = 4;

  __builtin_memset (a, 0, sizeof (int) * n);
  foo (j, n, a);
  for (i = j + 1; i < n; i ++)
    {
      if (a[i] != i)
	__builtin_abort ();
    }
  return 0;
}
