void __attribute__((noinline, noclone))
foo (int n, int *a, int workgroup_size)
{
  int i;
#pragma omp target
#pragma omp teams thread_limit(workgroup_size)
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i)
    for (i = 0; i < n; i++)
      a[i]++;
}

int main (int argc, char **argv)
{
  int n = 32;
  int *a = __builtin_malloc (sizeof (int) * n);
  int i;

  __builtin_memset (a, 0, sizeof (int) * n);
  foo (n, a, 32);
  for (i = 0; i < n; i ++)
    {
      if (a[i] != 1)
	__builtin_abort ();
    }
  return 0;
}
