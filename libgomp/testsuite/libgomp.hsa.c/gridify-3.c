#define THE_LOOP \
  for (i = j + 1; i < n; i += 3) \
    a[i] = i

void __attribute__((noinline, noclone))
foo (int j, int n, int *a)
{
  int i;
#pragma omp target
#pragma omp teams
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i) firstprivate(j)
  THE_LOOP;
}

void __attribute__((noinline, noclone))
bar (int j, int n, int *a)
{
  int i;
  THE_LOOP;
}

int main (int argc, char **argv)
{
  int n = 32;
  int *a = __builtin_malloc (sizeof (int) * n);
  int *ref = __builtin_malloc (sizeof (int) * n);
  int i, j = 4;

  __builtin_memset (a, 0, sizeof (int) * n);
  __builtin_memset (ref, 0, sizeof (int) * n);
  bar (j, n, ref);
  foo (j, n, a);
  for (i = 0; i < n; i ++)
    {
      if (a[i] != ref[i])
	__builtin_abort ();
    }
  return 0;
}
