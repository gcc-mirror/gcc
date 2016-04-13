/* PR hsa/69568 */

typedef float float2 __attribute__ ((vector_size (8)));
float2 *output;

void __attribute__((noinline, noclone))
foo (int n, float2 *a, int workgroup_size)
{
  int i;
#pragma omp target map(from:a[:n]) firstprivate(n, workgroup_size)
#pragma omp teams thread_limit(workgroup_size)
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i)
    for (i = 0; i < n; i++)
      { float2 v;
	v[0] = i;
	v[1] = 1+i;
	a[i] = v;
      }
}

int main (int argc, char **argv)
{
  int n = 32;
  float2 *a = __builtin_malloc (sizeof (float2) * n);
  int i;

  __builtin_memset (a, 0, sizeof (float2) * n);
  foo (n, a, 32);
  for (i = 0; i < n; i++)
    {
      float2 v = a[i];
      if (__builtin_abs (v[0] - i) > 0.1
	  || __builtin_abs (v[1] - i - 1) > 0.1)
	{
	  __builtin_abort ();
	  return 1;
	}
    }
  return 0;
}

