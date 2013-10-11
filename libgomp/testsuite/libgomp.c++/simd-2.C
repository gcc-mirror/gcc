// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

extern "C" void abort ();
__UINTPTR_TYPE__ arr[1027];

__attribute__((noinline, noclone)) void
foo ()
{
  int i, v;
  #pragma omp simd private (v) safelen(16)
  for (i = 0; i < 1027; i++)
    arr[i] = (__UINTPTR_TYPE__) &v;
}

int
main ()
{
  int i, j, cnt = 0;
  __UINTPTR_TYPE__ arr2[16];
  foo ();
  for (i = 0; i < 1027; i++)
    {
      for (j = 0; j < cnt; j++)
	if (arr[i] == arr2[j])
	  break;
      if (j != cnt)
	continue;
      if (cnt == 16)
	abort ();
      arr2[cnt++] = arr[i];
    }
  return 0;
}
