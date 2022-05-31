#ifdef __cplusplus
extern "C"
#endif
void abort ();

int
main ()
{
  int a[64] = {};
  int r = 0, r2 = 0, i, n = 64;
  #pragma omp parallel
  {
    #pragma omp scope nowait
    #pragma omp scope nowait firstprivate (n)
    #pragma omp for
    for (i = 0; i < 64; i++)
      a[i] += 1;
    #pragma omp scope reduction(+: r) nowait firstprivate (n)
    {
      #pragma omp for nowait
      for (i = 0; i < 64; i++)
	{
	  r += i;
	  if (a[i] != 1)
	    abort ();
	}
      #pragma omp barrier
      if (n != 64)
	abort ();
      else
	n = 128;
    }
    #pragma omp barrier
    if (r != 64 * 63 / 2)
      abort ();
    #pragma omp scope nowait private (i)
    #pragma omp scope reduction(+: r2)
    {
      #pragma omp for nowait
      for (i = 0; i < 64; i++)
	{
	  r2 += 2 * i;
	  a[i] += i;
	}
    }
    if (r2 != 64 * 63)
      abort ();
    #pragma omp for nowait
    for (i = 0; i < 64; i++)
      if (a[i] != i + 1)
	abort ();
  }
  return 0;
}
