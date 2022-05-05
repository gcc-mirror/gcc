int
main (void)
{
  unsigned val = 0;

#pragma omp target map(tofrom: val)
#pragma omp simd
  for (int i = 0 ; i < 1 ; i++)
    {
#pragma omp atomic update
      val = val + 1;
    }

  if (val != 1)
    __builtin_abort ();

  return 0;
}
