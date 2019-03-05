extern void abort (void);

int
foo (void)
{
  int i = -1, j = -1, k;
  void nested (void) { i++; j++; }
  nested ();
  #pragma omp taskgroup task_reduction (+: i)
  {
    #pragma omp task in_reduction (+: i)
    i++;
    #pragma omp task in_reduction (+: i)
    i += 6;
  }
  #pragma omp taskloop reduction (+: j)
  for (k = 0; k < 2; k++)
    {
      j += 5;
      #pragma omp task in_reduction (+: j)
      j += 31;
    }
  return i + j;
}

int
bar (void)
{
  int i = 0, j = 0;
  void nested (void)
  {
    int k;
    #pragma omp taskgroup task_reduction (+: i)
    {
      #pragma omp task in_reduction (+: i)
      i++;
      #pragma omp task in_reduction (+: i)
      i += 7;
    }
    #pragma omp taskloop reduction (+: j)
    for (k = 0; k < 2; k++)
      {
	j += 21;
	#pragma omp task in_reduction (+: j)
	j += 8;
      }
  }
  nested ();
  return i + j;
}

int
main ()
{
  if (foo () != (1 + 6 + (5 + 31) * 2))
    abort ();
  if (bar () != (1 + 7 + (21 + 8) * 2))
    abort ();
  return 0;
}
