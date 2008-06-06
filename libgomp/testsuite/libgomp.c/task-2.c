extern void abort (void);

int
f1 (void)
{
  int a = 6, e = 0;
  int nested (int x)
  {
    return x + a;
  }
  #pragma omp task
  {
    int n = nested (5);
    if (n != 11)
      #pragma omp atomic
	e += 1;
  }
  #pragma omp taskwait
  return e;
}

int
f2 (void)
{
  int a = 6, e = 0;
  int nested (int x)
  {
    return x + a;
  }
  a = nested (4);
  #pragma omp task
  {
    if (a != 10)
      #pragma omp atomic
	e += 1;
  }
  #pragma omp taskwait
  return e;
}

int
main (void)
{
  int e = 0;
  #pragma omp parallel num_threads(4) reduction(+:e)
  {
    e += f1 ();
    e += f2 ();
  }
  if (e)
    abort ();
  return 0;
}
