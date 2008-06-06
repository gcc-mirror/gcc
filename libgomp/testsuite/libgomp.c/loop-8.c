extern void abort (void);

int buf[256];

void __attribute__((noinline))
foo (void)
{
  int i;
  #pragma omp for schedule (auto)
    for (i = 0; i < 256; i++)
      buf[i] += i;
}

int
main (void)
{
  int i;
  #pragma omp parallel for schedule (auto)
    for (i = 0; i < 256; i++)
      buf[i] = i;
  #pragma omp parallel num_threads (4)
    foo ();
  for (i = 0; i < 256; i++)
    if (buf[i] != 2 * i)
      abort ();
  return 0;
}
