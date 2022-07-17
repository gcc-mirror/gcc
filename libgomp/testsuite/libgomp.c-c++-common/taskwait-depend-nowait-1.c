#ifdef __cplusplus
extern "C"
#endif
void abort (void);

int
main ()
{
  int a[64], b = 1;
  #pragma omp parallel num_threads (4)
  #pragma omp single
  {
    int i;
    #pragma omp taskwait depend(in: a) nowait
    #pragma omp taskwait depend(in: a) nowait
    #pragma omp taskwait
    #pragma omp taskgroup
    {
      #pragma omp taskwait depend(in: a) nowait
      #pragma omp taskwait depend(in: a) nowait
    }
    for (i = 0; i < 64; ++i)
      #pragma omp task depend(in: a) shared(a)
      a[i] = i;
    #pragma omp taskwait depend(inout: a) nowait
    for (i = 0; i < 64; ++i)
      #pragma omp task depend(inoutset: a) shared(a)
      if (a[i] != i)
	abort ();
      else
	a[i] = 2 * i + 1;
    #pragma omp taskwait nowait depend(out: a) depend(in: b)
    #pragma omp taskwait depend(inout: b)
    for (i = 0; i < 64; ++i)
      if (a[i] != 2 * i + 1)
	abort ();
  }
  return 0;
}
