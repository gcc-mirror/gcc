extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

void
bar (long long int *p)
{
  p[0] *= 2;
  #pragma omp task in_reduction (*: p[0])
  p[0] *= 3;
}

void
foo (long long int *p, long long int *q)
{
  #pragma omp taskgroup task_reduction (*: p[0])
  {
    #pragma omp task in_reduction (*: p[0])
    bar (p);
    #pragma omp task in_reduction (*: p[0])
    bar (p);
    bar (p);
    #pragma omp taskgroup task_reduction (*: q[0])
    {
      #pragma omp task in_reduction (*: q[0])
      bar (q);
      #pragma omp task in_reduction (*: q[0])
      bar (q);
      #pragma omp task in_reduction (*: q[0])
      bar (q);
      bar (q);
      #pragma omp task in_reduction (*: p[0])
      {
	#pragma omp taskgroup task_reduction (*: p[0])
	{
	  #pragma omp task in_reduction (*: p[0])
	  bar (p);
	  p[0] *= 2;
	  #pragma omp task in_reduction (*: p[0])
	  bar (p);
	}
      }
    }
  }
}

int
main ()
{
  long long int p = 1LL, q = 1LL;
  foo (&p, &q);
  if (p != 6LL * 6LL * 6LL * 6LL * 6LL * 2LL || q != 6LL * 6LL * 6LL * 6LL)
    abort ();
  p = 1LL;
  q = 1LL;
  #pragma omp taskgroup
  foo (&p, &q);
  if (p != 6LL * 6LL * 6LL * 6LL * 6LL * 2LL || q != 6LL * 6LL * 6LL * 6LL)
    abort ();
  p = 1LL;
  q = 1LL;
  #pragma omp parallel
  #pragma omp single
  foo (&p, &q);
  if (p != 6LL * 6LL * 6LL * 6LL * 6LL * 2LL || q != 6LL * 6LL * 6LL * 6LL)
    abort ();
  return 0;
}
