extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

int *q;

void
bar (int *p, int *r, int s)
{
  #pragma omp task in_reduction (*: p[0], q[0], r[s - 1])
  {
    *p *= 4;
    *q *= 5;
    r[s - 1] *= 6;
  }
}

void
foo (int *p, int *r, int s)
{
  int *p2 = p;
  #pragma omp taskgroup task_reduction (*: p[0], q[0], r[s])
  {
    p = (int *) 0;
    s++;
    bar (p2, r, s);
    r++;
    #pragma omp taskwait
    #pragma omp task in_reduction (*: p2[0], q[0], r[s - 2])
    {
      *p2 *= 2;
      *q *= 3;
      r[s - 2] *= 7;
    }
    s++;
    p2 = (int *) 0;
    q = (int *) 0;
    r = (int *) 0;
  }
}

int
main ()
{
  int a = 1, b = 1, c[2] = { 1, 0 };
  q = &b;
  #pragma omp parallel num_threads (2)
  #pragma omp master
  foo (&a, &c[0], 0);
  if (a != 8 || b != 15 || c[0] != 42 || c[1] != 0)
    abort ();
  return 0;
}
