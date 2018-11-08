typedef __SIZE_TYPE__ size_t;
extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

int *q;

void
bar (int *p, int *r, int *t, int s, size_t u)
{
  #pragma omp task in_reduction (*: p[0], q[0], r[s - 1], t[0:u + 1])
  {
    *p *= 4;
    *q *= 5;
    r[s - 1] *= 6;
    t[0] *= 8;
    t[1] *= 9;
  }
}

void
foo (int *p, int *r, int *t, int s, size_t u)
{
  int *p2 = p;
  #pragma omp taskgroup task_reduction (*: p[0], q[0], r[s], t[0:u + 1])
  {
    p = (int *) 0;
    s++;
    bar (p2, r, t, s, u);
    r++;
    #pragma omp taskwait
    #pragma omp task in_reduction (*: p2[0], q[0], r[s - 2], t[0:u + 1])
    {
      *p2 *= 2;
      *q *= 3;
      r[s - 2] *= 7;
      t[0] *= 10;
      t[1] *= 11;
    }
    u = (~(size_t) 0) / 4;
    s++;
    p2 = (int *) 0;
    q = (int *) 0;
    r = (int *) 0;
    t = (int *) 0;
  }
}

int
main ()
{
  int a = 1, b = 1, c[2] = { 1, 0 }, d[3] = { 1, 1, -1 };
  volatile int zero;
  zero = 0;
  q = &b;
  #pragma omp parallel num_threads (2)
  #pragma omp master
  foo (&a, &c[0], &d[0], zero, zero + 1);
  if (a != 8 || b != 15 || c[0] != 42 || c[1] != 0
      || d[0] != 80 || d[1] != 99 || d[2] != -1)
    abort ();
  return 0;
}
