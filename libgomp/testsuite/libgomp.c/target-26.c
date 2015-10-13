extern void abort (void);
#pragma omp declare target
int a[4] = { 2, 3, 4, 5 }, *b;
#pragma omp end declare target

int
main ()
{
  int err;
  int c[3] = { 6, 7, 8 };
  b = c;
  #pragma omp target map(to: a[0:2], b[0:2]) map(from: err)
  err = a[0] != 2 || a[1] != 3 || a[2] != 4 || a[3] != 5 || b[0] != 6 || b[1] != 7;
  if (err)
    abort ();
  a[1] = 9;
  a[2] = 10;
  #pragma omp target map(always,to:a[1:2]) map(from: err)
  err = a[0] != 2 || a[1] != 9 || a[2] != 10 || a[3] != 5;
  if (err)
    abort ();
  #pragma omp parallel firstprivate(a, b, c, err) num_threads (2)
  #pragma omp single
  {
    b = c + 1;
    a[0] = 11;
    a[2] = 13;
    c[1] = 14;
    int d = 0;
    #pragma omp target map(to: a[0:3], b[d:2]) map (from: err)
    err = a[0] != 11 || a[1] != 9 || a[2] != 13 || b[0] != 14 || b[1] != 8;
    if (err)
      abort ();
  }
  return 0;
}
