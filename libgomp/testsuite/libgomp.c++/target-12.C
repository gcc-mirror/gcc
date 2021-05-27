extern "C" void abort (void);
struct S { int s; int *u; int v[5]; };
volatile int z;

template <typename T>
void
foo ()
{
  int u[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, err = 0;
  T s = { 9, u + 3, { 10, 11, 12, 13, 14 } };
  int *v = u + 4;
  #pragma omp target enter data map (to: s.s, s.template u[0:5]) map (alloc: s.template v[1:3])
  s.s++;
  u[3]++;
  s.v[1]++;
  #pragma omp target update to (s.template s) to (s.u[0:2], s.v[1:3])
  #pragma omp target map (alloc: s.s, s.v[1:3]) map (from: err)
  {
    err = 0;
    if (s.s != 10 || s.v[1] != 12 || s.v[2] != 12 || s.v[3] != 13)
      err = 1;
    if (v[-1] != 4 || v[0] != 4 || v[1] != 5 || v[2] != 6 || v[3] != 7)
      err = 1;
    s.s++;
    s.v[2] += 2;
    v[-1] = 5;
    v[3] = 9;
  }
  if (err)
    abort ();
  #pragma omp target map (alloc: s.u[0:5])
  {
    err = 0;
    if (s.u[0] != 5 || s.u[1] != 4 || s.u[2] != 5 || s.u[3] != 6 || s.u[4] != 9)
      err = 1;
    s.u[1] = 12;
  }
  #pragma omp target update from (s.s, s.u[0:5]) from (s.v[1:3])
  if (err || s.s != 11 || u[0] != 0 || u[1] != 1 || u[2] != 2 || u[3] != 5
      || u[4] != 12 || u[5] != 5 || u[6] != 6 || u[7] != 9 || u[8] != 8
      || u[9] != 9 || s.v[0] != 10 || s.v[1] != 12 || s.v[2] != 14
      || s.v[3] != 13 || s.v[4] != 14)
    abort ();
  #pragma omp target exit data map (release: s.s)
  #pragma omp target exit data map (release: s.u[0:5])
  #pragma omp target exit data map (delete: s.v[1:3])
  #pragma omp target exit data map (release: s.s)
}

int
main ()
{
  int u[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, err = 0;
  S s = { 9, u + 3, { 10, 11, 12, 13, 14 } };
  int *v = u + 4;
  #pragma omp target enter data map (to: s.s, s.u, s.u[0:5]) map (alloc: s.v[1:3])
  s.s++;
  u[3]++;
  s.v[1]++;
  #pragma omp target update to (s.s) to (s.u[0:2], s.v[1:3])
  #pragma omp target map (alloc: s.s, s.v[1:3]) map (from: err)
  {
    err = 0;
    if (s.s != 10 || s.v[1] != 12 || s.v[2] != 12 || s.v[3] != 13)
      err = 1;
    if (v[-1] != 4 || v[0] != 4 || v[1] != 5 || v[2] != 6 || v[3] != 7)
      err = 1;
    s.s++;
    s.v[2] += 2;
    v[-1] = 5;
    v[3] = 9;
  }
  if (err)
    abort ();
  #pragma omp target map (alloc: s.u[0:5])
  {
    err = 0;
    if (s.u[0] != 5 || s.u[1] != 4 || s.u[2] != 5 || s.u[3] != 6 || s.u[4] != 9)
      err = 1;
    s.u[1] = 12;
  }
  #pragma omp target update from (s.s, s.u[0:5]) from (s.v[1:3])
  if (err || s.s != 11 || u[0] != 0 || u[1] != 1 || u[2] != 2 || u[3] != 5
      || u[4] != 12 || u[5] != 5 || u[6] != 6 || u[7] != 9 || u[8] != 8
      || u[9] != 9 || s.v[0] != 10 || s.v[1] != 12 || s.v[2] != 14
      || s.v[3] != 13 || s.v[4] != 14)
    abort ();
  #pragma omp target exit data map (release: s.s)
  #pragma omp target exit data map (release: s.u[0:5])
  #pragma omp target exit data map (always, delete: s.v[1:3])
  #pragma omp target exit data map (release: s.s)
  #pragma omp target exit data map (always delete : s.v[1:3])
}
