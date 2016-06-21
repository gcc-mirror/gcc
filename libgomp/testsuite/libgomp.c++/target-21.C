extern "C" void abort ();
struct T { char t[270]; };
struct S { int (&x)[10]; int *&y; T t; int &z; S (); ~S (); };

template <int N>
void
foo (S s)
{
  int err;
  #pragma omp target map (s.x[0:N], s.y[0:N]) map (s.t.t[16:3]) map (from: err)
  {
    err = s.x[2] != 28 || s.y[2] != 37 || s.t.t[17] != 81;
    s.x[2]++;
    s.y[2]++;
    s.t.t[17]++;
  }
  if (err || s.x[2] != 29 || s.y[2] != 38 || s.t.t[17] != 82)
    abort ();
}

template <int N>
void
bar (S s)
{
  int err;
  #pragma omp target map (s.x, s.z)map(from:err)
  {
    err = s.x[2] != 29 || s.z != 6;
    s.x[2]++;
    s.z++;
  }
  if (err || s.x[2] != 30 || s.z != 7)
    abort ();
}

template <int N>
void
foo2 (S &s)
{
  int err;
  #pragma omp target map (s.x[N:10], s.y[N:10]) map (from: err) map (s.t.t[N+16:N+3])
  {
    err = s.x[2] != 30 || s.y[2] != 38 || s.t.t[17] != 81;
    s.x[2]++;
    s.y[2]++;
    s.t.t[17]++;
  }
  if (err || s.x[2] != 31 || s.y[2] != 39 || s.t.t[17] != 82)
    abort ();
}

template <int N>
void
bar2 (S &s)
{
  int err;
  #pragma omp target map (s.x, s.z)map(from:err)
  {
    err = s.x[2] != 31 || s.z != 7;
    s.x[2]++;
    s.z++;
  }
  if (err || s.x[2] != 32 || s.z != 8)
    abort ();
}

template <typename U>
void
foo3 (U s)
{
  int err;
  #pragma omp target map (s.x[0:10], s.y[0:10]) map (from: err) map (s.t.t[16:3])
  {
    err = s.x[2] != 32 || s.y[2] != 39 || s.t.t[17] != 82;
    s.x[2]++;
    s.y[2]++;
    s.t.t[17]++;
  }
  if (err || s.x[2] != 33 || s.y[2] != 40 || s.t.t[17] != 83)
    abort ();
}

template <typename U>
void
bar3 (U s)
{
  int err;
  #pragma omp target map (s.x, s.z)map(from:err)
  {
    err = s.x[2] != 33 || s.z != 8;
    s.x[2]++;
    s.z++;
  }
  if (err || s.x[2] != 34 || s.z != 9)
    abort ();
}

template <typename U>
void
foo4 (U &s)
{
  int err;
  #pragma omp target map (s.x[0:10], s.y[0:10]) map (from: err) map (s.t.t[16:3])
  {
    err = s.x[2] != 34 || s.y[2] != 40 || s.t.t[17] != 82;
    s.x[2]++;
    s.y[2]++;
    s.t.t[17]++;
  }
  if (err || s.x[2] != 35 || s.y[2] != 41 || s.t.t[17] != 83)
    abort ();
}

template <typename U>
void
bar4 (U &s)
{
  int err;
  #pragma omp target map (s.x, s.z)map(from:err)
  {
    err = s.x[2] != 35 || s.z != 9;
    s.x[2]++;
    s.z++;
  }
  if (err || s.x[2] != 36 || s.z != 10)
    abort ();
}

int xt[10] = { 1, 2, 28, 3, 4, 5, 6, 7, 8, 9 };
int yt[10] = { 1, 2, 37, 3, 4, 5, 6, 7, 8, 9 };
int *yp = yt;
int zt = 6;

S::S () : x (xt), y (yp), z (zt)
{
}

S::~S ()
{
}

int
main ()
{
  S s;
  s.t.t[16] = 5;
  s.t.t[17] = 81;
  s.t.t[18] = 9;
  foo <10> (s);
  if (s.t.t[17] != 81)
    abort ();
  bar <7> (s);
  foo2 <0> (s);
  if (s.t.t[17] != 82)
    abort ();
  bar2 <21> (s);
  foo3 <S> (s);
  if (s.t.t[17] != 82)
    abort ();
  bar3 <S> (s);
  foo4 <S> (s);
  if (s.t.t[17] != 83)
    abort ();
  bar4 <S> (s);
  s.x[2] -= 4;
  s.y[2] -= 2;
  s.z -= 2;
  s.t.t[17]--;
  foo3 <S &> (s);
  if (s.t.t[17] != 83)
    abort ();
  bar3 <S &> (s);
}
