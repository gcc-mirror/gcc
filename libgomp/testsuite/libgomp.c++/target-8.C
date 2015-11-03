extern "C" void abort ();
struct S { int a; };
#ifdef __SIZEOF_INT128__
typedef __int128 T;
#else
typedef long long int T;
#endif

void
foo (T a, int b, struct S c)
{
  int err;
  #pragma omp target firstprivate (a, b, c) map(from:err)
  {
    err = 0;
    if (a != 131 || b != 276 || c.a != 59)
      err = 1;
    a = 936;
    b = 27;
    c.a = 98;
    if (a != 936 || b != 27 || c.a != 98)
      err = 1;
  }
  if (err || a != 131 || b != 276 || c.a != 59)
    abort ();
}

void
bar (T &a, int &b, struct S &c)
{
  int err;
  #pragma omp target firstprivate (a, b, c) map(from:err)
  {
    err = 0;
    if (a != 131 || b != 276 || c.a != 59)
      err = 1;
    a = 936;
    b = 27;
    c.a = 98;
    if (a != 936 || b != 27 || c.a != 98)
      err = 1;
  }
  if (err || a != 131 || b != 276 || c.a != 59)
    abort ();
}

int
main ()
{
  T a = 131;
  int b = 276;
  struct S c;
  c.a = 59;
  foo (a, b, c);
  bar (a, b, c);
  if (a != 131 || b != 276 || c.a != 59)
    abort ();
}
