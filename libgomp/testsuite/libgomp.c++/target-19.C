extern "C" void abort ();
struct S { char a[64]; int (&r)[2]; char b[64]; };

__attribute__((noinline, noclone)) void
foo (S s, int (&t)[3], int z)
{
  int err, sep = 1;
  // Test that implicit mapping of reference to array does NOT
  // behave like zero length array sections.  s.r can't be used
  // implicitly, as that means implicit mapping of the whole s
  // and trying to dereference the references in there is unspecified.
  #pragma omp target map(from: err) map(to: sep)
  {
    err = t[0] != 1 || t[1] != 2 || t[2] != 3;
    sep = 0;
  }
  if (err) abort ();
  // But explicit zero length array section mapping does.
  #pragma omp target map(from: err) map(tofrom: s.r[:0], t[:0])
  {
    if (sep)
      err = s.r != (int *) 0 || t != (int *) 0;
    else
      err = t[0] != 1 || t[1] != 2 || t[2] != 3 || s.r[0] != 6 || s.r[1] != 7;
  }
  if (err) abort ();
  // Similarly zero length array section, but unknown at compile time.
  #pragma omp target map(from: err) map(tofrom: s.r[:z], t[:z])
  {
    if (sep)
      err = s.r != (int *) 0 || t != (int *) 0;
    else
      err = t[0] != 1 || t[1] != 2 || t[2] != 3 || s.r[0] != 6 || s.r[1] != 7;
  }
  if (err) abort ();
  #pragma omp target enter data map (to: s.r, t)
  // But when already mapped, it binds to existing mappings.
  #pragma omp target map(from: err) map(tofrom: s.r[:0], t[:0])
  {
    err = t[0] != 1 || t[1] != 2 || t[2] != 3 || s.r[0] != 6 || s.r[1] != 7;
    sep = 0;
  }
  if (err) abort ();
  #pragma omp target map(from: err) map(tofrom: s.r[:z], t[:z])
  {
    err = t[0] != 1 || t[1] != 2 || t[2] != 3 || s.r[0] != 6 || s.r[1] != 7;
    sep = 0;
  }
  if (err) abort ();
}

int
main ()
{
  int t[3] = { 1, 2, 3 };
  int r[2] = { 6, 7 };
  S s = { {}, r, {} };
  foo (s, t, 0);
}
