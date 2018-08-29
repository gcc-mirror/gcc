#ifdef __cplusplus
extern "C"
#else
extern
#endif
void abort (void);
struct S { int s, t; };

void
foo ()
{
  int x = 5, y = 6, err = 0;
  struct S u = { 7, 8 }, v = { 9, 10 };
  double s = 11.5, t = 12.5;
  #pragma omp target private (x, u, s) firstprivate (y, v, t) map(from:err)
  {
    x = y;
    u = v;
    s = t;
    err = (x != 6 || y != 6
	   || u.s != 9 || u.t != 10 || v.s != 9 || v.t != 10
	   || s != 12.5 || t != 12.5);
    x += 1;
    y += 2;
    u.s += 3;
    v.t += 4;
    s += 2.5;
    t += 3.0;
    if (x != 7 || y != 8
	|| u.s != 12 || u.t != 10 || v.s != 9 || v.t != 14
	|| s != 15.0 || t != 15.5)
      err = 1;
  }
  if (err || x != 5 || y != 6
      || u.s != 7 || u.t != 8 || v.s != 9 || v.t != 10
      || s != 11.5 || t != 12.5)
    abort ();
}

int
main ()
{
  foo ();
  return 0;
}
