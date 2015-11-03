extern "C" void abort (void);
struct S { int s, t; };

void
foo (int &x, int &y, S &u, S &v, double &s, double &t)
{
  int err = 0, i;
  int a[y - 2], b[y - 2];
  int (&c)[y - 2] = a, (&d)[y - 2] = b;
  for (i = 0; i < y - 2; i++)
    {
      c[i] = i;
      d[i] = 3 + i;
    }
  #pragma omp target private (x, u, s, c, i) firstprivate (y, v, t, d) map(from:err)
  {
    x = y;
    u = v;
    s = t;
    for (i = 0; i < y - 2; i++)
      c[i] = d[i];
    err = (x != 6 || y != 6
	   || u.s != 9 || u.t != 10 || v.s != 9 || v.t != 10
	   || s != 12.5 || t != 12.5);
    for (i = 0; i < y - 2; i++)
      if (d[i] != 3 + i || c[i] != 3 + i)
	err = 1;
      else
	{
	  c[i] += 2 * i;
	  d[i] += i;
	}
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
    for (i = 0; i < y - 4; i++)
      if (d[i] != 3 + 2 * i || c[i] != 3 + 3 * i)
	err = 1;
  }
  if (err || x != 5 || y != 6
      || u.s != 7 || u.t != 8 || v.s != 9 || v.t != 10
      || s != 11.5 || t != 12.5)
    abort ();
  for (i = 0; i < y - 2; i++)
    if (d[i] != 3 + i || c[i] != i)
      abort ();
}

int
main ()
{
  int x = 5, y = 6;
  S u = { 7, 8 }, v = { 9, 10 };
  double s = 11.5, t = 12.5;
  foo (x, y, u, v, s, t);
  return 0;
}
