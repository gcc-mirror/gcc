struct S { int a, b, c[2]; };
#pragma omp declare reduction (+: S : (omp_out.a += omp_in.a, omp_out.b += omp_in.b)) \
  initializer (omp_priv = { 0, 0, { 0, 0 } })

void
foo (S &x, S *&y, int n, int v)
{
  S zu[3] = { { 45, 47, {} }, { 46, 48, {} }, { 47, 49, {} } };
  S uu[n], wu[n];
  S (&z)[3] = zu;
  S (&u)[n] = uu;
  S (&w)[n] = wu;
  int i;
  for (i = 0; i < n; i++)
    {
      w[i].a = u[i].a = n + i;
      w[i].b = u[i].b = n - i;
      w[i].c[0] = u[i].c[0] = 0;
      w[i].c[1] = u[i].c[1] = 0;
    }
  #pragma omp taskgroup task_reduction (+: x, y[:2], z[1:2], u, w[1:v])
  {
    #pragma omp task in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x.a++;
      x.b++;
      y[0].a += 2;
      y[0].b += 12;
      y[1].a += 3;
      y[1].b += 13;
      z[1].a += 4;
      z[1].b += 14;
      u[0].a += 5;
      u[0].b += 15;
      w[1].a += 6;
      w[1].b += 16;
    }
    #pragma omp target in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x.a += 4;
      x.b += 14;
      y[0].a += 5;
      y[0].b += 15;
      y[1].a += 6;
      y[1].b += 16;
      z[2].a += 7;
      z[2].b += 17;
      u[1].a += 8;
      u[1].b += 18;
      w[2].a += 7;
      w[2].b += 17;
    }
    #pragma omp target in_reduction (+: x, y[:v], z[1:v], u, w[1:2])
    {
      x.a += 9;
      x.b += 19;
      y[0].a += 10;
      y[0].b += 20;
      y[1].a += 11;
      y[1].b += 21;
      z[1].a += 12;
      z[1].b += 22;
      u[2].a += 13;
      u[2].b += 23;
      w[1].a += 14;
      w[1].b += 24;
    }
  }
  if (x.a != 56 || y[0].a != 60 || y[1].a != 64)
    __builtin_abort ();
  if (x.b != 86 || y[0].b != 100 || y[1].b != 104)
    __builtin_abort ();
  if (z[0].a != 45 || z[1].a != 62 || z[2].a != 54)
    __builtin_abort ();
  if (z[0].b != 47 || z[1].b != 84 || z[2].b != 66)
    __builtin_abort ();
  if (u[0].a != 8 || u[1].a != 12 || u[2].a != 18)
    __builtin_abort ();
  if (u[0].b != 18 || u[1].b != 20 || u[2].b != 24)
    __builtin_abort ();
  if (w[0].a != 3 || w[1].a != 24 || w[2].a != 12)
    __builtin_abort ();
  if (w[0].b != 3 || w[1].b != 42 || w[2].b != 18)
    __builtin_abort ();
}

void
bar (S &x, S *&y, int n, int v)
{
  S zu[3] = { { 45, 47, {} }, { 46, 48, {} }, { 47, 49, {} } };
  S uu[n], wu[n];
  S (&z)[3] = zu;
  S (&u)[n] = uu;
  S (&w)[n] = wu;
  int i;
  for (i = 0; i < n; i++)
    {
      w[i].a = u[i].a = n + i;
      w[i].b = u[i].b = n - i;
      w[i].c[0] = u[i].c[0] = 0;
      w[i].c[1] = u[i].c[1] = 0;
    }
  #pragma omp parallel master
  #pragma omp taskgroup task_reduction (+: x, y[:2], z[1:2], u, w[1:v])
  {
    #pragma omp task in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x.a++;
      x.b++;
      y[0].a += 2;
      y[0].b += 12;
      y[1].a += 3;
      y[1].b += 13;
      z[1].a += 4;
      z[1].b += 14;
      u[0].a += 5;
      u[0].b += 15;
      w[1].a += 6;
      w[1].b += 16;
    }
    #pragma omp target in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x.a += 4;
      x.b += 14;
      y[0].a += 5;
      y[0].b += 15;
      y[1].a += 6;
      y[1].b += 16;
      z[2].a += 7;
      z[2].b += 17;
      u[1].a += 8;
      u[1].b += 18;
      w[2].a += 7;
      w[2].b += 17;
    }
    #pragma omp target in_reduction (+: x, y[:v], z[1:v], u, w[1:2])
    {
      x.a += 9;
      x.b += 19;
      y[0].a += 10;
      y[0].b += 20;
      y[1].a += 11;
      y[1].b += 21;
      z[1].a += 12;
      z[1].b += 22;
      u[2].a += 13;
      u[2].b += 23;
      w[1].a += 14;
      w[1].b += 24;
    }
  }
  if (x.a != 56 || y[0].a != 77 || y[1].a != 84)
    __builtin_abort ();
  if (x.b != 86 || y[0].b != 147 || y[1].b != 154)
    __builtin_abort ();
  if (z[0].a != 45 || z[1].a != 62 || z[2].a != 54)
    __builtin_abort ();
  if (z[0].b != 47 || z[1].b != 84 || z[2].b != 66)
    __builtin_abort ();
  if (u[0].a != 8 || u[1].a != 12 || u[2].a != 18)
    __builtin_abort ();
  if (u[0].b != 18 || u[1].b != 20 || u[2].b != 24)
    __builtin_abort ();
  if (w[0].a != 3 || w[1].a != 24 || w[2].a != 12)
    __builtin_abort ();
  if (w[0].b != 3 || w[1].b != 42 || w[2].b != 18)
    __builtin_abort ();
}

int
main ()
{
  S x = { 42, 52 };
  S yu[2] = { { 43, 53 }, { 44, 54 } };
  S *y = yu;
  #pragma omp parallel master
  foo (x, y, 3, 2);
  x.a = 42;
  x.b = 52;
  bar (x, y, 3, 2);
  return 0;
}
