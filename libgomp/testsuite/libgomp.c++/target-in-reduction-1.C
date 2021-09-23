void
foo (int &x, int *&y, int n, int v)
{
  int zu[3] = { 45, 46, 47 };
  int uu[n], wu[n], i;
  int (&z)[3] = zu;
  int (&u)[n] = uu;
  int (&w)[n] = wu;
  for (i = 0; i < n; i++)
    w[i] = u[i] = n + i;
  #pragma omp taskgroup task_reduction (+: x, y[:2], z[1:2], u, w[1:v])
  {
    #pragma omp task in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x++;
      y[0] += 2;
      y[1] += 3;
      z[1] += 4;
      u[0] += 5;
      w[1] += 6;
    }
    #pragma omp target in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x += 4;
      y[0] += 5;
      y[1] += 6;
      z[2] += 7;
      u[1] += 8;
      w[2] += 7;
    }
    #pragma omp target in_reduction (+: x, y[:v], z[1:v], u, w[1:2])
    {
      x += 9;
      y[0] += 10;
      y[1] += 11;
      z[1] += 12;
      u[2] += 13;
      w[1] += 14;
    }
  }
  if (x != 56 || y[0] != 60 || y[1] != 64)
    __builtin_abort ();
  if (z[0] != 45 || z[1] != 62 || z[2] != 54)
    __builtin_abort ();
  if (u[0] != 8 || u[1] != 12 || u[2] != 18)
    __builtin_abort ();
  if (w[0] != 3 || w[1] != 24 || w[2] != 12)
    __builtin_abort ();
}

void
bar (int &x, int *&y, int n, int v)
{
  int zu[3] = { 45, 46, 47 };
  int uu[n], wu[n], i;
  int (&z)[3] = zu;
  int (&u)[n] = uu;
  int (&w)[n] = wu;
  for (i = 0; i < n; i++)
    w[i] = u[i] = n + i;
  #pragma omp parallel master
  #pragma omp taskgroup task_reduction (+: x, y[:2], z[1:2], u, w[1:v])
  {
    #pragma omp task in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x++;
      y[0] += 2;
      y[1] += 3;
      z[1] += 4;
      u[0] += 5;
      w[1] += 6;
    }
    #pragma omp target in_reduction (+: x, y[:2], z[1:2], u, w[1:v])
    {
      x += 4;
      y[0] += 5;
      y[1] += 6;
      z[2] += 7;
      u[1] += 8;
      w[2] += 7;
    }
    #pragma omp target in_reduction (+: x, y[:v], z[1:v], u, w[1:2])
    {
      x += 9;
      y[0] += 10;
      y[1] += 11;
      z[1] += 12;
      u[2] += 13;
      w[1] += 14;
    }
  }
  if (x != 56 || y[0] != 77 || y[1] != 84)
    __builtin_abort ();
  if (z[0] != 45 || z[1] != 62 || z[2] != 54)
    __builtin_abort ();
  if (u[0] != 8 || u[1] != 12 || u[2] != 18)
    __builtin_abort ();
  if (w[0] != 3 || w[1] != 24 || w[2] != 12)
    __builtin_abort ();
}

int
main ()
{
  int x = 42;
  int yu[2] = { 43, 44 };
  int *y = yu;
  #pragma omp parallel master
  foo (x, y, 3, 2);
  x = 42;
  bar (x, y, 3, 2);
  return 0;
}
