#include <stdlib.h>

int x;
long long y;
int r, s, t;

void
foo (const char *a)
{
  #pragma omp parallel sections lastprivate (conditional: x, y)
  {
    if (a[0])
      x = a[0];
    #pragma omp section
    {
      if (a[1])
	x = a[1];
      if (a[2])
	y = a[2];
    }
    #pragma omp section
    if (a[3])
      y = a[3];
    #pragma omp section
    if (a[4])
      x = a[4];
    #pragma omp section
    {
      if (a[5])
	x = a[5];
      if (a[6])
	y = a[6];
    }
  }
}

void
bar (const char *a)
{
  #pragma omp parallel sections lastprivate (conditional: x, y) reduction (task, +: t)
  {
    if (a[0])
      x = a[0];
    #pragma omp section
    {
      if (a[1])
	x = a[1];
      if (a[2])
	y = a[2];
      #pragma omp task in_reduction (+: t)
      t++;
    }
    #pragma omp section
    if (a[3])
      y = a[3];
    #pragma omp section
    if (a[4])
      x = a[4];
    #pragma omp section
    {
      #pragma omp task in_reduction (+: t)
      ++t;
      if (a[5])
	x = a[5];
      if (a[6])
	y = a[6];
    }
  }
}

void
baz (const char *a)
{
  #pragma omp parallel sections lastprivate (conditional: x, y) reduction (+: r, s)
  {
    if (a[0])
      x = a[0];
    #pragma omp section
    {
      if (a[1])
	x = a[1];
      ++r;
      ++s;
      if (a[2])
	y = a[2];
    }
    #pragma omp section
    if (a[3])
      y = a[3];
    #pragma omp section
    {
      ++s;
      if (a[4])
	x = a[4];
    }
    #pragma omp section
    {
      if (a[5])
	x = a[5];
      if (a[6])
	y = a[6];
      ++s;
    }
  }
}

int
main ()
{
  foo ("\0\1\2\3\0\5");
  if (x != 5 || y != 3)
    abort ();

  foo ("\6\0\0\0\0\0\7");
  if (x != 6 || y != 7)
    abort ();

  foo ("\7\6\5\4\3\2\1");
  if (x != 2 || y != 1)
    abort ();

  foo ("\0\0\4\3\0\7");
  if (x != 7 || y != 3)
    abort ();

  bar ("\0\1\2\4\0\5");
  if (x != 5 || y != 4 || t != 2)
    abort ();

  bar ("\6\0\0\0\0\0\7");
  if (x != 6 || y != 7 || t != 4)
    abort ();

  bar ("\7\6\5\4\3\2\1");
  if (x != 2 || y != 1 || t != 6)
    abort ();

  bar ("\0\0\4\3\0\7");
  if (x != 7 || y != 3 || t != 8)
    abort ();

  baz ("\0\1\2\4\0\5");
  if (x != 5 || y != 4 || r != 1 || s != 3)
    abort ();

  baz ("\6\0\0\0\0\0\7");
  if (x != 6 || y != 7 || r != 2 || s != 6)
    abort ();

  baz ("\7\6\5\4\3\2\1");
  if (x != 2 || y != 1 || r != 3 || s != 9)
    abort ();

  baz ("\0\0\4\3\0\7");
  if (x != 7 || y != 3 || r != 4 || s != 12)
    abort ();

  return 0;
}
