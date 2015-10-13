// { dg-do run }

#include <omp.h>

__attribute__((noinline, noclone)) void
foo (int &a, short &d, char &g)
{
  unsigned long b = 12;
  unsigned long &c = b;
  long long e = 21;
  long long &f = e;
  unsigned int h = 12;
  unsigned int &k = h;
  #pragma omp parallel default(none) private(a, c) firstprivate(d, f) shared(g, k)
    {
      int i = omp_get_thread_num ();
      a = i;
      c = 2 * i;
      if (d != 27 || f != 21)
	__builtin_abort ();
      d = 3 * (i & 0xfff);
      f = 4 * i;
      #pragma omp barrier
      if (a != i || c != 2 * i || d != 3 * (i & 0xfff) || f != 4 * i)
	__builtin_abort ();
      #pragma omp for lastprivate(g, k)
      for (int j = 0; j < 32; j++)
	{
	  g = j;
	  k = 3 * j;
	}
    }
  if (g != 31 || k != 31 * 3)
    __builtin_abort ();
  #pragma omp parallel for firstprivate (g, k) lastprivate (g, k)
  for (int j = 0; j < 32; j++)
    {
      if (g != 31 || k != 31 * 3)
	__builtin_abort ();
      if (j == 31)
	{
	  g = 29;
	  k = 138;
	}
    }
  if (g != 29 || k != 138)
    __builtin_abort ();
}

int
main ()
{
  int a = 5;
  short d = 27;
  char g = ' ';
  foo (a, d, g);
}
