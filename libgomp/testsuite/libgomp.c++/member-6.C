// { dg-do run }

#include <omp.h>

struct R { R () {}; ~R () {}; int r; };
struct T { T () {}; virtual ~T () {}; int t; };
int c;
struct A : public R, virtual public T { A () : b(c) {} int a; int &b; void m1 (); };

void
take (int &a, int &b, int &c, int &d)
{
  asm volatile ("" : : "g" (&a), "g" (&b), "g" (&c), "g" (&d) : "memory");
}

void
A::m1 ()
{
  #pragma omp parallel private (a, T::t) shared (r, A::b) default(none)
  {
    int q = omp_get_thread_num (), q2;
    a = q;
    t = 3 * q;
    #pragma omp single copyprivate (q2)
    {
      r = 2 * q;
      b = 4 * q;
      q2 = q;
    }
    take (a, r, t, b);
    #pragma omp barrier
    if (A::a != q || R::r != 2 * q2 || T::t != 3 * q || A::b != 4 * q2)
      __builtin_abort ();
  }
  a = 7;
  r = 8;
  t = 9;
  b = 10;
  #pragma omp parallel shared (A::a) default (none) firstprivate (R::r, b) shared (t)
  {
    int q = omp_get_thread_num (), q2;
    take (A::a, R::r, T::t, A::b);
    if (a != 7 || r != 8 || t != 9 || b != 10)
      __builtin_abort ();
    R::r = 6 * q;
    #pragma omp barrier
    #pragma omp single copyprivate (q2)
    {
      A::a = 5 * q;
      T::t = 7 * q;
      q2 = q;
    }
    A::b = 8 * q;
    take (a, r, t, b);
    #pragma omp barrier
    if (a != 5 * q2 || r != 6 * q || t != 7 * q2 || b != 8 * q)
      __builtin_abort ();
  }
  a = 1;
  b = 2;
  R::r = 3;
  t = 4;
  bool f = false;
  #pragma omp parallel private (f)
    {
      f = false;
    #pragma omp single
    #pragma omp taskloop default(none) firstprivate (r, A::a, f) shared (T::t, b)
      for (int i = 0; i < 30; i++)
	{
	  int q = omp_get_thread_num ();
	  int tv, bv;
	  #pragma omp atomic read
	  tv = t;
	  #pragma omp atomic read
	  bv = A::b;
	  if (i == 16)
	    {
	      if (bv != 2 || tv != 4)
		__builtin_abort ();
	    }
	  else
	    {
	      if ((bv != 2 && bv != 8) || (tv != 4 && tv != 9))
		__builtin_abort ();
	    }
	  if (!f)
	    {
	      if (A::a != 1 || R::r != 3)
		__builtin_abort ();
	    }
	  else if (a != 7 * q || r != 9 * q)
	    __builtin_abort ();
	  take (a, r, t, b);
	  A::a = 7 * q;
	  R::r = 9 * q;
	  if (i == 16)
	    {
	      #pragma omp atomic write
	      A::b = 8;
	      #pragma omp atomic write
	      T::t = 9;
	    }
	  f = true;
	}
    }
}

int
main ()
{
  A a;
  a.m1 ();
}
