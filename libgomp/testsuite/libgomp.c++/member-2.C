// { dg-do run }

#include <omp.h>

int c, d, e;
struct R { R () {}; ~R () {}; int r; };
template <typename Q>
struct T { T () : t(d) {}; virtual ~T () {}; Q t; };
template <typename Q>
struct A : public R, virtual public T<Q> { A () : b(c), a(e) {} Q a; int &b; void m1 (); };

void
take (int &a, int &b, int &c, int &d)
{
  asm volatile ("" : : "g" (&a), "g" (&b), "g" (&c), "g" (&d) : "memory");
}

template <typename Q>
void
A<Q>::m1 ()
{
  #pragma omp parallel private (a, r, T<Q>::t, A::b)
  {
    int q = omp_get_thread_num ();
    a = q;
    r = 2 * q;
    T<Q>::t = 3 * q;
    b = 4 * q;
    take (a, r, T<Q>::t, b);
    #pragma omp barrier
    if (A::a != q || R::r != 2 * q || T<Q>::t != 3 * q || A::b != 4 * q)
      __builtin_abort ();
  }
  a = 7;
  r = 8;
  T<Q>::t = 9;
  b = 10;
  #pragma omp parallel firstprivate (A::a, R::r, T<Q>::t, b)
  {
    int q = omp_get_thread_num ();
    take (A::a, R::r, T<Q>::t, A::b);
    if (a != 7 || r != 8 || T<Q>::t != 9 || b != 10)
      __builtin_abort ();
    A::a = 5 * q;
    R::r = 6 * q;
    T<Q>::t = 7 * q;
    A::b = 8 * q;
    take (a, r, T<Q>::t, b);
    #pragma omp barrier
    if (a != 5 * q || r != 6 * q || T<Q>::t != 7 * q || b != 8 * q)
      __builtin_abort ();
  }
  bool f = false;
  a = -5;
  b = -4;
  r = -3;
  T<Q>::t = -2;
  int n;
  #pragma omp parallel for firstprivate (a, T<Q>::t, b, f) lastprivate (A::a, r, T<Q>::t, n)
  for (int i = 0; i < omp_get_num_threads (); i++)
    {
      int q = omp_get_thread_num ();
      if (!f)
	{
	  if (A::a != -5 || A::b != -4 || T<Q>::t != -2)
	    __builtin_abort ();
	}
      else if (a != q || b != 2 * q || r != 3 * q || T<Q>::t != 4 * q)
	__builtin_abort ();
      take (a, r, T<Q>::t, b);
      A::a = q;
      A::b = 2 * q;
      R::r = 3 * q;
      T<Q>::t = 4 * q;
      n = q;
      f = true;
    }
  if (a != n || r != 3 * n || T<Q>::t != 4 * n)
    __builtin_abort ();
  b = 8;
  #pragma omp parallel
    #pragma omp single
      for (int i = 0; i < 5; i++)
	#pragma omp task firstprivate (T<Q>::t, b, n) private (a, R::r)
	  {
	    if (T<Q>::t != 4 * n || b != 8)
	      __builtin_abort ();
	    a = 9;
	    r = 8;
	    T<Q>::t = 12;
	    b = 18;
	    take (a, r, T<Q>::t, b);
	    if (a != 9 || r != 8 || T<Q>::t != 12 || b != 18)
	      __builtin_abort ();
	  }
  a = 1;
  b = 2;
  R::r = 3;
  T<Q>::t = 4;
  #pragma omp parallel private (f)
    {
      f = false;
    #pragma omp single
    #pragma omp taskloop firstprivate (r, T<Q>::t, b, f) lastprivate (a, T<Q>::t, b, n)
      for (int i = 0; i < 30; i++)
	{
	  int q = omp_get_thread_num ();
	  if (!f)
	    {
	      if (R::r != 3 || A::b != 2 || T<Q>::t != 4)
		__builtin_abort ();
	    }
	  else if (a != 7 * q || b != 8 * q || r != 9 * q || T<Q>::t != 10 * q)
	    __builtin_abort ();
	  take (a, r, T<Q>::t, b);
	  A::a = 7 * q;
	  A::b = 8 * q;
	  R::r = 9 * q;
	  T<Q>::t = 10 * q;
	  n = q;
	  f = true;
	}
    }
  if (a != 7 * n || b != 8 * n || T<Q>::t != 10 * n)
    __builtin_abort ();
  a = 1;
  b = 2;
  R::r = 3;
  T<Q>::t = 4;
  #pragma omp parallel private (f)
    {
      f = false;
    #pragma omp single
    #pragma omp taskloop firstprivate (r, T<Q>::t, b, A::a, f)
      for (int i = 0; i < 30; i++)
	{
	  int q = omp_get_thread_num ();
	  if (!f)
	    {
	      if (A::a != 1 || R::r != 3 || A::b != 2 || T<Q>::t != 4)
		__builtin_abort ();
	    }
	  else if (a != 7 * q || b != 8 * q || r != 9 * q || T<Q>::t != 10 * q)
	    __builtin_abort ();
	  take (a, r, T<Q>::t, b);
	  A::a = 7 * q;
	  A::b = 8 * q;
	  R::r = 9 * q;
	  T<Q>::t = 10 * q;
	  f = true;
	}
    }
  #pragma omp parallel private (f)
    {
      f = false;
    #pragma omp single
    #pragma omp taskloop lastprivate (a, T<Q>::t, b, n) private (R::r)
      for (int i = 0; i < 30; i++)
	{
	  int q = omp_get_thread_num ();
	  if (f && (a != 7 * q || b != 8 * q || r != 9 * q || T<Q>::t != 10 * q))
	    __builtin_abort ();
	  take (a, r, T<Q>::t, b);
	  A::a = 7 * q;
	  A::b = 8 * q;
	  R::r = 9 * q;
	  T<Q>::t = 10 * q;
	  n = q;
	  f = true;
	}
    }
  if (a != 7 * n || b != 8 * n || T<Q>::t != 10 * n)
    __builtin_abort ();
  #pragma omp parallel private (a, T<Q>::t, A::b, r)
    {
      int q = omp_get_thread_num ();
      a = q;
      b = 2 * q;
      r = 3 * q;
      T<Q>::t = 4 * q;
      take (a, b, r, T<Q>::t);
      #pragma omp single copyprivate (A::a, T<Q>::t, b, R::r)
	n = q;
      if (a != n || b != 2 * n || r != 3 * n || T<Q>::t != 4 * n)
	__builtin_abort ();
    }
  a = 0;
  b = 0;
  R::r = 0;
  T<Q>::t = 0;
  #pragma omp parallel for reduction (+: A::a, T<Q>::t, b, R::r)
  for (int i = 0; i < 30; i++)
    {
      a += i;
      A::b += 2 * i;
      r += 3 * i;
      T<Q>::t += 4 * i;
      take (a, b, r, T<Q>::t);
    }
  if (A::a != 435 || b != 2 * 435 || R::r != 3 * 435 || T<Q>::t != 4 * 435)
    __builtin_abort ();
}

int
main ()
{
  A<int> a;
  a.m1 ();
  A<int &> b;
  b.m1 ();
}
