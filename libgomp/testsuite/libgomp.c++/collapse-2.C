// { dg-do run }

#include <omp.h>
typedef __PTRDIFF_TYPE__ ptrdiff_t;
extern "C" void abort ();

template <typename T>
class I
{
public:
  typedef ptrdiff_t difference_type;
  I ();
  ~I ();
  I (T *);
  I (const I &);
  T &operator * ();
  T *operator -> ();
  T &operator [] (const difference_type &) const;
  I &operator = (const I &);
  I &operator ++ ();
  I operator ++ (int);
  I &operator -- ();
  I operator -- (int);
  I &operator += (const difference_type &);
  I &operator -= (const difference_type &);
  I operator + (const difference_type &) const;
  I operator - (const difference_type &) const;
  template <typename S> friend bool operator == (I<S> &, I<S> &);
  template <typename S> friend bool operator == (const I<S> &, const I<S> &);
  template <typename S> friend bool operator < (I<S> &, I<S> &);
  template <typename S> friend bool operator < (const I<S> &, const I<S> &);
  template <typename S> friend bool operator <= (I<S> &, I<S> &);
  template <typename S> friend bool operator <= (const I<S> &, const I<S> &);
  template <typename S> friend bool operator > (I<S> &, I<S> &);
  template <typename S> friend bool operator > (const I<S> &, const I<S> &);
  template <typename S> friend bool operator >= (I<S> &, I<S> &);
  template <typename S> friend bool operator >= (const I<S> &, const I<S> &);
  template <typename S> friend typename I<S>::difference_type operator - (I<S> &, I<S> &);
  template <typename S> friend typename I<S>::difference_type operator - (const I<S> &, const I<S> &);
  template <typename S> friend I<S> operator + (typename I<S>::difference_type , const I<S> &);
private:
  T *p;
};
template <typename T> I<T>::I () : p (0) {}
template <typename T> I<T>::~I () { p = (T *) 0; }
template <typename T> I<T>::I (T *x) : p (x) {}
template <typename T> I<T>::I (const I &x) : p (x.p) {}
template <typename T> T &I<T>::operator * () { return *p; }
template <typename T> T *I<T>::operator -> () { return p; }
template <typename T> T &I<T>::operator [] (const difference_type &x) const { return p[x]; }
template <typename T> I<T> &I<T>::operator = (const I &x) { p = x.p; return *this; }
template <typename T> I<T> &I<T>::operator ++ () { ++p; return *this; }
template <typename T> I<T> I<T>::operator ++ (int) { return I (p++); }
template <typename T> I<T> &I<T>::operator -- () { --p; return *this; }
template <typename T> I<T> I<T>::operator -- (int) { return I (p--); }
template <typename T> I<T> &I<T>::operator += (const difference_type &x) { p += x; return *this; }
template <typename T> I<T> &I<T>::operator -= (const difference_type &x) { p -= x; return *this; }
template <typename T> I<T> I<T>::operator + (const difference_type &x) const { return I (p + x); }
template <typename T> I<T> I<T>::operator - (const difference_type &x) const { return I (p - x); }
template <typename T> bool operator == (I<T> &x, I<T> &y) { return x.p == y.p; }
template <typename T> bool operator == (const I<T> &x, const I<T> &y) { return x.p == y.p; }
template <typename T> bool operator != (I<T> &x, I<T> &y) { return !(x == y); }
template <typename T> bool operator != (const I<T> &x, const I<T> &y) { return !(x == y); }
template <typename T> bool operator < (I<T> &x, I<T> &y) { return x.p < y.p; }
template <typename T> bool operator < (const I<T> &x, const I<T> &y) { return x.p < y.p; }
template <typename T> bool operator <= (I<T> &x, I<T> &y) { return x.p <= y.p; }
template <typename T> bool operator <= (const I<T> &x, const I<T> &y) { return x.p <= y.p; }
template <typename T> bool operator > (I<T> &x, I<T> &y) { return x.p > y.p; }
template <typename T> bool operator > (const I<T> &x, const I<T> &y) { return x.p > y.p; }
template <typename T> bool operator >= (I<T> &x, I<T> &y) { return x.p >= y.p; }
template <typename T> bool operator >= (const I<T> &x, const I<T> &y) { return x.p >= y.p; }
template <typename T> typename I<T>::difference_type operator - (I<T> &x, I<T> &y) { return x.p - y.p; }
template <typename T> typename I<T>::difference_type operator - (const I<T> &x, const I<T> &y) { return x.p - y.p; }
template <typename T> I<T> operator + (typename I<T>::difference_type x, const I<T> &y) { return I<T> (x + y.p); }

template <typename T>
class J
{
public:
  J(const I<T> &x, const I<T> &y) : b (x), e (y) {}
  const I<T> &begin ();
  const I<T> &end ();
private:
  I<T> b, e;
};

template <typename T> const I<T> &J<T>::begin () { return b; }
template <typename T> const I<T> &J<T>::end () { return e; }

int results[2000];

void
f1 (J<int> x, J<int> y, J<int> z)
{
  I<int> i, j, k;
  int l, f = 0, n = 0, m = 0;
#pragma omp parallel shared (i, j, k, l) firstprivate (f) \
		     reduction (+:n, m) num_threads (8)
  {
  #pragma omp for lastprivate (i, j, k, l) schedule (static, 9) \
		  collapse (4)
    for (i = x.begin (); i < x.end (); ++i)
      for (j = y.begin (); j <= y.end (); j += 1)
	for (l = 0; l < 1; l++)
	  for (k = z.begin () + 3; k < z.end () - 3; k++)
	    if (omp_get_num_threads () == 8
		&& ((*i + 2) * 12 + (*j + 5) * 4 + (*k - 13)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
  }
  if (n || i != x.end () || j != y.end () + 1 || k != z.end () - 3
      || m != 72 || l != 1)
    abort ();
}

void
f2 (J<int> x, J<int> y, J<int> z)
{
  int f = 0, n = 0, m = 0;
#pragma omp parallel for firstprivate (f) reduction (+:n, m) \
			 num_threads (8) schedule (static, 9) \
			 collapse (6 - 2)
  for (I<int> i = x.end () - 1; i >= x.begin (); --i)
    for (int l = -131; l >= -131; l--)
      for (I<int> j = y.end (); j > y.begin () - 1; j -= 1)
	{
	  for (I<int> k = z.end () - 4; k >= z.begin () + 3; k--)
	    if (omp_get_num_threads () == 8
		&& ((3 - *i) * 12 + (-3 - *j) * 4 + (16 - *k)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
	}
  if (n || m != 72)
    abort ();
}

template <typename T>
void
f3 (J<int> x, J<int> y, J<int> z)
{
  I<int> i, j, k;
  int l, f = 0, n = 0, m = 0;
#pragma omp parallel shared (i, j, k, l) firstprivate (f) \
		     reduction (+:n, m) num_threads (8)
  {
  #pragma omp for lastprivate (i, j, k, l) schedule (static, 9) \
		  collapse (4)
    for (i = x.begin (); i < x.end (); ++i)
      for (j = y.begin (); j <= y.end (); j += 1)
	for (k = z.begin () + 3; k < z.end () - 3; k++)
	  for (l = 7; l <= 7; l++)
	    if (omp_get_num_threads () == 8
		&& ((*i + 2) * 12 + (*j + 5) * 4 + (*k - 13)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
  }
  if (n || i != x.end () || j != y.end () + 1 || k != z.end () - 3
      || m != 72 || l != 8)
    abort ();
}

template <typename T>
void
f4 (J<int> x, J<int> y, J<int> z)
{
  int f = 0, n = 0, m = 0;
#pragma omp parallel for firstprivate (f) reduction (+:n, m) \
			 num_threads (8) schedule (static, 9) \
			 collapse (5 - 2)
  for (I<int> i = x.end () - 1; i >= x.begin (); --i)
    {
      for (I<int> j = y.end (); j > y.begin () - 1; j -= 1)
	{
	  for (I<int> k = z.end () - 4; k >= z.begin () + 3; k--)
	    if (omp_get_num_threads () == 8
		&& ((3 - *i) * 12 + (-3 - *j) * 4 + (16 - *k)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
	}
    }
  if (n || m != 72)
    abort ();
}

template <typename T>
void
f5 (J<int> x, J<int> y, J<int> z)
{
  I<int> i, j, k;
  int f = 0, n = 0, m = 0;
#pragma omp parallel shared (i, j, k) firstprivate (f) \
		     reduction (+:n, m) num_threads (8)
  {
  #pragma omp for lastprivate (i, j, k) schedule (static, 9) \
		  collapse (3)
    for (i = x.begin (); i < x.end (); ++i)
      for (j = y.begin (); j <= y.end (); j += (T) 1)
	{
	  for (k = z.begin () + 3; k < z.end () - 3; k++)
	    if (omp_get_num_threads () == 8
		&& ((*i + 2) * 12 + (*j + 5) * 4 + (*k - 13)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
	}
  }
  if (n || i != x.end () || j != y.end () + 1 || k != z.end () - 3
      || m != 72)
    abort ();
}

template <typename T>
void
f6 (J<int> x, J<int> y, J<int> z)
{
  int f = 0, n = 0, m = 0;
#pragma omp parallel for firstprivate (f) reduction (+:n, m) \
			 num_threads (8) schedule (static, 9) \
			 collapse (5 - 2)
  for (I<int> i = x.end () - 1; i >= x.begin (); --i)
    {
      for (I<int> j = y.end (); j > y.begin () - 1; j -= 1)
	{
	  for (I<int> k = z.end () - 4; k >= z.begin () + (T) 3; k--)
	    if (omp_get_num_threads () == 8
		&& ((3 - *i) * 12 + (-3 - *j) * 4 + (16 - *k)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
	}
    }
  if (n || m != 72)
    abort ();
}

template <typename T>
void
f7 (J<T> x, J<T> y, J<T> z)
{
  I<T> i, j, k, o = y.begin ();
  T l, f = 0, n = 0, m = 0;
#pragma omp parallel shared (i, j, k, l) firstprivate (f) \
		     reduction (+:n, m) num_threads (8)
  {
  #pragma omp for lastprivate (i, j, k, l) schedule (static, 9) \
		  collapse (4)
    for (i = x.begin (); i < x.end (); ++i)
      for (j = y.begin (); j <= y.end (); j += 1)
	for (l = *o; l <= *o; l = 1 + l)
	  for (k = z.begin () + 3; k < z.end () - 3; k++)
	    if (omp_get_num_threads () == 8
		&& ((*i + 2) * 12 + (*j + 5) * 4 + (*k - 13)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
  }
  if (n || i != x.end () || j != y.end () + 1 || k != z.end () - 3
      || m != 72 || l != *o + 1)
    abort ();
}

template <typename T>
void
f8 (J<T> x, J<T> y, J<T> z)
{
  T f = 0, n = 0, m = 0;
#pragma omp parallel for firstprivate (f) reduction (+:n, m) \
			 num_threads (8) schedule (static, 9) \
			 collapse (6 - 2)
  for (I<T> i = x.end () - 1; i >= x.begin (); --i)
    for (T l = 0; l < 1; l++)
      for (I<T> j = y.end (); j > y.begin () - 1; j -= 1)
	{
	  for (I<T> k = z.end () - 4; k >= z.begin () + 3; k--)
	    if (omp_get_num_threads () == 8
		&& ((3 - *i) * 12 + (-3 - *j) * 4 + (16 - *k)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
	}
  if (n || m != 72)
    abort ();
}

template <typename S, typename T>
void
f9 (J<T> x, J<T> y, J<T> z)
{
  S i, j, k, o = y.begin ();
  T l, f = 0, n = 0, m = 0;
#pragma omp parallel shared (i, j, k, l) firstprivate (f) \
		     reduction (+:n, m) num_threads (8)
  {
  #pragma omp for lastprivate (i, j, k, l) schedule (static, 9) \
		  collapse (4)
    for (i = x.begin (); i < x.end (); ++i)
      for (j = y.begin (); j <= y.end (); j += 1)
	for (l = *o; l <= *o; l = 1 + l)
	  for (k = z.begin () + 3; k < z.end () - 3; k++)
	    if (omp_get_num_threads () == 8
		&& ((*i + 2) * 12 + (*j + 5) * 4 + (*k - 13)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
  }
  if (n || i != x.end () || j != y.end () + 1 || k != z.end () - 3
      || m != 72 || l != *o + 1)
    abort ();
}

template <typename S, typename T>
void
f10 (J<T> x, J<T> y, J<T> z)
{
  T f = 0, n = 0, m = 0;
#pragma omp parallel for firstprivate (f) reduction (+:n, m) \
			 num_threads (8) schedule (static, 9) \
			 collapse (6 - 2)
  for (S i = x.end () - 1; i >= x.begin (); --i)
    for (T l = 0; l < 1; l++)
      for (S j = y.end (); j > y.begin () - 1; j -= 1)
	{
	  for (S k = z.end () - 4; k >= z.begin () + 3; k--)
	    if (omp_get_num_threads () == 8
		&& ((3 - *i) * 12 + (-3 - *j) * 4 + (16 - *k)
		    != (omp_get_thread_num () * 9 + f++)))
	      n++;
	    else
	      m++;
	}
  if (n || m != 72)
    abort ();
}

int
main ()
{
  int a[2000];
  long b[2000];
  for (int i = 0; i < 2000; i++)
    {
      a[i] = i - 1000;
      b[i] = i - 1000;
    }
  J<int> x (&a[998], &a[1004]);
  J<int> y (&a[995], &a[997]);
  J<int> z (&a[1010], &a[1020]);
  f1 (x, y, z);
  f2 (x, y, z);
  f3 <int> (x, y, z);
  f4 <int> (x, y, z);
  f5 <int> (x, y, z);
  f6 <int> (x, y, z);
  f7 <int> (x, y, z);
  f8 <int> (x, y, z);
  f9 <I<int>, int> (x, y, z);
  f10 <I<int>, int> (x, y, z);
}
