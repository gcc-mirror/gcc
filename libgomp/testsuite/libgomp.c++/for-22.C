// { dg-do run }

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

template <typename T>
void
baz (I<T> &i)
{
  if (*i < 0 || *i >= 2000)
    abort ();
  results[*i]++;
}

I<int>
f1 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel shared(i)
  {
#pragma omp for lastprivate (i) schedule(runtime)
    for (i = x; i != y; i++)
      baz (i);
#pragma omp single
    i += 3;
  }
  return I<int> (i);
}

I<int>
f2 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel for lastprivate(i)
  for (i = x; i != y - 1; i = 2 - 8 + 7 + i)
    baz (i);
  return I<int> (i);
}

template <typename T>
I<int>
f3 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel
  #pragma omp for lastprivate (i)
    for (i = x; i != y; i = i + 9 - 8)
      baz (i);
  return i;
}

template <typename T>
I<int>
f4 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel for lastprivate(i)
  for (i = x + 2000 - 64; i != y + 10; --i)
    baz (i);
  return I<int> (i);
}

template <typename T>
I<int>
f5 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel for lastprivate(i)
  for (i = x + 2000 - 64; i != y + T (10); i--)
    baz (i);
  return i;
}

template <typename T>
I<int>
f6 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel for lastprivate (i)
  for (i = x + 2000 - 64; i != y + 10; i = i - T (12) + T (11))
    {
      I<int> j = i + -10;
      baz (j);
    }
  return I<int> (i);
}

template <int N>
I<int>
f7 (I<int> i, const I<int> &x, const I<int> &y)
{
#pragma omp parallel for lastprivate(i)
  for (i = x - 10; i != y + 10; i += N)
    baz (i);
  return I<int> (i);
}

template <int N>
I<int>
f8 (J<int> j)
{
  I<int> i;
#pragma omp parallel shared \
(i)
#pragma omp for lastprivate (i)
  for (i = j.begin (); i != j.end () + N; i += 1)
    baz (i);
  return i;
}

I<int> i9;

template <long N>
I<int> &
f9 (J<int> j)
{
#pragma omp parallel for lastprivate(i9)
  for (i9 = j.begin (); i9 != j.end () - N; i9 = i9 - N)
    baz (i9);
  return i9;
}

template <typename T, int N>
I<T>
f10 (const I<T> &x, const I<T> &y)
{
  I<T> i;
#pragma omp parallel for lastprivate (i)
  for (i = x; i != y; i = i + N)
    baz (i);
  return i;
}

template <typename T, typename U>
T
f11 (T i, const T &x, const T &y)
{
#pragma omp parallel
  {
#pragma omp for lastprivate (i)
    for (i = x + U (0); i != y + U (2 - 2); i = U(3) + U(-2) + i)
      baz (i);
#pragma omp single
    {
      T j = y + 3;
      baz (j);
    }
  }
  return i;
}

template <typename T>
T
f12 (const T &x, const T &y)
{
  T i;
#pragma omp parallel for lastprivate (i)
  for (i = x; i != y; --i)
    baz (i);
  return i;
}

#define check(expr) \
  for (int i = 0; i < 2000; i++)			\
    if (expr)						\
      {							\
	if (results[i] != 1)				\
	  abort ();					\
	results[i] = 0;					\
      }							\
    else if (results[i])				\
      abort ()

int
main ()
{
  int a[2000];
  long b[2000];
  for (int i = 0; i < 2000; i++)
    {
      a[i] = i;
      b[i] = i;
    }
  if (*f1 (&a[10], &a[1990]) != 1993)
    abort ();
  check (i >= 10 && i < 1990);
  if (*f2 (&a[0], &a[1999]) != 1998)
    abort ();
  check (i < 1998);
  if (*f3<char> (&a[20], &a[1837]) != 1837)
    abort ();
  check (i >= 20 && i < 1837);
  if (*f4<int> (&a[0], &a[30]) != 40)
    abort ();
  check (i > 40 && i <= 2000 - 64);
  if (*f5<int> (&a[0], &a[100]) != 110)
    abort ();
  check (i > 110 && i <= 2000 - 64);
  if (*f6<int> (&a[10], &a[110]) != 120)
    abort ();
  check (i > 110 && i <= 2000 - 64);
  if (*f7<1> (I<int> (), &a[12], &a[1800]) != 1810)
    abort ();
  check (i >= 2 && i < 1810);
  if (*f8<121> (J<int> (&a[14], &a[1803])) != 1924)
    abort ();
  check (i >= 14 && i < 1924);
  if (*f9<-1> (J<int> (&a[33], &a[1967])) != 1968)
    abort ();
  check (i >= 33 && i <= 1967);
  if (*f10<int, -1> (&a[1939], &a[17]) != 17)
    abort ();
  check (i > 17 && i <= 1939);
  if (*f11<I<int>, int> (I<int> (), &a[16], &a[1981]) != 1981)
    abort ();
  check ((i >= 16 && i < 1981) || i == 1984);
  if (*f12<I<int> > (&a[1761], &a[37]) != 37)
    abort ();
  check (i > 37 && i <= 1761);
  if (*f10<long, -1> (&b[1939], &b[17]) != 17)
    abort ();
  check (i > 17 && i <= 1939);
  if (*f11<I<long>, long> (I<long> (), &b[16], &b[1981]) != 1981)
    abort ();
  check ((i >= 16 && i < 1981) || i == 1984);
  if (*f12<I<long> > (&b[1761], &b[37]) != 37)
    abort ();
  check (i > 37 && i <= 1761);
}
