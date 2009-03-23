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
template <typename T> I<T>::~I () {}
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

void
f1 (const I<int> &x, const I<int> &y)
{
#pragma omp parallel for
  for (I<int> i = x; y >= i; i += 6)
    baz (i);
}

void
f2 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel for private(i)
  for (i = x; y - 1 > i; i = 1 - 6 + 7 + i)
    baz (i);
}

template <typename T>
void
f3 (const I<int> &x, const I<int> &y)
{
#pragma omp parallel for
  for (I<int> i = x; y >= i; i = i + 9 - 8)
    baz (i);
}

template <typename T>
void
f4 (const I<int> &x, const I<int> &y)
{
  I<int> i;
#pragma omp parallel for lastprivate(i)
  for (i = x + 2000 - 64; y + 10 < i; --i)
    baz (i);
}

void
f5 (const I<int> &x, const I<int> &y)
{
#pragma omp parallel for
  for (I<int> i = x + 2000 - 64; y + 10 < i; i -= 10)
    baz (i);
}

template <int N>
void
f6 (const I<int> &x, const I<int> &y)
{
#pragma omp parallel for
  for (I<int> i = x + 2000 - 64; y + 10 < i; i = i - 12 + 2)
    {
      I<int> j = i + N;
      baz (j);
    }
}

template <int N>
void
f7 (I<int> i, const I<int> &x, const I<int> &y)
{
#pragma omp parallel for
  for (i = x - 10; y + 10 >= i; i += N)
    baz (i);
}

template <int N>
void
f8 (J<int> j)
{
  I<int> i;
#pragma omp parallel for
  for (i = j.begin (); j.end () + N >= i; i += 2)
    baz (i);
}

template <typename T, int N>
void
f9 (const I<T> &x, const I<T> &y)
{
#pragma omp parallel for
  for (I<T> i = x; y >= i; i = i + N)
    baz (i);
}

template <typename T, int N>
void
f10 (const I<T> &x, const I<T> &y)
{
  I<T> i;
#pragma omp parallel for
  for (i = x; y < i; i = i + N)
    baz (i);
}

template <typename T>
void
f11 (const T &x, const T &y)
{
#pragma omp parallel
  {
#pragma omp for nowait
    for (T i = x; y >= i; i += 3)
      baz (i);
#pragma omp single
    {
      T j = y + 3;
      baz (j);
    }
  }
}

template <typename T>
void
f12 (const T &x, const T &y)
{
  T i;
#pragma omp parallel for
  for (i = x; y < i; --i)
    baz (i);
}

template <int N>
struct K
{
  template <typename T>
  static void
  f13 (const T &x, const T &y)
  {
#pragma omp parallel for
    for (T i = x; y + N >= i; i += N)
      baz (i);
  }
};

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
  f1 (&a[10], &a[1990]);
  check (i >= 10 && i <= 1990 && (i - 10) % 6 == 0);
  f2 (&a[0], &a[1999]);
  check (i < 1998 && (i & 1) == 0);
  f3<char> (&a[20], &a[1837]);
  check (i >= 20 && i <= 1837);
  f4<int> (&a[0], &a[30]);
  check (i > 40 && i <= 2000 - 64);
  f5 (&a[0], &a[100]);
  check (i >= 116 && i <= 2000 - 64 && (i - 116) % 10 == 0);
  f6<-10> (&a[10], &a[110]);
  check (i >= 116 && i <= 2000 - 64 && (i - 116) % 10 == 0);
  f7<6> (I<int> (), &a[12], &a[1800]);
  check (i >= 2 && i <= 1808 && (i - 2) % 6 == 0);
  f8<121> (J<int> (&a[14], &a[1803]));
  check (i >= 14 && i <= 1924 && (i & 1) == 0);
  f9<int, 7> (&a[33], &a[1967]);
  check (i >= 33 && i <= 1967 && (i - 33) % 7 == 0);
  f10<int, -7> (&a[1939], &a[17]);
  check (i >= 21 && i <= 1939 && (i - 21) % 7 == 0);
  f11<I<int> > (&a[16], &a[1981]);
  check (i >= 16 && i <= 1984 && (i - 16) % 3 == 0);
  f12<I<int> > (&a[1761], &a[37]);
  check (i > 37 && i <= 1761);
  K<5>::f13<I<int> > (&a[1], &a[1935]);
  check (i >= 1 && i <= 1936 && (i - 1) % 5 == 0);
  f9<long, 7> (&b[33], &b[1967]);
  check (i >= 33 && i <= 1967 && (i - 33) % 7 == 0);
  f10<long, -7> (&b[1939], &b[17]);
  check (i >= 21 && i <= 1939 && (i - 21) % 7 == 0);
  f11<I<long> > (&b[16], &b[1981]);
  check (i >= 16 && i <= 1984 && (i - 16) % 3 == 0);
  f12<I<long> > (&b[1761], &b[37]);
  check (i > 37 && i <= 1761);
  K<5>::f13<I<long> > (&b[1], &b[1935]);
  check (i >= 1 && i <= 1936 && (i - 1) % 5 == 0);
}
