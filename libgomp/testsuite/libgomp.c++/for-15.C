// PR c++/86443
// { dg-do run }
// { dg-additional-options "-std=c++17" }

typedef __PTRDIFF_TYPE__ ptrdiff_t;
extern "C" void abort ();

#pragma omp declare target
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
baz (int i)
{
  if (i < 0 || i >= 2000)
    abort ();
  results[i]++;
}

void
f1 (J<int> j)
{
#pragma omp distribute parallel for default(none)
  for (I<int> i = j.begin (); i < j.end (); i += 3)
    baz (*i);
}

void
f2 (J<int> j)
{
  I<int> i;
#pragma omp distribute parallel for default(none)
  for (i = j.begin (); i < j.end (); ++i)
    baz (*i);
}

template <int N>
void
f3 (J<int> j)
{
#pragma omp distribute parallel for default(none)
  for (I<int> i = j.begin (); i < j.end (); i += 6)
    baz (*i);
}

template <int N>
void
f4 (J<int> j)
{
  I<int> i;
#pragma omp distribute parallel for default(none)
  for (i = j.begin (); i < j.end (); i += 9)
    baz (*i);
}

template <typename T>
void
f5 (J<T> j)
{
#pragma omp distribute parallel for default(none)
  for (I<T> i = j.begin (); i < j.end (); i += 4)
    baz (*i);
}

template <typename T>
void
f6 (J<T> j)
{
  I<T> i;
#pragma omp distribute parallel for default(none)
  for (i = j.begin (); i < j.end (); i += 7)
    baz (*i);
}

#pragma omp end declare target

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
  for (int i = 0; i < 2000; i++)
    a[i] = i;
  #pragma omp target data map (to: a)
  {
    #pragma omp target teams map (always, tofrom: results)
    {
      J<int> j (&a[75], &a[1945]);
      f1 (j);
    }
    check (i >= 75 && i < 1945 && (i - 75) % 3 == 0);
    #pragma omp target teams map (always, tofrom: results)
    {
      J<int> j (&a[63], &a[1949]);
      f2 (j);
    }
    check (i >= 63 && i < 1949);
    #pragma omp target teams map (always, tofrom: results)
    {
      J<int> j (&a[58], &a[1979]);
      f3 <2> (j);
    }
    check (i >= 58 && i < 1979 && (i - 58) % 6 == 0);
    #pragma omp target teams map (always, tofrom: results)
    {
      J<int> j (&a[59], &a[1981]);
      f4 <9> (j);
    }
    check (i >= 59 && i < 1981 && (i - 59) % 9 == 0);
    #pragma omp target teams map (always, tofrom: results)
    {
      J<int> j (&a[52], &a[1972]);
      f5 (j);
    }
    check (i >= 52 && i < 1972 && (i - 52) % 4 == 0);
    #pragma omp target teams map (always, tofrom: results)
    {
      J<int> j (&a[31], &a[1827]);
      f6 (j);
    }
    check (i >= 31 && i < 1827 && (i - 31) % 7 == 0);
  }
}
