// { dg-do run }

typedef __PTRDIFF_TYPE__ ptrdiff_t;

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

struct R { R () {}; ~R () {}; I<int> r; };
struct T { T () {}; virtual ~T () {}; I<int> t; };
struct A : public R, virtual public T { A () {} I<int> a; void m1 (const I<int> &, const I<int> &); };
template <typename Q>
struct U { U () {}; virtual ~U () {}; Q t; };
template <typename Q>
struct B : public R, virtual public U<Q> { B () {} Q a; void m2 (const Q &, const Q &, const I<int> &, const I<int> &); };

int d[64];

void
A::m1 (const I<int> &x, const I<int> &y)
{
  int w = 0;
  #pragma omp parallel for private (a) reduction(|:w)
  for (a = x; A::a < y - 33; a += 2)
    w |= (1 << *A::a);
  if (w != 0x55555555)
    __builtin_abort ();
  #pragma omp parallel for lastprivate (t)
  for (T::t = x; t < y - 32; t += 3)
    d[*T::t + 2] |= 1;
  if (*T::t != 33)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 1 : 0))
      __builtin_abort ();
  w = 0;
  #pragma omp parallel for reduction(|:w)
  for (a = x; A::a < y - 33; a += 2)
    w |= (1 << *A::a);
  if (w != 0x55555555)
    __builtin_abort ();
  #pragma omp taskloop
  for (R::r = x; r < y - 32; R::r += 2)
    d[*r + 8] |= 2;
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 1 : 0)
		 | ((i >= 8 && i < 32 + 8 && (i & 1) == 0) ? 2 : 0)))
      __builtin_abort ();
  #pragma omp taskloop collapse(2)
  for (T::t = x; t < y - 57; t += 2)
    for (a = x; A::a < y - 56; a++)
      d[((*t << 2) | *a) + 3] |= 4;
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 1 : 0)
		 | ((i >= 8 && i < 32 + 8 && (i & 1) == 0) ? 2 : 0)
		 | ((i >= 3 && i < 32 + 3) ? 4 : 0)))
      __builtin_abort ();
}

template <typename Q>
void
B<Q>::m2 (const Q &u, const Q &v, const I<int> &x, const I<int> &y)
{
  int w = 0;
  #pragma omp parallel for private (a) reduction(|:w)
  for (a = u; B::a < v - 33; a += 2)
    w |= (1 << *B::a);
  if (w != 0x55555555)
    __builtin_abort ();
  #pragma omp parallel for lastprivate (U<Q>::t)
  for (U<Q>::t = u; U<Q>::t < v - 32; U<Q>::t += 3)
    d[*U<Q>::t + 2] |= 1;
  if (*U<Q>::t != 33)
    __builtin_abort ();
  for (int i = 0; i < 64; i++)
    if (d[i] != ((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 1 : 0))
      __builtin_abort ();
  w = 0;
  #pragma omp parallel for reduction(|:w)
  for (a = u; B::a < v - 33; a += 2)
    w |= (1 << *B::a);
  if (w != 0x55555555)
    __builtin_abort ();
  #pragma omp taskloop
  for (R::r = x; r < y - 32; R::r += 2)
    d[*r + 8] |= 2;
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 1 : 0)
		 | ((i >= 8 && i < 32 + 8 && (i & 1) == 0) ? 2 : 0)))
      __builtin_abort ();
  #pragma omp taskloop collapse(2)
  for (U<Q>::t = u; U<Q>::t < v - 57; U<Q>::t += 2)
    for (a = u; B::a < v - 56; a++)
      d[((*U<Q>::t << 2) | *a) + 3] |= 4;
  for (int i = 0; i < 64; i++)
    if (d[i] != (((i >= 2 && i < 32 + 2 && (i - 2) % 3 == 0) ? 1 : 0)
		 | ((i >= 8 && i < 32 + 8 && (i & 1) == 0) ? 2 : 0)
		 | ((i >= 3 && i < 32 + 3) ? 4 : 0)))
      __builtin_abort ();
}

int
main ()
{
  A a;
  int b[128];
  for (int i = 0; i < 128; i++)
    b[i] = i - 32;
  a.m1 (&b[32], &b[96]);
  for (int i = 0; i < 64; i++)
    d[i] = 0;
  B<I<int> > c;
  c.m2 (&b[32], &b[96], &b[32], &b[96]);
  for (int i = 0; i < 64; i++)
    d[i] = 0;
  B<int *> d;
  d.m2 (&b[32], &b[96], &b[32], &b[96]);
}
