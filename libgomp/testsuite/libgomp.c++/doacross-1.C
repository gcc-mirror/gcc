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

int results[2048];

template <typename T>
void
baz (I<T> &i, I<T> &j, I<T> &k, T &l)
{
  if (*i < 0 || *i >= 16)
    abort ();
  if (*j < 0 || *j >= 16)
    abort ();
  if (*k < 0 || *k >= 16)
    abort ();
  if (l < 0 || l >= 16)
    abort ();
  #pragma omp atomic
    results[512 * *i + 64 * *j + 8 * *k + l]++;
}

template <typename T>
void
baz (T &i, T &j, T &k, T &l)
{
  if (i < 0 || i >= 16)
    abort ();
  if (j < 0 || j >= 16)
    abort ();
  if (k < 0 || k >= 16)
    abort ();
  if (l < 0 || l >= 16)
    abort ();
  #pragma omp atomic
    results[512 * i + 64 * j + 8 * k + l]++;
}

void
f1 (const I<int> &a, const I<int> &b, const I<int> &c, const I<int> &d,
    const I<int> &e, const I<int> &f, int g, int h,
    I<int> &r1, I<int> &r2, I<int> &r3)
{
  I<int> i, j, k;
  int l;
#pragma omp parallel for ordered(4) lastprivate (i, j, k) schedule(static, 1)
  for (i = a; i <= b; i++)
    for (j = c; j < d; j++)
      for (k = e; k < f; k++)
	for (l = g; l < h; l++)
	  {
	    #pragma omp ordered depend(sink: i - 1, j, k + 1, l - 2)
	    baz (i, j, k, l);
	    if (i > a && k < f - 1 && l > g + 1)
	      {
		int m;
		#pragma omp atomic read
		m = results[512 * *(i - 1) + 64 * *j + 8 * *(k + 1) + l - 2];
		if (m == 0)
		  abort ();
	      }
	    #pragma omp ordered depend(source)
	  }
  r1 = i;
  r2 = j;
  r3 = k;
}

void
f2 (int a, int b, int c, int d, int e, int f, int g, int h, int &r1, int &r2, int &r3)
{
  int i, j, k, l;
#pragma omp parallel for collapse (1) ordered(4) lastprivate (i, j, k) schedule(static, 2)
  for (i = a; i <= b; i++)
    for (j = c; j < d; j++)
      for (k = e; k < f; k++)
	for (l = g; l < h; l++)
	  {
	    #pragma omp ordered depend(sink: i - 1, j, k + 1, l - 2)
	    baz (i, j, k, l);
	    if (i > a && k < f - 1 && l > g + 1)
	      {
		int m;
		#pragma omp atomic read
		m = results[512 * (i - 1) + 64 * j + 8 * (k + 1) + l - 2];
		if (m == 0)
		  abort ();
	      }
	    #pragma omp ordered depend(source)
	  }
  r1 = i;
  r2 = j;
  r3 = k;
}

void
f3 (const I<int> &a, const I<int> &b, const I<int> &c, const I<int> &d,
    const I<int> &e, const I<int> &f, int g, int h,
    I<int> &r1, I<int> &r2, I<int> &r3)
{
  I<int> i, j, k;
  int l;
#pragma omp parallel for collapse (2) ordered(4) lastprivate (i, j, k) schedule(static, 1)
  for (i = a; i <= b; i++)
    for (j = c; j < d; j++)
      for (k = e; k < f; k++)
	for (l = g; l < h; l++)
	  {
	    #pragma omp ordered depend(sink: i - 1, j, k + 1, l - 2)
	    baz (i, j, k, l);
	    if (i > a && k < f - 1 && l > g + 1)
	      {
		int m;
		#pragma omp atomic read
		m = results[512 * *(i - 1) + 64 * *j + 8 * *(k + 1) + l - 2];
		if (m == 0)
		  abort ();
	      }
	    #pragma omp ordered depend(source)
	  }
  r1 = i;
  r2 = j;
  r3 = k;
}

void
f4 (int a, int b, int c, int d, int e, int f, int g, int h, int &r1, int &r2, int &r3)
{
  int i, j, k, l;
#pragma omp parallel for collapse (2) ordered(4) lastprivate (i, j, k) schedule(static, 2)
  for (i = a; i <= b; i++)
    for (j = c; j < d; j++)
      for (k = e; k < f; k++)
	for (l = g; l < h; l++)
	  {
	    #pragma omp ordered depend(sink: i - 1, j, k + 1, l - 2)
	    baz (i, j, k, l);
	    if (i > a && k < f - 1 && l > g + 1)
	      {
		int m;
		#pragma omp atomic read
		m = results[512 * (i - 1) + 64 * j + 8 * (k + 1) + l - 2];
		if (m == 0)
		  abort ();
	      }
	    #pragma omp ordered depend(source)
	  }
  r1 = i;
  r2 = j;
  r3 = k;
}

#define check(expr) \
  for (int i = 0; i < 2048; i++)			\
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
  int a[16], s1, s2, s3;
  I<int> r1, r2, r3;
  for (int i = 0; i < 16; i++)
    a[i] = i;
  r1 = &a[15]; r2 = &a[15]; r3 = &a[15];
  f1 (&a[1], &a[3], &a[2], &a[5], &a[1], &a[3], 0, 5, r1, r2, r3);
  if (*r1 != 4 || *r2 != 5 || *r3 != 3)
    abort ();
  check ((i / 512) - 1U < 3U && ((i / 64) & 7) - 2U < 3U && ((i / 8) & 7) - 1U < 2U && (i & 7) < 5);
  r1 = &a[15]; r2 = &a[15]; r3 = &a[15];
  f1 (&a[1], &a[3], &a[1], &a[4], &a[1], &a[5], 1, 0, r1, r2, r3);
  if (*r1 != 4 || *r2 != 4 || *r3 != 5)
    abort ();
  r1 = &a[15]; r2 = &a[15]; r3 = &a[15];
  f1 (&a[1], &a[3], &a[1], &a[9], &a[7], &a[2], 0, 7, r1, r2, r3);
  if (*r1 != 4 || *r2 != 9 || *r3 != 7)
    abort ();
  s1 = 15; s2 = 15; s3 = 15;
  f2 (1, 3, 2, 5, 1, 3, 0, 5, s1, s2, s3);
  if (s1 != 4 || s2 != 5 || s3 != 3)
    abort ();
  check ((i / 512) - 1U < 3U && ((i / 64) & 7) - 2U < 3U && ((i / 8) & 7) - 1U < 2U && (i & 7) < 5);
  s1 = 15; s2 = 15; s3 = 15;
  f2 (1, 3, 1, 4, 1, 5, 1, 0, s1, s2, s3);
  if (s1 != 4 || s2 != 4 || s3 != 5)
    abort ();
  s1 = 15; s2 = 15; s3 = 15;
  f2 (1, 3, 1, 9, 7, 2, 0, 7, s1, s2, s3);
  if (s1 != 4 || s2 != 9 || s3 != 7)
    abort ();
  r1 = &a[15]; r2 = &a[15]; r3 = &a[15];
  f3 (&a[1], &a[3], &a[2], &a[5], &a[1], &a[3], 0, 5, r1, r2, r3);
  if (*r1 != 4 || *r2 != 5 || *r3 != 3)
    abort ();
  check ((i / 512) - 1U < 3U && ((i / 64) & 7) - 2U < 3U && ((i / 8) & 7) - 1U < 2U && (i & 7) < 5);
  r1 = &a[15]; r2 = &a[15]; r3 = &a[15];
  f3 (&a[1], &a[3], &a[1], &a[4], &a[1], &a[5], 1, 0, r1, r2, r3);
  if (*r1 != 4 || *r2 != 4 || *r3 != 5)
    abort ();
  r1 = &a[15]; r2 = &a[15]; r3 = &a[15];
  f3 (&a[1], &a[3], &a[1], &a[9], &a[7], &a[2], 0, 7, r1, r2, r3);
  if (*r1 != 4 || *r2 != 9 || *r3 != 7)
    abort ();
  s1 = 15; s2 = 15; s3 = 15;
  f4 (1, 3, 2, 5, 1, 3, 0, 5, s1, s2, s3);
  if (s1 != 4 || s2 != 5 || s3 != 3)
    abort ();
  check ((i / 512) - 1U < 3U && ((i / 64) & 7) - 2U < 3U && ((i / 8) & 7) - 1U < 2U && (i & 7) < 5);
  s1 = 15; s2 = 15; s3 = 15;
  f4 (1, 3, 1, 4, 1, 5, 1, 0, s1, s2, s3);
  if (s1 != 4 || s2 != 4 || s3 != 5)
    abort ();
  s1 = 15; s2 = 15; s3 = 15;
  f4 (1, 3, 1, 9, 7, 2, 0, 7, s1, s2, s3);
  if (s1 != 4 || s2 != 9 || s3 != 7)
    abort ();
  return 0;
}
