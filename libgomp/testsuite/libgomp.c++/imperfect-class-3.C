// { dg-do run }
// Test that class iterators and imperfectly-nested loops work together.
// This variant tests range for.

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef int T;
typedef int S;

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
  friend bool operator == (I &, I &);
  friend bool operator == (const I &, const I &);
  friend bool operator < (I &, I &);
  friend bool operator < (const I &, const I &);
  friend bool operator <= (I &, I &);
  friend bool operator <= (const I &, const I &);
  friend bool operator > (I &, I &);
  friend bool operator > (const I &, const I &);
  friend bool operator >= (I &, I &);
  friend bool operator >= (const I &, const I &);
  friend typename I::difference_type operator - (I &, I &);
  friend typename I::difference_type operator - (const I &, const I &);
  friend I operator + (typename I::difference_type , const I &);
private:
  T *p;
};
 I::I () : p (0) {}
 I::~I () { p = (T *) 0; }
 I::I (T *x) : p (x) {}
 I::I (const I &x) : p (x.p) {}
 T &I::operator * () { return *p; }
 T *I::operator -> () { return p; }
 T &I::operator [] (const difference_type &x) const { return p[x]; }
 I &I::operator = (const I &x) { p = x.p; return *this; }
 I &I::operator ++ () { ++p; return *this; }
 I I::operator ++ (int) { return I (p++); }
 I &I::operator -- () { --p; return *this; }
 I I::operator -- (int) { return I (p--); }
 I &I::operator += (const difference_type &x) { p += x; return *this; }
 I &I::operator -= (const difference_type &x) { p -= x; return *this; }
 I I::operator + (const difference_type &x) const { return I (p + x); }
 I I::operator - (const difference_type &x) const { return I (p - x); }
 bool operator == (I &x, I &y) { return x.p == y.p; }
 bool operator == (const I &x, const I &y) { return x.p == y.p; }
 bool operator != (I &x, I &y) { return !(x == y); }
 bool operator != (const I &x, const I &y) { return !(x == y); }
 bool operator < (I &x, I &y) { return x.p < y.p; }
 bool operator < (const I &x, const I &y) { return x.p < y.p; }
 bool operator <= (I &x, I &y) { return x.p <= y.p; }
 bool operator <= (const I &x, const I &y) { return x.p <= y.p; }
 bool operator > (I &x, I &y) { return x.p > y.p; }
 bool operator > (const I &x, const I &y) { return x.p > y.p; }
 bool operator >= (I &x, I &y) { return x.p >= y.p; }
 bool operator >= (const I &x, const I &y) { return x.p >= y.p; }
 typename I::difference_type operator - (I &x, I &y) { return x.p - y.p; }
 typename I::difference_type operator - (const I &x, const I &y) { return x.p - y.p; }
 I operator + (typename I::difference_type x, const I &y) { return I (x + y.p); }

class J
{
 public:
 J(const I &x, const I &y) : b (x), e (y) {}
 const I &begin ();
 const I &end ();
 private:
 I b, e;
};

const I &J::begin () { return b; }
const I &J::end () { return e; }

static int f1count[3], f2count[3];

#ifndef __cplusplus
extern void abort (void);
#else
extern "C" void abort (void);
#endif

void f1 (int depth)
{
  f1count[depth]++;
}

void f2 (int depth)
{
  f2count[depth]++;
}

void s1 (J a1, J a2, J a3)
{
#pragma omp for collapse(3)
  for (auto i : a1)
    {
      f1 (0);
      for (auto j : a2)
	{
	  f1 (1);
	  for (auto k : a3)
	    {
	      f1 (2);
	      f2 (2);
	    }
	  f2 (1);
	}
      f2 (0);
    }
}


int
main (void)
{

  int index[] = {0, 1, 2, 3, 4, 5};

  J x (&index[0], &index[3]);
  J y (&index[0], &index[4]);
  J z (&index[0], &index[5]);

  f1count[0] = 0;
  f1count[1] = 0;
  f1count[2] = 0;
  f2count[0] = 0;
  f2count[1] = 0;
  f2count[2] = 0;

  s1 (x, y, z);

  /* All intervening code at the same depth must be executed the same
     number of times. */
  if (f1count[0] != f2count[0]) abort ();
  if (f1count[1] != f2count[1]) abort ();
  if (f1count[2] != f2count[2]) abort ();

  /* Intervening code must be executed at least as many times as the loop
     that encloses it. */
  if (f1count[0] < 3) abort ();
  if (f1count[1] < 3 * 4) abort ();

  /* Intervening code must not be executed more times than the number
     of logical iterations. */
  if (f1count[0] > 3 * 4 * 5) abort ();
  if (f1count[1] > 3 * 4 * 5) abort ();

  /* Check that the innermost loop body is executed exactly the number
     of logical iterations expected. */
  if (f1count[2] != 3 * 4 * 5) abort ();
}
