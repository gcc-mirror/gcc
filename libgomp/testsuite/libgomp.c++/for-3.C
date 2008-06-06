// { dg-do run }

#include <vector>
#include <cstdlib>

template <typename T>
class J
{
public:
  typedef typename std::vector<T>::const_iterator const_iterator;
  J(const const_iterator &x, const const_iterator &y) : b (x), e (y) {}
  const const_iterator &begin ();
  const const_iterator &end ();
private:
  const_iterator b, e;
};

template <typename T>
const typename std::vector<T>::const_iterator &J<T>::begin () { return b; }
template <typename T>
const typename std::vector<T>::const_iterator &J<T>::end () { return e; }

int results[2000];

template <typename T>
void
baz (T &i)
{
  if (*i < 0 || *i >= 2000)
    std::abort ();
  results[*i]++;
}

void
f1 (const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
#pragma omp parallel for
  for (std::vector<int>::const_iterator i = x; i <= y; i += 6)
    baz (i);
}

void
f2 (const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
  std::vector<int>::const_iterator i;
#pragma omp parallel for private(i)
  for (i = x; i < y - 1; i = 1 - 6 + 7 + i)
    baz (i);
}

template <typename T>
void
f3 (const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
#pragma omp parallel for schedule (dynamic, 6)
  for (std::vector<int>::const_iterator i = x; i <= y; i = i + 9 - 8)
    baz (i);
}

template <typename T>
void
f4 (const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
  std::vector<int>::const_iterator i;
#pragma omp parallel for lastprivate(i)
  for (i = x + 2000 - 64; i > y + 10; --i)
    baz (i);
}

void
f5 (const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
#pragma omp parallel for schedule (static, 10)
  for (std::vector<int>::const_iterator i = x + 2000 - 64; i > y + 10; i -= 10)
    baz (i);
}

template <int N>
void
f6 (const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
#pragma omp parallel for schedule (runtime)
  for (std::vector<int>::const_iterator i = x + 2000 - 64;
       i > y + 10; i = i - 12 + 2)
    {
      std::vector<int>::const_iterator j = i + N;
      baz (j);
    }
}

template <int N>
void
f7 (std::vector<int>::const_iterator i,
    const std::vector<int>::const_iterator &x,
    const std::vector<int>::const_iterator &y)
{
#pragma omp parallel for schedule (dynamic, 6)
  for (i = x - 10; i <= y + 10; i += N)
    baz (i);
}

template <int N>
void
f8 (J<int> j)
{
  std::vector<int>::const_iterator i;
#pragma omp parallel for schedule (dynamic, 40)
  for (i = j.begin (); i <= j.end () + N; i += 2)
    baz (i);
}

template <typename T, int N>
void
f9 (const typename std::vector<T>::const_iterator &x,
    const typename std::vector<T>::const_iterator &y)
{
#pragma omp parallel for schedule (static, 25)
  for (typename std::vector<T>::const_iterator i = x; i <= y; i = i + N)
    baz (i);
}

template <typename T, int N>
void
f10 (const typename std::vector<T>::const_iterator &x,
     const typename std::vector<T>::const_iterator &y)
{
  typename std::vector<T>::const_iterator i;
#pragma omp parallel for
  for (i = x; i > y; i = i + N)
    baz (i);
}

template <typename T>
void
f11 (const T &x, const T &y)
{
#pragma omp parallel
  {
#pragma omp for nowait schedule (static, 2)
    for (T i = x; i <= y; i += 3)
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
#pragma omp parallel for schedule (dynamic, 130)
  for (i = x; i > y; --i)
    baz (i);
}

template <int N>
struct K
{
  template <typename T>
  static void
  f13 (const T &x, const T &y)
  {
#pragma omp parallel for schedule (runtime)
    for (T i = x; i <= y + N; i += N)
      baz (i);
  }
};

#define check(expr) \
  for (int i = 0; i < 2000; i++)			\
    if (expr)						\
      {							\
	if (results[i] != 1)				\
	  std::abort ();				\
	results[i] = 0;					\
      }							\
    else if (results[i])				\
      std::abort ()

int
main ()
{
  std::vector<int> a(2000);
  std::vector<long> b(2000);
  for (int i = 0; i < 2000; i++)
    {
      a[i] = i;
      b[i] = i;
    }
  f1 (a.begin () + 10, a.begin () + 1990);
  check (i >= 10 && i <= 1990 && (i - 10) % 6 == 0);
  f2 (a.begin () + 0, a.begin () + 1999);
  check (i < 1998 && (i & 1) == 0);
  f3<char> (a.begin () + 20, a.begin () + 1837);
  check (i >= 20 && i <= 1837);
  f4<int> (a.begin () + 0, a.begin () + 30);
  check (i > 40 && i <= 2000 - 64);
  f5 (a.begin () + 0, a.begin () + 100);
  check (i >= 116 && i <= 2000 - 64 && (i - 116) % 10 == 0);
  f6<-10> (a.begin () + 10, a.begin () + 110);
  check (i >= 116 && i <= 2000 - 64 && (i - 116) % 10 == 0);
  f7<6> (std::vector<int>::const_iterator (), a.begin () + 12,
	 a.begin () + 1800);
  check (i >= 2 && i <= 1808 && (i - 2) % 6 == 0);
  f8<121> (J<int> (a.begin () + 14, a.begin () + 1803));
  check (i >= 14 && i <= 1924 && (i & 1) == 0);
  f9<int, 7> (a.begin () + 33, a.begin () + 1967);
  check (i >= 33 && i <= 1967 && (i - 33) % 7 == 0);
  f10<int, -7> (a.begin () + 1939, a.begin () + 17);
  check (i >= 21 && i <= 1939 && (i - 21) % 7 == 0);
  f11<std::vector<int>::const_iterator > (a.begin () + 16, a.begin () + 1981);
  check (i >= 16 && i <= 1984 && (i - 16) % 3 == 0);
  f12<std::vector<int>::const_iterator > (a.begin () + 1761, a.begin () + 37);
  check (i > 37 && i <= 1761);
  K<5>::f13<std::vector<int>::const_iterator > (a.begin () + 1,
						a.begin () + 1935);
  check (i >= 1 && i <= 1936 && (i - 1) % 5 == 0);
  f9<long, 7> (b.begin () + 33, b.begin () + 1967);
  check (i >= 33 && i <= 1967 && (i - 33) % 7 == 0);
  f10<long, -7> (b.begin () + 1939, b.begin () + 17);
  check (i >= 21 && i <= 1939 && (i - 21) % 7 == 0);
  f11<std::vector<long>::const_iterator > (b.begin () + 16, b.begin () + 1981);
  check (i >= 16 && i <= 1984 && (i - 16) % 3 == 0);
  f12<std::vector<long>::const_iterator > (b.begin () + 1761, b.begin () + 37);
  check (i > 37 && i <= 1761);
  K<5>::f13<std::vector<long>::const_iterator > (b.begin () + 1,
						 b.begin () + 1935);
  check (i >= 1 && i <= 1936 && (i - 1) % 5 == 0);
}
