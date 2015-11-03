// { dg-do run }

#include <string>
#include <cstdlib>

template <typename T>
class J
{
public:
  typedef typename std::basic_string<T>::iterator iterator;
  J(const iterator &x, const iterator &y) : b (x), e (y) {}
  const iterator &begin ();
  const iterator &end ();
private:
  iterator b, e;
};

template <typename T>
const typename std::basic_string<T>::iterator &J<T>::begin () { return b; }
template <typename T>
const typename std::basic_string<T>::iterator &J<T>::end () { return e; }

template <typename T>
void
baz (T &i)
{
  if (*i < L'a' || *i >= L'a' + 2000)
    std::abort ();
  (*i)++;
}

void
f1 (const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (std::basic_string<wchar_t>::iterator i = x; i <= y; i += 6)
    baz (i);
}

void
f2 (const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
  std::basic_string<wchar_t>::iterator i;
#pragma omp parallel
#pragma omp single
#pragma omp taskloop private(i)
  for (i = x; i < y - 1; i = 1 - 6 + 7 + i)
    baz (i);
}

template <typename T>
void
f3 (const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (std::basic_string<wchar_t>::iterator i = x; i <= y; i = i + 9 - 8)
    baz (i);
}

template <typename T>
void
f4 (const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
  std::basic_string<wchar_t>::iterator i;
#pragma omp parallel
#pragma omp single
#pragma omp taskloop lastprivate(i)
  for (i = x + 2000 - 64; i > y + 10; --i)
    baz (i);
}

void
f5 (const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (std::basic_string<wchar_t>::iterator i = x + 2000 - 64;
       i > y + 10; i -= 10)
    baz (i);
}

template <int N>
void
f6 (const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (std::basic_string<wchar_t>::iterator i = x + 2000 - 64;
       i > y + 10; i = i - 12 + 2)
    {
      std::basic_string<wchar_t>::iterator j = i + N;
      baz (j);
    }
}

template <int N>
void
f7 (std::basic_string<wchar_t>::iterator i,
    const std::basic_string<wchar_t>::iterator &x,
    const std::basic_string<wchar_t>::iterator &y)
{
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (i = x - 10; i <= y + 10; i += N)
    baz (i);
}

template <wchar_t N>
void
f8 (J<wchar_t> j)
{
  std::basic_string<wchar_t>::iterator i;
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (i = j.begin (); i <= j.end () + N; i += 2)
    baz (i);
}

template <typename T, int N>
void
f9 (const typename std::basic_string<T>::iterator &x,
    const typename std::basic_string<T>::iterator &y)
{
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (typename std::basic_string<T>::iterator i = x; i <= y; i = i + N)
    baz (i);
}

template <typename T, int N>
void
f10 (const typename std::basic_string<T>::iterator &x,
     const typename std::basic_string<T>::iterator &y)
{
  typename std::basic_string<T>::iterator i;
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
  for (i = x; i > y; i = i + N)
    baz (i);
}

template <typename T>
void
f11 (const T &x, const T &y)
{
#pragma omp parallel
  {
#pragma omp single nowait
#pragma omp taskloop nogroup
    for (T i = x; i <= y; i += 3)
      baz (i);
#pragma omp single nowait
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
#pragma omp parallel
#pragma omp single
#pragma omp taskloop private(i)
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
#pragma omp parallel
#pragma omp single
#pragma omp taskloop
    for (T i = x; i <= y + N; i += N)
      baz (i);
  }
};

#define check(expr) \
  for (int i = 0; i < 2000; i++)			\
    if (expr)						\
      {							\
	if (a[i] != L'a' + i + 1)			\
	  std::abort ();				\
	a[i] = L'a' + i;				\
      }							\
    else if (a[i] != L'a' + i)				\
      std::abort ()

int
main ()
{
  std::basic_string<wchar_t> a = L"";
  for (int i = 0; i < 2000; i++)
    a += L'a' + i;
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
  f7<6> (std::basic_string<wchar_t>::iterator (), a.begin () + 12,
	 a.begin () + 1800);
  check (i >= 2 && i <= 1808 && (i - 2) % 6 == 0);
  f8<121> (J<wchar_t> (a.begin () + 14, a.begin () + 1803));
  check (i >= 14 && i <= 1924 && (i & 1) == 0);
  f9<wchar_t, 7> (a.begin () + 33, a.begin () + 1967);
  check (i >= 33 && i <= 1967 && (i - 33) % 7 == 0);
  f10<wchar_t, -7> (a.begin () + 1939, a.begin () + 17);
  check (i >= 21 && i <= 1939 && (i - 21) % 7 == 0);
  f11<std::basic_string<wchar_t>::iterator > (a.begin () + 16,
					      a.begin () + 1981);
  check (i >= 16 && i <= 1984 && (i - 16) % 3 == 0);
  f12<std::basic_string<wchar_t>::iterator > (a.begin () + 1761,
					      a.begin () + 37);
  check (i > 37 && i <= 1761);
  K<5>::f13<std::basic_string<wchar_t>::iterator > (a.begin () + 1,
						    a.begin () + 1935);
  check (i >= 1 && i <= 1936 && (i - 1) % 5 == 0);
}
