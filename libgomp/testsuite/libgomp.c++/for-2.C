// { dg-do run }

extern "C" void abort ();

template <typename T>
class J
{
public:
  J(T x, T y) : b (x), e (y) {}
  T begin ();
  T end ();
private:
  T b, e;
};

template <typename T> T J<T>::begin () { return b; }
template <typename T> T J<T>::end () { return e; }

int results[2000];

void
baz (int i)
{
  if (i < 0 || i >= 2000)
    abort ();
  results[i]++;
}

void
f1 (int x, int y)
{
#pragma omp parallel for
  for (int i = x; i <= y; i += 6)
    baz (i);
}

void
f2 (int x, int y)
{
  int i;
#pragma omp parallel for private(i)
  for (i = x; i < y - 1; i = 1 - 6 + 7 + i)
    baz (i);
}

template <typename T>
void
f3 (int x, int y)
{
#pragma omp parallel for
  for (int i = x; i <= y; i = i + 9 - 8)
    baz (i);
}

template <typename T>
void
f4 (int x, int y)
{
  int i;
#pragma omp parallel for lastprivate(i)
  for (i = x + 2000 - 64; i > y + 10; --i)
    baz (i);
}

void
f5 (int x, int y)
{
#pragma omp parallel for
  for (int i = x + 2000 - 64; i > y + 10L; i -= 10L)
    baz (i);
}

template <int N>
void
f6 (int x, int y)
{
#pragma omp parallel for
  for (int i = x + 2000 - 64; i > y + 10L; i = i - 12 + 2L)
    baz (i + N);
}

template <long N>
void
f7 (int i, int x, int y)
{
#pragma omp parallel for
  for (i = x - 10; i <= y + 10; i += N)
    baz (i);
}

template <long N>
void
f8 (J<int> j)
{
  int i;
#pragma omp parallel for
  for (i = j.begin (); i <= j.end () + N; i += 2)
    baz (i);
}

template <typename T, long N>
void
f9 (T x, T y)
{
#pragma omp parallel for
  for (T i = x; i <= y; i = i + N)
    baz (i);
}

template <typename T, long N>
void
f10 (T x, T y)
{
  T i;
#pragma omp parallel for
  for (i = x; i > y; i = i + N)
    baz (i);
}

template <typename T>
void
f11 (T x, long y)
{
#pragma omp parallel
  {
#pragma omp for nowait
    for (T i = x; i <= y; i += 3L)
      baz (i);
#pragma omp single
    baz (y + 3);
  }
}

template <typename T>
void
f12 (T x, T y)
{
  T i;
#pragma omp parallel for
  for (i = x; i > y; --i)
    baz (i);
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
  f1 (10, 1990);
  check (i >= 10 && i <= 1990 && (i - 10) % 6 == 0);
  f2 (0, 1999);
  check (i < 1998 && (i & 1) == 0);
  f3<char> (20, 1837);
  check (i >= 20 && i <= 1837);
  f4<int> (0, 30);
  check (i > 40 && i <= 2000 - 64);
  f5 (0, 100);
  check (i >= 116 && i <= 2000 - 64 && (i - 116) % 10 == 0);
  f6<-10> (10, 110);
  check (i >= 116 && i <= 2000 - 64 && (i - 116) % 10 == 0);
  f7<6> (0, 12, 1800);
  check (i >= 2 && i <= 1808 && (i - 2) % 6 == 0);
  f8<121> (J<int> (14, 1803));
  check (i >= 14 && i <= 1924 && (i & 1) == 0);
  f9<int, 7> (33, 1967);
  check (i >= 33 && i <= 1967 && (i - 33) % 7 == 0);
  f10<int, -7> (1939, 17);
  check (i >= 21 && i <= 1939 && (i - 21) % 7 == 0);
  f11<int> (16, 1981);
  check (i >= 16 && i <= 1984 && (i - 16) % 3 == 0);
  f12<int> (1761, 37);
  check (i > 37 && i <= 1761);
}
