// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run }
// { dg-additional-options "-std=c++17" }
// { dg-require-effective-target tls_runtime }

#ifndef RANGE_FOR_EXT_TEMPS
#define RANGE_FOR_EXT_TEMPS (__cpp_range_based_for >= 202211L)
#endif

extern "C" void abort ();
void check (bool);

struct S
{
  S () { ++s; }
  S (const S &) { ++s; }
  ~S () { check (true); --s; }
  [[omp::decl (threadprivate)]] static int s;
};
int S::s;
S sv;
struct T
{
  T (const S &, const S &) { ++t; }
  T (const T &) { ++t; }
  ~T () { check (false); --t; }
  [[omp::decl (threadprivate)]] static int t;
};
int T::t;
T tv (sv, sv);
int a[4];
int c;

void
check (bool is_s)
{
  if (c)
    {
      if (is_s)
	{
	  if (T::t != (c == 1))
	    abort ();
	}
      else
	{
	  if (S::s != (c == 1 ? 0 : 2))
	    abort ();
	}
    }
}

template <typename T>
int *
begin (const T &)
{
  return &a[0];
}

template <typename T>
int *
end (const T &)
{
  return &a[4];
}

const S &
foo (const S &)
{
  return sv;
}

const T &
foo (const T &)
{
  return tv;
}

void
bar ()
{
  #pragma omp parallel num_threads (4)
  {
    if (S::s != 0)
      abort ();
    #pragma omp for
    for (auto x : S ())
      {
	if (S::s != 1)
	  abort ();
      }
    if (S::s != 0)
      abort ();
    #pragma omp for
    for (auto x : foo (S ()))
      {
	if (S::s != RANGE_FOR_EXT_TEMPS)
	  abort ();
      }
    if (S::s != 0)
      abort ();
    if (T::t != 0)
      abort ();
  }
  c = 1 + RANGE_FOR_EXT_TEMPS;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp for
    for (auto x : T (S (), S ()))
      {
	if (S::s != 2 * RANGE_FOR_EXT_TEMPS || T::t != 1)
	  abort ();
      }
    if (S::s != 0 || T::t != 0)
      abort ();
  }
  c = 2;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp for
    for (auto x : foo (T (S (), S ())))
      {
	if (S::s != 2 * RANGE_FOR_EXT_TEMPS
	    || T::t != RANGE_FOR_EXT_TEMPS)
	  abort ();
      }
    if (S::s != 0 || T::t != 0)
      abort ();
  }
  c = 0;
}

template <int N>
void
baz ()
{
  #pragma omp parallel num_threads (4)
  {
    if (S::s != 0)
      abort ();
    #pragma omp for
    for (auto x : S ())
      {
	if (S::s != 1)
	  abort ();
      }
    if (S::s != 0)
      abort ();
    #pragma omp for
    for (auto x : foo (S ()))
      {
	if (S::s != RANGE_FOR_EXT_TEMPS)
	  abort ();
      }
    if (S::s != 0)
      abort ();
    if (T::t != 0)
      abort ();
  }
  c = 1 + RANGE_FOR_EXT_TEMPS;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp for
    for (auto x : T (S (), S ()))
      {
	if (S::s != 2 * RANGE_FOR_EXT_TEMPS || T::t != 1)
	  abort ();
      }
    if (S::s != 0 || T::t != 0)
      abort ();
  }
  c = 2;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp for
    for (auto x : foo (T (S (), S ())))
      {
	if (S::s != 2 * RANGE_FOR_EXT_TEMPS
	    || T::t != RANGE_FOR_EXT_TEMPS)
	  abort ();
      }
    if (S::s != 0 || T::t != 0)
      abort ();
  }
  c = 0;
}

template <typename S, typename T>
void
qux ()
{
  #pragma omp parallel num_threads (4)
  {
    if (S::s != 0)
      abort ();
    #pragma omp for
    for (auto x : S ())
      {
	if (S::s != 1)
	  abort ();
      }
    if (S::s != 0)
      abort ();
    #pragma omp for
    for (auto x : foo (S ()))
      {
	if (S::s != RANGE_FOR_EXT_TEMPS)
	  abort ();
      }
    if (S::s != 0)
      abort ();
    if (T::t != 0)
      abort ();
  }
  c = 1 + RANGE_FOR_EXT_TEMPS;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp for
    for (auto x : T (S (), S ()))
      {
	if (S::s != 2 * RANGE_FOR_EXT_TEMPS || T::t != 1)
	  abort ();
      }
    if (S::s != 0 || T::t != 0)
      abort ();
  }
  c = 2;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp for
    for (auto x : foo (T (S (), S ())))
      {
	if (S::s != 2 * RANGE_FOR_EXT_TEMPS
	    || T::t != RANGE_FOR_EXT_TEMPS)
	  abort ();
      }
    if (S::s != 0 || T::t != 0)
      abort ();
  }
  c = 0;
}

int
main ()
{
  S::s--;
  T::t--;
  bar ();
  baz <0> ();
  qux <S, T> ();
}
