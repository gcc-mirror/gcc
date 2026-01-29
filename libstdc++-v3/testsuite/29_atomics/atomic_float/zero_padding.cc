// { dg-do run { target c++20 } }
// { dg-require-effective-target libatomic_available }
// { dg-additional-options "[atomic_link_flags [get_multilibs]] -latomic" }

#include <atomic>
#include <cstring>
#include <testsuite_hooks.h>

long double zld(10.5);
constexpr std::atomic<long double> cld(10.5);
std::atomic<long double> gld(10.5);

template<typename T>
void test_floating(std::atomic<T>& g, const T& zp)
{
  T const d = T(7.5);
  T t;

  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( g.compare_exchange_strong(t, d) );

  static std::atomic<T> st(T(10.5));
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( st.compare_exchange_strong(t, d) );

  thread_local std::atomic<T> tl(T(10.5));
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( tl.compare_exchange_strong(t, d) );

  std::atomic<T> l(T(10.5));
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( l.compare_exchange_strong(t, d) );

  std::atomic<T>* h = new std::atomic<T>(T(10.5));
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( h->compare_exchange_strong(t, d) );
  delete h;

  constexpr std::atomic<T> cl(T(10.5));
}

int main()
{
  test_floating(gld, zld);
}
