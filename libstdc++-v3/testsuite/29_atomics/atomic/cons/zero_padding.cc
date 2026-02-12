// { dg-do run { target c++11 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <cstring>
#include <testsuite_hooks.h>

struct TailPadding { int i; char c; };
TailPadding ztail{1, 2}; // zeroed-padding
constexpr std::atomic<TailPadding> ctail(TailPadding{1,2});
std::atomic<TailPadding> gtail(TailPadding{1,2});

struct MidPadding { char c; int x; };
MidPadding zmid{1, 2}; // zeroed-padding
constexpr std::atomic<MidPadding> cmid(MidPadding{1,2});
std::atomic<MidPadding> gmid(MidPadding{1,2});

struct BitPadding { int : 4; int i : 5; int : 4; int j : 5; int : 4;  };
BitPadding zbit{1, 2}; // zeroed-padding
constexpr std::atomic<BitPadding> cbit(BitPadding{1,2});
std::atomic<BitPadding> gbit(BitPadding{1,2});

struct Ctor
{
  Ctor() = default;

  constexpr Ctor(char pc, char pi)
    : c(pc), i(pi)
  {}

  char c;
  int i;
};

Ctor zctor{1, 2}; // zeroed-padding
constexpr std::atomic<Ctor> cctor(Ctor{1,2});
std::atomic<Ctor> gctor(Ctor{1,2});

template<typename T>
void test_struct(std::atomic<T>& g, const T& zp)
{
  T const d{3, 4};
  T t;

  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( g.compare_exchange_strong(t, d) );

  static std::atomic<T> st(T{1, 2});
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( st.compare_exchange_strong(t, d) );

  thread_local std::atomic<T> tl(T{1, 2});
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( tl.compare_exchange_strong(t, d) );

  std::atomic<T> l(T{1, 2});
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( l.compare_exchange_strong(t, d) );

  std::atomic<T>* h = new std::atomic<T>(T{1, 2});
  std::memcpy(&t, &zp, sizeof(T));
  VERIFY( h->compare_exchange_strong(t, d) );
  delete h;

  constexpr std::atomic<T> cl(T{1, 2});
}

int main()
{
  test_struct(gtail, ztail);
  test_struct(gmid, zmid);
  test_struct(gbit, zbit);
  test_struct(gctor, zctor);
}
