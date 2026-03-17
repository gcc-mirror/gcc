// { dg-do run { target c++11 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <cstring>
#include <cstdint>

#include <testsuite_hooks.h>

struct S
{
    char c;
    alignas(2) std::int16_t s;

    bool operator==(const S& rhs) const { return c == rhs.c && s == rhs.s; }
};

void __attribute__((noinline,noipa))
fill_struct(S& s)
{ std::memset(&s, 0xff, sizeof(S)); }

bool __attribute__((noinline,noipa))
padding_is_zero(const S& s)
{
  unsigned char bytes[sizeof(S)];
  std::memcpy(bytes, &s, sizeof(S));
  return bytes[1] == 0;
}

bool
padding_is_zero(std::atomic<S>& a)
{
  const S dummy{};

  // a.load() does not necessarily preserve padding bits, because the
  // temporary returned from a.load() might get SRA'd so that the two
  // data members are copied in separate registers and padding bits are
  // not copied.
  S s = a.load();

  // So instead we use compare-exchange with an incorrect 'expected' value,
  s.c = ~s.c;
  // so that the compare will fail and return the value (including padding):
  a.compare_exchange_weak(s, dummy);

  // Now we can inspect the padding bits to check they are zero:
  return padding_is_zero(s);
}

int
main ()
{
  S s;
  fill_struct(s);
  s.c = 'a';
  s.s = 42;

  std::atomic<S> as{ s };
  VERIFY( as.load() == s );      // members are set correctly
  VERIFY( padding_is_zero(as) ); // but padding was cleared on construction

  ++s.s;
  as.exchange(s);
  VERIFY( as.load() == s );      // members are set correctly
  VERIFY( padding_is_zero(as) ); // but padding was cleared on construction

  S n;
  fill_struct(n);
  n.c = 'b';
  n.s = 71;
  // padding in S ignored for compare, and padding in N not stored into AS:
  VERIFY( as.compare_exchange_strong(s, n) );
  VERIFY( as.load() == n );      // members are set correctly
  VERIFY( padding_is_zero(as) ); // but padding was cleared on compare exchange

  S w;
  fill_struct(w);
  w.c = 'c';
  w.s = 100;
  // Weak compare exchange can fail spuriously, so loop a few times.
  int count = 10;
  do
  {
    VERIFY( --count ); // Should not keep failing indefinitely.
    // padding in N ignored for compare, and padding in W not stored into AS:
  } while ( ! as.compare_exchange_weak(n, w) );
  auto ws = as.load(); // SRA might prevent copying of padding bits here.
  VERIFY( as.load() == w );      // members are set correctly
  VERIFY( padding_is_zero(as) ); // but padding was cleared on compare exchange
}
