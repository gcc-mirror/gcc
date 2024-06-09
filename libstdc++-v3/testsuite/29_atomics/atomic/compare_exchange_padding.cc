// { dg-do run { target c++20 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }
// { dg-additional-options "-fno-tree-sra" }

#include <atomic>
#include <cstring>

#include <testsuite_hooks.h>

struct S { char c; short s; };

void __attribute__((noinline,noipa))
fill_struct(S& s)
{ std::memset(&s, 0xff, sizeof(S)); }

bool
compare_struct(const S& a, const S& b)
{ return std::memcmp(&a, &b, sizeof(S)) == 0; }

int
main ()
{
  S s;
  fill_struct(s);
  s.c = 'a';
  s.s = 42;

  std::atomic<S> as{ s };
  auto ts = as.load(); // SRA might prevent copying of padding bits here.
  VERIFY( !compare_struct(s, ts) ); // padding cleared on construction
  as.exchange(s);
  auto es = as.load(); // SRA might prevent copying of padding bits here.
  VERIFY( compare_struct(ts, es) ); // padding cleared on exchange

  S n;
  fill_struct(n);
  n.c = 'b';
  n.s = 71;
  // padding cleared on compexchg
  VERIFY( as.compare_exchange_weak(s, n) );
  VERIFY( as.compare_exchange_strong(n, s) );
  return 0;
}
