// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-add-options libatomic }

#include <atomic>

#include <testsuite_hooks.h>

struct S { char c; short s; };

void __attribute__((noinline,noipa))
fill_struct(S& s)
{ __builtin_memset(&s, 0xff, sizeof(S)); }

bool
compare_struct(const S& a, const S& b)
{ return __builtin_memcmp(&a, &b, sizeof(S)) == 0; }

int
main ()
{
  S s;
  S ss{ s };
  fill_struct(ss);
  ss.c = 'a';
  ss.s = 42;

  std::atomic_ref<S> as{ s };
  as.store(ss);
  auto ts = as.load();
  VERIFY( !compare_struct(ss, ts) ); // padding cleared on store
  as.exchange(ss);
  auto es = as.load();
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
