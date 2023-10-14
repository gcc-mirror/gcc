// { dg-do run { target c++20 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>

#include <testsuite_hooks.h>

struct S
{
  char c;
  alignas(2) short s;
};

void __attribute__((noinline,noipa))
set_padding(S& s, unsigned char x)
{ reinterpret_cast<unsigned char*>(&s)[1] = x; }

unsigned char __attribute__((noinline,noipa))
get_padding(S& s)
{ return reinterpret_cast<unsigned char*>(&s)[1]; }

void
test01()
{
  S s;
  S ss;
  ss.c = 'a';
  ss.s = 42;
  set_padding(ss, 0xff);

  {
    std::atomic_ref<S> as{ s };
    as.store(ss); // copy value bits, clear padding bits
  }
  VERIFY( get_padding(s) == 0 ); // padding was cleared on store

  ss.c = 'b';
  set_padding(ss, 0x11);
  VERIFY( get_padding(ss) == 0x11 );
  {
    std::atomic_ref<S> as{ s };
    as.exchange(ss); // copy value bits, clear padding bits
  }
  VERIFY( get_padding(s) == 0 ); // padding was cleared on store

  S exp = s;
  set_padding(exp, 0xaa);
  set_padding(s, 0xbb);
  S n;
  n.c = 'c';
  n.s = 71;
  set_padding(n, 0xcc);

  // padding cleared on cmpexchg
  {
    std::atomic_ref<S> as{ s };
    // This assumes no spurious failures, hopefully true without contention.
    VERIFY( as.compare_exchange_weak(exp, n) ); // padding in exp ignored
  }
  VERIFY( get_padding(s) == 0 ); // padding in n was not copied to s

  {
    std::atomic_ref<S> as{ s };
    VERIFY( as.compare_exchange_strong(n, exp) ); // padding in n ignored
  }
  VERIFY( get_padding(s) == 0 ); // padding in exp was not copied to s
}

int main()
{
  test01();
}
