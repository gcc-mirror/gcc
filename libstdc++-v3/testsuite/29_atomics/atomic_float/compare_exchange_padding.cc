// { dg-do run { target c++20 } }
// { dg-options "-O0" }
// { dg-require-effective-target libatomic_available }
// { dg-additional-options "[atomic_link_flags [get_multilibs]] -latomic" }

#include <atomic>
#include <cstring>
#include <limits>
#include <testsuite_hooks.h>

template<typename T>
void __attribute__((noinline,noipa))
fill_padding(T& f)
{
  T mask;
  std::memset(&mask, 0xff, sizeof(T));
  __builtin_clear_padding(&mask);
  unsigned char* ptr_f = (unsigned char*)&f;
  unsigned char* ptr_mask = (unsigned char*)&mask;
  for (unsigned i = 0; i < sizeof(T); i++)
  {
    if (ptr_mask[i] == 0x00)
    {
      ptr_f[i] = 0xff;
    }
  }
}

void
test01()
{
  // test for long double with padding (float80)
  if constexpr (std::numeric_limits<long double>::digits == 64)
  {
    long double f = 0.5f; // long double has padding bits on x86
    fill_padding(f);
    std::atomic<long double> as{ f }; // padding cleared on constructor
    long double t = 1.5;

    as.fetch_add(t);
    long double s = f + t;
    t = as.load();
    VERIFY(s == t); // padding ignored on comparison
    fill_padding(s);
    VERIFY(as.compare_exchange_weak(s, f)); // padding cleared on cmpexchg
    fill_padding(f);
    VERIFY(as.compare_exchange_strong(f, t)); // padding cleared on cmpexchg
  }
}

int main()
{
  test01();
}
