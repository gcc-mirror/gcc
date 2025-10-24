// { dg-do run { target c++11 } }
// { dg-additional-options "-faligned-new" { target c++14_down } }

#include <valarray>
#include <cstdint>
#include <testsuite_hooks.h>

struct alignas(64) Num
{
  Num()
  {
    VERIFY(reinterpret_cast<std::uintptr_t>(this) % alignof(*this) == 0);
  }

  double val{};
};

int main()
{
  std::valarray<Num> v(2);
  v.resize(4, {});
}
