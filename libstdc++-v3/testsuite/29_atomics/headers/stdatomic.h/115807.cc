// { dg-do compile { target c++23 } }
#include <stdatomic.h>
namespace other {
  namespace std {
    int atomic = 0;
  }
  _Atomic(long) a{};
}

#include <type_traits>

namespace non::std {
  static_assert( ::std::is_same_v<_Atomic(int), ::std::atomic<int>> );
}
