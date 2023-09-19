// { dg-do compile { target c++23 } }
// { dg-require-normal-namespace "" }
// { dg-add-options no_pch }

#include <expected>

#ifndef __cpp_lib_expected
# error "Feature-test macro for expected missing in <expected>"
#elif __cpp_lib_expected != 202211L
# error "Feature-test macro for expected has wrong value in <expected>"
#endif

namespace std
{
  template<class E> class unexpected;
  template<class E> class bad_expected_access;
  template<> class bad_expected_access<void>;
  struct unexpect_t;
  extern inline const unexpect_t unexpect;
  template<class T, class E> class expected;
  template<class T, class E> requires is_void_v<T> class expected<T, E>;
}
