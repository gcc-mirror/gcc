// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-xfail-if "not supported" { debug_mode } }

#include <vector>
#include <testsuite_hooks.h>

constexpr bool
test_data()
{
  std::vector<int> v;
  VERIFY( v.data() == nullptr );
  v.reserve(1);
  VERIFY( v.data() != nullptr );
  const std::vector<int> v2{1,3,5,9};
  VERIFY( v.data() != v2.data() );
  VERIFY( v2.data()[2] == 5 );

  v = v2;
  VERIFY( v.data() != v2.data() );
  VERIFY( v.data()[1] == 3 );

  return true;
}

static_assert(test_data());
