// { dg-do run }
// { dg-require-sharedlib "" }
// { dg-require-effective-target rtti }
// { dg-options "./testsuite_shared.so" }

#include <typeinfo>
#include <testsuite_hooks.h>

namespace __gnu_test
{
namespace
{
  struct S { };
  struct T { };
}

// Defined in testsuite_shared.so, referring to private type in that library
// with the same mangled name as __gnu_test::<anonymous>::S defined here.
extern const std::type_info& pr103240_private_S;
}

const std::type_info& private_S = __gnu_test::pr103240_private_S;
const std::type_info& local_S = typeid(__gnu_test::S);
const std::type_info& local_T = typeid(__gnu_test::T);

int main()
{
  VERIFY( local_S == local_S );
  VERIFY( ! local_S.before(local_S) );

  VERIFY( local_S != local_T );
  VERIFY( local_S.before(local_T) || local_T.before(local_S) );

  VERIFY( local_S != private_S );
  // PR libstdc++/103240
  VERIFY( local_S.before(private_S) || private_S.before(local_S) );
}
