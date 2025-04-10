// { dg-do run { target c++26 } }
// { dg-options "-lstdc++exp" }
// { dg-require-cpp-feature-test __cpp_lib_debugging }
#include <debugging>
#include <type_traits>
#include <testsuite_hooks.h>

static_assert( noexcept(std::is_debugger_present()) );
static_assert( std::is_same_v<decltype(std::is_debugger_present()), bool> );

int main()
{
  VERIFY( ! std::is_debugger_present() );
}
