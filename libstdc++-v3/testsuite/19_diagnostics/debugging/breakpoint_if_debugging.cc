// { dg-do run { target c++26 } }
// { dg-options "-lstdc++exp" }
// { dg-require-cpp-feature-test __cpp_lib_debugging }
#include <debugging>
#include <type_traits>

static_assert( noexcept(std::breakpoint_if_debugging()) );
static_assert( std::is_void_v<decltype(std::breakpoint_if_debugging())> );

int main()
{
  std::breakpoint_if_debugging();
}
