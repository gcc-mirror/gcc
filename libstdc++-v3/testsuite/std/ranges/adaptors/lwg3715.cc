// { dg-do run { target c++23 } }

// Verify LWG 3715 changes.

#include <ranges>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::istringstream ints("0 1 2 3 4");
  auto i = std::views::istream<int>(ints);
  auto r4 = std::views::counted(i.begin(), 4) | std::views::chunk(2);
  VERIFY( !r4.empty() );
}

void
test02()
{
  std::istringstream ints("0 1 2 3 4");
  auto i = std::views::istream<int>(ints);
  auto r0 = std::views::counted(i.begin(), 0) | std::views::chunk(2);
  VERIFY( r0.empty() );
}

int
main()
{
  test01();
  test02();
}
