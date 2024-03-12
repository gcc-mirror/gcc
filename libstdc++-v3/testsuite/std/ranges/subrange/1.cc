// { dg-do run { target c++20 } }

#include <ranges>

void
test01()
{
  // LWG 3470
  int a[3] = {1,2,3};
  int* b[3] = {&a[2], &a[0], &a[1]};
  auto c = std::ranges::subrange<const int*const*>(b);
}

int
main()
{
  test01();
}
