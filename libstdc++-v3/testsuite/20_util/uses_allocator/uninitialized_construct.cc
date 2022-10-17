// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// TODO [!HOSTED]: avoidable std::allocator usage
// { dg-require-effective-target hosted }

#include <memory>

constexpr bool
test_pr104542()
{
  // PR libstdc++/104542 - missing constexpr
  std::allocator<int> a;
  int* p = a.allocate(1);
  int i = *std::uninitialized_construct_using_allocator<int>(p, a, 999);
  a.deallocate(p, 1);
  return i == 999;
}

static_assert( test_pr104542() );
