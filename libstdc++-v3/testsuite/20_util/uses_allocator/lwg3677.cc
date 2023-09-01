// { dg-do run { target c++20 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_hooks.h>

struct UsesAlloc
{
  using allocator_type = std::allocator<int>;

  bool passed_alloc;

  UsesAlloc(int) : passed_alloc(false) { }

  UsesAlloc(int, std::allocator<int>) : passed_alloc(true) { }
};

using Pair = std::pair<UsesAlloc, int>;

void
test_const()
{
  std::allocator<int> a;
  int i = 0;
  auto p = std::make_obj_using_allocator<const Pair>(a, i, i);
  VERIFY( p.first.passed_alloc );
}

void
test_volatile()
{
  std::allocator<int> a;
  int i = 0;
  auto p = std::make_obj_using_allocator<volatile Pair>(a, i, i);
  VERIFY( p.first.passed_alloc );
}

void
test_const_volatile()
{
  std::allocator<int> a;
  int i = 0;
  auto p = std::make_obj_using_allocator<volatile Pair>(a, i, i);
  VERIFY( p.first.passed_alloc );
}

int main()
{
  test_const();
  test_volatile();
  test_const_volatile();
}
