// { dg-do run { target c++23 } }

// Test for PR libstdc++/119721: tuple<> construction/assignment with array<T, 0>

#include <tuple>
#include <array>
#include <memory>
#include <testsuite_hooks.h>

constexpr void
test01()
{
  std::array<int, 0> a{};
  
  // Constructor from array<int, 0>
  std::tuple<> t1(a);
  std::tuple<> t2(std::move(a));
  
  // Assignment from array<int, 0>
  std::tuple<> t3;
  t3 = a;
  t3 = std::move(a);
  
  VERIFY( t1 == a );
  VERIFY( t2 == a );
  VERIFY( t3 == a );
}

constexpr void
test02()
{
  // Test with non-comparable element type
  struct NonComparable
  {
    void operator==(const NonComparable&) const = delete;
    void operator<=>(const NonComparable&) const = delete;
  };
  
  std::array<NonComparable, 0> a{};
  
  std::tuple<> t1(a);
  std::tuple<> t2(std::move(a));
  
  std::tuple<> t3;
  t3 = a;
  t3 = std::move(a);
  
  VERIFY( t1 == a );
}

constexpr void
test03()
{
  // Test assignment return type (non-const assignment)
  std::tuple<> t, u;
  std::tuple<>& r1 = (t = u);
  VERIFY( &r1 == &t );
  
  std::tuple<>& r2 = (t = {});
  VERIFY( &r2 == &t );
  
  std::array<int, 0> a{};
  std::tuple<>& r3 = (t = a);
  VERIFY( &r3 == &t );
}

constexpr void
test04()
{
  std::array<int, 0> a{};
  const std::tuple<> t1;

  // Const assignment from array
  std::tuple<> t2;
  const std::tuple<>& r1 = (t1 = t2);
  VERIFY( &r1 == &t1 );
  const std::tuple<>& r2 = (t1 = std::move(t2));
  VERIFY( &r2 == &t1 );

  const std::tuple<>& r3 = (t1 = {});
  VERIFY( &r3 == &t1 );

  // Const assignment from array
  const std::tuple<>& r4 = (t1 = a);
  VERIFY( &r4 == &t1 );
  const std::tuple<>& r5 = (t1 = std::move(a));
  VERIFY( &r5 == &t1 );
}

void
test05()
{
  std::array<int, 0> a{};
  std::allocator<int> alloc;
  
  // Allocator constructor from array
  std::tuple<> t1(std::allocator_arg, alloc, a);
  std::tuple<> t2(std::allocator_arg, alloc, std::move(a));
  
  VERIFY( t1 == a );
  VERIFY( t2 == a );
}

int main()
{
  auto test_all = [] {
    test01();
    test02();
    test03();
    test04();
    return true;
  };

  test_all();
  static_assert( test_all() );
  
  // allocator test is not constexpr
  test05(); 
  return 0;
}

