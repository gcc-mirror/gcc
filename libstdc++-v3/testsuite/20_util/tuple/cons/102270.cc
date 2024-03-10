// { dg-do compile { target c++20 } }

#include <tuple>

// PR libstdc++/102270 - std::tuple<>::swap missing constexpr specifier

constexpr bool swap_empty_tuple()
{
  std::tuple<> t, u;
  t.swap(u);
  return true;
}
static_assert( swap_empty_tuple() );

#include <testsuite_allocator.h>

constexpr bool construct_using_allocator()
{
  using Alloc = __gnu_test::SimpleAllocator<int>;

  Alloc a;
  const int i = 0;

  struct X0a {
    using allocator_type = Alloc;
    /* not constexpr */ X0a() { }
    constexpr X0a(allocator_type) { }
  };
  std::tuple<X0a> t0a(std::allocator_arg, a);
  std::tuple<X0a, X0a> t00a(std::allocator_arg, a);

  struct X0b {
    using allocator_type = Alloc;
    /* not constexpr */ X0b() { }
    constexpr X0b(std::allocator_arg_t, allocator_type) { }
  };
  std::tuple<X0b> t0b(std::allocator_arg, a);
  std::tuple<X0b, X0b> t00b(std::allocator_arg, a);

  struct X1a {
    using allocator_type = Alloc;
    /* not constexpr */ X1a(int) { }
    constexpr X1a(int, allocator_type) { }
  };
  std::tuple<X1a> t1a(std::allocator_arg, a, 1);
  std::tuple<X1a, X1a> t11a(std::allocator_arg, a, 1, i);

  struct X1b {
    using allocator_type = Alloc;
    /* not constexpr */ X1b(int) { }
    constexpr X1b(std::allocator_arg_t, allocator_type, int) { }
  };
  std::tuple<X1b> t1b(std::allocator_arg, a, 1);
  std::tuple<X1b, X1b> t11b(std::allocator_arg, a, 1, i);

  std::tuple<X1a, X1a, X1b, X1b> t1a1b(std::allocator_arg, a, 1, i, 1, i);

  const int c = 0;
  std::tuple<int, int> tii(std::allocator_arg, a, c, c);

  return true;
}
static_assert( construct_using_allocator() );
