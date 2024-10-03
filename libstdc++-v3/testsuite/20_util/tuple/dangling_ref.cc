// { dg-do compile { target c++11 } }
// { dg-options "-Wno-unused-variable" }
// { dg-additional-options "-D_GLIBCXX_DEBUG" { target c++17_down } }
// { dg-skip-if "cannot mix with DEBUG" { *-*-* } { "-D_GLIBCXX_PARALLEL" } }
#include <tuple>
#include <utility>

#if __cplusplus >= 202002L
// For C++20 and later, constructors are constrained to disallow dangling.
static_assert(!std::is_constructible_v<std::tuple<const int&>, long>);
static_assert(!std::is_constructible_v<std::tuple<const int&, int>, long, int>);
static_assert(!std::is_constructible_v<std::tuple<int, const int&>, int, long>);
static_assert(!std::is_constructible_v<std::tuple<const int&, int>,
				       std::tuple<long, long>>);
static_assert(!std::is_constructible_v<std::tuple<int, const int&>,
				       std::tuple<long, long>>);
static_assert(!std::is_constructible_v<std::tuple<const int&, int>,
				       const std::tuple<long, long>&>);
static_assert(!std::is_constructible_v<std::tuple<int, const int&>,
				       const std::tuple<long, long>&>);
static_assert(!std::is_constructible_v<std::tuple<const int&, int>,
				       std::pair<long, long>>);
static_assert(!std::is_constructible_v<std::tuple<int, const int&>,
				       std::pair<long, long>>);
static_assert(!std::is_constructible_v<std::tuple<const int&, int>,
				       const std::pair<long, long>&>);
static_assert(!std::is_constructible_v<std::tuple<int, const int&>,
				       const std::pair<long, long>&>);
#endif

void
test_ary_ctors()
{
  std::tuple<const int&> t1(1L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 34 }
  // { dg-error "use of deleted function" "" { target c++20 } 34 }

  std::tuple<const int&, int> t2(1L, 2);
  // { dg-error "here" "" { target { c++17_down && hosted } } 38 }
  // { dg-error "use of deleted function" "" { target c++20 } 38 }

  std::tuple<int, const int&> t3(1, 2L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 42 }
  // { dg-error "use of deleted function" "" { target c++20 } 42 }

  std::tuple<const int&, const int&> t4(1L, 2L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 46 }
  // { dg-error "use of deleted function" "" { target c++20 } 46 }

  std::tuple<const int&, const int&> t5(std::pair<long, int>{});
  // { dg-error "here" "" { target { c++17_down && hosted } } 50 }
  // { dg-error "use of deleted function" "" { target c++20 } 50 }

  std::pair<int, long> p;
  std::tuple<const int&, const int&> t6(p);
  // { dg-error "here" "" { target { c++17_down && hosted } } 55 }
  // { dg-error "use of deleted function" "" { target c++20 } 55 }

  std::tuple<const int&, int, int> t7(1L, 2, 3);
  // { dg-error "here" "" { target { c++17_down && hosted } } 59 }
  // { dg-error "use of deleted function" "" { target c++20 } 59 }
}

void
test_converting_ctors()
{
  std::tuple<long> t10;

  std::tuple<const int&> t11(t10);
  // { dg-error "here" "" { target { c++17_down && hosted } } 69 }
  // { dg-error "use of deleted function" "" { target c++20 } 69 }

  std::tuple<const int&> t12(std::move(t10));
  // { dg-error "here" "" { target { c++17_down && hosted } } 73 }
  // { dg-error "use of deleted function" "" { target c++20 } 73 }

  std::tuple<long, long> t20;

  std::tuple<const int&, int> t21(t20);
  // { dg-error "here" "" { target { c++17_down && hosted } } 79 }
  // { dg-error "use of deleted function" "" { target c++20 } 79 }

  std::tuple<int, const int&> t22(t20);
  // { dg-error "here" "" { target { c++17_down && hosted } } 83 }
  // { dg-error "use of deleted function" "" { target c++20 } 83 }

  std::tuple<const int&, const int&> t23(t20);
  // { dg-error "here" "" { target { c++17_down && hosted } } 87 }
  // { dg-error "use of deleted function" "" { target c++20 } 87 }

  std::tuple<const int&, int> t24(std::move(t20));
  // { dg-error "here" "" { target { c++17_down && hosted } } 91 }
  // { dg-error "use of deleted function" "" { target c++20 } 91 }

  std::tuple<int, const int&> t25(std::move(t20));
  // { dg-error "here" "" { target { c++17_down && hosted } } 95 }
  // { dg-error "use of deleted function" "" { target c++20 } 95 }

  std::tuple<const int&, const int&> t26(std::move(t20));
  // { dg-error "here" "" { target { c++17_down && hosted } } 99 }
  // { dg-error "use of deleted function" "" { target c++20 } 99 }

  std::pair<long, long> p0;
  std::tuple<const int&, int> t27(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 104 }
  // { dg-error "use of deleted function" "" { target c++20 } 104 }

  std::tuple<int, const int&> t28(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 108 }
  // { dg-error "use of deleted function" "" { target c++20 } 108 }

  std::tuple<const int&, int> t29(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 112 }
  // { dg-error "use of deleted function" "" { target c++20 } 112 }

  std::tuple<int, const int&> t210(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 116 }
  // { dg-error "use of deleted function" "" { target c++20 } 116 }
}

#include <memory>

void
test_allocator_extended_ctors()
{
  std::allocator<int> a;

  std::tuple<const int&> t1(std::allocator_arg, a, 1L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 128 }
  // { dg-error "use of deleted function" "" { target c++20 } 128 }

  std::tuple<const int&, int> t2(std::allocator_arg, a, 1L, 2);
  // { dg-error "here" "" { target { c++17_down && hosted } } 132 }
  // { dg-error "use of deleted function" "" { target c++20 } 132 }

  std::tuple<long> tl;

  std::tuple<const int&> t3(std::allocator_arg, a, tl);
  // { dg-error "here" "" { target { c++17_down && hosted } } 138 }
  // { dg-error "use of deleted function" "" { target c++20 } 138 }

  std::tuple<const int&> t4(std::allocator_arg, a, std::move(tl));
  // { dg-error "here" "" { target { c++17_down && hosted } } 142 }
  // { dg-error "use of deleted function" "" { target c++20 } 142 }

  std::tuple<long, long> tll;

  std::tuple<const int&, int> t5(std::allocator_arg, a, tll);
  // { dg-error "here" "" { target { c++17_down && hosted } } 148 }
  // { dg-error "use of deleted function" "" { target c++20 } 148 }

  std::tuple<const int&, int> t6(std::allocator_arg, a, std::move(tll));
  // { dg-error "here" "" { target { c++17_down && hosted } } 152 }
  // { dg-error "use of deleted function" "" { target c++20 } 152 }

  std::pair<long, long> pll;

  std::tuple<const int&, int> t7(std::allocator_arg, a, pll);
  // { dg-error "here" "" { target { c++17_down && hosted } } 158 }
  // { dg-error "use of deleted function" "" { target c++20 } 158 }

  std::tuple<const int&, int> t8(std::allocator_arg, a, std::move(pll));
  // { dg-error "here" "" { target { c++17_down && hosted } } 162 }
  // { dg-error "use of deleted function" "" { target c++20 } 162 }
}

// { dg-error "static assert.* dangling reference" "" { target { c++17_down && hosted } } 0 }
