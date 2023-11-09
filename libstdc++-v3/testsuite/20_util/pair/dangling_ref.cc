// { dg-do compile { target c++11 } }
// { dg-options "-Wno-unused-variable" }
// { dg-additional-options "-D_GLIBCXX_DEBUG" { target c++17_down } }

#include <utility>

#if __cplusplus >= 202002L
// For C++20 and later, constructors are constrained to disallow dangling.
static_assert(!std::is_constructible_v<std::pair<const int&, int>, long, int>);
static_assert(!std::is_constructible_v<std::pair<int, const int&>, int, long>);
static_assert(!std::is_constructible_v<std::pair<const int&, int>,
				       std::pair<long, long>>);
static_assert(!std::is_constructible_v<std::pair<int, const int&>,
				       std::pair<long, long>>);
static_assert(!std::is_constructible_v<std::pair<const int&, int>,
				       const std::pair<long, long>&>);
static_assert(!std::is_constructible_v<std::pair<int, const int&>,
				       const std::pair<long, long>&>);
#endif

void
test_binary_ctors()
{
  std::pair<const int&, int> p1(1L, 2);
  // { dg-error "here" "" { target { c++17_down && hosted } } 24 }
  // { dg-error "use of deleted function" "" { target c++20 } 24 }

  std::pair<int, const int&> p2(1, 2L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 28 }
  // { dg-error "use of deleted function" "" { target c++20 } 28 }

  std::pair<const int&, const int&> p3(1L, 2L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 32 }
  // { dg-error "use of deleted function" "" { target c++20 } 32 }
}

void
test_converting_ctors()
{
  std::pair<long, long> p0;

  std::pair<const int&, int> p1(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 42 }
  // { dg-error "use of deleted function" "" { target c++20 } 42 }

  std::pair<int, const int&> p2(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 46 }
  // { dg-error "use of deleted function" "" { target c++20 } 46 }

  std::pair<const int&, const int&> p3(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 50 }
  // { dg-error "use of deleted function" "" { target c++20 } 50 }

  std::pair<const int&, int> p4(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 54 }
  // { dg-error "use of deleted function" "" { target c++20 } 54 }

  std::pair<int, const int&> p5(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 58 }
  // { dg-error "use of deleted function" "" { target c++20 } 58 }

  std::pair<const int&, const int&> p6(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 62 }
  // { dg-error "use of deleted function" "" { target c++20 } 62 }
}

// { dg-error "static assert.* dangling reference" "" { target { c++17_down && hosted } } 0 }
