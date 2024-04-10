// { dg-do compile { target c++11 } }
// { dg-options "-Wno-unused-variable" }
// { dg-additional-options "-D_GLIBCXX_DEBUG" { target c++17_down } }

#include <tuple>
#include <utility>

#if __cplusplus >= 202002L
// For C++20 and later, constructors are constrained to disallow dangling.
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
  std::tuple<const int&, int> t1(1L, 2);
  // { dg-error "here" "" { target { c++17_down && hosted } } 33 }
  // { dg-error "use of deleted function" "" { target c++20 } 33 }

  std::tuple<int, const int&> t2(1, 2L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 37 }
  // { dg-error "use of deleted function" "" { target c++20 } 37 }

  std::tuple<const int&, const int&> t3(1L, 2L);
  // { dg-error "here" "" { target { c++17_down && hosted } } 41 }
  // { dg-error "use of deleted function" "" { target c++20 } 41 }

  std::tuple<const int&, const int&> t4(std::pair<long, int>{});
  // { dg-error "here" "" { target { c++17_down && hosted } } 45 }
  // { dg-error "use of deleted function" "" { target c++20 } 45 }

  std::pair<int, long> p;
  std::tuple<const int&, const int&> t5(p);
  // { dg-error "here" "" { target { c++17_down && hosted } } 50 }
  // { dg-error "use of deleted function" "" { target c++20 } 50 }
}

void
test_converting_ctors()
{
  std::tuple<long, long> t0;

  std::tuple<const int&, int> t1(t0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 60 }
  // { dg-error "use of deleted function" "" { target c++20 } 60 }

  std::tuple<int, const int&> t2(t0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 64 }
  // { dg-error "use of deleted function" "" { target c++20 } 64 }

  std::tuple<const int&, const int&> t3(t0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 68 }
  // { dg-error "use of deleted function" "" { target c++20 } 68 }

  std::tuple<const int&, int> t4(std::move(t0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 72 }
  // { dg-error "use of deleted function" "" { target c++20 } 72 }

  std::tuple<int, const int&> t5(std::move(t0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 76 }
  // { dg-error "use of deleted function" "" { target c++20 } 76 }

  std::tuple<const int&, const int&> t6(std::move(t0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 80 }
  // { dg-error "use of deleted function" "" { target c++20 } 80 }

  std::pair<long, long> p0;
  std::tuple<const int&, int> t7(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 85 }
  // { dg-error "use of deleted function" "" { target c++20 } 85 }

  std::tuple<int, const int&> t8(p0);
  // { dg-error "here" "" { target { c++17_down && hosted } } 89 }
  // { dg-error "use of deleted function" "" { target c++20 } 89 }

  std::tuple<const int&, int> t9(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 93 }
  // { dg-error "use of deleted function" "" { target c++20 } 93 }

  std::tuple<int, const int&> t10(std::move(p0));
  // { dg-error "here" "" { target { c++17_down && hosted } } 97 }
  // { dg-error "use of deleted function" "" { target c++20 } 97 }
}

// TODO: test allocator-extended ctors
// TODO: test 1-tuple or 3-tuple, not just 2-tuple

// { dg-error "static assert.* dangling reference" "" { target { c++17_down && hosted } } 0 }
