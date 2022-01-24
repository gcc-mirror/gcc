// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <variant>

// P2231R1 Missing constexpr in std::optional and std::variant

#ifndef __cpp_lib_variant
#error "Feature test macro for variant is missing in <variant>"
#elif __cpp_lib_variant < 202106L
# error "Feature test macro for variant has wrong value for C++20 in <variant>"
#endif

#include <testsuite_hooks.h>


constexpr bool
test_assign()
{
  std::variant<int, double> v1(1);
  v1 = 2.5;
  VERIFY( std::get<double>(v1) == 2.5 );

  v1 = 99;
  VERIFY( std::get<int>(v1) == 99 );
  v1 = 999;
  VERIFY( std::get<int>(v1) == 999 );

  struct S // non-trivial special members
  {
    constexpr S(int i) : i(i) { }
    constexpr ~S() { }
    constexpr S(const S& s) : i(s.i) { }

    int i;
  };

  std::variant<int, S> v;
  v = S(123);
  VERIFY( std::get<1>(v).i == 123 );

  const S s(456);
  v = s;
  VERIFY( std::get<1>(v).i == 456 );

  v = 789;
  VERIFY( std::get<0>(v) == 789 );

  return true;
}

static_assert( test_assign() );

constexpr bool
test_emplace()
{
  struct S // non-trivial special members
  {
    constexpr S(std::initializer_list<int> l) : i(l.begin()[0]) { }
    constexpr S(std::initializer_list<int> l, int n) : i(l.begin()[n]) { }
    constexpr ~S() { }
    constexpr S(const S& s) : i(s.i) { }

    int i;
  };

  std::variant<int, double, S> v(1);

  // template<class T, class... Args> constexpr T& emplace(Args&&... args);
  v.emplace<double>(2.0);
  VERIFY( std::get<1>(v) == 2.0 );
  v.emplace<double>(2.5);
  VERIFY( std::get<1>(v) == 2.5 );
  v.emplace<int>(2.5);
  VERIFY( std::get<0>(v) == 2 );

  // template<class T, class U, class... Args>
  // constexpr T& emplace(initializer_list<U>, Args&&... args);
  v.emplace<S>({3, 2, 1});
  VERIFY( std::get<2>(v).i == 3 );
  v.emplace<S>({3, 2, 1}, 1);
  VERIFY( std::get<2>(v).i == 2 );

  // template<size_t I, class... Args>
  // constexpr variant_alternative_t<I, ...>& emplace(Args&&... args);
  v.emplace<1>(3.0);
  VERIFY( std::get<1>(v) == 3.0 );
  v.emplace<1>(0.5);
  VERIFY( std::get<1>(v) == 0.5 );
  v.emplace<0>(1.5);
  VERIFY( std::get<0>(v) == 1 );

  // template<size_t I, class U, class... Args>
  // constexpr variant_alternative_t<I, ...>&
  // emplace(initializer_list<U>, Args&&... args);
  v.emplace<2>({7, 8, 9});
  VERIFY( std::get<2>(v).i == 7 );
  v.emplace<2>({13, 12, 11}, 1);
  VERIFY( std::get<2>(v).i == 12 );

  return true;
}

static_assert( test_emplace() );

constexpr bool
test_swap()
{
  std::variant<int, double> v1(1), v2(2.5);
  v1.swap(v2);
  VERIFY( std::get<double>(v1) == 2.5 );
  VERIFY( std::get<int>(v2) == 1 );

  swap(v1, v2);
  VERIFY( std::get<int>(v1) == 1 );
  VERIFY( std::get<double>(v2) == 2.5 );

  struct S
  {
    constexpr S(int i) : i(i) { }
    constexpr S(S&& s) : i(s.i) { }
    constexpr S& operator=(S&& s) { i = s.i; s.i = -1; return *this; }

    int i;
  };

  std::variant<int, S> v3(3), v4(S(4));
  v3.swap(v4);
  VERIFY( std::get<S>(v3).i == 4 );
  VERIFY( std::get<int>(v4) == 3 );
  v3.swap(v4);
  VERIFY( std::get<int>(v3) == 3 );
  VERIFY( std::get<S>(v4).i == 4 );

  return true;
}

static_assert( test_swap() );
