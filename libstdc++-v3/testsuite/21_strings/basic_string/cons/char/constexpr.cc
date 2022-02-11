// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <string>

#ifndef __cpp_lib_constexpr_string
# error "Feature-test macro for constexpr std::string missing in <string>"
#elif __cpp_lib_constexpr_string != 201907L
# error "Feature-test macro for constexpr std::string has wrong value in <string>"
#endif

#include <testsuite_hooks.h>

using C = char;
using T = std::char_traits<C>;

template<typename T>
struct Alloc : std::allocator<T>
{
  using std::allocator<T>::allocator;

  constexpr explicit Alloc(int p) : personality(p) { }

  template<typename U>
    constexpr Alloc(const Alloc<U>& a) : personality(a.personality) { }

  int personality = 0;

  constexpr Alloc select_on_container_copy_construction() const
  { return Alloc(-1); }

  constexpr bool operator==(const Alloc& a) const noexcept
  { return personality == a.personality; }
};

constexpr bool
test_default_ctor()
{
  std::basic_string<C> s0;
  VERIFY( s0.empty() );

  std::basic_string<C> s1(std::allocator<C>{});
  VERIFY( s1.empty() );

  std::basic_string<C, T, Alloc<C>> s2;
  VERIFY( s2.empty() );

  std::basic_string<C, T, Alloc<C>> s3(Alloc<C>(3));
  VERIFY( s3.empty() );
  VERIFY( s3.get_allocator().personality == 3 );

  return true;
}

static_assert( test_default_ctor() );

constexpr bool
test_cstr()
{
  const C cs[] = "This has an embedded \0 null";
  const auto len = (sizeof(cs) - 1)/sizeof(C);

  std::basic_string<C> s1(cs);
  VERIFY( s1.length() == 21 );
  std::basic_string<C> s2(cs, std::allocator<C>{});
  VERIFY( s2 == s1 );

  std::basic_string<C> s3(cs, len);
  VERIFY( s3.length() == len );
  VERIFY( s3[len] == '\0' );
  std::basic_string<C> s4(cs, len, std::allocator<C>{});
  VERIFY( s4 == s3 );

  std::basic_string<C, T, Alloc<C>> s5(cs);
  VERIFY( s5 == std::basic_string_view<C>(s1) );

  std::basic_string<C, T, Alloc<C>> s6(cs, Alloc<C>(6));
  VERIFY( s6 == std::basic_string_view<C>(s1) );
  VERIFY( s6.get_allocator().personality == 6 );

  std::basic_string<C, T, Alloc<C>> s7(cs, len, Alloc<C>(7));
  VERIFY( s7 == std::basic_string_view<C>(s3) );
  VERIFY( s7.get_allocator().personality == 7 );

  return true;
}

static_assert( test_cstr() );

constexpr bool
test_copy()
{
  const std::basic_string<C> short_string = "sh";
  const std::basic_string<C> long_string = "string longer than the SSO buffer";

  std::basic_string<C> s1 = short_string;
  VERIFY( s1 == short_string );
  std::basic_string<C> s2(short_string, s1.get_allocator());
  VERIFY( s2 == short_string );

  std::basic_string<C> s3 = long_string;
  VERIFY( s3 == long_string );
  std::basic_string<C> s4(long_string, s1.get_allocator());
  VERIFY( s4 == long_string );

  std::basic_string<C, T, Alloc<C>> a_short_string = short_string.c_str();
  std::basic_string<C, T, Alloc<C>> a_long_string = long_string.c_str();

  std::basic_string<C, T, Alloc<C>> s5(a_short_string);
  VERIFY( s5 == a_short_string );
  std::basic_string<C, T, Alloc<C>> s6(a_short_string, s5.get_allocator());
  VERIFY( s6 == a_short_string );
  std::basic_string<C, T, Alloc<C>> s7(a_short_string, Alloc<C>(7));
  VERIFY( s7 == a_short_string );
  VERIFY( s7.get_allocator().personality == 7 );

  std::basic_string<C, T, Alloc<C>> s8 = a_long_string;
  VERIFY( s8 == a_long_string );
  std::basic_string<C, T, Alloc<C>> s9(a_long_string, s5.get_allocator());
  VERIFY( s9 == a_long_string );
  std::basic_string<C, T, Alloc<C>> s10(a_long_string, Alloc<C>(10));
  VERIFY( s10 == a_long_string );
  VERIFY( s10.get_allocator().personality == 10 );

  return true;
}

static_assert( test_copy() );

constexpr bool
test_move()
{
  const std::basic_string<C> short_string = "sh";
  const std::basic_string<C> long_string = "string longer than the SSO buffer";

  std::basic_string<C> s0 = short_string;

  std::basic_string<C> s1 = std::move(s0);
  VERIFY( s1 == short_string );
  std::basic_string<C> s2(std::move(s1), std::allocator<C>());
  VERIFY( s2 == short_string );

  s0 = long_string;
  std::basic_string<C> s3 = std::move(s0);
  VERIFY( s3 == long_string );
  std::basic_string<C> s4(std::move(s3), s1.get_allocator());
  VERIFY( s4 == long_string );

  std::basic_string<C, T, Alloc<C>> a_short_string = short_string.c_str();
  std::basic_string<C, T, Alloc<C>> a_long_string = long_string.c_str();

  auto sa0 = a_short_string;
  std::basic_string<C, T, Alloc<C>> s5 = std::move(sa0);
  VERIFY( s5 == a_short_string );
  std::basic_string<C, T, Alloc<C>> s6(std::move(s5), sa0.get_allocator());
  VERIFY( s6 == a_short_string );
  std::basic_string<C, T, Alloc<C>> s7(std::move(s6), Alloc<C>(7));
  VERIFY( s7 == a_short_string );
  VERIFY( s7.get_allocator().personality == 7 );

  sa0 = a_long_string;
  std::basic_string<C, T, Alloc<C>> s8 = std::move(sa0);
  VERIFY( s8 == a_long_string );
  std::basic_string<C, T, Alloc<C>> s9(std::move(s8), s5.get_allocator());
  VERIFY( s9 == a_long_string );
  std::basic_string<C, T, Alloc<C>> s10(std::move(s9), Alloc<C>(10));
  VERIFY( s10 == a_long_string );
  VERIFY( s10.get_allocator().personality == 10 );

  return true;
}

static_assert( test_move() );
