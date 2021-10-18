// { dg-options "-std=gnu++23" }
// { dg-do run { target { c++23 && cxx11-abi } } }

#include <string>

#ifndef __cpp_lib_string_resize_and_overwrite
#error "Feature test macro for resize_and_overwrite is missing in <string>"
#elif __cpp_lib_string_resize_and_overwrite != 202110L
# error "Feature test macro for resize_and_overwrite has wrong value in <string>"
#endif


#include <cstring>
#include <testsuite_hooks.h>

// P1072R10 basic_string::resize_and_overwrite

void
test01()
{
  std::string s = "foo";
  s.resize_and_overwrite(99, [](char* p, int n) {
    VERIFY( n == 99 );
    VERIFY( !std::strncmp(p, "foo", 3) );
    std::strcpy(p, "monkey tennis");
    return 6;
  });
  VERIFY( s == "monkey" );
  VERIFY( s.size() == 6 );
  VERIFY( s.capacity() >= 99 );
  VERIFY( s[6] == '\0' );

  const auto str = s.data();

  s.resize_and_overwrite(50, [](char* p, int n) -> unsigned {
    VERIFY( n == 50 );
    VERIFY( !std::strncmp(p, "monkey", 3) );
    std::strcpy(p, "Partridge among the pidgeons");
    return 9;
  });
  VERIFY( s.data() == str ); // No reallocation
  VERIFY( s == "Partridge" );
  VERIFY( s[9] == '\0' );
}

void
test02()
{
  std::string s;
  auto p = s.data();
  s.resize_and_overwrite(0, [](auto...) { return 0; });
  VERIFY( s.empty() );
  VERIFY( s[0] == '\0' );
  VERIFY( s.data() == p );

  s = "short string";
  p = s.data();
  s.resize_and_overwrite(0, [](auto...) { return 0; });
  VERIFY( s.empty() );
  VERIFY( s[0] == '\0' );
  VERIFY( s.data() == p );

  s = "a string that is long enough to not be a short string";
  p = s.data();
  s.resize_and_overwrite(0, [](auto...) { return 0; });
  VERIFY( s.empty() );
  VERIFY( s[0] == '\0' );
  VERIFY( s.data() == p );
}

void
test03()
{
  struct Op
  {
    int operator()(char*, int) & = delete;
    int operator()(char*, int) const & = delete;
    int operator()(char* p, int n) && { std::memset(p, 'a', n+1); return n; }
    int operator()(char*, int) const && = delete;
  };
  std::string s;
  s.resize_and_overwrite(42, Op{});
  VERIFY( s.size() == 42 );
  VERIFY( s == std::string(42, 'a') );
  VERIFY( s[42] == '\0' );

  s.resize_and_overwrite(0, [](auto&& p, auto&& n) {
    static_assert( std::is_same_v<decltype(p), char*&> );
    static_assert( std::is_same_v<decltype(n), std::string::size_type&> );
    return 0;
  });
}

void
test04()
{
  std::string s = "this tests how the library copes with undefined behaviour";

  try {
    s.resize_and_overwrite(13, [](auto...) -> int { throw "undefined"; });
  } catch (...) {
    // The standard doesn't require this, but we leave the string empty:
    VERIFY( s.size() == 0 );
    VERIFY( s[0] == '\0' );
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
