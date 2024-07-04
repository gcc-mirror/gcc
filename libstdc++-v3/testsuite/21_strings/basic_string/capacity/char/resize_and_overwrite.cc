// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <string>

#if __cplusplus >= 202302L
#ifndef __cpp_lib_string_resize_and_overwrite
#error "Feature test macro for resize_and_overwrite is missing in <string>"
#elif __cpp_lib_string_resize_and_overwrite != 202110L
# error "Feature test macro for resize_and_overwrite has wrong value in <string>"
#endif
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
    VERIFY( !std::strncmp(p, "monkey", 6) );
    std::strcpy(p, "Partridge among the pigeons");
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
  s.resize_and_overwrite(0, [](char*, int) { return 0; });
  VERIFY( s.empty() );
  VERIFY( s[0] == '\0' );
  VERIFY( s.data() == p );

  s = "short string";
  p = s.data();
  s.resize_and_overwrite(0, [](char*, int) { return 0; });
  VERIFY( s.empty() );
  VERIFY( s[0] == '\0' );
  VERIFY( s.data() == p );

  s = "a string that is long enough to not be a short string";
  p = s.data();
  s.resize_and_overwrite(0, [](char*, int) { return 0; });
  VERIFY( s.empty() );
  VERIFY( s[0] == '\0' );
  VERIFY( s.data() == p );
}

void
test03()
{
  using size_type = std::string::size_type;

  // Op must be invoked as std::move(op)(p, m) to use &&-qualified overload.
  // The value category and cv-qualification of the arguments is unspecified.
  struct Op
  {
    int operator()(char* p, size_type n) &&
    {
      // N.B. this intentionally writes to p[n], see below, and test06().
      std::memset(p, 'a', n+1);
      return n;
    }

    // Wrong ref-quals:
    int operator()(char*, size_type) & = delete;
    int operator()(char*, size_type) const & = delete;
    int operator()(char*, size_type) const && = delete;

    // Wrong types:
    int operator()(char* p, int n) && = delete;
    int operator()(char* p, long n) && = delete;
    int operator()(char* p, long long n) && = delete;

#ifdef _GLIBCXX_RELEASE
    // This overload was used prior to the resolution of LWG 3645:
    // resize_and_overwrite is overspecified to call its callback with lvalues.
    // A conforming implementation might still pass lvalues to the op, but the
    // libstdc++ implementation passes prvalues and so this overload should
    // not be selected now.
    int operator()(char*&, std::string::size_type&) && = delete; // (*)
#endif
  };
  std::string s;
  s.resize_and_overwrite(42, Op{});
  VERIFY( s.size() == 42 );
  VERIFY( s == std::string(42, 'a') );
  VERIFY( s[42] == '\0' ); // Callback wrote to p[n] but it's null now.
}

void
test04()
{
  std::string s = "this tests how the library copes with undefined behaviour";

  try {
    s.resize_and_overwrite(13, [](...) -> int { throw "undefined"; });
  } catch (...) {
    // The standard doesn't require this, but we leave the string empty:
    VERIFY( s.size() == 0 );
    VERIFY( s[0] == '\0' );
  }
}

constexpr bool
test05()
{
#if __cpp_lib_constexpr_string >= 201907
  std::string s;
  s.resize_and_overwrite(20, [](char* p, auto n) {
    *p = '!'; // direct assignment should be OK
    std::char_traits<char>::copy(p, "constexpr", 9);
    return 9;
  });
  VERIFY( s == "constexpr" );
#endif
  return true;
}

void
test06()
{
  std::string s = "0123456789";
  s.resize_and_overwrite(16, [](char* p, int n) {
    // Even though s.capacity() == 20 this callback still gets n==16:
    VERIFY( n == 16 );
    // Standard requires [p, p+n] to be a valid range, so this sets p[16]='6':
    std::char_traits<char>::copy(p + 10, "0123456798", 7);
    return n;
  });
  VERIFY( s.size() == 16 );
  VERIFY( s == "0123456789012345" );
  // Although p[16] was written to by the callback, it must be set to '\0'
  // to maintain the invariant that the string is null-terminated:
  VERIFY( s[16] == '\0' );

  s.resize_and_overwrite(4, [](char* p, int n) {
    VERIFY( n == 4 );
    std::char_traits<char>::copy(p, "abcde", 5); // Writes to p[n].
    return n;
  });
  VERIFY( s.size() == 4 );
  VERIFY( s[4] == '\0' );

  std::string short_string;
  // For the SSO string this won't need to allocate anything,
  // but all the same checks should pass.
  short_string.resize_and_overwrite(4, [](char* p, int n) {
    VERIFY( n == 4 );
    std::char_traits<char>::copy(p, "abcde", 5); // Writes to p[n].
    return n;
  });
  VERIFY( short_string.size() == 4 );
  VERIFY( short_string[4] == '\0' );

}

void
test07()
{
#if __cpp_guaranteed_copy_elision
  // Non-copyable, non-movable type can be used as the callable.
  struct Op
  {
    Op() = default;
    Op(Op&&) = delete;
    Op& operator=(Op&&) = delete;
    int operator()(char* p, int n) && { return 0; }
  };
  std::string s;
  s.resize_and_overwrite(0, Op());
#endif
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  static_assert( test05() );
  test06();
  test07();
}
