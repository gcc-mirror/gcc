// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <format>
#include <testsuite_hooks.h>

struct S { };

template<> struct std::formatter<S> : std::formatter<const char*> {
  template<class Out>
  auto format(S, std::basic_format_context<Out, char>& ctx) const {
    return formatter<const char*>::format("ess", ctx);
  }
};

struct T { };

template<> struct std::formatter<T> : std::formatter<const char*> {
  // This function only accepts std::format_context, not other contexts.
  auto format(T, std::format_context& ctx) const {
    return formatter<const char*>::format("tee", ctx);
  }
};

struct U { };

void
test_concept() // [format.formattable]
{
  static_assert( std::formattable<int, char> );
  static_assert( std::formattable<const int, char> );
  static_assert( std::formattable<int, wchar_t> );
  static_assert( std::formattable<const int, wchar_t> );
  static_assert( std::formattable<char, char> );
  static_assert( std::formattable<char*, char> );
  static_assert( std::formattable<wchar_t, wchar_t> );
  static_assert( std::formattable<wchar_t*, wchar_t> );
  static_assert( std::formattable<char, wchar_t> );
  static_assert( ! std::formattable<char*, wchar_t> );
  static_assert( ! std::formattable<wchar_t, char> );
  static_assert( ! std::formattable<wchar_t*, char> );
  static_assert( std::formattable<S, char> );
  static_assert( std::formattable<const S, char> );
  static_assert( ! std::formattable<S, wchar_t> ); // only formats as char
  static_assert( ! std::formattable<T, char> ); // formatter not generic
  static_assert( ! std::formattable<U, char> ); // no formatter
}

enum color { red, green, blue };
const char* color_names[] = { "red", "green", "blue" };

template<> struct std::formatter<color> : std::formatter<const char*> {
  auto format(color c, format_context& ctx) const {
    return formatter<const char*>::format(color_names[c], ctx);
  }
};

struct err {};

void
test_specializations() // [format.formatter.spec]
{
  std::string s0 = std::format("{}", 42); // OK, library-provided formatter
  VERIFY( s0 == "42" );

  // std::string s1 = std::format("{}", L"foo"); // error: disabled formatter
  using Fw = std::format_context::formatter_type<wchar_t>;
  static_assert( ! std::is_default_constructible_v<Fw> );
  static_assert( ! std::is_copy_constructible_v<Fw> );
  static_assert( ! std::is_move_constructible_v<Fw> );
  static_assert( ! std::is_copy_assignable_v<Fw> );
  static_assert( ! std::is_move_assignable_v<Fw> );

  std::string s2 = std::format("{}", red);  // OK, user-provided formatter
  VERIFY( s2 == "red" );

  // std::string s3 = std::format("{}", err{}); // error: disabled formatter
  using Ferr = std::format_context::formatter_type<err>;
  static_assert( ! std::is_default_constructible_v<Ferr> );
  static_assert( ! std::is_copy_constructible_v<Ferr> );
  static_assert( ! std::is_move_constructible_v<Ferr> );
  static_assert( ! std::is_copy_assignable_v<Ferr> );
  static_assert( ! std::is_move_assignable_v<Ferr> );
}

int main()
{
  test_specializations();
}
