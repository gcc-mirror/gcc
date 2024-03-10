// { dg-do compile { target c++23 } }

#include <format>

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
