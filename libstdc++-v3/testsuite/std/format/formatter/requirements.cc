// { dg-do run { target c++20 } }

#include <format>
#include <testsuite_hooks.h>

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
  using Fi = std::format_context::formatter_type<int>;
  static_assert( std::is_default_constructible_v<Fi> );
  static_assert( std::is_copy_constructible_v<Fi> );
  static_assert( std::is_move_constructible_v<Fi> );
  static_assert( std::is_copy_assignable_v<Fi> );
  static_assert( std::is_move_assignable_v<Fi> );

#ifdef _GLIBCXX_USE_CHAR8_T
  // std::string s0 = std::format("{}", u8'a'); // error: disabled formatter
  using Fc8 = std::format_context::formatter_type<char8_t>;
  static_assert( ! std::is_default_constructible_v<Fc8> );
  static_assert( ! std::is_copy_constructible_v<Fc8> );
  static_assert( ! std::is_move_constructible_v<Fc8> );
  static_assert( ! std::is_copy_assignable_v<Fc8> );
  static_assert( ! std::is_move_assignable_v<Fc8> );
#endif

  // std::string s1 = std::format("{}", L"foo"); // error: disabled formatter
  using Fw = std::format_context::formatter_type<wchar_t>;
  static_assert( ! std::is_default_constructible_v<Fw> );
  static_assert( ! std::is_copy_constructible_v<Fw> );
  static_assert( ! std::is_move_constructible_v<Fw> );
  static_assert( ! std::is_copy_assignable_v<Fw> );
  static_assert( ! std::is_move_assignable_v<Fw> );

  using Fic = std::format_context::formatter_type<const int>; // disabled
  static_assert( ! std::is_default_constructible_v<Fic> );
  static_assert( ! std::is_copy_constructible_v<Fic> );
  static_assert( ! std::is_move_constructible_v<Fic> );
  static_assert( ! std::is_copy_assignable_v<Fic> );
  static_assert( ! std::is_move_assignable_v<Fic> );

  std::string s2 = std::format("{}", red);  // OK, user-provided formatter
  VERIFY( s2 == "red" );
  using Fc = std::format_context::formatter_type<color>;
  static_assert( std::is_default_constructible_v<Fc> );
  static_assert( std::is_copy_constructible_v<Fc> );
  static_assert( std::is_move_constructible_v<Fc> );
  static_assert( std::is_copy_assignable_v<Fc> );
  static_assert( std::is_move_assignable_v<Fc> );

  // std::string s3 = std::format("{}", err{}); // error: disabled formatter
  using Ferr = std::format_context::formatter_type<err>;
  static_assert( ! std::is_default_constructible_v<Ferr> );
  static_assert( ! std::is_copy_constructible_v<Ferr> );
  static_assert( ! std::is_move_constructible_v<Ferr> );
  static_assert( ! std::is_copy_assignable_v<Ferr> );
  static_assert( ! std::is_move_assignable_v<Ferr> );

  // LWG 3833. Remove specialization
  // template<size_t N> struct formatter<const charT[N], charT>
  // Formatter is only expected to be instantiated with only cv-unqual types
  // and attempting to instantiate this specialization is ill-formed
  // using Farr = std::format_context::formatter_type<const char[1]>;
  // static_assert( ! std::is_default_constructible_v<Farr> );
  // static_assert( ! std::is_copy_constructible_v<Farr> );
  // static_assert( ! std::is_move_constructible_v<Farr> );
  // static_assert( ! std::is_copy_assignable_v<Farr> );
  // static_assert( ! std::is_move_assignable_v<Farr> );
}

int main()
{
  test_specializations();
}
