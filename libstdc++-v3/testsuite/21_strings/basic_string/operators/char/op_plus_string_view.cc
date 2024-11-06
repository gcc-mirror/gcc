// { dg-do run { target c++26 } }

#include <string>
#include <type_traits>
#include <utility>

#include <testsuite_hooks.h>

#if !defined(__cpp_lib_string_view) || __cpp_lib_string_view < 202403L
#error "__cpp_lib_string_view should have been defined to 202403L or more"
#endif

static_assert(std::is_same_v<decltype(std::declval<std::string>() + std::declval<std::string_view>()), std::string>);
static_assert(std::is_same_v<decltype(std::declval<std::string &>() + std::declval<std::string_view>()), std::string>);
static_assert(std::is_same_v<decltype(std::declval<std::string_view>() + std::declval<std::string>()), std::string>);
static_assert(std::is_same_v<decltype(std::declval<std::string_view>() + std::declval<std::string &>()), std::string>);

struct convertible_to_string_view1
{
  constexpr operator std::string_view() const { return "convertible_to_sv1"; }
};

struct convertible_to_string_view2
{
  constexpr operator std::string_view()       { return "convertible_to_sv2"; }
};

struct convertible_to_string_view3
{
  constexpr operator std::string_view()       { return "convertible_to_sv3 non_const"; }
  constexpr operator std::string_view() const { return "convertible_to_sv3 const"; }
};

struct convertible_to_string_view_and_char_star
{
  constexpr operator std::string_view() const { return "convertible_to_sv_and_charstar1"; }
  constexpr operator const char *() const { return "convertible_to_sv_and_charstar2"; }
};

struct convertible_to_lots
{
  constexpr operator std::string_view() const { return "convertible_to_lots1"; }
  constexpr operator const char *() const { return "convertible_to_lots2"; }
  constexpr operator std::string() const { return "convertible_to_lots3"; }
};

#if __cpp_lib_constexpr_string >= 201907 // constexpr std::string
using namespace std::literals;
static_assert( "costa "s + "marbella"sv == "costa marbella"s );
static_assert( "costa "sv + "marbella"s == "costa marbella"s );
static_assert( "costa "s + convertible_to_string_view1{} == "costa convertible_to_sv1"s );
static_assert( "costa "s + convertible_to_string_view2{} == "costa convertible_to_sv2"s );
static_assert( "costa "s + convertible_to_string_view3{} == "costa convertible_to_sv3 non_const"s );
static_assert( "costa "s + convertible_to_string_view_and_char_star{} == "costa convertible_to_sv_and_charstar1"s );
static_assert( "costa "s + convertible_to_lots{} == "costa convertible_to_lots1"s );
#endif // __cpp_lib_constexpr_string

void
test01()
{
  std::string str_0("costa ");
  std::string str_1("marbella");

  std::string tmp;

  std::string_view str_0_view = str_0;
  std::string_view str_1_view = str_1;


  // string + string_view
  VERIFY( (str_0 + str_1_view) == "costa marbella" );
  VERIFY( (str_0 + std::as_const(str_1_view)) == "costa marbella" );
  VERIFY( (str_0 + std::string_view(str_1)) == "costa marbella" );
  VERIFY( (str_0_view + str_1) == "costa marbella" );
  VERIFY( (std::as_const(str_0_view) + str_1) == "costa marbella" );
  VERIFY( (std::string_view(str_0) + str_1) == "costa marbella" );
  tmp = str_0;
  VERIFY( (std::move(tmp) + str_1_view) == "costa marbella" );
  tmp = str_1;
  VERIFY( (str_0_view + std::move(tmp)) == "costa marbella" );


  // convertible to string_view
  convertible_to_string_view1 conv_string_view1;
  VERIFY( (str_0 + conv_string_view1) == "costa convertible_to_sv1" );
  VERIFY( (str_0 + std::as_const(conv_string_view1)) == "costa convertible_to_sv1" );
  VERIFY( (std::as_const(str_0) + conv_string_view1) == "costa convertible_to_sv1" );
  VERIFY( (std::as_const(str_0) + std::as_const(conv_string_view1)) == "costa convertible_to_sv1" );

  tmp = str_0;
  VERIFY( (std::move(tmp) + conv_string_view1) == "costa convertible_to_sv1" );
  tmp = str_1;
  VERIFY( (conv_string_view1 + std::move(tmp)) == "convertible_to_sv1marbella" );

  VERIFY( (conv_string_view1 + str_1) == "convertible_to_sv1marbella" );
  VERIFY( (conv_string_view1 + std::as_const(str_1)) == "convertible_to_sv1marbella" );
  VERIFY( (std::as_const(conv_string_view1) + str_1) == "convertible_to_sv1marbella" );
  VERIFY( (std::as_const(conv_string_view1) + std::as_const(str_1)) == "convertible_to_sv1marbella" );


  convertible_to_string_view2 conv_string_view2;
  VERIFY( (str_0 + conv_string_view2) == "costa convertible_to_sv2" );
  VERIFY( (std::as_const(str_0) + conv_string_view2) == "costa convertible_to_sv2" );
  tmp = str_0;
  VERIFY( (std::move(tmp) + conv_string_view2) == "costa convertible_to_sv2" );
  tmp = str_1;
  VERIFY( (conv_string_view2 + std::move(tmp)) == "convertible_to_sv2marbella" );


  convertible_to_string_view3 conv_string_view3;
  VERIFY( (str_0 + conv_string_view3) == "costa convertible_to_sv3 non_const" );
  VERIFY( (str_0 + std::as_const(conv_string_view3)) == "costa convertible_to_sv3 const" );
  VERIFY( (std::as_const(str_0) + conv_string_view3) == "costa convertible_to_sv3 non_const" );
  VERIFY( (std::as_const(str_0) + std::as_const(conv_string_view3)) == "costa convertible_to_sv3 const" );

  tmp = str_0;
  VERIFY( (std::move(tmp) + conv_string_view3) == "costa convertible_to_sv3 non_const" );
  tmp = str_1;
  VERIFY( (conv_string_view3 + std::move(tmp)) == "convertible_to_sv3 non_constmarbella" );

  VERIFY( (conv_string_view3 + str_1) == "convertible_to_sv3 non_constmarbella" );
  VERIFY( (conv_string_view3 + std::as_const(str_1)) == "convertible_to_sv3 non_constmarbella" );
  VERIFY( (std::as_const(conv_string_view3) + str_1) == "convertible_to_sv3 constmarbella" );
  VERIFY( (std::as_const(conv_string_view3) + std::as_const(str_1)) == "convertible_to_sv3 constmarbella" );


  convertible_to_string_view_and_char_star conv_sv_cs;
  VERIFY( (str_0 + conv_sv_cs) == "costa convertible_to_sv_and_charstar1" );
  VERIFY( (conv_sv_cs + str_1) == "convertible_to_sv_and_charstar1marbella" );


  convertible_to_lots conv_lots;
  VERIFY( (str_0 + conv_lots) == "costa convertible_to_lots1" );
  VERIFY( (conv_lots + str_1) == "convertible_to_lots1marbella" );


  // Check that we're not regressing common cases
  // string + string literal
  VERIFY( (str_0 + "marbella") == "costa marbella" );
  VERIFY( ("costa " + str_1) == "costa marbella" );

  tmp = str_0;
  VERIFY( (std::move(tmp) + "marbella") == "costa marbella" );
  tmp = str_1;
  VERIFY( ("costa " + std::move(tmp)) == "costa marbella" );


  // string + non-const char *
  VERIFY( (str_0 + str_1.data()) == "costa marbella" );
  VERIFY( (str_0.data() + str_1) == "costa marbella" );

  tmp = str_0;
  VERIFY( (std::move(tmp) + str_1.data()) == "costa marbella" );
  tmp = str_1;
  VERIFY( (str_0.data() + std::move(tmp)) == "costa marbella" );


  // string + const char *
  VERIFY( (str_0 + std::as_const(str_1).data()) == "costa marbella" );
  VERIFY( (std::as_const(str_0).data() + str_1) == "costa marbella" );

  tmp = str_0;
  VERIFY( (std::move(tmp) + std::as_const(str_1).data()) == "costa marbella" );
  tmp = str_1;
  VERIFY( (std::as_const(str_0).data() + std::move(tmp)) == "costa marbella" );
}

int main()
{
  test01();
}
