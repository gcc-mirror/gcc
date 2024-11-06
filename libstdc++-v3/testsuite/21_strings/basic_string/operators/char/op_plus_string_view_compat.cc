// { dg-do compile { target c++17 } }

#include <string>
#include <type_traits>
#include <utility>

#include <testsuite_hooks.h>

// "legacy" string view and operators
template <typename CharT>
struct basic_my_string_view
{
  std::basic_string_view<CharT> view;
};

using my_string_view = basic_my_string_view<char>;

template <class T>
struct my_type_identity { using type = T; };
template <class T>
using my_type_identity_t = typename my_type_identity<T>::type;

template <typename CharT, typename T, typename A>
std::string operator+(const std::basic_string<CharT, T, A> &s, basic_my_string_view<CharT> msv)
{
  std::string result = s;
  result += msv.view;
  result += " using my_string_view";
  return result;
}

template <typename CharT, typename T, typename A>
std::string operator+(const std::basic_string<CharT, T, A> &s, my_type_identity_t<basic_my_string_view<CharT>> msv)
{
  std::string result = s;
  result += msv.view;
  result += " using my_string_view";
  return result;
}


struct buffer
{
  std::string buf;

  // "legacy"
  operator my_string_view() const { return my_string_view{buf}; }
  // "modern"
  operator std::string_view() const { return std::string_view{buf}; }
};

int
main()
{
  std::string s = "costa ";
  buffer b{"marbella"};

  // This should be ambiguous in C++26 due to new operator+ overloads
  // between std::string and objects convertible to std::string_view
  // (P2591R5)
  std::string result = s + b; // { dg-error "ambiguous" "operator+(string,string_view) should make this ambiguous" { target c++26 } }
  VERIFY( result == "costa marbella using my_string_view" );
}
