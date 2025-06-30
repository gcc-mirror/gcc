// C++26 [bitset.cons]

// { dg-do run { target c++26 } }

#ifndef C
# define C char
# define L(s) s
#endif

#include <bitset>
#include <string>
#include <string_view>
#include <algorithm> // std::reverse, std::transform
#include <stdexcept>
#include <testsuite_hooks.h>

void test01()
{
  // template<_C,_T>
  // constexpr explicit
  // bitset(const basic_string_view<_C,_T>,
  //   size_type pos, size_type n, _C zero, _C one)
  try {
    std::basic_string_view<C> str(L("stuff smith sessions"));
    const std::size_t n = 128;
    std::bitset<n> bit(str, 5);
    VERIFY(false);
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  try {
    std::basic_string_view<C> str(L("010101000011"));
    const std::size_t n = 128;
    const auto sz = str.size();
    std::bitset<n> bit(str, 0);
    std::basic_string<C> str02;
    for (std::size_t i = 0; i < sz; ++i)
      str02 += (bit.test(i) ? C('1') : C('0'));
    std::reverse(str02.begin(), str02.end());
    VERIFY( str02 == str );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

  // substring<C> "010101000011"
  //              "10100001"
  try {
    std::basic_string_view<C> str(L("010101000011"));
    const std::size_t n = 128;
    const auto sz = 8;
    std::bitset<n> bit(str, 3, sz);
    std::basic_string<C> str02;
    for (std::size_t i = 0; i < sz; ++i)
      str02 += (bit.test(i) ? C('1') : C('0'));
    std::reverse(str02.begin(), str02.end());
    VERIFY( str02 == str.substr(3, sz) );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

  // "abababaaaabb", zero = 'a', one = 'b'
  try {
    std::basic_string_view<C> str(L("010101000011"));
    const std::size_t n = 128;
    const auto sz = str.size();
    std::basic_string<C> str02(str);
    std::transform(str02.cbegin(), str02.cend(), str02.begin(), [](auto c) {
      return (c - C('0')) + C('a');
    });
    std::basic_string_view<C> str03(str02);
    std::bitset<n> bit(str03, 0, 100, C('a'), C('b'));
    std::basic_string<C> str04;
    for (std::size_t i = 0; i < sz; ++i)
      str04 += (bit.test(i) ? C('1') : C('0'));
    std::reverse(str04.begin(), str04.end());
    VERIFY( str04 == str );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( false );
  }
  catch(...) {
    VERIFY( false );
  }

  // "aba0aba", zero = 'a', one = 'b', '0' appears
  try {
    const std::size_t n = 128;
    std::basic_string_view<C> str05(L("aba0aba"));
    std::bitset<n> bit(str05, 0, 100, C('a'), C('b'));
    VERIFY( false );
  }
  catch(std::invalid_argument& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }

  // pos > str.size()
  try {
    std::basic_string_view<C> str(L("010101000011"));
    const std::size_t n = 128;
    const auto sz = str.size();
    std::bitset<n> bit(str, sz + 1, 100);
    VERIFY( false );
  }
  catch(std::out_of_range& fail) {
    VERIFY( true );
  }
  catch(...) {
    VERIFY( false );
  }
}

int main()
{
  test01();
  return 0;
}
