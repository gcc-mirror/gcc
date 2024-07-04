// { dg-options "-Wno-deprecated-declarations" }
// { dg-do compile { target c++11 } }

// Copyright (C) 2008-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <functional>
#include <type_traits>

namespace __gnu_test
{

template<typename T> using void_t = void;

struct none;

#if __cplusplus <= 201703L
// For C++11/14/17 if the nested type is not found, require
// that the test used 'none' as the expected type.
template<typename U> using not_found = std::is_same<U, none>;
// A nested type needs to match the expected type.
template<typename U, typename V> using found = std::is_same<U, V>;
#else
// For C++20 the nested type should always be not found.
template<typename U> using not_found = std::true_type;
// Any nested type is bad.
template<typename U, typename V> using found = std::false_type;
#endif

template<typename T, typename U, typename = void>
struct check_result_type
: not_found<U>
{ };

// Matches when reference_wrapper<T>::result_type exists
template<typename T, typename U>
struct check_result_type<T, U, void_t<typename std::reference_wrapper<T>::result_type>>
: found<U, typename std::reference_wrapper<T>::result_type>
{ };

template<typename T, typename U, typename = void>
struct check_arg_type
: not_found<U>
{ };

// Matches when reference_wrapper<T>::argument_type exists
template<typename T, typename U>
struct check_arg_type<T, U, void_t<typename std::reference_wrapper<T>::argument_type>>
: found<U, typename std::reference_wrapper<T>::argument_type>
{ };

template<typename T, typename U, typename = void>
struct check_first_arg_type
: not_found<U>
{ };

// Matches when reference_wrapper<T>::first_argument_type exists
template<typename T, typename U>
struct check_first_arg_type<T, U, void_t<typename std::reference_wrapper<T>::first_argument_type>>
: found<U, typename std::reference_wrapper<T>::first_argument_type>
{ };

template<typename T, typename U, typename = void>
struct check_second_arg_type
: not_found<U>
{ };

// Matches when reference_wrapper<T>::second_argument_type exists
template<typename T, typename U>
struct check_second_arg_type<T, U, void_t<typename std::reference_wrapper<T>::second_argument_type>>
: found<U, typename std::reference_wrapper<T>::second_argument_type>
{ };

} // namespace __gnu_test

struct X {};

struct int_result_type { typedef int result_type; };

struct derives_unary : std::unary_function<int, int> {};

struct derives_binary : std::binary_function<int, float, int> {};

struct derives_unary_binary
  : std::unary_function<int, int>,
    std::binary_function<int, float, int>
{
  typedef int result_type;
};

void test01()
{
  using std::is_same;
  using __gnu_test::check_result_type;
  using __gnu_test::none;

  // Check result_type typedef
  static_assert( check_result_type<int_result_type, int>::value, "has result_type" );
  static_assert( check_result_type<derives_unary, int>::value, "has result_type" );
  static_assert( check_result_type<derives_binary, int>::value, "has result_type" );
  static_assert( check_result_type<derives_unary_binary, int>::value, "has result_type" );
  static_assert( check_result_type<int(void), int>::value, "has result_type" );
  static_assert( check_result_type<int(*)(void), int>::value, "has result_type" );
  static_assert( check_result_type<int (::X::*)(), int>::value, "has result_type" );
  static_assert( check_result_type<int (::X::*)(float), int>::value, "has result_type" );
}

void test02()
{
  using __gnu_test::check_arg_type;
  using __gnu_test::check_first_arg_type;
  using __gnu_test::check_second_arg_type;
  using __gnu_test::none;

  // Check argument_type typedef
  static_assert( check_arg_type<int_result_type, none>::value, "" );
  static_assert( check_arg_type<derives_unary, int>::value, "" );
  static_assert( check_arg_type<derives_binary, none>::value, "" );
  static_assert( check_arg_type<derives_unary_binary, int>::value, "" );
  static_assert( check_arg_type<int(void), none>::value, "" );
  static_assert( check_arg_type<int(*)(void), none>::value, "" );
  static_assert( check_arg_type<int (::X::*)(), X*>::value, "" );
  static_assert( check_arg_type<int (::X::*)() const, const X*>::value, "" );
  static_assert( check_arg_type<int (::X::*)(float), none>::value, "" );
  static_assert( check_arg_type<int (::X::*)(char, char), none>::value, "" );

  // Check first_argument_type typedef
  static_assert( check_first_arg_type<int_result_type, none>::value, "" );
  static_assert( check_first_arg_type<derives_unary, none>::value, "" );
  static_assert( check_first_arg_type<derives_binary, int>::value, "" );
  static_assert( check_first_arg_type<derives_unary_binary, int>::value, "" );
  static_assert( check_first_arg_type<int(void), none>::value, "" );
  static_assert( check_first_arg_type<int(*)(void), none>::value, "" );
  static_assert( check_first_arg_type<int (::X::*)(), none>::value, "" );
  static_assert( check_first_arg_type<int (::X::*)(float), X*>::value, "" );
  static_assert( check_first_arg_type<int (::X::*)(float) const, const X*>::value, "" );
  static_assert( check_first_arg_type<int (::X::*)(char, char), none>::value, "" );

  // Check second_argument_type typedef
  static_assert( check_second_arg_type<int_result_type, none>::value, "" );
  static_assert( check_second_arg_type<derives_unary, none>::value, "" );
  static_assert( check_second_arg_type<derives_binary, float>::value, "" );
  static_assert( check_second_arg_type<derives_unary_binary, float>::value, "" );
  static_assert( check_second_arg_type<int(void), none>::value, "" );
  static_assert( check_second_arg_type<int(*)(void), none>::value, "" );
  static_assert( check_second_arg_type<int (::X::*)(), none>::value, "" );
  static_assert( check_second_arg_type<int (::X::*)(float), float>::value, "" );
  static_assert( check_second_arg_type<int (::X::*)(float) const, float>::value, "" );
  static_assert( check_second_arg_type<int (::X::*)(char, char), none>::value, "" );
}

int main()
{
  test01();
}
