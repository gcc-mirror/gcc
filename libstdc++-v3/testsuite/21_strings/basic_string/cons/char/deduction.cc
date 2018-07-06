// Copyright (C) 2017-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <string>
#include <testsuite_iterators.h>

template<typename C>
  using input_iterator_seq
    = __gnu_test::test_container<C, __gnu_test::input_iterator_wrapper>;

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::string s0;
  std::allocator<char> a;

  std::basic_string s1 = s0;
  check_type<std::string>(s1);

  std::basic_string s2 = std::move(s0);
  check_type<std::string>(s2);

  const std::basic_string s3 = s0;
  check_type<const std::string>(s3);

  const std::basic_string s4 = s3;
  check_type<const std::string>(s4);

#if _GLIBCXX_USE_CXX11_ABI
  std::basic_string s5(s0, a);
  check_type<std::string>(s5);

  std::basic_string s6(std::move(s0), a);
  check_type<std::string>(s6);
#endif

  std::basic_string s7(s0, 0, 0);
  check_type<std::string>(s7);
}

void
test02()
{
  char a[1] = {};
  input_iterator_seq<char> seq(a);

  std::basic_string s1(seq.begin(), seq.end());
  check_type<std::string>(s1);

  std::basic_string s2(seq.begin(), seq.end(), std::allocator<char>());
  check_type<std::string>(s2);

  std::basic_string s3((char)1, 'a');
  check_type<std::string>(s3);

  std::basic_string s4((char)1, 'a', std::allocator<char>());
  check_type<std::string>(s4);
}

void
test03()
{
  char16_t a[1] = {};
  input_iterator_seq<char16_t> seq(a);

  std::basic_string s1(seq.begin(), seq.end());
  check_type<std::u16string>(s1);

  std::basic_string s2(seq.begin(), seq.end(), std::allocator<char16_t>());
  check_type<std::u16string>(s2);

  std::basic_string s3((char16_t)1, u'a');
  check_type<std::u16string>(s3);

  std::basic_string s4((char16_t)1, u'a', std::allocator<char16_t>());
  check_type<std::u16string>(s4);
}

void
test04()
{
  char32_t a[1] = {};
  input_iterator_seq<char32_t> seq(a);

  std::basic_string s1(seq.begin(), seq.end());
  check_type<std::u32string>(s1);

  std::basic_string s2(seq.begin(), seq.end(), std::allocator<char32_t>());
  check_type<std::u32string>(s2);

  std::basic_string s3((char32_t)1, U'a');
  check_type<std::u32string>(s3);

  std::basic_string s4((char32_t)1, U'a', std::allocator<char32_t>());
  check_type<std::u32string>(s4);
}
