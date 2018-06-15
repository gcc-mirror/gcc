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

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::wstring s0;
  std::allocator<wchar_t> a;

  std::basic_string s1 = s0;
  check_type<std::wstring>(s1);

  std::basic_string s2 = std::move(s0);
  check_type<std::wstring>(s2);

  const std::basic_string s3 = s0;
  check_type<const std::wstring>(s3);

  const std::basic_string s4 = s2;
  check_type<const std::wstring>(s4);

#if _GLIBCXX_USE_CXX11_ABI
  std::basic_string s5(s0, a);
  check_type<std::wstring>(s5);

  std::basic_string s6(std::move(s0), a);
  check_type<std::wstring>(s6);
#endif

  std::basic_string s7(s0, 0, 0);
  check_type<std::wstring>(s7);
}

void
test02()
{
  using namespace __gnu_test;
  wchar_t a[1] = {};
  test_container<wchar_t, input_iterator_wrapper> seq(a);

  std::basic_string s1(seq.begin(), seq.end());
  check_type<std::wstring>(s1);

  std::basic_string s2(seq.begin(), seq.end(), std::allocator<wchar_t>());
  check_type<std::wstring>(s2);

  std::basic_string s3((wchar_t)1, L'a');
  check_type<std::wstring>(s3);

  std::basic_string s4((wchar_t)1, L'a', std::allocator<wchar_t>());
  check_type<std::wstring>(s4);
}

void
test05()
{
  // LWG 3075 basic_string needs deduction guides from basic_string_view
  std::wstring_view sv{L"A View to a Kill"};
  const std::allocator<wchar_t> a;

  std::basic_string s1(sv);
  check_type<std::wstring>(s1);

  std::basic_string s2(sv, a);
  check_type<std::wstring>(s2);

  std::basic_string s3(sv, 2u, 6u);
  check_type<std::wstring>(s3);

  std::basic_string s4(sv, 2u, 6u, a);
  check_type<std::wstring>(s4);
}

void
test06()
{
  // LWG 3076 basic_string CTAD ambiguity
  using namespace std;
  wstring s0;

  basic_string s1(s0, 1, 1);
  check_type<std::wstring>(s1);

  basic_string s2(L"cat"sv, 1, 1);
  check_type<std::wstring>(s2);

  basic_string s3(L"cat", 1);
  check_type<std::wstring>(s3);
}
