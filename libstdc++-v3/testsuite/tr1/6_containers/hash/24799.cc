// 2005-11-11  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

// 6.3.3 Class template hash

#include <tr1/functional>
#include <string>
#include <tr1/type_traits>
#include <testsuite_tr1.h>
#include <testsuite_hooks.h>

template<typename T>
  void
  do_test()
  {
    using std::tr1::is_same;
    using __gnu_test::test_relationship;

    typedef typename std::tr1::hash<T>::argument_type  argument_type;
    typedef typename std::tr1::hash<T>::result_type    result_type;

    VERIFY( (test_relationship<is_same, argument_type, T>(true)) );
    VERIFY( (test_relationship<is_same, result_type, std::size_t>(true)) );
  }

// libstdc++/24799
void test01()
{
  do_test<bool>();
  do_test<char>();
  do_test<signed char>();
  do_test<unsigned char>();
  do_test<short>();
  do_test<int>();
  do_test<long>();
  do_test<unsigned short>();
  do_test<unsigned int>();
  do_test<unsigned long>();
  do_test<int*>();
  do_test<std::string>();
  do_test<float>();
  do_test<double>();
  do_test<long double>();

#ifdef _GLIBCXX_USE_WCHAR_T
  do_test<wchar_t>();
  do_test<std::wstring>();
#endif
}

int main()
{
  test01();
  return 0;
}
