// 2007-08-20  <benjamin@redhat.com>
//
// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

template<typename T>
  void
  do_test()
  {
    typedef T 				value_type;
    typedef std::tr1::hash<value_type> 	hash_type;
    using std::size_t;

    value_type v = T(); // default initialized is fine, same value all
                        // that matters.
    hash_type h1;
    size_t r1 = size_t(h1(v));

    hash_type h2;
    size_t r2 = size_t(h2(v));

    VERIFY( r1 == r2 );
  }

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
