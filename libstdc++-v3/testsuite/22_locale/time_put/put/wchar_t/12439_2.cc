// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// { dg-require-time "" }

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <ctime>
#include <cstring>
#include <testsuite_hooks.h>

// libstdc++/12439
// time_put::put writes narrowed characters to output iterator
void test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef time_put<wchar_t> tp_type;
  
  const wchar_t fmt[] = {
    0xa0, 0x103, 0xfc, 0xb3, 0xa0c3,
    L'%', L'c'
  };
  
  const size_t len = sizeof(fmt) / sizeof(fmt[0]);
  const size_t cmplen = wcschr(fmt, L'%') - fmt;
  
  locale loc;
  const tp_type& tp = use_facet<tp_type>(loc);
  time_t tt = time(0);
  wostringstream stream;
  
  tp.put(tp_type::iter_type(stream), stream, stream.fill(),
	 localtime(&tt), fmt, fmt + len);
  wstring str = stream.str();
  VERIFY( str.length() >= cmplen );
  VERIFY( !wmemcmp(str.data(), fmt, cmplen) );
}

int main()
{
  test02();
  return 0;
}
