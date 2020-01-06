// 2001-08-15 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

// 22.2.4.1.1 collate members

#include <locale>
#include <testsuite_hooks.h>

// Check "C" locale.
void test01()
{
  using namespace std;
  typedef std::collate<wchar_t>::string_type string_type;

  string_type str1(L"fffff");
  string_type str2(L"ffffffffffff");

  const std::locale cloc = std::locale::classic();
  const std::collate<wchar_t> &col = std::use_facet<std::collate<wchar_t> >(cloc);

  long l1 = col.hash(str1.c_str(), str1.c_str() + str1.size());
  long l2 = col.hash(str2.c_str(), str2.c_str() + str2.size());
  VERIFY( l1 != l2 );
}

int main()
{
  test01();
  return 0;
}
