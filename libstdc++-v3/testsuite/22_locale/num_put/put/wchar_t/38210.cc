// Copyright (C) 2008, 2009 Free Software Foundation
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

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/38210
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  wostringstream oss1, oss2, oss3, oss4;
  wstring result1, result2, result3, result4;

  const num_put<wchar_t>& ng1 = use_facet<num_put<wchar_t> >(oss1.getloc());
  const num_put<wchar_t>& ng2 = use_facet<num_put<wchar_t> >(oss2.getloc());
  const num_put<wchar_t>& ng3 = use_facet<num_put<wchar_t> >(oss3.getloc());
  const num_put<wchar_t>& ng4 = use_facet<num_put<wchar_t> >(oss4.getloc());

  void* p = (void*)0x1;

  oss1.width(5);
  ng1.put(oss1.rdbuf(), oss1, L'*', p);
  result1 = oss1.str();
  VERIFY( result1 == L"**0x1" );

  oss2.width(5);
  oss2.setf(ios_base::right, ios_base::adjustfield);
  ng2.put(oss2.rdbuf(), oss2, L'*', p);
  result2 = oss2.str();
  VERIFY( result2 == L"**0x1" );

  oss3.width(5);
  oss3.setf(ios_base::internal, ios_base::adjustfield);
  ng3.put(oss3.rdbuf(), oss3, L'*', p);
  result3 = oss3.str();
  VERIFY( result3 == L"0x**1" );

  oss4.width(5);
  oss4.setf(ios_base::left, ios_base::adjustfield);
  ng4.put(oss4.rdbuf(), oss4, L'*', p);
  result4 = oss4.str();
  VERIFY( result4 == L"0x1**" );
}

int main()
{
  test01();
  return 0;
}
