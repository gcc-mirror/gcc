// 2006-10-11  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2023 Free Software Foundation, Inc.
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

struct Punct1: std::numpunct<char>
{ std::string do_grouping() const { return "\003\002\001"; } };

struct Punct2: std::numpunct<char>
{ std::string do_grouping() const { return "\001\003"; } };

void test01()
{
  using namespace std;

  ostringstream oss1, oss2;
  string result1, result2, result3;
  const string empty;

  oss1.imbue(locale(oss1.getloc(), new Punct1));
  oss2.imbue(locale(oss2.getloc(), new Punct2));
  const num_put<char>& ng1 = use_facet<num_put<char> >(oss1.getloc());
  const num_put<char>& ng2 = use_facet<num_put<char> >(oss2.getloc());

  long l1 = 12345678l;
  double d1 = 1234567.0;
  double d2 = 123456.0;

  ng1.put(oss1.rdbuf(), oss1, '+', l1);
  result1 = oss1.str();
  VERIFY( result1 == "1,2,3,45,678" );

  oss2.precision(1);
  oss2.setf(ios_base::fixed, ios_base::floatfield);
  ng2.put(oss2.rdbuf(), oss2, '+', d1);
  result2 = oss2.str();
  VERIFY( result2 == "123,456,7.0" );

  oss2.str(empty);
  ng2.put(oss2.rdbuf(), oss2, '+', d2);
  result3 = oss2.str();
  VERIFY( result3 == "12,345,6.0" );
}

int main()
{
  test01();
  return 0;
}
