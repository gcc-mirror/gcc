// 2005-09-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

using namespace std;

struct Punct1: numpunct<char>
{ string do_grouping() const { return string(1, char(-1)); } };

struct Punct2: numpunct<char>
{ string do_grouping() const { return string("\002") + char(-1); } };

struct Punct3: numpunct<char>
{ string do_grouping() const { return string("\001\002") + char(-1); } };

// libstdc++/23953
void test01()
{
  bool test __attribute__((unused)) = true;
  
  ostringstream oss1, oss2, oss3;
  string result1, result2, result3;

  oss1.imbue(locale(oss1.getloc(), new Punct1));
  oss2.imbue(locale(oss2.getloc(), new Punct2));
  oss3.imbue(locale(oss3.getloc(), new Punct3));
  const num_put<char>& ng1 = use_facet<num_put<char> >(oss1.getloc());
  const num_put<char>& ng2 = use_facet<num_put<char> >(oss2.getloc());
  const num_put<char>& ng3 = use_facet<num_put<char> >(oss3.getloc());

  long l1 = 12345l;
  long l2 = 12345678l;
  double d1 = 1234567.0;

  ng1.put(oss1.rdbuf(), oss1, '+', l1);
  result1 = oss1.str();
  VERIFY( result1 == "12345" );

  ng2.put(oss2.rdbuf(), oss2, '+', l2);
  result2 = oss2.str();
  VERIFY( result2 == "123456,78" );

  oss3.precision(1);
  oss3.setf(ios_base::fixed, ios_base::floatfield);
  ng3.put(oss3.rdbuf(), oss3, '+', d1);
  result3 = oss3.str();
  VERIFY( result3 == "1234,56,7.0" );
}

int main()
{
  test01();
  return 0;
}
