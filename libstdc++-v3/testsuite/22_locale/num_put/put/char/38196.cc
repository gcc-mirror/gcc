// Copyright (C) 2008-2013 Free Software Foundation, Inc.
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

class my_punct : public std::numpunct<char>
{
protected:
  std::string do_falsename() const { return "-no-"; }
};

// libstdc++/38196
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  ostringstream oss1, oss2, oss3, oss4;
  string result1, result2, result3, result4;

  oss1.imbue(locale(oss1.getloc(), new my_punct));
  oss2.imbue(locale(oss2.getloc(), new my_punct));
  oss3.imbue(locale(oss3.getloc(), new my_punct));
  oss4.imbue(locale(oss4.getloc(), new my_punct));
  const num_put<char>& ng1 = use_facet<num_put<char> >(oss1.getloc());
  const num_put<char>& ng2 = use_facet<num_put<char> >(oss2.getloc());
  const num_put<char>& ng3 = use_facet<num_put<char> >(oss3.getloc());
  const num_put<char>& ng4 = use_facet<num_put<char> >(oss4.getloc());

  oss1.width(6);
  oss1.setf(ios_base::boolalpha);
  ng1.put(oss1.rdbuf(), oss1, '*', false);
  result1 = oss1.str();
  VERIFY( result1 == "**-no-" );

  oss2.width(6);
  oss2.setf(ios_base::right, ios_base::adjustfield);
  oss2.setf(ios_base::boolalpha);
  ng2.put(oss2.rdbuf(), oss2, '*', false);
  result2 = oss2.str();
  VERIFY( result2 == "**-no-" );

  oss3.width(6);
  oss3.setf(ios_base::internal, ios_base::adjustfield);
  oss3.setf(ios_base::boolalpha);
  ng3.put(oss3.rdbuf(), oss3, '*', false);
  result3 = oss3.str();
  VERIFY( result3 == "**-no-" );

  oss4.width(6);
  oss4.setf(ios_base::left, ios_base::adjustfield);
  oss4.setf(ios_base::boolalpha);
  ng4.put(oss4.rdbuf(), oss4, '*', false);
  result4 = oss4.str();
  VERIFY( result4 == "-no-**" );
}

int main()
{
  test01();
  return 0;
}
