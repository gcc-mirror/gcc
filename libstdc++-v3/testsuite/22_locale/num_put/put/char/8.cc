// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

struct Ctype: std::ctype<char>
{
  char
  do_widen(char c) const 
  { return 'A' + c % 26; }

  const char*
  do_widen(const char* lo, const char* hi, char* to) const
  {
    for (; lo != hi; *to++ = Ctype::do_widen(*lo++));
    return hi;
  } 
};

// See http://gcc.gnu.org/ml/libstdc++/2003-11/msg00154.html
void test01()
{
  using namespace std;

  ostringstream oss;
  oss.imbue(locale(locale::classic(), new Ctype));
  const num_put<char>& np = use_facet<num_put<char> >(oss.getloc());

  const string empty;
  string result;
  long inum = 123;
  double fnum = 123.456;

  np.put(oss.rdbuf(), oss, '+', inum);
  result = oss.str();
  VERIFY( result == "XYZ" );

  oss.clear();
  oss.str(empty);
  np.put(oss.rdbuf(), oss, '+', fnum);
  result = oss.str();
  VERIFY( result == "XYZ.ABC" );
}

int main()
{
  test01();
}
