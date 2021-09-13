// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-fileio "" }

// 27.9.1.12 Assign and swap [ostringstream.assign]

#include <fstream>
#include <string>
#include <testsuite_hooks.h>

using namespace std;

string const name = "ofstream-assign.txt";

void
test01()
{
  string s1 = "Let the whole outside world";
  string s2 = " consist of a long paper tape.";
  ofstream f(name, ios::trunc);
  VERIFY( f.is_open() );
  f << s1;
  {
    ofstream f1;
    f1 = std::move(f);
    VERIFY( f1.is_open() );
    VERIFY( !f.is_open() );
    f1 << s2;
    f1.swap(f);
    VERIFY( !f1.is_open() );
    VERIFY( f.is_open() );
    f << s1;
    swap(f1, f);
    f1 << s2;
  }
  ifstream in(name);
  string result;
  getline(in, result);
  VERIFY( result == (s1 + s2 + s1 + s2) );
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  std::wofstream s0, s;
  s = std::move(s0);
  s.swap(s0);
  swap(s, s0);
#endif
}

int
main()
{
  test01();
  test02();
}
