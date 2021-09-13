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

// 27.9.1.11 basic_ofstream constructors [ofstream.cons]

#include <fstream>
#include <string>
#include <testsuite_hooks.h>

using namespace std;

std::string const name = "ofstream-move.txt";

void
test01()
{
  string s1 = "Let the whole outside world";
  string s2 = " consist of a long paper tape.";
  ofstream f(name, ios::trunc);
  VERIFY( f.is_open() );
  f << s1;
  {
    ofstream f1 = std::move(f);
    VERIFY( f1.is_open() );
    VERIFY( !f.is_open() );
    f1 << s2;
  }
  ifstream in(name);
  string result;
  getline(in, result);
  VERIFY( result == (s1 + s2) );
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  wstring s1 = L"Let the whole outside world";
  wstring s2 = L" consist of a long paper tape.";
  wofstream f(name, ios::trunc);
  VERIFY( f.is_open() );
  f << s1;
  {
    wofstream f1 = std::move(f);
    VERIFY( f1.is_open() );
    VERIFY( !f.is_open() );
    f1 << s2;
  }
  wifstream in(name);
  wstring result;
  getline(in, result);
  VERIFY( result == (s1 + s2) );
#endif
}

int
main()
{
  test01();
  test02();
}
