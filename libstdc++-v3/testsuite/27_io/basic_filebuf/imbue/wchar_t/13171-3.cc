// { dg-require-namedlocale "" }

// Copyright (C) 2003, 2005 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <iostream>
#include <locale>
#include <testsuite_hooks.h>

// libstdc++/13171
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  locale::global(locale("fr_FR"));

  ios_base::sync_with_stdio(false);

  locale::global(locale("en_US"));
  cin.imbue(locale("en_US"));
  cout.imbue(locale("en_US"));
  cerr.imbue(locale("en_US"));
  clog.imbue(locale("de_DE"));
  wcin.imbue(locale("en_US"));
  wcout.imbue(locale("en_US"));
  wcerr.imbue(locale("en_US"));
  wclog.imbue(locale("de_DE"));
 
  cout << 'f' << endl;
  cerr << 'r' << endl;
  clog << 'A' << endl;
  wcout << L's' << endl;
  wcerr << L'i' << endl;
  wclog << L'L' << endl;
}

int main()
{
  test01();
}
