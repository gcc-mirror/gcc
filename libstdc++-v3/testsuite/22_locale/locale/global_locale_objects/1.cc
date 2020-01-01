// 2000-09-13 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2000-2020 Free Software Foundation, Inc.
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

// 22.1.1.5 locale static members [lib.locale.statics]

#include <cwchar> // for mbstate_t
#include <locale>
#include <testsuite_hooks.h>

typedef std::codecvt<char, char, std::mbstate_t> ccodecvt;
class gnu_codecvt: public ccodecvt { }; 

void test01()
{
  using namespace std;

  string str1, str2;

  // Construct a locale object with the C facet.
  const locale loc01 = locale::classic();

  // Construct a locale object with the specialized facet.
  locale loc02(locale::classic(), new gnu_codecvt);
  VERIFY ( loc01 != loc02 );
  VERIFY ( !(loc01 == loc02) );

  // classic
  locale loc06("C");
  VERIFY (loc06 == loc01);
  str1 = loc06.name();
  VERIFY( str1 == "C" );

  // global
  locale loc03;
  VERIFY ( loc03 == loc01);
  locale global_orig = locale::global(loc02);
  locale loc05;
  VERIFY (loc05 != loc03);
  VERIFY (loc05 == loc02);

  // Reset global settings.
  locale::global(global_orig);
}

int main ()
{
  test01();
  return 0;
}
