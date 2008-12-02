// { dg-require-namedlocale "" }

// Copyright (C) 2008 Free Software Foundation
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/38365
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  locale other(locale("C"));
  locale one(locale("en_US"), new ctype<char>());
  locale loc(other, one, locale::collate);

  VERIFY( one.name() == "*" );
  VERIFY( other.name() == "C" );
  VERIFY( loc.name() == "*" );
}

int main()
{
  test01();
  return 0;
}
