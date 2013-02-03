// { dg-require-namedlocale "en_US.ISO-8859-1" }

// 2003-03-12  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<wchar_t> members

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/9870
void test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  locale loc = locale("en_US.ISO-8859-1");
  const ctype<wchar_t>& wct = use_facet<ctype<wchar_t> >(loc);

  char c = 0xff;
  wchar_t wc = wct.widen(c);

  VERIFY( wc == static_cast<wchar_t>(0xff) );
}

int main() 
{
  test02();
  return 0;
}
