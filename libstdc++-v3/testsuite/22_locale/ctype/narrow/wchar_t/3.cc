// { dg-require-namedlocale "se_NO.UTF-8" }

// 2003-03-12  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

// libstdc++/9581
void test03()
{
  using namespace std;

  locale loc = locale("se_NO.UTF-8");
  const ctype<wchar_t>& wct = use_facet<ctype<wchar_t> >(loc);

  const wchar_t* wstrlit = L"\x80";
	
  char buf[2];
  wct.narrow(wstrlit, wstrlit + 2, ' ', buf);
  VERIFY( buf[0] == wct.narrow(wstrlit[0], ' ') );
  VERIFY( buf[1] == wct.narrow(wstrlit[1], ' ') );  
}

int main() 
{
  test03();
  return 0;
}
