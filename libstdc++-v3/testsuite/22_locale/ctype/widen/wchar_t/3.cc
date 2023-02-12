// { dg-require-namedlocale "se_NO.UTF-8" }

// 2003-03-12  Petur Runolfsson  <peturr02@ru.is>

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


// 22.2.1.3.2 ctype<wchar_t> members

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/9581
void test03()
{
  using namespace std;

  locale loc = locale("se_NO.UTF-8");
  const ctype<wchar_t>& wct = use_facet<ctype<wchar_t> >(loc);

  const char* strlit = "\xc2\x80";
	
  wchar_t wbuf[3];
  wct.widen(strlit, strlit + 3, wbuf);
  VERIFY( wbuf[0] == wct.widen(strlit[0]) );
  VERIFY( wbuf[1] == wct.widen(strlit[1]) );
  VERIFY( wbuf[2] == wct.widen(strlit[2]) );
}

int main() 
{
  test03();
  return 0;
}
