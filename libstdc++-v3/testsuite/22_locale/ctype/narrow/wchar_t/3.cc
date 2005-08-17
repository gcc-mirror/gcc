// { dg-require-namedlocale "" }

// 2003-03-12  Petur Runolfsson  <peturr02@ru.is>

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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 22.2.1.3.2 ctype<wchar_t> members

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/9581
void test03()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

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
