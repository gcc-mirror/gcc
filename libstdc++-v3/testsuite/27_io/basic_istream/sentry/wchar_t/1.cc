// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.6.1.1.2 class basic_istream::sentry

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  const wchar_t* lit01 = L"stereolab on the bolsinga tip";
  const std::wstring str01(lit01);

  std::wstringbuf strbuf01;
  std::wstringbuf strbuf02(str01);
  std::wistream istr01(&strbuf01);
  std::wistream istr02(&strbuf02);
  
  // test negatives
  std::wistream::sentry sentry01(istr01);	
  VERIFY( bool(sentry01) == false ); 

  std::wistream::sentry sentry02(istr01, true);
  VERIFY( bool(sentry02) == false ); 

  // positive tests
  std::wistream::sentry sentry03(istr02);	
  VERIFY( bool(sentry03) == true ); 

  std::wistream::sentry sentry04(istr02, true);
  VERIFY( bool(sentry04) == true ); 
}

int main() 
{
  test01();
  return 0;
}
