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

#include <sstream>
#include <locale>
#include <iomanip>
#include <testsuite_hooks.h>

void test02()
{
  bool test __attribute__((unused)) = true;
  const std::wstring 	str_blank;
  std::wstring 	        str_tmp;
  std::wstringbuf 	strbuf;
  std::wostream 	o(&strbuf);

  o << std::setw(6) << std::right << L"san";
  VERIFY( strbuf.str() == L"   san" ); 
  strbuf.str(str_blank);

  o << std::setw(6) << std::internal << L"fran";
  VERIFY( strbuf.str() == L"  fran" ); 
  strbuf.str(str_blank);

  o << std::setw(6) << std::left << L"cisco";
  VERIFY( strbuf.str() == L"cisco " ); 
  strbuf.str(str_blank);
}

int 
main() 
{
  test02();
  return 0;
}
