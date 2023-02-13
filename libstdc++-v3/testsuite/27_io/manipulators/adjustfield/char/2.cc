// Copyright (C) 1997-2023 Free Software Foundation, Inc.
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


#include <sstream>
#include <locale>
#include <iomanip>
#include <testsuite_hooks.h>

void test02()
{
  const std::string 	str_blank;
  std::string 	        str_tmp;
  std::stringbuf 	strbuf;
  std::ostream          o(&strbuf);

  o << std::setw(6) << std::right << "san";
  VERIFY( strbuf.str() == "   san" ); 
  strbuf.str(str_blank);

  o << std::setw(6) << std::internal << "fran";
  VERIFY( strbuf.str() == "  fran" ); 
  strbuf.str(str_blank);

  o << std::setw(6) << std::left << "cisco";
  VERIFY( strbuf.str() == "cisco " ); 
  strbuf.str(str_blank);
}

int 
main() 
{
  test02();
  return 0;
}
