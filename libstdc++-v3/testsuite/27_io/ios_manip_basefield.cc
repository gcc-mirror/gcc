// 981027 ncm work with libstdc++v3

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

#include <iostream>
#include <sstream>
#include <locale>
#include <iomanip>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif
			
struct MyNP : std::numpunct<char>
{
  std::string do_grouping() const;
  char   do_thousands_sep() const;
};

std::string MyNP::do_grouping() const { static std::string s("\3"); return s; }
char   MyNP::do_thousands_sep() const { return ' '; }

void test01()
{
  std::cout.imbue(std::locale(std::locale(), new MyNP));
  std::cout << std::oct << std::showbase;
  std::cout << -0123456l << std::endl;

  std::cout << ":" << std::setw(11);
  std::cout << -01234567l << ":" << std::endl;

  std::cout << ":" << std::setw(11) << std::left;
  std::cout << -0123456l << ":" << std::endl;

  std::cout << ":" << std::setw(11) << std::right;
  std::cout << -012345l << ":" << std::endl;

  std::cout << ":" << std::setw(11) << std::internal;
  std::cout << -01234l << ":" << std::endl;

  std::cout << std::hex;
  std::cout << std::setfill('0');
  std::cout << std::internal;
  std::cout << std::showbase;
  std::cout << std::setw(16);
  std::cout << 0x12345678l << std::endl;
#ifdef DEBUG_ASSERT
  assert (std::cout.good());
#endif
}

void test02()
{
  bool 		test = true;
  const std::string 	str_blank;
  std::string 	str_tmp;
  std::stringbuf 	strbuf;
  std::ostream 	o(&strbuf);

  o <<  std::setw(6) <<  std::right << "san";
  test &= strbuf.str() == "   san"; 
  strbuf.str(str_blank);

  o <<  std::setw(6) <<  std::internal << "fran";
  test &= strbuf.str() == "  fran"; 
  strbuf.str(str_blank);

  o << std::setw(6) <<  std::left << "cisco";
  test &= strbuf.str() == "cisco "; 
  strbuf.str(str_blank);

#ifdef DEBUG_ASSERT
  assert (test);
#endif
}

int main() {
  test01();
  return 0;
}

// Projected output:
/*
-0 123 456
:-01 234 567:
:-0 123 456 :
:   -012 345:
:-    01 234:
0x000012 345 678
*/
