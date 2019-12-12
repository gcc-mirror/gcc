// 1999-07-26 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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

// 27.6.1.2.3 character extractors

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void test02() 
{
  typedef std::ios::traits_type ctraits_type;

  std::string str_01;
  const std::string str_02("or coltrane playing tunji with jimmy garrison");
  const std::string str_03("coltrane");

  std::stringbuf isbuf_01(std::ios_base::in);
  std::stringbuf isbuf_02(str_02, std::ios_base::in);
  std::istream is_01(0);
  std::istream is_02(&isbuf_02);
  std::ios_base::iostate state1, state2, statefail;
  statefail = std::ios_base::failbit;

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(istream&, _CharT&)
  char c1 = 'c', c2 = 'c';
  state1 = is_01.rdstate();
  is_01 >> c1;   
  state2 = is_01.rdstate();
  VERIFY( state1 != state2 );
  VERIFY( c1 == c2 );
  VERIFY( static_cast<bool>(state2 & statefail) );

  state1 = is_02.rdstate();
  is_02 >> c1;   
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( c1 == 'o' );
  is_02 >> c1;   
  is_02 >> c1;   
  VERIFY( c1 == 'c' );
  VERIFY( !static_cast<bool>(state2 & statefail) );

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(istream&, unsigned char&)
  unsigned char uc1 = 'c';
  state1 = is_02.rdstate();
  is_02 >> uc1;   
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( uc1 == 'o' );
  is_02 >> uc1;   
  is_02 >> uc1;   
  VERIFY( uc1 == 't' );

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(istream&, signed char&)
  signed char sc1 = 'c';
  state1 = is_02.rdstate();
  is_02 >> sc1;   
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( sc1 == 'r' );
  is_02 >> sc1;   
  is_02 >> sc1;   
  VERIFY( sc1 == 'n' );
}

int main()
{
  test02();
  return 0;
}
