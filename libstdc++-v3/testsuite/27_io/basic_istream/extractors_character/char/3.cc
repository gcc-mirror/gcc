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

void test01() 
{
  std::string str_01;
  const std::string str_02("coltrane playing 'softly as a morning sunrise'");
  const std::string str_03("coltrane");

  std::stringbuf isbuf_01(std::ios_base::in);
  std::stringbuf isbuf_02(str_02, std::ios_base::in);
  std::istream is_01(0);
  std::istream is_02(&isbuf_02);

  std::ios_base::iostate state1, state2, statefail;
  statefail = std::ios_base::failbit;

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(istream&, _CharT*)
  int n = 20;
  char array1[n];
  typedef std::ios::traits_type ctraits_type;

  // testing with width() control enabled.
  is_02.width(8);
  state1 = is_02.rdstate();
  is_02 >> array1;   // should snake "coltran"
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( !ctraits_type::compare(array1, "coltran", 7) );

  is_02.width(1);
  state1 = is_02.rdstate();
  is_02 >> array1;   // should snake nothing, set failbit
  state2 = is_02.rdstate();
  VERIFY( state1 != state2 );
  VERIFY( state2 == statefail );
  VERIFY( array1[0] == '\0' );

  is_02.width(8);
  is_02.clear();
  state1 = is_02.rdstate();
  VERIFY( !state1 );
  is_02 >> array1;   // should snake "e"
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( !ctraits_type::compare(array1, "e", 1) );

  // testing for correct exception setting
  const std::string str_04("   impulse!!");
  std::stringbuf isbuf_03(str_04, std::ios_base::in);
  std::stringbuf isbuf_04(str_04, std::ios_base::in);
  std::istream is_03(&isbuf_03);
  std::istream is_04(&isbuf_04);

  is_03 >> array1;
  VERIFY( !ctraits_type::compare(array1,"impulse!!", 10) );
  VERIFY( is_03.rdstate() == std::ios_base::eofbit );

  is_04.width(9);
  is_04 >> array1;
  VERIFY( ! std::ios::traits_type::compare(array1,"impulse!", 9) );
  VERIFY( !is_04.rdstate() ); 
}

int main()
{
  test01();
  return 0;
}
