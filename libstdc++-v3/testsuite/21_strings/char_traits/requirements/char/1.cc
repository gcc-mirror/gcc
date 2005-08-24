// 1999-06-03 bkoz

// Copyright (C) 1999, 2000, 2001, 2003, 2004, 2005
// Free Software Foundation, Inc.
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

// 21.1.1 Characher traits requirements

#include <string>
#include <testsuite_hooks.h>

void test01(void)
{
  bool test __attribute__((unused)) = true;
  const std::string str_01("zuma beach");
  const std::string str_02("montara and ocean beach");
 
  // 21.1.1 character traits requirements

  // Key for decoding what function signatures really mean:
  // X        == char_traits<_CharT>
  // [c,d]    == _CharT
  // [p,q]    == const _CharT*
  // s        == _CharT*
  // [n,i,j]  == size_t
  // f        == X::int_type
  // pos      == X::pos_type
  // state    == X::state_type

  // void X::assign(char c, char d)
  // assigns c = d;
  char c1 = 'z';
  char c2 = 'u';
  VERIFY( c1 != c2 );
  std::char_traits<char>::assign(c1,c2);
  VERIFY( c1 == 'u' );

  // char* X::move(char* s, const char* p, size_t n)
  // for each i in [0,n) performs X::assign(s[i], p[i]). Copies
  // correctly even where p is in [s, s + n), and yields s.   
  char array1[] = {'z', 'u', 'm', 'a', ' ', 'b', 'e', 'a', 'c', 'h',  0};
  const char str_lit1[] = "montara and ocean beach";
  const char str_lit2[] = "boracay, philippines";
  const int len1 = sizeof(str_lit1)/sizeof(char);
  const int len2 = sizeof(str_lit2)/sizeof(char);
  char array2[len1 + len2 - 1]; // two terminating chars
  std::char_traits<char>::copy(array2, str_lit2, len2);

  VERIFY( str_lit1[0] == 'm' );
  c1 = array2[0];
  c2 = str_lit1[0];
  char c3 = array2[1];
  char c4 = str_lit1[1];
  std::char_traits<char>::move(array2, str_lit1, 0);
  VERIFY( array2[0] == c1 );
  VERIFY( str_lit1[0] == c2 );
  std::char_traits<char>::move(array2, str_lit1, 1);
  VERIFY( array2[0] == c2 );
  VERIFY( str_lit1[0] == c2 );
  VERIFY( array2[1] == c3 );
  VERIFY( str_lit1[1] == c4 );
  std::char_traits<char>::move(array2, str_lit1, 2);
  VERIFY( array2[0] == c2 );
  VERIFY( str_lit1[0] == c2 );
  VERIFY( array2[1] == c4 );
  VERIFY( str_lit1[1] == c4 );
 
  char* pc1 = array1 + 1;
  c1 = pc1[0];
  c2 = array1[0];
  VERIFY( c1 != c2 );
  char* pc2 = std::char_traits<char>::move(array1, pc1, 0);
  c3 = pc1[0];
  c4 = array1[0];
  VERIFY( c1 == c3 );
  VERIFY( c2 == c4 );
  VERIFY( pc2 == array1 );

  c1 = pc1[0];
  c2 = array1[0];
  char* pc3 = pc1;
  pc2 = std::char_traits<char>::move(array1, pc1, 10);
  c3 = pc1[0];
  c4 = array1[0];
  VERIFY( c1 != c3 ); // underlying char array changed.
  VERIFY( c4 != c3 );
  VERIFY( pc2 == array1 );
  VERIFY( pc3 == pc1 ); // but pointers o-tay
  c1 = *(str_01.data());
  c2 = array1[0];
  VERIFY( c1 != c2 );
}

int main()
{ 
  test01();
  return 0;
}
