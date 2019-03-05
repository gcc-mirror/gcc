// 1999-06-03 bkoz
// 2003-07-22 Matt Austern

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

// 21.1.1 Character traits requirements
// Make sure we can instantiate char_traits and basic_string for
// charT = 'short', and make sure the char_traits memeber functions
// satisfy the requirements of 21.1.1.

#include <string>
#include <cstring>
#include <testsuite_hooks.h>

void test02(void)
{
  typedef short char_type;
 
  // 21.1.1 character traits requirements

  // Key for decoding what function signatures really mean:
  // X                == char_traits<_CharT>
  // [c,d]    == _CharT
  // [p,q]    == const _CharT*
  // s                == _CharT*
  // [n,i,j]  == size_t
  // f                == X::int_type
  // pos      == X::pos_type
  // state    == X::state_type

  // void X::assign(char_type c, char_type d)
  // assigns c = d;
  char_type c1 = 'z';
  char_type c2 = 'u';
  VERIFY( c1 != c2 );
  std::char_traits<char_type>::assign(c1,c2);
  VERIFY( c1 == 'u' );

  // bool X::eq(char_type c, char_type d)
  c1 = 'z';
  c2 = 'u';
  VERIFY ( !std::char_traits<char_type>::eq(c1, c2) );
  VERIFY ( std::char_traits<char_type>::eq(c1, c1) );
  VERIFY ( std::char_traits<char_type>::eq(c2, c2) );

  // bool X::lt(char_type c, char_type d)
  c1 = 'z';
  c2 = 'u';
  VERIFY ( std::char_traits<char_type>::lt(c2, c1) );
  VERIFY ( !std::char_traits<char_type>::lt(c1, c2) );
  VERIFY ( !std::char_traits<char_type>::lt(c1, c1) );
  VERIFY ( !std::char_traits<char_type>::lt(c2, c2) );

  // char_type* X::move(char_type* s, const char_type* p, size_t n)
  // for each i in [0,n) performs X::assign(s[i], p[i]). Copies
  // correctly even where p is in [s, s + n), and yields s.   
  char_type array1[] = {'z', 'u', 'm', 'a', ' ', 'b', 'e', 'a', 'c', 'h',  0};
  const std::basic_string<char_type> str_01(array1 + 0, array1 + 10);

  const char_type str_lit1[] = {'m', 'o', 'n', 't', 'a', 'r', 'a', ' ', 'a', 'n', 'd', ' ', 'o', 'c', 'e', 'a', 'n', ' ', 'b', 'e', 'a', 'c', 'h', 0};

  const int array2_len = sizeof(str_lit1)/sizeof(char_type) + sizeof(array1)/sizeof(char_type) - 1;
  // two terminating chars
  char_type array3[] = {'b', 'o', 'r', 'a', 'c', 'a', 'y', ',', ' ', 'p', 'h', 'i', 'l', 'i', 'p', 'p', 'i', 'n', 'e', 's', 0};
  char_type array2[array2_len];
  int len = std::min<int>(array2_len, sizeof(array3)/sizeof(char_type));
  std::char_traits<char_type>::copy(array2, array3, len);

  VERIFY( str_lit1[0] == 'm' );
  c1 = array2[0];
  c2 = str_lit1[0];
  char_type c3 = array2[1];
  char_type c4 = str_lit1[1];
  std::char_traits<char_type>::move(array2, str_lit1, 0);
  VERIFY( array2[0] == c1 );
  VERIFY( str_lit1[0] == c2 );
  std::char_traits<char_type>::move(array2, str_lit1, 1);
  VERIFY( array2[0] == c2 );
  VERIFY( str_lit1[0] == c2 );
  VERIFY( array2[1] == c3 );
  VERIFY( str_lit1[1] == c4 );
  std::char_traits<char_type>::move(array2, str_lit1, 2);
  VERIFY( array2[0] == c2 );
  VERIFY( str_lit1[0] == c2 );
  VERIFY( array2[1] == c4 );
  VERIFY( str_lit1[1] == c4 );
 
  char_type* pc1 = array1 + 1;
  c1 = pc1[0];
  c2 = array1[0];
  VERIFY( c1 != c2 );
  char_type* pc2 = std::char_traits<char_type>::move(array1, pc1, 0);
  c3 = pc1[0];
  c4 = array1[0];
  VERIFY( c1 == c3 );
  VERIFY( c2 == c4 );
  VERIFY( pc2 == array1 );

  c1 = pc1[0];
  c2 = array1[0];
  char_type* pc3 = pc1;
  pc2 = std::char_traits<char_type>::move(array1, pc1, 10);
  c3 = pc1[0];
  c4 = array1[0];
  VERIFY( c1 != c3 ); // underlying char_type array changed.
  VERIFY( c4 != c3 );
  VERIFY( pc2 == array1 );
  VERIFY( pc3 == pc1 ); // but pointers o-tay
  c1 = *(str_01.data());
  c2 = array1[0];
  VERIFY( c1 != c2 );

  // size_t X::length(const char_type* p)
  len = std::char_traits<char_type>::length(str_lit1);
  VERIFY( len == sizeof(str_lit1) / sizeof(char_type) - 1 );

  // const char_type* X::find(const char_type* s, size_t n, char_type c)
  const int N4 = sizeof(str_lit1) / sizeof(char_type);
  const char_type* pc4 = std::char_traits<char_type>::find(str_lit1, N4, 'a');
  VERIFY( pc4 != 0 );
  VERIFY( *pc4 == 'a' );

  pc4 = std::char_traits<char_type>::find(str_lit1, N4, 0x0a73);
  VERIFY( pc4 == 0 );

  // char_type* X::assign(char_type* s, size_t n, char_type c)
  std::memset(array2, 0xaf, array2_len * sizeof(char_type));
  VERIFY( array2[0] != 0x15a8 );

  pc1 = std::char_traits<char_type>::assign (array2, array2_len, 0x15a8);
  VERIFY( pc1 == array2 );
  for (int i = 0; i < array2_len; ++i)
    VERIFY( array2[i] == 0x15a8 );

  // char_type* X::copy(char_type* s, const char_type* p, size_t n)
  int n1 = sizeof(str_lit1) / sizeof(char_type);
  pc1 = std::char_traits<char_type>::copy(array2, str_lit1, n1);
  len = std::char_traits<char_type>::length(array2);
  VERIFY( len == n1 - 1 );
  for (int i = 0; i < len; ++i)
    VERIFY( str_lit1[i] == array2[i] );

  // int X::compare(const char_type* p, const char_type* q, size_t n)
  const char_type* pconst1 = str_01.data();
  const char_type* pconst2 = str_lit1;

  VERIFY( std::char_traits<char_type>::compare(pconst1, pconst2, 10) > 0 );
  VERIFY( std::char_traits<char_type>::compare(pconst2, pconst1, 10) < 0 );
  VERIFY( std::char_traits<char_type>::compare(pconst1, pconst1, 10) == 0 );
  VERIFY( std::char_traits<char_type>::compare(pconst2, pconst2, 10) == 0 );
}

int main()
{ 
  test02();
  return 0;
}
