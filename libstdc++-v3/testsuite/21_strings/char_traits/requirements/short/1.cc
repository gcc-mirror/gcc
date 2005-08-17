// 1999-06-03 bkoz
// 2003-07-22 Matt Austern

// Copyright (C) 1999, 2000, 2001, 2003, 2004 Free Software Foundation, Inc.
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

// 21.1.1 Character traits requirements
// Make sure we can instantiate char_traits and basic_string for
// charT = 'short', and make sure the char_traits memeber functions
// satisfy the requirements of 21.1.1.

#include <string>
#include <testsuite_hooks.h>

void test02(void)
{
  bool test __attribute__((unused)) = true;
 
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

  // void X::assign(short c, short d)
  // assigns c = d;
  short c1 = 'z';
  short c2 = 'u';
  VERIFY( c1 != c2 );
  std::char_traits<short>::assign(c1,c2);
  VERIFY( c1 == 'u' );

  // bool X::eq(short c, short d)
  c1 = 'z';
  c2 = 'u';
  VERIFY ( !std::char_traits<short>::eq(c1, c2) );
  VERIFY ( std::char_traits<short>::eq(c1, c1) );
  VERIFY ( std::char_traits<short>::eq(c2, c2) );

  // bool X::lt(short c, short d)
  c1 = 'z';
  c2 = 'u';
  VERIFY ( std::char_traits<short>::lt(c2, c1) );
  VERIFY ( !std::char_traits<short>::lt(c1, c2) );
  VERIFY ( !std::char_traits<short>::lt(c1, c1) );
  VERIFY ( !std::char_traits<short>::lt(c2, c2) );

  // short* X::move(short* s, const short* p, size_t n)
  // for each i in [0,n) performs X::assign(s[i], p[i]). Copies
  // correctly even where p is in [s, s + n), and yields s.   
  short array1[] = {'z', 'u', 'm', 'a', ' ', 'b', 'e', 'a', 'c', 'h',  0};
  const std::basic_string<short> str_01(array1 + 0, array1 + 10);

  const short str_lit1[] = {'m', 'o', 'n', 't', 'a', 'r', 'a', ' ', 'a', 'n', 'd', ' ', 'o', 'c', 'e', 'a', 'n', ' ', 'b', 'e', 'a', 'c', 'h', 0};

  int len = sizeof(str_lit1)/sizeof(short) + sizeof(array1)/sizeof(short) - 1;
  // two terminating chars
  short array3[] = {'b', 'o', 'r', 'a', 'c', 'a', 'y', ',', ' ', 'p', 'h', 'i', 'l', 'i', 'p', 'p', 'i', 'n', 'e', 's', 0};
  short array2[len];
  std::char_traits<short>::copy(array2, array3, len);

  VERIFY( str_lit1[0] == 'm' );
  c1 = array2[0];
  c2 = str_lit1[0];
  short c3 = array2[1];
  short c4 = str_lit1[1];
  std::char_traits<short>::move(array2, str_lit1, 0);
  VERIFY( array2[0] == c1 );
  VERIFY( str_lit1[0] == c2 );
  std::char_traits<short>::move(array2, str_lit1, 1);
  VERIFY( array2[0] == c2 );
  VERIFY( str_lit1[0] == c2 );
  VERIFY( array2[1] == c3 );
  VERIFY( str_lit1[1] == c4 );
  std::char_traits<short>::move(array2, str_lit1, 2);
  VERIFY( array2[0] == c2 );
  VERIFY( str_lit1[0] == c2 );
  VERIFY( array2[1] == c4 );
  VERIFY( str_lit1[1] == c4 );
 
  short* pc1 = array1 + 1;
  c1 = pc1[0];
  c2 = array1[0];
  VERIFY( c1 != c2 );
  short* pc2 = std::char_traits<short>::move(array1, pc1, 0);
  c3 = pc1[0];
  c4 = array1[0];
  VERIFY( c1 == c3 );
  VERIFY( c2 == c4 );
  VERIFY( pc2 == array1 );

  c1 = pc1[0];
  c2 = array1[0];
  short* pc3 = pc1;
  pc2 = std::char_traits<short>::move(array1, pc1, 10);
  c3 = pc1[0];
  c4 = array1[0];
  VERIFY( c1 != c3 ); // underlying short array changed.
  VERIFY( c4 != c3 );
  VERIFY( pc2 == array1 );
  VERIFY( pc3 == pc1 ); // but pointers o-tay
  c1 = *(str_01.data());
  c2 = array1[0];
  VERIFY( c1 != c2 );

  // size_t X::length(const short* p)
  len = std::char_traits<short>::length(str_lit1);
  VERIFY( len == sizeof(str_lit1) / sizeof(short) - 1 );

  // const short* X::find(const short* s, size_t n, short c)
  const int N4 = sizeof(str_lit1) / sizeof(short);
  const short* pc4 = std::char_traits<short>::find(str_lit1, N4, 'a');
  VERIFY( pc4 != 0 );
  VERIFY( *pc4 == 'a' );

  pc4 = std::char_traits<short>::find(str_lit1, N4, 0x0a73);
  VERIFY( pc4 == 0 );

  // short* X::assign(short* s, size_t n, short c)
  len = sizeof(array2) / sizeof(short);
  memset(array2, 0xaf, len * sizeof(short));
  VERIFY( array2[0] != 0x15a8 );

  pc1 = std::char_traits<short>::assign (array2, len, 0x15a8);
  VERIFY( pc1 == array2 );
  for (int i = 0; i < len; ++i)
    VERIFY( array2[i] == 0x15a8 );

  // short* X::copy(short* s, const short* p, size_t n)
  int n1 = sizeof(str_lit1) / sizeof(short);
  pc1 = std::char_traits<short>::copy(array2, str_lit1, n1);
  len = std::char_traits<short>::length(array2);
  VERIFY( len == n1 - 1 );
  for (int i = 0; i < len; ++i)
    VERIFY( str_lit1[i] == array2[i] );

  // int X::compare(const short* p, const short* q, size_t n)
  const short* pconst1 = str_01.data();
  const short* pconst2 = str_lit1;

  VERIFY( std::char_traits<short>::compare(pconst1, pconst2, 10) > 0 );
  VERIFY( std::char_traits<short>::compare(pconst2, pconst1, 10) < 0 );
  VERIFY( std::char_traits<short>::compare(pconst1, pconst1, 10) == 0 );
  VERIFY( std::char_traits<short>::compare(pconst2, pconst2, 10) == 0 );
}



int main()
{ 
  test02();
  return 0;
}
