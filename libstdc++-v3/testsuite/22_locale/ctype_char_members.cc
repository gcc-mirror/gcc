// 2000-02-16 bkoz

// Copyright (C) 2000 Free Software Foundation, Inc.
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

// 22.2.1.3.2 ctype<char> members

#include <locale>
// NB: Don't include any other headers in this file.
#include <debug_assert.h>

class gnu_ctype: public std::ctype<char> { };

void test01()
{
  bool test = true;
  const char strlit00[] = "manilla, cebu, tandag PHILIPPINES";
  const char strlit01[] = "MANILLA, CEBU, TANDAG PHILIPPINES";
  const char strlit02[] = "manilla, cebu, tandag philippines";
  const char c00 = 'S';
  const char c10 = 's';
  const char c20 = '9';
  const char c30 = ' ';
  const char c40 = '!';
  const char c50 = 'F';
  const char c60 = 'f';
  const char c70 = 'X';
  const char c80 = 'x';

  gnu_ctype gctype;
  char c100;
  int len = std::char_traits<char>::length(strlit00);
  char c_array[len + 1];

  // sanity check ctype_base::mask members
  int i01 = std::ctype_base::space;
  int i02 = std::ctype_base::upper;
  int i03 = std::ctype_base::lower;
  int i04 = std::ctype_base::digit;
  int i05 = std::ctype_base::punct;
  int i06 = std::ctype_base::alpha;
  int i07 = std::ctype_base::xdigit;
  int i08 = std::ctype_base::alnum;
  int i09 = std::ctype_base::graph;
  int i10 = std::ctype_base::print;
  int i11 = std::ctype_base::cntrl;
  int i12 = sizeof(std::ctype_base::mask);
  VERIFY ( i01 != i02);
  VERIFY ( i02 != i03);
  VERIFY ( i03 != i04);
  VERIFY ( i04 != i05);
  VERIFY ( i05 != i06);
  VERIFY ( i06 != i07);
  VERIFY ( i07 != i08);
  VERIFY ( i08 != i09);
  VERIFY ( i09 != i10);
  VERIFY ( i10 != i11);
  VERIFY ( i11 != i01);

  // bool is(mask m, char c) const;
  VERIFY( gctype.is(std::ctype_base::space, c30) );
  VERIFY( gctype.is(std::ctype_base::upper, c00) );
  VERIFY( gctype.is(std::ctype_base::lower, c10) );
  VERIFY( gctype.is(std::ctype_base::digit, c20) );
  VERIFY( gctype.is(std::ctype_base::punct, c40) );
  VERIFY( gctype.is(std::ctype_base::alpha, c50) );
  VERIFY( gctype.is(std::ctype_base::alpha, c60) );
  VERIFY( gctype.is(std::ctype_base::xdigit, c20) );
  VERIFY( !gctype.is(std::ctype_base::xdigit, c80) );
  VERIFY( gctype.is(std::ctype_base::alnum, c50) );
  VERIFY( gctype.is(std::ctype_base::alnum, c20) );
  VERIFY( gctype.is(std::ctype_base::graph, c40) );
  VERIFY( gctype.is(std::ctype_base::graph, c20) );

  // const char* is(const char* low, const char* high, mask* vec) const
  std::ctype_base::mask m00 = static_cast<std::ctype_base::mask>(0);
  std::ctype_base::mask m01[3];
  std::ctype_base::mask m02[13];
  const char* cc0 = strlit00;
  const char* cc1 = NULL;
  const char* cc2 = NULL;

  cc0 = strlit00;
  m01[0] = m00;
  m01[1] = m00;
  m01[2] = m00;
  cc1 = gctype.is(cc0, cc0, m01);
  VERIFY( cc1 == strlit00 );
  VERIFY( m01[0] == m00 );
  VERIFY( m01[1] == m00 );
  VERIFY( m01[2] == m00 );

  cc0 = strlit00;
  m01[0] = m00;
  m01[1] = m00;
  m01[2] = m00;
  cc2 = gctype.is(cc0, cc0 + 3, m01);
  VERIFY( cc2 == strlit00 + 3);
  VERIFY( m01[0] != m00 );
  VERIFY( m01[1] != m00 );
  VERIFY( m01[2] != m00 );
  VERIFY( gctype.is(m01[0], cc0[0]) );
  VERIFY( gctype.is(m01[1], cc0[1]) );
  VERIFY( gctype.is(m01[2], cc0[2]) );

  cc0 = strlit01;
  cc1 = gctype.is(cc0, cc0 + 13, m02);
  VERIFY( cc1 == strlit01 + 13);
  VERIFY( m02[6] != m00 );
  VERIFY( m02[7] != m00 );
  VERIFY( m02[8] != m00 );
  VERIFY( m02[8] != m02[6] );
  VERIFY( m02[6] != m02[7] );
  VERIFY( static_cast<bool>(m02[6] & std::ctype_base::alnum) );
  VERIFY( static_cast<bool>(m02[6] & std::ctype_base::upper) );
  VERIFY( static_cast<bool>(m02[6] & std::ctype_base::alpha) );
  VERIFY( static_cast<bool>(m02[7] & std::ctype_base::punct) );
  VERIFY( static_cast<bool>(m02[8] & std::ctype_base::space) );
  VERIFY( gctype.is(m02[6], cc0[6]) );
  VERIFY( gctype.is(m02[7], cc0[7]) );
  VERIFY( gctype.is(m02[8], cc0[8]) );

  // char toupper(char c) const
  c100 = gctype.toupper(c10);
  VERIFY( c100 == c00 );

  // char tolower(char c) const
  c100 = gctype.tolower(c00);
  VERIFY( c100 == c10 );

  // char toupper(char* low, const char* hi) const
  std::char_traits<char>::copy(c_array, strlit02, len + 1);
  gctype.toupper(c_array, c_array + len);
  VERIFY( !std::char_traits<char>::compare(c_array, strlit01, len - 1) );

  // char tolower(char* low, const char* hi) const
  std::char_traits<char>::copy(c_array, strlit01, len + 1);
  gctype.tolower(c_array, c_array + len);
  VERIFY( !std::char_traits<char>::compare(c_array, strlit02, len - 1) );


#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

int main() {
  test01();
  return 0;
}
