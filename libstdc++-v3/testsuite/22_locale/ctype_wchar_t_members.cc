// 2000-09-01 Benjamin Kosnik <bkoz@redhat.com>

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

#if _GLIBCPP_USE_WCHAR_T
class gnu_ctype: public std::ctype<wchar_t> {};

void test01()
{
  bool test = true;
  typedef wchar_t 	char_type;

  const char_type strlit00[] = L"manilla, cebu, tandag PHILIPPINES";
  const char_type strlit01[] = L"MANILLA, CEBU, TANDAG PHILIPPINES";
  const char_type strlit02[] = L"manilla, cebu, tandag philippines";
  const char_type c00 = L'S';
  const char_type c10 = L's';
  const char_type c20 = L'9';
  const char_type c30 = L' ';
  const char_type c40 = L'!';
  const char_type c50 = L'F';
  const char_type c60 = L'f';
  const char_type c70 = L'X';
  const char_type c80 = L'x';

  gnu_ctype gctype;
  char_type c100;
  int len = std::char_traits<char_type>::length(strlit00);
  char_type c_array[len + 1];

  // bool is(mask m, char_type c) const;
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

  // char_type toupper(char_type c) const
  c100 = gctype.toupper(c10);
  VERIFY( c100 == c00 );

  // char_type tolower(char_type c) const
  c100 = gctype.tolower(c00);
  VERIFY( c100 == c10 );

  // char_type toupper(char_type* low, const char_type* hi) const
  std::char_traits<char_type>::copy(c_array, strlit02, len + 1);
  gctype.toupper(c_array, c_array + len);
  VERIFY( !std::char_traits<char_type>::compare(c_array, strlit01, len - 1) );

  // char_type tolower(char_type* low, const char_type* hi) const
  std::char_traits<char_type>::copy(c_array, strlit01, len + 1);
  gctype.tolower(c_array, c_array + len);
  VERIFY( !std::char_traits<char_type>::compare(c_array, strlit02, len - 1) );


#ifdef DEBUG_ASSERT
  assert(test);
#endif
}
#endif /* !defined(_GLIBCPP_USE_WCHAR_T) */

int main() {
#if _GLIBCPP_USE_WCHAR_T
  test01();
#endif /* !defined(_GLIBCPP_USE_WCHAR_T) */
  return 0;
}
