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
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

class gnu_ctype: public std::ctype<char> {};

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

  // bool is(mask m, char c) const;
  test &= gctype.is(std::ctype_base::space, c30);
  test &= gctype.is(std::ctype_base::upper, c00);
  test &= gctype.is(std::ctype_base::lower, c10);
  test &= gctype.is(std::ctype_base::digit, c20);
  test &= gctype.is(std::ctype_base::punct, c40);
  test &= gctype.is(std::ctype_base::alpha, c50);
  test &= gctype.is(std::ctype_base::alpha, c60);
  test &= gctype.is(std::ctype_base::xdigit, c20);
  test &= !gctype.is(std::ctype_base::xdigit, c80);
  test &= gctype.is(std::ctype_base::alnum, c50);
  test &= gctype.is(std::ctype_base::alnum, c20);
  test &= gctype.is(std::ctype_base::graph, c40);
  test &= gctype.is(std::ctype_base::graph, c20);

  // char toupper(char c) const
  c100 = gctype.toupper(c10);
  test &= c100 == c00;

  // char tolower(char c) const
  c100 = gctype.tolower(c00);
  test &= c100 == c10;

  // char toupper(char* low, const char* hi) const
  std::char_traits<char>::copy(c_array, strlit02, len + 1);
  gctype.toupper(c_array, c_array + len);
  test &= !std::char_traits<char>::compare(c_array, strlit01, len - 1);

  // char tolower(char* low, const char* hi) const
  std::char_traits<char>::copy(c_array, strlit01, len + 1);
  gctype.tolower(c_array, c_array + len);
  test &= !std::char_traits<char>::compare(c_array, strlit02, len - 1);


#ifdef DEBUG_ASSERT
  assert(test);
#endif
}

int main() {
  test01();
  return 0;
}


