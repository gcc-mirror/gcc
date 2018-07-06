// Copyright (C) 2000-2018 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <testsuite_hooks.h>

typedef char char_type;
class gnu_ctype: public std::ctype<char_type> { };

void test01()
{
  const char_type strlit00[] = "manilla, cebu, tandag PHILIPPINES";
  const char_type strlit01[] = "MANILLA, CEBU, TANDAG PHILIPPINES";
  const char_type strlit02[] = "manilla, cebu, tandag philippines";
  const char_type c00 = 'S';
  const char_type c10 = 's';

  gnu_ctype gctype;
  char_type c100;
  int len = std::char_traits<char_type>::length(strlit00);
  char_type c_array[len + 1];

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
}

int main() 
{
  test01();
  return 0;
}
