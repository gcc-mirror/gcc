// POD character, std::char_traits specialization -*- C++ -*-

// Copyright (C) 2002, 2003 Free Software Foundation, Inc.
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

// Gabriel Dos Reis <gdr@integrable-solutions.net>
// Benjamin Kosnik <bkoz@redhat.com>

#include <ext/pod_char_traits.h>

int main()
{
  using namespace __gnu_cxx;

  typedef unsigned short			value_type;
  typedef unsigned int				int_type;
  typedef character<value_type, int_type>	char_type;
  typedef std::char_traits<char_type> 		traits_type;

  bool test __attribute__((unused)) = true;

  // 1 char_type <-> value_type conversions
  value_type uc1 = 'c';
  value_type uc2 = 'd';
  char_type c1 = { uc1 };
  char_type c2 = { uc2 };
  test = !(c1 == c2);

  // 2 char_traits
  test = traits_type::eq(c1, c2);
  
  // 3 basic_string<char_type>
  typedef std::basic_string<char_type>	string_type;
  string_type str;
  char_type c3 = { value_type('b') };
  char_type c4 = { value_type('o') };
  char_type c5 = { value_type('r') };
  char_type c6 = { value_type('a') };
  char_type c7 = { value_type('c') };
  char_type c8 = { value_type('a') };
  char_type c9 = { value_type('y') };
  str += c3;
  str += c4;
  str += c5;
  str += c6;
  str += c7;
  str += c8;
  str += c9;
  string_type::size_type len __attribute__((unused)) = str.size();
  const char_type* arr __attribute__((unused)) = str.c_str();

  return 0;
}
