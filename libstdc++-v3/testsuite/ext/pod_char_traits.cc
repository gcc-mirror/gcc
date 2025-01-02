// POD character, std::char_traits specialization -*- C++ -*-

// Copyright (C) 2002-2025 Free Software Foundation, Inc.
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


// Gabriel Dos Reis <gdr@integrable-solutions.net>
// Benjamin Kosnik <bkoz@redhat.com>

#include <ext/pod_char_traits.h>
#include <testsuite_hooks.h>

int main()
{
  using namespace __gnu_cxx;

  typedef unsigned short			value_type;
  typedef unsigned int				int_type;
  typedef character<value_type, int_type>	char_type;
  typedef std::char_traits<char_type> 		traits_type;

  bool test = true;

  // 1 char_type <-> value_type conversions
  value_type uc1 = 'c';
  value_type uc2 = 'd';
  char_type c1 = { uc1 };
  char_type c2 = { uc2 };
  test = !(c1 == c2);
  VERIFY( test );

  // 2 char_traits
  test = traits_type::eq(c1, c2);
  VERIFY( ! test );

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
