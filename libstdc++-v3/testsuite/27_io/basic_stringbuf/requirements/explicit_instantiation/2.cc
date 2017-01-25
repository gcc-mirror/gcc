// { dg-do compile }
// 1999-01-17 bkoz test functionality of basic_filebuf for char_type == char

// Copyright (C) 1997-2017 Free Software Foundation, Inc.
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

// 27.7.1 - Template class basic_stringbuf
// NB: This file is for testing basic_filebuf with NO OTHER INCLUDES.

#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// test05
// libstdc++/1886
// should be able to instantiate basic_stringbuf for non-standard types.
namespace std
{
  using __gnu_test::pod_char;
  typedef short type_t;
  template class basic_stringbuf<type_t, char_traits<type_t> >;
  template class basic_stringbuf<pod_char, char_traits<pod_char> >;
} // std

// more surf!!!









