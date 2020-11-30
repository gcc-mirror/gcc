// { dg-do compile { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2010-06-10  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.10 Class template sub_match

#include <regex>

void
test01()
{
  typedef std::match_results<char*> mr;

  typedef mr::value_type       value_type;
  typedef mr::const_reference  const_reference;
  typedef mr::reference        reference;
  typedef mr::const_iterator   const_iterator;
  typedef mr::iterator         iterator;
  typedef mr::difference_type  difference_type;
  typedef mr::size_type        size_type;
  typedef mr::allocator_type   allocator_type;
  typedef mr::char_type        char_type;
  typedef mr::string_type      string_type;

  static_assert(std::is_same<value_type, std::sub_match<char*>>::value, "");
  static_assert(std::is_same<const_reference, const value_type&>::value, "");
  static_assert(std::is_same<reference, value_type&>::value, "DR 2306");
  static_assert(std::is_same<const_iterator, iterator>::value, "");
  static_assert(std::is_same<char_type, char>::value, "");
  static_assert(std::is_same<string_type, std::string>::value, "");
}
