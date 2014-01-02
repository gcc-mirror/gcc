// { dg-do compile }
// { dg-options "-std=c++0x" }

//
// 2010-06-07  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

// 28.9 Class template sub_match

#include <regex>


void
test01()
{
  typedef std::sub_match<char*> sm;

  typedef sm::value_type       value_type;
  typedef sm::difference_type  difference_type;
  typedef sm::iterator         iterator;
  typedef sm::string_type      string_type;
}
