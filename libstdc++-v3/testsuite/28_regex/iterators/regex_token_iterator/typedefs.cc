// { dg-do compile { target c++11 } }

//
// 2010-06-10  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// 28.12.2 Class template regex_token_iterator

#include <regex>

void
test01()
{
  typedef std::regex_token_iterator<char*> it;

  typedef it::regex_type         regex_type;
  typedef it::value_type         value_type;
  typedef it::difference_type    difference_type;
  typedef it::pointer            pointer;
  typedef it::reference          reference;
  typedef it::iterator_category  iterator_category;
}
