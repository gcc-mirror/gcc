// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }
// -*- C++ -*-
 
// Copyright (C) 2004-2016 Free Software Foundation, Inc.
 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 3, or (at
// your option) any later version.
 
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
 
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
 
 
// Benjamin Kosnik  <bkoz@redhat.com>

#include <ios>

// PR libstdc++/17922
// -Wall
typedef std::ios_base::iostate bitmask_type;

void
case_labels(bitmask_type b)
{
  switch (b) 
    {
    case std::ios_base::goodbit:
      break;
    case std::ios_base::badbit:
      break;
    case std::ios_base::eofbit:
      break;
    case std::ios_base::failbit:
      break;
    case std::_S_ios_iostate_end:
      break;
    case __INT_MAX__:
      break;
    case ~__INT_MAX__:
      break;
    }
  using underlying_type = std::underlying_type<bitmask_type>::type;
  static_assert( sizeof(underlying_type) == sizeof(int),
      "underlying type has same range of values as int");
}
