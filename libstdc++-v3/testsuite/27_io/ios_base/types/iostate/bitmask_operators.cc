// { dg-do compile }
// -*- C++ -*-
 
// Copyright (C) 2004-2025 Free Software Foundation, Inc.
 
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
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::bitmask_operators<std::ios_base::iostate>();
}
// { dg-warning "ignoring return value.*operator\\|" "" { target c++11 } 0 }
// { dg-warning "ignoring return value.*operator&" "" { target c++11 } 0 }
// { dg-warning "ignoring return value.*operator\\^" "" { target c++11 } 0 }
// { dg-warning "ignoring return value.*operator~" "" { target c++11 } 0 }
