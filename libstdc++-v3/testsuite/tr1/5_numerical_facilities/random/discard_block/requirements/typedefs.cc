// { dg-do compile }
//
// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 5.1.4.5 Class template linear_congruential
// 5.1.1 [1] Table 15

#include <tr1/random>

void
test01() 
{ 
  using namespace std::tr1;
  
  typedef discard_block
    <
    subtract_with_carry<long, (1 << 24), 10, 24>,
    389, 24
    > test_type;
  
  typedef test_type::result_type result_type;
}
