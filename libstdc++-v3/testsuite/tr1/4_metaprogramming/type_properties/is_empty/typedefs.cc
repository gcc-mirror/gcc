// 2005-01-28  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 
// NB: This file is for testing tr1/type_traits with NO OTHER INCLUDES.

#include <tr1/type_traits>

// { dg-do compile }

void test01()
{
  // Check for required typedefs
  typedef std::tr1::is_empty<int>             test_type;
  typedef test_type::value_type               value_type;
  typedef test_type::type                     type;
  typedef test_type::type::value_type         type_value_type;
  typedef test_type::type::type               type_type;
}
