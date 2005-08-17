// 2004-12-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004 Free Software Foundation, Inc.
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

// 
// NB: This file is for testing tr1/type_traits with NO OTHER INCLUDES.

#include <tr1/type_traits>

// { dg-do compile }

void test01()
{
  // Check for required typedefs
  typedef std::tr1::true_type                 true_type;
  typedef std::tr1::false_type                false_type;

  typedef true_type::value_type               true_value_type;
  typedef true_type::type                     true_type;
  typedef true_type::type::value_type         true_type_value_type;
  typedef true_type::type::type               true_type_type;
  
  typedef false_type::value_type              false_value_type;
  typedef false_type::type                    false_type;
  typedef false_type::type::value_type        false_type_value_type;
  typedef false_type::type::type              false_type_type;
}
