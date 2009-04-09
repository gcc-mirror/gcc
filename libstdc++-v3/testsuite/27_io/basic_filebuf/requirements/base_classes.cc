// { dg-do compile }
// 2003-03-26 B enjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2003, 2009 Free Software Foundation, Inc.
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

// 27.8.1.1 - Template class basic_filebuf 

#include <fstream>

void test01()
{
  // Check for required base class.
  typedef std::filebuf test_type;
  typedef std::streambuf base_type;
  const test_type& obj = *new test_type();
  const base_type* base __attribute__((unused)) = &obj;
}

// more surf!!!









