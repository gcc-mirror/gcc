// 2005-06-07 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

typedef std::basic_streambuf<char> 	streambuf_type;

struct testbuf : streambuf_type
{
  testbuf() { }
};

void test05() 
{
  typedef streambuf_type::int_type 	int_type;
  typedef streambuf_type::traits_type 	traits_type;
  typedef streambuf_type::pos_type 	pos_type;
  typedef streambuf_type::off_type 	off_type;
  typedef size_t 			size_type;

  bool test __attribute__((unused)) = true;
  std::streamoff  			strmof_1;
  testbuf	sb01;

  // int in_avail()
  strmof_1 = sb01.in_avail();
  VERIFY( strmof_1  == 0 ); 
}

int main() 
{
  test05();
  return 0;
}
