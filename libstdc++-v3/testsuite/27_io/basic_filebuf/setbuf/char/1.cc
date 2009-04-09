// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2009 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

class testbuf : public std::filebuf
{
public:

  // Typedefs:
  typedef std::filebuf base_type;
  typedef base_type::traits_type traits_type;
  typedef base_type::char_type char_type;

  testbuf(): base_type() 
  { _M_mode = (std::ios_base::in | std::ios_base::out); }

  bool
  check_pointers()
  { 
    bool test __attribute__((unused)) = true;
    test = (this->pbase() == NULL);
    test &= (this->pptr() == NULL);
    return test;
  }
};

const char name_01[] = "filebuf_virtuals-1.txt";

// Test overloaded virtual functions.
void test05() 
{
  using namespace std;
  typedef std::filebuf::int_type 	int_type;
  typedef std::filebuf::traits_type 	traits_type;
  typedef std::filebuf::pos_type 	pos_type;
  typedef std::filebuf::off_type 	off_type;
  typedef size_t 			size_type;

  bool test __attribute__((unused)) = true;

  {
    testbuf 				f_tmp;

    // setbuf
    // pubsetbuf(char_type* s, streamsize n)
    f_tmp.pubsetbuf(0,0);
    VERIFY( f_tmp.check_pointers() );
  }

  {
    testbuf 				f_tmp;
    
    f_tmp.open(name_01, ios_base::out | ios_base::in);
    f_tmp.sbumpc();
    
    // setbuf
    // pubsetbuf(char_type* s, streamsize n)
    f_tmp.pubsetbuf(0, 0);
    VERIFY( f_tmp.check_pointers() );
  }
}

int main() 
{
  test05();
  return 0;
}
