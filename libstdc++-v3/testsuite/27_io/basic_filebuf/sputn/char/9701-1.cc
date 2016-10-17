// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

#include <fstream>
#include <testsuite_hooks.h>

const char name_05[] = "filebuf_virtuals-5.txt"; // empty file, need to create

bool over_called;

class Derived_filebuf : public std::filebuf
{
public:
  int_type overflow(int_type c)
  {
    over_called = true;
    return std::filebuf::overflow(c);
  }
  
  const char_type* pub_epptr() const
  {
    return epptr();
  }
  
  const char_type* pub_pptr() const
  {
    return pptr();
  }
};

// libstdc++/9701 (partial)
void test11()
{
  bool over_expected;

  // sputn
  Derived_filebuf dfbuf_02;
  dfbuf_02.open(name_05, std::ios_base::out);
  over_called = false;
  dfbuf_02.sputn("sonne's", 7);
  VERIFY( over_called );
  over_expected = dfbuf_02.pub_epptr() == dfbuf_02.pub_pptr();
  over_called = false;
  dfbuf_02.sputn(" peak", 5);
  VERIFY( (!over_expected && !over_called)
	  || (over_expected && over_called) );
  dfbuf_02.close();
}

int main() 
{
  test11();
  return 0;
}
