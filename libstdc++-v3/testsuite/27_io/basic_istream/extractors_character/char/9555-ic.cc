// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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


#include <istream>
#include <streambuf>
#include <testsuite_hooks.h>

struct buf: std::streambuf
{
  virtual int_type overflow(int_type) 
  { throw 0; }
};

template<typename T>
void testthrow(T arg)
{
  buf b;
  std::istream is(&b);
  is.exceptions(std::ios::badbit);

  try 
    {
      is >> arg;
    }
  catch(int) 
      {
	// Expected return is zero.
        VERIFY( is.bad() );
      }
  catch(...) 
    {
      VERIFY( false );
    }    
}

int main()
{
  char c = 'a';
  unsigned char uc = 'a';
  signed char sc = 'a';
  char* cp = &c;
  signed char* scp = &sc;
  unsigned char* ucp = &uc;

  testthrow(c);
  testthrow(uc);
  testthrow(sc);
  testthrow(cp);
  testthrow(scp);
  testthrow(ucp);

  return 0;
}
