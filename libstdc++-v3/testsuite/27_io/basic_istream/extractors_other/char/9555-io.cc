// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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
  buf b;

  testthrow(&b);

  return 0;
}
