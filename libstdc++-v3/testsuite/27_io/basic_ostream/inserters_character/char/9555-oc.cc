// Copyright (C) 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <ostream>
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
  bool test __attribute__((unused)) = true;
  buf b;
  std::ostream os(&b);
  os.exceptions(std::ios::badbit);

  try 
    {
      os << arg;
    }
  catch(int) 
      {
	// Expected return is zero.
        VERIFY( os.bad() );
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
  const char* ccp = "governor ann richards";
  const signed char* cscp = reinterpret_cast<const signed char*>(ccp);
  const unsigned char* cucp = reinterpret_cast<const unsigned char*>(ccp);

  testthrow(c);
  testthrow(uc);
  testthrow(sc);
  testthrow(ccp);
  testthrow(cscp);
  testthrow(cucp);

  return 0;
}
