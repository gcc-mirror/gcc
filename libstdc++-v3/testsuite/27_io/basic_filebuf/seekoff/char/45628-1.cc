// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "tmp_seekoff_45628.tst";

unsigned underflows, overflows;

class my_filebuf
: public std::filebuf
{
  virtual int_type
  underflow()
  {
    ++underflows;
    return std::filebuf::underflow();
  }
  virtual int_type
  overflow(int_type c)
  {
    ++overflows;
    return std::filebuf::overflow(c);
  }
};

// libstdc++/45628
void test01()
{
  my_filebuf q;
  q.open(name_01, std::ios_base::in | std::ios_base::out 
	 | std::ios_base::trunc); 

  q.sputc('a');
  q.pubseekoff(0, std::ios_base::cur);
  q.sputc('b');
  q.pubseekoff(0, std::ios_base::cur);
  q.sputc('c');
  q.pubseekoff(0, std::ios_base::cur);

  VERIFY( overflows <= 1 ); // only initial sputc allowed to overflow
  q.pubseekoff(0, std::ios_base::beg);

  q.sbumpc();
  VERIFY( underflows == 1 );

  q.pubseekoff(0, std::ios_base::cur);
  q.sbumpc();
  q.pubseekoff(0, std::ios_base::cur);
  q.sbumpc();
  q.pubseekoff(0, std::ios_base::cur);

  VERIFY( underflows == 1 );
}

int main()
{
  test01();
  return 0;
}
