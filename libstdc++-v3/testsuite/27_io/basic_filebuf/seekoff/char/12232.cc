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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <testsuite_hooks.h>

const char name[] = "tmp_12232";

// libstdc++/12232
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  filebuf fbout;
  fbout.open(name, ios_base::out);
  fbout.sputn("abc", 3);
  
  streampos p1 = fbout.pubseekoff(0, ios_base::cur, ios_base::in);
  VERIFY( p1 != streampos(-1) );
  fbout.sputn("de", 2);
  
  streampos p2 = fbout.pubseekpos(p1, ios_base::openmode());
  VERIFY( p2 != streampos(-1) );
  fbout.sputn("34", 2);
  
  streampos p3 = fbout.pubseekoff(0, ios_base::beg, ios_base::ate);
  VERIFY( p3 != streampos(-1) );
  fbout.sputn("012", 3);
  
  fbout.close();
  
  filebuf fbin;
  fbin.open(name, ios_base::in);
  
  streampos p4 = fbin.pubseekoff(0, ios_base::beg, ios_base::ate);
  VERIFY( p4 != streampos(-1) );
  VERIFY( fbin.sgetc() == '0' );
  
  streampos p5 = fbin.pubseekoff(-1, ios_base::end, ios_base::out);
  VERIFY( p5 != streampos(-1) );
  VERIFY( fbin.sbumpc() == '4' );
  
  streampos p6 = fbin.pubseekpos(p4, ios_base::binary);
  VERIFY( p6 != streampos(-1) );
  VERIFY( fbin.sbumpc() == '0' );
  
  fbin.close();
}

int main()
{
  void test01();
  return 0;
}
