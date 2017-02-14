// 2004-09-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/10975
void test01()
{
  using namespace std;
  typedef wstreambuf::pos_type pos_type;
  typedef wstreambuf::off_type off_type;

  const pos_type good = pos_type(off_type(0));
  const pos_type bad = pos_type(off_type(-1));
  pos_type p;

  wstringbuf sbuf;
  
  p = sbuf.pubseekoff(0, ios_base::cur, ios_base::in);
  VERIFY( p == good );

  p = sbuf.pubseekoff(0, ios_base::beg, ios_base::out);
  VERIFY( p == good );

  p = sbuf.pubseekoff(0, ios_base::end);
  VERIFY( p == good );

  p = sbuf.pubseekoff(0, ios_base::cur);
  VERIFY( p == bad );
}

int main() 
{
  test01();
  return 0;
}
