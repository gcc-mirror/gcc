// 1999-09-20 bkoz

// Copyright (C) 1999, 2001, 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.4.3 template class fpos

#include <cwchar> // for mbstate_t
#include <ios>
#include <testsuite_hooks.h>

// 27.4.3.2 fpos requirements/invariants
void test02()
{
  bool test __attribute__((unused)) = true;

  typedef std::mbstate_t state_type;

  std::streamoff off01;
  std::streamoff off02 = 997;
  int i02 = 999;

  // p(i), p = i
  std::streampos pos01(i02);
  std::streampos pos02 = i02;
  VERIFY( pos01 == pos02 );
  
  // p(o), p = o 
  // NB: P(o) is only required.
  std::streampos pos03(off02);
  std::streampos pos04 = off02;
  VERIFY( pos03 == pos04 );
  
  // O(p)
  std::streamoff off03(pos04);
  VERIFY( off03 == off02 );

  // p == q, p!= q
  VERIFY( pos01 == pos02 );
  VERIFY( pos02 != pos03 );

  // q = p + o
  // p += o
  pos03 = pos03 + off02;
  pos04 += off02;
  VERIFY( pos03 == pos04 );
  std::streampos pos05 = pos03;
  std::streampos pos06 = pos03 + off02;
  VERIFY ( pos05 == pos03 );

  // q = p - o
  // p -= o
  pos03 = pos03 - off02;
  pos04 -= off02;
  VERIFY( pos03 == pos04 );
  std::streampos pos07 = pos03;
  std::streampos pos08 = pos03 - off02;
  VERIFY ( pos07 == pos03 );

  // o = p - q
  VERIFY( 0 == pos03 - pos04 );

  // streamsize -> streamoff
  // streamoff -> streamsize 
  off01 = off02;
  std::streamsize size01(off02);
  std::streamoff off04(size01);
  VERIFY( off01 == off04 );
} 

int main() 
{
  test02();
  return 0;
}
