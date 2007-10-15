// { dg-options "-std=gnu++0x" }

// 2007-10-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007 Free Software Foundation, Inc.
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

#include <array>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::array<int, 7> a;
  VERIFY( a.cbegin() == a.begin() );
  VERIFY( a.cend() == a.end() );
  VERIFY( a.crbegin() == a.rbegin() );
  VERIFY( a.crend() == a.rend() );
  VERIFY( a.cbegin() != a.cend() );
  VERIFY( a.crbegin() != a.crend() );  
}

int main()
{
  test01();
  return 0;
}
