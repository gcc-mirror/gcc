// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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

// NOTE: This makes use of the fact that we know how moveable
// is implemented on list (via swap). If the implementation changed
// this test may begin to fail.

#include <list>
#include <utility>
#include <testsuite_hooks.h>

int main()
{
  bool test __attribute__((unused)) = true;

  std::list<int> a,b;
  a.push_back(1);
  b = std::move(a);
  VERIFY( b.size() == 1 && *b.begin() == 1 && a.size() == 0 );

  std::list<int> c(std::move(b));
  VERIFY( c.size() == 1 && *c.begin() == 1 );
  VERIFY( b.size() == 0 );
  return 0;
}
