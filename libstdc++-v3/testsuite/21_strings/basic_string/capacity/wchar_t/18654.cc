// 2004-11-29  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

// 21.3.3 string capacity

// { dg-options "-DMAX_SIZE=16" { target simulator } }

#ifndef MAX_SIZE
#define MAX_SIZE 20
#endif

#include <string>
#include <testsuite_hooks.h>

// libstdc++/18654
void test01()
{
  using namespace std;

  typedef wstring::size_type size_type;

#if _GLIBCXX_USE_CXX11_ABI
  // Can't shrink below small string size.
  const size_type minsize = wstring().capacity() + 1;
#else
  // Exact shrink-to-size and shrink-to-fit
  const size_type minsize = 2 << 0;
#endif
  const size_type maxsize = 2 << MAX_SIZE;
  for (size_type i = minsize; i <= maxsize; i *= 2)
    {
      wstring str(i, L'x');
      str.reserve(3 * i);

      str.reserve(2 * i);
      VERIFY( str.capacity() == 2 * i );

      str.reserve();
      VERIFY( str.capacity() == i );
    }
}

int main() 
{
  test01();
  return 0;
}
