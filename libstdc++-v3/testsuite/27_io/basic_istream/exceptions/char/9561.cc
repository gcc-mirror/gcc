// 2003-03-08  Jerry Quinn  <jlquinn@optonline.net>

// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include <testsuite_io.h>

// libstdc++/9561
void test01()
{
  using namespace std;

  __gnu_test::fail_streambuf b;
  std::istream strm (&b);
  strm.exceptions (std::ios::badbit);
  int i = 0;

  try 
    {
      i = strm.get();
      i = strm.get();
      i = strm.get();
    }
  catch (__gnu_test::underflow_error&) 
    {
      // strm should throw facet_error and not do anything else
      VERIFY(strm.bad());
    }
  catch (...) 
    {
      VERIFY(false);
    }

  VERIFY(i == 's');
}


int main()
{
  test01();
  return 0;
}
