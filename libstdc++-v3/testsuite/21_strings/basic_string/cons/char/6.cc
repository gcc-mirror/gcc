// 2004-01-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

// 21.3.1 basic_string constructors.

#include <iterator>
#include <sstream>
#include <cstdlib>
#include <testsuite_hooks.h>

using namespace std;

string data(long len)
{
  string ret;
  for (long i = 0; i < len; ++i)
    ret.push_back('a' + rand() % 26);
  return ret;
}

void test01(int iter)
{
  for (long i = 0, j = 1; i < iter; ++i, j *= 3)
    {
      istringstream isstr(data(j));

      string str((istreambuf_iterator<char>(isstr)),
		 istreambuf_iterator<char>());
      VERIFY( str == isstr.str() );
    }
}

int main()
{
  test01(13);
  return 0;
}
