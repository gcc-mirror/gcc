// 2004-01-30  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation, Inc.
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

// 21.3.1 basic_string constructors.

#include <iterator>
#include <sstream>
#include <cstdlib>
#include <testsuite_hooks.h>

using namespace std;

wstring data(long len)
{
  wstring ret;
  for (long i = 0; i < len; ++i)
    ret.push_back(L'a' + rand() % 26);
  return ret;
}

void test01(int iter)
{
  bool test __attribute__((unused)) = true;

  for (long i = 0, j = 1; i < iter; ++i, j *= 3)
    {
      wistringstream isstr(data(j));

      wstring str((istreambuf_iterator<wchar_t>(isstr)),
		  istreambuf_iterator<wchar_t>());
      VERIFY( str == isstr.str() );
    }
}

int main()
{
  test01(13);
  return 0;
}
