// 2004-07-07  Paolo Carlini  <pcarlini@suse.de>

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

// 27.7.1.3 basic_stringbuf overridden virtual functions.

#include <sstream>
#include <cstdlib>
#include <testsuite_hooks.h>

using namespace std;

string
data(unsigned len)
{
  string ret;
  for (unsigned i = 0; i < len; ++i)
    ret.push_back('a' + rand() % 26);
  return ret;
}

void
test01(unsigned iter)
{
  bool test __attribute__((unused)) = true;

  for (unsigned n = 1; n <= iter; n *= 10)
    {
      const string str = data(n);
      stringbuf sstr;
      for (unsigned i = 0; i < n; ++i)
	sstr.sputc(str[i]);
      VERIFY( str == sstr.str() );
    }
}

int main()
{
  test01(10000000);
  return 0;
}
