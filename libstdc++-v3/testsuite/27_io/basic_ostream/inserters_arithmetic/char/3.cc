// 1999-11-15 Kevin Ediger  <kediger@licor.com>
// test the floating point inserters (facet num_put)

// Copyright (C) 1999, 2002, 2003 Free Software Foundation, Inc.
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

#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

using namespace std;

template<typename T>
bool
test03_check(T n)
{
  stringbuf strbuf;
  ostream o(&strbuf);
  const char *expect;
  bool test __attribute__((unused)) = true;

  if (numeric_limits<T>::digits + 1 == 16)
    expect = "177777 ffff";
  else if (numeric_limits<T>::digits + 1 == 32)
    expect = "37777777777 ffffffff";
  else if (numeric_limits<T>::digits + 1 == 64)
    expect = "1777777777777777777777 ffffffffffffffff";
  else
    expect = "wow, you've got some big numbers here";

  o << oct << n << ' ' << hex << n;
  VERIFY ( strbuf.str() == expect );

  return test;
}

void
test03()
{
  short s = -1;
  int i = -1;
  long l = -1;

  test03_check (s);
  test03_check (i);
  test03_check (l);
}

int 
main()
{
  test03();
  return 0;
}
