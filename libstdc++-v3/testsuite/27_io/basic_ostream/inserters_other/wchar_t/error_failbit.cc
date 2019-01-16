// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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


#include <ostream>
#include <streambuf>
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

using namespace std;

void test5()
{
  __gnu_test::fail_wstreambuf bob;
  wostream stream(&bob);
  wstringbuf sbuf(L"Foo, bar, qux", ios_base::in);

  stream << &sbuf;

  VERIFY( stream.rdstate() & ios_base::failbit );
  VERIFY( (stream.rdstate() & ios_base::badbit) == 0 );
}

void test7()
{
  wostringstream stream;
  __gnu_test::fail_wstreambuf bib;

  stream << &bib;

  VERIFY( stream.rdstate() & ios_base::failbit );
  VERIFY( (stream.rdstate() & ios_base::badbit) == 0 );
}

// libstdc++/9371
int main()
{
  test5();
  test7();
  return 0;
}
