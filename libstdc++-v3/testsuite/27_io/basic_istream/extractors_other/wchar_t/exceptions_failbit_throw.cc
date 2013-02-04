// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

using namespace std;

void test10()
{
  bool test __attribute__((unused)) = true;
  __gnu_test::fail_wstreambuf bib;
  wistream stream(&bib);
  stream.exceptions(ios_base::failbit);
  wstringbuf sbuf(L"", ios_base::out);

  try
    {
      stream >> &sbuf;
      VERIFY( false );
    }
  catch (ios_base::failure&)
    { VERIFY( false ); }
  catch (__gnu_test::underflow_error&)
    { }
  catch (...)
    { VERIFY( false ); }

  VERIFY( stream.rdstate() & ios_base::failbit );
  VERIFY( (stream.rdstate() & ios_base::badbit) == 0 );
}

void test12()
{
  bool test __attribute__((unused)) = true;
  wistringstream stream(L"foo, bar, qux");
  stream.exceptions(ios_base::failbit);
  __gnu_test::fail_wstreambuf bob;

  try
    {
      stream >> &bob;
      VERIFY( false );
    }
  catch (ios_base::failure&)
    { VERIFY( false ); }
  catch (__gnu_test::overflow_error&)
    { }
  catch (...)
    { VERIFY( false ); }

  VERIFY( stream.rdstate() & ios_base::failbit );
  VERIFY( (stream.rdstate() & ios_base::badbit) == 0 );
}

// libstdc++/9371
int main()
{
  test10();
  test12();
  return 0;
}
