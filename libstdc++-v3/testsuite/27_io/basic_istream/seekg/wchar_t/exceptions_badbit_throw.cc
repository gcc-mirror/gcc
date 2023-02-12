// Copyright (C) 2004-2023 Free Software Foundation, Inc.
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

void test01()
{
  __gnu_test::fail_wstreambuf bib;
  wistream stream(&bib);
  stream.exceptions(ios_base::badbit);

  wistream::pos_type pos;

  try
    {
      stream.seekg(pos);
      VERIFY( false );
    }
  catch (const __gnu_test::positioning_error&) 
    {
      // stream should set badbit and rethrow facet_error.
      VERIFY( stream.bad() );
      VERIFY( (stream.rdstate() & ios_base::failbit) == 0 );
      VERIFY( !stream.eof() );
    }
  catch (...) 
    {
      VERIFY(false);
    }
}

void test02()
{
  __gnu_test::fail_wstreambuf bib;
  wistream stream(&bib);
  stream.exceptions(ios_base::badbit);

  wistream::off_type off(5);

  try
    {
      stream.seekg(off, ios_base::cur);
      VERIFY( false );
    }
  catch (const __gnu_test::positioning_error&) 
    {
      // stream should set badbit and rethrow facet_error.
      VERIFY( stream.bad() );
      VERIFY( (stream.rdstate() & ios_base::failbit) == 0 );
      VERIFY( !stream.eof() );
    }
  catch (...) 
    {
      VERIFY(false);
    }
}

// libstdc++/9546
int main()
{
  test01();
  test02();
  return 0;
}
