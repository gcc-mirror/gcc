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

#include <istream>
#include <streambuf>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// libstdc++/9561
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  __gnu_test::fail_wstreambuf b;
  std::wistream strm (&b);
  strm.exceptions (std::wios::badbit);
  wchar_t i = 0;

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
      VERIFY( false );
    }

  VERIFY( i == L's' );
}


int main()
{
  test01();
  return 0;
}
