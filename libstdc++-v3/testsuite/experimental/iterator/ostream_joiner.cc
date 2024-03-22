// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }
// { dg-add-options no_pch }

#include <experimental/iterator>

#ifndef __cpp_lib_experimental_ostream_joiner
# error Feature-test macro is not defined.
#elif __cpp_lib_experimental_ostream_joiner < 201411
# error Feature-test macro has bad value.
#endif

#include <sstream>
#include <testsuite_hooks.h>

using std::experimental::ostream_joiner;

void
test01()
{
  std::ostringstream os;
  ostream_joiner<int> joiner{os, 9};
  for (int i : { 1, 2, 3, 4, 5 })
    *joiner++ = i;
  VERIFY( os.str() == "192939495" );
}

void
test02()
{
  std::ostringstream os;
  ostream_joiner<char> joiner{os, ','};
  for (int i : { 1, 2, 3, 4, 5 })
  {
    *joiner = i;
    ++joiner;
  }
  VERIFY( os.str() == "1,2,3,4,5" );
}

void
test03()
{
#if _GLIBCXX_USE_WCHAR_T
  std::wostringstream os;
  ostream_joiner<wchar_t, wchar_t> joiner{os, L','};
  for (int i : { 1, 2, 3, 4, 5 })
    *joiner++ = i;
  VERIFY( os.str() == L"1,2,3,4,5" );
#endif
}

int
main()
{
  test01();
  test02();
  test03();
}
