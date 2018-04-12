// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-options "-D_GLIBCXX_USE_CXX11_ABI=0" }
// { dg-do run { target c++11 } }

#include <fstream>
#include <system_error>
#include <testsuite_hooks.h>

void
test01()
{
  using std::ios;
  bool caught_ios_failure = false;
  bool rethrown = false;
  bool caught_system_error = false;
  try {
    std::ifstream f;
    f.exceptions(ios::failbit | ios::badbit | ios::eofbit);
    try {
      f.get();
    }
    catch (const ios::failure&) // catch as old ABI type
    {
      caught_ios_failure = true;
#if _GLIBCXX_USE_DUAL_ABI || _GLIBCXX_USE_CXX11_ABI == 1
      rethrown = true;
      throw; // re-throw, to catch as new ABI type
#endif
    }
  }
  catch (const std::system_error& e)
  {
    caught_system_error = true;
  }

  VERIFY( caught_ios_failure );
  if (rethrown)
    VERIFY( caught_system_error );
}

void
test02()
{
  using std::ios;
  const std::exception* p = nullptr;
  bool caught_ios_failure = false;
  bool caught_exception = false;
  try {
    std::ifstream f;
    f.exceptions(ios::failbit | ios::badbit | ios::eofbit);
    try {
      f.get();
    }
    catch (const std::exception& e1)
    {
      caught_exception = true;
      p = &e1;
      throw;
    }
  }
  catch (const ios::failure& e2)
  {
    caught_ios_failure = true;
#if _GLIBCXX_USE_DUAL_ABI
    // If the Dual ABI is active the library throws the new type,
    // so e1 was an object of that new type and so &e1 != &e2.
    VERIFY( p != &e2 );
#else
    // Otherwise there's only one type of ios::failure, so &e1 == &e2.
    VERIFY( p == &e2 );
#endif
  }

  VERIFY( caught_exception );
  VERIFY( caught_ios_failure );
}

int
main()
{
  test01();
  test02();
}
