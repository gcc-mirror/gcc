// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 27.6.2.5.3 basic_ostream manipulator inserters
//
// _GLIBCXX_RESOLVE_LIB_DEFECTS
// DR 60. What is a formatted input function?
// Inserters for manipulators do not behave as formatted output functions.

#include <ostream>
#include <stdexcept>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

std::wostream& func1(std::wostream&)
{ throw std::runtime_error(""); }

std::wios& func2(std::wios&)
{ throw std::runtime_error(""); }

std::ios_base& func3(std::ios_base&)
{ throw std::runtime_error(""); }

template<typename T>
void test(T& (*f)(T&))
{
  __gnu_test::sync_wstreambuf buf;
  std::wostream os(&buf);
  
  __gnu_test::sync_wstreambuf buf_tie;
  std::wostream os_tie(&buf_tie);

  // No sentry should be constructed so os.tie()->flush() should not be
  // called.
  os.tie(&os_tie);

  try
    {
      os << f;
      // Exceptions thrown by f should not be caught
      VERIFY( false );
    }
  catch (std::runtime_error&)
    {
    }

  // Exceptions thrown by f should not cause badbit to be set
  VERIFY( os.good() );
  VERIFY( !buf_tie.sync_called() );

  // The manipulator should be called even if !os.good().
  os.setstate(std::ios_base::eofbit);

  try
    {
      os << f;
      // Exceptions thrown by f should not be caught
      VERIFY( false );
    }
  catch (std::runtime_error&)
    {
    }

  // Exceptions thrown by f should not cause badbit to be set
  VERIFY( os.rdstate() == std::ios_base::eofbit );
  VERIFY( !buf_tie.sync_called() );
}

void test05()
{
  test(&func1);
  test(&func2);
  test(&func3);
}

int main()
{
  test05();
  return 0;
}
