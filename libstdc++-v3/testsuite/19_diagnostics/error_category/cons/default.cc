// { dg-options "-std=gnu++0x" }
// 2007-08-22 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2007 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <system_error>
#include <testsuite_hooks.h>

namespace __gnu_test
{
  struct test_category : public std::error_category
  { 
    virtual std::posix_error::posix_errno 
    posix(int __v) const
    { return std::posix_error::posix_errno(__v); }

    virtual const std::string& 
    name() const 
    { 
      static std::string s("__gnu_test::test_category");
      return s;
    }
  };

  struct test_derived_category : public test_category
  { 
    virtual const std::string& 
    name() const 
    { 
      static std::string s("__gnu_test::test_derived_category");
      return s;
    }
  };
}

int main()
{
  bool test __attribute__((unused)) = true;

  // 1
  __gnu_test::test_category c1;
  __gnu_test::test_derived_category c2;

  return 0;
}
