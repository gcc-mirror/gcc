// -*- C++ -*-
// Error handling utils for the C++ library testsuite.
//
// Copyright (C) 2007-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include <string>
#include <testsuite_hooks.h>

#ifndef _TESTSUITE_ERROR_H
#define _TESTSUITE_ERROR_H 1

namespace __gnu_test
{
  struct test_category : public std::error_category
  {
    virtual const char*
    name() const noexcept
    {
      const char* s = "__gnu_test::test_category";
      return s;
    }

    virtual std::string
    message(int) const
    { return std::string("message to be determined"); }
  };

  struct test_derived_category : public test_category
  {
    virtual const char*
    name() const noexcept
    {
      const char* s = "__gnu_test::test_derived_category";
      return s;
    }
  };

}
#endif
