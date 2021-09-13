// { dg-options "-Wno-deprecated" }
// { dg-do run { target c++11 } }

// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

// PR libstdc++/62258

#include <exception>
#include <testsuite_hooks.h>

struct check_on_destruct
{
  ~check_on_destruct();
};

check_on_destruct::~check_on_destruct()
{
  VERIFY(std::uncaught_exception());
}

int main ()
{
  VERIFY(!std::uncaught_exception());

  try
    {
      check_on_destruct check;

      try
        {
          throw 1;
        }
      catch (...)
        {
          VERIFY(!std::uncaught_exception());

          std::rethrow_exception(std::current_exception());
        }
    }
  catch (...)
    {
      VERIFY(!std::uncaught_exception());
    }

  VERIFY(!std::uncaught_exception());
}
