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

// PR 14026
// 18.6.4 uncaught_exception

#include <cstdlib>
#include <exception>
#include <testsuite_hooks.h>

static void
no_uncaught ()
{
  if (std::uncaught_exception())
    abort();
}

int
main()
{
  try
    {
      throw 1;
    }
  catch (...)
    {
      try
        {
          throw;
        }
      catch (...)
        {
          no_uncaught ();
        }
    }
  no_uncaught();

  return 0;
}
