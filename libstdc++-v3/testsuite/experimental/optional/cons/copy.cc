// { dg-options "-std=gnu++1y" }
// { dg-do run }

// Copyright (C) 2013 Free Software Foundation, Inc.
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

#include <experimental/optional>
#include <testsuite_hooks.h>

struct tracker
{
  tracker(int value) : value(value) { ++count; }
  ~tracker() { --count; }

  tracker(tracker const& other) : value(other.value) { ++count; }
  tracker(tracker&& other) : value(other.value)
  {
    other.value = -1;
    ++count;
  }

  tracker& operator=(tracker const&) = default;
  tracker& operator=(tracker&&) = default;

  int value;

  static int count;
};

int tracker::count = 0;

struct exception { };

struct throwing_copy
{
  throwing_copy() = default;
  throwing_copy(throwing_copy const&) { throw exception {}; }
};

int main()
{
  // [20.5.4.1] Constructors

  {
    std::experimental::optional<long> o;
    auto copy = o;
    VERIFY( !copy );
    VERIFY( !o );
  }

  {
    std::experimental::optional<long> o { std::experimental::in_place, 0x1234ABCDF1E2D3C4 };
    auto copy = o;
    VERIFY( copy );
    VERIFY( *copy == 0x1234ABCDF1E2D3C4 );
    VERIFY( o && o == 0x1234ABCDF1E2D3C4 );
  }

  {
    std::experimental::optional<tracker> o;
    auto copy = o;
    VERIFY( !copy );
    VERIFY( tracker::count == 0 );
    VERIFY( !o );
  }

  {
    std::experimental::optional<tracker> o { std::experimental::in_place, 333 };
    auto copy = o;
    VERIFY( copy );
    VERIFY( copy->value == 333 );
    VERIFY( tracker::count == 2 );
    VERIFY( o && o->value == 333 );
  }

  enum outcome { nothrow, caught, bad_catch };

  {
    outcome result = nothrow;
    std::experimental::optional<throwing_copy> o;

    try
    {
      auto copy = o;
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == nothrow );
  }

  {
    outcome result = nothrow;
    std::experimental::optional<throwing_copy> o { std::experimental::in_place };

    try
    {
      auto copy = o;
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == caught );
  }

  VERIFY( tracker::count == 0 );
}
