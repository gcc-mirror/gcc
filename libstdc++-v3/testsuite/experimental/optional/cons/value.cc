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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <experimental/optional>
#include <testsuite_hooks.h>

#include <vector>

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

struct throwing_construction
{
  explicit throwing_construction(bool propagate) : propagate(propagate) { }

  throwing_construction(throwing_construction const& other)
  : propagate(other.propagate)
  {
    if(propagate)
      throw exception {};
  }

  bool propagate;
};

int main()
{
  // [20.5.4.1] Constructors

  {
    auto i = 0x1234ABCDF1E2D3C4;
    std::experimental::optional<long> o { i };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCDF1E2D3C4 );
    VERIFY( i == 0x1234ABCDF1E2D3C4 );
  }

  {
    auto i = 0x1234ABCDF1E2D3C4;
    std::experimental::optional<long> o = i;
    VERIFY( o );
    VERIFY( *o == 0x1234ABCDF1E2D3C4 );
    VERIFY( i == 0x1234ABCDF1E2D3C4 );
  }

  {
    auto i = 0x1234ABCDF1E2D3C4;
    std::experimental::optional<long> o = { i };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCDF1E2D3C4 );
    VERIFY( i == 0x1234ABCDF1E2D3C4 );
  }

  {
    auto i = 0x1234ABCDF1E2D3C4;
    std::experimental::optional<long> o { std::move(i) };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCDF1E2D3C4 );
    VERIFY( i == 0x1234ABCDF1E2D3C4 );
  }

  {
    auto i = 0x1234ABCDF1E2D3C4;
    std::experimental::optional<long> o = std::move(i);
    VERIFY( o );
    VERIFY( *o == 0x1234ABCDF1E2D3C4 );
    VERIFY( i == 0x1234ABCDF1E2D3C4 );
  }

  {
    auto i = 0x1234ABCDF1E2D3C4;
    std::experimental::optional<long> o = { std::move(i) };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCDF1E2D3C4 );
    VERIFY( i == 0x1234ABCDF1E2D3C4 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::experimental::optional<std::vector<int>> o { v };
    VERIFY( !v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::experimental::optional<std::vector<int>> o = v;
    VERIFY( !v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::experimental::optional<std::vector<int>> o { v };
    VERIFY( !v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::experimental::optional<std::vector<int>> o { std::move(v) };
    VERIFY( v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::experimental::optional<std::vector<int>> o = std::move(v);
    VERIFY( v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::experimental::optional<std::vector<int>> o { std::move(v) };
    VERIFY( v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    tracker t { 333 };
    std::experimental::optional<tracker> o = t;
    VERIFY( o->value == 333 );
    VERIFY( tracker::count == 2 );
    VERIFY( t.value == 333 );
  }

  {
    tracker t { 333 };
    std::experimental::optional<tracker> o = std::move(t);
    VERIFY( o->value == 333 );
    VERIFY( tracker::count == 2 );
    VERIFY( t.value == -1 );
  }

  enum outcome { nothrow, caught, bad_catch };

  {
    outcome result = nothrow;
    throwing_construction t { false };

    try
    {
      std::experimental::optional<throwing_construction> o { t };
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == nothrow );
  }

  {
    outcome result = nothrow;
    throwing_construction t { true };

    try
    {
      std::experimental::optional<throwing_construction> o { t };
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == caught );
  }

  {
    outcome result = nothrow;
    throwing_construction t { false };

    try
    {
      std::experimental::optional<throwing_construction> o { std::move(t) };
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == nothrow );
  }

  {
    outcome result = nothrow;
    throwing_construction t { true };

    try
    {
      std::experimental::optional<throwing_construction> o { std::move(t) };
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == caught );
  }
}
