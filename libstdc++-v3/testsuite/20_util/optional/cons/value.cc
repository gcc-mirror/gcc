// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>

#include <vector>
#include <string>

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
    auto i = 0x1234ABCD;
    std::optional<long> o { i };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCD );
    VERIFY( i == 0x1234ABCD );
  }

  {
    auto i = 0x1234ABCD;
    std::optional<long> o = i;
    VERIFY( o );
    VERIFY( *o == 0x1234ABCD );
    VERIFY( i == 0x1234ABCD );
  }

  {
    auto i = 0x1234ABCD;
    std::optional<long> o = { i };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCD );
    VERIFY( i == 0x1234ABCD );
  }

  {
    auto i = 0x1234ABCD;
    std::optional<long> o { std::move(i) };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCD );
    VERIFY( i == 0x1234ABCD );
  }

  {
    auto i = 0x1234ABCD;
    std::optional<long> o = std::move(i);
    VERIFY( o );
    VERIFY( *o == 0x1234ABCD );
    VERIFY( i == 0x1234ABCD );
  }

  {
    auto i = 0x1234ABCD;
    std::optional<long> o = { std::move(i) };
    VERIFY( o );
    VERIFY( *o == 0x1234ABCD );
    VERIFY( i == 0x1234ABCD );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::optional<std::vector<int>> o { v };
    VERIFY( !v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::optional<std::vector<int>> o = v;
    VERIFY( !v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::optional<std::vector<int>> o { v };
    VERIFY( !v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::optional<std::vector<int>> o { std::move(v) };
    VERIFY( v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::optional<std::vector<int>> o = std::move(v);
    VERIFY( v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    std::vector<int> v = { 0, 1, 2, 3, 4, 5 };
    std::optional<std::vector<int>> o { std::move(v) };
    VERIFY( v.empty() );
    VERIFY( o->size() == 6 );
  }

  {
    tracker t { 333 };
    std::optional<tracker> o = t;
    VERIFY( o->value == 333 );
    VERIFY( tracker::count == 2 );
    VERIFY( t.value == 333 );
  }

  {
    tracker t { 333 };
    std::optional<tracker> o = std::move(t);
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
      std::optional<throwing_construction> o { t };
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
      std::optional<throwing_construction> o { t };
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
      std::optional<throwing_construction> o { std::move(t) };
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
      std::optional<throwing_construction> o { std::move(t) };
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == caught );
  }

  {
    std::optional<std::string> os = "foo";
    struct X
    {
      explicit X(int) {}
      X& operator=(int) {return *this;}
    };
    std::optional<X> ox{42};
    std::optional<int> oi{42};
    std::optional<X> ox2{oi};
    std::optional<std::string> os2;
    os2 = "foo";
    std::optional<X> ox3;
    ox3 = 42;
    std::optional<X> ox4;
    ox4 = oi;
  }
}
