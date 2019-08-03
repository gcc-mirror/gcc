// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include "../../../../../include/std/pmroptional"

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

  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  tracker(std::allocator_arg_t,allocator_type, int i)
     : tracker(i){}
  tracker(std::allocator_arg_t,allocator_type, const tracker& other)
     : tracker(other){}
  tracker(std::allocator_arg_t,allocator_type, tracker&& other)
       : tracker(std::move(other)){}

  tracker& operator=(tracker const&) = default;
  tracker& operator=(tracker&&) = default;

  int value;

  static int count;
};

int tracker::count = 0;

struct exception { };

struct throwing_move
{
  throwing_move() = default;
  throwing_move(throwing_move const&) { throw exception {}; }

  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  throwing_move(std::allocator_arg_t,allocator_type)
    : throwing_move(){}

  throwing_move(std::allocator_arg_t,allocator_type, throwing_move&& other)
    : throwing_move(std::move(other)){}
};

struct value_type
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  value_type(std::allocator_arg_t,allocator_type)
  : value_type(){}


  value_type(std::allocator_arg_t,allocator_type, long i)
  : value_type(i){}

  value_type(std::allocator_arg_t,allocator_type, value_type const& other)
    : value_type(other){}

  value_type(){};

  value_type(long _i) : i(_i){};

  value_type(value_type const& other): i(other.i)
      {};

  value_type& operator=(value_type const& other)
  {
    i = other.i;
    return *this;
  }
  long i = 0;
};
int main()
{
  // [20.5.4.1] Constructors

  {
    std::pmr::optional<value_type> o;
    auto moved_to = std::move(o);
    VERIFY( !moved_to );
    VERIFY( !o );
  }

  {
    const long val = 0x1234ABCD;
    std::pmr::optional<value_type> o { std::in_place, val};
    auto moved_to = std::move(o);
    VERIFY( moved_to );
    VERIFY( (*moved_to).i == val );
    VERIFY( o && o->i == val );
  }

  {
    std::pmr::optional<tracker> o;
    auto moved_to = std::move(o);
    VERIFY( !moved_to );
    VERIFY( tracker::count == 0 );
    VERIFY( !o );
  }

  {
    std::pmr::optional<tracker> o { std::in_place, 333 };
    auto moved_to = std::move(o);
    VERIFY( moved_to );
    VERIFY( moved_to->value == 333 );
    VERIFY( tracker::count == 2 );
    VERIFY( o && o->value == -1 );
  }

  enum outcome { nothrow, caught, bad_catch };

  {
    outcome result = nothrow;
    std::pmr::optional<throwing_move> o;

    try
    {
      auto moved_to = std::move(o);
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == nothrow );
  }

  {
    outcome result = nothrow;
    std::pmr::optional<throwing_move> o { std::in_place };

    try
    {
      auto moved_to = std::move(o);
    }
    catch(exception const&)
    { result = caught; }
    catch(...)
    { result = bad_catch; }

    VERIFY( result == caught );
  }

  VERIFY( tracker::count == 0 );
}
