// -*- C++ -*-
//
// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#ifndef _TESTSUITE_COUNTER_TYPE_H
#define _TESTSUITE_COUNTER_TYPE_H 1

namespace __gnu_test
{
  // Type counting how many constructors or assign operators are invoked.
  struct counter_type
  {
    // Constructor counters:
    static int default_count;
    static int specialize_count;
    static int copy_count;
    static int copy_assign_count;
    static int less_compare_count;
#if __cplusplus >= 201103L
    static int move_count;
    static int move_assign_count;
#endif
    static int destructor_count;

    int val;
    
    counter_type() : val(0)
    { ++default_count; }

    counter_type(int inval) : val(inval)
    { ++specialize_count; }

    counter_type(const counter_type& in) : val(in.val)
    { ++copy_count; }

    ~counter_type()
    { ++destructor_count; }

    counter_type&
    operator=(const counter_type& in)
    {
      val = in.val;
      ++copy_assign_count;
      return *this;
    }

#if __cplusplus >= 201103L
    counter_type(counter_type&& in) noexcept
    {
      val = in.val;
      ++move_count;
    }

    counter_type&
    operator=(counter_type&& rhs) noexcept
    {
      val = rhs.val;
      ++move_assign_count;
      return *this;
    }
#endif

    static void
    reset()
    {
      default_count = 0;
      specialize_count = 0;
      copy_count = 0;
      copy_assign_count = 0;
      less_compare_count = 0;
#if __cplusplus >= 201103L
      move_count = 0;
      move_assign_count = 0;
#endif
      destructor_count = 0;
    }

    bool operator==(const counter_type& rhs) const
    { return val == rhs.val; }

    bool operator<(const counter_type& rhs) const
    { return val < rhs.val; }
  };

  int counter_type::default_count = 0;
  int counter_type::specialize_count = 0;
  int counter_type::copy_count = 0;
  int counter_type::copy_assign_count = 0;
  int counter_type::less_compare_count = 0;

#if __cplusplus >= 201103L
  int counter_type::move_count = 0;
  int counter_type::move_assign_count = 0;
#endif
  int counter_type::destructor_count = 0;

  struct counter_type_hasher
  {
    std::size_t operator()(const counter_type& c) const
    {
      return c.val;
    }
  };

} // namespace __gnu_test
#endif
