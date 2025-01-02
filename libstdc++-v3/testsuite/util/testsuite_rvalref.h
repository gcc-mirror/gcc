// -*- C++ -*-
// Testing utilities for the rvalue reference.
//
// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_TESTSUITE_RVALREF_H
#define _GLIBCXX_TESTSUITE_RVALREF_H 1

#include <testsuite_hooks.h>
#include <bits/functional_hash.h>

namespace __gnu_test
{
  // This class is designed to test libstdc++'s template-based rvalue
  // reference support. It should fail at compile-time if there is an
  // attempt to copy it.
  struct rvalstruct
  {
    int val;
    bool valid;

    rvalstruct() : val(0), valid(true)
    { }

    rvalstruct(int inval) : val(inval), valid(true)
    { }

    rvalstruct&
    operator=(int newval)
    {
      val = newval;
      valid = true;
      return *this;
    }

    rvalstruct(const rvalstruct&) = delete;

    rvalstruct(rvalstruct&& in)
    {
      VERIFY( in.valid == true );
      val = in.val;
      in.valid = false;
      valid = true;
    }

    rvalstruct&
    operator=(const rvalstruct&) = delete;

    rvalstruct&
    operator=(rvalstruct&& in)
    {
      VERIFY( this != &in );
      VERIFY( in.valid == true );
      val = in.val;
      in.valid = false;
      valid = true;
      return *this;
    }
  };

  inline bool
  operator==(const rvalstruct& lhs, const rvalstruct& rhs)
  { return lhs.val == rhs.val; }

  inline bool
  operator<(const rvalstruct& lhs, const rvalstruct& rhs)
  { return lhs.val < rhs.val; }

  void
  swap(rvalstruct& lhs, rvalstruct& rhs)
  {
    VERIFY( lhs.valid && rhs.valid );
    int temp = lhs.val;
    lhs.val = rhs.val;
    rhs.val = temp;
  }

  // This is a moveable class which copies how many times it is copied.
  // This is mainly of use in the containers, where the an element inserted
  // into a container has to be copied once to get there, but we want to check
  // nothing else is copied.
  struct copycounter
  {
    static int copycount;
    int val;
    bool valid;

    copycounter() : val(0), valid(true)
    { }

    copycounter(int inval) : val(inval), valid(true)
    { }

    copycounter(const copycounter& in) : val(in.val), valid(true)
    {
      VERIFY( in.valid == true );
      ++copycount;
    }

    copycounter(copycounter&& in) noexcept
    {
      VERIFY( in.valid == true );
      val = in.val;
      in.valid = false;
      valid = true;
    }

    copycounter&
    operator=(int newval)
    {
      val = newval;
      valid = true;
      return *this;
    }

    bool
    operator=(const copycounter& in)
    {
      VERIFY( in.valid == true );
      ++copycount;
      val = in.val;
      valid = true;
      return true;
    }

    copycounter&
    operator=(copycounter&& in)
    {
      VERIFY(in.valid == true);
      val = in.val;
      in.valid = false;
      valid = true;
      return *this;
    }

    ~copycounter() noexcept
    { valid = false; }
  };

  int copycounter::copycount = 0;

  inline bool
  operator==(const copycounter& lhs, const copycounter& rhs)
  { return lhs.val == rhs.val; }

  inline bool
  operator<(const copycounter& lhs, const copycounter& rhs)
  { return lhs.val < rhs.val; }

  inline void
  swap(copycounter& lhs, copycounter& rhs)
  {
    VERIFY( lhs.valid && rhs.valid );
    int temp = lhs.val;
    lhs.val = rhs.val;
    rhs.val = temp;
  }

  // In the occasion of libstdc++/48038.
  struct rvalstruct_compare_by_value
  {
    int val;
    bool ok;

    rvalstruct_compare_by_value(int v)
    : val(v), ok(true) { }

    rvalstruct_compare_by_value(const rvalstruct_compare_by_value& rh)
    : val(rh.val), ok(rh.ok)
    {
      VERIFY(rh.ok);
    }

    rvalstruct_compare_by_value&
    operator=(const rvalstruct_compare_by_value& rh)
    {
      VERIFY( rh.ok );
      val = rh.val;
      ok = rh.ok;
      return *this;
    }

    rvalstruct_compare_by_value(rvalstruct_compare_by_value&& rh)
    : val(rh.val), ok(rh.ok)
    {
      VERIFY( rh.ok );
      rh.ok = false;
    }

    rvalstruct_compare_by_value&
    operator=(rvalstruct_compare_by_value&& rh)
    {
      VERIFY( rh.ok );
      val = rh.val;
      ok = rh.ok;
      rh.ok = false;
      return *this;
    }
  };

  inline bool
  operator<(rvalstruct_compare_by_value lh,
	    rvalstruct_compare_by_value rh)
  {
    VERIFY( rh.ok );
    VERIFY( lh.ok );
    return lh.val < rh.val;
  }

  inline bool
  order(rvalstruct_compare_by_value lh,
	rvalstruct_compare_by_value rh)
  {
    VERIFY( rh.ok );
    VERIFY( lh.ok );
    return lh.val < rh.val;
  }

  struct throwing_move_constructor
  {
    throwing_move_constructor() = default;

    throwing_move_constructor(throwing_move_constructor&&)
    { throw 1; }

    throwing_move_constructor(const throwing_move_constructor&) = default;

    throwing_move_constructor&
    operator=(const throwing_move_constructor&) = default;
  };

} // namespace __gnu_test

namespace std
{
  /// std::hash specialization for __gnu_test::rvalstruct.
  template<>
    struct hash<__gnu_test::rvalstruct>
    {
      typedef size_t                    result_type;
      typedef __gnu_test::rvalstruct  argument_type;

      size_t
      operator()(const __gnu_test::rvalstruct& __rvs) const
      { return __rvs.val; }
    };
}

#endif // _GLIBCXX_TESTSUITE_TR1_H
