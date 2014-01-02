// { dg-options "-std=gnu++0x" }

// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#include <memory>
#include <scoped_allocator>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// 20.12.4 Scoped allocator adaptor members [allocator.adaptor.members]
//
// Test piecewise construction of std::pair by scoped_allocator_adaptor

using __gnu_test::uneq_allocator;
using std::scoped_allocator_adaptor;

// a DefaultConstructible and CopyConstructible type
struct def
{
  def() : id(999) { }

  int id;
};

// a CopyConstructible and non-DefaultConstructible type
struct copyable
{
  copyable(int id) : id(id) { }

  // not constructed with an allocator so nothing to test
  bool verify() const { return true; }

  int id;
};

// a MoveConstructible and non-DefaultConstructible type
struct move_only
{
  move_only(int id) : id(id) { }
  move_only(move_only&&) = default;

  // not constructed with an allocator so nothing to test
  bool verify() const { return true; }

  int id;
};

// a type for which std::uses_allocator is true
struct uses_alloc_post
{
  typedef uneq_allocator<uses_alloc_post> allocator_type;

  uses_alloc_post(const allocator_type& alloc)
  : allocator_personality(alloc.get_personality()), id(999)
  { }

  uses_alloc_post(copyable arg, const allocator_type& alloc)
  : allocator_personality(alloc.get_personality()), id(arg.id)
  { }

  uses_alloc_post(move_only arg, const allocator_type& alloc)
  : allocator_personality(alloc.get_personality()), id(arg.id)
  { }

  // allocator-extended copy ctor
  uses_alloc_post(const uses_alloc_post& other, const allocator_type& alloc)
  : allocator_personality(alloc.get_personality()), id(other.id)
  { }

  // verify we were constructed with right allocator
  bool verify() const { return allocator_personality == id; }

  int allocator_personality;
  int id;
};

// a type for which std::uses_allocator is true
struct uses_alloc_pre : uses_alloc_post
{
  typedef uneq_allocator<uses_alloc_pre> allocator_type;

  uses_alloc_pre(std::allocator_arg_t, const allocator_type& alloc)
  : uses_alloc_post(alloc)
  { }

  uses_alloc_pre(std::allocator_arg_t, const allocator_type& alloc,
                 copyable arg)
  : uses_alloc_post(arg, alloc)
  { }

  // allocator-extended copy ctor
  uses_alloc_pre(std::allocator_arg_t, const allocator_type& alloc,
                 const uses_alloc_pre& other)
  : uses_alloc_post(other, alloc)
  { }

  uses_alloc_pre(std::allocator_arg_t, const allocator_type& alloc,
                 move_only arg)
  : uses_alloc_post(std::move(arg), alloc)
  { }
};

template<typename A, typename B>
  void
  test_def()
  {
    bool test __attribute((unused)) = false;

    typedef std::pair<A, B> test_type;
    typedef uneq_allocator<test_type> alloc_type;
    typedef scoped_allocator_adaptor<alloc_type, alloc_type> alloc_adaptor;

    int inner_id = 2;
    alloc_adaptor a(-1, alloc_type(inner_id)); // outer=-1, inner=2

    // all pair members that can be constructed with an allocator
    // should be constructed with the inner allocator, with personality==2

    auto p = a.allocate(1);

    // construct(pair<T1, T2>* p, piecewise_construct_t, tuple<...>, tuple<...>)
    std::tuple<> t;
    a.construct(p, std::piecewise_construct, t, t);
    VERIFY( p->first.id == 999 );
    VERIFY( p->second.id == 999 );
    a.destroy(p);

    // construct(pair<T1, T2>* __p)
    a.construct(p);
    VERIFY( p->first.id == 999 );
    VERIFY( p->second.id == 999 );
    auto pp = *p;
    a.destroy(p);

    // construct(pair<T1, T2>* p, const pair<U, V>& x)
    a.construct(p, pp);
    VERIFY( p->first.id == 999 );
    VERIFY( p->second.id == 999 );
    a.destroy(p);

    // construct(pair<T1, T2>* p, pair<U, V>&& x)
    a.construct(p, std::move(pp));
    VERIFY( p->first.id == 999 );
    VERIFY( p->second.id == 999 );
    a.destroy(p);

    a.deallocate(p, 1);
  }

template<typename A, typename B>
  void
  test_copying()
  {
    bool test __attribute((unused)) = false;

    typedef std::pair<A, B> test_type;
    typedef uneq_allocator<test_type> alloc_type;
    typedef scoped_allocator_adaptor<alloc_type, alloc_type> alloc_adaptor;

    int inner_id = 2;
    alloc_adaptor a(-1, alloc_type(inner_id)); // outer=-1, inner=2

    // all pair members that can be constructed with an allocator
    // should be constructed with the inner allocator, with personality==2

    auto p = a.allocate(1);

    // construct(pair<T1, T2>* p, piecewise_construct_t, tuple<...>, tuple<...>)
    auto t = std::make_tuple(copyable(inner_id));
    a.construct(p, std::piecewise_construct, t, t);
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    a.destroy(p);

    // construct(pair<T1, T2>* __p)
    // cannot test this overload using non-DefaultConstructible types

    // construct(pair<T1, T2>* p, U&& x, V&& y)
    copyable c(inner_id);
    a.construct(p, c, c);
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    auto pp = *p;
    a.destroy(p);

    // construct(pair<T1, T2>* p, const pair<U, V>& x)
    a.construct(p, pp);
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    a.destroy(p);

    // construct(pair<T1, T2>* p, pair<U, V>&& x)
    a.construct(p, std::move(pp));
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    a.destroy(p);

    a.deallocate(p, 1);
  }

template<typename A, typename B>
  void
  test_moving()
  {
    bool test __attribute((unused)) = false;

    typedef std::pair<A, B> test_type;
    typedef uneq_allocator<test_type> alloc_type;
    typedef scoped_allocator_adaptor<alloc_type, alloc_type> alloc_adaptor;

    int inner_id = 2;
    alloc_adaptor a(-1, alloc_type(inner_id)); // outer=-1, inner=2

    // all pair members that can be constructed with an allocator
    // should be constructed with the inner allocator, with personality==2

    auto p = a.allocate(1);

    // construct(pair<T1, T2>* p, piecewise_construct_t, tuple<...>, tuple<...>)
    a.construct(p, std::piecewise_construct,
                std::make_tuple(move_only(inner_id)),
                std::make_tuple(move_only(inner_id)));
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    a.destroy(p);

    // construct(pair<T1, T2>* __p)
    // cannot test this overload using non-DefaultConstructible types

    // construct(pair<T1, T2>* p, U&& x, V&& y)
    a.construct(p, move_only(inner_id), move_only(inner_id));
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    a.destroy(p);

    // construct(pair<T1, T2>* p, const pair<U, V>& x)
    // cannot test this overload using move-only types

    // construct(pair<T1, T2>* p, pair<U, V>&& x)
    a.construct(p, std::make_pair(move_only(inner_id), move_only(inner_id)));
    VERIFY( p->first.verify() );
    VERIFY( p->second.verify() );
    a.destroy(p);

    a.deallocate(p, 1);
  }

void test01()
{
  test_def<def, def>();
  test_def<def, uses_alloc_pre>();
  test_def<def, uses_alloc_post>();
  test_def<uses_alloc_pre, def>();
  test_def<uses_alloc_pre, uses_alloc_pre>();
  test_def<uses_alloc_pre, uses_alloc_post>();
  test_def<uses_alloc_post, def>();
  test_def<uses_alloc_post, uses_alloc_pre>();
  test_def<uses_alloc_post, uses_alloc_post>();
}

void test02()
{
  test_copying<copyable, copyable>();
  test_copying<copyable, uses_alloc_pre>();
  test_copying<copyable, uses_alloc_post>();
  test_copying<uses_alloc_pre, copyable>();
  test_copying<uses_alloc_pre, uses_alloc_pre>();
  test_copying<uses_alloc_pre, uses_alloc_post>();
  test_copying<uses_alloc_post, copyable>();
  test_copying<uses_alloc_post, uses_alloc_pre>();
  test_copying<uses_alloc_post, uses_alloc_post>();
}

void test03()
{
  test_moving<move_only, move_only>();
  test_moving<move_only, uses_alloc_pre>();
  test_moving<move_only, uses_alloc_post>();
  test_moving<uses_alloc_pre, move_only>();
  test_moving<uses_alloc_pre, uses_alloc_pre>();
  test_moving<uses_alloc_pre, uses_alloc_post>();
  test_moving<uses_alloc_post, move_only>();
  test_moving<uses_alloc_post, uses_alloc_pre>();
  test_moving<uses_alloc_post, uses_alloc_post>();
}

int main()
{
  test01();
  test02();
  test03();
}
