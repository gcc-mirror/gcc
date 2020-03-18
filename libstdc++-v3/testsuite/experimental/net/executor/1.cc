// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/executor>
#include <testsuite_hooks.h>

namespace net = std::experimental::net;

void
test01()
{
  net::executor e;
  VERIFY( !e );
  VERIFY( e == nullptr );
  VERIFY( nullptr == e );
  VERIFY( e == e );
  VERIFY( e == e );
  net::executor e2;
  VERIFY( e == e2 );
  swap(e, e2);
  VERIFY( e == e2 );
  e = e2;
  VERIFY( e == e2 );
}

void
test02()
{
  struct E
  {
    void on_work_started() const noexcept { }
    void on_work_finished() const noexcept { }
    net::execution_context& context() const noexcept { return c; }
    void dispatch(std::function<void()>, std::allocator<void>) const { }
    void post(std::function<void()>, std::allocator<void>) const { }
    void defer(std::function<void()>, std::allocator<void>) const { }

    net::execution_context& c;

    bool operator==(const E& rhs) const noexcept
    { return &c == &rhs.c; }
  };

  net::execution_context c;
  E d{c};
  net::executor e(d);
  VERIFY( e == e );
  VERIFY( e != nullptr );
  VERIFY( nullptr != e );

  VERIFY( &e.context() == &c );
#if __cpp_rtti
  VERIFY( e.target_type() == typeid(E) );
#endif
  VERIFY( *e.target<E>() == d );
  VERIFY( *e.target<const E>() == d );
  VERIFY( *const_cast<const net::executor&>(e).target<E>() == d );
  VERIFY( *const_cast<const net::executor&>(e).target<const E>() == d );

  net::executor f = e;
  VERIFY( f == e );
  e = nullptr;
  VERIFY( f != e );
  swap(e, f);
  VERIFY( f == nullptr );
  VERIFY( nullptr != e );

  net::executor g(E{c});
  VERIFY( e == g );
}

int
main()
{
  test01();
  test02();
}
