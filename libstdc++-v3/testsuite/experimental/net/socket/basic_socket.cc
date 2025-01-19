// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/socket>

namespace net = std::experimental::net;
using namespace std;

void
test01(net::io_context& io)
{
  struct proto
  {
    struct endpoint
    {
      using protocol_type = proto;
      protocol_type protocol() const { return {}; }

      void* data() { return nullptr; }
      const void* data() const { return nullptr; }
      std::size_t size() const { return 0; }
      void resize(std::size_t) { }
      std::size_t capacity() const { return 0; }
    };

    int family() const { return 0; }
    int type() const { return 0; }
    int protocol() const { return 0; }
  };

  static_assert( ! is_default_constructible<net::basic_socket<proto>>::value,
		 "no default ctor" );
  static_assert( ! is_copy_constructible<net::basic_socket<proto>>::value,
		 "copy ctor is deleted" );
  static_assert( ! is_move_constructible<net::basic_socket<proto>>::value,
		 "move ctor is protected" );
  static_assert( ! is_move_assignable<net::basic_socket<proto>>::value,
		 "move assignment op is protected" );

  struct socket : net::basic_socket<proto>
  {
    explicit
    socket(net::io_context& io)
    : basic_socket(io) { }

    socket(net::io_context& io, const proto& p)
    : basic_socket(io, p) { }

    socket(net::io_context& io, const proto::endpoint& e)
    : basic_socket(io, e) { }

    socket(net::io_context& io, const proto& p, int n)
    : basic_socket(io, p, n) { }
  };

  static_assert( ! is_copy_constructible<socket>::value, "deleted" );
  static_assert( is_move_constructible<socket>::value, "" );
  static_assert( is_move_assignable<socket>::value, "" );

  error_code ec;
  proto p;
  proto::endpoint e;

  socket s(io);
  s = socket(io, p);
  s = socket(io, e);
  s = socket(io, p, s.release());

  static_assert( is_same<decltype(s.get_executor()),
			 net::io_context::executor_type>::value, "" );
  static_assert( noexcept(s.get_executor()), "" );
  static_assert( is_same<decltype(s.native_handle()),
			 socket::native_handle_type>::value, "" );
  static_assert( noexcept(s.native_handle()), "GNU extension" );

  s.open();
  s.open(p);
  s.open(p, ec);

  s.assign(p, s.release());
  s.assign(p, s.release(ec), ec);

  static_assert( is_same<decltype(const_cast<const socket&>(s).is_open()),
			 bool>::value, "" );
  static_assert( noexcept(const_cast<const socket&>(s).is_open()), "" );

  s.close();
  s.close(ec);

  s.cancel();
  s.cancel(ec);

  s.bind(e);
  s.bind(e, ec);

#ifdef SHUT_RDWR
  s.shutdown(net::socket_base::shutdown_both);
  s.shutdown(net::socket_base::shutdown_both, ec);
#endif

  e = s.local_endpoint();
  e = s.local_endpoint(ec);
  e = s.remote_endpoint();
  e = s.remote_endpoint(ec);

  s.connect(e);
  s.connect(e, ec);

  s.wait(net::socket_base::wait_read);
  s.wait(net::socket_base::wait_read, ec);
}
