// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

// 20.8.9 Function template bind

// Verify that calls to bind() in BSD sockets API do not match std::bind()
// (this is a GNU extension)

// { dg-do compile { target c++11 } }

#include <functional>

struct my_sockaddr { };
typedef long my_socklen_t;
int bind(int, const my_sockaddr*, my_socklen_t);

using namespace std;

int test01()
{
  int fd = 1;
  my_sockaddr sa;           // N.B. non-const
  size_t len = sizeof(sa);  // N.B. size_t not my_socklen_t
  return bind(fd, &sa, len);
}

