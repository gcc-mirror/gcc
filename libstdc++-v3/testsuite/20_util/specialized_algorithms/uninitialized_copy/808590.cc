// Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

#include <vector>
#include <stdexcept>

// 4.4.x only
struct c
{
  void *m;

  c(void* o = 0) : m(o) {}
  c(const c &r) : m(r.m) {}

#if __cplusplus >= 201103L
  // Avoid -Wdeprecated-copy warning.
  c& operator=(const c &) = default;
#endif

  template<class T>
    explicit c(T &o) : m((void*)0xdeadbeef) { }
};

int main()
{
  std::vector<c> cbs;
  const c cb((void*)0xcafebabe);

  for (int fd = 62; fd < 67; ++fd)
    {
      cbs.resize(fd + 1);
      cbs[fd] = cb;
    }

  for (int fd = 62; fd< 67; ++fd)
    if (cb.m != cbs[fd].m)
      throw std::runtime_error("wrong");
  return 0;
}
