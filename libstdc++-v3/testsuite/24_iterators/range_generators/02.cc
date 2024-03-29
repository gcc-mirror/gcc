// { dg-do run { target c++23 } }
// Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

#include <iostream>
#include <generator>
#include <testsuite_hooks.h>

struct foo
{
  int id;

  foo(int id)
    : id { id }
  {}

  foo(const foo& o)
    : id { o.id * 100 }
  {
    std::cout << "copy-consed " << o.id << "->" << id << '\n';
  }

  foo&
  operator=(const foo& o)
  {
    id = o.id * 100;
    std::cout << "copied " << o.id << "->" << id << '\n';
    return *this;
  }

  foo(foo&& o)
    : id { o.id }
  {
    o.id = -1;
    std::cout << "moved " << id << '\n';
  }

  foo&
  operator=(foo&& o)
  {
    std::swap(o.id, id);
    std::cout << "swapped " << id << '\n';
    return *this;
  }
};

std::generator<foo>
foogen()
{
  co_yield foo{0};

  {
    foo f {1};
    co_yield f;
  }

  {
    const foo f {2};
    co_yield f;
  }

  {
    foo f {3};
    co_yield std::move(f);
  }
}

std::generator<const foo&>
foogen2()
{
  co_yield foo{0};

  {
    foo f {1};
    co_yield f;
  }

  {
    const foo f {2};
    co_yield f;
  }

  {
    foo f {3};
    co_yield std::move(f);
  }
}

std::generator<foo&&>
foogen3()
{
  co_yield foo{0};

  {
    foo f {1};
    co_yield f;
  }

  {
    const foo f {2};
    co_yield f;
  }

  {
    foo f {3};
    co_yield std::move(f);
  }
}

int
main()
{
  for (auto f : foogen())
    std::cout << f.id << '\n';
  for (const auto& f : foogen())
    std::cout << f.id << '\n';
  for (auto&& f : foogen())
    std::cout << f.id << '\n';

  std::cout << "---\n";

  for (auto f : foogen2())
    std::cout << f.id << '\n';
  for (const auto& f : foogen2())
    std::cout << f.id << '\n';
  for (auto&& f : foogen2())
    std::cout << f.id << '\n';

  std::cout << "---\n";

  for (auto f : foogen3())
    std::cout << f.id << '\n';
  for (const auto& f : foogen3())
    std::cout << f.id << '\n';
  for (auto&& f : foogen3())
    std::cout << f.id << '\n';
}

// { dg-output {moved 0(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {moved 100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {moved 200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {moved 3(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {---(\n|\r\n|\r)} }
// { dg-output {copy-consed 0->0(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {copy-consed 3->300(\n|\r\n|\r)} }
// { dg-output {300(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {1(\n|\r\n|\r)} }
// { dg-output {2(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {1(\n|\r\n|\r)} }
// { dg-output {2(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {---(\n|\r\n|\r)} }
// { dg-output {moved 0(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {moved 100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {moved 200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {moved 3(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
// { dg-output {0(\n|\r\n|\r)} }
// { dg-output {copy-consed 1->100(\n|\r\n|\r)} }
// { dg-output {100(\n|\r\n|\r)} }
// { dg-output {copy-consed 2->200(\n|\r\n|\r)} }
// { dg-output {200(\n|\r\n|\r)} }
// { dg-output {3(\n|\r\n|\r)} }
