// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <memory>
#include <testsuite_hooks.h>

struct incomplete;

// This function isn't called, we just need to check it compiles.
void
test01(std::unique_ptr<incomplete>& p1, std::unique_ptr<incomplete>& p2)
{
  // PR libstdc++/93562
  p1.swap(p2);
  swap(p1, p2);
}

// This function isn't called, we just need to check it compiles.
void
test02(std::unique_ptr<incomplete[]>& p1, std::unique_ptr<incomplete[]>& p2)
{
  // PR libstdc++/93562
  p1.swap(p2);
  swap(p1, p2);
}

namespace A
{
  struct Deleter
  {
    Deleter& operator=(const Deleter&) = delete;

    void operator()(int* p) const noexcept { delete p; }

    // found by ADL
    friend void swap(Deleter& lhs, Deleter& rhs) noexcept
    { std::swap(lhs.id, rhs.id); }

    int id;
  };

  static_assert(!std::is_move_assignable<Deleter>::value, "not assignable");
#if __cplusplus >= 201703L
  static_assert(std::is_swappable_v<Deleter>, "but swappable");
#endif
} // namespace A

void
test03()
{
  std::unique_ptr<int, A::Deleter> p1(new int(1), { -1 });
  std::unique_ptr<int, A::Deleter> p2(new int(2), { -2 });
  int* const pi1 = p1.get();
  int* const pi2 = p2.get();
  // This type must swappable even though the deleter is not move-assignable:
  swap(p1, p2);
  VERIFY(p1.get() == pi2);
  VERIFY(p1.get_deleter().id == -2);
  VERIFY(p2.get() == pi1);
  VERIFY(p2.get_deleter().id == -1);
}

void
test04()
{
  std::unique_ptr<int[], A::Deleter> p1(new int[1]{1}, { -1 });
  std::unique_ptr<int[], A::Deleter> p2(new int[2]{2, 2}, { -2 });
  int* const pi1 = p1.get();
  int* const pi2 = p2.get();
  // This type must swappable even though the deleter is not move-assignable:
  swap(p1, p2);
  VERIFY(p1.get() == pi2);
  VERIFY(p1.get_deleter().id == -2);
  VERIFY(p2.get() == pi1);
  VERIFY(p2.get_deleter().id == -1);
}

int main()
{
  test03();
  test04();
}
