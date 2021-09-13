// { dg-do run { target c++17 }  }

// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>
#include <string>

int main()
{
  std::optional<int> o = 42;
  std::optional<const int> o2 = 666;
  VERIFY(o == 42);
  VERIFY(o != 43);
  VERIFY(o < 43);
  VERIFY(o > 41);
  VERIFY(o <= 43);
  VERIFY(o >= 41);
  VERIFY(o2 == 666);
  VERIFY(o2 != 667);
  VERIFY(o2 < 667);
  VERIFY(o2 > 665);
  VERIFY(o2 <= 667);
  VERIFY(o2 >= 665);
  VERIFY(42 == o);
  VERIFY(43 != o);
  VERIFY(41< o);
  VERIFY(43 > o);
  VERIFY(41 <= o);
  VERIFY(43 >= o);
  VERIFY(666 == o2);
  VERIFY(667 != o2);
  VERIFY(665 < o2);
  VERIFY(667 > o2);
  VERIFY(665 <= o2);
  VERIFY(667 >= o2);
  std::optional<std::string> os = "jones";
  VERIFY(os == "jones");
  VERIFY(os != "bones");
  VERIFY(os < "kones");
  VERIFY(os > "hones");
  VERIFY(os <= "kones");
  VERIFY(os >= "hones");
  VERIFY("jones" == os);
  VERIFY("bones" != os);
  VERIFY("hones" < os);
  VERIFY("kones" > os);
  VERIFY("hones" <= os);
  VERIFY("kones" >= os);
  std::optional<int> oi = 42;
  std::optional<long int> ol = 666;
  VERIFY(!(oi == ol));
  VERIFY(!(ol == oi));
  VERIFY(oi != ol);
  VERIFY(ol != oi);
}
