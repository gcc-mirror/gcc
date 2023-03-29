// Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-add-options ieee }

#include <charconv>
#include <string>
#include <cfenv>
#include <testsuite_hooks.h>

int
main()
{
  // FP from_char not available otherwise.
#if __cpp_lib_to_chars >= 201611L \
    && _GLIBCXX_USE_C99_FENV_TR1 \
    && defined(FE_DOWNWARD) \
    && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  // PR libstdc++/107468
  float f;
  char buf[] = "3.355447e+07";
  std::fesetround(FE_DOWNWARD);
  auto [ptr, ec] = std::from_chars(buf, buf + sizeof(buf) - 1, f, std::chars_format::scientific);
  VERIFY( ec == std::errc() && ptr == buf + sizeof(buf) - 1 );
  VERIFY( f == 33554472.0f );
#endif
}
