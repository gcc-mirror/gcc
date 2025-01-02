// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;

template<bool B = std::is_same<fs::path::value_type, char>::value>
typename std::enable_if<B, const char*>::type
code_units()
{ return "\xf0\x9d\x84\x9e"; }

template<bool B = std::is_same<fs::path::value_type, wchar_t>::value>
typename std::enable_if<B, const wchar_t*>::type
code_units()
{ return L"\xD834\xDD1E"; }

// PR libstdc++/90281
void
test01()
{
  const fs::path::string_type expected = code_units();

  fs::path p8 = fs::u8path(u8"\U0001D11E");
  VERIFY( p8.native() == expected );
  fs::path p16(u"\U0001D11E");
  VERIFY( p16.native() == expected );
  fs::path p32(U"\U0001D11E");
  VERIFY( p32.native() == expected );
}

int
main()
{
  test01();
}
