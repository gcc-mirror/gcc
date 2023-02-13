// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// basic_string ends_with

#include <testsuite_string.h>
#include <testsuite_hooks.h>

void
test01()
{
  const wchar_t cstr_suf[] = L".jpg";
  const std::wstring_view sv_suf(L".jpg");
  const wchar_t cstr_suf2[] = L".rgb";
  const std::wstring_view sv_suf2(L".rgb");

  const __gnu_test::wstring s_test(L"slugs/slimy.jpg");

  const auto cstr_in_slugs = s_test.ends_with(cstr_suf);
  VERIFY( cstr_in_slugs );
  const auto sv_in_slugs = s_test.ends_with(sv_suf);
  VERIFY( sv_in_slugs );
  const auto char_g = s_test.ends_with(L'g');
  VERIFY( char_g );

  const auto cstr_in_worms = s_test.ends_with(cstr_suf2);
  VERIFY( !cstr_in_worms );
  const auto sv_in_worms = s_test.ends_with(sv_suf2);
  VERIFY( !sv_in_worms );
  const auto char_b = s_test.ends_with(L'b');
  VERIFY( !char_b );
}

int
main()
{ 
  test01();
  return 0;
}
