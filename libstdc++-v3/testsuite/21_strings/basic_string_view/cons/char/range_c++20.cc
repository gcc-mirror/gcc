// Copyright (C) 2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <string_view>
#include <testsuite_hooks.h>

void
test01()
{
  struct R
  {
    const char* begin() const
    { return str; }

    const char* end() const
    { return str + std::char_traits<char>::length(str); }

    const char* str = "Home on the range";
  };

  R r;
  std::string_view s = r;
  VERIFY( s == r.str );
  VERIFY( s.data() == std::ranges::data(r) );
  VERIFY( s.size() == std::ranges::size(r) );

  struct R2 : R
  {
    using R::begin;
    using R::end;
    operator std::string_view() const { return "Out of range"; }
  };
  static_assert( std::ranges::contiguous_range<R2> );
  static_assert( std::ranges::sized_range<R2> );
  R2 r2;
  std::string_view s2 = r2; // uses conversion to string_view
  VERIFY( s2 == "Out of range" );
  VERIFY( std::string_view(const_cast<const R2&>(r2)) == s2 );

  struct R3 : R
  {
    using R::begin;
    using R::end;
    operator const char*() { return "Orange"; }
  };
  static_assert( std::ranges::contiguous_range<R3> );
  static_assert( std::ranges::sized_range<R3> );
  R3 r3;
  std::string_view s3(r3); // uses conversion to const char*
  VERIFY( s3 == "Orange" );
  s3 = std::string_view(const_cast<const R3&>(r3)); // uses range constructor
  VERIFY( s3 == "Home on the range" );

  struct R4 : R
  {
    using R::begin;
    using R::end;
    operator std::string_view() { return "Strange"; }
  };
  static_assert( std::ranges::contiguous_range<R4> );
  static_assert( std::ranges::sized_range<R4> );
  R4 r4;
  std::string_view s4 = r4; // Uses conversion to string_view
  VERIFY( s4 == "Strange" );
  // Cannot construct from const R4 because of non-const conversion op:
  static_assert( ! std::is_constructible_v<std::string_view, const R4&> );

  struct R5 : R
  {
    using R::begin;
    using R::end;
    operator std::string_view() && { return "Stranger"; }
  };
  static_assert( std::ranges::contiguous_range<R5> );
  static_assert( std::ranges::sized_range<R5> );
  R5 r5;
  std::string_view s5 = r5; // Uses range constructor
  VERIFY( s5 == r5.str );
  s5 = std::string_view(std::move(r5)); // In C++20 this used conversion op.
  VERIFY( s5 == r5.str );	        // In C++23 it uses range constructor.

  char arr[] = "arrangement\0with\0nulls";
  std::string_view sa = arr; // Does not use range constructor
  VERIFY( sa.data() == arr );
  VERIFY( sa == "arrangement" );
  VERIFY( std::end(sa) != std::end(arr) );
}

void
test02()
{
  using V1 = std::basic_string_view<signed char>;
  // range_value_t<V1> is not the right type
  static_assert( ! std::is_constructible_v<std::string_view, V1> );

  using V2 = std::basic_string_view<char, __gnu_cxx::char_traits<char>>;
  // V2::traits_type is not the right type
  static_assert( ! std::is_constructible_v<std::string_view, V2> );

  struct V3 : V2
  {
  private:
    using V2::traits_type;
  };
  // V3::traits_type is not a valid (accessible) type
  static_assert( std::is_constructible_v<std::string_view, V3> );

  struct V4 : V2
  {
    using traits_type = std::string_view::traits_type;
  };
  // V4::traits_type is the right type
  static_assert( std::is_constructible_v<std::string_view, V4> );
}

void
test03()
{
  struct R
  {
    char* begin() { return nullptr; }
    const char* begin() const noexcept { return nullptr; }

    char* end() { return nullptr; }
    const char* end() const noexcept { return nullptr; }
  };

  static_assert( ! noexcept(std::string_view(std::declval<R&>())) );
  static_assert( noexcept(std::string_view(std::declval<const R&>())) );
}

void
test04()
{
  struct R
  {
    const char* begin() const { return nullptr; }
    const char* end() const { return nullptr; }
  };

  R r;
  std::basic_string_view s = r; // Use deduction guide.

  static_assert( std::is_same_v<decltype(s), std::string_view> );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
