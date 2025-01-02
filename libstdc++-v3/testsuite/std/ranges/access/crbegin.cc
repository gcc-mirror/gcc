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

// { dg-do run { target c++20 } }

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::crbegin));

struct R1
{
  int i = 0;
  int j = 0;

  const int* rbegin() const { return &i; }
  friend const int* rbegin(const R1&& r) { return &r.j; }
};

struct R1V // view on an R1
{
  R1& r;

  friend const long* rbegin(R1V&) { return nullptr; }
  friend const int* rbegin(const R1V& rv) noexcept { return rv.r.rbegin(); }
};

// Allow ranges::end to work with R1V&&
template<> constexpr bool std::ranges::enable_borrowed_range<R1V> = true;

#if __cpp_lib_ranges_as_const
struct R1VC // view on an R1
{
  R1& r;

  friend const long* rbegin(R1VC&); // this is not defined
  friend const int* rbegin(const R1VC& rv) noexcept { return rv.r.rbegin(); }

  // The following ensure that the following are satisfied:
  // constant_range<const R1VC> && ! constant_range<R1VC>
  friend int* begin(R1VC&);
  friend int* end(R1VC&);
  friend const int* begin(const R1VC&);
  friend const int* end(const R1VC&);
};

// Allow ranges::end to work with R1VC&&
template<> constexpr bool std::ranges::enable_borrowed_range<R1VC> = true;
#endif

void
test01()
{
  R1 r;
  const R1& c = r;
  VERIFY( std::ranges::crbegin(r) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(c) == std::ranges::rbegin(c) );

  R1V v{r};
#if ! __cpp_lib_ranges_as_const
  VERIFY( std::ranges::crbegin(v) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(v)) == std::ranges::rbegin(c) );
#else
  // constant_range<const R1V> is not satisfied, so crbegin(v) == rbegin(v).
  VERIFY( std::ranges::crbegin(v) == (long*)nullptr );
  VERIFY( std::ranges::crbegin(std::move(v)) == (long*)nullptr );
  R1VC v2{r};
  // But constant_range<const R1VC> is satisfied:
  VERIFY( std::ranges::crbegin(v2) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(v2)) == std::ranges::rbegin(c) );
  const R1VC cv2{r};
  VERIFY( std::ranges::crbegin(cv2) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(cv2)) == std::ranges::rbegin(c) );
#endif

  const R1V cv{r};
  VERIFY( std::ranges::crbegin(cv) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(cv)) == std::ranges::rbegin(c) );
}

struct R2
{
  int a[2] = { };
  long l[2] = { };

  const int* begin() const { return a; }
  const int* end() const { return a + 2; }

  friend const long* begin(const R2&&); // not defined
  friend const long* end(const R2&&); // not defined
};

// N.B. this is a lie, rbegin on an R2 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R2> = true;

void
test02()
{
  R2 r;
  const R2& c = r;
  VERIFY( std::ranges::crbegin(r) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(c) == std::ranges::rbegin(c) );

  VERIFY( std::ranges::crbegin(std::move(r)) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(c)) == std::ranges::rbegin(c) );
}

int
main()
{
  test01();
  test02();
}
