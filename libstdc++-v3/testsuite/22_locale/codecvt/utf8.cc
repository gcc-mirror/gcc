// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

// { dg-require-cstdint "" }
// { dg-do run { target c++11 } }

#include <locale>
#include <iterator>
#include <string>
#include <testsuite_hooks.h>

const char expected[] = u8"£¥€";
const std::size_t expected_len = std::char_traits<char>::length(expected);

template<typename C>
void test(const C* from)
{
  auto len = std::char_traits<C>::length(from);
  std::mbstate_t state{};
  char buf[16] = { };
  using test_type = std::codecvt<C, char, std::mbstate_t>;
  const test_type& cvt = std::use_facet<test_type>(std::locale::classic());
  auto from_end = from + len;
  auto from_next = from;
  auto buf_end = std::end(buf);
  auto buf_next = buf;
  auto res = cvt.out(state, from, from_end, from_next, buf, buf_end, buf_next);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( from_next == from_end );
  VERIFY( (buf_next - buf) == expected_len );
  VERIFY( 0 == std::char_traits<char>::compare(buf, expected, expected_len) );

  C buf2[16];
  auto exp_end = expected + expected_len;
  auto exp_next = expected;
  auto buf2_end = std::end(buf2);
  auto buf2_next = buf2;
  res = cvt.in(state, expected, exp_end, exp_next, buf2, buf2_end, buf2_next);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( exp_next == exp_end );
  VERIFY( (buf2_next - buf2) == len );
  VERIFY( 0 == std::char_traits<C>::compare(buf2, from, len) );
}

void
test01()
{
  test(u"£¥€");
}

void
test02()
{
  test(U"£¥€");
}

int
main()
{
  test01();
  test02();
}
