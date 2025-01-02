// { dg-options "-DSIMULATOR_TEST" { target simulator } }
// { dg-do run { target c++11 } }

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

// C++11 21.5 Numeric Conversions [string.conversions]

#include <string>
#include <limits>
#include <cstdint>
#include <cstdio>
#include <testsuite_hooks.h>

namespace test
{
static char buf[100];

// Canonical version of std::to_string(int) as specified in the standard.
static std::string to_string(int val)
{
  std::string str;
  const int len = std::snprintf(buf, sizeof(buf), "%d", val);
  VERIFY( len < (int)sizeof(buf) );
  return std::string(buf, len);
}

static std::string to_string(unsigned int val)
{
  std::string str;
  const int len = std::snprintf(buf, sizeof(buf), "%u", val);
  VERIFY( len < (int)sizeof(buf) );
  return std::string(buf, len);
}

static std::string to_string(long val)
{
  std::string str;
  const int len = std::snprintf(buf, sizeof(buf), "%ld", val);
  VERIFY( len < (int)sizeof(buf) );
  return std::string(buf, len);
}

static std::string to_string(unsigned long val)
{
  std::string str;
  const int len = std::snprintf(buf, sizeof(buf), "%lu", val);
  VERIFY( len < (int)sizeof(buf) );
  return std::string(buf, len);
}

static std::string to_string(long long val)
{
  std::string str;
  const int len = std::snprintf(buf, sizeof(buf), "%lld", val);
  VERIFY( len < (int)sizeof(buf) );
  return std::string(buf, len);
}

static std::string to_string(unsigned long long val)
{
  std::string str;
  const int len = std::snprintf(buf, sizeof(buf), "%llu", val);
  VERIFY( len < (int)sizeof(buf) );
  return std::string(buf, len);
}

} // namespace test

const std::uint_least32_t values[] = {
  0x10, 0x30, 0x50, 0x80, 0xc0,
  0x100, 0x180, 0x1c0, 0x200, 0x400, 0x800, 0xc00,
  0x1000, 0x1800, 0x2000, 0x4000, 0x8000, 0xc000,
  0x10000, 0x10101, 0x80000, 0x80706, 0xc0000, 0xccccc,
  0x100000, 0x101010, 0x800000, 0x807060, 0xc0fefe, 0xc1d2e3f,
  0x1000000, 0x1001000, 0x1008000, 0x1010000, 0x1080000, 0x1100000, 0x1234567,
  0x10000000, 0x10101010, 0x12345678, 0x80000010, 0x87654321, 0xaaaaaaaa,
  0xf0000000, 0xf0101010, 0xf0f00000, 0xf0f0f0f0, 0xf0ff0ff0, 0xff0ff00f,
  0xffff0000, 0xffff00f0, 0xffff0ff0, 0xffffff00
};

const std::size_t empty_string_capacity = std::string().capacity();

#include <set>

template<typename T>
  void check_value(T val)
  {
    const std::string s = std::to_string(val);
    const std::string expected = test::to_string(val);
    VERIFY( s == expected );
    VERIFY( s[s.size()] == '\0' ); // null-terminator not overwritten!
  }

#ifdef SIMULATOR_TEST
const int width = 3;
#else
const int width = 16;
#endif

template<typename T>
  void check_values()
  {
#ifdef SIMULATOR_TEST
    check_value((T)-1);
    check_value((T)0);
    check_value((T)+1);
#endif

    for (auto v : values)
    {
      for (int i = -width; i < +width; ++i)
      {
	const T val = (T)v + i;
	check_value(val);
      }

      if (std::numeric_limits<T>::digits > 32)
      {
	for (auto v2 : values)
	{
	  for (int i = -width; i < +width; ++i)
	  {
	    typename std::make_unsigned<T>::type hi = v2;
	    hi += i;
	    hi <<= 32;
	    const T val = T(hi) | v;
	    check_value(val);
	  }
	}
      }
    }
  }

void test02()
{
  check_values<int>();
  check_values<unsigned int>();
  check_values<long>();
  check_values<unsigned long>();
  check_values<long long>();
  check_values<unsigned long long>();
}

int main()
{
  test02();
}
