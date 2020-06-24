// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <charconv>
#include <string>
#include <testsuite_hooks.h>

#ifdef DEBUG
#include <stdio.h>
#endif

long long
read(const char* first, const char* last, int base)
{
  long long val = 0;
  long long place = 1;
  while (last > first)
  {
    val += (*--last - '0') * place;
    place *= base;
  }
  return val;
}

void
test01()
{
  std::from_chars_result res;
  long long val;
  for (auto s : { "10001", "10010", "10011", "10101", "10110", "10111",
		  "11001", "11010", "11011", "11101", "11110", "11111" })
  {
    std::string ss[2] = { s, std::string(64, '0') + s };
    for (const auto& str : ss)
    {
      const char* first = str.data();
      for (int base = 2; base < 37; ++base)
      {
	const char* last = str.data() + str.length();
	for (size_t n = 0; n < ss[0].length(); ++n)
	{
#ifdef DEBUG
	  printf("Parsing \"%.*s\" in base %d\n", int(last - first), first,
		 base);
#endif
	  res = std::from_chars(first, last, val, base);
	  VERIFY( res.ptr == last );
	  VERIFY( res.ec == std::errc{} );
	  VERIFY( val == read(first, last, base) );
	  // Test again with shorter string to check from_chars doesn't read
	  // the digits past the last pointer.
	  --last;
	}
      }
    }
  }
}

int
main()
{
  test01();
}
