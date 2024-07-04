// Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++23 } }
// { dg-require-effective-target ieee_floats }
// { dg-require-effective-target size32plus }
// { dg-add-options ieee }

#include <charconv>
#include <stdfloat>
#include <iostream>
#include <cmath>
#include <testsuite_hooks.h>

template<typename T>
void
test(std::chars_format fmt = std::chars_format{})
{
  char str1[128], str2[128], str3[128];
  union U { unsigned short s; T f; } u, v;
  for (int i = 0; i <= (unsigned short) ~0; ++i)
    {
      u.s = i;
      auto [ptr1, ec1] = (fmt == std::chars_format{}
			  ? std::to_chars(str1, str1 + sizeof(str1), u.f)
			  : std::to_chars(str1, str1 + sizeof(str1), u.f,
					  fmt));
      auto [ptr2, ec2] = (fmt == std::chars_format{}
			  ? std::to_chars(str2, str2 + sizeof(str2),
					  std::float32_t(u.f))
			  : std::to_chars(str2, str2 + sizeof(str2),
					  std::float32_t(u.f), fmt));
      VERIFY( ec1 == std::errc() && ec2 == std::errc() );
//    std::cout << i << ' ' << std::string_view (str1, ptr1)
//	<< '\t' << std::string_view (str2, ptr2) << '\n';
      if (fmt == std::chars_format::fixed)
	{
	  auto [ptr3, ec3] = std::to_chars(str3, str3 + (ptr1 - str1), u.f, fmt);
	  VERIFY( ec3 == std::errc() && ptr3 - str3 == ptr1 - str1 );
	  auto [ptr4, ec4] = std::to_chars(str3, str3 + (ptr1 - str1 - 1), u.f, fmt);
	  VERIFY( ec4 != std::errc() );
	}
      auto [ptr5, ec5] = std::from_chars(str1, ptr1, v.f,
					 fmt == std::chars_format{}
					 ? std::chars_format::general : fmt);
      VERIFY( ec5 == std::errc() && ptr5 == ptr1 );
      VERIFY( u.s == v.s || (std::isnan(u.f) && std::isnan(v.f)) );
    }
}

int
main()
{
#ifdef __STDCPP_FLOAT16_T__
  test<std::float16_t>();
  test<std::float16_t>(std::chars_format::fixed);
  test<std::float16_t>(std::chars_format::scientific);
  test<std::float16_t>(std::chars_format::general);
  test<std::float16_t>(std::chars_format::hex);
#endif
#ifdef __STDCPP_BFLOAT16_T__
  test<std::bfloat16_t>();
  test<std::bfloat16_t>(std::chars_format::fixed);
  test<std::bfloat16_t>(std::chars_format::scientific);
  test<std::bfloat16_t>(std::chars_format::general);
  test<std::bfloat16_t>(std::chars_format::hex);
#endif
}
