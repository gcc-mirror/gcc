// { dg-options "-std=gnu++0x" }
// { dg-require-effective-target dfp }

// 2011-02-23  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

#include <string>
#include <typeinfo>
#include <decimal/decimal>

template<typename _Tp>
  std::string
  gen_type_info()
  { 
    std::string s1 = typeid(_Tp).name(); 
    std::string s2 = typeid(_Tp*).name(); 
    std::string s3 = typeid(const _Tp*).name(); 
    return std::max(std::max(s1, s2), s3);
  }

// libstdc++/43622, others
int main()
{
  gen_type_info<bool>();
  gen_type_info<char>();
  gen_type_info<signed char>();
  gen_type_info<unsigned char>();
  gen_type_info<short>();
  gen_type_info<unsigned short>();
  gen_type_info<int>();
  gen_type_info<unsigned int>();
  gen_type_info<long>();
  gen_type_info<unsigned long>();
  gen_type_info<long long>();
  gen_type_info<unsigned long long>();
  gen_type_info<wchar_t>();
  gen_type_info<char16_t>();
  gen_type_info<char32_t>();

  gen_type_info<float>();
  gen_type_info<double>();
  gen_type_info<long double>();

  gen_type_info<void>();

  gen_type_info<std::nullptr_t>();

  // decimal
  gen_type_info<std::decimal::decimal32>();
  gen_type_info<std::decimal::decimal64>();
  gen_type_info<std::decimal::decimal128>();

  // attributes
#if 0
  typedef int ti_type __attribute__((__mode__(TI)));
  gen_type_info<ti_type>();

  typedef float tf_type __attribute__((__mode__(TF)));
  gen_type_info<tf_type>();
#endif

  return 0;
}
