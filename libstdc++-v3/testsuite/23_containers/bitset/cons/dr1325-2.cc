// { dg-do run { target c++11 } }

// 2010-10-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

#include <bitset>
#include <testsuite_hooks.h>

template<std::size_t Nb, typename CharT>
  std::bitset<Nb>
  test01_ref(const CharT* str,
	     typename std::basic_string<CharT>::size_type n
	     = std::basic_string<CharT>::npos,
	     CharT zero = CharT('0'), CharT one = CharT('1'))
  {
    return std::bitset<Nb>(n == std::basic_string<CharT>::npos
			   ? std::basic_string<CharT>(str)
			   : std::basic_string<CharT>(str, n),
			   0, n, zero, one);
  }

// DR 1325.
void test01()
{
  using namespace std;

  const char s1[4] = { '0', '1', '0', '1' };
  VERIFY( bitset<4>(s1, 4) == test01_ref<4>(s1, 4) );

  const char s2[3] = { '1', '1', '0' };
  VERIFY( bitset<6>(s2, 3) == test01_ref<6>(s2, 3) );

  const char* s3 = "1110110";
  VERIFY( bitset<7>(s3) == test01_ref<7>(s3) );

  const char* s4 = "0011";
  VERIFY( bitset<10>(s4) == test01_ref<10>(s4) );

  const char* s5 = "011110000111001";
  VERIFY( bitset<5>(s5) == test01_ref<5>(s5) );

  const char* s6 = "1cc1c1";
  VERIFY( bitset<6>(s6, basic_string<char>::npos, 'c')
	  == test01_ref<6>(s6, basic_string<char>::npos, 'c') );

  const char* s7 = "001011101";
  VERIFY( bitset<9>(s7, basic_string<char>::npos, '0', '1')
	  == test01_ref<9>(s7, basic_string<char>::npos, '0', '1') );

  const char* s8 = "babb";
  VERIFY( bitset<4>(s8, basic_string<char>::npos, 'a', 'b')
	  == test01_ref<4>(s8, basic_string<char>::npos, 'a', 'b') );

  const char* s9 = "bbabbbaaa";
  VERIFY( bitset<100>(s9, basic_string<char>::npos, 'a', 'b')
	  == test01_ref<100>(s9, basic_string<char>::npos, 'a', 'b') );
}

int main()
{
  test01();
  return 0;
}
