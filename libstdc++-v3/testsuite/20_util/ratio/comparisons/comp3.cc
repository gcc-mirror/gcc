// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 2011-02-28  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ratio>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  VERIFY( (std::ratio_less<std::ratio<59, 29131>,
	                   std::ratio<59, 29129>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<4733, 13>,
	                   std::ratio<4751, 13>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<25703, 25717>,
	                   std::ratio<25733, 25741>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<631, 769>,
	                   std::ratio<673, 773>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<8353, 16903>,
	                   std::ratio<17891, 32099>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<2311, 18701>,
	                   std::ratio<18457, 19571>>::value == 1) );

  VERIFY( (std::ratio_less<std::ratio<60, 29132>,
	                   std::ratio<60, 29130>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<4734, 14>,
	                   std::ratio<4752, 14>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<25704, 25718>,
	                   std::ratio<25732, 25742>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<632, 770>,
	                   std::ratio<674, 774>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<8352, 16904>,
	                   std::ratio<17892, 32100>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<2312, 18702>,
	                   std::ratio<18458, 19572>>::value == 1) );

  VERIFY( (std::ratio_less<std::ratio<58, 29130>,
	                   std::ratio<58, 29128>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<4732, 12>,
	                   std::ratio<4750, 12>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<25702, 25716>,
	                   std::ratio<25734, 25740>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<630, 768>,
	                   std::ratio<672, 772>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<8354, 16902>,
	                   std::ratio<17890, 32102>>::value == 1) );
  VERIFY( (std::ratio_less<std::ratio<2310, 18700>,
	                   std::ratio<18456, 19570>>::value == 1) );
}

int main()
{
  test01();
  return 0;
}
