// { dg-do run { target c++11 } }

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

static const std::intmax_t M = INTMAX_MAX;

void
test01()
{
  // No overflow with same denominator
  VERIFY( (std::ratio_less<std::ratio<M - 2, M>,
           std::ratio<M - 1, M>>::value == 1) );
  
  VERIFY( (std::ratio_less<std::ratio<M - 1, M>,
           std::ratio<M - 2, M>>::value == 0) );
     
  // No overflow if signs differ
  VERIFY( (std::ratio_less<std::ratio<-M, M - 1>,
           std::ratio<M - 1, M - 2>>::value == 1) );
  
  VERIFY( (std::ratio_less<std::ratio<M - 1, M - 2>,
           std::ratio<-M, M - 1>>::value == 0) );

  // No overflow
  VERIFY( (std::ratio_less<std::ratio<M, M - 1>,
           std::ratio<M, M - 2>>::value == 1) );

  VERIFY( (std::ratio_less<std::ratio<-M, M - 1>,
           std::ratio<-M, M - 2>>::value == 0) );

  VERIFY( (std::ratio_less<std::ratio<M - 2, M - 1>,
           std::ratio<M - 1, M>>::value == 1) );
}

int main()
{
  test01();
  return 0;
}
