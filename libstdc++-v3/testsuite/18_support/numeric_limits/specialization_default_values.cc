// { dg-add-options ieee }

// 1999-08-23 bkoz

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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

// 18.2.1.1 template class numeric_limits

#include <limits>
#include <limits.h>
#include <float.h>
#include <cwchar>
#include <testsuite_hooks.h>

template<typename T>
  struct A 
  {
    int key;
  public:
    A(int i = 0): key(i) { }
    bool
    operator==(int i) { return i == key; }
  };

struct B 
{
  B(int = 0) { }
};


void test01()
{
  std::numeric_limits< A<B> > obj;

  VERIFY( !obj.is_specialized );
  VERIFY( obj.min() == 0 );
  VERIFY( obj.max() == 0 );
  VERIFY( obj.digits ==  0 );
  VERIFY( obj.digits10 == 0 );
  VERIFY( !obj.is_signed );
  VERIFY( !obj.is_integer );
  VERIFY( !obj.is_exact );
  VERIFY( obj.radix == 0 );
  VERIFY( obj.epsilon() == 0 );
  VERIFY( obj.round_error() == 0 );
  VERIFY( obj.min_exponent == 0 );
  VERIFY( obj.min_exponent10 == 0 );
  VERIFY( obj.max_exponent == 0 );
  VERIFY( obj.max_exponent10 == 0 );
  VERIFY( !obj.has_infinity );
  VERIFY( !obj.has_quiet_NaN );
  VERIFY( !obj.has_signaling_NaN );
  VERIFY( !obj.has_denorm );
  VERIFY( !obj.has_denorm_loss );
  VERIFY( obj.infinity() == 0 );
  VERIFY( obj.quiet_NaN() == 0 );
  VERIFY( obj.signaling_NaN() == 0 );
  VERIFY( obj.denorm_min() == 0 );
  VERIFY( !obj.is_iec559 );
  VERIFY( !obj.is_bounded );
  VERIFY( !obj.is_modulo );
  VERIFY( !obj.traps );
  VERIFY( !obj.tinyness_before );
  VERIFY( obj.round_style == std::round_toward_zero );
}

// test linkage of the generic bits
template struct std::numeric_limits<B>;

void test02()
{
  typedef std::numeric_limits<B> b_nl_type;
  
  // Should probably do all of them...
  const int* __attribute__((unused)) pi1 = &b_nl_type::digits;
  const int* __attribute__((unused)) pi2 = &b_nl_type::digits10;
  const int* __attribute__((unused)) pi3 = &b_nl_type::max_exponent10;
  const bool* __attribute__((unused)) pb1 = &b_nl_type::traps;
}


int main()
{
  test01();
  test02();

  return 0;
}
