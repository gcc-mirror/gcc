// 1999-08-23 bkoz

// Copyright (C) 1999 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 18.2.1.1 template class numeric_limits

#include <limits>
#ifdef DEBUG_ASSERT
  #include <assert.h>
#endif


template<typename T>
  struct A 
  {
    int key;
  public:
    A(int i = 0): key(i) { }
    bool
    operator==(int i) { return i == key; }
  };

struct B { };


bool test01()
{
  bool test = true;
  std::numeric_limits< A<B> > obj;

  test &= !obj.is_specialized;
  test &= obj.min() == 0;
  test &= obj.max() == 0;
  test &= obj.digits ==  0;
  test &= obj.digits10 == 0;
  test &= !obj.is_signed;
  test &= !obj.is_integer;
  test &= !obj.is_exact;
  test &= obj.radix == 0;
  test &= obj.epsilon() == 0;
  test &= obj.round_error() == 0;
  test &= obj.min_exponent == 0;
  test &= obj.min_exponent10 == 0;
  test &= obj.max_exponent == 0;
  test &= obj.max_exponent10 == 0;
  test &= !obj.has_infinity;
  test &= !obj.has_quiet_NaN;
  test &= !obj.has_signaling_NaN;
  test &= !obj.has_denorm;
  test &= !obj.has_denorm_loss;
  test &= obj.infinity() == 0;
  test &= obj.quiet_NaN() == 0;
  test &= obj.signaling_NaN() == 0;
  test &= obj.denorm_min() == 0;
  test &= !obj.is_iec559;
  test &= !obj.is_bounded;
  test &= !obj.is_modulo;
  test &= !obj.traps;
  test &= !obj.tinyness_before;
  test &= obj.round_style == std::round_toward_zero;

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

int main()
{
  test01();

  return 0;
}





