// 2000-02-10
// Petter Urkedal <petter@matfys.lth.se>

// Copyright (C) 2000 Free Software Foundation
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


#include <iostream>
#include <string>
#include <sstream>
#include <complex>
#include <debug_assert.h>
#include <cmath>

template<typename R>
inline bool flteq(R x, R y)
{
  if (x == R(0)) return y == R(0);
  else return std::fabs(x-y) < 1e-6*std::fabs(x);
}

template<typename R>
int
test_good(std::string str, R x, R y)
{
  bool test = true;
  std::complex<R> z;
  char ch;
  std::istringstream iss(str);
  iss >> z >> ch;
  VERIFY( iss.good() );
  VERIFY( flteq(z.real(), x) );
  VERIFY( flteq(z.imag(), y) );
  VERIFY( ch == '#' );
  
#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return 0;
}

template<typename R>
int
test_fail(std::string str)
{
  std::complex<R> z;
  std::istringstream iss(str);
  iss >> z;
#ifdef DEBUG_ASSERT
  assert(iss.fail() && !iss.bad());
#endif
  return 0;
}

template<typename R>
int
testall()
{
  test_good<R>("(-1.1,3.7)#", -1.1, 3.7);
  test_good<R>("(  .7e6  ,  \n-3.1)#", .7e6, -3.1);
  test_good<R>("(\t0,-1)#", 0.0, -1.0);
  test_good<R>("(-3.14)#", -3.14, 0.0);
  test_good<R>("-.1#", -.1, 0.0);
  test_good<R>(" ( -2.7e3 )#", -2.7e3, 0.0);
  test_good<R>(" -.1#", -.1, 0.0);
  test_fail<R>("(a,1)");
  test_fail<R>("(,1)");
  test_fail<R>("(1,a)");
  test_fail<R>("(1, )");
  test_fail<R>("|1,1)");
  test_fail<R>("(1|1)");
  test_fail<R>("(1,1|");
  return 0;
}

int
main()
{
  testall<float>();
  testall<double>();
  testall<long double>();
  return 0;
}
