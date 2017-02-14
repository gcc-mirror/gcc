// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

// PR libstdc++/60401

#include <math.h>

namespace test
{
  template<typename T>
    using F = T*;

  F<float(float)>abs = ::abs;

#ifdef _GLIBCXX_USE_C99_MATH_TR1
  F<float(float)>		acosh		= ::acosh;
  F<float(float)>		asinh		= ::asinh;
  F<float(float)>		atanh		= ::atanh;
  F<float(float)>		cbrt		= ::cbrt;
  F<float(float, float)>	copysign	= ::copysign;
  F<float(float)>		erf		= ::erf;
  F<float(float)>		erfc		= ::erfc;
  F<float(float)>		exp2		= ::exp2;
  F<float(float)>		expm1		= ::expm1;
  F<float(float, float)>	fdim		= ::fdim;
  F<float(float, float, float)>	fma		= ::fma;
  F<float(float, float)>	fmax		= ::fmax;
  F<float(float, float)>	fmin		= ::fmin;
  F<float(float, float)>	hypot		= ::hypot;
  F<int(float)>			ilogb		= ::ilogb;
  F<float(float)>		lgamma		= ::lgamma;
  F<long long(float)>		llrint		= ::llrint;
  F<long long(float)>		llround		= ::llround;
  F<float(float)>		log1p		= ::log1p;
  F<float(float)>		log2		= ::log2;
  F<float(float)>		logb		= ::logb;
  F<long(float)>		lrint		= ::lrint;
  F<long(float)>		lround		= ::lround;
  F<float(float)>		nearbyint	= ::nearbyint;
  F<float(float, float)>	nextafter	= ::nextafter;
  F<float(float, long double)>	nexttoward	= ::nexttoward;
  F<float(float, float)>	remainder	= ::remainder;
  F<float(float, float, int*)>	remquo		= ::remquo;
  F<float(float)>		rint		= ::rint;
  F<float(float)>		round		= ::round;
  F<float(float, long)>		scalbln		= ::scalbln;
  F<float(float, int)>		scalbn		= ::scalbn;
  F<float(float)>		tgamma		= ::tgamma;
  F<float(float)>		trunc		= ::trunc;
#endif
}
