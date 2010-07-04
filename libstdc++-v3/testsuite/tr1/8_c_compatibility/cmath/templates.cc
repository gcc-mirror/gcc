// { dg-do compile }

// 2006-02-26  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006, 2009, 2010 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 8.16 Additions to header <cmath>

#include <tr1/cmath>

#if _GLIBCXX_USE_C99
#if !_GLIBCXX_USE_C99_FP_MACROS_DYNAMIC

template<typename T>
  void test01_do()
  {
    T x = T();
    
    bool ret;
    int iret;

    ret = std::tr1::signbit(x);
    
    iret = std::tr1::fpclassify(x);
    iret = iret; // Suppress unused warning.
    
    ret = std::tr1::isfinite(x);
    ret = std::tr1::isinf(x);
    ret = std::tr1::isnan(x);
    ret = std::tr1::isnormal(x);
    
    ret = std::tr1::isgreater(x, x);
    ret = std::tr1::isgreaterequal(x, x);
    ret = std::tr1::isless(x, x);
    ret = std::tr1::islessequal(x, x);
    ret = std::tr1::islessgreater(x, x);
    ret = std::tr1::isunordered(x, x);
    ret = ret; // Suppress unused warning.
  }

void test01()
{
  test01_do<float>();
  test01_do<double>();
  test01_do<long double>();
}

#endif
#endif
