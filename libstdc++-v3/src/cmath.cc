// -*- C++ -*- C math library.

// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 26.5  C library
// Code for signatures not found in the C library
//

#include <bits/std_cmath.h>

namespace std {

  namespace {
    template <typename T>
    inline T pow_helper(T x, unsigned int y)
    {
      T z = y&1? x : 1;
      while(y >>= 1)
        {
          x *= x;
          if(y & 1) z *= x;
        }
      return z;
    }
  }

  float
  pow(float x, int y)
  {
    if(y < 0)
      return 1.0f/pow_helper(x, -y);
    else
      return pow_helper(x, y);
  }

  double
  pow(double x, int y)
  {
    if(y < 0)
      return 1.0/pow_helper(x, -y);
    else
      return pow_helper(x, y);
  }

  long double
  pow(long double x, int y)
  {
    if(y < 0)
      return 1.0l/pow_helper(x, -y);
    else
      return pow_helper(x, y);
  }

} // std





