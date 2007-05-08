// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <cmath>

namespace gnu
{
  // C++0x changes from TR1.
  using std::assoc_laguerre;
  using std::assoc_legendre;
  using std::beta; 
  using std::comp_ellint_1;
  using std::comp_ellint_2;
  using std::comp_ellint_3;
  using std::conf_hyperg;
  using std::cyl_bessel_i;
  using std::cyl_bessel_j;
  using std::cyl_bessel_k;
  using std::cyl_neumann;
  using std::ellint_1;
  using std::ellint_2;
  using std::ellint_3;
  using std::expint;  
  using std::hermite;
  using std::hyperg;
  using std::laguerre;
  using std::legendre;
  using std::riemann_zeta;
  using std::sph_bessel;
  using std::sph_legendre;
  using std::sph_neumann;
}

// { dg-error "has not been declared" "" { xfail *-*-* } 27 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 28 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 29 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 30 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 31 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 32 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 33 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 34 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 35 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 36 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 37 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 38 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 39 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 40 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 41 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 42 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 43 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 44 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 45 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 46 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 47 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 48 } 
// { dg-error "has not been declared" "" { xfail *-*-* } 49 } 
