// { dg-do compile { target { ! c++17 } } }

// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

#include <cmath>

namespace gnu
{
  // C++11 changes from TR1.
  using std::assoc_laguerre;	// { dg-error "is not a member of" }
  using std::assoc_legendre;	// { dg-error "is not a member of" }
  using std::beta;		// { dg-error "is not a member of" }
  using std::comp_ellint_1;	// { dg-error "is not a member of" }
  using std::comp_ellint_2;	// { dg-error "is not a member of" }
  using std::comp_ellint_3;	// { dg-error "is not a member of" }
  using std::conf_hyperg;	// { dg-error "is not a member of" }
  using std::cyl_bessel_i;	// { dg-error "is not a member of" }
  using std::cyl_bessel_j;	// { dg-error "is not a member of" }
  using std::cyl_bessel_k;	// { dg-error "is not a member of" }
  using std::cyl_neumann;	// { dg-error "is not a member of" }
  using std::ellint_1;		// { dg-error "is not a member of" }
  using std::ellint_2;		// { dg-error "is not a member of" }
  using std::ellint_3;		// { dg-error "is not a member of" }
  using std::expint;		// { dg-error "is not a member of" }
  using std::hermite;		// { dg-error "is not a member of" }
  using std::hyperg;		// { dg-error "is not a member of" }
  using std::laguerre;		// { dg-error "is not a member of" }
  using std::legendre;		// { dg-error "is not a member of" }
  using std::riemann_zeta;	// { dg-error "is not a member of" }
  using std::sph_bessel;	// { dg-error "is not a member of" }
  using std::sph_legendre;	// { dg-error "is not a member of" }
  using std::sph_neumann;	// { dg-error "is not a member of" }
}
