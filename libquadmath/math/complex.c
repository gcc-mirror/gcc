/* GCC Quad-Precision Math Library
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.
   Written by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>

This file is part of the libquadmath library.
Libquadmath is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libquadmath is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libquadmath; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "quadmath-imp.h"

#ifdef HAVE_FENV_H
# include <fenv.h>
#endif


#define REALPART(z) (__real__(z)) 
#define IMAGPART(z) (__imag__(z)) 
#define COMPLEX_ASSIGN(z_, r_, i_) {__real__(z_) = (r_); __imag__(z_) = (i_);} 


__float128
cabsq (__complex128 z)
{
  return hypotq (REALPART (z), IMAGPART (z));
}


__complex128
cexpiq (__float128 x)
{
  __float128 sinix, cosix;
  __complex128 v;
  sincosq (x, &sinix, &cosix);
  COMPLEX_ASSIGN (v, cosix, sinix);
  return v;
}


__float128
cargq (__complex128 z)
{
  return atan2q (IMAGPART (z), REALPART (z));
}


__complex128
cpowq (__complex128 base, __complex128 power)
{
  return cexpq (power * clogq (base));
}


__complex128
ccosq (__complex128 x)
{
  __complex128 y;

  COMPLEX_ASSIGN (y, -IMAGPART (x), REALPART (x));
  return ccoshq (y);
}
