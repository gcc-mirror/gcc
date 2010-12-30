/* GCC Quad-Precision Math Library
   Copyright (C) 2010 Free Software Foundation, Inc.
   Written by Tobias Burnus  <burnus@net-b.de>

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef QUADMATH_WEAK_H
#define QUADMATH_WEAK_H

#include "quadmath.h"

#if SUPPORTS_WEAK
# define __qmath2(name,name2,type) \
  static __typeof(type) name __attribute__ ((__weakref__(#name2)));
# define __qmath_(name) __qmath_ ## name
#else
# define __qmath2(name,name2,type)
# define __qmath_(name) name
#endif

/* __qmath_foo is a weak reference to symbol foo.  */
#define __qmath3(name) __qmath2(__qmath_ ## name,name,name)

// Prototypes for real functions
__qmath3 (acosq)
__qmath3 (acoshq)
__qmath3 (asinq)
__qmath3 (asinhq)
__qmath3 (atanq)
__qmath3 (atanhq)
__qmath3 (atan2q)
__qmath3 (cbrtq)
__qmath3 (ceilq)
__qmath3 (copysignq)
__qmath3 (coshq)
__qmath3 (cosq)
__qmath3 (erfq)
__qmath3 (erfcq)
__qmath3 (expq)
__qmath3 (expm1q)
__qmath3 (fabsq)
__qmath3 (finiteq)
__qmath3 (floorq)
__qmath3 (fmodq)
__qmath3 (frexpq)
__qmath3 (hypotq)
__qmath3 (isinfq)
__qmath3 (isnanq)
__qmath3 (j0q)
__qmath3 (j1q)
__qmath3 (jnq)
__qmath3 (ldexpq)
__qmath3 (lgammaq)
__qmath3 (llroundq)
__qmath3 (logq)
__qmath3 (log10q)
__qmath3 (log1pq)
__qmath3 (lroundq)
__qmath3 (modfq)
__qmath3 (nanq)
__qmath3 (nextafterq)
__qmath3 (powq)
__qmath3 (remainderq)
__qmath3 (rintq)
__qmath3 (roundq)
__qmath3 (scalblnq)
__qmath3 (scalbnq)
__qmath3 (signbitq)
__qmath3 (sincosq)
__qmath3 (sinhq)
__qmath3 (sinq)
__qmath3 (sqrtq)
__qmath3 (tanq)
__qmath3 (tanhq)
__qmath3 (tgammaq)
__qmath3 (truncq)
__qmath3 (y0q)
__qmath3 (y1q)
__qmath3 (ynq)


// Prototypes for complex functions
__qmath3 (cabsq)
__qmath3 (cargq)
__qmath3 (ccosq)
__qmath3 (ccoshq)
__qmath3 (cexpq)
__qmath3 (cexpiq)
__qmath3 (clogq)
__qmath3 (clog10q)
__qmath3 (cpowq)
__qmath3 (csinq)
__qmath3 (csinhq)
__qmath3 (csqrtq)
__qmath3 (ctanq)
__qmath3 (ctanhq)


// Prototypes for our I/O functions
__qmath3 (quadmath_strtopQ)
__qmath3 (quadmath_dtoaq)

#endif
