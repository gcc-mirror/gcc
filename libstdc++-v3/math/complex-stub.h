
/* Copyright (C) 1997-1999 Free Software Foundation, Inc.

   This file is part of the GNU ISO C++ Library.  This library is free
   software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this library; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.

   As a special exception, you may use this file as part of a free software
   library without restriction.  Specifically, if other files instantiate
   templates or use macros or inline functions from this file, or you compile
   this file and link it with other files to produce an executable, this
   file does not by itself cause the resulting executable to be covered by
   the GNU General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be covered by
   the GNU General Public License.  */


/* This is no header meant to be used in general.  It's simply here to
   get libstdc++ compiled.  It should never be installed in an official
   directory.  */

#ifndef _COMPLEX_H
#define _COMPLEX_H	1

__complex__ double ccos (__complex__ double x);
__complex__ float ccosf (__complex__ float x);
__complex__ long double ccosl (__complex__ long double x);

__complex__ double ccosh (__complex__ double x);
__complex__ float ccoshf (__complex__ float x);
__complex__ long double ccoshl (__complex__ long double x);

__complex__ double cexp (__complex__ double x);
__complex__ float cexpf (__complex__ float x);
__complex__ long double cexpl (__complex__ long double x);

__complex__ double clog10 (__complex__ double x);
__complex__ float clog10f (__complex__ float x);
__complex__ long double clog10l (__complex__ long double x);

__complex__ double cpow (__complex__ double x, __complex__ double c);
__complex__ float cpowf (__complex__ float x, __complex__ float c);
__complex__ long double cpowl (__complex__ long double x, __complex__ long double c);

__complex__ double csin (__complex__ double x);
__complex__ float csinf (__complex__ float x);
__complex__ long double csinl (__complex__ long double x);

__complex__ double csinh (__complex__ double x);
__complex__ float csinhf (__complex__ float x);
__complex__ long double csinhl (__complex__ long double x);

__complex__ double csqrt (__complex__ double x);
__complex__ float csqrtf (__complex__ float x);
__complex__ long double csqrtl (__complex__ long double x);

__complex__ double ctan (__complex__ double x);
__complex__ float ctanf (__complex__ float x);
__complex__ long double ctanl (__complex__ long double x);

__complex__ double ctanh (__complex__ double x);
__complex__ float ctanhf (__complex__ float x);
__complex__ long double ctanhl (__complex__ long double x);

double carg (__complex__ double x);
float cargf (__complex__ float x);
long double cargl (__complex__ long double x);

double cabs (__complex__ double x);
float cabsf (__complex__ float x);
long double cabsl (__complex__ long double x);

double nan (void);

#endif

