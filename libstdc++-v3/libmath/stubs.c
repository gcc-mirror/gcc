/* Stub definitions for libmath subpart of libstdc++. */

/* Copyright (C) 2001 Free Software Foundation, Inc.

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

#include <math.h>
#include <config.h>

#ifndef _GLIBCPP_HAVE_ATAN2F
float
atan2f(float x, float y)
{
  return (float) atan2(x, y);
}
#endif

#if !defined(_GLIBCPP_HAVE_COSF) && !defined(_GLIBCPP_HAVE___BUILTIN_COSF)
float
cosf(float x)
{
  return (float) cos(x);
}
#endif

#ifndef _GLIBCPP_HAVE_COSHF
float
coshf(float x)
{
  return (float) cosh(x);
}
#endif

#ifndef _GLIBCPP_HAVE_EXPF
float
expf(float x)
{
  return (float) exp(x);
}
#endif

#ifndef _GLIBCPP_HAVE_LOGF
float
logf(float x)
{
  return (float) log(x);
}
#endif

#ifndef _GLIBCPP_HAVE_LOG10F
float
log10f(float x)
{
  return (float) log10(x);
}
#endif

#ifndef _GLIBCPP_HAVE_POWF
float
powf(float x, float y)
{
  return (float) pow(x, y);
}
#endif

#if !defined(_GLIBCPP_HAVE_SINF) && !defined(_GLIBCPP_HAVE___BUILTIN_SINF)
float
sinf(float x)
{
  return (float) sin(x);
}
#endif

#ifndef _GLIBCPP_HAVE_SINHF
float
sinhf(float x)
{
  return (float) sinh(x);
}
#endif

#ifndef _GLIBCPP_HAVE_SQRTF
float
sqrtf(float x)
{
  return (float) sqrt(x);
}
#endif

#ifndef _GLIBCPP_HAVE_TANF
float
tanf(float x)
{
  return (float) tan(x);
}
#endif

#ifndef _GLIBCPP_HAVE_TANHF
float
tanhf(float x)
{
  return (float) tanh(x);
}
#endif
