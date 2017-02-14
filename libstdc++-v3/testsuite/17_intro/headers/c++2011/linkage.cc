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

// { dg-options "-std=gnu++11" }
// { dg-do compile }

// libstdc++/69386

#include <bits/c++config.h>

extern "C"
{
#include <assert.h>
// See below for <complex.h>
#include <ctype.h>
#include <errno.h>
#ifdef _GLIBCXX_HAVE_FENV_H
#include <fenv.h>
#endif
#include <float.h>
#ifdef _GLIBCXX_HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include <iso646.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#if _GLIBCXX_HAVE_STDALIGN_H
#include <stdalign.h>
#endif
#include <stdarg.h>
#ifdef _GLIBCXX_HAVE_STDBOOL_H
#include <stdbool.h>
#endif
#include <stddef.h>
#ifdef _GLIBCXX_HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _GLIBCXX_HAVE_TGMATH_H
#include <tgmath.h>
#endif
#include <time.h>
#if __has_include(<uchar.h>)
#include <uchar.h>
#endif
#ifdef _GLIBCXX_HAVE_WCHAR_H
#include <wchar.h>
#endif
#ifdef _GLIBCXX_HAVE_WCTYPE_H
#include <wctype.h>
#endif

// Include this last, because it adds extern "C++" and so hides problems in
// other headers if included first (e.g. PR libstdc++/77814).
#ifdef _GLIBCXX_HAVE_COMPLEX_H
#include <complex.h>
#endif
}
