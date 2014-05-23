// FreeBSD wants warning clean system headers:
// { dg-options "-Wall -Wsystem-headers" { target *-*-freebsd* *-*-dragonfly* } }
// { dg-do compile }
// 1999-05-12 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

// 17.4.1.2 Headers

#include <bits/stdc++.h>

// "C" compatibility headers
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <iso646.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef _GLIBCXX_HAVE_WCHAR_H
#include <wchar.h>
#endif
#ifdef _GLIBCXX_HAVE_WCTYPE_H
#include <wctype.h>
#endif
