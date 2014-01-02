// -*- C++ -*- compatibility header.

// Copyright (C) 2002-2014 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file wctype.h
 *  This is a Standard C++ Library header.
 */

#include <cwctype>

#ifndef _GLIBCXX_CWCTYPE_H
#define _GLIBCXX_CWCTYPE_H 1

#ifdef _GLIBCXX_NAMESPACE_C
using std::wctype_t;
using std::wctrans_t;
using std::iswalpha;
using std::iswupper;
using std::iswlower;
using std::iswdigit;
using std::iswxdigit;
using std::iswalnum;
using std::iswspace;
using std::iswpunct;
using std::iswprint;
using std::iswgraph;
using std::iswcntrl;
using std::iswctype;
using std::towctrans;
using std::towlower;
using std::towupper;
using std::wctrans;
using std::wctype;
#endif

#endif
