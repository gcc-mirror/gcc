// -*- C++ -*- compatibility header.

// Copyright (C) 2002-2020 Free Software Foundation, Inc.
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

/** @file string.h
 *  This is a Standard C++ Library header.
 */

#include <cstring>

#ifndef _GLIBCXX_STRING_H
#define _GLIBCXX_STRING_H 1

#ifdef _GLIBCXX_NAMESPACE_C
using std::memcpy;
using std::memmove;
using std::strcpy;
using std::strncpy;
using std::strcat;
using std::strncat;
using std::memcmp;
using std::strcmp;
using std::strcoll;
using std::strncmp;
using std::strxfrm;
using std::memchr;
using std::strchr;
using std::strcspn;
using std::strpbrk;
using std::strrchr;
using std::strspn;
using std::strstr;
using std::strtok;
using std::memset;
using std::strerror;
using std::strlen;
#endif

#endif
