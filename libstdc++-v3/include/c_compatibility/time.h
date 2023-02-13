// -*- C++ -*- compatibility header.

// Copyright (C) 2002-2023 Free Software Foundation, Inc.
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

/** @file time.h
 *  This is a Standard C++ Library header.
 */

#include <ctime>

#ifndef _GLIBCXX_TIME_H
#define _GLIBCXX_TIME_H 1

#ifdef _GLIBCXX_NAMESPACE_C
// Get rid of those macros defined in <time.h> in lieu of real functions.
#undef clock
#undef difftime
#undef mktime
#undef time
#undef asctime
#undef ctime
#undef gmtime
#undef localtime
#undef strftime

using std::clock_t;
using std::time_t;
using std::tm;

using std::clock;
using std::difftime;
using std::mktime;
using std::time;
using std::asctime;
using std::ctime;
using std::gmtime;
using std::localtime;
using std::strftime;
#endif

#endif
