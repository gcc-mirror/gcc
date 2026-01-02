// Function-Based Exception Support -*- C++ -*-

// Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

/** @file bits/stdexcept_throw.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{stdexcept}
 */

//
// ISO C++ 14882: 19.1  Exception classes
//

#include <bits/c++config.h>
#include <bits/exception_defines.h>

#if (_GLIBCXX_HOSTED && __cpp_exceptions && __cplusplus > 202302L \
     && __cpp_constexpr_exceptions >= 202411L)
// This is a complicated case.  Classes like std::logic_error
// are defined in <stdexcept> but need std::string and <string>
// needs __throw_logic_error and a few others.  So, for C++26
// constant expression we need to forward declare the constexpr
// __throw_logic_error etc. functions when included recursively
// from <string> and then at the end of that header make sure
// <stdexcept> is included and define those.
// If <string> hasn't been included yet, include it at the end
// of this header and that will arrange for all these to be
// defined.
#if defined(_GLIBCXX_STRING) || __glibcxx_exc_in_string == 2
#ifdef __glibcxx_exc_in_string
#include <bits/stdexcept_throwfwd.h>
#else
#include <bits/stdexcept_throwdef.h>
#endif
#endif
#if !defined(_GLIBCXX_STRING) && __glibcxx_exc_in_string != 2
#include <string>
#endif
#else
#include <bits/stdexcept_throwfwd.h>
#endif
