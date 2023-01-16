// Specific definitions for VxWorks  -*- C++ -*-

// Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

/** @file bits/os_defines.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iosfwd}
 */

#ifndef _GLIBCXX_OS_DEFINES
#define _GLIBCXX_OS_DEFINES 1

// System-specific #define, typedefs, corrections, etc, go here.  This
// file will come before all others.

// The system environment we can rely on varies across VxWorks
// versions.
#include <_vxworks-versions.h>

// Weak refs are supported starting with VxWorks 7, in RTP mode only

#if _VXWORKS_MAJOR_GE(7) && defined(__RTP__)
#define _GLIBCXX_USE_WEAK_REF 1
#else
#define _GLIBCXX_USE_WEAK_REF 0
#endif

// We support TLS on VxWorks >= 6.6 (either directly or with emutls)
#if !_VXWORKS_PRE(6, 6)
#define _GLIBCXX_HAVE_TLS 1
#endif

// VxWorks7 comes with a DinkumWare library and the system headers which we
// are going to include for libstdc++ have a few related intrinsic
// assumptions.  We control our own configuration here to best integrate with
// this environment: use C99 math headers, do not use the FP macros for
// dynamic cast by default (overriden for RTPs below) and arrange to disable
// the system TR1 declarations as we'll provide ours.

#if _VXWORKS_MAJOR_GE(7)

#define _GLIBCXX_USE_C99_MATH 1
#define _GLIBCXX_USE_C99_FP_MACROS_DYNAMIC 0

#undef _HAS_TR1_DECLARATIONS
#define _HAS_TR1_DECLARATIONS 0

#else  // VXWORKS_MAJOR < 7

// For RTPs, use the VxWorks headers as a basis, from which we can use
// C99 dynamic FP macros and expect (after fixincludes) accurate c++11
// prototypes for FP.

#ifdef __RTP__

#define _GLIBCXX_INCLUDE_NEXT_C_HEADERS 1

#undef _GLIBCXX_USE_C99_FP_MACROS_DYNAMIC
#define _GLIBCXX_USE_C99_FP_MACROS_DYNAMIC 1

#define __CORRECT_ISO_CPP11_MATH_H_PROTO_FP

#else // !__RTP__

// libstdc++ will include system headers and vxWorks.h ought to have
// been included ahead for VxWorks kernel modules prior to VxWorks 7
#include <vxWorks.h>

#endif // __RTP__

#endif // _VXWORKS_MAJOR >= 7

// The min/max "functions" may be refered to with a namespace prefix.
// Prevent possible redefinitions as macros from VxWorks headers.
#undef NOMINMAX
#define NOMINMAX 1

// Do not include the inline definitions from VxWorks headers, as we'll
// want to use the ones from libstdc++ instead.
#undef _NO_CPP_INLINES
#define _NO_CPP_INLINES 1

#endif // _GLIBCXX_OS_DEFINES
