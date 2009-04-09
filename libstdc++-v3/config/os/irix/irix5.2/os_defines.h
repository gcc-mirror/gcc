// Specific definitions for IRIX  -*- C++ -*-

// Copyright (C) 2001, 2002, 2005, 2009 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_OS_DEFINES
#define _GLIBCXX_OS_DEFINES 1

// System-specific #define, typedefs, corrections, etc, go here.  This
// file will come before all others.

// We need large file support.  There are two ways to turn it on: by
// defining either _LARGEFILE64_SOURCE or _SGI_SOURCE.  However, it
// does not actually work to define only the former, as then
// <sys/stat.h> is invalid: `st_blocks' is defined to be a macro, but
// then used as a field name.  So, we have to turn on _SGI_SOURCE.
// That only works if _POSIX_SOURCE is turned off, so we have to
// explicitly turn it off.  (Some of the libio C files explicitly try
// to turn it on.)  _SGI_SOURCE is actually turned on implicitly via
// the command-line.
#undef _POSIX_SOURCE

// GCC does not use thunks on IRIX. 
#define _G_USING_THUNKS 0

#endif

