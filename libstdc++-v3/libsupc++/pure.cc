// -*- C++ -*- 
// Copyright (C) 2000-2013 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <bits/c++config.h>
#include <cxxabi.h>
#include "unwind-cxx.h"

#if _GLIBCXX_HOSTED && _GLIBCXX_VERBOSE
#ifdef _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
# define writestr(str)	write(2, str, sizeof(str) - 1)
# ifdef __GNU_LIBRARY__
  /* Avoid forcing the library's meaning of `write' on the user program
     by using the "internal" name (for use within the library).  */
/*#  define write(fd, buf, n)	__write((fd), (buf), (n))*/
# endif
#else
# include <cstdio>
# define writestr(str)	std::fputs(str, stderr)
#endif
#else
# define writestr(str) /* Empty */
#endif

extern "C" void
__cxxabiv1::__cxa_pure_virtual (void)
{
  writestr ("pure virtual method called\n");
  std::terminate ();
}

extern "C" void
__cxxabiv1::__cxa_deleted_virtual (void)
{
  writestr ("deleted virtual method called\n");
  std::terminate ();
}
