// Control various target specific ABI tweaks.  ARM version.

// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _CXXABI_TWEAKS_H
#define _CXXABI_TWEAKS_H 1

#ifdef __cplusplus
namespace __cxxabiv1
{
#endif

#ifdef __ARM_EABI__
  // The ARM EABI uses the least significan bit of a 32-bit
  // guard variable.  */
#define _GLIBCXX_GUARD_ACQUIRE(x) (!(*(x) & 1))
#define _GLIBCXX_GUARD_RELEASE(x) *(x) = 1
  typedef int __guard;

  // We also want the element size in array cookies.
#define _GLIBCXX_ELTSIZE_IN_COOKIE 1
  
#else
  // The generic ABI uses the first byte of a 64-bit guard variable.
#define _GLIBCXX_GUARD_ACQUIRE(x) (!*(char *) (x))
#define _GLIBCXX_GUARD_RELEASE(x) *(char *) (x) = 1
  __extension__ typedef int __guard __attribute__((mode (__DI__)));
#endif

#ifdef __cplusplus
} // namespace __cxxabiv1
#endif

#endif // __cxxabiv1
