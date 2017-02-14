// Low-level type for atomic operations -*- C++ -*-

// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_ATOMIC_WORD_H
#define _GLIBCXX_ATOMIC_WORD_H	1

#ifdef __arch64__
  typedef long _Atomic_word;
#else
  typedef int _Atomic_word;
#endif

#if defined(__sparc_v9__)
// These are necessary under the V9 RMO model, though it is almost never
// used in userspace.
#define _GLIBCXX_READ_MEM_BARRIER \
  __asm __volatile ("membar #LoadLoad":::"memory")
#define _GLIBCXX_WRITE_MEM_BARRIER \
  __asm __volatile ("membar #StoreStore":::"memory")

#elif defined(__sparc_v8__)
// This is necessary under the PSO model.
#define _GLIBCXX_WRITE_MEM_BARRIER __asm __volatile ("stbar":::"memory")

#endif

#endif
