// -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

/** @file parallel/compiletime_settings.h
 *  @brief Defines on options concerning debugging and performance, at
 *  compile-time.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#include <cstdio>

/** @brief Determine verbosity level of the parallel mode.
 *  Level 1 prints a message each time a parallel-mode function is entered. */
#define _GLIBCXX_VERBOSE_LEVEL 0

/** @def _GLIBCXX_CALL
 *  @brief Macro to produce log message when entering a function.
 *  @param n Input size.
 *  @see _GLIBCXX_VERBOSE_LEVEL */
#if (_GLIBCXX_VERBOSE_LEVEL == 0)
#define _GLIBCXX_CALL(n)
#endif
#if (_GLIBCXX_VERBOSE_LEVEL == 1)
#define _GLIBCXX_CALL(n) \
  printf("   %s:\niam = %d, n = %ld, num_threads = %d\n", \
  __PRETTY_FUNCTION__, omp_get_thread_num(), (n), get_max_threads());
#endif

#ifndef _GLIBCXX_SCALE_DOWN_FPU
/** @brief Use floating-point scaling instead of modulo for mapping
 *  random numbers to a range.  This can be faster on certain CPUs. */
#define _GLIBCXX_SCALE_DOWN_FPU 0
#endif

#ifndef _GLIBCXX_ASSERTIONS
/** @brief Switch on many _GLIBCXX_PARALLEL_ASSERTions in parallel code.
 *  Should be switched on only locally. */
#define _GLIBCXX_ASSERTIONS 0
#endif

#ifndef _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_L1
/** @brief Switch on many _GLIBCXX_PARALLEL_ASSERTions in parallel code.
 *  Consider the size of the L1 cache for
 *  __gnu_parallel::parallel_random_shuffle(). */
#define _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_L1 0
#endif
#ifndef _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_TLB
/** @brief Switch on many _GLIBCXX_PARALLEL_ASSERTions in parallel code.
 *  Consider the size of the TLB for
 *  __gnu_parallel::parallel_random_shuffle(). */
#define _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_TLB 0
#endif
