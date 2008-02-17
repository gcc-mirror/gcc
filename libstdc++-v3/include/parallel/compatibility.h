// -*- C++ -*-

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
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

/** @file parallel/compatibility.h
 *  @brief Compatibility layer, mostly concerned with atomic operations.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_COMPATIBILITY_H
#define _GLIBCXX_PARALLEL_COMPATIBILITY_H 1

#include <parallel/types.h>
#include <parallel/base.h>

#if defined(__SUNPRO_CC) && defined(__sparc)
#include <sys/atomic.h>
#endif

#if !defined(_WIN32) || defined (__CYGWIN__)
#include <sched.h>
#endif

#if defined(_MSC_VER)
#include <Windows.h>
#include <intrin.h>
#undef max
#undef min
#endif

#ifdef __MINGW32__
// Including <windows.h> will drag in all the windows32 names.  Since
// that can cause user code portability problems, we just declare the
// one needed function here.
extern "C"
__attribute((dllimport)) void __attribute__((stdcall)) Sleep (unsigned long);
#endif

namespace __gnu_parallel
{
#if defined(__ICC)
  template<typename must_be_int = int>
  int32 faa32(int32* x, int32 inc)
  {
    asm volatile("lock xadd %0,%1"
		 : "=r" (inc), "=m" (*x)
		 : "0" (inc)
		 : "memory");
    return inc;
  }
#if defined(__x86_64)
  template<typename must_be_int = int>
  int64 faa64(int64* x, int64 inc)
  {
    asm volatile("lock xadd %0,%1"
		 : "=r" (inc), "=m" (*x)
		 : "0" (inc)
		 : "memory");
    return inc;
  }
#endif
#endif

  // atomic functions only work on integers

  /** @brief Add a value to a variable, atomically.
   *
   *  Implementation is heavily platform-dependent.
   *  @param ptr Pointer to a 32-bit signed integer.
   *  @param addend Value to add.
   */
  inline int32
  fetch_and_add_32(volatile int32* ptr, int32 addend)
  {
#if defined(__ICC)	//x86 version
    return _InterlockedExchangeAdd((void*)ptr, addend);
#elif defined(__ECC)	//IA-64 version
    return _InterlockedExchangeAdd((void*)ptr, addend);
#elif defined(__ICL) || defined(_MSC_VER)
    return _InterlockedExchangeAdd(reinterpret_cast<volatile long*>(ptr),
				   addend);
#elif defined(__GNUC__)
    return __sync_fetch_and_add(ptr, addend);
#elif defined(__SUNPRO_CC) && defined(__sparc)
    volatile int32 before, after;
    do
      {
	before = *ptr;
	after = before + addend;
      } while (atomic_cas_32((volatile unsigned int*)ptr, before,
			     after) != before);
    return before;
#else	//fallback, slow
#pragma message("slow fetch_and_add_32")
    int32 res;
#pragma omp critical
    {
      res = *ptr;
      *(ptr) += addend;
    }
    return res;
#endif
  }

  /** @brief Add a value to a variable, atomically.
   *
   *  Implementation is heavily platform-dependent.
   *  @param ptr Pointer to a 64-bit signed integer.
   *  @param addend Value to add.
   */
  inline int64
  fetch_and_add_64(volatile int64* ptr, int64 addend)
  {
#if defined(__ICC) && defined(__x86_64)	//x86 version
    return faa64<int>((int64*)ptr, addend);
#elif defined(__ECC)	//IA-64 version
    return _InterlockedExchangeAdd64((void*)ptr, addend);
#elif defined(__ICL) || defined(_MSC_VER)
#ifndef _WIN64
    _GLIBCXX_PARALLEL_ASSERT(false);	//not available in this case
    return 0;
#else
    return _InterlockedExchangeAdd64(ptr, addend);
#endif
#elif defined(__GNUC__) && defined(__x86_64)
    return __sync_fetch_and_add(ptr, addend);
#elif defined(__GNUC__) && defined(__i386) &&			\
  (defined(__i686) || defined(__pentium4) || defined(__athlon))
    return __sync_fetch_and_add(ptr, addend);
#elif defined(__SUNPRO_CC) && defined(__sparc)
    volatile int64 before, after;
    do
      {
	before = *ptr;
	after = before + addend;
      } while (atomic_cas_64((volatile unsigned long long*)ptr, before,
			     after) != before);
    return before;
#else	//fallback, slow
#if defined(__GNUC__) && defined(__i386)
    // XXX doesn't work with -march=native
    //#warning "please compile with -march=i686 or better"
#endif
#pragma message("slow fetch_and_add_64")
    int64 res;
#pragma omp critical
    {
      res = *ptr;
      *(ptr) += addend;
    }
    return res;
#endif
  }

  /** @brief Add a value to a variable, atomically.
   *
   *  Implementation is heavily platform-dependent.
   *  @param ptr Pointer to a signed integer.
   *  @param addend Value to add.
   */
  template<typename T>
  inline T
  fetch_and_add(volatile T* ptr, T addend)
  {
    if (sizeof(T) == sizeof(int32))
      return (T)fetch_and_add_32((volatile int32*) ptr, (int32)addend);
    else if (sizeof(T) == sizeof(int64))
      return (T)fetch_and_add_64((volatile int64*) ptr, (int64)addend);
    else
      _GLIBCXX_PARALLEL_ASSERT(false);
  }


#if defined(__ICC)

  template<typename must_be_int = int>
  inline int32
  cas32(volatile int32* ptr, int32 old, int32 nw)
  {
    int32 before;
    __asm__ __volatile__("lock; cmpxchgl %1,%2"
			 : "=a"(before)
			 : "q"(nw), "m"(*(volatile long long*)(ptr)), "0"(old)
			 : "memory");
    return before;
  }

#if defined(__x86_64)
  template<typename must_be_int = int>
  inline int64
  cas64(volatile int64 *ptr, int64 old, int64 nw)
  {
    int64 before;
    __asm__ __volatile__("lock; cmpxchgq %1,%2"
			 : "=a"(before)
			 : "q"(nw), "m"(*(volatile long long*)(ptr)), "0"(old)
			 : "memory");
    return before;
  }
#endif

#endif

  /** @brief Compare @c *ptr and @c comparand. If equal, let @c
   * *ptr=replacement and return @c true, return @c false otherwise.
   *
   *  Implementation is heavily platform-dependent.
   *  @param ptr Pointer to 32-bit signed integer.
   *  @param comparand Compare value.
   *  @param replacement Replacement value.
   */
  inline bool
  compare_and_swap_32(volatile int32* ptr, int32 comparand, int32 replacement)
  {
#if defined(__ICC)	//x86 version
    return _InterlockedCompareExchange((void*)ptr, replacement,
				       comparand) == comparand;
#elif defined(__ECC)	//IA-64 version
    return _InterlockedCompareExchange((void*)ptr, replacement,
				       comparand) == comparand;
#elif defined(__ICL) || defined(_MSC_VER)
    return _InterlockedCompareExchange(reinterpret_cast<volatile long*>(ptr),
				       replacement, comparand) == comparand;
#elif defined(__GNUC__)
    return __sync_bool_compare_and_swap(ptr, comparand, replacement);
#elif defined(__SUNPRO_CC) && defined(__sparc)
    return atomic_cas_32((volatile unsigned int*)ptr, comparand,
			 replacement) == comparand;
#else
#pragma message("slow compare_and_swap_32")
    bool res = false;
#pragma omp critical
    {
      if (*ptr == comparand)
	{
	  *ptr = replacement;
	  res = true;
	}
    }
    return res;
#endif
  }

  /** @brief Compare @c *ptr and @c comparand. If equal, let @c
   * *ptr=replacement and return @c true, return @c false otherwise.
   *
   *  Implementation is heavily platform-dependent.
   *  @param ptr Pointer to 64-bit signed integer.
   *  @param comparand Compare value.
   *  @param replacement Replacement value.
   */
  inline bool
  compare_and_swap_64(volatile int64* ptr, int64 comparand, int64 replacement)
  {
#if defined(__ICC) && defined(__x86_64)	//x86 version
    return cas64<int>(ptr, comparand, replacement) == comparand;
#elif defined(__ECC)	//IA-64 version
    return _InterlockedCompareExchange64((void*)ptr, replacement,
					 comparand) == comparand;
#elif defined(__ICL) || defined(_MSC_VER)
#ifndef _WIN64
    _GLIBCXX_PARALLEL_ASSERT(false);	//not available in this case
    return 0;
#else
    return _InterlockedCompareExchange64(ptr, replacement,
					 comparand) == comparand;
#endif

#elif defined(__GNUC__) && defined(__x86_64)
    return __sync_bool_compare_and_swap(ptr, comparand, replacement);
#elif defined(__GNUC__) && defined(__i386) &&			\
  (defined(__i686) || defined(__pentium4) || defined(__athlon))
    return __sync_bool_compare_and_swap(ptr, comparand, replacement);
#elif defined(__SUNPRO_CC) && defined(__sparc)
    return atomic_cas_64((volatile unsigned long long*)ptr,
			 comparand, replacement) == comparand;
#else
#if defined(__GNUC__) && defined(__i386)
    // XXX -march=native
    //#warning "please compile with -march=i686 or better"
#endif
#pragma message("slow compare_and_swap_64")
    bool res = false;
#pragma omp critical
    {
      if (*ptr == comparand)
	{
	  *ptr = replacement;
	  res = true;
	}
    }
    return res;
#endif
  }

  /** @brief Compare @c *ptr and @c comparand. If equal, let @c
   * *ptr=replacement and return @c true, return @c false otherwise.
   *
   *  Implementation is heavily platform-dependent.
   *  @param ptr Pointer to signed integer.
   *  @param comparand Compare value.
   *  @param replacement Replacement value. */
  template<typename T>
  inline bool
  compare_and_swap(volatile T* ptr, T comparand, T replacement)
  {
    if (sizeof(T) == sizeof(int32))
      return compare_and_swap_32((volatile int32*) ptr, (int32)comparand, (int32)replacement);
    else if (sizeof(T) == sizeof(int64))
      return compare_and_swap_64((volatile int64*) ptr, (int64)comparand, (int64)replacement);
    else
      _GLIBCXX_PARALLEL_ASSERT(false);
  }

  /** @brief Yield the control to another thread, without waiting for
      the end to the time slice. */
  inline void
  yield()
  {
#if defined (_WIN32) && !defined (__CYGWIN__)
    Sleep(0);
#else
    sched_yield();
#endif
  }
} // end namespace

#endif
