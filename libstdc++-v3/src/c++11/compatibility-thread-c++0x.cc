// Compatibility symbols for previous versions, C++0x bits -*- C++ -*-

// Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#if defined(_GLIBCXX_HAVE_TLS) && defined(_GLIBCXX_SHARED)
#define _GLIBCXX_ASYNC_ABI_COMPAT
#endif

#include <future>
#include <mutex>

#ifndef __GXX_EXPERIMENTAL_CXX0X__
# error "compatibility-thread-c++0x.cc must be compiled with -std=gnu++0x"
#endif

#define _GLIBCXX_ASM_SYMVER(cur, old, version) \
   asm (".symver " #cur "," #old "@@@" #version);

// XXX GLIBCXX_ABI Deprecated
// gcc-4.6.0
// <future> export changes
#if defined(_GLIBCXX_SYMVER_GNU) && defined(_GLIBCXX_SHARED) \
    && defined(_GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE) \
    && defined(_GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT)

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
  const std::error_category* future_category = &std::future_category();
}

_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx15future_categoryE, _ZSt15future_category, GLIBCXX_3.4.14)

#endif

// XXX GLIBCXX_ABI Deprecated
// gcc-4.6.0
// <mutex> export changes
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
#if defined(_GLIBCXX_SYMVER_GNU) && defined(_GLIBCXX_SHARED) \
    && defined(_GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE) \
    && defined(_GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT)

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
  std::defer_lock_t defer_lock;
  std::try_to_lock_t try_to_lock;
  std::adopt_lock_t adopt_lock;
}

_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx10adopt_lockE, _ZSt10adopt_lock, GLIBCXX_3.4.11)
_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx10defer_lockE, _ZSt10defer_lock, GLIBCXX_3.4.11)
_GLIBCXX_ASM_SYMVER(_ZN9__gnu_cxx11try_to_lockE, _ZSt11try_to_lock, GLIBCXX_3.4.11)


#endif
#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1


// XXX GLIBCXX_ABI Deprecated
// gcc-4.7.0
// <future> export changes
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1) \
  && (ATOMIC_INT_LOCK_FREE > 1)
#if defined(_GLIBCXX_HAVE_TLS) && defined(_GLIBCXX_SHARED)
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  __future_base::_Async_state_common::~_Async_state_common() { _M_join(); }

  // Explicit instantiation due to -fno-implicit-instantiation.
  template void call_once(once_flag&, void (thread::*&&)(), reference_wrapper<thread>&&);
  template _Bind_simple_helper<void (thread::*)(), reference_wrapper<thread>>::__type __bind_simple(void (thread::*&&)(), reference_wrapper<thread>&&);
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif
#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
