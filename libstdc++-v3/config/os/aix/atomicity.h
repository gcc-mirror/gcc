// Low-level functions for atomic operations: AIX version  -*- C++ -*-

// Copyright (C) 2000-2018 Free Software Foundation, Inc.
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

#include <ext/atomicity.h>

/* We cannot use the cpu/powerpc/bits/atomicity.h inline assembly
   definitions for these operations since they depend on operations
   that are not available on the original POWER architecture.  AIX
   still runs on the POWER architecture, so it would be incorrect to
   assume the existence of these instructions.

   The definition of _Atomic_word must match the type pointed to by
   atomic_p in <sys/atomic_op.h>.  */

extern "C"
{
#include <sys/atomic_op.h>
}

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  _Atomic_word
  __attribute__ ((__unused__))
  __exchange_and_add (volatile _Atomic_word* __mem, int __val) throw ()
  { return ::fetch_and_add(const_cast<atomic_p>(__mem), __val); }

  void
  __attribute__ ((__unused__))
  __atomic_add (volatile _Atomic_word* __mem, int __val) throw ()
  { (void) ::fetch_and_add(const_cast<atomic_p>(__mem), __val); }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
