// Copyright (C) 2007-2025 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
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

#include <cxxabi.h>

#if defined(__arm__) && defined(__ARM_EABI__)

namespace __aeabiv1
{
  extern "C" int
  __aeabi_atexit (void *object,
		  void (*destructor) (void *),
		  void *dso_handle) throw ()
  {
    return abi::__cxa_atexit(destructor, object, dso_handle);
  }
} // namespace __aeabiv1

#endif // defined(__arm__) && defined(__ARM_EABI__)
