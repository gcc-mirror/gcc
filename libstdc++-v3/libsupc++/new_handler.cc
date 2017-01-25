// Implementation file for the -*- C++ -*- dynamic memory management header.

// Copyright (C) 1996-2017 Free Software Foundation, Inc.
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

#include "new"
#include <bits/atomic_lockfree_defines.h>

#if ATOMIC_POINTER_LOCK_FREE < 2
#include <ext/concurrence.h>
namespace
{
  __gnu_cxx::__mutex mx;
}
#endif

const std::nothrow_t std::nothrow = std::nothrow_t{ };

using std::new_handler;
namespace
{
  new_handler __new_handler;
}

new_handler
std::set_new_handler (new_handler handler) throw()
{
  new_handler prev_handler;
#if ATOMIC_POINTER_LOCK_FREE > 1
  __atomic_exchange (&__new_handler, &handler, &prev_handler,
		     __ATOMIC_ACQ_REL);
#else
  __gnu_cxx::__scoped_lock l(mx);
  prev_handler = __new_handler;
  __new_handler = handler;
#endif
  return prev_handler;
}

new_handler
std::get_new_handler () noexcept
{
  new_handler handler;
#if ATOMIC_POINTER_LOCK_FREE > 1
  __atomic_load (&__new_handler, &handler, __ATOMIC_ACQUIRE);
#else
  __gnu_cxx::__scoped_lock l(mx);
  handler = __new_handler;
#endif
  return handler;
}
