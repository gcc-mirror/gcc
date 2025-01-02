// Low-level functions for atomic operations: sh version  -*- C++ -*-

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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

// Use the default atomicity stuff, which will use __atomic* builtins
// if threads are available, or the *_single functions on single-thread
// configurations.
// Actually we wouldn't need this header at all, but because of PR 53579
// libstdc++'s configury will not pickup the -matomic-model= option when
// set in the environment.  This makes it impossible to enable the proper
// atomic model on SH without modifying GCC itself, because libstdc++ always
// thinks the target doesn't do any atomics and uses the default mutex based
// implementation from cpu/generic/atomicity_mutex.

#include <ext/atomicity.h>
