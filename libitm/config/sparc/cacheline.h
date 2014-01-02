/* Copyright (C) 2012-2014 Free Software Foundation, Inc.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef LIBITM_SPARC_CACHELINE_H
#define LIBITM_SPARC_CACHELINE_H 1

// A cacheline is the smallest unit with which locks are associated.
// The current implementation of the _ITM_[RW] barriers assumes that
// all data types can fit (aligned) within a cachline, which means
// in practice sizeof(complex long double) is the smallest cacheline size.
// It ought to be small enough for efficient manipulation of the
// modification mask, below.
#ifdef __arch64__
# define CACHELINE_SIZE 64
#else
# define CACHELINE_SIZE 32
#endif

#include "config/generic/cacheline.h"

#endif // LIBITM_SPARC_CACHELINE_H
