/* Copyright (C) 2009, 2011 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

#include "libitm_i.h"


using namespace GTM;

extern "C" {

/* Wrap: malloc (size_t sz)  */
void *
_ITM_malloc (size_t sz)
{
  void *r = malloc (sz);
  if (r)
    gtm_thr()->record_allocation (r, free);
  return r;
}

/* Wrap: calloc (size_t nm, size_t sz)  */
void *
_ITM_calloc (size_t nm, size_t sz)
{
  void *r = calloc (nm, sz);
  if (r)
    gtm_thr()->record_allocation (r, free);
  return r;
}

/* Wrap:  free (void *ptr)  */
void
_ITM_free (void *ptr)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, free);
}

/* Forget any internal references to PTR.  */

__attribute__((transaction_pure))
void ITM_REGPARM
_ITM_dropReferences (void *ptr, size_t len)
{
  // The semantics of _ITM_dropReferences are not sufficiently defined in the
  // ABI specification, so it does not make sense to support it right now. See
  // the libitm documentation for details.
  GTM_fatal("_ITM_dropReferences is not supported");
}

} // extern "C"
