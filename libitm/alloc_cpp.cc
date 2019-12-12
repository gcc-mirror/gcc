/* Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

/* Mangling the names by hand requires that we know how size_t is handled.
   We've gotten the letter from autoconf, now substitute it into the names.
   Everything below uses X as a placeholder for clarity.  */

#define S1(x,y)			x##y
#define S(x,y)			S1(x,y)

#define _ZnwX			S(_Znw,MANGLE_SIZE_T)
#define _ZnaX			S(_Zna,MANGLE_SIZE_T)
#define _ZdlPvX			S(_ZdlPv,MANGLE_SIZE_T)
#define _ZnwXRKSt9nothrow_t	S(S(_Znw,MANGLE_SIZE_T),RKSt9nothrow_t)
#define _ZnaXRKSt9nothrow_t	S(S(_Zna,MANGLE_SIZE_T),RKSt9nothrow_t)
#define _ZdlPvXRKSt9nothrow_t	S(S(_ZdlPv,MANGLE_SIZE_T),RKSt9nothrow_t)

#define _ZGTtnwX		S(_ZGTtnw,MANGLE_SIZE_T)
#define _ZGTtnaX		S(_ZGTtna,MANGLE_SIZE_T)
#define _ZGTtdlPvX		S(_ZGTtdlPv,MANGLE_SIZE_T)
#define _ZGTtnwXRKSt9nothrow_t	S(S(_ZGTtnw,MANGLE_SIZE_T),RKSt9nothrow_t)
#define _ZGTtnaXRKSt9nothrow_t	S(S(_ZGTtna,MANGLE_SIZE_T),RKSt9nothrow_t)
#define _ZGTtdlPvXRKSt9nothrow_t S(S(_ZGTtdlPv,MANGLE_SIZE_T),RKSt9nothrow_t)

/* Everything from libstdc++ is weak, to avoid requiring that library
   to be linked into plain C applications using libitm.so.  */

extern "C" {

extern void *_ZnwX  (size_t) __attribute__((weak));
extern void _ZdlPv  (void *) __attribute__((weak));
extern void _ZdlPvX (void *, size_t) __attribute__((weak));
extern void *_ZnaX  (size_t) __attribute__((weak));
extern void _ZdaPv  (void *) __attribute__((weak));

typedef const struct nothrow_t { } *c_nothrow_p;

extern void *_ZnwXRKSt9nothrow_t (size_t, c_nothrow_p) __attribute__((weak));
extern void _ZdlPvRKSt9nothrow_t (void *, c_nothrow_p) __attribute__((weak));
extern void _ZdlPvXRKSt9nothrow_t
(void *, size_t, c_nothrow_p) __attribute__((weak));
extern void *_ZnaXRKSt9nothrow_t (size_t, c_nothrow_p) __attribute__((weak));
extern void _ZdaPvRKSt9nothrow_t (void *, c_nothrow_p) __attribute__((weak));

#if !defined (HAVE_ELF_STYLE_WEAKREF) 
void *_ZnwX  (size_t) { return NULL; }
void _ZdlPv  (void *) { return; }
void _ZdlPvX (void *, size_t) { return; }
void *_ZnaX  (size_t) { return NULL; }
void _ZdaPv  (void *) { return; }

void *_ZnwXRKSt9nothrow_t  (size_t, c_nothrow_p) { return NULL; }
void _ZdlPvRKSt9nothrow_t  (void *, c_nothrow_p) { return; }
void _ZdlPvXRKSt9nothrow_t (void *, size_t, c_nothrow_p) { return; }
void *_ZnaXRKSt9nothrow_t  (size_t, c_nothrow_p) { return NULL; }
void _ZdaPvRKSt9nothrow_t  (void *, c_nothrow_p) { return; }
#endif /* HAVE_ELF_STYLE_WEAKREF */

/* Wrap the delete nothrow symbols for usage with a single argument.
   Perhaps should have a configure type check for this, because the
   std::nothrow_t reference argument is unused (empty class), and most
   targets don't actually need that second argument.  So we _could_
   invoke these functions as if they were a single argument free.  */
static void
del_opnt (void *ptr)
{
  _ZdlPvRKSt9nothrow_t (ptr, NULL);
}

static void
del_opvnt (void *ptr)
{
  _ZdaPvRKSt9nothrow_t (ptr, NULL);
}

static void
delsz_opnt (void *ptr, size_t sz)
{
  _ZdlPvXRKSt9nothrow_t (ptr, sz, NULL);
}

/* Wrap: operator new (std::size_t sz)  */
void *
_ZGTtnwX (size_t sz)
{
  void *r = _ZnwX (sz);
  if (r)
    gtm_thr()->record_allocation (r, _ZdlPv);
  return r;
}

/* Wrap: operator new (std::size_t sz, const std::nothrow_t&)  */
void *
_ZGTtnwXRKSt9nothrow_t (size_t sz, c_nothrow_p nt)
{
  void *r = _ZnwXRKSt9nothrow_t (sz, nt);
  if (r)
    gtm_thr()->record_allocation (r, del_opnt);
  return r;
}

/* Wrap: operator new[] (std::size_t sz)  */
void *
_ZGTtnaX (size_t sz)
{
  void *r = _ZnaX (sz);
  if (r)
    gtm_thr()->record_allocation (r, _ZdaPv);
  return r;
}

/* Wrap: operator new[] (std::size_t sz, const std::nothrow_t& nothrow)  */
void *
_ZGTtnaXRKSt9nothrow_t (size_t sz, c_nothrow_p nt)
{
  void *r = _ZnaXRKSt9nothrow_t (sz, nt);
  if (r)
    gtm_thr()->record_allocation (r, del_opvnt);
  return r;
}

/* Wrap: operator delete(void* ptr)  */
void
_ZGTtdlPv (void *ptr)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, _ZdlPv);
}

/* Wrap: operator delete (void *ptr, const std::nothrow_t&)  */
void
_ZGTtdlPvRKSt9nothrow_t (void *ptr, c_nothrow_p nt UNUSED)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, del_opnt);
}

/* Wrap: operator delete[] (void *ptr)  */
void
_ZGTtdaPv (void *ptr)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, _ZdaPv);
}

/* Wrap: operator delete[] (void *ptr, const std::nothrow_t&)  */
void
_ZGTtdaPvRKSt9nothrow_t (void *ptr, c_nothrow_p nt UNUSED)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, del_opvnt);
}

/* Wrap: operator delete(void* ptr, std::size_t sz)  */
void
_ZGTtdlPvX (void *ptr, size_t sz)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, sz, _ZdlPvX);
}

/* Wrap: operator delete (void *ptr, std::size_t sz, const std::nothrow_t&)  */
void
_ZGTtdlPvXRKSt9nothrow_t (void *ptr, size_t sz, c_nothrow_p nt UNUSED)
{
  if (ptr)
    gtm_thr()->forget_allocation (ptr, sz, delsz_opnt);
}

} // extern "C"
